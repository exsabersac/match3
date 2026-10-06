{-# LANGUAGE OverloadedStrings #-}
-- | 收集与计数类：离开盘面（被收走 / 打破）时按计数键记一笔，常作关卡目标。
--
-- 共同特征：缺省原型的占格本体，本身不削层、不变形；饼干打不动、落到底边被收走（离格也算覆盖地毯）；
-- 时间精灵命中 / 邻消即破，按步前步后个数差每个奖励 2 步；气泡（Custom "bubble"）命中 / 邻格真消除即破，
-- 按 CountNamed "bubble" 计数。时间精灵与气泡的邻格反应都是普通邻格 system（120 / 170）。
-- 变色龙（新玩法 7，Custom "chameleon" k）：例外地是普通棋子组件（可交换、按当前颜色匹配、命中即消），
-- 玩家交换的步末（PhaseMove 40）按固定顺序换到下一种颜色，被消除计 CountNamed "chameleon"。
module Match3.Element.Builtin.Collectible
  ( cookieArch
  , timeSpiritArch
  , bubbleArch
  , bubbleAdjacent
  , chameleonArch
  , chameleonName
  , chameleonCell
  , chameleonColor
  , chameleonNext
  , chameleonShift
  ) where

import Control.Applicative ((<|>))
import Data.List (nub)
import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Element.Event (EndEffect(..), EndItem(..), EventKind(..))
import Match3.ECS.Archetype
import Match3.ECS.Component
import Match3.Element.Kind (customPlace)
import Match3.Element.Near
import Match3.Element.Rules (nearBy)
import Match3.ECS.Stage
import Match3.ECS.System (System(..))
import Match3.Element.Types
import Match3.Obstacles (orthoNeighbors)
import Match3.Rainbow (isRainbow, rainbowClearSeeds)
import Match3.Types

-- | 饼干：打不动；随重力下落、可过传送门，落到底边被收走；离格也算覆盖地毯。
cookieArch :: Archetype ()
cookieArch = (archetype "cookie" (unitColumn (== Cookie) Cookie))
  { aSpawn = \_ _ -> Just Cookie
  , aPhysics = const gemPhysics {pRecolor = False, pPush = False, pKeepShuffle = True, pDrains = [EdgeBottom]}
  , aTally = const emptyTally {tCounter = Just CountCookies, tVacatesCarpet = True}
  }

-- | 时间精灵：命中 / 邻消即破（邻格 system 120），按个数差每个奖励 2 步。
timeSpiritArch :: Archetype ()
timeSpiritArch = (archetype "time_spirit" col)
  { aSpawn = \_ _ -> Just TimeSpirit
  , aHit = const breakHit
  , aDiff = Just (DiffCount CountSpirits 2)
  , aFace = const (baseFace "spirit" [])
  , aSystems = [SysNear 120 (nearBy col SkipDirect DiePrepend (\_ _ -> NearNudge Dies))]
  }
  where
    col = unitColumn (== TimeSpirit) TimeSpirit

-- | 气泡：占格本体 Custom "bubble" k。无色、挡交换、随重力下落、不穿传送门、洗牌保留；
-- 邻格有真消除（任意颜色）即破（邻格 system 170），直接命中也破；破掉计 CountNamed "bubble"。
bubbleArch :: Archetype Int
bubbleArch = (archetype "bubble" (customColumn "bubble"))
  { aSpawn = customPlace "bubble"
  , aHit = const breakHit
  , aTally = const emptyTally {tCounter = Just (CountNamed "bubble")}
  , aHud = noHud {hudLabel = Just "气泡"}
  , aSystems = [SysNear 170 bubbleAdjacent]
  }

-- | 气泡邻格：foldr 去重列表序（勿改成 nub，除非重录金标准）。
bubbleAdjacent :: System NearWorld
bubbleAdjacent = System $ \ctx ->
  let b = nwBoard ctx
      popped =
        [ q
        | q <- nubOrd [q' | p <- nwTrue ctx, q' <- orthoNeighbors p, inBounds b q']
        , q `notElem` nwDirect ctx
        , q `notElem` nwTrue ctx
        , isBubble (getCell b q)
        ]
  in ctx {nwDead = popped, nwSit = []}
  where
    isBubble cell = case cell of
      Custom "bubble" _ -> True
      _ -> False
    nubOrd = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | 变色龙（新玩法 7，开心消消乐的变色糖）：占格本体 Custom "chameleon" k，k = 当前颜色的下标（'colorAt' k，
-- 0..4 = C1..C5）。原型 Piece：可交换、按当前颜色参与匹配与提示、命中即消、随重力下落、过传送门、可被蜗牛推动；
-- 另外洗牌原样保留（洗牌只重排普通宝石）、不可被魔法帽 / 染色瓶改色（它自己会换色）。
--
-- * 玩家交换的步末（PhaseMove 40，蜗牛 / 毛球 / 雪怪之后；道具不触发）：盘上每只变色龙按固定顺序
--   （C1 → C2 → … → C5 → C1）换到下一种颜色，若这种颜色会让它立刻连成三消就顺延到再下一种（见 'chameleonShift'；
--   玩家得自己抓时机凑色），纯按盘面、不消耗 gsGen；记 EvTick "chameleon"（每项 = (p, p, 换色后的格)）。
--   其余四种都会连成时保持原色；连原色也会连成（只在换色前就有现成三消时）则取下一种，由步末补结算（settle）
--   照常消除（与蜗牛推出的匹配相同）。
-- * 被消除（匹配 / 特效 / 道具）计 CountNamed "chameleon"；同色消除也照常计入颜色目标。
-- * 与彩虹交换（成对交换 system 15，在彩虹取色 10 之后、特殊合成 20 之前）：清掉彩虹、变色龙当前颜色的全部宝石
--   （同内置彩虹取色）以及同色的全部变色龙。
-- | 元素名。
chameleonName :: ElementName
chameleonName = "chameleon"

-- | 颜色为 c 的变色龙格。
chameleonCell :: Color -> Cell
chameleonCell c = Custom chameleonName (CustomState (fromEnum c))

-- | 变色龙格的当前颜色（不是变色龙 = Nothing）。
chameleonColor :: Cell -> Maybe Color
chameleonColor cell = case cell of
  Custom "chameleon" (CustomState k) -> Just (colorAt k)
  _ -> Nothing

-- | 固定换色顺序的下一种颜色。
chameleonNext :: Color -> Color
chameleonNext c = colorAt (fromEnum c + 1)

-- | 变色龙原型（状态 = 当前颜色下标）。
chameleonArch :: Archetype Int
chameleonArch = (archetype chameleonName (customColumn chameleonName))
  { aSpawn = \args cell -> chameleonCell <$> (prefixArgs argColor args <|> gemColor cell)
  , aMatch = \k -> gemMatch (Just (colorAt k))
  , aHit = const gemHit
  , aPhysics = const gemPhysics {pKeepShuffle = True, pRecolor = False}
  , aTally = const emptyTally {tCounter = Just (CountNamed chameleonName)}
  , aHud = noHud {hudLabel = Just "变色龙", hudGoalIcon = Just "chameleon_icon"}
  , aFace = \k -> noFace {fExtras = [("c", FaceColor (colorAt k))]}
  , aSystems = [SysSwap (SwapSys 15 chameleonRainbowFires chameleonRainbowSeeds), SysEnd (moveSys 40 (effectSystem (chameleonRun . ewBoard)))]
  }
  where
    gemColor c = case c of Gem col _ _ _ -> Just col; _ -> Nothing

-- | 步末换色（纯函数，测试直接调用）：返回换了色的格（行优先）与新盘面。
-- 每只变色龙（行优先，在逐只换过的盘面上）按固定顺序从下一种颜色试起（五种里最后一种是原色），取第一种不会让它
-- 立刻连成三消的颜色；五种都会连成时取下一种（由步末补结算照常消除）。「连成」按 'plainColor' 判断：普通 / 特殊宝石（不带叠层）
-- 与变色龙的颜色，其余格打断连线。
chameleonShift :: Board -> ([Pos], Board)
chameleonShift b0 = foldl one ([], b0) (boardPositions b0)
  where
    one (ps, b) p = case chameleonColor (getCell b0 p) of
      Just col ->
        let tries = take numColors (iterate chameleonNext (chameleonNext col))
            pick = case [c | c <- tries, not (runsThrough (setCell b p (chameleonCell c)) p c)] of
              (c : _) -> c
              [] -> chameleonNext col
        in (ps ++ [p], setCell b p (chameleonCell pick))
      Nothing -> (ps, b)

-- | 盘面 b 上经过 p 的横向或纵向同色连线是否 ≥ 3（颜色按 'plainColor'）。
runsThrough :: Board -> Pos -> Color -> Bool
runsThrough b (r, c) col = span1 (0, 1) + span1 (0, -1) >= 2 || span1 (1, 0) + span1 (-1, 0) >= 2
  where
    span1 (dr, dc) = length (takeWhile same [(r + k * dr, c + k * dc) | k <- [1 .. 2]])
    same q = inBounds b q && plainColor (getCell b q) == Just col

-- | 换色时判断连线用的颜色：不带叠层的宝石（含冰、特殊块）与变色龙；其余 Nothing。
plainColor :: Cell -> Maybe Color
plainColor cell = case cell of
  Gem c _ _ Nothing -> Just c
  _ -> chameleonColor cell

chameleonRun :: Board -> (Maybe EndEffect, Board)
chameleonRun b =
  let (ps, b') = chameleonShift b
  in (if null ps then Nothing else Just (EndEffect EvTick chameleonName [EndItem p p (getCell b' p) Nothing | p <- ps]), b')

-- | 彩虹 × 变色龙：一端是能点火的彩虹、另一端是变色龙（交换前盘面）。
chameleonRainbowFires :: Board -> Pos -> Pos -> Bool
chameleonRainbowFires b p1 p2 = rainbowOn p1 p2 || rainbowOn p2 p1
  where
    rainbowOn p q = isRainbow (getCell b p) && specialActivates (getCell b p) && chameleonColor (getCell b q) /= Nothing

-- | 种子（交换后盘面）：把变色龙那一端当成同色普通宝石问内置彩虹取色，再加上同色的全部变色龙。
chameleonRainbowSeeds :: Board -> Pos -> Pos -> [Pos]
chameleonRainbowSeeds b p1 p2 = case [(q, col) | q <- [p1, p2], Just col <- [chameleonColor (getCell b q)]] of
  ((q, col) : _) ->
    nub
      ( rainbowClearSeeds (setCell b q (mkGem col)) p1 p2
          ++ positionsWhere ((== Just col) . chameleonColor) b
      )
  [] -> []

--------------------------------------------------------------------------------
-- 条目
