{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 收集与计数类：离开盘面（被收走 / 打破）时按计数键记一笔，常作关卡目标。
--
-- 共同特征：原型 Blocker 的占格本体，本身不削层、不变形；饼干打不动、落到底边被收走（离格也算覆盖地毯）；
-- 时间精灵命中 / 邻消即破，按步前步后个数差每个奖励 2 步；气泡（Custom "bubble"）命中 / 邻格真消除即破，
-- 按 CountNamed "bubble" 计数。时间精灵走 onNear；气泡邻格因去重序留逃生口 AdjacentPass 170。
-- 变色龙（新玩法 7，Custom "chameleon" k）：例外地是普通棋子原型（可交换、按当前颜色匹配、命中即消），
-- 玩家交换的步末（PhaseMove 40）按固定顺序换到下一种颜色，被消除计 CountNamed "chameleon"。
module Match3.Element.Builtin.Collectible
  ( CookieE(..)
  , TimeSpiritE(..)
  , Bubble(..)
  , bubbleAdjacent
  , Chameleon(..)
  , chameleonName
  , chameleonCell
  , chameleonColor
  , chameleonNext
  , chameleonShift
  ) where

import Control.Applicative ((<|>))
import Data.List (nub)
import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Element.Ability
import Match3.Element.Event (EndEffect(..), EndItem(..), EventKind(..))
import Match3.Element.Kind
import Match3.Element.Near
import Match3.Element.Phase
import Match3.Element.Types
import Match3.Obstacles (orthoNeighbors)
import Match3.Rainbow (isRainbow, rainbowClearSeeds)
import Match3.Types

-- | 饼干：打不动；随重力下落、可过传送门，落到底边被收走；离格也算覆盖地毯。
data CookieE = CookieE
  deriving (Eq, Show)
  deriving (Matchable, Hittable) via (Obstacle CookieE)

instance Cellular CookieE where
  nameOf _ = "cookie"
  toCell _ = Cookie

instance Movable CookieE where
  drains _ = [EdgeBottom]
  keepOnShuffle _ = True
  recolorable _ = False
  pushable _ = False

instance Countable CookieE where
  counter _ = Just CountCookies
  vacatesCarpet _ = True

instance Renders CookieE

instance Phase CookieE where
  codec = Codec
    { cName = "cookie"
    , cToCell = \_ -> Cookie
    , cFromCell = \cell -> case cell of Cookie -> Just CookieE; _ -> Nothing
    , cPlace = \_ _ -> Just Cookie
    , cMeta = emptyMeta { metaCounter = Just CountCookies, metaVacatesCarpet = True }
    , cNear = Nothing
    }
  onMatch _ = obstacleMatch
  onHit _ _ = HitOut Immune False Nothing Nothing
  physics _ = gemPhysics { pRecolor = False, pPush = False, pKeepShuffle = True, pDrains = [EdgeBottom] }
  view _ = emptyFace "cookie"

instance Kind CookieE where
  place _ = cPlace (codec @CookieE)
  neighbourPrio _ = phaseNearPrio @CookieE
  reach _ = phaseReach @CookieE
  dieOrder _ = phaseDieOrder @CookieE
  onNear = phaseOnNear

-- | 时间精灵：命中 / 邻消即破，按个数差每个奖励 2 步。
data TimeSpiritE = TimeSpiritE
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle TimeSpiritE)

instance Cellular TimeSpiritE where
  nameOf _ = "time_spirit"
  toCell _ = TimeSpirit

instance Hittable TimeSpiritE where
  fires _ = False

instance Countable TimeSpiritE
instance Renders TimeSpiritE where
  faceBase _ = Just ("spirit", [])

instance Phase TimeSpiritE where
  codec = Codec
    { cName = "time_spirit"
    , cToCell = \_ -> TimeSpirit
    , cFromCell = \cell -> case cell of TimeSpirit -> Just TimeSpiritE; _ -> Nothing
    , cPlace = \_ _ -> Just TimeSpirit
    , cMeta = emptyMeta { metaDiffCounter = Just CountSpirits, metaBonusMoves = 2 }
    , cNear = Just (NearRule 120 SkipDirect DiePrepend)
    }
  onMatch _ = obstacleMatch
  onHit _ _ = HitOut Destroy False Nothing Nothing
  physics _ = obstaclePhysics
  onNear _ _ _ = NearNudge Dies
  view _ = emptyFace "spirit"

instance Kind TimeSpiritE where
  place _ = cPlace (codec @TimeSpiritE)
  neighbourPrio _ = phaseNearPrio @TimeSpiritE
  reach _ = phaseReach @TimeSpiritE
  dieOrder _ = phaseDieOrder @TimeSpiritE
  onNear = phaseOnNear
  diffCounter _ = Just CountSpirits
  bonusMoves _ = 2

-- | 气泡：占格本体 Custom "bubble" k。无色、挡交换、随重力下落、不穿传送门、洗牌保留；
-- 邻格有真消除（任意颜色）即破，直接命中也破；破掉计 CountNamed "bubble"。
newtype Bubble = Bubble Int
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle Bubble)

instance Cellular Bubble where
  nameOf _ = "bubble"

instance Hittable Bubble where
  fires _ = False

instance Countable Bubble where
  counter _ = Just (CountNamed "bubble")

instance Renders Bubble

instance Phase Bubble where
  codec = Codec
    { cName = "bubble"
    , cToCell = toCell
    , cFromCell = fromCustom "bubble" Bubble
    , cPlace = customPlace "bubble"
    , cMeta = emptyMeta { metaCounter = Just (CountNamed "bubble") }
    , cNear = Nothing  -- 邻格逃生口 boardPasses
    }
  onMatch _ = obstacleMatch
  onHit _ _ = HitOut Destroy False Nothing Nothing
  physics _ = obstaclePhysics
  view _ = emptyFace "bubble"

instance Kind Bubble where
  place _ = cPlace (codec @Bubble)
  label _ = Just "气泡"
  -- 邻格打碎留在逃生口：foldr 去重序与 nub+DieAppend 不等价
  boardPasses _ = [AdjacentPass 170 bubbleAdjacent]

-- | 气泡邻格：foldr 去重列表序（勿改成 nub，除非重录金标准）。
bubbleAdjacent :: AdjCtx -> Board -> AdjOut
bubbleAdjacent ctx b =
  let popped =
        [ q
        | q <- nubOrd [q' | p <- acTrue ctx, q' <- orthoNeighbors p, inBounds b q']
        , q `notElem` acDirect ctx
        , q `notElem` acTrue ctx
        , isBubble (getCell b q)
        ]
  in AdjOut b popped []
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
-- * 与彩虹交换（成对交换规则 15，在彩虹取色 10 之后、特殊合成 20 之前）：清掉彩虹、变色龙当前颜色的全部宝石
--   （同内置彩虹取色）以及同色的全部变色龙。
newtype Chameleon = Chameleon Int
  deriving (Eq, Show)

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

instance Cellular Chameleon where
  nameOf _ = chameleonName

instance Matchable Chameleon where
  color (Chameleon k) = Just (colorAt k)

instance Hittable Chameleon

instance Movable Chameleon where
  keepOnShuffle _ = True
  recolorable _ = False

instance Countable Chameleon where
  counter _ = Just (CountNamed "chameleon")

instance Renders Chameleon where
  face (Chameleon k) = [("c", FaceColor (colorAt k))]  -- 当前颜色（网页格子 JSON 的 "c"，同宝石；前端不自己换算 v）

instance Phase Chameleon where
  codec = Codec
    { cName = chameleonName
    , cToCell = toCell
    , cFromCell = fromCustom chameleonName Chameleon
    , cPlace = \args cell -> chameleonCell <$> (prefixArgs argColor args <|> gemColor cell)
    , cMeta = emptyMeta { metaCounter = Just (CountNamed chameleonName) }
    , cNear = Nothing
    }
    where
      gemColor c = case c of Gem col _ _ _ -> Just col; _ -> Nothing
  onMatch (Chameleon k) = gemMatch (Just (colorAt k))
  onHit _ _ = gemHit
  physics _ = gemPhysics { pKeepShuffle = True, pRecolor = False }
  view _ = emptyFace "chameleon"

instance Kind Chameleon where
  place _ = cPlace (codec @Chameleon)
  label _ = Just "变色龙"
  goalIconName _ = Just "chameleon_icon"
  boardPasses _ = [SwapPass (SwapRule 15 chameleonRainbowFires chameleonRainbowSeeds), EndPass (moveRule 40 chameleonRun)]

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

chameleonRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
chameleonRun _ b =
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
