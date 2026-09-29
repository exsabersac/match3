{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | 第 9 刀前的元素类（27 个带默认实现的方法）与全部内置本体 instance 的**逐字副本**，只用来和能力记录 Caps 的新写法对照
-- （Spec.Caps 的 qc_caps_match_legacy_elements / qc_caps_rules_match_legacy / qc_default_caps_match_legacy_defaults）。
--
-- 改动只有改名：类 Element → LElement，装箱 SomeElement → LSome，受击结果 Hit → LHit（LAbsorb / LDestroy / LImmune），
-- 修饰组合 Modified → LModified（修饰器类 Modifier 与各叠层 instance 第 9 刀没有改，直接用 src 里的）。
-- 内置本体的 instance 挂在 src 的元素类型上（孤儿 instance），私有辅助函数（chip / tickRun / snailRun /
-- traceSnailsBy / bubbleAdjacent）一并逐字复制。解码 legacyElementOf 与注册表的分派一致（本体按构造器、冰 → 叠层自上而下）。
module Spec.Support.LegacyElement
  ( LElement(..)
  , LHit(..)
  , LSome(..)
  , LModified(..)
  , lmodify
  , legacyBodyOf
  , legacyElementOf
  ) where

import Data.Typeable (Typeable, cast)
import Match3.Board.Grid (getCell, inBounds)
import Match3.Countdown (explodeSeedsFor, tickCountdowns)
import Match3.Element.Builtin
  ( PlainGem(..), SpecialGem(..), Bubble(..), Jelly(..), specialBlast )
import Match3.Element.Builtin.Actor (BottleE(..), CountdownE(..), MagicHatE(..), MakerE(..), SnailE(..))
import Match3.Element.Builtin.Collectible (CookieE(..), TimeSpiritE(..))
import Match3.Element.Builtin.Common (deadRule)
import Match3.Element.Builtin.Layer
import Match3.Element.Builtin.Obstacle (BalloonE(..), CakeE(..), ChestE(..), FlipE(..), HoneyE(..), SafeE(..), StoneE(..), SurpriseEgg(..))
import Match3.Element.Class (Archetype(..), Inert(..), ModHit(..), Modifier(..), SomeMessage(..), SomeModifier(..))
import Match3.Element.Event
import Match3.Element.Types
import Match3.Obstacles
  ( chargeAdjacentMakersSit
  , chipAdjacentBalloonsExcept
  , chipAdjacentCakesExcept
  , chipAdjacentChestsExcept
  , chipAdjacentHoneyExcept
  , chipAdjacentSafesExcept
  , chipAdjacentStonesExcept
  , chipAdjacentTimeSpiritsExcept
  , openSurprises
  , orthoNeighbors
  , triggerAdjacentBottlesBy
  , triggerAdjacentHatsBy
  )
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Snail (snailPositions, stepSnailAtBy)
import Match3.Types

-- | 第 9 刀前的 Hit（装的是 LSome）。
data LHit
  = LAbsorb LSome
  | LDestroy
  | LImmune
  deriving (Eq, Show)

-- | 第 9 刀前的 Element 类（逐字，只改类名）。
class (Show e, Eq e, Typeable e) => LElement e where
  -- | 元素名：注册表的键，也是关卡放置表、计数键、前端贴图的键。
  name :: e -> ElementName
  -- | 把元素值写回格子（盘面的存储编码）。
  toCell :: e -> Cell

  archetype :: e -> Archetype
  archetype _ = Piece

  -- | 本体颜色（参与匹配与颜色袋计数）；缺省取写回格子后的宝石颜色（只有宝石格有）。
  color :: e -> Maybe Color
  color e = case toCell e of
    Gem c _ _ _ -> Just c
    _ -> Nothing
  -- | 参与匹配的颜色；缺省 = 'color'（修饰器可以挡住）。
  matchColor :: e -> Maybe Color
  matchColor = color
  blocksSwap :: e -> Bool
  blocksSwap e = archetype e /= Piece
  -- | 特殊块能否点火（自上而下第一个 Just 决定）。
  activates :: e -> Maybe Bool
  activates e = Just (archetype e == Piece)
  falls :: e -> Bool
  falls e = archetype e /= Fixed
  portal :: e -> Bool
  portal e = archetype e == Piece
  -- | 边缘收集：本体到达这些边时被收走。
  drains :: e -> [Edge]
  drains _ = []
  onHit :: e -> LHit
  onHit e = if archetype e == Piece then LDestroy else LImmune
  -- | 真消除时随格清掉的上层（修饰器用；本体恒 False）。
  stripOnClear :: e -> Bool
  stripOnClear _ = False
  counter :: e -> Maybe CounterKey
  counter _ = Nothing
  -- | 按步前 / 步后盘面上的个数差计数（保险箱开启、时间精灵）。
  diffCounter :: e -> Maybe CounterKey
  diffCounter _ = Nothing
  bonusMoves :: e -> Int
  bonusMoves _ = 0
  -- | 离开格子（不进清除格）也算覆盖地毯。
  vacatesCarpet :: e -> Bool
  vacatesCarpet _ = False
  keepOnShuffle :: e -> Bool
  keepOnShuffle e = archetype e /= Piece
  -- | 被消除且能点火时的爆炸范围。
  blast :: e -> Maybe (Pos -> [Pos])
  blast _ = Nothing
  recolorable :: e -> Bool
  recolorable e = archetype e == Piece
  pushable :: e -> Bool
  pushable e = archetype e == Piece
  -- | 普通匹配提示是否试这个格（彩虹 = False：它只经成对交换规则给提示）。
  hintable :: e -> Bool
  hintable _ = True

  -- 规则类能力（在原型值上取）
  adjacentRule :: e -> Maybe AdjacentRule
  adjacentRule _ = Nothing
  endRule :: e -> Maybe EndRule
  endRule _ = Nothing
  swapRule :: e -> Maybe SwapRule
  swapRule _ = Nothing
  openRule :: e -> Maybe OpenRule
  openRule _ = Nothing
  -- | 地面层：上方格子被消除一次时，层数 → 新层数（Nothing = 清掉）。
  groundRule :: e -> Maybe (Int -> Maybe Int)
  groundRule _ = Nothing

  -- | 处理一条消息：Nothing = 不关心；Just = 新的元素值。
  handleMessage :: e -> SomeMessage -> Maybe LSome
  handleMessage _ _ = Nothing


-- | 第 9 刀前的 SomeElement。
data LSome = forall e. LElement e => LSome e

instance Eq LSome where
  LSome a == LSome b = maybe False (== b) (cast a)

instance Show LSome where
  showsPrec d (LSome e) =
    showParen (d > 10) (showString "SomeElement " . showsPrec 11 (name e) . showChar ' ' . showsPrec 11 e)

instance LElement LSome where
  name (LSome e) = name e
  toCell (LSome e) = toCell e
  archetype (LSome e) = archetype e
  color (LSome e) = color e
  matchColor (LSome e) = matchColor e
  blocksSwap (LSome e) = blocksSwap e
  activates (LSome e) = activates e
  falls (LSome e) = falls e
  portal (LSome e) = portal e
  drains (LSome e) = drains e
  onHit (LSome e) = onHit e
  stripOnClear (LSome e) = stripOnClear e
  counter (LSome e) = counter e
  diffCounter (LSome e) = diffCounter e
  bonusMoves (LSome e) = bonusMoves e
  vacatesCarpet (LSome e) = vacatesCarpet e
  keepOnShuffle (LSome e) = keepOnShuffle e
  blast (LSome e) = blast e
  recolorable (LSome e) = recolorable e
  pushable (LSome e) = pushable e
  hintable (LSome e) = hintable e
  adjacentRule (LSome e) = adjacentRule e
  endRule (LSome e) = endRule e
  swapRule (LSome e) = swapRule e
  openRule (LSome e) = openRule e
  groundRule (LSome e) = groundRule e
  handleMessage (LSome e) = handleMessage e


-- | 第 9 刀前的 Modified。
data LModified = LModified SomeModifier LSome
  deriving (Eq, Show)

lmodify :: Modifier m => m -> LSome -> LSome
lmodify m e = LSome (LModified (SomeModifier m) e)

instance LElement LModified where
  name (LModified _ e) = name e
  toCell (LModified (SomeModifier m) e) = modApply m (toCell e)
  archetype (LModified _ e) = archetype e
  color (LModified _ e) = color e
  matchColor (LModified (SomeModifier m) e)
    | modBlocksMatch m = Nothing
    | otherwise = matchColor e
  blocksSwap (LModified (SomeModifier m) e) = modBlocksSwap m || blocksSwap e
  activates (LModified (SomeModifier m) e) = maybe (activates e) Just (modActivates m)
  falls (LModified _ e) = falls e
  portal (LModified _ e) = portal e
  drains (LModified _ e) = drains e
  onHit (LModified (SomeModifier m) e) = case modOnHit m of
    Pierce -> case onHit e of
      LAbsorb e' -> LAbsorb (lmodify m e')
      r -> r
    Keep m' -> LAbsorb (lmodify m' e)
    Remove -> LAbsorb e
    Shatter -> LDestroy
  stripOnClear (LModified (SomeModifier m) _) = modStripOnClear m
  counter (LModified _ e) = counter e
  diffCounter (LModified _ e) = diffCounter e
  bonusMoves (LModified _ e) = bonusMoves e
  vacatesCarpet (LModified _ e) = vacatesCarpet e
  keepOnShuffle _ = True
  blast (LModified _ e) = blast e
  recolorable (LModified _ e) = recolorable e
  pushable (LModified _ e) = pushable e
  hintable (LModified _ e) = hintable e
  handleMessage (LModified sm@(SomeModifier m) e) msg = case modHandleMessage m msg of
    Just Nothing -> Just e
    Just (Just m') -> Just (lmodify m' e)
    Nothing -> fmap (LSome . LModified sm) (handleMessage e msg)


instance LElement Inert where
  name (Inert n _) = n
  toCell (Inert _ cell) = cell
  archetype _ = Blocker
  color _ = Nothing


--------------------------------------------------------------------------------
-- 内置本体（第 9 刀前的 instance，逐字）

-- Gem.hs
instance LElement PlainGem where
  name _ = "gem"
  toCell (PlainGem c) = Gem c Normal 0 Nothing

instance LElement SpecialGem where
  name (SpecialGem _ k) = case k of
    LineH -> "line_h"
    LineV -> "line_v"
    Bomb -> "bomb"
    Rainbow -> "rainbow"
    Normal -> "gem"
  toCell (SpecialGem c k) = Gem c k 0 Nothing
  keepOnShuffle (SpecialGem _ k) = k /= Normal
  blast (SpecialGem _ k) = specialBlast k
  -- 彩虹不进普通匹配提示（只经成对交换规则给提示）
  hintable (SpecialGem _ k) = k /= Rainbow
  swapRule (SpecialGem _ k) = case k of
    -- 彩虹取色：由交换对象决定清哪种颜色；先于特殊合成（组合表，次序 20）判定
    Rainbow -> Just (SwapRule 10 isRainbowSwap rainbowClearSeeds)
    _ -> Nothing

-- Obstacle.hs
instance LElement StoneE where
  name _ = "stone"
  toCell (StoneE n) = Stone n
  archetype _ = Blocker
  onHit (StoneE n) = chip n StoneE
  adjacentRule _ = Just (AdjacentRule 10 (deadRule chipAdjacentStonesExcept))
  counter _ = Just CountStones

instance LElement ChestE where
  name _ = "chest"
  toCell (ChestE n) = Chest n
  archetype _ = Blocker
  onHit (ChestE n) = chip n ChestE
  adjacentRule _ = Just (AdjacentRule 20 (deadRule chipAdjacentChestsExcept))
  counter _ = Just CountChests

instance LElement HoneyE where
  name _ = "honey"
  toCell (HoneyE n) = Honey n
  archetype _ = Blocker
  onHit (HoneyE n) = chip n HoneyE
  adjacentRule _ = Just (AdjacentRule 30 (deadRule chipAdjacentHoneyExcept))
  counter _ = Just CountHoney

instance LElement CakeE where
  name _ = "cake"
  toCell (CakeE n) = Cake n
  archetype _ = Blocker
  onHit (CakeE n) = chip n CakeE
  adjacentRule _ = Just (AdjacentRule 40 (deadRule chipAdjacentCakesExcept))
  counter _ = Just CountCakes

instance LElement BalloonE where
  name _ = "balloon"
  toCell (BalloonE c) = Balloon c
  archetype _ = Blocker
  onHit _ = LDestroy
  adjacentRule _ = Just (AdjacentRule 50 (deadRule chipAdjacentBalloonsExcept))
  counter _ = Just CountBalloons

instance LElement SafeE where
  name _ = "safe"
  toCell (SafeE n) = Safe n
  archetype _ = Blocker
  onHit (SafeE n)
    | n <= 1 = LAbsorb (LSome CookieE)
    | otherwise = LAbsorb (LSome (SafeE (n - 1)))
  adjacentRule _ = Just (AdjacentRule 110 (\ctx b -> AdjOut (fst (chipAdjacentSafesExcept b (acTrue ctx) (acDirect ctx))) [] []))
  diffCounter _ = Just CountSafes
  vacatesCarpet _ = True

instance LElement FlipE where
  name _ = "flip"
  toCell (FlipE f b) = Flip f b
  archetype _ = Blocker
  color (FlipE f _) = Just f
  blocksSwap _ = False
  portal _ = True
  pushable _ = True
  recolorable _ = True
  onHit (FlipE _ back) = LAbsorb (LSome (PlainGem back))

instance LElement SurpriseEgg where
  name _ = "surprise"
  toCell _ = Surprise
  archetype _ = Blocker
  onHit _ = LDestroy
  openRule _ = Just (OpenRule openSurprises)

-- Collectible.hs
instance LElement CookieE where
  name _ = "cookie"
  toCell _ = Cookie
  archetype _ = Blocker
  portal _ = True
  drains _ = [EdgeBottom]
  counter _ = Just CountCookies
  vacatesCarpet _ = True

instance LElement TimeSpiritE where
  name _ = "time_spirit"
  toCell _ = TimeSpirit
  archetype _ = Blocker
  onHit _ = LDestroy
  adjacentRule _ = Just (AdjacentRule 120 (deadRule chipAdjacentTimeSpiritsExcept))
  diffCounter _ = Just CountSpirits
  bonusMoves _ = 2

instance LElement Bubble where
  name _ = "bubble"
  toCell (Bubble k) = Custom "bubble" (CustomState k)
  archetype _ = Blocker
  onHit _ = LDestroy
  adjacentRule _ = Just (AdjacentRule 170 bubbleAdjacent)
  counter _ = Just (CountNamed "bubble")

-- Actor.hs
instance LElement MagicHatE where
  name _ = "magic_hat"
  toCell _ = MagicHat
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 60 (\ctx b -> AdjOut (triggerAdjacentHatsBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] []))

instance LElement MakerE where
  name _ = "maker"
  toCell (MakerE c n) = Maker c n
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 130 (\ctx b -> let (b', sit) = chargeAdjacentMakersSit b (acTrue ctx) in AdjOut b' [] sit))

instance LElement SnailE where
  name _ = "snail"
  toCell (SnailE dr dc) = Snail dr dc
  archetype _ = Fixed
  endRule _ = Just (EndRule PhaseMove 10 snailRun (const []) (const []))

instance LElement BottleE where
  name _ = "bottle"
  toCell (BottleE c) = Bottle c
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 140 (\ctx b -> AdjOut (triggerAdjacentBottlesBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] []))

instance LElement CountdownE where
  name _ = "countdown"
  toCell (CountdownE c n) = Countdown c n
  archetype _ = Blocker
  color (CountdownE c _) = Just c
  blocksSwap _ = False
  portal _ = True
  pushable _ = True
  recolorable _ = True
  onHit _ = LDestroy
  endRule _ = Just (EndRule PhaseTick 10 tickRun explodeSeedsFor (const []))

-- Ground.hs
instance LElement Jelly where
  name _ = "jelly"
  toCell (Jelly n) = Custom "jelly" (CustomState n)
  groundRule _ = Just (\n -> if n > 1 then Just (n - 1) else Nothing)
  counter _ = Just (CountNamed "jelly")

--------------------------------------------------------------------------------
-- 私有辅助函数（逐字）

chip :: LElement e => Int -> (Int -> e) -> LHit
chip n con
  | n <= 1 = LDestroy
  | otherwise = LAbsorb (LSome (con (n - 1)))

tickRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
tickRun _ b =
  let b' = tickCountdowns b
      ticked = [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell b p /= getCell b' p]
  in (if null ticked then Nothing else Just (EndEffect EvTick "countdown" [EndItem p p (getCell b' p) Nothing | p <- ticked]), b')

snailRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
snailRun ctx b =
  let (ms, b') = traceSnailsBy (ecPushable ctx) (ecAvoid ctx) (ecWalls ctx) b
  in (if null ms then Nothing else Just (EndEffect EvMove "snail" ms), b')

traceSnailsBy :: (Cell -> Bool) -> [Pos] -> [Pos] -> Board -> ([EndItem], Board)
traceSnailsBy canPush avoid walls b0 =
  let (movesRev, b1) = foldl one ([], b0) [p | p <- snailPositions b0, p `notElem` avoid]
  in (reverse movesRev, b1)
  where
    one (acc, board) pos = case getCell board pos of
      Snail dr dc ->
        let board' = stepSnailAtBy canPush walls board pos
            next = (fst pos + dr, snd pos + dc)
            mv = case getCell board' pos of
              Snail dr' dc' -> EndItem pos pos (Snail dr' dc') Nothing
              pushed -> EndItem pos next (Snail dr dc) (Just pushed)
        in (mv : acc, board')
      _ -> (acc, board)

bubbleAdjacent :: AdjCtx -> Board -> AdjOut
bubbleAdjacent ctx b =
  let popped =
        [ q
        | q <- nubOrd [q' | p <- acTrue ctx, q' <- orthoNeighbors p, inBounds q']
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

--------------------------------------------------------------------------------
-- 解码（与内置注册表的分派一致）

-- | 本体：按构造器；Custom 只认 bubble，其余名字是惰性占格。
legacyBodyOf :: Cell -> LSome
legacyBodyOf cell = case cell of
  Gem c Normal _ _ -> LSome (PlainGem c)
  Gem c k _ _ -> LSome (SpecialGem c k)
  Stone n -> LSome (StoneE n)
  Chest n -> LSome (ChestE n)
  Honey n -> LSome (HoneyE n)
  Balloon c -> LSome (BalloonE c)
  Cookie -> LSome CookieE
  Cake n -> LSome (CakeE n)
  MagicHat -> LSome MagicHatE
  Maker c n -> LSome (MakerE c n)
  Snail dr dc -> LSome (SnailE dr dc)
  Safe n -> LSome (SafeE n)
  Flip f b -> LSome (FlipE f b)
  Surprise -> LSome SurpriseEgg
  Bottle c -> LSome (BottleE c)
  TimeSpirit -> LSome TimeSpiritE
  Countdown c n -> LSome (CountdownE c n)
  Custom "bubble" k -> LSome (Bubble (unCustomState k))
  Custom n _ -> LSome (Inert n cell)

-- | 整格：冰 → 叠层（自上而下）包着本体。
legacyElementOf :: Cell -> LSome
legacyElementOf cell = foldr (\(SomeModifier m) e -> lmodify m e) (legacyBodyOf cell) uppers
  where
    uppers = case cell of
      Gem _ _ ice ov -> [SomeModifier (Ice ice) | ice > 0] ++ maybe [] (\o -> [overlayMod o]) ov
      _ -> []
    overlayMod o = case o of
      Grass -> SomeModifier GrassL
      Vine -> SomeModifier VineL
      Choco -> SomeModifier ChocoL
      Fog n -> SomeModifier (FogL n)
      Chain n -> SomeModifier (ChainL n)
      Freeze n -> SomeModifier (FreezeL n)
      Curtain n -> SomeModifier (CurtainL n)
      Steam -> SomeModifier SteamL
