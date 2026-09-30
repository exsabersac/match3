{-# LANGUAGE OverloadedStrings #-}
-- | 第 9 刀：元素能力记录 Caps。元素类只剩 name / toCell / caps，各能力按职责分成五组带默认值的记录
-- （匹配与交换 / 消除与受击 / 重力与移动 / 计数与目标 / 步末与变化），元素只声明自己用到的能力。
--
-- 对照的旧实现是 Spec.Support.LegacyElement：第 9 刀前的 Element 类（27 个方法）与全部内置本体 instance 的逐字副本。
-- 性质统一挂在固定种子下（同 Spec.Properties）。
module Spec.Caps
  ( tests
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, nub)
import Data.Maybe (isJust)
import Match3.Core (GameState(..), namedCounts, newGame)
import Match3.Board.Grid (getCell, setCell)
import Match3.Element
import Match3.Element.Caps (blocker, counts, hit)
import Match3.Element.Class
import Match3.Game.Boosters (resolveHammerWith)
import Spec.Support (stableBoard)
import Spec.Support.LegacyElement (LElement, LHit(..), LSome(..), legacyElementOf)
import qualified Spec.Support.LegacyElement as L
import Spec.Support.Source (builtinSources, readCode)
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Fixed)

tests :: [TestTree]
tests =
  map fixedSeed
    [ testProperty "qc_caps_match_legacy_elements" (withMaxSuccess 3000 qc_caps_match_legacy_elements)
    , testProperty "qc_caps_rules_match_legacy" (withMaxSuccess 600 qc_caps_rules_match_legacy)
    , testProperty "qc_default_caps_match_legacy_defaults" (withMaxSuccess 1000 qc_default_caps_match_legacy_defaults)
    ]
    ++ [ testCase "caps_element_class_is_thin" caps_element_class_is_thin
       , testCase "ext_caps_element_plugs_in" ext_caps_element_plugs_in
       ]
  where
    fixedSeed = localOption (QuickCheckReplayLegacy 20260930)

-- | 探针消息：没有内置元素收它（两边的 handleMessage 都应是 Nothing）。
data Probe = Probe

instance Message Probe

blastProbes :: [Pos]
blastProbes = [(0, 0), (3, 4), (7, 7)]

-- | 新写法下一个元素值的全部查询结果（受击后的新元素递归展开）。
newSig :: SomeElement -> [String]
newSig e =
  [ show (name e), show (toCell e), show (archetype e), show (color e), show (matchColor e)
  , show (blocksSwap e), show (activates e), show (falls e), show (portal e), show (drains e)
  , hitSig (onHit e), show (stripOnClear e), show (counter e), show (diffCounter e), show (bonusMoves e)
  , show (vacatesCarpet e), show (keepOnShuffle e), show (fmap (\f -> map f blastProbes) (blast e))
  , show (recolorable e), show (pushable e), show (hintable e)
  , show (fmap arOrder (adjacentRule e)), show (fmap (\r -> (erPhase r, erOrder r)) (endRule e))
  , show (fmap srOrder (swapRule e)), show (isJust (openRule e)), show (fmap (\g -> map g [0 .. 3]) (groundRule e))
  , show (isJust (handleMessage e (SomeMessage Probe)))
  ]
  where
    hitSig h = case h of
      Absorb e' -> "Absorb " ++ show (newSig e')
      Destroy -> "Destroy"
      Immune -> "Immune"

-- | 第 9 刀前的同一组查询。
oldSig :: LSome -> [String]
oldSig e =
  [ show (L.name e), show (L.toCell e), show (L.archetype e), show (L.color e), show (L.matchColor e)
  , show (L.blocksSwap e), show (L.activates e), show (L.falls e), show (L.portal e), show (L.drains e)
  , hitSig (L.onHit e), show (L.stripOnClear e), show (L.counter e), show (L.diffCounter e), show (L.bonusMoves e)
  , show (L.vacatesCarpet e), show (L.keepOnShuffle e), show (fmap (\f -> map f blastProbes) (L.blast e))
  , show (L.recolorable e), show (L.pushable e), show (L.hintable e)
  , show (fmap arOrder (L.adjacentRule e)), show (fmap (\r -> (erPhase r, erOrder r)) (L.endRule e))
  , show (fmap srOrder (L.swapRule e)), show (isJust (L.openRule e)), show (fmap (\g -> map g [0 .. 3]) (L.groundRule e))
  , show (isJust (L.handleMessage e (SomeMessage Probe)))
  ]
  where
    hitSig h = case h of
      LAbsorb e' -> "Absorb " ++ show (oldSig e')
      LDestroy -> "Destroy"
      LImmune -> "Immune"

genColor :: Gen Color
genColor = elements [minBound .. maxBound]

genOverlay :: Gen CellOverlay
genOverlay =
  oneof
    [ elements [Grass, Vine, Choco, Steam]
    , Fog <$> choose (1, 3)
    , Chain <$> choose (1, 3)
    , Freeze <$> choose (1, 3)
    , Curtain <$> choose (1, 3)
    ]

-- | 任意格：全部内置本体（各种状态）、宝石 × 冰 0..3 × 叠层、已注册 / 未注册的 Custom。
genCell :: Gen Cell
genCell =
  frequency
    [ (8, Gem <$> genColor <*> elements [Normal, LineH, LineV, Bomb, Rainbow] <*> choose (0, 3) <*> frequency [(2, pure Nothing), (3, Just <$> genOverlay)])
    , (1, Stone <$> choose (1, 3))
    , (1, Chest <$> choose (1, 3))
    , (1, Honey <$> choose (1, 3))
    , (1, Balloon <$> genColor)
    , (1, pure Cookie)
    , (1, Cake <$> choose (1, 3))
    , (1, pure MagicHat)
    , (1, Maker <$> genColor <*> choose (0, 3))
    , (1, elements [Snail 0 1, Snail 0 (-1), Snail 1 0, Snail (-1) 0])
    , (1, Safe <$> choose (1, 3))
    , (1, Flip <$> genColor <*> genColor)
    , (1, pure Surprise)
    , (1, Bottle <$> genColor)
    , (1, pure TimeSpirit)
    , (1, Countdown <$> genColor <*> choose (1, 5))
    , (1, Custom <$> elements ["bubble", "qc_unregistered"] <*> (CustomState <$> choose (1, 3)))
    ]

-- | 内置注册表解码出的元素（Caps 写法）与第 9 刀前的类 + instance 在每个查询上逐项相同
-- （含冰 / 叠层的组合、受击后变成的新元素、规则的有无与次序、消息）。
qc_caps_match_legacy_elements :: Property
qc_caps_match_legacy_elements =
  forAll genCell $ \cell ->
    counterexample (show cell) $
      conjoin
        [ newSig (elementOf defaultRegistry cell) === oldSig (legacyElementOf cell)
        , newSig (bodyOf defaultRegistry cell) === oldSig (L.legacyBodyOf cell)
        ]

--------------------------------------------------------------------------------
-- 规则的实际输出

-- | 任意盘面：全部内置本体混在宝石里。
genAnyBoard :: Gen Board
genAnyBoard = boardFromRows <$> vectorOf boardSize (vectorOf boardSize genCell)

genPos :: Gen Pos
genPos = (,) <$> choose (0, boardSize - 1) <*> choose (0, boardSize - 1)

genNeighborPair :: Gen (Pos, Pos)
genNeighborPair = do
  (r, c) <- genPos
  (dr, dc) <- elements [(0, 1), (1, 0), (0, -1), (-1, 0)]
  let q = (r + dr, c + dc)
  pure (if fst q < 0 || snd q < 0 || fst q >= boardSize || snd q >= boardSize then ((r, c), (r - dr, c - dc)) else ((r, c), q))

-- | 规则在同一盘面 / 上下文上的实际输出（邻格 / 步末 / 成对交换 / 开启）。步末的种子 / 挖空在运行前后的盘面上各取一次
-- （倒计时的种子只在走到 0 之后出现）。
ruleOut :: Board -> [Pos] -> [Pos] -> [Pos] -> (Pos, Pos) -> Maybe AdjacentRule -> Maybe EndRule -> Maybe SwapRule -> Maybe OpenRule -> [String]
ruleOut b tru direct protect (p1, p2) adj end swp opn =
  [ show (fmap (\r -> let o = arRun r actx b in (aoBoard o, aoDead o, aoSit o)) adj)
  , show (fmap (\r -> let (eff, b') = erRun r ectx b in (eff, b', erSeeds r b, erSeeds r b', erHoles r b, erHoles r b')) end)
  , show (fmap (\r -> (srFires r b p1 p2, srSeeds r b p1 p2)) swp)
  , show (fmap (\r -> orOpen r b tru) opn)
  ]
  where
    actx = AdjCtx {acTrue = tru, acDirect = direct, acProtect = protect, acRecolor = isGemCell}
    ectx = EndCtx {ecAvoid = protect, ecWalls = direct, ecPushable = isGemCell}
    isGemCell c = case c of
      Gem {} -> True
      _ -> False

-- | 各元素的邻格 / 步末 / 成对交换 / 开启规则在随机盘面与随机上下文上的输出，与第 9 刀前逐项相同。
qc_caps_rules_match_legacy :: Property
qc_caps_rules_match_legacy =
  forAll genCell $ \cell ->
    forAll genAnyBoard $ \b ->
      forAll (nub <$> listOf genPos) $ \tru ->
        forAll (nub <$> listOf genPos) $ \direct ->
          forAll (nub <$> listOf genPos) $ \protect ->
            forAll genNeighborPair $ \pq ->
              let b' = setCell b (fst pq) cell
                  new = elementOf defaultRegistry cell
                  old = legacyElementOf cell
              in counterexample (show cell) $
                   ruleOut b' tru direct protect pq (adjacentRule new) (endRule new) (swapRule new) (openRule new)
                     === ruleOut b' tru direct protect pq (L.adjacentRule old) (L.endRule old) (L.swapRule old) (L.openRule old)

--------------------------------------------------------------------------------
-- 缺省能力

-- | 只声明原型、其余全用缺省能力的新写法元素。
data NewPlain = NewPlain Archetype Cell
  deriving (Eq, Show)

instance Element NewPlain where
  name _ = "plain"
  toCell (NewPlain _ c) = c
  caps (NewPlain a _) = capsOf a

-- | 旧写法里只覆盖 archetype 的同一元素（其余 24 项全是类的缺省方法）。
data OldPlain = OldPlain Archetype Cell
  deriving (Eq, Show)

instance LElement OldPlain where
  name _ = "plain"
  toCell (OldPlain _ c) = c
  archetype (OldPlain a _) = a

-- | 按原型的缺省能力（capsOf 三种原型、惰性占格 Inert）与第 9 刀前类的缺省方法逐项相同（任意写回格子）。
qc_default_caps_match_legacy_defaults :: Property
qc_default_caps_match_legacy_defaults =
  forAll genCell $ \cell ->
    forAll (elements [Piece, Blocker, Fixed]) $ \a ->
      counterexample (show (a, cell)) $
        conjoin
          [ newSig (SomeElement (NewPlain a cell)) === oldSig (LSome (OldPlain a cell))
          , newSig (SomeElement (Inert "inert" cell)) === oldSig (LSome (Inert "inert" cell))
          ]

--------------------------------------------------------------------------------
-- 类变薄 / 扩展元素

-- | 元素类只剩 name / toCell / caps 三个方法；内置本体的 instance 也只定义这三项。
caps_element_class_is_thin :: Assertion
caps_element_class_is_thin = do
  cls <- readCode "src/Match3/Element/Class.hs"
  let body = takeWhile indented (drop 1 (dropWhile (not . ("class (Show e, Eq e, Typeable e) => Element e where" `isPrefixOf`)) (lines cls)))
  assertEqual "class methods" ["name", "toCell", "caps"] [m | l <- body, (m, rest) <- [span isIdent (dropWhile isSpace l)], not (null m), "::" `isPrefixOf` dropWhile isSpace rest]
  srcs <- builtinSources
  insts <- concat <$> mapM (fmap instanceMethods . readCode) srcs
  assertEqual "builtin body instances" 24 (length insts)
  assertEqual "builtin instances only define name / toCell / caps" [] (filter (`notElem` ["name", "toCell", "caps"]) (nub (concat insts)))
  where
    indented l = case l of
      [] -> True
      ch : _ -> isSpace ch
    isIdent ch = isAlphaNum ch || ch == '_' || ch == '\''
    instanceMethods src = go (lines src)
      where
        go ls = case dropWhile (not . ("instance Element " `isPrefixOf`)) ls of
          [] -> []
          (_ : rest) ->
            let (blk, more) = span indented rest
            in [m | l <- blk, take 2 l == "  ", ch : _ <- [drop 2 l], not (isSpace ch), let m = takeWhile isIdent (drop 2 l), not (null m)] : go more

-- | 用 Caps 写的扩展元素「荆棘」：四行 instance（名字、写回格子、能力 = 占格障碍 + 受击掉耐久 + 计数），
-- customEntry 注册后不改主流程就能被锤子打两下碎掉、按名字计数。
newtype Thorn = Thorn Int
  deriving (Eq, Show)

instance Element Thorn where
  name _ = "thorn"
  toCell (Thorn n) = Custom "thorn" (CustomState n)
  caps (Thorn n) = blocker [hit (if n <= 1 then Destroy else Absorb (SomeElement (Thorn (n - 1)))), counts (CountNamed "thorn")]

ext_caps_element_plugs_in :: Assertion
ext_caps_element_plugs_in = do
  let reg = register (customEntry (Thorn 2) (Thorn . unCustomState)) defaultRegistry
      p = (3, 3)
      thorn = Custom "thorn" (CustomState 2)
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "thorn") 1)) 1) {gsBoard = setCell stableBoard p thorn, gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith reg p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
  assertEqual "decoded through the registry" (SomeElement (Thorn 2)) (bodyOf reg thorn)
  assertEqual "a blocker by default caps" (Blocker, True, Nothing) (archetype (bodyOf reg thorn), blocksSwapWith reg thorn, matchColorWith reg thorn)
  assertEqual "first hit chips it" (Custom "thorn" (CustomState 1)) (getCell (gsBoard gs1) p)
  assertBool "second hit breaks it" (not (isThorn (getCell (gsBoard gs2) p)))
  assertEqual "counted by name" [("thorn", 1)] (namedCounts (gsCounts gs2))
  assertEqual "unknown to the default registry" (Just (Inert "thorn" thorn)) (fromElement (bodyOf defaultRegistry thorn))
  where
    isThorn c = case c of
      Custom "thorn" _ -> True
      _ -> False
