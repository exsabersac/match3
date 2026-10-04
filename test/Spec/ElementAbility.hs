{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素能力类（元素类重构第 1 刀起）：新写法（Match3.Element.Ability / Kind / Layer / World）的
--
-- * 适配器对照：把内置的宝石 / 直线特效 / 石头 / 翻转块 / 气泡 / 雪怪 / 冰 / 巧克力 / 锁链 / 果冻换成新写法、
--   经适配器注册进内置注册表，元素对照快照（element-oracle.txt）逐行不变；
-- * 叠层合成：新写法的世界解码出的 'Layered' 元素与旧注册表的 Modified 在全部宝石 × 冰 × 叠层格上逐方法相等；
-- * 透明性：'SomeElement' 与 'Layered' 的转发 instance 按源码核对没有漏掉任何能力方法（漏了会静默退回默认方法），
--   且装箱前后 'abilityProbe' 逐项相等。
module Spec.ElementAbility
  ( tests
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isSpace)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified ElementOracle
import Match3.Board.Grid (getCell)
import Match3.Element.Ability
import Match3.Element.Adapter (groundKindEntry, kindEntry, layerEntry)
import Match3.Element.Builtin (Bubble(..), SnowBoss(..), defaultRegistry, specialBlast)
import Match3.Element.Builtin.Common (deadRule)
import Match3.Element.Builtin.Layer (ChainL(..), ChocoL(..), putOverlay)
import qualified Match3.Element.Class as C
import Match3.Element.Kind
import Match3.Element.Layer
import Match3.Element.Registry (Registry, elementOf, register)
import Match3.Element.Types
import Match3.Element.World
import Match3.Grass (clearChocoAdjacent)
import Match3.Obstacles (chipAdjacentStonesExcept)
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "ab_adapter_oracle_unchanged" ab_adapter_oracle_unchanged
  , testCase "ab_layered_matches_modified" ab_layered_matches_modified
  , testCase "ab_some_element_forwards_all_methods" ab_some_element_forwards_all_methods
  , testCase "ab_layered_composes_all_methods" ab_layered_composes_all_methods
  , testCase "ab_boxing_is_transparent" ab_boxing_is_transparent
  , testCase "ab_world_decode_order" ab_world_decode_order
  ]

--------------------------------------------------------------------------------
-- 新写法的样本元素（与内置同名同行为）

newtype GemV = GemV Color
  deriving (Eq, Show)

instance Cellular GemV where
  nameOf _ = "gem"
  toCell (GemV c) = Gem c Normal 0 Nothing

instance Matchable GemV
instance Hittable GemV
instance Movable GemV
instance Countable GemV
instance Renders GemV

instance Kind GemV where
  kindName _ = "gem"
  fromCell cell = case cell of
    Gem c Normal _ _ -> Just (GemV c)
    _ -> Nothing

newtype LineHV = LineHV Color
  deriving (Eq, Show)

instance Cellular LineHV where
  nameOf _ = "line_h"
  toCell (LineHV c) = Gem c LineH 0 Nothing

instance Matchable LineHV

instance Hittable LineHV where
  blast _ = specialBlast LineH

instance Movable LineHV where
  keepOnShuffle _ = True

instance Countable LineHV
instance Renders LineHV

instance Kind LineHV where
  kindName _ = "line_h"
  fromCell cell = case cell of
    Gem c LineH _ _ -> Just (LineHV c)
    _ -> Nothing
  place _ _ cell = case cell of
    Gem c _ _ _ -> Just (Gem c LineH 0 Nothing)
    _ -> Nothing

newtype StoneV = StoneV Int
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle StoneV)

instance Cellular StoneV where
  nameOf _ = "stone"
  toCell (StoneV n) = Stone n

instance Hittable StoneV where
  struck (StoneV n) = if n <= 1 then Destroy else Absorb (Stone (n - 1))
  fires _ = False

instance Countable StoneV where
  counter _ = Just CountStones

instance Renders StoneV

instance Kind StoneV where
  kindName _ = "stone"
  fromCell cell = case cell of
    Stone n -> Just (StoneV n)
    _ -> Nothing
  place _ args _ = Stone <$> exactArgs (max 1 <$> argInt <|> pure 1) args
  boardPasses _ = [AdjacentPass 10 (deadRule chipAdjacentStonesExcept)]

data FlipV = FlipV Color Color
  deriving (Eq, Show)

instance Cellular FlipV where
  nameOf _ = "flip"
  toCell (FlipV f b) = Flip f b

instance Matchable FlipV where
  color (FlipV f _) = Just f

instance Hittable FlipV where
  struck (FlipV _ b) = Absorb (Gem b Normal 0 Nothing)
  fires _ = False

instance Movable FlipV where
  keepOnShuffle _ = True

instance Countable FlipV
instance Renders FlipV

instance Kind FlipV where
  kindName _ = "flip"
  fromCell cell = case cell of
    Flip f b -> Just (FlipV f b)
    _ -> Nothing
  place _ args _ = exactArgs (Flip <$> argColor <*> argColor) args

newtype BubbleV = BubbleV Int
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle BubbleV)

instance Cellular BubbleV where
  nameOf _ = "bubble"

instance Hittable BubbleV where
  fires _ = False

instance Countable BubbleV where
  counter _ = Just (CountNamed "bubble")

instance Renders BubbleV

instance Kind BubbleV where
  kindName _ = "bubble"
  fromCell = fromCustom "bubble" BubbleV
  place _ = customPlace "bubble"
  label _ = Just "气泡"
  boardPasses _ = [AdjacentPass (arOrder r) (arRun r) | Just r <- [C.adjacentRule (Bubble 1)]]

data BossV = BossV Int Int Int Int
  deriving (Eq, Show)
  deriving (Movable) via (Fixed BossV)

instance Cellular BossV where
  nameOf _ = "snow_boss"
  toCell (BossV hp mx t q) = C.toCell (SnowBoss hp mx t q)

instance Matchable BossV where
  blocksSwap _ = True
  hintable _ = False

instance Hittable BossV where
  struck b = Absorb (toCell b)
  fires _ = False

instance Countable BossV where
  diffWeight (BossV hp _ _ q) = if q == 0 then hp else 0

instance Renders BossV where
  face (BossV hp mx t q) = [("q", FaceInt q), ("hurt", FaceBool (hp * 2 <= mx)), ("turn", FaceInt t), ("every", FaceInt 3)]

instance Kind BossV where
  kindName _ = "snow_boss"
  fromCell cell = case cell of
    Custom "snow_boss" (CustomState v) -> Just (BossV ((v `div` 16) `mod` 256) (v `div` 4096) ((v `div` 4) `mod` 4) (v `mod` 4))
    _ -> Nothing
  place _ args _ = case exactArgs ((,) <$> argInt <*> argInt) args of
    Just (hp, q) | hp > 0 && hp <= 255 && q >= 0 && q < 4 -> Just (toCell (BossV hp hp 0 q))
    _ -> Nothing
  label _ = Just "雪怪"
  loseHint _ = Just (\n -> "用身边的消除和特效打雪怪，目标 " ++ show n ++ " 点血")
  diffCounter _ = Just (CountNamed "snow_boss")
  boardPasses _ =
    [AdjacentPass (arOrder r) (arRun r) | Just r <- [C.adjacentRule proto]] ++ [EndPass r | Just r <- [C.endRule proto]]
    where
      proto = SnowBoss 1 1 0 0

newtype IceV = IceV Int
  deriving (Eq, Show)

instance Layer IceV where
  layerName _ = "ice"
  peel cell = case cell of
    Gem c k n ov | n > 0 -> Just (IceV n, Gem c k 0 ov)
    _ -> Nothing
  putOn (IceV n) cell = case cell of
    Gem c k _ ov -> Gem c k n ov
    _ -> cell
  layerFires (IceV n) = Just (n <= 1)
  layerHit (IceV n) = if n > 1 then Keep (IceV (n - 1)) else Shatter
  layerPlace _ args cell = case cell of
    Gem col kind _ ov -> (\n -> Gem col kind n ov) <$> exactArgs argInt args
    _ -> Nothing

data ChocoV = ChocoV
  deriving (Eq, Show)

instance Layer ChocoV where
  layerName _ = "choco"
  peel cell = case cell of
    Gem c k i (Just Choco) -> Just (ChocoV, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Choco
  layerStripsOnClear _ = True
  layerPlace _ _ cell = case cell of
    Gem col kind ice _ -> Just (Gem col kind ice (Just Choco))
    _ -> Nothing
  layerPasses _ =
    AdjacentPass 150 (\ctx b -> AdjOut (clearChocoAdjacent b (acTrue ctx)) [] [])
      : [EndPass r | Just r <- [C.modEnd ChocoL]]

newtype ChainV = ChainV Int
  deriving (Eq, Show)

instance Layer ChainV where
  layerName _ = "chain"
  peel cell = case cell of
    Gem c k i (Just (Chain n)) -> Just (ChainV n, Gem c k i Nothing)
    _ -> Nothing
  putOn (ChainV n) = putOverlay (Chain n)
  layerBlocksMatch _ = True
  layerBlocksSwap _ = True
  layerFires _ = Just False
  layerHit (ChainV n) = if n <= 1 then Peel else Keep (ChainV (n - 1))
  layerPlace _ args cell = case cell of
    Gem col kind ice _ -> (\n -> Gem col kind ice (Just (Chain n))) <$> exactArgs argInt args
    _ -> Nothing
  layerPasses _ = [AdjacentPass (arOrder r) (arRun r) | Just r <- [C.modAdjacent (ChainL 1)]]

data JellyV

instance GroundKind JellyV where
  groundName _ = "jelly"
  groundHit _ n = if n > 1 then Just (n - 1) else Nothing
  groundCounter _ = Just (CountNamed "jelly")
  groundLabel _ = Just "果冻"

-- | 内置注册表里这些条目换成新写法（经适配器；同名替换，注册位置不变）。
adapted :: Registry
adapted =
  foldl
    (flip register)
    defaultRegistry
    [ kindEntry (GemV C1)
    , kindEntry (LineHV C1)
    , kindEntry (StoneV 1)
    , kindEntry (FlipV C1 C2)
    , kindEntry (BubbleV 1)
    , kindEntry (BossV 1 1 0 0)
    , layerEntry (IceV 1)
    , layerEntry ChocoV
    , layerEntry (ChainV 1)
    , groundKindEntry @JellyV
    ]

ab_adapter_oracle_unchanged :: Assertion
ab_adapter_oracle_unchanged = do
  expected <- lines <$> readFile "test/golden/element-oracle.txt"
  let actual = ElementOracle.oracleLinesWith adapted
  case [(i, x, y) | (i, x, y) <- zip3 [1 :: Int ..] expected actual, x /= y] of
    ((i, x, y) : _) -> assertFailure ("line " ++ show i ++ " differs\nexpected: " ++ take 400 x ++ "\nactual:   " ++ take 400 y)
    [] -> assertEqual "line count" (length expected) (length actual)

--------------------------------------------------------------------------------
-- 叠层合成与旧 Modified 逐方法相等

world :: World
world = mkWorld [kindDef @GemV, kindDef @LineHV, layerDef @IceV, layerDef @ChocoV, layerDef @ChainV, kindDef @StoneV]

-- | 旧写法元素值上的同一组读数（对应 'abilityProbe' 的各项）。
oldProbe :: C.SomeElement -> [(String, String)]
oldProbe e =
  [ ("nameOf", show (C.name e))
  , ("toCell", show (C.toCell e))
  , ("color", show (C.color e))
  , ("blocksMatch", show (C.mcBlocksMatch (C.capMatch (C.caps e))))
  , ("blocksSwap", show (C.blocksSwap e))
  , ("hintable", show (C.hintable e))
  , ("struck", show (case C.onHit e of C.Absorb x -> Absorb (C.toCell x); C.Destroy -> Destroy; C.Immune -> Immune))
  , ("fires", show (fromMaybe False (C.activates e)))
  , ("blast", show (fmap (\f -> f probeBoard (3, 3)) (C.blast e)))
  , ("falls", show (C.falls e))
  , ("portal", show (C.portal e))
  , ("drains", show (C.drains e))
  , ("keepOnShuffle", show (C.keepOnShuffle e))
  , ("recolorable", show (C.recolorable e))
  , ("pushable", show (C.pushable e))
  , ("counter", show (C.counter e))
  , ("diffWeight", show (C.diffWeight e))
  , ("vacatesCarpet", show (C.vacatesCarpet e))
  , ("face", show (C.faceFields e))
  ]
  where
    probeBoard = gridFromRows (replicate 8 (replicate 8 (Gem C1 Normal 0 Nothing)))

layeredCells :: [Cell]
layeredCells =
  [Gem c k i ov | c <- [C1, C3], k <- [Normal, LineH], i <- [0 .. 3], ov <- [Nothing, Just Choco, Just (Chain 1), Just (Chain 2)]]
    ++ [Stone 1, Stone 3]

ab_layered_matches_modified :: Assertion
ab_layered_matches_modified =
  mapM_
    (\cell -> assertEqual (show cell) (oldProbe (elementOf defaultRegistry cell)) (abilityProbe (decode world cell)))
    layeredCells

--------------------------------------------------------------------------------
-- 透明性

-- | 源码里某个块（以给定前缀的行开头，到下一个顶格行为止）里各行开头的标识符。
blockNames :: (String -> Bool) -> String -> [[String]]
blockNames isHead src = go (lines src)
  where
    go ls = case break isHead ls of
      (_, []) -> []
      (_, _ : rest) ->
        let (body, rest') = span (\l -> case l of [] -> True; ch : _ -> isSpace ch) rest
        in [w | l <- body, "  " `isPrefixOf` l, not ("   " `isPrefixOf` l), not ("  --" `isPrefixOf` l), let w = takeWhile (\ch -> isAlphaNum ch || ch == '\'' || ch == '_') (drop 2 l), not (null w), w `notElem` ["default"]] : go rest'

uniq :: [String] -> [String]
uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

abilityClasses :: [String]
abilityClasses = ["Cellular", "Matchable", "Hittable", "Movable", "Countable", "Renders"]

-- | 每个能力类在类声明里的方法名。
classMethods :: String -> [(String, [String])]
classMethods src =
  [ (c, uniq (concat (blockNames (\l -> "class " `isPrefixOf` l && (" " ++ c ++ " e where") `isInfixOf` l) src)))
  | c <- abilityClasses
  ]

ab_some_element_forwards_all_methods :: Assertion
ab_some_element_forwards_all_methods = do
  src <- readFile "src/Match3/Element/Ability.hs"
  let methods = classMethods src
  assertEqual "six ability classes found" 6 (length (filter (not . null . snd) methods))
  mapM_
    ( \(c, ms) -> do
        let fwd = uniq (concat (blockNames (== ("instance " ++ c ++ " SomeElement where")) src))
        assertEqual ("SomeElement forwards every " ++ c ++ " method") (sort ms) (sort fwd)
        mapM_ (\m -> assertBool ("abilityProbe covers " ++ m) (("(\"" ++ m ++ "\",") `isInfixOf` src)) ms
    )
    methods

ab_layered_composes_all_methods :: Assertion
ab_layered_composes_all_methods = do
  src <- readFile "src/Match3/Element/Ability.hs"
  lsrc <- readFile "src/Match3/Element/Layer.hs"
  mapM_
    ( \(c, ms) -> do
        let composed = uniq (concat (blockNames (\l -> "instance " `isPrefixOf` l && (" => " ++ c ++ " (Layered l e) where") `isInfixOf` l) lsrc))
        assertEqual ("Layered composes every " ++ c ++ " method") (sort ms) (sort composed)
    )
    (classMethods src)

-- | 装箱前后、叠层装箱前后逐方法相等。
ab_boxing_is_transparent :: Assertion
ab_boxing_is_transparent = do
  let check :: Element e => String -> e -> Assertion
      check what e = assertEqual what (abilityProbe e) (abilityProbe (SomeElement e))
  check "gem" (GemV C2)
  check "line_h" (LineHV C4)
  check "stone" (StoneV 2)
  check "flip" (FlipV C1 C5)
  check "bubble" (BubbleV 3)
  check "boss" (BossV 3 6 1 0)
  check "inert" (Inert "x" (Custom "x" (CustomState 2)))
  check "iced choco gem" (Layered (IceV 2) (Layered ChocoV (GemV C1)))
  check "boxed inner" (Layered (ChainV 1) (SomeElement (LineHV C3)))
  -- 每个能力方法在这组样本上至少有一个不是缺省值（否则漏转发测不出来）
  let samples = [abilityProbe (StoneV 2), abilityProbe (FlipV C1 C5), abilityProbe (BossV 3 6 1 0), abilityProbe (LineHV C4), abilityProbe (Layered (IceV 2) (GemV C1)), abilityProbe (Layered (ChainV 1) (GemV C1))]
      defaults = abilityProbe (GemV C1)
      boring = [k | (k, v) <- defaults, k `notElem` ["nameOf", "toCell", "drains", "vacatesCarpet"], all (\s -> lookup k s == Just v) samples]
  assertEqual "every method varies in the samples" [] boring

-- | 解码：冰在外、叠层在内，剩下的交给本体；未注册的 Custom 名字 = 惰性占格；同名以后注册的为准。
ab_world_decode_order :: Assertion
ab_world_decode_order = do
  let cell = Gem C2 Normal 2 (Just (Chain 1))
  assertEqual "layers" ["ice", "chain"] (map layerValueName (fst (decodeLayers world cell)))
  assertEqual "inner" (Gem C2 Normal 0 Nothing) (snd (decodeLayers world cell))
  assertEqual "roundtrip" cell (toCell (decode world cell))
  assertEqual "unregistered custom" (Just (Inert "moss" (Custom "moss" (CustomState 1)))) (fromElement (decode world (Custom "moss" (CustomState 1))))
  assertEqual "unclaimed cell" "?" (nameOf (decode world Cookie))
  assertEqual "names in order" ["gem", "line_h", "ice", "choco", "chain", "stone"] (map defName (worldDefs world))
  let w2 = mkWorld [kindDef @StoneV, kindDef @GemV, kindDef @StoneV]
  assertEqual "dedupe keeps first position" ["stone", "gem"] (map defName (worldDefs w2))
  assertEqual "body" (Just (StoneV 2)) (fromElement (decode w2 (Stone 2)))
  assertEqual "mapMaybe sanity" [1 :: Int] (mapMaybe (\c -> case c of Stone n -> Just n; _ -> Nothing) [getCell (gridFromRows [[Stone 1]]) (0, 0)])
