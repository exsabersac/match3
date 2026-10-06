{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素值级行为（元素类重构第 1 刀起；slim-9 起只有 Match3.Element.Phase）：新写法（Phase / Kind / Layer / Registry）的
--
-- * 同名替换：把内置的宝石 / 直线特效 / 石头 / 翻转块 / 气泡 / 雪怪 / 冰 / 巧克力 / 锁链 / 果冻换成测试里独立写的
--   新写法副本、按同名注册进内置元素世界，元素对照快照（element-oracle.txt）逐行不变；
-- * 叠层合成：测试世界解码出的 'Layered' 元素与内置元素世界解码出的元素在全部宝石 × 冰 × 叠层格上逐方法相等；
-- * 名字一致：每个注册本体的 fromCell 认下的格子，解码值的 nameOf 都等于 kindName；
-- * 透明性：'Layered' 的 Phase instance 按源码核对覆盖了每个 Phase 方法（漏了会静默退回默认方法），
--   'phaseProbe' 读到每个方法，且装箱（'SomePhase'）前后逐项相等。
module Spec.ElementAbility
  ( tests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy(..))
import qualified ElementOracle
import Match3.Board.Grid (getCell)
import Match3.Element.Builtin (Bubble(..), Fuzzball(..), SnowBoss(..), defaultRegistry, snowBossEntity, specialBlast)
import Match3.Element.Builtin.Actor (BottleE, MagicHatE, MakerE)
import Match3.Element.Builtin.Collectible (CookieE(..), TimeSpiritE)
import Match3.Element.Builtin.Layer (ChainL, ChocoL, CurtainL, FogL, FreezeL, GrassL, SteamL, VineL, putOverlay)
import Match3.Element.Builtin.Obstacle (BalloonE, CakeE, ChestE, HoneyE, MagicStone, SafeE, StoneE, balloonPop, balloonPopLegacy)
import Match3.Element.Rules (kindRules, layerRules)
import Match3.Element.Kind
import Match3.Element.Phase
import Match3.Element.Near
import Match3.Element.Layer
import Match3.Element.Types
import Match3.ECS.Registry
import Match3.ECS.Stage (NearWorld(..), nearWorld)
import Match3.ECS.System (System(..))
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, forAll, testProperty, (===))
import Spec.Properties (genCell)
import Match3.View (cellFace)

tests :: [TestTree]
tests =
  [ testCase "ab_same_name_copies_oracle_unchanged" ab_same_name_copies_oracle_unchanged
  , testCase "ab_layered_matches_builtin" ab_layered_matches_builtin
  , testCase "ab_kind_name_matches_decoded" ab_kind_name_matches_decoded
  , testCase "ab_some_element_forwards_all_methods" ab_some_element_forwards_all_methods
  , testCase "ab_layered_composes_all_methods" ab_layered_composes_all_methods
  , testCase "ab_boxing_is_transparent" ab_boxing_is_transparent
  , testCase "ab_world_decode_order" ab_world_decode_order
  , testCase "ab_rule_methods_pinned" ab_rule_methods_pinned
  , testCase "ab_near_escape_absorbed" ab_near_escape_absorbed
  , testCase "ab_cell_face_matches_legacy_zoo" ab_cell_face_matches_legacy_zoo
  , testProperty "qc_cell_face_matches_legacy" qc_cell_face_matches_legacy
  ]

--------------------------------------------------------------------------------
-- 新写法的样本元素（与内置同名同行为）

newtype GemV = GemV Color
  deriving (Eq, Show)

instance Phase GemV where
  codec = Codec
    { cName = "gem"
    , cToCell = \(GemV c) -> Gem c Normal 0 Nothing
    , cFromCell = \cell -> case cell of
        Gem c Normal _ _ -> Just (GemV c)
        _ -> Nothing
    , cPlace = \_ _ -> Nothing
    , cMeta = emptyMeta
    , cNear = Nothing
    , cHud = noHud
    , cSystems = []
    }
  onMatch (GemV c) = gemMatch (Just c)
  onHit _ _ = gemHit
  physics _ = gemPhysics
  view _ = noFace

newtype LineHV = LineHV Color
  deriving (Eq, Show)

instance Phase LineHV where
  codec = Codec
    { cName = "line_h"
    , cToCell = \(LineHV c) -> Gem c LineH 0 Nothing
    , cFromCell = \cell -> case cell of
        Gem c LineH _ _ -> Just (LineHV c)
        _ -> Nothing
    , cPlace = \_ cell -> case cell of
        Gem c _ _ _ -> Just (Gem c LineH 0 Nothing)
        _ -> Nothing
    , cMeta = emptyMeta
    , cNear = Nothing
    , cHud = noHud
    , cSystems = []
    }
  onMatch (LineHV c) = gemMatch (Just c)
  onHit _ _ = gemHit { hBlast = specialBlast LineH }
  physics _ = gemPhysics { pKeepShuffle = True }
  view _ = noFace

newtype StoneV = StoneV Int
  deriving (Eq, Show)

instance Phase StoneV where
  codec = Codec
    { cName = "stone"
    , cToCell = \(StoneV n) -> Stone n
    , cFromCell = \cell -> case cell of
        Stone n -> Just (StoneV n)
        _ -> Nothing
    , cPlace = \args _ -> Stone <$> exactArgs (max 1 <$> argInt <|> pure 1) args
    , cMeta = emptyMeta { metaCounter = Just CountStones }
    , cNear = Just (NearRule 10 SkipDirect DiePrepend)
    , cHud = noHud
    , cSystems = []
    }
  onNear _ _ (StoneV n) = NearNudge (if n <= 1 then Dies else Becomes (Stone (n - 1)))
  onMatch _ = obstacleMatch
  onHit _ (StoneV n) = HitOut (if n <= 1 then Destroy else Absorb (Stone (n - 1))) False Nothing Nothing
  physics _ = obstaclePhysics
  view (StoneV n) = baseFace "stone" [("n", FieldInt n)]

data FlipV = FlipV Color Color
  deriving (Eq, Show)

instance Phase FlipV where
  codec = Codec
    { cName = "flip"
    , cToCell = \(FlipV f b) -> Flip f b
    , cFromCell = \cell -> case cell of
        Flip f b -> Just (FlipV f b)
        _ -> Nothing
    , cPlace = \args _ -> exactArgs (Flip <$> argColor <*> argColor) args
    , cMeta = emptyMeta
    , cNear = Nothing
    , cHud = noHud
    , cSystems = []
    }
  onMatch (FlipV f _) = gemMatch (Just f)
  onHit _ (FlipV _ b) = HitOut (Absorb (Gem b Normal 0 Nothing)) False Nothing Nothing
  physics _ = gemPhysics { pKeepShuffle = True }
  view _ = noFace

newtype BubbleV = BubbleV Int
  deriving (Eq, Show)

instance Phase BubbleV where
  codec = Codec
    { cName = "bubble"
    , cToCell = intCell "bubble"
    , cFromCell = fromCustom "bubble" BubbleV
    , cPlace = customPlace "bubble"
    , cMeta = emptyMeta { metaCounter = Just (CountNamed "bubble") }
    , cNear = Nothing
    , cHud = noHud { hudLabel = Just "气泡" }
    , cSystems = boardSystems (Proxy :: Proxy Bubble)
    }
  onMatch _ = obstacleMatch
  onHit _ _ = HitOut Destroy False Nothing Nothing
  physics _ = obstaclePhysics
  view _ = noFace

data BossV = BossV Int Int Int Int
  deriving (Eq, Show)

instance Phase BossV where
  codec = Codec
    { cName = "snow_boss"
    , cToCell = \(BossV hp mx t q) -> toCell (SnowBoss hp mx t q)
    , cFromCell = \cell -> case cell of
        Custom "snow_boss" (CustomState v) -> Just (BossV ((v `div` 16) `mod` 256) (v `div` 4096) ((v `div` 4) `mod` 4) (v `mod` 4))
        _ -> Nothing
    , cPlace = \args _ -> case exactArgs ((,) <$> argInt <*> argInt) args of
        Just (hp, q) | hp > 0 && hp <= 255 && q >= 0 && q < 4 -> Just (toCell (BossV hp hp 0 q))
        _ -> Nothing
    , cMeta = emptyMeta { metaDiffCounter = Just (CountNamed "snow_boss") }
    , cNear = Nothing
      -- HUD（中文名 / 失败提示 / 血条）与规则（扣血 + 步末移动）照抄雪怪
    , cHud = cHud (codec @SnowBoss)
    , cSystems = cSystems (codec @SnowBoss)
    }
  onMatch _ = MatchRule Nothing False True False
  onHit _ b = HitOut (Absorb (toCell b)) False Nothing Nothing
  physics _ = fixedPhysics
  liveMeta (BossV hp _ _ q) = (cMeta (codec @BossV)) { metaDiffWeight = if q == 0 then hp else 0 }
  view (BossV hp mx t q) = noFace { fExtras = [("q", FaceInt q), ("hurt", FaceBool (hp * 2 <= mx)), ("turn", FaceInt t), ("every", FaceInt 3)] }

newtype IceV = IceV Int
  deriving (Eq, Show)

instance Layer IceV where
  peel cell = case cell of
    Gem c k n ov | n > 0 -> Just (IceV n, Gem c k 0 ov)
    _ -> Nothing
  putOn (IceV n) cell = case cell of
    Gem c k _ ov -> Gem c k n ov
    _ -> cell
  layerCover = (defaultCover "ice")
    { lcFires = \(IceV n) -> Just (n <= 1)
    , lcHit = \(IceV n) -> if n > 1 then Keep (IceV (n - 1)) else Shatter
    , lcPlace = \args cell -> case cell of
        Gem col kind _ ov -> (\n -> Gem col kind n ov) <$> exactArgs argInt args
        _ -> Nothing
    }

data ChocoV = ChocoV
  deriving (Eq, Show)

instance Layer ChocoV where
  peel cell = case cell of
    Gem c k i (Just Choco) -> Just (ChocoV, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Choco
  layerCover = (defaultCover "choco")
    { lcStripsOnClear = const True
    , lcPlace = \_ cell -> case cell of
        Gem col kind ice _ -> Just (Gem col kind ice (Just Choco))
        _ -> Nothing
    , lcNearPrio = Just 150
    , lcReach = AllNeighbours
    , lcOnNear = \_ cell -> case cell of
        Gem c k i _ -> Becomes (Gem c k i Nothing)
        _ -> Untouched
    , lcSpreads = Just (20, ChocoV)
    }

newtype ChainV = ChainV Int
  deriving (Eq, Show)

instance Layer ChainV where
  peel cell = case cell of
    Gem c k i (Just (Chain n)) -> Just (ChainV n, Gem c k i Nothing)
    _ -> Nothing
  putOn (ChainV n) = putOverlay (Chain n)
  layerCover = (defaultCover "chain")
    { lcBlocksMatch = const True
    , lcBlocksSwap = const True
    , lcFires = const (Just False)
    , lcHit = \(ChainV n) -> if n <= 1 then Peel else Keep (ChainV (n - 1))
    , lcPlace = \args cell -> case cell of
        Gem col kind ice _ -> (\n -> Gem col kind ice (Just (Chain n))) <$> exactArgs argInt args
        _ -> Nothing
    , lcNearPrio = Just 80
    , lcOnNear = \(ChainV n) cell -> case cell of
        Gem c k i _ | n <= 1 -> Becomes (Gem c k i Nothing)
        _ -> Becomes (putOn (ChainV (n - 1)) cell)
    }

jellyV :: GroundKind
jellyV = (groundKind "jelly")
  { groundHit = \n -> if n > 1 then Just (n - 1) else Nothing
  , groundCounter = Just (CountNamed "jelly")
  , groundLabel = Just "果冻"
  }

-- | 内置元素世界里这些条目换成测试里的副本（同名替换，注册位置不变）。
replaced :: Registry
replaced =
  foldl
    (flip register)
    defaultRegistry
    [ kindDef @GemV
    , kindDef @LineHV
    , kindDef @StoneV
    , kindDef @FlipV
    , kindDef @BubbleV
    , kindDef @BossV
    , layerDef @IceV
    , layerDef @ChocoV
    , layerDef @ChainV
    , groundDef jellyV
    ]

ab_same_name_copies_oracle_unchanged :: Assertion
ab_same_name_copies_oracle_unchanged = do
  expected <- lines <$> readFile "test/golden/element-oracle.txt"
  let actual = ElementOracle.oracleLinesWith replaced
  assertEqual "names unchanged" (map defName (registryDefs defaultRegistry)) (map defName (registryDefs replaced))
  case [(i, x, y) | (i, x, y) <- zip3 [1 :: Int ..] expected actual, x /= y] of
    ((i, x, y) : _) -> assertFailure ("line " ++ show i ++ " differs\nexpected: " ++ take 400 x ++ "\nactual:   " ++ take 400 y)
    [] -> assertEqual "line count" (length expected) (length actual)

--------------------------------------------------------------------------------
-- 叠层合成与内置元素世界逐方法相等

world :: Registry
world = mkRegistry [kindDef @GemV, kindDef @LineHV, layerDef @IceV, layerDef @ChocoV, layerDef @ChainV, kindDef @StoneV]

layeredCells :: [Cell]
layeredCells =
  [Gem c k i ov | c <- [C1, C3], k <- [Normal, LineH], i <- [0 .. 3], ov <- [Nothing, Just Choco, Just (Chain 1), Just (Chain 2)]]
    ++ [Stone 1, Stone 3]

ab_layered_matches_builtin :: Assertion
ab_layered_matches_builtin =
  mapM_
    (\cell -> assertEqual (show cell) (somePhaseProbe (elementOf defaultRegistry cell)) (somePhaseProbe (decode world cell)))
    layeredCells

--------------------------------------------------------------------------------
-- 名字一致

-- | 样本格：每个注册项按对照快照的参数表放到几种底格上的结果（覆盖全部内置本体）。
sampleCells :: [Cell]
sampleCells =
  [ c
  | d <- registryDefs defaultRegistry
  , base <- [Gem C1 Normal 0 Nothing, Gem C3 LineV 2 Nothing, Stone 2]
  , args <- [[], [AInt 1], [AInt 2], [AInt 4], [AColor C2], [AColor C1, AColor C4], [AColor C3, AInt 5], [AInt 1, AInt 0], [AInt 5, AInt 2], [AInt 2, AColor C1]]
  , Just c <- [defPlace d args base]
  ]

ab_kind_name_matches_decoded :: Assertion
ab_kind_name_matches_decoded = do
  let kinds = [k | KindDef k <- registryDefs defaultRegistry]
      accepted (SomeKind p) = [(kindName p, nameOf e) | c <- sampleCells, Just e <- [fromCellAs p c]]
  assertEqual "all builtin kinds" 25 (length kinds)
  mapM_
    ( \k@(SomeKind p) -> do
        let got = accepted k
        assertBool ("some sample decodes as " ++ show (kindName p)) (not (null got))
        mapM_ (\(kn, en) -> assertEqual "nameOf decoded == kindName" kn en) got
    )
    kinds

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

-- | Phase 类声明里的方法名。
phaseMethods :: String -> [String]
phaseMethods src = uniq (concat (blockNames (\l -> "class " `isPrefixOf` l && ") => Phase e where" `isInfixOf` l) src))

-- | 'phaseProbe' 读到每个值级 Phase 方法（漏读的方法透明性测不出来；codec 是类型级，onNear 由规则快照锁定）。
ab_some_element_forwards_all_methods :: Assertion
ab_some_element_forwards_all_methods = do
  src <- readFile "src/Match3/Element/Phase.hs"
  let ms = phaseMethods src
      probeSrc = unlines (takeWhile (not . ("somePhaseProbe ::" `isPrefixOf`)) (dropWhile (not . ("phaseProbe ::" `isPrefixOf`)) (lines src)))
  assertBool "Phase methods found" (length ms >= 6)
  mapM_ (\m -> assertBool ("phaseProbe reads " ++ m) (m `elem` ["codec", "onNear"] || (m ++ " ") `isInfixOf` probeSrc)) ms

ab_layered_composes_all_methods :: Assertion
ab_layered_composes_all_methods = do
  src <- readFile "src/Match3/Element/Phase.hs"
  lsrc <- readFile "src/Match3/Element/Layer.hs"
  let composed = uniq (concat (blockNames (\l -> "instance " `isPrefixOf` l && "=> Phase (Layered l e) where" `isInfixOf` l) lsrc))
  assertEqual "Layered composes every Phase method" (sort (phaseMethods src)) (sort composed)

-- | 装箱前后、叠层装箱前后逐方法相等。
ab_boxing_is_transparent :: Assertion
ab_boxing_is_transparent = do
  let check :: Phase e => String -> e -> Assertion
      check what e = assertEqual what (phaseProbe e) (somePhaseProbe (SomePhase e))
  check "gem" (GemV C2)
  check "line_h" (LineHV C4)
  check "stone" (StoneV 2)
  check "flip" (FlipV C1 C5)
  check "bubble" (BubbleV 3)
  check "boss" (BossV 3 6 1 0)
  check "inert" (Inert "x" (Custom "x" (CustomState 2)))
  check "iced choco gem" (Layered (IceV 2) (Layered ChocoV (GemV C1)))
  check "chained line" (Layered (ChainV 1) (LineHV C3))
  check "cookie" CookieE
  -- 每个读数在这组样本上至少有一个不是缺省值（否则漏读测不出来）
  let samples = [phaseProbe (StoneV 2), phaseProbe (FlipV C1 C5), phaseProbe (BossV 3 6 1 0), phaseProbe (LineHV C4), phaseProbe (Layered (IceV 2) (GemV C1)), phaseProbe (Layered (ChainV 1) (GemV C1)), phaseProbe CookieE]
      defaults = phaseProbe (GemV C1)
      boring = [k | (k, v) <- defaults, k `notElem` ["name", "toCell"], all (\s -> lookup k s == Just v) samples]
  assertEqual "every reading varies in the samples" [] boring

-- | 解码：冰在外、叠层在内，剩下的交给本体；未注册的 Custom 名字 = 惰性占格；同名以后注册的为准。
ab_world_decode_order :: Assertion
ab_world_decode_order = do
  let cell = Gem C2 Normal 2 (Just (Chain 1))
  assertEqual "layers" ["ice", "chain"] (map layerValueName (fst (decodeLayers world cell)))
  assertEqual "inner" (Gem C2 Normal 0 Nothing) (snd (decodeLayers world cell))
  assertEqual "roundtrip" cell (phaseToCell (decode world cell))
  assertEqual "unregistered custom" (Just (Inert "moss" (Custom "moss" (CustomState 1)))) (fromPhase (decode world (Custom "moss" (CustomState 1))))
  assertEqual "unclaimed cell" "?" (phaseName (decode world Cookie))
  assertEqual "names in order" ["gem", "line_h", "ice", "choco", "chain", "stone"] (map defName (registryDefs world))
  let w2 = mkRegistry [kindDef @StoneV, kindDef @GemV, kindDef @StoneV]
  assertEqual "dedupe keeps first position" ["stone", "gem"] (map defName (registryDefs w2))
  assertEqual "body" (Just (StoneV 2)) (fromPhase (decode w2 (Stone 2)))
  assertEqual "mapMaybe sanity" [1 :: Int] (mapMaybe (\c -> case c of Stone n -> Just n; _ -> Nothing) [getCell (gridFromRows [[Stone 1]]) (0, 0)])

--------------------------------------------------------------------------------
-- 规则方法（第 3 刀）：优先级 / 波及范围 / 蔓延写死（与旧 R 行的次序一致；元素对照快照另有整盘锁定）

-- | slim-1：气球 onNear+DieAppend 与旧 balloonPopLegacy 一致；气泡/毛球因 foldr 去重序留逃生口。
ab_near_escape_absorbed :: Assertion
ab_near_escape_absorbed = do
  let b = boardFromRows
        [ [Gem C1 Normal 0 Nothing, Balloon C1, Gem C2 Normal 0 Nothing, Flip C1 C2, Countdown C1 2]
        , [Balloon C2, Gem C1 Normal 0 Nothing, Balloon C1, Gem C3 Normal 0 Nothing, Gem C1 Normal 0 Nothing]
        , [Gem C4 Normal 0 Nothing, Balloon C1, Gem C1 Normal 0 Nothing, Balloon C2, Balloon C1]
        ]
      clears = [(0, 0), (0, 3), (0, 4), (1, 1), (2, 2)]
      ctx = nearWorld (const True) clears [] [] b
      outB = runSystem balloonPop ctx
      legB = runSystem balloonPopLegacy ctx
  assertEqual "balloon board" (nwBoard outB) (nwBoard legB)
  assertEqual "balloon dead" (nwDead outB) (nwDead legB)
  assertEqual "bubble still hatch" [170] [o | SysNear o _ <- boardSystems (Proxy @Bubble)]
  assertEqual "fuzzball still hatch" [190] [o | SysNear o _ <- boardSystems (Proxy @Fuzzball)]

ab_rule_methods_pinned :: Assertion
ab_rule_methods_pinned = do
  assertEqual "kind neighbourPrio"
    [Just 10, Just 20, Just 30, Just 40, Just 50, Just 60, Just 110, Just 120, Just 130, Just 140, Nothing, Just 180, Nothing, Nothing]
    [ phaseNearPrio @StoneE, phaseNearPrio @ChestE, phaseNearPrio @HoneyE
    , phaseNearPrio @CakeE, phaseNearPrio @BalloonE, phaseNearPrio @MagicHatE
    , phaseNearPrio @SafeE, phaseNearPrio @TimeSpiritE, phaseNearPrio @MakerE
    , phaseNearPrio @BottleE, phaseNearPrio @Bubble, phaseNearPrio @MagicStone
    , phaseNearPrio @Fuzzball, phaseNearPrio @SnowBoss ]
  assertEqual "layer neighbourPrio"
    [Just 70, Just 80, Just 90, Just 100, Just 150, Just 160, Nothing, Nothing]
    [ layerNeighbourPrio (Proxy @FogL), layerNeighbourPrio (Proxy @ChainL), layerNeighbourPrio (Proxy @FreezeL)
    , layerNeighbourPrio (Proxy @CurtainL), layerNeighbourPrio (Proxy @ChocoL), layerNeighbourPrio (Proxy @SteamL)
    , layerNeighbourPrio (Proxy @VineL), layerNeighbourPrio (Proxy @GrassL) ]
  assertEqual "layer reach"
    [SkipDirect, SkipDirect, SkipDirect, SkipDirect, AllNeighbours, AllNeighbours]
    [ layerReach (Proxy @FogL), layerReach (Proxy @ChainL), layerReach (Proxy @FreezeL)
    , layerReach (Proxy @CurtainL), layerReach (Proxy @ChocoL), layerReach (Proxy @SteamL) ]
  assertEqual "spreads" [Just 10, Just 20, Just 30, Nothing]
    [fst <$> spreads (Proxy @VineL), fst <$> spreads (Proxy @ChocoL), fst <$> spreads (Proxy @SteamL), fst <$> spreads (Proxy @FogL)]
  -- 收集：方法给出的规则在逃生口之前
  assertEqual "kindRules stone" [10] [o | SysNear o _ <- kindRules (Proxy @StoneE)]
  assertEqual "layerRules choco" (1, 1) (length [() | SysNear 150 _ <- layerRules (Proxy @ChocoL)], length [() | SysEnd _ <- layerRules (Proxy @ChocoL)])
  assertEqual "snow boss footprint" [(2, 3), (2, 4), (3, 3), (3, 4)] (footprint snowBossEntity (2, 3))
  assertEqual "snow boss rules" [200] [o | SysNear o _ <- kindRules (Proxy @SnowBoss)]

--------------------------------------------------------------------------------
-- 前端格子描述（第 6 刀）：cellFace 由 Phase view 的 fBase 驱动，与第 6 刀前按构造器写死的 case 逐字段相同

-- | 第 6 刀前 Match3.View.cellFace 的逐字副本。
legacyCellFace :: Cell -> (String, [(String, CellField)])
legacyCellFace cell = case cell of
  Gem c k ice ov ->
    ( "G"
    , [ col c, ("k", FieldText (kindCodeL k)), ("i", FieldInt ice)
      , ("o", maybe FieldNull (FieldText . overlayNameL) ov), n (maybe 0 overlayLayersL ov) ] )
  Stone k -> ("stone", [n k])
  Chest k -> ("chest", [n k])
  Honey k -> ("honey", [n k])
  Balloon c -> ("balloon", [col c])
  Cookie -> ("cookie", [])
  Cake k -> ("cake", [n k])
  MagicHat -> ("hat", [])
  Maker c k -> ("maker", [col c, n k])
  Snail dr dc -> ("snail", [("dr", FieldInt dr), ("dc", FieldInt dc)])
  Safe k -> ("safe", [n k])
  Flip f b -> ("flip", [col f, ("b", FieldInt (fromEnum b + 1))])
  Surprise -> ("surprise", [])
  Bottle c -> ("bottle", [col c])
  TimeSpirit -> ("spirit", [])
  Countdown c k -> ("countdown", [col c, n k])
  Custom name v -> ("custom", [("name", FieldText (unElementName name)), ("v", FieldInt (unCustomState v))])
  where
    n k = ("n", FieldInt k)
    col c = ("c", FieldInt (fromEnum c + 1))
    kindCodeL k = case k of
      Normal -> "N"
      LineH -> "H"
      LineV -> "V"
      Bomb -> "B"
      Rainbow -> "R"
    overlayNameL ov = case ov of
      Grass -> "grass"
      Vine -> "vine"
      Choco -> "choco"
      Fog _ -> "fog"
      Chain _ -> "chain"
      Freeze _ -> "freeze"
      Curtain _ -> "curtain"
      Steam -> "steam"
    overlayLayersL ov = case ov of
      Fog k -> k
      Chain k -> k
      Freeze k -> k
      Curtain k -> k
      _ -> 0

ab_cell_face_matches_legacy_zoo :: Assertion
ab_cell_face_matches_legacy_zoo =
  forM_ zoo $ \cell -> assertEqual (show cell) (legacyCellFace cell) (cellFace cell)
  where
    customs = [Custom (defName d) (CustomState v) | d <- registryDefs defaultRegistry, v <- [0, 1, 3, 17]] ++ [Custom "nobody" (CustomState 2)]
    zoo =
      customs
        ++ [Stone 2, Chest 1, Honey 3, Balloon C4, Cookie, Cake 2, MagicHat, Maker C3 2, Snail 0 (-1), Safe 1, Flip C2 C5, Surprise, Bottle C1, TimeSpirit, Countdown C5 4]
        ++ [Gem c k i ov | c <- [C1, C5], k <- [Normal, Bomb, Rainbow], i <- [0, 2], ov <- [Nothing, Just Grass, Just (Fog 2), Just Steam]]

qc_cell_face_matches_legacy :: Property
qc_cell_face_matches_legacy = forAll genCell $ \cell -> cellFace cell === legacyCellFace cell
