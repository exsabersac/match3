{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | 原型与组件（ecs-3 起取代 Phase 类）：
--
-- * 同名替换：把内置的宝石 / 直线特效 / 石头 / 翻转块 / 气泡 / 雪怪 / 冰 / 巧克力 / 锁链 / 果冻换成测试里独立写的
--   原型副本、按同名注册进内置注册表，元素对照快照（element-oracle.txt）逐行不变；
-- * 叠层合成：测试注册表与内置注册表的整格组件（'wholeMatch' / 'wholeHit' / 'wholePhysics' + 本体组件）在全部
--   宝石 × 冰 × 叠层格上逐项相等；
-- * 名字一致：每个注册原型的存储列认下的格子，经注册表解码的行名都等于原型名、状态往返不变；
-- * 透明性：'rowProbe' 按源码核对读到每种 'Component'，整格合成按源码核对读到每个叠层查询；装箱（'Row'）不改读数。
module Spec.ElementAbility
  ( tests
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, mapMaybe)
import qualified ElementOracle
import Match3.Board.Grid (getCell)
import Match3.ECS.Archetype
import Match3.ECS.Component
import Match3.Element.Builtin
  ( SnowBoss(..), bubbleArch, defaultRegistry, fuzzballArch, snowBossArch, snowBossEntity, specialBlast
  , stoneArch, chestArch, honeyArch, cakeArch, balloonArch, magicHatArch, safeArch, timeSpiritArch, makerArch
  , bottleArch, magicStoneArch, cookieArch )
import Match3.ECS.Cover
import Match3.Element.Builtin.Layer (chainCover, chocoCover, curtainCover, fogCover, freezeCover, grassCover, steamCover, vineCover)
import Match3.Element.Builtin.Obstacle (balloonPop, balloonPopLegacy)
import Match3.Element.Rules (coverNear, coverSpread, nearBy)
import Match3.Element.Kind
import Match3.Element.Types
import Match3.ECS.Registry
import Match3.ECS.Stage (EndSys(..), NearWorld(..), SysDef(..), nearWorld, spreadSys)
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
  , testCase "ab_row_probe_reads_all_components" ab_row_probe_reads_all_components
  , testCase "ab_whole_reads_every_layer_query" ab_whole_reads_every_layer_query
  , testCase "ab_row_is_transparent" ab_row_is_transparent
  , testCase "ab_world_decode_order" ab_world_decode_order
  , testCase "ab_rule_methods_pinned" ab_rule_methods_pinned
  , testCase "ab_cover_column_law" ab_cover_column_law
  , testCase "ab_near_escape_absorbed" ab_near_escape_absorbed
  , testCase "ab_cell_face_matches_legacy_zoo" ab_cell_face_matches_legacy_zoo
  , testProperty "qc_cell_face_matches_legacy" qc_cell_face_matches_legacy
  ]

--------------------------------------------------------------------------------
-- 原型副本（与内置同名同行为）

gemV :: Archetype Color
gemV = (archetype "gem" (Column get (\c -> Gem c Normal 0 Nothing)))
  { aMatch = gemMatch . Just
  , aHit = const gemHit
  , aPhysics = const gemPhysics
  }
  where
    get cell = case cell of
      Gem c Normal _ _ -> Just c
      _ -> Nothing

lineHV :: Archetype Color
lineHV = (archetype "line_h" (Column get (\c -> Gem c LineH 0 Nothing)))
  { aSpawn = \_ cell -> case cell of
      Gem c _ _ _ -> Just (Gem c LineH 0 Nothing)
      _ -> Nothing
  , aMatch = gemMatch . Just
  , aHit = const gemHit {hBlast = specialBlast LineH}
  , aPhysics = const gemPhysics {pKeepShuffle = True}
  }
  where
    get cell = case cell of
      Gem c LineH _ _ -> Just c
      _ -> Nothing

stoneV :: Archetype Int
stoneV = (archetype "stone" col)
  { aSpawn = \args _ -> Stone <$> exactArgs (max 1 <$> argInt <|> pure 1) args
  , aTally = const emptyTally {tCounter = Just CountStones}
  , aHit = \n -> if n <= 1 then breakHit else absorbHit (Stone (n - 1))
  , aFace = \n -> baseFace "stone" [("n", FieldInt n)]
  , aSystems = [SysNear 10 (nearBy col SkipDirect DiePrepend (\_ n -> NearNudge (if n <= 1 then Dies else Becomes (Stone (n - 1)))))]
  }
  where
    col = Column (\cell -> case cell of Stone n -> Just n; _ -> Nothing) Stone

flipV :: Archetype (Color, Color)
flipV = (archetype "flip" (Column get (uncurry Flip)))
  { aSpawn = \args _ -> exactArgs (Flip <$> argColor <*> argColor) args
  , aMatch = \(f, _) -> gemMatch (Just f)
  , aHit = \(_, b) -> absorbHit (Gem b Normal 0 Nothing)
  , aPhysics = const gemPhysics {pKeepShuffle = True}
  }
  where
    get cell = case cell of
      Flip f b -> Just (f, b)
      _ -> Nothing

bubbleV :: Archetype Int
bubbleV = (archetype "bubble" (customColumn "bubble"))
  { aSpawn = customPlace "bubble"
  , aTally = const emptyTally {tCounter = Just (CountNamed "bubble")}
  , aHit = const breakHit
  , aHud = noHud {hudLabel = Just "气泡"}
  , aSystems = aSystems bubbleArch
  }

-- | 雪怪副本：状态是 (血量, 满血, 计数, 象限)，自己解码；HUD 与 system（扣血 + 步末召唤）照抄雪怪。
bossV :: Archetype (Int, Int, Int, Int)
bossV = (archetype "snow_boss" (Column get put))
  { aSpawn = \args _ -> case exactArgs ((,) <$> argInt <*> argInt) args of
      Just (hp, q) | hp > 0 && hp <= 255 && q >= 0 && q < 4 -> Just (put (hp, hp, 0, q))
      _ -> Nothing
  , aMatch = const (Match Nothing False True False)
  , aHit = absorbHit . put
  , aPhysics = const fixedPhysics
  , aTally = \(hp, _, _, q) -> emptyTally {tDiffWeight = if q == 0 then hp else 0}
  , aDiff = Just (DiffCount (CountNamed "snow_boss") 0)
  , aHud = aHud snowBossArch
  , aFace = \(hp, mx, t, q) -> noFace {fExtras = [("q", FaceInt q), ("hurt", FaceBool (hp * 2 <= mx)), ("turn", FaceInt t), ("every", FaceInt 3)]}
  , aSystems = aSystems snowBossArch
  }
  where
    get cell = case cell of
      Custom "snow_boss" (CustomState v) -> Just ((v `div` 16) `mod` 256, v `div` 4096, (v `div` 4) `mod` 4, v `mod` 4)
      _ -> Nothing
    put (hp, mx, t, q) = colPut (aColumn snowBossArch) (SnowBoss hp mx t q)

iceV :: Cover Int
iceV = (mkCover "ice" (CoverColumn peel put))
  { cvShield = \n -> openShield {sFires = Just (n <= 1), sHit = if n > 1 then Keep (n - 1) else Shatter}
  , cvSpawn = \args cell -> case cell of
      Gem col kind _ ov -> (\n -> Gem col kind n ov) <$> exactArgs argInt args
      _ -> Nothing
  }
  where
    peel cell = case cell of
      Gem c k n ov | n > 0 -> Just (n, Gem c k 0 ov)
      _ -> Nothing
    put n cell = case cell of
      Gem c k _ ov -> Gem c k n ov
      _ -> cell

chocoV :: Cover ()
chocoV = (mkCover "choco" (CoverColumn peel (const (putOverlay Choco))))
  { cvShield = const openShield {sStrips = True}
  , cvSpawn = \_ cell -> case cell of
      Gem col kind ice _ -> Just (Gem col kind ice (Just Choco))
      _ -> Nothing
  , cvSystems =
      [ SysNear 150 (coverNear chocoV AllNeighbours (\_ cell -> case cell of
          Gem c k i _ -> Becomes (Gem c k i Nothing)
          _ -> Untouched))
      , SysEnd (spreadSys 20 (coverSpread chocoV ()))
      ]
  }
  where
    peel cell = case cell of
      Gem c k i (Just Choco) -> Just ((), Gem c k i Nothing)
      _ -> Nothing

chainV :: Cover Int
chainV = (mkCover "chain" col)
  { cvShield = \n -> openShield {sBlocksMatch = True, sBlocksSwap = True, sFires = Just False, sHit = if n <= 1 then Peel else Keep (n - 1)}
  , cvSpawn = \args cell -> case cell of
      Gem col' kind ice _ -> (\n -> Gem col' kind ice (Just (Chain n))) <$> exactArgs argInt args
      _ -> Nothing
  , cvSystems = [SysNear 80 (coverNear chainV SkipDirect (\n cell -> case cell of
      Gem c k i _ | n <= 1 -> Becomes (Gem c k i Nothing)
      _ -> Becomes (ccPut col (n - 1) cell)))]
  }
  where
    col = CoverColumn peel (putOverlay . Chain)
    peel cell = case cell of
      Gem c k i (Just (Chain n)) -> Just (n, Gem c k i Nothing)
      _ -> Nothing

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
    [ kindDef gemV
    , kindDef lineHV
    , kindDef stoneV
    , kindDef flipV
    , kindDef bubbleV
    , kindDef bossV
    , coverDef iceV
    , coverDef chocoV
    , coverDef chainV
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
-- 叠层合成与内置注册表逐组件相等

world :: Registry
world = mkRegistry [kindDef gemV, kindDef lineHV, coverDef iceV, coverDef chocoV, coverDef chainV, kindDef stoneV]

-- | 整格读数：名字、写回、整格的匹配 / 命中 / 物理（叠层合成后）与本体的计数 / 显示。
wholeProbe :: Registry -> Cell -> [(String, String)]
wholeProbe w cell =
  [ ("name", show (elementName w cell))
  , ("mColor", show (mColor m))
  , ("mBlockMatch", show (mBlockMatch m))
  , ("mBlockSwap", show (mBlockSwap m))
  , ("mHintable", show (mHintable m))
  , ("hStrike", show (hStrike h))
  , ("hFires", show (hFires h))
  , ("hBlast", show (hBlast h))
  , ("physics", show (wholePhysics w cell))
  , ("tally", show (body w cell :: Tally))
  , ("fBase", show (fBase f))
  , ("fExtras", show (fExtras f))
  ]
  where
    m = wholeMatch w cell
    h = wholeHit w cell
    f = body w cell :: Face

layeredCells :: [Cell]
layeredCells =
  [Gem c k i ov | c <- [C1, C3], k <- [Normal, LineH], i <- [0 .. 3], ov <- [Nothing, Just Choco, Just (Chain 1), Just (Chain 2)]]
    ++ [Stone 1, Stone 3]

ab_layered_matches_builtin :: Assertion
ab_layered_matches_builtin =
  mapM_
    (\cell -> assertEqual (show cell) (wholeProbe defaultRegistry cell) (wholeProbe world cell))
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
  let kinds = registryKinds defaultRegistry
  assertEqual "all builtin kinds" 25 (length kinds)
  mapM_
    ( \(SomeArchetype a) -> do
        let got = [(c, st) | c <- sampleCells, Just st <- [colGet (aColumn a) c]]
        assertBool ("some sample decodes as " ++ show (aName a)) (not (null got))
        forM_ got $ \(c, st) -> do
          assertEqual ("registry name of " ++ show c) (aName a) (rowName (bodyOf defaultRegistry c))
          assertEqual ("state roundtrip " ++ show c) (Just (colPut (aColumn a) st)) (fmap (colPut (aColumn a)) (colGet (aColumn a) (colPut (aColumn a) st)))
    )
    kinds

--------------------------------------------------------------------------------
-- 透明性

-- | 源码里某个顶层定义的块（从以给定前缀开头的行起，含签名与各等式，到下一个别的顶格行为止）。
blockOf :: String -> String -> String
blockOf prefix src =
  case break (prefix `isPrefixOf`) (lines src) of
    (_, l : rest) -> unlines (l : takeWhile (\x -> null x || take 1 x == " " || prefix `isPrefixOf` x) rest)
    _ -> ""

-- | 'rowProbe' 读到每种组件（漏读的组件透明性测不出来）：Archetype 模块里每个 @instance Component X@ 的 X
-- 都在 rowProbe 的定义里出现为 @:: X@。
ab_row_probe_reads_all_components :: Assertion
ab_row_probe_reads_all_components = do
  src <- readFile "src/Match3/ECS/Archetype.hs"
  let comps = [c | l <- lines src, ("instance" : "Component" : c : _) <- [words l]]
      probeSrc = blockOf "rowProbe " src
  assertBool "components found" (length comps >= 5)
  mapM_ (\c -> assertBool ("rowProbe reads " ++ c) ((":: " ++ c) `isInfixOf` probeSrc)) comps

-- | 整格合成读到叠层组件 'Shield' 的每个字段：挡匹配 / 挡交换在 wholeMatch，点火 / 命中在 wholeHit，随清在 stripOnClearWith；
-- 洗牌保留在 wholePhysics（有层即保留）。
ab_whole_reads_every_layer_query :: Assertion
ab_whole_reads_every_layer_query = do
  src <- readFile "src/Match3/ECS/Registry.hs"
  let has blk q = assertBool (blk ++ " reads " ++ q) (q `isInfixOf` blockOf (blk ++ " ") src)
  has "wholeMatch" "sBlocksMatch"
  has "wholeMatch" "sBlocksSwap"
  has "wholeHit" "sFires"
  has "wholeHit" "sHit"
  has "stripOnClearWith" "sStrips"
  has "wholePhysics" "pKeepShuffle = True"

-- | 装箱（'Row'）前后逐组件相等；无叠层的格整格读数就是本体读数；每个读数在样本上至少有一个不是缺省值。
ab_row_is_transparent :: Assertion
ab_row_is_transparent = do
  let check :: String -> Archetype s -> s -> Assertion
      check what a st = do
        let row = Row a st
            direct =
              [ show (aMatch a st), show (aHit a st), show (aPhysics a st), show (aTally a st), show (aFace a st) ]
        assertEqual what direct [show (rowGet row :: Match), show (rowGet row :: OnHit), show (rowGet row :: Physics), show (rowGet row :: Tally), show (rowGet row :: Face)]
  check "gem" gemV C2
  check "line_h" lineHV C4
  check "stone" stoneV 2
  check "flip" flipV (C1, C5)
  check "bubble" bubbleV 3
  check "boss" bossV (3, 6, 1, 0)
  check "inert" (inertArch "x") (Custom "x" (CustomState 2))
  check "cookie" cookieArch ()
  forM_ [Gem C2 Normal 0 Nothing, Gem C4 LineH 0 Nothing, Stone 2, Custom "x" (CustomState 2)] $ \cell ->
    let r = bodyOf world cell
    in assertEqual ("no layers: whole = body " ++ show cell)
         (show (rowGet r :: Match), show (rowGet r :: OnHit), show (rowGet r :: Physics))
         (show (wholeMatch world cell), show (wholeHit world cell), show (wholePhysics world cell))
  let samples = map (wholeProbe replaced) [Stone 2, Flip C1 C5, Custom "snow_boss" (CustomState (((6 * 256 + 3) * 4 + 1) * 4)), Gem C4 LineH 0 Nothing, Gem C1 Normal 2 Nothing, Gem C1 Normal 0 (Just (Chain 1)), Cookie, Custom "bubble" (CustomState 1)]
      defaults = wholeProbe replaced (Gem C1 Normal 0 Nothing)
      boring = [k | (k, v) <- defaults, k /= "name", all (\smp -> lookup k smp == Just v) samples]
  assertEqual "every reading varies in the samples" [] boring

-- | 解码：冰在外、叠层在内，剩下的交给本体；未注册的 Custom 名字 = 惰性占格；同名以后注册的为准。
ab_world_decode_order :: Assertion
ab_world_decode_order = do
  let cell = Gem C2 Normal 2 (Just (Chain 1))
  assertEqual "layers" ["ice", "chain"] (map peeledName (fst (decodeLayers world cell)))
  assertEqual "inner" (Gem C2 Normal 0 Nothing) (snd (decodeLayers world cell))
  assertEqual "roundtrip" cell (recodeWith world cell)
  let moss = Custom "moss" (CustomState 1)
  assertEqual "unregistered custom" ("moss", moss) (let r = bodyOf world moss in (rowName r, rowCell r))
  assertEqual "unclaimed cell" "?" (rowName (bodyOf world Cookie))
  assertEqual "names in order" ["gem", "line_h", "ice", "choco", "chain", "stone"] (map defName (registryDefs world))
  let w2 = mkRegistry [kindDef stoneV, kindDef gemV, kindDef stoneV]
  assertEqual "dedupe keeps first position" ["stone", "gem"] (map defName (registryDefs w2))
  assertEqual "body" ("stone", Stone 2) (let r = bodyOf w2 (Stone 2) in (rowName r, rowCell r))
  assertEqual "mapMaybe sanity" [1 :: Int] (mapMaybe (\c -> case c of Stone n -> Just n; _ -> Nothing) [getCell (gridFromRows [[Stone 1]]) (0, 0)])

--------------------------------------------------------------------------------
-- 规则方法（第 3 刀）：优先级 / 波及范围 / 蔓延写死（与旧 R 行的次序一致；元素对照快照另有整盘锁定）

-- | 叠层存储列的定律：剥得下来就盖得回去（@ccPeel c == Just (l, inner)@ ⇒ @ccPut l inner == c@），
-- 每种内置叠层都至少认一个样本格；剥下之后同一种叠层不再认里面的格子。
ab_cover_column_law :: Assertion
ab_cover_column_law = do
  let samples = [Gem c k i ov | c <- [C1, C4], k <- [Normal, Bomb], i <- [0, 1, 3], ov <- Nothing : map Just [Grass, Vine, Choco, Fog 1, Fog 3, Chain 2, Freeze 1, Curtain 2, Steam]]
  forM_ (registryLayers defaultRegistry) $ \(SomeCover cv) -> do
    let hits = [(cell, l, inner) | cell <- samples, Just (l, inner) <- [peelWith cv cell]]
    assertBool ("some sample carries " ++ show (cvName cv)) (not (null hits))
    forM_ hits $ \(cell, l, inner) -> do
      assertEqual ("put . peel " ++ show (cvName cv) ++ " " ++ show cell) cell (ccPut (cvColumn cv) l inner)
      assertBool ("peeled once " ++ show (cvName cv) ++ " " ++ show cell) (not (isJust (peelWith cv inner)))

-- | 气球的邻格 system（nearBy + DieAppend）与旧 balloonPopLegacy 一致；气泡 / 毛球的邻格 system 自己写（foldr 去重序）。
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
  assertEqual "bubble near system" [170] [o | SysNear o _ <- aSystems bubbleArch]
  assertEqual "fuzzball near system" [190] [o | SysNear o _ <- aSystems fuzzballArch]

ab_rule_methods_pinned :: Assertion
ab_rule_methods_pinned = do
  let nearOrders (SomeArchetype a) = [o | SysNear o _ <- aSystems a]
  assertEqual "kind near systems"
    [[10], [20], [30], [40], [50], [60], [110], [120], [130], [140], [170], [180], [190], [200]]
    (map nearOrders
      [ SomeArchetype stoneArch, SomeArchetype chestArch, SomeArchetype honeyArch
      , SomeArchetype cakeArch, SomeArchetype balloonArch, SomeArchetype magicHatArch
      , SomeArchetype safeArch, SomeArchetype timeSpiritArch, SomeArchetype makerArch
      , SomeArchetype bottleArch, SomeArchetype bubbleArch, SomeArchetype magicStoneArch
      , SomeArchetype fuzzballArch, SomeArchetype snowBossArch ])
  let layerNear (SomeCover c) = [o | SysNear o _ <- cvSystems c]
      layerSpreads (SomeCover c) = [esOrder e | SysEnd e <- cvSystems c, esPhase e == PhaseSpread]
      covers = [SomeCover fogCover, SomeCover chainCover, SomeCover freezeCover, SomeCover curtainCover, SomeCover chocoCover, SomeCover steamCover, SomeCover vineCover, SomeCover grassCover]
      -- 波及范围按行为测：被直接命中的邻格上的叠层，SkipDirect 不动、AllNeighbours 照样反应
      reachesDirect (SomeCover c) = or
        [ nwBoard (runSystem sys w0) /= b
        | cell <- take 1 [x | ov <- [Grass, Vine, Choco, Fog 2, Chain 2, Freeze 2, Curtain 2, Steam], let x = Gem C2 Normal 0 (Just ov), isJust (peelWith c x)]
        , let b = boardFromRows [[Gem C1 Normal 0 Nothing, cell]]
              w0 = nearWorld (const True) [(0, 0)] [(0, 1)] [] b
        , SysNear _ sys <- cvSystems c
        ]
  assertEqual "layer near systems" [[70], [80], [90], [100], [150], [160], [], []] (map layerNear covers)
  assertEqual "layer reach (direct-hit neighbour reacts)" [False, False, False, False, True, True] (map reachesDirect (take 6 covers))
  assertEqual "spreads" [[10], [20], [30], []] (map layerSpreads [SomeCover vineCover, SomeCover chocoCover, SomeCover steamCover, SomeCover fogCover])
  assertEqual "stone systems" [10] [o | SysNear o _ <- aSystems stoneArch]
  assertEqual "choco systems" (1, 1) (length [() | SysNear 150 _ <- cvSystems chocoCover], length [() | SysEnd _ <- cvSystems chocoCover])
  assertEqual "snow boss footprint" [(2, 3), (2, 4), (3, 3), (3, 4)] (footprint snowBossEntity (2, 3))
  assertEqual "snow boss near system" [200] [o | SysNear o _ <- aSystems snowBossArch]

--------------------------------------------------------------------------------
-- 前端格子描述（第 6 刀）：cellFace 由 Face 组件的 fBase 驱动，与第 6 刀前按构造器写死的 case 逐字段相同

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
