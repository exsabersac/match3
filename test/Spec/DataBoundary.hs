{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Haskell 特性第 8 项：数据边界（docs/haskell-features/08-数据边界.md）。
--
-- * 关卡校验：'Validation' 是 Applicative（定律用 QuickCheck 查），所有失败一次报全；全部关卡与每日关都通过；
--   一关里同时有多处问题时按固定顺序全部列出；只有行列一处问题时文字与 checkLevelDims 逐字节相同。
-- * 放置参数解析：'ArgP' 改写后的每个放置函数，在随机参数表 × 随机格上与改写前的手写 case（Spec.Support.LegacyBoundary）逐项相同；
--   精确匹配 / 前缀匹配 / '<|>' 缺省值的语义用单元测试钉住。
-- * NonEmpty：'beats' 的每组非空，去掉 NonEmpty 后与旧版逐项相同，拼回去就是原列表。
-- * Generic 覆盖：用 GHC.Generics 的 'conName' 列出 Color / GemKind / CellOverlay / CellContents / Outcome 的全部构造器
--   （'Constructors' 类的默认实现 + DeriveAnyClass 一行一个实例），检查测试生成器 genCell 覆盖每个构造器、
--   每个构造器在注册表 / cellFace / 桌面 UI.CellTable / 网页 encodeOutcome 都有对应项，且解码往返。
--   新增构造器而忘了这些地方之一，这组测试就失败（以前只有编译器的不完全匹配警告，表和生成器不报）。
module Spec.DataBoundary
  ( tests
  ) where

import Control.Applicative ((<|>))
import Data.Proxy (Proxy (..))
import GHC.Generics
import Match3.Element.Class (toCell)
import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf, nub, sort, tails)
import Engine.Effect (Effect (..), beats)
import Match3.Core
import Match3.Element (defaultRegistry)
import Match3.Element.Registry (elementName, elementOf, entryName, entrySlot, placeWith, registryDefs, topLayerName)
import Match3.Element.Types (Slot (..), cellSlot, overlaySlot)
import Match3.View (cellFace)
import Match3.Element.Types (Arg (..), Placement (..), argColor, argInt, exactArgs, prefixArgs)
import Match3.Levels.Level
  ( DropSpec (..)
  , LevelIssue (..)
  , Validation (..)
  , assertLevel
  , checkLevel
  , checkLevelDims
  , failure
  , renderIssue
  , validateLevel
  )
import Spec.Properties (genCell, genColor, genOverlay)
import qualified Spec.Support.LegacyBoundary as Old
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Failure, Success)

tests :: [TestTree]
tests =
  [ testCase "level_validation_all_levels_valid" level_validation_all_levels_valid
  , testCase "level_validation_reports_all_issues" level_validation_reports_all_issues
  , testProperty "qc_validation_applicative_laws" qc_validation_applicative_laws
  , testProperty "qc_argp_placers_match_legacy" qc_argp_placers_match_legacy
  , testCase "argp_exact_vs_prefix" argp_exact_vs_prefix
  , testProperty "qc_beats_nonempty_matches_legacy" qc_beats_nonempty_matches_legacy
  , testCase "generic_constructor_lists" generic_constructor_lists
  , testCase "generic_generators_cover_constructors" generic_generators_cover_constructors
  , testCase "generic_every_constructor_has_registry_face_and_ui" generic_every_constructor_has_registry_face_and_ui
  ]

--------------------------------------------------------------------------------
-- 关卡校验

-- | 全部关卡与一年里每天的每日关都通过全部检查（这些检查只收现有数据都满足的不变量）。
level_validation_all_levels_valid :: Assertion
level_validation_all_levels_valid = do
  let dailies = [dailyLevel (Year y) (Month m) (Day d) | y <- [2025, 2026], m <- [1 .. 12], d <- [1 .. 28]]
      bad = [(lvlName l, is) | l <- allLevels ++ dailies, Failure is <- [validateLevel l]]
  assertEqual "no issues" [] bad
  assertEqual "lookupLevel 全部可取" levelCount (length [() | i <- [0 .. levelCount - 1], Just _ <- [lookupLevel i]])

-- | 一关里多处问题一次列全（顺序：行列、皮带、传送门、飞碟、地毯、地面层、掉落口、放置表）；
-- 只有行列一处问题时，文字与 checkLevelDims 相同；assertLevel 的 error 列出全部问题。
level_validation_reports_all_issues :: Assertion
level_validation_reports_all_issues = do
  let base = level 0 "坏关" 20 (goalScore 100)
      bad =
        base
          { lvlRows = 4
          , lvlBelts = [[(0, 0)], [(1, 1), (1, 9), (1, 1)]]
          , lvlPortals = [((2, 2), (2, 2)), ((0, 0), (-1, 3))]
          , lvlUfos = [mkUfo (7, 7) C1]
          , lvlCarpets = [(0, 1), (0, 1)]
          , lvlGround = [((3, 3), ("magic", 0)), ((3, 3), ("magic", 1))]
          , lvlDrops = [DropSpec [] Cookie 0]
          , lvlPlacements = [Place "stone" [] [(0, 0), (4, 0)]]
          }
      expected =
        [ BadDims 4 8
        , BeltTooShort 0 1
        , BeltOutOfBounds 1 (1, 9)
        , BeltDuplicate 1 (1, 1)
        , PortalSameCell 0 (2, 2)
        , PortalOutOfBounds 1 (-1, 3)
        , UfoOutOfBounds 0 (7, 7)
        , CarpetDuplicate (0, 1)
        , GroundNoLayers (3, 3) 0
        , GroundDuplicate (3, 3)
        , DropNoCells 0
        , DropKeepNonPositive 0 0
        , PlacementOutOfBounds 0 "stone" (4, 0)
        ]
  assertEqual "all issues, in order" (Failure expected) (validateLevel bad)
  assertEqual "dims-only text = checkLevelDims" (checkLevelDims (base {lvlRows = 4})) (checkLevel (base {lvlRows = 4}))
  assertEqual "dims-only text exact" (Left "关卡「坏关」尺寸 4×8 超出允许范围 5–10") (checkLevel (base {lvlRows = 4}))
  assertEqual "valid level unchanged" (Right base) (checkLevel base)
  err <- try (evaluate (length (lvlName (assertLevel bad)))) :: IO (Either ErrorCall Int)
  case err of
    Left (ErrorCall msg) -> do
      assertBool "mentions dims" ("4×8" `isInfixOf` msg)
      assertBool "mentions every issue" (all (\i -> renderIssue bad i `isInfixOf` msg) expected)
    Right _ -> assertFailure "expected error"

-- | Applicative 定律（单位元 / 组合 / 同态 / 交换），外加「两边失败时错误按左右顺序拼接」。
qc_validation_applicative_laws :: Property
qc_validation_applicative_laws =
  forAll genV $ \v -> forAll genV $ \u -> forAll genV $ \w -> forAllBlind genF $ \fv -> forAllBlind genF $ \gv -> forAll (arbitrary :: Gen Int) $ \x ->
    conjoin
      [ counterexample "identity" ((pure id <*> v) === v)
      , counterexample "composition" ((pure (.) <*> fv <*> gv <*> v) === (fv <*> (gv <*> v)))
      , counterexample "homomorphism" ((pure (+ 1) <*> (pure x :: Validation [Int] Int)) === pure (x + 1))
      , counterexample "interchange" ((fv <*> pure x) === (pure ($ x) <*> fv))
      , counterexample "accumulates" (case (u, w) of
          (Failure e1, Failure e2) -> ((,) <$> u <*> w) === Failure (e1 ++ e2)
          _ -> property True)
      ]
  where
    genV :: Gen (Validation [Int] Int)
    genV = oneof [Success <$> arbitrary, Failure <$> listOf1 arbitrary]
    genF :: Gen (Validation [Int] (Int -> Int))
    genF = oneof [(\k -> Success (+ k)) <$> arbitrary, (\k -> Success (* k)) <$> arbitrary, Failure <$> listOf1 arbitrary, pure (failure 7)]

--------------------------------------------------------------------------------
-- 放置参数解析

-- | 随机参数表（0–3 个，整数含 0 / 1 / 3 / 4 / 255 / 256 等边界）。
genArgs :: Gen [Arg]
genArgs = do
  k <- choose (0, 3)
  vectorOf k (oneof [AInt <$> oneof [choose (-2, 6), elements [255, 256, 300]], AColor <$> genColor])

-- | 每个改写过的放置函数：在 1×1 盘上经注册表按名字放置（placeWith），与旧手写 case 的结果相同
-- （旧放置返回 Nothing 时格子不变，与 placeWith 的约定一致）。
qc_argp_placers_match_legacy :: Property
qc_argp_placers_match_legacy =
  withMaxSuccess 300 $ forAll genArgs $ \args -> forAll genCell $ \cell ->
    let b0 = boardFromRows [[cell]]
    in conjoin
         [ counterexample (show (n, args, cell)) $
             placeWith defaultRegistry n args b0 [(0, 0)] === Right (maybe b0 (setCell b0 (0, 0)) (old args cell))
         | (n, old) <- Old.legacyPlacers
         ]

-- | 两种跑法与左偏 '<|>'：精确匹配多一个参数就失败；前缀匹配忽略剩下的；'<|>' 只在这一步失败时用右边。
argp_exact_vs_prefix :: Assertion
argp_exact_vs_prefix = do
  assertEqual "exact: one int" (Just 2) (exactArgs argInt [AInt 2])
  assertEqual "exact: extra arg fails" Nothing (exactArgs argInt [AInt 2, AInt 3])
  assertEqual "prefix: extra arg ignored" (Just 2) (prefixArgs argInt [AInt 2, AColor C1])
  assertEqual "prefix: wrong head fails" Nothing (prefixArgs argInt [AColor C1, AInt 2])
  assertEqual "default when empty" (Just 1) (exactArgs (argInt <|> pure 1) [])
  assertEqual "default does not swallow a color" Nothing (exactArgs (argInt <|> pure 1) [AColor C1])
  assertEqual "sequence" (Just (C2, 5)) (exactArgs ((,) <$> argColor <*> argInt) [AColor C2, AInt 5])
  assertEqual "sequence order matters" Nothing (exactArgs ((,) <$> argColor <*> argInt) [AInt 5, AColor C2])

--------------------------------------------------------------------------------
-- NonEmpty

-- | 'beats' 与旧版逐项相同（组内容转回列表比较），拼接还原输入，相邻两组的节拍不同。
qc_beats_nonempty_matches_legacy :: Property
qc_beats_nonempty_matches_legacy =
  forAll (listOf genEffect) $ \effs ->
    let new = beats effs
        plain = [(k, toList g) | (k, g) <- new]
        keys = map fst new
    in conjoin
         [ counterexample "same as legacy" (plain === Old.beats effs)
         , counterexample "concat restores input" (concatMap snd plain === effs)
         , counterexample "key = head beat" (and [k == efBeat e | (k, e : _) <- plain])
         , counterexample "adjacent beats differ" (and (zipWith (/=) keys (drop 1 keys)))
         ]
  where
    genEffect = do
      b <- choose (0, 3)
      a <- choose (0, 9)
      k <- elements ["clear", "score", "spawn"]
      pure (Effect b k "x" [] a)

--------------------------------------------------------------------------------
-- Generic 覆盖（只在测试里用；src 里的 deriving Generic 原样不动）

-- | 构造器清单：'conNamesOf' 列出类型的全部构造器名（定义顺序），'conOf' 给出一个值的构造器名。
-- 两个方法都有基于 Generic 的默认实现，所以实例只要一行 @deriving anyclass instance Constructors T@。
class Constructors a where
  conNamesOf :: Proxy a -> [String]
  default conNamesOf :: GCons (Rep a) => Proxy a -> [String]
  conNamesOf _ = gconNames (Proxy :: Proxy (Rep a))
  conOf :: a -> String
  default conOf :: (Generic a, GCons (Rep a)) => a -> String
  conOf = gconOf . from

-- | 在 Rep 上走一遍：类型（D1）→ 和（:+:）→ 构造器（C1，取 conName），字段不看。
class GCons f where
  gconNames :: Proxy f -> [String]
  gconOf :: f p -> String

instance GCons f => GCons (D1 d f) where
  gconNames _ = gconNames (Proxy :: Proxy f)
  gconOf (M1 x) = gconOf x

instance (GCons f, GCons g) => GCons (f :+: g) where
  gconNames _ = gconNames (Proxy :: Proxy f) ++ gconNames (Proxy :: Proxy g)
  gconOf (L1 x) = gconOf x
  gconOf (R1 y) = gconOf y

instance Constructor c => GCons (C1 c f) where
  gconNames _ = [conName (undefined :: C1 c f ())]
  gconOf m = conName m

-- | 全是无参构造器的类型：按定义顺序列出全部值（GemKind 没有 Enum / Bounded，靠这个枚举）。
class GNullary f where
  gvalues :: [f p]

instance GNullary f => GNullary (D1 d f) where
  gvalues = map M1 gvalues

instance (GNullary f, GNullary g) => GNullary (f :+: g) where
  gvalues = map L1 gvalues ++ map R1 gvalues

instance GNullary (C1 c U1) where
  gvalues = [M1 U1]

allValues :: (Generic a, GNullary (Rep a)) => [a]
allValues = map to gvalues

deriving anyclass instance Constructors Color
deriving anyclass instance Constructors GemKind
deriving anyclass instance Constructors CellOverlay
deriving anyclass instance Constructors CellContents
deriving anyclass instance Constructors Outcome

-- | 构造器清单本身：与 Show / Enum 一致（防止 Generic 实例与预期脱节）。
generic_constructor_lists :: Assertion
generic_constructor_lists = do
  assertEqual "Color = Enum" (map show [minBound .. maxBound :: Color]) (conNamesOf (Proxy :: Proxy Color))
  assertEqual "Color allValues" [minBound .. maxBound :: Color] allValues
  assertEqual "GemKind" ["Normal", "LineH", "LineV", "Bomb", "Rainbow"] (conNamesOf (Proxy :: Proxy GemKind))
  assertEqual "GemKind allValues" (conNamesOf (Proxy :: Proxy GemKind)) (map show (allValues :: [GemKind]))
  assertEqual "CellOverlay" 8 (length (conNamesOf (Proxy :: Proxy CellOverlay)))
  assertEqual "CellContents" 17 (length (conNamesOf (Proxy :: Proxy CellContents)))
  assertEqual "Outcome" ["InvalidSwap", "NoMatch", "MoveApplied", "Won", "Lost", "LevelClear"] (conNamesOf (Proxy :: Proxy Outcome))
  assertEqual "conOf = Show 首词" "Countdown" (conOf (Countdown C2 3))
  assertEqual "conOf Outcome" "LevelClear" (conOf (LevelClear 10 2))

-- | 样本：genCell 抽 6000 个（每个本体构造器权重约 1/31，漏掉某个的概率 < 1e-80），
-- 按构造器各取第一个代表。
cellSamples :: IO [Cell]
cellSamples = generate (vectorOf 6000 genCell)

representatives :: Constructors a => [a] -> [(String, a)]
representatives xs = [(n, x) | n <- nub (map conOf xs), x <- take 1 [y | y <- xs, conOf y == n]]

-- | 测试生成器覆盖全部构造器：genCell 的本体 / 宝石种类 / 叠层，genOverlay、genColor 也各自覆盖。
generic_generators_cover_constructors :: Assertion
generic_generators_cover_constructors = do
  cells <- cellSamples
  ovs <- generate (vectorOf 2000 genOverlay)
  cols <- generate (vectorOf 500 genColor)
  let names :: Constructors a => Proxy a -> [String]
      names = sort . conNamesOf
      seen :: Constructors a => [a] -> [String]
      seen = sort . nub . map conOf
  assertEqual "genCell 本体" (names (Proxy :: Proxy CellContents)) (seen cells)
  assertEqual "genCell 宝石种类" (names (Proxy :: Proxy GemKind)) (seen [k | Gem _ k _ _ <- cells])
  assertEqual "genCell 叠层" (names (Proxy :: Proxy CellOverlay)) (seen [o | Gem _ _ _ (Just o) <- cells])
  assertEqual "genOverlay" (names (Proxy :: Proxy CellOverlay)) (seen ovs)
  assertEqual "genColor" (names (Proxy :: Proxy Color)) (seen cols)

-- | 每个构造器在各张表里都有对应项：
--
-- * 注册表：本体名落在槽位一致的条目上（内置本体 SlotCell (cellSlot 格)；Custom 用已注册的名字 → SlotCustom），
--   每种宝石种类 / 每种叠层的名字互不相同且槽位一致；解码往返（toCell . elementOf = id）；
-- * Match3.View.cellFace：每个本体构造器的类型标签互不相同（网页 JSON 的 "t"），每种叠层的 "o" 互不相同，
--   每种宝石种类的 "k" 互不相同；
-- * 桌面 UI.CellTable（源码扫描，app/ 不在测试的源码目录里）：每个内置本体名、每种宝石种类名都是 cellTable 的键，
--   每个已注册的 Custom 名字都是 customTable 的键；
-- * 网页 Match3Web.Api.encodeOutcome（源码扫描）：每个 Outcome 构造器都有 @tag "构造器名"@。
generic_every_constructor_has_registry_face_and_ui :: Assertion
generic_every_constructor_has_registry_face_and_ui = do
  cells <- cellSamples
  let reg = defaultRegistry
      entries = registryDefs reg
      slotOf n = [entrySlot e | e <- entries, entryName e == n]
      customNames = [entryName e | e <- entries, entrySlot e == SlotCustom]
      reps = representatives ([c | c <- cells, notUnregistered c] ++ [Custom n (CustomState 0) | n <- customNames])
      notUnregistered c = case c of
        Custom n _ -> n `elem` customNames
        _ -> True
      builtinReps = [(k, c) | (k, c) <- reps, k /= "Custom"]
      kinds = [Gem C1 k 0 Nothing | k <- allValues]
      overlays = [Gem C1 Normal 0 (Just o) | (_, o) <- representatives [o | Gem _ _ _ (Just o) <- cells]]
      faceField f c = lookup f (snd (cellFace c))
      distinct xs = length (nub xs) == length xs
  assertEqual "每个本体构造器都有代表" (length (conNamesOf (Proxy :: Proxy CellContents))) (length (nub (map fst reps)))
  -- 注册表
  sequence_
    [ assertEqual ("registry slot " ++ k) [SlotCell (cellSlot c)] (slotOf (elementName reg c)) | (k, c) <- builtinReps ++ map ((,) "Gem") kinds ]
  sequence_ [assertEqual ("custom slot " ++ show n) [SlotCustom] (slotOf n) | n <- customNames]
  assertBool "gem kind names distinct" (distinct (map (elementName reg) kinds))
  sequence_
    [ assertEqual ("overlay slot " ++ show c) [SlotOverlay (overlaySlot o)] (slotOf (topLayerName reg c))
    | c@(Gem _ _ _ (Just o)) <- overlays
    ]
  assertBool "overlay names distinct" (distinct (map (topLayerName reg) overlays))
  sequence_ [assertEqual ("roundtrip " ++ show c) c (toCell (elementOf reg c)) | c <- map snd reps ++ kinds ++ overlays]
  -- cellFace
  assertBool "cellFace 标签互不相同" (distinct [fst (cellFace c) | (_, c) <- reps])
  assertBool "cellFace k 互不相同" (distinct (map (faceField "k") kinds))
  assertBool "cellFace o 互不相同" (distinct (map (faceField "o") overlays))
  -- 桌面 UI.CellTable
  table <- readFile "app/UI/CellTable.hs"
  let keys = tableKeys table
  sequence_
    [ assertBool ("UI.CellTable 缺 " ++ show (elementName reg c)) (unElementName (elementName reg c) `elem` keys)
    | c <- map snd builtinReps ++ kinds ++ [Custom n (CustomState 0) | n <- customNames]
    ]
  -- 网页 encodeOutcome
  api <- readFile "web/hs/Match3Web/Api.hs"
  sequence_
    [ assertBool ("encodeOutcome 缺 " ++ n) (("tag \"" ++ n ++ "\"") `isInfixOf` api)
    | n <- conNamesOf (Proxy :: Proxy Outcome)
    ]

-- | 表里形如 @("名字", …)@ 的键（cellTable 与 customTable 的每一行都是这样开头）。
tableKeys :: String -> [String]
tableKeys src =
  [ key
  | t <- tails src
  , "(\"" `isPrefixOf` t
  , let (key, rest) = break (== '"') (drop 2 t)
  , "\"," `isPrefixOf` rest
  ]
