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
-- * 放置参数解析：每个用 'ArgP' 写的放置函数在一张参数表 × 两种原格上的结果写死（期望值由现实现生成，
--   生成时与删除前的手写 case 副本核对过）；精确匹配 / 前缀匹配 / '<|>' 缺省值的语义用单元测试钉住。
-- * NonEmpty：'beats' 的分组写成固定例子（只合并相邻的同节拍效果），每组非空、拼回去就是原列表。
-- * Generic 覆盖：用 GHC.Generics 的 'conName' 列出 Color / GemKind / CellOverlay / CellContents / Outcome 的全部构造器
--   （'Constructors' 类的默认实现 + DeriveAnyClass 一行一个实例），检查测试生成器 genCell 覆盖每个构造器、
--   每个构造器在注册表 / cellFace / 桌面 UI.CellTable / 网页 encodeOutcome 都有对应项，且解码往返。
--   新增构造器而忘了这些地方之一，这组测试就失败（编译器的不完全匹配警告只管 case，表和生成器它不报）。
module Spec.DataBoundary
  ( tests
  ) where

import Control.Applicative ((<|>))
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import GHC.Generics
import Data.Maybe (isJust)
import Match3.Element.Ability (toCell)
import Match3.Element.Kind (Kind(kindName), SomeKind(..), fromCellAs)
import Match3.Element.Layer (Layer(layerName), SomeLayer(..), peelAs)
import Match3.Element.World (Def(..), decodeLayers)
import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf, nub, sort, tails)
import Engine.Effect (Effect (..), beats)
import Match3.Core
import Match3.Element.Registry (elementOf, placeWith, registryDefs, registryWorld, topLayerName)
import Match3.Types (goalScore)
import Match3.Ufo (mkUfo)
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
  , level
  , renderIssue
  , validateLevel
  )
import Spec.Properties (genCell, genColor, genOverlay)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Failure, Success)

tests :: [TestTree]
tests =
  [ testCase "level_validation_all_levels_valid" level_validation_all_levels_valid
  , testCase "level_validation_reports_all_issues" level_validation_reports_all_issues
  , testProperty "qc_validation_applicative_laws" qc_validation_applicative_laws
  , testCase "argp_placers_pinned" argp_placers_pinned
  , testCase "argp_exact_vs_prefix" argp_exact_vs_prefix
  , testCase "beats_examples_pinned" beats_examples_pinned
  , testProperty "qc_beats_nonempty_groups" qc_beats_nonempty_groups
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

-- | 放置参数表的固定例子：0–3 个参数，整数含 0 / 1 / 3 / -1 / 256 等边界，颜色在前 / 在后，多余参数。
argpArgs :: [[Arg]]
argpArgs =
  [ [], [AInt 0], [AInt 1], [AInt 3], [AInt (-1)], [AInt 256], [AColor C3]
  , [AColor C3, AInt 2], [AInt 2, AColor C4], [AInt 256, AColor C4, AColor C3], [AInt 0, AInt (-1)]
  ]

-- | 写放置的原格：普通宝石（冰 / 叠层的放置要看它）与倒计时（倒计时的放置要看原格）。
argpCells :: [Cell]
argpCells = [Gem C2 Normal 0 Nothing, Countdown C4 2]

-- | 每个用 'ArgP' 写的放置函数：在 1×1 盘上经注册表按名字放置（placeWith），结果格写死
-- （argpCells × argpArgs，按格在外、参数在内的顺序；放置不认参数时格子不变）。
argp_placers_pinned :: Assertion
argp_placers_pinned = do
  assertEqual "placer count" 21 (length pinnedPlacements)
  sequence_
    [ do
        assertEqual (n ++ " / count") (length expected) (length actual)
        sequence_ [assertEqual (show (n, args, cell)) e a | ((cell, args), e, a) <- zip3 inputs expected actual]
    | (n, expected) <- pinnedPlacements
    , let inputs = [(cell, args) | cell <- argpCells, args <- argpArgs]
          actual = [either (const (Stone 99)) (`getCell` (0, 0)) (placeWith defaultRegistry (fromString n) args (boardFromRows [[cell]]) [(0, 0)]) | (cell, args) <- inputs]
    ]

-- | argp_placers_pinned 的期望（由现实现生成，生成时与删除前的手写 case 副本核对过）。
pinnedPlacements :: [(String, [Cell])]
pinnedPlacements =
  [ ("stone",[Stone 1,Stone 1,Stone 1,Stone 3,Stone 1,Stone 256,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Stone 1,Stone 1,Stone 1,Stone 3,Stone 1,Stone 256,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("chest",[Chest 1,Chest 1,Chest 1,Chest 3,Chest 1,Chest 256,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Chest 1,Chest 1,Chest 1,Chest 3,Chest 1,Chest 256,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("honey",[Honey 1,Honey 1,Honey 1,Honey 3,Honey 1,Honey 256,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Honey 1,Honey 1,Honey 1,Honey 3,Honey 1,Honey 256,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("cake",[Cake 1,Cake 1,Cake 1,Cake 3,Cake 1,Cake 256,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Cake 1,Cake 1,Cake 1,Cake 3,Cake 1,Cake 256,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("safe",[Safe 1,Safe 1,Safe 1,Safe 3,Safe 1,Safe 256,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Safe 1,Safe 1,Safe 1,Safe 3,Safe 1,Safe 256,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("balloon",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Balloon C3,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Balloon C3,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("bottle",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Bottle C3,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Bottle C3,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("flip",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("magic_stone",[Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 1),Custom "magic_stone" (CustomState 3),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 3),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 2),Custom "magic_stone" (CustomState 3),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 1),Custom "magic_stone" (CustomState 3),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 3),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 0),Custom "magic_stone" (CustomState 2),Custom "magic_stone" (CustomState 3),Custom "magic_stone" (CustomState 0)])
  , ("snow_boss",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("maker",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Maker C3 3,Maker C3 2,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Maker C3 3,Maker C3 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("snail",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Snail 0 (-1),Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Snail 0 (-1)])
  , ("countdown",[Gem C2 Normal 0 Nothing,Countdown C2 1,Countdown C2 1,Countdown C2 3,Countdown C2 1,Countdown C2 256,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 1,Countdown C4 1,Countdown C4 3,Countdown C4 1,Countdown C4 256,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("ice",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 1 Nothing,Gem C2 Normal 3 Nothing,Gem C2 Normal (-1) Nothing,Gem C2 Normal 256 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("fog",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 (Just (Fog 0)),Gem C2 Normal 0 (Just (Fog 1)),Gem C2 Normal 0 (Just (Fog 3)),Gem C2 Normal 0 (Just (Fog (-1))),Gem C2 Normal 0 (Just (Fog 256)),Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("chain",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 (Just (Chain 0)),Gem C2 Normal 0 (Just (Chain 1)),Gem C2 Normal 0 (Just (Chain 3)),Gem C2 Normal 0 (Just (Chain (-1))),Gem C2 Normal 0 (Just (Chain 256)),Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("freeze",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 (Just (Freeze 0)),Gem C2 Normal 0 (Just (Freeze 1)),Gem C2 Normal 0 (Just (Freeze 3)),Gem C2 Normal 0 (Just (Freeze (-1))),Gem C2 Normal 0 (Just (Freeze 256)),Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("curtain",[Gem C2 Normal 0 Nothing,Gem C2 Normal 0 (Just (Curtain 0)),Gem C2 Normal 0 (Just (Curtain 1)),Gem C2 Normal 0 (Just (Curtain 3)),Gem C2 Normal 0 (Just (Curtain (-1))),Gem C2 Normal 0 (Just (Curtain 256)),Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Gem C2 Normal 0 Nothing,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2])
  , ("bubble",[Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 0),Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 3),Custom "bubble" (CustomState (-1)),Custom "bubble" (CustomState 256),Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 2),Custom "bubble" (CustomState 256),Custom "bubble" (CustomState 0),Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 0),Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 3),Custom "bubble" (CustomState (-1)),Custom "bubble" (CustomState 256),Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 1),Custom "bubble" (CustomState 2),Custom "bubble" (CustomState 256),Custom "bubble" (CustomState 0)])
  , ("fuzzball",[Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 0),Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 3),Custom "fuzzball" (CustomState (-1)),Custom "fuzzball" (CustomState 256),Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 2),Custom "fuzzball" (CustomState 256),Custom "fuzzball" (CustomState 0),Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 0),Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 3),Custom "fuzzball" (CustomState (-1)),Custom "fuzzball" (CustomState 256),Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 1),Custom "fuzzball" (CustomState 2),Custom "fuzzball" (CustomState 256),Custom "fuzzball" (CustomState 0)])
  , ("chameleon",[Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 2),Custom "chameleon" (CustomState 2),Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 1),Custom "chameleon" (CustomState 1),Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Countdown C4 2,Custom "chameleon" (CustomState 2),Custom "chameleon" (CustomState 2),Countdown C4 2,Countdown C4 2,Countdown C4 2])
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

-- | 'beats' 的分组写死：只合并相邻的同节拍效果，不排序（换成 @NE.groupAllWith@ 时第一个例子就变成 @[0, 1, 2]@）。
beats_examples_pinned :: Assertion
beats_examples_pinned = do
  let eff b a = Effect b "clear" "x" [] a
      groups es = [(k, map efAmount (toList g)) | (k, g) <- beats es]
  assertEqual "descending beats" [(2, [0]), (1, [1]), (0, [2])] (groups [eff 2 0, eff 1 1, eff 0 2])
  assertEqual "runs of beats" [(0, [0, 1]), (1, [2, 3, 4]), (3, [5])] (groups [eff 0 0, eff 0 1, eff 1 2, eff 1 3, eff 1 4, eff 3 5])
  assertEqual "beat comes back later" [(1, [0, 1]), (0, [2]), (1, [3])] (groups [eff 1 0, eff 1 1, eff 0 2, eff 1 3])
  assertEqual "empty" [] (groups [])

-- | 'beats' 的每组非空：拼接还原输入，组的节拍 = 组内首个效果的节拍，相邻两组的节拍不同。
qc_beats_nonempty_groups :: Property
qc_beats_nonempty_groups =
  forAll (listOf genEffect) $ \effs ->
    let new = beats effs
        plain = [(k, toList g) | (k, g) <- new]
        keys = map fst new
    in conjoin
         [ counterexample "concat restores input" (concatMap snd plain === effs)
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
-- * 注册表：本体名落在认这个（拆掉叠层后的）格子的本体种类上（Custom 用已注册的名字），
--   每种宝石种类 / 每种叠层的名字互不相同、最上层落在 peel 认这个格子的叠层种类上；解码往返（toCell . elementOf = id）；
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
      kindAccepts n c = or [isJust (fromCellAs p c) | KindDef (SomeKind p) <- entries, kindName p == n]
      layerAccepts n c = or [isJust (peelAs p c) | LayerDef (SomeLayer p) <- entries, layerName p == n]
      inner = snd . decodeLayers (registryWorld reg)
      customNames = [kindName p | KindDef (SomeKind p) <- entries, any (\k -> isJust (fromCellAs p (Custom (kindName p) (CustomState k)))) [0 .. 20]]
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
    [ assertBool ("registry claims " ++ k) (kindAccepts (elementName reg c) (inner c)) | (k, c) <- builtinReps ++ map ((,) "Gem") kinds ]
  sequence_ [assertEqual ("custom name " ++ show n) n (elementName reg (Custom n (CustomState 0))) | n <- customNames]
  assertEqual "custom kinds" ["bubble", "magic_stone", "fuzzball", "snow_boss", "chameleon"] customNames
  assertBool "gem kind names distinct" (distinct (map (elementName reg) kinds))
  sequence_
    [ assertBool ("overlay claimed " ++ show c) (layerAccepts (topLayerName reg c) c)
    | c <- overlays
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
