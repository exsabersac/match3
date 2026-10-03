{-# LANGUAGE OverloadedStrings #-}

-- | Haskell 特性第 8 项：数据边界（docs/haskell-features/08-数据边界.md）。
--
-- * 关卡校验：'Validation' 是 Applicative（定律用 QuickCheck 查），所有失败一次报全；全部关卡与每日关都通过；
--   一关里同时有多处问题时按固定顺序全部列出；只有行列一处问题时文字与 checkLevelDims 逐字节相同。
module Spec.DataBoundary
  ( tests
  ) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.List (isInfixOf)
import Match3.Core
import Match3.Element.Types (Placement (..))
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
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Failure, Success)

tests :: [TestTree]
tests =
  [ testCase "level_validation_all_levels_valid" level_validation_all_levels_valid
  , testCase "level_validation_reports_all_issues" level_validation_reports_all_issues
  , testProperty "qc_validation_applicative_laws" qc_validation_applicative_laws
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
