{-# LANGUAGE OverloadedStrings #-}
-- | 关卡记录与关卡表（第 6 刀）：lookupLevel / clampLevelIndex / campaignGame 的性质，
-- placeWith 的 Either 失败分支，以及「全部内置关卡与每日挑战的放置表都能放成功（Right）」。
module Spec.Levels
  ( tests
  ) where

import Control.Exception (ErrorCall(..), evaluate, try)
import Control.Monad (filterM)
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Maybe (isJust, isNothing)
import Match3.Board.Default (hasValidMove)
import Match3.Board.Random (randomPlayableBoard)
import Match3.Core
import Match3.Daily (dailyConfig)
import Match3.Levels.Level (assertLevelDims, checkLevelDims, level)
import Match3.Element (Arg(..), PlaceError(..), Placement(..), placeAllWith, placeWith)
import Match3.Game.Level (decorateLevel, decorateLevelWith, goalDecorWith, newGame)
import Match3.Types (goalScore)
import Spec.Support.Source (readCode, sourcesUnderAll, stripStrings)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | 本模块的测试（平铺进顶层 "match3" 组）。
tests :: [TestTree]
tests =
  [ testCase "level_placements_all_right" level_placements_all_right
  , testCase "daily_placements_all_right" daily_placements_all_right
  , testCase "place_with_reports_errors" place_with_reports_errors
  , testCase "decorate_level_error_names_level" decorate_level_error_names_level
  , testCase "campaign_game_matches_level_config" campaign_game_matches_level_config
  , testCase "restart_next_out_of_range_clamped" restart_next_out_of_range_clamped
  , testCase "no_all_levels_index_scan" no_all_levels_index_scan
  , testCase "board_size_out_of_range_rejected" board_size_out_of_range_rejected
  , testCase "wide_board_level_is_6x9" wide_board_level_is_6x9
  , testCase "default_levels_stay_8x8" default_levels_stay_8x8
  ]
    ++ map fixedSeed
      [ testProperty "qc_lookup_level_in_range" (withMaxSuccess 500 qc_lookup_level_in_range)
      , testProperty "qc_clamp_level_index_found" (withMaxSuccess 500 qc_clamp_level_index_found)
      ]
  where
    fixedSeed = localOption (QuickCheckReplayLegacy 20260930)

-- | 若干开局随机盘（不带任何装饰）。
bareBoards :: [Board]
bareBoards = [fst (randomPlayableBoard (mkStdGen s)) | s <- [1 .. 6 :: Int]]

-- | 第一张开局盘。
board1 :: Board
board1 = fst (randomPlayableBoard (mkStdGen (1 :: Int)))

isRight' :: Either a b -> Bool
isRight' = either (const False) (const True)

-- | 每个内置关卡的装饰放置表、以及其后的目标补齐，在多张开局盘上都是 Right；lvlIndex 与表中下标一致。
level_placements_all_right :: Assertion
level_placements_all_right = do
  assertEqual "levelCount = length allLevels" (length allLevels) levelCount
  assertEqual "lvlIndex = 下标" [0 .. levelCount - 1] (map lvlIndex allLevels)
  sequence_
    [ case decorateLevelWith defaultRegistry l b of
        Left e -> assertFailure ("第 " ++ show (lvlIndex l + 1) ++ " 关装饰失败：" ++ show e)
        Right b' ->
          assertBool ("第 " ++ show (lvlIndex l + 1) ++ " 关目标补齐失败")
            (isRight' (goalDecorWith defaultRegistry (lvlGoal l) b'))
    | l <- allLevels
    , b <- bareBoards
    ]
  -- 目标补齐单独作用在裸盘上（补齐分支一定会真正放置）
  sequence_
    [ assertBool ("第 " ++ show (lvlIndex l + 1) ++ " 关目标补齐（裸盘）失败") (isRight' (goalDecorWith defaultRegistry (lvlGoal l) b))
    | l <- allLevels
    , b <- bareBoards
    ]

-- | 每日挑战（2026、2027 两年每天；覆盖全部 10 种目标）：第 1 关装饰 + 目标补齐都是 Right。
daily_placements_all_right :: Assertion
daily_placements_all_right = do
  let days = [(y, m, d) | y <- [2026, 2027], m <- [1 .. 12], d <- [1 .. 28]]
      goals = [cfgGoal (dailyConfig (Year y) (Month m) (Day d)) | (y, m, d) <- days]
      l0 = case lookupLevel 0 of
        Just l -> l
        Nothing -> error "no level 0"
  assertEqual "10 种每日目标都覆盖到" 10 (length (nub (map show goals)))
  sequence_
    [ assertBool ("每日目标 " ++ show g ++ " 放置失败") (isRight' (decorateLevelWith defaultRegistry l0 b >>= goalDecorWith defaultRegistry g))
      >> assertBool ("每日目标 " ++ show g ++ " 裸盘补齐失败") (isRight' (goalDecorWith defaultRegistry g b))
    | g <- nub goals
    , b <- bareBoards
    ]

-- | 未注册的名字 → UnknownElement；越界格 → PlaceOutOfBounds；放置表遇到第一处失败即停。
place_with_reports_errors :: Assertion
place_with_reports_errors = do
  let b = board1
  assertEqual "unknown" (Left (UnknownElement "no_such_elem")) (placeWith defaultRegistry "no_such_elem" [] b [(0, 0)])
  assertEqual "row out of bounds" (Left (PlaceOutOfBounds "stone" (boardSize, 0))) (placeWith defaultRegistry "stone" [] b [(1, 1), (boardSize, 0)])
  assertEqual "negative col" (Left (PlaceOutOfBounds "ice" (0, -1))) (placeWith defaultRegistry "ice" [AInt 1] b [(0, -1)])
  assertBool "valid placement is Right" (isRight' (placeWith defaultRegistry "stone" [] b [(1, 1), (2, 2)]))
  assertEqual "empty list is identity" (Right b) (placeWith defaultRegistry "stone" [] b [])
  assertEqual "table stops at first error"
    (Left (UnknownElement "nope"))
    (placeAllWith defaultRegistry b [Place "stone" [] [(1, 1)], Place "nope" [] [(2, 2)], Place "grass" [] [(99, 99)]])

-- | 静态数据边界：坏放置表在 decorateLevel 里报错，错误信息带关卡序号与名字。
decorate_level_error_names_level :: Assertion
decorate_level_error_names_level = do
  let bad = (level 41 "坏关" 20 (goalScore 100)) {lvlPlacements = [Place "no_such_elem" [] [(0, 0)]]}
  r <- try (evaluate (decorateLevel bad (board1)))
  case r of
    Left (ErrorCall msg) -> do
      assertBool ("message names the level: " ++ msg) ("坏关" `isInfixOf` msg && "第 42 关" `isInfixOf` msg)
      assertBool ("message names the element: " ++ msg) ("no_such_elem" `isInfixOf` msg)
    Right _ -> assertFailure "expected an error for a bad placement table"

-- | campaignGame li = newGameAtLevel li (levelConfig 该关)；越界为 Nothing。
campaign_game_matches_level_config :: Assertion
campaign_game_matches_level_config = do
  sequence_
    [ assertEqual ("level " ++ show (lvlIndex l) ++ " seed " ++ show s)
        (Just (show (newGameAtLevel (lvlIndex l) (levelConfig l) s)))
        (show <$> campaignGame (lvlIndex l) s)
    | l <- allLevels
    , s <- [1, 42]
    ]
  assertBool "past the end" (isNothing (campaignGame levelCount 1))
  assertBool "negative" (isNothing (campaignGame (-1) 1))

-- | 越界的当前关：重开 / 下一关都夹到关卡表范围内，不会因越界下标崩溃。
restart_next_out_of_range_clamped :: Assertion
restart_next_out_of_range_clamped = do
  let gs = newGame defaultConfig 5
      lastI = levelCount - 1
  assertEqual "restart past end → last level" lastI (gsLevel (restartLevel gs {gsLevel = levelCount + 5} 3))
  assertEqual "restart negative → first level" 0 (gsLevel (restartLevel gs {gsLevel = -3} 3))
  assertEqual "next past end → last level" lastI (gsLevel (nextLevel gs {gsLevel = levelCount + 5, gsOver = Nothing} 3))
  assertEqual "restart in range unchanged" 7 (gsLevel (restartLevel gs {gsLevel = 7} 3))

-- | 源码扫描：src / app / web/hs / test 的代码里（去掉注释与字符串）不再出现按下标取关的 allLevels !!，一律经 lookupLevel。
no_all_levels_index_scan :: Assertion
no_all_levels_index_scan = do
  files <- sourcesUnderAll ["src", "app", "web/hs", "test"]
  assertBool "scanned the frontends too" (any (isPrefixOf "app/") files && any (isPrefixOf "web/hs/") files)
  offenders <- filterM (fmap (isInfixOf "allLevels !!" . unwords . words . stripStrings) . readCode) files
  assertEqual "files still indexing allLevels with !!" [] offenders

-- | lookupLevel i 为 Just ⟺ 0 ≤ i < levelCount，且取到的关 lvlIndex = i。
qc_lookup_level_in_range :: Property
qc_lookup_level_in_range =
  forAll (choose (-20, levelCount + 20)) $ \i ->
    let r = lookupLevel i
    in (isJust r === (i >= 0 && i < levelCount)) .&&. (fmap lvlIndex r === (if isJust r then Just i else Nothing))

-- | clampLevelIndex 的结果总能被 lookupLevel 找到；范围内不变；幂等。
qc_clamp_level_index_found :: Property
qc_clamp_level_index_found =
  forAll (oneof [choose (-1000, 1000), arbitrary]) $ \i ->
    let c = clampLevelIndex i
    in isJust (lookupLevel c)
         .&&. (clampLevelIndex c === c)
         .&&. (if i >= 0 && i < levelCount then c === i else property True)

-- | 行列越界在加载时拒绝（error，不夹取）。
board_size_out_of_range_rejected :: Assertion
board_size_out_of_range_rejected = do
  let badRows = (level 0 "坏行" 20 (goalScore 100)) {lvlRows = 4, lvlCols = 8}
      badCols = (level 0 "坏列" 20 (goalScore 100)) {lvlRows = 8, lvlCols = 11}
      badBoth = (level 0 "双坏" 20 (goalScore 100)) {lvlRows = 3, lvlCols = 12}
  assertEqual "rows too small" (Left "关卡「坏行」尺寸 4×8 超出允许范围 5–10") (checkLevelDims badRows)
  assertEqual "cols too big" (Left "关卡「坏列」尺寸 8×11 超出允许范围 5–10") (checkLevelDims badCols)
  assertEqual "both bad" (Left "关卡「双坏」尺寸 3×12 超出允许范围 5–10") (checkLevelDims badBoth)
  assertEqual "ok 5×10" (Right ((level 0 "ok" 20 (goalScore 100)) {lvlRows = 5, lvlCols = 10})) (checkLevelDims ((level 0 "ok" 20 (goalScore 100)) {lvlRows = 5, lvlCols = 10}))
  err <- try (evaluate (assertLevelDims badRows)) :: IO (Either ErrorCall Level)
  case err of
    Left (ErrorCall msg) -> assertBool "assert mentions size" ("4×8" `isInfixOf` msg || "超出允许范围" `isInfixOf` msg)
    Right _ -> assertFailure "expected error for out-of-range size"

-- | 第 49 关「宽域」为 6×9 矩形盘。
wide_board_level_is_6x9 :: Assertion
wide_board_level_is_6x9 = case lookupLevel 48 of
  Nothing -> assertFailure "lookupLevel 48 = Nothing"
  Just lvl -> do
    assertEqual "name" "宽域" (lvlName lvl)
    assertEqual "rows" 6 (lvlRows lvl)
    assertEqual "cols" 9 (lvlCols lvl)
    let gs = newGameAtLevel 48 (levelConfig lvl) 1
        b = gsBoard gs
    assertEqual "board rows" 6 (length (boardRows b))
    assertBool "board cols" (all ((== 9) . length) (boardRows b))
    assertEqual "dims helper" (6, 9) (boardDims b)
    assertBool "playable" (hasValidMove b)

-- | 既有关卡未改尺寸配置（缺省 8×8）。
default_levels_stay_8x8 :: Assertion
default_levels_stay_8x8 = do
  mapM_
    ( \l -> do
        assertEqual ("rows L" ++ show (lvlIndex l)) 8 (lvlRows l)
        assertEqual ("cols L" ++ show (lvlIndex l)) 8 (lvlCols l)
    )
    (take 48 allLevels)
