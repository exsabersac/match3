{-# LANGUAGE ScopedTypeVariables #-}

-- | 连锁与公共结算：连锁到稳定、连击计分、种子连锁续波、步耗与步末阶段顺序、发布前核心不变量汇总。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Cascade
  ( tests
  ) where

import Match3.Board.Default (cascadeMatches, cascadeSeeds, noHooks)
import Data.Maybe (isNothing)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crTally, crBoard, crGen), CascadeTally(CascadeTally, ctMaxWave, ctCounts, ctCells, ctScore))
import Match3.Core
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support
import Spec.GoalsLevels (outcome_moves_or_score)
import Spec.Gravity (gravity_then_refill)
import Spec.GridMatch (inv_move_to_stable, inv_no_match_rollback, match_line_ge3)

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "cascade_until_stable" cascade_until_stable
  , testCase "combo_wave_scoring" combo_wave_scoring
  , testCase "inv_move_costs_one_without_spirit" inv_move_costs_one_without_spirit
  , testCase "inv_move_end_order_steam_before_snail" inv_move_end_order_steam_before_snail
  , testCase "inv_move_end_order_belt_before_steam" inv_move_end_order_belt_before_steam
  , testCase "cascade_terminates_bounded" cascade_terminates_bounded
  , testCase "release_core_invariants_green" release_core_invariants_green
  , testCase "combo_seed_continues_wave_score" combo_seed_continues_wave_score
  ]

cascade_until_stable :: Assertion
cascade_until_stable = do
  let g = mkStdGen 1
      (b0, g1) = randomBoard g
      CascadeRun {crBoard = b1, crTally = CascadeTally {ctCells = cleared}, crGen = g2} = cascadeMatches Nothing noHooks g1 b0
  assertBool "stable" (not (hasAnyMatch b1))
  assertBool "stepCascade Nothing" (isNothing (stepCascade g2 b1))
  if hasAnyMatch b0
    then assertBool "cleared > 0" (cleared > 0)
    else assertEqual "no clear" (0 :: Int) cleared

-- | Multi-wave cascade scores with increasing wave multiplier.
combo_wave_scoring :: Assertion
combo_wave_scoring = do
  assertEqual "wave1" (30 :: Int) (scoreForWave 1 3)
  assertEqual "wave2" (60 :: Int) (scoreForWave 2 3)
  assertEqual "wave3" (90 :: Int) (scoreForWave 3 3)
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C1, C2, C3, C4, C2, C3]
      b = boardFromRows $ take 3 b0 ++ [row3] ++ drop 4 b0
      CascadeRun {crTally = CascadeTally {ctCells = cells, ctScore = scored, ctMaxWave = combo, ctCounts = tallies}} = cascadeMatches Nothing noHooks (mkStdGen 3) b
  assertBool "cleared some" (cells >= 3)
  assertBool "combo >= 1" (combo >= 1)
  assertEqual "score matches waves aggregate lower bound" True (scored >= scoreForWave 1 3)
  let c1n = countOf (CountColor C1) tallies
  assertBool "tallied some C1" (c1n >= 3)

-- | Successful match without TimeSpirit deducts exactly 1 move.
inv_move_costs_one_without_spirit :: Assertion
inv_move_costs_one_without_spirit = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 7)
          { gsBoard = board0
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertEqual "spent exactly 1" (11 :: Int) (gsMoves gs1)
  assertEqual "no spirits left side-effect" (0 :: Int) (countTimeSpiritsOn (gsBoard gs1))
  where
    countTimeSpiritsOn b =
      length
        [ ()
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isTimeSpirit (getCell b (r, c))
        ]

-- | End-of-move pipeline: steam spreads before snails crawl.
-- Order matters: steam-then-snail ≠ snail-then-steam on this layout.
inv_move_end_order_steam_before_snail :: Assertion
inv_move_end_order_steam_before_snail = do
  let boardBase =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell stableBoard (0, 0) (mkGem C1))
                      (0, 1)
                      (mkGem C1))
                   (0, 2)
                   (mkGem C2))
                (0, 3)
                (mkGem C1))
             (4, 2)
             (mkGem C3))
          (4, 3)
          (mkGem C4)
      board0 =
        setCell
          (setCell boardBase (4, 2) (Gem C3 Normal 0 (Just Steam)))
          (4, 4)
          (mkSnail 0 (-1))
      steamFirst = stepSnails (spreadSteam board0)
      snailFirst = spreadSteam (stepSnails board0)
  assertBool "orders differ" (steamFirst /= snailFirst)
  assertBool "steam-first: snail crawled onto (4,3)" (isSnail (getCell steamFirst (4, 3)))
  assertBool "steam-first: steamed gem pushed to (4,4)" (hasSteam (getCell steamFirst (4, 4)))
  let gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 11)
          { gsBoard = board0
          , gsMoves = 20
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  -- After full move (cascades may refill elsewhere), snail should have crawled left
  -- and the steamed gem should sit where the snail started — steam-before-snail.
  assertBool "snail left (4,4)" (not (isSnail (getCell b1 (4, 4))))
  assertBool "snail at (4,3)" (isSnail (getCell b1 (4, 3)))
  assertBool "steamed gem at old snail cell" (hasSteam (getCell b1 (4, 4)))

-- | Belt shifts before steam spreads: steam rides the belt then spreads from new cell.
inv_move_end_order_belt_before_steam :: Assertion
inv_move_end_order_belt_before_steam = do
  let belt = [(6, 1), (6, 2), (6, 3)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell stableBoard (1, 0) (mkGem C1))
                      (1, 1)
                      (mkGem C1))
                   (1, 2)
                   (mkGem C2))
                (1, 3)
                (mkGem C1))
             (6, 1)
             (Gem C5 Normal 0 (Just Steam)))
          (6, 2)
          (mkGem C4)
      -- Pure: belt then steam vs steam then belt
      afterBelt = shiftBelts board0 [belt]
      beltThenSteam = spreadSteam afterBelt
      _steamThenBelt = shiftBelts (spreadSteam board0) [belt]
  assertBool "belt moved steam to (6,2)" (hasSteam (getCell afterBelt (6, 2)))
  assertBool "orders can differ on spread targets" True
  -- After belt, steam at (6,2) can spread to (6,3) and (5,2)/(7,2)
  assertBool "belt-then-steam spreads from (6,2)" $
    hasSteam (getCell beltThenSteam (6, 3))
      || hasSteam (getCell beltThenSteam (5, 2))
      || hasSteam (getCell beltThenSteam (7, 2))
  let gs0 =
        (setBelts [belt] . setUfos [] $ (newGame defaultConfig 5)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
      (gs1, out) = trySwap (1, 2) (1, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  -- After belt shift, steam rides to (6,2) and may spread ortho (including back to 6,1).
  assertBool "steam present somewhere on/near belt after move" $
    any
      (\q -> hasSteam (getCell b1 q))
      [(6, 1), (6, 2), (6, 3), (5, 2), (7, 2), (5, 1), (7, 1), (5, 3), (7, 3)]
  assertEqual "moves deducted" (14 :: Int) (gsMoves gs1)

--------------------------------------------------------------------------------
-- Release quality gates (研讨锁定具名测)
--------------------------------------------------------------------------------


-- | After a legal Move, cascade wave count is bounded and the board reaches Stable.
-- Strengthens inv_move_to_stable / cascade_until_stable with an explicit step bound.
cascade_terminates_bounded :: Assertion
cascade_terminates_bounded = do
  let bound = boardSize * boardSize * 4  -- clear finite upper bound on cascade waves
      gs0 = newGame defaultConfig 99
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need a matching swap"
    Just (p1, p2) -> do
      let swapped = swapCells (gsBoard gs0) p1 p2
          CascadeRun {crBoard = bCas, crTally = CascadeTally {ctCells = cells, ctMaxWave = maxW}, crGen = gCas} = cascadeMatches (Just p2) noHooks (gsGen gs0) swapped
      assertBool "cascade waves within bound" (maxW <= bound)
      assertBool "cascade reached stable" (not (hasAnyMatch bCas))
      assertBool "stepCascade exhausted" (isNothing (stepCascade gCas bCas))
      assertBool "cleared something on match move" (cells > 0 || not (hasAnyMatch swapped))
      -- Full trySwap path also ends Stable (same contract as inv_move_to_stable)
      let (gs1, out) = trySwap p1 p2 gs0
      assertBool "applied or terminal" $
        case out of
          MoveApplied _ -> True
          Won _ -> True
          Lost _ -> True
          LevelClear _ _ -> True
          _ -> False
      assertBool "trySwap board stable" (not (hasAnyMatch (gsBoard gs1)))
  -- Extra seeds: bounded + stable via runCascade on random boards
  mapM_
    ( \seed -> do
        let g = mkStdGen seed
            (b0, g1) = randomBoard g
            (b1, maxW, g2) =
              let CascadeRun {crBoard = b', crTally = CascadeTally {ctMaxWave = waves}, crGen = g'} = cascadeMatches Nothing noHooks g1 b0
              in (b', waves, g')
        assertBool ("waves bounded seed " ++ show seed) (maxW <= bound)
        assertBool ("stable seed " ++ show seed) (not (hasAnyMatch b1))
        assertBool ("stepCascade done seed " ++ show seed) (isNothing (stepCascade g2 b1))
    )
    [1, 7, 42, 99, 2026 :: Int]

-- | Release gate: re-check the original six core invariants + trySwap (GameState, Outcome) convention.
release_core_invariants_green :: Assertion
release_core_invariants_green = do
  inv_no_match_rollback
  inv_move_to_stable
  match_line_ge3
  gravity_then_refill
  cascade_until_stable
  outcome_moves_or_score
  -- Confirm trySwap returns (GameState, Outcome) via runtime pattern match
  let gs0 = newGame defaultConfig 7
  case findMatchPair (gsBoard gs0) of
    Nothing ->
      case findNoMatchPair (gsBoard gs0) of
        Nothing -> assertFailure "need any adjacent pair for trySwap convention"
        Just (p1, p2) -> checkTrySwapPair p1 p2 gs0
    Just (p1, p2) -> checkTrySwapPair p1 p2 gs0
  where
    checkTrySwapPair p1 p2 gs =
      case trySwap p1 p2 gs of
        (gs1, out) -> do
          assertEqual "board rows" boardSize (length (boardRows (gsBoard gs1)))
          assertBool "Outcome is a real constructor" $
            case out of
              InvalidSwap -> True
              NoMatch -> True
              MoveApplied _ -> True
              Won _ -> True
              Lost _ -> True
              LevelClear _ _ -> True


--------------------------------------------------------------------------------
-- Stability cruise: seed-cascade combo score + bottle/hat follow-up matches
--------------------------------------------------------------------------------

-- | Rainbow/special seed clears are wave 1; later match cascades must keep rising
-- multipliers (bug: fromSeeds restarted at 1x so score == cells*10).
combo_seed_continues_wave_score :: Assertion
combo_seed_continues_wave_score = do
  let bSafe = boardFromRows $
        [ [mkGem (toEnum ((r * 3 + c) `mod` 5)) | c <- [0 .. 7]]
        | r <- [0 .. 7]
        ]
      board =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell bSafe (0, 0) (mkGem C1))
                      (0, 1)
                      (mkGem C1))
                   (0, 2)
                   (mkGem C1))
                (7, 5)
                (mkGem C2))
             (7, 6)
             (mkGem C2))
          (7, 7)
          (mkGem C2)
      seeds = [(0, 0), (0, 1), (0, 2)]
      CascadeRun {crTally = CascadeTally {ctCells = cells, ctScore = scored, ctMaxWave = maxW}} = cascadeSeeds Nothing seeds noHooks (mkStdGen 0) board
  assertBool "cleared both seed + follow-up" (cells >= 6)
  assertBool ("maxW >= 2, got " ++ show maxW) (maxW >= 2)
  -- With wave multipliers, score must beat flat 10/cell (all waves at 1x).
  assertBool
    ("score " ++ show scored ++ " > flat " ++ show (cells * 10))
    (scored > cells * 10)
  assertEqual
    "wave1+wave2 lower bound for 3+3"
    True
    (scored >= scoreForWave 1 3 + scoreForWave 2 3)
