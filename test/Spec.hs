{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Maybe (fromMaybe, isNothing)
import Match3.Board (applyGravity, clearMatches, refill)
import Match3.Core
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "match3"
    [ testCase "inv_no_match_rollback" inv_no_match_rollback
    , testCase "inv_move_to_stable" inv_move_to_stable
    , testCase "match_line_ge3" match_line_ge3
    , testCase "gravity_then_refill" gravity_then_refill
    , testCase "cascade_until_stable" cascade_until_stable
    , testCase "outcome_moves_or_score" outcome_moves_or_score
    , testProperty "qc_findMatches_ge3" qc_findMatches_ge3
    , testCase "special_line_from_4" special_line_from_4
    , testCase "special_bomb_from_5" special_bomb_from_5
    , testCase "hint_finds_move" hint_finds_move
    , testCase "undo_restores" undo_restores
    , testCase "shuffle_when_no_moves" shuffle_when_no_moves
    , testCase "playable_board_stable_and_has_move" playable_board_stable_and_has_move
    , testCase "combo_wave_scoring" combo_wave_scoring
    , testCase "collect_goal_progress" collect_goal_progress
    , testCase "collect_goal_clears_level" collect_goal_clears_level
    , testCase "collect_goal_lose_on_moves" collect_goal_lose_on_moves
    , testCase "level_table_mixes_collect" level_table_mixes_collect
    , testCase "score_goal_ignores_collect" score_goal_ignores_collect
    ]

findNoMatchPair :: Board -> Maybe (Pos, Pos)
findNoMatchPair b =
  case
    [ ((r, c), (r, c + 1))
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 2]
    , not (hasAnyMatch (swapCells b (r, c) (r, c + 1)))
    ] of
    (p : _) -> Just p
    [] -> Nothing

findMatchPair :: Board -> Maybe (Pos, Pos)
findMatchPair b =
  case
    [ ((r, c), (r, c + 1))
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 2]
    , hasAnyMatch (swapCells b (r, c) (r, c + 1))
    ]
      ++ [ ((r, c), (r + 1, c))
         | r <- [0 .. boardSize - 2]
         , c <- [0 .. boardSize - 1]
         , hasAnyMatch (swapCells b (r, c) (r + 1, c))
         ] of
    (p : _) -> Just p
    [] -> Nothing

inv_no_match_rollback :: Assertion
inv_no_match_rollback = do
  let gs0 = newGame defaultConfig 12345
  case findNoMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need a no-match adjacent pair"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      out @?= NoMatch
      gsBoard gs1 @?= gsBoard gs0
      gsScore gs1 @?= gsScore gs0
      gsMoves gs1 @?= gsMoves gs0

inv_move_to_stable :: Assertion
inv_move_to_stable = do
  let gs0 = newGame defaultConfig 99
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need a matching swap"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      assertBool "applied or terminal" $
        case out of
          MoveApplied _ -> True
          Won _ -> True
          Lost _ -> True
          LevelClear _ _ -> True
          _ -> False
      assertBool "board stable" (not (hasAnyMatch (gsBoard gs1)))

-- | Horizontal/vertical runs of length >= 3 match; length-2 and diagonal do not.
match_line_ge3 :: Assertion
match_line_ge3 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C1, C2, C3, C4, C5, C2]
      b = take 3 b0 ++ [row3] ++ drop 4 b0
      ms = findMatches b
  assertBool "(3,0)" ((3, 0) `elem` ms)
  assertBool "(3,1)" ((3, 1) `elem` ms)
  assertBool "(3,2)" ((3, 2) `elem` ms)
  let colBoard =
        [ [ if c == 1 && r >= 2 && r <= 4 then mkGem C3 else mkGem C5
          | c <- [0 .. boardSize - 1]
          ]
        | r <- [0 .. boardSize - 1]
        ]
      vs = findMatches colBoard
  assertBool "vert (2,1)" ((2, 1) `elem` vs)
  assertBool "vert (3,1)" ((3, 1) `elem` vs)
  assertBool "vert (4,1)" ((4, 1) `elem` vs)

  -- Negatives on an explicit stable board (no incidental 3-runs)
  let stable =
        [ map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
        , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
        , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
        , map mkGem [C4, C5, C1, C2, C3, C4, C5, C1]
        , map mkGem [C5, C1, C2, C3, C4, C5, C1, C2]
        , map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
        , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
        , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
        ]
  assertBool "stable base" (not (hasAnyMatch stable))

  -- Negative: only 2-in-a-row horizontally — not a match
  let b2 = setCell (setCell stable (0, 0) (mkGem C1)) (0, 1) (mkGem C1)
  assertBool "2-in-a-row not a match" (null (findMatches b2))

  -- Negative: only 2-in-a-row vertically — not a match
  let bV = setCell (setCell stable (3, 0) (mkGem C5)) (4, 0) (mkGem C5)
  assertBool "vert 2-in-a-row not a match" (null (findMatches bV))

  -- Negative: diagonal same color does not count
  let diag =
        setCell
          (setCell
             (setCell stable (0, 0) (mkGem C1))
             (1, 1)
             (mkGem C1))
          (2, 2)
          (mkGem C1)
  assertBool "diagonal not a match" (null (findMatches diag))
  assertBool "diag hasAnyMatch false" (not (hasAnyMatch diag))


gravity_then_refill :: Assertion
gravity_then_refill = do
  let b =
        [ [ if c == 0
              then if r >= 5 then mkGem C1 else mkGem (toEnum (r `mod` 5))
              else mkGem C5
          | c <- [0 .. boardSize - 1]
          ]
        | r <- [0 .. boardSize - 1]
        ]
  assertBool "has match" (hasAnyMatch b)
  let (mb, n) = clearMatches b
  assertBool "cleared >= 3" (n >= 3)
  let fallen = applyGravity mb
      colsOk =
        all
          ( \c ->
              let col = map (!! c) fallen
                  (holes, rest) = span (== Nothing) col
              in all (/= Nothing) rest && length holes + length rest == boardSize
          )
          [0 .. boardSize - 1]
  assertBool "gravity: holes on top" colsOk
  let g = mkStdGen 7
      (b', _) = refill g fallen
  assertEqual "rows" boardSize (length b')
  assertBool "full rows" (all ((== boardSize) . length) b')

cascade_until_stable :: Assertion
cascade_until_stable = do
  let g = mkStdGen 1
      (b0, g1) = randomBoard g
      (b1, cleared, g2) = runCascade g1 b0
  assertBool "stable" (not (hasAnyMatch b1))
  assertBool "stepCascade Nothing" (isNothing (stepCascade g2 b1))
  if hasAnyMatch b0
    then assertBool "cleared > 0" (cleared > 0)
    else assertEqual "no clear" (0 :: Int) cleared

outcome_moves_or_score :: Assertion
outcome_moves_or_score = do
  let gs0 = newGame defaultConfig 42
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "have mover"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      case out of
        MoveApplied gained -> do
          assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)
          assertEqual "score" (gsScore gs0 + gained) (gsScore gs1)
          assertBool "gained > 0" (gained > 0)
        Won s -> assertBool "won" (goalMet (gsGoal gs1) s (gsCollected gs1))
        LevelClear s _ -> assertBool "level" (goalMet (gsGoal gs1) s (gsCollected gs1))
        Lost _ -> gsMoves gs1 @?= 0
        other -> assertFailure ("unexpected: " ++ show other)

  let cfgW = GameConfig { cfgMoves = 5, cfgGoal = GoalScore 1 }
      gsW0 = newGameAtLevel (length allLevels - 1) cfgW 42
  case findMatchPair (gsBoard gsW0) of
    Nothing -> assertFailure "win mover"
    Just (p1, p2) -> do
      let (_, outW) = trySwap p1 p2 gsW0
      case outW of
        Won _ -> pure ()
        other -> assertFailure ("expected Won on last level, got " ++ show other)

  let cfgL = GameConfig { cfgMoves = 1, cfgGoal = GoalScore 999999 }
      gsL0 = newGame cfgL 42
  case findMatchPair (gsBoard gsL0) of
    Nothing -> assertFailure "lose mover"
    Just (p1, p2) -> do
      let (gsL1, outL) = trySwap p1 p2 gsL0
      case outL of
        Lost _ -> gsMoves gsL1 @?= 0
        Won _ -> assertFailure "should not win"
        LevelClear _ _ -> assertFailure "should not clear"
        other -> assertFailure ("expected Lost, got " ++ show other)

qc_findMatches_ge3 :: Property
qc_findMatches_ge3 =
  forAll (choose (0, boardSize - 1)) $ \r ->
    forAll (choose (0, boardSize - 3)) $ \c0 ->
      let fill = mkGem C5
          b0 = replicate boardSize (replicate boardSize fill)
          row =
            [ if c >= c0 && c < c0 + 3 then mkGem C1 else mkGem C5
            | c <- [0 .. boardSize - 1]
            ]
          b = take r b0 ++ [row] ++ drop (r + 1) b0
          ms = findMatches b
      in (r, c0) `elem` ms
           && (r, c0 + 1) `elem` ms
           && (r, c0 + 2) `elem` ms

-- | 4-in-a-row spawns a Line special.
special_line_from_4 :: Assertion
special_line_from_4 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row2 = map mkGem [C1, C1, C1, C1, C2, C3, C4, C2]
      b = take 2 b0 ++ [row2] ++ drop 3 b0
      (mb, n) = clearMatches b
  assertBool "cleared" (n >= 4)
  let specials =
        [ (r, c, cellKind cell)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [ (mb !! r) !! c ]
        , cellKind cell /= Normal
        ]
  assertBool ("spawned line: " ++ show specials) $
    any (\(_, _, k) -> k == LineH || k == LineV) specials

-- | 5-in-a-row spawns a Bomb.
special_bomb_from_5 :: Assertion
special_bomb_from_5 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row1 = map mkGem [C1, C1, C1, C1, C1, C2, C3, C2]
      b = take 1 b0 ++ [row1] ++ drop 2 b0
      (mb, _) = clearMatches b
      bombs =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [ (mb !! r) !! c ]
        , cellKind cell == Bomb
        ]
  assertBool ("bomb spawned: " ++ show bombs) (not (null bombs))

hint_finds_move :: Assertion
hint_finds_move = do
  let gs0 = newGame defaultConfig 7
      (gs1, h) = applyHint gs0
  case h of
    Nothing -> assertFailure "expected a hint on a fresh board"
    Just (p1, p2) -> do
      gsHint gs1 @?= Just (p1, p2)
      assertBool "hint creates match" (hasAnyMatch (swapCells (gsBoard gs0) p1 p2))

undo_restores :: Assertion
undo_restores = do
  let gs0 = newGame defaultConfig 11
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need move"
    Just (p1, p2) -> do
      let (gs1, _) = trySwap p1 p2 gs0
      case undoMove gs1 of
        Nothing -> assertFailure "undo should work"
        Just gsU -> do
          gsBoard gsU @?= gsBoard gs0
          gsScore gsU @?= gsScore gs0
          gsMoves gsU @?= gsMoves gs0
          gsCollected gsU @?= gsCollected gs0

-- | Stuck board (no valid adjacent swap) is reshuffled to a playable stable board.
shuffle_when_no_moves :: Assertion
shuffle_when_no_moves = do
  let stuck = stuckNoMoveBoard
  assertBool "fixture has no match" (not (hasAnyMatch stuck))
  assertBool "fixture has no valid move" (not (hasValidMove stuck))
  let gs0 =
        (newGame defaultConfig 1)
          { gsBoard = stuck
          , gsGen = mkStdGen 999
          , gsOver = Nothing
          , gsHint = Nothing
          , gsShuffled = False
          }
      gs1 = ensurePlayable gs0
  assertBool "did shuffle" (gsShuffled gs1)
  assertBool "after: no initial match" (not (hasAnyMatch (gsBoard gs1)))
  assertBool "after: has valid move" (hasValidMove (gsBoard gs1))
  -- Force shuffleGame also yields playable
  let gs2 = shuffleGame gs0
  assertBool "shuffleGame playable" (hasValidMove (gsBoard gs2))
  assertBool "shuffleGame stable" (not (hasAnyMatch (gsBoard gs2)))

-- | Cyclic (r+c) mod 5 board: stable and no valid adjacent swap.
stuckNoMoveBoard :: Board
stuckNoMoveBoard =
  [ [ mkGem (toEnum ((r + c) `mod` 5))
    | c <- [0 .. boardSize - 1]
    ]
  | r <- [0 .. boardSize - 1]
  ]

playable_board_stable_and_has_move :: Assertion
playable_board_stable_and_has_move = do
  mapM_
    ( \seed -> do
        let (b, _) = randomPlayableBoard (mkStdGen seed)
        assertBool ("stable " ++ show seed) (not (hasAnyMatch b))
        assertBool ("has move " ++ show seed) (hasValidMove b)
    )
    [0 .. 30 :: Int]

-- | Multi-wave cascade scores with increasing wave multiplier.
combo_wave_scoring :: Assertion
combo_wave_scoring = do
  assertEqual "wave1" (30 :: Int) (scoreForWave 1 3)
  assertEqual "wave2" (60 :: Int) (scoreForWave 2 3)
  assertEqual "wave3" (90 :: Int) (scoreForWave 3 3)
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C1, C2, C3, C4, C2, C3]
      b = take 3 b0 ++ [row3] ++ drop 4 b0
      (_, cells, scored, combo, tallies, _) = runCascadeScored Nothing (mkStdGen 3) b
  assertBool "cleared some" (cells >= 3)
  assertBool "combo >= 1" (combo >= 1)
  assertEqual "score matches waves aggregate lower bound" True (scored >= scoreForWave 1 3)
  let c1n = fromMaybe 0 (lookup C1 tallies)
  assertBool "tallied some C1" (c1n >= 3)

--------------------------------------------------------------------------------
-- Color-collect goals
--------------------------------------------------------------------------------

-- | Clearing gems of the target color increments gsCollected.
collect_goal_progress :: Assertion
collect_goal_progress = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = GoalCollect C1 100 }
      -- Build a board with a clearable C1 triple at row 3, rest C5 (won't make C1 match elsewhere)
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      -- Place C1 C1 C2 and an adjacent C1 so swap creates three C1
      -- row3: C1 C1 C2 C3 C4 C5 C2 C3  — swap (3,2)=C2 with (3,1) wouldn't help
      -- Better: put C1 at (3,0)(3,1)(3,3) and C2 at (3,2); swap (3,2)<->something...
      -- Simpler: board already has match of three C1 — but then newGame uses random board.
      -- Override board after newGame, then force a matching swap.
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = take 3 b0 ++ [row3] ++ drop 4 b0
      -- Swap (3,2)=C2 with (3,3)=C1 → row becomes C1 C1 C1 C2 ... match!
      gs0 =
        (newGame cfg 55)
          { gsBoard = board
          , gsCollected = 0
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    MoveApplied _ -> pure ()
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("expected applied/terminal, got " ++ show other)
  assertBool
    ("collected C1 increased, got " ++ show (gsCollected gs1))
    (gsCollected gs1 >= 3)

-- | Reaching collect count triggers LevelClear (or Won on last level).
collect_goal_clears_level :: Assertion
collect_goal_clears_level = do
  let cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalCollect C1 3 }
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = take 3 b0 ++ [row3] ++ drop 4 b0
      -- Level 0 so LevelClear (not Won)
      gs0 =
        (newGameAtLevel 0 cfg 55)
          { gsBoard = board
          , gsCollected = 0
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    LevelClear _ next -> do
      assertEqual "next level" (1 :: Int) next
      assertBool "collected enough" (gsCollected gs1 >= 3)
      assertBool "gsOver set" (gsOver gs1 == Just out)
    Won _ -> assertFailure "should LevelClear on non-last level"
    other -> assertFailure ("expected LevelClear, got " ++ show other ++ " collected=" ++ show (gsCollected gs1))


-- | One legal clear with moves=1 but GoalCollect unmet → Lost; collected < target.
collect_goal_lose_on_moves :: Assertion
collect_goal_lose_on_moves = do
  let cfg = GameConfig { cfgMoves = 1, cfgGoal = GoalCollect C1 99 }
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = take 3 b0 ++ [row3] ++ drop 4 b0
      gs0 =
        (newGameAtLevel 0 cfg 55)
          { gsBoard = board
          , gsCollected = 0
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    Lost s -> do
      assertBool "score non-negative" (s >= 0)
      assertBool
        ("collected < 99, got " ++ show (gsCollected gs1))
        (gsCollected gs1 < 99)
      assertBool "gsOver is Lost" (gsOver gs1 == Just out)
      assertEqual "moves spent" (0 :: Int) (gsMoves gs1)
    other ->
      assertFailure
        ("expected Lost, got " ++ show other
           ++ " collected=" ++ show (gsCollected gs1)
           ++ " moves=" ++ show (gsMoves gs1))

-- | Campaign table mixes GoalScore and GoalCollect stages.
level_table_mixes_collect :: Assertion
level_table_mixes_collect = do
  let goals = map lvlGoal allLevels
      scores = [g | g@GoalScore {} <- goals]
      collects = [g | g@GoalCollect {} <- goals]
  assertBool "has score levels" (not (null scores))
  assertBool "has collect levels" (not (null collects))
  assertBool "at least 5 levels" (length allLevels >= 5)
  -- Named collect stages present
  let names = map lvlName allLevels
  assertBool "has 采红" ("采红" `elem` names)
  assertBool "has 采蓝" ("采蓝" `elem` names)
  assertBool "has 采绿" ("采绿" `elem` names)

-- | Score-goal levels do not increment gsCollected (stays 0).
score_goal_ignores_collect :: Assertion
score_goal_ignores_collect = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = GoalScore 99999 }
      gs0 = newGame cfg 42
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need move"
    Just (p1, p2) -> do
      let (gs1, _) = trySwap p1 p2 gs0
      gsCollected gs1 @?= 0
