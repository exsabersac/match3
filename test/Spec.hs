{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Maybe (isNothing)
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
        Won s -> assertBool "won" (s >= gsTarget gs1)
        LevelClear s _ -> assertBool "level" (s >= gsTarget gs1)
        Lost _ -> gsMoves gs1 @?= 0
        other -> assertFailure ("unexpected: " ++ show other)

  let cfgW = GameConfig { cfgMoves = 5, cfgTarget = 1 }
      gsW0 = newGameAtLevel (length allLevels - 1) cfgW 42
  case findMatchPair (gsBoard gsW0) of
    Nothing -> assertFailure "win mover"
    Just (p1, p2) -> do
      let (_, outW) = trySwap p1 p2 gsW0
      case outW of
        Won _ -> pure ()
        other -> assertFailure ("expected Won on last level, got " ++ show other)

  let cfgL = GameConfig { cfgMoves = 1, cfgTarget = 999999 }
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
      -- Row 2: four C1 then others — but that's already a match of 4
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
