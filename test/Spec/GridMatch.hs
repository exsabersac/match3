{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 盘面与匹配：交换回滚、稳定盘面、三连判定、提示、可玩开局。
module Spec.GridMatch
  ( tests
  , inv_move_to_stable
  , inv_no_match_rollback
  , match_line_ge3
  ) where

import Match3.Board.Default (findMatches, hasAnyMatch, hasValidMove)
import Match3.Board.Grid (setCell, swapCells)
import Match3.Board.Random (randomPlayableBoard)
import Match3.Core
import Match3.Game.Level (newGame)
import Match3.Game.Move (trySwap)
import Match3.Types
  ( balloonColor
  , bottleColor
  , cellColor
  , cellKind
  , colorAt
  , flipBack
  , flipFront
  , makerColor
  , mkMaker
  , mkStone
  , numColors
  )
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（平铺进顶层 "match3" 组）。
tests :: [TestTree]
tests =
  [ testCase "inv_no_match_rollback" inv_no_match_rollback
  , testCase "inv_move_to_stable" inv_move_to_stable
  , testCase "match_line_ge3" match_line_ge3
  , testCase "hint_finds_move" hint_finds_move
  , testCase "playable_board_stable_and_has_move" playable_board_stable_and_has_move
  , testCase "cell_accessors_total" cell_accessors_total
  ]

-- | 格子取值函数是总函数：没有颜色 / 种类的格给 Nothing；colorAt 按 allColors 取模（负数也落在范围内）。
cell_accessors_total :: Assertion
cell_accessors_total = do
  assertEqual "gem color" (Just C3) (cellColor (mkGem C3))
  assertEqual "flip front color" (Just C2) (cellColor (mkFlip C2 C4))
  assertEqual "countdown color" (Just C5) (cellColor (Countdown C5 2))
  assertEqual "stone has no color" Nothing (cellColor mkStone)
  assertEqual "custom has no color" Nothing (cellColor (Custom "x" (CustomState 1)))
  assertEqual "gem kind" (Just Bomb) (cellKind (Gem C1 Bomb 0 Nothing))
  assertEqual "countdown kind" (Just Normal) (cellKind (Countdown C1 1))
  assertEqual "cookie has no kind" Nothing (cellKind mkCookie)
  assertEqual "balloon color" (Just C2) (balloonColor (mkBalloon C2))
  assertEqual "not a balloon" Nothing (balloonColor (mkGem C2))
  assertEqual "maker color" (Just C4) (makerColor (mkMaker C4))
  assertEqual "not a maker" Nothing (makerColor mkCookie)
  assertEqual "flip faces" (Just C1, Just C2) (flipFront (mkFlip C1 C2), flipBack (mkFlip C1 C2))
  assertEqual "not a flip" (Nothing, Nothing) (flipFront mkStone, flipBack mkStone)
  assertEqual "bottle color" (Just C3) (bottleColor (mkBottle C3))
  assertEqual "not a bottle" Nothing (bottleColor mkStone)
  assertEqual "colorAt 0..4 = allColors" allColors (map colorAt [0 .. numColors - 1])
  assertEqual "colorAt wraps" (take 12 (cycle allColors)) (map colorAt [0 .. 11])
  assertEqual "colorAt negative" C5 (colorAt (-1))

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
      b = boardFromRows $ take 3 b0 ++ [row3] ++ drop 4 b0
      ms = findMatches b
  assertBool "(3,0)" ((3, 0) `elem` ms)
  assertBool "(3,1)" ((3, 1) `elem` ms)
  assertBool "(3,2)" ((3, 2) `elem` ms)
  let colBoard = boardFromRows $
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
  let stable = boardFromRows $
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


hint_finds_move :: Assertion
hint_finds_move = do
  let gs0 = newGame defaultConfig 7
      (gs1, h) = applyHint gs0
  case h of
    Nothing -> assertFailure "expected a hint on a fresh board"
    Just (p1, p2) -> do
      gsHint gs1 @?= Just (p1, p2)
      assertBool "hint creates match" (hasAnyMatch (swapCells (gsBoard gs0) p1 p2))

playable_board_stable_and_has_move :: Assertion
playable_board_stable_and_has_move = do
  mapM_
    ( \seed -> do
        let (b, _) = randomPlayableBoard (mkStdGen seed)
        assertBool ("stable " ++ show seed) (not (hasAnyMatch b))
        assertBool ("has move " ++ show seed) (hasValidMove b)
    )
    [0 .. 30 :: Int]
