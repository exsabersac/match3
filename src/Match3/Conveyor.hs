-- | 传送带：环路径上格内容循环前移一格。移位后的连锁/收饼由 Game + Board.runPostBeltCascade 处理。
module Match3.Conveyor
  ( Belt
  , shiftBelt
  , shiftBelts
  ) where

import Data.List (foldl')
import Match3.Types

-- | Cyclic belt path (length >= 2). Cells advance toward higher indices;
-- the last cell wraps to the first.
type Belt = [Pos]

-- | Rotate cells one step along the belt (forward).
shiftBelt :: Board -> Belt -> Board
shiftBelt b ps
  | length ps < 2 = b
  | otherwise =
      let cells = map (at b) ps
          -- content moves forward: new[i] = old[i-1], new[0] = old[last]
          rotated = last cells : init cells
      in foldl' (\board (p, cell) -> set board p cell) b (zip ps rotated)
  where
    at board (r, c) = (board !! r) !! c
    set board (r, c) v =
      take r board
        ++ [take c row ++ [v] ++ drop (c + 1) row]
        ++ drop (r + 1) board
      where
        row = board !! r

-- | Apply every belt in order (later belts see earlier shifts).
shiftBelts :: Board -> [Belt] -> Board
shiftBelts = foldl' shiftBelt
