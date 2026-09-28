-- | Stone blockers: occupy a cell, never match, block swaps, cleared by adjacent clears.
module Match3.Obstacles
  ( swapBlockedByStone
  , orthoNeighbors
  , stonesAdjacentTo
  , withAdjacentStones
  ) where

import Data.List (nub)
import Match3.Types (Board, Pos, boardSize, isStone)

-- | True if either swap endpoint is a stone (cannot swap stones).
swapBlockedByStone :: Board -> Pos -> Pos -> Bool
swapBlockedByStone b p1 p2 =
  isStone (at b p1) || isStone (at b p2)
  where
    at board (r, c) = (board !! r) !! c

-- | Up / down / left / right neighbors (may be out of bounds).
orthoNeighbors :: Pos -> [Pos]
orthoNeighbors (r, c) =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | Stone positions orthogonally adjacent to any of the given cleared positions.
stonesAdjacentTo :: Board -> [Pos] -> [Pos]
stonesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBoard p
    , isStone (at b p)
    ]
  where
    at board (r, c) = (board !! r) !! c
    inBoard (r, c) =
      r >= 0 && r < boardSize && c >= 0 && c < boardSize

-- | Extend a clear-set with any stones sitting next to cleared cells.
withAdjacentStones :: Board -> [Pos] -> [Pos]
withAdjacentStones b seeds = nub (seeds ++ stonesAdjacentTo b seeds)
