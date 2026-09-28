-- | Simplified boosters (开心消消乐道具简版): hammer / free-swap / cross clear.
module Match3.Boosters
  ( clearCellSeeds
  , hammerClearSeeds
  , crossClearSeeds
  ) where

import Data.List (nub)
import Match3.Types

-- | Seeds for hammer: the target cell (specials on it still expand via cascade).
hammerClearSeeds :: Pos -> [Pos]
hammerClearSeeds p = [p]

-- | Alias kept for tests / clarity.
clearCellSeeds :: Pos -> [Pos]
clearCellSeeds = hammerClearSeeds

-- | Cross clear (十字清除): entire row + column through the target cell.
crossClearSeeds :: Pos -> [Pos]
crossClearSeeds (r, c) =
  nub $
    [(r, c') | c' <- [0 .. boardSize - 1]]
      ++ [(r', c) | r' <- [0 .. boardSize - 1]]
