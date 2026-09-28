-- | Simplified boosters (开心消消乐道具简版): hammer one cell / free-swap any two.
module Match3.Boosters
  ( clearCellSeeds
  , hammerClearSeeds
  ) where

import Match3.Types

-- | Seeds for hammer: the target cell (specials on it still expand via cascade).
hammerClearSeeds :: Pos -> [Pos]
hammerClearSeeds p = [p]

-- | Alias kept for tests / clarity.
clearCellSeeds :: Pos -> [Pos]
clearCellSeeds = hammerClearSeeds
