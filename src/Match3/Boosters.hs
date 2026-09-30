-- | 道具种子几何（简版）：锤子单格、十字整行+整列。
-- 只返回清除种子位置；扣次数与连锁由 Game.use* 完成。
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
crossClearSeeds :: Board -> Pos -> [Pos]
crossClearSeeds b (r, c) =
  nub $
    [(r, c') | c' <- boardColIndices b]
      ++ [(r', c) | r' <- boardRowIndices b]
