-- | 倒计时炸弹：可按色匹配的计时器；tick −1；归零产生 3×3 爆炸种子。
-- 爆炸后的连锁由 Board.Cascade.cascadeCountdowns 调用种子连锁完成。
module Match3.Countdown
  ( mkCountdown
  , isCountdown
  , countdownTurns
  , spawnCountdown
  , tickCountdowns
  , countdownsAtZero
  , explodeRadius
  , explodeSeedsFor
  ) where

import Data.List (nub)
import Match3.Types

-- | Place a countdown bomb on the board (level / test spawn helper).
spawnCountdown :: Board -> Pos -> Color -> Int -> Board
spawnCountdown b p col n = boardSet b p (mkCountdown col n)

-- | Decrement every countdown by 1 (floor at 0).
tickCountdowns :: Board -> Board
tickCountdowns = mapBoard tickCell
  where
    tickCell (Countdown col n) = Countdown col (max 0 (n - 1))
    tickCell x = x

-- | Positions whose countdown has reached 0 (about to explode).
countdownsAtZero :: Board -> [Pos]
countdownsAtZero b =
  [ p
  | p <- boardPositions b
  , case boardAt b p of
      Countdown _ 0 -> True
      _ -> False
  ]

-- | 3×3 blast centered at a countdown that hit zero (裁到盘内).
explodeRadius :: Board -> Pos -> [Pos]
explodeRadius b (r, c) =
  [ (rr, cc)
  | rr <- [r - 1 .. r + 1]
  , cc <- [c - 1 .. c + 1]
  , rr >= 0 && rr < boardNRows b
  , cc >= 0 && cc < boardNCols b
  ]

-- | Union of explosion seeds for all zero countdowns.
explodeSeedsFor :: Board -> [Pos]
explodeSeedsFor b = nub (concatMap (explodeRadius b) (countdownsAtZero b))
