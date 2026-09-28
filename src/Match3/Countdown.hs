-- | Countdown bombs (开心消消乐倒计时炸弹): colored matchable timers.
-- Tick -1 after each successful move; at 0 explode in a 3×3; matching/specials disarm.
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
spawnCountdown b (r, c) col n =
  take r b
    ++ [take c row ++ [mkCountdown col n] ++ drop (c + 1) row]
    ++ drop (r + 1) b
  where
    row = b !! r

-- | Decrement every countdown by 1 (floor at 0).
tickCountdowns :: Board -> Board
tickCountdowns = map (map tickCell)
  where
    tickCell (Countdown col n) = Countdown col (max 0 (n - 1))
    tickCell x = x

-- | Positions whose countdown has reached 0 (about to explode).
countdownsAtZero :: Board -> [Pos]
countdownsAtZero b =
  [ (r, c)
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , case (b !! r) !! c of
      Countdown _ 0 -> True
      _ -> False
  ]

-- | 3×3 blast centered at a countdown that hit zero.
explodeRadius :: Pos -> [Pos]
explodeRadius (r, c) =
  [ (rr, cc)
  | rr <- [r - 1 .. r + 1]
  , cc <- [c - 1 .. c + 1]
  , rr >= 0
  , rr < boardSize
  , cc >= 0
  , cc < boardSize
  ]

-- | Union of explosion seeds for all zero countdowns.
explodeSeedsFor :: Board -> [Pos]
explodeSeedsFor b = nub (concatMap explodeRadius (countdownsAtZero b))
