-- | Daily challenge: date-derived seed + fixed goal mix (开心消消乐每日关感).
module Match3.Daily
  ( dailySeed
  , dailyConfig
  , dailyLevel
  , starRating
  ) where

import Match3.Types

-- | Deterministic seed from YYYY-MM-DD (local calendar ints).
dailySeed :: Int -> Int -> Int -> Int
dailySeed year month day =
  year * 10000 + month * 100 + day

-- | Rotate goal flavor by day-of-year-ish hash.
dailyConfig :: Int -> Int -> Int -> GameConfig
dailyConfig year month day =
  let s = dailySeed year month day
      flavor = s `mod` 5
  in case flavor of
       0 -> GameConfig 28 (GoalScore 600)
       1 -> GameConfig 28 (GoalCollect C1 18)
       2 -> GameConfig 28 (GoalCollectMulti [(C2, 10), (C4, 10)])
       3 -> GameConfig 26 (GoalClearStone 6)
       _ -> GameConfig 26 (GoalUfo 8)

dailyLevel :: Int -> Int -> Int -> Level
dailyLevel year month day =
  let cfg = dailyConfig year month day
  in Level
       { lvlIndex = 0
       , lvlName = "每日"
       , lvlMoves = cfgMoves cfg
       , lvlGoal = cfgGoal cfg
       }

-- | Stars from moves left vs starting moves (3 = plenty left, 1 = clutch).
-- Optional 开心消消乐-style clear rating; pure, no API shape change.
-- | Stars from moves left vs starting moves.
-- Tuned: 3★ need ≥40% moves left; 2★ ≥15%; else 1★ (开心消消乐步数三星感).
starRating :: MovesLeft -> MovesLeft -> Int
starRating startMoves left
  | startMoves <= 0 = 1
  | left * 5 >= startMoves * 2 = 3  -- left/start >= 0.4
  | left * 20 >= startMoves * 3 = 2  -- left/start >= 0.15
  | otherwise = 1
