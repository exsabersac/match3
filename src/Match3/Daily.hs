-- | 每日挑战：YYYY-MM-DD 种子、10 种目标轮换、三星（相对印制步数剩余比例）。
-- 不生成盘面装饰（由 Game.newDailyGame / ensureGoalDecor）；通关结局由 Game 标为 Won。
module Match3.Daily
  ( dailySeed
  , dailyConfig
  , dailyLevel
  , starRating
  ) where

import Match3.Counts (CounterKey(..))
import Match3.Types

-- | Deterministic seed from YYYY-MM-DD (local calendar ints).
dailySeed :: Int -> Int -> Int -> Int
dailySeed year month day =
  year * 10000 + month * 100 + day

-- | Rotate goal flavor by day-of-year-ish hash.
dailyConfig :: Int -> Int -> Int -> GameConfig
dailyConfig year month day =
  let s = dailySeed year month day
      flavor = s `mod` 10
  in case flavor of
       0 -> GameConfig 28 (goalScore 600)
       1 -> GameConfig 28 (goalCollect C1 18)
       2 -> GameConfig 28 (goalColors [(C2, 10), (C4, 10)])
       3 -> GameConfig 26 (goalCount CountStones 6)
       4 -> GameConfig 26 (goalCount CountHoney 6)
       5 -> GameConfig 26 (goalCount CountUfo 8)
       6 -> GameConfig 26 (goalCount CountChests 5)
       7 -> GameConfig 26 (goalCount CountCakes 5)
       8 -> GameConfig 26 (goalCount CountSafes 4)
       _ -> GameConfig 26 (goalCount CountBalloons 6)

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
