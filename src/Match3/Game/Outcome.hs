{-# LANGUAGE NamedFieldPuns #-}

-- | 目标与结局：目标是否满足、一步之后的结局判定（每日 Won / 战役 LevelClear / 终章 Won / Lost）、
-- 选关解锁与地图跳转、失败提示文案。
--
-- 依赖：State、Match3.Types。
-- 不变量：每日挑战通关为 Won，不推进战役解锁（unlockAfterOutcome）。
module Match3.Game.Outcome
  ( checkOutcome
  , goalSatisfied
  , decideOutcome
  , unlockAfterClear
  , unlockAfterOutcome
  , mapClickJump
  , loseHint
  ) where

import Match3.Counts (CounterKey(..))
import Match3.Types
import Match3.Game.State

-- | 不走一步、只看当前状态的结局（目标满足 → Won，步数用尽 → Lost）。
checkOutcome :: GameState -> Outcome
checkOutcome gs
  | goalSatisfied gs = Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied 0

-- | 当前计数是否满足关卡目标（各目标的判定统一在 Types.goalMetEx；第 4 刀起计数从 gsCounts 按键读）。
goalSatisfied :: GameState -> Bool
goalSatisfied gs =
  goalMetEx
    (gsGoal gs)
    (gsScore gs)
    (gsCollected gs)
    (gsColorBag gs)
    (gsCount CountStones gs)
    (gsCount CountUfo gs)
    (gsCount CountChests gs)
    (gsCount CountHoney gs)
    (gsCount CountBalloons gs)
    (gsCount CountCookies gs)
    (gsCount CountCakes gs)
    (gsCount CountSafes gs)

-- | 目标满足：每日 → Won（不推进战役）；否则 LevelClear 或终章 Won。
-- 步数耗尽 → Lost；否则 MoveApplied。
decideOutcome :: GameState -> Score -> Outcome
decideOutcome gs gained
  | goalSatisfied gs =
      if gsDaily gs
        then Won (gsScore gs)  -- daily complete ≠ campaign LevelClear
        else
          let nextIdx = gsLevel gs + 1
          in if nextIdx < length allLevels
               then LevelClear (gsScore gs) nextIdx
               else Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied gained

-- | Map unlock index after a terminal outcome (LevelClear unlocks through nextIdx).
unlockAfterClear :: Int -> Outcome -> Int
unlockAfterClear reached (LevelClear _ n) = max reached n
unlockAfterClear reached (Won _) = max reached (length allLevels - 1)
unlockAfterClear reached _ = reached

-- | Like unlockAfterClear, but daily challenges never bump campaign map progress
-- (daily Won must not unlock all 38 nodes the way finale Won does).
unlockAfterOutcome :: GameState -> Int -> Outcome -> Int
unlockAfterOutcome gs reached out
  | gsDaily gs = reached
  | otherwise = unlockAfterClear reached out

-- | Map node click: Just li to jump; Nothing = resume/ignore (same level or locked).
mapClickJump :: Int -> Int -> Int -> Maybe Int
mapClickJump curLevel reached clicked
  | clicked < 0 || clicked > reached = Nothing
  | clicked == curLevel = Nothing
  | otherwise = Just clicked

-- | Short tip shown after a Lost outcome (失败提示).
loseHint :: LevelGoal -> String
loseHint (GoalScore t) = "再冲冲分数吧，目标 " ++ show t
loseHint (GoalCollect _ n) = "优先收集该色宝石，目标 " ++ show n ++ " 个"
loseHint (GoalCollectMulti reqs) =
  "兼顾多色收集：" ++ show (length reqs) ++ " 种配额"
loseHint (GoalClearStone n) = "用邻消或特效砸箱子，目标 " ++ show n ++ " 个"
loseHint (GoalChest n) = "邻消打开宝箱，目标 " ++ show n ++ " 个"
loseHint (GoalHoney n) = "邻消砸开蜂蜜罐，目标 " ++ show n ++ " 个"
loseHint (GoalBalloon n) = "用同色邻消戳破气球，目标 " ++ show n ++ " 个"
loseHint (GoalCookie n) = "打通下方让饼干掉到底部，目标 " ++ show n ++ " 个"
loseHint (GoalCake n) = "邻消削掉蛋糕层，目标 " ++ show n ++ " 个"
loseHint (GoalSafe n) = "邻消打开保险箱掉出饼干，目标 " ++ show n ++ " 个"
loseHint (GoalUfo n) = "让飞碟吸走同色宝石，目标 " ++ show n ++ " 个"
loseHint (GoalCarpet n) = "在地毯格上消除宝石以铺地毯，目标 " ++ show n ++ " 格"
loseHint (GoalNamed name n) = "消除目标元素 " ++ name ++ "，目标 " ++ show n ++ " 个"
