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
import Match3.Element.Builtin (snowBossName)
import Match3.GoalLabel (countLabel)
import Match3.Levels.Campaign (levelCount)
import Match3.Types
import Match3.Game.State

-- | 不走一步、只看当前状态的结局（目标满足 → Won，步数用尽 → Lost）。
checkOutcome :: GameState -> Outcome
checkOutcome gs
  | goalSatisfied gs = Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied 0

-- | 当前计数是否满足关卡目标（第 5 刀：由目标数据统一判定，Match3.Goal.goalMet）。
goalSatisfied :: GameState -> Bool
goalSatisfied = gsGoalMet

-- | 目标满足：每日 → Won（不推进战役）；否则 LevelClear 或终章 Won。
-- 步数耗尽 → Lost；否则 MoveApplied。
decideOutcome :: GameState -> Score -> Outcome
decideOutcome gs gained
  | goalSatisfied gs =
      if gsDaily gs
        then Won (gsScore gs)  -- daily complete ≠ campaign LevelClear
        else
          let nextIdx = gsLevel gs + 1
          in if nextIdx < levelCount
               then LevelClear (gsScore gs) nextIdx
               else Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied gained

-- | Map unlock index after a terminal outcome (LevelClear unlocks through nextIdx).
unlockAfterClear :: Int -> Outcome -> Int
unlockAfterClear reached (LevelClear _ n) = max reached n
unlockAfterClear reached (Won _) = max reached (levelCount - 1)
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

-- | Short tip shown after a Lost outcome (失败提示)。按元素名计数的目标用中文标签（Match3.GoalLabel.countLabel），
-- 不再露出元素内部名（outcome_lose_hint_no_internal_names 逐关核对）。
loseHint :: LevelGoal -> String
loseHint g = case goalView g of
  ViewScore t -> "再冲冲分数吧，目标 " ++ show t
  ViewCollect _ n -> "优先收集该色宝石，目标 " ++ show n ++ " 个"
  ViewCollectMulti reqs -> "兼顾多色收集：" ++ show (length reqs) ++ " 种配额"
  ViewCount k n -> case k of
    CountStones -> "用邻消或特效砸开" ++ countLabel k ++ "，目标 " ++ show n ++ " 个"  -- 碎石（第 8 / 41 / 42 / 44 / 48 关；原写「砸箱子」）
    CountChests -> "邻消打开宝箱，目标 " ++ show n ++ " 个"
    CountHoney -> "邻消砸开蜂蜜罐，目标 " ++ show n ++ " 个"
    CountBalloons -> "用同色邻消戳破气球，目标 " ++ show n ++ " 个"
    CountCookies -> "打通下方让饼干掉到底部，目标 " ++ show n ++ " 个"
    CountCakes -> "邻消削掉蛋糕层，目标 " ++ show n ++ " 个"
    CountSafes -> "邻消打开保险箱掉出饼干，目标 " ++ show n ++ " 个"
    CountUfo -> "让飞碟吸走同色宝石，目标 " ++ show n ++ " 个"
    CountCarpets -> "在地毯格上消除宝石以铺地毯，目标 " ++ show n ++ " 格"
    CountNamed name
      | name == snowBossName -> "用身边的消除和特效打雪怪，目标 " ++ show n ++ " 点血"
      | otherwise -> "消除" ++ countLabel k ++ "，目标 " ++ show n ++ " 个"
    _ -> generic
  ViewOther _ -> generic
  where
    generic = "完成关卡目标，目标 " ++ show (goalTarget g)
