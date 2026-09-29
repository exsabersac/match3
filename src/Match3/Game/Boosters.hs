-- | 道具：锤子 / 自由交换 / 十字清除。每种道具一个 resolve* 函数（校验 + 起手方式），
-- 结算与回放都是它的投影（use* = 结果，trace* = 回放脚本），公共结算见 Match3.Game.Resolve。
--
-- 依赖：Resolve、State、Trace、Match3.Board、Match3.Boosters（十字种子几何）、Obstacles、Rainbow / Combos。
-- 不变量：道具不耗步、不推进倒计时、没有皮带 / 蜗牛，步末只有蔓延；锤子对免疫格不扣次数（NoMatch）。
-- 护栏 trace_boosters_final_equal_result、trace_end_steps_boosters_replay。
module Match3.Game.Boosters
  ( traceFreeSwap
  , traceHammer
  , traceCrossClear
  , hammerImmune
  , useHammer
  , useFreeSwap
  , useCrossClear
  , resolveHammer
  , resolveFreeSwap
  , resolveCrossClear
  ) where

import Match3.Board (hasAnyMatch, inBounds, swapCells, getCell)
import Match3.Obstacles (swapBlockedByStone)
import Match3.Combos (isSpecialCombo, comboClearSeeds)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Boosters (crossClearSeeds)
import Match3.Types
import Match3.Game.Resolve
import Match3.Game.State
import Match3.Game.Trace

-- | 直接种子打不动的格（chipIceOnClear 原样保留）：果汁机 / 蜗牛 / 染色瓶 / 魔法帽 / 饼干。
hammerImmune :: Cell -> Bool
hammerImmune c = isMaker c || isSnail c || isBottle c || isMagicHat c || isCookie c

fst3 :: (a, b, c) -> (a, b)
fst3 (a, b, _) = (a, b)

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | 锤子：花一次清掉一格（种子起手连锁），不耗步。免疫格拒绝且不扣次数。
resolveHammer :: Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveHammer p gs
  | Just o <- gsOver gs = (gs, o, emptyTrace gs)
  | gsHammers gs <= 0 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (inBounds p) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | hammerImmune (getCell (gsBoard gs) p) = (rejectMove gs, NoMatch, emptyTrace gs)
  | otherwise = resolveMove KindHammer (gsBoard gs) (OpenSeeds Nothing [p]) gs

-- | 自由交换：花一次交换任意两格（不必相邻），成消才结算；起手规则同玩家交换。
resolveFreeSwap :: Pos -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveFreeSwap p1 p2 gs
  | Just o <- gsOver gs = (gs, o, emptyTrace gs)
  | gsFreeSwaps gs <= 0 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (inBounds p1 && inBounds p2) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | p1 == p2 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | swapBlockedByStone board0 p1 p2 = (rejectMove gs, NoMatch, emptyTrace gs)
  | not rainbow && not specialCombo && not (hasAnyMatch swapped) = (rejectMove gs, NoMatch, emptyTrace gs)
  | otherwise = resolveMove KindFreeSwap swapped opening gs
  where
    board0 = gsBoard gs
    swapped = swapCells board0 p1 p2
    rainbow = isRainbowSwap board0 p1 p2
    specialCombo = isSpecialCombo board0 p1 p2
    opening
      | rainbow = OpenSeeds (Just p2) (rainbowClearSeeds swapped p1 p2)
      | specialCombo = OpenSeeds (Just p2) (comboClearSeeds swapped p1 p2)
      | otherwise = OpenMatch (Just p2)

-- | 十字清除：花一次清掉一格所在的整行 + 整列（种子起手连锁），不耗步。
resolveCrossClear :: Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveCrossClear p gs
  | Just o <- gsOver gs = (gs, o, emptyTrace gs)
  | gsCrossClears gs <= 0 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (inBounds p) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | otherwise = resolveMove KindCross (gsBoard gs) (OpenSeeds Nothing (crossClearSeeds p)) gs

-- | 锤子（结算结果）。
useHammer :: Pos -> GameState -> (GameState, Outcome)
useHammer p = fst3 . resolveHammer p

-- | 自由交换（结算结果）。
useFreeSwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
useFreeSwap p1 p2 = fst3 . resolveFreeSwap p1 p2

-- | 十字清除（结算结果）。
useCrossClear :: Pos -> GameState -> (GameState, Outcome)
useCrossClear p = fst3 . resolveCrossClear p

-- | 锤子的回放脚本。
traceHammer :: Pos -> GameState -> MoveTrace
traceHammer p = thd3 . resolveHammer p

-- | 自由交换的回放脚本。
traceFreeSwap :: Pos -> Pos -> GameState -> MoveTrace
traceFreeSwap p1 p2 = thd3 . resolveFreeSwap p1 p2

-- | 十字清除的回放脚本。
traceCrossClear :: Pos -> GameState -> MoveTrace
traceCrossClear p = thd3 . resolveCrossClear p
