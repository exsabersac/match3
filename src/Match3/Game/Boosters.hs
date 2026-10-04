-- | 道具：锤子 / 自由交换 / 十字清除。每种道具一个 resolve* 函数（校验 + 起手方式），
-- 结算与回放都是它的投影（use* = 结果，trace* = 回放脚本），公共结算见 Match3.Game.Resolve。
--
-- 依赖：Resolve、State、Trace、Match3.Board.*、Match3.Boosters（十字种子几何）、元素注册表（挡交换 / 锤子免疫 / 成对交换规则 = 彩虹与特殊合成）。
-- 不变量：道具不耗步、不推进倒计时、没有皮带 / 蜗牛，步末只有蔓延；锤子对免疫格不扣次数（NoMatch）。
-- 类型层：锤子 / 十字从原盘起手（Stage 'Full，起手只能是 OpenSeeds），自由交换从交换后的盘起手（Stage 'Swapped），
-- 由 Game.Resolve 的 StartPhase 检查。
-- 护栏 trace_boosters_final_equal_result、trace_end_steps_boosters_replay。
module Match3.Game.Boosters
  ( traceFreeSwap
  , traceHammer
  , traceCrossClear
  , useHammer
  , useFreeSwap
  , useCrossClear
  , resolveHammer
  , resolveFreeSwap
  , resolveCrossClear
  , resolveHammerWith
  , resolveFreeSwapWith
  , resolveCrossClearWith
  ) where

import Match3.Board.Grid (inBounds, getCell)
import Match3.Board.Match (hasAnyMatchWith)
import Match3.Board.Phase (fullStage, stageBoard, swapStage)
import Match3.Element.Builtin (defaultWorld)
import Match3.Element.World (World, hitImmuneWith, swapBlockedWith, swapOpeningWith)
import Match3.Boosters (crossClearSeeds)
import Match3.Types
import Match3.Game.Resolve
import Match3.Game.State
import Match3.Game.Trace

fst3 :: (a, b, c) -> (a, b)
fst3 (a, b, _) = (a, b)

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | 锤子：花一次清掉一格（种子起手连锁），不耗步。免疫格拒绝且不扣次数。
resolveHammer :: Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveHammer = resolveHammerWith defaultWorld

-- | resolveHammer（指定注册表）。
resolveHammerWith :: World -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveHammerWith reg p gs
  | Just t <- gsOver gs = (gs, fromTerminal t, emptyTrace gs)
  | gsHammers gs <= 0 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (inBounds (gsBoard gs) p) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | hitImmuneWith reg (getCell (gsBoard gs) p) = (rejectMove gs, NoMatch, emptyTrace gs)
  | otherwise = resolveMoveWith reg SKindHammer (fullStage (gsBoard gs)) (OpenSeeds Nothing [p]) gs

-- | 自由交换：花一次交换任意两格（不必相邻），成消才结算；起手规则同玩家交换。
resolveFreeSwap :: Pos -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveFreeSwap = resolveFreeSwapWith defaultWorld

-- | resolveFreeSwap（指定注册表）。
resolveFreeSwapWith :: World -> Pos -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveFreeSwapWith reg p1 p2 gs
  | Just t <- gsOver gs = (gs, fromTerminal t, emptyTrace gs)
  | gsFreeSwaps gs <= 0 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (inBounds board0 p1 && inBounds board0 p2) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | p1 == p2 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | swapBlockedWith reg board0 p1 p2 = (rejectMove gs, NoMatch, emptyTrace gs)
  | pairRule == Nothing && not (hasAnyMatchWith reg swapped) = (rejectMove gs, NoMatch, emptyTrace gs)
  | otherwise = resolveMoveWith reg SKindFreeSwap swappedS opening gs
  where
    board0 = gsBoard gs
    swappedS = swapStage p1 p2 (fullStage board0)
    swapped = stageBoard swappedS
    -- 成对交换规则（同 Move.resolveSwapWith）
    pairRule = swapOpeningWith reg board0 swapped p1 p2
    opening = maybe (OpenMatch (Just p2)) (OpenSeeds (Just p2)) pairRule

-- | 十字清除：花一次清掉一格所在的整行 + 整列（种子起手连锁），不耗步。
resolveCrossClear :: Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveCrossClear = resolveCrossClearWith defaultWorld

-- | resolveCrossClear（指定注册表）。
resolveCrossClearWith :: World -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveCrossClearWith reg p gs
  | Just t <- gsOver gs = (gs, fromTerminal t, emptyTrace gs)
  | gsCrossClears gs <= 0 = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (inBounds (gsBoard gs) p) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | otherwise = resolveMoveWith reg SKindCross (fullStage (gsBoard gs)) (OpenSeeds Nothing (crossClearSeeds (gsBoard gs) p)) gs

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
