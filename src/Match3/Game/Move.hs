-- | 玩家交换：trySwap（= runMove）与回放 traceSwap。两者都是 resolveSwap 的投影：
-- 同一次计算同时产出新状态、结局与回放脚本（Match3.Game.Resolve.resolveMove），天然一致。
--
-- 依赖：Resolve、State、Trace、Match3.Board.*、元素注册表（挡交换）、Rainbow / Combos（起手种子）。
-- 护栏 trace_swap_final_equals_trySwap、trace_end_steps_replay_to_trySwap_final、trace_rejected_move_is_empty、
-- trace_shuffle_step_replays。
module Match3.Game.Move
  ( trySwap
  , runMove
  , traceSwap
  , resolveSwap
  , resolveSwapWith
  , trySwapWith
  ) where

import Match3.Board.Grid (inBounds, adjacent, swapCells)
import Match3.Board.Match (hasAnyMatchWith)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry, swapBlockedWith)
import Match3.Combos (isSpecialCombo, comboClearSeeds)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types
import Match3.Game.Resolve
import Match3.Game.State
import Match3.Game.Trace

-- | 玩家相邻交换：校验 → 选择起手（彩虹 / 特殊合成用种子，否则普通匹配，prefer = p2）→ 公共结算。
-- 拒绝路径：已结束原样返回；越界 / 不相邻 → InvalidSwap；挡交换 / 交换后无匹配 → NoMatch 回滚。
-- 拒绝时清零 gsCombo / gsLastCleared（clearMoveFx / rejectMove），回放脚本为空。
resolveSwap :: Pos -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveSwap = resolveSwapWith defaultRegistry

-- | resolveSwap（指定注册表）：挡交换、成消判定与结算都查这张表。
resolveSwapWith :: Registry -> Pos -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveSwapWith reg p1 p2 gs
  | Just o <- gsOver gs = (gs, o, emptyTrace gs)
  | not (inBounds p1 && inBounds p2) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (adjacent p1 p2) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | swapBlockedWith reg board0 p1 p2 = (rejectMove gs, NoMatch, emptyTrace gs)
  | not rainbow && not specialCombo && not (hasAnyMatchWith reg swapped) = (rejectMove gs, NoMatch, emptyTrace gs)
  | otherwise = resolveMoveWith reg KindSwap swapped opening gs
  where
    board0 = gsBoard gs
    swapped = swapCells board0 p1 p2
    rainbow = isRainbowSwap board0 p1 p2
    specialCombo = isSpecialCombo board0 p1 p2
    opening
      | rainbow = OpenSeeds (Just p2) (rainbowClearSeeds swapped p1 p2)
      | specialCombo = OpenSeeds (Just p2) (comboClearSeeds swapped p1 p2)
      | otherwise = OpenMatch (Just p2)

-- | 玩家相邻交换入口（结算结果）。步骤见 Match3.Game.Resolve。
trySwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
trySwap p1 p2 gs = let (g, o, _) = resolveSwap p1 p2 gs in (g, o)

-- | trySwap（指定注册表；测试专用元素经此接入）。
trySwapWith :: Registry -> Pos -> Pos -> GameState -> (GameState, Outcome)
trySwapWith reg p1 p2 gs = let (g, o, _) = resolveSwapWith reg p1 p2 gs in (g, o)

-- | trySwap 的别名（冻结 API）。
runMove :: Pos -> Pos -> GameState -> (GameState, Outcome)
runMove = trySwap

-- | trySwap 的逐轮回放脚本（与结算同一次计算）。
traceSwap :: Pos -> Pos -> GameState -> MoveTrace
traceSwap p1 p2 gs = let (_, _, t) = resolveSwap p1 p2 gs in t
