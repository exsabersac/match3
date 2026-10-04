-- | 玩家交换：trySwap（= runMove）与回放 traceSwap。两者都是 resolveSwap 的投影：
-- 同一次计算同时产出新状态、结局与回放脚本（Match3.Game.Resolve.resolveMove），天然一致。
--
-- 依赖：Resolve、State、Trace、Match3.Board.*、元素注册表（挡交换、成对交换规则 = 彩虹 / 特殊合成的起手种子）。
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

import Match3.Board.Grid (inBounds, adjacent)
import Match3.Board.Match (hasAnyMatchWith)
import Match3.Board.Phase (fullStage, stageBoard, swapStage)
import Data.Maybe (isNothing)
import Match3.Element.Builtin (defaultWorld)
import Match3.Element.Level (morphIn)
import Match3.Element.Mechanic (Morph(..))
import Match3.Element.World (World, swapBlockedWith, swapOpeningWith)
import Match3.Types
import Match3.Game.Resolve
import Match3.Game.State
import Match3.Game.Trace

-- | 玩家相邻交换：校验 → 选择起手（彩虹 / 特殊合成用种子，否则普通匹配，prefer = p2）→ 公共结算。
-- 拒绝路径：已结束原样返回；越界 / 不相邻 → InvalidSwap；挡交换 / 交换后无匹配 → NoMatch 回滚。
-- 拒绝时清零 gsCombo / gsLastCleared（clearMoveFx / rejectMove），回放脚本为空。
resolveSwap :: Pos -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveSwap = resolveSwapWith defaultWorld

-- | resolveSwap（指定注册表）：挡交换、成消判定与结算都查这张表。
resolveSwapWith :: World -> Pos -> Pos -> GameState -> (GameState, Outcome, MoveTrace)
resolveSwapWith reg p1 p2 gs
  | Just t <- gsOver gs = (gs, fromTerminal t, emptyTrace gs)
  | not (inBounds board0 p1 && inBounds board0 p2) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | not (adjacent p1 p2) = (clearMoveFx gs, InvalidSwap, emptyTrace gs)
  | swapBlockedWith reg board0 p1 p2 = (rejectMove gs, NoMatch, emptyTrace gs)
  | isNothing morph && pairRule == Nothing && not (hasAnyMatchWith reg swapped) = (rejectMove gs, NoMatch, emptyTrace gs)
  | otherwise = resolveMoveWith reg SKindSwap swappedS opening gs
  where
    board0 = gsBoard gs
    -- 交换：Stage 'Full → Stage 'Swapped（类型层的阶段标签，见 Match3.Board.Phase）；
    -- 起手方式 opening 的类型因此是 Opening 'Swapped，三种起手（匹配 / 种子 / 变身）都允许
    swappedS = swapStage p1 p2 (fullStage board0)
    swapped = stageBoard swappedS
    -- 成对交换规则（彩虹取色 / 特殊合成经注册表的 swapRule，按 srOrder 取第一条成立的）
    pairRule = swapOpeningWith reg board0 swapped p1 p2
    -- 交换变身（新玩法 4：关卡级机制的 morph，内置 = 规则开关 rainbow_combos）：先变身再按种子起手，
    -- 变身记成第 0 轮之前的一条步末效果；没人回复 = 原有起手
    morph = morphIn reg (gsLevelElems gs) board0 swapped p1 p2
    opening = case morph of
      Just m
        | null (morphCells m) -> OpenSeeds (Just p2) (morphSeeds m)
        | otherwise -> OpenMorph (Just p2) (EndEffect EvSpread (morphName m) [EndItem s q cell Nothing | (s, q, cell) <- morphCells m]) (morphSeeds m)
      Nothing -> maybe (OpenMatch (Just p2)) (OpenSeeds (Just p2)) pairRule

-- | 玩家相邻交换入口（结算结果）。步骤见 Match3.Game.Resolve。
trySwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
trySwap p1 p2 gs = let (g, o, _) = resolveSwap p1 p2 gs in (g, o)

-- | trySwap（指定注册表；测试专用元素经此接入）。
trySwapWith :: World -> Pos -> Pos -> GameState -> (GameState, Outcome)
trySwapWith reg p1 p2 gs = let (g, o, _) = resolveSwapWith reg p1 p2 gs in (g, o)

-- | trySwap 的别名（冻结 API）。
runMove :: Pos -> Pos -> GameState -> (GameState, Outcome)
runMove = trySwap

-- | trySwap 的逐轮回放脚本（与结算同一次计算）。
traceSwap :: Pos -> Pos -> GameState -> MoveTrace
traceSwap p1 p2 gs = let (_, _, t) = resolveSwap p1 p2 gs in t
