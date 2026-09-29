-- | 对局状态机的外观模块：按原有导出列表重新导出 Match3.Game.* 子模块，
-- 让 Match3.Core 和测试的 import 保持不变。本模块自身不含实现。
--
-- 子模块（依赖单向：State / Tally → Outcome / Shuffle / Trace → Level / Move / Boosters）：
--
-- * "Match3.Game.State"    —— GameState、撤销快照、MoveFx 边沿触发、提示 / 撤销
-- * "Match3.Game.Tally"    —— 结算计数辅助（颜色袋、保险箱、时间精灵、地毯腾空格）
-- * "Match3.Game.Outcome"  —— 目标满足、结局判定、选关解锁、失败提示
-- * "Match3.Game.Shuffle"  —— 保装饰洗牌、自动洗牌 ensurePlayable
-- * "Match3.Game.Level"    —— 开局、战役装饰、每日、重开 / 下一关、步数携带
-- * "Match3.Game.Trace"    —— 回放脚本类型 MoveTrace / EndStep、applyEndEffect、步末记录、效果事件 traceEvents
-- * "Match3.Game.Resolve"  —— 交换与三种道具共用的结算 resolveMove（同时产出 MoveTrace）
-- * "Match3.Game.Move"     —— trySwap（= runMove）与 traceSwap（resolveSwap 的两个投影）
-- * "Match3.Game.Boosters" —— 锤子 / 自由交换 / 十字清除及其回放（resolve* 的投影）
--
-- 编排顺序「主连锁 → 倒计时 → 皮带 → 蔓延 → 蜗牛 → 可选再连锁」见 Match3.Game.Resolve；
-- 每日通关为 Won，不推进战役解锁。
module Match3.Game
  ( GameState(..)
  , newGame
  , newGameAtLevel
  , newDailyGame
  , trySwap
  , runMove
  , MoveFx(..)
  , moveFx
  , clearMoveFx
  , MoveTrace(..)
  , EndStep(..)
  , EndEffect(..)
  , SpreadKind(..)
  , SnailMove(..)
  , applyEndEffect
  , traceSwap
  , traceFreeSwap
  , traceHammer
  , traceCrossClear
  , restart
  , restartLevel
  , checkOutcome
  , undoMove
  , applyHint
  , nextLevel
  , ensurePlayable
  , shuffleGame
  , useHammer
  , useFreeSwap
  , useCrossClear
  , loseHint
  , unlockAfterClear
  , unlockAfterOutcome
  , mapClickJump
    -- * 指定注册表的入口（元素框架；不带 With 的 = 内置注册表）
  , trySwapWith
  , resolveSwapWith
  , resolveHammerWith
  , resolveFreeSwapWith
  , resolveCrossClearWith
  , ensurePlayableWith
  , extractDecorWith
  , decorateLevelWith
  , levelPlacements
    -- * 效果事件
  , EventKind(..)
  , Event(..)
  , traceEvents
  , traceEventsWith
  ) where

import Match3.Game.Boosters
import Match3.Game.Level
import Match3.Game.Move
import Match3.Game.Outcome
import Match3.Game.Shuffle
import Match3.Game.State
import Match3.Game.Trace
