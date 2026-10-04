-- | 连锁的关卡级钩子（第 7 刀 7a）：Board 层（Gravity / Cascade）不再直接收关卡状态（第 3 刀留下的
-- @[Ufo]@ / 传送门对参数），只收这一个钩子记录；钩子背后是哪些关卡级元素、各自的状态是什么，Board 层不看。
--
-- 钩子由 Match3.Element.Level.levelHooksWith 从注册表 + GameState.gsLevelElems 造出：每个钩子在对应的
-- 流水线节拍上调关卡级机制的节拍方法（Match3.Element.Mechanic）。会推进状态的钩子（onAbsorb）交回推进后的钩子，连锁把它一路传下去，
-- Game 层最后从 'hookLevel' 取回推进后的关卡级元素。
--
-- 依赖：Board.Grid（MBoard）、Board.Refill（补子策略，第 8 刀）、Element.Class（SomeMechanic，只当不透明的载荷）。
module Match3.Board.Hooks
  ( LevelHooks(..)
  , noHooks
  ) where

import Match3.Board.Grid (MBoard)
import Match3.Board.Refill (RefillPolicy)
import Match3.Element.Mechanic (SomeMechanic)
import Match3.Types

-- | 连锁在各节拍上调用的关卡级钩子。
data LevelHooks = LevelHooks
  { onSettle  :: MBoard -> MBoard
    -- ^ 沉降节拍（每次沉降、下落并收边之后）：传送（内置 = 传送门）。只读状态、不推进。
  , onAbsorb  :: Board -> ([Pos], LevelHooks)
    -- ^ 补子之后的整轮吸收（内置 = 飞碟）：吸走的格（空 = 本轮没有吸收轮）与推进后的钩子。
  , hookRefill :: Maybe RefillPolicy
    -- ^ 补子节拍（第 8 刀）：关卡级元素换的补子策略（Mechanic 的 refillPolicy 回复）；
    -- Nothing = 没有元素换，用注册表的策略（Gravity.activeRefill）。
  , hookLevel :: [SomeMechanic]
    -- ^ 钩子背后的关卡级元素的当前状态（Game 层取回写进 gsLevelElems；Board 层不读）。
  }

-- | 没有任何关卡级元素的钩子：不传送、不吸收、不换补子策略（测试与只看盘面的调用方用）。
noHooks :: LevelHooks
noHooks = LevelHooks {onSettle = id, onAbsorb = const ([], noHooks), hookRefill = Nothing, hookLevel = []}
