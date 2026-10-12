-- | 元素框架的门面：类型、元素世界与查询钩子、内置元素、事件词汇、一局的关卡级元素（第 7 刀）、
-- 规则表的解释器与补子策略（第 8 刀：Element.Special、Board.Refill）。
--
-- 新增一种本体元素：写一个原型值（Match3.ECS.Archetype 的 'archetype' 名字 列 + 记录更新：
-- aMatchKind / aHitKind / aTallyKind 是配方，aPhysics / aHud 是纯数据，aFace 仍按状态派生显示；
-- 元素特有行为写成 system 放进 aSystems），用 kindDef 注册后再走 *With 入口；主流程不用改。
-- 叠层是 Cover（ShieldFlags + FireSpec + CoverHitSpec + cvSystems）；地面层 GroundArch；关卡机制 Mechanic。
-- 步骤清单见 docs/architecture.md「元素框架与事件」。
module Match3.Element
  ( module Match3.Element.Types
  , module Match3.ECS.Registry
  , module Match3.Element.Builtin
  , module Match3.Element.Event
  , module Match3.Element.Level
  , module Match3.Element.Special
  , module Match3.Board.Refill
  , module Match3.ECS.Stage
  , module Match3.ECS.Archetype
  , module Match3.ECS.Cover
  , module Match3.ECS.Component
  , module Match3.ECS.System
  ) where

import Match3.Board.Refill
import Match3.Element.Builtin
import Match3.Element.Event
import Match3.Element.Level
import Match3.ECS.Registry
import Match3.ECS.Stage
import Match3.ECS.Archetype
import Match3.ECS.Cover
import Match3.ECS.Component
import Match3.ECS.System
import Match3.Element.Special
import Match3.Element.Types
