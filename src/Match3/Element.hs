-- | 元素框架的门面：类型、元素世界与查询钩子、内置元素、事件词汇、一局的关卡级元素（第 7 刀）、
-- 规则表的解释器与补子策略（第 8 刀：Element.Special、Board.Refill）。
--
-- 新增一种本体元素：写一个原型值（Match3.ECS.Archetype 的 'archetype' 名字 列 + 记录更新改掉不同的组件：
-- aMatch / aHit / aPhysics / aTally / aFace 由状态派生纯数据组件，Match3.ECS.Component），元素特有的行为写成
-- system 放进 aSystems（邻格用 Match3.Element.Rules 的 nearBy / chipNear，步末用 tickSys / moveSys …），
-- 用 kindDef 原型 register 进注册表，再用各 *With 入口（trySwapWith / resolveMoveWith / cascade*With …）跑；主流程不用改。
-- 叠层是 Match3.Element.Layer 的 Layer 类（ecs-4 改数据），地面层是 GroundKind 记录，关卡级元素是 Mechanic 类（ecs-5 改）。
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
import Match3.ECS.Component
import Match3.ECS.System
import Match3.Element.Special
import Match3.Element.Types
