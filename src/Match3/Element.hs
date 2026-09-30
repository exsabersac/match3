-- | 元素框架的门面：类型、注册表与查询钩子、内置元素、事件词汇、一局的关卡级元素（第 7 刀）、
-- 规则表的解释器与补子策略（第 8 刀：Element.Special、Board.Refill）。
--
-- 新增一种元素：定义一个类型，写 Match3.Element.Class 的 Element instance（只写 name / toCell 和
-- caps = blocker [能力 …]，能力简写在 Match3.Element.Caps，其余用缺省），用 customEntry 等构造器 register
-- 进注册表，再用各 *With 入口（trySwapWith / resolveMoveWith / cascade*With …）跑；主流程不用改。
-- 元素类、能力声明与消息单独 import Match3.Element.Class / Match3.Element.Caps（名字较通用，不经门面再导出）。
-- 步骤清单见 docs/architecture.md「元素框架与事件」。
module Match3.Element
  ( module Match3.Element.Types
  , module Match3.Element.Registry
  , module Match3.Element.Builtin
  , module Match3.Element.Event
  , module Match3.Element.Level
  , module Match3.Element.Special
  , module Match3.Board.Refill
  ) where

import Match3.Board.Refill
import Match3.Element.Builtin
import Match3.Element.Event
import Match3.Element.Level
import Match3.Element.Registry
import Match3.Element.Special
import Match3.Element.Types
