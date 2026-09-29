-- | 元素框架的门面：类型、注册表与查询钩子、内置定义、事件词汇。
--
-- 新增一种元素：写一个 ElementDef（通常从 baseDef 起步），register 进注册表，
-- 用各 *With 入口（trySwapWith / resolveMoveWith / cascade*With …）跑；主流程不用改。
-- 步骤清单见 docs/architecture.md「元素框架与事件」。
module Match3.Element
  ( module Match3.Element.Types
  , module Match3.Element.Registry
  , module Match3.Element.Builtin
  , module Match3.Element.Event
  ) where

import Match3.Element.Builtin
import Match3.Element.Event
import Match3.Element.Registry
import Match3.Element.Types
