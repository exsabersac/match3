-- | 元素框架的门面：类型、注册表与查询钩子、内置元素、事件词汇、一局的关卡级元素（第 7 刀）、
-- 规则表的解释器与补子策略（第 8 刀：Element.Special、Board.Refill）。
--
-- 新增一种元素：定义一个类型，写能力类的 instance（Match3.Element.Ability 的 Cellular / Matchable / Hittable /
-- Movable / Countable / Renders，只覆盖和普通棋子不同的方法；占格障碍 / 固定格用 @deriving … via Obstacle@ /
-- @via Fixed@ 原型包）和一个类型级 instance（本体 Match3.Element.Kind 的 Kind、叠层 Match3.Element.Layer 的
-- Layer、地面层 GroundKind），用 kindDef \@T / layerDef \@T / groundDef \@T register 进注册表，再用各 *With 入口
-- （trySwapWith / resolveMoveWith / cascade*With …）跑；主流程不用改。
-- 能力类、Kind / Layer 与关卡级元素类单独 import Match3.Element.Ability / Kind / Layer / Class（名字较通用，
-- 不经门面再导出）。
-- 步骤清单见 docs/architecture.md「元素框架与事件」。
module Match3.Element
  ( module Match3.Element.Types
  , module Match3.Element.World
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
import Match3.Element.World
import Match3.Element.Special
import Match3.Element.Types
