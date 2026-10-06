-- | 元素作者的写法助手与地面层记录（ecs-3 起本体元素是 Match3.ECS.Archetype 的原型值，不再有 Phase / Kind 类）。
--
-- 地面层元素（不占格，层数在 GameState.gsGround 里）是一条全数据的 'GroundArch' 记录（ecs-5 起）；
-- 叠层见 Match3.ECS.Cover；邻格反应的共享类型见 Match3.Element.Near（这里再导出）。
module Match3.Element.Kind
  ( Reach(..)
  , Nudge(..)
  , DieOrder(..)
  , NearCtx(..)
  , NearOut(..)
    -- * 地面层
  , GroundArch(..)
  , groundArch
    -- * 写法助手
  , customCell
  , fromCustom
  , customPlace
  ) where

import Data.Maybe (fromMaybe)
import Match3.ECS.Component (Hud, Wear(..), Widen, noHud)
import Match3.Element.Near
import Match3.Element.Types
import Match3.Types

-- | 地面层原型（层数在 gsGround 里按名字记）：全是数据，由核心关卡级机制 GroundLayer 与注册表的地面层查询消费。
data GroundArch = GroundArch
  { groundName :: ElementName
  , groundWear :: Wear                  -- ^ 上方格子被消除一次时怎么变（'wearHit' 解释）
  , groundCounter :: Maybe CounterKey   -- ^ 去层时的计数键（只有 CountNamed 并入 gsCounts）
  , groundWiden :: Maybe Widen          -- ^ 本格上的特效引爆时的扩爆规则（内置只有魔法地格；'widenArea' 解释）
  , groundHud :: Hud                    -- ^ 中文名 / 失败提示（只用 hudLabel / hudLoseHint）
  }

-- | 缺省地面层：被消除不变（'Durable'）、不计数、不扩爆、没有中文名。
groundArch :: ElementName -> GroundArch
groundArch n = GroundArch n Durable Nothing Nothing noHud

-- | 自定义本体的格子：@Custom 名字 状态值@。
customCell :: ElementName -> Int -> Cell
customCell n k = Custom n (CustomState k)

-- | 自定义本体的解码：名字对上就按状态值构造。
fromCustom :: ElementName -> (Int -> e) -> Cell -> Maybe e
fromCustom n mk cell = case cell of
  Custom n' (CustomState k) | n' == n -> Just (mk k)
  _ -> Nothing

-- | 自定义本体的缺省放置：@Custom 名字 参数@（头一个整数参数，缺省 1；后面多出的参数忽略）。
customPlace :: ElementName -> Placer
customPlace n args _ = Just (customCell n (fromMaybe 1 (prefixArgs argInt args)))
