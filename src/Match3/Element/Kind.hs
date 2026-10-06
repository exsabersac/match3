-- | 元素作者的写法助手与地面层记录（ecs-3 起本体元素是 Match3.ECS.Archetype 的原型值，不再有 Phase / Kind 类）。
--
-- 地面层元素（不占格，层数在 GameState.gsGround 里）仍是一条 'GroundKind' 记录（ecs-5 改成数据组件）；
-- 叠层见 Match3.ECS.Cover；邻格反应的共享类型见 Match3.Element.Near（这里再导出）。
module Match3.Element.Kind
  ( Reach(..)
  , Nudge(..)
  , DieOrder(..)
  , NearCtx(..)
  , NearOut(..)
    -- * 地面层
  , GroundKind(..)
  , groundKind
    -- * 写法助手
  , customCell
  , fromCustom
  , customPlace
  ) where

import Data.Maybe (fromMaybe)
import Match3.Element.Near
import Match3.Element.Types
import Match3.Types

-- | 地面层元素（层数在 gsGround 里按名字记）：一条数据，由关卡级元素 GroundLayer 与世界的地面层查询消费。
data GroundKind = GroundKind
  { groundName :: ElementName
  , groundHit :: Int -> Maybe Int                     -- ^ 上方格子被消除一次时：层数 → 新层数（Nothing = 清掉）
  , groundCounter :: Maybe CounterKey                 -- ^ 去层时的计数键（只有 CountNamed 并入 gsCounts）
  , groundWiden :: Maybe (Board -> [Pos] -> [Pos])    -- ^ 本格上的特效引爆时改写爆炸范围（内置只有魔法地格）
  , groundLabel :: Maybe String
  , groundLoseHint :: Maybe (Int -> String)
  }

-- | 缺省地面层：被消除不变、不计数、不改爆炸范围、没有中文名。
groundKind :: ElementName -> GroundKind
groundKind n = GroundKind n Just Nothing Nothing Nothing Nothing

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
