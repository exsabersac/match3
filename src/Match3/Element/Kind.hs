{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 本体元素的类型级读数（slim-10 起没有 Kind 类了）：一种本体元素 = 一个类型 + 'Phase' instance。
-- 名字 / 解码 / 放置 / 计数元数据 / HUD / 逃生口整盘趟都是 'Codec' 里的数据，这里的同名函数只是按 proxy 读它，
-- 方便注册表与调用方（@kindName p@、@place p@ …）。
--
-- 多格实体（雪怪）是一条 'Entity' 记录，扣血驱动 Match3.Element.Rules.entityDamage 由元素自己挂进 'cPasses'；
-- 地面层元素（不占格，层数在 GameState.gsGround 里）是一条 'GroundKind' 记录；叠层见 Match3.Element.Layer。
module Match3.Element.Kind
  ( -- * 本体
    SomeKind(..)
  , someKind
  , kindName
  , fromCell
  , fromCellAs
  , place
  , label
  , loseHint
  , diffCounter
  , bonusMoves
  , boardPasses
  , boardBossHp
  , goalIconName
  , BoardPass(..)
  , Hud(..)
  , noHud
  , Reach(..)
  , Nudge(..)
  , DieOrder(..)
  , NearCtx(..)
  , NearOut(..)
    -- * 多格实体
  , Entity(..)
    -- * 地面层
  , GroundKind(..)
  , groundKind
    -- * 写法助手
  , customCell
  , fromCustom
  , customPlace
  ) where

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Match3.Element.Near
import Match3.Element.Phase (BoardPass(..), Codec(..), Hud(..), Meta(..), Phase(..), noHud)
import Match3.Element.Types
import Match3.Types

-- | 元素名（元素世界的键）。
kindName :: forall e proxy. Phase e => proxy e -> ElementName
kindName _ = cName (codec @e)

-- | 解码（'cFromCell'）。
fromCell :: forall e. Phase e => Cell -> Maybe e
fromCell = cFromCell (codec @e)

-- | 按 proxy 指定类型解码。
fromCellAs :: forall e proxy. Phase e => proxy e -> Cell -> Maybe e
fromCellAs _ = cFromCell (codec @e)

place :: forall e proxy. Phase e => proxy e -> Placer
place _ = cPlace (codec @e)

-- | 按元素名计数的目标（CountNamed）的中文名。
label :: forall e proxy. Phase e => proxy e -> Maybe String
label _ = hudLabel (cHud (codec @e))

-- | 该目标的失败提示（参数 = 目标值）。
loseHint :: forall e proxy. Phase e => proxy e -> Maybe (Int -> String)
loseHint _ = hudLoseHint (cHud (codec @e))

-- | 按步前 / 步后盘面上的个数差计数（保险箱开启、时间精灵、雪怪血量）：'metaDiffCounter'。
diffCounter :: forall e proxy. Phase e => proxy e -> Maybe CounterKey
diffCounter _ = metaDiffCounter (cMeta (codec @e))

-- | 按差计数时每少一个奖励的步数：'metaBonusMoves'。
bonusMoves :: forall e proxy. Phase e => proxy e -> Int
bonusMoves _ = metaBonusMoves (cMeta (codec @e))

-- | 逃生口：元素自带的整盘趟（'cPasses'）。
boardPasses :: forall e proxy. Phase e => proxy e -> [BoardPass]
boardPasses _ = cPasses (codec @e)

-- | HUD 血条：盘上剩余 HP；Nothing = 本元素不提供血条。
boardBossHp :: forall e proxy. Phase e => proxy e -> Board -> Maybe Int
boardBossHp _ b = ($ b) <$> hudBossHp (cHud (codec @e))

-- | 目标图标贴图名（覆盖默认的元素名）；Nothing = 用 'unElementName'。
goalIconName :: forall e proxy. Phase e => proxy e -> Maybe String
goalIconName _ = hudGoalIcon (cHud (codec @e))

-- | 多格实体（雪怪 Boss）：各部件仍是独立的格子，记录告诉驱动锚点怎么认（'partNo' == 0）、部件在哪
-- （'footprint'，第 i 项 = 第 i 号部件）、血量怎么读写。伤害由 Match3.Element.Rules.entityDamage 统一算，
-- 元素把 @AdjacentPass 200 (entityDamage 记录)@ 放进自己的 'cPasses'。
data Entity e = Entity
  { footprint :: Pos -> [Pos]
  , partNo :: e -> Int
  , hitPoints :: e -> Int
  , withHp :: Int -> e -> e
  }

-- | 装箱的本体种类（元素世界里的一项）。
data SomeKind = forall e. Phase e => SomeKind (Proxy e)

-- | @someKind \@StoneE@。
someKind :: forall e. Phase e => SomeKind
someKind = SomeKind (Proxy :: Proxy e)

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
