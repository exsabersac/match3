{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素的类型级 API：一种本体元素 = 一个类型 + 'Phase' instance（Match3.Element.Phase）+ 一个 'Kind' instance。
-- 'Kind' 的方法都带 @proxy e@ 参数（解码、放置、标签、按差计数、规则），不需要「原型值」。
-- 地面层元素（不占格，层数在 GameState.gsGround 里）是 'GroundKind'；叠层见 Match3.Element.Layer。
--
-- 规则：邻格波及走 Phase（cNear / onNear），由 'kindNeighbour' 驱动。多格实体扣血经 'entityHit'。
-- 步末 / 交换 / 开启等仍可走 'boardPasses'。
module Match3.Element.Kind
  ( -- * 本体
    Kind(..)
  , SomeKind(..)
  , someKind
  , fromCellAs
  , BoardPass(..)
  , Reach(..)
  , Nudge(..)
  , DieOrder(..)
  , NearCtx(..)
  , NearOut(..)
    -- * 多格实体
  , Entity(..)
    -- * 地面层
  , GroundKind(..)
  , SomeGround(..)
  , someGround
    -- * 写法助手
  , customCell
  , fromCustom
  , customPlace
  ) where

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Match3.Element.Near
import Match3.Element.Phase (Phase(..), cFromCell, cName, codec)
import Match3.Element.Types
import Match3.Types

-- | 元素自带的整盘趟（逃生口）：读整盘、按自己的顺序写回的规则。
data BoardPass
  = AdjacentPass Int (AdjCtx -> Board -> AdjOut)  -- ^ 邻格波及（优先级小的先；内置 10..200）
  | EndPass EndRule                                -- ^ 步末规则（阶段 + 次序）
  | SwapPass SwapRule                              -- ^ 成对交换规则
  | OpenPass OpenRule                              -- ^ 开启规则（彩蛋类）

-- | 一种本体元素（类型级）。
class Phase e => Kind e where
  -- | 元素名（元素世界的键；与值级 nameOf 相同）。有 Phase 时默认 codec。
  kindName :: proxy e -> ElementName
  kindName _ = cName (codec @e)
  fromCell :: Cell -> Maybe e
  fromCell = cFromCell (codec @e)
  place :: proxy e -> Placer
  place _ _ _ = Nothing
  -- | 按元素名计数的目标（CountNamed）的中文名（HUD / 网页 / 失败提示）。
  label :: proxy e -> Maybe String
  label _ = Nothing
  -- | 该目标的失败提示（参数 = 目标值）；缺省「消除<中文名>，目标 n 个」。
  loseHint :: proxy e -> Maybe (Int -> String)
  loseHint _ = Nothing
  -- | 按步前 / 步后盘面上的个数差计数（保险箱开启、时间精灵、雪怪血量）。
  diffCounter :: proxy e -> Maybe CounterKey
  diffCounter _ = Nothing
  -- | 按差计数时每少一个奖励的步数。
  bonusMoves :: proxy e -> Int
  bonusMoves _ = 0
  -- | 逃生口：元素自带的整盘趟（不要把 'entityDamage' 写在这里；多格扣血用 'entityHit'）。
  boardPasses :: proxy e -> [BoardPass]
  boardPasses _ = []
  -- | 多格实体扣血：'Just (顺序号, 驱动)' 时由 'kindRules' 自动挂成 'AdjacentPass'。
  -- 写了 'Entity' 的类型必须覆盖（通常 @entityHit p = Just (entityHitOrder p, entityDamage p)@）；
  -- 护栏 'ec_entity_wires_damage' 按源码核对。缺省 Nothing = 不是多格实体。
  entityHit :: proxy e -> Maybe (Int, AdjCtx -> Board -> AdjOut)
  entityHit _ = Nothing
  -- | HUD 血条：盘上剩余 HP；Nothing = 本元素不提供血条（View 按目标名查世界，不点名具体元素）。
  boardBossHp :: proxy e -> Board -> Maybe Int
  boardBossHp _ _ = Nothing
  -- | 目标图标贴图名（覆盖默认的元素名）；Nothing = 用 'unElementName'。
  goalIconName :: proxy e -> Maybe String
  goalIconName _ = Nothing

-- | 多格实体（雪怪 Boss）：各部件仍是独立的格子，'Entity' 告诉驱动锚点怎么认（'partNo' == 0）、部件在哪
-- （'footprint'，第 i 项 = 第 i 号部件）、血量怎么读写。伤害由 Match3.Element.Rules.entityDamage 统一算，
-- 并经 Kind.'entityHit' 挂进规则表（顺序缺省 'entityHitOrder' = 200）。
class Kind e => Entity e where
  footprint :: proxy e -> Pos -> [Pos]
  partNo :: e -> Int
  hitPoints :: e -> Int
  withHp :: Int -> e -> e
  -- | 扣血邻格规则的顺序号（与其它 AdjacentPass 一起排序）。
  entityHitOrder :: proxy e -> Int
  entityHitOrder _ = 200

-- | 装箱的本体种类（元素世界里的一项）。
data SomeKind = forall e. Kind e => SomeKind (Proxy e)

-- | @someKind \@StoneE@。
someKind :: forall e. Kind e => SomeKind
someKind = SomeKind (Proxy :: Proxy e)

-- | 按 proxy 指定类型解码。
fromCellAs :: Kind e => proxy e -> Cell -> Maybe e
fromCellAs _ = fromCell

-- | 地面层元素（类型级；层数在 gsGround 里按名字记）。
class GroundKind g where
  groundName :: proxy g -> ElementName
  -- | 上方格子被消除一次时：层数 → 新层数（Nothing = 清掉）；缺省不变。
  groundHit :: proxy g -> Int -> Maybe Int
  groundHit _ = Just
  -- | 去层时的计数键（只有 CountNamed 并入 gsCounts）。
  groundCounter :: proxy g -> Maybe CounterKey
  groundCounter _ = Nothing
  -- | 本格上的特效引爆时改写爆炸范围（Nothing = 不改；内置只有魔法地格）。
  groundWiden :: proxy g -> Maybe (Board -> [Pos] -> [Pos])
  groundWiden _ = Nothing
  groundLabel :: proxy g -> Maybe String
  groundLabel _ = Nothing
  groundLoseHint :: proxy g -> Maybe (Int -> String)
  groundLoseHint _ = Nothing

-- | 装箱的地面层种类。
data SomeGround = forall g. GroundKind g => SomeGround (Proxy g)

someGround :: forall g. GroundKind g => SomeGround
someGround = SomeGround (Proxy :: Proxy g)

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
