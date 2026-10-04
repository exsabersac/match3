{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素的类型级 API：一种本体元素 = 一个类型 + 值级能力（Match3.Element.Ability）+ 一个 'Kind' instance。
-- 'Kind' 的方法都带 @proxy e@ 参数（解码、放置、标签、按差计数、规则），不需要「原型值」。
-- 地面层元素（不占格，层数在 GameState.gsGround 里）是 'GroundKind'；叠层见 Match3.Element.Layer。
--
-- 规则（元素类重构第 3 刀起）：能写成「邻格真消除时这一格怎么变」的邻格规则是方法（'neighbourPrio' / 'reach' /
-- 'onNeighbourClear'），由通用驱动（Match3.Element.Rules.kindNeighbour）统一找邻格、跳过直接命中、按顺序写回；
-- 多格实体（'Entity'）的扣血同样由驱动 entityDamage 算。其余读整盘、按自己的顺序写回的规则（步末 / 成对交换 /
-- 开启 / 同色邻消 / 改色 …）走逃生口 'boardPasses'。
module Match3.Element.Kind
  ( -- * 本体
    Kind(..)
  , SomeKind(..)
  , someKind
  , fromCellAs
  , BoardPass(..)
  , Reach(..)
  , Nudge(..)
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
import Match3.Element.Ability (Element)
import Match3.Element.Types
import Match3.Types

-- | 元素自带的整盘趟（逃生口）：读整盘、按自己的顺序写回的规则。
data BoardPass
  = AdjacentPass Int (AdjCtx -> Board -> AdjOut)  -- ^ 邻格波及（优先级小的先；内置 10..200）
  | EndPass EndRule                                -- ^ 步末规则（阶段 + 次序）
  | SwapPass SwapRule                              -- ^ 成对交换规则
  | OpenPass OpenRule                              -- ^ 开启规则（彩蛋类）

-- | 邻格波及跳不跳过本轮被直接命中的格（直接命中已经结算过一次）。
data Reach
  = SkipDirect     -- ^ 跳过（石头 / 宝箱 / 迷雾 …）
  | AllNeighbours  -- ^ 不跳过（巧克力 / 蒸汽）
  deriving (Eq, Show)

-- | 邻格有真消除时，这一格怎么变。
data Nudge
  = Untouched     -- ^ 不变
  | Becomes Cell  -- ^ 原地变成别的格（削一层 / 揭叠层 / 保险箱开成饼干）
  | Dies          -- ^ 打碎：并入本轮清除格（格子原样留着，由清除管线移走）
  deriving (Eq, Show)

-- | 一种本体元素（类型级）。
class Element e => Kind e where
  -- | 元素名（注册表的键；与值级 'Match3.Element.Ability.nameOf' 相同）。
  kindName :: proxy e -> ElementName
  -- | 从格子解码出元素值（Nothing = 这格不是本元素）。
  fromCell :: Cell -> Maybe e
  -- | 关卡放置（缺省不放）。
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
  -- | 邻格规则的优先级（小的先；Nothing = 没有邻格规则）。内置：石头 10 / 宝箱 20 / 蜂蜜 30 / 蛋糕 40 /
  -- 保险箱 110 / 时间精灵 120。
  neighbourPrio :: proxy e -> Maybe Int
  neighbourPrio _ = Nothing
  reach :: proxy e -> Reach
  reach _ = SkipDirect
  -- | 与本轮真消除格正交相邻时这一格怎么变。
  onNeighbourClear :: e -> Nudge
  onNeighbourClear _ = Untouched
  -- | 逃生口：元素自带的整盘趟。
  boardPasses :: proxy e -> [BoardPass]
  boardPasses _ = []

-- | 多格实体（雪怪 Boss）：各部件仍是独立的格子，'Entity' 告诉驱动锚点怎么认（'partNo' == 0）、部件在哪
-- （'footprint'，第 i 项 = 第 i 号部件）、血量怎么读写。伤害由 Match3.Element.Rules.entityDamage 统一算。
class Kind e => Entity e where
  footprint :: proxy e -> Pos -> [Pos]
  partNo :: e -> Int
  hitPoints :: e -> Int
  withHp :: Int -> e -> e

-- | 装箱的本体种类（注册表里的一项）。
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
