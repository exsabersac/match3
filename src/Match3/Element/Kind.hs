{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素的类型级 API：一种本体元素 = 一个类型 + 值级能力（Match3.Element.Ability）+ 一个 'Kind' instance。
-- 'Kind' 的方法都带 @proxy e@ 参数（解码、放置、标签、按差计数、规则），不需要「原型值」。
-- 地面层元素（不占格，层数在 GameState.gsGround 里）是 'GroundKind'；叠层见 Match3.Element.Layer。
--
-- 规则：'boardPasses' 是逃生口——元素自带的整盘趟（邻格 / 步末 / 成对交换 / 开启）。
module Match3.Element.Kind
  ( -- * 本体
    Kind(..)
  , SomeKind(..)
  , someKind
  , fromCellAs
  , BoardPass(..)
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
  -- | 逃生口：元素自带的整盘趟。
  boardPasses :: proxy e -> [BoardPass]
  boardPasses _ = []

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
