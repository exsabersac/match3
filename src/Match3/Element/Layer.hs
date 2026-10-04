{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | 叠层（冰、草 / 藤 / 巧 / 雾 / 锁链 / 冻结 / 窗帘 / 蒸汽）：对应 xmonad 的 LayoutModifier。
-- 一种叠层 = 一个类型 + 一个 'Layer' instance，只覆盖要改的方法；包在本体外面的通用包装 'Layered'
-- 把叠层和里面的元素合成一个元素，合成规则**只在 'Layered' 的各 instance 里写一次**：
--
-- * 挡匹配 / 挡交换：本层 OR 里层；
-- * 点火：本层有意见（'layerFires' = Just）就听本层，否则问里层；
-- * 直接命中：本层先回答（'layerHit'）——穿透时问里层，里层吃掉命中就把本层盖回去；
-- * 洗牌保留：有任何一层就保留；
-- * 名字 / 颜色 / 下落 / 穿门 / 计数 / 显示等本体属性：取里层。
--
-- 规则是类型级的，与格子值无关：邻格揭层 / 清掉（'layerNeighbourPrio' / 'layerReach' / 'onLayerNeighbourClear'）
-- 与步末蔓延（'spreads'）是方法，由通用驱动（Match3.Element.Rules.layerNeighbour / layerSpread）执行；其余走
-- 逃生口 'layerPasses'。
module Match3.Element.Layer
  ( Layer(..)
  , LayerHit(..)
  , Layered(..)
  , SomeLayer(..)
  , someLayer
  , SomeLayerValue(..)
  , peelAs
  , layerValueName
  ) where

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Match3.Element.Ability
import Match3.Element.Kind (BoardPass, Nudge(..), Reach(..))
import Match3.Element.Types (Placer)
import Match3.Types

-- | 叠层对直接命中的回答。
data LayerHit l
  = Pierce   -- ^ 本层不管，问里面（里面吃掉命中时本层盖回去）
  | Keep l   -- ^ 吃掉命中，本层换成新值（冰 3 → 2），里面不动，本格不消除
  | Peel     -- ^ 吃掉命中，本层揭掉（锁链 / 窗帘末层），本格不消除
  | Shatter  -- ^ 本格被消除（末层冰随宝石一起碎）
  deriving (Eq, Show)

-- | 叠在本体之上的一层。
class (Show l, Eq l, Typeable l) => Layer l where
  layerName :: proxy l -> ElementName
  -- | 从格子拆出本层与里面的格子（Nothing = 这格没有本层）。
  peel :: Cell -> Maybe (l, Cell)
  -- | 把本层写回格子。
  putOn :: l -> Cell -> Cell
  -- | 盖住的格子不参与匹配。
  layerBlocksMatch :: l -> Bool
  layerBlocksMatch _ = False
  layerBlocksSwap :: l -> Bool
  layerBlocksSwap _ = False
  -- | 点火意见：Nothing = 没意见（问里面）。
  layerFires :: l -> Maybe Bool
  layerFires _ = Nothing
  layerHit :: l -> LayerHit l
  layerHit _ = Pierce
  -- | 本格真消除时随格清掉（草 / 藤 / 巧）。
  layerStripsOnClear :: l -> Bool
  layerStripsOnClear _ = False
  -- | 关卡放置（缺省不放）。
  layerPlace :: proxy l -> Placer
  layerPlace _ _ _ = Nothing
  -- | 邻格规则的优先级（Nothing = 没有）。内置：迷雾 70 / 锁链 80 / 火箭冰冻 90 / 窗帘 100 / 巧克力 150 / 蒸汽 160。
  layerNeighbourPrio :: proxy l -> Maybe Int
  layerNeighbourPrio _ = Nothing
  layerReach :: proxy l -> Reach
  layerReach _ = SkipDirect
  -- | 与本轮真消除格正交相邻时这一格（整格，含本层）怎么变。
  onLayerNeighbourClear :: l -> Cell -> Nudge
  onLayerNeighbourClear _ _ = Untouched
  -- | 步末蔓延（PhaseSpread）：(次序, 种到相邻裸宝石上的本层值)；Nothing = 不蔓延。
  spreads :: proxy l -> Maybe (Int, l)
  spreads _ = Nothing
  -- | 逃生口：本层自带的整盘趟。
  layerPasses :: proxy l -> [BoardPass]
  layerPasses _ = []

-- | 修饰过的元素（同 xmonad 的 ModifiedLayout）：叠层在外，被修饰的元素（本体或更内层的叠层）在里。
data Layered l e = Layered l e
  deriving (Eq, Show)

instance (Layer l, Element e) => Cellular (Layered l e) where
  nameOf (Layered _ e) = nameOf e
  toCell (Layered l e) = putOn l (toCell e)

instance (Layer l, Element e) => Matchable (Layered l e) where
  color (Layered _ e) = color e
  blocksMatch (Layered l e) = layerBlocksMatch l || blocksMatch e
  blocksSwap (Layered l e) = layerBlocksSwap l || blocksSwap e
  hintable (Layered _ e) = hintable e

instance (Layer l, Element e) => Hittable (Layered l e) where
  struck (Layered l e) = case layerHit l of
    Pierce -> case struck e of
      Absorb inner -> Absorb (putOn l inner)
      r -> r
    Keep l' -> Absorb (putOn l' (toCell e))
    Peel -> Absorb (toCell e)
    Shatter -> Destroy
  fires (Layered l e) = fromMaybe (fires e) (layerFires l)
  blast (Layered _ e) = blast e

instance Element e => Movable (Layered l e) where
  falls (Layered _ e) = falls e
  portal (Layered _ e) = portal e
  drains (Layered _ e) = drains e
  keepOnShuffle _ = True
  recolorable (Layered _ e) = recolorable e
  pushable (Layered _ e) = pushable e

instance Element e => Countable (Layered l e) where
  counter (Layered _ e) = counter e
  diffWeight (Layered _ e) = diffWeight e
  vacatesCarpet (Layered _ e) = vacatesCarpet e

instance Element e => Renders (Layered l e) where
  face (Layered _ e) = face e
  faceBase (Layered _ e) = faceBase e

-- | 装箱的叠层种类（注册表里的一项）。
data SomeLayer = forall l. Layer l => SomeLayer (Proxy l)

someLayer :: forall l. Layer l => SomeLayer
someLayer = SomeLayer (Proxy :: Proxy l)

-- | 装箱的叠层值（解码一格得到的各层）。
data SomeLayerValue = forall l. Layer l => SomeLayerValue l

-- | 按 proxy 指定类型拆层。
peelAs :: Layer l => proxy l -> Cell -> Maybe (l, Cell)
peelAs _ = peel

-- | 叠层值的名字。
layerValueName :: SomeLayerValue -> ElementName
layerValueName (SomeLayerValue l) = layerName (proxyOf l)
  where
    proxyOf :: x -> Proxy x
    proxyOf _ = Proxy
