{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 叠层（冰、草 / 藤 / 巧 / 雾 / 锁链 / 冻结 / 窗帘 / 蒸汽）：对应 xmonad 的 LayoutModifier。
-- 类只有三个方法：'peel' / 'putOn'（与格子存储编码互转）和类型级的 'layerCover'（一份静态记录：名字、放置、
-- 挡匹配 / 挡交换 / 点火 / 直接命中 / 随清，邻格规则与蔓延、逃生口；值级的项是取自叠层值的函数）。
-- 'Layered' 是 Phase 的修饰器。
--
-- 合成规则只在 'Layered' 的 Phase instance 里写一次：
--
-- * 挡匹配 / 挡交换：本层 OR 里层；
-- * 点火：本层有意见（'lcFires' = Just）就听本层，否则问里层；
-- * 直接命中：本层先回答（'lcHit'）——穿透时问里层，里层吃掉命中就把本层盖回去；
-- * 洗牌保留：有任何一层就保留；
-- * 名字 / 颜色 / 下落 / 穿门 / 计数 / 显示等本体属性：取里层。
--
-- 邻格揭层 / 清掉与步末蔓延（'lcNearPrio' / 'lcOnNear' / 'lcSpreads'）由 Match3.Element.Rules 驱动。
module Match3.Element.Layer
  ( Layer(..)
  , LayerHit(..)
  , LayerCover(..)
  , defaultCover
  , layerBlocksMatch
  , layerBlocksSwap
  , layerFires
  , layerHit
  , layerStripsOnClear
  , layerName
  , layerPlace
  , layerNeighbourPrio
  , layerReach
  , onLayerNeighbourClear
  , spreads
  , layerPasses
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
import Match3.Element.Kind (Nudge(..), Reach(..))
import Match3.Element.Phase
import Match3.Element.Types (Placer)
import Match3.Types

-- | 叠层对直接命中的回答。
data LayerHit l
  = Pierce   -- ^ 本层不管，问里面（里面吃掉命中时本层盖回去）
  | Keep l   -- ^ 吃掉命中，本层换成新值（冰 3 → 2），里面不动，本格不消除
  | Peel     -- ^ 吃掉命中，本层揭掉（锁链 / 窗帘末层），本格不消除
  | Shatter  -- ^ 本格被消除（末层冰随宝石一起碎）
  deriving (Eq, Show)

-- | 一种叠层的全部数据（类型级：每种叠层一份静态记录；值级的项是取自叠层值的函数）。
data LayerCover l = LayerCover
  { lcName :: ElementName                -- ^ 元素名（元素世界的键）
  , lcPlace :: Placer                    -- ^ 关卡放置
  , lcBlocksMatch :: l -> Bool           -- ^ 挡匹配
  , lcBlocksSwap :: l -> Bool            -- ^ 挡交换
  , lcFires :: l -> Maybe Bool           -- ^ 点火（Nothing = 没意见，问里层）
  , lcHit :: l -> LayerHit l             -- ^ 直接命中
  , lcStripsOnClear :: l -> Bool         -- ^ 本格被消除时随格清掉
  , lcNearPrio :: Maybe Int              -- ^ 邻格规则的优先级（Nothing = 对邻格消除无反应）
  , lcReach :: Reach                     -- ^ 邻格规则的目标范围
  , lcOnNear :: l -> Cell -> Nudge       -- ^ 邻格有真消除时本格怎么变
  , lcSpreads :: Maybe (Int, l)          -- ^ 步末蔓延：(次序, 种上的值)
  , lcPasses :: [BoardPass]              -- ^ 逃生口：自带的整盘趟
  }

-- | 缺省：不能放置、全穿透、不挡、不随清、对邻格无反应、不蔓延。
defaultCover :: ElementName -> LayerCover l
defaultCover n = LayerCover
  { lcName = n
  , lcPlace = \_ _ -> Nothing
  , lcBlocksMatch = const False
  , lcBlocksSwap = const False
  , lcFires = const Nothing
  , lcHit = const Pierce
  , lcStripsOnClear = const False
  , lcNearPrio = Nothing
  , lcReach = SkipDirect
  , lcOnNear = \_ _ -> Untouched
  , lcSpreads = Nothing
  , lcPasses = []
  }

-- | 叠在本体之上的一层。
class (Show l, Eq l, Typeable l) => Layer l where
  -- | 从格子上剥下本层（外 → 内）。
  peel :: Cell -> Maybe (l, Cell)
  -- | 把本层盖回格子。
  putOn :: l -> Cell -> Cell
  -- | 本层的全部数据（类型级）。
  layerCover :: LayerCover l

layerBlocksMatch :: forall l. Layer l => l -> Bool
layerBlocksMatch = lcBlocksMatch (layerCover @l)

layerBlocksSwap :: forall l. Layer l => l -> Bool
layerBlocksSwap = lcBlocksSwap (layerCover @l)

layerFires :: forall l. Layer l => l -> Maybe Bool
layerFires = lcFires (layerCover @l)

layerHit :: forall l. Layer l => l -> LayerHit l
layerHit = lcHit (layerCover @l)

layerStripsOnClear :: forall l. Layer l => l -> Bool
layerStripsOnClear = lcStripsOnClear (layerCover @l)

layerName :: forall l proxy. Layer l => proxy l -> ElementName
layerName _ = lcName (layerCover @l)

layerPlace :: forall l proxy. Layer l => proxy l -> Placer
layerPlace _ = lcPlace (layerCover @l)

layerNeighbourPrio :: forall l proxy. Layer l => proxy l -> Maybe Int
layerNeighbourPrio _ = lcNearPrio (layerCover @l)

layerReach :: forall l proxy. Layer l => proxy l -> Reach
layerReach _ = lcReach (layerCover @l)

onLayerNeighbourClear :: forall l. Layer l => l -> Cell -> Nudge
onLayerNeighbourClear = lcOnNear (layerCover @l)

spreads :: forall l proxy. Layer l => proxy l -> Maybe (Int, l)
spreads _ = lcSpreads (layerCover @l)

layerPasses :: forall l proxy. Layer l => proxy l -> [BoardPass]
layerPasses _ = lcPasses (layerCover @l)

-- | 修饰过的元素（同 xmonad 的 ModifiedLayout）：叠层在外，被修饰的元素在里。
data Layered l e = Layered l e
  deriving (Eq, Show)

-- | Phase 修饰器：外层先回答、里层兜底（合成规则见模块头）。
instance (Layer l, Phase e) => Phase (Layered l e) where
  codec = Codec
    { cName = cName (codec @e)
    , cToCell = \(Layered l e) -> putOn l (cToCell (codec @e) e)
    , cFromCell = const Nothing
    , cPlace = \_ _ -> Nothing
    , cMeta = cMeta (codec @e)
    , cNear = Nothing
    , cHud = noHud
    , cPasses = []
    }
  onMatch (Layered l e) =
    let m = onMatch e
     in m
          { mBlockMatch = layerBlocksMatch l || mBlockMatch m
          , mBlockSwap = layerBlocksSwap l || mBlockSwap m
          }
  onHit h (Layered l e) =
    let HitOut st fi bl nx = onHit h e
        fi' = fromMaybe fi (layerFires l)
     in case layerHit l of
          Pierce ->
            let st' = case st of
                  Absorb inner -> Absorb (putOn l inner)
                  r -> r
             in HitOut st' fi' bl (fmap (Layered l) nx)
          Keep l' -> HitOut (Absorb (putOn l' (cToCell (codec @e) e))) fi' bl Nothing
          Peel -> HitOut (Absorb (cToCell (codec @e) e)) fi' bl Nothing
          Shatter -> HitOut Destroy fi' bl Nothing
  physics (Layered _ e) =
    let p = physics e
     in p { pKeepShuffle = True }
  onNear r ctx (Layered _ e) = onNear r ctx e
  view (Layered _ e) = view e
  liveMeta (Layered _ e) = liveMeta e

data SomeLayer = forall l. Layer l => SomeLayer (Proxy l)

someLayer :: forall l. Layer l => SomeLayer
someLayer = SomeLayer (Proxy :: Proxy l)

data SomeLayerValue = forall l. Layer l => SomeLayerValue l

peelAs :: Layer l => proxy l -> Cell -> Maybe (l, Cell)
peelAs _ = peel

layerValueName :: SomeLayerValue -> ElementName
layerValueName (SomeLayerValue l) = layerName (proxyOf l)
  where
    proxyOf :: x -> Proxy x
    proxyOf _ = Proxy
