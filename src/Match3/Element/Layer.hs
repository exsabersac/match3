{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 叠层（冰、草 / 藤 / 巧 / 雾 / 锁链 / 冻结 / 窗帘 / 蒸汽）：对应 xmonad 的 LayoutModifier。
-- slim-4：值级挡匹配 / 命中 / 点火 / 随清 收成 'layerCover'；'Layered' 同时是 Ability 与 Phase 的修饰器，
-- 不再在 Layer 类上重复声明五个值级方法。
--
-- 合成规则只在 'Layered' 的 Ability / Phase instance 里写一次：
--
-- * 挡匹配 / 挡交换：本层 OR 里层；
-- * 点火：本层有意见（'lcFires' = Just）就听本层，否则问里层；
-- * 直接命中：本层先回答（'lcHit'）——穿透时问里层，里层吃掉命中就把本层盖回去；
-- * 洗牌保留：有任何一层就保留；
-- * 名字 / 颜色 / 下落 / 穿门 / 计数 / 显示等本体属性：取里层。
--
-- 邻格揭层 / 清掉与步末蔓延仍是类型级方法（'layerNeighbourPrio' 等），由 Rules 驱动。
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

-- | 值级覆盖（slim-4：五个旧方法合成一包，由 'Layered' 的 Phase / Ability 读取）。
data LayerCover l = LayerCover
  { lcBlocksMatch :: Bool
  , lcBlocksSwap :: Bool
  , lcFires :: Maybe Bool
  , lcHit :: LayerHit l
  , lcStripsOnClear :: Bool
  }
  deriving (Eq, Show)

defaultCover :: LayerCover l
defaultCover = LayerCover False False Nothing Pierce False

-- | 叠在本体之上的一层。
class (Show l, Eq l, Typeable l) => Layer l where
  layerName :: proxy l -> ElementName
  peel :: Cell -> Maybe (l, Cell)
  putOn :: l -> Cell -> Cell
  -- | 值级覆盖（挡匹配 / 命中 / 点火 / 随清）；缺省全穿透、不挡、不随清。
  layerCover :: l -> LayerCover l
  layerCover _ = defaultCover
  layerPlace :: proxy l -> Placer
  layerPlace _ _ _ = Nothing
  layerNeighbourPrio :: proxy l -> Maybe Int
  layerNeighbourPrio _ = Nothing
  layerReach :: proxy l -> Reach
  layerReach _ = SkipDirect
  onLayerNeighbourClear :: l -> Cell -> Nudge
  onLayerNeighbourClear _ _ = Untouched
  spreads :: proxy l -> Maybe (Int, l)
  spreads _ = Nothing
  layerPasses :: proxy l -> [BoardPass]
  layerPasses _ = []

layerBlocksMatch :: Layer l => l -> Bool
layerBlocksMatch = lcBlocksMatch . layerCover

layerBlocksSwap :: Layer l => l -> Bool
layerBlocksSwap = lcBlocksSwap . layerCover

layerFires :: Layer l => l -> Maybe Bool
layerFires = lcFires . layerCover

layerHit :: Layer l => l -> LayerHit l
layerHit = lcHit . layerCover

layerStripsOnClear :: Layer l => l -> Bool
layerStripsOnClear = lcStripsOnClear . layerCover

-- | 修饰过的元素（同 xmonad 的 ModifiedLayout）：叠层在外，被修饰的元素在里。
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

-- | Phase 修饰器：与 Ability 'Layered' 同语义（生产值级仍可走 Ability，对照走 Phase）。
instance (Layer l, Phase e) => Phase (Layered l e) where
  codec = Codec
    { cName = cName (codec @e)
    , cToCell = \(Layered l e) -> putOn l (cToCell (codec @e) e)
    , cFromCell = const Nothing
    , cPlace = \_ _ -> Nothing
    , cMeta = cMeta (codec @e)
    , cNear = Nothing
    }
  onSwap (Layered _ e) = onSwap e
  onMatch (Layered l e) =
    let m = onMatch e
     in m
          { mBlockMatch = layerBlocksMatch l || mBlockMatch m
          , mBlockSwap = layerBlocksSwap l || mBlockSwap m
          }
  onHit h (Layered l e) = case layerHit l of
    Pierce ->
      let HitOut st fi bl nx = onHit h e
          st' = case st of
            Absorb inner -> Absorb (putOn l inner)
            r -> r
          nx' = fmap (Layered l) nx
       in HitOut st' (fromMaybe fi (layerFires l)) bl nx'
    Keep l' -> HitOut (Absorb (putOn l' (cToCell (codec @e) e))) False Nothing Nothing
    Peel -> HitOut (Absorb (cToCell (codec @e) e)) False Nothing Nothing
    Shatter -> HitOut Destroy (fromMaybe True (layerFires l)) Nothing Nothing
  physics (Layered _ e) =
    let p = physics e
     in p { pKeepShuffle = True }
  onNear r ctx (Layered _ e) = onNear r ctx e
  view (Layered _ e) = view e

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
