{-# LANGUAGE ExistentialQuantification #-}
-- | 叠层原型（ecs-4 起取代 Layer 类）：冰、草 / 藤 / 巧 / 雾 / 锁链 / 冻结 / 窗帘 / 蒸汽。
--
-- 和本体原型（Match3.ECS.Archetype）同一个思路：一份**数据**描述一种叠层——
--
-- * 存储列 'CoverColumn'：从格子上剥下本层 / 把本层盖回（Cell 存储编码决定：冰层在外、叠层在内）；
-- * 组件 'Shield'（纯数据，由本层状态算出）：挡匹配 / 挡交换 / 点火 / 直接命中 / 随清；
-- * system：邻格揭层 / 清掉（Match3.Element.Rules 的 coverNear）、步末蔓延（coverSpread）都是普通 system，
--   放在 'cvSystems' 里，和本体原型的 aSystems 一起由注册表收集、排程。
--
-- 合成规则（本层先回答，没意见再问里层）只在注册表的 wholeMatch / wholeHit / wholePhysics 里写一次：
--
-- * 挡匹配 / 挡交换：本层 OR 里层；
-- * 点火：本层有意见（'sFires' = Just）就听本层，否则问里层；
-- * 直接命中：本层先回答（'sHit'）——穿透时问里层，里层吃掉命中就把本层盖回去；
-- * 洗牌保留：有任何一层就保留；
-- * 名字 / 颜色 / 下落 / 穿门 / 计数 / 显示等本体属性：取里层。
module Match3.ECS.Cover
  ( -- * 存储列
    CoverColumn(..)
  , overlayColumn
  , putOverlay
    -- * 组件
  , LayerHit(..)
  , Shield(..)
  , openShield
    -- * 叠层原型
  , Cover(..)
  , mkCover
  , SomeCover(..)
  , coverName
  , coversCell
    -- * 解码出的一层
  , Peeled(..)
  , peelWith
  , peeledName
  , peeledShield
  , putBack
  ) where

import Data.Maybe (isJust)
import Match3.ECS.Stage (SysDef)
import Match3.Element.Types (Placer)
import Match3.Types

-- | 叠层的存储列：剥下（外 → 内）与盖回。定律：@ccPeel c == Just (l, inner)@ 时 @ccPut l inner == c@。
data CoverColumn l = CoverColumn
  { ccPeel :: Cell -> Maybe (l, Cell)
  , ccPut :: l -> Cell -> Cell
  }

-- | 宝石格 overlay 字段上的一种叠层（@overlayColumn (preview _Fog) Fog@：状态 = 层数；无层数的叠层状态是 ()）。
overlayColumn :: (CellOverlay -> Maybe l) -> (l -> CellOverlay) -> CoverColumn l
overlayColumn get con = CoverColumn peel (putOverlay . con)
  where
    peel cell = case cell of
      Gem c k i (Just ov) | Just l <- get ov -> Just (l, Gem c k i Nothing)
      _ -> Nothing
{-# INLINE overlayColumn #-}

-- | 把叠层写回宝石格（替换原叠层；非宝石格不变）。
putOverlay :: CellOverlay -> Cell -> Cell
putOverlay ov cell = case cell of
  Gem c k i _ -> Gem c k i (Just ov)
  _ -> cell

-- | 叠层对直接命中的回答（数据）。
data LayerHit l
  = Pierce   -- ^ 本层不管，问里面（里面吃掉命中时本层盖回去）
  | Keep l   -- ^ 吃掉命中，本层换成新状态（冰 3 → 2），里面不动，本格不消除
  | Peel     -- ^ 吃掉命中，本层揭掉（锁链 / 窗帘末层），本格不消除
  | Shatter  -- ^ 本格被消除（末层冰随宝石一起碎）
  deriving (Eq, Show)

-- | 叠层组件：一层在当前状态下对主流程各问题的回答（全是数据）。
data Shield l = Shield
  { sBlocksMatch :: Bool        -- ^ 挡匹配
  , sBlocksSwap :: Bool         -- ^ 挡交换
  , sFires :: Maybe Bool        -- ^ 点火（Nothing = 没意见，问里层）
  , sHit :: LayerHit l          -- ^ 直接命中
  , sStrips :: Bool             -- ^ 本格被消除时随格清掉
  }
  deriving (Eq, Show)

-- | 缺省组件：全穿透、不挡、不随清。
openShield :: Shield l
openShield = Shield False False Nothing Pierce False

-- | 一种叠层（状态类型 @l@）：名字、存储列、放置、组件、system。
data Cover l = Cover
  { cvName :: ElementName          -- ^ 元素名（注册表的键）
  , cvColumn :: CoverColumn l      -- ^ 存储列
  , cvSpawn :: Placer              -- ^ 关卡放置
  , cvShield :: l -> Shield l      -- ^ 组件（由状态算出）
  , cvSystems :: [SysDef]          -- ^ 本层带来的 system（邻格、蔓延……）
  }

-- | 缺省叠层原型：不能放置、'openShield'、没有 system；元素作者用记录更新改自己不同的部分。
mkCover :: ElementName -> CoverColumn l -> Cover l
mkCover n col = Cover
  { cvName = n
  , cvColumn = col
  , cvSpawn = \_ _ -> Nothing
  , cvShield = const openShield
  , cvSystems = []
  }

-- | 装箱的叠层原型（注册表里存的）。
data SomeCover = forall l. SomeCover (Cover l)

coverName :: SomeCover -> ElementName
coverName (SomeCover c) = cvName c

-- | 这种叠层是否认这个格子（注册表建分派表用）。
coversCell :: SomeCover -> Cell -> Bool
coversCell (SomeCover c) = isJust . ccPeel (cvColumn c)

-- | 解码出的一层：叠层原型 + 本层状态。
data Peeled = forall l. Peeled (Cover l) l

-- | 用某种叠层剥格子。
peelWith :: Cover l -> Cell -> Maybe (l, Cell)
peelWith = ccPeel . cvColumn
{-# INLINE peelWith #-}

peeledName :: Peeled -> ElementName
peeledName (Peeled c _) = cvName c

-- | 本层组件里与状态类型无关的部分（命中的回答 'sHit' 带状态，抹成 'Pierce'；要用它就在 'Peeled' 上就地拆）。
peeledShield :: Peeled -> Shield ()
peeledShield (Peeled c l) = (cvShield c l) {sHit = Pierce}
{-# INLINE peeledShield #-}

-- | 把这一层盖回格子。
putBack :: Peeled -> Cell -> Cell
putBack (Peeled c l) = ccPut (cvColumn c) l
