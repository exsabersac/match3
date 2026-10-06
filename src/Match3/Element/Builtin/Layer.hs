{-# LANGUAGE OverloadedStrings #-}
-- | 冰层与叠层：盖在宝石上的叠层（'Layer'，对应 xmonad 的 LayoutModifier）。
--
-- 共同特征：不占格，只在 Gem 格的冰层数 / overlay 字段里；解码时由 'Layered' 包在本体外面，合成规则
-- （本层先回答，没意见再问里面）只写在 Match3.Element.Layer 里一次。冰层削层点火；草 / 藤 / 巧随格清掉；迷雾 / 锁链 / 火箭冰冻 / 窗帘带层数、邻消揭一层；
-- 巧克力 / 蒸汽被邻格真消除清掉；藤 10 → 巧 20 → 蒸汽 30 步末蔓延（PhaseSpread）。
-- 邻格规则顺序：迷雾 70 → 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 巧克力 150 → 蒸汽 160。
-- 这些规则全是 'layerCover' 里的数据（lcNearPrio / lcOnNear / lcSpreads），由 Match3.Element.Rules 的通用驱动执行。
module Match3.Element.Builtin.Layer
  ( Ice(..)
  , GrassL(..)
  , VineL(..)
  , ChocoL(..)
  , FogL(..)
  , ChainL(..)
  , FreezeL(..)
  , CurtainL(..)
  , SteamL(..)
  , putOverlay
  ) where

import Match3.Element.Kind (Nudge(..), Reach(..))
import Match3.Element.Layer
import Match3.Element.Types
import Match3.Types

-- | 冰层：不挡匹配 / 交换；多层冰只削一层（不点火），末层冰随宝石一起碎（并点火）。
newtype Ice = Ice Int
  deriving (Eq, Show)

instance Layer Ice where
  peel cell = case cell of
    Gem c k n ov | n > 0 -> Just (Ice n, Gem c k 0 ov)
    _ -> Nothing
  putOn (Ice n) cell = case cell of
    Gem c k _ ov -> Gem c k n ov
    _ -> cell
  layerCover = (defaultCover "ice")
    { lcFires = \(Ice n) -> Just (n <= 1)
    , lcHit = \(Ice n) -> if n > 1 then Keep (Ice (n - 1)) else Shatter
      -- 放置：设冰层数（精确一个整数参数）；原格不是宝石时不放
    , lcPlace = \args cell -> case cell of
        Gem col kind _ ov -> (\n -> Gem col kind n ov) <$> exactArgs argInt args
        _ -> Nothing
    }

-- | 把叠层写回宝石格（替换原叠层）。
putOverlay :: CellOverlay -> Cell -> Cell
putOverlay ov cell = case cell of
  Gem c k i _ -> Gem c k i (Just ov)
  _ -> cell

-- | 草：真消除时随格清掉。
data GrassL = GrassL
  deriving (Eq, Show)

instance Layer GrassL where
  peel cell = case cell of
    Gem c k i (Just Grass) -> Just (GrassL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Grass
  layerCover = (defaultCover "grass")
    { lcPlace = overlayPlace Grass
    , lcStripsOnClear = const True
    }

-- | 藤：真消除时随格清掉；步末向相邻裸宝石蔓延。
data VineL = VineL
  deriving (Eq, Show)

instance Layer VineL where
  peel cell = case cell of
    Gem c k i (Just Vine) -> Just (VineL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Vine
  layerCover = (defaultCover "vine")
    { lcPlace = overlayPlace Vine
    , lcStripsOnClear = const True
    , lcSpreads = Just (10, VineL)
    }

-- | 巧克力：真消除时随格清掉；邻格真消除清掉它；步末蔓延。
data ChocoL = ChocoL
  deriving (Eq, Show)

instance Layer ChocoL where
  peel cell = case cell of
    Gem c k i (Just Choco) -> Just (ChocoL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Choco
  layerCover = (defaultCover "choco")
    { lcPlace = overlayPlace Choco
    , lcStripsOnClear = const True
    , lcNearPrio = Just 150
    , lcReach = AllNeighbours
    , lcOnNear = \_ cell -> Becomes (stripOverlay cell)
    , lcSpreads = Just (20, ChocoL)
    }

-- | 迷雾：挡匹配，邻消揭一层。
newtype FogL = FogL Int
  deriving (Eq, Show)

instance Layer FogL where
  peel cell = case cell of
    Gem c k i (Just (Fog n)) -> Just (FogL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (FogL n) = putOverlay (Fog n)
  layerCover = (defaultCover "fog")
    { lcPlace = layeredPlace Fog
    , lcBlocksMatch = const True
    , lcNearPrio = Just 70
    , lcOnNear = \(FogL n) -> chipLayer n FogL
    }

-- | 锁链：挡匹配 / 交换、不点火；直接命中与邻消各揭一层。
newtype ChainL = ChainL Int
  deriving (Eq, Show)

instance Layer ChainL where
  peel cell = case cell of
    Gem c k i (Just (Chain n)) -> Just (ChainL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (ChainL n) = putOverlay (Chain n)
  layerCover = (defaultCover "chain")
    { lcPlace = layeredPlace Chain
    , lcBlocksMatch = const True
    , lcBlocksSwap = const True
    , lcFires = const (Just False)
    , lcHit = \(ChainL n) -> peelHit n ChainL
    , lcNearPrio = Just 80
    , lcOnNear = \(ChainL n) -> chipLayer n ChainL
    }

-- | 火箭冰冻：不挡匹配、挡交换；邻消揭一层。
newtype FreezeL = FreezeL Int
  deriving (Eq, Show)

instance Layer FreezeL where
  peel cell = case cell of
    Gem c k i (Just (Freeze n)) -> Just (FreezeL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (FreezeL n) = putOverlay (Freeze n)
  layerCover = (defaultCover "freeze")
    { lcPlace = layeredPlace Freeze
    , lcBlocksSwap = const True
    , lcNearPrio = Just 90
    , lcOnNear = \(FreezeL n) -> chipLayer n FreezeL
    }

-- | 窗帘：挡匹配、不点火；直接命中与邻消各揭一层。
newtype CurtainL = CurtainL Int
  deriving (Eq, Show)

instance Layer CurtainL where
  peel cell = case cell of
    Gem c k i (Just (Curtain n)) -> Just (CurtainL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (CurtainL n) = putOverlay (Curtain n)
  layerCover = (defaultCover "curtain")
    { lcPlace = layeredPlace Curtain
    , lcBlocksMatch = const True
    , lcFires = const (Just False)
    , lcHit = \(CurtainL n) -> peelHit n CurtainL
    , lcNearPrio = Just 100
    , lcOnNear = \(CurtainL n) -> chipLayer n CurtainL
    }

-- | 蒸汽：挡匹配；邻格真消除清掉它；步末蔓延。
data SteamL = SteamL
  deriving (Eq, Show)

instance Layer SteamL where
  peel cell = case cell of
    Gem c k i (Just Steam) -> Just (SteamL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Steam
  layerCover = (defaultCover "steam")
    { lcPlace = overlayPlace Steam
    , lcBlocksMatch = const True
    , lcNearPrio = Just 160
    , lcReach = AllNeighbours
    , lcOnNear = \_ cell -> Becomes (stripOverlay cell)
    , lcSpreads = Just (30, SteamL)
    }

-- | 直接命中揭一层（锁链 / 窗帘）：宝石留下，不消除。
peelHit :: Int -> (Int -> l) -> LayerHit l
peelHit n con
  | n <= 1 = Peel
  | otherwise = Keep (con (n - 1))

-- | 邻格真消除揭一层（迷雾 / 锁链 / 火箭冰冻 / 窗帘）：末层去掉叠层，宝石留下。
chipLayer :: Layer l => Int -> (Int -> l) -> Cell -> Nudge
chipLayer n con cell
  | n <= 1 = Becomes (stripOverlay cell)
  | otherwise = Becomes (putOn (con (n - 1)) cell)

-- | 去掉宝石上的叠层（非宝石格不变）。
stripOverlay :: Cell -> Cell
stripOverlay cell = case cell of
  Gem c k i _ -> Gem c k i Nothing
  _ -> cell

--------------------------------------------------------------------------------
-- 条目

-- | 冰层：放置参数 = 冰层数（原样，精确匹配一个整数）。

-- | 放置一种无层数的叠层：换掉原格的叠层（原格不是宝石时不放）。
overlayPlace :: CellOverlay -> Placer
overlayPlace ov _ cell = case cell of
  Gem col kind ice _ -> Just (Gem col kind ice (Just ov))
  _ -> Nothing

-- | 放置一种带层数的叠层（精确一个整数参数）。
layeredPlace :: (Int -> CellOverlay) -> Placer
layeredPlace con args cell = case cell of
  Gem col kind ice _ -> (\n -> Gem col kind ice (Just (con n))) <$> exactArgs argInt args
  _ -> Nothing
