{-# LANGUAGE OverloadedStrings #-}
-- | 冰层与叠层：盖在宝石上的叠层（'Layer'，对应 xmonad 的 LayoutModifier）。
--
-- 共同特征：不占格，只在 Gem 格的冰层数 / overlay 字段里；解码时由 'Layered' 包在本体外面，合成规则
-- （本层先回答，没意见再问里面）只写在 Match3.Element.Layer 里一次。冰层削层点火；草 / 藤 / 巧随格清掉；迷雾 / 锁链 / 火箭冰冻 / 窗帘带层数、邻消揭一层；
-- 巧克力 / 蒸汽被邻格真消除清掉；藤 10 → 巧 20 → 蒸汽 30 步末蔓延（PhaseSpread）。
-- 邻格规则顺序：迷雾 70 → 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 巧克力 150 → 蒸汽 160。
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

import Match3.Board.Grid (getCell)
import Match3.Element.Event
import Match3.Element.Kind (BoardPass(..))
import Match3.Element.Layer
import Match3.Element.Types
import Match3.Grass
  ( chipAdjacentChainExcept
  , chipAdjacentCurtainExcept
  , chipAdjacentFogExcept
  , chipAdjacentFreezeExcept
  , clearChocoAdjacent
  , clearSteamAdjacent
  , spreadChoco
  , spreadSteam
  , spreadVines
  )
import Match3.Types

-- | 冰层：不挡匹配 / 交换；多层冰只削一层（不点火），末层冰随宝石一起碎（并点火）。
newtype Ice = Ice Int
  deriving (Eq, Show)

instance Layer Ice where
  layerName _ = "ice"
  peel cell = case cell of
    Gem c k n ov | n > 0 -> Just (Ice n, Gem c k 0 ov)
    _ -> Nothing
  putOn (Ice n) cell = case cell of
    Gem c k _ ov -> Gem c k n ov
    _ -> cell
  layerFires (Ice n) = Just (n <= 1)
  layerHit (Ice n)
    | n > 1 = Keep (Ice (n - 1))
    | otherwise = Shatter
  -- 放置：设冰层数（精确一个整数参数）；原格不是宝石时不放
  layerPlace _ args cell = case cell of
    Gem col kind _ ov -> (\n -> Gem col kind n ov) <$> exactArgs argInt args
    _ -> Nothing

-- | 把叠层写回宝石格（替换原叠层）。
putOverlay :: CellOverlay -> Cell -> Cell
putOverlay ov cell = case cell of
  Gem c k i _ -> Gem c k i (Just ov)
  _ -> cell

-- | 草：真消除时随格清掉。
data GrassL = GrassL
  deriving (Eq, Show)

instance Layer GrassL where
  layerName _ = "grass"
  peel cell = case cell of
    Gem c k i (Just Grass) -> Just (GrassL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Grass
  layerStripsOnClear _ = True
  layerPlace _ = overlayPlace Grass

-- | 藤：真消除时随格清掉；步末向相邻裸宝石蔓延。
data VineL = VineL
  deriving (Eq, Show)

instance Layer VineL where
  layerName _ = "vine"
  peel cell = case cell of
    Gem c k i (Just Vine) -> Just (VineL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Vine
  layerStripsOnClear _ = True
  layerPasses _ = [EndPass (overlaySpreadRule 10 "vine" Vine spreadVines)]
  layerPlace _ = overlayPlace Vine

-- | 巧克力：真消除时随格清掉；邻格真消除清掉它；步末蔓延。
data ChocoL = ChocoL
  deriving (Eq, Show)

instance Layer ChocoL where
  layerName _ = "choco"
  peel cell = case cell of
    Gem c k i (Just Choco) -> Just (ChocoL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Choco
  layerStripsOnClear _ = True
  layerPasses _ =
    [ AdjacentPass 150 (\ctx b -> AdjOut (clearChocoAdjacent b (acTrue ctx)) [] [])
    , EndPass (overlaySpreadRule 20 "choco" Choco spreadChoco)
    ]
  layerPlace _ = overlayPlace Choco

-- | 迷雾：挡匹配，邻消揭一层。
newtype FogL = FogL Int
  deriving (Eq, Show)

instance Layer FogL where
  layerName _ = "fog"
  peel cell = case cell of
    Gem c k i (Just (Fog n)) -> Just (FogL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (FogL n) = putOverlay (Fog n)
  layerBlocksMatch _ = True
  layerPasses _ = [layerChip 70 chipAdjacentFogExcept]
  layerPlace _ = layeredPlace Fog

-- | 锁链：挡匹配 / 交换、不点火；直接命中与邻消各揭一层。
newtype ChainL = ChainL Int
  deriving (Eq, Show)

instance Layer ChainL where
  layerName _ = "chain"
  peel cell = case cell of
    Gem c k i (Just (Chain n)) -> Just (ChainL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (ChainL n) = putOverlay (Chain n)
  layerBlocksMatch _ = True
  layerBlocksSwap _ = True
  layerFires _ = Just False
  layerHit (ChainL n) = peelHit n ChainL
  layerPasses _ = [layerChip 80 chipAdjacentChainExcept]
  layerPlace _ = layeredPlace Chain

-- | 火箭冰冻：不挡匹配、挡交换；邻消揭一层。
newtype FreezeL = FreezeL Int
  deriving (Eq, Show)

instance Layer FreezeL where
  layerName _ = "freeze"
  peel cell = case cell of
    Gem c k i (Just (Freeze n)) -> Just (FreezeL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (FreezeL n) = putOverlay (Freeze n)
  layerBlocksSwap _ = True
  layerPasses _ = [layerChip 90 chipAdjacentFreezeExcept]
  layerPlace _ = layeredPlace Freeze

-- | 窗帘：挡匹配、不点火；直接命中与邻消各揭一层。
newtype CurtainL = CurtainL Int
  deriving (Eq, Show)

instance Layer CurtainL where
  layerName _ = "curtain"
  peel cell = case cell of
    Gem c k i (Just (Curtain n)) -> Just (CurtainL n, Gem c k i Nothing)
    _ -> Nothing
  putOn (CurtainL n) = putOverlay (Curtain n)
  layerBlocksMatch _ = True
  layerFires _ = Just False
  layerHit (CurtainL n) = peelHit n CurtainL
  layerPasses _ = [layerChip 100 chipAdjacentCurtainExcept]
  layerPlace _ = layeredPlace Curtain

-- | 蒸汽：挡匹配；邻格真消除清掉它；步末蔓延。
data SteamL = SteamL
  deriving (Eq, Show)

instance Layer SteamL where
  layerName _ = "steam"
  peel cell = case cell of
    Gem c k i (Just Steam) -> Just (SteamL, Gem c k i Nothing)
    _ -> Nothing
  putOn _ = putOverlay Steam
  layerBlocksMatch _ = True
  layerPasses _ =
    [ AdjacentPass 160 (\ctx b -> AdjOut (clearSteamAdjacent b (acTrue ctx)) [] [])
    , EndPass (overlaySpreadRule 30 "steam" Steam spreadSteam)
    ]
  layerPlace _ = overlayPlace Steam

-- | 直接命中揭一层（锁链 / 窗帘）：宝石留下，不消除。
peelHit :: Int -> (Int -> l) -> LayerHit l
peelHit n con
  | n <= 1 = Peel
  | otherwise = Keep (con (n - 1))

-- | 叠层的邻消规则：揭一层，不打碎格子。
layerChip :: Int -> (Board -> [Pos] -> [Pos] -> (Board, Int)) -> BoardPass
layerChip order f = AdjacentPass order (\ctx b -> AdjOut (fst (f b (acTrue ctx) (acDirect ctx))) [] [])

-- | 蔓延：每只幸存的叠层向正交相邻的裸宝石长一格；记录 (来源, 新格)。
overlaySpreadRule :: Int -> ElementName -> CellOverlay -> (Board -> Board) -> EndRule
overlaySpreadRule order nm ov spread = spreadRule order run
  where
    run _ b =
      let b' = spread b
          ps = spreadPairs ov b b'
      in (if null ps then Nothing else Just (EndEffect EvSpread nm [EndItem src q (getCell b' q) Nothing | (src, q) <- ps]), b')

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
