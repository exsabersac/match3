{-# LANGUAGE OverloadedStrings #-}
-- | 冰层与叠层：盖在宝石上的修饰器（Modifier，对应 xmonad 的 LayoutModifier）。
--
-- 共同特征：不占格，只在 Gem 格的冰层数 / overlay 字段里；由 Modified 包在本体外面，各方法「修饰器先说，
-- 没意见再问里面」。冰层削层点火；草 / 藤 / 巧随格清掉；迷雾 / 锁链 / 火箭冰冻 / 窗帘带层数、邻消揭一层；
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
  , iceEntry
  , grassEntry
  , vineEntry
  , chocoEntry
  , fogEntry
  , chainEntry
  , freezeEntry
  , curtainEntry
  , steamEntry
  ) where

import Match3.Board.Grid (getCell)
import Match3.Element.Class
import Match3.Element.Event
import Match3.Element.Registry
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

instance Modifier Ice where
  modName _ = "ice"
  modApply (Ice n) cell = case cell of
    Gem c k _ ov -> Gem c k n ov
    _ -> cell
  modActivates (Ice n) = Just (n <= 1)
  modOnHit (Ice n)
    | n > 1 = Keep (Ice (n - 1))
    | otherwise = Shatter

-- | 把叠层写回宝石格（替换原叠层）。
putOverlay :: CellOverlay -> Cell -> Cell
putOverlay ov cell = case cell of
  Gem c k i _ -> Gem c k i (Just ov)
  _ -> cell

-- | 草：真消除时随格清掉。
data GrassL = GrassL
  deriving (Eq, Show)

instance Modifier GrassL where
  modName _ = "grass"
  modApply _ = putOverlay Grass
  modStripOnClear _ = True

-- | 藤：真消除时随格清掉；步末向相邻裸宝石蔓延。
data VineL = VineL
  deriving (Eq, Show)

instance Modifier VineL where
  modName _ = "vine"
  modApply _ = putOverlay Vine
  modStripOnClear _ = True
  modEnd _ = Just (overlaySpreadRule 10 "vine" Vine spreadVines)

-- | 巧克力：真消除时随格清掉；邻格真消除清掉它；步末蔓延。
data ChocoL = ChocoL
  deriving (Eq, Show)

instance Modifier ChocoL where
  modName _ = "choco"
  modApply _ = putOverlay Choco
  modStripOnClear _ = True
  modAdjacent _ = Just (AdjacentRule 150 (\ctx b -> AdjOut (clearChocoAdjacent b (acTrue ctx)) [] []))
  modEnd _ = Just (overlaySpreadRule 20 "choco" Choco spreadChoco)

-- | 迷雾：挡匹配，邻消揭一层。
newtype FogL = FogL Int
  deriving (Eq, Show)

instance Modifier FogL where
  modName _ = "fog"
  modApply (FogL n) = putOverlay (Fog n)
  modBlocksMatch _ = True
  modAdjacent _ = Just (layerChip 70 chipAdjacentFogExcept)

-- | 锁链：挡匹配 / 交换、不点火；直接命中与邻消各揭一层。
newtype ChainL = ChainL Int
  deriving (Eq, Show)

instance Modifier ChainL where
  modName _ = "chain"
  modApply (ChainL n) = putOverlay (Chain n)
  modBlocksMatch _ = True
  modBlocksSwap _ = True
  modActivates _ = Just False
  modOnHit (ChainL n) = peel n ChainL
  modAdjacent _ = Just (layerChip 80 chipAdjacentChainExcept)

-- | 火箭冰冻：不挡匹配、挡交换；邻消揭一层。
newtype FreezeL = FreezeL Int
  deriving (Eq, Show)

instance Modifier FreezeL where
  modName _ = "freeze"
  modApply (FreezeL n) = putOverlay (Freeze n)
  modBlocksSwap _ = True
  modAdjacent _ = Just (layerChip 90 chipAdjacentFreezeExcept)

-- | 窗帘：挡匹配、不点火；直接命中与邻消各揭一层。
newtype CurtainL = CurtainL Int
  deriving (Eq, Show)

instance Modifier CurtainL where
  modName _ = "curtain"
  modApply (CurtainL n) = putOverlay (Curtain n)
  modBlocksMatch _ = True
  modActivates _ = Just False
  modOnHit (CurtainL n) = peel n CurtainL
  modAdjacent _ = Just (layerChip 100 chipAdjacentCurtainExcept)

-- | 蒸汽：挡匹配；邻格真消除清掉它；步末蔓延。
data SteamL = SteamL
  deriving (Eq, Show)

instance Modifier SteamL where
  modName _ = "steam"
  modApply _ = putOverlay Steam
  modBlocksMatch _ = True
  modAdjacent _ = Just (AdjacentRule 160 (\ctx b -> AdjOut (clearSteamAdjacent b (acTrue ctx)) [] []))
  modEnd _ = Just (overlaySpreadRule 30 "steam" Steam spreadSteam)

-- | 直接命中揭一层（锁链 / 窗帘）：宝石留下，不消除。
peel :: Int -> (Int -> m) -> ModHit m
peel n con
  | n <= 1 = Remove
  | otherwise = Keep (con (n - 1))

-- | 叠层的邻消规则：揭一层，不打碎格子。
layerChip :: Int -> (Board -> [Pos] -> [Pos] -> (Board, Int)) -> AdjacentRule
layerChip order f = AdjacentRule order (\ctx b -> AdjOut (fst (f b (acTrue ctx) (acDirect ctx))) [] [])

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

-- | 冰层：放置参数 = 冰层数（原样）。
iceEntry :: Entry
iceEntry = modifierEntry (Ice 1) (\cell -> case cell of Gem _ _ n _ -> Just (Ice n); _ -> Nothing) $ \args cell -> case (args, cell) of
  ([AInt n], Gem col kind _ ov) -> Just (Gem col kind n ov)
  _ -> Nothing

grassEntry, vineEntry, chocoEntry, fogEntry, chainEntry, freezeEntry, curtainEntry, steamEntry :: Entry
grassEntry = overlay GrassL Grass (\o -> case o of Grass -> Just GrassL; _ -> Nothing)
vineEntry = overlay VineL Vine (\o -> case o of Vine -> Just VineL; _ -> Nothing)
chocoEntry = overlay ChocoL Choco (\o -> case o of Choco -> Just ChocoL; _ -> Nothing)
fogEntry = layeredOverlay (FogL 1) (\o -> case o of Fog n -> Just (FogL n); _ -> Nothing) Fog
chainEntry = layeredOverlay (ChainL 1) (\o -> case o of Chain n -> Just (ChainL n); _ -> Nothing) Chain
freezeEntry = layeredOverlay (FreezeL 1) (\o -> case o of Freeze n -> Just (FreezeL n); _ -> Nothing) Freeze
curtainEntry = layeredOverlay (CurtainL 1) (\o -> case o of Curtain n -> Just (CurtainL n); _ -> Nothing) Curtain
steamEntry = overlay SteamL Steam (\o -> case o of Steam -> Just SteamL; _ -> Nothing)

ovDecode :: (CellOverlay -> Maybe m) -> Cell -> Maybe m
ovDecode f cell = case cell of
  Gem _ _ _ (Just o) -> f o
  _ -> Nothing

-- | 叠层放置 = 盖在宝石上（替换原叠层）。原型值、放置写的叠层、解码。
overlay :: Modifier m => m -> CellOverlay -> (CellOverlay -> Maybe m) -> Entry
overlay proto ov f = modifierEntry proto (ovDecode f) $ \_ cell -> case cell of
  Gem col kind ice _ -> Just (Gem col kind ice (Just ov))
  _ -> Nothing

-- | 带层数的叠层：放置参数 = 层数（原样）。
layeredOverlay :: Modifier m => m -> (CellOverlay -> Maybe m) -> (Int -> CellOverlay) -> Entry
layeredOverlay proto f con = modifierEntry proto (ovDecode f) $ \args cell -> case (args, cell) of
  ([AInt n], Gem col kind ice _) -> Just (Gem col kind ice (Just (con n)))
  _ -> Nothing
