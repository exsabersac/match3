{-# LANGUAGE RankNTypes #-}

-- | 叠层（草 / 藤 / 巧克力 / 迷雾 / 锁链 / 火箭冰冻 / 窗帘 / 蒸汽）的构造与谓词（第 6 刀从 Match3.Types 拆出）。
--
-- 依赖：Match3.Color、Match3.Types.Cell、Match3.Types.Optics（第 6 项）。
module Match3.Types.Overlay
  ( mkGrassGem
  , mkVineGem
  , mkChocoGem
  , mkFogGem
  , mkChainGem
  , mkFreezeGem
  , mkCurtainGem
  , hasGrass
  , hasVine
  , hasChoco
  , hasFog
  , fogLayers
  , hasChain
  , chainLayers
  , hasFreeze
  , freezeLayers
  , hasCurtain
  , curtainLayers
  , clearOverlay
  , setOverlay
  , mkSteamGem
  , hasSteam
  ) where

import Data.Maybe (fromMaybe)
import Engine.Optics
import Match3.Color (Color)
import Match3.Types.Cell
import Match3.Types.Optics (gemOverlay, overlay, _Chain, _Curtain, _Fog, _Freeze)

-- | Gem covered by grass (草坪): match on this cell clears the grass.
mkGrassGem :: Color -> Cell
mkGrassGem c = Gem c Normal 0 (Just Grass)

-- | Gem wrapped by vine (藤蔓): spreads after move unless cleared.
mkVineGem :: Color -> Cell
mkVineGem c = Gem c Normal 0 (Just Vine)

-- | Gem covered by chocolate (巧克力): cleared by adjacent match; spreads after move.
mkChocoGem :: Color -> Cell
mkChocoGem c = Gem c Normal 0 (Just Choco)

-- | Gem covered by fog / cloud (迷雾): adjacent clears peel one layer.
mkFogGem :: Color -> Int -> Cell
mkFogGem c n = Gem c Normal 0 (Just (Fog (max 1 n)))

-- | Gem locked by iron chain (锁链): adjacent clears peel; cannot swap/match while chained.
mkChainGem :: Color -> Int -> Cell
mkChainGem c n = Gem c Normal 0 (Just (Chain (max 1 n)))

-- | Gem sealed under rocket freeze (火箭冰冻): blocks swap only; adjacent clears peel.
-- Unlike Ice (match-chips gem ice) and Chain (blocks match too).
mkFreezeGem :: Color -> Int -> Cell
mkFreezeGem c n = Gem c Normal 0 (Just (Freeze (max 1 n)))

-- | Gem behind a curtain / roller shade (窗帘): adjacent clears peel; no match until clear.
mkCurtainGem :: Color -> Int -> Cell
mkCurtainGem c n = Gem c Normal 0 (Just (Curtain (max 1 n)))


-- 读数与写入（Haskell 特性第 6 项起用 Match3.Types.Optics 的光学写；第 6 项前每种叠层一个 case，语义逐项相同）：
-- hasX = 宝石上的叠层是不是 X；xLayers = X 的层数（不是 X 时 0）；clearOverlay / setOverlay = 写宝石的叠层槽（非宝石格不变）。

hasGrass :: Cell -> Bool
hasGrass = has (overlay . only Grass)

hasVine :: Cell -> Bool
hasVine = has (overlay . only Vine)

hasChoco :: Cell -> Bool
hasChoco = has (overlay . only Choco)

hasFog :: Cell -> Bool
hasFog = has (overlay . _Fog)

fogLayers :: Cell -> Int
fogLayers = layersOf _Fog

hasChain :: Cell -> Bool
hasChain = has (overlay . _Chain)

chainLayers :: Cell -> Int
chainLayers = layersOf _Chain

hasFreeze :: Cell -> Bool
hasFreeze = has (overlay . _Freeze)

freezeLayers :: Cell -> Int
freezeLayers = layersOf _Freeze

hasCurtain :: Cell -> Bool
hasCurtain = has (overlay . _Curtain)

curtainLayers :: Cell -> Int
curtainLayers = layersOf _Curtain

layersOf :: Prism' CellOverlay Int -> Cell -> Int
layersOf layer = fromMaybe 0 . preview (overlay . layer)

-- | Strip overlay, keep gem/ice.
clearOverlay :: Cell -> Cell
clearOverlay = set gemOverlay Nothing

setOverlay :: Maybe CellOverlay -> Cell -> Cell
setOverlay = set gemOverlay

-- | Gem covered by steam (蒸汽): blocks match; adjacent clear extinguishes; spreads after move.
mkSteamGem :: Color -> Cell
mkSteamGem c = Gem c Normal 0 (Just Steam)

hasSteam :: Cell -> Bool
hasSteam = has (overlay . only Steam)
