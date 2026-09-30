-- | 叠层（草 / 藤 / 巧克力 / 迷雾 / 锁链 / 火箭冰冻 / 窗帘 / 蒸汽）的构造与谓词（第 6 刀从 Match3.Types 拆出）。
--
-- 依赖：Match3.Color、Match3.Types.Cell。
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

import Match3.Color (Color)
import Match3.Types.Cell

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


hasGrass :: Cell -> Bool
hasGrass c = cellOverlay c == Just Grass

hasVine :: Cell -> Bool
hasVine c = cellOverlay c == Just Vine

hasChoco :: Cell -> Bool
hasChoco c = cellOverlay c == Just Choco

hasFog :: Cell -> Bool
hasFog c = case cellOverlay c of
  Just (Fog _) -> True
  _ -> False

fogLayers :: Cell -> Int
fogLayers c = case cellOverlay c of
  Just (Fog n) -> n
  _ -> 0

hasChain :: Cell -> Bool
hasChain c = case cellOverlay c of
  Just (Chain _) -> True
  _ -> False

chainLayers :: Cell -> Int
chainLayers c = case cellOverlay c of
  Just (Chain n) -> n
  _ -> 0

hasFreeze :: Cell -> Bool
hasFreeze c = case cellOverlay c of
  Just (Freeze _) -> True
  _ -> False

freezeLayers :: Cell -> Int
freezeLayers c = case cellOverlay c of
  Just (Freeze n) -> n
  _ -> 0

hasCurtain :: Cell -> Bool
hasCurtain c = case cellOverlay c of
  Just (Curtain _) -> True
  _ -> False

curtainLayers :: Cell -> Int
curtainLayers c = case cellOverlay c of
  Just (Curtain n) -> n
  _ -> 0

-- | Strip overlay, keep gem/ice.
clearOverlay :: Cell -> Cell
clearOverlay (Gem col kind ice _) = Gem col kind ice Nothing
clearOverlay x = x

setOverlay :: Maybe CellOverlay -> Cell -> Cell
setOverlay o (Gem col kind ice _) = Gem col kind ice o
setOverlay _ x = x


-- | Gem covered by steam (蒸汽): blocks match; adjacent clear extinguishes; spreads after move.
mkSteamGem :: Color -> Cell
mkSteamGem c = Gem c Normal 0 (Just Steam)

hasSteam :: Cell -> Bool
hasSteam c = cellOverlay c == Just Steam

