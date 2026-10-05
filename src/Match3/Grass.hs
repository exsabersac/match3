{-# LANGUAGE RankNTypes #-}

-- | 宝石叠层的查询与构造：草/藤/巧克力/迷雾/锁链/火箭冰冻/窗帘/蒸汽的位置、层数、构造器，以及真清除格上
-- 随格清掉草 / 藤 / 巧（'clearOverlaysOn'）。不拥有蜗牛、冰层 Int、或 Game 步末编排顺序。
--
-- 邻消揭层 / 清掉与步末蔓延自元素类重构第 3 刀起是叠层方法（onLayerNeighbourClear / spreads）+ 通用驱动
-- （Match3.Element.Rules.layerNeighbour / layerSpread），不在这里（Haskell 特性第 6 项按光学去重的那几份已随之删除）。
module Match3.Grass
  ( clearOverlaysOn
  , vinePositions
  , chocoPositions
  , fogPositions
  , chainPositions
  , freezePositions
  , curtainPositions
  , steamPositions
  , mkGrassGem
  , mkVineGem
  , mkChocoGem
  , mkFogGem
  , mkChainGem
  , mkFreezeGem
  , mkCurtainGem
  , mkSteamGem
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
  , hasSteam
  , cellOverlay
  ) where

import Data.List (nub)
import Data.Monoid (Any)
import Engine.Optics
import Match3.Types
import Match3.Types.Optics (cellAt, gemOverlay, overlay, _Chain, _Curtain, _Fog, _Freeze)

at :: Board -> Pos -> Cell
at = boardAt

-- | 界内邻格，上 / 下 / 左 / 右（蔓延目标的先后由它定）。
-- | 盘面上焦点非空的格（行主序）。
positionsWith :: Getting Any Cell a -> Board -> [Pos]
positionsWith o = positionsWhere (has o)

-- | 宝石上恰好是这种叠层（草 / 藤 / 巧克力 / 蒸汽这类不带参数的叠层）。
overlayIs :: CellOverlay -> Traversal' Cell ()
overlayIs o = overlay . only o

-- | 去掉 p 格宝石上的叠层（非宝石格不变）。
stripAt :: Board -> Pos -> Board
stripAt board p = board & cellAt p . gemOverlay .~ Nothing

-- | Strip Grass/Vine/Choco on *true clear* cells (so they will not spread).
-- Callers must pass iceFree/trueClears — not raw expand seeds — so soft hits
-- (ice>1 chip / Flip / Chain·Curtain peel) keep on-cell overlays.
-- Does NOT strip peel-locks (Chain/Curtain/Fog/Freeze/Steam) — those peel via
-- chipAdjacent* or direct-hit peel in chipIceOnClear (hammer/cross/line).
clearOverlaysOn :: Board -> [Pos] -> Board
clearOverlaysOn b seeds = foldl strip b (nub seeds)
  where
    strip board p
      | any (\o -> has (overlayIs o) (at board p)) [Grass, Vine, Choco] = stripAt board p
      | otherwise = board

vinePositions :: Board -> [Pos]
vinePositions = positionsWith (overlayIs Vine)

chocoPositions :: Board -> [Pos]
chocoPositions = positionsWith (overlayIs Choco)

fogPositions :: Board -> [Pos]
fogPositions = positionsWith (overlay . _Fog)

chainPositions :: Board -> [Pos]
chainPositions = positionsWith (overlay . _Chain)

freezePositions :: Board -> [Pos]
freezePositions = positionsWith (overlay . _Freeze)

curtainPositions :: Board -> [Pos]
curtainPositions = positionsWith (overlay . _Curtain)

steamPositions :: Board -> [Pos]
steamPositions = positionsWith (overlayIs Steam)
