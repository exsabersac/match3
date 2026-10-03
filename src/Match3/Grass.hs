{-# LANGUAGE RankNTypes #-}

-- | 宝石叠层：草/藤/巧克力/迷雾/锁链/火箭冰冻/窗帘/蒸汽的清除、揭层与步末蔓延。
-- 草随本格真清除；藤/巧/蒸汽步末蔓延（已清则不蔓）；雾/链/冻/帘邻消揭层。
-- 不拥有蜗牛、冰层 Int、或 Game 步末编排顺序。
--
-- Haskell 特性第 6 项（docs/haskell-features/06-测试与光学.md）：第 6 项前四种揭层叠层（雾 / 链 / 冻 / 帘）各有一份
-- 逐字相同的「邻格目标 + 揭一层」、三种蔓延叠层（藤 / 巧 / 蒸汽）各有一份「找裸宝石 + 种上」、两种邻消叠层（巧 / 蒸汽）
-- 各有一份「邻格目标 + 去掉」，只差构造器。现在每类写一次，差别（哪种叠层）作为光学传进来：
-- 揭层叠层传棱镜（_Fog 等，Match3.Types.Optics），其余传叠层值本身（only Vine 等）。
-- 对外导出与语义逐项不变（Spec.Optics 与第 6 项前的逐字副本对照）。
module Match3.Grass
  ( clearOverlaysOn
  , clearChocoAdjacent
  , clearSteamAdjacent
  , chipAdjacentFog
  , chipAdjacentFogExcept
  , chipAdjacentChain
  , chipAdjacentChainExcept
  , chipAdjacentFreeze
  , chipAdjacentFreezeExcept
  , chipAdjacentCurtain
  , chipAdjacentCurtainExcept
  , vinePositions
  , chocoPositions
  , fogPositions
  , chainPositions
  , freezePositions
  , curtainPositions
  , steamPositions
  , spreadVines
  , spreadChoco
  , spreadSteam
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
import Match3.Board.Grid (neighborsInBounds)
import Match3.Types
import Match3.Types.Optics (cellAt, gemOverlay, overlay, _Chain, _Curtain, _Fog, _Freeze)

at :: Board -> Pos -> Cell
at = boardAt

-- | 界内邻格，上 / 下 / 左 / 右（蔓延目标的先后由它定）。
ortho :: Board -> Pos -> [Pos]
ortho = neighborsInBounds upDownLeftRight

-- | 盘面上焦点非空的格（行主序）。
positionsWith :: Getting Any Cell a -> Board -> [Pos]
positionsWith o b = [p | p <- boardPositions b, has o (at b p)]

-- | 宝石上恰好是这种叠层（草 / 藤 / 巧克力 / 蒸汽这类不带参数的叠层）。
overlayIs :: CellOverlay -> Traversal' Cell ()
overlayIs o = overlay . only o

-- | 没有叠层的宝石（蔓延的落点）。
bareGem :: Cell -> Bool
bareGem cell = preview gemOverlay cell == Just Nothing

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

-- | 邻消即去的叠层（巧克力 / 蒸汽）：与 seeds 正交相邻、带这种叠层的格去掉叠层，宝石留下。
clearAdjacentOverlay :: CellOverlay -> Board -> [Pos] -> Board
clearAdjacentOverlay o b seeds =
  foldl strip b targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , has (overlayIs o) (at b q)
        ]
    strip board p
      | has (overlayIs o) (at board p) = stripAt board p
      | otherwise = board

-- | Chocolate (巧克力): cleared when orthogonally adjacent to a *true* clear hole.
-- Callers must pass iceFree/surpFree (not raw expand seeds): ice-chip / Flip soft
-- hits must not extinguish chocolate. Gem under chocolate stays.
clearChocoAdjacent :: Board -> [Pos] -> Board
clearChocoAdjacent = clearAdjacentOverlay Choco

-- | 带层数的叠层（迷雾 / 锁链 / 火箭冰冻 / 窗帘，由棱镜 layer 指定）：与 seeds 正交相邻、不在 except 里的格揭一层；
-- 1 层（或更少）时去掉叠层并计数，否则层数减一。宝石留下。返回 (新盘面, 揭完的个数)。
chipAdjacentLayerExcept :: Prism' CellOverlay Int -> Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentLayerExcept layer b seeds except =
  foldl hit (b, 0) targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , q `notElem` except
        , has (overlay . layer) (at b q)
        ]
    hit (board, n) p =
      case at board p ^? overlay . layer of
        Just layers
          | layers <= 1 -> (stripAt board p, n + 1)
          | otherwise -> (board & cellAt p . overlay . layer .~ layers - 1, n)
        Nothing -> (board, n)

-- | Fog / cloud (迷雾): peel one layer on cells orthogonally adjacent to clears.
-- Fog 1 -> strip; Fog n>1 -> Fog (n-1). Gem stays. Returns fully cleared fog count.
chipAdjacentFog :: Board -> [Pos] -> (Board, Int)
chipAdjacentFog b seeds = chipAdjacentFogExcept b seeds []

-- | Like chipAdjacentFog but skips cells in 'except' (already direct-hit this wave).
chipAdjacentFogExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentFogExcept = chipAdjacentLayerExcept _Fog

vinePositions :: Board -> [Pos]
vinePositions = positionsWith (overlayIs Vine)

chocoPositions :: Board -> [Pos]
chocoPositions = positionsWith (overlayIs Choco)

fogPositions :: Board -> [Pos]
fogPositions = positionsWith (overlay . _Fog)

-- | Chain / iron lock (锁链): peel one layer on cells orthogonally adjacent to clears.
-- Chain 1 -> strip; Chain n>1 -> Chain (n-1). Gem stays. Returns fully unlocked count.
chipAdjacentChain :: Board -> [Pos] -> (Board, Int)
chipAdjacentChain b seeds = chipAdjacentChainExcept b seeds []

-- | Like chipAdjacentChain but skips cells in 'except' (already direct-hit this wave).
chipAdjacentChainExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentChainExcept = chipAdjacentLayerExcept _Chain

chainPositions :: Board -> [Pos]
chainPositions = positionsWith (overlay . _Chain)

-- | Rocket freeze (火箭冰冻): peel one layer on cells orthogonally adjacent to clears.
-- Freeze 1 -> strip; Freeze n>1 -> Freeze (n-1). Gem stays and can still match.
-- Returns fully thawed count.
chipAdjacentFreeze :: Board -> [Pos] -> (Board, Int)
chipAdjacentFreeze b seeds = chipAdjacentFreezeExcept b seeds []

-- | Like chipAdjacentFreeze but skips cells in 'except' (already direct-hit this wave).
chipAdjacentFreezeExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentFreezeExcept = chipAdjacentLayerExcept _Freeze

freezePositions :: Board -> [Pos]
freezePositions = positionsWith (overlay . _Freeze)

-- | Curtain / roller shade (窗帘): peel one layer on cells orthogonally adjacent to clears.
-- Curtain 1 -> strip; Curtain n>1 -> Curtain (n-1). Gem stays. Returns fully cleared count.
chipAdjacentCurtain :: Board -> [Pos] -> (Board, Int)
chipAdjacentCurtain b seeds = chipAdjacentCurtainExcept b seeds []

-- | Like chipAdjacentCurtain but skips cells in 'except' (already direct-hit this wave).
chipAdjacentCurtainExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentCurtainExcept = chipAdjacentLayerExcept _Curtain

curtainPositions :: Board -> [Pos]
curtainPositions = positionsWith (overlay . _Curtain)

-- | 蔓延叠层 o（藤 / 巧克力 / 蒸汽）：现有的每一格向正交相邻的裸宝石（无叠层）种上 o；新种上的这一步不再蔓延。
spreadLayer :: CellOverlay -> Board -> Board
spreadLayer o b =
  let sources = positionsWith (overlayIs o) b
      targets =
        nub
          [ q
          | p <- sources
          , q <- ortho b p
          , bareGem (at b q)
          ]
  in foldl plant b targets
  where
    plant board p
      | bareGem (at board p) = board & cellAt p . gemOverlay .~ Just o
      | otherwise = board

-- | Each remaining vine spreads onto every orthogonally adjacent bare gem
-- (no overlay, not stone). Newly placed vines do not chain-spread this turn.
spreadVines :: Board -> Board
spreadVines = spreadLayer Vine

-- | Surviving chocolate spreads onto adjacent bare gems (same rules as vine).
-- Cleared chocolate is already stripped, so it cannot seed new spreads.
spreadChoco :: Board -> Board
spreadChoco = spreadLayer Choco

steamPositions :: Board -> [Pos]
steamPositions = positionsWith (overlayIs Steam)

-- | Steam (蒸汽): extinguished when orthogonally adjacent to a *true* clear hole.
-- Same seed discipline as clearChocoAdjacent (no soft-hit extinguish).
clearSteamAdjacent :: Board -> [Pos] -> Board
clearSteamAdjacent = clearAdjacentOverlay Steam

-- | Surviving steam spreads onto adjacent bare gems (same rules as vine/choco).
spreadSteam :: Board -> Board
spreadSteam = spreadLayer Steam
