{-# LANGUAGE TypeApplications #-}
-- | 叠层邻消 / 蔓延的旧签名包装，只给叠层测试用。
--
-- 元素类重构第 3 刀起，迷雾 / 锁链 / 火箭冰冻 / 窗帘 / 巧克力 / 蒸汽的邻消是 'onLayerNeighbourClear' 方法 + 通用驱动
-- 'layerNeighbour'，藤 / 巧 / 蒸汽的步末蔓延是 'spreads' 方法 + 驱动 'layerSpread'（Match3.Grass 里的专门函数已删）。
module Spec.Support.Layers
  ( chipAdjacentFog
  , chipAdjacentChain
  , chipAdjacentFreeze
  , chipAdjacentCurtain
  , clearChocoAdjacent
  , clearSteamAdjacent
  , spreadVines
  , spreadChoco
  , spreadSteam
  ) where

import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Match3.Board.Grid (getCell)
import Match3.Element.Builtin.Layer (ChainL(..), ChocoL(..), CurtainL(..), FogL(..), FreezeL(..), SteamL(..), VineL(..))
import Match3.Element.Layer (Layer(..), peelAs)
import Match3.Element.Rules (layerNeighbour, layerSpread)
import Match3.Element.Types (AdjCtx(..), AdjOut(..), EndCtx(..))
import Match3.Types (Board, Pos, boardPositions)

-- | 用通用驱动跑一种叠层的邻格规则（没有直接命中的格）。
neighbourVia :: Layer l => Proxy l -> Board -> [Pos] -> Board
neighbourVia p b gems = aoBoard (layerNeighbour p (AdjCtx gems [] [] (const True)) b)

-- | 揭一层；返回（新盘面, 本次整层去掉的格数）。
chipVia :: Layer l => Proxy l -> Board -> [Pos] -> (Board, Int)
chipVia p b gems =
  let b' = neighbourVia p b gems
      has bd q = isJust (peelAs p (getCell bd q))
  in (b', length [q | q <- boardPositions b, has b q, not (has b' q)])

chipAdjacentFog, chipAdjacentChain, chipAdjacentFreeze, chipAdjacentCurtain :: Board -> [Pos] -> (Board, Int)
chipAdjacentFog = chipVia (Proxy @FogL)
chipAdjacentChain = chipVia (Proxy @ChainL)
chipAdjacentFreeze = chipVia (Proxy @FreezeL)
chipAdjacentCurtain = chipVia (Proxy @CurtainL)

-- | 巧克力 / 蒸汽：与真消除格相邻的整层去掉（不跳过直接命中）。
clearChocoAdjacent, clearSteamAdjacent :: Board -> [Pos] -> Board
clearChocoAdjacent = neighbourVia (Proxy @ChocoL)
clearSteamAdjacent = neighbourVia (Proxy @SteamL)

-- | 步末蔓延（只要盘面）。
spreadVia :: Layer l => Proxy l -> l -> Board -> Board
spreadVia p seed b = snd (layerSpread p seed noCtx b)
  where
    noCtx = EndCtx [] [] (const False)

spreadVines, spreadChoco, spreadSteam :: Board -> Board
spreadVines = spreadVia (Proxy @VineL) VineL
spreadChoco = spreadVia (Proxy @ChocoL) ChocoL
spreadSteam = spreadVia (Proxy @SteamL) SteamL
