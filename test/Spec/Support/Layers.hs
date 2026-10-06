-- | 叠层邻消 / 蔓延的旧签名包装，只给叠层测试用。
--
-- ecs-4 起叠层是叠层原型（Match3.ECS.Cover）：迷雾 / 锁链 / 火箭冰冻 / 窗帘 / 巧克力 / 蒸汽的邻消是 cvSystems 里的
-- 邻格 system（Rules.coverNear），藤 / 巧 / 蒸汽的步末蔓延是 cvSystems 里的步末 system（Rules.coverSpread）。
-- 这里直接跑叠层原型自带的 system。
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
import Match3.Board.Grid (getCell)
import Match3.ECS.Cover (Cover(..), peelWith)
import Match3.Element.Builtin.Layer (chainCover, chocoCover, curtainCover, fogCover, freezeCover, steamCover, vineCover)
import Match3.ECS.Stage (EndSys(..), EndWorld(..), NearWorld(..), SysDef(..), endWorld, nearWorld)
import Match3.ECS.System (System(..))
import Match3.Types (Board, Pos, boardPositions)

-- | 跑一种叠层的邻格 system（没有直接命中的格）。
neighbourVia :: Cover l -> Board -> [Pos] -> Board
neighbourVia cv b gems = foldl (\bd s -> nwBoard (runSystem s (nearWorld (const True) gems [] [] bd))) b [s | SysNear _ s <- cvSystems cv]

-- | 揭一层；返回（新盘面, 本次整层去掉的格数）。
chipVia :: Cover l -> Board -> [Pos] -> (Board, Int)
chipVia cv b gems =
  let b' = neighbourVia cv b gems
      has bd q = isJust (peelWith cv (getCell bd q))
  in (b', length [q | q <- boardPositions b, has b q, not (has b' q)])

chipAdjacentFog, chipAdjacentChain, chipAdjacentFreeze, chipAdjacentCurtain :: Board -> [Pos] -> (Board, Int)
chipAdjacentFog = chipVia fogCover
chipAdjacentChain = chipVia chainCover
chipAdjacentFreeze = chipVia freezeCover
chipAdjacentCurtain = chipVia curtainCover

-- | 巧克力 / 蒸汽：与真消除格相邻的整层去掉（不跳过直接命中）。
clearChocoAdjacent, clearSteamAdjacent :: Board -> [Pos] -> Board
clearChocoAdjacent = neighbourVia chocoCover
clearSteamAdjacent = neighbourVia steamCover

-- | 步末蔓延（只要盘面）：跑叠层原型自带的步末 system。
spreadVia :: Cover l -> Board -> Board
spreadVia cv b = foldl (\bd e -> ewBoard (runSystem (esSystem e) (endWorld [] [] (const False) bd))) b [e | SysEnd e <- cvSystems cv]

spreadVines, spreadChoco, spreadSteam :: Board -> Board
spreadVines = spreadVia vineCover
spreadChoco = spreadVia chocoCover
spreadSteam = spreadVia steamCover
