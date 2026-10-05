{-# LANGUAGE TypeApplications #-}
-- | Match3.Obstacles 邻消函数的无 except 写法（except = []，即本轮没有被直接命中的格），只给障碍测试用。
--
-- 元素类重构第 3 刀起，多层障碍 / 保险箱 / 时间精灵的邻消不再有专门函数，而是 'onNeighbourClear' 方法 + 通用驱动
-- 'kindNeighbour'；这里的 chipAdjacent* 是驱动之上的旧签名包装（返回值语义不变）。
module Spec.Support.Obstacles
  ( chipAdjacentStonesExcept
  , chipAdjacentChestsExcept
  , chipAdjacentHoneyExcept
  , chipAdjacentCakesExcept
  , chipAdjacentSafesExcept
  , chipAdjacentStones
  , chipAdjacentChests
  , chipAdjacentHoney
  , chipAdjacentCakes
  , chipAdjacentSafes
  , popAdjacentBalloons
  , openAdjacentSurprises
  , triggerAdjacentHats
  , triggerAdjacentBottles
  , chargeAdjacentMakers
  ) where

import Data.Proxy (Proxy(..))
import Match3.Board.Grid (getCell)
import Match3.Element.Builtin.Obstacle (CakeE, ChestE, HoneyE, SafeE, StoneE, balloonPop)
import Match3.Element.Kind (Kind)
import Match3.Element.Rules (kindNeighbour)
import Match3.Element.Types (AdjCtx(..), AdjOut(..))
import Match3.Obstacles
  ( chargeAdjacentMakersSit
  , openSurprises
  , triggerAdjacentBottlesExcept
  , triggerAdjacentHatsExcept
  )
import Match3.Types (Board, CellContents(..), Pos, boardPositions)

-- | 用通用驱动跑一种本体的邻格规则：真消除 = gems，直接命中 = except；返回（新盘面, 打碎的位置）。
viaDriver :: Kind e => Proxy e -> Board -> [Pos] -> [Pos] -> (Board, [Pos])
viaDriver p b gems except =
  let out = kindNeighbour p (AdjCtx gems except [] (const True)) b
  in (aoBoard out, aoDead out)

-- | 石头 / 宝箱 / 蜂蜜 / 蛋糕：削一层；返回（新盘面, 末层被削掉、并入清除格的位置，后处理的在前）。
chipAdjacentStonesExcept, chipAdjacentChestsExcept, chipAdjacentHoneyExcept, chipAdjacentCakesExcept
  :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentStonesExcept = viaDriver (Proxy @StoneE)
chipAdjacentChestsExcept = viaDriver (Proxy @ChestE)
chipAdjacentHoneyExcept = viaDriver (Proxy @HoneyE)
chipAdjacentCakesExcept = viaDriver (Proxy @CakeE)

-- | 保险箱：削一层，末层原地开成饼干；返回（新盘面, 本次开成饼干的位置，行优先）。
chipAdjacentSafesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentSafesExcept b gems except =
  let (b', _) = viaDriver (Proxy @SafeE) b gems except
      opened q = case (getCell b q, getCell b' q) of
        (Safe _, Cookie) -> True
        _ -> False
  in (b', filter opened (boardPositions b))

-- | 邻消削一层；返回（新盘面, 最后一层被削掉的位置）。
chipAdjacentStones, chipAdjacentChests, chipAdjacentHoney, chipAdjacentCakes, chipAdjacentSafes
  :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentStones b gems = chipAdjacentStonesExcept b gems []
chipAdjacentChests b gems = chipAdjacentChestsExcept b gems []
chipAdjacentHoney b gems = chipAdjacentHoneyExcept b gems []
chipAdjacentCakes b gems = chipAdjacentCakesExcept b gems []
chipAdjacentSafes b gems = chipAdjacentSafesExcept b gems []

-- | 同色邻消打爆的气球位置（盘面不变，由清除管线移走）：气球自己的邻格规则 'balloonPop'，没有直接命中格。
popAdjacentBalloons :: Board -> [Pos] -> (Board, [Pos])
popAdjacentBalloons b gems = let out = balloonPop (AdjCtx gems [] [] (const True)) b in (aoBoard out, aoDead out)

-- | 'openSurprises' 去掉「原地变成特殊块的位置」；返回（新盘面, 爆炸种子）。
openAdjacentSurprises :: Board -> [Pos] -> (Board, [Pos])
openAdjacentSurprises b gems = let (b', expl, _) = openSurprises b gems in (b', expl)

-- | 魔法帽 / 染色瓶改色，不保护任何格。
triggerAdjacentHats, triggerAdjacentBottles :: Board -> [Pos] -> Board
triggerAdjacentHats b gems = triggerAdjacentHatsExcept b gems []
triggerAdjacentBottles b gems = triggerAdjacentBottlesExcept b gems []

-- | 果汁机充能，只要盘面。
chargeAdjacentMakers :: Board -> [Pos] -> Board
chargeAdjacentMakers b gems = fst (chargeAdjacentMakersSit b gems)
