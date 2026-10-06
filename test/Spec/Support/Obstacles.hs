-- | Match3.Obstacles 邻消函数的无 except 写法（except = []，即本轮没有被直接命中的格），只给障碍测试用。
--
-- ecs-3 起邻消是各原型自带的邻格 system（'aSystems' 里的 SysNear）；这里的 chipAdjacent* 是 system 之上的
-- 旧签名包装（返回值语义不变）。
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

import Match3.Board.Grid (getCell)
import Match3.ECS.Archetype (Archetype(..))
import Match3.Element.Builtin.Obstacle (balloonPop, cakeArch, chestArch, honeyArch, safeArch, stoneArch)
import Match3.ECS.Stage (NearWorld(..), SysDef(..), nearWorld)
import Match3.ECS.System (System(..))
import Match3.Obstacles
  ( chargeAdjacentMakersSit
  , openSurprises
  , triggerAdjacentBottlesExcept
  , triggerAdjacentHatsExcept
  )
import Match3.Types (Board, CellContents(..), Pos, boardPositions)

-- | 跑一种本体原型自带的邻格 system：真消除 = gems，直接命中 = except；返回（新盘面, 打碎的位置）。
viaDriver :: Archetype s -> Board -> [Pos] -> [Pos] -> (Board, [Pos])
viaDriver a b gems except =
  let out = foldl (\w f -> runSystem f w) (nearWorld (const True) gems except [] b) [f | SysNear _ f <- aSystems a]
  in (nwBoard out, nwDead out)

-- | 石头 / 宝箱 / 蜂蜜 / 蛋糕：削一层；返回（新盘面, 末层被削掉、并入清除格的位置，后处理的在前）。
chipAdjacentStonesExcept, chipAdjacentChestsExcept, chipAdjacentHoneyExcept, chipAdjacentCakesExcept
  :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentStonesExcept = viaDriver stoneArch
chipAdjacentChestsExcept = viaDriver chestArch
chipAdjacentHoneyExcept = viaDriver honeyArch
chipAdjacentCakesExcept = viaDriver cakeArch

-- | 保险箱：削一层，末层原地开成饼干；返回（新盘面, 本次开成饼干的位置，行优先）。
chipAdjacentSafesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentSafesExcept b gems except =
  let (b', _) = viaDriver safeArch b gems except
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
popAdjacentBalloons b gems = let out = runSystem balloonPop (nearWorld (const True) gems [] [] b) in (nwBoard out, nwDead out)

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
