-- | Match3.Obstacles 邻消函数的无 except 写法（except = []，即本轮没有被直接命中的格），只给障碍测试用。
module Spec.Support.Obstacles
  ( chipAdjacentStones
  , chipAdjacentChests
  , chipAdjacentHoney
  , chipAdjacentCakes
  , chipAdjacentSafes
  , chipAdjacentBalloons
  , openAdjacentSurprises
  , triggerAdjacentHats
  , triggerAdjacentBottles
  , chargeAdjacentMakers
  ) where

import Match3.Obstacles
  ( chargeAdjacentMakersSit
  , chipAdjacentBalloonsExcept
  , chipAdjacentCakesExcept
  , chipAdjacentChestsExcept
  , chipAdjacentHoneyExcept
  , chipAdjacentSafesExcept
  , chipAdjacentStonesExcept
  , openSurprises
  , triggerAdjacentBottlesExcept
  , triggerAdjacentHatsExcept
  )
import Match3.Types (Board, Pos)

-- | 邻消削一层；返回（新盘面, 最后一层被削掉的位置）。
chipAdjacentStones, chipAdjacentChests, chipAdjacentHoney, chipAdjacentCakes, chipAdjacentSafes
  :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentStones b gems = chipAdjacentStonesExcept b gems []
chipAdjacentChests b gems = chipAdjacentChestsExcept b gems []
chipAdjacentHoney b gems = chipAdjacentHoneyExcept b gems []
chipAdjacentCakes b gems = chipAdjacentCakesExcept b gems []
chipAdjacentSafes b gems = chipAdjacentSafesExcept b gems []

-- | 同色邻消打爆的气球位置（盘面不变，由清除管线移走）。
chipAdjacentBalloons :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentBalloons b gems = chipAdjacentBalloonsExcept b gems []

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
