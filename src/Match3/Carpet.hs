-- | 地毯 / 目标地砖：未铺格是关卡级元素 CarpetLevel 的状态（读数 gsCarpetOpen）；清除命中则覆盖并计数。
-- 饼干腾空与保险箱开启的覆盖种子由 Game.carpetVacateSeeds 补充；
-- 沉降中途底行收饼由 Board 把 drain 位并入清除列表。不拥有重力逻辑。
-- 各关的地毯布局第 6 刀起在关卡记录 lvlCarpets 里（Match3.Levels.Campaign.levelCarpets 按下标取）。
module Match3.Carpet
  ( coverCarpets
  ) where

import Data.List (nub)
import Match3.Types (Pos)

-- | Cover every open carpet cell that appears in @cleared@.
-- Returns (remaining open carpets, number newly covered).
-- Already-covered / non-target cells never increment the count.
coverCarpets :: [Pos] -> [Pos] -> ([Pos], Int)
coverCarpets open cleared =
  let hit = nub [p | p <- cleared, p `elem` open]
  in (filter (`notElem` hit) open, length hit)
