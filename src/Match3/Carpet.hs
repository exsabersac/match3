-- | 地毯 / 目标地砖：未铺格在 gsCarpetOpen；清除命中则覆盖并计数。
-- 饼干腾空与保险箱开启的覆盖种子由 Game.carpetVacateSeeds 补充；
-- 沉降中途底行收饼由 Board 把 drain 位并入清除列表。不拥有重力逻辑。
module Match3.Carpet
  ( coverCarpets
  , levelCarpets
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

-- | Campaign carpet layouts (uncovered target cells) by 0-based level index.
levelCarpets :: Int -> [Pos]
levelCarpets 36 =
  -- 地毯: 2×4 center patch
  [(3, 2), (3, 3), (3, 4), (3, 5), (4, 2), (4, 3), (4, 4), (4, 5)]
levelCarpets 37 =
  -- 织毯: larger patch + corners (mix décor via decorateLevel)
  [ (2, 2), (2, 3), (2, 4), (2, 5)
  , (3, 2), (3, 5), (4, 2), (4, 5)
  , (5, 2), (5, 3), (5, 4), (5, 5)
  ]
levelCarpets 27 =
  -- 终章: a few carpet tiles mixed into the finale
  [(4, 2), (4, 5), (5, 3), (5, 4)]
levelCarpets _ = []
