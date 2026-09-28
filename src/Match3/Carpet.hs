-- | Carpet / floor tiles (地毯 / 目标地砖).
-- Uncovered carpet targets sit under gems; when a gem on that cell is fully
-- cleared (match / special / booster), the tile becomes covered. Cookie vacate
-- (gravity / bottom drain) and Safe→Cookie open also cover (via Game
-- carpetVacateSeeds) — otherwise immune Cookie / Safe soft-lock GoalCarpet.
-- GoalCarpet counts newly covered tiles. Re-clearing an already-covered cell is a no-op.
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
