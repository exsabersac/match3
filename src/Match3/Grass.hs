-- | Grass / vine overlays on gems (开心消消乐草·藤蔓).
-- Grass: cleared when the cell is part of a match / special clear.
-- Vine: spreads to adjacent bare gems after a successful move; cleared vines do not spread.
module Match3.Grass
  ( clearOverlaysOn
  , vinePositions
  , spreadVines
  , mkGrassGem
  , mkVineGem
  , hasGrass
  , hasVine
  , cellOverlay
  ) where

import Data.List (nub)
import Match3.Types

at :: Board -> Pos -> Cell
at b (r, c) = (b !! r) !! c

setAt :: Board -> Pos -> Cell -> Board
setAt b (r, c) v =
  take r b ++ [take c row ++ [v] ++ drop (c + 1) row] ++ drop (r + 1) b
  where
    row = b !! r

inBoard :: Pos -> Bool
inBoard (r, c) =
  r >= 0 && r < boardSize && c >= 0 && c < boardSize

ortho :: Pos -> [Pos]
ortho (r, c) =
  filter inBoard [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | Strip Grass/Vine on any seed cell that still holds a gem (match-above clears grass;
-- clear-hit vines are removed so they will not spread).
clearOverlaysOn :: Board -> [Pos] -> Board
clearOverlaysOn b seeds = foldl strip b (nub seeds)
  where
    strip board p =
      case at board p of
        Gem col kind ice (Just _) ->
          setAt board p (Gem col kind ice Nothing)
        _ -> board

vinePositions :: Board -> [Pos]
vinePositions b =
  [ (r, c)
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , hasVine (at b (r, c))
  ]

-- | Each remaining vine spreads onto every orthogonally adjacent bare gem
-- (no overlay, not stone). Newly placed vines do not chain-spread this turn.
spreadVines :: Board -> Board
spreadVines b =
  let sources = vinePositions b
      targets =
        nub
          [ q
          | p <- sources
          , q <- ortho p
          , case at b q of
              Gem _ _ _ Nothing -> True
              _ -> False
          ]
  in foldl plant b targets
  where
    plant board p =
      case at board p of
        Gem col kind ice Nothing ->
          setAt board p (Gem col kind ice (Just Vine))
        _ -> board
