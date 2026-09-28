-- | Special × special combos (开心消消乐 / Candy Crush style).
-- Line + Bomb: clear 3 full rows and 3 full columns centered on the bomb.
module Match3.Combos
  ( isLineBombCombo
  , isSpecialCombo
  , comboClearSeeds
  ) where

import Data.List (nub)
import Match3.Types

at :: Board -> Pos -> Cell
at b (r, c) = (b !! r) !! c

isLine :: GemKind -> Bool
isLine LineH = True
isLine LineV = True
isLine _ = False

isBomb :: GemKind -> Bool
isBomb Bomb = True
isBomb _ = False

-- | Adjacent Line (H/V) + Bomb pair (order-independent), pre-swap board.
isLineBombCombo :: Board -> Pos -> Pos -> Bool
isLineBombCombo b p1 p2 =
  case (at b p1, at b p2) of
    (Gem _ k1, Gem _ k2) ->
      (isLine k1 && isBomb k2) || (isBomb k1 && isLine k2)
    _ -> False

-- | Any special×special combo we currently support (extend later).
isSpecialCombo :: Board -> Pos -> Pos -> Bool
isSpecialCombo = isLineBombCombo

-- | On the *already swapped* board, seeds for a Line+Bomb combo.
-- Centers the 3×3 line cross on the Bomb's post-swap position.
comboClearSeeds :: Board -> Pos -> Pos -> [Pos]
comboClearSeeds b p1 p2 =
  case (at b p1, at b p2) of
    (Gem _ Bomb, _) -> lineBombCross b p1
    (_, Gem _ Bomb) -> lineBombCross b p2
    _ -> nub (lineBombCross b p1 ++ lineBombCross b p2)

-- | Three full rows and three full columns centered at (r,c).
lineBombCross :: Board -> Pos -> [Pos]
lineBombCross _ (r, c) =
  nub $
    [ (rr, cc)
    | rr <- [r - 1 .. r + 1]
    , rr >= 0
    , rr < boardSize
    , cc <- [0 .. boardSize - 1]
    ]
      ++ [ (rr, cc)
         | cc <- [c - 1 .. c + 1]
         , cc >= 0
         , cc < boardSize
         , rr <- [0 .. boardSize - 1]
         ]
