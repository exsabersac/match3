-- | Special × special combos (开心消消乐 / Candy Crush style).
module Match3.Combos
  ( isLineBombCombo
  , isRainbowLineCombo
  , isSpecialCombo
  , comboClearSeeds
  ) where

import Data.List (nub)
import Match3.Rainbow (rainbowClearSeeds)
import Match3.Types

at :: Board -> Pos -> Cell
at b (r, c) = (b !! r) !! c

isLine :: GemKind -> Bool
isLine LineH = True
isLine LineV = True
isLine _ = False

isBombK :: GemKind -> Bool
isBombK Bomb = True
isBombK _ = False

isRainbowK :: GemKind -> Bool
isRainbowK Rainbow = True
isRainbowK _ = False

isLineBombCombo :: Board -> Pos -> Pos -> Bool
isLineBombCombo b p1 p2 =
  case (at b p1, at b p2) of
    (Gem _ k1 _, Gem _ k2 _) ->
      (isLine k1 && isBombK k2) || (isBombK k1 && isLine k2)
    _ -> False

-- | Rainbow + Line: clear all of line's color (rainbow effect using line color).
isRainbowLineCombo :: Board -> Pos -> Pos -> Bool
isRainbowLineCombo b p1 p2 =
  case (at b p1, at b p2) of
    (Gem _ k1 _, Gem _ k2 _) ->
      (isRainbowK k1 && isLine k2) || (isLine k1 && isRainbowK k2)
    _ -> False

isSpecialCombo :: Board -> Pos -> Pos -> Bool
isSpecialCombo b p1 p2 =
  isLineBombCombo b p1 p2 || isRainbowLineCombo b p1 p2

comboClearSeeds :: Board -> Pos -> Pos -> [Pos]
comboClearSeeds b p1 p2
  | isLineBombCombo b p1 p2 || bombAfter = lineBombSeeds
  | isRainbowLineCombo b p1 p2 = rainbowClearSeeds b p1 p2
  | otherwise = []
  where
    -- After swap detection uses pre-swap board in is*; seeds use post-swap board `b`.
    bombAfter =
      case (at b p1, at b p2) of
        (Gem _ Bomb _, Gem _ k _) | isLine k -> True
        (Gem _ k _, Gem _ Bomb _) | isLine k -> True
        _ -> False
    lineBombSeeds =
      case (at b p1, at b p2) of
        (Gem _ Bomb _, _) -> lineBombCross b p1
        (_, Gem _ Bomb _) -> lineBombCross b p2
        _ -> nub (lineBombCross b p1 ++ lineBombCross b p2)

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
