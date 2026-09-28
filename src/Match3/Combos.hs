-- | Special × special combos (开心消消乐 / Candy Crush style).
module Match3.Combos
  ( isLineBombCombo
  , isRainbowLineCombo
  , isBombBombCombo
  , isLineLineCombo
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

kindOf :: Cell -> Maybe GemKind
kindOf (Gem _ k _) = Just k
kindOf _ = Nothing

isLineBombCombo :: Board -> Pos -> Pos -> Bool
isLineBombCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just k1, Just k2) ->
      (isLine k1 && isBombK k2) || (isBombK k1 && isLine k2)
    _ -> False

isRainbowLineCombo :: Board -> Pos -> Pos -> Bool
isRainbowLineCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just k1, Just k2) ->
      (isRainbowK k1 && isLine k2) || (isLine k1 && isRainbowK k2)
    _ -> False

isBombBombCombo :: Board -> Pos -> Pos -> Bool
isBombBombCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just Bomb, Just Bomb) -> True
    _ -> False

isLineLineCombo :: Board -> Pos -> Pos -> Bool
isLineLineCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just k1, Just k2) -> isLine k1 && isLine k2
    _ -> False

isSpecialCombo :: Board -> Pos -> Pos -> Bool
isSpecialCombo b p1 p2 =
  isLineBombCombo b p1 p2
    || isRainbowLineCombo b p1 p2
    || isBombBombCombo b p1 p2
    || isLineLineCombo b p1 p2

comboClearSeeds :: Board -> Pos -> Pos -> [Pos]
comboClearSeeds b p1 p2
  | isBombBombCombo b p1 p2 || bothBombsAfter =
      nub (bigBomb b p1 ++ bigBomb b p2)
  | isLineLineCombo b p1 p2 || bothLinesAfter =
      nub (fullRowCol b p1 ++ fullRowCol b p2)
  | isLineBombCombo b p1 p2 || lineBombAfter =
      case (at b p1, at b p2) of
        (Gem _ Bomb _, _) -> lineBombCross b p1
        (_, Gem _ Bomb _) -> lineBombCross b p2
        _ -> nub (lineBombCross b p1 ++ lineBombCross b p2)
  | isRainbowLineCombo b p1 p2 = rainbowClearSeeds b p1 p2
  | otherwise = []
  where
    bothBombsAfter =
      case (kindOf (at b p1), kindOf (at b p2)) of
        (Just Bomb, Just Bomb) -> True
        _ -> False
    bothLinesAfter =
      case (kindOf (at b p1), kindOf (at b p2)) of
        (Just k1, Just k2) -> isLine k1 && isLine k2
        _ -> False
    lineBombAfter =
      case (kindOf (at b p1), kindOf (at b p2)) of
        (Just Bomb, Just k) | isLine k -> True
        (Just k, Just Bomb) | isLine k -> True
        _ -> False

-- | 5×5 blast centered at pos.
bigBomb :: Board -> Pos -> [Pos]
bigBomb _ (r, c) =
  [ (rr, cc)
  | rr <- [r - 2 .. r + 2]
  , cc <- [c - 2 .. c + 2]
  , rr >= 0
  , rr < boardSize
  , cc >= 0
  , cc < boardSize
  ]

fullRowCol :: Board -> Pos -> [Pos]
fullRowCol _ (r, c) =
  [(r, cc) | cc <- [0 .. boardSize - 1]]
    ++ [(rr, c) | rr <- [0 .. boardSize - 1]]

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
