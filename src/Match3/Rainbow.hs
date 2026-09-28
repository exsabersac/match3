-- | Rainbow (color-bomb) special: 5-match spawn; swap with a color clears all of that color.
module Match3.Rainbow
  ( isRainbow
  , isRainbowSwap
  , rainbowClearSeeds
  ) where

import Data.List (nub)
import Match3.Types

isRainbow :: Cell -> Bool
isRainbow (Gem _ Rainbow _ _) = True
isRainbow _ = False

-- | Adjacent swap where exactly one endpoint is a Rainbow and the other is a colored gem.
isRainbowSwap :: Board -> Pos -> Pos -> Bool
isRainbowSwap b p1 p2 =
  case (at b p1, at b p2) of
    (Gem _ Rainbow _ _, Gem _ k _ _) | k /= Rainbow -> True
    (Gem _ k _ _, Gem _ Rainbow _ _) | k /= Rainbow -> True
    (Gem _ Rainbow _ _, Gem _ Rainbow _ _) -> True -- double rainbow: clear all gems
    (Gem _ Rainbow _ _, Countdown _ _) -> True
    (Countdown _ _, Gem _ Rainbow _ _) -> True
    _ -> False
  where
    at board (r, c) = (board !! r) !! c

-- | On the *already swapped* board, positions to clear for a rainbow activation.
-- Includes the rainbow cell(s) and every gem of the partner color (or all gems if double).
rainbowClearSeeds :: Board -> Pos -> Pos -> [Pos]
rainbowClearSeeds b p1 p2 =
  nub (rainbows ++ targets)
  where
    at board (r, c) = (board !! r) !! c
    c1 = at b p1
    c2 = at b p2
    rainbows =
      [ p
      | p <- [p1, p2]
      , isRainbow (at b p)
      ]
    targets = case (c1, c2) of
      (Gem _ Rainbow _ _, Gem _ Rainbow _ _) ->
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isGem (at b (r, c))
        ]
      (Gem _ Rainbow _ _, Gem col _ _ _) -> colorPositions b col
      (Gem col _ _ _, Gem _ Rainbow _ _) -> colorPositions b col
      (Gem _ Rainbow _ _, Countdown col _) -> colorPositions b col
      (Countdown col _, Gem _ Rainbow _ _) -> colorPositions b col
      _ -> []

colorPositions :: Board -> Color -> [Pos]
colorPositions b col =
  [ (r, c)
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , case (b !! r) !! c of
      Gem col' _ _ _ -> col' == col
      Countdown col' _ -> col' == col
      Stone _ -> False
      Chest _ -> False
      Honey _ -> False
      Balloon _ -> False
      Cookie -> False
      Cake _ -> False
      MagicHat -> False
  ]
