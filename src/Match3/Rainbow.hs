-- | 彩虹特殊块：五消生成；与搭档色（含 Countdown / Flip 正面）交换清该色。
-- 软锁彩虹不激活。双彩虹清全盘可匹配宝石。合成几何见 Combos。
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

-- | Adjacent swap where exactly one endpoint is a Rainbow and the other is a
-- colored partner (Normal/special gem, Countdown, or Flip front color).
-- Flip matches Countdown as a colored activator — without this, Rainbow×Flip
-- rolled back as NoMatch whenever the swap formed no classic 3-match.
-- Soft-locked Rainbow (ice>1 / Chain / Curtain) does not activate — same
-- discipline as expandSpecials for Line/Bomb (no fire-and-survive).
isRainbowSwap :: Board -> Pos -> Pos -> Bool
isRainbowSwap b p1 p2 =
  case (c1, c2) of
    (Gem _ Rainbow _ _, Gem _ k _ _) | k /= Rainbow -> specialActivates c1
    (Gem _ k _ _, Gem _ Rainbow _ _) | k /= Rainbow -> specialActivates c2
    (Gem _ Rainbow _ _, Gem _ Rainbow _ _) ->
      specialActivates c1 && specialActivates c2 -- double rainbow
    (Gem _ Rainbow _ _, Countdown _ _) -> specialActivates c1
    (Countdown _ _, Gem _ Rainbow _ _) -> specialActivates c2
    (Gem _ Rainbow _ _, Flip _ _) -> specialActivates c1
    (Flip _ _, Gem _ Rainbow _ _) -> specialActivates c2
    _ -> False
  where
    at board (r, c) = (board !! r) !! c
    c1 = at b p1
    c2 = at b p2

-- | On the *already swapped* board, positions to clear for a rainbow activation.
-- Includes the rainbow cell(s) and every gem of the partner color (or all gems if double).
-- Partner Flip uses front color (same as match / colorPositions).
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
      (Gem _ Rainbow _ _, Flip col _) -> colorPositions b col
      (Flip col _, Gem _ Rainbow _ _) -> colorPositions b col
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
      Maker _ _ -> False
      Snail _ _ -> False
      Safe _ -> False
      Surprise -> False
      Bottle _ -> False
      TimeSpirit -> False
      Flip col' _ -> col' == col
  ]
