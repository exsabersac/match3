-- | Stone / chest / honey / cake / maker blockers + magic hat trigger.
-- Never match, block swaps; adjacent gem clears chip one layer; removed at 0
-- (开心消消乐箱子 / 宝箱 / 蜂蜜罐 / 蛋糕 / 果汁机). MagicHat: adjacent clear swaps neighbor colors.
-- Maker: same-color adjacent clear charges; at 0 produces Bomb in place.
module Match3.Obstacles
  ( swapBlockedByStone
  , orthoNeighbors
  , stonesAdjacentTo
  , chestsAdjacentTo
  , honeysAdjacentTo
  , cakesAdjacentTo
  , safesAdjacentTo
  , chipAdjacentStones
  , chipAdjacentChests
  , chipAdjacentHoney
  , chipAdjacentCakes
  , chipAdjacentSafes
  , chipAdjacentBalloons
  , balloonsAdjacentSameColor
  , hatsAdjacentTo
  , triggerAdjacentHats
  , makersAdjacentSameColor
  , chargeAdjacentMakers
  , withAdjacentStones
  ) where

import Data.List (nub, sort)
import Match3.Types
  ( Board
  , Cell
  , Color(..)
  , Pos
  , boardSize
  , isStone
  , isChest
  , isHoney
  , isBalloon
  , isCookie
  , isCake
  , isMagicHat
  , isMaker
  , isSafe
  , hasChain
  , hasFreeze
  , isSnail
  , mkSafeLayers
  , safeLayers
  , mkCookie
  , balloonColor
  , makerColor
  , makerCharges
  , mkStoneLayers
  , mkChestLayers
  , mkHoneyLayers
  , mkCakeLayers
  , mkMakerCharges
  , stoneLayers
  , chestLayers
  , honeyLayers
  , cakeLayers
  , CellContents(..)
  , GemKind(..)
  , cellColor
  , isGem
  )

at :: Board -> Pos -> Cell
at board (r, c) = (board !! r) !! c

setAt :: Board -> Pos -> Cell -> Board
setAt b (r, c) v =
  take r b ++ [take c row ++ [v] ++ drop (c + 1) row] ++ drop (r + 1) b
  where
    row = b !! r

-- | True if either swap endpoint is a blocker, chained, frozen, or snail.
swapBlockedByStone :: Board -> Pos -> Pos -> Bool
swapBlockedByStone b p1 p2 =
  let block c =
        isStone c || isChest c || isHoney c || isBalloon c || isCookie c
          || isCake c || isMagicHat c || isMaker c || isSnail c || isSafe c
          || hasChain c || hasFreeze c
  in block (at b p1) || block (at b p2)

-- | Up / down / left / right neighbors (may be out of bounds).
orthoNeighbors :: Pos -> [Pos]
orthoNeighbors (r, c) =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

inBoard :: Pos -> Bool
inBoard (r, c) =
  r >= 0 && r < boardSize && c >= 0 && c < boardSize

-- | Stone positions orthogonally adjacent to any of the given cleared positions.
stonesAdjacentTo :: Board -> [Pos] -> [Pos]
stonesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBoard p
    , isStone (at b p)
    ]

-- | Chest positions orthogonally adjacent to cleared positions.
chestsAdjacentTo :: Board -> [Pos] -> [Pos]
chestsAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBoard p
    , isChest (at b p)
    ]

-- | Chip one layer off each adjacent stone.
-- Returns (board with surviving stones decremented, positions whose last layer was chipped).
chipAdjacentStones :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentStones b clearedGems =
  foldl hitOne (b, []) (stonesAdjacentTo b clearedGems)
  where
    hitOne (board, dead) p =
      case at board p of
        cell | isStone cell ->
          let n = stoneLayers cell
          in if n <= 1
               then (board, nub (p : dead))
               else (setAt board p (mkStoneLayers (n - 1)), dead)
        _ -> (board, dead)

-- | Chip one layer off each adjacent treasure chest (宝箱).
chipAdjacentChests :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentChests b clearedGems =
  foldl hitOne (b, []) (chestsAdjacentTo b clearedGems)
  where
    hitOne (board, dead) p =
      case at board p of
        cell | isChest cell ->
          let n = chestLayers cell
          in if n <= 1
               then (board, nub (p : dead))
               else (setAt board p (mkChestLayers (n - 1)), dead)
        _ -> (board, dead)

-- | Honey jar positions orthogonally adjacent to cleared positions.
honeysAdjacentTo :: Board -> [Pos] -> [Pos]
honeysAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBoard p
    , isHoney (at b p)
    ]

-- | Chip one layer off each adjacent honey jar (蜂蜜罐).
chipAdjacentHoney :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentHoney b clearedGems =
  foldl hitOne (b, []) (honeysAdjacentTo b clearedGems)
  where
    hitOne (board, dead) p =
      case at board p of
        cell | isHoney cell ->
          let n = honeyLayers cell
          in if n <= 1
               then (board, nub (p : dead))
               else (setAt board p (mkHoneyLayers (n - 1)), dead)
        _ -> (board, dead)

-- | Balloon positions orthogonally adjacent to a same-color cleared gem.
balloonsAdjacentSameColor :: Board -> [Pos] -> [Pos]
balloonsAdjacentSameColor b cleared =
  nub
    [ p
    | cpos <- cleared
    , let clearedCell = at b cpos
    , isGem clearedCell
    , let col = cellColor clearedCell
    , p <- orthoNeighbors cpos
    , inBoard p
    , isBalloon (at b p)
    , balloonColor (at b p) == col
    ]

-- | Pop balloons adjacent to same-color clears (single hit; no layers).
chipAdjacentBalloons :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentBalloons b clearedGems =
  let dead = balloonsAdjacentSameColor b clearedGems
  in (b, dead)  -- board unchanged until clear pipeline removes them

-- | Cake positions orthogonally adjacent to cleared positions.
cakesAdjacentTo :: Board -> [Pos] -> [Pos]
cakesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBoard p
    , isCake (at b p)
    ]

-- | Chip one layer off each adjacent cake (蛋糕). Cleared at 0.
chipAdjacentCakes :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentCakes b clearedGems =
  foldl hitOne (b, []) (cakesAdjacentTo b clearedGems)
  where
    hitOne (board, dead) p =
      case at board p of
        cell | isCake cell ->
          let n = cakeLayers cell
          in if n <= 1
               then (board, nub (p : dead))
               else (setAt board p (mkCakeLayers (n - 1)), dead)
        _ -> (board, dead)

-- | Safe / vault positions orthogonally adjacent to cleared positions.
safesAdjacentTo :: Board -> [Pos] -> [Pos]
safesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBoard p
    , isSafe (at b p)
    ]

-- | Chip one layer off each adjacent safe (保险箱).
-- Last layer opens into a Cookie in place (collectible drop); cookie is NOT removed here.
-- Returns (board, positions that fully opened).
chipAdjacentSafes :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentSafes b clearedGems =
  foldl hitOne (b, []) (safesAdjacentTo b clearedGems)
  where
    hitOne (board, opened) p =
      case at board p of
        cell | isSafe cell ->
          let n = safeLayers cell
          in if n <= 1
               then (setAt board p mkCookie, nub (p : opened))
               else (setAt board p (mkSafeLayers (n - 1)), opened)
        _ -> (board, opened)

-- | Magic hat positions orthogonally adjacent to cleared gems.
hatsAdjacentTo :: Board -> [Pos] -> [Pos]
hatsAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBoard p
    , isMagicHat (at b p)
    ]

recolorCell :: Cell -> Color -> Cell
recolorCell (Gem _ kind ice ov) col = Gem col kind ice ov
recolorCell (Countdown _ n) col = Countdown col n
recolorCell (Flip _ back) col = Flip col back
recolorCell x _ = x

cycleColor :: Color -> Color
cycleColor c =
  let i = fromEnum c
  in toEnum ((i + 1) `mod` 5)

-- | Trigger magic hats adjacent to clears: swap colors of two ortho gem neighbors
-- (deterministic: sorted positions). If only one gem neighbor, cycle its color.
-- Hat itself stays. Neighbors in the cleared set are skipped.
triggerAdjacentHats :: Board -> [Pos] -> Board
triggerAdjacentHats b cleared =
  foldl triggerOne b (hatsAdjacentTo b cleared)
  where
    triggerOne board hatPos =
      let nbrs =
            [ p
            | p <- orthoNeighbors hatPos
            , inBoard p
            , p `notElem` cleared
            , let cell = at board p
            , isGem cell
            ]
          sorted = nub (sort nbrs)
      in case sorted of
           (p1 : p2 : _) ->
             let c1 = cellColor (at board p1)
                 c2 = cellColor (at board p2)
                 b1 = setAt board p1 (recolorCell (at board p1) c2)
             in setAt b1 p2 (recolorCell (at board p2) c1)
           [p1] ->
             let c = cellColor (at board p1)
             in setAt board p1 (recolorCell (at board p1) (cycleColor c))
           [] -> board

-- | Maker positions orthogonally adjacent to a same-color cleared gem.
makersAdjacentSameColor :: Board -> [Pos] -> [Pos]
makersAdjacentSameColor b cleared =
  nub
    [ p
    | cpos <- cleared
    , let clearedCell = at b cpos
    , isGem clearedCell
    , let col = cellColor clearedCell
    , p <- orthoNeighbors cpos
    , inBoard p
    , isMaker (at b p)
    , makerColor (at b p) == col
    ]

-- | Charge juice makers adjacent to same-color clears.
-- Charge 1 -> produce Bomb of maker color in place; n>1 -> decrement.
-- Returns board (makers never enter the clear-hole set).
chargeAdjacentMakers :: Board -> [Pos] -> Board
chargeAdjacentMakers b clearedGems =
  foldl chargeOne b (makersAdjacentSameColor b clearedGems)
  where
    chargeOne board p =
      case at board p of
        Maker col n
          | n <= 1 ->
              setAt board p (Gem col Bomb 0 Nothing)
          | otherwise ->
              setAt board p (mkMakerCharges col (n - 1))
        _ -> board

-- | Legacy helper: positions that should be removed (last-layer stones only).
-- Prefer chipAdjacentStones in clear pipeline.
withAdjacentStones :: Board -> [Pos] -> [Pos]
withAdjacentStones b seeds =
  let (_, dead) = chipAdjacentStones b seeds
  in nub (seeds ++ dead)
