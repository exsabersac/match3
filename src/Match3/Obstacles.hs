-- | Stone / chest / honey blockers: layered crates, treasure chests, honey jars.
-- Never match, block swaps; adjacent gem clears chip one layer; removed at 0
-- (开心消消乐箱子 / 宝箱 / 蜂蜜罐).
module Match3.Obstacles
  ( swapBlockedByStone
  , orthoNeighbors
  , stonesAdjacentTo
  , chestsAdjacentTo
  , honeysAdjacentTo
  , chipAdjacentStones
  , chipAdjacentChests
  , chipAdjacentHoney
  , chipAdjacentBalloons
  , balloonsAdjacentSameColor
  , withAdjacentStones
  ) where

import Data.List (nub)
import Match3.Types
  ( Board
  , Cell
  , Pos
  , boardSize
  , isStone
  , isChest
  , isHoney
  , isBalloon
  , balloonColor
  , mkStoneLayers
  , mkChestLayers
  , mkHoneyLayers
  , stoneLayers
  , chestLayers
  , honeyLayers
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

-- | True if either swap endpoint is a stone, chest, or honey jar.
swapBlockedByStone :: Board -> Pos -> Pos -> Bool
swapBlockedByStone b p1 p2 =
  let block c = isStone c || isChest c || isHoney c || isBalloon c
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

-- | Legacy helper: positions that should be removed (last-layer stones only).
-- Prefer chipAdjacentStones in clear pipeline.
withAdjacentStones :: Board -> [Pos] -> [Pos]
withAdjacentStones b seeds =
  let (_, dead) = chipAdjacentStones b seeds
  in nub (seeds ++ dead)
