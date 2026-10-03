-- | Haskell 特性第 9 项前的占格障碍代码逐字副本（Match3.Obstacles 的全部函数，以及 Match3.Types.Body 里
-- 五种叠层障碍的构造 / 读数 / 谓词、Match3.Types.Cell 的 cellColor / isGem），只给 Spec.RulesDedup 对照用：
-- 新写法（棱镜 + 一个 chipAdjacentLayeredExcept、改色遍历 cellColorT）与这里逐项相同。
-- 依赖只剩构造器：读数都是这里的副本，不经新写法。
module Spec.Support.LegacyObstacles
  ( orthoNeighbors
  , stonesAdjacentTo
  , chestsAdjacentTo
  , honeysAdjacentTo
  , cakesAdjacentTo
  , safesAdjacentTo
  , chipAdjacentStones
  , chipAdjacentStonesExcept
  , chipAdjacentChests
  , chipAdjacentChestsExcept
  , chipAdjacentHoney
  , chipAdjacentHoneyExcept
  , chipAdjacentCakes
  , chipAdjacentCakesExcept
  , chipAdjacentSafes
  , chipAdjacentSafesExcept
  , chipAdjacentBalloons
  , chipAdjacentBalloonsExcept
  , balloonsAdjacentSameColor
  , hatsAdjacentTo
  , triggerAdjacentHats
  , triggerAdjacentHatsExcept
  , triggerAdjacentHatsBy
  , makersAdjacentSameColor
  , chargeAdjacentMakers
  , chargeAdjacentMakersSit
  , surprisesAdjacentTo
  , openAdjacentSurprises
  , openSurprises
  , bottlesAdjacentTo
  , triggerAdjacentBottles
  , triggerAdjacentBottlesExcept
  , triggerAdjacentBottlesBy
  , spiritsAdjacentTo
  , chipAdjacentTimeSpirits
  , chipAdjacentTimeSpiritsExcept
  , withAdjacentStones
  , mkStoneLayers
  , stoneLayers
  , isStone
  , mkChestLayers
  , chestLayers
  , isChest
  , mkHoneyLayers
  , honeyLayers
  , isHoney
  , mkCakeLayers
  , cakeLayers
  , isCake
  , mkSafeLayers
  , safeLayers
  , isSafe
  , isMagicHat
  , isSurprise
  , isBottle
  , isTimeSpirit
  , balloonColor
  , makerColor
  , mkMakerCharges
  , mkCookie
  , cellColor
  , isGem
  , recolorCell
  ) where

import Data.List (nub, sort)
import Match3.Board.Grid (inBounds)
import Match3.Types (Board, boardAt, boardSet, Cell, Color(..), Pos, CellContents(..), GemKind(..), colorAt)

at :: Board -> Pos -> Cell
at = boardAt

setAt :: Board -> Pos -> Cell -> Board
setAt = boardSet

-- | Up / down / left / right neighbors (may be out of bounds).
orthoNeighbors :: Pos -> [Pos]
orthoNeighbors (r, c) =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]


-- | Stone positions orthogonally adjacent to any of the given cleared positions.
stonesAdjacentTo :: Board -> [Pos] -> [Pos]
stonesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , isStone (at b p)
    ]

-- | Chest positions orthogonally adjacent to cleared positions.
chestsAdjacentTo :: Board -> [Pos] -> [Pos]
chestsAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , isChest (at b p)
    ]

-- | Chip one layer off each adjacent stone.
-- Returns (board with surviving stones decremented, positions whose last layer was chipped).
chipAdjacentStones :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentStones b clearedGems = chipAdjacentStonesExcept b clearedGems []

-- | Like chipAdjacentStones but skips cells in 'except' (already direct-hit this wave).
chipAdjacentStonesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentStonesExcept b clearedGems except =
  foldl hitOne (b, []) [p | p <- stonesAdjacentTo b clearedGems, p `notElem` except]
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
chipAdjacentChests b clearedGems = chipAdjacentChestsExcept b clearedGems []

-- | Like chipAdjacentChests but skips cells in 'except' (already direct-hit this wave).
chipAdjacentChestsExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentChestsExcept b clearedGems except =
  foldl hitOne (b, []) [p | p <- chestsAdjacentTo b clearedGems, p `notElem` except]
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
    , inBounds b p
    , isHoney (at b p)
    ]

-- | Chip one layer off each adjacent honey jar (蜂蜜罐).
chipAdjacentHoney :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentHoney b clearedGems = chipAdjacentHoneyExcept b clearedGems []

-- | Like chipAdjacentHoney but skips cells in 'except' (already direct-hit this wave).
chipAdjacentHoneyExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentHoneyExcept b clearedGems except =
  foldl hitOne (b, []) [p | p <- honeysAdjacentTo b clearedGems, p `notElem` except]
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
    , Just col <- [cellColor clearedCell]
    , p <- orthoNeighbors cpos
    , inBounds b p
    , balloonColor (at b p) == Just col
    ]

-- | Pop balloons adjacent to same-color clears (single hit; no layers).
chipAdjacentBalloons :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentBalloons b clearedGems = chipAdjacentBalloonsExcept b clearedGems []

-- | Like chipAdjacentBalloons but skips cells in 'except' (already direct-hit this wave).
chipAdjacentBalloonsExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentBalloonsExcept b clearedGems except =
  let dead = [p | p <- balloonsAdjacentSameColor b clearedGems, p `notElem` except]
  in (b, dead)  -- board unchanged until clear pipeline removes them

-- | Cake positions orthogonally adjacent to cleared positions.
cakesAdjacentTo :: Board -> [Pos] -> [Pos]
cakesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , isCake (at b p)
    ]

-- | Chip one layer off each adjacent cake (蛋糕). Cleared at 0.
chipAdjacentCakes :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentCakes b clearedGems = chipAdjacentCakesExcept b clearedGems []

-- | Like chipAdjacentCakes but skips cells in 'except' (already direct-hit this wave).
chipAdjacentCakesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentCakesExcept b clearedGems except =
  foldl hitOne (b, []) [p | p <- cakesAdjacentTo b clearedGems, p `notElem` except]
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
    , inBounds b p
    , isSafe (at b p)
    ]

-- | Chip one layer off each adjacent safe (保险箱).
-- Last layer opens into a Cookie in place (collectible drop); cookie is NOT removed here.
-- Returns (board, positions that fully opened).
chipAdjacentSafes :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentSafes b clearedGems = chipAdjacentSafesExcept b clearedGems []

-- | Like chipAdjacentSafes but skips cells in 'except' (already direct-hit this wave).
chipAdjacentSafesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentSafesExcept b clearedGems except =
  foldl hitOne (b, []) [p | p <- safesAdjacentTo b clearedGems, p `notElem` except]
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
    , inBounds b p
    , isMagicHat (at b p)
    ]

recolorCell :: Cell -> Color -> Cell
recolorCell (Gem _ kind ice ov) col = Gem col kind ice ov
recolorCell (Countdown _ n) col = Countdown col n
recolorCell (Flip _ back) col = Flip col back
recolorCell x _ = x

cycleColor :: Color -> Color
cycleColor c = colorAt (fromEnum c + 1)

-- | Trigger magic hats adjacent to clears: swap colors of two ortho gem neighbors
-- (deterministic: sorted positions). If only one gem neighbor, cycle its color.
-- Hat itself stays. Neighbors in the cleared set are skipped.
triggerAdjacentHats :: Board -> [Pos] -> Board
triggerAdjacentHats b cleared = triggerAdjacentHatsExcept b cleared []

-- | Like triggerAdjacentHats, but also skip recoloring `protected` cells.
-- Surprise-opened specials (saved same wave) must sit unchanged — Hat must not
-- swap/cycle their color before the next move (parity with maker_bomb_survives_wave).
triggerAdjacentHatsExcept :: Board -> [Pos] -> [Pos] -> Board
triggerAdjacentHatsExcept = triggerAdjacentHatsBy isGem

-- | 段 4：可改色谓词由调用方给出（元素框架里 = 注册表的 recolorable；内置等于 isGem）。
triggerAdjacentHatsBy :: (Cell -> Bool) -> Board -> [Pos] -> [Pos] -> Board
triggerAdjacentHatsBy recolorable b cleared protected =
  foldl triggerOne b (hatsAdjacentTo b cleared)
  where
    skip = nub (cleared ++ protected)
    triggerOne board hatPos =
      let nbrs =
            [ p
            | p <- orthoNeighbors hatPos
            , inBounds b p
            , p `notElem` skip
            , let cell = at board p
            , recolorable cell
            ]
          sorted = nub (sort nbrs)
      in case sorted of
           (p1 : p2 : _)
             | Just c1 <- cellColor (at board p1)
             , Just c2 <- cellColor (at board p2) ->
                 let b1 = setAt board p1 (recolorCell (at board p1) c2)
                 in setAt b1 p2 (recolorCell (at board p2) c1)
           [p1]
             | Just c <- cellColor (at board p1) ->
                 setAt board p1 (recolorCell (at board p1) (cycleColor c))
           _ -> board

-- | Maker positions orthogonally adjacent to a same-color cleared gem.
makersAdjacentSameColor :: Board -> [Pos] -> [Pos]
makersAdjacentSameColor b cleared =
  nub
    [ p
    | cpos <- cleared
    , let clearedCell = at b cpos
    , isGem clearedCell
    , Just col <- [cellColor clearedCell]
    , p <- orthoNeighbors cpos
    , inBounds b p
    , makerColor (at b p) == Just col
    ]

-- | Charge juice makers adjacent to same-color clears.
-- Charge 1 -> produce Bomb of maker color in place; n>1 -> decrement.
-- Returns board (makers never enter the clear-hole set).
chargeAdjacentMakers :: Board -> [Pos] -> Board
chargeAdjacentMakers b clearedGems = fst (chargeAdjacentMakersSit b clearedGems)

-- | Like chargeAdjacentMakers, also returns positions converted to Bomb this wave.
-- Those Bombs must sit through same-wave Bottle dye (Surprise special parity;
-- Hat runs before Maker so only Bottle can rewrite a freshly produced Bomb).
chargeAdjacentMakersSit :: Board -> [Pos] -> (Board, [Pos])
chargeAdjacentMakersSit b clearedGems =
  foldl chargeOne (b, []) (makersAdjacentSameColor b clearedGems)
  where
    chargeOne (board, saved) p =
      case at board p of
        Maker col n
          | n <= 1 ->
              (setAt board p (Gem col Bomb 0 Nothing), nub (p : saved))
          | otherwise ->
              (setAt board p (mkMakerCharges col (n - 1)), saved)
        _ -> (board, saved)

-- | Surprise box positions orthogonally adjacent to cleared positions.
surprisesAdjacentTo :: Board -> [Pos] -> [Pos]
surprisesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , isSurprise (at b p)
    ]

-- | Deterministic surprise outcome from board position.
-- 0..2 → become LineH / LineV / Bomb; 3 → 3×3 explosion (box cleared).
surpriseOutcome :: Pos -> Int
surpriseOutcome (r, c) = (r * 8 + c) `mod` 4

surpriseSpecial :: Pos -> Cell
surpriseSpecial (r, c) =
  let col = colorAt (r + 3 * c)
      kind = case surpriseOutcome (r, c) of
        0 -> LineH
        1 -> LineV
        _ -> Bomb
  in Gem col kind 0 Nothing

-- | 3×3 blast centered at pos (same footprint as countdown / bomb).
surpriseBlast :: Board -> Pos -> [Pos]
surpriseBlast b (r, c) =
  [ (rr, cc)
  | rr <- [r - 1 .. r + 1]
  , cc <- [c - 1 .. c + 1]
  , inBounds b (rr, cc)
  ]

-- | Open surprises adjacent to clears *or* sitting on a clear seed
-- (hammer / cross / line / bomb direct hit). Special outcomes replace the box
-- in place; those positions are returned so callers can keep them out of clear
-- holes (otherwise a direct-hit special was spawned then immediately dug away).
-- Explosion outcomes contribute 3×3 clear seeds (box cleared via those seeds).
-- Returns (board, explosion seeds, special-placed positions).
openSurprises :: Board -> [Pos] -> (Board, [Pos], [Pos])
openSurprises b clearedGems =
  foldl openOne (b, [], []) targets
  where
    direct =
      [ p
      | p <- nub clearedGems
      , isSurprise (at b p)
      ]
    targets = nub (surprisesAdjacentTo b clearedGems ++ direct)
    openOne (board, explodes, saved) p =
      case at board p of
        Surprise
          | surpriseOutcome p == 3 ->
              (board, nub (surpriseBlast board p ++ explodes), saved)
          | otherwise ->
              (setAt board p (surpriseSpecial p), explodes, nub (p : saved))
        _ -> (board, explodes, saved)

-- | Adjacent-only wrapper (same targets as openSurprises, discards saved list).
openAdjacentSurprises :: Board -> [Pos] -> (Board, [Pos])
openAdjacentSurprises b clearedGems =
  let (b', expl, _) = openSurprises b clearedGems
  in (b', expl)

-- | Dye bottle positions orthogonally adjacent to cleared gems.
bottlesAdjacentTo :: Board -> [Pos] -> [Pos]
bottlesAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , isBottle (at b p)
    ]

-- | Trigger dye bottles: recolor every ortho gem neighbor to the bottle color.
-- Bottle itself stays. Neighbors in the cleared set are skipped.
triggerAdjacentBottles :: Board -> [Pos] -> Board
triggerAdjacentBottles b cleared = triggerAdjacentBottlesExcept b cleared []

-- | Like triggerAdjacentBottles, but also skip dyeing `protected` cells.
-- Surprise-opened specials and Maker-produced Bombs sit same-wave; Bottle
-- must not recolor them (parity with maker_bomb_survives_wave / Surprise sit).
triggerAdjacentBottlesExcept :: Board -> [Pos] -> [Pos] -> Board
triggerAdjacentBottlesExcept = triggerAdjacentBottlesBy isGem

-- | 段 4：可改色谓词由调用方给出（同 triggerAdjacentHatsBy）。
triggerAdjacentBottlesBy :: (Cell -> Bool) -> Board -> [Pos] -> [Pos] -> Board
triggerAdjacentBottlesBy recolorable b cleared protected =
  foldl dyeOne b (bottlesAdjacentTo b cleared)
  where
    skip = nub (cleared ++ protected)
    dyeOne board bottlePos =
      case at board bottlePos of
        Bottle col ->
          let nbrs =
                [ p
                | p <- orthoNeighbors bottlePos
                , inBounds b p
                , p `notElem` skip
                , let cell = at board p
                , recolorable cell
                ]
          in foldl (\bd p -> setAt bd p (recolorCell (at bd p) col)) board (nub nbrs)
        _ -> board


-- | Time spirit positions orthogonally adjacent to cleared gems.
spiritsAdjacentTo :: Board -> [Pos] -> [Pos]
spiritsAdjacentTo b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , isTimeSpirit (at b p)
    ]

-- | Remove adjacent time spirits (时间精灵). Dead positions cleared with the wave.
chipAdjacentTimeSpirits :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentTimeSpirits b clearedGems = chipAdjacentTimeSpiritsExcept b clearedGems []

-- | Like chipAdjacentTimeSpirits but skips cells in 'except' (already direct-hit this wave).
chipAdjacentTimeSpiritsExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentTimeSpiritsExcept b clearedGems except =
  foldl hitOne (b, []) [p | p <- spiritsAdjacentTo b clearedGems, p `notElem` except]
  where
    hitOne (board, dead) p =
      case at board p of
        TimeSpirit -> (board, nub (p : dead))
        _ -> (board, dead)

-- | Legacy helper: positions that should be removed (last-layer stones only).
-- Prefer chipAdjacentStones in clear pipeline.
withAdjacentStones :: Board -> [Pos] -> [Pos]
withAdjacentStones b seeds =
  let (_, dead) = chipAdjacentStones b seeds
  in nub (seeds ++ dead)

--------------------------------------------------------------------------------
-- Match3.Types.Body / Match3.Types.Cell 的旧读数

mkStoneLayers :: Int -> Cell
mkStoneLayers n = Stone (max 1 n)

stoneLayers :: Cell -> Int
stoneLayers (Stone n) = n
stoneLayers _ = 0

isStone :: Cell -> Bool
isStone (Stone _) = True
isStone _ = False

mkChestLayers :: Int -> Cell
mkChestLayers n = Chest (max 1 n)

chestLayers :: Cell -> Int
chestLayers (Chest n) = n
chestLayers _ = 0

isChest :: Cell -> Bool
isChest (Chest _) = True
isChest _ = False

mkHoneyLayers :: Int -> Cell
mkHoneyLayers n = Honey (max 1 n)

honeyLayers :: Cell -> Int
honeyLayers (Honey n) = n
honeyLayers _ = 0

isHoney :: Cell -> Bool
isHoney (Honey _) = True
isHoney _ = False

mkCakeLayers :: Int -> Cell
mkCakeLayers n = Cake (max 1 n)

cakeLayers :: Cell -> Int
cakeLayers (Cake n) = n
cakeLayers _ = 0

isCake :: Cell -> Bool
isCake (Cake _) = True
isCake _ = False

mkSafeLayers :: Int -> Cell
mkSafeLayers n = Safe (max 1 n)

safeLayers :: Cell -> Int
safeLayers (Safe n) = n
safeLayers _ = 0

isSafe :: Cell -> Bool
isSafe (Safe _) = True
isSafe _ = False

isMagicHat :: Cell -> Bool
isMagicHat MagicHat = True
isMagicHat _ = False

isSurprise :: Cell -> Bool
isSurprise Surprise = True
isSurprise _ = False

isBottle :: Cell -> Bool
isBottle (Bottle _) = True
isBottle _ = False

isTimeSpirit :: Cell -> Bool
isTimeSpirit TimeSpirit = True
isTimeSpirit _ = False

balloonColor :: Cell -> Maybe Color
balloonColor (Balloon c) = Just c
balloonColor _ = Nothing

makerColor :: Cell -> Maybe Color
makerColor (Maker c _) = Just c
makerColor _ = Nothing

mkMakerCharges :: Color -> Int -> Cell
mkMakerCharges c n = Maker c (max 1 n)

mkCookie :: Cell
mkCookie = Cookie

cellColor :: Cell -> Maybe Color
cellColor cell = case cell of
  Gem c _ _ _ -> Just c
  Countdown c _ -> Just c
  Flip f _ -> Just f
  _ -> Nothing

isGem :: Cell -> Bool
isGem (Gem _ _ _ _) = True
isGem (Countdown _ _) = True
isGem (Stone _) = False
isGem (Chest _) = False
isGem (Honey _) = False
isGem (Balloon _) = False
isGem Cookie = False
isGem (Cake _) = False
isGem MagicHat = False
isGem (Maker _ _) = False
isGem (Snail _ _) = False
isGem (Safe _) = False
isGem (Flip _ _) = True
isGem Surprise = False
isGem (Bottle _) = False
isGem TimeSpirit = False
isGem (Custom _ _) = False
