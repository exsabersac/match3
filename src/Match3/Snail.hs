-- | Snails (开心消消乐蜗牛): mobile blockers that crawl one step after each
-- successful player move. Push gems ahead; reverse at edges / solid blockers /
-- portal endpoints (immortal on a portal permanently kills the pair).
-- trySwap runs a follow-up cascade if the crawl assembles a match (no re-crawl).
module Match3.Snail
  ( stepSnailAt
  , stepSnailAtBlocked
  , stepSnails
  , stepSnailsAvoiding
  , stepSnailsAvoidingBlocked
  , snailPositions
  , mkSnail
  , isSnail
  , snailDir
  ) where

import Data.List (sort)
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

-- | Solids that stop a snail (reverse instead of push).
blocksSnail :: Cell -> Bool
blocksSnail c =
  isStone c
    || isChest c
    || isHoney c
    || isBalloon c
    || isCookie c
    || isCake c
    || isMagicHat c
    || isMaker c
    || isSafe c
    || isSurprise c
    || isBottle c
    || isSnail c

-- | Pushable: ordinary gems and countdown bombs.
pushable :: Cell -> Bool
pushable (Gem _ _ _ _) = True
pushable (Countdown _ _) = True
pushable (Flip _ _) = True
pushable _ = False

snailPositions :: Board -> [Pos]
snailPositions b =
  sort
    [ (r, c)
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , isSnail (at b (r, c))
    ]

-- | Crawl one snail at @pos@: push gem ahead, or reverse at wall/blocker.
stepSnailAt :: Board -> Pos -> Board
stepSnailAt = stepSnailAtBlocked []

-- | Like 'stepSnailAt' but treat @walls@ as impassable (e.g. portal endpoints).
-- Snails are immortal and not portal-transferable; occupying a portal forever
-- kills the pair (same class of bug as Bottle/Maker/Hat seeded on a portal).
stepSnailAtBlocked :: [Pos] -> Board -> Pos -> Board
stepSnailAtBlocked walls b pos = case at b pos of
  Snail dr dc ->
    let next = (fst pos + dr, snd pos + dc)
    in if not (inBoard next) || next `elem` walls || blocksSnail (at b next)
         then setAt b pos (Snail (-dr) (-dc))
         else if pushable (at b next)
           then
             -- Swap: snail moves to next, gem is pushed into snail's old cell
             let gem = at b next
                 b1 = setAt b next (Snail dr dc)
             in setAt b1 pos gem
           else setAt b pos (Snail (-dr) (-dc))
  _ -> b

-- | Step every snail once (left-to-right, top-to-bottom snapshot order).
-- Newly moved snails are not stepped again this turn.
stepSnails :: Board -> Board
stepSnails = stepSnailsAvoiding []

-- | Like stepSnails but skip snails currently sitting on @avoid@ cells.
-- Used after conveyor shift so a snail on a belt is not also crawled
-- (belt already moved it once this turn — avoids double-step on belt/same-col).
stepSnailsAvoiding :: [Pos] -> Board -> Board
stepSnailsAvoiding avoid = stepSnailsAvoidingBlocked avoid []

-- | Skip snails on @avoid@ (belt double-step) and reverse at @walls@
-- (portal endpoints — immortal must not occupy a portal).
stepSnailsAvoidingBlocked :: [Pos] -> [Pos] -> Board -> Board
stepSnailsAvoidingBlocked avoid walls b0 =
  foldl stepOne b0 [p | p <- snailPositions b0, p `notElem` avoid]
  where
    stepOne board pos =
      -- Only step if a snail is still at the snapshot position
      if isSnail (at board pos) then stepSnailAtBlocked walls board pos else board
