-- | 蜗牛：移动障碍，成功玩家步后爬一格；推宝石/倒计时/双面；碰边/硬障/墙列表则掉头。
-- Game 传入皮带格与传送门端点为墙。爬后若成匹配由 trySwap 再跑一轮连锁（不重爬）。
module Match3.Snail
  ( stepSnailAt
  , stepSnailAtBlocked
  , stepSnailAtBy
  , pushable
  , stepSnails
  , stepSnailsAvoiding
  , stepSnailsAvoidingBlocked
  , snailPositions
  , mkSnail
  , isSnail
  , snailDir
  ) where

import Data.List (sort)
import Match3.Board.Grid (inBounds)
import Match3.Types

at :: Board -> Pos -> Cell
at = boardAt

setAt :: Board -> Pos -> Cell -> Board
setAt = boardSet


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
  sort (positionsWhere isSnail b)

-- | Crawl one snail at @pos@: push gem ahead, or reverse at wall/blocker.
stepSnailAt :: Board -> Pos -> Board
stepSnailAt = stepSnailAtBlocked []

-- | Like 'stepSnailAt' but treat @walls@ as impassable (e.g. portal endpoints).
-- Snails are immortal and not portal-transferable; occupying a portal forever
-- kills the pair (same class of bug as Bottle/Maker/Hat seeded on a portal).
stepSnailAtBlocked :: [Pos] -> Board -> Pos -> Board
stepSnailAtBlocked = stepSnailAtBy pushable

-- | 段 4：可推动谓词由调用方给出（元素框架里 = 注册表的 pushable；内置等于 pushable）。
stepSnailAtBy :: (Cell -> Bool) -> [Pos] -> Board -> Pos -> Board
stepSnailAtBy canPush walls b pos = case at b pos of
  Snail dr dc ->
    let next = (fst pos + dr, snd pos + dc)
    in if not (inBounds b next) || next `elem` walls || blocksSnail (at b next)
         then setAt b pos (Snail (-dr) (-dc))
         else if canPush (at b next)
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
