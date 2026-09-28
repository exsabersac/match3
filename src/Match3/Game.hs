{-# LANGUAGE NamedFieldPuns #-}
module Match3.Game
  ( GameState(..)
  , newGame
  , newGameAtLevel
  , trySwap
  , runMove
  , restart
  , restartLevel
  , checkOutcome
  , undoMove
  , applyHint
  , nextLevel
  , ensurePlayable
  , shuffleGame
  ) where

import Data.Maybe (fromMaybe)
import Match3.Board
  ( findHint
  , hasAnyMatch
  , hasValidMove
  , inBounds
  , adjacent
  , randomPlayableBoard
  , runCascadeScored
  , runCascadeScoredFromSeeds
  , shufflePlayable
  , swapCells
  )
import Match3.Obstacles (swapBlockedByStone)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types
import System.Random (StdGen, mkStdGen)

data GameState = GameState
  { gsBoard     :: Board
  , gsScore     :: Score
  , gsMoves     :: MovesLeft
  , gsGoal      :: LevelGoal
  , gsCollected :: Int          -- gems of collect-color cleared (0 if score goal)
  , gsGen       :: StdGen
  , gsOver      :: Maybe Outcome
  , gsLevel     :: Int
  , gsHistory   :: [GameState]
  , gsHint      :: Maybe (Pos, Pos)
  , gsCombo     :: Int   -- last move max cascade wave (0 if none)
  , gsShuffled  :: Bool  -- True if last ensurePlayable reshuffled
  } deriving (Show)

instance Eq GameState where
  a == b =
    gsBoard a == gsBoard b
      && gsScore a == gsScore b
      && gsMoves a == gsMoves b
      && gsGoal a == gsGoal b
      && gsCollected a == gsCollected b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b

snapshot :: GameState -> GameState
snapshot gs = gs { gsHistory = [], gsHint = Nothing, gsShuffled = False }

newGame :: GameConfig -> Int -> GameState
newGame = newGameAtLevel 0

newGameAtLevel :: Int -> GameConfig -> Int -> GameState
newGameAtLevel li cfg seed =
  let g0 = mkStdGen seed
      (board, g1) = randomPlayableBoard g0
  in GameState
       { gsBoard = board
       , gsScore = 0
       , gsMoves = cfgMoves cfg
       , gsGoal = cfgGoal cfg
       , gsCollected = 0
       , gsGen = g1
       , gsOver = Nothing
       , gsLevel = li
       , gsHistory = []
       , gsHint = Nothing
       , gsCombo = 0
       , gsShuffled = False
       }

restart :: GameConfig -> Int -> GameState
restart = newGame

restartLevel :: GameState -> Int -> GameState
restartLevel gs seed =
  let lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
  in newGameAtLevel (lvlIndex lvl) (levelConfig lvl) seed

checkOutcome :: GameState -> Outcome
checkOutcome gs
  | goalMet (gsGoal gs) (gsScore gs) (gsCollected gs) = Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied 0

decideOutcome :: GameState -> Score -> Outcome
decideOutcome gs gained
  | goalMet (gsGoal gs) (gsScore gs) (gsCollected gs) =
      let nextIdx = gsLevel gs + 1
      in if nextIdx < length allLevels
           then LevelClear (gsScore gs) nextIdx
           else Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied gained

lookupColor :: [(Color, Int)] -> Color -> Int
lookupColor tallies col = fromMaybe 0 (lookup col tallies)

-- | If board has no valid move (and game not over), reshuffle to a playable board.
ensurePlayable :: GameState -> GameState
ensurePlayable gs
  | Just _ <- gsOver gs = gs { gsShuffled = False }
  | hasValidMove (gsBoard gs) = gs { gsShuffled = False }
  | otherwise =
      let (board, g') = shufflePlayable (gsGen gs)
      in gs { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }

-- | Force reshuffle (e.g. player key S).
shuffleGame :: GameState -> GameState
shuffleGame gs =
  let (board, g') = shufflePlayable (gsGen gs)
  in gs { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True, gsOver = Nothing }

trySwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
trySwap p1 p2 gs
  | Just o <- gsOver gs = (gs, o)
  | not (inBounds p1 && inBounds p2) = (gs, InvalidSwap)
  | not (adjacent p1 p2) = (gs, InvalidSwap)
  | swapBlockedByStone (gsBoard gs) p1 p2 =
      (gs { gsHint = Nothing, gsShuffled = False }, NoMatch)
  | otherwise =
      let board0 = gsBoard gs
          swapped = swapCells board0 p1 p2
          rainbow = isRainbowSwap board0 p1 p2
      in if not rainbow && not (hasAnyMatch swapped)
           then (gs { gsHint = Nothing, gsShuffled = False }, NoMatch)
           else
             let (board1, _cleared, gained, combo, tallies, g') =
                   if rainbow
                     then
                       let seeds = rainbowClearSeeds swapped p1 p2
                       in runCascadeScoredFromSeeds (Just p2) seeds (gsGen gs) swapped
                     else runCascadeScored (Just p2) (gsGen gs) swapped
                 collectDelta = case gsGoal gs of
                   GoalCollect col _ -> lookupColor tallies col
                   GoalScore _ -> 0
                 score' = gsScore gs + gained
                 collected' = gsCollected gs + collectDelta
                 moves' = gsMoves gs - 1
                 hist = take 20 (snapshot gs : gsHistory gs)
                 gs' =
                   gs
                     { gsBoard = board1
                     , gsScore = score'
                     , gsMoves = moves'
                     , gsCollected = collected'
                     , gsGen = g'
                     , gsHistory = hist
                     , gsHint = Nothing
                     , gsCombo = combo
                     , gsShuffled = False
                     }
                 outcome = decideOutcome gs' gained
                 gs'' = case outcome of
                   Won s -> gs' { gsOver = Just (Won s) }
                   Lost s -> gs' { gsOver = Just (Lost s) }
                   LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
                   _ -> gs'
                 -- Auto-shuffle if stuck after a non-terminal move
                 gs''' = case outcome of
                   MoveApplied _ -> ensurePlayable gs''
                   _ -> gs''
             in (gs''', outcome)

runMove :: Pos -> Pos -> GameState -> (GameState, Outcome)
runMove = trySwap

undoMove :: GameState -> Maybe GameState
undoMove gs = case gsHistory gs of
  (prev : rest) ->
    Just prev { gsHistory = rest, gsHint = Nothing, gsOver = Nothing, gsShuffled = False }
  [] -> Nothing

applyHint :: GameState -> (GameState, Maybe (Pos, Pos))
applyHint gs =
  let h = findHint (gsBoard gs)
  in (gs { gsHint = h }, h)

nextLevel :: GameState -> Int -> GameState
nextLevel gs seed =
  let idx = case gsOver gs of
        Just (LevelClear _ n) -> n
        _ -> min (gsLevel gs + 1) (length allLevels - 1)
      lvl = allLevels !! idx
  in newGameAtLevel idx (levelConfig lvl) seed
