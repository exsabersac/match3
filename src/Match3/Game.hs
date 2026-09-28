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
  ) where

import Match3.Board
  ( findHint
  , hasAnyMatch
  , inBounds
  , adjacent
  , randomStableBoard
  , runCascadeAt
  , scoreForCleared
  , swapCells
  )
import Match3.Types
import System.Random (StdGen, mkStdGen)

data GameState = GameState
  { gsBoard   :: Board
  , gsScore   :: Score
  , gsMoves   :: MovesLeft
  , gsTarget  :: TargetScore
  , gsGen     :: StdGen
  , gsOver    :: Maybe Outcome
  , gsLevel   :: Int
  , gsHistory :: [GameState]
  , gsHint    :: Maybe (Pos, Pos)
  } deriving (Show)

instance Eq GameState where
  a == b =
    gsBoard a == gsBoard b
      && gsScore a == gsScore b
      && gsMoves a == gsMoves b
      && gsTarget a == gsTarget b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b

snapshot :: GameState -> GameState
snapshot gs = gs { gsHistory = [], gsHint = Nothing }

newGame :: GameConfig -> Int -> GameState
newGame = newGameAtLevel 0

newGameAtLevel :: Int -> GameConfig -> Int -> GameState
newGameAtLevel li cfg seed =
  let g0 = mkStdGen seed
      (board, g1) = randomStableBoard g0
  in GameState
       { gsBoard = board
       , gsScore = 0
       , gsMoves = cfgMoves cfg
       , gsTarget = cfgTarget cfg
       , gsGen = g1
       , gsOver = Nothing
       , gsLevel = li
       , gsHistory = []
       , gsHint = Nothing
       }

restart :: GameConfig -> Int -> GameState
restart = newGame

restartLevel :: GameState -> Int -> GameState
restartLevel gs seed =
  let lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
  in newGameAtLevel (lvlIndex lvl) (levelConfig lvl) seed

checkOutcome :: GameState -> Outcome
checkOutcome gs
  | gsScore gs >= gsTarget gs = Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied 0

decideOutcome :: GameState -> Score -> Outcome
decideOutcome gs gained
  | gsScore gs >= gsTarget gs =
      let nextIdx = gsLevel gs + 1
      in if nextIdx < length allLevels
           then LevelClear (gsScore gs) nextIdx
           else Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied gained

trySwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
trySwap p1 p2 gs
  | Just o <- gsOver gs = (gs, o)
  | not (inBounds p1 && inBounds p2) = (gs, InvalidSwap)
  | not (adjacent p1 p2) = (gs, InvalidSwap)
  | otherwise =
      let swapped = swapCells (gsBoard gs) p1 p2
      in if not (hasAnyMatch swapped)
           then (gs { gsHint = Nothing }, NoMatch)
           else
             let (board1, cleared, g') = runCascadeAt (Just p2) (gsGen gs) swapped
                 gained = scoreForCleared cleared
                 score' = gsScore gs + gained
                 moves' = gsMoves gs - 1
                 hist = take 20 (snapshot gs : gsHistory gs)
                 gs' =
                   gs
                     { gsBoard = board1
                     , gsScore = score'
                     , gsMoves = moves'
                     , gsGen = g'
                     , gsHistory = hist
                     , gsHint = Nothing
                     }
                 outcome = decideOutcome gs' gained
                 gs'' = case outcome of
                   Won s -> gs' { gsOver = Just (Won s) }
                   Lost s -> gs' { gsOver = Just (Lost s) }
                   LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
                   _ -> gs'
             in (gs'', outcome)

runMove :: Pos -> Pos -> GameState -> (GameState, Outcome)
runMove = trySwap

undoMove :: GameState -> Maybe GameState
undoMove gs = case gsHistory gs of
  (prev : rest) -> Just prev { gsHistory = rest, gsHint = Nothing, gsOver = Nothing }
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
