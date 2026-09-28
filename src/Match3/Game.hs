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
  , resolveCountdowns
  , shufflePlayable
  , swapCells
  )
import Match3.Obstacles (swapBlockedByStone)
import Match3.Conveyor (Belt, shiftBelts)
import Match3.Combos (isSpecialCombo, comboClearSeeds)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types
import System.Random (StdGen, mkStdGen)

data GameState = GameState
  { gsBoard         :: Board
  , gsScore         :: Score
  , gsMoves         :: MovesLeft
  , gsGoal          :: LevelGoal
  , gsCollected     :: Int          -- primary collect-color cleared (GoalCollect)
  , gsColorBag      :: [(Color, Int)] -- cumulative clears per color
  , gsStonesCleared :: Int          -- fully destroyed stones
  , gsGen           :: StdGen
  , gsOver          :: Maybe Outcome
  , gsLevel         :: Int
  , gsHistory       :: [GameState]
  , gsHint          :: Maybe (Pos, Pos)
  , gsCombo         :: Int   -- last move max cascade wave (0 if none)
  , gsShuffled      :: Bool  -- True if last ensurePlayable reshuffled
  , gsBelts         :: [Belt] -- conveyor paths (开心消消乐传送带)
  } deriving (Show)

instance Eq GameState where
  a == b =
    gsBoard a == gsBoard b
      && gsScore a == gsScore b
      && gsMoves a == gsMoves b
      && gsGoal a == gsGoal b
      && gsCollected a == gsCollected b
      && gsColorBag a == gsColorBag b
      && gsStonesCleared a == gsStonesCleared b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b
      && gsBelts a == gsBelts b

snapshot :: GameState -> GameState
snapshot gs = gs { gsHistory = [], gsHint = Nothing, gsShuffled = False }

newGame :: GameConfig -> Int -> GameState
newGame = newGameAtLevel 0

-- | Demo conveyor on 碎石 (level 7): top-row cycle.
levelBelts :: Int -> [Belt]
levelBelts 7 = [[(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, 5), (2, 4), (2, 3), (2, 2), (2, 1)]]
levelBelts _ = []

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
       , gsColorBag = zip allColors (repeat 0)
       , gsStonesCleared = 0
       , gsGen = g1
       , gsOver = Nothing
       , gsLevel = li
       , gsHistory = []
       , gsHint = Nothing
       , gsCombo = 0
       , gsShuffled = False
       , gsBelts = levelBelts li
       }

restart :: GameConfig -> Int -> GameState
restart = newGame

restartLevel :: GameState -> Int -> GameState
restartLevel gs seed =
  let lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
  in newGameAtLevel (lvlIndex lvl) (levelConfig lvl) seed

checkOutcome :: GameState -> Outcome
checkOutcome gs
  | goalSatisfied gs = Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied 0

goalSatisfied :: GameState -> Bool
goalSatisfied gs =
  goalMetEx
    (gsGoal gs)
    (gsScore gs)
    (gsCollected gs)
    (gsColorBag gs)
    (gsStonesCleared gs)

decideOutcome :: GameState -> Score -> Outcome
decideOutcome gs gained
  | goalSatisfied gs =
      let nextIdx = gsLevel gs + 1
      in if nextIdx < length allLevels
           then LevelClear (gsScore gs) nextIdx
           else Won (gsScore gs)
  | gsMoves gs <= 0 = Lost (gsScore gs)
  | otherwise = MoveApplied gained

lookupColor :: [(Color, Int)] -> Color -> Int
lookupColor tallies col = fromMaybe 0 (lookup col tallies)

mergeTallies :: [(Color, Int)] -> [(Color, Int)] -> [(Color, Int)]
mergeTallies a b =
  [(col, lookupColor a col + lookupColor b col) | col <- allColors]

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
          specialCombo = isSpecialCombo board0 p1 p2
      in if not rainbow && not specialCombo && not (hasAnyMatch swapped)
           then (gs { gsHint = Nothing, gsShuffled = False }, NoMatch)
           else
             let (board0', cleared0, gained0, combo0, tallies0, stones0, g0') =
                   if rainbow
                     then
                       let seeds = rainbowClearSeeds swapped p1 p2
                       in runCascadeScoredFromSeeds (Just p2) seeds (gsGen gs) swapped
                     else if specialCombo
                       then
                         let seeds = comboClearSeeds swapped p1 p2
                         in runCascadeScoredFromSeeds (Just p2) seeds (gsGen gs) swapped
                       else runCascadeScored (Just p2) (gsGen gs) swapped
                 -- Countdown bombs: tick after move; zeros explode 3×3
                 (boardCd, cleared1, gained1, combo1, tallies1, stones1, g1') =
                   resolveCountdowns g0' board0'
                 -- Conveyor belts: shift then cascade if new matches
                 boardBelt = shiftBelts boardCd (gsBelts gs)
                 (board1, cleared2, gained2, combo2, tallies2, stones2, g') =
                   if null (gsBelts gs)
                     then (boardCd, 0, 0, 0, zip allColors (repeat 0), 0, g1')
                     else runCascadeScored Nothing g1' boardBelt
                 gained = gained0 + gained1 + gained2
                 combo =
                   let c1 = max combo0 (if cleared1 > 0 then combo0 + combo1 else combo0)
                   in max c1 (if cleared2 > 0 then c1 + combo2 else c1)
                 tallies = mergeTallies (mergeTallies tallies0 tallies1) tallies2
                 stonesHit = stones0 + stones1 + stones2
                 _cleared = cleared0 + cleared1 + cleared2
                 collectDelta = case gsGoal gs of
                   GoalCollect col _ -> lookupColor tallies col
                   GoalCollectMulti _ -> 0
                   GoalClearStone _ -> 0
                   GoalScore _ -> 0
                 -- For multi-collect, primary meter = sum of progress toward reqs
                 collected' = case gsGoal gs of
                   GoalCollect _ _ -> gsCollected gs + collectDelta
                   GoalCollectMulti reqs ->
                     let bag' = mergeTallies (gsColorBag gs) tallies
                     in sum [min n (lookupColor bag' c) | (c, n) <- reqs]
                   GoalClearStone _ -> gsStonesCleared gs + stonesHit
                   GoalScore _ -> gsCollected gs
                 score' = gsScore gs + gained
                 moves' = gsMoves gs - 1
                 hist = take 20 (snapshot gs : gsHistory gs)
                 gs' =
                   gs
                     { gsBoard = board1
                     , gsScore = score'
                     , gsMoves = moves'
                     , gsCollected = collected'
                     , gsColorBag = mergeTallies (gsColorBag gs) tallies
                     , gsStonesCleared = gsStonesCleared gs + stonesHit
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
