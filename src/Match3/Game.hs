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
  , useHammer
  , useFreeSwap
  , loseHint
  ) where

import Data.Maybe (fromMaybe)
import Match3.Board
  ( findHint
  , hasAnyMatch
  , hasValidMove
  , inBounds
  , adjacent
  , randomPlayableBoard
  , runCascadeScoredWithUfos
  , runCascadeScoredFromSeedsWithUfos
  , resolveCountdowns
  , shufflePlayable
  , swapCells
  , setCell
  , getCell
  )
import Match3.Ufo (Ufo(..), mkUfo)
import Match3.Obstacles (swapBlockedByStone)
import Match3.Conveyor (Belt, shiftBelts)
import Match3.Grass (spreadVines, spreadChoco)
import Match3.Countdown (spawnCountdown)
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
  , gsChestsCleared :: Int          -- fully opened treasure chests (宝箱)
  , gsGen           :: StdGen
  , gsOver          :: Maybe Outcome
  , gsLevel         :: Int
  , gsHistory       :: [GameState]
  , gsHint          :: Maybe (Pos, Pos)
  , gsCombo         :: Int   -- last move max cascade wave (0 if none)
  , gsShuffled      :: Bool  -- True if last ensurePlayable reshuffled
  , gsBelts         :: [Belt] -- conveyor paths (开心消消乐传送带)
  , gsHammers       :: Int    -- hammer booster charges
  , gsFreeSwaps     :: Int    -- free-swap booster charges (any two cells)
  , gsUfos          :: [Ufo]  -- flying saucers (飞碟)
  , gsUfoCollected  :: Int    -- gems absorbed by UFOs
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
      && gsChestsCleared a == gsChestsCleared b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b
      && gsBelts a == gsBelts b
      && gsHammers a == gsHammers b
      && gsFreeSwaps a == gsFreeSwaps b
      && gsUfos a == gsUfos b
      && gsUfoCollected a == gsUfoCollected b

snapshot :: GameState -> GameState
snapshot gs = gs { gsHistory = [], gsHint = Nothing, gsShuffled = False }

newGame :: GameConfig -> Int -> GameState
newGame = newGameAtLevel 0

-- | Conveyor paths for mixed campaign levels.
levelBelts :: Int -> [Belt]
levelBelts 7 = [[(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, 5), (2, 4), (2, 3), (2, 2), (2, 1)]]
levelBelts 10 = [[(3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7)]]
levelBelts 15 =
  [ [(0, 2), (0, 3), (0, 4), (0, 5), (1, 5), (1, 4), (1, 3), (1, 2)]
  , [(6, 1), (6, 2), (6, 3), (6, 4), (6, 5)]
  ]
levelBelts 13 = [[(4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7)]]
levelBelts _ = []

-- | Place stones / grass / vines / countdown décor (preserves gem color for overlays).
decorateLevel :: Int -> Board -> Board
decorateLevel 4 b = overlayAt b Choco [(2, 3), (3, 2), (3, 5), (5, 4)]
decorateLevel 5 b =
  -- Ice seals on a few gems (开心消消乐冰层入门)
  foldl
    (\board p ->
        case getCell board p of
          Gem col kind _ ov -> setCell board p (Gem col kind 1 ov)
          _ -> board
    )
    b
    [(2, 2), (2, 5), (5, 3), (6, 6)]
decorateLevel 7 b =
  foldl (\board p -> setCell board p mkStone)
        b
        [(4, 1), (4, 3), (4, 5), (5, 2), (5, 4), (6, 1), (6, 3), (6, 5)]
decorateLevel 8 b = overlayAt b Grass [(2, 2), (2, 5), (3, 3), (3, 6), (5, 1), (5, 4), (6, 3), (6, 6)]
decorateLevel 9 b = overlayAt b Vine [(2, 2), (2, 4), (4, 3), (5, 5)]
decorateLevel 10 b = overlayAt b Grass [(5, 1), (5, 3), (5, 5), (6, 2), (6, 4)]
decorateLevel 11 b =
  foldl
    (\board p ->
        case getCell board p of
          Gem col _ _ _ -> spawnCountdown board p col 4
          Countdown col _ -> spawnCountdown board p col 4
          _ -> board
    )
    b
    [(2, 2), (2, 5), (5, 3), (6, 6)]
decorateLevel 14 b =
  let b1 = overlayAt b Grass [(2, 2), (3, 5), (5, 3)]
  in overlayAt b1 Choco [(1, 1), (1, 6), (6, 2)]
decorateLevel 15 b =
  let b1 =
        foldl (\board p -> setCell board p mkStone)
              b
              [(4, 0), (4, 7), (5, 1), (5, 6)]
      b2 = overlayAt b1 Grass [(2, 1), (2, 6)]
      b3 = overlayAt b2 Vine [(6, 3)]
      b3' = overlayAt b3 Choco [(1, 3), (7, 4)]
      b4 = setCell (setCell b3' (7, 1) mkChest) (7, 6) (mkChestLayers 2)
  in foldl
       (\board p ->
           case getCell board p of
             Gem col _ _ _ -> spawnCountdown board p col 4
             _ -> board
       )
       b4
       [(3, 3)]
decorateLevel 16 b =
  foldl (\board (p, layers) -> setCell board p (mkChestLayers layers))
        b
        [ ((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1) ]
decorateLevel _ b = b

-- | UFO placements for campaign levels.
levelUfos :: Int -> [Ufo]
levelUfos 12 = [mkUfo (2, 3) C1]
levelUfos 13 = [mkUfo (1, 2) C1, mkUfo (1, 5) C3]
levelUfos 15 = [mkUfo (0, 4) C2]
levelUfos _ = []

-- | Stamp Grass/Vine/Choco onto existing gems (keep color/kind/ice).
overlayAt :: Board -> CellOverlay -> [Pos] -> Board
overlayAt b ov = foldl step b
  where
    step board p =
      case getCell board p of
        Gem col kind ice _ -> setCell board p (Gem col kind ice (Just ov))
        _ -> board

newGameAtLevel :: Int -> GameConfig -> Int -> GameState
newGameAtLevel li cfg seed =
  let g0 = mkStdGen seed
      (board0, g1) = randomPlayableBoard g0
      board = decorateLevel li board0
  in GameState
       { gsBoard = board
       , gsScore = 0
       , gsMoves = cfgMoves cfg
       , gsGoal = cfgGoal cfg
       , gsCollected = 0
       , gsColorBag = zip allColors (repeat 0)
       , gsStonesCleared = 0
       , gsChestsCleared = 0
       , gsGen = g1
       , gsOver = Nothing
       , gsLevel = li
       , gsHistory = []
       , gsHint = Nothing
       , gsCombo = 0
       , gsShuffled = False
       , gsBelts = levelBelts li
       , gsHammers = 2
       , gsFreeSwaps = 1
       , gsUfos =
           let placed = levelUfos li
           in if null placed
                then case cfgGoal cfg of
                       GoalUfo _ -> [mkUfo (1, 3) C1]
                       _ -> []
                else placed
       , gsUfoCollected = 0
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
    (gsUfoCollected gs)
    (gsChestsCleared gs)

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


-- | Snapshot blockers/overlays so reshuffle does not erase level décor.
data CellDecor = CellDecor
  { cdPos :: Pos
  , cdCell :: Cell
  } deriving (Eq, Show)

extractDecor :: Board -> [CellDecor]
extractDecor b =
  [ CellDecor (r, c) cell
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , let cell = getCell b (r, c)
  , keep cell
  ]
  where
    keep (Stone _) = True
    keep (Chest _) = True
    keep (Countdown _ _) = True
    keep (Gem _ _ ice ov) = ice > 0 || ov /= Nothing
    -- Normal bare gems are shuffled away

restoreDecor :: Board -> [CellDecor] -> Board
restoreDecor b = foldl (\board (CellDecor p cell) -> setCell board p cell) b

-- | If board has no valid move (and game not over), reshuffle to a playable board.
ensurePlayable :: GameState -> GameState
ensurePlayable gs
  | Just _ <- gsOver gs = gs { gsShuffled = False }
  | hasValidMove (gsBoard gs) = gs { gsShuffled = False }
  | otherwise =
      let decor = extractDecor (gsBoard gs)
          (board0, g') = shufflePlayable (gsGen gs)
          board = restoreDecor board0 decor
      in gs { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }

-- | Force reshuffle (e.g. player key S). Preserves stones / ice / overlays / bombs; keeps UFOs.
shuffleGame :: GameState -> GameState
shuffleGame gs =
  let decor = extractDecor (gsBoard gs)
      (board0, g') = shufflePlayable (gsGen gs)
      board = restoreDecor board0 decor
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
             let ufos0 = gsUfos gs
                 (board0', cleared0, gained0, combo0, tallies0, stones0, chests0, uAbs0, ufos1, g0') =
                   if rainbow
                     then
                       let seeds = rainbowClearSeeds swapped p1 p2
                       in runCascadeScoredFromSeedsWithUfos (Just p2) seeds ufos0 (gsGen gs) swapped
                     else if specialCombo
                       then
                         let seeds = comboClearSeeds swapped p1 p2
                         in runCascadeScoredFromSeedsWithUfos (Just p2) seeds ufos0 (gsGen gs) swapped
                       else runCascadeScoredWithUfos (Just p2) ufos0 (gsGen gs) swapped
                 -- Countdown bombs: tick after move; zeros explode 3×3
                 (boardCd, cleared1, gained1, combo1, tallies1, stones1, chests1, g1') =
                   resolveCountdowns g0' board0'
                 -- Conveyor belts: shift then cascade if new matches
                 boardBelt = shiftBelts boardCd (gsBelts gs)
                 (boardBeltCas, cleared2, gained2, combo2, tallies2, stones2, chests2, uAbs2, ufos2, g') =
                   if null (gsBelts gs)
                     then (boardCd, 0, 0, 0, zip allColors (repeat 0), 0, 0, 0, ufos1, g1')
                     else runCascadeScoredWithUfos Nothing ufos1 g1' boardBelt
                 -- Vine / chocolate spread at end of move (cleared overlays already stripped)
                 board1 = spreadChoco (spreadVines boardBeltCas)
                 gained = gained0 + gained1 + gained2
                 combo =
                   let c1 = max combo0 (if cleared1 > 0 then combo0 + combo1 else combo0)
                   in max c1 (if cleared2 > 0 then c1 + combo2 else c1)
                 tallies = mergeTallies (mergeTallies tallies0 tallies1) tallies2
                 stonesHit = stones0 + stones1 + stones2
                 chestsHit = chests0 + chests1 + chests2
                 uAbs = uAbs0 + uAbs2
                 ufoCollected' = gsUfoCollected gs + uAbs
                 _cleared = cleared0 + cleared1 + cleared2
                 collectDelta = case gsGoal gs of
                   GoalCollect col _ -> lookupColor tallies col
                   GoalCollectMulti _ -> 0
                   GoalClearStone _ -> 0
                   GoalChest _ -> 0
                   GoalScore _ -> 0
                   GoalUfo _ -> uAbs
                 -- For multi-collect, primary meter = sum of progress toward reqs
                 collected' = case gsGoal gs of
                   GoalCollect _ _ -> gsCollected gs + collectDelta
                   GoalCollectMulti reqs ->
                     let bag' = mergeTallies (gsColorBag gs) tallies
                     in sum [min n (lookupColor bag' c) | (c, n) <- reqs]
                   GoalClearStone _ -> gsStonesCleared gs + stonesHit
                   GoalChest _ -> gsChestsCleared gs + chestsHit
                   GoalScore _ -> gsCollected gs
                   GoalUfo _ -> ufoCollected'
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
                     , gsChestsCleared = gsChestsCleared gs + chestsHit
                     , gsGen = g'
                     , gsHistory = hist
                     , gsHint = Nothing
                     , gsCombo = combo
                     , gsShuffled = False
                     , gsUfos = ufos2
                     , gsUfoCollected = ufoCollected'
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

-- | Hammer: spend one charge to clear a single in-bounds cell, then cascade.
-- Does not consume a move.
useHammer :: Pos -> GameState -> (GameState, Outcome)
useHammer p gs
  | Just o <- gsOver gs = (gs, o)
  | gsHammers gs <= 0 = (gs, InvalidSwap)
  | not (inBounds p) = (gs, InvalidSwap)
  | otherwise =
      let seeds = [p]
          (boardH, _n, gained, combo, tallies, stonesHit, chestsHit, uAbs, ufos', g') =
            runCascadeScoredFromSeedsWithUfos Nothing seeds (gsUfos gs) (gsGen gs) (gsBoard gs)
          board1 = spreadChoco (spreadVines boardH)
          score' = gsScore gs + gained
          hist = take 20 (snapshot gs : gsHistory gs)
          ufoCollected' = gsUfoCollected gs + uAbs
          collectDelta = case gsGoal gs of
            GoalCollect col _ -> lookupColor tallies col
            GoalUfo _ -> uAbs
            _ -> 0
          collected' = case gsGoal gs of
            GoalCollect _ _ -> gsCollected gs + collectDelta
            GoalCollectMulti reqs ->
              let bag' = mergeTallies (gsColorBag gs) tallies
              in sum [min n (lookupColor bag' c) | (c, n) <- reqs]
            GoalClearStone _ -> gsStonesCleared gs + stonesHit
            GoalChest _ -> gsChestsCleared gs + chestsHit
            GoalScore _ -> gsCollected gs
            GoalUfo _ -> ufoCollected'
          gs' =
            gs
              { gsBoard = board1
              , gsScore = score'
              , gsCollected = collected'
              , gsColorBag = mergeTallies (gsColorBag gs) tallies
              , gsStonesCleared = gsStonesCleared gs + stonesHit
              , gsChestsCleared = gsChestsCleared gs + chestsHit
              , gsGen = g'
              , gsHistory = hist
              , gsHint = Nothing
              , gsCombo = combo
              , gsShuffled = False
              , gsHammers = gsHammers gs - 1
              , gsUfos = ufos'
              , gsUfoCollected = ufoCollected'
              }
          outcome = decideOutcome gs' gained
          gs'' = case outcome of
            Won s -> gs' { gsOver = Just (Won s) }
            Lost s -> gs' { gsOver = Just (Lost s) }
            LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
            _ -> gs'
          gs''' = case outcome of
            MoveApplied _ -> ensurePlayable gs''
            _ -> gs''
      in (gs''', outcome)

-- | Free-swap: spend one charge to swap any two in-bounds cells (need not be adjacent).
useFreeSwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
useFreeSwap p1 p2 gs
  | Just o <- gsOver gs = (gs, o)
  | gsFreeSwaps gs <= 0 = (gs, InvalidSwap)
  | not (inBounds p1 && inBounds p2) = (gs, InvalidSwap)
  | p1 == p2 = (gs, InvalidSwap)
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
             let (boardF, _c, gained, combo, tallies, stonesHit, chestsHit, uAbs, ufos', g') =
                   if rainbow
                     then runCascadeScoredFromSeedsWithUfos (Just p2) (rainbowClearSeeds swapped p1 p2) (gsUfos gs) (gsGen gs) swapped
                     else if specialCombo
                       then runCascadeScoredFromSeedsWithUfos (Just p2) (comboClearSeeds swapped p1 p2) (gsUfos gs) (gsGen gs) swapped
                       else runCascadeScoredWithUfos (Just p2) (gsUfos gs) (gsGen gs) swapped
                 board1 = spreadChoco (spreadVines boardF)
                 score' = gsScore gs + gained
                 hist = take 20 (snapshot gs : gsHistory gs)
                 ufoCollected' = gsUfoCollected gs + uAbs
                 collectDelta = case gsGoal gs of
                   GoalCollect col _ -> lookupColor tallies col
                   GoalUfo _ -> uAbs
                   _ -> 0
                 collected' = case gsGoal gs of
                   GoalCollect _ _ -> gsCollected gs + collectDelta
                   GoalCollectMulti reqs ->
                     let bag' = mergeTallies (gsColorBag gs) tallies
                     in sum [min n (lookupColor bag' c) | (c, n) <- reqs]
                   GoalClearStone _ -> gsStonesCleared gs + stonesHit
                   GoalChest _ -> gsChestsCleared gs + chestsHit
                   GoalScore _ -> gsCollected gs
                   GoalUfo _ -> ufoCollected'
                 gs' =
                   gs
                     { gsBoard = board1
                     , gsScore = score'
                     , gsCollected = collected'
                     , gsColorBag = mergeTallies (gsColorBag gs) tallies
                     , gsStonesCleared = gsStonesCleared gs + stonesHit
                     , gsChestsCleared = gsChestsCleared gs + chestsHit
                     , gsGen = g'
                     , gsHistory = hist
                     , gsHint = Nothing
                     , gsCombo = combo
                     , gsShuffled = False
                     , gsFreeSwaps = gsFreeSwaps gs - 1
                     , gsUfos = ufos'
                     , gsUfoCollected = ufoCollected'
                     }
                 outcome = decideOutcome gs' gained
                 gs'' = case outcome of
                   Won s -> gs' { gsOver = Just (Won s) }
                   Lost s -> gs' { gsOver = Just (Lost s) }
                   LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
                   _ -> gs'
                 gs''' = case outcome of
                   MoveApplied _ -> ensurePlayable gs''
                   _ -> gs''
             in (gs''', outcome)

-- | Short tip shown after a Lost outcome (失败提示).
loseHint :: LevelGoal -> String
loseHint (GoalScore t) = "再冲冲分数吧，目标 " ++ show t
loseHint (GoalCollect _ n) = "优先收集该色宝石，目标 " ++ show n ++ " 个"
loseHint (GoalCollectMulti reqs) =
  "兼顾多色收集：" ++ show (length reqs) ++ " 种配额"
loseHint (GoalClearStone n) = "用邻消或特效砸箱子，目标 " ++ show n ++ " 个"
loseHint (GoalChest n) = "邻消打开宝箱，目标 " ++ show n ++ " 个"
loseHint (GoalUfo n) = "让飞碟吸走同色宝石，目标 " ++ show n ++ " 个"
