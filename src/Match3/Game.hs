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
  , useCrossClear
  , loseHint
  , unlockAfterClear
  , mapClickJump
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
import Match3.Grass (spreadVines, spreadChoco, spreadSteam)
import Match3.Carpet (coverCarpets, levelCarpets)
import Data.List (nub)
import Match3.Snail (stepSnailsAvoiding)
import Match3.Countdown (spawnCountdown)
import Match3.Combos (isSpecialCombo, comboClearSeeds)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Boosters (crossClearSeeds)
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
  , gsHoneyCleared  :: Int          -- fully smashed honey jars (蜂蜜罐)
  , gsBalloonsPopped :: Int         -- balloons popped (气球)
  , gsCookiesCollected :: Int       -- biscuits collected at bottom (饼干)
  , gsCakesCleared  :: Int          -- cakes fully cleared (蛋糕)
  , gsSafesOpened   :: Int          -- vaults / safes opened (保险箱)
  , gsGen           :: StdGen
  , gsOver          :: Maybe Outcome
  , gsLevel         :: Int
  , gsHistory       :: [GameState]
  , gsHint          :: Maybe (Pos, Pos)
  , gsCombo         :: Int   -- last move max cascade wave (0 if none)
  , gsShuffled      :: Bool  -- True if last ensurePlayable reshuffled
  , gsBelts         :: [Belt] -- conveyor paths (开心消消乐传送带)
  , gsPortals       :: [(Pos, Pos)] -- bidirectional portal pairs (传送门)
  , gsHammers       :: Int    -- hammer booster charges
  , gsFreeSwaps     :: Int    -- free-swap booster charges (any two cells)
  , gsCrossClears   :: Int    -- cross-clear booster charges
  , gsUfos          :: [Ufo]  -- flying saucers (飞碟)
  , gsUfoCollected  :: Int    -- gems absorbed by UFOs
  , gsCarpetOpen    :: [Pos]  -- uncovered carpet / floor tiles (地毯目标)
  , gsCarpetsCovered :: Int   -- carpet tiles covered this level
  , gsLastCleared   :: [Pos]  -- cells cleared last move (UI particles; not belt/snail noise)
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
      && gsHoneyCleared a == gsHoneyCleared b
      && gsBalloonsPopped a == gsBalloonsPopped b
      && gsCookiesCollected a == gsCookiesCollected b
      && gsCakesCleared a == gsCakesCleared b
      && gsSafesOpened a == gsSafesOpened b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b
      && gsBelts a == gsBelts b
      && gsPortals a == gsPortals b
      && gsHammers a == gsHammers b
      && gsFreeSwaps a == gsFreeSwaps b
      && gsCrossClears a == gsCrossClears b
      && gsUfos a == gsUfos b
      && gsUfoCollected a == gsUfoCollected b
      && gsCarpetOpen a == gsCarpetOpen b
      && gsCarpetsCovered a == gsCarpetsCovered b

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
levelBelts 27 = [[(1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7)]]
levelBelts _ = []

-- | Bidirectional portal pairs for campaign levels.
levelPortals :: Int -> [(Pos, Pos)]
levelPortals 26 = [((0, 1), (7, 6)), ((0, 6), (7, 1))]
levelPortals 27 = [((0, 3), (7, 4))]
levelPortals _ = []

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
decorateLevel 17 b =
  let b1 =
        foldl (\board (p, layers) -> setCell board p (mkChestLayers layers))
              b
              [ ((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1) ]
  in overlayAt b1 Choco [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]
decorateLevel 18 b =
  -- 蜂蜜: honey jars to smash
  foldl (\board (p, layers) -> setCell board p (mkHoneyLayers layers))
        b
        [ ((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1) ]
decorateLevel 19 b =
  let b1 =
        foldl (\board (p, layers) -> setCell board p (mkHoneyLayers layers))
              b
              [ ((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1) ]
  in overlayAt b1 Choco [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]
decorateLevel 20 b =
  -- 气球: colored balloons popped by same-color adjacent clears
  foldl (\board (p, col) -> setCell board p (mkBalloon col))
        b
        [ ((2, 2), C1), ((2, 5), C3), ((4, 1), C2), ((4, 3), C1), ((4, 5), C4), ((6, 2), C3), ((6, 5), C5) ]
decorateLevel 21 b =
  -- 饼干: cookies high on board; clear below so they fall to bottom
  foldl (\board p -> setCell board p mkCookie)
        b
        [(0, 1), (0, 3), (0, 5), (1, 2), (1, 4), (1, 6), (2, 3)]
decorateLevel 22 b =
  let b1 =
        foldl (\board p -> setCell board p mkCookie)
              b
              [(0, 2), (0, 5), (1, 1), (1, 4), (1, 6), (2, 3)]
      b2 = overlayAt b1 Choco [(3, 1), (3, 6)]
  in overlayAt b2 (Fog 1) [(4, 2), (4, 3), (4, 4), (5, 3), (6, 2), (6, 5)]
decorateLevel 23 b =
  -- 蛋糕: layered cakes to smash (distinct from Cookie)
  foldl (\board (p, layers) -> setCell board p (mkCakeLayers layers))
        b
        [ ((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1) ]
decorateLevel 24 b =
  -- 帽宴: cakes + magic hats mixed
  let b1 =
        foldl (\board (p, layers) -> setCell board p (mkCakeLayers layers))
              b
              [ ((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1) ]
      b2 =
        foldl (\board p -> setCell board p mkMagicHat)
              b1
              [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]
  in overlayAt b2 Choco [(6, 1), (6, 5)]
decorateLevel 25 b =
  -- 锁链: iron chains lock gems; peel by adjacent clear
  let b1 = overlayAt b (Chain 1) [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
      b2 = overlayAt b1 (Chain 2) [(4, 3), (4, 4)]
      b3 =
        foldl (\board p -> setCell board p mkStone)
              b2
              [(1, 1), (1, 6)]
  in overlayAt b3 Choco [(5, 1), (5, 6)]
decorateLevel 26 b =
  -- 果汁: juice makers + fog; portals applied via levelPortals
  let b1 =
        foldl (\board (p, col, n) -> setCell board p (mkMakerCharges col n))
              b
              [ ((2, 2), C1, 2), ((2, 5), C1, 3), ((5, 3), C1, 2), ((5, 4), C3, 2) ]
      b2 = overlayAt b1 (Fog 1) [(3, 1), (3, 6), (6, 2), (6, 5)]
  in overlayAt b2 Choco [(1, 3), (1, 4)]
decorateLevel 27 b =
  let b1 =
        foldl (\board p -> setCell board p mkStone)
              b
              [(0, 0), (0, 7), (7, 0), (7, 7)]
      b2 =
        foldl (\board p -> setCell board p mkChest)
              b1
              [(3, 1), (3, 6)]
      b2' =
        foldl (\board p -> setCell board p mkHoney)
              b2
              [(2, 3), (2, 4)]
      b2'' =
        foldl (\board (p, col) -> setCell board p (mkBalloon col))
              b2'
              [((5, 1), C1), ((5, 6), C3)]
      b2c =
        foldl (\board p -> setCell board p mkCookie)
              b2''
              [(0, 2), (0, 5)]
      b2k =
        foldl (\board p -> setCell board p (mkCakeLayers 1))
              b2c
              [(1, 3), (1, 4)]
      b2h =
        foldl (\board p -> setCell board p mkMagicHat)
              b2k
              [(7, 2), (7, 5)]
      b2m =
        foldl (\board p -> setCell board p (mkMakerCharges C2 2))
              b2h
              [(4, 0), (4, 7)]
      b3 = overlayAt b2m Choco [(2, 2), (2, 5)]
      b3f = overlayAt b3 (Fog 2) [(5, 2), (5, 5)]
      b3c = overlayAt b3f (Chain 1) [(6, 1), (6, 6)]
      b3z = overlayAt b3c (Freeze 1) [(3, 3), (3, 4)]
      b3u = overlayAt b3z (Curtain 1) [(5, 3), (5, 4)]
      b3s =
        foldl (\board p -> setCell board p (mkSafeLayers 1))
              b3u
              [(6, 4)]
      b3p = setCell b3s (7, 3) (mkFlip C1 C3)
      b3q = setCell b3p (5, 0) mkSurprise
      b3r = setCell b3q (0, 3) (mkBottle C1)
      b4 = overlayAt b3r Vine [(6, 3)]
      b5 =
        foldl (\board (p, dr, dc) -> setCell board p (mkSnail dr dc))
              b4
              [((0, 4), 0, 1)]
  in foldl
       (\board p ->
           case getCell board p of
             Gem col _ _ _ -> spawnCountdown board p col 5
             _ -> board
       )
       b5
       [(4, 4)]
decorateLevel 28 b =
  -- 蜗牛: crawling snails that push gems after each move
  foldl (\board (p, dr, dc) -> setCell board p (mkSnail dr dc))
        b
        [ ((1, 1), 0, 1), ((1, 6), 0, -1), ((4, 2), 1, 0), ((4, 5), 1, 0)
        , ((6, 3), 0, 1), ((2, 4), 0, -1)
        ]
decorateLevel 29 b =
  -- 冰冻: rocket freeze overlays (block swap, peel by adjacent; gems still match)
  let b1 = overlayAt b (Freeze 1) [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
      b2 = overlayAt b1 (Freeze 2) [(4, 3), (4, 4)]
  in overlayAt b2 Choco [(1, 1), (1, 6)]
decorateLevel 30 b =
  -- 窗帘: curtain columns (遮挡整列/区域，邻消揭开)
  let cols = [1, 6]
      rows = [1, 2, 3, 4, 5, 6]
      pos1 = [(r, c) | r <- rows, c <- cols]
      b1 = overlayAt b (Curtain 1) pos1
      b2 = overlayAt b1 (Curtain 2) [(2, 3), (2, 4), (5, 3), (5, 4)]
  in overlayAt b2 Choco [(0, 2), (0, 5)]
decorateLevel 31 b =
  -- 金库: safes open into cookies; dual-face flips mixed in
  let b1 =
        foldl (\board (p, layers) -> setCell board p (mkSafeLayers layers))
              b
              [ ((2, 2), 1), ((2, 5), 2), ((4, 1), 1), ((4, 3), 2), ((4, 5), 1), ((6, 2), 1), ((6, 5), 2) ]
      b2 =
        foldl (\board (p, f, bk) -> setCell board p (mkFlip f bk))
              b1
              [ ((1, 1), C1, C3), ((1, 6), C2, C4), ((3, 0), C3, C1)
              , ((3, 7), C4, C2), ((5, 3), C1, C2), ((5, 4), C2, C1)
              ]
  in overlayAt b2 Choco [(7, 1), (7, 6)]
decorateLevel 32 b =
  -- surprise boxes: open to special or 3x3 pop
  let b1 =
        foldl (\board p -> setCell board p mkSurprise)
              b
              [ (1, 1), (1, 6), (2, 3), (3, 1), (3, 6), (4, 0), (4, 4), (5, 2), (5, 5), (6, 3) ]
  in overlayAt b1 Choco [(7, 2), (7, 5)]
decorateLevel 33 b =
  -- dye bottles paint neighbors on adjacent clear
  let b1 =
        foldl (\board (p, col) -> setCell board p (mkBottle col))
              b
              [ ((2, 2), C3), ((2, 5), C3), ((4, 1), C1), ((4, 6), C3)
              , ((5, 3), C3), ((5, 4), C2), ((6, 2), C3), ((6, 5), C3)
              ]
  in overlayAt b1 (Fog 1) [(1, 3), (1, 4)]
decorateLevel 34 b =
  -- 时灵: time spirits award +2 moves when adjacent-cleared
  foldl (\board p -> setCell board p mkTimeSpirit)
        b
        [ (1, 1), (1, 6), (2, 3), (3, 2), (3, 5), (4, 4), (5, 1), (5, 6), (6, 3) ]
decorateLevel 35 b =
  -- 蒸汽: steam overlays block match, adjacent extinguish, then spread
  let b1 = overlayAt b Steam [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
      b2 = overlayAt b1 Steam [(1, 3), (1, 4), (4, 3), (4, 4)]
  in overlayAt b2 Choco [(7, 2), (7, 5)]
decorateLevel 36 b =
  -- 地毯: open floor tiles via levelCarpets; light choco garnish
  overlayAt b Choco [(1, 1), (1, 6), (6, 1), (6, 6)]
decorateLevel 37 b =
  -- 织毯: carpet + choco + fog mix
  let b1 = overlayAt b Choco [(1, 2), (1, 5), (6, 2), (6, 5)]
  in overlayAt b1 (Fog 1) [(0, 3), (0, 4), (7, 3), (7, 4)]
decorateLevel _ b = b

-- | UFO placements for campaign levels.
levelUfos :: Int -> [Ufo]
levelUfos 12 = [mkUfo (2, 3) C1]
levelUfos 13 = [mkUfo (1, 2) C1, mkUfo (1, 5) C3]
levelUfos 15 = [mkUfo (0, 4) C2]
levelUfos 27 = [mkUfo (2, 4) C1]
levelUfos _ = []

-- | Stamp Grass/Vine/Choco onto existing gems (keep color/kind/ice).
overlayAt :: Board -> CellOverlay -> [Pos] -> Board
overlayAt b ov = foldl step b
  where
    step board p =
      case getCell board p of
        Gem col kind ice _ -> setCell board p (Gem col kind ice (Just ov))
        _ -> board

-- | Daily (and any bare board) must still be completable: if the goal needs
-- board entities but level décor did not place enough, seed a minimal set.
ensureGoalDecor :: LevelGoal -> Board -> Board
ensureGoalDecor goal b =
  case goal of
    GoalClearStone n | countKind isStone b < n ->
      place n mkStone
        [(4, 1), (4, 3), (4, 5), (5, 2), (5, 4), (6, 1), (6, 3), (6, 5)]
    GoalHoney n | countKind isHoney b < n ->
      place n (mkHoneyLayers 1)
        [(2, 2), (2, 5), (4, 1), (4, 3), (4, 5), (6, 2), (6, 5)]
    GoalChest n | countKind isChest b < n ->
      place n (mkChestLayers 1)
        [(2, 2), (2, 5), (4, 1), (4, 3), (4, 5), (6, 2), (6, 5)]
    GoalCake n | countKind isCake b < n ->
      place n (mkCakeLayers 1)
        [(2, 2), (2, 5), (4, 1), (4, 3), (4, 5), (6, 2), (6, 5)]
    GoalSafe n | countKind isSafe b < n ->
      place n (mkSafeLayers 1)
        [(2, 2), (2, 5), (4, 1), (4, 3), (4, 5), (6, 2), (6, 5)]
    GoalBalloon n | countKind isBalloon b < n ->
      foldl
        (\board (pos, col) -> setCell board pos (mkBalloon col))
        b
        (take (max n 6)
           [ ((2, 2), C1), ((2, 5), C3), ((4, 1), C2), ((4, 3), C1)
           , ((4, 5), C4), ((6, 2), C3), ((6, 5), C5) ])
    _ -> b
  where
    countKind keep board =
      length
        [ ()
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , keep (getCell board (r, c))
        ]
    place n cell slots =
      foldl (\board pos -> setCell board pos cell) b (take (max n 6) slots)

newGameAtLevel :: Int -> GameConfig -> Int -> GameState
newGameAtLevel li cfg seed =
  let g0 = mkStdGen seed
      (board0, g1) = randomPlayableBoard g0
      board = ensureGoalDecor (cfgGoal cfg) (decorateLevel li board0)
      gs0 =
        GameState
          { gsBoard = board
          , gsScore = 0
          , gsMoves = cfgMoves cfg
          , gsGoal = cfgGoal cfg
          , gsCollected = 0
          , gsColorBag = zip allColors (repeat 0)
          , gsStonesCleared = 0
          , gsChestsCleared = 0
          , gsHoneyCleared = 0
          , gsBalloonsPopped = 0
          , gsCookiesCollected = 0
          , gsCakesCleared = 0
          , gsSafesOpened = 0
          , gsGen = g1
          , gsOver = Nothing
          , gsLevel = li
          , gsHistory = []
          , gsHint = Nothing
          , gsCombo = 0
          , gsShuffled = False
          , gsBelts = levelBelts li
          , gsPortals = levelPortals li
          , gsHammers = 2
          , gsFreeSwaps = 1
          , gsCrossClears = 1
          , gsUfos =
              let placed = levelUfos li
              in if null placed
                   then case cfgGoal cfg of
                          GoalUfo _ -> [mkUfo (1, 3) C1]
                          _ -> []
                   else placed
          , gsUfoCollected = 0
          , gsCarpetOpen = levelCarpets li
          , gsCarpetsCovered = 0
          , gsLastCleared = []
          }
  -- Décor can remove the only legal swap (e.g. dense 终章); auto-reshuffle gems.
  in ensurePlayable gs0

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
    (gsHoneyCleared gs)
    (gsBalloonsPopped gs)
    (gsCookiesCollected gs)
    (gsCakesCleared gs)
    (gsSafesOpened gs)

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
    keep (Honey _) = True
    keep (Balloon _) = True
    keep Cookie = True
    keep (Cake _) = True
    keep MagicHat = True
    keep (Maker _ _) = True
    keep (Snail _ _) = True
    keep (Safe _) = True
    keep (Flip _ _) = True
    keep Surprise = True
    keep (Bottle _) = True
    keep TimeSpirit = True
    keep (Countdown _ _) = True
    keep (Gem _ kind ice ov) =
      kind /= Normal || ice > 0 || ov /= Nothing
    -- Bare Normal gems are shuffled away; Line/Bomb/Rainbow specials stay

restoreDecor :: Board -> [CellDecor] -> Board
restoreDecor b = foldl (\board (CellDecor p cell) -> setCell board p cell) b

-- | If board has no valid move (and game not over), reshuffle to a playable board.
-- Retries a few times because restoreDecor can recreate a stuck layout.
ensurePlayable :: GameState -> GameState
ensurePlayable gs
  | Just _ <- gsOver gs = gs { gsShuffled = False }
  | hasValidMove (gsBoard gs) = gs { gsShuffled = False }
  | otherwise = go (24 :: Int) gs
  where
    go 0 g =
      let decor = extractDecor (gsBoard g)
          (board0, g') = shufflePlayable (gsGen g)
          board = restoreDecor board0 decor
      in g { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }
    go n g =
      let decor = extractDecor (gsBoard g)
          (board0, g') = shufflePlayable (gsGen g)
          board = restoreDecor board0 decor
          g2 = g { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }
      in if hasValidMove board then g2 else go (n - 1) g2

-- | Force reshuffle (e.g. player key S). Preserves stones / ice / overlays /
-- specials (Line/Bomb/Rainbow) / countdown bombs; keeps UFOs.
shuffleGame :: GameState -> GameState
shuffleGame gs =
  let decor = extractDecor (gsBoard gs)
      (board0, g') = shufflePlayable (gsGen gs)
      board = restoreDecor board0 decor
  in gs { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True, gsOver = Nothing }

countSafes :: Board -> Int
countSafes b =
  length
    [ ()
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , isSafe (getCell b (r, c))
    ]

countTimeSpirits :: Board -> Int
countTimeSpirits b =
  length
    [ ()
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , isTimeSpirit (getCell b (r, c))
    ]

-- | Leftover moves carried into the next campaign level (cap 3).
carryMovesBonus :: MovesLeft -> MovesLeft
carryMovesBonus left = min 3 (max 0 left)


trySwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
trySwap p1 p2 gs
  | Just o <- gsOver gs = (gs, o)
  | not (inBounds p1 && inBounds p2) = (gs, InvalidSwap)
  | not (adjacent p1 p2) = (gs, InvalidSwap)
  | swapBlockedByStone (gsBoard gs) p1 p2 =
      (gs { gsHint = Nothing, gsShuffled = False, gsLastCleared = [] }, NoMatch)
  | otherwise =
      let board0 = gsBoard gs
          swapped = swapCells board0 p1 p2
          rainbow = isRainbowSwap board0 p1 p2
          specialCombo = isSpecialCombo board0 p1 p2
      in if not rainbow && not specialCombo && not (hasAnyMatch swapped)
           then (gs { gsHint = Nothing, gsShuffled = False, gsLastCleared = [] }, NoMatch)
           else
             let ufos0 = gsUfos gs
                 (board0', cleared0, gained0, combo0, tallies0, stones0, chests0, honey0, balloons0, cookies0, cakes0, uAbs0, ufos1, pos0, g0') =
                   if rainbow
                     then
                       let seeds = rainbowClearSeeds swapped p1 p2
                       in runCascadeScoredFromSeedsWithUfos (Just p2) seeds ufos0 (gsPortals gs) (gsGen gs) swapped
                     else if specialCombo
                       then
                         let seeds = comboClearSeeds swapped p1 p2
                         in runCascadeScoredFromSeedsWithUfos (Just p2) seeds ufos0 (gsPortals gs) (gsGen gs) swapped
                       else runCascadeScoredWithUfos (Just p2) ufos0 (gsPortals gs) (gsGen gs) swapped
                 -- Countdown bombs: tick after move; zeros explode 3×3 (keep UFOs + portals)
                 (boardCd, cleared1, gained1, combo1, tallies1, stones1, chests1, honey1, balloons1, cookies1, cakes1, uAbs1, ufosCd, pos1, g1') =
                   resolveCountdowns ufos1 (gsPortals gs) g0' board0'
                 -- Conveyor belts: shift then cascade if new matches
                 boardBelt = shiftBelts boardCd (gsBelts gs)
                 (boardBeltCas, cleared2, gained2, combo2, tallies2, stones2, chests2, honey2, balloons2, cookies2, cakes2, uAbs2, ufos2, pos2, g') =
                   if null (gsBelts gs)
                     then (boardCd, 0, 0, 0, zip allColors (repeat 0), 0, 0, 0, 0, 0, 0, 0, ufosCd, [], g1')
                     else runCascadeScoredWithUfos Nothing ufosCd (gsPortals gs) g1' boardBelt
                 -- Vine / chocolate / steam, then snails crawl (skip belt cells — no double-step)
                 beltCells = nub (concat (gsBelts gs))
                 board1 = stepSnailsAvoiding beltCells (spreadSteam (spreadChoco (spreadVines boardBeltCas)))
                 clearedSites = nub (pos0 ++ pos1 ++ pos2)
                 gained = gained0 + gained1 + gained2
                 combo =
                   let c1 = max combo0 (if cleared1 > 0 then combo0 + combo1 else combo0)
                   in max c1 (if cleared2 > 0 then c1 + combo2 else c1)
                 tallies = mergeTallies (mergeTallies tallies0 tallies1) tallies2
                 stonesHit = stones0 + stones1 + stones2
                 chestsHit = chests0 + chests1 + chests2
                 honeyHit = honey0 + honey1 + honey2
                 balloonHit = balloons0 + balloons1 + balloons2
                 cookieHit = cookies0 + cookies1 + cookies2
                 cakeHit = cakes0 + cakes1 + cakes2
                 safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
                 spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
                 (carpetOpen', carpetHit) = coverCarpets (gsCarpetOpen gs) (pos0 ++ pos1 ++ pos2)
                 uAbs = uAbs0 + uAbs1 + uAbs2
                 ufoCollected' = gsUfoCollected gs + uAbs
                 cookies' = gsCookiesCollected gs + cookieHit
                 cakes' = gsCakesCleared gs + cakeHit
                 safes' = gsSafesOpened gs + safesHit
                 carpets' = gsCarpetsCovered gs + carpetHit
                 _cleared = cleared0 + cleared1 + cleared2
                 collectDelta = case gsGoal gs of
                   GoalCollect col _ -> lookupColor tallies col
                   GoalCollectMulti _ -> 0
                   GoalClearStone _ -> 0
                   GoalChest _ -> 0
                   GoalHoney _ -> 0
                   GoalBalloon _ -> 0
                   GoalCookie _ -> 0
                   GoalCake _ -> 0
                   GoalSafe _ -> 0
                   GoalCarpet _ -> 0
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
                   GoalHoney _ -> gsHoneyCleared gs + honeyHit
                   GoalBalloon _ -> gsBalloonsPopped gs + balloonHit
                   GoalCookie _ -> cookies'
                   GoalCake _ -> cakes'
                   GoalSafe _ -> safes'
                   GoalCarpet _ -> carpets'
                   GoalScore _ -> gsCollected gs
                   GoalUfo _ -> ufoCollected'
                 score' = gsScore gs + gained
                 moves' = gsMoves gs - 1 + 2 * spiritHit
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
                     , gsHoneyCleared = gsHoneyCleared gs + honeyHit
                     , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
                     , gsCookiesCollected = cookies'
                     , gsCakesCleared = cakes'
                     , gsSafesOpened = safes'
                     , gsGen = g'
                     , gsHistory = hist
                     , gsHint = Nothing
                     , gsCombo = combo
                     , gsShuffled = False
                     , gsUfos = ufos2
                     , gsUfoCollected = ufoCollected'
                     , gsCarpetOpen = carpetOpen'
                     , gsCarpetsCovered = carpets'
                     , gsLastCleared = clearedSites
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
      bonus = case gsOver gs of
        Just (LevelClear _ _) -> carryMovesBonus (gsMoves gs)
        _ -> 0
      gs' = newGameAtLevel idx (levelConfig lvl) seed
  in gs' { gsMoves = gsMoves gs' + bonus }


-- | Cells that chipIceOnClear leaves untouched (no peel / no clear).
hammerImmune :: Cell -> Bool
hammerImmune c = isMaker c || isSnail c || isBottle c || isMagicHat c

-- | Hammer: spend one charge to clear a single in-bounds cell, then cascade.
-- Does not consume a move.
-- Maker / Snail / Bottle / MagicHat are immune to direct seeds — reject without spending.
useHammer :: Pos -> GameState -> (GameState, Outcome)
useHammer p gs
  | Just o <- gsOver gs = (gs, o)
  | gsHammers gs <= 0 = (gs, InvalidSwap)
  | not (inBounds p) = (gs, InvalidSwap)
  | hammerImmune (getCell (gsBoard gs) p) =
      (gs { gsHint = Nothing, gsShuffled = False, gsLastCleared = [] }, NoMatch)
  | otherwise =
      let seeds = [p]
          (boardH, _n, gained, combo, tallies, stonesHit, chestsHit, honeyHit, balloonHit, cookieHit, cakeHit, uAbs, ufos', posCleared, g') =
            runCascadeScoredFromSeedsWithUfos Nothing seeds (gsUfos gs) (gsPortals gs) (gsGen gs) (gsBoard gs)
          board1 = spreadSteam (spreadChoco (spreadVines boardH))
          score' = gsScore gs + gained
          hist = take 20 (snapshot gs : gsHistory gs)
          ufoCollected' = gsUfoCollected gs + uAbs
          cookies' = gsCookiesCollected gs + cookieHit
          cakes' = gsCakesCleared gs + cakeHit
          safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
          spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
          safes' = gsSafesOpened gs + safesHit
          (carpetOpen', carpetHit) = coverCarpets (gsCarpetOpen gs) posCleared
          carpets' = gsCarpetsCovered gs + carpetHit
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
            GoalHoney _ -> gsHoneyCleared gs + honeyHit
            GoalBalloon _ -> gsBalloonsPopped gs + balloonHit
            GoalCookie _ -> cookies'
            GoalCake _ -> cakes'
            GoalSafe _ -> safes'
            GoalCarpet _ -> carpets'
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
              , gsHoneyCleared = gsHoneyCleared gs + honeyHit
              , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
              , gsCookiesCollected = cookies'
              , gsCakesCleared = cakes'
              , gsSafesOpened = safes'
              , gsGen = g'
              , gsHistory = hist
              , gsHint = Nothing
              , gsCombo = combo
              , gsShuffled = False
              , gsHammers = gsHammers gs - 1
              , gsMoves = gsMoves gs + 2 * spiritHit
              , gsUfos = ufos'
              , gsUfoCollected = ufoCollected'
              , gsCarpetOpen = carpetOpen'
              , gsCarpetsCovered = carpets'
              , gsLastCleared = nub posCleared
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
      (gs { gsHint = Nothing, gsShuffled = False, gsLastCleared = [] }, NoMatch)
  | otherwise =
      let board0 = gsBoard gs
          swapped = swapCells board0 p1 p2
          rainbow = isRainbowSwap board0 p1 p2
          specialCombo = isSpecialCombo board0 p1 p2
      in if not rainbow && not specialCombo && not (hasAnyMatch swapped)
           then (gs { gsHint = Nothing, gsShuffled = False, gsLastCleared = [] }, NoMatch)
           else
             let (boardF, _c, gained, combo, tallies, stonesHit, chestsHit, honeyHit, balloonHit, cookieHit, cakeHit, uAbs, ufos', posCleared, g') =
                   if rainbow
                     then runCascadeScoredFromSeedsWithUfos (Just p2) (rainbowClearSeeds swapped p1 p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
                     else if specialCombo
                       then runCascadeScoredFromSeedsWithUfos (Just p2) (comboClearSeeds swapped p1 p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
                       else runCascadeScoredWithUfos (Just p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
                 board1 = spreadSteam (spreadChoco (spreadVines boardF))
                 score' = gsScore gs + gained
                 hist = take 20 (snapshot gs : gsHistory gs)
                 ufoCollected' = gsUfoCollected gs + uAbs
                 cookies' = gsCookiesCollected gs + cookieHit
                 cakes' = gsCakesCleared gs + cakeHit
                 safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
                 spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
                 safes' = gsSafesOpened gs + safesHit
                 (carpetOpen', carpetHit) = coverCarpets (gsCarpetOpen gs) posCleared
                 carpets' = gsCarpetsCovered gs + carpetHit
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
                   GoalHoney _ -> gsHoneyCleared gs + honeyHit
                   GoalBalloon _ -> gsBalloonsPopped gs + balloonHit
                   GoalCookie _ -> cookies'
                   GoalCake _ -> cakes'
                   GoalSafe _ -> safes'
                   GoalCarpet _ -> carpets'
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
                     , gsHoneyCleared = gsHoneyCleared gs + honeyHit
                     , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
                     , gsCookiesCollected = cookies'
                     , gsCakesCleared = cakes'
                     , gsSafesOpened = safes'
                     , gsGen = g'
                     , gsHistory = hist
                     , gsHint = Nothing
                     , gsCombo = combo
                     , gsShuffled = False
                     , gsFreeSwaps = gsFreeSwaps gs - 1
                     , gsMoves = gsMoves gs + 2 * spiritHit
                     , gsUfos = ufos'
                     , gsUfoCollected = ufoCollected'
                     , gsCarpetOpen = carpetOpen'
                     , gsCarpetsCovered = carpets'
                     , gsLastCleared = nub posCleared
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

-- | Cross clear: spend one charge to clear row+col through a cell, then cascade.
-- Does not consume a move.
useCrossClear :: Pos -> GameState -> (GameState, Outcome)
useCrossClear p gs
  | Just o <- gsOver gs = (gs, o)
  | gsCrossClears gs <= 0 = (gs, InvalidSwap)
  | not (inBounds p) = (gs, InvalidSwap)
  | otherwise =
      let seeds = crossClearSeeds p
          (boardH, _n, gained, combo, tallies, stonesHit, chestsHit, honeyHit, balloonHit, cookieHit, cakeHit, uAbs, ufos', posCleared, g') =
            runCascadeScoredFromSeedsWithUfos Nothing seeds (gsUfos gs) (gsPortals gs) (gsGen gs) (gsBoard gs)
          board1 = spreadSteam (spreadChoco (spreadVines boardH))
          score' = gsScore gs + gained
          hist = take 20 (snapshot gs : gsHistory gs)
          ufoCollected' = gsUfoCollected gs + uAbs
          cookies' = gsCookiesCollected gs + cookieHit
          cakes' = gsCakesCleared gs + cakeHit
          safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
          spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
          safes' = gsSafesOpened gs + safesHit
          (carpetOpen', carpetHit) = coverCarpets (gsCarpetOpen gs) posCleared
          carpets' = gsCarpetsCovered gs + carpetHit
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
            GoalHoney _ -> gsHoneyCleared gs + honeyHit
            GoalBalloon _ -> gsBalloonsPopped gs + balloonHit
            GoalCookie _ -> cookies'
            GoalCake _ -> cakes'
            GoalSafe _ -> safes'
            GoalCarpet _ -> carpets'
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
              , gsHoneyCleared = gsHoneyCleared gs + honeyHit
              , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
              , gsCookiesCollected = cookies'
              , gsCakesCleared = cakes'
              , gsSafesOpened = safes'
              , gsGen = g'
              , gsHistory = hist
              , gsHint = Nothing
              , gsCombo = combo
              , gsShuffled = False
              , gsCrossClears = gsCrossClears gs - 1
              , gsMoves = gsMoves gs + 2 * spiritHit
              , gsUfos = ufos'
              , gsUfoCollected = ufoCollected'
              , gsCarpetOpen = carpetOpen'
              , gsCarpetsCovered = carpets'
              , gsLastCleared = nub posCleared
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

-- | Map unlock index after a terminal outcome (LevelClear unlocks through nextIdx).
unlockAfterClear :: Int -> Outcome -> Int
unlockAfterClear reached (LevelClear _ n) = max reached n
unlockAfterClear reached (Won _) = max reached (length allLevels - 1)
unlockAfterClear reached _ = reached

-- | Map node click: Just li to jump; Nothing = resume/ignore (same level or locked).
mapClickJump :: Int -> Int -> Int -> Maybe Int
mapClickJump curLevel reached clicked
  | clicked < 0 || clicked > reached = Nothing
  | clicked == curLevel = Nothing
  | otherwise = Just clicked

-- | Short tip shown after a Lost outcome (失败提示).
loseHint :: LevelGoal -> String
loseHint (GoalScore t) = "再冲冲分数吧，目标 " ++ show t
loseHint (GoalCollect _ n) = "优先收集该色宝石，目标 " ++ show n ++ " 个"
loseHint (GoalCollectMulti reqs) =
  "兼顾多色收集：" ++ show (length reqs) ++ " 种配额"
loseHint (GoalClearStone n) = "用邻消或特效砸箱子，目标 " ++ show n ++ " 个"
loseHint (GoalChest n) = "邻消打开宝箱，目标 " ++ show n ++ " 个"
loseHint (GoalHoney n) = "邻消砸开蜂蜜罐，目标 " ++ show n ++ " 个"
loseHint (GoalBalloon n) = "用同色邻消戳破气球，目标 " ++ show n ++ " 个"
loseHint (GoalCookie n) = "打通下方让饼干掉到底部，目标 " ++ show n ++ " 个"
loseHint (GoalCake n) = "邻消削掉蛋糕层，目标 " ++ show n ++ " 个"
loseHint (GoalSafe n) = "邻消打开保险箱掉出饼干，目标 " ++ show n ++ " 个"
loseHint (GoalUfo n) = "让飞碟吸走同色宝石，目标 " ++ show n ++ " 个"
loseHint (GoalCarpet n) = "在地毯格上消除宝石以铺地毯，目标 " ++ show n ++ " 格"
