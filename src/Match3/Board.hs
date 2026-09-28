{-# LANGUAGE ScopedTypeVariables #-}
module Match3.Board
  ( getCell
  , setCell
  , swapCells
  , inBounds
  , adjacent
  , findMatches
  , findMatchRuns
  , hasAnyMatch
  , clearMatches
  , clearMatchesAt
  , applyGravity
  , refill
  , stepCascade
  , stepCascadeAt
  , runCascade
  , runCascadeAt
  , runCascadeScored
  , randomBoard
  , randomStableBoard
  , randomPlayableBoard
  , shufflePlayable
  , hasValidMove
  , scoreForCleared
  , scoreForWave
  , findHint
  , MatchRun(..)
  , countColor
  ) where

import Data.List (foldl', nub)
import Match3.Obstacles (withAdjacentStones)
import Match3.Types
import System.Random (RandomGen, randomR)

inBounds :: Pos -> Bool
inBounds (r, c) = r >= 0 && r < boardSize && c >= 0 && c < boardSize

getCell :: Board -> Pos -> Cell
getCell b (r, c) = (b !! r) !! c

setCell :: Board -> Pos -> Cell -> Board
setCell b (r, c) v =
  take r b ++ [take c row ++ [v] ++ drop (c + 1) row] ++ drop (r + 1) b
  where
    row = b !! r

swapCells :: Board -> Pos -> Pos -> Board
swapCells b p1 p2 =
  let a = getCell b p1
      b' = setCell b p1 (getCell b p2)
  in setCell b' p2 a

adjacent :: Pos -> Pos -> Bool
adjacent (r1, c1) (r2, c2) =
  (abs (r1 - r2) == 1 && c1 == c2) || (abs (c1 - c2) == 1 && r1 == r2)

-- | A contiguous same-color gem run of length >= 3 (stones break runs).
data MatchRun = MatchRun
  { runColor :: Color
  , runPos   :: [Pos]
  , runIsH   :: Bool  -- True = horizontal
  } deriving (Eq, Show)

findMatchRuns :: Board -> [MatchRun]
findMatchRuns b = filter ((>= 3) . length . runPos) (hRuns ++ vRuns)
  where
    hRuns =
      [ MatchRun col ps True
      | r <- [0 .. boardSize - 1]
      , let rowPs = [(r, c) | c <- [0 .. boardSize - 1]]
      , (col, ps) <- groupGemRuns b rowPs
      ]
    vRuns =
      [ MatchRun col ps False
      | c <- [0 .. boardSize - 1]
      , let colPs = [(r, c) | r <- [0 .. boardSize - 1]]
      , (col, ps) <- groupGemRuns b colPs
      ]

-- | Group contiguous same-color *gems*; stones and color changes break runs.
groupGemRuns :: Board -> [Pos] -> [(Color, [Pos])]
groupGemRuns _ [] = []
groupGemRuns b (p : ps) = case getCell b p of
  Stone -> groupGemRuns b ps
  Gem col _ -> go [p] col ps
  where
    go run col [] = [(col, reverse run)]
    go run col (q : qs) = case getCell b q of
      Gem col' _ | col' == col -> go (q : run) col qs
      _ -> (col, reverse run) : groupGemRuns b (q : qs)

findMatches :: Board -> [Pos]
findMatches b = nub (concatMap runPos (findMatchRuns b))

hasAnyMatch :: Board -> Bool
hasAnyMatch = not . null . findMatchRuns

type MBoard = [[Maybe Cell]]

toM :: Board -> MBoard
toM = map (map Just)

setM :: MBoard -> Pos -> Maybe Cell -> MBoard
setM b (r, c) v =
  take r b ++ [take c row ++ [v] ++ drop (c + 1) row] ++ drop (r + 1) b
  where
    row = b !! r

-- | Expand clears: LineH/LineV/Bomb effects when those gem cells are in the match set.
expandSpecials :: Board -> [Pos] -> [Pos]
expandSpecials b seeds = go (nub seeds) (nub seeds)
  where
    go acc [] = acc
    go acc (p : ps) =
      let extra = case getCell b p of
            Gem _ LineH -> [(fst p, c) | c <- [0 .. boardSize - 1]]
            Gem _ LineV -> [(r, snd p) | r <- [0 .. boardSize - 1]]
            Gem _ Bomb ->
              [ (r, c)
              | r <- [fst p - 1 .. fst p + 1]
              , c <- [snd p - 1 .. snd p + 1]
              , inBounds (r, c)
              ]
            Gem _ Normal -> []
            Stone -> []
          new = filter (`notElem` acc) extra
      in go (acc ++ new) (ps ++ new)

-- | Specials spawned from runs: len>=5 Bomb, len==4 Line (orient by run).
-- Prefer spawnPos if provided and in the run; else middle of run.
spawnSpecials :: Maybe Pos -> [MatchRun] -> [(Pos, Cell)]
spawnSpecials prefer runs =
  [ (pos, Gem (runColor run) kind)
  | run <- runs
  , let n = length (runPos run)
  , n >= 4
  , let kind
          | n >= 5 = Bomb
          | runIsH run = LineH
          | otherwise = LineV
        pos = case prefer of
          Just p | p `elem` runPos run -> p
          _ -> runPos run !! (n `div` 2)
  ]

-- | Count how many cleared positions have a given color (pre-clear board; stones skip).
countColor :: Board -> [Pos] -> Color -> Int
countColor b ps col =
  length
    [ p
    | p <- ps
    , case getCell b p of
        Gem c _ -> c == col
        Stone -> False
    ]

-- | Clear matches (+ special expansions + adjacent stones), place new specials.
clearMatches :: Board -> (MBoard, Int)
clearMatches b = clearMatchesAt Nothing b

clearMatchesAt :: Maybe Pos -> Board -> (MBoard, Int)
clearMatchesAt prefer b =
  let (mb, n, _) = clearMatchesDetailed prefer b
  in (mb, n)

-- | Like clearMatchesAt but also returns the cleared positions (pre-spawn).
clearMatchesDetailed :: Maybe Pos -> Board -> (MBoard, Int, [Pos])
clearMatchesDetailed prefer b =
  let runs = findMatchRuns b
      base = nub (concatMap runPos runs)
      expanded = expandSpecials b base
      -- Adjacent stones chip away when a neighbor clears
      allPos = withAdjacentStones b expanded
      n = length allPos
      mb0 = foldl' (\m p -> setM m p Nothing) (toM b) allPos
      spawns = spawnSpecials prefer runs
      mb1 =
        foldl'
          ( \m (p, cell) ->
              if p `elem` allPos then setM m p (Just cell) else m
          )
          mb0
          spawns
  in (mb1, n, allPos)

colGravity :: [Maybe Cell] -> [Maybe Cell]
colGravity col =
  let solids = [x | Just x <- col]
      holes = length col - length solids
  in replicate holes Nothing ++ map Just solids

transposeM :: MBoard -> MBoard
transposeM [] = []
transposeM ([] : _) = []
transposeM rows = map head rows : transposeM (map tail rows)

applyGravity :: MBoard -> MBoard
applyGravity mb = transposeM (map colGravity (transposeM mb))

randomColor :: RandomGen g => g -> (Color, g)
randomColor g =
  let (i, g') = randomR (0, numColors - 1) g
  in (toEnum i, g')

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

refill :: RandomGen g => g -> MBoard -> (Board, g)
refill g0 mb =
  let (filled, g') = fillList g0 (concat mb)
  in (chunk boardSize (map (maybe (error "refill: hole") id) filled), g')
  where
    fillList g [] = ([], g)
    fillList g (Nothing : xs) =
      let (c, g1) = randomColor g
          (rest, g2) = fillList g1 xs
      in (Just (mkGem c) : rest, g2)
    fillList g (Just x : xs) =
      let (rest, g1) = fillList g xs
      in (Just x : rest, g1)

scoreForCleared :: Int -> Score
scoreForCleared n = n * 10

-- | Wave 1 = 1x, wave 2 = 2x, ... (cells * 10 * wave).
scoreForWave :: Int -> Int -> Score
scoreForWave wave n = n * 10 * max 1 wave

stepCascade :: RandomGen g => g -> Board -> Maybe (Board, Int, g)
stepCascade = stepCascadeAt Nothing

stepCascadeAt :: RandomGen g => Maybe Pos -> g -> Board -> Maybe (Board, Int, g)
stepCascadeAt prefer g b =
  case stepCascadeDetailed prefer g b of
    Nothing -> Nothing
    Just (b', n, _, g') -> Just (b', n, g')

-- | Cascade step returning cleared positions for color tallying.
stepCascadeDetailed
  :: RandomGen g
  => Maybe Pos
  -> g
  -> Board
  -> Maybe (Board, Int, [Pos], g)
stepCascadeDetailed prefer g b
  | not (hasAnyMatch b) = Nothing
  | otherwise =
      let (mb, n, pos) = clearMatchesDetailed prefer b
          fallen = applyGravity mb
          (b', g') = refill g fallen
      in Just (b', n, pos, g')

runCascade :: RandomGen g => g -> Board -> (Board, Int, g)
runCascade = runCascadeAt Nothing

-- | First cascade step uses prefer (swap dest) for special spawn; later steps don't.
runCascadeAt :: RandomGen g => Maybe Pos -> g -> Board -> (Board, Int, g)
runCascadeAt prefer g b = case stepCascadeAt prefer g b of
  Nothing -> (b, 0, g)
  Just (b', n, g') ->
    let (b'', n', g'') = runCascadeAt Nothing g' b'
    in (b'', n + n', g'')

-- | Cascade with per-wave combo scoring + color tallies from cleared cells.
-- Returns (board, cellsCleared, scoreGained, maxComboWave, colorCounts, gen).
runCascadeScored
  :: RandomGen g
  => Maybe Pos
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], g)
runCascadeScored prefer g b = go prefer g b 0 0 0 (zip allColors (repeat 0))
  where
    go pref g' b' cells score maxW tallies =
      case stepCascadeDetailed pref g' b' of
        Nothing -> (b', cells, score, maxW, tallies, g')
        Just (b'', n, pos, g'') ->
          let wave = maxW + 1
              score' = score + scoreForWave wave n
              tallies' =
                [ (col, cnt + countColor b' pos col)
                | (col, cnt) <- tallies
                ]
          in go Nothing g'' b'' (cells + n) score' wave tallies'

randomBoard :: RandomGen g => g -> (Board, g)
randomBoard g0 =
  let (cells, g') = go (boardSize * boardSize) g0
  in (chunk boardSize cells, g')
  where
    go 0 g = ([], g)
    go n g =
      let (c, g1) = randomColor g
          (rest, g2) = go (n - 1) g1
      in (mkGem c : rest, g2)

randomStableBoard :: RandomGen g => g -> (Board, g)
randomStableBoard g =
  let (b, g') = randomBoard g
  in if hasAnyMatch b then randomStableBoard g' else (b, g')

-- | True if some adjacent gem-gem swap would create a match.
hasValidMove :: Board -> Bool
hasValidMove = maybe False (const True) . findHint

-- | Stable board with at least one valid move (no initial three-in-a-row).
randomPlayableBoard :: RandomGen g => g -> (Board, g)
randomPlayableBoard g =
  let (b, g') = randomStableBoard g
  in if hasValidMove b then (b, g') else randomPlayableBoard g'

-- | Reshuffle into a playable stable board (ignores previous layout).
shufflePlayable :: RandomGen g => g -> (Board, g)
shufflePlayable = randomPlayableBoard

-- | First adjacent swap that would create a match (for hint). Skips stone cells.
findHint :: Board -> Maybe (Pos, Pos)
findHint b =
  case
    [ (p1, p2)
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , let p1 = (r, c)
    , isGem (getCell b p1)
    , p2 <- [(r, c + 1), (r + 1, c)]
    , inBounds p2
    , isGem (getCell b p2)
    , hasAnyMatch (swapCells b p1 p2)
    ] of
    (x : _) -> Just x
    [] -> Nothing
