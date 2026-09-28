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
  , runCascadeScoredWithUfos
  , runCascadeScoredFromSeeds
  , runCascadeScoredFromSeedsWithUfos
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
  , resolveCountdowns
  , applyPortalTeleports
  , settleBoardPortals
  , expandSpecials
  ) where

import Data.List (foldl', nub)
import Match3.Ice (chipIceOnClear)
import Match3.Grass (clearOverlaysOn, clearChocoAdjacent, clearSteamAdjacent, chipAdjacentFog, chipAdjacentChain, chipAdjacentFreeze, chipAdjacentCurtain)
import Match3.Obstacles
  ( chipAdjacentStones
  , chipAdjacentChests
  , chipAdjacentHoney
  , chipAdjacentCakes
  , chipAdjacentSafes
  , chipAdjacentBalloons
  , chipAdjacentTimeSpirits
  , triggerAdjacentHats
  , chargeAdjacentMakers
  , openSurprises
  , triggerAdjacentBottles
  )
import Match3.Countdown
  ( countdownsAtZero
  , explodeSeedsFor
  , tickCountdowns
  )
import Match3.Combos (isSpecialCombo)
import Match3.Rainbow (isRainbow, isRainbowSwap)
import Match3.Types
import Match3.Ufo (Ufo, stepUfos)
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
  Stone _ -> groupGemRuns b ps
  Chest _ -> groupGemRuns b ps
  Honey _ -> groupGemRuns b ps
  Balloon _ -> groupGemRuns b ps
  Cookie -> groupGemRuns b ps
  Cake _ -> groupGemRuns b ps
  MagicHat -> groupGemRuns b ps
  Maker _ _ -> groupGemRuns b ps
  Snail _ _ -> groupGemRuns b ps
  Safe _ -> groupGemRuns b ps
  Surprise -> groupGemRuns b ps
  Bottle _ -> groupGemRuns b ps
  TimeSpirit -> groupGemRuns b ps
  Gem _ _ _ (Just (Fog _)) -> groupGemRuns b ps  -- fog hides gem from matches
  Gem _ _ _ (Just (Chain _)) -> groupGemRuns b ps  -- chain locks gem from matches
  Gem _ _ _ (Just (Curtain _)) -> groupGemRuns b ps  -- curtain hides gem from matches
  Gem _ _ _ (Just Steam) -> groupGemRuns b ps  -- steam hides gem from matches
  -- Freeze does NOT break runs: frozen gems still match (vs Chain/Fog/Curtain/Steam)
  Gem col _ _ _ -> go [p] col ps
  Flip col _ -> go [p] col ps
  Countdown col _ -> go [p] col ps
  where
    go run col [] = [(col, reverse run)]
    go run col (q : qs) = case getCell b q of
      Gem _ _ _ (Just (Fog _)) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem _ _ _ (Just (Chain _)) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem _ _ _ (Just (Curtain _)) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem _ _ _ (Just Steam) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem col' _ _ _ | col' == col -> go (q : run) col qs
      Flip col' _ | col' == col -> go (q : run) col qs
      Countdown col' _ | col' == col -> go (q : run) col qs
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

-- | Expand clears: LineH/LineV/Bomb effects when those gem cells are in the seed set.
-- Soft-locked specials do not fire: ice>1 only chips; Chain/Curtain peel without
-- clearing (same discipline as chipIceOnClear). Last ice (ice==1) clears + activates.
-- Rainbow is a no-op here (partner color comes from rainbowClearSeeds only).
expandSpecials :: Board -> [Pos] -> [Pos]
expandSpecials b seeds = go (nub seeds) (nub seeds)
  where
    -- True when this wave would hole the cell (so Line/Bomb may expand).
    activates ice ov
      | ice > 1 = False
      | ice == 1 = True -- last ice clears gem + overlay
      | Just (Chain _) <- ov = False
      | Just (Curtain _) <- ov = False
      | otherwise = True
    go acc [] = acc
    go acc (p : ps) =
      let extra = case getCell b p of
            Gem _ LineH ice ov | activates ice ov ->
              [(fst p, c) | c <- [0 .. boardSize - 1]]
            Gem _ LineV ice ov | activates ice ov ->
              [(r, snd p) | r <- [0 .. boardSize - 1]]
            Gem _ Bomb ice ov | activates ice ov ->
              [ (r, c)
              | r <- [fst p - 1 .. fst p + 1]
              , c <- [snd p - 1 .. snd p + 1]
              , inBounds (r, c)
              ]
            Gem _ LineH _ _ -> []
            Gem _ LineV _ _ -> []
            Gem _ Bomb _ _ -> []
            -- Rainbow activation is only via rainbowClearSeeds (swap partner color).
            -- Expanding own color here double-cleared partner+own on every Rainbow×gem swap.
            Gem _ Rainbow _ _ -> []
            Gem _ Normal _ _ -> []
            Stone _ -> []
            Chest _ -> []
            Honey _ -> []
            Balloon _ -> []
            Cookie -> []
            Cake _ -> []
            MagicHat -> []
            Maker _ _ -> []
            Snail _ _ -> []
            Safe _ -> []
            Flip _ _ -> []
            Surprise -> []
            Bottle _ -> []
            TimeSpirit -> []
            Countdown _ _ -> []
          new = filter (`notElem` acc) extra
      in go (acc ++ new) (ps ++ new)

-- | Specials spawned from runs: len>=5 Rainbow, len==4 Line (orient by run).
-- Only place onto positions that actually clear (holes). Flip / ice>1 seeds stay
-- on the board, so prefer/middle must fall back to a clearable cell in the run
-- (otherwise a 4-match with Flip/ice at the anchor silently drops the special).
spawnSpecials :: Maybe Pos -> [MatchRun] -> [Pos] -> [(Pos, Cell)]
spawnSpecials prefer runs clearable =
  [ (pos, Gem (runColor run) kind 0 Nothing)
  | run <- runs
  , let n = length (runPos run)
  , n >= 4
  , let kind
          | n >= 5 = Rainbow
          | runIsH run = LineH
          | otherwise = LineV
        slots = filter (`elem` clearable) (runPos run)
  , not (null slots)
  , let pos = case prefer of
          Just p | p `elem` slots -> p
          _ -> slots !! (length slots `div` 2)
  ]

-- | Count how many cleared positions have a given color (pre-clear board; stones skip).
countColor :: Board -> [Pos] -> Color -> Int
countColor b ps col =
  length
    [ p
    | p <- ps
    , case getCell b p of
        Gem c _ _ _ -> c == col
        Flip c _ -> c == col
        Countdown c _ -> c == col
        Stone _ -> False
        Chest _ -> False
        Honey _ -> False
        Balloon _ -> False
        Cookie -> False
        Cake _ -> False
        MagicHat -> False
        Maker _ _ -> False
        Snail _ _ -> False
        Safe _ -> False
        Surprise -> False
        Bottle _ -> False
        TimeSpirit -> False
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
      -- Match/special hits strip Grass/Vine/Choco on-cell (cleared cannot spread)
      bStripped = clearOverlaysOn b expanded
      -- Ice chips first: iced gems stay, ice-free positions may clear
      (bIced, iceFree) = chipIceOnClear bStripped expanded
      -- Surprise opens against match/special clears *before* adjacent peels, so a
      -- 3×3 explode contributes to trueClears (Bomb-parity for stone/fog/chain/…).
      -- Specials placed on former seed cells must stay out of clear holes.
      (bSurp, surpExplode0, surpSaved) = openSurprises bIced iceFree
      surpExpanded = expandSpecials bSurp surpExplode0
      bSurpStrip = clearOverlaysOn bSurp surpExpanded
      (bSurp2, surpFree) = chipIceOnClear bSurpStrip surpExpanded
      iceFree' = filter (`notElem` surpSaved) iceFree
      -- Soft hits (ice chip, Flip) stay out of iceFree/surpFree — not true holes.
      trueClears = nub (iceFree' ++ surpFree)
      -- Adjacent obstacle peels / charges against *all* true clear holes (once)
      (bChipped, deadStones) = chipAdjacentStones bSurp2 trueClears
      (bChest, deadChests) = chipAdjacentChests bChipped trueClears
      (bHoney, deadHoney) = chipAdjacentHoney bChest trueClears
      (bCake, deadCakes) = chipAdjacentCakes bHoney trueClears
      (bBal, deadBalloons) = chipAdjacentBalloons bCake trueClears
      bHat = triggerAdjacentHats bBal trueClears
      (bFog, _fogCleared) = chipAdjacentFog bHat trueClears
      (bChain, _chainCleared) = chipAdjacentChain bFog trueClears
      (bFreeze, _freezeCleared) = chipAdjacentFreeze bChain trueClears
      (bCurtain, _curtainCleared) = chipAdjacentCurtain bFreeze trueClears
      (bSafe, _openedSafes) = chipAdjacentSafes bCurtain trueClears
      (bSpirit, deadSpirits) = chipAdjacentTimeSpirits bSafe trueClears
      bMaker = chargeAdjacentMakers bSpirit trueClears
      bBottle = triggerAdjacentBottles bMaker trueClears
      -- Chocolate / steam: only true clears extinguish (not soft hits)
      bNoChoco = clearChocoAdjacent bBottle trueClears
      bNoSteam = clearSteamAdjacent bNoChoco trueClears
      allPos = nub (trueClears ++ deadStones ++ deadChests ++ deadHoney ++ deadCakes ++ deadBalloons ++ deadSpirits)
      n = length allPos
      mb0 = foldl' (\m p -> setM m p Nothing) (toM bNoSteam) allPos
      spawns = spawnSpecials prefer runs allPos
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

-- | Collect cookies that sit on the bottom row after gravity (开心消消乐饼干掉落收集).
-- Removes them, re-applies gravity, repeats until no bottom-row cookies remain.
drainBottomCookies :: MBoard -> (MBoard, Int)
drainBottomCookies mb =
  let bottom = boardSize - 1
      cols =
        [ c
        | c <- [0 .. boardSize - 1]
        , case (mb !! bottom) !! c of
            Just Cookie -> True
            _ -> False
        ]
      n = length cols
  in if n == 0
       then (mb, 0)
       else
         let mb1 =
               foldl
                 (\m c -> setM m (bottom, c) Nothing)
                 mb
                 cols
             fallen = applyGravity mb1
             (mb2, n2) = drainBottomCookies fallen
         in (mb2, n + n2)

-- | Bidirectional portal teleport on MBoard: gem/cookie/countdown on A with hole at B
-- moves A -> B (and reverse). Used after gravity + bottom-cookie drain so clears can
-- open exits without snatching cookies that already touched the bottom row.
applyPortalTeleports :: [(Pos, Pos)] -> MBoard -> MBoard
applyPortalTeleports portals mb =
  -- Each pair teleports at most one way per settle (A→B else B→A) to avoid bounce-back.
  foldl tryPair mb (nub portals)
  where
    atM m (r, c) = (m !! r) !! c
    transferable (Just (Gem _ _ _ _)) = True
    transferable (Just (Countdown _ _)) = True
    transferable (Just Cookie) = True
    transferable (Just (Flip _ _)) = True
    transferable _ = False
    tryPair m (a, b) =
      case (atM m a, atM m b) of
        (ca, Nothing)
          | transferable ca -> setM (setM m a Nothing) b ca
        (Nothing, cb)
          | transferable cb -> setM (setM m b Nothing) a cb
        _ -> m

-- | Gravity, drain bottom cookies, portal teleports (optional), gravity, drain again.
-- Cookies that reach the bottom must collect before a portal can snatch them
-- (触底优先于传送门); cookies that teleport onto a bottom exit still drain after.
settleBoardPortals :: [(Pos, Pos)] -> MBoard -> (MBoard, Int)
settleBoardPortals portals mb =
  let fallen = applyGravity mb
      (drained1, n1) = drainBottomCookies fallen
      ported = applyPortalTeleports portals drained1
      fallen2 = if ported == drained1 then ported else applyGravity ported
      (drained2, n2) = drainBottomCookies fallen2
  in (drained2, n1 + n2)

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
  case stepCascadeDetailed prefer [] g b of
    Nothing -> Nothing
    Just (b', n, _, _, _, _, _, _, _, g') -> Just (b', n, g')

-- | Cascade step returning cleared positions for color tallying.
-- Extra ints: stones, chests, honey, balloons, cookies, cakes fully cleared.
stepCascadeDetailed
  :: RandomGen g
  => Maybe Pos
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> Maybe (Board, Int, [Pos], Int, Int, Int, Int, Int, Int, g)
stepCascadeDetailed prefer portals g b
  | not (hasAnyMatch b) = Nothing
  | otherwise =
      let (mb, n, pos) = clearMatchesDetailed prefer b
          stonesHit =
            length [p | p <- pos, isStone (getCell b p)]
          chestsHit =
            length [p | p <- pos, isChest (getCell b p)]
          honeyHit =
            length [p | p <- pos, isHoney (getCell b p)]
          balloonHit =
            length [p | p <- pos, isBalloon (getCell b p)]
          cookiesCleared =
            length [p | p <- pos, isCookie (getCell b p)]
          cakesHit =
            length [p | p <- pos, isCake (getCell b p)]
          (settled, cookiesFallen) = settleBoardPortals portals mb
          (b', g') = refill g settled
      in Just (b', n, pos, stonesHit, chestsHit, honeyHit, balloonHit, cookiesCleared + cookiesFallen, cakesHit, g')

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
-- Returns (board, cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, gen).
runCascadeScored
  :: RandomGen g
  => Maybe Pos
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, g)
runCascadeScored prefer g b =
  let (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, _uAbs, _ufos, _cleared, g') =
        runCascadeScoredWithUfos prefer [] [] g b
  in (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, g')

-- | Like runCascadeScored but steps UFOs after each cascade wave (吸同色 + 移格).
-- portals: bidirectional pairs applied during settle (落入 A 从 B 出).
-- Returns (... stones, chests, honey, balloons, cookies, cakes, ufoAbsorbed, ufos', clearedPos, gen).
runCascadeScoredWithUfos
  :: RandomGen g
  => Maybe Pos
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredWithUfos prefer ufos0 portals g b =
  runCascadeScoredWithUfosFromWave 0 prefer ufos0 portals g b

-- | Like runCascadeScoredWithUfos but combo wave numbering continues from startW
-- (waves already completed, e.g. seed clear / UFO absorb before match cascades).
runCascadeScoredWithUfosFromWave
  :: RandomGen g
  => Int
  -> Maybe Pos
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredWithUfosFromWave startW prefer ufos0 portals g b =
  go prefer g b 0 0 startW (zip allColors (repeat 0)) 0 0 0 0 0 0 0 ufos0 []
  where
    go pref g' b' cells score maxW tallies stones chests honey balloons cookies cakes uAbs ufos clearedAcc =
      case stepCascadeDetailed pref portals g' b' of
        Nothing -> (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, uAbs, ufos, nub clearedAcc, g')
        Just (b'', n, pos, stn, cht, hny, bal, cok, cak, g'') ->
          let wave = maxW + 1
              score' = score + scoreForWave wave n
              tallies' =
                [ (col, cnt + countColor b' pos col)
                | (col, cnt) <- tallies
                ]
              (absorbed, ufos') = stepUfos b'' ufos
          in if null absorbed
               then go Nothing g'' b'' (cells + n) score' wave tallies' (stones + stn) (chests + cht) (honey + hny) (balloons + bal) (cookies + cok) (cakes + cak) uAbs ufos' (clearedAcc ++ pos)
               else
                 let (mb, n2, pos2) = clearFromSeedsDetailed Nothing b'' absorbed
                     stn2 = length [p | p <- pos2, isStone (getCell b'' p)]
                     cht2 = length [p | p <- pos2, isChest (getCell b'' p)]
                     hny2 = length [p | p <- pos2, isHoney (getCell b'' p)]
                     bal2 = length [p | p <- pos2, isBalloon (getCell b'' p)]
                     cok2 = length [p | p <- pos2, isCookie (getCell b'' p)]
                     cak2 = length [p | p <- pos2, isCake (getCell b'' p)]
                     (settled, cokFall) = settleBoardPortals portals mb
                     (b3, g3) = refill g'' settled
                     score2 = score' + scoreForWave (wave + 1) n2
                     tallies2 =
                       [ (col, cnt + countColor b'' pos2 col)
                       | (col, cnt) <- tallies'
                       ]
                     uAbs' = uAbs + length [p | p <- absorbed, p `elem` pos2]
                 in go Nothing g3 b3 (cells + n + n2) score2 (wave + 1) tallies2 (stones + stn + stn2) (chests + cht + cht2) (honey + hny + hny2) (balloons + bal + bal2) (cookies + cok + cok2 + cokFall) (cakes + cak + cak2) uAbs' ufos' (clearedAcc ++ pos ++ pos2)

-- | Clear an explicit seed set (expand specials + adjacent stones).
clearFromSeedsDetailed :: Maybe Pos -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearFromSeedsDetailed prefer b seeds0 =
  let runs = findMatchRuns b
      base = nub seeds0
      expanded = expandSpecials b base
      bStripped = clearOverlaysOn b expanded
      (bIced, iceFree) = chipIceOnClear bStripped expanded
      -- Surprise before adjacent peels (same as clearMatchesDetailed / Bomb parity)
      (bSurp, surpExplode0, surpSaved) = openSurprises bIced iceFree
      surpExpanded = expandSpecials bSurp surpExplode0
      bSurpStrip = clearOverlaysOn bSurp surpExpanded
      (bSurp2, surpFree) = chipIceOnClear bSurpStrip surpExpanded
      iceFree' = filter (`notElem` surpSaved) iceFree
      trueClears = nub (iceFree' ++ surpFree)
      (bChipped, deadStones) = chipAdjacentStones bSurp2 trueClears
      (bChest, deadChests) = chipAdjacentChests bChipped trueClears
      (bHoney, deadHoney) = chipAdjacentHoney bChest trueClears
      (bCake, deadCakes) = chipAdjacentCakes bHoney trueClears
      (bBal, deadBalloons) = chipAdjacentBalloons bCake trueClears
      bHat = triggerAdjacentHats bBal trueClears
      (bFog, _) = chipAdjacentFog bHat trueClears
      (bChain, _) = chipAdjacentChain bFog trueClears
      (bFreeze, _) = chipAdjacentFreeze bChain trueClears
      (bCurtain, _) = chipAdjacentCurtain bFreeze trueClears
      (bSafe, _) = chipAdjacentSafes bCurtain trueClears
      (bSpirit, deadSpirits) = chipAdjacentTimeSpirits bSafe trueClears
      bMaker = chargeAdjacentMakers bSpirit trueClears
      bBottle = triggerAdjacentBottles bMaker trueClears
      bNoChoco = clearChocoAdjacent bBottle trueClears
      bNoSteam = clearSteamAdjacent bNoChoco trueClears
      allPos = nub (trueClears ++ deadStones ++ deadChests ++ deadHoney ++ deadCakes ++ deadBalloons ++ deadSpirits)
      n = length allPos
      mb0 = foldl' (\m p -> setM m p Nothing) (toM bNoSteam) allPos
      spawns = spawnSpecials prefer runs allPos
      mb1 =
        foldl'
          ( \m (p, cell) ->
              if p `elem` allPos then setM m p (Just cell) else m
          )
          mb0
          spawns
  in (mb1, n, allPos)

-- | First wave clears explicit seeds (rainbow etc.), then normal match cascades.
runCascadeScoredFromSeeds
  :: RandomGen g
  => Maybe Pos
  -> [Pos]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, g)
runCascadeScoredFromSeeds prefer seeds g b =
  let (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, _u, _ufos, _cleared, g') =
        runCascadeScoredFromSeedsWithUfos prefer seeds [] [] g b
  in (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, g')

runCascadeScoredFromSeedsWithUfos
  :: RandomGen g
  => Maybe Pos
  -> [Pos]
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredFromSeedsWithUfos prefer seeds ufos0 portals g b
  | null seeds = runCascadeScoredWithUfos prefer ufos0 portals g b
  | otherwise =
      let (mb, n, pos) = clearFromSeedsDetailed prefer b seeds
          stones0 = length [p | p <- pos, isStone (getCell b p)]
          chests0 = length [p | p <- pos, isChest (getCell b p)]
          honey0 = length [p | p <- pos, isHoney (getCell b p)]
          balloons0 = length [p | p <- pos, isBalloon (getCell b p)]
          cookies0 = length [p | p <- pos, isCookie (getCell b p)]
          cakes0 = length [p | p <- pos, isCake (getCell b p)]
          (settled0, cookiesFall0) = settleBoardPortals portals mb
          (b1, g1) = refill g settled0
          score0 = scoreForWave 1 n
          tallies0 = [(col, countColor b pos col) | col <- allColors]
          (absorbed, ufos1) = stepUfos b1 ufos0
          (b1', g1', nU, stonesU, chestsU, honeyU, balloonsU, cookiesU, cakesU, talliesU, uAbs0, ufos2, posU) =
            if null absorbed
              then (b1, g1, 0, 0, 0, 0, 0, 0, 0, zip allColors (repeat 0), 0, ufos1, [])
              else
                let (mb2, n2, pos2) = clearFromSeedsDetailed Nothing b1 absorbed
                    stn2 = length [p | p <- pos2, isStone (getCell b1 p)]
                    cht2 = length [p | p <- pos2, isChest (getCell b1 p)]
                    hny2 = length [p | p <- pos2, isHoney (getCell b1 p)]
                    bal2 = length [p | p <- pos2, isBalloon (getCell b1 p)]
                    cok2 = length [p | p <- pos2, isCookie (getCell b1 p)]
                    cak2 = length [p | p <- pos2, isCake (getCell b1 p)]
                    (settled2, cokFall2) = settleBoardPortals portals mb2
                    (b2u, g2u) = refill g1 settled2
                    t2 = [(col, countColor b1 pos2 col) | col <- allColors]
                in (b2u, g2u, n2, stn2, cht2, hny2, bal2, cok2 + cokFall2, cak2, t2, length [p | p <- absorbed, p `elem` pos2], ufos1, pos2)
          wavesDone = (if n > 0 then 1 else 0) + (if nU > 0 then 1 else 0)
          (b2, cells2, score2, maxW2, tallies2, stones2, chests2, honey2, balloons2, cookies2, cakes2, uAbs2, ufos3, cleared2, g2) =
            -- Continue wave multipliers after seed (+ optional UFO) clear.
            runCascadeScoredWithUfosFromWave wavesDone Nothing ufos2 portals g1' b1'
          mergeT a b' =
            [ (col, lc a col + lc b' col) | col <- allColors ]
          lc xs col = maybe 0 id (lookup col xs)
          maxW = if cells2 > 0 then maxW2 else wavesDone
      in ( b2
         , n + nU + cells2
         , score0 + (if nU > 0 then scoreForWave 2 nU else 0) + score2
         , maxW
         , mergeT (mergeT tallies0 talliesU) tallies2
         , stones0 + stonesU + stones2
         , chests0 + chestsU + chests2
         , honey0 + honeyU + honey2
         , balloons0 + balloonsU + balloons2
         , cookies0 + cookiesFall0 + cookiesU + cookies2
         , cakes0 + cakesU + cakes2
         , uAbs0 + uAbs2
         , ufos3
         , nub (pos ++ posU ++ cleared2)
         , g2
         )

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


-- | After a successful cascade: tick countdown bombs; any at 0 explode (3×3) + cascade.
-- Threads UFOs + portals so explode settle still teleports / absorbs (到期爆炸不丢飞碟与门).
-- Returns (... stones, chests, honey, balloons, cookies, cakes, ufoAbsorbed, ufos', clearedPos, gen).
resolveCountdowns
  :: RandomGen g
  => [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
resolveCountdowns ufos0 portals g b =
  let bTick = tickCountdowns b
      zeros = countdownsAtZero bTick
  in if null zeros
       then (bTick, 0, 0, 0, zip allColors (repeat 0), 0, 0, 0, 0, 0, 0, 0, ufos0, [], g)
       else
         let seeds = explodeSeedsFor bTick
             (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, uAbs, ufos', cleared, g') =
               runCascadeScoredFromSeedsWithUfos Nothing seeds ufos0 portals g bTick
         in (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, uAbs, ufos', cleared, g')

-- | First adjacent swap that would create a match or activate a rainbow (for hint).
findHint :: Board -> Maybe (Pos, Pos)
findHint b =
  case matchHints ++ rainbowHints ++ comboHints of
    (x : _) -> Just x
    [] -> Nothing
  where
    matchHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , isGem (getCell b p1)
      , not (hasChain (getCell b p1) || hasFreeze (getCell b p1))
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , isGem (getCell b p2)
      , not (hasChain (getCell b p2) || hasFreeze (getCell b p2))
      , not (isRainbow (getCell b p1) || isRainbow (getCell b p2))
      , hasAnyMatch (swapCells b p1 p2)
      ]
    rainbowHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , not (hasChain (getCell b p1) || hasChain (getCell b p2)
               || hasFreeze (getCell b p1) || hasFreeze (getCell b p2))
      , isRainbowSwap b p1 p2
      ]
    comboHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , not (hasChain (getCell b p1) || hasChain (getCell b p2)
               || hasFreeze (getCell b p1) || hasFreeze (getCell b p2))
      , isSpecialCombo b p1 p2
      ]
