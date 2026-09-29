{-# LANGUAGE NamedFieldPuns #-}

-- | 开局与关卡装饰：新局 / 指定关 / 每日 / 重开 / 下一关，战役装饰布局（decorateLevel）、
-- 目标所需装饰补齐、皮带 / 传送门 / 飞碟布局、步数携带。
--
-- 依赖：State、Shuffle（开局 ensurePlayable）、Match3.Board、各机制模块的构造器。
-- 不变量：同一 (关卡, 种子) 总得到同一开局（随机数只来自 mkStdGen seed）。
module Match3.Game.Level
  ( newGame
  , levelBelts
  , levelPortals
  , decorateLevel
  , levelUfos
  , overlayAt
  , ensureGoalDecor
  , newGameAtLevel
  , newDailyGame
  , restart
  , restartLevel
  , carryMovesBonus
  , nextLevel
  ) where

import Match3.Board (randomPlayableBoard, setCell, getCell)
import Match3.Ufo (Ufo(..), mkUfo)
import Match3.Conveyor (Belt)
import Match3.Carpet (levelCarpets)
import Match3.Countdown (spawnCountdown)
import Match3.Types
import System.Random (mkStdGen)
import Match3.Game.Shuffle
import Match3.Game.State

-- | 按给定配置与种子开局，使用第 1 关（下标 0）的装饰。
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
      -- Bottle off portal entrance (0,3) *and* off row-1 belt: Bottle never clears
      -- and is not portal-transferable; on a belt cell it permanently occupies one
      -- slot of the cycle (same immortal-blocker class as portal endpoints).
      b3r = setCell b3q (2, 7) (mkBottle C1)
      b4 = overlayAt b3r Vine [(6, 3)]
      -- Snail off portal row 0: Cookie at (0,5) would reverse it onto portal A (0,3).
      -- Engine also walls portal endpoints; décor keeps crawl path clear of the pair.
      b5 =
        foldl (\board (p, dr, dc) -> setCell board p (mkSnail dr dc))
              b4
              [((2, 0), 0, 1)]
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
    -- Cookies high on board so clearing below can drop them to the exit row
    GoalCookie n | countKind isCookie b < n ->
      place n mkCookie
        [(0, 1), (0, 3), (0, 5), (1, 2), (1, 4), (1, 6), (2, 1), (2, 3), (2, 5)]
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

-- | 指定关卡（0 基下标）+ 配置 + 种子开局：可玩随机盘 → 关卡装饰 → 目标所需装饰，
-- 填好皮带 / 传送门 / 飞碟 / 地毯字段 → ensurePlayable。同一 (关卡, 种子) 总是同一开局。
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
          , gsCarpetOpen =
              let placed = levelCarpets li
              in if null placed
                   then case cfgGoal cfg of
                          GoalCarpet n ->
                            take (max n 1)
                              [ (3, 2), (3, 3), (3, 4), (3, 5)
                              , (4, 2), (4, 3), (4, 4), (4, 5)
                              , (2, 2), (2, 5), (5, 2), (5, 5)
                              ]
                          _ -> []
                   else placed
          , gsCarpetsCovered = 0
          , gsLastCleared = []
          , gsDaily = False
          }
  -- Décor can remove the only legal swap (e.g. dense 终章); auto-reshuffle gems.
  in ensurePlayable gs0

-- | Date-seeded daily challenge: same bare board as L0 décor, but clears as Won
-- (not LevelClear into campaign) and never bumps map unlock.
newDailyGame :: GameConfig -> Int -> GameState
newDailyGame cfg seed =
  (newGameAtLevel 0 cfg seed) { gsDaily = True }

-- | newGame 的别名（冻结 API）。
restart :: GameConfig -> Int -> GameState
restart = newGame

-- | 同一关换种子重开。
restartLevel :: GameState -> Int -> GameState
restartLevel gs seed =
  let lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
  in newGameAtLevel (lvlIndex lvl) (levelConfig lvl) seed

-- | 战役过关剩余步携带入下一关，上限 3；与三星分母（印制步数）无关。
-- | Leftover moves carried into the next campaign level (cap 3).
carryMovesBonus :: MovesLeft -> MovesLeft
carryMovesBonus left = min 3 (max 0 left)


-- | 进入下一关：LevelClear 时取它给出的下一关并携带剩余步数（carryMovesBonus，最多 3 步）；
-- 其它情况进入当前关的下一关（不超过最后一关），不携带步数。
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
