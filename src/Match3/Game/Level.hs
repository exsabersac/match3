{-# LANGUAGE NamedFieldPuns #-}

-- | 开局与关卡装饰：新局 / 指定关 / 每日 / 重开 / 下一关，战役装饰布局（decorateLevel）、
-- 目标所需装饰补齐、皮带 / 传送门 / 飞碟布局、步数携带。
--
-- 依赖：State、Shuffle（开局 ensurePlayable）、Match3.Board.Grid / Random、元素注册表（装饰按元素名放置）。
-- 不变量：同一 (关卡, 种子) 总得到同一开局（随机数只来自 mkStdGen seed）。
module Match3.Game.Level
  ( newGame
  , levelBelts
  , levelPortals
  , decorateLevel
  , decorateLevelWith
  , levelPlacements
  , levelUfos
  , ensureGoalDecor
  , newGameAtLevel
  , newDailyGame
  , restart
  , restartLevel
  , carryMovesBonus
  , nextLevel
  ) where

import Match3.Board.Random (randomPlayableBoard)
import Match3.Ufo (Ufo(..), mkUfo)
import Match3.Conveyor (Belt)
import Match3.Carpet (levelCarpets)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry, countElementWith, placeAllWith)
import Match3.Element.Types (Arg(..), ElementName, Placement(..))
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

-- | 各关的地面层（段 2c 的扩展点）。前 38 关都没有地面层元素；段 5 的第 39 关铺 16 格双层果冻（共 32 层）：
-- 中间两行各 6 格 + 四个内角。
levelGround :: Int -> Ground
levelGround 38 = [(p, ("jelly", 2)) | p <- [(r, c) | r <- [3, 4], c <- [1 .. 6]] ++ [(1, 1), (1, 6), (6, 1), (6, 6)]]
levelGround _ = []

-- | Bidirectional portal pairs for campaign levels.
levelPortals :: Int -> [(Pos, Pos)]
levelPortals 26 = [((0, 1), (7, 6)), ((0, 6), (7, 1))]
levelPortals 27 = [((0, 3), (7, 4))]
levelPortals _ = []

-- | Place stones / grass / vines / countdown décor (preserves gem color for overlays).
-- 第二刀 2b：各关装饰改为按元素名引用的放置表（levelPlacements），由元素定义的 edPlace 解释。
decorateLevel :: Int -> Board -> Board
decorateLevel = decorateLevelWith defaultRegistry

-- | decorateLevel（指定注册表）。
decorateLevelWith :: Registry -> Int -> Board -> Board
decorateLevelWith reg li b = placeAllWith reg b (levelPlacements li)

-- | 逐格参数不同的放置（层数 / 颜色）：按列表顺序展开成单格放置。
each :: ElementName -> (a -> [Arg]) -> [(Pos, a)] -> [Placement]
each name f xs = [Place name (f x) [p] | (p, x) <- xs]

-- | 按层数放置（每格层数不同）。
layersAt :: ElementName -> [(Pos, Int)] -> [Placement]
layersAt name = each name (\n -> [AInt n])

-- | 战役各关的装饰放置表（按元素名；应用顺序 = 列表顺序）。
levelPlacements :: Int -> [Placement]
levelPlacements li = case li of
  4 -> [Place "choco" [] [(2, 3), (3, 2), (3, 5), (5, 4)]]
  -- Ice seals on a few gems (开心消消乐冰层入门)
  5 -> [Place "ice" [AInt 1] [(2, 2), (2, 5), (5, 3), (6, 6)]]
  7 -> [Place "stone" [] [(4, 1), (4, 3), (4, 5), (5, 2), (5, 4), (6, 1), (6, 3), (6, 5)]]
  8 -> [Place "grass" [] [(2, 2), (2, 5), (3, 3), (3, 6), (5, 1), (5, 4), (6, 3), (6, 6)]]
  9 -> [Place "vine" [] [(2, 2), (2, 4), (4, 3), (5, 5)]]
  10 -> [Place "grass" [] [(5, 1), (5, 3), (5, 5), (6, 2), (6, 4)]]
  11 -> [Place "countdown" [AInt 4] [(2, 2), (2, 5), (5, 3), (6, 6)]]
  14 ->
    [ Place "grass" [] [(2, 2), (3, 5), (5, 3)]
    , Place "choco" [] [(1, 1), (1, 6), (6, 2)]
    ]
  15 ->
    [ Place "stone" [] [(4, 0), (4, 7), (5, 1), (5, 6)]
    , Place "grass" [] [(2, 1), (2, 6)]
    , Place "vine" [] [(6, 3)]
    , Place "choco" [] [(1, 3), (7, 4)]
    , Place "chest" [AInt 1] [(7, 1)]
    , Place "chest" [AInt 2] [(7, 6)]
    , Place "countdown" [AInt 4] [(3, 3)]
    ]
  16 -> layersAt "chest" [((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1)]
  17 ->
    layersAt "chest" [((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1)]
      ++ [Place "choco" [] [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]]
  -- 蜂蜜: honey jars to smash
  18 -> layersAt "honey" [((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1)]
  19 ->
    layersAt "honey" [((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1)]
      ++ [Place "choco" [] [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]]
  -- 气球: colored balloons popped by same-color adjacent clears
  20 -> each "balloon" (\c -> [AColor c]) [((2, 2), C1), ((2, 5), C3), ((4, 1), C2), ((4, 3), C1), ((4, 5), C4), ((6, 2), C3), ((6, 5), C5)]
  -- 饼干: cookies high on board; clear below so they fall to bottom
  21 -> [Place "cookie" [] [(0, 1), (0, 3), (0, 5), (1, 2), (1, 4), (1, 6), (2, 3)]]
  22 ->
    [ Place "cookie" [] [(0, 2), (0, 5), (1, 1), (1, 4), (1, 6), (2, 3)]
    , Place "choco" [] [(3, 1), (3, 6)]
    , Place "fog" [AInt 1] [(4, 2), (4, 3), (4, 4), (5, 3), (6, 2), (6, 5)]
    ]
  -- 蛋糕: layered cakes to smash (distinct from Cookie)
  23 -> layersAt "cake" [((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1)]
  -- 帽宴: cakes + magic hats mixed
  24 ->
    layersAt "cake" [((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1)]
      ++ [ Place "magic_hat" [] [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]
         , Place "choco" [] [(6, 1), (6, 5)]
         ]
  -- 锁链: iron chains lock gems; peel by adjacent clear
  25 ->
    [ Place "chain" [AInt 1] [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
    , Place "chain" [AInt 2] [(4, 3), (4, 4)]
    , Place "stone" [] [(1, 1), (1, 6)]
    , Place "choco" [] [(5, 1), (5, 6)]
    ]
  -- 果汁: juice makers + fog; portals applied via levelPortals
  26 ->
    each "maker" (\(c, n) -> [AColor c, AInt n]) [((2, 2), (C1, 2)), ((2, 5), (C1, 3)), ((5, 3), (C1, 2)), ((5, 4), (C3, 2))]
      ++ [ Place "fog" [AInt 1] [(3, 1), (3, 6), (6, 2), (6, 5)]
         , Place "choco" [] [(1, 3), (1, 4)]
         ]
  27 ->
    [ Place "stone" [] [(0, 0), (0, 7), (7, 0), (7, 7)]
    , Place "chest" [] [(3, 1), (3, 6)]
    , Place "honey" [] [(2, 3), (2, 4)]
    ]
      ++ each "balloon" (\c -> [AColor c]) [((5, 1), C1), ((5, 6), C3)]
      ++ [ Place "cookie" [] [(0, 2), (0, 5)]
         , Place "cake" [AInt 1] [(1, 3), (1, 4)]
         , Place "magic_hat" [] [(7, 2), (7, 5)]
         , Place "maker" [AColor C2, AInt 2] [(4, 0), (4, 7)]
         , Place "choco" [] [(2, 2), (2, 5)]
         , Place "fog" [AInt 2] [(5, 2), (5, 5)]
         , Place "chain" [AInt 1] [(6, 1), (6, 6)]
         , Place "freeze" [AInt 1] [(3, 3), (3, 4)]
         , Place "curtain" [AInt 1] [(5, 3), (5, 4)]
         , Place "safe" [AInt 1] [(6, 4)]
         , Place "flip" [AColor C1, AColor C3] [(7, 3)]
         , Place "surprise" [] [(5, 0)]
           -- Bottle off portal entrance (0,3) *and* off row-1 belt: Bottle never clears
           -- and is not portal-transferable; on a belt cell it permanently occupies one
           -- slot of the cycle (same immortal-blocker class as portal endpoints).
         , Place "bottle" [AColor C1] [(2, 7)]
         , Place "vine" [] [(6, 3)]
           -- Snail off portal row 0: Cookie at (0,5) would reverse it onto portal A (0,3).
           -- Engine also walls portal endpoints; décor keeps crawl path clear of the pair.
         , Place "snail" [AInt 0, AInt 1] [(2, 0)]
         , Place "countdown" [AInt 5] [(4, 4)]
         ]
  -- 蜗牛: crawling snails that push gems after each move
  28 ->
    each "snail" (\(dr, dc) -> [AInt dr, AInt dc])
      [((1, 1), (0, 1)), ((1, 6), (0, -1)), ((4, 2), (1, 0)), ((4, 5), (1, 0)), ((6, 3), (0, 1)), ((2, 4), (0, -1))]
  -- 冰冻: rocket freeze overlays (block swap, peel by adjacent; gems still match)
  29 ->
    [ Place "freeze" [AInt 1] [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
    , Place "freeze" [AInt 2] [(4, 3), (4, 4)]
    , Place "choco" [] [(1, 1), (1, 6)]
    ]
  -- 窗帘: curtain columns (遮挡整列/区域，邻消揭开)
  30 ->
    [ Place "curtain" [AInt 1] [(r, c) | r <- [1, 2, 3, 4, 5, 6], c <- [1, 6]]
    , Place "curtain" [AInt 2] [(2, 3), (2, 4), (5, 3), (5, 4)]
    , Place "choco" [] [(0, 2), (0, 5)]
    ]
  -- 金库: safes open into cookies; dual-face flips mixed in
  31 ->
    layersAt "safe" [((2, 2), 1), ((2, 5), 2), ((4, 1), 1), ((4, 3), 2), ((4, 5), 1), ((6, 2), 1), ((6, 5), 2)]
      ++ each "flip" (\(f, bk) -> [AColor f, AColor bk])
           [ ((1, 1), (C1, C3)), ((1, 6), (C2, C4)), ((3, 0), (C3, C1))
           , ((3, 7), (C4, C2)), ((5, 3), (C1, C2)), ((5, 4), (C2, C1))
           ]
      ++ [Place "choco" [] [(7, 1), (7, 6)]]
  -- surprise boxes: open to special or 3x3 pop
  32 ->
    [ Place "surprise" [] [(1, 1), (1, 6), (2, 3), (3, 1), (3, 6), (4, 0), (4, 4), (5, 2), (5, 5), (6, 3)]
    , Place "choco" [] [(7, 2), (7, 5)]
    ]
  -- dye bottles paint neighbors on adjacent clear
  33 ->
    each "bottle" (\c -> [AColor c])
      [ ((2, 2), C3), ((2, 5), C3), ((4, 1), C1), ((4, 6), C3)
      , ((5, 3), C3), ((5, 4), C2), ((6, 2), C3), ((6, 5), C3)
      ]
      ++ [Place "fog" [AInt 1] [(1, 3), (1, 4)]]
  -- 时灵: time spirits award +2 moves when adjacent-cleared
  34 -> [Place "time_spirit" [] [(1, 1), (1, 6), (2, 3), (3, 2), (3, 5), (4, 4), (5, 1), (5, 6), (6, 3)]]
  -- 蒸汽: steam overlays block match, adjacent extinguish, then spread
  35 ->
    [ Place "steam" [] [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
    , Place "steam" [] [(1, 3), (1, 4), (4, 3), (4, 4)]
    , Place "choco" [] [(7, 2), (7, 5)]
    ]
  -- 地毯: open floor tiles via levelCarpets; light choco garnish
  36 -> [Place "choco" [] [(1, 1), (1, 6), (6, 1), (6, 6)]]
  -- 织毯: carpet + choco + fog mix
  37 ->
    [ Place "choco" [] [(1, 2), (1, 5), (6, 2), (6, 5)]
    , Place "fog" [AInt 1] [(0, 3), (0, 4), (7, 3), (7, 4)]
    ]
  -- 气泡（段 5）：12 个，邻消即破、随重力下落
  39 -> [Place "bubble" [] [(1, 1), (1, 6), (2, 3), (2, 4), (3, 0), (3, 7), (4, 2), (4, 5), (5, 1), (5, 6), (6, 3), (6, 4)]]
  _ -> []

-- | UFO placements for campaign levels.
levelUfos :: Int -> [Ufo]
levelUfos 12 = [mkUfo (2, 3) C1]
levelUfos 13 = [mkUfo (1, 2) C1, mkUfo (1, 5) C3]
levelUfos 15 = [mkUfo (0, 4) C2]
levelUfos 27 = [mkUfo (2, 4) C1]
levelUfos _ = []

-- | Daily (and any bare board) must still be completable: if the goal needs
-- board entities but level décor did not place enough, seed a minimal set.
-- 第二刀 2b：目标 → (元素名, 放置表)；计数按元素名（countElementWith）。
ensureGoalDecor :: LevelGoal -> Board -> Board
ensureGoalDecor goal b =
  case goalDecor goal of
    Just (name, n, placements)
      | countElementWith defaultRegistry name b < n -> placeAllWith defaultRegistry b placements
    _ -> b
  where
    slots7 = [(2, 2), (2, 5), (4, 1), (4, 3), (4, 5), (6, 2), (6, 5)]
    layered name n = Just (name, n, [Place name [AInt 1] (take (max n 6) slots7)])
    goalDecor g = case g of
      GoalClearStone n -> Just ("stone", n, [Place "stone" [] (take (max n 6) [(4, 1), (4, 3), (4, 5), (5, 2), (5, 4), (6, 1), (6, 3), (6, 5)])])
      GoalHoney n -> layered "honey" n
      GoalChest n -> layered "chest" n
      GoalCake n -> layered "cake" n
      GoalSafe n -> layered "safe" n
      GoalBalloon n ->
        Just ("balloon", n, each "balloon" (\c -> [AColor c]) (take (max n 6) (zip slots7 [C1, C3, C2, C1, C4, C3, C5])))
      -- Cookies high on board so clearing below can drop them to the exit row
      GoalCookie n -> Just ("cookie", n, [Place "cookie" [] (take (max n 6) [(0, 1), (0, 3), (0, 5), (1, 2), (1, 4), (1, 6), (2, 1), (2, 3), (2, 5)])])
      _ -> Nothing

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
          , gsElementCounts = []
          , gsGround = levelGround li
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
