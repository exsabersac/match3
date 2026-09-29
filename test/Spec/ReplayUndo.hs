{-# LANGUAGE ScopedTypeVariables #-}

-- | 回放、撤销与洗牌：回放脚本（trace*）的终盘 / 逐轮 / 步末重放与结算一致，撤销恢复，洗牌保留装饰与目标进度。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.ReplayUndo
  ( tests
  ) where

import Match3.Board.Default (cascadeMatches, cascadeSeeds)
import Control.Monad (when)
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crGen, crTally, crWaves, crBoard, crUfos), CascadeTally(CascadeTally, ctCleared, ctMaxWave, ctScore))
import Match3.Core
import Match3.Board.Grid (atM)
import Match3.Element (defaultRegistry)
import qualified Match3.Engine as M3E
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Match3.Counts (singleCount)
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "undo_restores" undo_restores
  , testCase "shuffle_when_no_moves" shuffle_when_no_moves
  , testCase "shuffle_preserves_decor" shuffle_preserves_decor
  , testCase "undo_restores_carry_moves" undo_restores_carry_moves
  , testCase "shuffle_preserves_goal_progress" shuffle_preserves_goal_progress
  , testCase "shuffle_preserves_specials" shuffle_preserves_specials
  , testCase "trace_cascade_final_equals_stabilized" trace_cascade_final_equals_stabilized
  , testCase "trace_seeds_final_equals_stabilized" trace_seeds_final_equals_stabilized
  , testCase "trace_swap_final_equals_trySwap" trace_swap_final_equals_trySwap
  , testCase "trace_boosters_final_equal_result" trace_boosters_final_equal_result
  , testCase "trace_rejected_move_is_empty" trace_rejected_move_is_empty
  , testCase "trace_multi_wave_each_round_visible" trace_multi_wave_each_round_visible
  , testCase "trace_end_steps_replay_to_trySwap_final" trace_end_steps_replay_to_trySwap_final
  , testCase "trace_end_steps_boosters_replay" trace_end_steps_boosters_replay
  , testCase "trace_end_snail_push_and_turn" trace_end_snail_push_and_turn
  , testCase "trace_end_spread_from_adjacent_source" trace_end_spread_from_adjacent_source
  , testCase "trace_shuffle_step_replays" trace_shuffle_step_replays
  ]

undo_restores :: Assertion
undo_restores = do
  let gs0 = newGame defaultConfig 11
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need move"
    Just (p1, p2) -> do
      case stepThenUndo defaultRegistry gs0 (M3E.Swap p1 p2) of
        Nothing -> assertFailure "undo should work"
        Just gsU -> do
          gsBoard gsU @?= gsBoard gs0
          gsScore gsU @?= gsScore gs0
          gsMoves gsU @?= gsMoves gs0
          gsCollected gsU @?= gsCollected gs0

-- | Stuck board (no valid adjacent swap) is reshuffled to a playable stable board.
shuffle_when_no_moves :: Assertion
shuffle_when_no_moves = do
  let stuck = stuckNoMoveBoard
  assertBool "fixture has no match" (not (hasAnyMatch stuck))
  assertBool "fixture has no valid move" (not (hasValidMove stuck))
  let gs0 =
        (newGame defaultConfig 1)
          { gsBoard = stuck
          , gsGen = mkStdGen 999
          , gsOver = Nothing
          , gsHint = Nothing
          , gsShuffled = False
          }
      gs1 = ensurePlayable gs0
  assertBool "did shuffle" (gsShuffled gs1)
  assertBool "after: no initial match" (not (hasAnyMatch (gsBoard gs1)))
  assertBool "after: has valid move" (hasValidMove (gsBoard gs1))
  -- Force shuffleGame also yields playable
  let gs2 = shuffleGame gs0
  assertBool "shuffleGame playable" (hasValidMove (gsBoard gs2))
  assertBool "shuffleGame stable" (not (hasAnyMatch (gsBoard gs2)))

-- | Reshuffle keeps stones / ice / overlays; UFO list unchanged.
-- Also preserves Curtain / Freeze overlay positions and Carpet open-cell set.
shuffle_preserves_decor :: Assertion
shuffle_preserves_decor = do
  let cfg = GameConfig 20 (GoalScore 999)
      gs0 = newGameAtLevel 7 cfg 33  -- stones + belt level
      stonesBefore =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , isStone (getCell (gsBoard gs0) p)
        ]
      gs1 = shuffleGame gs0
      stonesAfter =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , isStone (getCell (gsBoard gs1) p)
        ]
  assertEqual "stones survive shuffle" (sort stonesBefore) (sort stonesAfter)
  assertBool "still playable or terminal-ok" (hasValidMove (gsBoard gs1) || isJust (gsOver gs1))
  -- UFO entities persist across shuffle
  let gsU = (newGameAtLevel 12 cfg 44)
      gsU' = shuffleGame gsU
  assertEqual "UFO count kept" (length (gsUfos gsU)) (length (gsUfos gsU'))
  assertEqual "UFO cells kept" (map ufoCell (gsUfos gsU)) (map ufoCell (gsUfos gsU'))
  -- Curtain overlays keep positions
  let gsCu = newGameAtLevel 30 cfg 55
      curtainsBefore =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasCurtain (getCell (gsBoard gsCu) p)
        ]
      gsCu' = shuffleGame gsCu
      curtainsAfter =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasCurtain (getCell (gsBoard gsCu') p)
        ]
  assertBool "curtains present before shuffle" (not (null curtainsBefore))
  assertEqual "curtains survive shuffle" (sort curtainsBefore) (sort curtainsAfter)
  -- Freeze overlays keep positions
  let gsFr = newGameAtLevel 29 cfg 66
      freezeBefore =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasFreeze (getCell (gsBoard gsFr) p)
        ]
      gsFr' = shuffleGame gsFr
      freezeAfter =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasFreeze (getCell (gsBoard gsFr') p)
        ]
  assertBool "freeze present before shuffle" (not (null freezeBefore))
  assertEqual "freeze survive shuffle" (sort freezeBefore) (sort freezeAfter)
  -- Carpet open cells are unchanged by shuffle (count + positions)
  let gsCa = newGameAtLevel 36 (levelConfig (allLevels !! 36)) 77
      openBefore = sort (gsCarpetOpen gsCa)
      gsCa' = shuffleGame gsCa
  assertBool "carpet open cells present" (not (null openBefore))
  assertEqual "carpet open cells kept" openBefore (sort (gsCarpetOpen gsCa'))
  assertEqual "carpet open count kept" (length openBefore) (length (gsCarpetOpen gsCa'))


-- | Undo after a move restores leftover moves (carry bank is only on nextLevel).
undo_restores_carry_moves :: Assertion
undo_restores_carry_moves = do
  let gs0 =
        (newGameAtLevel 0 (levelConfig (allLevels !! 0)) 42)
          { gsMoves = 7 }
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need match"
    Just (p1, p2) -> do
      let (gs1, _) = trySwap p1 p2 gs0
      assertEqual "spent one" (6 :: Int) (gsMoves gs1)
      case stepThenUndo defaultRegistry gs0 (M3E.Swap p1 p2) of
        Nothing -> assertFailure "undo"
        Just gsU -> do
          assertEqual "moves restored" (7 :: Int) (gsMoves gsU)
          -- nextLevel carry still caps at 3 from leftover
          let gsClear = gsU { gsOver = Just (LevelClear 10 1), gsMoves = 7 }
              gsNext = nextLevel gsClear 99
              base = lvlMoves (allLevels !! 1)
          assertEqual "carry cap 3" (base + 3) (gsMoves gsNext)

-- | Shuffle / ensurePlayable must not wipe goal tallies.
shuffle_preserves_goal_progress :: Assertion
shuffle_preserves_goal_progress = do
  let gs0 =
        (newGameAtLevel 7 (GameConfig 20 (GoalClearStone 8)) 33)
          { gsCounts = singleCount CountStones 3
          , gsCollected = 3
          , gsScore = 120
          , gsOver = Nothing
          }
      gs1 = shuffleGame gs0
  assertEqual "stones tally kept" (3 :: Int) (gsCount CountStones gs1)
  assertEqual "collected kept" (3 :: Int) (gsCollected gs1)
  assertEqual "score kept" (120 :: Int) (gsScore gs1)
  assertEqual "goal kept" (GoalClearStone 8) (gsGoal gs1)


--------------------------------------------------------------------------------
-- Shuffle must keep Line / Bomb / Rainbow (extractDecor specials)
--------------------------------------------------------------------------------

-- | Bare Line/Bomb/Rainbow specials must survive shuffleGame / ensurePlayable
-- décor restore. Regression: extractDecor only kept iced/overlaid gems, so
-- shuffle wiped player-earned specials while countdown bombs stayed (comment
-- claimed bombs were preserved).
shuffle_preserves_specials :: Assertion
shuffle_preserves_specials = do
  let board =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (1, 1) (Gem C1 Bomb 0 Nothing))
                (1, 2)
                (Gem C2 LineH 0 Nothing))
             (1, 3)
             (Gem C3 LineV 0 Nothing))
          (1, 4)
          (Gem C4 Rainbow 0 Nothing)
      gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 20
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
      gs1 = shuffleGame gs0
      b1 = gsBoard gs1
  assertEqual "bomb kept" (Just Bomb) (cellKind (getCell b1 (1, 1)))
  assertEqual "bomb color" (Just C1) (cellColor (getCell b1 (1, 1)))
  assertEqual "lineH kept" (Just LineH) (cellKind (getCell b1 (1, 2)))
  assertEqual "lineV kept" (Just LineV) (cellKind (getCell b1 (1, 3)))
  assertEqual "rainbow kept" (Just Rainbow) (cellKind (getCell b1 (1, 4)))
  -- Countdown still kept (pre-existing decor path)
  let boardCd = setCell board (2, 2) (mkCountdown C5 4)
      gsCd = shuffleGame (gs0 { gsBoard = boardCd })
  assertBool "countdown kept" (isCountdown (getCell (gsBoard gsCd) (2, 2)))
  assertEqual "countdown turns" (4 :: Int) (countdownTurns (getCell (gsBoard gsCd) (2, 2)))

-- | 普通匹配连锁：cascadeMatches 的逐轮回放（crWaves）与结算计数（crTally）一致——各轮得分之和、
-- 清除格并集、波数，终盘稳定且逐轮首尾相接（元组 API 已删，两个投影直接取同一 CascadeRun 的字段）。
-- 含飞碟与传送门的情形一起覆盖。
trace_cascade_final_equals_stabilized :: Assertion
trace_cascade_final_equals_stabilized = do
  let cases =
        [ (seed, ufos, portals)
        | seed <- [1 .. 120 :: Int]
        , (ufos, portals) <-
            [ ([], [])
            , ([mkUfo (2, 3) C1], [])
            , ([], [((0, 1), (7, 6)), ((0, 6), (7, 1))])
            ]
        ]
  multi <- fmap sum $ mapM
    ( \(seed, ufos, portals) -> do
        let g = mkStdGen seed
            (b0, g1) = randomBoard g
            CascadeRun {crBoard = bR, crTally = CascadeTally {ctScore = score, ctMaxWave = maxW, ctCleared = clearedR}, crUfos = ufosR, crGen = gR} = cascadeMatches Nothing ufos portals g1 b0
            CascadeRun {crWaves = ws, crBoard = bT, crUfos = ufosT, crGen = gT} = cascadeMatches Nothing ufos portals g1 b0
            tag = "seed " ++ show seed
        bT @?= bR
        show gT @?= show gR
        ufosT @?= ufosR
        assertEqual (tag ++ ": wave scores sum") score (sum (map cwScore ws))
        assertEqual (tag ++ ": cleared union") (sort (nub clearedR)) (sort (nub (concatMap (\w -> cwCleared w ++ cwDrained w) ws)))
        assertEqual (tag ++ ": wave count = max combo wave") maxW (length ws)
        assertBool (tag ++ ": final is stable") (not (hasAnyMatch bT))
        checkWaveChain tag b0 ws bT
        pure (if length ws >= 3 then 1 else 0 :: Int)
    )
    cases
  assertBool "sample includes multi-wave cascades" (multi > 0)

-- | 种子清除（彩虹 / 特殊组合 / 道具 / 倒计时爆炸）起手的连锁同样一致。
trace_seeds_final_equals_stabilized :: Assertion
trace_seeds_final_equals_stabilized =
  mapM_
    ( \(seed, seeds, ufos) -> do
        let (b0, g1) = randomPlayableBoard (mkStdGen seed)
            CascadeRun {crBoard = bR, crTally = CascadeTally {ctScore = score, ctCleared = clearedR}, crUfos = ufosR, crGen = gR} = cascadeSeeds Nothing seeds ufos [] g1 b0
            CascadeRun {crWaves = ws, crBoard = bT, crUfos = ufosT, crGen = gT} = cascadeSeeds Nothing seeds ufos [] g1 b0
            tag = "seed " ++ show seed
        bT @?= bR
        show gT @?= show gR
        ufosT @?= ufosR
        assertEqual (tag ++ ": wave scores sum") score (sum (map cwScore ws))
        assertEqual (tag ++ ": cleared union") (sort (nub clearedR)) (sort (nub (concatMap (\w -> cwCleared w ++ cwDrained w) ws)))
        checkWaveChain tag b0 ws bT
    )
    [ (seed, seeds, ufos)
    | seed <- [1 .. 40 :: Int]
    , seeds <- [[(3, 3)], crossClearSeeds (seed `mod` boardSize, (seed * 3) `mod` boardSize)]
    , ufos <- [[], [mkUfo (1, 3) C2]]
    ]

-- | 全部相邻交换：traceSwap 的最终盘面与 trySwap 结算后的盘面一致（自动洗牌除外，
-- 那时比较洗牌前不可见），逐轮得分之和 = 本步得分，清除格并集 = gsLastCleared，
-- 有清除的轮数 = gsCombo。覆盖皮带 / 倒计时 / 飞碟 / 传送门 / 蜗牛 / 蔓延等关卡。
trace_swap_final_equals_trySwap :: Assertion
trace_swap_final_equals_trySwap = do
  let levels = [0, 4, 7, 9, 10, 11, 12, 13, 15, 21, 26, 27, 28, 32, 33, 35, 36]
      results =
        [ (li, seed, (p1, p2), gs0, gs1, out)
        | li <- levels
        , seed <- [1 .. 3 :: Int]
        , let lvl = allLevels !! li
              gs0 = newGameAtLevel li (levelConfig lvl) seed
        , r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p1 = (r, c)
        , p2 <- [(r, c + 1), (r + 1, c)]
        , inBounds p2
        , let (gs1, out) = trySwap p1 p2 gs0
        ]
  counts <- mapM
    ( \(li, seed, (p1, p2), gs0, gs1, out) -> do
        let mt = traceSwap p1 p2 gs0
            ws = mtWaves mt
            tag = "L" ++ show (li + 1) ++ " seed " ++ show seed ++ " " ++ show (p1, p2)
        case out of
          NoMatch -> do
            assertBool (tag ++ ": rejected swap has no waves") (null ws)
            pure (0 :: Int, 0 :: Int)
          InvalidSwap -> do
            assertBool (tag ++ ": invalid swap has no waves") (null ws)
            pure (0, 0)
          _ -> do
            assertEqual (tag ++ ": start = swapped board") (swapCells (gsBoard gs0) p1 p2) (mtStart mt)
            -- 自动洗牌会换掉整盘，只能跳过终盘对比；跳过数单独统计（见下方底线断言）
            when (not (gsShuffled gs1)) $
              assertEqual (tag ++ ": final board") (gsBoard gs1) (mtFinal mt)
            assertEqual (tag ++ ": score") (gsScore gs1 - gsScore gs0) (sum (map cwScore ws))
            assertEqual (tag ++ ": cleared union") (sort (nub (gsLastCleared gs1))) (sort (nub (concatMap (\w -> cwCleared w ++ cwDrained w) ws)))
            assertEqual (tag ++ ": combo") (gsCombo gs1) (nonEmptyWaves ws)
            pure (if gsShuffled gs1 then (0, 1) else (1, 0))
    )
    results
  let compared = sum (map fst counts)
      skipped = sum (map snd counts)
  assertBool
    ("enough non-shuffled applied swaps compared on final board (compared " ++ show compared ++ ", skipped for auto-shuffle " ++ show skipped ++ ")")
    (compared > 400)

-- | 道具（锤子 / 十字 / 自由交换）的回放终局与结算结果一致。
trace_boosters_final_equal_result :: Assertion
trace_boosters_final_equal_result =
  mapM_
    ( \(li, seed) -> do
        let lvl = allLevels !! li
            gs0 = newGameAtLevel li (levelConfig lvl) seed
            check tag gs1 out mt = case out of
              MoveApplied _ -> do
                when (not (gsShuffled gs1)) $ assertEqual (tag ++ ": final") (gsBoard gs1) (mtFinal mt)
                assertEqual (tag ++ ": score") (gsScore gs1 - gsScore gs0) (sum (map cwScore (mtWaves mt)))
                assertEqual (tag ++ ": combo") (gsCombo gs1) (nonEmptyWaves (mtWaves mt))
              NoMatch -> assertBool (tag ++ ": no waves") (null (mtWaves mt))
              InvalidSwap -> assertBool (tag ++ ": no waves") (null (mtWaves mt))
              _ -> do
                -- 道具直接终局（过关 / 胜 / 负）：盘面、得分、连击都要与回放一致
                assertEqual (tag ++ ": final") (gsBoard gs1) (mtFinal mt)
                assertEqual (tag ++ ": score (terminal)") (gsScore gs1 - gsScore gs0) (sum (map cwScore (mtWaves mt)))
                assertEqual (tag ++ ": combo (terminal)") (gsCombo gs1) (nonEmptyWaves (mtWaves mt))
            tag0 = "L" ++ show (li + 1) ++ " seed " ++ show seed
        sequence_
          [ do
              let (gsH, outH) = useHammer p gs0
              check (tag0 ++ " hammer " ++ show p) gsH outH (traceHammer p gs0)
              let (gsX, outX) = useCrossClear p gs0
              check (tag0 ++ " cross " ++ show p) gsX outX (traceCrossClear p gs0)
          | p <- [(0, 0), (3, 4), (7, 7), (5, 2)]
          ]
        sequence_
          [ do
              let (gsF, outF) = useFreeSwap p1 p2 gs0
              check (tag0 ++ " free " ++ show (p1, p2)) gsF outF (traceFreeSwap p1 p2 gs0)
          | (p1, p2) <- [((0, 0), (7, 7)), ((2, 3), (5, 6)), ((4, 4), (4, 5))]
          ]
    )
    [ (li, seed) | li <- [0, 12, 21, 26, 27, 32], seed <- [1 .. 3 :: Int] ]

-- | 被拒的操作（无匹配 / 非相邻 / 已结束 / 道具无效）回放脚本为空：前端不会播任何一轮。
trace_rejected_move_is_empty :: Assertion
trace_rejected_move_is_empty = withComboState $ \_ gs1 -> do
  case noMatchSwap gs1 of
    Nothing -> assertFailure "need a no-match swap"
    Just (p1, p2) -> assertBool "no-match swap has no waves" (null (mtWaves (traceSwap p1 p2 gs1)))
  assertBool "non-adjacent has no waves" (null (mtWaves (traceSwap (0, 0) (2, 2) gs1)))
  let gsOverSt = gs1 { gsOver = Just (Won (gsScore gs1)) }
  case findHint (gsBoard gsOverSt) of
    Nothing -> assertFailure "need a hint"
    Just (p1, p2) -> assertBool "finished game has no waves" (null (mtWaves (traceSwap p1 p2 gsOverSt)))
  assertBool "no hammer charges -> no waves" (null (mtWaves (traceHammer (3, 3) gs1 { gsHammers = 0 })))
  assertBool "no cross charges -> no waves" (null (mtWaves (traceCrossClear (3, 3) gs1 { gsCrossClears = 0 })))
  -- 被拒的操作也没有步末效果（不播蔓延 / 蜗牛）
  assertBool "non-adjacent has no end steps" (null (mtEnd (traceSwap (0, 0) (2, 2) gs1)))
  assertBool "no hammer charges -> no end steps" (null (mtEnd (traceHammer (3, 3) gs1 { gsHammers = 0 })))
  case noMatchSwap gs1 of
    Nothing -> assertFailure "need a no-match swap"
    Just (p1, p2) -> assertBool "no-match swap has no end steps" (null (mtEnd (traceSwap p1 p2 gs1)))
  -- 带巧克力的关卡里无匹配交换同样不蔓延
  let gsC = newGameAtLevel 4 (levelConfig (allLevels !! 4)) 1
  case findNoMatchPair (gsBoard gsC) of
    Nothing -> assertFailure "need a no-match pair on choco level"
    Just (p1, p2) -> do
      snd (trySwap p1 p2 gsC) @?= NoMatch
      mtEnd (traceSwap p1 p2 gsC) @?= []

-- | 3 连及以上的一步：每一轮都有自己的被消格，且被消格在该轮之前的盘面上确实存在。
trace_multi_wave_each_round_visible :: Assertion
trace_multi_wave_each_round_visible =
  case
    [ (gs0, p1, p2, gs1)
    | seed <- [1 .. 400 :: Int]
    , let gs0 = newGameAtLevel 0 (levelConfig firstLevel) seed
    , Just (p1, p2) <- [findHint (gsBoard gs0)]
    , let (gs1, out) = trySwap p1 p2 gs0
    , out /= NoMatch
    , gsCombo gs1 >= 3
    ] of
    [] -> assertFailure "need a 3+ cascade from a hinted swap"
    ((gs0, p1, p2, gs1) : _) -> do
      let ws = mtWaves (traceSwap p1 p2 gs0)
      length ws @?= gsCombo gs1
      assertBool "every round clears something" (all (not . null . cwCleared) ws)
      sequence_
        [ assertBool ("round " ++ show i ++ " clears real cells") (all (\p -> inBounds p) (cwCleared w))
        | (i, w) <- zip [1 :: Int ..] ws
        ]
      -- 每一轮都能从「消除前盘面」上找到匹配（第一轮之后都是天然掉落形成的连锁）
      assertBool "later rounds start from a matching board" (all (hasAnyMatch . cwBefore) (drop 1 ws))
      -- 空洞盘面在被消格上确实是空的（除非放下了新特殊块）
      sequence_
        [ assertBool "holes at cleared cells"
            (all (\(r, c) -> case atM (cwHoles w) (r, c) of
                                Nothing -> True
                                Just cell -> isGem cell) (cwCleared w))
        | w <- ws
        ]

-- | 步末效果（倒计时减一 / 皮带移位 / 藤巧蒸汽蔓延 / 蜗牛爬行）按时间线重放后与 trySwap 的终盘一致；
-- 抽样必须覆盖所有种类，保证测试有效。
trace_end_steps_replay_to_trySwap_final :: Assertion
trace_end_steps_replay_to_trySwap_final = do
  names <- fmap concat $ sequence
    [ do
        let tag = "L" ++ show (li + 1) ++ " seed " ++ show seed ++ " " ++ show (p1, p2)
            mt = traceSwap p1 p2 gs0
        ks <- replayTimeline tag mt
        when (not (gsShuffled gs1)) $ assertEqual (tag ++ ": replayed final = trySwap board") (gsBoard gs1) (mtFinal mt)
        pure ks
    | li <- [0 .. length allLevels - 1]
    , seed <- [1 .. 3 :: Int]
    , let gs0 = newGameAtLevel li (levelConfig (allLevels !! li)) seed
    , r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , let p1 = (r, c)
    , p2 <- [(r, c + 1), (r + 1, c)]
    , inBounds p2
    , let (gs1, out) = trySwap p1 p2 gs0
    , out /= NoMatch && out /= InvalidSwap
    ]
  sequence_
    [ assertBool ("sample covers end effect " ++ k ++ " (seen " ++ show (length (filter (== k) names)) ++ ")") (k `elem` names)
    | k <- ["tick", "belt", "SpreadVine", "SpreadChoco", "SpreadSteam", "snail"]
    ]

-- | 道具（锤子 / 十字 / 自由交换）只有蔓延类步末效果，重放后同样到达终盘。
trace_end_steps_boosters_replay :: Assertion
trace_end_steps_boosters_replay = do
  names <- fmap concat $ sequence
    [ do
        let tag = "L" ++ show (li + 1) ++ " seed " ++ show seed ++ " " ++ name
        ks <- replayTimeline tag mt
        case out of
          MoveApplied _ | not (gsShuffled gs1) -> assertEqual (tag ++ ": final") (gsBoard gs1) (mtFinal mt)
          _ -> pure ()
        assertBool (tag ++ ": boosters only spread") (all (`elem` ["SpreadVine", "SpreadChoco", "SpreadSteam"]) ks)
        pure ks
    | li <- [4, 9, 15, 27, 35]
    , seed <- [1 .. 2 :: Int]
    , let gs0 = newGameAtLevel li (levelConfig (allLevels !! li)) seed
    , (name, (gs1, out), mt) <-
        [ ("hammer " ++ show p, useHammer p gs0, traceHammer p gs0) | p <- [(0, 0), (3, 4), (5, 2)] ]
          ++ [ ("cross " ++ show p, useCrossClear p gs0, traceCrossClear p gs0) | p <- [(2, 2), (6, 5)] ]
          ++ [ ("free " ++ show pq, useFreeSwap (fst pq) (snd pq) gs0, traceFreeSwap (fst pq) (snd pq) gs0) | pq <- [((0, 0), (7, 7)), ((4, 4), (4, 5))] ]
    ]
  assertBool "booster sample includes a spread" (not (null names))

-- | 蜗牛：碰壁原地掉头（smFrom == smTo，朝向反转），前方是宝石则爬过去、宝石换到原格。
trace_end_snail_push_and_turn :: Assertion
trace_end_snail_push_and_turn = do
  let base = newGame defaultConfig 7
      b0 = setCell (setCell (gsBoard base) (0, 0) (mkSnail 0 (-1))) (3, 3) (mkSnail 0 1)
      gs0 = base { gsBoard = b0 }
      applied =
        [ (p1, p2, gs1)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p1 = (r, c)
        , p2 <- [(r, c + 1), (r + 1, c)]
        , inBounds p2
        , all (`notElem` [(0, 0), (3, 3), (3, 4)]) [p1, p2]
        , let (gs1, out) = trySwap p1 p2 gs0
        , isMoveApplied out
        ]
  case applied of
    [] -> assertFailure "need an applied swap away from the snails"
    ((p1, p2, gs1) : _) -> do
      let mt = traceSwap p1 p2 gs0
      _ <- replayTimeline "snail" mt
      case [(e, ms) | e <- mtEnd mt, EndSnail ms <- [esEffect e]] of
        [(e, ms)] -> do
          assertBool "wall snail turns in place" (SnailMove (0, 0) (0, 0) (0, 1) Nothing `elem` ms)
          case [m | m <- ms, smFrom m == (3, 3)] of
            [m] -> do
              smTo m @?= (3, 4)
              smDir m @?= (0, 1)
              smPushed m @?= Just (getCell (esBefore e) (3, 4))
            other -> assertFailure ("expected one move for snail at (3,3), got " ++ show other)
        other -> assertFailure ("expected exactly one snail end step, got " ++ show (length other))
      when (not (gsShuffled gs1)) $ gsBoard gs1 @?= mtFinal mt
  where
    isMoveApplied o = case o of
      MoveApplied _ -> True
      _ -> False

-- | 巧克力 / 藤蔓：新占格都能在之前的盘面找到正交相邻的来源（前端从来源方向「长出」）。
trace_end_spread_from_adjacent_source :: Assertion
trace_end_spread_from_adjacent_source = do
  let found =
        [ (kind, pairs)
        | li <- [4, 9]
        , seed <- [1 .. 3 :: Int]
        , let gs0 = newGameAtLevel li (levelConfig (allLevels !! li)) seed
        , Just (p1, p2) <- [findHint (gsBoard gs0)]
        , e <- mtEnd (traceSwap p1 p2 gs0)
        , EndSpread kind pairs <- [esEffect e]
        ]
  assertBool "choco spread seen" (SpreadChoco `elem` map fst found)
  assertBool "vine spread seen" (SpreadVine `elem` map fst found)
  assertBool "every spread has targets" (all (not . null . snd) found)

-- | 补上「自动洗牌步」的逐帧比对缺口（第二刀：MoveTrace 新增 mtGen / mtShuffle）。
-- 全部 38 关 × 种子 1–3 × 全部相邻交换（开局状态）+ 每个成交交换之后再走 2 手：每个成交步都按
-- 「轮 → 步末 → 轮」时间线重放到 mtFinal，再从 (mtFinal, mtGen) 重放 ensurePlayable，必须到达结算后的
-- gsBoard / gsGen（没洗牌时 mtFinal == gsBoard、mtGen == gsGen，mtShuffle == Nothing）。
-- 底线：逐帧比对的成交步 > 3000，其中洗牌步 > 20（当前 3801 / 29；失败信息打印实际数）。
trace_shuffle_step_replays :: Assertion
trace_shuffle_step_replays = do
  let pairs =
        [ ((r, c), p2)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , p2 <- [(r, c + 1), (r + 1, c)]
        , inBounds p2
        ]
      firstApplied gs = [ (p1, p2, g) | (p1, p2) <- pairs, let (g, o) = trySwap p1 p2 gs, o /= NoMatch && o /= InvalidSwap ]
      -- 开局的全部成交交换，外加每条之后沿「第一手成交」再走两手
      chains gs0 =
        concat
          [ (gs0, p1, p2, gs1) : follow (2 :: Int) gs1
          | (p1, p2, gs1) <- firstApplied gs0
          ]
      follow 0 _ = []
      follow n gs
        | isJust (gsOver gs) = []
        | otherwise = case firstApplied gs of
            ((p1, p2, g) : _) -> (gs, p1, p2, g) : follow (n - 1) g
            [] -> []
      cases =
        [ (li, seed, st)
        | li <- [0 .. length allLevels - 1]
        , seed <- [1 .. 3 :: Int]
        , st <- chains (newGameAtLevel li (levelConfig (allLevels !! li)) seed)
        ]
  counts <- mapM
    ( \(li, seed, (gs0, p1, p2, gs1)) -> do
        let mt = traceSwap p1 p2 gs0
            tag = "L" ++ show (li + 1) ++ " seed " ++ show seed ++ " " ++ show (p1, p2)
        _ <- replayTimeline tag mt
        let replay = ensurePlayable gs1 {gsBoard = mtFinal mt, gsGen = mtGen mt, gsShuffled = False}
        if gsShuffled gs1
          then do
            assertEqual (tag ++ ": mtShuffle = settled board") (Just (gsBoard gs1)) (mtShuffle mt)
            assertEqual (tag ++ ": shuffle replays from (mtFinal, mtGen)") (gsBoard gs1) (gsBoard replay)
            assertEqual (tag ++ ": shuffle generator") (show (gsGen gs1)) (show (gsGen replay))
            pure (1 :: Int, 1 :: Int)
          else do
            assertEqual (tag ++ ": final board") (gsBoard gs1) (mtFinal mt)
            assertEqual (tag ++ ": mtGen = settled generator") (show (gsGen gs1)) (show (mtGen mt))
            assertEqual (tag ++ ": no shuffle recorded") Nothing (mtShuffle mt)
            pure (1, 0)
    )
    cases
  let compared = sum (map fst counts)
      shuffled = sum (map snd counts)
  assertBool
    ("frame-compared applied swaps " ++ show compared ++ ", of which auto-shuffle steps " ++ show shuffled)
    (compared > 3000 && shuffled > 20)
