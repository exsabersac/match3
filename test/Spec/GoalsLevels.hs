{-# LANGUAGE ScopedTypeVariables #-}

-- | 目标、结局与关卡：各类目标计数、胜负判定、星级、战役关卡表、每日挑战、地图选关与步数结转。
module Spec.GoalsLevels
  ( tests
  , outcome_moves_or_score
  ) where

import Data.List (nub)
import Match3.Board.Default (findHint, hasAnyMatch, hasValidMove)
import Match3.Board.Grid (setCell)
import Match3.Core
import Match3.Daily (dailyConfig)
import Match3.Game.Level (newGame)
import Match3.Game.Move (trySwap)
import Match3.Game.Outcome (unlockAfterClear)
import Match3.Game.State (gsCarpetOpen, gsCollected, gsCount, gsGoalMet)
import Match3.Types
  ( goalCollect
  , goalColors
  , goalCount
  , goalMet
  , goalProgress
  , goalScore
  , goalTarget
  , isBalloon
  , isCake
  , isChest
  , isCookie
  , isFlip
  , isHoney
  , isMagicHat
  , isSafe
  , isStone
  , mkStone
  , terminalOf
  , validBoardDim
  )
import Test.Tasty
import Test.Tasty.HUnit
import Match3.Counts (countsFromList, noCounts, singleCount)
import Spec.Support
import Match3.Levels.Level (DropSpec(..))

-- | 本模块的测试（平铺进顶层 "match3" 组）。
tests :: [TestTree]
tests =
  [ testCase "outcome_moves_or_score" outcome_moves_or_score
  , testCase "collect_goal_progress" collect_goal_progress
  , testCase "collect_goal_clears_level" collect_goal_clears_level
  , testCase "collect_goal_lose_on_moves" collect_goal_lose_on_moves
  , testCase "level_table_mixes_collect" level_table_mixes_collect
  , testCase "score_goal_ignores_collect" score_goal_ignores_collect
  , testCase "goal_collect_multi_color" goal_collect_multi_color
  , testCase "goal_clear_stone_counts" goal_clear_stone_counts
  , testCase "daily_seed_stable" daily_seed_stable
  , testCase "star_rating_tiers" star_rating_tiers
  , testCase "lose_hint_by_goal" lose_hint_by_goal
  , testCase "goal_chest_counts" goal_chest_counts
  , testCase "goal_honey_counts" goal_honey_counts
  , testCase "goal_balloon_counts" goal_balloon_counts
  , testCase "goal_cookie_counts" goal_cookie_counts
  , testCase "goal_cake_counts" goal_cake_counts
  , testCase "goal_safe_counts" goal_safe_counts
  , testCase "daily_ufo_goal_spawns_saucer" daily_ufo_goal_spawns_saucer
  , testCase "goal_carpet_counts" goal_carpet_counts
  , testCase "carry_moves_on_next_level" carry_moves_on_next_level
  , testCase "daily_goal_rotates_ten" daily_goal_rotates_ten
  , testCase "campaign_levels_batch_ok" campaign_levels_batch_ok
  , testCase "finale_and_pressure_moves_reasonable" finale_and_pressure_moves_reasonable
  , testCase "daily_obstacle_goal_spawns_decor" daily_obstacle_goal_spawns_decor
  , testCase "map_select_no_carry_moves" map_select_no_carry_moves
  , testCase "star_rating_vs_carry_base" star_rating_vs_carry_base
  , testCase "unlock_after_clear_bumps_map" unlock_after_clear_bumps_map
  , testCase "map_click_same_level_resumes" map_click_same_level_resumes
  , testCase "goal_carpet_seeds_open_tiles" goal_carpet_seeds_open_tiles
  , testCase "daily_clear_is_won_not_levelclear" daily_clear_is_won_not_levelclear
  , testCase "daily_won_does_not_unlock_map" daily_won_does_not_unlock_map
  , testCase "find_match_pair_engine_accepts" find_match_pair_engine_accepts
  , testCase "terminal_outcome_mapping" terminal_outcome_mapping
  ]

-- | 回归（测试辅助 'findMatchPair' 的缺陷）：它选出的对引擎必须接受。逐关（全部战役关卡，含第 43 关毛球）
-- 用 outcome_moves_or_score 的两种开局（'newGameAtLevel' 配置 5 步 / 1 分、种子 42）与 'campaignGame' 种子 1–3：
-- 选出的对交给 'trySwap' 不是 NoMatch / InvalidSwap；并且旧版（不查能否交换）在这些开局里
-- 至少一次选中了挡交换的对，说明本用例确实覆盖到了那个缺陷。
find_match_pair_engine_accepts :: Assertion
find_match_pair_engine_accepts = do
  let cfgW = GameConfig { cfgMoves = 5, cfgGoal = goalScore 1 }
      starts =
        [ ("level " ++ show (i + 1) ++ " cfgW seed 42", newGameAtLevel i cfgW 42) | i <- [0 .. length allLevels - 1] ]
          ++ [ ("level " ++ show (i + 1) ++ " seed " ++ show s, levelGame i s) | i <- [0 .. length allLevels - 1], s <- [1, 2, 3] ]
      rejected gs (p1, p2) = case snd (trySwap p1 p2 gs) of
        NoMatch -> True
        InvalidSwap -> True
        _ -> False
  mapM_
    ( \(lbl, gs) -> case findMatchPair (gsBoard gs) of
        Nothing -> assertFailure (lbl ++ ": no match pair")
        Just (p1, p2) -> do
          assertBool (lbl ++ ": engine rejected " ++ show (p1, p2) ++ " (" ++ show (snd (trySwap p1 p2 gs)) ++ ")") (not (rejected gs (p1, p2)))
    )
    starts
  assertBool
    "naive finder picks a pair the engine rejects on some start (defect is covered)"
    (or [maybe False (rejected gs) (findMatchPairNaive (gsBoard gs)) | (_, gs) <- starts])

-- | 'Terminal' 与 'Outcome' 的对应：terminalOf 只对 Won / Lost / LevelClear 给 Just，
-- 构造器与参数一一对应、来回换不丢信息；GameState 的 Show 里 gsOver 仍按 Outcome 打印（金标准、指纹依赖这段文本）。
terminal_outcome_mapping :: Assertion
terminal_outcome_mapping = do
  let pairs = [(Won 1234, TWon 1234), (Lost 7, TLost 7), (LevelClear 99 4, TLevelClear 99 4), (LevelClear 0 0, TLevelClear 0 0)]
  mapM_ (\(o, t) -> do
           assertEqual ("terminalOf " ++ show o) (Just t) (terminalOf o)
           assertEqual ("fromTerminal " ++ show t) o (fromTerminal t)
           assertEqual ("round trip " ++ show t) (Just t) (terminalOf (fromTerminal t)))
        pairs
  mapM_ (\o -> assertEqual ("non-terminal " ++ show o) Nothing (terminalOf o)) [InvalidSwap, NoMatch, MoveApplied 0, MoveApplied 5]
  let g0 = newGame defaultConfig 42
      shown t = show (g0 {gsOver = t})
      has needle hay = any (\i -> take (length needle) (drop i hay) == needle) [0 .. length hay - length needle]
  assertBool "Show: gsOver = Nothing" (has "gsOver = Nothing," (shown Nothing))
  assertBool "Show: TWon prints as Won" (has "gsOver = Just (Won 1234)," (shown (Just (TWon 1234))))
  assertBool "Show: TLost prints as Lost" (has "gsOver = Just (Lost 7)," (shown (Just (TLost 7))))
  assertBool "Show: TLevelClear prints as LevelClear" (has "gsOver = Just (LevelClear 99 4)," (shown (Just (TLevelClear 99 4))))

outcome_moves_or_score :: Assertion
outcome_moves_or_score = do
  let gs0 = newGame defaultConfig 42
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "have mover"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      case out of
        MoveApplied gained -> do
          assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)
          assertEqual "score" (gsScore gs0 + gained) (gsScore gs1)
          assertBool "gained > 0" (gained > 0)
        Won _ -> assertBool "won" (gsGoalMet gs1)
        LevelClear _ _ -> assertBool "level" (gsGoalMet gs1)
        Lost _ -> gsMoves gs1 @?= 0
        other -> assertFailure ("unexpected: " ++ show other)

  let cfgW = GameConfig { cfgMoves = 5, cfgGoal = goalScore 1 }
      gsW0 = newGameAtLevel (length allLevels - 1) cfgW 42
  case findMatchPair (gsBoard gsW0) of
    Nothing -> assertFailure "win mover"
    Just (p1, p2) -> do
      let (_, outW) = trySwap p1 p2 gsW0
      case outW of
        Won _ -> pure ()
        other -> assertFailure ("expected Won on last level, got " ++ show other)

  let cfgL = GameConfig { cfgMoves = 1, cfgGoal = goalScore 999999 }
      gsL0 = newGame cfgL 42
  case findMatchPair (gsBoard gsL0) of
    Nothing -> assertFailure "lose mover"
    Just (p1, p2) -> do
      let (gsL1, outL) = trySwap p1 p2 gsL0
      case outL of
        Lost _ -> gsMoves gsL1 @?= 0
        Won _ -> assertFailure "should not win"
        LevelClear _ _ -> assertFailure "should not clear"
        other -> assertFailure ("expected Lost, got " ++ show other)

--------------------------------------------------------------------------------
-- Color-collect goals
--------------------------------------------------------------------------------

-- | Clearing gems of the target color increments gsCollected.
collect_goal_progress :: Assertion
collect_goal_progress = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = goalCollect C1 100 }
      -- Build a board with a clearable C1 triple at row 3, rest C5 (won't make C1 match elsewhere)
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      -- Place C1 C1 C2 and an adjacent C1 so swap creates three C1
      -- row3: C1 C1 C2 C3 C4 C5 C2 C3  — swap (3,2)=C2 with (3,1) wouldn't help
      -- Better: put C1 at (3,0)(3,1)(3,3) and C2 at (3,2); swap (3,2)<->something...
      -- Simpler: board already has match of three C1 — but then newGame uses random board.
      -- Override board after newGame, then force a matching swap.
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = boardFromRows $ take 3 b0 ++ [row3] ++ drop 4 b0
      -- Swap (3,2)=C2 with (3,3)=C1 → row becomes C1 C1 C1 C2 ... match!
      gs0 =
        (newGame cfg 55)
          { gsBoard = board
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    MoveApplied _ -> pure ()
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("expected applied/terminal, got " ++ show other)
  assertBool
    ("collected C1 increased, got " ++ show (gsCollected gs1))
    (gsCollected gs1 >= 3)

-- | Reaching collect count triggers LevelClear (or Won on last level).
collect_goal_clears_level :: Assertion
collect_goal_clears_level = do
  let cfg = GameConfig { cfgMoves = 10, cfgGoal = goalCollect C1 3 }
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = boardFromRows $ take 3 b0 ++ [row3] ++ drop 4 b0
      -- Level 0 so LevelClear (not Won)
      gs0 =
        (newGameAtLevel 0 cfg 55)
          { gsBoard = board
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    LevelClear _ next -> do
      assertEqual "next level" (1 :: Int) next
      assertBool "collected enough" (gsCollected gs1 >= 3)
      assertBool "gsOver set" (gsOver gs1 == terminalOf out)
    Won _ -> assertFailure "should LevelClear on non-last level"
    other -> assertFailure ("expected LevelClear, got " ++ show other ++ " collected=" ++ show (gsCollected gs1))


-- | One legal clear with moves=1 but GoalCollect unmet → Lost; collected < target.
collect_goal_lose_on_moves :: Assertion
collect_goal_lose_on_moves = do
  let cfg = GameConfig { cfgMoves = 1, cfgGoal = goalCollect C1 99 }
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = boardFromRows $ take 3 b0 ++ [row3] ++ drop 4 b0
      gs0 =
        (newGameAtLevel 0 cfg 55)
          { gsBoard = board
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    Lost s -> do
      assertBool "score non-negative" (s >= 0)
      assertBool
        ("collected < 99, got " ++ show (gsCollected gs1))
        (gsCollected gs1 < 99)
      assertBool "gsOver is Lost" (gsOver gs1 == terminalOf out)
      assertEqual "moves spent" (0 :: Int) (gsMoves gs1)
    other ->
      assertFailure
        ("expected Lost, got " ++ show other
           ++ " collected=" ++ show (gsCollected gs1)
           ++ " moves=" ++ show (gsMoves gs1))

-- | Campaign table mixes GoalScore and GoalCollect stages.
level_table_mixes_collect :: Assertion
level_table_mixes_collect = do
  let goals = map lvlGoal allLevels
      scores = [g | g <- goals, ViewScore _ <- [goalView g]]
      collects = [g | g <- goals, ViewCollect _ _ <- [goalView g]]
  assertBool "has score levels" (not (null scores))
  assertBool "has collect levels" (not (null collects))
  assertBool "at least 5 levels" (length allLevels >= 5)
  let names = map lvlName allLevels
  assertBool "has 采红" ("采红" `elem` names)
  assertBool "has 采蓝" ("采蓝" `elem` names)
  assertBool "has 冰绿" ("冰绿" `elem` names)
  assertBool "has 双采" ("双采" `elem` names)
  assertBool "has 碎石" ("碎石" `elem` names)
  assertBool "has multi goal"
    (any (\g -> case goalView g of ViewCollectMulti _ -> True; _ -> False) goals)
  assertBool "has clear-stone goal"
    (any (\g -> case goalView g of ViewCount CountStones _ -> True; _ -> False) goals)
  assertBool "has 飞碟" ("飞碟" `elem` names)
  assertBool "has ufo goal"
    (any (\g -> case goalView g of ViewCount CountUfo _ -> True; _ -> False) goals)

-- | Score-goal levels do not increment gsCollected (stays 0).
score_goal_ignores_collect :: Assertion
score_goal_ignores_collect = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = goalScore 99999 }
      gs0 = newGame cfg 42
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need move"
    Just (p1, p2) -> do
      let (gs1, _) = trySwap p1 p2 gs0
      gsCollected gs1 @?= 0

--------------------------------------------------------------------------------
-- Multi goals (开心消消乐-style diverse targets)
--------------------------------------------------------------------------------

-- | GoalCollectMulti requires quotas for every listed color.
goal_collect_multi_color :: Assertion
goal_collect_multi_color = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = goalColors [(C1, 3), (C2, 1)] }
      board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      -- Swap (3,2)<->(3,3): row becomes C1 C1 C1 C2 — clears 3×C1
      gs0 =
        (newGameAtLevel 0 cfg 5)
          { gsBoard = board0
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  assertBool "C1 tallied" (gsCount (CountColor C1) gs1 >= 3)
  -- Not yet clear: still need C2 quota unless cascade luck
  case out of
    LevelClear _ _ ->
      assertBool "if cleared, both quotas met" $
        gsGoalMet gs1
    MoveApplied _ ->
      assertBool "multi goal not met with only C1" $
        not (goalMet (goalColors [(C1, 3), (C2, 1)]) 0 (gsCounts gs1))
          || gsCount (CountColor C2) gs1 >= 1
    _ -> pure ()
  -- Direct unit: goalMet logic（多色配额）
  assertBool "both met"
    (goalMet (goalColors [(C1, 2), (C3, 1)]) 0 (countsFromList [(CountColor C1, 2), (CountColor C3, 1)]))
  assertBool "missing color"
    (not (goalMet (goalColors [(C1, 2), (C3, 1)]) 0 (countsFromList [(CountColor C1, 5)])))

-- | GoalClearStone counts fully destroyed stones toward the goal.
goal_clear_stone_counts :: Assertion
goal_clear_stone_counts = do
  assertBool "0 stones unmet"
    (not (goalMet (goalCount CountStones 2) 0 noCounts))
  assertBool "2 stones met"
    (goalMet (goalCount CountStones 2) 0 (singleCount CountStones 2))
  assertEqual "goal target" (8 :: Int) (goalTarget (goalCount CountStones 8))
  let cfg = GameConfig { cfgMoves = 15, cfgGoal = goalCount CountStones 2 }
      boardN =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C2))
             (3, 3)
             (mkGem C1))
          (4, 1)
          mkStone
      gsN =
        (newGameAtLevel 0 cfg 5)
          { gsBoard = boardN
          , gsCounts = noCounts
          , gsOver = Nothing
          }
      (gsN1, outN) = trySwap (3, 2) (3, 3) gsN
  assertBool
    ("stonesCleared incremented, got " ++ show (gsCount CountStones gsN1))
    (gsCount CountStones gsN1 >= 1)
  case outN of
    LevelClear _ _ -> assertBool "enough stones" (gsCount CountStones gsN1 >= 2)
    MoveApplied _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("unexpected " ++ show other)

--------------------------------------------------------------------------------
-- Daily challenge + stars
--------------------------------------------------------------------------------

daily_seed_stable :: Assertion
daily_seed_stable = do
  assertEqual "seed" (20260929 :: Int) (dailySeed (Year 2026) (Month 9) (Day 29))
  assertEqual "same day same seed" (dailySeed (Year 2026) (Month 1) (Day 1)) (dailySeed (Year 2026) (Month 1) (Day 1))
  assertBool "diff day diff seed" (dailySeed (Year 2026) (Month 1) (Day 1) /= dailySeed (Year 2026) (Month 1) (Day 2))
  let lvl = dailyLevel (Year 2026) (Month 9) (Day 29)
  assertEqual "name" "每日" (lvlName lvl)
  assertBool "moves positive" (lvlMoves lvl > 0)
  let gs = newGameAtLevel 0 (dailyConfig (Year 2026) (Month 9) (Day 29)) (dailySeed (Year 2026) (Month 9) (Day 29))
  assertBool "playable daily board" (hasValidMove (gsBoard gs))
  assertBool "stable daily board" (not (hasAnyMatch (gsBoard gs)))

star_rating_tiers :: Assertion
star_rating_tiers = do
  assertEqual "3 star plenty" (3 :: Int) (starRating 30 20)
  assertEqual "3 star boundary 40%" (3 :: Int) (starRating 30 12)
  assertEqual "2 star just below 40%" (2 :: Int) (starRating 30 11)  -- 11/30 < 0.4
  assertEqual "2 star" (2 :: Int) (starRating 30 8)
  assertEqual "2 star boundary ~15%" (2 :: Int) (starRating 30 5)
  assertEqual "1 star just below 15%" (1 :: Int) (starRating 30 4)  -- 4/30 < 0.15
  assertEqual "1 star" (1 :: Int) (starRating 30 2)
  assertEqual "zero left" (1 :: Int) (starRating 30 0)
  assertEqual "zero start" (1 :: Int) (starRating 0 0)
  assertEqual "single move clutch 3★" (3 :: Int) (starRating 1 1)

lose_hint_by_goal :: Assertion
lose_hint_by_goal = do
  assertBool "score hint" (not (null (loseHint (goalScore 500))))
  assertBool "collect hint" (not (null (loseHint (goalCollect C1 20))))
  assertBool "multi hint" (not (null (loseHint (goalColors [(C1, 1)]))))
  assertBool "stone hint" (not (null (loseHint (goalCount CountStones 8))))
  assertBool "chest hint" (not (null (loseHint (goalCount CountChests 6))))
  assertBool "honey hint" (not (null (loseHint (goalCount CountHoney 6))))
  assertBool "balloon hint" (not (null (loseHint (goalCount CountBalloons 6))))
  assertBool "cookie hint" (not (null (loseHint (goalCount CountCookies 6))))
  assertBool "cake hint" (not (null (loseHint (goalCount CountCakes 6))))
  assertBool "safe hint" (not (null (loseHint (goalCount CountSafes 5))))
  assertBool "ufo hint" (not (null (loseHint (goalCount CountUfo 10))))
  assertBool "carpet hint" (not (null (loseHint (goalCount CountCarpets 8))))

goal_chest_counts :: Assertion
goal_chest_counts = do
  assertBool "unmet" (not (goalMet (goalCount CountChests 2) 0 noCounts))
  assertBool "met" (goalMet (goalCount CountChests 2) 0 (singleCount CountChests 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountChests 5) 0 (singleCount CountChests 2))
  assertEqual "target" (6 :: Int) (goalTarget (goalCount CountChests 6))
  assertBool
    "campaign has GoalChest"
    (any (\g -> case goalView g of ViewCount CountChests _ -> True; _ -> False) (map lvlGoal allLevels))
  -- Level 16 décor places chests
  let gs = levelGame 16 42
      nChests =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isChest (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor chests >= 6, got " ++ show nChests) (nChests >= 6)


goal_honey_counts :: Assertion
goal_honey_counts = do
  assertBool "unmet" (not (goalMet (goalCount CountHoney 2) 0 noCounts))
  assertBool "met" (goalMet (goalCount CountHoney 2) 0 (singleCount CountHoney 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountHoney 5) 0 (singleCount CountHoney 2))
  assertEqual "target" (6 :: Int) (goalTarget (goalCount CountHoney 6))
  assertBool
    "campaign has GoalHoney"
    (any (\g -> case goalView g of ViewCount CountHoney _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = levelGame 18 42
      nHoney =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isHoney (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor honey >= 6, got " ++ show nHoney) (nHoney >= 6)



goal_balloon_counts :: Assertion
goal_balloon_counts = do
  assertBool "unmet" (not (goalMet (goalCount CountBalloons 2) 0 noCounts))
  assertBool "met" (goalMet (goalCount CountBalloons 2) 0 (singleCount CountBalloons 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountBalloons 5) 0 (singleCount CountBalloons 2))
  assertEqual "target" (6 :: Int) (goalTarget (goalCount CountBalloons 6))
  assertBool
    "campaign has GoalBalloon"
    (any (\g -> case goalView g of ViewCount CountBalloons _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = levelGame 20 42
      nBal =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isBalloon (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor balloons >= 6, got " ++ show nBal) (nBal >= 6)


goal_cookie_counts :: Assertion
goal_cookie_counts = do
  assertBool "unmet" (not (goalMet (goalCount CountCookies 2) 0 noCounts))
  assertBool "met" (goalMet (goalCount CountCookies 2) 0 (singleCount CountCookies 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountCookies 5) 0 (singleCount CountCookies 2))
  assertEqual "target" (6 :: Int) (goalTarget (goalCount CountCookies 6))
  assertBool
    "campaign has GoalCookie"
    (any (\g -> case goalView g of ViewCount CountCookies _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = levelGame 21 42
      nCookie =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isCookie (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor cookies >= 6, got " ++ show nCookie) (nCookie >= 6)


goal_cake_counts :: Assertion
goal_cake_counts = do
  assertBool "unmet" (not (goalMet (goalCount CountCakes 2) 0 noCounts))
  assertBool "met" (goalMet (goalCount CountCakes 2) 0 (singleCount CountCakes 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountCakes 5) 0 (singleCount CountCakes 2))
  assertEqual "target" (6 :: Int) (goalTarget (goalCount CountCakes 6))
  assertBool
    "campaign has GoalCake"
    (any (\g -> case goalView g of ViewCount CountCakes _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = levelGame 23 42
      nCake =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isCake (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor cake >= 6, got " ++ show nCake) (nCake >= 6)
  let gsHat = levelGame 24 42
      nHat =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isMagicHat (getCell (gsBoard gsHat) (r, c))
          ]
  assertBool ("decor hats >= 3, got " ++ show nHat) (nHat >= 3)

goal_safe_counts :: Assertion
goal_safe_counts = do
  assertBool "unmet" (not (goalMet (goalCount CountSafes 2) 0 noCounts))
  assertBool "met" (goalMet (goalCount CountSafes 2) 0 (singleCount CountSafes 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountSafes 5) 0 (singleCount CountSafes 2))
  assertEqual "target" (5 :: Int) (goalTarget (goalCount CountSafes 5))
  assertBool
    "campaign has GoalSafe"
    (any (\g -> case goalView g of ViewCount CountSafes _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = levelGame 31 42
      nSafes =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isSafe (getCell (gsBoard gs) (r, c))
          ]
      nFlip =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isFlip (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor safes >= 5, got " ++ show nSafes) (nSafes >= 5)
  assertBool ("decor flips >= 4, got " ++ show nFlip) (nFlip >= 4)

-- | Daily (or any) GoalUfo config without level décor still gets a default UFO.
daily_ufo_goal_spawns_saucer :: Assertion
daily_ufo_goal_spawns_saucer = do
  let cfg = GameConfig 26 (goalCount CountUfo 8)
      gs = newGame cfg 20260929
  assertBool "default UFO placed" (not (null (gsUfos gs)))
  assertEqual "target color" [C1] (map ufoColor (take 1 (gsUfos gs)))


goal_carpet_counts :: Assertion
goal_carpet_counts = do
  assertBool "unmet" (not (goalMet (goalCount CountCarpets 2) 0 noCounts))
  assertBool "met" (goalMet (goalCount CountCarpets 2) 0 (singleCount CountCarpets 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountCarpets 5) 0 (singleCount CountCarpets 2))
  assertEqual "target" (5 :: Int) (goalTarget (goalCount CountCarpets 5))
  -- Campaign includes GoalCarpet
  assertBool "campaign has GoalCarpet" $
    any (\g -> case goalView g of ViewCount CountCarpets _ -> True; _ -> False) (map lvlGoal allLevels)
  let gs = levelGame 36 42
  assertEqual "level 36 carpet open" (8 :: Int) (length (gsCarpetOpen gs))
  assertEqual "goal" (goalCount CountCarpets 8) (gsGoal gs)
  assertEqual "campaign levels" campaignLevelCount (length allLevels)

carry_moves_on_next_level :: Assertion
carry_moves_on_next_level = do
  let cfg0 = levelConfig (levelAt 0)
      gs0 =
        (newGameAtLevel 0 cfg0 1)
          { gsOver = Just (TLevelClear 100 1)
          , gsMoves = 5  -- leftover
          }
      gs1 = nextLevel gs0 99
      base = lvlMoves (levelAt 1)
  assertEqual "level advanced" (1 :: Int) (gsLevel gs1)
  assertEqual "carried min(3,left)" (base + 3) (gsMoves gs1)  -- cap 3
  let gs2 =
        (newGameAtLevel 0 cfg0 2)
          { gsOver = Just (TLevelClear 50 1)
          , gsMoves = 2
          }
      gs3 = nextLevel gs2 100
  assertEqual "carry 2" (base + 2) (gsMoves gs3)

daily_goal_rotates_ten :: Assertion
daily_goal_rotates_ten = do
  let flavors =
        [ cfgGoal (dailyConfig (Year 2026) (Month 9) (Day d)) | d <- [1 .. 20] ]
      kinds = length (nub [ show g | g <- flavors ])
  assertBool ("at least 6 distinct daily goals, got " ++ show kinds) (kinds >= 6)
  -- Sample includes newer flavors
  assertBool "has chest or cake or safe or balloon among first 20 days" $
    any
      ( \g -> case goalView g of
          ViewCount CountChests _ -> True
          ViewCount CountCakes _ -> True
          ViewCount CountSafes _ -> True
          ViewCount CountBalloons _ -> True
          _ -> False
      )
      flavors


-- | Batch: all campaign levels (every entry of allLevels) constructible, positive goals/moves,
-- board size in bounds, décor enough for obstacle goals, legal move after ensure.
campaign_levels_batch_ok :: Assertion
campaign_levels_batch_ok = do
  assertEqual "campaign levels" campaignLevelCount (length allLevels)
  let seeds = [42, 99, 7] :: [Int]
  mapM_
    ( \seed ->
        mapM_
          ( \(i, lvl) -> do
              assertEqual ("index " ++ show i) i (lvlIndex lvl)
              assertBool ("moves>0 L" ++ show i) (lvlMoves lvl > 0)
              assertBool ("goal>0 L" ++ show i) (goalTarget (lvlGoal lvl) > 0)
              let gs = newGameAtLevel i (levelConfig lvl) (seed + i * 17)
                  b = gsBoard gs
              assertEqual ("rows L" ++ show i) (lvlRows lvl) (length (boardRows b))
              assertBool ("cols L" ++ show i) (all ((== lvlCols lvl) . length) (boardRows b))
              assertBool ("rows in range L" ++ show i) (validBoardDim (lvlRows lvl))
              assertBool ("cols in range L" ++ show i) (validBoardDim (lvlCols lvl))
              assertEqual ("cfg moves L" ++ show i) (lvlMoves lvl) (gsMoves gs)
              assertBool ("playable L" ++ show i ++ " s=" ++ show seed) (hasValidMove b)
              case goalView (lvlGoal lvl) of
                ViewCount CountStones n ->
                  assertBool ("stones L" ++ show i) (countCells isStone b >= n)
                ViewCount CountChests n ->
                  assertBool ("chests L" ++ show i) (countCells isChest b >= n)
                ViewCount CountHoney n ->
                  assertBool ("honey L" ++ show i) (countCells isHoney b >= n)
                ViewCount CountBalloons n ->
                  assertBool ("balloons L" ++ show i) (countCells isBalloon b >= n)
                ViewCount CountCookies n
                  -- 新玩法 6：有掉落口的关卡开局只有掉落口上的几块（至少 min 保持数 掉落口格数），其余由掉落口陆续补进场
                  | not (null (lvlDrops lvl)) ->
                      let cookieDrops = [ds | ds <- lvlDrops lvl, dropCell ds == Cookie]
                      in assertBool ("cookies L" ++ show i ++ " (drop level)")
                           (not (null cookieDrops) && and [countCells isCookie b >= min (dropKeep ds) (length (dropCells ds)) | ds <- cookieDrops])
                  | otherwise ->
                      assertBool ("cookies L" ++ show i) (countCells isCookie b >= n)
                ViewCount CountCakes n ->
                  assertBool ("cakes L" ++ show i) (countCells isCake b >= n)
                ViewCount CountSafes n ->
                  assertBool ("safes L" ++ show i) (countCells isSafe b >= n)
                ViewCount CountCarpets n ->
                  assertBool ("carpets L" ++ show i) (length (gsCarpetOpen gs) >= n)
                ViewCount CountUfo _ ->
                  assertBool ("ufo L" ++ show i) (not (null (gsUfos gs)))
                _ -> pure ()
          )
          (zip [0 :: Int ..] allLevels)
    )
    seeds
  where
    countCells p b =
      length
        [ ()
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , p (getCell b (r, c))
        ]

-- | Finale / high-pressure levels keep a reasonable move budget.
finale_and_pressure_moves_reasonable :: Assertion
finale_and_pressure_moves_reasonable = do
  let finale = levelAt 27
      master = levelAt 15
      pressure = levelAt 14
      steam = levelAt 35
      carpet = levelAt 36
      weave = levelAt 37
  assertEqual "终章 name" "终章" (lvlName finale)
  assertBool "终章 moves >= 24" (lvlMoves finale >= 24)
  assertBool "大师 moves >= 22" (lvlMoves master >= 22)
  assertBool "压力 moves >= 20" (lvlMoves pressure >= 20)
  assertBool "蒸汽 moves >= 22" (lvlMoves steam >= 22)
  assertBool "地毯 moves >= 24" (lvlMoves carpet >= 24)
  assertBool "织毯 moves >= 24" (lvlMoves weave >= 24)
  -- Soft score caps so dense décor levels stay fair (numbers only; rules frozen)
  case goalView (lvlGoal master) of
    ViewScore n -> assertBool "大师 score <= 1000" (n <= 1000)
    _ -> assertFailure "大师 should be GoalScore"
  case goalView (lvlGoal finale) of
    ViewScore n -> assertBool "终章 score <= 1400" (n <= 1400)
    _ -> assertFailure "终章 should be GoalScore"
  -- Every level at least 18 moves; no zero/negative goals
  mapM_
    ( \lvl -> do
        assertBool (lvlName lvl ++ " moves>=18") (lvlMoves lvl >= 18)
        assertBool (lvlName lvl ++ " goal>0") (goalTarget (lvlGoal lvl) > 0)
    )
    allLevels


-- | Daily obstacle goals (li=0 bare décor) still spawn enough entities.
daily_obstacle_goal_spawns_decor :: Assertion
daily_obstacle_goal_spawns_decor = do
  let check keep goal tag = do
        let gs = newGame (GameConfig 26 goal) 20260903
            n =
              length
                [ ()
                | r <- [0 .. boardSize - 1]
                , c <- [0 .. boardSize - 1]
                , keep (getCell (gsBoard gs) (r, c))
                ]
        assertBool (tag ++ " decor count=" ++ show n) (n >= 4)
  check isStone (goalCount CountStones 6) "stone"
  check isHoney (goalCount CountHoney 6) "honey"
  check isChest (goalCount CountChests 5) "chest"
  check isCake (goalCount CountCakes 5) "cake"
  check isSafe (goalCount CountSafes 4) "safe"
  check isBalloon (goalCount CountBalloons 6) "balloon"
  check isCookie (goalCount CountCookies 6) "cookie"

-- | Map / restart jump uses printed moves only (no leftover carry bank).
map_select_no_carry_moves :: Assertion
map_select_no_carry_moves = do
  let gsPrev =
        (levelGame 0 1)
          { gsOver = Just (TLevelClear 100 1)
          , gsMoves = 9
          }
      carried = nextLevel gsPrev 2
      base1 = lvlMoves (levelAt 1)
  assertEqual "carry path adds bonus" (base1 + 3) (gsMoves carried)
  -- Map-like jump / restart: fresh allotment
  let gsMap = levelGame 1 3
      gsRestart = restartLevel gsPrev { gsLevel = 1, gsOver = Nothing } 4
  assertEqual "map select no carry" base1 (gsMoves gsMap)
  assertEqual "restart no carry" base1 (gsMoves gsRestart)

-- | Star tiers use printed base moves; carry must not tighten the denominator.
star_rating_vs_carry_base :: Assertion
star_rating_vs_carry_base = do
  let base = 20 :: Int
      carry = 3 :: Int
      left = 8 :: Int
  assertEqual "3★ at 40% of base" (3 :: Int) (starRating base left)
  assertEqual "inflated start would wrongly drop to 2★" (2 :: Int) (starRating (base + carry) left)
  -- After nextLevel, gsMoves is printed+carry; UI must rate vs printed (Main advanceOrMsg).
  let gsPrev =
        (levelGame 0 1)
          { gsOver = Just (TLevelClear 50 1)
          , gsMoves = 5
          }
      gsNext = nextLevel gsPrev 9
      printed = lvlMoves (levelAt (gsLevel gsNext))
  assertEqual "carry cap on gsMoves" (printed + 3) (gsMoves gsNext)
  assertEqual "skill tier vs printed still 3★ at 40%" (3 :: Int) (starRating printed (printed * 2 `div` 5))
  assertEqual "skill tier vs inflated would be 2★" (2 :: Int) (starRating (gsMoves gsNext) (printed * 2 `div` 5))

-- | LevelClear must unlock the next map index immediately (before N advance).
unlock_after_clear_bumps_map :: Assertion
unlock_after_clear_bumps_map = do
  assertEqual "clear L0 unlocks 1" (1 :: Int) (unlockAfterClear 0 (LevelClear 10 1))
  assertEqual "already past stays" (5 :: Int) (unlockAfterClear 5 (LevelClear 10 3))
  assertEqual "move no bump" (2 :: Int) (unlockAfterClear 2 (MoveApplied 30))
  assertEqual "won unlocks finale" (length allLevels - 1) (unlockAfterClear 0 (Won 99))

-- | Clicking the active level on the map resumes (no restart / progress loss).
map_click_same_level_resumes :: Assertion
map_click_same_level_resumes = do
  assertEqual "same level -> resume" Nothing (mapClickJump 3 7 3)
  assertEqual "locked -> ignore" Nothing (mapClickJump 3 7 9)
  assertEqual "other unlocked -> jump" (Just 5) (mapClickJump 3 7 5)
  assertEqual "earlier unlocked -> jump" (Just 1) (mapClickJump 3 7 1)

-- | Bare GoalCarpet (no levelCarpets) still gets open floor tiles (UFO décor parity).
goal_carpet_seeds_open_tiles :: Assertion
goal_carpet_seeds_open_tiles = do
  let gs = newGame (GameConfig 26 (goalCount CountCarpets 8)) 20260929
  assertEqual "goal" (goalCount CountCarpets 8) (gsGoal gs)
  assertBool
    ("open carpets >= 8, got " ++ show (length (gsCarpetOpen gs)))
    (length (gsCarpetOpen gs) >= 8)
  assertEqual "covered start" (0 :: Int) (gsCount CountCarpets gs)
  -- GoalCookie bare newGame must seed high biscuits (ensureGoalDecor)
  let gsCk = newGame (GameConfig 26 (goalCount CountCookies 6)) 20260929
      nCk =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isCookie (getCell (gsBoard gsCk) (r, c))
          ]
  assertBool ("cookies >= 6, got " ++ show nCk) (nCk >= 6)
  assertBool "cookies not only on bottom" $
    any
      (\(r, c) -> r < boardSize - 1 && isCookie (getCell (gsBoard gsCk) (r, c)))
      [ (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] ]


--------------------------------------------------------------------------------
-- Daily clear must not LevelClear into campaign / unlock map
--------------------------------------------------------------------------------

-- | newDailyGame clear is Won (not LevelClear nextIdx=1 into 入门→采红).
-- Regression: daily used newGameAtLevel 0, so meeting the goal looked like
-- campaign L0 clear and offered NEXT L2 / unlocked map node 1.
daily_clear_is_won_not_levelclear :: Assertion
daily_clear_is_won_not_levelclear = do
  let cfg = GameConfig 20 (goalScore 10)
      gs0 = newDailyGame cfg 42
  assertBool "flagged daily" (gsDaily gs0)
  assertEqual "daily sits at index 0" (0 :: Int) (gsLevel gs0)
  let hint = findHint (gsBoard gs0)
  case hint of
    Nothing -> assertFailure "daily board must have a move"
    Just (p1, p2) -> do
      let (_, out) = trySwap p1 p2 (gs0 { gsScore = 0, gsGoal = goalScore 10, gsMoves = 15, gsOver = Nothing })
      case out of
        Won _ -> pure ()
        LevelClear _ n ->
          assertFailure ("daily must Won, got LevelClear next=" ++ show n)
        MoveApplied _ ->
          -- score goal 10 may need more points; force via already-met score
          let gsMet =
                gs0
                  { gsScore = 50
                  , gsGoal = goalScore 10
                  , gsMoves = 15
                  , gsOver = Nothing
                  }
              (gs2, out2) = trySwap p1 p2 gsMet
          in case out2 of
               Won s -> do
                 assertBool "won score" (s >= 10)
                 case gsOver gs2 of
                   Just (TWon _) -> pure ()
                   other -> assertFailure ("gsOver should be Won, got " ++ show other)
               LevelClear _ n ->
                 assertFailure ("daily must Won even when score already met, got LevelClear " ++ show n)
               other -> assertFailure ("expected Won, got " ++ show other)
        other -> assertFailure ("expected Won/MoveApplied, got " ++ show other)
  -- Campaign L0 with same goal still LevelClears
  let gsCamp = newGameAtLevel 0 (GameConfig 20 (goalScore 10)) 42
  assertBool "campaign not daily" (not (gsDaily gsCamp))
  case findHint (gsBoard gsCamp) of
    Nothing -> assertFailure "campaign L0 needs a move"
    Just (p1, p2) -> do
      let (gsC, outC) =
            trySwap p1 p2
              (gsCamp { gsScore = 50, gsGoal = goalScore 10, gsMoves = 15, gsOver = Nothing })
      case outC of
        LevelClear _ 1 -> pure ()
        Won _ -> assertFailure "campaign L0 must LevelClear, not Won"
        other -> assertFailure ("expected LevelClear 1, got " ++ show other ++ " over=" ++ show (gsOver gsC))

-- | Daily Won must not bump map unlock (finale Won still unlocks all).
daily_won_does_not_unlock_map :: Assertion
daily_won_does_not_unlock_map = do
  let gsD = newDailyGame (GameConfig 26 (goalScore 600)) 1
      reached0 = 0 :: Int
  assertEqual "daily Won keeps unlock" reached0 (unlockAfterOutcome gsD reached0 (Won 100))
  assertEqual "daily LevelClear also no-op" reached0 (unlockAfterOutcome gsD reached0 (LevelClear 100 1))
  -- Campaign parity: unlockAfterClear / unlockAfterOutcome still bump
  let gsC = newGameAtLevel 0 defaultConfig 1
  assertEqual "campaign LevelClear unlocks" (1 :: Int) (unlockAfterOutcome gsC 0 (LevelClear 10 1))
  assertEqual "finale Won unlocks all" (length allLevels - 1) (unlockAfterOutcome gsC 0 (Won 999))
  assertEqual "unlockAfterClear finale Won unchanged" (length allLevels - 1) (unlockAfterClear 0 (Won 999))
