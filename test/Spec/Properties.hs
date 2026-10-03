{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QuickCheck 性质测试。
--
-- qc_findMatches_ge3 是原有的性质（逐字不变）。其余是第 1 刀新增的不变量，覆盖重力、补子、连锁、
-- 撤销 / 重做、回放、目标进度与元素注册表；第 8 刀加形状规则表 / 组合表 / 补子策略与旧实现的对照、组合表的对称性。为了可复现，新性质统一挂在固定种子下
-- （localOption (QuickCheckReplayLegacy 20260930)），每条用 withMaxSuccess 控制次数。
-- 生成器只用本模块里的 Gen（格子、可空盘面、战役关卡 + 种子 + 动作选择），不依赖规则实现。
module Spec.Properties
  ( tests
    -- * 生成器（Spec.Perf / Spec.Optics / Spec.Invariants 也用）
  , genColor
  , genOverlay
  , genGem
  , genCell
  , genPlayBoard
  , genPos
  , genStart
  , genPick
  , playPicks
  , startState
  ) where

import Data.Array (bounds)
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing)
import Engine.Game (Game(..), Step(..), runActions)
import Engine.History (History(..), HistoryPolicy(..), Undoable(..), historyDepth, startHistory)
import Match3.Board.Cascade (AfterEntry(..), CascadeRun(..), cascadeAfterWith, cascadeCountdownsTracedWith, cascadeMatchesWith, cascadeSeedsWith, endHolesWith, stillRun)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Match3.Conveyor (applyBeltMoves)
import Match3.Element.Class (LevelElement(..), SomeLevelElement(..), SomeMessage(..))
import Match3.Element.Message (Message, fromMessage)
import Match3.Game.EndPhase (boosterEndTable, runEndTable, runPhase, swapEndTable)
import Match3.Game.Level (newGame)
import Match3.Game.State
  ( gsBelts
  , gsCarpetOpen
  , gsCollected
  , gsColorBag
  , gsCount
  , gsGoalMet
  , gsGround
  , gsPortals
  , gsProgress
  , setGround
  )
import Match3.Game.Trace (traceSpreadsWith)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types
  ( Meter(..)
  , Quota(..)
  , goalCollect
  , goalColors
  , goalCount
  , goalMet
  , goalProgress
  , goalScore
  , goalTarget
  , meterValue
  , numColors
  , specialActivates
  )
import Match3.Ufo (mkUfo, stepUfos)
import System.Random (StdGen)
import Match3.Board.Default (LevelHooks(..), builtinHooks, cascadeMatches, findMatches, hasAnyMatch, noHooks)
import Match3.Board.Match (findHintWith, findMatchRunsWith, hasAnyMatchWith)
import Match3.Board.Grid (MBoard, mboardFromRows, randomColor, setCell, swapCells)
import Match3.Board.Gravity (activeRefill, applyGravityWith, gravityFixedCellWith, refill)
import Match3.Board.Clear (spawnSpecialsWith)
import qualified Match3.Combos as Combos
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import Match3.Core
import Match3.Counts
  ( Counts
  , bumpCount
  , colorBag
  , countOf
  , countsFromList
  , countsToList
  , namedCounts
  , noCounts
  , plusCounts
  )
import Match3.Element
import Match3.Element.Class (levelNameOf, toCell)
import Spec.Support (levelGame)
import qualified Match3.Engine as M3E
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.QuickCheck

-- | 本模块的测试（平铺进顶层 "match3" 组）。
tests :: [TestTree]
tests =
  [ testProperty "qc_findMatches_ge3" qc_findMatches_ge3
  ]
    ++ map fixedSeed
      [ testProperty "qc_gravity_keeps_cells_and_column_order" (withMaxSuccess 500 qc_gravity_keeps_cells_and_column_order)
      , testProperty "qc_refill_leaves_no_holes" (withMaxSuccess 500 qc_refill_leaves_no_holes)
      , testProperty "qc_cascade_terminates_stable" (withMaxSuccess 300 qc_cascade_terminates_stable)
      , testProperty "qc_undo_redo_roundtrip" (withMaxSuccess 100 qc_undo_redo_roundtrip)
      , testProperty "qc_replay_same_seed_deterministic" (withMaxSuccess 60 qc_replay_same_seed_deterministic)
      , testProperty "qc_goal_progress_monotone" (withMaxSuccess 60 qc_goal_progress_monotone)
      , testProperty "qc_goal_matches_legacy" (withMaxSuccess 2000 qc_goal_matches_legacy)
      , testProperty "qc_goal_progress_laws" (withMaxSuccess 1000 qc_goal_progress_laws)
      , testProperty "qc_goal_progress_bounded" (withMaxSuccess 60 qc_goal_progress_bounded)
      , testProperty "qc_registry_decode_roundtrip" (withMaxSuccess 1000 qc_registry_decode_roundtrip)
      , testProperty "qc_registry_names_slots_unique" (once qc_registry_names_slots_unique)
      , testProperty "qc_find_hint_local_matches_reference" (withMaxSuccess 400 qc_find_hint_local_matches_reference)
      , testProperty "qc_counts_algebra" (withMaxSuccess 1000 qc_counts_algebra)
      , testProperty "qc_counts_monotone_legacy_view" (withMaxSuccess 60 qc_counts_monotone_legacy_view)
      , testProperty "qc_name_newtypes_show_ord" (withMaxSuccess 1000 qc_name_newtypes_show_ord)
      , testProperty "qc_level_hooks_match_legacy" (withMaxSuccess 500 qc_level_hooks_match_legacy)
      , testProperty "qc_level_elems_readers_roundtrip" (withMaxSuccess 100 qc_level_elems_readers_roundtrip)
      , testProperty "qc_end_table_matches_legacy" (withMaxSuccess 300 qc_end_table_matches_legacy)
      , testProperty "qc_ask_levels_folds_in_order" (withMaxSuccess 300 qc_ask_levels_folds_in_order)
      , testProperty "qc_shape_table_matches_legacy" (withMaxSuccess 1000 (checkCoverage qc_shape_table_matches_legacy))
      , testProperty "qc_combo_table_matches_legacy" (withMaxSuccess 3000 (checkCoverage qc_combo_table_matches_legacy))
      , testProperty "qc_combo_table_symmetric" (withMaxSuccess 3000 qc_combo_table_symmetric)
      , testProperty "qc_refill_policy_default_matches_legacy" (withMaxSuccess 500 qc_refill_policy_default_matches_legacy)
      ]
  where
    -- 新性质固定种子，每次运行生成同一批用例（命令行 --quickcheck-replay 对它们不生效）
    fixedSeed = localOption (QuickCheckReplayLegacy 20260930)

qc_findMatches_ge3 :: Property
qc_findMatches_ge3 =
  forAll (choose (0, boardSize - 1)) $ \r ->
    forAll (choose (0, boardSize - 3)) $ \c0 ->
      let fill = mkGem C5
          b0 = replicate boardSize (replicate boardSize fill)
          row =
            [ if c >= c0 && c < c0 + 3 then mkGem C1 else mkGem C5
            | c <- [0 .. boardSize - 1]
            ]
          b = boardFromRows $ take r b0 ++ [row] ++ drop (r + 1) b0
          ms = findMatches b
      in (r, c0) `elem` ms
           && (r, c0 + 1) `elem` ms
           && (r, c0 + 2) `elem` ms

--------------------------------------------------------------------------------
-- 生成器

genColor :: Gen Color
genColor = elements [minBound .. maxBound]

genOverlay :: Gen CellOverlay
genOverlay =
  oneof
    [ elements [Grass, Vine, Choco, Steam]
    , Fog <$> choose (1, 2)
    , Chain <$> choose (1, 2)
    , Freeze <$> choose (1, 2)
    , Curtain <$> choose (1, 2)
    ]

-- | 宝石（任意种类、冰层 0..2、可带叠层）。
genGem :: Gen Cell
genGem =
  Gem
    <$> genColor
    <*> frequency [(6, pure Normal), (1, elements [LineH, LineV, Bomb, Rainbow])]
    <*> frequency [(4, pure 0), (1, choose (1, 2))]
    <*> frequency [(4, pure Nothing), (1, Just <$> genOverlay)]

-- | 任意格：宝石为主，覆盖全部内置本体与几种自定义名字（含已注册的 bubble 与未注册的名字）。
genCell :: Gen Cell
genCell =
  frequency
    [ (12, genGem)
    , (1, Stone <$> choose (1, 3))
    , (1, Chest <$> choose (1, 3))
    , (1, Honey <$> choose (1, 3))
    , (1, Balloon <$> genColor)
    , (1, pure Cookie)
    , (1, Cake <$> choose (1, 3))
    , (1, pure MagicHat)
    , (1, Maker <$> genColor <*> choose (0, 3))
    , (1, elements [Snail 0 1, Snail 0 (-1), Snail 1 0, Snail (-1) 0])
    , (1, Safe <$> choose (1, 3))
    , (1, Flip <$> genColor <*> genColor)
    , (1, pure Surprise)
    , (1, Bottle <$> genColor)
    , (1, pure TimeSpirit)
    , (1, Countdown <$> genColor <*> choose (1, 5))
    , (1, Custom <$> elements ["bubble", "qc_unregistered"] <*> (CustomState <$> choose (1, 3)))
    ]

-- | 可空盘面（行优先），约 1/4 是空洞。
genMBoard :: Gen MBoard
genMBoard = mboardFromRows <$> vectorOf boardSize (vectorOf boardSize (frequency [(3, Just <$> genCell), (1, pure Nothing)]))

-- | 只含普通 / 特殊宝石（少量冰、叠层）与少量障碍的完整盘面，常带现成的匹配。
genPlayBoard :: Gen Board
genPlayBoard =
  boardFromRows
    <$> vectorOf boardSize (vectorOf boardSize (frequency [(10, mkGem <$> genColor), (2, genGem), (1, genCell)]))

column :: MBoard -> Int -> [Maybe Cell]
column mb c = [atM mb (r, c) | r <- [0 .. boardSize - 1]]

--------------------------------------------------------------------------------
-- 重力 / 补子 / 连锁

-- | 重力：每列非空格数量不变；固定格（falls = False）原地不动；可下落格自上而下的相对顺序不变；
-- 相邻两个固定格之间（及列两端）的每一段里空洞都在上面、实格沉在下面。
qc_gravity_keeps_cells_and_column_order :: Property
qc_gravity_keeps_cells_and_column_order =
  forAll genMBoard $ \mb ->
    let reg = defaultRegistry
        mb' = applyGravityWith reg mb
        fixed = gravityFixedCellWith reg
        isFixedM = maybe False fixed
        colOk c =
          let colBefore = column mb c
              colAfter = column mb' c
              fixedAt col = [(i, x) | (i, Just x) <- zip [0 :: Int ..] col, fixed x]
              movable col = [x | Just x <- col, not (fixed x)]
              segments col = case break isFixedM col of
                (seg, _ : rest) -> seg : segments rest
                (seg, []) -> [seg]
              packed seg = let (hs, ss) = span isNothing seg in all isJust ss && length hs + length ss == length seg
          in length [() | Just _ <- colBefore] == length [() | Just _ <- colAfter]
               && fixedAt colBefore == fixedAt colAfter
               && movable colBefore == movable colAfter
               && all packed (segments colAfter)
    in counterexample (show mb') (bounds mb' == ((0, 0), (boardSize - 1, boardSize - 1)) && all colOk [0 .. boardSize - 1])

-- | 补子：任何可空盘面都能补满（不触发 "refill: hole"）；原有格不变；每个空洞补成普通宝石。
qc_refill_leaves_no_holes :: Int -> Property
qc_refill_leaves_no_holes seed =
  forAll genMBoard $ \mb ->
    let (b, _) = refill (mkStdGen seed) mb
        cellOk (r, c) = case atM mb (r, c) of
          Just x -> getCell b (r, c) == x
          Nothing -> case getCell b (r, c) of
            Gem _ Normal 0 Nothing -> True
            _ -> False
    in all cellOk [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

-- | 连锁：从带现成匹配的盘面开始，内置注册表下的连锁一定结束（2 秒内），结束后盘面上没有现成匹配，
-- 每一轮首尾相接。
qc_cascade_terminates_stable :: Int -> Property
qc_cascade_terminates_stable seed =
  forAll genPlayBoard $ \b0 ->
    within 2000000 $
      let run = cascadeMatches Nothing noHooks (mkStdGen seed) b0
          ws = crWaves run
          chained = and (zipWith (\a b -> cwAfter a == cwBefore b) ws (drop 1 ws))
      in classify (not (null ws)) "cascaded" $
           classify (length ws >= 3) "3+ waves" $
           counterexample (show (crBoard run)) $
           not (hasAnyMatch (crBoard run))
             && chained
             && (null ws || cwAfter (last ws) == crBoard run)

--------------------------------------------------------------------------------
-- 整局：战役关卡 + 种子 + 动作选择

-- | 战役关卡下标与种子。
genStart :: Gen (Int, Int)
genStart = (,) <$> choose (0, length allLevels - 1) <*> choose (1, 100000)

-- | 一步动作的选择：交换的起点、方向，偶尔换成道具（没有次数时被拒，也是合法输入）。
data Pick = Pick Int Int Bool Int
  deriving (Show)

genPick :: Gen Pick
genPick = Pick <$> choose (0, boardSize * boardSize - 1) <*> choose (0, 1) <*> frequency [(9, pure False), (1, pure True)] <*> choose (0, 2)

-- | 从选择得到一个动作：交换时从起点开始按行优先找第一对「会被接受」的相邻交换（找不到就用起点那一对），
-- 道具时按起点格用锤子 / 十字清除 / 自由交换。
pickAction :: GameState -> Pick -> M3E.Action
pickAction gs (Pick start dir booster which)
  | booster = case which of
      0 -> M3E.Hammer (at start)
      1 -> M3E.CrossClear (at start)
      _ -> M3E.FreeSwap (at start) (at (start + 9))
  | otherwise = case filter accepted candidates of
      (a : _) -> a
      [] -> case candidates of
        (a : _) -> a
        [] -> M3E.Hint
  where
    (nr, nc) = boardDims (gsBoard gs)
    n = max 1 (nr * nc)
    at i = let k = i `mod` n in (k `div` nc, k `mod` nc)
    candidates =
      [ M3E.Swap p q
      | i <- [start .. start + n - 1]
      , d <- [dir, 1 - dir]
      , let p@(r, c) = at i
            q = if d == 0 then (r, c + 1) else (r + 1, c)
      , fst q >= 0 && fst q < nr && snd q >= 0 && snd q < nc
      ]
    accepted a = stepAccepted (gameStep M3E.match3Game gs a)

-- | 按选择依次走（遇到结局停）；返回动作序列与每一步的结果。
playPicks :: GameState -> [Pick] -> ([M3E.Action], [Step GameState Event Terminal M3E.Played])
playPicks _ [] = ([], [])
playPicks gs (p : ps)
  | isJust (gsOver gs) = ([], [])
  | otherwise =
      let a = pickAction gs p
          st = gameStep M3E.match3Game gs a
          (as, sts) = playPicks (stepState st) ps
      in (a : as, st : sts)

startState :: (Int, Int) -> GameState
startState (li, seed) = gameNew M3E.match3Game (M3E.Campaign li) seed

-- | 撤销 / 重做往返：走若干步到局中，再走一步被接受的走步；撤销回到走步前的状态（按撤销规则整理过的快照，
-- 包括随机数生成器，按 Show 全字段比较）、历史深度复原；再把同一个动作重做一遍，结果与第一次逐字段相同。
qc_undo_redo_roundtrip :: Property
qc_undo_redo_roundtrip =
  forAll genStart $ \start ->
    forAll (choose (0, 3) >>= \k -> vectorOf k genPick) $ \prefixPicks ->
      forAll genPick $ \lastPick ->
        let g = M3E.match3Shell
            policy = M3E.match3History
            (prefix, _) = playPicks (startState start) prefixPicks
            h0 = foldl (\h act -> stepState (gameStep g h (Act act))) (startHistory (startState start)) prefix
            s0 = histNow h0
            a = pickAction s0 lastPick
            st1 = gameStep g h0 (Act a)
            h1 = stepState st1
            recorded = stepAccepted st1 && historyDepth h1 == historyDepth h0 + 1
            st2 = gameStep g h1 Undo
            h2 = stepState st2
            st3 = gameStep g h2 (Act a)
            h3 = stepState st3
        in isNothing (gsOver s0) && recorded ==>
             counterexample (show a) $
               conjoin
                 [ counterexample "undo accepted" (stepAccepted st2)
                 , counterexample "undo restores the snapshot" (show (histNow h2) === show (hpRestore policy (hpSnapshot policy s0)))
                 , counterexample "undo restores depth" (historyDepth h2 === historyDepth h0)
                 , counterexample "redo accepted" (stepAccepted st3)
                 , counterexample "redo repeats the move" (show (histNow h3) === show (histNow h1))
                 ]

-- | 回放确定：同关卡、同种子、同一串动作，两次独立执行的每一步状态与事件逐字相同。
qc_replay_same_seed_deterministic :: Property
qc_replay_same_seed_deterministic =
  forAll genStart $ \start ->
    forAll (choose (1, 6) >>= \k -> vectorOf k genPick) $ \picks ->
      let (acts, steps1) = playPicks (startState start) picks
          steps2 = runActions M3E.match3Game (startState start) acts
          summary sts = [(show (stepState st), show (stepEvents st), stepAccepted st) | st <- sts]
      in classify (any stepAccepted steps1) "some move accepted" $
           counterexample (show acts) (summary steps1 === summary steps2)

-- | 目标进度单调：一局里每一步之后，分数、主进度（gsProgress 与派生的 gsCollected）、各类计数字段、
-- 各色清除数、按名字的计数都不减；目标一旦满足就一直满足。
qc_goal_progress_monotone :: Property
qc_goal_progress_monotone =
  forAll genStart $ \start ->
    forAll (choose (1, 8) >>= \k -> vectorOf k genPick) $ \picks ->
      let s0 = startState start
          (_, steps) = playPicks s0 picks
          states = s0 : map stepState steps
          pairs = zip states (drop 1 states)
          ok (a, b) =
            and (zipWith (<=) (meters a) (meters b))
              && and [lookupN n b >= v | (n, v) <- namedCounts (gsCounts a)]
              && (not (gsGoalMet a) || gsGoalMet b)
      in classify (length (filter stepAccepted steps) >= 3) "3+ accepted steps" $
           classify (any (\st -> gsScore (stepState st) > gsScore s0) steps) "scored" $
           counterexample (show (map meters states)) (all ok pairs)
  where
    meters gs =
      [ gsScore gs, gsCollected gs, gsProgress gs
      , gsCount CountStones gs, gsCount CountChests gs, gsCount CountHoney gs, gsCount CountBalloons gs
      , gsCount CountCookies gs, gsCount CountCakes gs, gsCount CountSafes gs, gsCount CountUfo gs, gsCount CountCarpets gs
      ]
        ++ map snd (gsColorBag gs)
    lookupN n gs = maybe 0 id (lookup n (namedCounts (gsCounts gs)))

-- | 第 5 刀前的目标（13 个构造器，派生 Show）与它的判定 / 进度（逐字抄自旧 Types.goalMetEx / goalProgressEx /
-- goalTarget，12 个位置参数按旧结算的口径从计数里取）：新目标数据必须与它处处一致。
data OldGoal
  = GoalScore Int
  | GoalCollect Color Int
  | GoalCollectMulti [(Color, Int)]
  | GoalClearStone Int
  | GoalChest Int
  | GoalHoney Int
  | GoalBalloon Int
  | GoalCookie Int
  | GoalCake Int
  | GoalSafe Int
  | GoalUfo Int
  | GoalCarpet Int
  | GoalNamed String Int
  deriving (Show)

toNewGoal :: OldGoal -> LevelGoal
toNewGoal og = case og of
  GoalScore t -> goalScore t
  GoalCollect c n -> goalCollect c n
  GoalCollectMulti rs -> goalColors rs
  GoalClearStone n -> goalCount CountStones n
  GoalChest n -> goalCount CountChests n
  GoalHoney n -> goalCount CountHoney n
  GoalBalloon n -> goalCount CountBalloons n
  GoalCookie n -> goalCount CountCookies n
  GoalCake n -> goalCount CountCakes n
  GoalSafe n -> goalCount CountSafes n
  GoalUfo n -> goalCount CountUfo n
  GoalCarpet n -> goalCount CountCarpets n
  GoalNamed name n -> goalCount (CountNamed (ElementName name)) n

oldLookupCount :: [(Color, Int)] -> Color -> Int
oldLookupCount xs col = maybe 0 id (lookup col xs)

oldGoalMetEx :: OldGoal -> Int -> Int -> [(Color, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
oldGoalMetEx (GoalScore t) score _ _ _ _ _ _ _ _ _ _ = score >= t
oldGoalMetEx (GoalCollect _ n) _ collected _ _ _ _ _ _ _ _ _ = collected >= n
oldGoalMetEx (GoalCollectMulti reqs) _ _ bag _ _ _ _ _ _ _ _ =
  all (\(col, n) -> oldLookupCount bag col >= n) reqs
oldGoalMetEx (GoalClearStone n) _ _ _ stones _ _ _ _ _ _ _ = stones >= n
oldGoalMetEx (GoalUfo n) _ _ _ _ ufos _ _ _ _ _ _ = ufos >= n
oldGoalMetEx (GoalChest n) _ _ _ _ _ chests _ _ _ _ _ = chests >= n
oldGoalMetEx (GoalHoney n) _ _ _ _ _ _ honey _ _ _ _ = honey >= n
oldGoalMetEx (GoalBalloon n) _ _ _ _ _ _ _ balloons _ _ _ = balloons >= n
oldGoalMetEx (GoalCookie n) _ _ _ _ _ _ _ _ cookies _ _ = cookies >= n
oldGoalMetEx (GoalCake n) _ _ _ _ _ _ _ _ _ cakes _ = cakes >= n
oldGoalMetEx (GoalSafe n) _ _ _ _ _ _ _ _ _ _ safes = safes >= n
oldGoalMetEx (GoalCarpet n) _ collected _ _ _ _ _ _ _ _ _ = collected >= n
oldGoalMetEx (GoalNamed _ n) _ collected _ _ _ _ _ _ _ _ _ = collected >= n

oldGoalProgressEx :: OldGoal -> Int -> Int -> [(Color, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
oldGoalProgressEx (GoalScore _) score _ _ _ _ _ _ _ _ _ _ = score
oldGoalProgressEx (GoalCollect _ _) _ collected _ _ _ _ _ _ _ _ _ = collected
oldGoalProgressEx (GoalCollectMulti reqs) _ _ bag _ _ _ _ _ _ _ _ =
  sum [min n (oldLookupCount bag c) | (c, n) <- reqs]
oldGoalProgressEx (GoalClearStone _) _ _ _ stones _ _ _ _ _ _ _ = stones
oldGoalProgressEx (GoalUfo _) _ _ _ _ ufos _ _ _ _ _ _ = ufos
oldGoalProgressEx (GoalChest _) _ _ _ _ _ chests _ _ _ _ _ = chests
oldGoalProgressEx (GoalHoney _) _ _ _ _ _ _ honey _ _ _ _ = honey
oldGoalProgressEx (GoalBalloon _) _ _ _ _ _ _ _ balloons _ _ _ = balloons
oldGoalProgressEx (GoalCookie _) _ _ _ _ _ _ _ _ cookies _ _ = cookies
oldGoalProgressEx (GoalCake _) _ _ _ _ _ _ _ _ _ cakes _ = cakes
oldGoalProgressEx (GoalSafe _) _ _ _ _ _ _ _ _ _ _ safes = safes
oldGoalProgressEx (GoalCarpet _) _ collected _ _ _ _ _ _ _ _ _ = collected
oldGoalProgressEx (GoalNamed _ _) _ collected _ _ _ _ _ _ _ _ _ = collected

oldGoalTarget :: OldGoal -> Int
oldGoalTarget og = case og of
  GoalScore t -> t
  GoalCollect _ n -> n
  GoalCollectMulti reqs -> sum [n | (_, n) <- reqs]
  GoalClearStone n -> n
  GoalChest n -> n
  GoalHoney n -> n
  GoalBalloon n -> n
  GoalCookie n -> n
  GoalCake n -> n
  GoalSafe n -> n
  GoalUfo n -> n
  GoalCarpet n -> n
  GoalNamed _ n -> n

-- | 旧结算写进 gsCollected 的值（第 5 刀前 Resolve 的 collected'，从 0 累加）：单色 = 该色清除数，
-- 多色 = Σ min 配额，石块 … 飞碟 / 地毯 / 名字 = 对应计数，分数目标恒 0。
oldCollected :: OldGoal -> Counts -> Int
oldCollected og cs = case og of
  GoalScore _ -> 0
  GoalCollect c _ -> countOf (CountColor c) cs
  GoalCollectMulti reqs -> sum [min n (countOf (CountColor c) cs) | (c, n) <- reqs]
  GoalClearStone _ -> countOf CountStones cs
  GoalChest _ -> countOf CountChests cs
  GoalHoney _ -> countOf CountHoney cs
  GoalBalloon _ -> countOf CountBalloons cs
  GoalCookie _ -> countOf CountCookies cs
  GoalCake _ -> countOf CountCakes cs
  GoalSafe _ -> countOf CountSafes cs
  GoalUfo _ -> countOf CountUfo cs
  GoalCarpet _ -> countOf CountCarpets cs
  GoalNamed name _ -> countOf (CountNamed (ElementName name)) cs

-- | 旧的 12 个位置参数，从计数按旧字段取。
oldArgs :: OldGoal -> Int -> Counts -> (Int -> Int -> [(Color, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> r) -> r
oldArgs og score cs f =
  f score (oldCollected og cs) (colorBag cs)
    (countOf CountStones cs) (countOf CountUfo cs) (countOf CountChests cs) (countOf CountHoney cs)
    (countOf CountBalloons cs) (countOf CountCookies cs) (countOf CountCakes cs) (countOf CountSafes cs)

genOldGoal :: Gen OldGoal
genOldGoal =
  oneof
    [ GoalScore <$> choose (0, 1500)
    , GoalCollect <$> genColor <*> tgt
    , GoalCollectMulti <$> oneof [pure [], choose (2, 3) >>= \k -> vectorOf k ((,) <$> genColor <*> tgt)]
    , GoalClearStone <$> tgt, GoalChest <$> tgt, GoalHoney <$> tgt, GoalBalloon <$> tgt, GoalCookie <$> tgt
    , GoalCake <$> tgt, GoalSafe <$> tgt, GoalUfo <$> tgt, GoalCarpet <$> tgt
    , GoalNamed <$> elements ["jelly", "bubble", "a b"] <*> tgt
    ]
  where
    tgt = choose (0, 30)

-- | 随机计数：内置各键、时间精灵、各色、两个名字，每键 0..40。
genGoalCounts :: Gen Counts
genGoalCounts = countsFromList <$> mapM (\k -> (,) k <$> frequency [(1, pure 0), (3, choose (0, 40))]) goalKeys
  where
    goalKeys =
      [ CountStones, CountChests, CountHoney, CountBalloons, CountCookies, CountCakes, CountSafes, CountSpirits
      , CountUfo, CountCarpets, CountNamed "jelly", CountNamed "bubble", CountNamed "a b" ]
        ++ map CountColor allColors

-- | 新旧对照：任意旧目标换成新目标数据后，Show（含加括号的上下文）、目标值、在任意分数 / 计数下的
-- 达成判定与进度都与旧实现相同（旧的 gsCollected 按旧结算口径由计数给出）。
qc_goal_matches_legacy :: Property
qc_goal_matches_legacy =
  forAll genOldGoal $ \og ->
    forAll (choose (0, 2000)) $ \score ->
      forAll genGoalCounts $ \cs ->
        let g = toNewGoal og
        in counterexample (show og ++ " " ++ show score ++ " " ++ show cs) $
             show g === show og
               .&&. show (Just g) === show (Just og)
               .&&. goalTarget g === oldGoalTarget og
               .&&. goalMet g score cs === oldArgs og score cs (oldGoalMetEx og)
               .&&. goalProgress g score cs === oldArgs og score cs (oldGoalProgressEx og)

-- | 进度的代数性质（任意 1..3 项配额，含分数与计数混合）：进度非负；达成 ⟺ 每项度量 ≥ 目标值；
-- 单项：达成 ⟺ 进度 ≥ 目标值；多项：进度 ≤ 目标值，且达成 ⟺ 进度 = 目标值；
-- 分数 / 计数只增时进度不减、达成保持。
qc_goal_progress_laws :: Property
qc_goal_progress_laws =
  forAll (choose (1, 3) >>= \k -> vectorOf k genQuota) $ \qs ->
    forAll ((,) <$> choose (0, 2000) <*> genGoalCounts) $ \(score, cs) ->
      forAll ((,) <$> choose (0, 500) <*> genGoalCounts) $ \(dScore, extra) ->
        let g = LevelGoal qs
            p = goalProgress g score cs
            t = goalTarget g
            met = goalMet g score cs
            each = all (\q -> meterValue (quotaMeter q) score cs >= quotaTarget q) qs
            p' = goalProgress g (score + dScore) (cs <> extra)
            met' = goalMet g (score + dScore) (cs <> extra)
            shape
              | length qs == 1 = met === (p >= t)
              | otherwise = (p <= t) .&&. (met === (p == t))
        in classify met "met" $
             classify (length qs > 1) "multi" $
               counterexample (show qs ++ " score=" ++ show score ++ " " ++ show cs) $
                 (p >= 0) .&&. (met === each) .&&. shape .&&. (p' >= p) .&&. (not met || met')
  where
    genQuota =
      Quota
        <$> frequency
              [ (1, pure MeterScore)
              , (4, MeterCount <$> elements ([CountStones, CountUfo, CountCarpets, CountNamed "jelly"] ++ map CountColor allColors))
              ]
        <*> choose (0, 30)

-- | 整局里的目标读数：进度非负；达成 ⟺ 各项配额都达到；单项目标 达成 ⟺ 进度 ≥ 目标值，
-- 多色目标 进度 ≤ 目标值；过关 / 通关结局的那一步目标一定达成、失败结局一定没达成（多色关偏重抽样）。
qc_goal_progress_bounded :: Property
qc_goal_progress_bounded =
  forAll (frequency [(2, genStart), (1, (,) <$> elements [6, 14] <*> choose (1, 100000))]) $ \start ->
    forAll (choose (1, 10) >>= \k -> vectorOf k genPick) $ \picks ->
      let s0 = startState start
          (_, steps) = playPicks s0 picks
          states = s0 : map stepState steps
          qs gs = goalQuotas (gsGoal gs)
          okState gs =
            let p = gsProgress gs
                t = goalTarget (gsGoal gs)
                each = all (\q -> meterValue (quotaMeter q) (gsScore gs) (gsCounts gs) >= quotaTarget q) (qs gs)
                shape
                  | length (qs gs) == 1 = gsGoalMet gs == (p >= t)
                  | otherwise = p <= t
            in p >= 0 && gsGoalMet gs == each && shape
          okOutcome gs = case gsOver gs of
            Just (TWon _) -> gsGoalMet gs
            Just (TLevelClear _ _) -> gsGoalMet gs
            Just (TLost _) -> not (gsGoalMet gs)
            Nothing -> True
      in classify (length (qs s0) > 1) "multi" $
           classify (any gsGoalMet states) "met" $
             counterexample (show [(gsProgress gs, goalTarget (gsGoal gs), gsGoalMet gs) | gs <- states]) $
               all okState states && all okOutcome states

--------------------------------------------------------------------------------
-- 元素注册表

-- | 解码往返：任意格解码成元素值（修饰器包着本体）再编码回去，得到原格；本体的元素名落在注册表的条目上，
-- 且条目的槽位与格子的编号一致（内置本体 = SlotCell (cellSlot 格)，已注册的自定义 = SlotCustom），
-- 最上层的叠层 / 冰层同样落在槽位一致的条目上。未注册的自定义名字解码成惰性占格，编码仍是原格。
qc_registry_decode_roundtrip :: Property
qc_registry_decode_roundtrip =
  forAll genCell $ \cell ->
    let reg = defaultRegistry
        entries = registryDefs reg
        slotOf n = [entrySlot e | e <- entries, entryName e == n]
        bodyName = elementName reg cell
        bodyOk = case cell of
          Custom n _
            | n `elem` map entryName entries -> bodyName == n && slotOf n == [SlotCustom]
            | otherwise -> bodyName == n && null (slotOf n)
          _ -> slotOf bodyName == [SlotCell (cellSlot cell)]
        topName = topLayerName reg cell
        topOk = case cell of
          Gem _ _ ice ov
            | ice > 0 -> slotOf topName == [SlotIce]
            | Just o <- ov -> slotOf topName == [SlotOverlay (overlaySlot o)]
          _ -> topName == bodyName
    in counterexample (show (bodyName, topName, slotOf bodyName, slotOf topName)) $
         toCell (elementOf reg cell) === cell .&&. bodyOk .&&. topOk

-- | 内置条目表（去重之前的原始列表）：名字互不相同；内置本体 / 叠层的槽号互不相同；冰层只有一个；
-- 全部内置本体槽号 0..19 与叠层槽号 0..7 都有条目。
qc_registry_names_slots_unique :: Property
qc_registry_names_slots_unique =
  let names = map entryName builtinDefs
      cells = sort [i | SlotCell i <- map entrySlot builtinDefs]
      ovs = sort [i | SlotOverlay i <- map entrySlot builtinDefs]
      ices = [() | SlotIce <- map entrySlot builtinDefs]
  in conjoin
       [ counterexample "names unique" (length (nub names) === length names)
       , counterexample "body slots unique and complete" (cells === [0 .. 19])
       , counterexample "overlay slots unique and complete" (ovs === [0 .. 7])
       , counterexample "one ice entry" (length ices === 1)
       ]

--------------------------------------------------------------------------------
-- 计数（第 4 刀）

genCounterKey :: Gen CounterKey
genCounterKey =
  frequency
    [ (8, elements [CountStones, CountChests, CountHoney, CountBalloons, CountCookies, CountCakes, CountSafes, CountSpirits, CountUfo, CountCarpets])
    , (2, CountNamed <$> elements ["jelly", "bubble", "crate"])
    ]

genCountList :: Gen [(CounterKey, Int)]
genCountList = listOf ((,) <$> genCounterKey <*> choose (0, 5))

allKeysOf :: [[(CounterKey, Int)]] -> [CounterKey]
allKeysOf xss = nub (map fst (concat xss)) ++ [CountStones, CountNamed "absent"]

-- | Counts 的代数：按键求和、稀疏（不存 0，键升序）、加法交换 / 结合、noCounts 为单位元、bumpCount = 加一个单键。
qc_counts_algebra :: Property
qc_counts_algebra =
  forAll genCountList $ \xs ->
    forAll genCountList $ \ys ->
      forAll genCountList $ \zs ->
        let a = countsFromList xs
            b = countsFromList ys
            c = countsFromList zs
            ks = allKeysOf [xs, ys, zs]
            sumFor k l = sum [n | (k', n) <- l, k' == k]
            listed = countsToList a
        in conjoin
             [ counterexample "countOf = 按键求和" (and [countOf k a == sumFor k xs | k <- ks])
             , counterexample "稀疏且键升序" (all ((> 0) . snd) listed && map fst listed == sort (nub (map fst listed)))
             , counterexample "加法逐键" (and [countOf k (a `plusCounts` b) == countOf k a + countOf k b | k <- ks])
             , counterexample "交换 / 结合 / 单位元" ((a <> b) == (b <> a) && ((a <> b) <> c) == (a <> (b <> c)) && (a <> noCounts) == a)
             , counterexample "bumpCount" (and [bumpCount k n a == a <> countsFromList [(k, n)] | (k, n) <- ys])
             , counterexample "namedCounts" (namedCounts a == sort [(n, v) | (CountNamed n, v) <- listed])
             ]

-- | 整局里的计数：每步之后存的个数都 > 0（非负、稀疏），每个键不减；与第 4 刀前字段的对照——
-- 主进度 gsCollected 在「按某个计数键」的目标下等于该键的个数（旧实现直接取对应字段），
-- Show 仍按旧字段名打印同一个数（元素查询快照对 show 取散列）。
qc_counts_monotone_legacy_view :: Property
qc_counts_monotone_legacy_view =
  forAll genStart $ \start ->
    forAll (choose (1, 8) >>= \k -> vectorOf k genPick) $ \picks ->
      let s0 = startState start
          (_, steps) = playPicks s0 picks
          states = s0 : map stepState steps
          pairs = zip states (drop 1 states)
          positive gs = all ((> 0) . snd) (countsToList (gsCounts gs))
          grows (a, b) = and [countOf k (gsCounts a) <= countOf k (gsCounts b) | (k, _) <- countsToList (gsCounts a)]
          goalKey gs = case goalView (gsGoal gs) of
            ViewCount k _ -> Just k
            _ -> Nothing
          collectedMatches gs = maybe True (\k -> gsCollected gs == gsCount k gs) (goalKey gs)
          legacyShow gs =
            let txt = show gs
            in and
                 [ (name ++ " = " ++ show (gsCount k gs) ++ ",") `isInfixOf` txt
                 | (name, k) <-
                     [ ("gsStonesCleared", CountStones), ("gsChestsCleared", CountChests), ("gsHoneyCleared", CountHoney)
                     , ("gsBalloonsPopped", CountBalloons), ("gsCookiesCollected", CountCookies), ("gsCakesCleared", CountCakes)
                     , ("gsSafesOpened", CountSafes), ("gsUfoCollected", CountUfo), ("gsCarpetsCovered", CountCarpets) ]
                 ]
                 && ("gsElementCounts = " ++ show (namedCounts (gsCounts gs)) ++ ",") `isInfixOf` txt
      in classify (any (\gs -> not (null (countsToList (gsCounts gs)))) states) "counted something" $
           classify (any (isJust . goalKey) states) "counter goal" $
           counterexample (show (map (countsToList . gsCounts) states)) $
             all positive states && all grows pairs && all collectedMatches states && all legacyShow states

--------------------------------------------------------------------------------
-- 提示

-- | 第 3 刀之前的 findHintWith（整盘 hasAnyMatchWith (swapCells b p1 p2)），留作参照实现。
findHintReference :: Registry -> Board -> Maybe (Pos, Pos)
findHintReference reg b =
  case matchHints ++ concatMap ruleHints (swapRules reg) of
    (x : _) -> Just x
    [] -> Nothing
  where
    matchHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , hintable (getCell b p1)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , fst p2 >= 0 && fst p2 < boardSize && snd p2 >= 0 && snd p2 < boardSize
      , hintable (getCell b p2)
      , hintableWith reg (getCell b p1) && hintableWith reg (getCell b p2)
      , hasAnyMatchWith reg (swapCells b p1 p2)
      ]
    ruleHints rule =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , fst p2 >= 0 && fst p2 < boardSize && snd p2 >= 0 && snd p2 < boardSize
      , not (upperLocked (getCell b p1) || upperLocked (getCell b p2))
      , srFires rule b p1 p2
      ]
    hintable cell = isJust (colorOfWith reg cell) && not (blocksSwapWith reg cell)
    upperLocked = upperBlocksSwapWith reg

-- | findHintWith 的局部匹配检查与参照实现（整盘检查）结果相同：随机盘面（常带现成匹配）、
-- 各关开局（稳定、无现成匹配，提示走局部检查路径）、整盘随机格（含障碍与自定义）三类。
qc_find_hint_local_matches_reference :: Property
qc_find_hint_local_matches_reference =
  forAll genHintBoard $ \b ->
    findHintWith defaultRegistry b === findHintReference defaultRegistry b
  where
    genHintBoard =
      oneof
        [ genPlayBoard
        , do
            li <- choose (0, length allLevels - 1)
            seed <- choose (1, 100000)
            pure (gsBoard (levelGame li seed))
        , do
            seed <- choose (1, 100000)
            pure (gsBoard (newGame defaultConfig seed))
        , boardFromRows <$> vectorOf boardSize (vectorOf boardSize genCell)
        ]

-- | 第 6b 刀：ElementName / CustomState 是 newtype，但 Show（任意优先级）与 Ord 和底层 String / Int 相同，
-- 所以 Custom 格（以及含它的 Cell / GameState）的 Show 与改前逐字相同、排序不变。
qc_name_newtypes_show_ord :: Property
qc_name_newtypes_show_ord =
  forAll ((,,,) <$> nameGen <*> nameGen <*> arbitrary <*> arbitrary) $ \(a, b, m, n) ->
    let ea = ElementName a
        eb = ElementName b
        cm = CustomState m
        cn = CustomState n
    in conjoin
         [ show ea === show a
         , showsPrec 11 ea "" === showsPrec 11 a ""
         , showsPrec 11 cm "" === showsPrec 11 (m :: Int) ""
         , show cm === show m
         , compare ea eb === compare a b
         , compare cm cn === compare m n
         , (ea == eb) === (a == b)
         , show (Custom ea cm) === ("Custom " ++ showsPrec 11 a " " ++ showsPrec 11 m "")
         , showsPrec 11 (Custom ea cm) "" === ("(Custom " ++ showsPrec 11 a " " ++ showsPrec 11 m ")")
         , compare (Custom ea cm) (Custom eb cn) === compare (a, m) (b, n)
         , unElementName ea === a
         , unCustomState cm === m
         ]
  where
    nameGen = oneof [elements ["bubble", "jelly", "nest", "a\"b", "中文", ""], arbitrary]

-- | 第 7 刀（7a）：Board 层的关卡级钩子（levelHooksWith 内置注册表 + 飞碟 / 传送门的元素值）与第 7 刀前的直接调用相同：
-- 补子后吸收 = stepUfos（吸走的格、移动后的飞碟），沉降传送 = portalTeleport（本体可穿门谓词取自注册表）。
qc_level_hooks_match_legacy :: Property
qc_level_hooks_match_legacy =
  forAll genPlayBoard $ \b ->
    forAll genMBoard $ \mb ->
      forAll (choose (0, 3) >>= \k -> vectorOf k (mkUfo <$> genPos <*> genColor)) $ \ufos ->
        forAll (choose (0, 3) >>= \k -> vectorOf k ((,) <$> genPos <*> genPos)) $ \portals ->
          let hooks = builtinHooks ufos portals
              (ps, hooks') = onAbsorb hooks b
          in conjoin
               [ (ps, levelUfos (hookLevel hooks')) === stepUfos b ufos
               , onSettle hooks mb === portalTeleport (portalWith defaultRegistry) portals mb
               , onSettle hooks' mb === onSettle hooks mb
               ]

-- | 第 7 刀（7a）：战役开局的关卡级元素 = 内置四种（注册顺序）+ 地面层；第 7 刀前的五个字段改为派生读数，
-- 写回同一值是恒等（相等与 Show 都不变），读数等于关卡记录（没有放置的飞碟 / 地毯按目标补齐）。
qc_level_elems_readers_roundtrip :: Property
qc_level_elems_readers_roundtrip =
  forAll genStart $ \(li, seed) -> case lookupLevel li of
    Nothing -> counterexample ("no level " ++ show li) False
    Just lvl ->
     let gs = levelGame li seed
         same gs' = gs' == gs .&&. show gs' === show gs
     in conjoin
         [ map levelNameOf (gsLevelElems gs) === ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop", "ground"]
         , same (setUfos (gsUfos gs) gs)
         , same (setBelts (gsBelts gs) gs)
         , same (setPortals (gsPortals gs) gs)
         , same (setCarpetOpen (gsCarpetOpen gs) gs)
         , same (setGround (gsGround gs) gs)
         , gsBelts gs === lvlBelts lvl
         , gsPortals gs === lvlPortals lvl
         , gsGround gs === lvlGround lvl
         , if null (lvlUfos lvl) then property True else gsUfos gs === lvlUfos lvl
         , if null (lvlCarpets lvl) then property True else gsCarpetOpen gs === lvlCarpets lvl
         ]

-- | 第 7 刀（7b）：步末表与第 7 刀前手写的步末流程（下面逐字保留的 legacySwapEnd / legacyBoosterEnd，只把皮带效果
-- 换成通用形状）逐段相同：各段连锁（终盘 / 轮次 / 计数 / 生成器 / 关卡级元素）、步末记录、终盘、地毯腾空盘面。
-- 局面 = 战役任意关 + 种子走 0–4 手之后；交换取提示，道具取随机格为种子。
qc_end_table_matches_legacy :: Property
qc_end_table_matches_legacy =
  forAll genStart $ \start ->
    forAll (choose (0, 4) >>= \k -> vectorOf k genPick) $ \picks ->
      let s0 = startState start
          gs0 = last (s0 : map stepState (snd (playPicks s0 picks)))
          (nr, nc) = boardDims (gsBoard gs0)
      in forAll ((,) <$> choose (0, nr - 1) <*> choose (0, nc - 1)) $ \seedPos ->
        let gs = gs0
            reg = defaultRegistry
            hooks0 = levelHooksWith reg (gsLevelElems gs)
            summary (segs, ends, board, vacate) =
              ( [(crBoard r, crWaves r, crTally r, show (crGen r), hookLevel (crHooks r)) | r <- NE.toList segs]
              , ends
              , board
              , vacate
              )
            segB = cascadeSeedsWith reg Nothing [seedPos] hooks0 (gsGen gs) (gsBoard gs)
            segS = [cascadeMatchesWith reg (Just b) hooks0 (gsGen gs) (swapCells (gsBoard gs) a b) | Just (a, b) <- [findHintWith reg (gsBoard gs)]]
            endsOf (_, e, _, _) = e
        in classify (any (not . null . endsOf . legacySwapEnd reg) segS) "swap has end effects" $
             conjoin [summary (runEndTable reg swapEndTable seg) === summary (legacySwapEnd reg seg) | seg <- segS]
               .&&. summary (runEndTable reg boosterEndTable segB) === summary (legacyBoosterEnd reg segB)

-- | 第 7 刀前 Resolve.swapEnd 的逐字副本（皮带效果改为通用形状）。
legacySwapEnd :: Registry -> CascadeRun StdGen -> (NonEmpty (CascadeRun StdGen), [EndStep], Board, Board)
legacySwapEnd reg seg0 =
  let ws0 = crWaves seg0
      board0' = crBoard seg0
      (tickSteps, seg1) = cascadeCountdownsTracedWith reg (crHooks seg0) (crGen seg0) board0'
      endTick = [EndStep (length ws0) bB bA e | (bB, bA, e) <- tickSteps]
      elems1 = hookLevel (crHooks seg1)
      (hasBelts, mvBelt, hooks1) = case beltShiftIn reg elems1 of
        Just (mv, es) -> (True, mv, levelHooksWith reg es)
        Nothing -> (False, [], crHooks seg1)
      boardCd = crBoard seg1
      boardBelt = applyBeltMoves boardCd mvBelt
      nBelt = length ws0 + length (crWaves seg1)
      endBelt =
        [ EndStep nBelt boardCd boardBelt (EndEffect EvBelt "belt" [EndItem o d (getCell boardCd o) Nothing | (o, d) <- mvBelt])
        | hasBelts
        , not (null mvBelt)
        ]
      seg2 =
        if not hasBelts
          then stillRun boardCd hooks1 (crGen seg1)
          else cascadeAfterWith reg AfterBelt hooks1 (crGen seg1) boardBelt
      boardBeltCas = crBoard seg2
      nEnd = nBelt + length (crWaves seg2)
      elems2 = hookLevel (crHooks seg2)
      avoid = nub (avoidCellsIn reg elems2)
      walls = nub (wallCellsIn reg elems2)
      (endSpread, boardSpread) = traceSpreadsWith reg nEnd boardBeltCas
      (endMove, boardSnail) = runPhase reg PhaseMove (EndCtx avoid walls (pushableWith reg)) nEnd boardSpread
      seg3 = cascadeAfterWith reg (AfterEnd (endHolesWith reg boardSnail)) (crHooks seg2) (crGen seg2) boardSnail
      board1 = crBoard seg3
  in (seg0 :| [seg1, seg2, seg3], endTick ++ endBelt ++ endSpread ++ endMove, board1, board1)

-- | 第 7 刀前 Resolve.boosterEnd 的逐字副本。
legacyBoosterEnd :: Registry -> CascadeRun StdGen -> (NonEmpty (CascadeRun StdGen), [EndStep], Board, Board)
legacyBoosterEnd reg seg0 =
  let boardH = crBoard seg0
      (ends, boardSp) = traceSpreadsWith reg (length (crWaves seg0)) boardH
      seg1 = cascadeAfterWith reg (AfterEnd (endHolesWith reg boardSp)) (crHooks seg0) (crGen seg0) boardSp
  in (seg0 :| [seg1], ends, crBoard seg1, boardH)

-- | 第 7 刀（7b）：askLevels / askLevelsIn 折叠所有回复者 = 按顺序把每个回复者的回复当作下一个的问题；
-- 用若干个「加常数」的测试元素随机注册（可含重复的数），结果 = 起始值 + 各回复者的数按注册顺序依次作用。
-- | 测试元素：名字由编号决定（adder1、adder5 …），状态是被问的次数。
data Adder = Adder Int Int
  deriving (Eq, Show)

newtype AdderMsg = AdderMsg [Int]

instance Message AdderMsg

instance LevelElement Adder where
  levelName (Adder k _) = ElementName ("adder" ++ show k)
  levelReply (Adder k n) msg = case fromMessage msg of
    Just (AdderMsg acc) -> Just (SomeMessage (AdderMsg (acc ++ [k])), Adder k (n + 1))
    Nothing -> Nothing

qc_ask_levels_folds_in_order :: Property
qc_ask_levels_folds_in_order =
  forAll (choose (0, 5) >>= \n -> vectorOf n (choose (1, 9 :: Int))) $ \ks0 ->
    let ks = nub ks0
        reg = foldl (\r k -> registerLevel (SomeLevelElement (Adder k 0)) r) defaultRegistry ks
        elems = [SomeLevelElement (Adder k 5) | k <- ks]
        viaReg = fmap (\(AdderMsg xs) -> xs) (askLevels reg (AdderMsg []))
        viaIn = fmap (\(AdderMsg xs, es) -> (xs, es)) (askLevelsIn reg elems (AdderMsg []))
        expected = if null ks then Nothing else Just ks
    in conjoin
         [ viaReg === expected
         , fmap fst viaIn === expected
         -- 每个回复者推进后的状态都写回（同名替换，顺序不变）
         , fmap snd viaIn === (if null ks then Nothing else Just [SomeLevelElement (Adder k 6) | k <- ks])
         ]

--------------------------------------------------------------------------------
-- 第 8 刀：规则表（形状 / 组合）与补子策略对照旧实现

-- | 第 8 刀前的 Board.Clear.spawnSpecials（逐字副本，只用来对照形状规则表）。
legacySpawnSpecials :: Maybe Pos -> [MatchRun] -> [Pos] -> [(Pos, Cell)]
legacySpawnSpecials prefer runs clearable =
  [ (pos, Gem (runColor run) kind 0 Nothing)
  | run <- runs
  , let n = length (runPos run)
  , n >= 4
  , let kind
          | n >= 5 = Rainbow
          | runIsH run = LineH
          | otherwise = LineV
        slots = filter (`elem` clearable) (runPos run)
  , pos <- take 1 $ case prefer of
      Just p | p `elem` slots -> [p]
      _ -> drop (length slots `div` 2) slots
  ]

-- | 第 8 刀前的 Combos.isSpecialCombo（逐字副本）。
legacyIsSpecialCombo :: Board -> Pos -> Pos -> Bool
legacyIsSpecialCombo b p1 p2 =
  kindCombo && specialActivates (getCell b p1) && specialActivates (getCell b p2)
  where
    kindCombo =
      Combos.isLineBombCombo b p1 p2
        || Combos.isRainbowLineCombo b p1 p2
        || Combos.isBombBombCombo b p1 p2
        || Combos.isLineLineCombo b p1 p2

-- | 第 8 刀前的 Combos.comboClearSeeds（逐字副本）。
legacyComboClearSeeds :: Board -> Pos -> Pos -> [Pos]
legacyComboClearSeeds b p1 p2
  | Combos.isBombBombCombo b p1 p2 || bothBombsAfter =
      nub (Combos.bigBomb b p1 ++ Combos.bigBomb b p2)
  | Combos.isLineLineCombo b p1 p2 || bothLinesAfter =
      nub (Combos.fullRowCol b p1 ++ Combos.fullRowCol b p2)
  | Combos.isLineBombCombo b p1 p2 || lineBombAfter =
      case (getCell b p1, getCell b p2) of
        (Gem _ Bomb _ _, _) -> Combos.lineBombCross b p1
        (_, Gem _ Bomb _ _) -> Combos.lineBombCross b p2
        _ -> nub (Combos.lineBombCross b p1 ++ Combos.lineBombCross b p2)
  | Combos.isRainbowLineCombo b p1 p2 = rainbowClearSeeds b p1 p2
  | otherwise = []
  where
    kinds = (kindOf (getCell b p1), kindOf (getCell b p2))
    kindOf cell = case cell of
      Gem _ k _ _ -> Just k
      _ -> Nothing
    isLine k = k == LineH || k == LineV
    bothBombsAfter = kinds == (Just Bomb, Just Bomb)
    bothLinesAfter = case kinds of
      (Just k1, Just k2) -> isLine k1 && isLine k2
      _ -> False
    lineBombAfter = case kinds of
      (Just Bomb, Just k) | isLine k -> True
      (Just k, Just Bomb) | isLine k -> True
      _ -> False

-- | 第 8 刀前的 Gravity.refill（逐字副本）。
legacyRefill :: StdGen -> MBoard -> (Board, StdGen)
legacyRefill g0 mb =
  let (filled, g') = fillList g0 [atM mb (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
  in (boardFromRows (chunks filled), g')
  where
    fillList g [] = ([], g)
    fillList g (Nothing : xs) =
      let (c, g1) = randomColor g
          (rest, g2) = fillList g1 xs
      in (mkGem c : rest, g2)
    fillList g (Just x : xs) =
      let (rest, g1) = fillList g xs
      in (x : rest, g1)
    chunks [] = []
    chunks xs = take boardSize xs : chunks (drop boardSize xs)

genPos :: Gen Pos
genPos = (,) <$> choose (0, boardSize - 1) <*> choose (0, boardSize - 1)

-- | 形状用例：两三色为主的盘面（常有 4 / 5 连与横竖交叉）、随机落点、连线格的随机子集当可清格。
genShapeCase :: Gen (Board, Maybe Pos, [Pos])
genShapeCase = do
  b <- boardFromRows <$> vectorOf boardSize (vectorOf boardSize (frequency [(12, mkGem <$> elements [C1, C2, C3]), (2, genGem), (1, genCell)]))
  let ps = nub (concatMap runPos (findMatchRunsWith defaultRegistry b))
  clearable <- filterM (const (frequency [(4, pure True), (1, pure False)])) ps
  prefer <- oneof ([pure Nothing, Just <$> genPos] ++ [Just <$> elements ps | not (null ps)])
  pure (b, prefer, clearable)

-- | 形状规则表：内置表 = [5 连彩虹, 横 4, 竖 4]，逐连线的产出（位置、种类、先后）与旧 spawnSpecials 逐字相同。
qc_shape_table_matches_legacy :: Property
qc_shape_table_matches_legacy =
  forAll genShapeCase $ \(b, prefer, clearable) ->
    let runs = findMatchRunsWith defaultRegistry b
        legacy = legacySpawnSpecials prefer runs clearable
    in cover 20 (not (null legacy)) "spawns a special" $
       cover 3 (length legacy >= 2) "spawns two or more" $
       conjoin
         [ map shapeName (shapeRules defaultRegistry) === ["line5→rainbow", "line4h→line_h", "line4v→line_v"]
         , spawnSpecialsWith defaultRegistry prefer runs clearable === legacySpawnSpecials prefer runs clearable
         , spawnByShapes builtinShapeRules prefer runs clearable === legacySpawnSpecials prefer runs clearable
         -- 空表不生成
         , spawnByShapes [] prefer runs clearable === []
         ]

-- | 交换两端的格：各种宝石（含冰 / 叠层 / 软锁）为主，也有障碍；p2 多半与 p1 相邻，偶尔是任意格（也可重合）。
genComboCase :: Gen (Board, Pos, Pos)
genComboCase = do
  b <- genPlayBoard
  p1@(r, c) <- genPos
  let nbrs = [q | q@(rr, cc) <- [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)], rr >= 0, rr < boardSize, cc >= 0, cc < boardSize]
  p2 <- frequency [(8, elements nbrs), (1, genPos)]
  let end =
        frequency
          [ (8, Gem <$> genColor <*> elements [Normal, LineH, LineV, Bomb, Rainbow] <*> frequency [(5, pure 0), (1, choose (1, 2))] <*> frequency [(5, pure Nothing), (1, Just <$> genOverlay)])
          , (1, genCell)
          ]
  c1 <- end
  c2 <- end
  pure (setCell (setCell b p1 c1) p2 c2, p1, p2)

-- | 组合表：成立判定与清种子（交换前 / 交换后两个盘面）与旧 isSpecialCombo / comboClearSeeds 逐字相同；
-- 并进成对交换规则后次序仍是 [10, 20]，交换起手（swapOpeningWith / swapFiresWith）与旧的两条规则相同。
qc_combo_table_matches_legacy :: Property
qc_combo_table_matches_legacy =
  forAll genComboCase $ \(b, p1, p2) ->
    let swapped = swapCells b p1 p2
        rules = comboRules defaultRegistry
        legacyOpening =
          listToMaybe ([rainbowClearSeeds swapped p1 p2 | isRainbowSwap b p1 p2] ++ [legacyComboClearSeeds swapped p1 p2 | legacyIsSpecialCombo b p1 p2])
    in cover 5 (legacyIsSpecialCombo b p1 p2) "combo fires" $
       cover 5 (not (null (legacyComboClearSeeds b p1 p2)) && not (legacyIsSpecialCombo b p1 p2)) "kinds match but soft-locked" $
       conjoin
         [ map comboName rules === ["bomb×bomb", "line×line", "line×bomb", "rainbow×line"]
         , map srOrder (swapRules defaultRegistry) === [10, 15, 20]
         , map srOrder (elementSwapRules defaultRegistry) === [10, 15]
         , comboFires rules b p1 p2 === legacyIsSpecialCombo b p1 p2
         , Combos.isSpecialCombo b p1 p2 === legacyIsSpecialCombo b p1 p2
         , conjoin [comboSeedsFor rules bb p1 p2 === legacyComboClearSeeds bb p1 p2 | bb <- [b, swapped]]
         , conjoin [Combos.comboClearSeeds bb p1 p2 === legacyComboClearSeeds bb p1 p2 | bb <- [b, swapped]]
         , swapOpeningWith defaultRegistry b swapped p1 p2 === legacyOpening
         , swapFiresWith defaultRegistry b p1 p2 === (isRainbowSwap b p1 p2 || legacyIsSpecialCombo b p1 p2)
         ]

-- | 组合表对称：交换两端（p1 ↔ p2）后，每条规则是否对得上、整张表是否成立不变，清种子的格集合不变。
qc_combo_table_symmetric :: Property
qc_combo_table_symmetric =
  forAll genComboCase $ \(b, p1, p2) ->
    let swapped = swapCells b p1 p2
        rules = comboRules defaultRegistry
        matched r q1 q2 = isJust (comboMatch [r] b q1 q2)
        seedSet bb q1 q2 = sort (nub (comboSeedsFor rules bb q1 q2))
    in conjoin
         [ conjoin [counterexample (comboName r) (matched r p1 p2 === matched r p2 p1) | r <- rules]
         , comboFires rules b p1 p2 === comboFires rules b p2 p1
         , conjoin [seedSet bb p1 p2 === seedSet bb p2 p1 | bb <- [b, swapped]]
         ]

-- | 补子策略：缺省策略（defaultRefill、colorsRefill numColors、Gravity.refill）与旧 refill 逐字相同——
-- 盘面与推进后的生成器都相同（随机数消费顺序不变）；内置关卡级元素不换策略（activeRefill = 注册表的缺省）。
qc_refill_policy_default_matches_legacy :: Int -> Property
qc_refill_policy_default_matches_legacy seed =
  forAll genMBoard $ \mb ->
    let g = mkStdGen seed
        (b0, g0) = legacyRefill g mb
        same (b1, g1) = b1 === b0 .&&. show g1 === show g0
        builtinPolicy = activeRefill defaultRegistry (builtinHooks [] [])
    in conjoin
         [ same (refillWith defaultRefill g mb)
         , same (refillWith (colorsRefill numColors) g mb)
         , same (refill g mb)
         , same (refillWith (activeRefill defaultRegistry noHooks) g mb)
         , same (refillWith builtinPolicy g mb)
         , refillName builtinPolicy === "random-gem"
         , refillName (refillPolicyWith defaultRegistry) === "random-gem"
         ]
