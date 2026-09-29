-- | 段 5：双层果冻（地面层）与气泡（占格本体）。两者是内置元素，但只经注册表（Element.Builtin 的定义）
-- 与白名单钩子（SlotGround / groundRule、onHit / adjacentRule、CountNamed / GoalNamed）接入；
-- 主流程源码里没有它们的名字（jb_main_flow_untouched_scan）。设定见 docs/domain.md「双层果冻与气泡」。
module Spec.JellyBubble
  ( tests
  ) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Match3.Board.Match (findHintWith)
import Match3.Core
import Match3.Element
import Engine.Game (Game(..), Step(..))
import Match3.Game.Move (resolveSwapWith)
import qualified Match3.Engine as M3E
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "jb_jelly_two_layers_counted_per_layer" jb_jelly_two_layers_counted_per_layer
  , testCase "jb_jelly_goal_wins_on_last_layer" jb_jelly_goal_wins_on_last_layer
  , testCase "jb_jelly_keeps_on_shuffle_and_undo" jb_jelly_keeps_on_shuffle_and_undo
  , testCase "jb_bubble_pops_on_adjacent_clear" jb_bubble_pops_on_adjacent_clear
  , testCase "jb_bubble_pops_on_direct_hit" jb_bubble_pops_on_direct_hit
  , testCase "jb_bubble_blocks_swap_falls_no_match" jb_bubble_blocks_swap_falls_no_match
  , testCase "jb_levels_appended" jb_levels_appended
  , testCase "jb_main_flow_untouched_scan" jb_main_flow_untouched_scan
  ]

-- | 交换 (1,2)↔(2,2) 后第 1 行 (1,0)–(1,3) 是 C5 四连（同 Spec.Extension 的盘）。
tripleBoard :: Board
tripleBoard = foldl (\b (p, c) -> setCell b p c) stableBoard [((1, 0), mkGem C5), ((1, 1), mkGem C5)]

tripleMove :: (Pos, Pos)
tripleMove = ((1, 2), (2, 2))

allPos :: [Pos]
allPos = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

bubblesOn :: Board -> [Pos]
bubblesOn b = [p | p <- allPos, getCell b p == Custom "bubble" 1]

isWin :: Maybe Outcome -> Bool
isWin o = case o of
  Just (Won _) -> True
  Just (LevelClear _ _) -> True
  _ -> False

-- | 上方格子每被消除一次去一层、每层计 1；没被消到的果冻不动；不占格（盘面与无果冻时相同）。
jb_jelly_two_layers_counted_per_layer :: Assertion
jb_jelly_two_layers_counted_per_layer = do
  let ground0 = [((1, 1), ("jelly", 2)), ((6, 6), ("jelly", 2))]
      gs0 = (newGame (GameConfig 5 (GoalNamed "jelly" 4)) 1) {gsBoard = tripleBoard, gsGround = ground0}
      (p1, p2) = tripleMove
      (gs1, o1, _) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertBool "move applied" (moveApplied o1)
  assertEqual "one layer peeled at (1,1)" [((1, 1), ("jelly", 1)), ((6, 6), ("jelly", 2))] (gsGround gs1)
  assertEqual "counted per layer" [("jelly", 1)] (gsElementCounts gs1)
  assertEqual "goal progress" 1 (gsCollected gs1)
  assertEqual "jelly does not occupy the board" (gsBoard (fst (trySwap p1 p2 gs0 {gsGround = []}))) (gsBoard gs1)
  let (gs2, _, _) = resolveSwapWith defaultRegistry p1 p2 gs1 {gsBoard = tripleBoard}
  assertEqual "second clear removes the tile" [((6, 6), ("jelly", 2))] (gsGround gs2)
  assertEqual "count accumulates" [("jelly", 2)] (gsElementCounts gs2)
  -- 通用接口入口一样
  let st = gameStep M3E.match3Game gs0 (M3E.Swap p1 p2)
  assertEqual "engine path same ground" (gsGround gs1) (gsGround (stepState st))

-- | 最后一层去掉即达成 GoalNamed "jelly"。
jb_jelly_goal_wins_on_last_layer :: Assertion
jb_jelly_goal_wins_on_last_layer = do
  let gs0 = (newGame (GameConfig 5 (GoalNamed "jelly" 2)) 1) {gsBoard = tripleBoard, gsGround = [((1, 1), ("jelly", 2))]}
      (p1, p2) = tripleMove
      (gs1, _, _) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertBool "not yet" (gsOver gs1 == Nothing)
  let (gs2, _, _) = resolveSwapWith defaultRegistry p1 p2 gs1 {gsBoard = tripleBoard}
  assertBool "won on last layer" (isWin (gsOver gs2))
  assertEqual "ground empty" [] (gsGround gs2)

-- | 洗牌不动果冻，撤销恢复层数。
jb_jelly_keeps_on_shuffle_and_undo :: Assertion
jb_jelly_keeps_on_shuffle_and_undo = do
  let gs0 = newGameAtLevel 38 (levelConfig (allLevels !! 38)) 3
      g0 = gsGround gs0
  assertEqual "16 double-layer tiles" 16 (length [() | (_, ("jelly", 2)) <- g0])
  let stS = gameStep M3E.match3Game gs0 M3E.Shuffle
  assertEqual "shuffle keeps ground" g0 (gsGround (stepState stS))
  case findHintWith defaultRegistry (gsBoard gs0) of
    Nothing -> assertFailure "level 39 should be playable"
    Just (a, b) -> assertEqual "undo restores ground" (Just g0) (gsGround <$> stepThenUndo defaultRegistry gs0 (M3E.Swap a b))

-- | 邻格真消除（任意颜色）一次即破，计 CountNamed "bubble"；不相邻的不破。
jb_bubble_pops_on_adjacent_clear :: Assertion
jb_bubble_pops_on_adjacent_clear = do
  let board0 = foldl (\b p -> setCell b p (Custom "bubble" 1)) tripleBoard [(0, 1), (6, 6)]
      gs0 = (newGame (GameConfig 5 (GoalNamed "bubble" 1)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertBool "move applied" (moveApplied o1)
  assertBool "adjacent bubble cleared in the first wave" ((0, 1) `elem` cwCleared (head (mtWaves mt1)))
  assertEqual "far bubble stays" [(6, 6)] (bubblesOn (gsBoard gs1))
  assertEqual "counted" [("bubble", 1)] (gsElementCounts gs1)
  assertBool "goal reached" (isWin (gsOver gs1))

-- | 直接命中（锤子）也破并计数。
jb_bubble_pops_on_direct_hit :: Assertion
jb_bubble_pops_on_direct_hit = do
  let gs0 = (newGame (GameConfig 5 (GoalNamed "bubble" 5)) 1) {gsBoard = setCell stableBoard (4, 4) (Custom "bubble" 1), gsHammers = 1}
      (gs1, o1) = useHammer (4, 4) gs0
  assertBool "hammer applied" (moveApplied o1)
  assertEqual "bubble gone" [] (bubblesOn (gsBoard gs1))
  assertEqual "counted" [("bubble", 1)] (gsElementCounts gs1)

-- | 挡交换；无色不成三连；随重力下落（下方格被消除后落一格）。
jb_bubble_blocks_swap_falls_no_match :: Assertion
jb_bubble_blocks_swap_falls_no_match = do
  let bRow = foldl (\b p -> setCell b p (Custom "bubble" 1)) stableBoard [(5, 2), (5, 3), (5, 4)]
  assertBool "three bubbles in a row are not a match" (not (hasAnyMatch bRow))
  let gsR = (newGame defaultConfig 1) {gsBoard = bRow}
      (_, oR) = trySwap (5, 2) (4, 2) gsR
  assertBool "swap with a bubble rejected" (not (moveApplied oR))
  -- 不相邻、下方也没被消除的气泡原地不动：(0,5) 所在的列 5 本轮不消
  let board1 = setCell tripleBoard (0, 5) (Custom "bubble" 1)
      board2 = setCell (setCell board1 (1, 5) (mkGem C1)) (2, 5) (mkGem C2)
      gs0 = (newGame (GameConfig 5 (GoalScore 99999)) 1) {gsBoard = board2}
      (_, _, mt) = resolveSwapWith defaultRegistry (1, 2) (2, 2) gs0
  assertEqual "untouched column: bubble stays" (Custom "bubble" 1) (getCell (cwAfter (head (mtWaves mt))) (0, 5))
  -- 下落：第 3 行 (3,0)(3,1)(3,2) 成三连被清；气泡放在 (1,0)（与清除格隔一格，不被波及），
  -- 清掉 (3,0) 后 (2,0)、(1,0) 各落一格，气泡到 (2,0)
  let bF0 = foldl (\b (p, c) -> setCell b p c) stableBoard [((3, 0), mkGem C2), ((3, 1), mkGem C2), ((1, 0), Custom "bubble" 1)]
      gsF = (newGame (GameConfig 5 (GoalScore 99999)) 1) {gsBoard = bF0}
  case [(a, b) | (a, b) <- [((3, 2), (2, 2)), ((3, 2), (4, 2))], moveApplied (snd (trySwap a b gsF))] of
    ((a, b) : _) -> do
      let (_, _, mtF) = resolveSwapWith defaultRegistry a b gsF
      assertEqual "bubble fell one row" (Custom "bubble" 1) (getCell (cwAfter (head (mtWaves mtF))) (2, 0))
    [] -> assertFailure "fixture: no move completes row 3"

-- | 追加在 38 关之后：第 39 关果冻、第 40 关气泡；目标按元素名；前 38 关的名字 / 目标不变（金标准另外逐字锁定）。
jb_levels_appended :: Assertion
jb_levels_appended = do
  assertEqual "40 levels" 40 (length allLevels)
  let l39 = allLevels !! 38
      l40 = allLevels !! 39
  assertEqual "L39 goal" (GoalNamed "jelly" 32) (lvlGoal l39)
  assertEqual "L40 goal" (GoalNamed "bubble" 12) (lvlGoal l40)
  forM_ [1, 2, 3 :: Int] $ \seed -> do
    let g39 = newGameAtLevel 38 (levelConfig l39) seed
        g40 = newGameAtLevel 39 (levelConfig l40) seed
    assertEqual "L39 layers = goal" 32 (sum [n | (_, ("jelly", n)) <- gsGround g39])
    assertEqual "L40 bubbles = goal" 12 (length (bubblesOn (gsBoard g40)))
    assertBool "L39 playable" (findHintWith defaultRegistry (gsBoard g39) /= Nothing)
    assertBool "L40 playable" (findHintWith defaultRegistry (gsBoard g40) /= Nothing)
  assertBool "earlier levels have no ground" (all (\li -> null (gsGround (newGameAtLevel li (levelConfig (allLevels !! li)) 1))) [0 .. 37])
  assertBool "earlier levels have no bubbles" (all (\li -> null (bubblesOn (gsBoard (newGameAtLevel li (levelConfig (allLevels !! li)) 1)))) [0 .. 37])

-- | 主流程没有为这两个元素改代码：规则流水线与通用层源码里没有 "jelly" / "bubble"；它们只出现在
-- 元素定义（Element.Builtin）和关卡数据（Types 的关卡表、Game.Level 的放置 / 地面表）里。
jb_main_flow_untouched_scan :: Assertion
jb_main_flow_untouched_scan = do
  let mainFlow =
        [ "src/Match3/Board/" ++ m ++ ".hs" | m <- ["Cascade", "Clear", "Gravity", "Match", "Grid", "Random", "Default"] ]
          ++ [ "src/Match3/Game/" ++ m ++ ".hs" | m <- ["Resolve", "Move", "Boosters", "Trace", "Tally", "Shuffle", "State", "Outcome"] ]
          ++ [ "src/Match3/Element/" ++ m ++ ".hs" | m <- ["Registry", "Types", "Event"] ]
          ++ [ "src/Match3/Engine.hs", "src/Engine/Game.hs", "src/Engine/History.hs", "src/Engine/Effect.hs", "src/Engine/Playback.hs" ]
  srcs <- mapM readFile mainFlow
  let bad = [f | (f, s) <- zip mainFlow srcs, "jelly" `isInfixOf` s || "bubble" `isInfixOf` s]
  assertEqual "no jelly / bubble in the main flow" [] bad
  builtin <- readFile "src/Match3/Element/Builtin.hs"
  assertBool "both defined in Element.Builtin" ("\"jelly\"" `isInfixOf` builtin && "\"bubble\"" `isInfixOf` builtin)
  assertBool "both registered" (all (`elem` map entryName (registryDefs defaultRegistry)) ["jelly", "bubble"])
