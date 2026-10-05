{-# LANGUAGE DataKinds #-}
-- 本模块故意含有几处类型错误（见下方「非法写法」一节）。-fdefer-type-errors 把它们推迟到运行时：
-- 模块照常编译，只有求值那几个绑定时才抛出 TypeError（消息正是编译器本来会报的错）。
-- -Wno-deferred-type-errors 关掉「推迟了一个类型错误」的警告，保持 0 警告；模块里其余代码与平时一样做完整类型检查。
-- -fdefer-type-errors 默认还会顺带推迟「名字不在作用域」和类型洞（_x），这里用 -fno-defer-out-of-scope-variables /
-- -fno-defer-typed-holes 关回去：拼错 / 漏导入的名字仍是编译错误，只有真正的类型错误被推迟（同 Spec.Classes）。
-- 这就是 should-not-typecheck 库的做法，这里不加依赖、手写一遍。
{-# OPTIONS_GHC -fdefer-type-errors -fno-defer-out-of-scope-variables -fno-defer-typed-holes -Wno-deferred-type-errors #-}

-- | 类型层（Haskell 特性第 1 项）：一轮连锁的阶段标签（Match3.Board.Phase）与起手阶段（Match3.Game.Resolve）。
--
-- * 合法路径：带标签的转换与原来的无标签函数逐格、逐随机数相同（标签不改变任何计算）。
-- * 非法写法：没下落就补子、下落两次、对有空洞的盘面再消、把有空洞的盘当满盘、交换两次、
--   锤子起手要普通匹配、锤子拿交换后的盘起手
--   都是类型错误（推迟到运行时求值才抛出，测试检查确实抛出、且消息点名了冲突的阶段）。
module Spec.Phase
  ( tests
  ) where

import Control.Exception (TypeError(..), evaluate, try)
import Data.List (isInfixOf)
import Match3.Board.Clear (clearMatchesDetailedWith)
import Match3.Board.Default (noHooks)
import Match3.Board.Gravity (settleDrainWith)
import Match3.Board.Grid (MBoard, swapCells)
import Match3.Board.Phase
import Match3.Board.Random (randomBoard)
import Match3.Board.Refill (defaultRefill, refillWith)
import Match3.Core
import Match3.Game.Boosters (resolveHammer)
import Match3.Game.Resolve (Opening(..), SMoveKind(..), resolveMove)
import Match3.Types (Score)
import Spec.Support (levelGame, stableBoard)
import System.Random (StdGen, mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "phase_round_same_as_untyped" phase_round_same_as_untyped
  , testCase "phase_swap_same_as_swapCells" phase_swap_same_as_swapCells
  , testCase "phase_typed_hammer_same_as_resolveHammer" phase_typed_hammer_same_as_resolveHammer
  , testCase "phase_illegal_transitions_do_not_typecheck" phase_illegal_transitions_do_not_typecheck
  ]

-- 合法路径 ---------------------------------------------------------------------

-- | 有三连的随机盘（randomBoard 不保证无三连）上跑一轮：消除 → 下落 → 补子，与无标签的三个函数逐项相同。
phase_round_same_as_untyped :: Assertion
phase_round_same_as_untyped =
  mapM_ one [1 .. 40]
  where
    reg = defaultWorld
    one seed = do
      let (b, g) = randomBoard (mkStdGen seed)
          -- 带阶段标签
          (cleared, n, pos) = clearStage (clearMatchesDetailedWith reg Nothing) (fullStage b)
          (fallen, drained) = fallStage reg noHooks cleared
          (filled, g1) = refillStage defaultRefill g fallen
          -- 改动前的写法（三个 MBoard / Board 之间没有阶段区分）
          (mb, n', pos') = clearMatchesDetailedWith reg Nothing b
          (mb2, drained') = settleDrainWith reg noHooks mb
          (b', g1') = refillWith defaultRefill g mb2
          tag = "seed " ++ show seed
      assertEqual (tag ++ ": cleared") (rows mb) (rows (stageGrid cleared))
      assertEqual (tag ++ ": count / cells") (n', pos') (n, pos)
      assertEqual (tag ++ ": fallen") (rows mb2) (rows (stageGrid fallen))
      assertEqual (tag ++ ": drained") drained' drained
      assertEqual (tag ++ ": refilled") b' (stageGrid filled)
      assertEqual (tag ++ ": generator") (show g1') (show (g1 :: StdGen))
    rows :: MBoard -> [[Maybe Cell]]
    rows = mboardRows

phase_swap_same_as_swapCells :: Assertion
phase_swap_same_as_swapCells =
  assertEqual "swap" (swapCells stableBoard (2, 3) (2, 4)) (stageBoard (swapStage (2, 3) (2, 4) (fullStage stableBoard)))

-- | 直接用类型化入口（SKindHammer + Stage 'Full + OpenSeeds）结算，与 resolveHammer 完全相同。
phase_typed_hammer_same_as_resolveHammer :: Assertion
phase_typed_hammer_same_as_resolveHammer = do
  let gs = levelGame 0 7
      p = (4, 4)
      (g1, o1, t1) = resolveMove SKindHammer (fullStage (gsBoard gs)) (OpenSeeds Nothing [p]) gs
      (g2, o2, t2) = resolveHammer p gs
  assertEqual "state" (show g2) (show g1)
  assertEqual "outcome" o2 o1
  assertEqual "trace" (show t2) (show t1)

-- 非法写法（类型错误，推迟到运行时） ----------------------------------------------

clearedSample :: Stage 'Cleared
clearedSample = fst3 (clearStage (clearMatchesDetailedWith defaultWorld Nothing) (fullStage stableBoard))
  where fst3 (a, _, _) = a

fallenSample :: Stage 'Fallen
fallenSample = fst (fallStage defaultWorld noHooks clearedSample)

-- | 没下落就补子：refillStage 要 Stage 'Fallen，给的是 Stage 'Cleared。
bad_refillBeforeFall :: Board
bad_refillBeforeFall = stageGrid (fst (refillStage defaultRefill (mkStdGen 1) clearedSample))

-- | 下落两次：fallStage 要 Stage 'Cleared，给的是 Stage 'Fallen。
bad_fallTwice :: MBoard
bad_fallTwice = stageGrid (fst (fallStage defaultWorld noHooks fallenSample))

-- | 有空洞的盘面再消一次：clearStage 要 Stage 'Full。
bad_clearWithHoles :: MBoard
bad_clearWithHoles = stageGrid (fst3 (clearStage (clearMatchesDetailedWith defaultWorld Nothing) clearedSample))
  where fst3 (a, _, _) = a

-- | 把有空洞的盘面当满盘取出：IsFull 'Cleared 化简成 MBoard ~ Board，不成立。
bad_holesAsBoard :: Board
bad_holesAsBoard = stageBoard clearedSample

-- | 交换两次：swapStage 要 Stage 'Full，给的是 Stage 'Swapped（交换后的盘要先结算回到满盘才能再交换）。
bad_swapTwice :: Board
bad_swapTwice = stageBoard (swapStage (2, 3) (2, 4) (swapStage (2, 3) (2, 4) (fullStage stableBoard)))

-- | 锤子（起手阶段 'Full）却要求普通匹配起手（只有 Opening 'Swapped）。
bad_hammerOpenMatch :: Score
bad_hammerOpenMatch =
  let gs = levelGame 0 7
      (g, _, _) = resolveMove SKindHammer (fullStage (gsBoard gs)) (OpenMatch Nothing) gs
  in gsScore g

-- | 锤子拿交换后的盘起手：StartPhase 'KindHammer = 'Full，给的是 Stage 'Swapped。
bad_hammerOnSwapped :: Score
bad_hammerOnSwapped =
  let gs = levelGame 0 7
      (g, _, _) = resolveMove SKindHammer (swapStage (4, 4) (4, 5) (fullStage (gsBoard gs))) (OpenSeeds Nothing [(4, 4)]) gs
  in gsScore g

phase_illegal_transitions_do_not_typecheck :: Assertion
phase_illegal_transitions_do_not_typecheck = do
  rejects "refill before fall" ["Fallen", "Cleared"] (length (boardRows bad_refillBeforeFall))
  rejects "fall twice" ["Cleared", "Fallen"] (length (mboardRows bad_fallTwice))
  rejects "clear with holes" ["Full", "Cleared"] (length (mboardRows bad_clearWithHoles))
  rejects "holes as board" ["Board"] (length (boardRows bad_holesAsBoard))
  rejects "swap twice" ["Swapped", "Full"] (length (boardRows bad_swapTwice))
  rejects "hammer + OpenMatch" ["Swapped", "Full"] bad_hammerOpenMatch
  rejects "hammer on swapped board" ["Swapped", "Full"] bad_hammerOnSwapped
  where
    rejects what mentions v = do
      r <- try (evaluate v)
      case r of
        Left (TypeError msg) ->
          assertBool (what ++ ": message should mention " ++ show mentions ++ "\n" ++ msg) (all (`isInfixOf` msg) mentions)
        Right _ -> assertFailure (what ++ ": expected a (deferred) type error, but it evaluated")
