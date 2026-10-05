{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | 效果与架构（Haskell 特性第 3 项，docs/haskell-features/03-效果与架构.md）：连锁核心改写成只依赖能力类的程序
-- （Match3.Board.Cascade 的 *M 函数，能力见 Match3.Board.Effect）之后，
--
-- * 对外入口（纯解释器）的结果写死成指纹：终盘、计数、回放轮次、推进后的关卡级元素、推进后的生成器（show），
--   每个入口一个；倒计时另有步末记录、单轮另有一个指纹（生成时与删除前的逐字旧副本核对过）；
-- * 追踪解释器的结果与纯解释器相同，事件日志与回放 / 盘面自洽（日志里的轮次就是回放，补进的格子就是下一轮回放的终盘格子）；
-- * 第三种解释器：同一个程序解释成手写的 free monad 指令树，再用一个小解释器跑，结果与纯解释器相同。
--
-- 用例：全部战役关卡 × 2 个种子 × 3 个盘面（开局盘做一手能消的交换 / 同尺寸随机盘 / 开局静止盘），
-- 关卡级元素（飞碟、传送门、补子策略……）按该关接上；入口覆盖匹配连锁、带起始波次的匹配连锁、种子起手（含空种子）、
-- 皮带后、步末后（带空洞）、倒计时、单轮。
module Spec.Effects
  ( tests
  ) where

import Control.Monad (ap, liftM)
import Match3.Board.Cascade
import Match3.Board.Effect
import Match3.Board.Grid (swapCells)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Board.Phase (Phase(..), Stage, refillStage)
import Match3.Board.Random (randomBoardSized)
import Match3.Element.Mechanic (SomeMechanic)
import Match3.Types (boardAt)
import Match3.Board.Refill (RefillPolicy)
import Match3.Core
import Match3.Element.Level (levelHooksWith, levelWorldIn)
import Match3.Element.World (World)
import Spec.Support (digest, findMatchPair)
import System.Random (RandomGen, StdGen, mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "effects_entries_pinned" effects_entries_pinned
  , testCase "effects_traced_same_as_pure" effects_traced_same_as_pure
  , testCase "effects_free_interpreter_same_as_pure" effects_free_interpreter_same_as_pure
  ]

--------------------------------------------------------------------------------
-- 用例

-- | 用例：(标签, 本关注册表, 本关钩子, 起始盘面, 生成器)。
data Case = Case String World LevelHooks Board StdGen

cases :: [Case]
cases =
  [ Case (concat ["L", show li, " seed ", show seed, " ", name]) world hooks b (mkStdGen (1000 * li + seed))
  | li <- [0 .. levelCount - 1]
  , seed <- [1, 2]
  , Just gs <- [campaignGame li seed]
  , let elems = gsLevelElems gs
        world = levelWorldIn defaultWorld elems
        hooks = levelHooksWith world elems
        b0 = gsBoard gs
        (nr, nc) = boardDims b0
  , (name, b) <-
      [("swapped", swapCells b0 p q) | Just (p, q) <- [findMatchPair b0]]
        ++ [("random", fst (randomBoardSized nr nc (mkStdGen (seed + 77 * li)))), ("start", b0)]
  ]

-- | 一个连锁程序：对任何满足 MonadCascade 的 monad 都成立（所以能交给三种解释器）。
newtype Prog = Prog (forall m. MonadCascade m => m (Board, CascadeTally))

-- | 每个用例上的入口：(名字, 对外入口的结果, 程序)。
entries :: Case -> [(String, CascadeRun StdGen, Prog)]
entries (Case _ world hooks b g) =
  [ ("matches", cascadeMatchesWith world Nothing hooks g b, Prog (cascadeMatchesM world Nothing b))
  , ("matchesFrom 2", cascadeMatchesFromWith world 2 (Just (1, 1)) hooks g b, Prog (cascadeMatchesFromM world 2 (Just (1, 1)) b))
  , ("seeds", cascadeSeedsWith world Nothing seeds hooks g b, Prog (cascadeSeedsM world Nothing seeds b))
  , ("seeds []", cascadeSeedsWith world Nothing [] hooks g b, Prog (cascadeSeedsM world Nothing [] b))
  , ("after belt", cascadeAfterWith world AfterBelt hooks g b, Prog (cascadeAfterM world AfterBelt b))
  , ("after end", cascadeAfterWith world (AfterEnd holes) hooks g b, Prog (cascadeAfterM world (AfterEnd holes) b))
  , ("countdowns", cascadeCountdownsWith world hooks g b, Prog (snd <$> cascadeCountdownsM world b))
  ]
  where
    (nr, nc) = boardDims b
    seeds = [(nr `div` 2, nc `div` 2), (0, nc - 1)]
    holes = [(0, 1), (nr - 1, 2)]

-- | CascadeRun 的全部可比内容（钩子是函数记录，比它背后的关卡级元素；生成器比 show）。
type RunView = (Board, CascadeTally, [CascadeWave], [SomeMechanic], String)

view :: CascadeRun StdGen -> RunView
view r = (crBoard r, crTally r, crWaves r, hookLevel (crHooks r), show (crGen r))

--------------------------------------------------------------------------------
-- 1. 对外入口的固定结果

-- | 每个入口在全部用例上的结果（view 的 show，按用例顺序拼接）的指纹。
entryDigest :: String -> String
entryDigest name = digest (concat [show (view run) | c <- cases, (n, run, _) <- entries c, n == name])

-- | 倒计时的步末记录、单轮（stepCascadeAtWith，落点 (1,1)）在全部用例上的指纹。
countdownStepsDigest, singleRoundDigest :: String
countdownStepsDigest = digest (concat [show (fst (cascadeCountdownsTracedWith world hooks g b)) | Case _ world hooks b g <- cases])
singleRoundDigest =
  digest (concat [show (fmap (\(b', n, g') -> (b', n, show g')) (stepCascadeAtWith world (Just (1, 1)) g b)) | Case _ world _ b g <- cases])

effects_entries_pinned :: Assertion
effects_entries_pinned = do
  assertBool "enough cases" (length cases > 100)
  sequence_ [assertEqual ("entry " ++ name) d (entryDigest name) | (name, d) <- pinnedEntryDigests]
  assertEqual "entries covered" [n | c <- take 1 cases, (n, _, _) <- entries c] (map fst pinnedEntryDigests)
  assertEqual "countdown steps" pinnedCountdownSteps countdownStepsDigest
  assertEqual "single round" pinnedSingleRound singleRoundDigest

-- | 各入口的指纹（由现实现生成，生成时与删除前的逐字旧副本核对过：每个用例、每个入口逐项相等）。
pinnedEntryDigests :: [(String, String)]
pinnedEntryDigests =
  [ ("matches","994bfd325ed1da19")
  , ("matchesFrom 2","56aec5b0ec0d7d31")
  , ("seeds","025712d5d8a2c5a2")
  , ("seeds []","994bfd325ed1da19")
  , ("after belt","994bfd325ed1da19")
  , ("after end","38357190d3143a4e")
  , ("countdowns","16ae5a7e85fe0768")
  ]

pinnedCountdownSteps, pinnedSingleRound :: String
pinnedCountdownSteps = "bac34069ae0eb23a"
pinnedSingleRound = "5efc0aa1fd0a8019"

--------------------------------------------------------------------------------
-- 2. 追踪解释器

effects_traced_same_as_pure :: Assertion
effects_traced_same_as_pure = do
  stats <- concat <$> mapM one cases
  -- 用例确实走到了各种效果：吸收节拍真吸走过格、补子补过格、步末后的「只沉降」轮（补了子但不一定记回放）
  assertBool "some absorb beats took cells" (any (\(a, _, _) -> a > 0) stats)
  assertBool "some refills" (any (\(_, r, _) -> r > 0) stats)
  assertBool "some refills without a wave (settle-only, unchanged board)" (any (\(_, _, u) -> u > 0) stats)
  where
    one c@(Case tag _ hooks _ g) =
      sequence
        [ do
            let (run, logs) = runCascadeTraced hooks g prog
            assertEqual (tag ++ " / " ++ name ++ ": traced = pure") (view (runCascade hooks g prog)) (view run)
            assertEqual (tag ++ " / " ++ name ++ ": logged waves") (crWaves run) [w | LogWave w <- logs]
            -- 补子日志紧跟着的那一轮回放：补进的格子就是这一轮终盘上的格子，且恰好是下落后的空洞
            sequence_
              [ assertBool (tag ++ " / " ++ name ++ ": refill matches wave") (all (\(q, cell) -> boardAt (cwAfter w) q == cell) fills)
              | (LogRefill fills, LogWave w) <- zip logs (drop 1 logs)
              ]
            pure
              ( length [() | LogAbsorb ps <- logs, not (null ps)]
              , length [() | LogRefill fs <- logs, not (null fs)]
              , length [() | (LogRefill _, next) <- zip logs (drop 1 logs ++ [LogAbsorb []]), not (isWave next)]
              )
        | (name, _, Prog prog) <- entries c
        ]
    isWave e = case e of
      LogWave _ -> True
      _ -> False

--------------------------------------------------------------------------------
-- 3. 第三种解释器：free monad

-- | 连锁的指令集：每种能力一条指令，续体（continuation）接住指令的结果。
data CascadeF next
  = RefillF RefillPolicy (Stage 'Fallen) (Stage 'Full -> next)
  | HooksF (LevelHooks -> next)
  | AbsorbF Board ([Pos] -> next)
  | EmitF CascadeWave next
  deriving (Functor)

-- | 手写的 free monad（不加 free 包）：程序 = 指令树。
data Free f a = Done a | Step (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Applicative (Free f) where
  pure = Done
  (<*>) = ap

instance Functor f => Monad (Free f) where
  Done a >>= k = k a
  Step op >>= k = Step (fmap (>>= k) op)

liftF :: Functor f => f a -> Free f a
liftF = Step . fmap Done

-- | 能力 → 指令：同一个连锁程序在 Free CascadeF 上「运行」只是造出一棵指令树，什么都不执行。
instance MonadRefill (Free CascadeF) where
  refillHoles pol s = liftF (RefillF pol s id)

instance MonadLevelHooks (Free CascadeF) where
  currentHooks = liftF (HooksF id)
  absorbHooks b = liftF (AbsorbF b id)

instance MonadWaves (Free CascadeF) where
  emitWave w = liftF (EmitF w ())

-- | 指令树的解释器：逐条执行，生成器与钩子显式传递（这一处就是全部的手工传递）。
interpret :: RandomGen g => LevelHooks -> g -> Free CascadeF a -> (a, LevelHooks, [CascadeWave], g)
interpret hooks g prog = case prog of
  Done a -> (a, hooks, [], g)
  Step (RefillF pol s k) -> let (s', g') = refillStage pol g s in interpret hooks g' (k s')
  Step (HooksF k) -> interpret hooks g (k hooks)
  Step (AbsorbF b k) -> let (ps, hooks') = onAbsorb hooks b in interpret hooks' g (k ps)
  Step (EmitF w next) -> let (a, h, ws, g') = interpret hooks g next in (a, h, w : ws, g')

effects_free_interpreter_same_as_pure :: Assertion
effects_free_interpreter_same_as_pure =
  sequence_
    [ assertEqual (tag ++ " / " ++ name ++ ": free") (view new) (view (CascadeRun b' t h ws g'))
    | c@(Case tag _ hooks _ g) <- cases
    , (name, new, Prog prog) <- entries c
    , let ((b', t), h, ws, g') = interpret hooks g prog
    ]
