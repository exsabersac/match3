{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | 效果与架构（Haskell 特性第 3 项，docs/haskell-features/03-效果与架构.md）：连锁核心改写成只依赖能力类的程序
-- （Match3.Board.Cascade 的 *M 函数，能力见 Match3.Board.Effect）之后，
--
-- * 对外入口（纯解释器）与第 3 项前手工传递生成器 / 钩子的写法（Spec.Support.LegacyCascade，逐字副本）逐项相同：
--   终盘、计数、回放轮次、推进后的关卡级元素、推进后的生成器；倒计时另比步末记录；
-- * 追踪解释器的结果与纯解释器相同，事件日志与回放 / 盘面自洽（日志里的轮次就是回放，补进的格子就是下一轮回放的终盘格子）；
-- * 第三种解释器：同一个程序解释成手写的 free monad 指令树，再用一个小解释器跑，结果仍与旧写法相同。
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
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Board.Phase (Phase(..), Stage, refillStage)
import Match3.Board.Random (randomBoardSized)
import Match3.Types (boardAt)
import Match3.Board.Refill (RefillPolicy)
import Match3.Core
import Match3.Element (defaultRegistry)
import Match3.Element.Level (levelHooksWith, levelRegistryIn)
import Match3.Element.Registry (Registry)
import qualified Spec.Support.LegacyCascade as Old
import Spec.Support (findMatchPair)
import System.Random (RandomGen, StdGen, mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "effects_entries_same_as_legacy" effects_entries_same_as_legacy
  , testCase "effects_traced_same_as_pure" effects_traced_same_as_pure
  , testCase "effects_free_interpreter_same_as_legacy" effects_free_interpreter_same_as_legacy
  ]

--------------------------------------------------------------------------------
-- 用例

-- | 用例：(标签, 本关注册表, 本关钩子, 起始盘面, 生成器)。
data Case = Case String Registry LevelHooks Board StdGen

cases :: [Case]
cases =
  [ Case (concat ["L", show li, " seed ", show seed, " ", name]) reg hooks b (mkStdGen (1000 * li + seed))
  | li <- [0 .. levelCount - 1]
  , seed <- [1, 2]
  , Just gs <- [campaignGame li seed]
  , let elems = gsLevelElems gs
        reg = levelRegistryIn defaultRegistry elems
        hooks = levelHooksWith reg elems
        b0 = gsBoard gs
        (nr, nc) = boardDims b0
  , (name, b) <-
      [("swapped", swapCells b0 p q) | Just (p, q) <- [findMatchPair b0]]
        ++ [("random", fst (randomBoardSized nr nc (mkStdGen (seed + 77 * li)))), ("start", b0)]
  ]

-- | 一个连锁程序：对任何满足 MonadCascade 的 monad 都成立（所以能交给三种解释器）。
newtype Prog = Prog (forall m. MonadCascade m => m (Board, CascadeTally))

-- | 每个用例上的入口：(名字, 旧写法的结果, 对外入口的结果, 程序)。
entries :: Case -> [(String, CascadeRun StdGen, CascadeRun StdGen, Prog)]
entries (Case _ reg hooks b g) =
  [ ("matches", Old.cascadeMatchesWith reg Nothing hooks g b, cascadeMatchesWith reg Nothing hooks g b, Prog (cascadeMatchesM reg Nothing b))
  , ("matchesFrom 2", Old.cascadeMatchesFromWith reg 2 (Just (1, 1)) hooks g b, cascadeMatchesFromWith reg 2 (Just (1, 1)) hooks g b, Prog (cascadeMatchesFromM reg 2 (Just (1, 1)) b))
  , ("seeds", Old.cascadeSeedsWith reg Nothing seeds hooks g b, cascadeSeedsWith reg Nothing seeds hooks g b, Prog (cascadeSeedsM reg Nothing seeds b))
  , ("seeds []", Old.cascadeSeedsWith reg Nothing [] hooks g b, cascadeSeedsWith reg Nothing [] hooks g b, Prog (cascadeSeedsM reg Nothing [] b))
  , ("after belt", Old.cascadeAfterWith reg AfterBelt hooks g b, cascadeAfterWith reg AfterBelt hooks g b, Prog (cascadeAfterM reg AfterBelt b))
  , ("after end", Old.cascadeAfterWith reg (AfterEnd holes) hooks g b, cascadeAfterWith reg (AfterEnd holes) hooks g b, Prog (cascadeAfterM reg (AfterEnd holes) b))
  , ("countdowns", Old.cascadeCountdownsWith reg hooks g b, cascadeCountdownsWith reg hooks g b, Prog (snd <$> cascadeCountdownsM reg b))
  ]
  where
    (nr, nc) = boardDims b
    seeds = [(nr `div` 2, nc `div` 2), (0, nc - 1)]
    holes = [(0, 1), (nr - 1, 2)]

-- | CascadeRun 的全部可比内容（钩子是函数记录，比它背后的关卡级元素；生成器比 show）。
type RunView = (Board, CascadeTally, [CascadeWave], [SomeLevelElement], String)

view :: CascadeRun StdGen -> RunView
view r = (crBoard r, crTally r, crWaves r, hookLevel (crHooks r), show (crGen r))

--------------------------------------------------------------------------------
-- 1. 对外入口 = 旧写法

effects_entries_same_as_legacy :: Assertion
effects_entries_same_as_legacy = do
  assertBool "enough cases" (length cases > 100)
  mapM_ one cases
  where
    one c@(Case tag reg hooks b g) = do
      sequence_ [assertEqual (tag ++ " / " ++ name) (view old) (view new) | (name, old, new, _) <- entries c]
      -- 倒计时：步末记录也相同
      let (stepsOld, _) = Old.cascadeCountdownsTracedWith reg hooks g b
          (stepsNew, _) = cascadeCountdownsTracedWith reg hooks g b
      assertBool (tag ++ " / countdown steps") (stepsOld == stepsNew)
      -- 单轮
      let single = fmap (\(b', n, g') -> (b', n, show g'))
      assertEqual (tag ++ " / single round") (single (Old.stepCascadeAtWith reg (Just (1, 1)) g b)) (single (stepCascadeAtWith reg (Just (1, 1)) g b))

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
            assertEqual (tag ++ " / " ++ name ++ ": traced = legacy") (view old) (view run)
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
        | (name, old, _, Prog prog) <- entries c
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

-- | 指令树的解释器：逐条执行，生成器与钩子显式传递（这正是第 3 项前连锁代码手写的样子，现在只写这一处）。
interpret :: RandomGen g => LevelHooks -> g -> Free CascadeF a -> (a, LevelHooks, [CascadeWave], g)
interpret hooks g prog = case prog of
  Done a -> (a, hooks, [], g)
  Step (RefillF pol s k) -> let (s', g') = refillStage pol g s in interpret hooks g' (k s')
  Step (HooksF k) -> interpret hooks g (k hooks)
  Step (AbsorbF b k) -> let (ps, hooks') = onAbsorb hooks b in interpret hooks' g (k ps)
  Step (EmitF w next) -> let (a, h, ws, g') = interpret hooks g next in (a, h, w : ws, g')

effects_free_interpreter_same_as_legacy :: Assertion
effects_free_interpreter_same_as_legacy =
  sequence_
    [ assertEqual (tag ++ " / " ++ name ++ ": free") (view old) (view (CascadeRun b' t h ws g'))
    | c@(Case tag _ hooks _ g) <- cases
    , (name, old, _, Prog prog) <- entries c
    , let ((b', t), h, ws, g') = interpret hooks g prog
    ]
