{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 连锁的效果层（Haskell 特性第 3 项「效果与架构」，见 docs/haskell-features/03-效果与架构.md）。
--
-- 连锁核心（Match3.Board.Cascade）只用到三种效果，各是一个能力类（mtl 风格的「自定义能力」，不绑定具体 monad）：
--
-- * 'MonadRefill' —— 补子：按策略补满下落后的空洞。这是连锁里**唯一**消耗随机数的地方；
-- * 'MonadLevelHooks' —— 关卡级钩子：读当前钩子（沉降节拍、补子策略），以及整轮吸收（会推进钩子的状态）；
-- * 'MonadWaves' —— 发出一轮回放记录（'CascadeWave'）。
--
-- 连锁的每个函数只写 @(MonadRefill m, MonadLevelHooks m, MonadWaves m) => ... -> m r@，
-- 由调用方选解释器（「纯核心 + 可替换效果」）：
--
-- * 'PureCascade' —— 纯解释器：生成器、钩子、回放都放在 State 里，结果与第 3 项前手工传递 g / hooks 的写法逐项相同；
--   Cascade 对外的 @... -> g -> Board -> CascadeRun g@ 入口全部经它运行，签名不变；
-- * 'TracedCascade' —— 追踪解释器：在纯解释器之上再叠一层（transformers 的 StateT），把每个效果记成一条事件日志
--   （'CascadeLog'：补了哪些格、吸收节拍吸走了什么、发出了哪一轮），结果与纯解释器完全相同。
--
-- 依赖：transformers（State / StateT）、Board.Phase（阶段标签）、Board.Refill、Board.Hooks、Board.Wave。
-- 本模块不依赖注册表与具体元素。
module Match3.Board.Effect
  ( -- * 能力
    MonadRefill(..)
  , MonadLevelHooks(..)
  , MonadWaves(..)
  , MonadCascade
    -- * 运行结果
  , Ran(..)
    -- * 解释器 1：纯
  , PureCascade
  , runPureCascade
    -- * 解释器 2：追踪
  , TracedCascade
  , CascadeLog(..)
  , runTracedCascade
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, StateT, get, gets, modify, put, runState, runStateT)
import Data.Array (assocs)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Board.Phase (Phase(..), Stage, refillStage, stageGrid)
import Match3.Board.Refill (RefillPolicy)
import Match3.Board.Wave (CascadeWave)
import Match3.Types
import System.Random (RandomGen)

--------------------------------------------------------------------------------
-- 能力

-- | 补子能力：按策略把下落后的空洞补满（行优先，每个空洞问一次策略）。阶段标签保证只能补「下落后」的盘面。
class Monad m => MonadRefill m where
  refillHoles :: RefillPolicy -> Stage 'Fallen -> m (Stage 'Full)

-- | 关卡级钩子能力。
class Monad m => MonadLevelHooks m where
  -- | 当前钩子（沉降节拍 onSettle、补子策略 hookRefill 读它；只读）。
  currentHooks :: m LevelHooks
  -- | 整轮吸收节拍（钩子 onAbsorb）：给出补子后的盘面，返回被吸走的格（空 = 没有吸收轮），并推进钩子。
  absorbHooks :: Board -> m [Pos]

-- | 发出一轮回放记录（按发生顺序）。
class Monad m => MonadWaves m where
  emitWave :: CascadeWave -> m ()

-- | 连锁要的全部能力。
type MonadCascade m = (MonadRefill m, MonadLevelHooks m, MonadWaves m)

-- | 运行完一段连锁程序之后的全部输出：程序的返回值、推进后的钩子、按顺序发出的回放、推进后的生成器。
data Ran g a = Ran
  { ranValue :: a
  , ranHooks :: LevelHooks
  , ranWaves :: [CascadeWave]
  , ranGen   :: g
  }

--------------------------------------------------------------------------------
-- 解释器 1：纯

data PureSt g = PureSt
  { psGen      :: g
  , psHooks    :: LevelHooks
  , psWavesRev :: [CascadeWave]  -- 反向累积，收尾时反转
  }

-- | 纯解释器：State (生成器, 钩子, 已发出的回放)。
newtype PureCascade g a = PureCascade (State (PureSt g) a)
  deriving newtype (Functor, Applicative, Monad)

instance RandomGen g => MonadRefill (PureCascade g) where
  refillHoles pol fallen = PureCascade $ do
    st <- get
    let (filled, g') = refillStage pol (psGen st) fallen
    put st {psGen = g'}
    pure filled

instance MonadLevelHooks (PureCascade g) where
  currentHooks = PureCascade (gets psHooks)
  absorbHooks b = PureCascade $ do
    st <- get
    let (absorbed, hooks') = onAbsorb (psHooks st) b
    put st {psHooks = hooks'}
    pure absorbed

instance MonadWaves (PureCascade g) where
  emitWave w = PureCascade (modify (\st -> st {psWavesRev = w : psWavesRev st}))

-- | 从给定的钩子与生成器出发运行一段连锁程序。
runPureCascade :: LevelHooks -> g -> PureCascade g a -> Ran g a
runPureCascade hooks g (PureCascade m) =
  let (a, st) = runState m (PureSt g hooks [])
  in Ran a (psHooks st) (reverse (psWavesRev st)) (psGen st)

--------------------------------------------------------------------------------
-- 解释器 2：追踪

-- | 事件日志的一条（按效果发生的顺序）。
data CascadeLog
  = LogRefill [(Pos, Cell)]  -- ^ 补子：补进的 (格, 新格子)，行优先
  | LogAbsorb [Pos]          -- ^ 整轮吸收节拍：被吸走的格（空 = 这一拍没有吸收）
  | LogWave CascadeWave      -- ^ 发出一轮回放
  deriving (Eq, Show)

-- | 追踪解释器：纯解释器外面叠一层 StateT（反向累积的日志）。每个能力先交给里面的纯解释器执行，再记一条日志。
newtype TracedCascade g a = TracedCascade (StateT [CascadeLog] (PureCascade g) a)
  deriving newtype (Functor, Applicative, Monad)

record :: CascadeLog -> StateT [CascadeLog] (PureCascade g) ()
record e = modify (e :)

instance RandomGen g => MonadRefill (TracedCascade g) where
  refillHoles pol fallen = TracedCascade $ do
    filled <- lift (refillHoles pol fallen)
    let holes = [p | (p, Nothing) <- assocs (stageGrid fallen)]
    record (LogRefill [(p, boardAt (stageGrid filled) p) | p <- holes])
    pure filled

instance MonadLevelHooks (TracedCascade g) where
  currentHooks = TracedCascade (lift currentHooks)
  absorbHooks b = TracedCascade $ do
    absorbed <- lift (absorbHooks b)
    record (LogAbsorb absorbed)
    pure absorbed

instance MonadWaves (TracedCascade g) where
  emitWave w = TracedCascade $ do
    lift (emitWave w)
    record (LogWave w)

-- | 运行一段连锁程序，另外返回事件日志（按发生顺序）。'Ran' 部分与 'runPureCascade' 逐项相同。
runTracedCascade :: LevelHooks -> g -> TracedCascade g a -> (Ran g a, [CascadeLog])
runTracedCascade hooks g (TracedCascade m) =
  let r = runPureCascade hooks g (runStateT m [])
      (a, logRev) = ranValue r
  in (r {ranValue = a}, reverse logRev)
