-- | ECS 的 system 与调度（与具体游戏无关）：system 统一是「拿整个世界、返回新世界」的纯函数 @w -> w@。
--
-- * 每个结算阶段有自己的世界类型（Match3.ECS.Stage：邻格 'Match3.ECS.Stage.NearWorld'、步末 'Match3.ECS.Stage.EndWorld' …），
--   里面是整盘（实体 = 格、组件存储 = 格子编码）加上本阶段的黑板资源（真消除格、避让格、产出 …）；
-- * system 之间的顺序是**显式**的：'System' 的 'Semigroup' 是「先左后右」的顺序组合（与 'Data.Monoid.Endo' 的
--   复合方向相反，读起来就是流水线的书写顺序），'pipeline' = 'mconcat'；
-- * 注册表里的 system 带一个次序数字（'Scheduled'），按次序稳定排序后组成阶段流水线（同次序按注册先后）；
--   次序数字与重构前的 arOrder / erOrder / srOrder 相同，快照的逐条探针按它枚举。
module Match3.ECS.System
  ( System(..)
  , system
  , pipeline
  , Scheduled(..)
  , at
  , schedule
  , scheduledPipeline
  ) where

import Data.List (sortOn)

-- | 一个 system：整个（本阶段的）世界 → 新世界。
newtype System w = System {runSystem :: w -> w}

-- | 先左后右：@a <> b@ 先跑 a 再跑 b。
instance Semigroup (System w) where
  System f <> System g = System (g . f)

-- | 什么也不做的 system。
instance Monoid (System w) where
  mempty = System id

-- | 由函数造 system。
system :: (w -> w) -> System w
system = System

-- | 按书写顺序依次运行的流水线（= 'mconcat'）。
pipeline :: [System w] -> System w
pipeline = mconcat

-- | 带次序的 system（注册表里的一项）。
data Scheduled w = Scheduled
  { schOrder  :: Int       -- ^ 次序（小的先跑）
  , schSystem :: System w  -- ^ system 本身
  }

-- | @at 10 s@：次序 10 的 system。
at :: Int -> System w -> Scheduled w
at = Scheduled

-- | 按次序稳定排序（同次序保持注册先后）。
schedule :: [Scheduled w] -> [Scheduled w]
schedule = sortOn schOrder

-- | 排好序的 system 连成一条流水线。
scheduledPipeline :: [Scheduled w] -> System w
scheduledPipeline = pipeline . map schSystem . schedule
