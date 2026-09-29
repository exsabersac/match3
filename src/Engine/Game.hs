-- | 多游戏通用接口（第三刀）：一个「回合制、纯函数、固定种子可复现」的游戏由一条记录描述。
--
-- 通用层**不依赖**任何具体游戏（不 import Match3.*）；三消在 "Match3.Engine" 里实现这条记录，
-- 测试里另有一个极小的玩具实现（test/Toy.hs），证明接口本身可以单独编译、单独跑通。
--
-- 取舍：用 record-of-functions 而不是带关联类型的 typeclass ——
--   * 同一种游戏可以有多份配置不同的实例（三消：不同元素注册表 / 规则变体），记录是一等值，类型类做不到；
--   * 不需要 TypeFamilies / 孤儿实例，玩具实现在测试里写一个值就行；
--   * 状态 / 动作 / 事件 / 结局都是普通类型参数，调用处的类型推断直接。
--
-- 随机数与种子约定：
--   * 随机数状态放在游戏状态 s 里（三消是 StdGen），step 是纯函数：同一状态 + 同一动作 ⇒ 同一结果；
--   * 只有开局 gameNew 接受外部种子（Seed = Int），UI / 测试决定种子，规则层从不读时钟或 IO。
module Engine.Game
  ( Seed
  , Step(..)
  , Game(..)
  , rejectedStep
  , runActions
  , finalState
  , stepEffects
  ) where

import Engine.Effect (Effect)

-- | 开局种子。
type Seed = Int

-- | 一次 step 的结果。
data Step s e o = Step
  { stepState    :: s        -- ^ 新状态（被拒时 = 原状态）
  , stepEvents   :: [e]      -- ^ 本步的效果事件（纯数据，按时间顺序；给播放层用，不参与结算）
  , stepOutcome  :: Maybe o  -- ^ 本步之后的结局（Nothing = 未结束）
  , stepAccepted :: Bool     -- ^ 动作是否被接受（False：非法 / 无效动作，状态不变、没有事件）
  }

-- | 一种游戏。类型参数：cfg 开局配置、s 状态、a 动作、e 效果事件、o 结局。
data Game cfg s a e o = Game
  { gameName    :: String                -- ^ 名字（日志 / 标题用）
  , gameNew     :: cfg -> Seed -> s      -- ^ 开局：配置 + 种子 → 初始状态（纯函数）
  , gameStep    :: s -> a -> Step s e o  -- ^ 推进一步（纯函数；随机数只来自 s）
  , gameOutcome :: s -> Maybe o          -- ^ 结局判定：Nothing = 仍可继续
  , gameActions :: s -> [a]              -- ^ 当前可被接受的动作（供测试 / 自动演示；可以是有限枚举的子集）
  , gameStatus  :: s -> [(String, Int)]  -- ^ 给外壳看的具名数值（标题栏 / HUD：分数、步数、连击……）
  , gameEffect  :: e -> Effect           -- ^ 本游戏事件 → 通用效果事件（通用播放层只认 Effect）
  }

-- | 被拒的一步：状态原样返回、没有事件、结局沿用当前判定。
rejectedStep :: Game cfg s a e o -> s -> Step s e o
rejectedStep g s = Step s [] (gameOutcome g s) False

-- | 依次执行动作，遇到结局即停（其后的动作不再执行）。返回每一步的结果。
runActions :: Game cfg s a e o -> s -> [a] -> [Step s e o]
runActions g = go
  where
    go _ [] = []
    go s (a : as)
      | Just _ <- gameOutcome g s = []
      | otherwise =
          let st = gameStep g s a
          in st : case stepOutcome st of
               Just _ -> []
               Nothing -> go (stepState st) as

-- | runActions 之后的最终状态。
finalState :: Game cfg s a e o -> s -> [a] -> s
finalState g s0 as = case reverse (runActions g s0 as) of
  (st : _) -> stepState st
  [] -> s0

-- | 一步的效果事件映射成通用效果。
stepEffects :: Game cfg s a e o -> Step s e o -> [Effect]
stepEffects g = map (gameEffect g) . stepEvents
