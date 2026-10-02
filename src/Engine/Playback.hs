{-# LANGUAGE BangPatterns #-}

-- | 通用的纯播放层（第三刀）：帧节拍、分段推进、加速。和具体游戏无关，不含 SDL。
--
-- 一段回放由若干「阶段」组成，阶段本身是游戏自己的类型 st：
--   * stageLen st  —— 这一阶段播多少帧；
--   * stageNext st —— 播完之后：Right (下一阶段, 进入时触发的一次性事件) 或 Left 最终状态（播完）。
-- Player 只负责时钟：每帧推进 1 帧，加速时推进 fastStep 帧；到达阶段长度就切到下一阶段、帧归零
-- （越过的零头丢弃，与第二刀之前 ComboFx 的写法逐帧相同）。
--
-- 另有 cueStages：最简单的「固定队列」阶段机（每个提示播固定帧数），通用效果 Effect 可以按节拍
-- 排进这样的队列（effectCues），玩具实现和没有复杂时间线的游戏直接用它。
module Engine.Playback
  ( -- * 阶段机与播放器
    Stages(..)
  , Player(..)
  , Tick(..)
  , newPlayer
  , stepPlayer
  , acceleratePlayer
  , playerProgress
  , runPlayer
    -- * 固定队列
  , Cue(..)
  , cueStages
  , effectCues
  ) where

import Engine.Effect (Effect, beats)

-- | 阶段机：长度与后继由具体游戏给出。ev = 进入阶段时的一次性事件（弹字、粒子……）。
data Stages st ev = Stages
  { stageLen  :: st -> Int
  , stageNext :: st -> Either st (st, [ev])
  }

-- | 播放器：当前阶段、阶段内帧号、是否加速。
data Player st = Player
  { plStage :: st
  , plFrame :: Int
  , plFast  :: Bool
  }

-- | 推进一帧的结果：还在播（可能带进入新阶段的事件）或播完（最终阶段状态）。
data Tick st ev
  = Playing (Player st) [ev]
  | Done st

-- | 从某个阶段开始、帧号 0、不加速。
newPlayer :: st -> Player st
newPlayer st = Player st 0 False

-- | 推进一帧：加速时一次推进 fastStep 帧。
stepPlayer :: Int -> Stages st ev -> Player st -> Tick st ev
stepPlayer fastStep sm p =
  let f = plFrame p + (if plFast p then fastStep else 1)
  in if f < stageLen sm (plStage p)
       then Playing p {plFrame = f} []
       else case stageNext sm (plStage p) of
         Left final -> Done final
         Right (st', evs) -> Playing p {plStage = st', plFrame = 0} evs

-- | 加速（此后每帧推进 fastStep 帧）。
acceleratePlayer :: Player st -> Player st
acceleratePlayer p = p {plFast = True}

-- | 当前阶段进度 0..1（长度 ≤ 0 的阶段视为已完成）。
playerProgress :: Stages st ev -> Player st -> Double
playerProgress sm p =
  let n = stageLen sm (plStage p)
  in if n <= 0 then 1 else min 1 (fromIntegral (plFrame p) / fromIntegral n)

-- | 一直播到结束：返回总帧数、按顺序触发的全部事件与最终阶段状态（测试 / 离线统计用）。
--
-- 严格性（Haskell 特性第 4 项）：第 4 项前有两处随帧数线性增长的堆积——
--   * 帧计数 n 只在最后返回的元组里用到，没被优化掉时每帧留下一个未求值的 (n + 1)，播 N 帧就是 N 层 thunk 链
--     （实验里 -O0 会堆，-O1 下 GHC 自己看出来了；bang pattern 让它不再取决于优化器）；
--   * 累积器 acc 每帧压一个事件表，绝大多数帧是空表——这才是 -O1 下的大头。
-- 现在 n 逐帧求值、空事件表不入栈（concat 本来就会丢掉它们，结果逐项相同）。
-- 1000 万帧、10 万个事件的实验（docs/haskell-features/demo/Strictness.hs）：-O1 最大驻留约 244 MB → 5 MB。
-- 实际回放只有几十到几百帧，这是卫生而不是修线上问题。
runPlayer :: Int -> Stages st ev -> Player st -> (Int, [ev], st)
runPlayer fastStep sm = go 0 []
  where
    go !n acc p = case stepPlayer fastStep sm p of
      Done final -> (n + 1, concat (reverse acc), final)
      Playing p' [] -> go (n + 1) acc p'
      Playing p' evs -> go (n + 1) (evs : acc) p'

-- | 固定队列里的一个提示：播 cueFrames 帧，进入时触发 cuePayload。
data Cue ev = Cue
  { cueFrames  :: Int
  , cuePayload :: ev
  } deriving (Eq, Show)

-- | 固定队列阶段机：阶段 = 剩余队列（head 为当前）。空队列长度 0，播完即结束。
-- 起始阶段的事件由调用方在开播时自行触发（与 Stages 的「进入时触发」一致：进入第一段 = 开播）。
cueStages :: Stages [Cue ev] ev
cueStages = Stages len next
  where
    len (c : _) = cueFrames c
    len [] = 0
    next (_ : rest@(c : _)) = Right (rest, [cuePayload c])
    next _ = Left []

-- | 把效果按节拍排成队列：每个节拍一个提示，帧数由 framesFor 按该节拍的效果决定。
effectCues :: ([Effect] -> Int) -> [Effect] -> [Cue [Effect]]
effectCues framesFor effs = [Cue (framesFor es) es | (_, es) <- beats effs]
