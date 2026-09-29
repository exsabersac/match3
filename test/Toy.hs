-- | 通用接口的玩具实现（只用于测试，第三刀）：一维计数器。
--
-- 规则：开局目标 = 配置 + 种子 mod 3；每步 Inc n（n ∈ 1..3）把计数加 n，超过目标即判负，
-- 恰好等于目标即判胜；步数用完判负。Reset 把计数归零（消耗一步）；Inc 的 n 不在 1..3 时被拒。
-- 事件：每步一个 Bumped（加了几）或 Zeroed，结局时再加一个 Finished。
--
-- 本模块**只** import Engine.*，不 import 任何 Match3 模块：它能编译、能跑通 step / 结局判定 /
-- 播放层，就证明通用接口与通用纯播放层独立于三消（测试 engine_toy_counter_game）。
module Toy
  ( ToyState(..)
  , ToyAction(..)
  , ToyEvent(..)
  , ToyEnd(..)
  , toyGame
  , toyFrames
  ) where

import Engine.Effect (Effect(..))
import Engine.Game (Game(..), Step(..))

data ToyState = ToyState
  { tsCount  :: Int
  , tsTarget :: Int
  , tsTurns  :: Int
  } deriving (Eq, Show)

data ToyAction = Inc Int | Reset
  deriving (Eq, Show)

data ToyEvent = Bumped Int | Zeroed | Finished ToyEnd
  deriving (Eq, Show)

data ToyEnd = ToyWon | ToyLost
  deriving (Eq, Show)

toyGame :: Game Int ToyState ToyAction ToyEvent ToyEnd ()
toyGame =
  Game
    { gameName = "toy-counter"
    , gameNew = \base seed -> ToyState 0 (base + seed `mod` 3) 5
    , gameStep = step
    , gameOutcome = outcome
    , gameActions = \s -> if outcome s == Nothing then Reset : [Inc n | n <- [1 .. 3]] else []
    , gameStatus = \s -> [("count", tsCount s), ("target", tsTarget s), ("turns", tsTurns s)]
    , gameEffect = effect
    }
  where
    outcome s
      | tsCount s == tsTarget s = Just ToyWon
      | tsCount s > tsTarget s || tsTurns s <= 0 = Just ToyLost
      | otherwise = Nothing
    step s a
      | outcome s /= Nothing = Step s [] (outcome s) False Nothing
      | otherwise = case a of
          Inc n
            | n < 1 || n > 3 -> Step s [] Nothing False Nothing
            | otherwise -> finish (s {tsCount = tsCount s + n, tsTurns = tsTurns s - 1}) [Bumped n]
          Reset -> finish (s {tsCount = 0, tsTurns = tsTurns s - 1}) [Zeroed]
    finish s' evs =
      let o = outcome s'
      in Step s' (evs ++ [Finished e | Just e <- [o]]) o True Nothing
    effect ev = case ev of
      Bumped n -> Effect 0 "bump" "counter" [] n
      Zeroed -> Effect 0 "zero" "counter" [] 0
      Finished ToyWon -> Effect 1 "end" "won" [] 0
      Finished ToyLost -> Effect 1 "end" "lost" [] 0

-- | 播放层配置：每个节拍的帧数（结局节拍 20 帧，其它 6 帧）。
toyFrames :: [Effect] -> Int
toyFrames es = if any ((== "end") . efKind) es then 20 else 6
