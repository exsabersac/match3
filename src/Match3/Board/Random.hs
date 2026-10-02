{-# LANGUAGE ScopedTypeVariables #-}

-- | 随机盘面：随机盘、无初始三连的稳定盘、至少有一手可走的可玩盘，以及洗牌用的 shufflePlayable。
--
-- 依赖：Grid（randomColor / chunk）、Match（hasAnyMatch / hasValidMove）。
-- 不变量：拒绝采样（不满足就用推进后的生成器重来），所以同一种子总得到同一盘面。
module Match3.Board.Random
  ( randomBoard
  , randomBoardSized
  , randomStableBoard
  , randomStableBoardSized
  , randomPlayableBoard
  , randomPlayableBoardSized
  , shufflePlayable
  , shufflePlayableSized
  ) where

import Data.Traversable (mapAccumL)
import Match3.Types
import System.Random (RandomGen)
import Match3.Board.Grid
import Match3.Board.Default (hasAnyMatch, hasValidMove)

-- | 整盘随机普通宝石（缺省 8×8；逐格 randomColor，行优先）。
randomBoard :: RandomGen g => g -> (Board, g)
randomBoard = randomBoardSized boardSize boardSize

-- | 指定行列的整盘随机普通宝石（逐格 randomColor，行优先）。
--
-- 第 2 项（Traversable）：先建一张只有形状的 Grid ()（行列与第 2 项前 boardFromRows (chunk cols cells) 完全相同），
-- 再用 mapAccumL 把生成器当累积器逐格穿过去。Traversable 保证按下标顺序（行主序）每格恰好访问一次、形状不变，
-- 所以随机数的消耗顺序与原来手写的递归逐个相同（同一种子同一盘面；Spec.Classes 与旧写法逐种子对照）。
randomBoardSized :: RandomGen g => Int -> Int -> g -> (Board, g)
randomBoardSized rows cols g0 =
  let (g', b) = mapAccumL (\g () -> let (c, g1) = randomColor g in (g1, mkGem c)) g0 shape
  in (b, g')
  where
    shape = gridFromRows (chunk cols (replicate (rows * cols) ()))

-- | 拒绝采样：直到没有初始三连为止（缺省 8×8）。
randomStableBoard :: RandomGen g => g -> (Board, g)
randomStableBoard = randomStableBoardSized boardSize boardSize

randomStableBoardSized :: RandomGen g => Int -> Int -> g -> (Board, g)
randomStableBoardSized rows cols g =
  let (b, g') = randomBoardSized rows cols g
  in if hasAnyMatch b then randomStableBoardSized rows cols g' else (b, g')

-- | Stable board with at least one valid move (no initial three-in-a-row; 缺省 8×8).
randomPlayableBoard :: RandomGen g => g -> (Board, g)
randomPlayableBoard = randomPlayableBoardSized boardSize boardSize

randomPlayableBoardSized :: RandomGen g => Int -> Int -> g -> (Board, g)
randomPlayableBoardSized rows cols g =
  let (b, g') = randomStableBoardSized rows cols g
  in if hasValidMove b then (b, g') else randomPlayableBoardSized rows cols g'

-- | Reshuffle into a playable stable board (ignores previous layout; 缺省 8×8).
shufflePlayable :: RandomGen g => g -> (Board, g)
shufflePlayable = randomPlayableBoard

shufflePlayableSized :: RandomGen g => Int -> Int -> g -> (Board, g)
shufflePlayableSized = randomPlayableBoardSized
