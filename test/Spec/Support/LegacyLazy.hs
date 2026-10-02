-- | 第 4 项（惰性与递归模式）改写前的写法，逐字副本（只改了函数名前缀与必要的 import），供 Spec.Lazy 逐项对照：
--
-- * Match3.Board.Random 的两个拒绝采样（手写尾递归）；
-- * Match3.Game.Shuffle.ensurePlayableWith 的计数循环 go 24 … go 0（内部的重洗也用这里的旧采样）；
-- * Engine.Playback.runPlayer（惰性帧计数、空事件表也入栈）；
-- * Match3.Counts.countsFromList（foldl）。
module Spec.Support.LegacyLazy
  ( oldRandomStableBoardSized
  , oldRandomPlayableBoardSized
  , oldEnsurePlayableWith
  , oldRunPlayer
  , oldCountsFromList
  ) where

import Engine.Playback (Player, Stages, Tick(..), stepPlayer)
import Match3.Board.Default (hasAnyMatch, hasValidMove)
import Match3.Board.Match (hasValidMoveWith)
import Match3.Board.Random (randomBoardSized)
import Match3.Counts (CounterKey, Counts, bumpCount, noCounts)
import Match3.Element.Registry (Registry)
import Match3.Game.Shuffle (extractDecorWith, restoreDecor)
import Match3.Game.State
import Match3.Types
import System.Random (RandomGen)

oldRandomStableBoardSized :: RandomGen g => Int -> Int -> g -> (Board, g)
oldRandomStableBoardSized rows cols g =
  let (b, g') = randomBoardSized rows cols g
  in if hasAnyMatch b then oldRandomStableBoardSized rows cols g' else (b, g')

oldRandomPlayableBoardSized :: RandomGen g => Int -> Int -> g -> (Board, g)
oldRandomPlayableBoardSized rows cols g =
  let (b, g') = oldRandomStableBoardSized rows cols g
  in if hasValidMove b then (b, g') else oldRandomPlayableBoardSized rows cols g'

oldEnsurePlayableWith :: Registry -> GameState -> GameState
oldEnsurePlayableWith reg gs
  | Just _ <- gsOver gs = gs { gsShuffled = False }
  | hasValidMoveWith reg (gsBoard gs) = gs { gsShuffled = False }
  | otherwise = go (24 :: Int) gs
  where
    dims g = boardDims (gsBoard g)
    reshuffle g =
      let (rows, cols) = dims g
          decor = extractDecorWith reg (gsBoard g)
          (board0, g') = oldRandomPlayableBoardSized rows cols (gsGen g)
          board = restoreDecor board0 decor
      in (board, g')
    go 0 g =
      let (board, g') = reshuffle g
      in g { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }
    go n g =
      let (board, g') = reshuffle g
          g2 = g { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }
      in if hasValidMoveWith reg board then g2 else go (n - 1) g2

oldRunPlayer :: Int -> Stages st ev -> Player st -> (Int, [ev], st)
oldRunPlayer fastStep sm = go 0 []
  where
    go n acc p = case stepPlayer fastStep sm p of
      Done final -> (n + 1, concat (reverse acc), final)
      Playing p' evs -> go (n + 1) (evs : acc) p'

oldCountsFromList :: [(CounterKey, Int)] -> Counts
oldCountsFromList = foldl (\c (k, n) -> bumpCount k n c) noCounts
