{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

-- | 第 4 项文档 §4 的严格性小实验（不在任何构建目标里；手动编译运行）。
--
-- 编译（在仓库根目录；-O0 / -O1 各编一份对比）：
--
-- > b=$(mktemp -d); stack exec -- ghc -O1 -rtsopts -package match3 -outputdir "$b" -o "$b/s" docs/haskell-features/demo/Strictness.hs
-- > "$b/s" player-old 100000 +RTS -s      # 看 "maximum residency" 一行
--
-- 实验（第一个参数）：
--
-- * player-old / player-new：Engine.Playback.runPlayer 第 4 项前 / 后的写法，播 100 × N 帧（每 100 帧一个事件）；
--   player-bang-only 只加 bang pattern、不跳过空事件表，用来拆开看两处改动。player-new 调的是库里的 runPlayer
--   （库总是按 stack 的缺省 -O1 编译），其余写法随本文件的优化级别。
-- * player-hylo / player-hylo-strict：同一回放写成 hylo（惰性代数 / 严格代数），说明这里为什么不用 hylo。
-- * counts-foldl / counts-foldl'：Match3.Counts.countsFromList 第 4 项前 / 后的写法，累加 N 项。
module Main (main) where

import Engine.Playback (Player, Stages(..), Tick(..), newPlayer, runPlayer, stepPlayer)
import Match3.Color (allColors)
import Match3.Counts (CounterKey(..), Counts, bumpCount, countOf, noCounts)
import System.Environment (getArgs)

-- | 第 4 项前的 runPlayer（逐字）。
oldRunPlayer :: Int -> Stages st ev -> Player st -> (Int, [ev], st)
oldRunPlayer fastStep sm = go 0 []
  where
    go n acc p = case stepPlayer fastStep sm p of
      Done final -> (n + 1, concat (reverse acc), final)
      Playing p' evs -> go (n + 1) (evs : acc) p'

-- | 只加 bang pattern、空事件表照样入栈（拆开看两处改动各自的作用）。
bangOnlyRunPlayer :: Int -> Stages st ev -> Player st -> (Int, [ev], st)
bangOnlyRunPlayer fastStep sm = go 0 []
  where
    go !n acc p = case stepPlayer fastStep sm p of
      Done final -> (n + 1, concat (reverse acc), final)
      Playing p' evs -> go (n + 1) (evs : acc) p'

data TickF st ev r = DoneF st | PlayingF [ev] r
  deriving (Functor)

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg co = alg . fmap (hylo alg co) . co

coalg :: Int -> Stages st ev -> Player st -> TickF st ev (Player st)
coalg fs sm p = case stepPlayer fs sm p of
  Done final -> DoneF final
  Playing p' evs -> PlayingF evs p'

hyloLazy, hyloStrict :: Int -> Stages st ev -> Player st -> (Int, [ev], st)
hyloLazy fs sm = hylo alg (coalg fs sm)
  where
    alg (DoneF st) = (1, [], st)
    alg (PlayingF evs ~(n, rest, st)) = (n + 1, evs ++ rest, st)
hyloStrict fs sm = hylo alg (coalg fs sm)
  where
    alg (DoneF st) = (1, [], st)
    alg (PlayingF evs (n, rest, st)) = let !m = n + 1 in (m, evs ++ rest, st)

-- | N 段、每段 100 帧、进入时报段号的阶段机。
stages :: Stages Int Int
stages = Stages (const 100) (\i -> if i <= 0 then Left 0 else Right (i - 1, [i]))

countsLazy, countsStrict :: [(CounterKey, Int)] -> Counts
countsLazy = foldl (\c (k, n) -> bumpCount k n c) noCounts
countsStrict = foldl' (\c (k, n) -> bumpCount k n c) noCounts

main :: IO ()
main = do
  args <- getArgs
  case args of
    [which, nStr] -> do
      let n = read nStr :: Int
          player f = let (fr, evs, st) = f 3 stages (newPlayer n) in print (fr, length evs, st)
          counts f = print (sum [countOf (CountColor c) (f [(CountColor c', 1) | _ <- [1 .. n], c' <- allColors]) | c <- take 1 allColors])
      case which of
        "player-old" -> player oldRunPlayer
        "player-bang-only" -> player bangOnlyRunPlayer
        "player-new" -> player runPlayer
        "player-hylo" -> player hyloLazy
        "player-hylo-strict" -> player hyloStrict
        "counts-foldl" -> counts countsLazy
        "counts-foldl'" -> counts countsStrict
        _ -> putStrLn "未知实验"
    _ -> putStrLn "用法：Strictness <实验> <N> +RTS -s"
