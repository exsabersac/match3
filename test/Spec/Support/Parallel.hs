{-# LANGUAGE ScopedTypeVariables #-}

-- | 确定性并行批量求值（Haskell 特性第 5 项，docs/haskell-features/05-性能与并发.md）。
--
-- 用途：金标准这类「一大串互不依赖的纯计算」（关卡 × 种子 × 逐步推进）在多核上一起算，结果按**原顺序**拼回。
-- 只用 GHC 自带的 base + stm：
--
-- * 任务表是一个不可变数组；共享的只有一个「下一个要领的下标」计数器 TVar Int，工人用一个 STM 事务
--   「读出下标并加一」领任务——事务是原子的，两个工人不可能领到同一个下标，也不会漏掉；
-- * 每个任务有自己的结果槽 TMVar（只写一次）；主线程按下标顺序逐个 takeTMVar，所以输出顺序与调度无关；
-- * 求值在工人线程里用 evaluate (force x) 做完（不是把 thunk 原样交回主线程再算）；任务抛出的异常存进槽里，
--   主线程按顺序取到这一格时重新抛出，和串行求值时「第一个出错的任务」报同一个错。
--
-- 结果是纯值，所以并行与串行的结果逐项相同；Spec.Perf 对照，金标准测试本身也逐行比对入库文件。
module Spec.Support.Parallel
  ( parallelForce
  , forceLines
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, newTVarIO, putTMVar, readTVar, takeTMVar, writeTVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM, replicateM_)
import Data.Array (Array, listArray, (!))

-- | 用 workers 个线程（至少 1 个、至多任务数个）求值 force x，按原顺序返回各个 x（已求值）。
parallelForce :: forall a. Int -> (a -> ()) -> [a] -> IO [a]
parallelForce workers force xs = do
  let n = length xs
      tasks = listArray (0, n - 1) xs :: Array Int a
  next <- newTVarIO (0 :: Int)
  slotList <- forM xs (const newEmptyTMVarIO)
  let slots = listArray (0, n - 1) slotList
      claim = atomically $ do
        i <- readTVar next
        if i >= n
          then pure Nothing
          else writeTVar next (i + 1) >> pure (Just i)
      worker = do
        mi <- claim
        case mi of
          Nothing -> pure ()
          Just i -> do
            let x = tasks ! i
            r <- try (evaluate (force x))
            atomically (putTMVar (slots ! i) (fmap (const x) (r :: Either SomeException ())))
            worker
  replicateM_ (max 1 (min workers n)) (forkIO worker)
  forM slotList (\s -> atomically (takeTMVar s) >>= either throwIO pure)

-- | 把一段文本行完全求值（每一行的每个字符），不留 thunk。
forceLines :: [String] -> ()
forceLines = foldr (\l acc -> foldr seq () l `seq` acc) ()
