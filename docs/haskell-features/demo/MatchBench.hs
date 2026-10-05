{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 第 5 项文档 §4 的计时实验：匹配扫描 / 提示搜索 / 重力的新旧写法，以及一个没采用的对照变体（不在任何构建目标里）。
--
-- 编译运行（仓库根目录；旧写法的逐字副本在同目录的 LegacyPerf.hs，原 test/Spec/Support/LegacyPerf.hs）：
--
-- > b=$(mktemp -d)
-- > stack exec -- ghc -O1 -Wall -package-id "$(stack exec -- ghc-pkg --simple-output field match3 id)" -idocs/haskell-features/demo -outputdir "$b" -o "$b/m" docs/haskell-features/demo/MatchBench.hs
--
-- （用 -package-id 而不是 -package match3：内部库 match3-pure 的包名也是 match3，-package match3 会选中它、把主库藏起来。）
-- > "$b/m"
--
-- 盘面：全部关卡 × 种子 1–3 的开局盘，加上每张开局盘的全部相邻交换（多数带现成的匹配），共一万多张；
-- 每个函数在整组盘面上跑若干遍，用 getCPUTime 计 CPU 时间，结果折成一个数防止被优化掉。
-- 对照变体 hint-st：同样先算匹配码，但把码 thaw 进 STUArray，每个候选就地交换、查、换回——
-- 用来看 ST 的就地交换比现行的纯函数写法（读码时对调两个下标）有没有额外收益。
-- 重力：gravity-list 是第 5 项前的列表版（逐字副本），gravity-st 是现行的 applyGravityWith。
module Main (main) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import Data.Array.ST (STUArray, readArray, thaw, writeArray)
import qualified Data.Array.Unboxed as U
import Data.List (nub)
import Data.Maybe (isJust)
import Match3.Board.Gravity (applyGravityWith)
import Match3.Board.Grid (inBounds, swapCells, toM)
import Match3.Board.Match (findHintWith, findMatchRunsWith, hasAnyMatchWith, matchCodesWith)
import Match3.Core
import Match3.Element.Builtin (defaultWorld)
import Match3.Levels.Campaign (levelCount)
import Match3.Types (boardNCols, boardNRows, boardPositions)
import Match3.Element.World (World, blocksSwapWith, colorOfWith, hintableWith)
import qualified LegacyPerf as Old
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

boards :: [Board]
boards =
  [ b'
  | li <- [0 .. levelCount - 1]
  , seed <- [1, 2, 3 :: Int]
  , Just gs <- [campaignGame li seed]
  , let b = gsBoard gs
  , b' <- b : [swapCells b p q | p@(r, c) <- boardPositions b, q <- [(r, c + 1), (r + 1, c)], inBounds b q]
  ]

-- | 跑 reps 遍，返回每张盘面的平均微秒数。
timeIt :: String -> Int -> (Board -> Int) -> [Board] -> IO Double
timeIt name reps f bs = do
  t0 <- getCPUTime
  let go k acc
        | k <= 0 = acc
        | otherwise = let !s = sum (map f bs) in go (k - 1) (acc + s)
      total = go reps (0 :: Int)
  when (total < 0) (putStrLn "?")
  t1 <- getCPUTime
  let us = fromIntegral (t1 - t0) / 1e6 / fromIntegral (reps * length bs) :: Double
  printf "%-22s %8.2f µs / 盘\n" name us
  pure us

-- | 对照变体：ST 版提示搜索（只做普通匹配提示这一半；规则提示与现行实现相同，这里略去）。
-- 匹配码 thaw 进一张 STUArray，每个候选「就地换过去、查交换触及的行 / 列、再换回来」；
-- 未触及的行 / 列仍用原盘的结论（与现行纯函数写法相同）。
hintST :: World -> Board -> Maybe (Pos, Pos)
hintST world b = runST $ do
  m <- thaw codes
  let tryPair :: forall s. STUArray s Pos Int -> (Pos, Pos) -> ST s Bool
      tryPair arr (p1@(r1, c1), p2@(r2, c2)) = do
        let rs = nub [r1, r2]
            cs = nub [c1, c2]
            swap = do
              x <- readArray arr p1
              y <- readArray arr p2
              writeArray arr p1 y
              writeArray arr p2 x
            line ps = codesHaveRun <$> mapM (readArray arr) ps
        swap
        hit <- orM (map (\r -> line [(r, c) | c <- cols]) rs ++ map (\c -> line [(r, c) | r <- rows]) cs)
        swap
        pure (hit || or [rowHas A.! r | r <- rows, r `notElem` rs] || or [colHas A.! c | c <- cols, c `notElem` cs])
      firstM [] = pure Nothing
      firstM (x : xs) = do
        ok <- tryPair m x
        if ok then pure (Just x) else firstM xs
  firstM candidates
  where
    rows = boardRowIndices b
    cols = boardColIndices b
    codes = matchCodesWith world b
    hintable cell = isJust (colorOfWith world cell) && not (blocksSwapWith world cell)
    candidates =
      [ (p1, p2)
      | r <- rows, c <- cols, let p1 = (r, c), hintable (getCell b p1)
      , p2 <- [(r, c + 1), (r + 1, c)], inBounds b p2, hintable (getCell b p2)
      , hintableWith world (getCell b p1) && hintableWith world (getCell b p2) ]
    rowHas = A.listArray (0, boardNRows b - 1) [codesHaveRun [codes U.! (r, c) | c <- cols] | r <- rows] :: A.Array Int Bool
    colHas = A.listArray (0, boardNCols b - 1) [codesHaveRun [codes U.! (r, c) | r <- rows] | c <- cols] :: A.Array Int Bool
    orM [] = pure False
    orM (x : xs) = x >>= \v -> if v then pure True else orM xs

-- | 与 Match3.Board.Match 内部的 codesHaveRun 相同（那里不导出）。
codesHaveRun :: [Int] -> Bool
codesHaveRun = go (-1) (0 :: Int)
  where
    go _ _ [] = False
    go prev n (k : ks)
      | k < 0 = go (-1) 0 ks
      | k == prev = n + 1 >= 3 || go prev (n + 1) ks
      | otherwise = go k 1 ks

main :: IO ()
main = do
  let bs = boards
      world = defaultWorld
      holed b = toM b A.// [((r, c), Nothing) | (r, c) <- boardPositions b, (r * 7 + c * 3) `mod` 5 == 0]
      mbs = map holed (take 3000 bs)
  printf "盘面数 %d\n" (length bs)
  _ <- timeIt "runs-old" 5 (length . Old.oldFindMatchRunsWith world) bs
  _ <- timeIt "runs-new" 5 (length . findMatchRunsWith world) bs
  _ <- timeIt "any-old" 5 (fromEnum . Old.oldHasAnyMatchWith world) bs
  _ <- timeIt "any-new" 5 (fromEnum . hasAnyMatchWith world) bs
  _ <- timeIt "hint-old" 3 (maybe 0 (fst . fst) . Old.oldFindHintWith world) bs
  _ <- timeIt "hint-new" 3 (maybe 0 (fst . fst) . findHintWith world) bs
  _ <- timeIt "hint-st（未采用）" 3 (maybe 0 (fst . fst) . hintST world) bs
  -- 正确性顺带核对
  printf "hint-st 与现行一致（普通匹配提示能找到时）：%s\n"
    (show (and [hintST world b == findHintWith world b | b <- bs, isJust (hintST world b)]))
  printf "gravity-st 与 gravity-list 一致：%s\n" (show (and [applyGravityWith world mb == Old.oldApplyGravityWith world mb | mb <- mbs]))
  t0 <- getCPUTime
  let !g1 = length (filter isJust (concatMap A.elems [Old.oldApplyGravityWith world mb | _ <- [1 .. 5 :: Int], mb <- mbs]))
  t1 <- getCPUTime
  let !g2 = length (filter isJust (concatMap A.elems [applyGravityWith world mb | _ <- [1 .. 5 :: Int], mb <- mbs]))
  t2 <- getCPUTime
  printf "%-22s %8.2f µs / 盘\n" "gravity-list" (fromIntegral (t1 - t0) / 1e6 / fromIntegral (5 * length mbs) :: Double)
  printf "%-22s %8.2f µs / 盘\n" "gravity-st" (fromIntegral (t2 - t1) / 1e6 / fromIntegral (5 * length mbs) :: Double)
  when (g1 /= g2) (putStrLn "gravity mismatch")
