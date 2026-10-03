{-# LANGUAGE ScopedTypeVariables #-}

-- | 盘面基础：坐标边界、读写格、交换、相邻判定与界内邻格（方向 Dir 在 Match3.Types.Board），以及沉降用的可空盘面 MBoard 与随机颜色。
--
-- 依赖：只依赖 Match3.Types（和 random）。是 Board 各子模块的最底层，自身不含任何规则。
-- 不变量：getCell / setCell 只接受 inBounds 的坐标（调用方保证）；randomColor 每次恰好消耗一次
-- randomR，refill / randomBoard 的随机数顺序依赖于此，改动会改变所有固定种子的盘面。
module Match3.Board.Grid
  ( inBounds
  , mInBounds
  , getCell
  , setCell
  , swapCells
  , adjacent
  , neighborsInBounds
  , MBoard
  , toM
  , mboardFromRows
  , mboardRows
  , atM
  , setM
  , setManyM
  , randomColor
  , chunk
  ) where

import Data.Array (Array, accum, bounds, inRange, listArray, (!))
import Data.Maybe (isJust)
import Match3.Types
import System.Random (RandomGen, randomR)

-- | 坐标是否落在给定盘面内（由数组下界判定，支持矩形盘）。
inBounds :: Board -> Pos -> Bool
inBounds b p = inRange (bounds (boardArray b)) p

-- | 坐标是否落在可空盘面内。
mInBounds :: MBoard -> Pos -> Bool
mInBounds mb p = inRange (bounds mb) p

-- | 读一格，O(1)（Board 是二维数组；调用方保证 inBounds，越界会直接报错）。
getCell :: Board -> Pos -> Cell
getCell = boardAt

-- | 写一格，返回新盘面（纯函数，不修改原盘）。
setCell :: Board -> Pos -> Cell -> Board
setCell = boardSet

-- | 交换两格内容；不做任何合法性检查（门禁在 trySwap / useFreeSwap）。
swapCells :: Board -> Pos -> Pos -> Board
swapCells b p1 p2 =
  let a = getCell b p1
      b' = setCell b p1 (getCell b p2)
  in setCell b' p2 a

-- | 两格是否上下 / 左右相邻（不含对角）：q 是 p 朝某个方向走一格。
-- 第 7 项前写成 @(abs (r1 - r2) == 1 && c1 == c2) || (abs (c1 - c2) == 1 && r1 == r2)@，逐对等价（测试对照）。
adjacent :: Pos -> Pos -> Bool
adjacent p q = isJust (dirBetween p q)

-- | 按给定方向顺序列出界内的邻格（= @filter (inBounds b) . neighborsIn ds@）。
neighborsInBounds :: [Dir] -> Board -> Pos -> [Pos]
neighborsInBounds ds b = filter (inBounds b) . neighborsIn ds

-- | 可空盘面（沉降用；Nothing = 空洞）：与 Board 同形的二维数组，(行, 列) 下标、行主序。
-- 第 3 刀之前是 [[Maybe Cell]]（读格走两次 (!!)，重力要转置两次）。
type MBoard = Array Pos (Maybe Cell)

toM :: Board -> MBoard
toM = fmap Just . boardArray

-- | 由行列表建可空盘面（每行等长；测试与回放构造用）。
mboardFromRows :: [[Maybe Cell]] -> MBoard
mboardFromRows rows =
  let nr = length rows
      nc = case rows of
        [] -> 0
        (r0 : _) -> length r0
  in listArray ((0, 0), (nr - 1, nc - 1)) (concat rows)

-- | 行列表视图（行主序；回放 JSON / 金标准打印用）。
mboardRows :: MBoard -> [[Maybe Cell]]
mboardRows mb =
  let ((r0, c0), (r1, c1)) = bounds mb
  in [[mb ! (r, c) | c <- [c0 .. c1]] | r <- [r0 .. r1]]

-- | 可空盘面读格；越界按空洞（Nothing）处理。
atM :: MBoard -> Pos -> Maybe Cell
atM mb p
  | inRange (bounds mb) p = mb ! p
  | otherwise = Nothing

-- | 可空盘面写格；越界不改。
setM :: MBoard -> Pos -> Maybe Cell -> MBoard
setM mb p v = setManyM mb [(p, v)]

-- | 一次写多格：按列表顺序写（同一格以后写的为准），越界的项忽略。
setManyM :: MBoard -> [(Pos, Maybe Cell)] -> MBoard
setManyM mb kvs = accum (\_ v -> v) mb [kv | kv@(p, _) <- kvs, inRange (bounds mb) p]

-- | 均匀随机取一种颜色；恰好调用一次 randomR（随机数消耗顺序的基本单位）。
randomColor :: RandomGen g => g -> (Color, g)
randomColor g =
  let (i, g') = randomR (0, numColors - 1) g
  in (colorAt i, g')

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
