{-# LANGUAGE ScopedTypeVariables #-}

-- | 盘面基础：坐标边界、读写格、交换、相邻判定，以及沉降用的可空盘面 MBoard 与随机颜色。
--
-- 依赖：只依赖 Match3.Types（和 random）。是 Board 各子模块的最底层，自身不含任何规则。
-- 不变量：getCell / setCell 只接受 inBounds 的坐标（调用方保证）；randomColor 每次恰好消耗一次
-- randomR，refill / randomBoard 的随机数顺序依赖于此，改动会改变所有固定种子的盘面。
module Match3.Board.Grid
  ( inBounds
  , getCell
  , setCell
  , swapCells
  , adjacent
  , MBoard
  , toM
  , atM
  , setM
  , transposeM
  , randomColor
  , chunk
  ) where

import Data.List (transpose)
import Match3.Types
import System.Random (RandomGen, randomR)

-- | 坐标是否落在 8×8 盘内（行、列都在 [0, boardSize)）。
inBounds :: Pos -> Bool
inBounds (r, c) = r >= 0 && r < boardSize && c >= 0 && c < boardSize

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

-- | 两格是否上下 / 左右相邻（不含对角）。
adjacent :: Pos -> Pos -> Bool
adjacent (r1, c1) (r2, c2) =
  (abs (r1 - r2) == 1 && c1 == c2) || (abs (c1 - c2) == 1 && r1 == r2)

-- | 沉降过程中的可空盘面：Nothing = 本轮挖空、等待重力 / 补子的洞。
type MBoard = [[Maybe Cell]]

toM :: Board -> MBoard
toM = map (map Just) . boardRows

-- | 可空盘面读格；越界按空洞（Nothing）处理。
atM :: MBoard -> Pos -> Maybe Cell
atM b (r, c) = case drop r b of
  row : _ | c >= 0, r >= 0 -> case drop c row of
    x : _ -> x
    [] -> Nothing
  _ -> Nothing

-- | 可空盘面写格；越界不改。
setM :: MBoard -> Pos -> Maybe Cell -> MBoard
setM b (r, c) v =
  [ if i == r then [if j == c then v else x | (j, x) <- zip [0 ..] row] else row
  | (i, row) <- zip [0 :: Int ..] b
  ]

-- | 转置（盘面恒为矩形，与逐列取行首等价）。
transposeM :: MBoard -> MBoard
transposeM = transpose

-- | 均匀随机取一种颜色；恰好调用一次 randomR（随机数消耗顺序的基本单位）。
randomColor :: RandomGen g => g -> (Color, g)
randomColor g =
  let (i, g') = randomR (0, numColors - 1) g
  in (colorAt i, g')

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
