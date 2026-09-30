-- | 盘面（第 6 刀从 Match3.Types 拆出）：坐标 Pos、以 (行, 列) 为下标的二维数组 Board 及其读写 / 转换。
--
-- 依赖：Match3.Types.Cell。不变量：Show 按行列表打印，与旧列表盘输出相同。
module Match3.Types.Board
  ( Pos
  , Board
  , boardFromRows
  , boardRows
  , boardCells
  , boardAssocs
  , boardAt
  , boardSet
  , boardSetMany
  , boardArray
  , boardFromArray
  , mapBoard
  , boardSize
  ) where

import Data.Array (Array, assocs, bounds, elems, listArray, (!), (//))
import Match3.Types.Cell (Cell)

type Pos = (Int, Int)
-- | 盘面：以 (行, 列) 为下标的二维数组（第三刀起；之前是 [[Cell]]，读格要走两次 (!!)）。
-- 读格 boardAt 为 O(1)；写格 boardSet 复制一次数组（64 格，与原来重建行列表同量级）。
-- 与行列表互转用 boardFromRows / boardRows（行主序，与旧表示逐格一一对应；Show 仍按行列表打印）。
newtype Board = Board (Array (Int, Int) Cell)
  deriving (Eq)

instance Show Board where
  showsPrec d b = showsPrec d (boardRows b)

-- | 由行列表建盘（每行等长；空列表 = 0×0 盘）。
boardFromRows :: [[Cell]] -> Board
boardFromRows rows =
  let nr = length rows
      nc = case rows of
        [] -> 0
        (r0 : _) -> length r0
  in if any ((/= nc) . length) rows
       then error "boardFromRows: rows of unequal length"
       else Board (listArray ((0, 0), (nr - 1, nc - 1)) (concat rows))

-- | 行列表视图（行主序）。
boardRows :: Board -> [[Cell]]
boardRows (Board a) =
  let ((r0, c0), (r1, c1)) = bounds a
  in [[a ! (r, c) | c <- [c0 .. c1]] | r <- [r0 .. r1]]

-- | 全部格子，行主序。
boardCells :: Board -> [Cell]
boardCells (Board a) = elems a

-- | 全部 (坐标, 格子)，行主序。
boardAssocs :: Board -> [(Pos, Cell)]
boardAssocs (Board a) = assocs a

-- | 读一格，O(1)；越界报错（与旧 (!!) 相同）。
boardAt :: Board -> Pos -> Cell
boardAt (Board a) p = a ! p

-- | 写一格，返回新盘面。
boardSet :: Board -> Pos -> Cell -> Board
boardSet (Board a) p v = Board (a // [(p, v)])

-- | 一次写多格（后写的覆盖先写的）。
boardSetMany :: Board -> [(Pos, Cell)] -> Board
boardSetMany (Board a) kvs = Board (a // kvs)

-- | 底层二维数组（(行, 列) 下标，行主序）。
boardArray :: Board -> Array Pos Cell
boardArray (Board a) = a

-- | 由二维数组建盘（与 boardArray 互逆）。
boardFromArray :: Array Pos Cell -> Board
boardFromArray = Board

-- | 逐格变换。
mapBoard :: (Cell -> Cell) -> Board -> Board
mapBoard f (Board a) = Board (fmap f a)

boardSize :: Int
boardSize = 8
