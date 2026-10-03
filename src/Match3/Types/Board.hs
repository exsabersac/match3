{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 盘面（第 6 刀从 Match3.Types 拆出）：坐标 Pos、以 (行, 列) 为下标的二维数组 Board 及其读写 / 转换。
--
-- Haskell 特性第 2 项（类型类与抽象，见 docs/haskell-features/02-类型类与抽象.md §1.4）：盘面的容器是
-- 多态的 'Grid' a，'Board' = Grid 'Cell'。Grid 有 Functor / Foldable / Traversable：逐格变换是 fmap，
-- 逐格统计是 foldMap / length / sum / toList，带状态的逐格生成（随机盘）是 mapAccumL / traverse，
-- 都按下标顺序（行主序）走、且不改形状（行列数、下标范围）。原来的 mapBoard / boardCells 保留为这些方法的别名。
--
-- Haskell 特性第 7 项（网格几何，见 docs/haskell-features/07-网格几何.md）：四个正交方向 'Dir' 与 'stepDir'，
-- 以及把仓库里原有的几种邻格顺序起成名字（'upDownLeftRight' / 'readingOrder' / 'clockwiseFromRight' / 'rightAndDown'）。
--
-- 依赖：Match3.Types.Cell。不变量：Show 按行列表打印，与旧列表盘输出相同。
module Match3.Types.Board
  ( Pos
    -- * 方向
  , Dir (..)
  , dirDelta
  , stepDir
  , dirBetween
  , neighborsIn
  , upDownLeftRight
  , readingOrder
  , clockwiseFromRight
  , rightAndDown
    -- * 盘面
  , Grid
  , Board
  , gridFromRows
  , gridRows
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
  , minBoardDim
  , maxBoardDim
  , validBoardDim
  , boardDims
  , boardNRows
  , boardNCols
  , boardPositions
  , boardRowIndices
  , boardColIndices
  ) where

import Data.Array (Array, assocs, bounds, listArray, (!), (//))
import Data.Foldable (toList)
import Data.List (find)
import Match3.Types.Cell (Cell)

-- | 格子坐标 (行, 列)；行向下增长、列向右增长。
type Pos = (Int, Int)

--------------------------------------------------------------------------------
-- 方向（第 7 项）

-- | 四个正交方向。行向下增长，所以 'North'（上）是行 − 1、'West'（左）是列 − 1。
-- 构造器顺序（也就是 Enum / Bounded / Ord 的顺序）= 上、下、左、右，与 'upDownLeftRight' 相同。
data Dir = North | South | West | East
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | 方向的 (行增量, 列增量)。
dirDelta :: Dir -> (Int, Int)
dirDelta d = case d of
  North -> (-1, 0)
  South -> (1, 0)
  West -> (0, -1)
  East -> (0, 1)

-- | 朝一个方向走一格（不查边界；越界由调用方用 inBounds 过滤）。
stepDir :: Dir -> Pos -> Pos
stepDir d (r, c) = let (dr, dc) = dirDelta d in (r + dr, c + dc)

-- | q 是 p 朝哪个方向走一格到的（不相邻、或是同一格时 Nothing）。
dirBetween :: Pos -> Pos -> Maybe Dir
dirBetween p q = find (\d -> stepDir d p == q) [minBound .. maxBound]

-- | 按给定的方向顺序列出邻格（不查边界）。
neighborsIn :: [Dir] -> Pos -> [Pos]
neighborsIn ds p = map (`stepDir` p) ds

-- 下面四个是仓库里原有的四种邻格顺序。它们决定「去重后谁在前」「取第一个」这类结果，
-- 金标准与元素查询快照依赖它们，所以各自起名、各自保留，不统一成一种。

-- | 上、下、左、右：邻消波及（Match3.Obstacles.orthoNeighbors）、叠层蔓延（Match3.Grass）、飞碟吸附（Match3.Ufo）。
upDownLeftRight :: [Dir]
upDownLeftRight = [North, South, West, East]

-- | 上、左、右、下：邻格按行主序排好的顺序（蔓延的来源格取第一个，Match3.Element.Event.spreadPairs）。
readingOrder :: [Dir]
readingOrder = [North, West, East, South]

-- | 右、下、左、上（顺时针，从右起）：飞碟没吸到东西时的移动候选（Match3.Ufo.moveUfo）。
clockwiseFromRight :: [Dir]
clockwiseFromRight = [East, South, West, North]

-- | 右、下：枚举相邻的格对时每对只数一次（提示搜索 Match3.Board.Match、Match3.Engine 的合法动作）。
rightAndDown :: [Dir]
rightAndDown = [East, South]
-- | 盘面：以 (行, 列) 为下标的二维数组（第三刀起；之前是 [[Cell]]，读格要走两次 (!!)）。
-- 读格 boardAt 为 O(1)；写格 boardSet 复制一次数组（64 格，与原来重建行列表同量级）。
-- 与行列表互转用 boardFromRows / boardRows（行主序，与旧表示逐格一一对应；Show 仍按行列表打印）。
--
-- 'Grid' 是盘面的形状（下标范围 + 行主序），格子类型是参数：Board = Grid Cell。实例的来源用 DerivingStrategies 写明：
--
-- * Eq / Functor / Foldable 走 newtype 策略：直接复用 Array 的实例（Array 的 length / elem 等都是 O(1) 或按 elems 走）。
-- * Traversable 只能走 stock 策略：traverse 的结果是 f (t b)，newtype 策略要把 f (Array Pos b) coerce 成 f (Grid b)，
--   而 f 的参数角色未知（可能是 nominal），GHC 拒绝；stock 派生按构造器结构生成 traverse f (Grid a) = Grid <$> traverse f a。
newtype Grid a = Grid (Array Pos a)
  deriving newtype (Eq, Functor, Foldable)
  deriving stock (Traversable)

-- | 盘面：格子的 Grid。
type Board = Grid Cell

-- | 按行列表打印（与第三刀前的 [[Cell]] 盘面输出相同；金标准锁定）。
instance Show a => Show (Grid a) where
  showsPrec d g = showsPrec d (gridRows g)

-- | 由行列表建网格（每行等长；空列表 = 0×0）。
gridFromRows :: [[a]] -> Grid a
gridFromRows rows =
  let nr = length rows
      nc = case rows of
        [] -> 0
        (r0 : _) -> length r0
  in if any ((/= nc) . length) rows
       then error "boardFromRows: rows of unequal length"
       else Grid (listArray ((0, 0), (nr - 1, nc - 1)) (concat rows))

-- | 行列表视图（行主序）。
gridRows :: Grid a -> [[a]]
gridRows (Grid a) =
  let ((r0, c0), (r1, c1)) = bounds a
  in [[a ! (r, c) | c <- [c0 .. c1]] | r <- [r0 .. r1]]

-- | 由行列表建盘（每行等长；空列表 = 0×0 盘）。
boardFromRows :: [[Cell]] -> Board
boardFromRows = gridFromRows

-- | 行列表视图（行主序）。
boardRows :: Board -> [[Cell]]
boardRows = gridRows

-- | 全部格子，行主序（= Foldable 的 toList）。
boardCells :: Board -> [Cell]
boardCells = toList

-- | 全部 (坐标, 格子)，行主序。
boardAssocs :: Board -> [(Pos, Cell)]
boardAssocs (Grid a) = assocs a

-- | 读一格，O(1)；越界报错（与旧 (!!) 相同）。
boardAt :: Board -> Pos -> Cell
boardAt (Grid a) p = a ! p

-- | 写一格，返回新盘面。
boardSet :: Board -> Pos -> Cell -> Board
boardSet (Grid a) p v = Grid (a // [(p, v)])

-- | 一次写多格（后写的覆盖先写的）。
boardSetMany :: Board -> [(Pos, Cell)] -> Board
boardSetMany (Grid a) kvs = Grid (a // kvs)

-- | 底层二维数组（(行, 列) 下标，行主序）。
boardArray :: Board -> Array Pos Cell
boardArray (Grid a) = a

-- | 由二维数组建盘（与 boardArray 互逆）。
boardFromArray :: Array Pos Cell -> Board
boardFromArray = Grid

-- | 逐格变换（= fmap，保留给旧调用点与 Match3.Core 的 API）。
mapBoard :: (Cell -> Cell) -> Board -> Board
mapBoard = fmap

-- | 缺省盘面边长（关卡未指定行列时用；旧关卡均为 8×8）。
boardSize :: Int
boardSize = 8

-- | 关卡允许的行列闭区间下界 / 上界（含）。
minBoardDim, maxBoardDim :: Int
minBoardDim = 5
maxBoardDim = 10

-- | 行列是否在允许范围内（含端点）；越界在加载时拒绝，不静默夹取。
validBoardDim :: Int -> Bool
validBoardDim n = n >= minBoardDim && n <= maxBoardDim

-- | 盘面实际行列数（行, 列）；由数组下界推出，可矩形。
boardDims :: Board -> (Int, Int)
boardDims (Grid a) =
  let ((r0, c0), (r1, c1)) = bounds a
  in (r1 - r0 + 1, c1 - c0 + 1)

boardNRows, boardNCols :: Board -> Int
boardNRows b = fst (boardDims b)
boardNCols b = snd (boardDims b)

-- | 全部坐标（行优先）。
boardPositions :: Board -> [Pos]
boardPositions b =
  [(r, c) | r <- boardRowIndices b, c <- boardColIndices b]

boardRowIndices, boardColIndices :: Board -> [Int]
boardRowIndices b =
  let ((r0, _), (r1, _)) = bounds (boardArray b)
  in [r0 .. r1]
boardColIndices b =
  let ((_, c0), (_, c1)) = bounds (boardArray b)
  in [c0 .. c1]

