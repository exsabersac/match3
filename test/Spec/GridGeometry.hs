{-# LANGUAGE OverloadedStrings #-}

-- | Haskell 特性第 7 项：网格几何（docs/haskell-features/07-网格几何.md）。
--
-- * 方向：'Dir' / 'stepDir' / 'dirBetween' 的基本性质，四种具名邻格顺序写死，并与字面量的坐标列表逐格逐项相同
--   （邻消 / 蔓延 / 飞碟的上下左右、飞碟移动的右下左上、蔓延来源的上左右下、交换枚举的右下）；
-- * 带坐标的折叠：'ifoldMap' / 'ifoldr' / 'ifoldl'' / 'positionsWhere' 都是行主序，与 @p <- boardPositions b@ 相同；
-- * UI 边界：像素 newtype 下的 'gridCellAt' / 'gridCellOrigin'、每日挑战 Year / Month / Day 的种子与配置写成固定例子
--   （期望值由现实现生成，生成时与删除前的逐字旧副本核对过）。
module Spec.GridGeometry
  ( tests
  ) where

import Data.List (sort)
import Engine.GridUI (GridGeom (..), PxX (..), PxY (..), pxXY)
import qualified Engine.GridUI as NewUI
import Match3.Board.Grid (inBounds)
import Match3.Core
import Match3.Board.Grid (adjacent, neighborsInBounds)
import Match3.Daily (dailySeed)
import Match3.Types (boardDims, boardPositions, positionsWhere, upDownLeftRight)
import Match3.Daily (dailyConfig)
import qualified Match3.Obstacles as NewObs
import Match3.Types
  ( Dir(..)
  , boardAssocs
  , clockwiseFromRight
  , colorAt
  , dirBetween
  , dirDelta
  , gridFromRows
  , gridRows
  , ifoldMap
  , ifoldl'
  , ifoldr
  , mkSnailFacing
  , neighborsIn
  , numColors
  , readingOrder
  , rightAndDown
  , snailFacing
  , stepDir
  )
import Spec.Properties (genCell, genColor, genGem, genOverlay)
import Spec.Support.Arbitrary (shrinkBoard)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testCase "grid_dir_basics" grid_dir_basics
  , testProperty "qc_grid_neighbor_orders_literal" (withMaxSuccess 300 qc_grid_neighbor_orders_literal)
  , testProperty "qc_grid_indexed_folds_row_major" (withMaxSuccess 300 qc_grid_indexed_folds_row_major)
  , testCase "grid_ui_px_pinned" grid_ui_px_pinned
  , testCase "daily_date_pinned" daily_date_pinned
  ]

--------------------------------------------------------------------------------
-- 生成器

-- | 网格几何关心的格：蜗牛、倒计时（含 0）、毛球、变色龙、魔法石、带叠层的宝石、彩虹，混在任意格里。
genGeoCell :: Gen Cell
genGeoCell =
  frequency
    [ (4, genGem)
    , (3, genCell)
    , (1, elements [Snail 0 1, Snail 0 (-1), Snail 1 0, Snail (-1) 0])
    , (1, Countdown <$> genColor <*> choose (0, 2))
    , (1, pure (Custom "fuzzball" (CustomState 0)))
    , (1, Custom "chameleon" . CustomState <$> choose (0, 4))
    , (1, Custom "magic_stone" . CustomState <$> choose (0, 4))
    , (1, (\c o -> Gem c Normal 0 (Just o)) <$> genColor <*> genOverlay)
    , (1, (\c -> Gem c Rainbow 0 Nothing) <$> genColor)
    ]

-- | 任意行列（1–8）的盘。
genGeoBoard :: Gen Board
genGeoBoard = do
  r <- choose (1, 8)
  c <- choose (1, 8)
  boardFromRows <$> vectorOf r (vectorOf c genGeoCell)

--------------------------------------------------------------------------------
-- 方向

grid_dir_basics :: Assertion
grid_dir_basics = do
  [minBound .. maxBound] @?= [North, South, West, East]
  map dirDelta [North, South, West, East] @?= [(-1, 0), (1, 0), (0, -1), (0, 1)]
  map (`stepDir` (3, 5)) [North, South, West, East] @?= [(2, 5), (4, 5), (3, 4), (3, 6)]
  -- 四种具名顺序（金标准依赖它们，写死在这里）
  upDownLeftRight @?= [North, South, West, East]
  readingOrder @?= [North, West, East, South]
  clockwiseFromRight @?= [East, South, West, North]
  rightAndDown @?= [East, South]
  -- 上左右下 = 邻格按坐标排好序（行主序）；三种完整顺序都是四个方向的排列
  sequence_ [neighborsIn readingOrder p @?= sort (neighborsIn upDownLeftRight p) | p <- [(r, c) | r <- [-2 .. 3], c <- [-2 .. 3]]]
  sequence_ [sort ds @?= [minBound .. maxBound] | ds <- [upDownLeftRight, readingOrder, clockwiseFromRight]]
  -- dirBetween 是 stepDir 的逆；不相邻（含同一格、对角、隔一格）时 Nothing
  sequence_ [dirBetween p (stepDir d p) @?= Just d | d <- [minBound .. maxBound], p <- [(0, 0), (4, -3), (-1, 7)]]
  sequence_ [dirBetween (2, 2) q @?= Nothing | q <- [(2, 2), (3, 3), (1, 1), (0, 2), (2, 4), (4, 2)]]
  -- adjacent / orthoAdjacent：曼哈顿距离恰好为 1
  let ps = [(r, c) | r <- [-1 .. 4], c <- [-1 .. 4]]
      manhattan1 (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) == 1
  sequence_
    [ do
        assertEqual ("adjacent " ++ show (p, q)) (manhattan1 p q) (adjacent p q)
        assertEqual ("orthoAdjacent " ++ show (p, q)) (manhattan1 p q) (NewUI.orthoAdjacent p q)
    | p <- ps, q <- ps
    ]
  -- 蜗牛：构造器与 Show 不变，方向写法只在 API 层
  map mkSnailFacing [North, South, West, East] @?= [Snail (-1) 0, Snail 1 0, Snail 0 (-1), Snail 0 1]
  show (mkSnailFacing East) @?= "Snail 0 1"
  sequence_ [mkSnailFacing d @?= uncurry mkSnail (dirDelta d) | d <- [minBound .. maxBound]]
  sequence_ [snailFacing (mkSnailFacing d) @?= Just d | d <- [minBound .. maxBound]]
  map snailFacing [Snail 0 0, Snail 2 0, Snail 1 1, mkGem C1] @?= [Nothing, Nothing, Nothing, Nothing]

-- | 每处具名顺序的邻格列表，与写成字面量的坐标列表逐项相同（含越界格、含顺序）。
qc_grid_neighbor_orders_literal :: Property
qc_grid_neighbor_orders_literal =
  forAllShrink genGeoBoard shrinkBoard $ \b ->
    let (nr, nc) = boardDims b
        -- 盘外一圈也试：邻格函数不要求中心格在盘内
        ps = [(r, c) | r <- [-1 .. nr], c <- [-1 .. nc]]
        one p@(r, c) =
          conjoin
            [ counterexample ("orthoNeighbors " ++ show p) (NewObs.orthoNeighbors p === [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)])
            , counterexample ("邻消 / 蔓延 / 飞碟 " ++ show p)
                (neighborsInBounds upDownLeftRight b p === filter (inBounds b) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)])
            , counterexample ("moveUfo cands " ++ show p)
                (neighborsInBounds clockwiseFromRight b p === filter (inBounds b) [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)])
            , counterexample ("spreadPairs srcs " ++ show p)
                (neighborsInBounds readingOrder b p === [n | n <- [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)], inBounds b n])
            , counterexample ("findHint p2 " ++ show p)
                (neighborsInBounds rightAndDown b p === [q | q <- [(r, c + 1), (r + 1, c)], inBounds b q])
            ]
     in conjoin (map one ps)
          .&&. counterexample "swap candidates"
            ([(p, q) | p <- boardPositions b, q <- neighborsInBounds rightAndDown b p]
               === [((r, c), q) | r <- [0 .. fst (boardDims b) - 1], c <- [0 .. snd (boardDims b) - 1], q <- [(r, c + 1), (r + 1, c)], inBounds b q])

-- | 带坐标的折叠都是行主序（= boardPositions / boardAssocs），对任意元素类型、任意形状（含 0×0）成立。
qc_grid_indexed_folds_row_major :: Property
qc_grid_indexed_folds_row_major =
  forAll (choose (0, 7)) $ \r ->
    forAll (choose (0, 7)) $ \c ->
      forAll (vectorOf r (vectorOf c (choose (0, 9 :: Int)))) $ \rows ->
        let g = gridFromRows (if c == 0 then [] else rows)
            b = fmap (\k -> mkGem (colorAt (k `mod` numColors))) g
            assocsNew = ifoldMap (\p x -> [(p, x)]) g
            ps = boardPositions b
            xs = concat (gridRows g)
         in conjoin
              [ counterexample "ifoldMap = zip boardPositions" (assocsNew === zip ps xs)
              , counterexample "boardAssocs" (map fst (boardAssocs b) === ps)
              , counterexample "ifoldr" (ifoldr (\p x acc -> (p, x) : acc) [] g === assocsNew)
              , counterexample "ifoldl'" (ifoldl' (\acc p x -> acc ++ [(p, x)]) [] g === assocsNew)
              , counterexample "positionsWhere" (positionsWhere even g === [p | (p, x) <- zip ps xs, even x])
              , counterexample "positionsWhere 惰性取前缀" (take 1 (positionsWhere (const True) g) === take 1 ps)
              ]

-- | 换算用的三种网格：棋盘几何（16, 124, 56, 8×8）、非正方的 3 行 5 列（左上角为负）、0 行。
pxGeoms :: [GridGeom Int]
pxGeoms = [GridGeom 16 124 56 8 8, GridGeom (-10) 30 7 3 5, GridGeom 0 0 10 0 4]

-- | 像素探针：网格内外、格边界 ±1 像素。
pxProbes :: [(Int, Int)]
pxProbes = [(-1, -1), (0, 0), (15, 123), (16, 124), (71, 179), (72, 180), (463, 571), (464, 572), (30, 200), (300, 130), (-10, 30), (24, 50), (25, 51)]

-- | 格探针：盘内、行列不同的格、盘外。
pxCells :: [(Int, Int)]
pxCells = [(0, 0), (0, 1), (1, 0), (2, 4), (7, 7), (-1, 3), (8, 0)]

-- | 像素 newtype 下的 'gridCellAt' / 'gridCellOrigin' 写死（三种网格 × 像素探针 / 格探针）。
-- (行, 列) 解构写反时，非正方网格与 (2,4) 这类格立刻不同。
grid_ui_px_pinned :: Assertion
grid_ui_px_pinned =
  sequence_
    [ do
        assertEqual ("gridCellAt " ++ show g) cellsAt [NewUI.gridCellAt g (PxX x) (PxY y) | (x, y) <- pxProbes]
        assertEqual ("gridCellOrigin " ++ show g) origins [pxXY (NewUI.gridCellOrigin g rc) | rc <- pxCells]
    | (g, (cellsAt, origins)) <- zip pxGeoms pinnedPx
    ]

-- | grid_ui_px_pinned 的期望（由现实现生成，生成时与删除前的裸 Int 版副本核对过）。
pinnedPx :: [([Maybe (Int, Int)], [(Int, Int)])]
pinnedPx =
  [ ([Nothing,Nothing,Nothing,Just (0,0),Just (0,0),Just (1,1),Just (7,7),Nothing,Just (1,0),Just (0,5),Nothing,Nothing,Nothing],[(16,124),(72,124),(16,180),(240,236),(408,516),(184,68),(16,572)])
  , ([Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just (0,0),Just (2,4),Nothing],[(-10,30),(-3,30),(-10,37),(18,44),(39,79),(11,23),(-10,86)])
  , ([Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[(0,0),(10,0),(0,10),(40,20),(70,70),(30,-10),(0,80)])
  ]

-- | 每日挑战的日期：闰日、跨年、月底等。
dailyDates :: [(Int, Int, Int)]
dailyDates = [(2024, 2, 29), (2025, 1, 1), (2025, 12, 31), (2026, 9, 29), (2026, 10, 3), (2027, 6, 15), (2030, 12, 31)]

-- | Year / Month / Day 包起来之后的种子（YYYYMMDD）与当日配置写死。
daily_date_pinned :: Assertion
daily_date_pinned =
  sequence_
    [ do
        assertEqual ("seed " ++ show (y, m, d)) seed (dailySeed (Year y) (Month m) (Day d))
        assertEqual ("config " ++ show (y, m, d)) cfg (show (dailyConfig (Year y) (Month m) (Day d)))
    | ((y, m, d), (seed, cfg)) <- zip dailyDates pinnedDaily
    ]

-- | daily_date_pinned 的期望（由现实现生成，生成时与删除前的三个 Int 版副本核对过）。
pinnedDaily :: [(Int, String)]
pinnedDaily =
  [ (20240229,"GameConfig {cfgMoves = 26, cfgGoal = GoalBalloon 6}")
  , (20250101,"GameConfig {cfgMoves = 28, cfgGoal = GoalCollect C1 18}")
  , (20251231,"GameConfig {cfgMoves = 28, cfgGoal = GoalCollect C1 18}")
  , (20260929,"GameConfig {cfgMoves = 26, cfgGoal = GoalBalloon 6}")
  , (20261003,"GameConfig {cfgMoves = 26, cfgGoal = GoalClearStone 6}")
  , (20270615,"GameConfig {cfgMoves = 26, cfgGoal = GoalUfo 8}")
  , (20301231,"GameConfig {cfgMoves = 28, cfgGoal = GoalCollect C1 18}")
  ]
