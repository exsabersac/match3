{-# LANGUAGE OverloadedStrings #-}

-- | Haskell 特性第 7 项：网格几何（docs/haskell-features/07-网格几何.md）。
--
-- 每处改写都和改写前的逐字副本对照（Spec.Support.LegacyGeometry）：
--
-- * 方向：'Dir' / 'stepDir' / 'dirBetween' 的基本性质，四种具名邻格顺序与原来手写的四份邻格列表逐格逐项相同
--   （邻消 / 蔓延 / 飞碟的上下左右、飞碟移动的右下左上、蔓延来源的上左右下、交换枚举的右下）；
-- * 带坐标的折叠：'ifoldMap' / 'ifoldr' / 'ifoldl'' / 'positionsWhere' 都是行主序，与 @p <- boardPositions b@ 相同；
--   改写过的遍历（蜗牛 / 倒计时 / 洗牌保留 / 彩虹 / 毛球 / 变色龙 / 魔法石 / 叠层）与旧写法逐项相同；
-- * UI 边界：像素 newtype 下的 'gridCellAt' / 'gridCellOrigin' 与旧的裸 Int 版逐点相同；每日挑战的 Year / Month / Day 与旧的三个 Int 相同。
module Spec.GridGeometry
  ( tests
  ) where

import Data.List (sort)
import Engine.GridUI (GridGeom (..), PxX (..), PxY (..), pxXY)
import qualified Engine.GridUI as NewUI
import Engine.Optics (has)
import Match3.Core
import Match3.Element (defaultRegistry)
import qualified Match3.Element.Event as NewEv
import qualified Match3.Game.Shuffle as NewSh
import qualified Match3.Obstacles as NewObs
import qualified Match3.Rainbow as NewRb
import qualified Match3.Snail as NewSn
import qualified Match3.Countdown as NewCd
import qualified Match3.Ufo as NewUfo
import Match3.Element.Builtin.Collectible (chameleonColor)
import Match3.Types (boardSetMany, gridFromRows, gridRows)
import Match3.Types.Optics (overlay, _Chain, _Fog)
import Spec.Properties (genCell, genColor, genGem, genOverlay)
import Spec.Support.Arbitrary (shrinkBoard)
import qualified Spec.Support.LegacyGeometry as Old
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testCase "grid_dir_basics" grid_dir_basics
  , testProperty "qc_grid_neighbor_orders_same_as_legacy" (withMaxSuccess 300 qc_grid_neighbor_orders_same_as_legacy)
  , testProperty "qc_grid_indexed_folds_row_major" (withMaxSuccess 300 qc_grid_indexed_folds_row_major)
  , testProperty "qc_grid_fold_sites_same_as_legacy" (withMaxSuccess 500 qc_grid_fold_sites_same_as_legacy)
  , testProperty "qc_grid_spread_ufo_same_as_legacy" (withMaxSuccess 500 qc_grid_spread_ufo_same_as_legacy)
  , testProperty "qc_grid_ui_px_same_as_legacy" (withMaxSuccess 1000 qc_grid_ui_px_same_as_legacy)
  , testCase "daily_date_newtypes_same_as_legacy" daily_date_newtypes_same_as_legacy
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

-- | 盘内任意一格。
genPosIn :: Board -> Gen Pos
genPosIn b = elements (boardPositions b)

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
  -- adjacent / orthoAdjacent 与旧写法逐对相同
  let ps = [(r, c) | r <- [-1 .. 4], c <- [-1 .. 4]]
  sequence_
    [ do
        assertEqual ("adjacent " ++ show (p, q)) (Old.adjacent p q) (adjacent p q)
        assertEqual ("orthoAdjacent " ++ show (p, q)) (Old.orthoAdjacent p q) (NewUI.orthoAdjacent p q)
    | p <- ps, q <- ps
    ]
  -- 蜗牛：构造器与 Show 不变，方向写法只在 API 层
  map mkSnailFacing [North, South, West, East] @?= [Snail (-1) 0, Snail 1 0, Snail 0 (-1), Snail 0 1]
  show (mkSnailFacing East) @?= "Snail 0 1"
  sequence_ [mkSnailFacing d @?= uncurry mkSnail (dirDelta d) | d <- [minBound .. maxBound]]
  sequence_ [snailFacing (mkSnailFacing d) @?= Just d | d <- [minBound .. maxBound]]
  map snailFacing [Snail 0 0, Snail 2 0, Snail 1 1, mkGem C1] @?= [Nothing, Nothing, Nothing, Nothing]

-- | 每处换成具名顺序的邻格列表，与原来手写的列表逐项相同（含越界格、含顺序）。
qc_grid_neighbor_orders_same_as_legacy :: Property
qc_grid_neighbor_orders_same_as_legacy =
  forAllShrink genGeoBoard shrinkBoard $ \b ->
    let (nr, nc) = boardDims b
        -- 盘外一圈也试：邻格函数不要求中心格在盘内
        ps = [(r, c) | r <- [-1 .. nr], c <- [-1 .. nc]]
        one p@(r, c) =
          conjoin
            [ counterexample ("orthoNeighbors " ++ show p) (NewObs.orthoNeighbors p === Old.orthoNeighbors p)
            , counterexample ("Grass.ortho " ++ show p) (neighborsInBounds upDownLeftRight b p === Old.grassOrtho b p)
            , counterexample ("Ufo.ortho " ++ show p) (neighborsInBounds upDownLeftRight b p === Old.ufoOrtho b p)
            , counterexample ("moveUfo cands " ++ show p)
                (neighborsInBounds clockwiseFromRight b p === filter (inBounds b) [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)])
            , counterexample ("spreadPairs srcs " ++ show p)
                (neighborsInBounds readingOrder b p === [n | n <- [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)], inBounds b n])
            , counterexample ("findHint p2 " ++ show p)
                (neighborsInBounds rightAndDown b p === [q | q <- [(r, c + 1), (r + 1, c)], inBounds b q])
            ]
     in conjoin (map one ps)
          .&&. counterexample "swap candidates"
            ([(p, q) | p <- boardPositions b, q <- neighborsInBounds rightAndDown b p] === Old.swapCandidates b)

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

-- | 改写成 positionsWhere / ifoldMap 的遍历与旧的 @p <- boardPositions b@ 写法逐项相同。
qc_grid_fold_sites_same_as_legacy :: Property
qc_grid_fold_sites_same_as_legacy =
  forAllShrink genGeoBoard shrinkBoard $ \b ->
    forAll (genPosIn b) $ \p1 ->
      forAll (genPosIn b) $ \p2 ->
        forAll genColor $ \col ->
          let bR = setCell b p1 (Gem col Rainbow 0 Nothing)
           in cover 30 (not (null (Old.snailPositions b))) "有蜗牛" $
              cover 30 (not (null (Old.countdownsAtZero b))) "有归零倒计时" $
              cover 30 (length (Old.rainbowClearSeeds bR p1 p2) > 2) "彩虹清出多格" $
              conjoin
                [ counterexample "snailPositions" (NewSn.snailPositions b === Old.snailPositions b)
                , counterexample "countdownsAtZero" (NewCd.countdownsAtZero b === Old.countdownsAtZero b)
                , counterexample "extractDecorWith" (NewSh.extractDecorWith defaultRegistry b === Old.extractDecorWith defaultRegistry b)
                , counterexample ("rainbowClearSeeds " ++ show (p1, p2)) (NewRb.rainbowClearSeeds b p1 p2 === Old.rainbowClearSeeds b p1 p2)
                , counterexample ("rainbowClearSeeds（p1 放彩虹）" ++ show (p1, p2)) (NewRb.rainbowClearSeeds bR p1 p2 === Old.rainbowClearSeeds bR p1 p2)
                , -- 下面三处在模块内部，这里抄的是新写法本身（与源码同一行表达式）
                  counterexample "fuzzball balls" (positionsWhere Old.isFuzzball b === Old.fuzzballPositions b)
                , counterexample "chameleon same color" (positionsWhere ((== Just col) . chameleonColor) b === Old.chameleonPositions col b)
                , counterexample "magicStones"
                    (ifoldMap (\p cell -> [(p, k) | Custom "magic_stone" (CustomState k) <- [cell]]) b === Old.magicStones b)
                , counterexample "Grass.positionsWith _Fog" (positionsWhere (has (overlay . _Fog)) b === Old.positionsWith (overlay . _Fog) b)
                , counterexample "Grass.positionsWith _Chain" (positionsWhere (has (overlay . _Chain)) b === Old.positionsWith (overlay . _Chain) b)
                ]

-- | 蔓延的 (来源, 新格) 与飞碟移动：与旧写法逐项相同。蔓延用「随机把一些格盖上同一种叠层」的前后两盘。
qc_grid_spread_ufo_same_as_legacy :: Property
qc_grid_spread_ufo_same_as_legacy =
  forAllShrink genGeoBoard shrinkBoard $ \before ->
    forAll genOverlay $ \ov ->
      forAll (vectorOf (length (boardPositions before)) (frequency [(2, pure False), (1, pure True)])) $ \mask ->
        forAll (genPosIn before) $ \u ->
          forAll genColor $ \col ->
            forAll (sublistOf (boardPositions before)) $ \absorbed ->
              let grown = [(p, Gem c0 Normal 0 (Just ov)) | ((p, cell), True) <- zip (boardAssocs before) mask, Just c0 <- [cellColor cell]]
                  afterB = boardSetMany before grown
                  ufo = NewUfo.Ufo u col
               in cover 50 (not (null (Old.spreadPairs ov before afterB))) "有蔓延的新格" $
                  conjoin
                    [ counterexample "spreadPairs" (NewEv.spreadPairs ov before afterB === Old.spreadPairs ov before afterB)
                    , counterexample "moveUfo (no absorb)" (NewUfo.moveUfo before [] ufo === Old.moveUfo before [] ufo)
                    , counterexample "moveUfo" (NewUfo.moveUfo before absorbed ufo === Old.moveUfo before absorbed ufo)
                    ]

-- | 像素 newtype 下的换算与旧的裸 Int 版逐点相同（任意左上角、单格边长、行列数，含 0 行 / 0 列）。
qc_grid_ui_px_same_as_legacy :: Property
qc_grid_ui_px_same_as_legacy =
  forAll genGeom $ \g ->
    forAll (choose (-100, 800)) $ \x ->
      forAll (choose (-100, 800)) $ \y ->
        forAll ((,) <$> choose (-2, 11) <*> choose (-2, 11)) $ \rc ->
          conjoin
            [ counterexample "gridCellAt" (NewUI.gridCellAt g (PxX x) (PxY y) === Old.gridCellAt g x y)
            , counterexample "gridCellOrigin" (pxXY (NewUI.gridCellOrigin g rc) === Old.gridCellOrigin g rc)
            ]
  where
    genGeom :: Gen (GridGeom Int)
    genGeom = GridGeom <$> choose (-50, 50) <*> choose (-50, 150) <*> choose (1, 60) <*> choose (0, 10) <*> choose (0, 10)

-- | Year / Month / Day 包起来之后，种子与目标轮换和旧的三个 Int 逐日相同。
daily_date_newtypes_same_as_legacy :: Assertion
daily_date_newtypes_same_as_legacy =
  sequence_
    [ do
        assertEqual ("seed " ++ show (y, m, d)) (Old.dailySeed y m d) (dailySeed (Year y) (Month m) (Day d))
        assertEqual ("config " ++ show (y, m, d)) (Old.dailyConfig y m d) (dailyConfig (Year y) (Month m) (Day d))
    | y <- [2024 .. 2030], m <- [1 .. 12], d <- [1 .. 31]
    ]
