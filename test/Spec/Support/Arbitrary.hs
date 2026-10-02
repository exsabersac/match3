-- | 自定义 Arbitrary（Haskell 特性第 6 项，docs/haskell-features/06-测试与光学.md）：任意行列的盘面，带 shrink。
--
-- 原有的盘面性质都用 forAll + 生成器（genCell / genPlayBoard / genMBoard 等，Spec.Properties 里固定 8×8），失败时 QuickCheck
-- 不会缩小反例，报出来的是一整张盘。这里的两个 newtype 带 shrink：
--
-- * 先缩形状：去掉最后一行 / 最后一列（至少留 1×1），盘面始终是矩形；
-- * 再缩格子：每次只把一格换成更简单的格（'shrinkCell'：宝石去叠层 → 去冰 → 变普通 → 换成 C1；其他本体 → 普通 C1 宝石；
--   可空盘的空洞保持空洞，实格也可以缩成空洞）。
--
-- 每一步都比原来「严格更简单」（行列数或格子复杂度下降），所以缩小一定会停。
module Spec.Support.Arbitrary
  ( AnyBoard(..)
  , HoledBoard(..)
  , shrinkCell
  , shrinkBoard
  , genAnyBoard
  , genHoledBoard
  ) where

import Data.Maybe (isJust)
import Match3.Board.Grid (MBoard, mboardFromRows, mboardRows)
import Match3.Core
import Spec.Properties (genCell, genColor)
import Test.Tasty.QuickCheck

-- | 任意行列（1–10）的完整盘面：宝石为主，混入全部内置本体与叠层。
newtype AnyBoard = AnyBoard Board
  deriving (Eq, Show)

-- | 任意行列（1–10）的可空盘面（约 1/3 是空洞）。
newtype HoledBoard = HoledBoard MBoard
  deriving (Eq, Show)

genAnyBoard :: Gen Board
genAnyBoard = do
  r <- choose (1, 10)
  c <- choose (1, 10)
  boardFromRows <$> vectorOf r (vectorOf c (frequency [(3, mkGem <$> genColor), (2, genCell)]))

genHoledBoard :: Gen MBoard
genHoledBoard = do
  r <- choose (1, 10)
  c <- choose (1, 10)
  mboardFromRows <$> vectorOf r (vectorOf c (frequency [(2, Just <$> genCell), (1, pure Nothing)]))

instance Arbitrary AnyBoard where
  arbitrary = AnyBoard <$> genAnyBoard
  shrink (AnyBoard b) = map AnyBoard (shrinkBoard b)

instance Arbitrary HoledBoard where
  arbitrary = HoledBoard <$> genHoledBoard
  shrink (HoledBoard mb) = map (HoledBoard . mboardFromRows) (shrinkRows shrinkSlot (mboardRows mb))
    where
      shrinkSlot Nothing = []
      shrinkSlot (Just x) = Nothing : map Just (shrinkCell x)

-- | 盘面的缩小（AnyBoard 的 shrink；给 forAllShrink 用）。
shrinkBoard :: Board -> [Board]
shrinkBoard b = map boardFromRows (shrinkRows shrinkCell (boardRows b))

-- | 行列表的缩小：先去最后一行 / 最后一列，再逐格缩。
shrinkRows :: (a -> [a]) -> [[a]] -> [[[a]]]
shrinkRows cellShrink rows =
  [init rows | length rows > 1]
    ++ [map init rows | all ((> 1) . length) rows]
    ++ [ replaceAt i (replaceAt j x' row) rows
       | (i, row) <- zip [0 ..] rows
       , (j, x) <- zip [0 ..] row
       , x' <- cellShrink x
       ]
  where
    replaceAt k v xs = [if n == k then v else y | (n, y) <- zip [0 :: Int ..] xs]

-- | 一格的「更简单」版本（每个都严格更简单；普通 C1 宝石没有更简单的）。
shrinkCell :: Cell -> [Cell]
shrinkCell cell = case cell of
  Gem c k i o ->
    [Gem c k i Nothing | isJust o]
      ++ [Gem c k 0 o | i > 0]
      ++ [Gem c Normal i o | k /= Normal]
      ++ [Gem C1 k i o | c /= C1]
  _ -> [mkGem C1]
