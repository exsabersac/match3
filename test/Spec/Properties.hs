{-# LANGUAGE ScopedTypeVariables #-}

-- | QuickCheck 性质测试。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Properties
  ( tests
  ) where

import Match3.Core
import Test.Tasty
import Test.Tasty.QuickCheck

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testProperty "qc_findMatches_ge3" qc_findMatches_ge3
  ]

qc_findMatches_ge3 :: Property
qc_findMatches_ge3 =
  forAll (choose (0, boardSize - 1)) $ \r ->
    forAll (choose (0, boardSize - 3)) $ \c0 ->
      let fill = mkGem C5
          b0 = replicate boardSize (replicate boardSize fill)
          row =
            [ if c >= c0 && c < c0 + 3 then mkGem C1 else mkGem C5
            | c <- [0 .. boardSize - 1]
            ]
          b = boardFromRows $ take r b0 ++ [row] ++ drop (r + 1) b0
          ms = findMatches b
      in (r, c0) `elem` ms
           && (r, c0 + 1) `elem` ms
           && (r, c0 + 2) `elem` ms
