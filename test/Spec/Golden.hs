{-# LANGUAGE ScopedTypeVariables #-}

-- | 行为金标准：Golden.goldenLines 与 test/golden/golden.txt 逐行全等。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Golden
  ( tests
  ) where

import Control.Concurrent (getNumCapabilities)
import qualified Golden
import Spec.Support.Parallel (forceLines, parallelForce)
import Test.Tasty
import Test.Tasty.HUnit

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "golden_behaviour_snapshot" golden_behaviour_snapshot
  ]

-- | 行为金标准：test/golden/Golden.hs 的投影输出必须与入库的 golden.txt 逐行全等
-- （重构护栏；前 2186 行在 3bd26d8 与 5eef3e3 上生成且全等，末尾 H4–H6 在 31275da 与 13094d1 上生成且全等）。失败时报告第一处分叉的行号与两边内容。
golden_behaviour_snapshot :: Assertion
golden_behaviour_snapshot = do
  expected <- lines <$> readFile "test/golden/golden.txt"
  -- 第 5 项：各段（Golden.goldenSections，concat = goldenLines）在所有核上并行求值、按原顺序拼回；比对与之前逐字相同
  workers <- getNumCapabilities
  actual <- concat <$> parallelForce workers forceLines Golden.goldenSections
  let diffs = [ (i, e, a) | (i, e, a) <- zip3 [1 :: Int ..] expected actual, e /= a ]
  case diffs of
    ((i, e, a) : _) ->
      assertFailure ("golden line " ++ show i ++ " differs (" ++ show (length diffs) ++ " lines differ)\nexpected: " ++ take 400 e ++ "\nactual:   " ++ take 400 a)
    [] -> assertEqual "golden line count" (length expected) (length actual)
