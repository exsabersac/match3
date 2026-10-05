-- | 元素对照快照（元素类重构第 0 刀起）：test/golden/element-oracle.txt 由重构前的实现生成，
-- 之后每一刀逐行比对（生成与覆盖面见 test/golden/ElementOracle.hs）。内部 API 变了只改投影，快照不动。
module Spec.ElementOracle
  ( tests
  ) where

import Data.List (isPrefixOf)
import qualified ElementOracle
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "oracle_values_match" (part ["V "])
  , testCase "oracle_names_and_placement_match" (part ["N ", "P "])
  , testCase "oracle_rules_per_rule_match" (part ["AR ", "ER ", "SR ", "SO "])
  , testCase "oracle_round_queries_match" (part ["RA ", "OP ", "CH ", "ST ", "BW ", "HG ", "CT "])
  ]

-- | 快照里以给定前缀开头的行，两边逐行比；报告第一处分叉。
part :: [String] -> Assertion
part prefixes = do
  expected <- lines <$> readFile "test/golden/element-oracle.txt"
  let sel = filter (\l -> any (`isPrefixOf` l) prefixes)
      e = sel expected
      a = sel ElementOracle.oracleLines
  assertBool "oracle part not empty" (not (null e))
  case [(i, x, y) | (i, x, y) <- zip3 [1 :: Int ..] e a, x /= y] of
    ((i, x, y) : _) -> assertFailure ("line " ++ show i ++ " differs\nexpected: " ++ take 400 x ++ "\nactual:   " ++ take 400 y)
    [] -> assertEqual "line count" (length e) (length a)
