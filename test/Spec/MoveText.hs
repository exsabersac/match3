-- | 道具点选模式规则（app/pure/UI/MoveText.hs 的 keepsTool；网页 m3FreeSwap 的 keepTool 字段）。
-- 原桌面走步提示文案 moveMsg 的逐字用例随 SDL2 前端移除（refactor/web-only）。
module Spec.MoveText
  ( tests
  ) where

import Match3.Core
import Test.Tasty
import Test.Tasty.HUnit
import UI.MoveText

tests :: [TestTree]
tests =
  [ testCase "move_text_keeps_tool_only_free_swap_no_match" move_text_keeps_tool_only_free_swap_no_match
  ]

-- | 每种结果各一个代表值。
outcomes :: [Outcome]
outcomes = [InvalidSwap, NoMatch, MoveApplied 120, LevelClear 450 3, Won 9000, Lost 210]

move_text_keeps_tool_only_free_swap_no_match :: Assertion
move_text_keeps_tool_only_free_swap_no_match =
  [(ui, o) | ui <- [minBound .. maxBound], o <- outcomes, keepsTool ui o] @?= [(UiFreeSwap, NoMatch)]
