-- | 源码扫描工具（Spec.Support.Source）自身的测试：注释剥离、import 解析、标识符匹配。
-- 其他模块的源码扫描测试（element_registry_custom_crate_extensibility、ext_board_modules_take_registry、
-- ec_flat_record_removed、jb_main_flow_untouched_scan、br_main_flow_no_special_branches、
-- engine_layer_is_game_agnostic、engine_frontend_steps_only_via_gameStep）都建立在这些行为上。
module Spec.SourceScan
  ( tests
  ) where

import Spec.Support.Source
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "support_source_scanner" support_source_scanner
  ]

support_source_scanner :: Assertion
support_source_scanner = do
  let src =
        unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , "module M (f) where"
          , "import qualified Match3.Combos as C -- trailing comment: import Match3.Rainbow"
          , "import \"pkg\" Data.Map (Map)"
          , "-- import Match3.Obstacles"
          , "{- outer {- nested stepUfos -} still comment beltMoves -}"
          , "f = x --> y  -- coverCarpets"
          , "g = \"a -- not a comment {- nor this\" ++ ['\"', '\\'', '-'] ++ h'"
          , "k = M3E.play 1"
          ]
      code = stripComments src
  assertEqual "keeps every line" (length (lines src)) (length (lines code))
  assertEqual "imports (comments ignored, qualified / package imports parsed)" ["Match3.Combos", "Data.Map"] (importsOf src)
  assertBool "nested block comment removed" (not (any (`elem` codeIdents src) ["stepUfos", "beltMoves", "nested", "still"]))
  assertBool "line comment removed, operator --> kept" ("x --> y" `elem` map (take 7 . dropWhile (/= 'x')) (lines code))
  assertBool "trailing comment removed" (not (mentionsIdent "coverCarpets" src))
  assertBool "string literal kept by stripComments" (any (== "g = \"a -- not a comment {- nor this\" ++ ['\"', '\\'', '-'] ++ h'") (lines code))
  assertBool "strings ignored by codeIdents" (not (mentionsIdent "comment" src) && not (mentionsIdent "nor" src))
  assertBool "identifier after literals still seen" (mentionsIdent "h'" src)
  assertBool "qualified name matches base name" (mentionsIdent "play" src && not (mentionsIdent "pla" src))
