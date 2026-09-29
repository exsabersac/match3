-- | 测试入口：只汇总各功能模块（test/Spec/*.hs）的测试。
-- 各模块导出平铺的 [TestTree]，这里拼进同一个顶层组 "match3"，
-- 因此 --list-tests 的完整路径（match3.<测试名>）与拆分前逐字相同。
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Spec.GridMatch
import qualified Spec.Gravity
import qualified Spec.Cascade
import qualified Spec.Specials
import qualified Spec.Obstacles.Body
import qualified Spec.Obstacles.Layers
import qualified Spec.Obstacles.Features
import qualified Spec.Boosters
import qualified Spec.GoalsLevels
import qualified Spec.Element
import qualified Spec.Extension
import qualified Spec.Branches
import qualified Spec.Engine
import qualified Spec.UIEvents
import qualified Spec.ReplayUndo
import qualified Spec.Golden
import qualified Spec.Properties

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "match3"
    ( concat
        [ Spec.GridMatch.tests
        , Spec.Gravity.tests
        , Spec.Cascade.tests
        , Spec.Specials.tests
        , Spec.Obstacles.Body.tests
        , Spec.Obstacles.Layers.tests
        , Spec.Obstacles.Features.tests
        , Spec.Boosters.tests
        , Spec.GoalsLevels.tests
        , Spec.Element.tests
        , Spec.Extension.tests
        , Spec.Branches.tests
        , Spec.Engine.tests
        , Spec.UIEvents.tests
        , Spec.ReplayUndo.tests
        , Spec.Golden.tests
        , Spec.Properties.tests
        ]
    )
