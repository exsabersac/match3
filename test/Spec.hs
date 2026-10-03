-- | 测试入口：只汇总各功能模块（test/Spec/*.hs）的测试。
-- 各模块导出平铺的 [TestTree]，这里拼进同一个顶层组 "match3"，
-- 因此 --list-tests 的完整路径（match3.<测试名>）与拆分前逐字相同。
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Spec.GridMatch
import qualified Spec.Gravity
import qualified Spec.Cascade
import qualified Spec.Specials
import qualified Spec.Builtin.Obstacle
import qualified Spec.Builtin.Collectible
import qualified Spec.Builtin.Actor
import qualified Spec.Builtin.Layer
import qualified Spec.Builtin.Level
import qualified Spec.Boosters
import qualified Spec.GoalsLevels
import qualified Spec.Levels
import qualified Spec.Element
import qualified Spec.Extension
import qualified Spec.Branches
import qualified Spec.JellyBubble
import qualified Spec.ElementClass
import qualified Spec.Engine
import qualified Spec.UIEvents
import qualified Spec.ReplayUndo
import qualified Spec.Golden
import qualified Spec.Properties
import qualified Spec.SourceScan
import qualified Spec.Caps
import qualified Spec.Presentation
import qualified Spec.View
import qualified Spec.BombShapes
import qualified Spec.MagicStone
import qualified Spec.Fuzzball
import qualified Spec.SnowBoss
import qualified Spec.CookieDrop
import qualified Spec.Chameleon
import qualified Spec.MagicGround
import qualified Spec.RainbowCombos
import qualified Spec.Phase
import qualified Spec.Classes
import qualified Spec.Effects
import qualified Spec.Lazy
import qualified Spec.Perf
import qualified Spec.Optics
import qualified Spec.Invariants
import qualified Spec.RulesDedup
import qualified Spec.GridGeometry
import qualified Spec.DataBoundary
import qualified Spec.BoardSeed

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
        , Spec.Builtin.Obstacle.tests
        , Spec.Builtin.Collectible.tests
        , Spec.Builtin.Actor.tests
        , Spec.Builtin.Layer.tests
        , Spec.Builtin.Level.tests
        , Spec.Boosters.tests
        , Spec.GoalsLevels.tests
        , Spec.Levels.tests
        , Spec.Element.tests
        , Spec.Extension.tests
        , Spec.Branches.tests
        , Spec.JellyBubble.tests
        , Spec.ElementClass.tests
        , Spec.Engine.tests
        , Spec.UIEvents.tests
        , Spec.ReplayUndo.tests
        , Spec.Golden.tests
        , Spec.Properties.tests
        , Spec.Caps.tests
        , Spec.Presentation.tests
        , Spec.View.tests
        , Spec.BombShapes.tests
        , Spec.MagicStone.tests
        , Spec.Fuzzball.tests
        , Spec.SnowBoss.tests
        , Spec.CookieDrop.tests
        , Spec.Chameleon.tests
        , Spec.MagicGround.tests
        , Spec.RainbowCombos.tests
        , Spec.Phase.tests
        , Spec.Classes.tests
        , Spec.Effects.tests
        , Spec.Lazy.tests
        , Spec.Perf.tests
        , Spec.Optics.tests
        , Spec.Invariants.tests
        , Spec.RulesDedup.tests
        , Spec.GridGeometry.tests
        , Spec.DataBoundary.tests
        , Spec.BoardSeed.tests
        , Spec.SourceScan.tests
        ]
    )
