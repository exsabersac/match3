{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | slim-2：Phase 骨架；slim-9 起对照内置 PlainGem 与测试里独立写的 PhaseGem（两份 Phase 写法逐项相等）。
module Spec.PhaseSlim (tests) where

import Match3.Element.Builtin.Gem (PlainGem(..))
import Match3.Element.Kind (DieOrder(..), NearOut(..), Reach(..))
import Match3.Element.Phase
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | Phase 侧的普通宝石（与 Builtin.Gem.PlainGem 对照）。
newtype PhaseGem = PhaseGem Color deriving (Eq, Show)

instance Phase PhaseGem where
  codec = Codec
    { cName = "gem"
    , cToCell = \(PhaseGem c) -> Gem c Normal 0 Nothing
    , cFromCell = \cell -> case cell of
        Gem c Normal 0 Nothing -> Just (PhaseGem c)
        _ -> Nothing
    , cPlace = \_ _ -> Nothing
    , cMeta = emptyMeta
    , cNear = Nothing
    , cHud = noHud
    , cPasses = []
    }
  onMatch (PhaseGem c) = gemMatch (Just c)
  onHit _ _ = gemHit
  physics _ = gemPhysics
  view _ = noFace

tests :: [TestTree]
tests =
  [ testCase "phase_gem_matches_ability_gem" phase_gem_matches_ability_gem
  , testCase "phase_defaults_silent_near" phase_defaults_silent_near
  ]

phase_gem_matches_ability_gem :: Assertion
phase_gem_matches_ability_gem = do
  let g = PlainGem C3
      p = PhaseGem C3
      cdc = codec @PhaseGem
  assertEqual "name" (nameOf g) (cName cdc)
  assertEqual "toCell" (toCell g) (cToCell cdc p)
  assertEqual "probe" (drop 1 (phaseProbe g)) (drop 1 (phaseProbe p))
  assertEqual "decode" (Just p) (cFromCell cdc (toCell g))

phase_defaults_silent_near :: Assertion
phase_defaults_silent_near = do
  let p = PhaseGem C1
      rule = NearRule 10 SkipDirect DiePrepend
  case onNear rule (error "no ctx") p of
    NearIdle -> pure ()
    _ -> assertFailure "expected NearIdle"
