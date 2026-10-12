{-# LANGUAGE OverloadedStrings #-}
-- | 元素行为性质（ecs-11 起取代 element-oracle.txt）：组件读数、命中配方、叠层盾、调度次序。
module Spec.ElementOracle
  ( tests
  ) where

import Data.List (nub, sort)
import Match3.ECS.Archetype (MatchKind(..), aHit, aMatch, aPhysics, aTally, resolveMatch)
import Match3.ECS.Component
import Match3.ECS.Cover (CoverHitSpec(..), FireSpec(..), coverShield)
import Match3.ECS.Registry (registryKinds)
import Match3.ECS.Stage (SysDef(..))
import Match3.Element
import Match3.Element.Builtin (gemArch, iceCover, rainbowArch, stoneArch)
import Match3.Element.Builtin.Layer (chainCover, grassCover)
import Match3.Element.Mechanic (mechNameOf)
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "or_gem_components" or_gem_components
  , testCase "or_stone_hit_kind_chip" or_stone_hit_kind_chip
  , testCase "or_ice_and_chain_shield_specs" or_ice_and_chain_shield_specs
  , testCase "or_grass_strips" or_grass_strips
  , testCase "or_builtin_archetype_names_unique" or_builtin_archetype_names_unique
  , testCase "or_mechanic_names_unique" or_mechanic_names_unique
  , testCase "or_match_color_hintable" or_match_color_hintable
  ]

or_gem_components :: Assertion
or_gem_components = do
  let m = aMatch gemArch C2
      h = aHit gemArch C2
      p = aPhysics gemArch
  assertEqual "color" (Just C2) (mColor m)
  assertEqual "strike" Destroy (hStrike h)
  assertBool "falls" (pFalls p)
  assertBool "not fixed" (not (pFixed p))

or_stone_hit_kind_chip :: Assertion
or_stone_hit_kind_chip = do
  assertEqual "3→2" (Absorb (Stone 2)) (hStrike (aHit stoneArch 3))
  assertEqual "tally stones" (Just CountStones) (tCounter (aTally stoneArch 2))

or_ice_and_chain_shield_specs :: Assertion
or_ice_and_chain_shield_specs = do
  let ice2 = coverShield iceCover 2
      ice1 = coverShield iceCover 1
      ch2 = coverShield chainCover 2
      ch1 = coverShield chainCover 1
  assertEqual "ice2 keep" (Keep 1) (sHit ice2)
  assertEqual "ice1 shatter" Shatter (sHit ice1)
  assertEqual "ice1 fires" (Just True) (sFires ice1)
  assertEqual "chain2 keep" (Keep 1) (sHit ch2)
  assertEqual "chain1 peel" Peel (sHit ch1)
  assertEqual "chain no fire" (Just False) (sFires ch2)

or_grass_strips :: Assertion
or_grass_strips =
  assertBool "grass strips on clear" (sStrips (coverShield grassCover ()))

or_builtin_archetype_names_unique :: Assertion
or_builtin_archetype_names_unique = do
  let ns = map archName (registryKinds defaultRegistry)
  assertEqual "unique names" (sort ns) (sort (nub ns))
  assertBool "enough builtins" (length ns >= 20)

or_mechanic_names_unique :: Assertion
or_mechanic_names_unique = do
  let ns = map mechNameOf (mechanicDefs defaultRegistry)
  assertEqual "unique" (sort ns) (sort (nub ns))

or_match_color_hintable :: Assertion
or_match_color_hintable = do
  assertEqual "gem" (gemMatch (Just C4)) (resolveMatch (MatchColor True) C4)
  assertBool "rainbow not hintable" (not (mHintable (aMatch rainbowArch C1)))
