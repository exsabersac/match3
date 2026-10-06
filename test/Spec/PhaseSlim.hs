{-# LANGUAGE OverloadedStrings #-}
-- | ecs-3：原型骨架。对照内置 gemArch 与测试里独立写的普通宝石原型（两份写法逐组件相等）；
-- 缺省原型（'archetype'）是惰性占格的组件、不带任何 system。
module Spec.PhaseSlim (tests) where

import Match3.ECS.Archetype
import Match3.ECS.Component
import Match3.Element.Builtin (gemArch)
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | 测试侧的普通宝石原型（与 Builtin.Gem.gemArch 对照）。
testGem :: Archetype Color
testGem = (archetype "gem" (Column get (\c -> Gem c Normal 0 Nothing)))
  { aMatch = gemMatch . Just
  , aHit = const gemHit
  , aPhysics = const gemPhysics
  }
  where
    get cell = case cell of
      Gem c Normal _ _ -> Just c
      _ -> Nothing

tests :: [TestTree]
tests =
  [ testCase "archetype_gem_matches_builtin_gem" archetype_gem_matches_builtin_gem
  , testCase "archetype_defaults_inert_no_systems" archetype_defaults_inert_no_systems
  ]

archetype_gem_matches_builtin_gem :: Assertion
archetype_gem_matches_builtin_gem = do
  assertEqual "name" (aName gemArch) (aName testGem)
  sequence_
    [ do
        assertEqual ("put " ++ show c) (colPut (aColumn gemArch) c) (colPut (aColumn testGem) c)
        assertEqual ("get " ++ show c) (colGet (aColumn gemArch) cell) (colGet (aColumn testGem) cell)
        assertEqual ("match " ++ show c) (aMatch gemArch c) (aMatch testGem c)
        assertEqual ("hit " ++ show c) (aHit gemArch c) (aHit testGem c)
        assertEqual ("physics " ++ show c) (aPhysics gemArch c) (aPhysics testGem c)
        assertEqual ("tally " ++ show c) (aTally gemArch c) (aTally testGem c)
        assertEqual ("face " ++ show c) (aFace gemArch c) (aFace testGem c)
    | c <- allColors
    , cell <- [Gem c Normal 0 Nothing, Gem c Normal 2 (Just Grass), Gem c LineH 0 Nothing, Stone 1]
    ]
  assertEqual "hud" (aHud gemArch) (aHud testGem)
  assertEqual "no systems" 0 (length (aSystems gemArch))

archetype_defaults_inert_no_systems :: Assertion
archetype_defaults_inert_no_systems = do
  let a = archetype "x" (customColumn "x")
  assertEqual "match" obstacleMatch (aMatch a 1)
  assertEqual "hit" immuneHit (aHit a 1)
  assertEqual "physics" obstaclePhysics (aPhysics a 1)
  assertEqual "tally" emptyTally (aTally a 1)
  assertEqual "face" noFace (aFace a 1)
  assertEqual "hud" noHud (aHud a)
  assertEqual "diff" Nothing (aDiff a)
  assertEqual "systems" 0 (length (aSystems a))
  assertEqual "spawn" Nothing (aSpawn a [] (Stone 1))
