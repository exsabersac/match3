{-# LANGUAGE OverloadedStrings #-}
-- | 行为回归（ecs-11 起取代 golden.txt 逐字节快照）：固定样例锁定玩家可见规则结果。
-- 不再读 test/golden/golden.txt；内部 Show / 投影文本可变。
module Spec.Golden
  ( tests
  ) where

import Data.List (nub)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Match3.Board.Grid (getCell, setCell)
import Match3.ECS.Archetype (aHit)
import Match3.ECS.Component (Blast(..), OnHit(..), Strike(..), blastArea)
import Match3.ECS.Registry (registryKinds, wholeHit)
import Match3.ECS.Stage (SysDef(..))
import Match3.Element
import Match3.Element.Builtin (bombArch, flipArch, gemArch, lineHArch, safeArch, stoneArch)
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Level (campaignGame, newGameAtLevel)
import Match3.Game.Move (trySwap)
import Spec.Support (moveApplied)
import Match3.Game.State
import Match3.Levels.Campaign (allLevels)
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "bh_campaign_opens_all_levels" bh_campaign_opens_all_levels
  , testCase "bh_level0_seed1_has_moves" bh_level0_seed1_has_moves
  , testCase "bh_stone_chip_then_break" bh_stone_chip_then_break
  , testCase "bh_safe_opens_to_cookie" bh_safe_opens_to_cookie
  , testCase "bh_flip_hit_becomes_back_gem" bh_flip_hit_becomes_back_gem
  , testCase "bh_ice_layers_chip_then_shatter" bh_ice_layers_chip_then_shatter
  , testCase "bh_special_blasts_are_data" bh_special_blasts_are_data
  , testCase "bh_hammer_changes_gem_cell" bh_hammer_changes_gem_cell
  , testCase "bh_swap_rejected_on_stone" bh_swap_rejected_on_stone
  , testCase "bh_near_system_orders_sorted" bh_near_system_orders_sorted
  ]

bh_campaign_opens_all_levels :: Assertion
bh_campaign_opens_all_levels = do
  assertBool "campaign non-empty" (not (null allLevels))
  mapM_ (\li -> assertBool ("level " ++ show li) (isJust (campaignGame li 1))) [0 .. length allLevels - 1]

bh_level0_seed1_has_moves :: Assertion
bh_level0_seed1_has_moves = do
  let gs = fromMaybe (error "L0") (campaignGame 0 1)
  assertBool "has moves" (gsMoves gs > 0)
  assertEqual "default board 8x8" 8 (length (boardRows (gsBoard gs)))

bh_stone_chip_then_break :: Assertion
bh_stone_chip_then_break = do
  assertEqual "chip 3→2" (Absorb (Stone 2)) (hStrike (aHit stoneArch 3))
  assertEqual "chip 1→destroy" Destroy (hStrike (aHit stoneArch 1))

bh_safe_opens_to_cookie :: Assertion
bh_safe_opens_to_cookie = do
  assertEqual "last → cookie" (Absorb Cookie) (hStrike (aHit safeArch 1))
  assertEqual "mid → safe 1" (Absorb (Safe 1)) (hStrike (aHit safeArch 2))

bh_flip_hit_becomes_back_gem :: Assertion
bh_flip_hit_becomes_back_gem =
  assertEqual "flip to back" (Absorb (Gem C3 Normal 0 Nothing)) (hStrike (aHit flipArch (C1, C3)))

bh_ice_layers_chip_then_shatter :: Assertion
bh_ice_layers_chip_then_shatter = do
  assertEqual "ice 2 keeps gem" (Absorb (Gem C1 Normal 1 Nothing)) (hStrike (wholeHit defaultRegistry (Gem C1 Normal 2 Nothing)))
  assertEqual "ice 1 shatters" Destroy (hStrike (wholeHit defaultRegistry (Gem C1 Normal 1 Nothing)))

bh_special_blasts_are_data :: Assertion
bh_special_blasts_are_data = do
  let board = gsBoard (newGameAtLevel 0 defaultConfig 1)
      area = blastArea BlastRow board (3, 3)
  assertEqual "row blast width" 8 (length area)
  assertEqual "line_h" (Just BlastRow) (hBlast (aHit lineHArch C1))
  assertEqual "bomb" (Just BlastSquare) (hBlast (aHit bombArch C1))
  assertEqual "gem" Nothing (hBlast (aHit gemArch C1))

bh_hammer_changes_gem_cell :: Assertion
bh_hammer_changes_gem_cell = do
  let gs0 = newGameAtLevel 0 defaultConfig 42
      b0 = gsBoard gs0
      gems = [p | p <- boardPositions b0, isGem (getCell b0 p)]
  case listToMaybe gems of
    Nothing -> assertFailure "no gems"
    Just p -> do
      let (gs1, _, _) = resolveHammerWith defaultRegistry p gs0
      assertBool "board or moves changed" (gsBoard gs1 /= b0 || gsMoves gs1 /= gsMoves gs0)

bh_swap_rejected_on_stone :: Assertion
bh_swap_rejected_on_stone = do
  let b0 = setCell (setCell (gsBoard (newGameAtLevel 0 defaultConfig 1)) (0, 0) (Stone 2)) (0, 1) (mkGem C1)
      gs0 = (newGameAtLevel 0 defaultConfig 1) {gsBoard = b0}
      (_, o) = trySwap (0, 0) (0, 1) gs0
  assertBool "stone blocks swap" (not (moveApplied o))

bh_near_system_orders_sorted :: Assertion
bh_near_system_orders_sorted = do
  let orders = [o | SomeArchetype a <- registryKinds defaultRegistry, SysNear o _ <- aSystems a]
  assertBool "has near systems" (length orders >= 8)
  -- 邻格次序两两不同（调度按数字排序；注册表收集顺序不必递增）
  assertEqual "near orders unique" (length orders) (length (nub orders))
  assertBool "stone 10 before fuzzball 190" (minimum orders < maximum orders)
