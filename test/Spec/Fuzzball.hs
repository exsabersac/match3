{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 3（2026-09-30）：毛球 Custom "fuzzball"。
-- 占格障碍：挡交换、无色、随重力下落；正交邻格有真消除即被消灭（命中也消灭），计 CountNamed "fuzzball"。
-- 玩家交换的步末（PhaseMove 20）每个毛球跳到一个正交相邻的普通宝石格（与之换位），选格按盘面散列，
-- 不消耗 gsGen——没有毛球的关卡随机序列、盘面与快照都不变。第 43 关「毛球」用到它。
module Spec.Fuzzball
  ( tests
  ) where

import Match3.Core
import Match3.Board.Match (findHintWith)
import Match3.Element
  ( HitResult(..), blocksSwapWith, builtinDefs, builtinLevelDefs, builtinShapeRules, colorOfWith, defaultRegistry
  , directHitWith, fallsWith, fuzzballJumps, keepOnShuffleWith, runAdjacentWith )
import Match3.Combos (builtinComboRules)
import Match3.Element.Event (EventKind(..))
import Match3.Element.Registry (Registry, entryName, mkRegistry, registerLevel, setComboRules, setShapeRules)
import Match3.Game.Move (resolveSwapWith)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "fz_caps_blocker_falls_breaks" fz_caps_blocker_falls_breaks
  , testCase "fz_adjacent_clear_kills" fz_adjacent_clear_kills
  , testCase "fz_jumps_to_plain_gem_neighbour" fz_jumps_to_plain_gem_neighbour
  , testCase "fz_jumps_respect_walls_avoid_and_blockers" fz_jumps_respect_walls_avoid_and_blockers
  , testCase "fz_step_end_belt_effect_replays" fz_step_end_belt_effect_replays
  , testCase "fz_other_levels_unchanged" fz_other_levels_unchanged
  , testCase "fz_level43_layout_and_play" fz_level43_layout_and_play
  ]

-- | 第 43 关（0 基下标 42）。
fuzzLevel :: Int
fuzzLevel = 42

fuzz :: Cell
fuzz = Custom "fuzzball" (CustomState 1)

fuzzAt :: Board -> [Pos]
fuzzAt = customsOn "fuzzball"

-- | 能力：挡交换、随重力下落、无色、洗牌保留；命中即消灭。
fz_caps_blocker_falls_breaks :: Assertion
fz_caps_blocker_falls_breaks = do
  let reg = defaultRegistry
  assertBool "blocks swap" (blocksSwapWith reg fuzz)
  assertBool "falls" (fallsWith reg fuzz)
  assertEqual "colorless" Nothing (colorOfWith reg fuzz)
  assertBool "kept on shuffle" (keepOnShuffleWith reg fuzz)
  assertEqual "hit destroys" HitDestroy (directHitWith reg fuzz)

-- | 邻格规则：与真消除格正交相邻的毛球进入死亡格；斜角不算；本轮已被直接命中的不重复算。
fz_adjacent_clear_kills :: Assertion
fz_adjacent_clear_kills = do
  let b0 = setCells stableBoard [((3, 3), fuzz), ((0, 0), fuzz), ((5, 5), fuzz)]
      (_, dead, _) = runAdjacentWith defaultRegistry [(3, 4), (2, 3), (1, 1), (5, 6)] [(5, 5)] [] b0
  assertEqual "orthogonal neighbour dies once" 1 (length (filter (== (3, 3)) dead))
  assertBool "diagonal survives" ((0, 0) `notElem` dead)
  assertBool "direct hit not repeated" ((5, 5) `notElem` dead)

-- | 跳格：四周都是普通宝石时跳到其中一格（与之换位）；同一盘面结果确定。
fz_jumps_to_plain_gem_neighbour :: Assertion
fz_jumps_to_plain_gem_neighbour = do
  let b0 = setCells stableBoard [((3, 3), fuzz)]
      (ms, b1) = fuzzballJumps [] [] b0
  case ms of
    [((3, 3), q)] -> do
      assertBool "orthogonal neighbour" (q `elem` [(2, 3), (4, 3), (3, 2), (3, 4)])
      assertEqual "fuzzball moved" [q] (fuzzAt b1)
      assertEqual "gem swapped back" (getCell b0 q) (getCell b1 (3, 3))
    _ -> assertFailure ("expected one jump from (3,3), got " ++ show ms)
  assertEqual "deterministic" (ms, b1) (fuzzballJumps [] [] b0)

-- | 墙（传送门端点）、避让格（皮带本步移过）、非普通宝石（障碍 / 特殊块 / 另一个毛球）都不跳；
-- 两个毛球争同一格时只有一个跳过去；无候选不动。
fz_jumps_respect_walls_avoid_and_blockers :: Assertion
fz_jumps_respect_walls_avoid_and_blockers = do
  let b0 = setCells stableBoard [((3, 3), fuzz)]
      (ms, _) = fuzzballJumps [(3, 2)] [(2, 3), (4, 3)] b0
  assertEqual "only (3,4) left" [((3, 3), (3, 4))] ms
  let bStuck = setCells stableBoard [((0, 0), fuzz), ((0, 1), Stone 2), ((1, 0), Gem C1 LineH 0 Nothing)]
      (msStuck, bStuck') = fuzzballJumps [] [] bStuck
  assertEqual "no candidate: no jump" [] msStuck
  assertEqual "board unchanged" bStuck bStuck'
  let bPair = setCells stableBoard [((0, 0), fuzz), ((0, 1), fuzz), ((1, 1), Stone 2), ((0, 2), Stone 2)]
      (msPair, _) = fuzzballJumps [] [] bPair
  assertEqual "(0,0) takes (1,0), (0,1) stuck" [((0, 0), (1, 0))] msPair
  let bShare = setCells stableBoard [((3, 3), fuzz), ((3, 5), fuzz)]
      walls = [(2, 3), (4, 3), (3, 2), (2, 5), (4, 5), (3, 6)]
      (msShare, bShare') = fuzzballJumps [] walls bShare
  assertEqual "shared cell: one jump" 1 (length msShare)
  assertEqual "two fuzzballs remain" 2 (length (fuzzAt bShare'))

-- | tripleBoard 的交换（消第 1 行）后，远处 (6,6) 的毛球在步末跳一格：一条 EvBelt "fuzzball"、两项
-- （毛球 原格 → 新格、宝石 新格 → 原格），applyEndEffect 由前盘重放得到后盘。
fz_step_end_belt_effect_replays :: Assertion
fz_step_end_belt_effect_replays = do
  let gs0 = (levelGame fuzzLevel 7) {gsBoard = setCells tripleBoard [((6, 6), fuzz)]}
      (a, b) = tripleMove
      (gs1, o, mt) = resolveSwapWith defaultRegistry a b gs0
  assertBool "move accepted" (o `notElem` [NoMatch, InvalidSwap])
  let steps = [es | es <- mtEnd mt, endEffectElement (esEffect es) == "fuzzball"]
  case steps of
    [es] -> do
      assertEqual "belt kind" EvBelt (endEffectKind (esEffect es))
      assertEqual "two items" 2 (length (endEffectItems (esEffect es)))
      assertEqual "replay" (esAfter es) (applyEndEffect (esEffect es) (esBefore es))
      case fuzzAt (gsBoard gs1) of
        [q] -> assertBool "jumped to a neighbour" (q `elem` [(5, 6), (7, 6), (6, 5), (6, 7)])
        other -> assertFailure ("fuzzballs after: " ++ show other)
    _ -> assertFailure ("expected one fuzzball step, got " ++ show (length steps))

-- | 去掉毛球条目的注册表：前 42 关按提示各走 6 步，盘面、分数与随机种子逐一相同（毛球不耗 gsGen）；原有关卡开局没有毛球。
fz_other_levels_unchanged :: Assertion
fz_other_levels_unchanged = do
  let noFz :: Registry
      noFz = setShapeRules builtinShapeRules . setComboRules builtinComboRules $
        foldl (flip registerLevel) (mkRegistry (filter ((/= "fuzzball") . entryName) builtinDefs)) builtinLevelDefs
      play reg gs n
        | n <= (0 :: Int) || gsOver gs /= Nothing = gs
        | otherwise = case findHintWith reg (gsBoard gs) of
            Nothing -> gs
            Just (p, q) -> let (gs', _, _) = resolveSwapWith reg p q gs in play reg gs' (n - 1)
      key gs = (gsBoard gs, gsScore gs, show (gsGen gs))
  assertEqual "older levels have none" [] [li | li <- [0 .. fuzzLevel - 1], not (null (fuzzAt (gsBoard (levelGame li 1))))]
  mapM_
    (\li -> assertEqual ("level " ++ show (li + 1)) (key (play noFz (levelGame li 3) 6)) (key (play defaultRegistry (levelGame li 3) 6)))
    [0 .. fuzzLevel - 1]

-- | 第 43 关：10 个毛球在上两行附近开局；按提示走 22 步（种子 1–6）：实战里跳过格、被消灭计数，至少一局过关。
fz_level43_layout_and_play :: Assertion
fz_level43_layout_and_play = do
  mapM_ (\s -> assertEqual ("level 43 seed " ++ show s) 10 (length (fuzzAt (gsBoard (levelGame fuzzLevel s))))) [1 .. 3]
  let play gs n acc
        | n <= (0 :: Int) || gsOver gs /= Nothing = (gs, acc)
        | otherwise = case findHintWith defaultRegistry (gsBoard gs) of
            Nothing -> (gs, acc)
            Just (p, q) ->
              let (gs', _, mt) = resolveSwapWith defaultRegistry p q gs
                  jumps = sum [length (endEffectItems (esEffect es)) `div` 2 | es <- mtEnd mt, endEffectElement (esEffect es) == "fuzzball"]
              in play gs' (n - 1) (acc + jumps)
      runs = [play (levelGame fuzzLevel s) 22 0 | s <- [1 .. 6]]
  assertBool "jumped in play" (sum (map snd runs) > 0)
  assertBool "fuzzball goal progressed" (all (\(gs, _) -> countOf (CountNamed "fuzzball") (gsCounts gs) > 0) runs)
  assertBool "some run wins" (any (\(gs, _) -> isWin (gsOver gs)) runs)
