{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 2（2026-09-30）：魔法石 Custom "magic_stone"。
-- 固定格、打不动、无色；邻格真消除每轮充能 1 格（满 3），玩家交换的步末（PhaseTick 20）满格的魔法石发射：
-- 以所在整行 + 整列为种子引爆，发射后归零。道具没有 PhaseTick 步末，满格的等到下一次交换。第 42 关「魔石」用到它。
module Spec.MagicStone
  ( tests
  ) where

import Match3.Core
import Match3.Board.Match (findHintWith)
import Match3.Element
  ( HitResult(..), blocksSwapWith, colorOfWith, defaultRegistry, directHitWith, fallsWith, keepOnShuffleWith
  , magicStoneFiring, magicStoneFull, runAdjacentWith )
import Match3.Element.Event (EventKind(..))
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Move (resolveSwapWith)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "ms_caps_fixed_immune_colorless" ms_caps_fixed_immune_colorless
  , testCase "ms_charges_once_per_round" ms_charges_once_per_round
  , testCase "ms_fires_row_and_col_at_step_end" ms_fires_row_and_col_at_step_end
  , testCase "ms_not_full_does_not_fire" ms_not_full_does_not_fire
  , testCase "ms_boosters_wait_for_next_swap" ms_boosters_wait_for_next_swap
  , testCase "ms_level42_layout_and_play" ms_level42_layout_and_play
  ]

-- | 第 42 关（0 基下标 41）。
stoneLevel :: Int
stoneLevel = 41

magic :: Int -> Cell
magic k = Custom "magic_stone" (CustomState k)

stateAt :: Board -> Pos -> Maybe Int
stateAt b p = case getCell b p of
  Custom "magic_stone" (CustomState k) -> Just k
  _ -> Nothing

-- | 能力：固定（不下落）、挡交换、无色、洗牌保留；平时打不动，发射中被命中归零。
ms_caps_fixed_immune_colorless :: Assertion
ms_caps_fixed_immune_colorless = do
  let reg = defaultRegistry
  assertEqual "full = 3, firing = 4" (3, 4) (magicStoneFull, magicStoneFiring)
  mapM_
    (\k -> do
       assertBool ("blocks swap " ++ show k) (blocksSwapWith reg (magic k))
       assertBool ("does not fall " ++ show k) (not (fallsWith reg (magic k)))
       assertEqual ("colorless " ++ show k) Nothing (colorOfWith reg (magic k))
       assertBool ("kept on shuffle " ++ show k) (keepOnShuffleWith reg (magic k))
       assertEqual ("immune " ++ show k) HitImmune (directHitWith reg (magic k)))
    [0 .. 3]
  assertEqual "firing: hit resets to 0" (HitAbsorb (magic 0)) (directHitWith reg (magic 4))

-- | 邻格规则：与真消除格正交相邻的魔法石 +1；同一轮两个邻格也只 +1；满 3 不再涨；斜角 / 远处不动。
ms_charges_once_per_round :: Assertion
ms_charges_once_per_round = do
  let b0 = setCells stableBoard [((3, 3), magic 0), ((6, 6), magic 3), ((0, 0), magic 1)]
      adj tc b = let (b', _, _) = runAdjacentWith defaultRegistry tc [] [] b in b'
      b1 = adj [(3, 4), (2, 3), (5, 5), (6, 5), (1, 1)] b0
  assertEqual "two neighbours: +1" (Just 1) (stateAt b1 (3, 3))
  assertEqual "full stays 3" (Just 3) (stateAt b1 (6, 6))
  assertEqual "diagonal only: unchanged" (Just 1) (stateAt b1 (0, 0))
  let b3 = adj [(3, 2)] (adj [(4, 3)] b1)
  assertEqual "three rounds: full" (Just 3) (stateAt b3 (3, 3))
  assertEqual "fourth round: still 3" (Just 3) (stateAt (adj [(3, 4)] b3) (3, 3))

-- | tripleBoard 的交换在第 1 行消出四连；(2,1) 的魔法石差 1 格满——这一步充满，步末发射：
-- 记一条 EvTick "magic_stone"，之后某一轮清掉第 2 行和第 1 列（除它自己），结算后归零。
ms_fires_row_and_col_at_step_end :: Assertion
ms_fires_row_and_col_at_step_end = do
  let gs0 = (levelGame stoneLevel 7) {gsBoard = setCells tripleBoard [((2, 1), magic 2)]}
      (a, b) = tripleMove
      (gs1, o, mt) = resolveSwapWith defaultRegistry a b gs0
  assertBool "move accepted" (o `notElem` [NoMatch, InvalidSwap])
  let fired = [es | es <- mtEnd mt, endEffectKind (esEffect es) == EvTick, endEffectElement (esEffect es) == "magic_stone"]
  assertEqual "one firing step" 1 (length fired)
  let expected = [(2, c) | c <- [0 .. 7], c /= 1] ++ [(r, 1) | r <- [0 .. 7], r /= 2]
  assertBool "a round clears row 2 + column 1" (any (\w -> all (`elem` cwCleared w) expected) (mtWaves mt))
  assertEqual "reset after firing" (Just 0) (stateAt (gsBoard gs1) (2, 1))

-- | 同一步，魔法石只有 0 格：充到 1 格，不发射。
ms_not_full_does_not_fire :: Assertion
ms_not_full_does_not_fire = do
  let gs0 = (levelGame stoneLevel 7) {gsBoard = setCells tripleBoard [((2, 1), magic 0)]}
      (a, b) = tripleMove
      (gs1, _, mt) = resolveSwapWith defaultRegistry a b gs0
  assertEqual "no firing" [] [es | es <- mtEnd mt, endEffectElement (esEffect es) == "magic_stone"]
  assertEqual "charged to 1" (Just 1) (stateAt (gsBoard gs1) (2, 1))

-- | 满格的魔法石遇到道具（锤子，远处一格）不发射；下一次交换的步末才发射。
ms_boosters_wait_for_next_swap :: Assertion
ms_boosters_wait_for_next_swap = do
  let gs0 = (levelGame stoneLevel 7) {gsBoard = setCells tripleBoard [((2, 1), magic 3)]}
      (gsH, oH, mtH) = resolveHammerWith defaultRegistry (7, 7) gs0
  assertBool "hammer accepted" (oH `notElem` [NoMatch, InvalidSwap])
  assertEqual "hammer: no firing" [] [es | es <- mtEnd mtH, endEffectElement (esEffect es) == "magic_stone"]
  assertEqual "hammer: still full" (Just 3) (stateAt (gsBoard gsH) (2, 1))
  case findHintWith defaultRegistry (gsBoard gsH) of
    Nothing -> assertFailure "fixture: no hint after hammer"
    Just (p, q) -> do
      let (gs2, _, mt2) = resolveSwapWith defaultRegistry p q gsH
      assertEqual "next swap fires" 1 (length [es | es <- mtEnd mt2, endEffectElement (esEffect es) == "magic_stone"])
      assertEqual "reset" (Just 0) (stateAt (gsBoard gs2) (2, 1))

-- | 第 42 关：四块魔法石在 (2,2)(2,5)(5,2)(5,5)、0 格；原有 41 关开局没有魔法石。
-- 按提示走 20 步（种子 1–6）：魔法石始终原地（固定、打不动），实战里发射过，石头目标有进度。
ms_level42_layout_and_play :: Assertion
ms_level42_layout_and_play = do
  let spots = [(2, 2), (2, 5), (5, 2), (5, 5)]
  mapM_ (\s -> assertEqual ("level 42 seed " ++ show s) [(p, Just 0) | p <- spots] [(p, stateAt (gsBoard (levelGame stoneLevel s)) p) | p <- customsOn "magic_stone" (gsBoard (levelGame stoneLevel s))]) [1 .. 3]
  assertEqual "older levels have none" [] [li | li <- [0 .. stoneLevel - 1], not (null (customsOn "magic_stone" (gsBoard (levelGame li 1))))]
  let play gs n acc
        | n <= (0 :: Int) || gsOver gs /= Nothing = (gs, acc)
        | otherwise = case findHintWith defaultRegistry (gsBoard gs) of
            Nothing -> (gs, acc)
            Just (p, q) ->
              let (gs', _, mt) = resolveSwapWith defaultRegistry p q gs
                  fires = length [es | es <- mtEnd mt, endEffectElement (esEffect es) == "magic_stone"]
                  stay = customsOn "magic_stone" (gsBoard gs') == spots
              in if stay then play gs' (n - 1) (acc + fires) else (gs', -1000)
      runs = [play (levelGame stoneLevel s) 20 0 | s <- [1 .. 6]]
  assertBool "magic stones stay put" (all ((>= 0) . snd) runs)
  assertBool "fired in play" (sum (map snd runs) > 0)
  assertBool "stone goal progressed" (any (\(gs, _) -> countOf CountStones (gsCounts gs) > 0) runs)
