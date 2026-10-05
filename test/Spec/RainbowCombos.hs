{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 4（2026-09-30）：魔力鸟组合增强（规则开关 "rainbow_combos"，关卡级元素 RainbowCombos）。
-- 彩虹 × 直线：盘上与直线同色的普通宝石全部变成直线（横竖按格子奇偶交替）再一起引爆；
-- 彩虹 × 炸弹：同色普通宝石全部变成炸弹再一起引爆。变身记成第 0 轮之前的一条步末效果（EvSpread，
-- 元素名 rainbow_line / rainbow_bomb），前端按「长出新格」播放。只有第 44 关「魔力鸟」打开开关，原有关卡不变。
module Spec.RainbowCombos
  ( tests
  ) where

import Data.List (isInfixOf)
import Match3.Board.Grid (swapCells)
import Match3.Core
import Match3.Combos (rainbowComboMorph)
import Match3.Counts (countOf)
import Match3.Element (removeMechanic)
import Match3.Element.Event (Event(..), EventKind(..))
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.Trace (applyEndEffect, traceEvents)
import Match3.Types (cellKind)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "rc_switch_only_on_new_level" rc_switch_only_on_new_level
  , testCase "rc_morph_pairs_and_soft_lock" rc_morph_pairs_and_soft_lock
  , testCase "rc_rainbow_line_morphs_then_fires" rc_rainbow_line_morphs_then_fires
  , testCase "rc_rainbow_bomb_morphs_then_fires" rc_rainbow_bomb_morphs_then_fires
  , testCase "rc_morph_skips_iced_and_overlaid" rc_morph_skips_iced_and_overlaid
  , testCase "rc_old_levels_unchanged" rc_old_levels_unchanged
  , testCase "rc_level44_layout_and_play" rc_level44_layout_and_play
  ]

-- | 第 44 关（0 基下标 43）。
rcLevel :: Int
rcLevel = 43

rainbow :: Color -> Cell
rainbow c = Gem c Rainbow 0 Nothing

-- | stableBoard 第 3 行：(3,3) 放彩虹、(3,4) 是 C3 → 换成 C3 的特效；交换 (3,3)-(3,4) 后彩虹在 (3,4)。
comboBoard :: GemKind -> Board
comboBoard k = setCells stableBoard [((3, 3), rainbow C1), ((3, 4), Gem C3 k 0 Nothing)]

comboMove :: (Pos, Pos)
comboMove = ((3, 3), (3, 4))

-- | 交换后盘面上 C3 的普通宝石（变身目标）。
plainC3 :: Board -> [Pos]
plainC3 b = [p | p <- allPos, getCell b p == Gem C3 Normal 0 Nothing]

-- | 这一步的变身步末效果（第 0 轮之前）。
morphSteps :: MoveTrace -> [EndStep]
morphSteps mt = [e | e <- mtEnd mt, endEffectElement (esEffect e) `elem` ["rainbow_line", "rainbow_bomb"]]

-- | 开关：只有第 44 关写了 rainbow_combos；GameState 的 Show 不打印它；关着的关卡不回复变身。
rc_switch_only_on_new_level :: Assertion
rc_switch_only_on_new_level = do
  assertEqual "only level 44" [rcLevel] [li | (li, l) <- zip [0 ..] allLevels, "rainbow_combos" `elem` lvlRules l]
  assertBool "Show hides the switch" (not (any (`isInfixOf` show (levelGame rcLevel 1)) ["RainbowCombos", "rainbow_combos"]))
  let (a, b) = comboMove
      (_, _, mt1) = resolveSwapWith defaultWorld a b ((levelGame 0 7) {gsBoard = comboBoard LineH})
  assertEqual "level 1: no morph" [] (morphSteps mt1)

-- | 成立条件：一端彩虹、另一端直线 / 炸弹，两端都能点火；彩虹 × 普通宝石 / 彩虹 × 彩虹 / 直线 × 炸弹 / 软锁彩虹都不成立。
rc_morph_pairs_and_soft_lock :: Assertion
rc_morph_pairs_and_soft_lock = do
  let (a, b) = comboMove
      name k0 = fmap (\(n, _, _) -> n) (rainbowComboMorph b0 (swapCells b0 a b) a b) where b0 = k0
  assertEqual "rainbow × line_h" (Just "rainbow_line") (name (comboBoard LineH))
  assertEqual "rainbow × line_v" (Just "rainbow_line") (name (comboBoard LineV))
  assertEqual "rainbow × bomb" (Just "rainbow_bomb") (name (comboBoard Bomb))
  assertEqual "line × rainbow (other end)" (Just "rainbow_line") (name (setCells stableBoard [((3, 3), Gem C2 LineH 0 Nothing), ((3, 4), rainbow C3)]))
  assertEqual "rainbow × gem" Nothing (name (comboBoard Normal))
  assertEqual "rainbow × rainbow" Nothing (name (comboBoard Rainbow))
  assertEqual "line × bomb" Nothing (name (setCells stableBoard [((3, 3), Gem C1 LineH 0 Nothing), ((3, 4), Gem C3 Bomb 0 Nothing)]))
  assertEqual "soft-locked rainbow (ice 2)" Nothing (name (setCells (comboBoard LineH) [((3, 3), Gem C1 Rainbow 2 Nothing)]))

-- | 彩虹 × 直线（第 44 关）：步末效果第一条在第 0 轮之前、从交换后的盘面到变身后的盘面；目标 = 全部 C3 普通宝石，
-- 来源 = 彩虹所在格 (3,4)，横竖按 (行 + 列) 奇偶；第一轮清掉每个横向直线的整行、每个竖向直线的整列。
rc_rainbow_line_morphs_then_fires :: Assertion
rc_rainbow_line_morphs_then_fires = do
  let b0 = comboBoard LineH
      (a, b) = comboMove
      swapped = swapCells b0 a b
      (_, o, mt) = resolveSwapWith defaultWorld a b ((levelGame rcLevel 7) {gsBoard = b0})
  assertBool "move accepted" (o `notElem` [NoMatch, InvalidSwap])
  assertEqual "mtStart = swapped board" swapped (mtStart mt)
  case morphSteps mt of
    [e] -> do
      assertEqual "before the first wave" 0 (esAfterWaves e)
      assertEqual "kind" EvSpread (endEffectKind (esEffect e))
      assertEqual "from the swapped board" swapped (esBefore e)
      assertEqual "replay" (esAfter e) (applyEndEffect (esEffect e) (esBefore e))
      let targets = plainC3 swapped
          kindAt (r, c) = if even (r + c) then LineH else LineV
      assertBool "has targets" (length targets >= 8)
      assertEqual "targets and sources" [((3, 4), q) | q <- targets] (endEffectPairs (esEffect e))
      assertEqual "alternating lines" [Gem C3 (kindAt q) 0 Nothing | q <- targets] [getCell (esAfter e) q | q <- targets]
      w0 <- firstWave mt
      assertEqual "first wave starts from the morphed board" (esAfter e) (cwBefore w0)
      let covered = concat [if kindAt (r, c) == LineH then [(r, c') | c' <- [0 .. 7]] else [(r', c) | r' <- [0 .. 7]] | (r, c) <- targets]
      assertEqual "every line fired in the first wave" [] [p | p <- covered, p `notElem` cwCleared w0]
      assertBool "event at wave 0" (any (\ev -> evKind ev == EvSpread && evWave ev == 0 && evElement ev == "rainbow_line") (traceEvents mt))
    other -> assertFailure ("expected one morph step, got " ++ show (length other))

-- | 彩虹 × 炸弹（第 44 关）：C3 普通宝石全部变炸弹，第一轮清掉每颗炸弹的 3×3。
rc_rainbow_bomb_morphs_then_fires :: Assertion
rc_rainbow_bomb_morphs_then_fires = do
  let b0 = comboBoard Bomb
      (a, b) = comboMove
      swapped = swapCells b0 a b
      (_, _, mt) = resolveSwapWith defaultWorld a b ((levelGame rcLevel 7) {gsBoard = b0})
      targets = plainC3 swapped
  case morphSteps mt of
    [e] -> do
      assertEqual "name" "rainbow_bomb" (endEffectElement (esEffect e))
      assertEqual "all bombs" [Gem C3 Bomb 0 Nothing | _ <- targets] [getCell (esAfter e) q | q <- targets]
      w0 <- firstWave mt
      let around (r, c) = [(r', c') | r' <- [r - 1 .. r + 1], c' <- [c - 1 .. c + 1], r' >= 0, r' < 8, c' >= 0, c' < 8]
      assertEqual "every bomb fired in the first wave" [] [p | q <- targets, p <- around q, p `notElem` cwCleared w0]
    other -> assertFailure ("expected one morph step, got " ++ show (length other))

-- | 带冰 / 带叠层的同色宝石不变身（仍在起手种子里：冰被削一层、草被清掉）；普通的照变。
rc_morph_skips_iced_and_overlaid :: Assertion
rc_morph_skips_iced_and_overlaid = do
  let iced = (0, 4)
      grassy = (1, 3)
      b0 = setCells (comboBoard LineH) [(iced, Gem C3 Normal 1 Nothing), (grassy, Gem C3 Normal 0 (Just Grass))]
      (a, b) = comboMove
  (cells, seeds) <- maybe (assertFailure "combo should fire" >> pure ([], [])) (\(_, cs, ss) -> pure (cs, ss)) (rainbowComboMorph b0 (swapCells b0 a b) a b)
  assertBool "iced not morphed" (iced `notElem` [q | (_, q, _) <- cells])
  assertBool "overlaid not morphed" (grassy `notElem` [q | (_, q, _) <- cells])
  assertBool "both still seeds" (all (`elem` seeds) [iced, grassy])
  assertEqual "plain ones morphed" (plainC3 (swapCells b0 a b)) [q | (_, q, _) <- cells]

-- | 原有 43 关：同一个彩虹 × 直线 / 炸弹交换，在默认元素世界和去掉 rainbow_combos 的元素世界下结果逐项相同（没有变身）；
-- 第 44 关两者不同。
rc_old_levels_unchanged :: Assertion
rc_old_levels_unchanged = do
  let off = removeMechanic "rainbow_combos" defaultWorld
      (a, b) = comboMove
      key world li k = let (gs, o, mt) = resolveSwapWith world a b ((levelGame li 3) {gsBoard = comboBoard k}) in (gsBoard gs, gsScore gs, show (gsGen gs), show o, length (mtEnd mt), map cwCleared (mtWaves mt))
  mapM_
    (\(li, k) -> assertEqual ("level " ++ show (li + 1) ++ " " ++ show k) (key off li k) (key defaultWorld li k))
    [(li, k) | li <- [0 .. rcLevel - 1], k <- [LineH, Bomb]]
  assertBool "level 44 differs" (key off rcLevel LineH /= key defaultWorld rcLevel LineH)

-- | 第 44 关：种子 1–3 开局两组「彩虹 + 直线 / 炸弹」与 12 块双层石头；打出彩虹 × 直线：有变身、石头被削掉至少 8 层。
rc_level44_layout_and_play :: Assertion
rc_level44_layout_and_play = do
  let layers gs = sum [k | p <- allPos, Stone k <- [getCell (gsBoard gs) p]]
  mapM_
    ( \s -> do
        let gs0 = levelGame rcLevel s
            kinds = [cellKind (getCell (gsBoard gs0) p) | p <- [(3, 2), (3, 3), (4, 4), (4, 5)]]
        assertEqual ("seed " ++ show s ++ " specials") (map Just [Rainbow, LineH, Bomb, Rainbow]) kinds
        assertEqual ("seed " ++ show s ++ " stones") 24 (layers gs0)
        let (gs1, o, mt) = resolveSwapWith defaultWorld (3, 2) (3, 3) gs0
        assertBool "accepted" (o `notElem` [NoMatch, InvalidSwap])
        assertEqual "one morph" ["rainbow_line"] (map (endEffectElement . esEffect) (morphSteps mt))
        assertBool ("stones chipped: " ++ show (layers gs1)) (layers gs1 + countOf CountStones (gsCounts gs1) * 1 <= 16)
    )
    [1 .. 3]
