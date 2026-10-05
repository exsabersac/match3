{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 1（2026-09-30）：L / T 形 → 炸弹，关卡规则开关 "bomb_shapes"。
-- 规则开关是关卡级元素 BombShapes：只有 lvlRules 含 "bomb_shapes" 的关卡（第 41 关「爆破」）在每步结算开始时
-- 回复 Shaping，把 ltBombRule 插进本关的形状表；原有 40 关、每日挑战的形状表和内置表完全相同。
module Spec.BombShapes
  ( tests
  ) where

import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Match3.Board.Default (findMatchRuns)
import Match3.Core
import Match3.Board.Clear (clearMatchesDetailedWith)
import Match3.Board.Match (MatchRun(..), findHintWith)
import Match3.Element
  ( World
  , ShapeRule(..)
  , builtinShapeRules
  , levelWorldIn
  , ltBombRule
  , removeMechanic
  , setShapeRules
  , shapeRules
  , withBombShapes
  )
import Match3.Game.Level (newGame)
import Match3.Game.Move (resolveSwapWith)
import Match3.Types (goalScore)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "bs_switch_only_on_new_level" bs_switch_only_on_new_level
  , testCase "bs_rule_order" bs_rule_order
  , testCase "bs_l_shape_bomb_on_level41" bs_l_shape_bomb_on_level41
  , testCase "bs_five_still_rainbow" bs_five_still_rainbow
  , testCase "bs_four_in_l_gives_bomb_not_line" bs_four_in_l_gives_bomb_not_line
  , testCase "bs_level41_play_spawns_bombs" bs_level41_play_spawns_bombs
  ]

-- | 第 41 关（0 基下标 40）。
bombLevel :: Int
bombLevel = 40

shapeNames :: World -> [String]
shapeNames = map shapeName . shapeRules

-- | 本局每步结算用的形状表（= Game.Resolve 开头换上的那张）。
gameShapes :: GameState -> [String]
gameShapes gs = shapeNames (levelWorldIn defaultWorld (gsLevelElems gs))

-- | 只有第 41 关与第 48 关（新玩法 8 魔法格复用这个开关）打开开关：原有 40 关（种子 1 / 2）、每日挑战、自由开局的形状表都等于内置表；
-- 关卡记录里 bomb_shapes 只写在第 41 / 48 关；GameState 的 Show 不打印这个内置元素。
bs_switch_only_on_new_level :: Assertion
bs_switch_only_on_new_level = do
  let builtin = map shapeName builtinShapeRules
  assertEqual "builtin table unchanged" builtin (shapeNames defaultWorld)
  assertEqual "only levels 41 / 48 have bomb_shapes" [(bombLevel, ["bomb_shapes"]), (47, ["bomb_shapes"])] [(li, lvlRules l) | (li, l) <- zip [0 ..] allLevels, "bomb_shapes" `elem` lvlRules l]
  mapM_
    (\(li, seed) -> assertEqual ("level " ++ show (li + 1) ++ " seed " ++ show seed) builtin (gameShapes (levelGame li seed)))
    [(li, seed) | li <- [0 .. bombLevel - 1], seed <- [1, 2]]
  mapM_ (\s -> assertEqual ("daily " ++ show s) builtin (gameShapes (newDailyGame (GameConfig 20 (goalScore 900)) s))) [0 .. 9]
  assertEqual "free game" builtin (gameShapes (newGame (GameConfig 20 (goalScore 900)) 3))
  assertEqual "level 41 table" (map shapeName (withBombShapes builtinShapeRules)) (gameShapes (levelGame bombLevel 1))
  assertBool "Show hides the switch" (not (any (`isInfixOf` show (levelGame bombLevel 1)) ["BombShapes", "bomb_shapes"]))

-- | 插入位置：五连 → 彩虹之后、直线规则之前；表里没有五连规则时放最前面。
bs_rule_order :: Assertion
bs_rule_order = do
  let names = map shapeName (withBombShapes builtinShapeRules)
  assertEqual "after line5, before line4" ("line5→rainbow" : "l/t→bomb" : drop 1 (map shapeName builtinShapeRules)) names
  assertEqual "no line5 rule: goes first" ["l/t→bomb"] (map shapeName (withBombShapes []))
  assertEqual "rule name" "l/t→bomb" (shapeName ltBombRule)

-- | 两条三连交叉成 L 的局面（交换后）：第 41 关第一轮在交点留下炸弹；同一局面在第 1 关、
-- 以及去掉规则开关的注册表下，交点是空洞（内置表不认 L 形）。
bs_l_shape_bomb_on_level41 :: Assertion
bs_l_shape_bomb_on_level41 = do
  let lBoard = setCells stableBoard [((3, 1), mkGem C1), ((3, 2), mkGem C1), ((4, 3), mkGem C1), ((5, 3), mkGem C1), ((2, 3), mkGem C1), ((3, 3), mkGem C3)]
      (p1, p2) = ((2, 3), (3, 3))
      corner world li = do
        let gs0 = (levelGame li 7) {gsBoard = lBoard}
            (_, o, mt) = resolveSwapWith world p1 p2 gs0
        assertBool "move accepted" (o `notElem` [NoMatch, InvalidSwap])
        w <- firstWave mt
        pure (atM' (cwHoles w) (3, 3))
  c41 <- corner defaultWorld bombLevel
  assertEqual "level 41: bomb at the corner" (Just (Gem C1 Bomb 0 Nothing)) c41
  c1 <- corner defaultWorld 0
  assertEqual "level 1: corner is a hole" Nothing c1
  cOff <- corner (removeMechanic "bomb_shapes" defaultWorld) bombLevel
  assertEqual "switch removed: corner is a hole" Nothing cOff
  where
    atM' mb (r, c) = mboardRows mb !! r !! c

-- | 特殊块种类统计（本轮消除、放下新特殊块之后）。
spawnedKinds :: World -> Board -> [GemKind]
spawnedKinds world b =
  let (mb, _, _) = clearMatchesDetailedWith world Nothing b
  in [k | Gem _ k _ _ <- catMaybes (concat (mboardRows mb)), k /= Normal]

bombReg :: World
bombReg = setShapeRules (withBombShapes builtinShapeRules) defaultWorld

-- | 横五连 + 从端点往下的竖三连（交点在五连端点）：五连优先出彩虹，竖线被 L / T 规则认领但不生成，没有炸弹（和内置表一样只出一个彩虹）。
bs_five_still_rainbow :: Assertion
bs_five_still_rainbow = do
  let b = setCells stableBoard ([((3, c), mkGem C1) | c <- [0 .. 4]] ++ [((4, 0), mkGem C1)])
  assertEqual "fixture: one 5-run + one 3-run" [(True, 5), (False, 3)] [(runIsH r, length (runPos r)) | r <- findMatchRuns b]
  assertEqual "bomb table: rainbow only" [Rainbow] (spawnedKinds bombReg b)
  assertEqual "builtin table: same (3-run spawns nothing)" [Rainbow] (spawnedKinds defaultWorld b)

-- | 横四连 + 竖三连交成 L：开关打开时交点出炸弹、不出直线；内置表出一个直线。
bs_four_in_l_gives_bomb_not_line :: Assertion
bs_four_in_l_gives_bomb_not_line = do
  let b = setCells stableBoard ([((3, c), mkGem C1) | c <- [0 .. 3]] ++ [((4, 0), mkGem C1)])
  assertEqual "fixture: one 4-run + one 3-run" [(True, 4), (False, 3)] [(runIsH r, length (runPos r)) | r <- findMatchRuns b]
  let (mb, _, _) = clearMatchesDetailedWith bombReg Nothing b
  assertEqual "bomb at the corner" (Just (Gem C1 Bomb 0 Nothing)) (mboardRows mb !! 3 !! 0)
  assertEqual "bomb table: bomb only" [Bomb] (spawnedKinds bombReg b)
  assertEqual "builtin table: a line" [LineH] (spawnedKinds defaultWorld b)

-- | 第 41 关按提示走 12 步（种子 1–6）：开关打开时对局里真的生成过炸弹；去掉开关后同样的对局一颗炸弹也没有
-- （本关没有其他炸弹来源）。
bs_level41_play_spawns_bombs :: Assertion
bs_level41_play_spawns_bombs = do
  let bombsIn world = sum [bombsInGame world (levelGame bombLevel s) (12 :: Int) | s <- [1 .. 6]]
      bombsInGame world gs n
        | n <= 0 || gsOver gs /= Nothing = 0
        | otherwise = case findHintWith world (gsBoard gs) of
            Nothing -> 0
            Just (a, b) ->
              let (gs', _, mt) = resolveSwapWith world a b gs
                  here = length [() | w <- mtWaves mt, Just (Gem _ Bomb _ _) <- concat (mboardRows (cwHoles w))]
              in here + bombsInGame world gs' (n - 1)
  assertBool "level 41 play spawns bombs" (bombsIn defaultWorld > 0)
  assertEqual "without the switch: no bombs" 0 (bombsIn (removeMechanic "bomb_shapes" defaultWorld))
