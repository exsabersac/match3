{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 5（2026-09-30）：雪怪 Boss Custom "snow_boss"（2×2）。
-- 固定格：挡交换、不下落、无色、洗牌保留；身外一圈的真消除与打到它的直接命中扣血（四格同血），
-- 归零四格一起消除；计数 CountNamed "snow_boss" 按前后盘面差（左上格权重 = 血量）= 扣掉的血，
-- 目标 goalCount (CountNamed "snow_boss") 满血 =「击败 Boss」。交换的步末（PhaseMove 30）召唤计数 +1，
-- 每 3 次在身外一圈召唤一块雪块（1 层石头），选格按盘面散列，不消耗 gsGen。第 45 关「雪怪」用到它。
-- 主流程唯一的通用钩子：按差计数的格子权重（ccDiffWeight，缺省 1）。
module Spec.SnowBoss
  ( tests
  ) where

import Data.List (nub, sort)
import Match3.Core
import Match3.Element.Builtin (defaultWorld)
import Match3.Types (boardSize)
import Match3.Board.Match (findHintWith)
import Match3.Counts (countOf, countsFromList)
import Match3.Element
  ( Arg(..)
  , Strike(..)
  , blocksSwapWith
  , builtinDefs
  , builtinMechanics
  , builtinShapeRules
  , colorOfWith
  , directHitWith
  , fallsWith
  , keepOnShuffleWith
  , runAdjacentWith
  , snowBossHp
  , snowBossSpawn
  , snowBosses
  , SnowBoss(..)
  , decodeBoss
  )
import Match3.Combos (builtinComboRules)
import Data.Proxy (Proxy(..))
import Match3.Element.Ability (Countable(diffWeight), toCell)
import Match3.Element.Kind (Kind(diffCounter))
import Match3.Element.Event (EventKind(..))
import Match3.Element.World (World, countElementWith, defName, mkWorld, placeWith, registerMechanic, setComboRules, setShapeRules, weighElementWith)
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.Trace (applyEndEffect)
import Match3.Types (goalCount, goalTarget, terminalOf)
import Match3.View (BossView(..), gameView, gvBoss)
import UI.CellFace (BossPart(..), bossPart)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "sb_caps_fixed_blocker" sb_caps_fixed_blocker
  , testCase "sb_placement_and_weight" sb_placement_and_weight
  , testCase "sb_adjacent_and_direct_damage" sb_adjacent_and_direct_damage
  , testCase "sb_hammer_and_defeat_wins" sb_hammer_and_defeat_wins
  , testCase "sb_step_end_summons_snow" sb_step_end_summons_snow
  , testCase "sb_other_levels_unchanged" sb_other_levels_unchanged
  , testCase "sb_level45_layout_and_play" sb_level45_layout_and_play
  ]

-- | 第 45 关（0 基下标 44）。
bossLevel :: Int
bossLevel = 44

anchor :: Pos
anchor = (2, 3)

body :: [Pos]
body = [(2, 3), (2, 4), (3, 3), (3, 4)]

-- | 身外一圈（8 格）。
ring :: [Pos]
ring = [(1, 3), (1, 4), (2, 2), (2, 5), (3, 2), (3, 5), (4, 3), (4, 4)]

-- | 一格 Boss（满血 40）。
boss :: Int -> Int -> Int -> Cell
boss hp t q = toCell (SnowBoss hp 40 t q)

-- | 在 stableBoard 上放一只 Boss（左上角 (2,3)）。
bossBoard :: Int -> Board
bossBoard hp = setCells stableBoard [(p, boss hp 0 q) | (q, p) <- zip [0 ..] body]

hpOf :: Board -> [Int]
hpOf b = [sbHp s | (_, s) <- snowBosses b]

-- | 能力：固定（挡交换、不下落）、无色、洗牌保留；直接命中原样吃掉（不免疫，锤子可打）；左上格按血量加权计差。
sb_caps_fixed_blocker :: Assertion
sb_caps_fixed_blocker = do
  let reg = defaultWorld
  mapM_
    ( \q -> do
        let c = boss 12 1 q
        assertBool "blocks swap" (blocksSwapWith reg c)
        assertBool "does not fall" (not (fallsWith reg c))
        assertEqual "colorless" Nothing (colorOfWith reg c)
        assertBool "kept on shuffle" (keepOnShuffleWith reg c)
        assertEqual "hit absorbed as itself" (Absorb c) (directHitWith reg c)
    )
    [0 .. 3]
  assertEqual "weights: top-left = hp, others 0" [12, 0, 0, 0] [diffWeight (SnowBoss 12 40 1 q) | q <- [0 .. 3]]
  assertEqual "counter" (Just (CountNamed "snow_boss")) (diffCounter (Proxy :: Proxy SnowBoss))
  assertEqual "encoding round-trips" [SnowBoss hp 40 t q | hp <- [0, 12, 40], t <- [0 .. 2], q <- [0 .. 3]]
    [decodeBoss st | hp <- [0, 12, 40], t <- [0 .. 2], q <- [0 .. 3], Custom _ st <- [boss hp t q]]

-- | 放置参数 [血量, 象限]；第 45 关开局四格在 (2,3)–(3,4)、满血 = 目标值；其余内置元素的差计权重都是 1。
sb_placement_and_weight :: Assertion
sb_placement_and_weight = do
  let reg = defaultWorld
      placed args = fmap (\b -> getCell b (4, 4)) (placeWith reg "snow_boss" args stableBoard [(4, 4)])
  assertEqual "place [40, 2]" (Just (boss 40 0 2)) (either (const Nothing) Just (placed [AInt 40, AInt 2]))
  assertEqual "bad args: cell unchanged" [] [a | a <- [[], [AInt 40], [AInt 0, AInt 0], [AInt 5, AInt 4]], placed a /= Right (getCell stableBoard (4, 4))]
  mapM_
    ( \s -> do
        let gs = levelGame bossLevel s
            b = gsBoard gs
        assertEqual ("seed " ++ show s ++ " boss cells") body (sort (customsOn "snow_boss" b))
        assertEqual ("seed " ++ show s ++ " quadrants") [boss 40 0 q | q <- [0 .. 3]] (map (getCell b) body)
        assertEqual "one boss at (2,3)" [anchor] (map fst (snowBosses b))
        assertEqual "hp = goal target" (goalTarget (gsGoal gs)) (snowBossHp b)
        assertEqual "weighted count = hp" 40 (weighElementWith reg "snow_boss" b)
    )
    [1 .. 3]
  assertEqual "goal = defeat boss" (goalCount (CountNamed "snow_boss") 40) (gsGoal (levelGame bossLevel 1))
  -- 视图模型：第 45 关有血条（满血 40），其余关没有；每格的象限 / 受伤（血量 ≤ 一半）/ 召唤计数
  assertEqual "hud boss bar" (Just (BossView 40 40)) (gvBoss (gameView (levelGame bossLevel 1)))
  assertEqual "no bar elsewhere" [] [li | li <- [0 .. bossLevel - 1], gvBoss (gameView (levelGame li 1)) /= Nothing]
  assertEqual "boss parts" [Just (BossPart q False 0 3) | q <- [0 .. 3]] (map (bossPart . getCell (gsBoard (levelGame bossLevel 1))) body)
  assertEqual "hurt at half" (Just (BossPart 3 True 2 3)) (bossPart (boss 20 2 3))
  assertEqual "not hurt above half" (Just (BossPart 3 False 2 3)) (bossPart (boss 21 2 3))
  assertEqual "gem is not a boss part" Nothing (bossPart (mkGem C1))
  -- 权重缺省 1：没有 Boss 的盘面上，加权个数 = 格数（保险箱 / 时间精灵的差计不变）
  let b = setCells stableBoard [((0, 0), Safe 2), ((0, 1), Safe 1), ((5, 5), TimeSpirit)]
  mapM_ (\n -> assertEqual (show n) (countElementWith reg n b) (weighElementWith reg n b)) ["safe", "time_spirit", "stone", "gem"]

-- | 邻格规则：身外一圈的真消除每格扣 1（斜角 / 远处不算），直接命中的 Boss 格每格扣 1；四格同血；归零四格并入清除格。
sb_adjacent_and_direct_damage :: Assertion
sb_adjacent_and_direct_damage = do
  let b0 = bossBoard 10
      (b1, dead1, _) = runAdjacentWith defaultWorld [(1, 3), (1, 4), (4, 4), (1, 2), (5, 5)] [] [] b0
  assertEqual "3 ring clears -> 7" [7] (hpOf b1)
  assertEqual "all four cells share hp" [boss 7 0 q | q <- [0 .. 3]] (map (getCell b1) body)
  assertEqual "nothing dies" [] (filter (`elem` body) dead1)
  let (b2, _, _) = runAdjacentWith defaultWorld [(2, 2)] [(2, 3), (3, 3), (2, 2)] [] b0
  assertEqual "1 ring clear + 2 direct -> 7" [7] (hpOf b2)
  let (b3, dead3, _) = runAdjacentWith defaultWorld ring [] [] (bossBoard 5)
  assertEqual "defeated: all four cells cleared" (sort body) (sort (filter (`elem` body) dead3))
  assertEqual "cells untouched until cleared (hp not rewritten)" (map (getCell (bossBoard 5)) body) (map (getCell b3) body)
  let (b4, dead4, _) = runAdjacentWith defaultWorld [(0, 0), (7, 7)] [] [] b0
  assertEqual "far clears: no change" b0 b4
  assertEqual "far clears: no dead" [] (filter (`elem` body) dead4)

-- | 锤子打 Boss 格：接受、扣 1 血、计 1；血量 1 时再打一下：四格消失、计数达到目标即过关。
sb_hammer_and_defeat_wins :: Assertion
sb_hammer_and_defeat_wins = do
  let gs0 = levelGame bossLevel 1
  assertBool "has hammers" (gsHammers gs0 > 0)
  let (gs1, o1, _) = resolveHammerWith defaultWorld (3, 4) gs0
  assertBool "hammer accepted" (o1 `notElem` [NoMatch, InvalidSwap])
  assertEqual "hp 39" 39 (snowBossHp (gsBoard gs1))
  assertEqual "counted 1" 1 (countOf (CountNamed "snow_boss") (gsCounts gs1))
  assertEqual "boss stays in place" body (sort (customsOn "snow_boss" (gsBoard gs1)))
  let gsLow = gs0 {gsBoard = setCells (gsBoard gs0) [(p, boss 1 0 q) | (q, p) <- zip [0 ..] body], gsCounts = countsFromList [(CountNamed "snow_boss", 39)]}
      (gs2, o2, mt) = resolveHammerWith defaultWorld (2, 3) gsLow
  assertEqual "boss gone" [] (customsOn "snow_boss" (gsBoard gs2))
  assertEqual "counted to 40" 40 (countOf (CountNamed "snow_boss") (gsCounts gs2))
  assertEqual "hud bar after hammer" (Just (BossView 39 40)) (gvBoss (gameView gs1))
  assertEqual "hud bar empty after defeat" (Just (BossView 0 40)) (gvBoss (gameView gs2))
  assertBool "defeat wins" (isWin (terminalOf o2) || isWin (gsOver gs2))
  assertBool "boss cells cleared in first wave" (all (`elem` concatMap cwCleared (mtWaves mt)) body)

-- | 交换的步末：每步一条 EvTick "snow_boss"（四格计数变化），第 3 步多一格雪块（身外一圈的普通宝石 → 1 层石头）；
-- 重放一致；选格确定；锤子不推进计数。
sb_step_end_summons_snow :: Assertion
sb_step_end_summons_snow = do
  let gs0 = levelGame bossLevel 2
      stepHint gs = case findHintWith defaultWorld (gsBoard gs) of
        Just (p, q) -> resolveSwapWith defaultWorld p q gs
        Nothing -> error "no hint"
      (gs1, _, mt1) = stepHint gs0
      (gs2, _, mt2) = stepHint gs1
      (gs3, _, mt3) = stepHint gs2
      bossSteps mt = [es | es <- mtEnd mt, endEffectElement (esEffect es) == "snow_boss"]
      turns gs = [sbTurn s | (_, s) <- snowBosses (gsBoard gs)]
  mapM_
    ( \(i, mt) -> case bossSteps mt of
        [es] -> do
          assertEqual ("step " ++ show i ++ " tick") EvTick (endEffectKind (esEffect es))
          assertEqual ("step " ++ show i ++ " replay") (esAfter es) (applyEndEffect (esEffect es) (esBefore es))
          _ <- replayTimeline ("step " ++ show i) mt
          pure ()
        other -> assertFailure ("step " ++ show i ++ ": expected one snow_boss end step, got " ++ show (length other))
    )
    (zip [1 :: Int ..] [mt1, mt2, mt3])
  assertEqual "turn counter" [[1], [2], [0]] [turns gs1, turns gs2, turns gs3]
  let snow = [eiTo it | es <- bossSteps mt3, it <- endEffectItems (esEffect es), eiCell it == Stone 1]
  assertEqual "third step summons one snow block" 1 (length snow)
  assertBool "snow on the ring" (all (`elem` ring) snow)
  mapM_ (\es -> assertEqual "summon choice is deterministic" (snowBossSpawn [] [] (esBefore es) anchor) (snowBossSpawn [] [] (esBefore es) anchor)) (bossSteps mt3)
  assertEqual "no snow on steps 1-2" [] [it | mt <- [mt1, mt2], es <- bossSteps mt, it <- endEffectItems (esEffect es), eiCell it == Stone 1]
  let (gsH, _, mtH) = resolveHammerWith defaultWorld (7, 0) gs1
  assertEqual "hammer: no boss end step" [] (bossSteps mtH)
  assertEqual "hammer: counter unchanged" (turns gs1) (turns gsH)
  -- 无候选（身外一圈都不是普通宝石）：不召唤
  let full = setCells (bossBoard 9) [(q, Stone 2) | q <- ring]
  assertEqual "no candidate" Nothing (snowBossSpawn [] [] full anchor)
  assertEqual "avoid / walls respected" Nothing (snowBossSpawn (take 4 ring) (drop 4 ring) (bossBoard 9) anchor)

-- | 去掉雪怪条目的注册表：前 44 关按提示各走 6 步，盘面、分数、计数与随机种子逐一相同；原有关卡开局没有 Boss。
sb_other_levels_unchanged :: Assertion
sb_other_levels_unchanged = do
  let noBoss :: World
      noBoss = setShapeRules builtinShapeRules . setComboRules builtinComboRules $
        foldl (flip registerMechanic) (mkWorld (filter ((/= "snow_boss") . defName) builtinDefs)) builtinMechanics
      play reg gs n
        | n <= (0 :: Int) || gsOver gs /= Nothing = gs
        | otherwise = case findHintWith reg (gsBoard gs) of
            Nothing -> gs
            Just (p, q) -> let (gs', _, _) = resolveSwapWith reg p q gs in play reg gs' (n - 1)
      key gs = (gsBoard gs, gsScore gs, gsCounts gs, gsMoves gs, show (gsGen gs))
  assertEqual "older levels have none" [] [li | li <- [0 .. bossLevel - 1], not (null (customsOn "snow_boss" (gsBoard (levelGame li 1))))]
  mapM_
    (\li -> assertEqual ("level " ++ show (li + 1)) (key (play noBoss (levelGame li 3) 6)) (key (play defaultWorld (levelGame li 3) 6)))
    [0 .. bossLevel - 1]

-- | 第 45 关：种子 1–6 按提示走满 24 步：Boss 始终在原位（活着时）、每局都扣过血且计数 = 扣掉的血、召唤过雪块、
-- 时间线逐步可重放；按「优先打 Boss」走（每步选扣血最多的交换）种子 1–3 都能击败 Boss。
sb_level45_layout_and_play :: Assertion
sb_level45_layout_and_play = do
  let play gs n acc
        | n <= (0 :: Int) || gsOver gs /= Nothing = pure (gs, acc)
        | otherwise = case findHintWith defaultWorld (gsBoard gs) of
            Nothing -> pure (gs, acc)
            Just (p, q) -> do
              let (gs', _, mt) = resolveSwapWith defaultWorld p q gs
              _ <- replayTimeline "level 45" mt
              let alive = customsOn "snow_boss" (gsBoard gs')
              assertBool "boss stays put while alive" (null alive || sort alive == body)
              let snow = length [() | es <- mtEnd mt, endEffectElement (esEffect es) == "snow_boss", EndItem _ _ (Stone 1) _ <- endEffectItems (esEffect es)]
              play gs' (n - 1) (acc + snow)
  runs <- mapM (\s -> play (levelGame bossLevel s) 24 0) [1 .. 6]
  mapM_
    ( \(gs, _) -> do
        let dealt = 40 - snowBossHp (gsBoard gs)
        assertBool "damage dealt" (dealt > 0)
        assertEqual "counter = damage dealt" dealt (countOf (CountNamed "snow_boss") (gsCounts gs))
    )
    runs
  assertBool "summoned snow in play" (sum (map snd runs) > 0)
  let greedy gs
        | gsOver gs /= Nothing = gs
        | otherwise =
            case [ (snowBossHp (gsBoard g'), negate (gsScore g'), i, g')
                 | (i, (p, q)) <- zip [0 :: Int ..] [((r, c), d) | r <- [0 .. 7], c <- [0 .. 7], d <- [(r, c + 1), (r + 1, c)], fst d >= 0 && fst d < boardSize && snd d >= 0 && snd d < boardSize]
                 , let (g', o, _) = resolveSwapWith defaultWorld p q gs
                 , o `notElem` [NoMatch, InvalidSwap] ] of
              [] -> gs
              cs -> let (_, _, _, g') = minimum4 cs in greedy g'
      minimum4 = foldr1 (\a b -> if key4 a <= key4 b then a else b)
      key4 (h, s, i, _) = (h, s, i)
  mapM_ (\s -> assertBool ("boss-first play wins seed " ++ show s) (isWin (gsOver (greedy (levelGame bossLevel s))))) [1 .. 3]
  assertEqual "no duplicate boss cells" 4 (length (nub (customsOn "snow_boss" (gsBoard (levelGame bossLevel 1)))))
