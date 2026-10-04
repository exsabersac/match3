{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 7（2026-09-30）：变色龙 Custom "chameleon" k（k = 当前颜色下标，colorAt k）。
-- 普通棋子原型：可交换、按当前颜色参与匹配 / 提示、命中即消、随重力下落；洗牌保留、不可被改色。
-- 玩家交换的步末（PhaseMove 40）每只变色龙按 C1 → … → C5 → C1 换到下一种不会立刻连成三消的颜色（纯按盘面，
-- 不消耗 gsGen），记 EvTick "chameleon"；道具不触发。与彩虹交换（成对规则 15）清掉它当前颜色的宝石与同色变色龙。
-- 被消除计 CountNamed "chameleon"。第 47 关「变色龙」用到它（开局 2 只 + 复用新玩法 6 的掉落口补变色龙）。
module Spec.Chameleon
  ( tests
  ) where

import Data.Foldable (toList)
import Match3.Core
import Match3.Board.Grid (setM, toM)
import Match3.Board.Match (findHintWith, hasAnyMatchWith)
import Match3.Board.Refill (defaultRefill, refillWith)
import Match3.Combos (builtinComboRules)
import Match3.Counts (countOf)
import Match3.Daily (dailyConfig)
import Match3.Element
  ( Arg(..)
  , Strike(..)
  , blocksSwapWith
  , builtinDefs
  , builtinLevelDefs
  , builtinShapeRules
  , chameleonCell
  , chameleonShift
  , colorOfWith
  , counterWith
  , directHitWith
  , dropRefill
  , fallsWith
  , hintableWith
  , keepOnShuffleWith
  , placeWith
  , recolorableWith
  , swapFiresWith
  , swapOpeningWith
  )
import Match3.Element.Event (EventKind(..))
import Match3.Element.World (World, defName, mkWorld, registerLevel, setComboRules, setShapeRules)
import Match3.Game.Boosters (useHammer)
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.Trace (applyEndEffect)
import Match3.Levels.Level (DropSpec(..))
import Match3.View (BoardView(..), boardView)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "ch_caps_piece_by_current_color" ch_caps_piece_by_current_color
  , testCase "ch_matches_by_current_color" ch_matches_by_current_color
  , testCase "ch_shift_fixed_order_skips_instant_runs" ch_shift_fixed_order_skips_instant_runs
  , testCase "ch_step_end_shift_swap_only" ch_step_end_shift_swap_only
  , testCase "ch_rainbow_swap_clears_current_color" ch_rainbow_swap_clears_current_color
  , testCase "ch_drop_port_counts_any_color" ch_drop_port_counts_any_color
  , testCase "ch_other_levels_unchanged" ch_other_levels_unchanged
  , testCase "ch_level47_layout_and_difficulty" ch_level47_layout_and_difficulty
  ]

-- | 第 47 关（0 基下标 46）。
chamLevel :: Int
chamLevel = 46

cham :: Color -> Cell
cham = chameleonCell

chamsOn :: Board -> [Pos]
chamsOn = customsOn "chameleon"

stoneBoard :: Board
stoneBoard = boardFromRows (replicate boardSize (replicate boardSize (Stone 1)))

-- | 能力：可交换、会下落、按当前颜色（CustomState = 颜色下标）上色、进提示、命中即消、洗牌保留、不可改色、
-- 消除计 CountNamed "chameleon"；放置取原格宝石颜色（或 AColor 指定），原格不是宝石时不放。
ch_caps_piece_by_current_color :: Assertion
ch_caps_piece_by_current_color = do
  let reg = defaultWorld
  mapM_ (\c -> assertEqual ("color " ++ show c) (Just c) (colorOfWith reg (cham c))) allColors
  assertEqual "state = colour index" (Custom "chameleon" (CustomState 2)) (cham C3)
  assertBool "swappable" (not (blocksSwapWith reg (cham C1)))
  assertBool "falls" (fallsWith reg (cham C1))
  assertBool "hintable" (hintableWith reg (cham C1))
  assertEqual "hit destroys" Destroy (directHitWith reg (cham C1))
  assertBool "kept on shuffle" (keepOnShuffleWith reg (cham C1))
  assertBool "not recolorable" (not (recolorableWith reg (cham C1)))
  assertEqual "counter" (Just (CountNamed "chameleon")) (counterWith reg (cham C4))
  assertEqual "place takes gem colour" (Right (getCell stableBoard (2, 2))) (fmap (\b -> maybe (getCell b (2, 2)) mkGem (chameleonColor (getCell b (2, 2)))) (placeWith reg "chameleon" [] stableBoard [(2, 2)]))
  assertEqual "place with colour" (Right (cham C5)) (fmap (`getCell` (0, 0)) (placeWith reg "chameleon" [AColor C5] stableBoard [(0, 0)]))
  assertEqual "not on stone" (Right (Stone 1)) (fmap (`getCell` (0, 0)) (placeWith reg "chameleon" [] stoneBoard [(0, 0)]))

-- | 匹配按当前颜色：tripleBoard 的 (2,2)（C5）换成 C5 变色龙，交换后连成 C5 三消、变色龙被消除计数；
-- 换成 C4 变色龙则这一对不成消。测试辅助 findMatchPair 也按当前颜色找到引擎接受的对。
ch_matches_by_current_color :: Assertion
ch_matches_by_current_color = do
  let gs0 = levelGame chamLevel 7
      (a, b) = tripleMove
      withCham c = gs0 {gsBoard = setCells tripleBoard [((2, 2), cham c)]}
      (gs1, o1, _) = resolveSwapWith defaultWorld a b (withCham C5)
      (_, o2, _) = resolveSwapWith defaultWorld a b (withCham C4)
  assertBool "same colour chameleon matches" (o1 `notElem` [NoMatch, InvalidSwap])
  assertBool "counted" (countOf (CountNamed "chameleon") (gsCounts gs1) >= 1)
  assertEqual "other colour does not" NoMatch o2
  assertBool "standing run with a chameleon" (hasAnyMatchWith defaultWorld (setCells stableBoard [((0, 0), mkGem C2), ((0, 1), mkGem C2), ((0, 2), cham C2)]))
  assertBool "different colour breaks the run" (not (hasAnyMatchWith defaultWorld (setCells stableBoard [((0, 0), mkGem C2), ((0, 1), mkGem C2), ((0, 2), cham C3)])))
  -- 只有经过变色龙的一对能成消：stuckNoMoveBoard（没有可走步）的 (1,2) 换成 C3 变色龙后，唯一可走步是
  -- (1,0)↔(2,0)（C3 上移，与 (1,1) 的 C3 和变色龙连成三消）；同一格换成无色的石头则仍然无步可走。
  let bOnly = setCells stuckNoMoveBoard [((1, 2), cham C3)]
  assertEqual "stone there: no move" Nothing (findMatchPair (setCells stuckNoMoveBoard [((1, 2), Stone 1)]))
  assertEqual "findMatchPair uses the chameleon's colour" (Just ((1, 0), (2, 0))) (findMatchPair bOnly)
  assertEqual "hint agrees" (Just ((1, 0), (2, 0))) (findHintWith defaultWorld bOnly)
  let (gs2, o, _) = resolveSwapWith defaultWorld (1, 0) (2, 0) (gs0 {gsBoard = bOnly})
  assertBool "engine accepts it" (o `notElem` [NoMatch, InvalidSwap])
  assertBool "chameleon cleared" (countOf (CountNamed "chameleon") (gsCounts gs2) >= 1)

-- | 换色（纯函数 chameleonShift）：四周不成连线时按固定顺序循环 C2 → C3 → C4 → C5 → C1 → C2；
-- 下一种颜色会让它立刻连成三消时顺延（左边两颗 C2 → 跳过 C2 取 C3；再加上边两颗 C3 → 取 C4）；
-- 四种都会连成时回到原色。多只按行优先、在逐只换过的盘面上判断。
ch_shift_fixed_order_skips_instant_runs :: Assertion
ch_shift_fixed_order_skips_instant_runs = do
  let b0 = setCells stoneBoard [((3, 3), cham C2)]
      cycleColors = map (\b -> chameleonColor (getCell b (3, 3))) (take 6 (iterate (snd . chameleonShift) b0))
  assertEqual "fixed order" (map Just [C2, C3, C4, C5, C1, C2]) cycleColors
  assertEqual "moved cells" [(3, 3)] (fst (chameleonShift b0))
  let bL = setCells stoneBoard [((3, 3), cham C1), ((3, 1), mkGem C2), ((3, 2), mkGem C2)]
  assertEqual "skip C2" (Just C3) (chameleonColor (getCell (snd (chameleonShift bL)) (3, 3)))
  let bLU = setCells bL [((1, 3), mkGem C3), ((2, 3), mkGem C3)]
  assertEqual "skip C2, C3" (Just C4) (chameleonColor (getCell (snd (chameleonShift bLU)) (3, 3)))
  let bAll = setCells bLU [((3, 4), mkGem C4), ((3, 5), mkGem C4), ((4, 3), mkGem C5), ((5, 3), mkGem C5)]
  assertEqual "all blocked: back to own colour" (Just C1) (chameleonColor (getCell (snd (chameleonShift bAll)) (3, 3)))
  -- 两只相邻：(3,3) 先换成 C3，(3,4) 判断时看到的是换过的 (3,3)
  let bPair = setCells stoneBoard [((3, 3), cham C2), ((3, 4), cham C2), ((3, 5), mkGem C3)]
      bAfter = snd (chameleonShift bPair)
  assertEqual "first becomes C3" (Just C3) (chameleonColor (getCell bAfter (3, 3)))
  assertEqual "second skips C3 (would join (3,3) and (3,5))" (Just C4) (chameleonColor (getCell bAfter (3, 4)))
  assertEqual "no chameleons: identity" ([], stableBoard) (chameleonShift stableBoard)

-- | 玩家交换的步末：远处 (6,6) 的 C3 变色龙换成 C4、(7,7) 的 C5 绕回 C1，记一条 EvTick "chameleon"，applyEndEffect 由前盘重放得到后盘；
-- 道具（锤子）不触发换色。
ch_step_end_shift_swap_only :: Assertion
ch_step_end_shift_swap_only = do
  -- 两只（第 47 关掉落口保持 2 只，这样本步不会从掉落口补进新的）
  let gs0 = (levelGame chamLevel 7) {gsBoard = setCells tripleBoard [((6, 6), cham C3), ((7, 7), cham C5)]}
      (a, b) = tripleMove
      (gs1, o, mt) = resolveSwapWith defaultWorld a b gs0
  assertBool "move accepted" (o `notElem` [NoMatch, InvalidSwap])
  case [es | es <- mtEnd mt, endEffectElement (esEffect es) == "chameleon"] of
    [es] -> do
      assertEqual "tick kind" EvTick (endEffectKind (esEffect es))
      assertEqual "items at (6,6), (7,7)" [(6, 6), (7, 7)] (map eiTo (endEffectItems (esEffect es)))
      assertEqual "replay" (esAfter es) (applyEndEffect (esEffect es) (esBefore es))
    other -> assertFailure ("expected one chameleon step, got " ++ show (length other))
  assertEqual "now C4" (Just C4) (chameleonColor (getCell (gsBoard gs1) (6, 6)))
  assertEqual "C5 wraps to C1" (Just C1) (chameleonColor (getCell (gsBoard gs1) (7, 7)))
  let (gsH, _) = useHammer (4, 4) gs0
  assertEqual "hammer: colour kept" (Just C3) (chameleonColor (getCell (gsBoard gsH) (6, 6)))

-- | 彩虹 × 变色龙（成对规则 15）：清掉彩虹、变色龙当前颜色（C3）的全部宝石与同色变色龙；别的颜色的变色龙不动。
ch_rainbow_swap_clears_current_color :: Assertion
ch_rainbow_swap_clears_current_color = do
  let b0 = setCells stableBoard [((3, 3), Gem C1 Rainbow 0 Nothing), ((3, 4), cham C3), ((7, 7), cham C3), ((0, 1), cham C2)]
      swapped = setCells b0 [((3, 3), cham C3), ((3, 4), Gem C1 Rainbow 0 Nothing)]
      c3gems = [p | p <- allPos, getCell b0 p == mkGem C3]
  assertBool "fires" (swapFiresWith defaultWorld b0 (3, 3) (3, 4))
  case swapOpeningWith defaultWorld b0 swapped (3, 3) (3, 4) of
    Just seeds -> do
      assertBool "all C3 gems" (all (`elem` seeds) c3gems)
      assertBool "both C3 chameleons" (all (`elem` seeds) [(3, 3), (7, 7)])
      assertBool "rainbow" ((3, 4) `elem` seeds)
      assertBool "C2 chameleon untouched" ((0, 1) `notElem` seeds)
    Nothing -> assertFailure "rainbow × chameleon did not open"
  let gs0 = (levelGame chamLevel 7) {gsBoard = b0}
      (gs1, o, _) = resolveSwapWith defaultWorld (3, 3) (3, 4) gs0
  assertBool "accepted" (o `notElem` [NoMatch, InvalidSwap])
  assertBool "two chameleons counted" (countOf (CountNamed "chameleon") (gsCounts gs1) >= 2)
  assertBool "not fired without a rainbow" (not (swapFiresWith defaultWorld (setCells stableBoard [((3, 4), cham C3)]) (3, 3) (3, 4)))

-- | 掉落口（复用新玩法 6）按 Custom 名字数同种：盘上已有两只（不同颜色）变色龙时不再掉；只有一只时掉一只 C1。
ch_drop_port_counts_any_color :: Assertion
ch_drop_port_counts_any_color = do
  let pol = dropRefill [DropSpec [(0, 3)] (cham C1) 2] defaultRefill
      mb0 = setM (toM (setCells stableBoard [((5, 5), cham C3)])) (0, 3) Nothing
      fill mb = fst (refillWith pol (mkStdGen 3) mb)
      base mb = fst (refillWith defaultRefill (mkStdGen 3) mb)
  assertEqual "one on board: drop" (cham C1) (getCell (fill mb0) (0, 3))
  let mb1 = setM mb0 (6, 1) (Just (cham C5))
  assertEqual "two of other colours: no drop" (base mb1) (fill mb1)
  assertEqual "sanity: two chameleons" 2 (length [() | Just (Custom "chameleon" _) <- toList mb1])

-- | 去掉变色龙条目的注册表：前 46 关与 3 天的每日挑战按提示各走 6 步，盘面、得分、计数、gsGen 逐项相同
-- （变色龙规则在没有变色龙的盘面上不做任何事，也不耗随机数）；原有关卡开局没有变色龙。
ch_other_levels_unchanged :: Assertion
ch_other_levels_unchanged = do
  let noCh :: World
      noCh = setShapeRules builtinShapeRules . setComboRules builtinComboRules $
        foldl (flip registerLevel) (mkWorld (filter ((/= "chameleon") . defName) builtinDefs)) builtinLevelDefs
      play reg gs n
        | n <= (0 :: Int) || gsOver gs /= Nothing = gs
        | otherwise = case findHintWith reg (gsBoard gs) of
            Nothing -> gs
            Just (p, q) -> let (gs', _, _) = resolveSwapWith reg p q gs in play reg gs' (n - 1)
      key gs = (gsBoard gs, gsScore gs, gsCounts gs, gsMoves gs, show (gsGen gs))
      daily (y, m, d) = newDailyGame (dailyConfig (Year y) (Month m) (Day d)) (dailySeed (Year y) (Month m) (Day d))
  assertEqual "older levels have none" [] [li | li <- [0 .. chamLevel - 1], not (null (chamsOn (gsBoard (levelGame li 1))))]
  mapM_
    (\li -> assertEqual ("level " ++ show (li + 1)) (key (play noCh (levelGame li 3) 6)) (key (play defaultWorld (levelGame li 3) 6)))
    [0 .. chamLevel - 1]
  mapM_
    (\d -> assertEqual ("daily " ++ show d) (key (play noCh (daily d) 6)) (key (play defaultWorld (daily d) 6)))
    [(2026, 9, 28), (2026, 9, 29), (2026, 9, 30)]

-- | 第 47 关：开局 2 只变色龙在 (3,1) / (5,6)、颜色取原格宝石（开局无现成三消）；目标消 30 只、18 步；
-- 掉落口 (0,3)（视图 bvDrops）；实战里换色、掉落都发生；按提示 30 局赢 ≤ 25（backlog 的「太容易」标准），
-- 一步贪心（先多消变色龙、再得分）种子 2 能过关。
ch_level47_layout_and_difficulty :: Assertion
ch_level47_layout_and_difficulty = do
  let lvl = levelAt chamLevel
  assertEqual "name" "变色龙" (lvlName lvl)
  assertEqual "goal" (Just 30) (case goalView (lvlGoal lvl) of ViewCount (CountNamed "chameleon") n -> Just n; _ -> Nothing)
  assertEqual "moves" 18 (lvlMoves lvl)
  mapM_
    ( \s -> do
        let gs = levelGame chamLevel s
        assertEqual ("seed " ++ show s ++ " chameleons") [(3, 1), (5, 6)] (chamsOn (gsBoard gs))
        assertBool ("seed " ++ show s ++ " no standing run") (not (hasAnyMatchWith defaultWorld (gsBoard gs)))
        assertEqual ("seed " ++ show s ++ " drop port") [(0, 3)] (bvDrops (boardView gs))
    )
    [1 .. 3]
  let hintRun s = go (levelGame chamLevel s) (18 :: Int) (0 :: Int)
        where
          go gs n ticks
            | n <= 0 || gsOver gs /= Nothing = (gs, ticks)
            | otherwise = case findHintWith defaultWorld (gsBoard gs) of
                Nothing -> (gs, ticks)
                Just (p, q) ->
                  let (gs', _, mt) = resolveSwapWith defaultWorld p q gs
                  in go gs' (n - 1) (ticks + length [() | es <- mtEnd mt, endEffectElement (esEffect es) == "chameleon"])
      runs = map hintRun [1 .. 30]
      hintWins = length (filter (isWin . gsOver . fst) runs)
  assertBool "shifted in play" (sum (map snd runs) > 0)
  assertBool "chameleons cleared and dropped in play" (any (\(gs, _) -> countOf (CountNamed "chameleon") (gsCounts gs) > 2) runs)
  assertBool ("hint wins " ++ show hintWins ++ "/30 <= 25") (hintWins <= 25)
  assertBool "greedy clears seed 2" (isWin (gsOver (greedy (levelGame chamLevel 2))))
  where
    greedy gs
      | gsOver gs /= Nothing = gs
      | otherwise =
          case [ ((negate (countOf (CountNamed "chameleon") (gsCounts g')), negate (gsScore g'), i), g')
               | (i, (p, q)) <- zip [0 :: Int ..] [((r, c), d) | r <- [0 .. 7], c <- [0 .. 7], d <- [(r, c + 1), (r + 1, c)], fst d >= 0 && fst d < boardSize && snd d >= 0 && snd d < boardSize]
               , let (g', o, _) = resolveSwapWith defaultWorld p q gs
               , o `notElem` [NoMatch, InvalidSwap] ] of
            [] -> gs
            cs -> greedy (snd (foldr1 (\x y -> if fst x <= fst y then x else y) cs))
