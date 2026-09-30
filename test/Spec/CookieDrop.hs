{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 6（2026-09-30）：饼干掉落口（关卡级元素 CookieDrop，关卡记录 lvlDrops）。
-- 补子时掉落口格（顶行）的空洞在盘上饼干少于 dropKeep 块时补饼干而不是宝石；饼干沿用原有的
-- 底边收集（drains EdgeBottom，计 CountCookies）。每个空洞仍照原策略消耗一次随机数，所以生成器的推进与
-- 没有掉落口时相同；掉落与否完全由盘面决定。有掉落口的关卡不做目标补齐（收集物由掉落口陆续补进场）。
-- 第 46 关「掉落口」用到它；其余关卡 lvlDrops 为空、CookieDrop 不回复，行为不变。
module Spec.CookieDrop
  ( tests
  ) where

import Data.Foldable (toList)
import Data.List (isInfixOf)
import Match3.Core
import Match3.Board.Grid (MBoard, atM, setM, toM)
import Match3.Board.Match (findHintWith)
import Match3.Board.Refill (RefillPolicy(..), defaultRefill, refillWith)
import Match3.Element (defaultRegistry, dropRefill, removeLevel)
import Match3.Element.Level (levelDrops)
import Match3.Element.Registry (Registry)
import Match3.Game.Level (newGameAtLevelWith)
import Match3.Game.Move (resolveSwapWith)
import Match3.Levels.Level (DropSpec(..))
import Match3.View (BoardView(..), boardView)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "cd_only_level46_has_drops" cd_only_level46_has_drops
  , testCase "cd_refill_drops_cookie_at_drop_cells" cd_refill_drops_cookie_at_drop_cells
  , testCase "cd_refill_same_rng_as_base" cd_refill_same_rng_as_base
  , testCase "cd_level46_start_no_goal_decor" cd_level46_start_no_goal_decor
  , testCase "cd_drops_and_collects_in_play" cd_drops_and_collects_in_play
  , testCase "cd_other_levels_unchanged" cd_other_levels_unchanged
  , testCase "cd_level46_difficulty" cd_level46_difficulty
  ]

-- | 第 46 关（0 基下标 45）。
dropLevel :: Int
dropLevel = 45

dropCellsL46 :: [Pos]
dropCellsL46 = [(0, 1), (0, 3), (0, 4), (0, 6)]

-- | 某天的每日挑战开局（与前端相同：日期种子 + 当天配置）。
dailyGame :: (Int, Int, Int) -> GameState
dailyGame (y, m, d) = newDailyGame (dailyConfig y m d) (dailySeed y m d)

cookiesOn :: Board -> [Pos]
cookiesOn b = [p | p <- allPos, getCell b p == Cookie]

cookiesM :: MBoard -> Int
cookiesM mb = length (filter (== Just Cookie) (toList mb))

-- | 按提示走 n 步（注册表 reg），记下每步之后的状态。
hintPlay :: Registry -> GameState -> Int -> [GameState]
hintPlay reg gs n
  | n <= 0 || gsOver gs /= Nothing = []
  | otherwise = case findHintWith reg (gsBoard gs) of
      Nothing -> []
      Just (p, q) -> let (gs', _, _) = resolveSwapWith reg p q gs in gs' : hintPlay reg gs' (n - 1)

-- | 一步贪心：优先收走饼干最多，其次盘上饼干越靠下越好，再其次得分；同分取行优先第一对。
greedyPlay :: GameState -> [GameState]
greedyPlay gs
  | gsOver gs /= Nothing = []
  | otherwise =
      case [ (key g', g')
           | (i, (p, q)) <- zip [0 :: Int ..] [((r, c), d) | r <- [0 .. 7], c <- [0 .. 7], d <- [(r, c + 1), (r + 1, c)], inBounds d]
           , let (g', o, _) = resolveSwapWith defaultRegistry p q gs
           , o `notElem` [NoMatch, InvalidSwap]
           , let key g = (negate (gsCount CountCookies g), negate (sum (map fst (cookiesOn (gsBoard g)))), negate (gsScore g), i) ] of
        [] -> []
        cs -> let (_, g') = foldr1 (\a b -> if fst a <= fst b then a else b) cs in g' : greedyPlay g'

-- | 关卡表里只有第 46 关写了 lvlDrops（四个顶行掉落口、饼干、保持 4 块）；开局的关卡级元素只有它回复；
-- 视图模型 bvDrops 给出掉落口格；GameState 的 Show 不打印这个内置元素。
cd_only_level46_has_drops :: Assertion
cd_only_level46_has_drops = do
  assertEqual "only level 46" [(dropLevel, [DropSpec dropCellsL46 Cookie 4])] [(li, lvlDrops l) | (li, l) <- zip [0 ..] allLevels, not (null (lvlDrops l))]
  assertEqual "level 46 drop cells" dropCellsL46 (levelDrops (gsLevelElems (levelGame dropLevel 1)))
  assertEqual "view drop cells" dropCellsL46 (bvDrops (boardView (levelGame dropLevel 1)))
  assertEqual "no drops elsewhere" [] [li | li <- [0 .. dropLevel - 1], not (null (levelDrops (gsLevelElems (levelGame li 1))))]
  assertEqual "no drops in daily" [] (levelDrops (gsLevelElems (dailyGame (2026, 9, 30))))
  assertBool "Show hides the element" (not (any (`isInfixOf` show (levelGame dropLevel 1)) ["CookieDrop", "cookie_drop", "lvlDrops"]))

-- | dropRefill：掉落口格的空洞、盘上饼干不足时补饼干；非掉落口格、或饼干已够时照原策略；
-- 同一次补子里行优先先补的掉落口先占名额。
cd_refill_drops_cookie_at_drop_cells :: Assertion
cd_refill_drops_cookie_at_drop_cells = do
  let spec = DropSpec [(0, 1), (0, 6)] Cookie 2
      pol = dropRefill [spec] defaultRefill
      holes ps = foldl (\mb p -> setM mb p Nothing) (toM stableBoard) ps
      fill mb = fst (refillWith pol (mkStdGen 7) mb)
      base mb = fst (refillWith defaultRefill (mkStdGen 7) mb)
      -- 列 1 / 列 6 / 列 3 顶上各两个空洞
      mb0 = holes [(0, 1), (1, 1), (0, 6), (1, 6), (0, 3), (1, 3)]
      b0 = fill mb0
  assertEqual "both drop cells get cookies" [(0, 1), (0, 6)] (cookiesOn b0)
  assertEqual "other holes as base" [getCell (base mb0) p | p <- [(1, 1), (1, 6), (0, 3), (1, 3)]] [getCell b0 p | p <- [(1, 1), (1, 6), (0, 3), (1, 3)]]
  -- 盘上已有一块饼干：只剩一个名额，行优先先补的 (0,1) 拿到
  let mb1 = setM mb0 (5, 5) (Just Cookie)
      b1 = fill mb1
  assertEqual "one slot left, row-major first" [(0, 1), (5, 5)] (cookiesOn b1)
  assertEqual "second drop cell as base" (getCell (base mb1) (0, 6)) (getCell b1 (0, 6))
  -- 已够两块：不再掉
  let mb2 = setM (setM mb0 (5, 5) (Just Cookie)) (6, 2) (Just Cookie)
  assertEqual "enough cookies: no drop" (base mb2) (fill mb2)
  -- 掉落口格没有空洞：什么都不发生
  let mb3 = holes [(0, 3), (1, 3)]
  assertEqual "no hole at drop cells" (base mb3) (fill mb3)
  assertEqual "counts on the partially filled board" 1 (cookiesM (setM mb0 (5, 5) (Just Cookie)))
  assertEqual "atM sanity" (Just Nothing) (Just (atM mb0 (0, 1)))

-- | 随机数：掉落口策略对每个空洞照样调一次原策略，补满后的生成器与原策略完全相同；
-- 补出来的盘面只在掉落口格上不同。
cd_refill_same_rng_as_base :: Assertion
cd_refill_same_rng_as_base = do
  let pol = dropRefill [DropSpec dropCellsL46 Cookie 4] defaultRefill
      holesAt = [(r, c) | c <- [0 .. 7], r <- [0 .. c `mod` 3]]
      mb = foldl (\m p -> setM m p Nothing) (toM stableBoard) holesAt
  mapM_
    ( \s -> do
        let (bD, gD) = refillWith pol (mkStdGen s) mb
            (bB, gB) = refillWith defaultRefill (mkStdGen s) mb
        assertEqual ("seed " ++ show s ++ " generator") (show gB) (show gD)
        assertEqual ("seed " ++ show s ++ " differs only at drop cells") [] [p | p <- allPos, getCell bD p /= getCell bB p, p `notElem` dropCellsL46]
        assertEqual ("seed " ++ show s ++ " cookies at drop cells") dropCellsL46 (cookiesOn bD)
    )
    [1 .. 5]
  assertEqual "policy name" "random-gem+drop" (refillName pol)

-- | 第 46 关开局：四块饼干恰在掉落口格（没有目标补齐）；目标收 8 块；原有饼干关（第 22 / 23 关）仍按目标补齐。
cd_level46_start_no_goal_decor :: Assertion
cd_level46_start_no_goal_decor = do
  mapM_ (\s -> assertEqual ("seed " ++ show s) dropCellsL46 (cookiesOn (gsBoard (levelGame dropLevel s)))) [1 .. 3]
  assertEqual "goal" (goalCount CountCookies 8) (gsGoal (levelGame dropLevel 1))
  mapM_
    ( \li -> do
        let gs = levelGame li 1
        assertBool ("level " ++ show (li + 1) ++ " still topped up to its goal") (length (cookiesOn (gsBoard gs)) >= goalTarget (gsGoal gs))
    )
    [21, 22]
  -- 掉落口开局与注册表无关：去掉 cookie_drop 条目开局相同（只是之后不再掉）
  let off = removeLevel "cookie_drop" defaultRegistry
      l46 = levelAt dropLevel
  assertEqual "start board independent of the entry" (gsBoard (levelGame dropLevel 2)) (gsBoard (newGameAtLevelWith off dropLevel (levelConfig l46) 2))

-- | 实战（第 46 关种子 1–6，一步贪心走满）：盘上饼干始终 ≤ 4；每局都掉过饼干（收走 + 盘上 > 开局 4 块）并有收集计数；
-- 新出现的饼干只在掉落口格；去掉 cookie_drop 后同样走法不会有新饼干。
cd_drops_and_collects_in_play :: Assertion
cd_drops_and_collects_in_play = do
  mapM_
    ( \s -> do
        let gs0 = levelGame dropLevel s
            states = gs0 : greedyPlay gs0
            onBoard = map (length . cookiesOn . gsBoard) states
            collected = gsCount CountCookies (last states)
        assertBool ("seed " ++ show s ++ " at most 4 on board") (all (<= 4) onBoard)
        assertBool ("seed " ++ show s ++ " dropped some") (collected + last onBoard > 4)
        assertBool ("seed " ++ show s ++ " collected some") (collected > 0)
        -- 每一步：新增的饼干（盘上多出来的 + 本步收走的）不超过掉落口数
        mapM_
          ( \(a, b) -> do
              let new = length (cookiesOn (gsBoard b)) + gsCount CountCookies b - length (cookiesOn (gsBoard a)) - gsCount CountCookies a
              assertBool ("seed " ++ show s ++ " new cookies per step") (new >= 0 && new <= length dropCellsL46)
          )
          (zip states (drop 1 states))
    )
    [1 .. 6]
  let off = removeLevel "cookie_drop" defaultRegistry
      noDrop = hintPlay off (levelGame dropLevel 1) 26
  assertBool "without the entry: never more than the starting 4" (all (\g -> length (cookiesOn (gsBoard g)) + gsCount CountCookies g <= 4) noDrop)
  assertEqual "counts via drains" (gsCount CountCookies (last (levelGame dropLevel 1 : greedyPlay (levelGame dropLevel 1)))) (countOf CountCookies (gsCounts (last (levelGame dropLevel 1 : greedyPlay (levelGame dropLevel 1)))))

-- | 去掉 cookie_drop 条目的注册表：前 45 关与每日挑战按提示各走 6 步，盘面、分数、计数、步数、gsGen 逐关相同。
cd_other_levels_unchanged :: Assertion
cd_other_levels_unchanged = do
  let off = removeLevel "cookie_drop" defaultRegistry
      key gs = (gsBoard gs, gsScore gs, gsCounts gs, gsMoves gs, show (gsGen gs))
      run reg gs = key (last (gs : hintPlay reg gs 6))
  mapM_ (\li -> assertEqual ("level " ++ show (li + 1)) (run off (levelGame li 3)) (run defaultRegistry (levelGame li 3))) [0 .. dropLevel - 1]
  mapM_ (\d -> assertEqual ("daily " ++ show d) (run off (dailyGame d)) (run defaultRegistry (dailyGame d))) [(2026, 9, 30), (2026, 10, 1), (2026, 10, 2)]
  -- 第 46 关两者不同（确实经这个条目生效；按提示要收走过饼干才会掉，所以走满 26 步、看 30 个种子）
  let runAll reg gs = key (last (gs : hintPlay reg gs 26))
  assertBool "level 46 differs" (any (\s -> runAll off (levelGame dropLevel s) /= runAll defaultRegistry (levelGame dropLevel s)) [1 .. 30])

-- | 难度（backlog 标准：按提示 30 局赢超过 25 局算太容易）：按提示走种子 1–30 赢不超过 25 局；
-- 一步贪心能过关（种子 1 过关），说明关卡可玩。
cd_level46_difficulty :: Assertion
cd_level46_difficulty = do
  let hintWins = length [() | s <- [1 .. 30], let st = hintPlay defaultRegistry (levelGame dropLevel s) 26, not (null st), isWin (gsOver (last st))]
  assertBool ("hint wins " ++ show hintWins ++ "/30 <= 25") (hintWins <= 25)
  let g1 = last (levelGame dropLevel 1 : greedyPlay (levelGame dropLevel 1))
  assertBool "greedy clears seed 1" (isWin (gsOver g1))
