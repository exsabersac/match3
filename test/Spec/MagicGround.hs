{-# LANGUAGE OverloadedStrings #-}
-- | 新玩法 8（2026-09-30）：魔法地格——地面层 "magic"（值恒为 1）。不被消耗、不计数、不挡交换 / 匹配；
-- 特效（直线 / 炸弹）在这一格上引爆时爆炸范围向外扩一圈（原范围各格的八邻格并进来：直线一行 → 三行，
-- 炸弹 3×3 → 5×5），扩出来的格与原范围一样算直接命中。只看引爆格；彩虹取色、特效 × 特效组合给的清除种子不扩
-- （种子里的特效照常按各自的引爆格判断）。
-- 实现是通用钩子：能力 widens（StepCaps.stWiden）→ 每步 levelRegistryIn 把地面层的扩爆格写进注册表
-- （regWiden，缺省空）→ blastWith 按引爆格改写范围。第 48 关「魔法格」用到它（底行 8 块三层碎石）。
module Spec.MagicGround
  ( tests
  ) where

import Match3.Core
import Match3.Board.Match (findHintWith, hasAnyMatchWith)
import Match3.Combos (builtinComboRules)
import Match3.Counts (countOf)
import Match3.Daily (dailyConfig)
import Match3.Element
  ( Jelly(..)
  , MagicGround(..)
  , blastWith
  , builtinDefs
  , builtinLevelDefs
  , builtinShapeRules
  , hitGroundWith
  , levelRegistryIn
  , magicGroundName
  , magicWiden
  , swapOpeningWith
  , widenedCells
  )
import Match3.Element.Class (Element(..), widenRule)
import Match3.Element.Event (Event(..), EventKind(..))
import Match3.Element.Registry (Registry, entryName, mkRegistry, registerLevel, setComboRules, setShapeRules)
import Match3.Engine (Action(..), Played(..), playWith)
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.State (gsGround)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "mg_caps_ground_not_consumed" mg_caps_ground_not_consumed
  , testCase "mg_widen_one_ring" mg_widen_one_ring
  , testCase "mg_blast_widened_only_at_magic_cell" mg_blast_widened_only_at_magic_cell
  , testCase "mg_swap_and_hammer_reach_bottom_row" mg_swap_and_hammer_reach_bottom_row
  , testCase "mg_combo_seeds_not_widened" mg_combo_seeds_not_widened
  , testCase "mg_default_no_widening" mg_default_no_widening
  , testCase "mg_other_levels_unchanged" mg_other_levels_unchanged
  , testCase "mg_level48_layout_and_difficulty" mg_level48_layout_and_difficulty
  ]

-- | 第 48 关（0 基下标 47）。
mgLevel :: Int
mgLevel = 47

-- | 第 48 关的魔法地格（地面层顺序）。
mgCells :: [Pos]
mgCells = [(6, 2), (6, 5), (5, 3), (5, 4)]

-- | 去掉魔法地格条目的注册表（地面层里的 "magic" 查不到条目 → 没有扩爆格）。
noMg :: Registry
noMg = setShapeRules builtinShapeRules . setComboRules builtinComboRules $
  foldl (flip registerLevel) (mkRegistry (filter ((/= "magic") . entryName) builtinDefs)) builtinLevelDefs

-- | 第 48 关开局（种子 1），盘面上 7 行换成 stableBoard（没有现成三消），底行仍是 8 块三层碎石。
mgGame :: [(Pos, Cell)] -> GameState
mgGame extra =
  let gs = levelGame mgLevel 1
      base = setCells stableBoard [((7, c), getCell (gsBoard gs) (7, c)) | c <- [0 .. 7]]
  in gs {gsBoard = setCells base extra}

row7 :: GameState -> [Cell]
row7 gs = [getCell (gsBoard gs) (7, c) | c <- [0 .. 7]]

-- | 列 cs 上的碎石削到 2 层、其余仍 3 层。
chipped :: [Int] -> [Cell]
chipped cs = [Stone (if c `elem` cs then 2 else 3) | c <- [0 .. 7]]

line :: GemKind -> Cell
line sp = Gem C4 sp 0 Nothing

blastCells :: Played -> [[Pos]]
blastCells pd = [map snd (evCells e) | e <- pdEvents pd, evKind e == EvBlast]

-- | 能力：显示值恒为 1（Custom "magic" 1）、带扩爆规则；没有地面反应（上方消除不去层、不计数），
-- 果冻没有扩爆规则。实战里地面层始终是开局的 4 格、计数里没有 magic。
mg_caps_ground_not_consumed :: Assertion
mg_caps_ground_not_consumed = do
  assertEqual "name" "magic" magicGroundName
  assertEqual "display cell" (Custom "magic" (CustomState 1)) (toCell (MagicGround 1))
  let b8 = boardFromRows (replicate boardSize (replicate boardSize (mkGem C1)))
  assertEqual "widen rule = magicWiden" (Just (magicWiden b8 [(3, 3)])) (fmap (\f -> f b8 [(3, 3)]) (widenRule (MagicGround 1)))
  assertBool "jelly does not widen" (null (fmap (\f -> f b8 [(3, 3)]) (widenRule (Jelly 2))))
  let g0 = [(p, ("magic", 1)) | p <- mgCells]
  assertEqual "hit: kept, no counter" (g0, []) (hitGroundWith defaultRegistry mgCells g0)
  let gs0 = levelGame mgLevel 2
      play gs n
        | n <= (0 :: Int) || gsOver gs /= Nothing = gs
        | otherwise = case findHintWith defaultRegistry (gsBoard gs) of
            Nothing -> gs
            Just (p, q) -> let (gs', _, _) = resolveSwapWith defaultRegistry p q gs in play gs' (n - 1)
      gs6 = play gs0 6
  assertEqual "ground after 6 moves" (gsGround gs0) (gsGround gs6)
  assertEqual "no magic counter" 0 (countOf (CountNamed "magic") (gsCounts gs6))

-- | 扩一圈的几何：原范围在前、新并进来的格按行优先接在后面；盘边截断。
mg_widen_one_ring :: Assertion
mg_widen_one_ring = do
  let b8 = boardFromRows (replicate boardSize (replicate boardSize (mkGem C1)))
      row6 = [(6, c) | c <- [0 .. 7]]
  assertEqual "line: rows 5-7" (row6 ++ [(r, c) | r <- [5, 7], c <- [0 .. 7]]) (magicWiden b8 row6)
  let bomb33 = [(r, c) | r <- [2 .. 4], c <- [2 .. 4]]
  assertEqual "bomb: 5x5" 25 (length (magicWiden b8 bomb33))
  assertEqual "bomb: same cells" [(r, c) | r <- [1 .. 5], c <- [1 .. 5]] (filter (`elem` magicWiden b8 bomb33) [(r, c) | r <- [0 .. 7], c <- [0 .. 7]])
  assertEqual "corner clipped" [(0, 0), (0, 1), (1, 0), (1, 1)] (magicWiden b8 [(0, 0)])
  assertEqual "empty stays empty" [] (magicWiden b8 [])

-- | 第 48 关的注册表：本步扩爆格 = 4 格魔法地格；直线 / 炸弹只在引爆格是魔法地格时扩（别的格原样），
-- 缺省注册表不扩；非特效没有爆炸。
mg_blast_widened_only_at_magic_cell :: Assertion
mg_blast_widened_only_at_magic_cell = do
  let gs = levelGame mgLevel 1
      b = gsBoard gs
      reg = levelRegistryIn defaultRegistry (gsLevelElems gs)
      plainH p = blastWith defaultRegistry b (line LineH) p
  assertEqual "widened cells" mgCells (widenedCells reg)
  mapM_ (\p -> assertEqual ("widened at " ++ show p) (magicWiden b (plainH p)) (blastWith reg b (line LineH) p)) mgCells
  mapM_ (\p -> assertEqual ("plain at " ++ show p) (plainH p) (blastWith reg b (line LineH) p)) [(6, 3), (5, 2), (4, 3), (7, 2)]
  assertEqual "bomb at (6,2): 5x5 clipped" 20 (length (blastWith reg b (line Bomb) (6, 2)))
  assertEqual "plain gem: no blast" [] (blastWith reg b (mkGem C1) (6, 2))
  assertEqual "default registry: not widened" 8 (length (plainH (6, 2)))

-- | 实战：
-- * 交换 (5,4)↔(6,4) 连成 (6,2..4) 的 C4 三消，(6,2) 的横直线在魔法地格上引爆 → EvBlast 覆盖 5–7 行共 24 格；
-- * 锤子敲 (5,3)（魔法地格）上的横直线：扩到 4–6 行，第 6 行消掉后邻消削到底行，8 块碎石全削 1 层；
--   去掉魔法条目时只消第 5 行、底行不动；敲非魔法格 (5,2) 也不动；
-- * 锤子敲 (6,2) 上的炸弹 / 竖直线：底行削到的列 0–4 / 1–3（去掉魔法条目时 1–3 / 2）。
mg_swap_and_hammer_reach_bottom_row :: Assertion
mg_swap_and_hammer_reach_bottom_row = do
  let g0 = mgGame [((6, 2), line LineH), ((6, 3), mkGem C4), ((5, 4), mkGem C4)]
      pd = playWith defaultRegistry (Swap (5, 4) (6, 4)) g0
  assertBool "swap accepted" (pdAccepted pd)
  assertEqual "blast footprint rows 5-7"
    [[(6, c) | c <- [0 .. 7]] ++ [(r, c) | r <- [5, 7], c <- [0 .. 7]]] (blastCells pd)
  let pdNo = playWith noMg (Swap (5, 4) (6, 4)) g0
  assertEqual "without the entry: one row" [[(6, c) | c <- [0 .. 7]]] (blastCells pdNo)
  let hammer reg p sp = let (gs, _, _) = resolveHammerWith reg p (mgGame [(p, line sp)]) in row7 gs
  assertEqual "hammer line at (5,3)" (chipped [0 .. 7]) (hammer defaultRegistry (5, 3) LineH)
  assertEqual "hammer line at (5,3), no entry" (chipped []) (hammer noMg (5, 3) LineH)
  assertEqual "hammer line at (5,2) (not magic)" (chipped []) (hammer defaultRegistry (5, 2) LineH)
  assertEqual "hammer bomb at (6,2)" (chipped [0 .. 4]) (hammer defaultRegistry (6, 2) Bomb)
  assertEqual "hammer bomb at (6,2), no entry" (chipped [1 .. 3]) (hammer noMg (6, 2) Bomb)
  assertEqual "hammer vline at (6,2)" (chipped [1 .. 3]) (hammer defaultRegistry (6, 2) LineV)
  assertEqual "hammer vline at (6,2), no entry" (chipped [2]) (hammer noMg (6, 2) LineV)

-- | 成对交换规则（彩虹取色、特效 × 特效组合）给出的清除种子不扩（与缺省注册表逐项相同）；
-- 但种子里的特效照常逐个引爆、只看各自的引爆格：
-- * 彩虹 × 宝石（盘上没有别的特效）：与去掉魔法条目时逐项相同；
-- * 直线 × 直线：交换后 (6,2)（魔法地格）上的竖直线扩成 1–3 列（24 格），(6,3) 上的横直线仍是一行（8 格）；
-- * 炸弹 × 炸弹：(6,2) 上的炸弹 3×3 → 5×5（贴底边截成 20 格），(6,3) 上的仍是 9 格。
mg_combo_seeds_not_widened :: Assertion
mg_combo_seeds_not_widened = do
  let regW = levelRegistryIn defaultRegistry (gsLevelElems (levelGame mgLevel 1))
      key (gs, o, _) = (gsBoard gs, gsScore gs, gsCounts gs, o)
      swapped g = setCells (gsBoard g) [((6, 2), getCell (gsBoard g) (6, 3)), ((6, 3), getCell (gsBoard g) (6, 2))]
      seedsSame what g = assertEqual (what ++ ": seeds") (swapOpeningWith defaultRegistry (gsBoard g) (swapped g) (6, 2) (6, 3)) (swapOpeningWith regW (gsBoard g) (swapped g) (6, 2) (6, 3))
      blasts reg g = [(evElement e, src, length (evCells e)) | e <- pdEvents (playWith reg (Swap (6, 2) (6, 3)) g), evKind e == EvBlast, (src, _) : _ <- [evCells e]]
      gR = mgGame [((6, 2), Gem C1 Rainbow 0 Nothing)]
      gL = mgGame [((6, 2), line LineH), ((6, 3), Gem C2 LineV 0 Nothing)]
      gB = mgGame [((6, 2), line Bomb), ((6, 3), Gem C2 Bomb 0 Nothing)]
  mapM_ (uncurry seedsSame) [("rainbow x gem", gR), ("line x line", gL), ("bomb x bomb", gB)]
  assertBool "rainbow opens" (swapOpeningWith regW (gsBoard gR) (swapped gR) (6, 2) (6, 3) /= Nothing)
  assertEqual "rainbow x gem: as without the entry" (key (resolveSwapWith noMg (6, 2) (6, 3) gR)) (key (resolveSwapWith defaultRegistry (6, 2) (6, 3) gR))
  assertEqual "line x line" [("line_h", (6, 3), 8), ("line_v", (6, 2), 24)] (blasts defaultRegistry gL)
  assertEqual "line x line, no entry" [("line_h", (6, 3), 8), ("line_v", (6, 2), 8)] (blasts noMg gL)
  assertEqual "bomb x bomb" [("bomb", (6, 3), 9), ("bomb", (6, 2), 20)] (blasts defaultRegistry gB)
  assertEqual "bomb x bomb, no entry" [("bomb", (6, 3), 9), ("bomb", (6, 2), 9)] (blasts noMg gB)

-- | 没有魔法地格的关卡：本步注册表不设扩爆格（levelRegistryIn 不写 regWiden）；前 47 关与每日挑战都是这样。
mg_default_no_widening :: Assertion
mg_default_no_widening = do
  assertEqual "default registry" [] (widenedCells defaultRegistry)
  mapM_
    (\li -> assertEqual ("level " ++ show (li + 1)) [] (widenedCells (levelRegistryIn defaultRegistry (gsLevelElems (levelGame li 1)))))
    [0 .. mgLevel - 1]
  mapM_
    (\(y, m, d) -> assertEqual ("daily " ++ show (y, m, d)) [] (widenedCells (levelRegistryIn defaultRegistry (gsLevelElems (newDailyGame (dailyConfig (Year y) (Month m) (Day d)) (dailySeed (Year y) (Month m) (Day d)))))))
    [(2026, 9, 28), (2026, 9, 29), (2026, 9, 30)]

-- | 去掉魔法地格条目的注册表：前 47 关与 3 天的每日挑战按提示各走 6 步，盘面、得分、计数、步数、gsGen 逐项相同
-- （扩爆钩子缺省什么都不做，也不耗随机数）；原有关卡的地面层里没有 magic。
mg_other_levels_unchanged :: Assertion
mg_other_levels_unchanged = do
  let play reg gs n
        | n <= (0 :: Int) || gsOver gs /= Nothing = gs
        | otherwise = case findHintWith reg (gsBoard gs) of
            Nothing -> gs
            Just (p, q) -> let (gs', _, _) = resolveSwapWith reg p q gs in play reg gs' (n - 1)
      key gs = (gsBoard gs, gsScore gs, gsCounts gs, gsMoves gs, show (gsGen gs))
      daily (y, m, d) = newDailyGame (dailyConfig (Year y) (Month m) (Day d)) (dailySeed (Year y) (Month m) (Day d))
  assertEqual "older levels have no magic ground" [] [li | li <- [0 .. mgLevel - 1], any ((== "magic") . fst . snd) (lvlGround (levelAt li))]
  mapM_
    (\li -> assertEqual ("level " ++ show (li + 1)) (key (play noMg (levelGame li 3) 6)) (key (play defaultRegistry (levelGame li 3) 6)))
    [0 .. mgLevel - 1]
  mapM_
    (\d -> assertEqual ("daily " ++ show d) (key (play noMg (daily d) 6)) (key (play defaultRegistry (daily d) 6)))
    [(2026, 9, 28), (2026, 9, 29), (2026, 9, 30)]

-- | 第 48 关：名字「魔法格」、目标碎石 8 块、18 步、形状规则 bomb_shapes；地面层 4 格魔法地格；
-- 底行 8 块三层碎石、开局无现成三消；按提示 30 局赢 ≤ 25，一步贪心（先多碎石、再得分）种子 3 能过关。
mg_level48_layout_and_difficulty :: Assertion
mg_level48_layout_and_difficulty = do
  let lvl = levelAt mgLevel
  assertEqual "name" "魔法格" (lvlName lvl)
  assertEqual "goal" (Just 8) (case goalView (lvlGoal lvl) of ViewCount CountStones n -> Just n; _ -> Nothing)
  assertEqual "moves" 18 (lvlMoves lvl)
  assertEqual "rules" ["bomb_shapes"] (lvlRules lvl)
  assertEqual "ground" [(p, ("magic", 1)) | p <- mgCells] (lvlGround lvl)
  mapM_
    ( \s -> do
        let gs = levelGame mgLevel s
        assertEqual ("seed " ++ show s ++ " bottom row") (chipped []) (row7 gs)
        assertBool ("seed " ++ show s ++ " no standing run") (not (hasAnyMatchWith defaultRegistry (gsBoard gs)))
        assertEqual ("seed " ++ show s ++ " ground") [(p, ("magic", 1)) | p <- mgCells] (gsGround gs)
    )
    [1 .. 3]
  let hintRun s = go (levelGame mgLevel s) (18 :: Int)
        where
          go gs n
            | n <= 0 || gsOver gs /= Nothing = gs
            | otherwise = case findHintWith defaultRegistry (gsBoard gs) of
                Nothing -> gs
                Just (p, q) -> let (gs', _, _) = resolveSwapWith defaultRegistry p q gs in go gs' (n - 1)
      hintWins = length (filter (isWin . gsOver . hintRun) [1 .. 30])
  assertBool ("hint wins " ++ show hintWins ++ "/30 <= 25") (hintWins <= 25)
  assertBool "greedy clears seed 3" (isWin (gsOver (greedy (levelGame mgLevel 3))))
  where
    greedy gs
      | gsOver gs /= Nothing = gs
      | otherwise =
          case [ ((negate (countOf CountStones (gsCounts g')), negate (gsScore g'), i), g')
               | (i, (p, q)) <- zip [0 :: Int ..] [((r, c), d) | r <- [0 .. 7], c <- [0 .. 7], d <- [(r, c + 1), (r + 1, c)], fst d >= 0 && fst d < boardSize && snd d >= 0 && snd d < boardSize]
               , let (g', o, _) = resolveSwapWith defaultRegistry p q gs
               , o `notElem` [NoMatch, InvalidSwap] ] of
            [] -> gs
            cs -> greedy (snd (foldr1 (\x y -> if fst x <= fst y then x else y) cs))
