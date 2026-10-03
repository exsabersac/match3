{-# LANGUAGE OverloadedStrings #-}
-- | 扩展钩子（段 2c）：按元素名计数的目标 GoalNamed、地面层元素槽、方向可配的边缘收集、
-- 步末之后的补结算、自定义注册表下的手动洗牌。所用样例元素（木箱、苔藓、风筝、陷坑、浮尘）
-- **只定义在测试里**，主流程源码里没有它们的名字；这组测试证明双层果冻、气泡这类元素今后
-- 可以只靠注册表 + 白名单钩子接入。第 8 刀：扩展一条形状规则（L / T → 炸弹）、一条组合规则（直线 × 普通宝石）、
-- 一种补子策略（关卡级元素换成掉金币 / 关卡颜色数），都只改注册表的规则表或加一个关卡级元素，主流程不用改。
module Spec.Extension
  ( tests
  ) where

import Data.List (isPrefixOf)
import Engine.Game (Game(..), Step(..))
import Match3.Board.Default (findMatchRuns, hasAnyMatch)
import Match3.Board.Match (MatchRun(..))
import Match3.Core
import Match3.Board.Grid (inBounds, setCell, swapCells)
import Match3.Counts (namedCounts)
import Match3.Element (EndPhase(..), EndRule(..), Edge(..), Entry, customEntry, groundEntry, register)
import Match3.Element.Caps (Element(..), atEnd, blocker, counts, displays, drainsAt, fixed, ground, labelled, loseHintIs, piece, reshuffles)
import Match3.Element.Registry (displayLabelWith, loseHintWith)
import Match3.Element.Types (FaceValue(..))
import Match3.View (cellExtras, cellExtrasWith)
import Match3.Element.Event (Event(..), EventKind(..))
import Match3.Game.Level (newGame)
import Match3.Game.State (gsCollected, gsCount, gsGround, setGround)
import Match3.Game.Trace (traceEventsWith)
import Match3.Game.Shuffle (shuffleGameWith)
import Match3.Game.Move (resolveSwapWith, trySwap)
import qualified Match3.Engine as M3E
import Data.List (intersect, nub, sort)
import Match3.Board.Clear (clearMatchesDetailedWith)
import Match3.Board.Gravity (settleRefillWith)
import Match3.Board.Grid (mboardFromRows)
import Match3.Element
  ( ComboRule(..), RefillPolicy(..), ShapeCtx(..), ShapeRule(..), colorsRefill, comboFires, comboRules
  , inertEntry, levelHooksWith, registerLevel, setComboRules, setRefillPolicy, setShapeRules, shapeRules )
import Match3.Element.Class (LevelElement(..), SomeLevelElement(..), SomeMessage(..))
import Match3.Element.Message (Judging(..), Refilling(..), fromMessage)
import Match3.Element.Level (judgeIn)
import qualified Match3.Combos as Combos
import Match3.Types (goalCount, goalScore)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "ext_board_modules_take_registry" ext_board_modules_take_registry
  , testCase "ext_goal_named_counts_crate" ext_goal_named_counts_crate
  , testCase "ext_ground_layer_test_element" ext_ground_layer_test_element
  , testCase "ext_edge_drain_side_collectible" ext_edge_drain_side_collectible
  , testCase "ext_post_end_settle_hole_element" ext_post_end_settle_hole_element
  , testCase "ext_manual_shuffle_keeps_crate_via_engine" ext_manual_shuffle_keeps_crate_via_engine
  , testCase "ext_end_effect_generic_hopper" ext_end_effect_generic_hopper
  , testCase "ext_shape_rule_lt_bomb" ext_shape_rule_lt_bomb
  , testCase "ext_combo_rule_line_gem" ext_combo_rule_line_gem
  , testCase "ext_refill_policy_level_element" ext_refill_policy_level_element
  , testCase "ext_refill_policy_level_colors" ext_refill_policy_level_colors
  , testCase "ext_judging_level_element" ext_judging_level_element
  , testCase "judge_default_no_replier" judge_default_no_replier
  , testCase "ext_element_display_fields" ext_element_display_fields
  ]

-- tripleBoard / tripleMove / allPos / customsOn / isWin 见 Spec.Support。

-- | 通用层之下的 Board.*（除了按内置注册表包一层的 Board.Default）不再直接依赖内置注册表（全程收 reg）：
-- 不 import Element.Builtin*，代码里（去掉注释与字符串）不用 defaultRegistry。
ext_board_modules_take_registry :: Assertion
ext_board_modules_take_registry = do
  files <- filter (/= "src/Match3/Board/Default.hs") <$> sourcesUnder "src/Match3/Board"
  assertBool "scanned Board core" (all (`elem` files) ["src/Match3/Board/" ++ m ++ ".hs" | m <- ["Cascade", "Clear", "Gravity", "Match"]])
  srcs <- mapM readFile files
  let importsBuiltin s = any ("Match3.Element.Builtin" `isPrefixOf`) (importsOf s)
      bad = [f | (f, s) <- zip files srcs, mentionsIdent "defaultRegistry" s || importsBuiltin s]
  assertEqual "no defaultRegistry / Element.Builtin in Board core" [] bad

-- | GoalNamed：测试专用木箱经 CountNamed "crate" 计数，GoalNamed "crate" 1 达成即过关（直接结算与通用接口两条入口）。
ext_goal_named_counts_crate :: Assertion
ext_goal_named_counts_crate = do
  let reg = register crateDef defaultRegistry
      gs0 d = (newGame (GameConfig 5 (goalCount (CountNamed "crate") 1)) 1) {gsBoard = crateBoard d}
      (gsA, oA, _) = resolveSwapWith reg (1, 2) (2, 2) (gs0 2)
  assertBool "durability 2: chipped, not counted, not over" (moveApplied oA && gsCollected gsA == 0 && gsOver gsA == Nothing)
  let (gsB, oB, _) = resolveSwapWith reg (1, 2) (2, 2) (gs0 1)
  assertBool "durability 1: broken, goal reached" (moveApplied oB && isWin (gsOver gsB))
  assertEqual "collected by name" 1 (gsCollected gsB)
  assertEqual "named counter" [("crate", 1)] (namedCounts (gsCounts gsB))
  let st = gameStep (M3E.match3GameWith reg) (gs0 1) (M3E.Swap (1, 2) (2, 2))
  assertBool "engine: accepted and won" (stepAccepted st && isWin (stepOutcome st))
  -- 内置表下木箱是惰性占格：不计数、不过关
  let (gsD, _, _) = resolveSwapWith defaultRegistry (1, 2) (2, 2) (gs0 1)
  assertEqual "default registry: not counted" 0 (gsCollected gsD)

-- | 测试专用地面层元素「苔藓」（地面层，2 层）：上方格子每被消除一次去一层、按层计入 GoalNamed；
-- 不占格、不挡交换、洗牌不动、撤销恢复；未注册时地面层原样不动。
newtype Moss = Moss Int
  deriving (Eq, Show)

instance Element Moss where
  name _ = "moss"
  toCell (Moss n) = Custom "moss" (CustomState n)
  caps _ = piece [ground (\n -> if n > 1 then Just (n - 1) else Nothing), counts (CountNamed "moss")]

mossDef :: Entry
mossDef = groundEntry (Moss 2)

ext_ground_layer_test_element :: Assertion
ext_ground_layer_test_element = do
  let reg = register mossDef defaultRegistry
      ground0 = [((1, 1), ("moss", 2)), ((6, 6), ("moss", 1))]
      gs0 = (setGround ground0 $ (newGame (GameConfig 5 (goalCount (CountNamed "moss") 3)) 1) {gsBoard = tripleBoard})
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
      hitsAt p = length [() | w <- mtWaves mt1, p `elem` (cwCleared w ++ cwDrained w)]
  assertBool "move applied" (moveApplied o1)
  assertEqual "(1,1) cleared exactly once this move" 1 (hitsAt (1, 1))
  assertEqual "(6,6) untouched this move" 0 (hitsAt (6, 6))
  assertEqual "one layer peeled, other tile untouched" [((1, 1), ("moss", 1)), ((6, 6), ("moss", 1))] (gsGround gs1)
  assertEqual "counted per layer" [("moss", 1)] (namedCounts (gsCounts gs1))
  assertEqual "goal progress by name" 1 (gsCollected gs1)
  assertEqual "ground does not occupy the board" (gsBoard (fst (trySwap p1 p2 (setGround [] $ gs0)))) (gsBoard gs1)
  -- 第二次消除同一格：清掉，累计 2
  let gs1' = gs1 {gsBoard = tripleBoard}
      (gs2, _, _) = resolveSwapWith reg p1 p2 gs1'
  assertEqual "second clear removes the tile" [((6, 6), ("moss", 1))] (gsGround gs2)
  assertEqual "count accumulates by layer" [("moss", 2)] (namedCounts (gsCounts gs2))
  -- 撤销恢复、洗牌不动
  assertEqual "undo restores ground" (Just (gsGround gs1)) (gsGround <$> stepThenUndo reg gs1' (M3E.Swap p1 p2))
  assertEqual "shuffle keeps ground" (gsGround gs1) (gsGround (shuffleGameWith reg gs1))
  -- 未注册：地面层原样
  let (gsD, _, _) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertEqual "unregistered ground untouched" ground0 (gsGround gsD)

-- | 测试专用侧边收集物「风筝」：drains = [EdgeLeft]，到左边即被收走并按 CountNamed "kite" 计数；
-- 内部格与底边的风筝不收；未注册时是惰性占格。内置饼干的底边收集由原有 cookie_* 测试与金标准锁定。
newtype Kite = Kite Int
  deriving (Eq, Show)

instance Element Kite where
  name _ = "kite"
  toCell (Kite k) = Custom "kite" (CustomState k)
  caps _ = blocker [drainsAt [EdgeLeft], counts (CountNamed "kite")]

kiteDef :: Entry
kiteDef = customEntry (Kite 1) (Kite . unCustomState)

ext_edge_drain_side_collectible :: Assertion
ext_edge_drain_side_collectible = do
  let reg = register kiteDef defaultRegistry
      board0 = foldl (\b p -> setCell b p (Custom "kite" (CustomState 1))) tripleBoard [(4, 0), (4, 3), (7, 5)]
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "kite") 1)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
  assertBool "move applied" (moveApplied o1)
  w1 <- firstWave mt1
  assertBool "left-edge kite drained in the first settle" ((4, 0) `elem` cwDrained w1)
  assertEqual "inner and bottom-edge kites stay" [(4, 3), (7, 5)] (customsOn "kite" (gsBoard gs1))
  assertEqual "counted by name" [("kite", 1)] (namedCounts (gsCounts gs1))
  assertBool "goal reached" (isWin (gsOver gs1))
  assertEqual "cookie counter untouched" 0 (gsCount CountCookies gs1)
  let (gsD, _, _) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertEqual "unregistered: nothing drained" [(4, 0), (4, 3), (7, 5)] (customsOn "kite" (gsBoard gsD))

-- | 测试专用「陷坑」（固定格）：步末规则（PhaseMove）不改盘，只经 erHoles 声明自己所在格为空洞 →
-- 步末补结算把它挖空、上方下落、补子、成消再连锁；终盘稳定、没有陷坑，回放里多一个只有沉降的轮次。
-- 补结算是统一路径（内置元素步末从不留下空洞，见 docs/testing.md 的扫描），没有开关。
newtype Sinkhole = Sinkhole Int
  deriving (Eq, Show)

instance Element Sinkhole where
  name _ = "sinkhole"
  toCell (Sinkhole k) = Custom "sinkhole" (CustomState k)
  caps _ = fixed [atEnd (EndRule PhaseMove 90 (\_ b -> (Nothing, b)) (const []) (customsOn "sinkhole"))]

sinkholeDef :: Entry
sinkholeDef = customEntry (Sinkhole 1) (Sinkhole . unCustomState)

ext_post_end_settle_hole_element :: Assertion
ext_post_end_settle_hole_element = do
  let reg = register sinkholeDef defaultRegistry
      board0 = setCell tripleBoard (5, 5) (Custom "sinkhole" (CustomState 1))
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
      settleWaves = [w | w <- mtWaves mt1, null (cwCleared w), atM (cwHoles w) (5, 5) == Nothing]
  assertBool "move applied" (moveApplied o1)
  assertEqual "sinkhole vacated" [] (customsOn "sinkhole" (gsBoard gs1))
  assertBool "board stable after post-end settle" (not (hasAnyMatch (mtFinal mt1)))
  case settleWaves of
    [w] -> do
      assertEqual "hole filled by the cell above" (getCell (cwBefore w) (4, 5)) (getCell (cwAfter w) (5, 5))
      assertEqual "settle wave scores nothing" 0 (cwScore w)
    _ -> assertFailure ("expected exactly one post-end settle wave, got " ++ show (length settleWaves))
  -- 通用接口入口一样生效
  let st = gameStep (M3E.match3GameWith reg) gs0 (M3E.Swap p1 p2)
  assertEqual "engine path same board" (gsBoard gs1) (gsBoard (stepState st))
  -- 内置表：陷坑是惰性占格，不产生空洞
  let (gsD, _, mtD) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertEqual "default registry: sinkhole stays" [(5, 5)] (customsOn "sinkhole" (gsBoard gsD))
  assertBool "default registry: no settle-only wave" (all (not . null . cwCleared) (mtWaves mtD))

-- | 手动洗牌走 match3GameWith customReg 的 Shuffle 动作：木箱原样保留；「浮尘」（keepOnShuffle = False 的障碍）
-- 只有按自定义表判定才会被洗走——证明洗牌用的是传进来的注册表，不再退回内置表。
newtype Dust = Dust Int
  deriving (Eq, Show)

instance Element Dust where
  name _ = "dust"
  toCell (Dust k) = Custom "dust" (CustomState k)
  caps _ = blocker [reshuffles]

dustDef :: Entry
dustDef = customEntry (Dust 1) (Dust . unCustomState)

ext_manual_shuffle_keeps_crate_via_engine :: Assertion
ext_manual_shuffle_keeps_crate_via_engine = do
  let reg = register dustDef (register crateDef defaultRegistry)
      gs0 = (newGame defaultConfig 1) {gsBoard = setCell (crateBoard 2) (5, 5) (Custom "dust" (CustomState 1))}
      st = gameStep (M3E.match3GameWith reg) gs0 M3E.Shuffle
      b1 = gsBoard (stepState st)
  assertBool "shuffle accepted" (stepAccepted st)
  assertBool "board reshuffled" (b1 /= gsBoard gs0 && gsShuffled (stepState st))
  assertEqual "crate kept in place" [((0, 1), Custom "crate" (CustomState 2))] (cratesOn b1)
  assertEqual "dust washed away under the custom registry" [] (customsOn "dust" b1)
  let stD = gameStep M3E.match3Game gs0 M3E.Shuffle
  assertEqual "built-in registry keeps unknown dust (inert default)" [(5, 5)] (customsOn "dust" (gsBoard (stepState stD)))

-- | 第 7 刀（7b）：步末效果是通用形状（事件类型 + 元素名 + 逐项 EndItem）。测试专用「跳跳虫」（固定格）在步末
-- （PhaseMove，排在蜗牛之后）向右跳一格、与右边的宝石换位，产出 EndEffect EvMove "hopper" —— 不改 Event / Trace /
-- 主流程：回放按时间线重放到终盘（applyEndEffect 逐项重放）、效果事件里有它、内置表下它是惰性占格。
newtype Hopper = Hopper Int
  deriving (Eq, Show)

instance Element Hopper where
  name _ = "hopper"
  toCell (Hopper k) = Custom "hopper" (CustomState k)
  caps _ = fixed [atEnd (EndRule PhaseMove 80 hop (const []) (const []))]
    where
      hop _ b0 =
        let step (items, b) p =
              let q = (fst p, snd p + 1)
              in case (inBounds b q, getCell b q) of
                   (True, g@Gem {}) -> (items ++ [EndItem p q (getCell b p) (Just g)], setCell (setCell b q (getCell b p)) p g)
                   _ -> (items, b)
            (its, b1) = foldl step ([], b0) (customsOn "hopper" b0)
        in (if null its then Nothing else Just (EndEffect EvMove "hopper" its), b1)

ext_end_effect_generic_hopper :: Assertion
ext_end_effect_generic_hopper = do
  let reg = register (customEntry (Hopper 1) (Hopper . unCustomState)) defaultRegistry
      hopper = Custom "hopper" (CustomState 1)
      board0 = setCell tripleBoard (7, 0) hopper
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
      hops = [e | e <- mtEnd mt1, endEffectElement (esEffect e) == "hopper"]
  assertBool "move applied" (moveApplied o1)
  case hops of
    [e] -> do
      endEffectKind (esEffect e) @?= EvMove
      endEffectItems (esEffect e) @?= [EndItem (7, 0) (7, 1) hopper (Just (getCell (esBefore e) (7, 1)))]
      assertBool "Show falls back to the record form" ("EndEffect {endEffectKind = EvMove" `isPrefixOf` show (esEffect e))
      assertEqual "event carries the pairs" [(EvMove, "hopper", [((7, 0), (7, 1))])]
        [(evKind ev, evElement ev, evCells ev) | ev <- traceEventsWith reg mt1, evElement ev == "hopper"]
    _ -> assertFailure ("expected one hopper end step, got " ++ show (length hops))
  _ <- replayTimeline "hopper" mt1
  assertEqual "hopped right" [(7, 1)] (customsOn "hopper" (gsBoard gs1))
  let (gsD, _, mtD) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertEqual "default registry: hopper stays" [(7, 0)] (customsOn "hopper" (gsBoard gsD))
  assertBool "default registry: no hopper effect" (all ((/= "hopper") . endEffectElement . esEffect) (mtEnd mtD))

--------------------------------------------------------------------------------
-- 第 8 刀：规则表 / 补子策略的扩展

-- | L / T 形状规则（只在测试里）：同色横竖两条连线交叉时，横线在交点（可清时）放一颗炸弹，竖线认领但不生成
-- （否则竖线的直线规则会在交点上覆盖）；不交叉的连线交给后面的直线规则。
ltBombRule :: ShapeRule
ltBombRule = ShapeRule "l/t→bomb" spawn
  where
    spawn ctx run =
      case [x | o <- scRuns ctx, runColor o == runColor run, runIsH o /= runIsH run, x <- runPos run `intersect` runPos o] of
        [] -> Nothing
        (x : _)
          | runIsH run -> Just [(x, Gem (runColor run) Bomb 0 Nothing) | x `elem` scClearable ctx]
          | otherwise -> Just []

-- | L 形用例：交换 (2,3)↔(3,3) 后 (3,1)(3,2)(3,3) 与 (3,3)(4,3)(5,3) 都是 C1 三连（交点 (3,3)）。
lBoard :: Board
lBoard = setCells stableBoard [((3, 1), mkGem C1), ((3, 2), mkGem C1), ((4, 3), mkGem C1), ((5, 3), mkGem C1), ((2, 3), mkGem C1), ((3, 3), mkGem C3)]

-- | 形状规则表扩展：把 L / T 规则插到内置表最前面（优先于直线规则），注册表之外什么都不改——
-- 同一局面在内置表下不生成特殊块（两条三连），扩展后交点生成炸弹；走正式的交换流程（resolveSwapWith）也一样。
ext_shape_rule_lt_bomb :: Assertion
ext_shape_rule_lt_bomb = do
  let reg = setShapeRules (ltBombRule : shapeRules defaultRegistry) defaultRegistry
      (p1, p2) = ((2, 3), (3, 3))
      swapped = swapCells lBoard p1 p2
      spawnedAt r = let (mb, _, _) = clearMatchesDetailedWith r (Just p2) swapped in atM mb (3, 3)
  assertBool "no match before the swap" (not (hasAnyMatch lBoard))
  assertEqual "two crossing runs" [True, False] (map runIsH (findMatchRuns swapped))
  assertEqual "builtin table: plain L spawns nothing" Nothing (spawnedAt defaultRegistry)
  assertEqual "extended table: bomb at the corner" (Just (Gem C1 Bomb 0 Nothing)) (spawnedAt reg)
  let gs0 = (newGame (GameConfig 5 (goalScore 99999)) 7) {gsBoard = lBoard}
      (_, o, mt) = resolveSwapWith reg p1 p2 gs0
  assertBool "move accepted" (o `notElem` [NoMatch, InvalidSwap])
  w <- firstWave mt
  assertEqual "first wave leaves the bomb at the corner" (Just (Gem C1 Bomb 0 Nothing)) (atM (cwHoles w) (3, 3))
  -- 内置表下同一步的第一轮交点是空洞
  let (_, _, mt0) = resolveSwapWith defaultRegistry p1 p2 gs0
  w0 <- firstWave mt0
  assertEqual "builtin: corner is a hole" Nothing (atM (cwHoles w0) (3, 3))

-- | 直线 × 普通宝石组合（只在测试里）：直线端清整行 + 整列。
lineGemRule :: ComboRule
lineGemRule = ComboRule "line×gem" isLineCell isNormalCell (\b l _ -> Combos.fullRowCol b l)
  where
    isLineCell c = case c of
      Gem _ k _ _ -> k == LineH || k == LineV
      _ -> False
    isNormalCell c = case c of
      Gem _ Normal _ _ -> True
      _ -> False

-- | 组合表扩展：往内置组合表末尾加一条「直线 × 普通宝石」，注册表之外什么都不改——内置表下这一步不成三消、
-- 被拒；扩展后按组合起手（两个方向都成立），第一轮清掉直线端所在的整行整列。
ext_combo_rule_line_gem :: Assertion
ext_combo_rule_line_gem = do
  let reg = setComboRules (comboRules defaultRegistry ++ [lineGemRule]) defaultRegistry
      board = setCell stableBoard (0, 0) (Gem C1 LineH 0 Nothing)
      (p1, p2) = ((0, 0), (0, 1))
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 7) {gsBoard = board}
      (_, o0, _) = resolveSwapWith defaultRegistry p1 p2 gs0
      (_, o, mt) = resolveSwapWith reg p1 p2 gs0
  assertEqual "builtin: rejected" NoMatch o0
  assertBool "table rule fires both ways" (comboFires (comboRules reg) board p1 p2 && comboFires (comboRules reg) board p2 p1)
  assertBool "builtin table does not fire" (not (comboFires (comboRules defaultRegistry) board p1 p2))
  assertBool "extended: accepted" (o `notElem` [NoMatch, InvalidSwap])
  w <- firstWave mt
  let rowCol = nub ([(0, c) | c <- [0 .. boardSize - 1]] ++ [(r, 1) | r <- [0 .. boardSize - 1]])
  assertBool "first wave clears the line end's row and column" (all (`elem` cwCleared w) rowCol)

-- | 掉金币的关卡级元素（只在测试里）：回复补子查询，把补子策略换成「每个空洞补一枚金币」（不消耗随机数）。
data CoinRain = CoinRain
  deriving (Eq, Show)

instance LevelElement CoinRain where
  levelName _ = "coin_rain"
  levelReply e msg
    | Just (Refilling _) <- fromMessage msg =
        Just (SomeMessage (Refilling (RefillPolicy "coins" (\_ g -> (Custom "coin" (CustomState 1), g)))), e)
    | otherwise = Nothing

-- | 补子策略扩展（关卡级元素）：注册 CoinRain 之后，交换出的 4 连（横消落在交换点 (1,2)）挖出的 3 个洞补成金币，
-- 金币无色不再连锁；其余什么都不改。注册表缺省策略下同一步不出金币。
ext_refill_policy_level_element :: Assertion
ext_refill_policy_level_element = do
  let reg = registerLevel (SomeLevelElement CoinRain) (register (inertEntry "coin") defaultRegistry)
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 7) {gsBoard = tripleBoard}
      (p1, p2) = tripleMove
      (gs1, o, mt) = resolveSwapWith reg p1 p2 gs0
      (gsD, _, _) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertBool "accepted" (o `notElem` [NoMatch, InvalidSwap])
  assertEqual "one wave" 1 (length (mtWaves mt))
  assertEqual "three coins refilled at the top (the line_h sits at (1,2))" [(0, 0), (0, 1), (0, 3)] (sort (customsOn "coin" (gsBoard gs1)))
  assertEqual "default policy: no coins" [] (customsOn "coin" (gsBoard gsD))

-- | 补子策略扩展（关卡颜色数）：注册表换成只用 3 色的策略（colorsRefill 3），沉降补子只补 C1..C3；
-- 同一盘面在缺省策略下会出现别的颜色。
ext_refill_policy_level_colors :: Assertion
ext_refill_policy_level_colors = do
  let reg = setRefillPolicy (colorsRefill 3) defaultRegistry
      mb = mboardFromRows (replicate boardSize (replicate boardSize Nothing))
      colorsOf r = nub (sort [c | p <- allPos, Gem c Normal 0 Nothing <- [getCell b p]])
        where
          (b, _, _) = settleRefillWith r (levelHooksWith r []) (mkStdGen 42) mb
  assertEqual "three colours only" [C1, C2, C3] (colorsOf reg)
  assertEqual "default: all five" [C1, C2, C3, C4, C5] (colorsOf defaultRegistry)

-- | 胜负节拍（Judging）：测试专用「限时」关卡级元素在剩余步数 ≤ 3 时把未结束的一步判成输（Lost 总分）；
-- 只 registerLevel 即可接入，主流程不改。内置注册表下同一步照常 MoveApplied；步数充足时限时元素不改结局。
data TimeLimit = TimeLimit
  deriving (Eq, Show)

instance LevelElement TimeLimit where
  levelName _ = "time_limit"
  levelReply l msg
    | Just (Judging b score moves (MoveApplied _)) <- fromMessage msg
    , moves <= 3 =
        Just (SomeMessage (Judging b score moves (Lost score)), l)
    | otherwise = Nothing

ext_judging_level_element :: Assertion
ext_judging_level_element = do
  let reg = registerLevel (SomeLevelElement TimeLimit) defaultRegistry
      start moves = (newGame (GameConfig moves (goalScore 99999)) 7) {gsBoard = tripleBoard}
      (p1, p2) = tripleMove
      (gs1, o, _) = resolveSwapWith reg p1 p2 (start 4)
      (gsD, oD, _) = resolveSwapWith defaultRegistry p1 p2 (start 4)
      (gsL, oL, _) = resolveSwapWith reg p1 p2 (start 10)
  assertEqual "time limit: 3 moves left -> Lost" (Lost (gsScore gs1)) o
  assertEqual "gsOver follows the judged outcome" (Just (TLost (gsScore gs1))) (gsOver gs1)
  assertBool "default registry: move applied" (moveApplied oD && gsOver gsD == Nothing)
  assertBool "plenty of moves: not judged" (moveApplied oL && gsOver gsL == Nothing)

-- | 内置关卡级元素都不回复胜负节拍：全部战役关开局（种子 1），任何结局交给 judgeIn 都原样返回。
judge_default_no_replier :: Assertion
judge_default_no_replier =
  sequence_
    [ assertEqual ("level " ++ show li ++ " " ++ show o) o (judgeIn defaultRegistry (gsLevelElems gs) (gsBoard gs) (gsScore gs) (gsMoves gs) o)
    | li <- [0 .. campaignLevelCount - 1]
    , let gs = levelGame li 1
    , o <- [MoveApplied 5, Lost 3, Won 7, LevelClear 9 (li + 1)]
    ]


-- | 测试专用灯笼（Custom "lantern" k）：显示附加字段、目标中文名与失败提示都只写在元素的 caps 里
-- （displays / labelled / loseHintIs），View 的 cellExtrasWith 与注册表的 displayLabelWith / loseHintWith 直接取到，
-- 主流程与前端不用改；没注册时什么都没有。
newtype Lantern = Lantern Int
  deriving (Eq, Show)

instance Element Lantern where
  name _ = "lantern"
  toCell (Lantern k) = Custom "lantern" (CustomState k)
  caps (Lantern k) =
    blocker [counts (CountNamed "lantern"), labelled "灯笼", loseHintIs (\n -> "点亮灯笼，目标 " ++ show n ++ " 盏"), displays [("lit", FaceBool (k > 0)), ("k", FaceInt k)]]

ext_element_display_fields :: Assertion
ext_element_display_fields = do
  let reg = register (customEntry (Lantern 0) (Lantern . unCustomState)) defaultRegistry
      cell k = Custom "lantern" (CustomState k)
  assertEqual "extras (lit)" [("lit", FaceBool True), ("k", FaceInt 2)] (cellExtrasWith reg (cell 2))
  assertEqual "extras (dark)" [("lit", FaceBool False), ("k", FaceInt 0)] (cellExtrasWith reg (cell 0))
  assertEqual "unregistered: none" [] (cellExtras (cell 2))
  assertEqual "label" (Just "灯笼") (displayLabelWith reg "lantern")
  assertEqual "lose hint" (Just "点亮灯笼，目标 5 盏") (fmap ($ 5) (loseHintWith reg "lantern"))
  assertEqual "unregistered: no label" Nothing (displayLabelWith defaultRegistry "lantern")
  assertEqual "builtin without hint" Nothing (fmap ($ 5) (loseHintWith reg "bubble"))

