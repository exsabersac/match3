-- | 扩展钩子（段 2c）：按元素名计数的目标 GoalNamed、地面层元素槽、方向可配的边缘收集、
-- 步末之后的补结算、自定义注册表下的手动洗牌。所用样例元素（木箱、苔藓、风筝、陷坑、浮尘）
-- **只定义在测试里**，主流程源码里没有它们的名字；这组测试证明双层果冻、气泡这类元素今后
-- 可以只靠注册表 + 白名单钩子接入。
module Spec.Extension
  ( tests
  ) where

import Data.List (isInfixOf)
import Engine.Game (Game(..), Step(..))
import Match3.Core
import Match3.Element (Counter(..), EndPhase(..), EndRule(..), Edge(..), Entry, customEntry, defaultRegistry, groundEntry, register)
import Match3.Element.Class (Archetype(..), Element(..))
import Match3.Game.Shuffle (shuffleGameWith)
import Match3.Game.Move (resolveSwapWith)
import qualified Match3.Engine as M3E
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
  ]

-- | 第 1 行 (1,0)(1,1) 为 C5 的稳定盘：交换 (1,2)↔(2,2) 在第 1 行成 C5 三连。
tripleBoard :: Board
tripleBoard = foldl (\b (p, c) -> setCell b p c) stableBoard [((1, 0), mkGem C5), ((1, 1), mkGem C5)]

tripleMove :: (Pos, Pos)
tripleMove = ((1, 2), (2, 2))

allPos :: [Pos]
allPos = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

customsOn :: String -> Board -> [Pos]
customsOn n b = [p | p <- allPos, case getCell b p of Custom m _ -> m == n; _ -> False]

isWin :: Maybe Outcome -> Bool
isWin o = case o of
  Just (Won _) -> True
  Just (LevelClear _ _) -> True
  _ -> False

-- | 通用层之下的 Board.{Cascade, Clear, Gravity, Match} 不再直接依赖内置注册表（全程收 reg）。
ext_board_modules_take_registry :: Assertion
ext_board_modules_take_registry = do
  let files = ["src/Match3/Board/" ++ m ++ ".hs" | m <- ["Cascade", "Clear", "Gravity", "Match"]]
  srcs <- mapM readFile files
  let importsBuiltin s = any (\l -> take 7 l == "import " && "Element.Builtin" `isInfixOf` l) (lines s)
      bad = [f | (f, s) <- zip files srcs, "defaultRegistry" `isInfixOf` s || importsBuiltin s]
  assertEqual "no defaultRegistry / Element.Builtin in Board core" [] bad

-- | GoalNamed：测试专用木箱经 CountNamed "crate" 计数，GoalNamed "crate" 1 达成即过关（直接结算与通用接口两条入口）。
ext_goal_named_counts_crate :: Assertion
ext_goal_named_counts_crate = do
  let reg = register crateDef defaultRegistry
      gs0 d = (newGame (GameConfig 5 (GoalNamed "crate" 1)) 1) {gsBoard = crateBoard d}
      (gsA, oA, _) = resolveSwapWith reg (1, 2) (2, 2) (gs0 2)
  assertBool "durability 2: chipped, not counted, not over" (moveApplied oA && gsCollected gsA == 0 && gsOver gsA == Nothing)
  let (gsB, oB, _) = resolveSwapWith reg (1, 2) (2, 2) (gs0 1)
  assertBool "durability 1: broken, goal reached" (moveApplied oB && isWin (gsOver gsB))
  assertEqual "collected by name" 1 (gsCollected gsB)
  assertEqual "named counter" [("crate", 1)] (gsElementCounts gsB)
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
  toCell (Moss n) = Custom "moss" n
  groundRule _ = Just (\n -> if n > 1 then Just (n - 1) else Nothing)
  counter _ = Just (CountNamed "moss")

mossDef :: Entry
mossDef = groundEntry (Moss 2)

ext_ground_layer_test_element :: Assertion
ext_ground_layer_test_element = do
  let reg = register mossDef defaultRegistry
      ground0 = [((1, 1), ("moss", 2)), ((6, 6), ("moss", 1))]
      gs0 = (newGame (GameConfig 5 (GoalNamed "moss" 3)) 1) {gsBoard = tripleBoard, gsGround = ground0}
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
      hitsAt p = length [() | w <- mtWaves mt1, p `elem` (cwCleared w ++ cwDrained w)]
  assertBool "move applied" (moveApplied o1)
  assertEqual "(1,1) cleared exactly once this move" 1 (hitsAt (1, 1))
  assertEqual "(6,6) untouched this move" 0 (hitsAt (6, 6))
  assertEqual "one layer peeled, other tile untouched" [((1, 1), ("moss", 1)), ((6, 6), ("moss", 1))] (gsGround gs1)
  assertEqual "counted per layer" [("moss", 1)] (gsElementCounts gs1)
  assertEqual "goal progress by name" 1 (gsCollected gs1)
  assertEqual "ground does not occupy the board" (gsBoard (fst (trySwap p1 p2 gs0 {gsGround = []}))) (gsBoard gs1)
  -- 第二次消除同一格：清掉，累计 2
  let gs1' = gs1 {gsBoard = tripleBoard}
      (gs2, _, _) = resolveSwapWith reg p1 p2 gs1'
  assertEqual "second clear removes the tile" [((6, 6), ("moss", 1))] (gsGround gs2)
  assertEqual "count accumulates by layer" [("moss", 2)] (gsElementCounts gs2)
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
  toCell (Kite k) = Custom "kite" k
  archetype _ = Blocker
  drains _ = [EdgeLeft]
  counter _ = Just (CountNamed "kite")

kiteDef :: Entry
kiteDef = customEntry (Kite 1) Kite

ext_edge_drain_side_collectible :: Assertion
ext_edge_drain_side_collectible = do
  let reg = register kiteDef defaultRegistry
      board0 = foldl (\b p -> setCell b p (Custom "kite" 1)) tripleBoard [(4, 0), (4, 3), (7, 5)]
      gs0 = (newGame (GameConfig 5 (GoalNamed "kite" 1)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
  assertBool "move applied" (moveApplied o1)
  assertBool "left-edge kite drained in the first settle" ((4, 0) `elem` cwDrained (head (mtWaves mt1)))
  assertEqual "inner and bottom-edge kites stay" [(4, 3), (7, 5)] (customsOn "kite" (gsBoard gs1))
  assertEqual "counted by name" [("kite", 1)] (gsElementCounts gs1)
  assertBool "goal reached" (isWin (gsOver gs1))
  assertEqual "cookie counter untouched" 0 (gsCookiesCollected gs1)
  let (gsD, _, _) = resolveSwapWith defaultRegistry p1 p2 gs0
  assertEqual "unregistered: nothing drained" [(4, 0), (4, 3), (7, 5)] (customsOn "kite" (gsBoard gsD))

-- | 测试专用「陷坑」（固定格）：步末规则（PhaseMove）不改盘，只经 erHoles 声明自己所在格为空洞 →
-- 步末补结算把它挖空、上方下落、补子、成消再连锁；终盘稳定、没有陷坑，回放里多一个只有沉降的轮次。
-- 补结算是统一路径（内置元素步末从不留下空洞，见 docs/testing.md 的扫描），没有开关。
newtype Sinkhole = Sinkhole Int
  deriving (Eq, Show)

instance Element Sinkhole where
  name _ = "sinkhole"
  toCell (Sinkhole k) = Custom "sinkhole" k
  archetype _ = Fixed
  endRule _ = Just (EndRule PhaseMove 90 (\_ b -> (Nothing, b)) (const []) (customsOn "sinkhole"))

sinkholeDef :: Entry
sinkholeDef = customEntry (Sinkhole 1) Sinkhole

ext_post_end_settle_hole_element :: Assertion
ext_post_end_settle_hole_element = do
  let reg = register sinkholeDef defaultRegistry
      board0 = setCell tripleBoard (5, 5) (Custom "sinkhole" 1)
      gs0 = (newGame (GameConfig 5 (GoalScore 99999)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
      settleWaves = [w | w <- mtWaves mt1, null (cwCleared w), (cwHoles w !! 5) !! 5 == Nothing]
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
  toCell (Dust k) = Custom "dust" k
  archetype _ = Blocker
  keepOnShuffle _ = False

dustDef :: Entry
dustDef = customEntry (Dust 1) Dust

ext_manual_shuffle_keeps_crate_via_engine :: Assertion
ext_manual_shuffle_keeps_crate_via_engine = do
  let reg = register dustDef (register crateDef defaultRegistry)
      gs0 = (newGame defaultConfig 1) {gsBoard = setCell (crateBoard 2) (5, 5) (Custom "dust" 1)}
      st = gameStep (M3E.match3GameWith reg) gs0 M3E.Shuffle
      b1 = gsBoard (stepState st)
  assertBool "shuffle accepted" (stepAccepted st)
  assertBool "board reshuffled" (b1 /= gsBoard gs0 && gsShuffled (stepState st))
  assertEqual "crate kept in place" [((0, 1), Custom "crate" 2)] (cratesOn b1)
  assertEqual "dust washed away under the custom registry" [] (customsOn "dust" b1)
  let stD = gameStep M3E.match3Game gs0 M3E.Shuffle
  assertEqual "built-in registry keeps unknown dust (inert default)" [(5, 5)] (customsOn "dust" (gsBoard (stepState stD)))
