{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Haskell 特性第 9 项：规则去重（docs/haskell-features/09-规则去重.md）。
--
-- * 光学定律：五个占格障碍棱镜的往返律、改色遍历 cellColorT 的遍历定律；
-- * 占格障碍：相邻查询（adjacentWhere）与邻消削层（ecs-3 起是原型的邻格 system，nearBy 列 + 反应函数）的结果顺序写成固定例子；
-- * 规则折叠：runEndStage = 逐个 system 跑再丢掉空效果；
-- * （能力声明 Cap 的幺半群在元素类重构第 2 刀随 Caps 一起删除，原型包的缺省方法见 Spec.Archetype）；
-- * 阶段智能构造器 tickRule / spreadRule / moveRule；
-- * 魔法石充能：方法 + 通用驱动与留在这里的旧整盘写法逐盘等价（命名清理时局部化）。
--
-- 固定例子的期望值由现实现生成，生成时与删除前的逐字旧副本核对过。
module Spec.RulesDedup
  ( tests
  ) where

import Data.Functor.Identity (Identity(..))
import Engine.Optics
import Match3.Core (Board, Cell, CellContents(..), Color(..), GemKind(..), Pos, boardFromRows, getCell)
import Match3.Types (boardPositions)
import Match3.Board.Grid (inBounds)
import Match3.Element (defaultRegistry)
import Match3.Element.Builtin.Obstacle (magicStoneCharge, magicStoneFull)
import Match3.ECS.Registry (endSystems, pushableWith)
import Match3.ECS.Stage
import Match3.ECS.System (System(..))
import qualified Match3.Obstacles as New
import qualified Match3.Types as NewB
import Match3.Types.Optics (cellColorT, _Cake, _Chest, _Honey, _Safe, _Stone)
import Spec.Properties (genCell, genColor, genGem)
import Spec.Support.Obstacles (chipAdjacentChestsExcept, chipAdjacentHoneyExcept, chipAdjacentSafesExcept)
import Spec.Support.Arbitrary (shrinkBoard)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testProperty "qc_dedup_cell_optics_laws" qc_dedup_cell_optics_laws
  , testCase "dedup_obstacle_orders_pinned" dedup_obstacle_orders_pinned
  , testProperty "qc_run_end_rules_is_fold" (withMaxSuccess 300 qc_run_end_rules_is_fold)
  , testCase "end_rule_smart_constructors" end_rule_smart_constructors
  , testProperty "qc_magic_stone_charge_via_driver" (withMaxSuccess 500 qc_magic_stone_charge_via_driver)
  ]

--------------------------------------------------------------------------------
-- 生成器

-- | 障碍多的格：五种带层数障碍（层数 1–3）、魔法帽 / 彩蛋 / 染色瓶 / 时间精灵 / 果汁机 / 气球，混在宝石与任意格里。
genObstacleCell :: Gen Cell
genObstacleCell =
  frequency
    [ (4, genGem)
    , (2, genCell)
    , (2, Stone <$> choose (1, 3))
    , (2, Chest <$> choose (1, 3))
    , (2, Honey <$> choose (1, 3))
    , (2, Cake <$> choose (1, 3))
    , (2, Safe <$> choose (1, 3))
    , (1, pure MagicHat)
    , (1, pure Surprise)
    , (1, Bottle <$> genColor)
    , (1, pure TimeSpirit)
    , (1, Maker <$> genColor <*> choose (1, 3))
    , (1, Balloon <$> genColor)
    , (1, Countdown <$> genColor <*> choose (1, 3))
    , (1, Flip <$> genColor <*> genColor)
    ]

-- | 盘内的若干格（可重复，测 nub 的顺序）。
genSomePos :: Board -> Gen [Pos]
genSomePos b = listOf (elements (boardPositions b))

-- | 带层数的格（层数也可以是 0 / 负数：棱镜原样构造，看 mkXLayers 的 max 1 与读数）。
genLayeredCell :: Gen Cell
genLayeredCell = do
  n <- choose (-1, 4)
  oneof [genCell, elements [Stone n, Chest n, Honey n, Cake n, Safe n]]

--------------------------------------------------------------------------------
-- 光学定律

-- | 五个障碍棱镜的两条往返律；cellColorT 的遍历定律（恒等、over 合成、set-set、读到的就是 cellColor）。
qc_dedup_cell_optics_laws :: Property
qc_dedup_cell_optics_laws =
  forAll genLayeredCell $ \cell -> forAll (choose (-2, 5)) $ \n -> forAll genColor $ \c1 -> forAll genColor $ \c2 ->
    conjoin
      [ prismLaws "_Stone" _Stone cell n
      , prismLaws "_Chest" _Chest cell n
      , prismLaws "_Honey" _Honey cell n
      , prismLaws "_Cake" _Cake cell n
      , prismLaws "_Safe" _Safe cell n
      , counterexample "traversal identity" (runIdentity (cellColorT Identity cell) === cell)
      , counterexample "over compose" (over cellColorT bump (over cellColorT (const c1) cell) === over cellColorT (bump . const c1) cell)
      , counterexample "set-set" (set cellColorT c2 (set cellColorT c1 cell) === set cellColorT c2 cell)
      , counterexample "preview = cellColor" (preview cellColorT cell === NewB.cellColor cell)
      , counterexample "set then preview" (preview cellColorT (set cellColorT c1 cell) === (c1 <$ preview cellColorT cell))
      ]
  where
    bump c = if c == C1 then C2 else C1

prismLaws :: String -> Prism' Cell Int -> Cell -> Int -> Property
prismLaws nm p cell n =
  counterexample nm $
    conjoin
      [ counterexample "preview p (review p n) == Just n" (preview p (review p n) === Just n)
      , counterexample "preview p s == Just a  ==>  review p a == s" (maybe (property True) (\a -> review p a === cell) (preview p cell))
      ]

--------------------------------------------------------------------------------
-- 占格障碍：顺序的固定例子

-- | 无匹配意义的宝石底（只为填满盘面），extra 覆盖指定格。
obstacleBoard :: Int -> Int -> [(Pos, Cell)] -> Board
obstacleBoard rows cols extra =
  boardFromRows [[maybe (Gem (toEnum ((r + 2 * c) `mod` 5)) Normal 0 Nothing) id (lookup (r, c) extra) | c <- [0 .. cols - 1]] | r <- [0 .. rows - 1]]

-- | 相邻查询与邻消削层的结果顺序写死（金标准依赖这些顺序）。
--
-- * 蜂蜜：清除 (3,2)，上 (2,2) 两层、下 (4,2) 与右 (3,3) 各一层——'adjacentWhere' 按上 / 下 / 左 / 右给出
--   @[(2,2),(4,2),(3,3)]@；末层位置前插，得到 @[(3,3),(4,2)]@（docs/haskell-features/07 §3.4 的反例盘）。
-- * 宝箱：4×6 盘只有 (2,4)、(3,5) 两格 @Chest 1@，清除 (2,5)：末层位置 @[(2,4),(3,5)]@；
--   except 里有 (3,5) 时只削 (2,4)（docs/haskell-features/09 §3.5 的反例盘）。
-- * 保险箱：末层原地变饼干，两层的减一层。
dedup_obstacle_orders_pinned :: Assertion
dedup_obstacle_orders_pinned = do
  let honeyB = obstacleBoard 6 6 [((2, 2), Honey 2), ((4, 2), Honey 1), ((3, 3), Honey 1)]
      (honeyB', honeyDead) = chipAdjacentHoneyExcept honeyB [(3, 2)] []
  assertEqual "adjacentWhere (has _Honey)" [(2, 2), (4, 2), (3, 3)] (New.adjacentWhere (has _Honey) honeyB [(3, 2)])
  assertEqual "chipAdjacentHoneyExcept: last layers" [(3, 3), (4, 2)] honeyDead
  assertEqual "chipAdjacentHoneyExcept: cells" [Honey 1, Honey 1, Honey 1] (map (getCell honeyB') [(2, 2), (4, 2), (3, 3)])
  let chestB = obstacleBoard 4 6 [((2, 4), Chest 1), ((3, 5), Chest 1)]
  assertEqual "chipAdjacentChestsExcept" [(2, 4), (3, 5)] (snd (chipAdjacentChestsExcept chestB [(2, 5)] []))
  assertEqual "chipAdjacentChestsExcept (except)" [(2, 4)] (snd (chipAdjacentChestsExcept chestB [(2, 5)] [(3, 5)]))
  let safeB = obstacleBoard 3 3 [((0, 1), Safe 1), ((1, 0), Safe 2)]
      (safeB', safeDead) = chipAdjacentSafesExcept safeB [(0, 0)] []
  assertEqual "chipAdjacentSafesExcept: last layers" [(0, 1)] safeDead
  assertEqual "chipAdjacentSafesExcept: cells" [Cookie, Safe 1] (map (getCell safeB') [(0, 1), (1, 0)])

--------------------------------------------------------------------------------
-- 规则折叠

-- | runEndStage 跑任意一串真实步末 system（三个阶段任取、任意顺序、可重复）= 逐个 runSystem 再丢掉空效果
-- （记录按规则顺序，盘面从一条规则穿到下一条）。
qc_run_end_rules_is_fold :: Property
qc_run_end_rules_is_fold =
  forAllShrink genRuleBoard shrinkBoard $ \b ->
    forAll (genSomePos b) $ \avoid -> forAll (genSomePos b) $ \walls ->
      forAll (listOf (choose (0, length allRules - 1))) $ \ixs ->
        let world = defaultRegistry
            picked = map (allRules !!) ixs  -- 按下标挑（EndSys 没有 Show）
            ctx = endWorld avoid walls (pushableWith world) b
            (recs, bEnd) = runEndStage ctx picked
            step bd r = let w = runSystem (esSystem r) ctx {ewBoard = bd} in (ewEffect w, ewBoard w)
            naive = foldl (\(acc, bd) r -> let (e, bd') = step bd r in (acc ++ [(bd, bd', x) | Just x <- [e]], bd')) ([], b) picked
        in classify (not (null recs)) "runEndStage recorded" ((recs, bEnd) === naive)
  where
    allRules = concat [endSystems defaultRegistry ph | ph <- [PhaseTick, PhaseSpread, PhaseMove]]
    -- 步末规则要有东西可做：蜗牛 / 倒计时 / 藤 / 巧克力 / 蒸汽 / 毛球等都在 genCell 里
    genRuleBoard = do
      r <- choose (2, 8)
      c <- choose (2, 8)
      boardFromRows <$> vectorOf r (vectorOf c (frequency [(3, genCell), (1, genObstacleCell), (1, Countdown <$> genColor <*> choose (1, 2))]))

--------------------------------------------------------------------------------
-- 阶段智能构造器

-- | tickSys / spreadSys / moveSys：阶段、次序、system 原样；只有 tickSys 带种子；空洞恒为 []。
-- 内置元素世界里的步末 system（全部用智能构造器）在各阶段的 esHoles 都是 []，PhaseSpread / PhaseMove 的 esSeeds 也是 []。
end_rule_smart_constructors :: Assertion
end_rule_smart_constructors = do
  let b = boardFromRows (replicate 4 (replicate 4 (Stone 1)))
      run = mempty
      t = tickSys 7 run (const [(1, 1)])
      sp = spreadSys 8 run
      mv = moveSys 9 run
  assertEqual "phases" [PhaseTick, PhaseSpread, PhaseMove] (map esPhase [t, sp, mv])
  assertEqual "orders" [7, 8, 9] (map esOrder [t, sp, mv])
  assertEqual "seeds" [[(1, 1)], [], []] (map (`esSeeds` b) [t, sp, mv])
  assertEqual "holes" [[], [], []] (map (`esHoles` b) [t, sp, mv])
  assertEqual "run passes through" [b, b, b] [ewBoard (runSystem (esSystem r) (endWorld [] [] (const True) b)) | r <- [t, sp, mv]]
  let world = defaultRegistry
      phases = [PhaseTick, PhaseSpread, PhaseMove]
  assertEqual "builtin end rules (phase, order)" [(PhaseTick, [10, 20]), (PhaseSpread, [10, 20, 30]), (PhaseMove, [10, 20, 30, 40])] [(ph, map esOrder (endSystems world ph)) | ph <- phases]
  assertEqual "builtin holes all empty" [] [esOrder r | ph <- phases, r <- endSystems world ph, not (null (esHoles r b))]
  assertEqual "only tick rules seed" [] [esOrder r | ph <- [PhaseSpread, PhaseMove], r <- endSystems world ph, not (null (esSeeds r b))]

--------------------------------------------------------------------------------
-- 魔法石充能：局部化前后等价

-- | 局部化之前的整盘写法（Element.Builtin.Obstacle 的 magicStoneCharge，逐字留作参照）：盘上每块魔法石，
-- 未满、不在直接命中格、正交邻格里有真消除格的充能 1 格。
magicStoneChargeReference :: NearWorld -> NearWorld
magicStoneChargeReference ctx =
  let b = nwBoard ctx
      near p = any (`elem` nwTrue ctx) (filter (inBounds b) (New.orthoNeighbors p))
      charged = [(p, Custom "magic_stone" (NewB.CustomState (k + 1))) | (p, k) <- stones, k < magicStoneFull, p `notElem` nwDirect ctx, near p]
      stones = NewB.ifoldMap (\p cell -> [(p, k) | Custom "magic_stone" (NewB.CustomState k) <- [cell]]) b
  in ctx {nwBoard = foldl (\bd (p, cell) -> NewB.boardSet bd p cell) b charged}

-- | 魔法石（状态 -1–5，含满格 3 与发射中 4）混在宝石 / 任意格里的随机盘，随机真消除格与直接命中格（可重复）：
-- 邻格 system（nearBy）与旧整盘写法给出相同的盘面，都不打碎、不坐住格。
qc_magic_stone_charge_via_driver :: Property
qc_magic_stone_charge_via_driver =
  forAll genStoneBoard $ \b -> forAll (genSomePos b) $ \trues -> forAll (genSomePos b) $ \direct ->
    let ctx = nearWorld (const True) trues direct [] b
        new = runSystem magicStoneCharge ctx
        old = magicStoneChargeReference ctx
    in counterexample (show (trues, direct))
         (nwBoard new == nwBoard old .&&. nwDead new === nwDead old .&&. nwSit new === nwSit old)
  where
    genStoneBoard = do
      rows <- choose (1, 7)
      cols <- choose (1, 7)
      cells <- vectorOf rows (vectorOf cols (frequency [(3, stone), (3, genGem), (1, genCell)]))
      pure (boardFromRows cells)
    stone = (\k -> Custom "magic_stone" (NewB.CustomState k)) <$> choose (-1, 5)

