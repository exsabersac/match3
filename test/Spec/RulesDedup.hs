{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Haskell 特性第 9 项：规则去重（docs/haskell-features/09-规则去重.md）。
--
-- * 光学定律：五个占格障碍棱镜的往返律、改色遍历 cellColorT 的遍历定律；
-- * 占格障碍：相邻查询（adjacentWhere）与邻消削层（chipAdjacentLayeredExcept）的结果顺序写成固定例子；
-- * 规则折叠：runEndRules = 逐条 erRun 再丢掉空效果；
-- * 能力声明：Cap 是带 Dual (Endo Caps) 幺半群的 newtype——一组固定例子的能力记录写死（拼接方向），
--   幺半群律与「后面的覆盖前面的」用 QuickCheck 查；
-- * 阶段智能构造器 tickRule / spreadRule / moveRule。
--
-- 固定例子的期望值由现实现生成，生成时与删除前的逐字旧副本核对过。
module Spec.RulesDedup
  ( tests
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Maybe (isJust)
import Engine.Optics
import Match3.Core (Board, Cell, CellContents(..), Color(..), GemKind(..), Pos, boardFromRows, boardPositions, getCell)
import Match3.Element (defaultRegistry)
import Match3.Element.Caps (Cap(..), applyCap, setCap)
import qualified Match3.Element.Caps as N
import Match3.Element.Class
import Match3.Element.Registry (endRules, pushableWith)
import Match3.Element.Types
import qualified Match3.Obstacles as New
import qualified Match3.Types as NewB
import Match3.Types.Optics (cellColorT, _Cake, _Chest, _Honey, _Safe, _Stone)
import Spec.Properties (genCell, genColor, genGem)
import Spec.Support.Arbitrary (shrinkBoard)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Fixed)

tests :: [TestTree]
tests =
  [ testProperty "qc_dedup_cell_optics_laws" qc_dedup_cell_optics_laws
  , testCase "dedup_obstacle_orders_pinned" dedup_obstacle_orders_pinned
  , testProperty "qc_run_end_rules_is_fold" (withMaxSuccess 300 qc_run_end_rules_is_fold)
  , testCase "dedup_caps_examples_pinned" dedup_caps_examples_pinned
  , testProperty "qc_caps_monoid_laws" (withMaxSuccess 500 qc_caps_monoid_laws)
  , testCase "end_rule_smart_constructors" end_rule_smart_constructors
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
      (honeyB', honeyDead) = New.chipAdjacentHoneyExcept honeyB [(3, 2)] []
  assertEqual "adjacentWhere (has _Honey)" [(2, 2), (4, 2), (3, 3)] (New.adjacentWhere (has _Honey) honeyB [(3, 2)])
  assertEqual "chipAdjacentHoneyExcept: last layers" [(3, 3), (4, 2)] honeyDead
  assertEqual "chipAdjacentHoneyExcept: cells" [Honey 1, Honey 1, Honey 1] (map (getCell honeyB') [(2, 2), (4, 2), (3, 3)])
  let chestB = obstacleBoard 4 6 [((2, 4), Chest 1), ((3, 5), Chest 1)]
  assertEqual "chipAdjacentChestsExcept" [(2, 4), (3, 5)] (snd (New.chipAdjacentChestsExcept chestB [(2, 5)] []))
  assertEqual "chipAdjacentChestsExcept (except)" [(2, 4)] (snd (New.chipAdjacentChestsExcept chestB [(2, 5)] [(3, 5)]))
  let safeB = obstacleBoard 3 3 [((0, 1), Safe 1), ((1, 0), Safe 2)]
      (safeB', safeDead) = New.chipAdjacentSafesExcept safeB [(0, 0)] []
  assertEqual "chipAdjacentSafesExcept: last layers" [(0, 1)] safeDead
  assertEqual "chipAdjacentSafesExcept: cells" [Cookie, Safe 1] (map (getCell safeB') [(0, 1), (1, 0)])

--------------------------------------------------------------------------------
-- 规则折叠

-- | runEndRules 跑任意一串真实步末规则（三个阶段的规则任取、任意顺序、可重复）= 逐条 erRun 再丢掉空效果
-- （记录按规则顺序，盘面从一条规则穿到下一条）。
qc_run_end_rules_is_fold :: Property
qc_run_end_rules_is_fold =
  forAllShrink genRuleBoard shrinkBoard $ \b ->
    forAll (genSomePos b) $ \avoid -> forAll (genSomePos b) $ \walls ->
      forAll (listOf (choose (0, length allRules - 1))) $ \ixs ->
        let reg = defaultRegistry
            picked = map (allRules !!) ixs  -- 按下标挑（EndRule 没有 Show）
            ctx = EndCtx avoid walls (pushableWith reg)
            (recs, bEnd) = runEndRules ctx picked b
            naive = foldl (\(acc, bd) r -> let (e, bd') = erRun r ctx bd in (acc ++ [(bd, bd', x) | Just x <- [e]], bd')) ([], b) picked
        in classify (not (null recs)) "runEndRules recorded" ((recs, bEnd) === naive)
  where
    allRules = concat [endRules defaultRegistry ph | ph <- [PhaseTick, PhaseSpread, PhaseMove]]
    -- 步末规则要有东西可做：蜗牛 / 倒计时 / 藤 / 巧克力 / 蒸汽 / 毛球等都在 genCell 里
    genRuleBoard = do
      r <- choose (2, 8)
      c <- choose (2, 8)
      boardFromRows <$> vectorOf r (vectorOf c (frequency [(3, genCell), (1, genObstacleCell), (1, Countdown <$> genColor <*> choose (1, 2))]))

--------------------------------------------------------------------------------
-- 能力声明

-- | 能力记录的可观察部分（函数字段在探针上取值）。
capsSig :: Caps -> [String]
capsSig c =
  [ show (capArchetype c)
  , show (map (mcColor m) probes), show (mcBlocksMatch m), show (mcBlocksSwap m), show (mcHintable m), show (fmap srOrder (mcSwapRule m))
  , show (hcActivates h), show (hcOnHit h), show (fmap (\f -> f probeBoard (1, 1)) (hcBlast h)), show (hcStrip h)
  , show (fmap arOrder (hcAdjacent h)), show (isJust (hcOpen h))
  , show (mvFalls v), show (mvPortal v), show (mvDrains v), show (mvKeepShuffle v), show (mvRecolorable v), show (mvPushable v)
  , show (ccCounter n), show (ccDiffCounter n), show (ccDiffWeight n), show (ccBonusMoves n), show (ccVacatesCarpet n)
  , show (fmap (\r -> (erPhase r, erOrder r, erHoles r probeBoard, erSeeds r probeBoard)) (stEnd s))
  , show (fmap (\g -> map g [0 .. 3]) (stGround s)), show (fmap (\f -> f probeBoard [(0, 0)]) (stWiden s))
  , show (stMessage s (SomeMessage Probe))
  ]
  where
    m = capMatch c
    h = capHit c
    v = capMove c
    n = capCount c
    s = capStep c
    probes = [Gem C2 Normal 0 Nothing, Stone 1, Countdown C3 2]
    probeBoard = boardFromRows (replicate 3 (replicate 3 (Gem C1 Normal 0 Nothing)))

-- | 探针消息（capsSig 用它看 stMessage）。
data Probe = Probe

instance Message Probe

-- | 每个能力声明，参数各取一个能看出差别的值。
capList :: [(String, Cap)]
capList =
  [ ("colorIs", N.colorIs C4), ("colorless", N.colorless)
  , ("swappable", N.swappable), ("notHintable", N.notHintable)
  , ("onSwap", N.onSwap swapR)
  , ("hit Destroy", N.hit Destroy), ("breaks", N.breaks), ("noFire", N.noFire)
  , ("explodes", N.explodes (\_ p -> [p]))
  , ("onAdjacent", N.onAdjacent 77 (\_ bd -> AdjOut bd [] []))
  , ("opens", N.opens (\bd _ -> (bd, [], [])))
  , ("teleports", N.teleports), ("drainsAt", N.drainsAt [EdgeBottom, EdgeLeft])
  , ("keepsOnShuffle", N.keepsOnShuffle), ("recolors", N.recolors), ("pushes", N.pushes)
  , ("reshuffles", N.reshuffles), ("noRecolor", N.noRecolor), ("noPush", N.noPush)
  , ("counts", N.counts CountStones), ("countsDiff", N.countsDiff CountCookies)
  , ("weighs", N.weighs 5), ("bonus", N.bonus 2), ("vacates", N.vacates)
  , ("atEnd", N.atEnd (moveRule 33 (\_ bd -> (Nothing, bd))))
  , ("ground", N.ground (\x -> if x > 1 then Just (x - 1) else Nothing))
  , ("widens", N.widens (\_ ps -> ps ++ ps))
  , ("onMessage", N.onMessage answer)
  , ("withMatch", N.withMatch (\x -> x {mcBlocksMatch = True}))
  , ("withHit", N.withHit (\x -> x {hcStrip = True}))
  , ("withMove", N.withMove (\x -> x {mvFalls = False}))
  , ("withCount", N.withCount (\x -> x {ccBonusMoves = ccBonusMoves x + 3}))
  , ("withStep", N.withStep (\x -> x {stGround = Nothing}))
  , ("set blocksSwap True", setCap (N.matchL . N.mcBlocksSwapL) True)
  , ("set weight 0", setCap (N.countL . N.ccDiffWeightL) 0)
  ]
  where
    swapR = SwapRule 42 (\_ _ _ -> True) (\_ p _ -> [p])
    -- 只回复探针：回复的元素值（Show）能看出装进的是不是这个函数
    answer msg = case fromMessage msg of
      Just Probe -> Just (SomeElement (Inert "probe" (Stone 2)))
      Nothing -> Nothing

-- | 按名字取声明（只用于下面的固定例子）。
capNamed :: String -> Cap
capNamed nm = maybe (error ("capNamed: " ++ nm)) id (lookup nm capList)

-- | 固定例子：(说明, 原型, 一串声明的名字)。
capExamples :: [(String, Archetype, [String])]
capExamples =
  [ ("piece 缺省", Piece, [])
  , ("blocker 缺省", Blocker, [])
  , ("fixed 缺省", Fixed, [])
  , ("blocker：keepsOnShuffle 与 reshuffles 冲突（09 §3.5 的反例）", Blocker, ["colorless", "notHintable", "keepsOnShuffle", "atEnd", "reshuffles", "vacates"])
  , ("piece：冲突与累加", Piece, ["swappable", "set blocksSwap True", "weighs", "set weight 0", "withCount", "withCount", "noPush", "pushes", "colorIs", "colorless"])
  , ("fixed：全部声明", Fixed, map fst capList)
  , ("piece：全部声明（倒序）", Piece, reverse (map fst capList))
  ]

-- | 固定例子的能力记录（capsSig）写死：withCaps 按声明顺序拼接、后面的覆盖前面的。
-- 'Cap' 的实例写成 @via Endo Caps@（去掉 Dual，拼接方向反过来）时第 4 个例子就不同：keepsOnShuffle 胜出。
dedup_caps_examples_pinned :: Assertion
dedup_caps_examples_pinned = do
  assertEqual "example count" (length capExamples) (length pinnedCapsSigs)
  sequence_
    [ sequence_ [assertEqual (nm ++ " / field " ++ show i) e a | (i, e, a) <- zip3 [0 :: Int ..] expected actual]
        >> assertEqual (nm ++ " / length") (length expected) (length actual)
    | ((nm, a0, names), expected) <- zip capExamples pinnedCapsSigs
    , let actual = capsSig (N.withCaps a0 (map capNamed names))
    ]

-- | capExamples 各例的 capsSig（由现实现生成，生成时与删除前的逐字旧副本核对过）。
pinnedCapsSigs :: [[String]]
pinnedCapsSigs =
  [ ["Piece","[Just C2,Nothing,Nothing]","False","False","True","Nothing","Just True","Destroy","Nothing","False","Nothing","False","True","True","[]","False","True","True","Nothing","Nothing","1","0","False","Nothing","Nothing","Nothing","Nothing"]
  , ["Blocker","[Just C2,Nothing,Nothing]","False","True","True","Nothing","Just False","Immune","Nothing","False","Nothing","False","True","False","[]","True","False","False","Nothing","Nothing","1","0","False","Nothing","Nothing","Nothing","Nothing"]
  , ["Fixed","[Just C2,Nothing,Nothing]","False","True","True","Nothing","Just False","Immune","Nothing","False","Nothing","False","False","False","[]","True","False","False","Nothing","Nothing","1","0","False","Nothing","Nothing","Nothing","Nothing"]
  , ["Blocker","[Nothing,Nothing,Nothing]","False","True","False","Nothing","Just False","Immune","Nothing","False","Nothing","False","True","False","[]","False","False","False","Nothing","Nothing","1","0","True","Just (PhaseMove,33,[],[])","Nothing","Nothing","Nothing"]
  , ["Piece","[Nothing,Nothing,Nothing]","False","True","True","Nothing","Just True","Destroy","Nothing","False","Nothing","False","True","True","[]","False","True","True","Nothing","Nothing","0","6","False","Nothing","Nothing","Nothing","Nothing"]
  , ["Fixed","[Nothing,Nothing,Nothing]","True","True","False","Just 42","Just False","Destroy","Just [(1,1)]","True","Just 77","True","False","True","[EdgeBottom,EdgeLeft]","False","False","False","Just CountStones","Just CountCookies","0","5","True","Just (PhaseMove,33,[],[])","Nothing","Just [(0,0),(0,0)]","Just (SomeElement \"probe\" (Inert \"probe\" (Stone 2)))"]
  , ["Piece","[Just C4,Just C4,Just C4]","True","False","False","Just 42","Just False","Destroy","Just [(1,1)]","True","Just 77","True","False","True","[EdgeBottom,EdgeLeft]","True","True","True","Just CountStones","Just CountCookies","5","2","True","Just (PhaseMove,33,[],[])","Just [Nothing,Nothing,Just 1,Just 2]","Just [(0,0),(0,0)]","Just (SomeElement \"probe\" (Inert \"probe\" (Stone 2)))"]
  ]

-- | 任意原型 × 任意一串声明（可重复、互相冲突，如 swappable 与「挡交换」、weighs 5 与 weight 0、累加的 withCount）：
-- withCaps = applyCap (mconcat 声明) (capsOf 原型)，piece / blocker / fixed = 对应原型的 withCaps；
-- 幺半群律（结合、单位元）在观察上成立；「后面的覆盖前面的」：applyCap (a <> b) = applyCap b . applyCap a。
qc_caps_monoid_laws :: Property
qc_caps_monoid_laws =
  forAll (elements [Piece, Blocker, Fixed]) $ \a ->
    forAll (listOf (choose (0, length capList - 1))) $ \ixs ->
      forAll (choose (0, length capList - 1)) $ \i -> forAll (choose (0, length capList - 1)) $ \j ->
        let ncs = map (snd . (capList !!)) ixs
            ci = snd (capList !! i)
            cj = snd (capList !! j)
            base = capsOf a
            sig = capsSig
        in counterexample (show (a, map (fst . (capList !!)) ixs)) $
             conjoin
               [ counterexample "withCaps" (sig (N.withCaps a ncs) === sig (applyCap (mconcat ncs) base))
               , counterexample "piece / blocker / fixed" (map sig [N.piece ncs, N.blocker ncs, N.fixed ncs] === map sig [N.withCaps Piece ncs, N.withCaps Blocker ncs, N.withCaps Fixed ncs])
               , counterexample "later overrides earlier" (sig (applyCap (ci <> cj) base) === sig (applyCap cj (applyCap ci base)))
               , counterexample "associativity" (sig (applyCap ((ci <> cj) <> mconcat ncs) base) === sig (applyCap (ci <> (cj <> mconcat ncs)) base))
               , counterexample "identity" (map sig [applyCap (mempty <> ci) base, applyCap (ci <> mempty) base] === map sig [applyCap ci base, applyCap ci base])
               ]

--------------------------------------------------------------------------------
-- 阶段智能构造器

-- | tickRule / spreadRule / moveRule：阶段、次序、执行函数原样；只有 tickRule 带种子；空洞恒为 []。
-- 内置注册表里的步末规则（全部改用智能构造器）在各阶段的 erHoles 都是 []，PhaseSpread / PhaseMove 的 erSeeds 也是 []。
end_rule_smart_constructors :: Assertion
end_rule_smart_constructors = do
  let b = boardFromRows (replicate 4 (replicate 4 (Stone 1)))
      run _ bd = (Nothing, bd)
      t = tickRule 7 run (const [(1, 1)])
      sp = spreadRule 8 run
      mv = moveRule 9 run
  assertEqual "phases" [PhaseTick, PhaseSpread, PhaseMove] (map erPhase [t, sp, mv])
  assertEqual "orders" [7, 8, 9] (map erOrder [t, sp, mv])
  assertEqual "seeds" [[(1, 1)], [], []] (map (`erSeeds` b) [t, sp, mv])
  assertEqual "holes" [[], [], []] (map (`erHoles` b) [t, sp, mv])
  assertEqual "run passes through" [b, b, b] [snd (erRun r (EndCtx [] [] (const True)) b) | r <- [t, sp, mv]]
  let reg = defaultRegistry
      phases = [PhaseTick, PhaseSpread, PhaseMove]
  assertEqual "builtin end rules (phase, order)" [(PhaseTick, [10, 20]), (PhaseSpread, [10, 20, 30]), (PhaseMove, [10, 20, 30, 40])] [(ph, map erOrder (endRules reg ph)) | ph <- phases]
  assertEqual "builtin holes all empty" [] [erOrder r | ph <- phases, r <- endRules reg ph, not (null (erHoles r b))]
  assertEqual "only tick rules seed" [] [erOrder r | ph <- [PhaseSpread, PhaseMove], r <- endRules reg ph, not (null (erSeeds r b))]
