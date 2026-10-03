{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Haskell 特性第 9 项：规则去重（docs/haskell-features/09-规则去重.md）。
--
-- 每处去重都和改写前的逐字副本对照（Spec.Support.LegacyObstacles / Spec.Support.LegacyRules）：
--
-- * 占格障碍：五种带层数障碍的邻消揭层 → 一个 chipAdjacentLayeredExcept（棱镜参数），九份「相邻的某种格」→ adjacentWhere，
--   改色 recolorCell → 遍历 cellColorT；Match3.Types.Body 的五组构造 / 读数 / 谓词 → 棱镜派生；
-- * 规则折叠：步末规则的 foldl + reverse（蔓延 / 会走的元素）、邻格波及的四元组 foldl → runEndRules / mapAccumL；
-- * 能力声明：Cap 是带 Dual (Endo Caps) 幺半群的 newtype、字段写入器经透镜——与旧的 Caps -> Caps 与 foldl 逐项相同；
-- * 阶段智能构造器 tickRule / spreadRule / moveRule。
module Spec.RulesDedup
  ( tests
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Maybe (isJust)
import Engine.Optics
import Match3.Core (Board, Cell, CellContents(..), Color(..), GemKind(..), Pos, boardFromRows, boardPositions)
import Match3.Element (defaultRegistry)
import Match3.Element.Caps (Cap(..), applyCap, setCap)
import qualified Match3.Element.Caps as N
import Match3.Element.Class
import Match3.Element.Registry (endRules, pushableWith, runAdjacentWith)
import Match3.Element.Types
import qualified Match3.Game.EndPhase as NewE
import qualified Match3.Game.Trace as NewT
import qualified Match3.Obstacles as New
import qualified Match3.Types as NewB
import Match3.Types.Optics (cellColorT, _Cake, _Chest, _Honey, _Safe, _Stone)
import Spec.Properties (genCell, genColor, genGem)
import Spec.Support.Arbitrary (shrinkBoard)
import qualified Spec.Support.LegacyObstacles as Old
import qualified Spec.Support.LegacyRules as L
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Fixed)

tests :: [TestTree]
tests =
  [ testProperty "qc_dedup_cell_optics_laws" qc_dedup_cell_optics_laws
  , testProperty "qc_dedup_body_readers_same_as_legacy" qc_dedup_body_readers_same_as_legacy
  , testProperty "qc_dedup_obstacles_same_as_legacy" (withMaxSuccess 500 qc_dedup_obstacles_same_as_legacy)
  , testProperty "qc_dedup_rule_folds_same_as_legacy" (withMaxSuccess 300 qc_dedup_rule_folds_same_as_legacy)
  , testProperty "qc_dedup_caps_same_as_legacy" (withMaxSuccess 500 qc_dedup_caps_same_as_legacy)
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

-- | 任意行列（1–8）的障碍盘。
genObstacleBoard :: Gen Board
genObstacleBoard = do
  r <- choose (1, 8)
  c <- choose (1, 8)
  boardFromRows <$> vectorOf r (vectorOf c genObstacleCell)

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
-- 与旧写法对照：读数

-- | Match3.Types.Body 的五组 mkXLayers / xLayers / isX、Match3.Types.Cell 的 cellColor、改色（旧 recolorCell = set cellColorT）。
qc_dedup_body_readers_same_as_legacy :: Property
qc_dedup_body_readers_same_as_legacy =
  forAll genLayeredCell $ \cell -> forAll (choose (-2, 5)) $ \n -> forAll genColor $ \col ->
    counterexample (show cell) $
      conjoin
        [ counterexample "mkXLayers" ([NewB.mkStoneLayers n, NewB.mkChestLayers n, NewB.mkHoneyLayers n, NewB.mkCakeLayers n, NewB.mkSafeLayers n] === [Old.mkStoneLayers n, Old.mkChestLayers n, Old.mkHoneyLayers n, Old.mkCakeLayers n, Old.mkSafeLayers n])
        , counterexample "xLayers" ([NewB.stoneLayers cell, NewB.chestLayers cell, NewB.honeyLayers cell, NewB.cakeLayers cell, NewB.safeLayers cell] === [Old.stoneLayers cell, Old.chestLayers cell, Old.honeyLayers cell, Old.cakeLayers cell, Old.safeLayers cell])
        , counterexample "isX" ([NewB.isStone cell, NewB.isChest cell, NewB.isHoney cell, NewB.isCake cell, NewB.isSafe cell] === [Old.isStone cell, Old.isChest cell, Old.isHoney cell, Old.isCake cell, Old.isSafe cell])
        , counterexample "cellColor" (NewB.cellColor cell === Old.cellColor cell)
        , counterexample "recolor" (set cellColorT col cell === Old.recolorCell cell col)
        ]

--------------------------------------------------------------------------------
-- 与旧写法对照：占格障碍

-- | Match3.Obstacles 的全部导出（邻消揭层 ×5 及其无 except 版、相邻查询 ×9、魔法帽 / 染色瓶改色、果汁机、彩蛋、时间精灵、
-- withAdjacentStones）与第 9 项前逐项相同：结果盘面与位置列表（含顺序）。改色谓词取 isGem（内置）与「全都可改色」两种。
qc_dedup_obstacles_same_as_legacy :: Property
qc_dedup_obstacles_same_as_legacy =
  forAllShrink genObstacleBoard shrinkBoard $ \b ->
    forAll (genSomePos b) $ \seeds ->
      forAll (genSomePos b) $ \except ->
        let chips =
              [ ("stones", New.chipAdjacentStonesExcept, Old.chipAdjacentStonesExcept, New.chipAdjacentStones, Old.chipAdjacentStones)
              , ("chests", New.chipAdjacentChestsExcept, Old.chipAdjacentChestsExcept, New.chipAdjacentChests, Old.chipAdjacentChests)
              , ("honey", New.chipAdjacentHoneyExcept, Old.chipAdjacentHoneyExcept, New.chipAdjacentHoney, Old.chipAdjacentHoney)
              , ("cakes", New.chipAdjacentCakesExcept, Old.chipAdjacentCakesExcept, New.chipAdjacentCakes, Old.chipAdjacentCakes)
              , ("safes", New.chipAdjacentSafesExcept, Old.chipAdjacentSafesExcept, New.chipAdjacentSafes, Old.chipAdjacentSafes)
              , ("balloons", New.chipAdjacentBalloonsExcept, Old.chipAdjacentBalloonsExcept, New.chipAdjacentBalloons, Old.chipAdjacentBalloons)
              , ("spirits", New.chipAdjacentTimeSpiritsExcept, Old.chipAdjacentTimeSpiritsExcept, New.chipAdjacentTimeSpirits, Old.chipAdjacentTimeSpirits)
              ]
            adjs =
              [ ("stonesAdjacentTo", New.stonesAdjacentTo, Old.stonesAdjacentTo)
              , ("chestsAdjacentTo", New.chestsAdjacentTo, Old.chestsAdjacentTo)
              , ("honeysAdjacentTo", New.honeysAdjacentTo, Old.honeysAdjacentTo)
              , ("cakesAdjacentTo", New.cakesAdjacentTo, Old.cakesAdjacentTo)
              , ("safesAdjacentTo", New.safesAdjacentTo, Old.safesAdjacentTo)
              , ("hatsAdjacentTo", New.hatsAdjacentTo, Old.hatsAdjacentTo)
              , ("surprisesAdjacentTo", New.surprisesAdjacentTo, Old.surprisesAdjacentTo)
              , ("bottlesAdjacentTo", New.bottlesAdjacentTo, Old.bottlesAdjacentTo)
              , ("spiritsAdjacentTo", New.spiritsAdjacentTo, Old.spiritsAdjacentTo)
              , ("balloonsAdjacentSameColor", New.balloonsAdjacentSameColor, Old.balloonsAdjacentSameColor)
              , ("makersAdjacentSameColor", New.makersAdjacentSameColor, Old.makersAdjacentSameColor)
              , ("withAdjacentStones", New.withAdjacentStones, Old.withAdjacentStones)
              ]
            preds = [("isGem", NewB.isGem, Old.isGem), ("any", const True, const True)]
            peeled = or [length (snd (f b seeds except)) > 0 | (_, f, _, _, _) <- take 5 chips]
            chipped = or [fst (f b seeds except) /= b | (_, f, _, _, _) <- take 5 chips]
            hatHit = not (null (New.hatsAdjacentTo b seeds))
        in classify peeled "some last layer chipped" $
             classify chipped "some layer decremented" $
               classify hatHit "hat triggered" $
                 conjoin
                   ( [ counterexample nm (conjoin [f b seeds except === g b seeds except, f0 b seeds === g0 b seeds])
                     | (nm, f, g, f0, g0) <- chips
                     ]
                       ++ [counterexample nm (f b seeds === g b seeds) | (nm, f, g) <- adjs]
                       ++ concat
                         [ [ counterexample ("hats/" ++ pn) (New.triggerAdjacentHatsBy np b seeds except === Old.triggerAdjacentHatsBy op b seeds except)
                           , counterexample ("bottles/" ++ pn) (New.triggerAdjacentBottlesBy np b seeds except === Old.triggerAdjacentBottlesBy op b seeds except)
                           ]
                         | (pn, np, op) <- preds
                         ]
                       ++ [ counterexample "triggerAdjacentHats" (New.triggerAdjacentHats b seeds === Old.triggerAdjacentHats b seeds)
                          , counterexample "triggerAdjacentBottles" (New.triggerAdjacentBottles b seeds === Old.triggerAdjacentBottles b seeds)
                          , counterexample "chargeAdjacentMakersSit" (New.chargeAdjacentMakersSit b seeds === Old.chargeAdjacentMakersSit b seeds)
                          , counterexample "openSurprises" (New.openSurprises b seeds === Old.openSurprises b seeds)
                          , counterexample "adjacentWhere isStone" (New.adjacentWhere NewB.isStone b seeds === Old.stonesAdjacentTo b seeds)
                          , counterexample "chipAdjacentLayeredExcept _Safe (Just Cookie)" (New.chipAdjacentLayeredExcept _Safe (Just Cookie) b seeds except === Old.chipAdjacentSafesExcept b seeds except)
                          ]
                   )

--------------------------------------------------------------------------------
-- 与旧写法对照：规则折叠

-- | 步末规则：EndPhase.runPhase（三个阶段、任意上下文）与 Trace.traceSpreadsWith 与旧的 foldl + reverse 相同；
-- runEndRules 跑任意一串真实步末规则（三个阶段的规则任取、任意顺序、可重复）等于逐条 erRun 再丢掉空效果；
-- 邻格波及 runAdjacentWith（mapAccumL）与旧的四元组 foldl 相同。
qc_dedup_rule_folds_same_as_legacy :: Property
qc_dedup_rule_folds_same_as_legacy =
  forAllShrink genRuleBoard shrinkBoard $ \b ->
    forAll (genSomePos b) $ \avoid -> forAll (genSomePos b) $ \walls -> forAll (choose (0, 5)) $ \k ->
      forAll (genSomePos b) $ \tru -> forAll (genSomePos b) $ \direct -> forAll (genSomePos b) $ \protect ->
        forAll (listOf (choose (0, length allRules - 1))) $ \ixs ->
          let reg = defaultRegistry
              picked = map (allRules !!) ixs  -- 按下标挑（EndRule 没有 Show）
              ctx = EndCtx avoid walls (pushableWith reg)
              phases = [PhaseTick, PhaseSpread, PhaseMove]
              (recs, bEnd) = runEndRules ctx picked b
              naive = foldl (\(acc, bd) r -> let (e, bd') = erRun r ctx bd in (acc ++ [(bd, bd', x) | Just x <- [e]], bd')) ([], b) picked
              anyEffect = or [not (null (fst (NewE.runPhase reg ph ctx k b))) | ph <- phases]
          in classify anyEffect "some end effect" $
               classify (not (null recs)) "runEndRules recorded" $
                 conjoin
                   ( [counterexample ("runPhase " ++ show ph) (NewE.runPhase reg ph ctx k b === L.runPhase reg ph ctx k b) | ph <- phases]
                       ++ [ counterexample "traceSpreadsWith" (NewT.traceSpreadsWith reg k b === L.traceSpreadsWith reg k b)
                          , counterexample "runEndRules" ((recs, bEnd) === naive)
                          , counterexample "runAdjacentWith" (runAdjacentWith reg tru direct protect b === L.runAdjacentWith reg tru direct protect b)
                          ]
                   )
  where
    allRules = concat [endRules defaultRegistry ph | ph <- [PhaseTick, PhaseSpread, PhaseMove]]
    -- 步末规则要有东西可做：蜗牛 / 倒计时 / 藤 / 巧克力 / 蒸汽 / 毛球等都在 genCell 里
    genRuleBoard = do
      r <- choose (2, 8)
      c <- choose (2, 8)
      boardFromRows <$> vectorOf r (vectorOf c (frequency [(3, genCell), (1, genObstacleCell), (1, Countdown <$> genColor <*> choose (1, 2))]))

--------------------------------------------------------------------------------
-- 与旧写法对照：能力声明

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

-- | 每个能力声明（新：透镜写入、旧：记录更新），参数各取一个能看出差别的值。
capPairs :: [(String, Cap, L.LCap)]
capPairs =
  [ ("colorIs", N.colorIs C4, L.colorIs C4), ("colorless", N.colorless, L.colorless)
  , ("swappable", N.swappable, L.swappable), ("notHintable", N.notHintable, L.notHintable)
  , ("onSwap", N.onSwap swapR, L.onSwap swapR)
  , ("hit Destroy", N.hit Destroy, L.hit Destroy), ("breaks", N.breaks, L.breaks), ("noFire", N.noFire, L.noFire)
  , ("explodes", N.explodes (\_ p -> [p]), L.explodes (\_ p -> [p]))
  , ("onAdjacent", N.onAdjacent 77 (\_ bd -> AdjOut bd [] []), L.onAdjacent 77 (\_ bd -> AdjOut bd [] []))
  , ("opens", N.opens (\bd _ -> (bd, [], [])), L.opens (\bd _ -> (bd, [], [])))
  , ("teleports", N.teleports, L.teleports), ("drainsAt", N.drainsAt [EdgeBottom, EdgeLeft], L.drainsAt [EdgeBottom, EdgeLeft])
  , ("keepsOnShuffle", N.keepsOnShuffle, L.keepsOnShuffle), ("recolors", N.recolors, L.recolors), ("pushes", N.pushes, L.pushes)
  , ("reshuffles", N.reshuffles, L.reshuffles), ("noRecolor", N.noRecolor, L.noRecolor), ("noPush", N.noPush, L.noPush)
  , ("counts", N.counts CountStones, L.counts CountStones), ("countsDiff", N.countsDiff CountCookies, L.countsDiff CountCookies)
  , ("weighs", N.weighs 5, L.weighs 5), ("bonus", N.bonus 2, L.bonus 2), ("vacates", N.vacates, L.vacates)
  , ("atEnd", N.atEnd (moveRule 33 (\_ bd -> (Nothing, bd))), L.atEnd (EndRule PhaseMove 33 (\_ bd -> (Nothing, bd)) (const []) (const [])))
  , ("ground", N.ground (\x -> if x > 1 then Just (x - 1) else Nothing), L.ground (\x -> if x > 1 then Just (x - 1) else Nothing))
  , ("widens", N.widens (\_ ps -> ps ++ ps), L.widens (\_ ps -> ps ++ ps))
  , ("onMessage", N.onMessage answer, L.onMessage answer)
  , ("withMatch", N.withMatch (\x -> x {mcBlocksMatch = True}), L.withMatch (\x -> x {mcBlocksMatch = True}))
  , ("withHit", N.withHit (\x -> x {hcStrip = True}), L.withHit (\x -> x {hcStrip = True}))
  , ("withMove", N.withMove (\x -> x {mvFalls = False}), L.withMove (\x -> x {mvFalls = False}))
  , ("withCount", N.withCount (\x -> x {ccBonusMoves = ccBonusMoves x + 3}), L.withCount (\x -> x {ccBonusMoves = ccBonusMoves x + 3}))
  , ("withStep", N.withStep (\x -> x {stGround = Nothing}), L.withStep (\x -> x {stGround = Nothing}))
  , ("set blocksSwap True", setCap (N.matchL . N.mcBlocksSwapL) True, \c -> c {capMatch = (capMatch c) {mcBlocksSwap = True}})
  , ("set weight 0", setCap (N.countL . N.ccDiffWeightL) 0, \c -> c {capCount = (capCount c) {ccDiffWeight = 0}})
  ]
  where
    swapR = SwapRule 42 (\_ _ _ -> True) (\_ p _ -> [p])
    -- 只回复探针：回复的元素值（Show）能看出新旧是否装进了同一个函数
    answer msg = case fromMessage msg of
      Just Probe -> Just (SomeElement (Inert "probe" (Stone 2)))
      Nothing -> Nothing

-- | 任意原型 × 任意一串声明（可重复、互相冲突，如 swappable 与「挡交换」、weighs 5 与 weight 0、累加的 withCount）：
-- 新的 withCaps（mconcat，Dual (Endo Caps)）与旧的 foldl 逐项相同；幺半群律（结合、单位元）在观察上成立；
-- 「后面的覆盖前面的」：applyCap (a <> b) = applyCap b . applyCap a。
qc_dedup_caps_same_as_legacy :: Property
qc_dedup_caps_same_as_legacy =
  forAll (elements [Piece, Blocker, Fixed]) $ \a ->
    forAll (listOf (choose (0, length capPairs - 1))) $ \ixs ->
      forAll (choose (0, length capPairs - 1)) $ \i -> forAll (choose (0, length capPairs - 1)) $ \j ->
        let picked = map (capPairs !!) ixs
            (_, ci, _) = capPairs !! i
            (_, cj, _) = capPairs !! j
            ncs = [c | (_, c, _) <- picked]
            ocs = [o | (_, _, o) <- picked]
            base = capsOf a
            sig = capsSig
        in counterexample (show (a, [nm | (nm, _, _) <- picked])) $
             conjoin
               [ counterexample "withCaps" (sig (N.withCaps a ncs) === sig (L.withCaps a ocs))
               , counterexample "piece / blocker / fixed" (map sig [N.piece ncs, N.blocker ncs, N.fixed ncs] === map sig [L.piece ocs, L.blocker ocs, L.fixed ocs])
               , counterexample "each writer" (conjoin [counterexample nm (sig (applyCap c base) === sig (o base)) | (nm, c, o) <- picked])
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
