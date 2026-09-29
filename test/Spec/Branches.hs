{-# LANGUAGE OverloadedStrings #-}
-- | 段 4：原先写死在主流程里的专门分支收进元素框架后的行为锁定。
--
-- * 成对交换规则（swapRule）：内置彩虹取色 / 特殊合成；测试专用「拉杆」只靠 swapRule 就能让无匹配的交换生效、进提示。
-- * 开启规则（openRule）：内置彩蛋；测试专用「豆荚」被邻格真消除时开出直线，本轮坐住不引爆。
-- * 可改色 / 可推动谓词（recolorable / pushable）：魔法帽 / 染色瓶、蜗牛只看注册表，内置取值与原写死的 isGem / pushable 相同。
-- * 关卡级元素（LevelElement：飞碟 / 皮带 / 传送门 / 地毯 / 地面层）：状态在元素值里，按消息回复，removeLevel 之后该机制不生效（地面层是核心元素）。
-- * 源码扫描：主流程模块不再点名这些元素的专门函数。
--
-- 样例元素（拉杆、豆荚、小车）只定义在这里，主流程源码里没有它们的名字。
module Spec.Branches
  ( tests
  ) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.Maybe (isNothing)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Board.Grid (setM, toM)
import Match3.Board.Match (findHintWith)
import Match3.Conveyor (beltMoves)
import Match3.Core
import Match3.Element
import Match3.Element.Class (Archetype(..), Element(..), Hit(..), SomeLevelElement(..), levelNameOf)
import Match3.Game.Move (resolveSwapWith)
import qualified Match3.Snail as Snail
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "br_swap_rule_test_element" br_swap_rule_test_element
  , testCase "br_open_rule_test_element" br_open_rule_test_element
  , testCase "br_builtin_predicates_match_legacy" br_builtin_predicates_match_legacy
  , testCase "br_recolorable_from_registry" br_recolorable_from_registry
  , testCase "br_pushable_from_registry" br_pushable_from_registry
  , testCase "br_level_hooks_builtin_and_removable" br_level_hooks_builtin_and_removable
  , testCase "br_level_hooks_removed_in_play" br_level_hooks_removed_in_play
  , testCase "br_main_flow_no_special_branches" br_main_flow_no_special_branches
  , testCase "br_board_takes_hooks_only" br_board_takes_hooks_only
  ]

-- tripleBoard / tripleMove / isCustomNamed / firstWave 见 Spec.Support。

allCells :: Board -> [Cell]
allCells b = map (getCell b) allPos

-- | 替换内置「gem」的测试版本：原型同普通宝石，只关掉可改色 / 可推动（原先写成旧记录的字段更新）。
data TweakedGem = TweakedGem Bool Bool Color
  deriving (Eq, Show)

instance Element TweakedGem where
  name _ = "gem"
  toCell (TweakedGem _ _ c) = Gem c Normal 0 Nothing
  recolorable (TweakedGem r _ _) = r
  pushable (TweakedGem _ p _) = p

tweakedGem :: Bool -> Bool -> Entry
tweakedGem r p = bodyEntry (TweakedGem r p C1) (\cell -> case cell of Gem c _ _ _ -> Just (TweakedGem r p c); _ -> Nothing) (\_ _ -> Nothing)

-- | 测试专用「拉杆」：可交换、直接命中即毁；和任意格交换时成对规则成立，种子 = 交换两端（无需成三连）。
newtype Lever = Lever Int
  deriving (Eq, Show)

instance Element Lever where
  name _ = "lever"
  toCell (Lever k) = Custom "lever" (CustomState k)
  archetype _ = Blocker
  blocksSwap _ = False
  onHit _ = Destroy
  swapRule _ = Just (SwapRule 5 fires (\_ p1 p2 -> [p1, p2]))
    where
      fires b p1 p2 = isCustomNamed "lever" (getCell b p1) || isCustomNamed "lever" (getCell b p2)

leverDef :: Entry
leverDef = customEntry (Lever 1) (Lever . unCustomState)

br_swap_rule_test_element :: Assertion
br_swap_rule_test_element = do
  let reg = register leverDef defaultRegistry
      board0 = setCell stableBoard (4, 4) (Custom "lever" (CustomState 1))
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = board0}
      (gs1, o1, mt1) = resolveSwapWith reg (4, 4) (4, 5) gs0
  assertBool "no ordinary match from this swap" (not (hasAnyMatch (swapCells board0 (4, 4) (4, 5))))
  assertBool "swap accepted through the pair rule" (moveApplied o1)
  w1 <- firstWave mt1
  assertEqual "seeds = both ends, cleared in the first wave" [(4, 4), (4, 5)] (sort (filter (`elem` [(4, 4), (4, 5)]) (cwCleared w1)))
  assertBool "lever gone" (not (any (isCustomNamed "lever") (allCells (gsBoard gs1))))
  assertBool "rule is listed by the registry" (swapFiresWith reg board0 (4, 4) (4, 5))
  -- 提示也经同一条规则：无普通匹配的盘上只有拉杆能走
  let stuck = setCell stuckNoMoveBoard (0, 0) (Custom "lever" (CustomState 1))
  assertEqual "no hint without the rule" Nothing (findHintWith defaultRegistry stuck)
  assertBool "hint via the rule" (maybe False (\(a, b) -> (0, 0) `elem` [a, b]) (findHintWith reg stuck))
  -- 内置表：拉杆是惰性占格，挡交换
  let (_, oD, _) = resolveSwapWith defaultRegistry (4, 4) (4, 5) gs0
  assertBool "default registry rejects" (not (moveApplied oD))

-- | 测试专用「豆荚」：被命中或邻格在本批前沿里时开出 C2 直线（本轮坐住，不在本轮清除）。
newtype Pod = Pod Int
  deriving (Eq, Show)

instance Element Pod where
  name _ = "pod"
  toCell (Pod k) = Custom "pod" (CustomState k)
  archetype _ = Blocker
  openRule _ = Just (OpenRule openPods)

podDef :: Entry
podDef = customEntry (Pod 1) (Pod . unCustomState)

openPods :: Board -> [Pos] -> (Board, [Pos], [Pos])
openPods b front =
  let near p = p `elem` front || any (`elem` front) [(fst p + dr, snd p + dc) | (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
      pods = [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), isCustomNamed "pod" (getCell b p), near p]
      b' = foldl (\bd p -> setCell bd p (Gem C2 LineH 0 Nothing)) b pods
  in (b', [], pods)

br_open_rule_test_element :: Assertion
br_open_rule_test_element = do
  let reg = register podDef defaultRegistry
      board0 = setCell tripleBoard (0, 1) (Custom "pod" (CustomState 1))
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      (_, o1, mt1) = resolveSwapWith reg p1 p2 gs0
  w1 <- firstWave mt1
  assertBool "move applied" (moveApplied o1)
  assertBool "opened pod sits this wave (not cleared)" ((0, 1) `notElem` cwCleared w1)
  assertEqual "opened into a line gem that fell one row" (Gem C2 LineH 0 Nothing) (getCell (cwAfter w1) (1, 1))
  -- 与内置彩蛋并存：两条开启规则都跑
  let board1 = setCell board0 (0, 0) Surprise
      (_, _, mt2) = resolveSwapWith reg p1 p2 gs0 {gsBoard = board1}
  w2 <- firstWave mt2
  assertBool "surprise opened too" (getCell (cwAfter w2) (1, 0) /= Surprise && (0, 0) `notElem` [p | p <- [(1, 0)], getCell (cwAfter w2) p == Surprise])
  assertEqual "pod still opened" (Gem C2 LineH 0 Nothing) (getCell (cwAfter w2) (1, 1))
  -- 内置表：豆荚是惰性占格，原样下落
  let (_, _, mtD) = resolveSwapWith defaultRegistry p1 p2 gs0
  wD <- firstWave mtD
  assertBool "default registry: pod stays a pod" (isCustomNamed "pod" (getCell (cwAfter wD) (1, 1)))

-- | 内置取值与段 4 之前写死的谓词逐格相同（改色 = isGem，推动 = Snail.pushable）。
br_builtin_predicates_match_legacy :: Assertion
br_builtin_predicates_match_legacy = do
  let samples =
        [ mkGem C1, Gem C2 LineH 0 Nothing, Gem C3 LineV 1 Nothing, Gem C4 Bomb 0 (Just Grass), Gem C5 Rainbow 0 Nothing
        , Countdown C1 3, Flip C2 C3, Stone 2, Chest 1, Honey 1, Balloon C1, Cookie, Cake 2, MagicHat, Maker C1 0
        , mkSnail 0 1, Safe 1, Surprise, Bottle C4, TimeSpirit, Custom "x" (CustomState 1)
        ]
  forM_ samples $ \cell -> do
    assertEqual ("recolorable " ++ show cell) (isGem cell) (recolorableWith defaultRegistry cell)
    assertEqual ("pushable " ++ show cell) (Snail.pushable cell) (pushableWith defaultRegistry cell)

-- | 魔法帽只给注册表里可改色（recolorable）的格换色：把普通宝石改成不可改色后，帽子不再动它们。
br_recolorable_from_registry :: Assertion
br_recolorable_from_registry = do
  let noRecolor = register (tweakedGem False True) defaultRegistry
      board0 = setCell tripleBoard (0, 1) MagicHat
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      afterWave reg = let (_, _, mt) = resolveSwapWith reg p1 p2 gs0 in cwAfter <$> firstWave mt
  -- (0,0) C1 与 (0,2) C3 是帽子的两个未消除邻格。第 1 行实际是四连（(1,3) 也是 C5），(1,2) 生成直线坐住，
  -- 所以 (0,0) 落到 (1,0)、(0,2) 留在原处
  bDef <- afterWave defaultRegistry
  bNo <- afterWave noRecolor
  assertEqual "default: hat swapped the two colors" (mkGem C3, mkGem C1) (getCell bDef (1, 0), getCell bDef (0, 2))
  assertEqual "not recolorable: colors kept" (mkGem C1, mkGem C3) (getCell bNo (1, 0), getCell bNo (0, 2))

-- | 蜗牛只推注册表里可推动（pushable）的格：测试专用「小车」可推；普通宝石改成不可推后蜗牛掉头。
newtype Cart = Cart Int
  deriving (Eq, Show)

instance Element Cart where
  name _ = "cart"
  toCell (Cart k) = Custom "cart" (CustomState k)
  archetype _ = Blocker
  pushable _ = True

cartDef :: Entry
cartDef = customEntry (Cart 1) (Cart . unCustomState)

br_pushable_from_registry :: Assertion
br_pushable_from_registry = do
  let board0 = setCell (setCell tripleBoard (7, 1) (mkSnail 0 1)) (7, 2) (Custom "cart" (CustomState 1))
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = board0}
      (p1, p2) = tripleMove
      final reg b = let (gs1, _, _) = resolveSwapWith reg p1 p2 gs0 {gsBoard = b} in gsBoard gs1
      bCart = final (register cartDef defaultRegistry) board0
  assertBool "cart pushed back, snail advanced" (isCustomNamed "cart" (getCell bCart (7, 1)) && isSnail (getCell bCart (7, 2)))
  let bD = final defaultRegistry board0
  assertBool "default registry: snail turns around" (isSnail (getCell bD (7, 1)) && isCustomNamed "cart" (getCell bD (7, 2)))
  let boardG = setCell tripleBoard (7, 1) (mkSnail 0 1)
      bG = final defaultRegistry boardG
      bNoPush = final (register (tweakedGem True False) defaultRegistry) boardG
  assertBool "default: gem pushed" (isSnail (getCell bG (7, 2)))
  assertBool "gem not pushable: snail stays" (isSnail (getCell bNoPush (7, 1)))

-- | 关卡级元素：内置表里四种元素的节拍回复就是原实现（第 7 刀起状态在元素值里，Board 层经钩子 LevelHooks 调用）；
-- 去掉后各自退化为「不生效」（状态原样）。地面层是核心元素：去掉同名注册也照常按注册表的地面层规则命中。
br_level_hooks_builtin_and_removable :: Assertion
br_level_hooks_builtin_and_removable = do
  assertEqual "builtin level defs" ["ufo", "belt", "portal", "carpet"] (map levelNameOf (levelDefs defaultRegistry))
  let b0 = fst (randomStableBoard (mkStdGen 101))
      ufos = [mkUfo (3, 3) C1, mkUfo (0, 0) C2]
      noUfo = removeLevel "ufo" defaultRegistry
      absorb reg = (\(ps, h) -> (ps, levelUfos (hookLevel h))) (onAbsorb (levelHooksWith reg [SomeLevelElement (UfoLevel ufos)]) b0)
  assertEqual "ufo hook = stepUfos" (stepUfos b0 ufos) (absorb defaultRegistry)
  assertEqual "ufo removed: no absorb, ufos stay" ([], ufos) (absorb noUfo)
  let belts = [[(2, 0), (2, 1), (2, 2), (3, 2)]]
      beltEl = [SomeLevelElement (BeltLevel belts)]
  assertEqual "belt hook = beltMoves" (Just (beltMoves belts)) (fst <$> beltShiftIn defaultRegistry beltEl)
  assertEqual "belt cells avoided" (concat belts) (avoidCellsIn defaultRegistry beltEl)
  assertBool "belt removed" (isNothing (beltShiftIn (removeLevel "belt" defaultRegistry) beltEl))
  assertEqual "no belts = nobody answers" Nothing (fst <$> beltShiftIn defaultRegistry [SomeLevelElement (BeltLevel [])])
  let mb = setM (toM b0) (0, 5) Nothing
      portals = [((7, 0), (0, 5))]
      settle reg = onSettle (levelHooksWith reg [SomeLevelElement (PortalLevel portals)]) mb
  assertEqual "portal hook = portalTeleport" (portalTeleport (portalWith defaultRegistry) portals mb) (settle defaultRegistry)
  assertBool "portal hook moves something here" (settle defaultRegistry /= mb)
  assertEqual "portal removed: no teleport" mb (settle (removeLevel "portal" defaultRegistry))
  assertEqual "portal ends are walls" [(7, 0), (0, 5)] (wallCellsIn defaultRegistry [SomeLevelElement (PortalLevel portals)])
  let open0 = [(3, 0), (3, 1), (4, 4)]
      hit = [(3, 0), (3, 1), (5, 5)]
      cover reg = (\(n, es) -> (levelCarpetOpen es, n)) (coverIn reg hit [SomeLevelElement (CarpetLevel open0)])
  assertEqual "carpet hook = coverCarpets" (coverCarpets open0 hit) (cover defaultRegistry)
  assertEqual "carpet removed: nothing covered" (open0, 0) (cover (removeLevel "carpet" defaultRegistry))
  let ground0 = [((3, 0), ("jelly", 2)), ((5, 5), ("jelly", 1)), ((6, 6), ("jelly", 1))]
      groundHit reg = (\(cs, es) -> (levelGround es, cs)) (hitGroundIn reg hit [SomeLevelElement (GroundLayer ground0)])
  assertEqual "ground hook = hitGroundWith" (hitGroundWith defaultRegistry hit ground0) (groundHit defaultRegistry)
  assertEqual "ground is core" (groundHit defaultRegistry) (groundHit (removeLevel "ground" defaultRegistry))
  assertBool "ground hit something here" (fst (groundHit defaultRegistry) /= ground0)

-- | 38 关 × 前 6 手（种子 1，走提示）：四个关卡级元素都去掉后，飞碟不动不吸、皮带不移位、地毯不覆盖；
-- 内置表下同样的对局里这三件事都真的发生过（证明扫描覆盖到了）。
br_level_hooks_removed_in_play :: Assertion
br_level_hooks_removed_in_play = do
  let bare = foldr removeLevel defaultRegistry ["ufo", "belt", "portal", "carpet"]
      play reg li = go (6 :: Int) (levelGame li 1) []
        where
          go 0 gs acc = (gs, reverse acc)
          go n gs acc
            | gsOver gs /= Nothing = (gs, reverse acc)
            | otherwise = case findHintWith reg (gsBoard gs) of
                Nothing -> (gs, reverse acc)
                Just (a, b) -> let (gs', _, mt) = resolveSwapWith reg a b gs in go (n - 1) gs' ((gs, gs', mt) : acc)
      beltShifts steps = [() | (_, _, mt) <- steps, EndStep {esEffect = EndBeltShift (_ : _)} <- mtEnd mt]
      levelsWith f = [li | li <- [0 .. length allLevels - 1], not (f (levelGame li 1))]
      ufoLv = levelsWith (null . gsUfos)
      beltLv = levelsWith (null . gsBelts)
      carpetLv = levelsWith (null . gsCarpetOpen)
  assertBool "campaign has ufo / belt / carpet levels" (not (null ufoLv || null beltLv || null carpetLv))
  forM_ ufoLv $ \li -> do
    let (gsE, steps) = play bare li
        gs0 = levelGame li 1
    assertEqual ("L" ++ show li ++ " bare: ufos unchanged") (gsUfos gs0) (gsUfos gsE)
    assertEqual ("L" ++ show li ++ " bare: no ufo absorb") 0 (gsCount CountUfo gsE)
    assertBool ("L" ++ show li ++ " bare: played") (not (null steps))
  assertBool "default: some ufo moved" (or [gsUfos gsE /= gsUfos (levelGame li 1) | li <- ufoLv, let gsE = fst (play defaultRegistry li)])
  forM_ beltLv $ \li ->
    assertEqual ("L" ++ show li ++ " bare: no belt shift") 0 (length (beltShifts (snd (play bare li))))
  assertBool "default: belts shifted" (sum [length (beltShifts (snd (play defaultRegistry li))) | li <- beltLv] > 0)
  forM_ carpetLv $ \li ->
    assertEqual ("L" ++ show li ++ " bare: nothing covered") 0 (gsCount CountCarpets (fst (play bare li)))
  assertBool "default: carpets covered" (sum [gsCount CountCarpets (fst (play defaultRegistry li)) | li <- carpetLv] > 0)

-- | 主流程不再点名这些元素的专门函数：结算流水线（Board/*、Game/*，不含关卡数据与回放记录层，见
-- Spec.Support.Source.pipelineSources）不 import 彩虹 / 特殊合成 / 障碍 / 地毯的实现模块，
-- 代码里（去掉注释与字符串）也不用它们的专门函数。
br_main_flow_no_special_branches :: Assertion
br_main_flow_no_special_branches = do
  files <- pipelineSources
  assertBool "scanned the pipeline" (all (`elem` files) ["src/Match3/Game/" ++ m ++ ".hs" | m <- ["Move", "Boosters", "Resolve"]] && all (`elem` files) ["src/Match3/Board/" ++ m ++ ".hs" | m <- ["Match", "Clear", "Cascade", "Gravity"]])
  let bannedImports = ["Match3.Combos", "Match3.Rainbow", "Match3.Obstacles", "Match3.Carpet"]
      bannedIdents =
        [ "isRainbowSwap", "isSpecialCombo", "rainbowClearSeeds", "comboClearSeeds", "openSurprises"
        , "stepUfos", "coverCarpets", "beltMoves", "portalWith" ]
  srcs <- mapM readFile files
  let bad =
        [(f, "import " ++ m) | (f, s) <- zip files srcs, m <- importsOf s, m `elem` bannedImports]
          ++ [(f, w) | (f, s) <- zip files srcs, w <- bannedIdents, mentionsIdent w s]
  assertEqual "special-cased names in main flow" [] bad

-- | 第 7 刀（7a）：Board 层（连锁 / 沉降）只收关卡级钩子 LevelHooks，不 import 飞碟 / 皮带 / 地毯的实现模块、
-- 也不碰 GameState；结算流水线不读第 7 刀前的五个关卡字段（它们在 Game/State.hs 里只是派生读数）；
-- 第 7 刀删掉的名字（各元素的专用问法、crUfos、无状态的 SomeLevel、分开的回复消息）在 src / app / web 里都不再出现。
br_board_takes_hooks_only :: Assertion
br_board_takes_hooks_only = do
  let boardCore = ["src/Match3/Board/" ++ m ++ ".hs" | m <- ["Match", "Clear", "Cascade", "Gravity", "Hooks", "Grid"]]
  boardSrcs <- mapM readFile boardCore
  assertEqual "Board core imports no level element state"
    []
    [(f, m) | (f, s) <- zip boardCore boardSrcs, m <- importsOf s, m `elem` ["Match3.Ufo", "Match3.Conveyor", "Match3.Carpet", "Match3.Game.State", "Match3.Element.Level", "Match3.Element.Builtin.Level"]]
  assertEqual "Board core never names Ufo" [] [f | (f, s) <- zip boardCore boardSrcs, mentionsIdent "Ufo" s]
  flow <- pipelineSources
  let flowNoState = filter (/= "src/Match3/Game/State.hs") flow
  flowSrcs <- mapM readFile flowNoState
  assertBool "scanned Resolve" ("src/Match3/Game/Resolve.hs" `elem` flowNoState)
  assertEqual "pipeline does not read the old level fields"
    []
    [(f, w) | (f, s) <- zip flowNoState flowSrcs, w <- ["gsUfos", "gsBelts", "gsPortals", "gsCarpetOpen", "gsGround"], mentionsIdent w s]
  everything <- sourcesUnderAll ["src", "app", "web/hs"]
  allSrcs <- mapM readFile everything
  assertEqual "removed names are gone"
    []
    [ (f, w)
    | (f, s) <- zip everything allSrcs
    , w <- ["crUfos", "absorbWith", "beltShiftWith", "teleportWith", "coverWith", "applyPortalTeleportsWith", "SomeLevel", "Absorbed", "Shifted", "Settled", "Covered"]
    , mentionsIdent w s
    ]
