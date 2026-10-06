{-# LANGUAGE OverloadedStrings #-}
-- | 元素（ecs-3 起：原型 'Archetype' = 存储列 + 纯数据组件 + system；叠层仍是 Layer 类，ecs-4 换成数据）。
--
-- 阶段 2 删掉了扁平 ElementDef 记录，新旧两条路径不能再在同一进程里并排跑；等价性改由阶段 1（9ae6a7b，
-- 新旧记录并存且逐手相等）上生成的元素查询快照 test/golden/element-queries.txt 锁定：全部 *With 查询 ×
-- 样本格、放置、规则表、规则在样例盘上的输出、40 关逐手对局（见 test/golden/ElementQueries.hs）。
-- 另锁定原型的约定（按组件类型查询、叠层组合、状态在格子里、关卡级消息）与阶段 2 / 元素类重构 / ECS 消掉的遗留项。
module Spec.ElementClass
  ( tests
  ) where

import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Maybe (isJust, isNothing)
import qualified ElementQueries
import Match3.Board.Grid (setCell)
import Match3.Board.Match (findHintWith)
import Match3.Core
import Match3.Game.Level (newGameAtLevel)
import Match3.Types (boardSize, defaultConfig)
import Match3.Counts (namedCounts)
import Match3.Element
import Match3.Element.Mechanic
  ( Mechanic(..)
  , MechSys(..)
  , SomeMechanic(..)
  , fromMechanic
  , mechanic
  , mechNameOf
  )
import Match3.Element.Kind (customPlace)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Level (newGame, newGameAtLevelWith)
import Match3.Game.Move (resolveSwapWith, trySwap)
import Match3.Game.State (gsCount)
import Match3.Types (colorAt, goalCount, goalScore)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "ec_queries_match_stage1_snapshot" ec_queries_match_stage1_snapshot
  , testCase "ec_rules_match_stage1_snapshot" ec_rules_match_stage1_snapshot
  , testCase "ec_play_matches_stage1_snapshot" ec_play_matches_stage1_snapshot
  , testCase "ec_row_decodes_components" ec_row_decodes_components
  , testCase "ec_same_name_replaces_archetype" ec_same_name_replaces_archetype
  , testCase "ec_ice_layer_composes" ec_ice_layer_composes
  , testCase "ec_state_lives_in_element_value" ec_state_lives_in_element_value
  , testCase "ec_mechanic_defaults_silent" ec_mechanic_defaults_silent
  , testCase "ec_flat_record_removed" ec_flat_record_removed
  , testCase "ec_entity_wires_damage" ec_entity_wires_damage
  , testCase "ec_mechanic_names_unique" ec_mechanic_names_unique
  , testCase "ec_mechanics_by_beat" ec_mechanics_by_beat
  , testCase "ec_mechanic_stateful_extension" ec_mechanic_stateful_extension
  , testCase "ec_custom_matchable_gem" ec_custom_matchable_gem
  , testCase "ec_world_checked_cells" ec_world_checked_cells
  ]

-- | 快照里以给定前缀开头的行，两边逐行比；报告第一处分叉。
snapshotPart :: [String] -> Assertion
snapshotPart prefixes = do
  expected <- lines <$> readFile "test/golden/element-queries.txt"
  let pick = filter (\l -> any (`isPrefixOf` l) prefixes)
      e = pick expected
      a = pick ElementQueries.queryLines
  assertBool "snapshot part not empty" (not (null e))
  case [(i, x, y) | (i, x, y) <- zip3 [1 :: Int ..] e a, x /= y] of
    ((i, x, y) : _) -> assertFailure ("line " ++ show i ++ " differs\nexpected: " ++ take 400 x ++ "\nactual:   " ++ take 400 y)
    [] -> assertEqual "line count" (length e) (length a)

-- | 逐格查询（Q）、放置（P）、规则表与条目 / 关卡级元素清单（R）与阶段 1 全等。
ec_queries_match_stage1_snapshot :: Assertion
ec_queries_match_stage1_snapshot = snapshotPart ["Q ", "P ", "R "]

-- | 邻格 / 步末 / 成对交换 / 开启规则、直接命中、地面层在样例盘上的输出（A / E / S / O / C / G）与阶段 1 全等。
ec_rules_match_stage1_snapshot :: Assertion
ec_rules_match_stage1_snapshot = snapshotPart ["A ", "E ", "S ", "O ", "C ", "G "]

-- | 40 关 × 种子 1–2 × 12 手（提示、锤子、十字、交换）的逐手散列（M）与阶段 1 全等。
ec_play_matches_stage1_snapshot :: Assertion
ec_play_matches_stage1_snapshot = snapshotPart ["M "]

-- | 注册表把格子解码成行（原型 + 状态），按组件类型查询；宝石 = 普通棋子组件（gemMatch / gemHit / gemPhysics）。
ec_row_decodes_components :: Assertion
ec_row_decodes_components = do
  let g = bodyOf defaultRegistry (mkGem C2)
      gm = rowGet g :: Match
      gh = rowGet g :: OnHit
      gp = rowGet g :: Physics
      egg = bodyOf defaultRegistry Surprise
  assertEqual "gem name / state" ("gem", Just C2) (rowName g, colGet (gemColumn Normal) (rowCell g))
  assertEqual "gem color" (Just C2) (mColor gm)
  assertEqual "gem defaults" (False, True, True, True, Destroy, True, True, False, True) (mBlockSwap gm, hFires gh, pFalls gp, pPortal gp, hStrike gh, pRecolor gp, pPush gp, pKeepShuffle gp, mHintable gm)
  assertBool "rainbow not hintable" (not (mHintable (rowGet (bodyOf defaultRegistry (Gem C1 Rainbow 0 Nothing)))))
  assertEqual "egg is an obstacle" (True, False, Nothing, True) (mBlockSwap (rowGet egg), hFires (rowGet egg), mColor (rowGet egg), pKeepShuffle (rowGet egg))
  -- 叠层在外、本体在内：冰层名单 + 本体行
  let iced = Gem C1 LineV 2 Nothing
  assertEqual "iced line: layers" ["ice"] (map peeledName (upperOf defaultRegistry iced))
  assertEqual "iced line: body" ("line_v", Gem C1 LineV 0 Nothing) (let r = bodyOf defaultRegistry iced in (rowName r, rowCell r))

-- | 同名原型按名字替换（以后注册的为准），状态类型可以不同：两个都叫 "twin" 的原型写回同一种格子，
-- 一个状态是 Int、一个是 Bool；替换后同一格按新原型的列解码、组件也来自新原型。名字是 ElementName（newtype）。
twinA :: Archetype Int
twinA = archetype "twin" (customColumn "twin")

twinB :: Archetype Bool
twinB = (archetype "twin" (Column get (\b -> Custom "twin" (CustomState (fromEnum b)))))
  {aMatch = \b -> obstacleMatch {mHintable = b}}
  where
    get cell = case cell of
      Custom "twin" (CustomState v) -> Just (v > 0)
      _ -> Nothing

ec_same_name_replaces_archetype :: Assertion
ec_same_name_replaces_archetype = do
  let wA = register (kindDef twinA) defaultRegistry
      wB = register (kindDef twinB) wA
      cell = Custom "twin" (CustomState 1)
  assertEqual "names collide" (aName twinA) (aName twinB)
  assertEqual "same cell" (colPut (aColumn twinA) 1) (colPut (aColumn twinB) True)
  assertEqual "replaced in place" (length (registryDefs wA)) (length (registryDefs wB))
  assertEqual "A: obstacle match" obstacleMatch (rowGet (bodyOf wA cell))
  assertEqual "B: state decoded as Bool, component from B" (obstacleMatch {mHintable = True}) (rowGet (bodyOf wB cell))
  assertEqual "name is a newtype with String's Show" "\"twin\"" (show (aName twinA))
  assertEqual "unElementName" "twin" (unElementName (aName twinB))

-- | 冰层：包在宝石外面，整格组件与逐层询问一致。
ec_ice_layer_composes :: Assertion
ec_ice_layer_composes = do
  let iced n = Gem C3 Bomb n Nothing
      plain n = Gem C3 Normal n Nothing
      w = defaultRegistry
  assertEqual "match color passes" (Just C3) (matchColorWith w (plain 2))
  assertEqual "name is the body's" "bomb" (elementName w (iced 1))
  assertEqual "multi-ice does not fire" False (hFires (wholeHit w (iced 2)))
  assertEqual "last ice fires" True (hFires (wholeHit w (iced 1)))
  assertEqual "hit peels one ice" (Absorb (plain 1)) (hStrike (wholeHit w (plain 2)))
  assertEqual "last ice shatters with the gem" Destroy (hStrike (wholeHit w (plain 1)))
  assertBool "iced normal gem kept on shuffle" (pKeepShuffle (wholePhysics w (plain 1)))
  assertEqual "layer hit" (Keep 1) (sHit (cvShield iceCover 2))
  forM_ [(c, k, i, ov) | c <- [C1, C4], k <- [Normal, Bomb], i <- [1 .. 3], ov <- [Nothing, Just Grass]] $ \(c, k, i, ov) -> do
    let cell = Gem c k i ov
    assertEqual ("world directHit " ++ show cell) (if i > 1 then Absorb (Gem c k (i - 1) ov) else Destroy) (directHitWith defaultRegistry cell)

-- | 测试专用「鸟窝」：状态（剩余命中数）在格子 Custom "nest" k 里，原型的命中组件按状态给出新格子；
-- 注册进注册表后，锤子每敲一次减一，最后一下才碎并计数。
nestArch :: Archetype Int
nestArch = (archetype "nest" (customColumn "nest"))
  { aSpawn = customPlace "nest"
  , aTally = const emptyTally {tCounter = Just (CountNamed "nest")}
  , aHit = \k -> if k > 1 then absorbHit (colPut (customColumn "nest") (k - 1)) else breakHit
  }

ec_state_lives_in_element_value :: Assertion
ec_state_lives_in_element_value = do
  let world = register (kindDef nestArch) defaultRegistry
      p = (3, 3)
      gs0 = (newGame defaultConfig 7) {gsBoard = setCell stableBoard p (Custom "nest" (CustomState 3)), gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith world p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
      gs3 = hammer gs2
  assertEqual "struck returns the new state" (Absorb (Custom "nest" (CustomState 2))) (hStrike (aHit nestArch 3))
  assertEqual "decoded state" ("nest", Just 3) (let r = bodyOf world (Custom "nest" (CustomState 3)) in (rowName r, colGet (customColumn "nest") (rowCell r)))
  assertEqual "first hit" (Custom "nest" (CustomState 2)) (getCell (gsBoard gs1) p)
  assertEqual "second hit" (Custom "nest" (CustomState 1)) (getCell (gsBoard gs2) p)
  assertBool "third hit breaks it" (not (isNest (getCell (gsBoard gs3) p)))
  assertEqual "counted once" [("nest", 1)] (namedCounts (gsCounts gs3))
  assertEqual "placed via the constructor" (Right (Custom "nest" (CustomState 2))) (getCell <$> placeWith world "nest" [AInt 2] stableBoard [p] <*> pure p)
  where
    isNest cell = case cell of
      Custom "nest" _ -> True
      _ -> False

-- | 关卡级机制缺省不回复任何节拍（ecs-5 起 = 原型的 system 列表为空）：只写名字的机制注册进去，
-- 每个节拍都没有回复，结果与没注册时相同。
newtype Quiet = Quiet ()
  deriving (Eq, Show)

quiet :: SomeMechanic
quiet = SomeMechanic (mechanic "quiet") (Quiet ())

ec_mechanic_defaults_silent :: Assertion
ec_mechanic_defaults_silent = do
  let world = registerMechanic quiet (foldl (flip removeMechanic) defaultRegistry (map mechNameOf builtinMechanics))
      es = [quiet]
  assertBool "no beat reply" (isNothing (beatIn world es [] onEndTick) && isNothing (queryIn world es [] askAvoid) && isNothing (queryIn world es [] askWall))
  assertBool "no query reply" (isNothing (queryIn world es [] askShapes) && isNothing (morphIn world es stableBoard stableBoard (0, 0) (0, 1)))
  assertEqual "no readings" ([], []) (levelUfos es, levelBelts es)
  assertBool "fromMechanic type check" (fromMechanic quiet == Just (Quiet ()))

-- | 阶段 2 消掉的遗留项：源码里没有扁平记录 / 封闭钩子；findHint 不再点名彩虹；
-- 主流程不再点名关卡级元素的实现（只经消息）。全部内置元素都是 instance（条目 33 个：新玩法 2 追加魔法石、新玩法 3 追加毛球，名字与阶段 1 相同由快照锁定）。
ec_flat_record_removed :: Assertion
ec_flat_record_removed = do
  srcFiles <- sourcesUnderAll ["src/Match3/Element", "src/Match3/ECS", "src/Match3/Board", "src/Match3/Game"]
  assertBool "scanned Element / Board / Game" (all (`elem` srcFiles) ["src/Match3/ECS/Registry.hs", "src/Match3/Element/Builtin/Gem.hs", "src/Match3/Board/Cascade.hs", "src/Match3/Game/Resolve.hs"])
  srcs <- mapM (fmap stripStrings . readFile) srcFiles
  -- 按完整标识符比（第 7 刀的钩子记录 LevelHooks 不是段 4 的封闭钩子 LevelHook）
  let bad = [(f, w) | (f, s) <- zip srcFiles srcs, w <- ["ElementDef", "baseDef", "LevelHook", "HookAbsorb", "HookShift", "HookTeleport", "HookCover", "Caps", "capsOf", "SomeModifier", "Modified", "sendMessage", "handleMessage", "customEntry", "bodyEntry", "SomeMessage", "fromMessage", "LevelElement", "SomeLevelElement", "levelReply", "HitResult", "SomePhase", "phaseProbe", "Layered", "SpecialKind", "kindRules", "boardSystems", "Codec", "layerCover", "LayerCover", "defaultCover", "layerRules", "peelAs", "SomeLayerValue", "onBeat", "MechLayout", "emptyLayout", "GroundKind", "groundKind", "mlName", "mechStart"], mentionsIdent w s]
  assertEqual "no flat record / closed hooks / old element class / Phase / Layer / Mechanic typeclass" [] bad
  match <- readFile "src/Match3/Board/Match.hs"
  assertBool "findHint no longer names the rainbow" (not ("isRainbow" `isInfixOf` stripStrings match) && "Match3.Rainbow" `notElem` importsOf match)
  flowFiles <- pipelineSources
  flow <- mapM readFile flowFiles
  assertEqual "main flow does not call level element implementations" [] [(f, w) | (f, s) <- zip flowFiles flow, w <- ["stepUfos", "beltMoves", "coverCarpets"], mentionsIdent w s]
  assertEqual "builtin entries" builtinEntryCount (length builtinDefs)
  assertEqual "level elements" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop"] (map mechNameOf builtinMechanics)

-- | 凡定义了 'Entity' 记录的模块，必须把扣血 system 挂进原型的 'aSystems'（@SysNear … (entityDamage 列 记录)@）；
-- 不得只写 Entity 却漏挂。slim-10 起 Entity 是记录；ecs-3 起扣血是原型自己的邻格 system。
ec_entity_wires_damage :: Assertion
ec_entity_wires_damage = do
  srcFiles <- sourcesUnderAll ["src/Match3/Element"]
  pairs <- mapM (\f -> (,) f <$> readFile f) srcFiles
  let entityFiles =
        [ (f, s)
        | (f, s) <- pairs
        , ":: Entity " `isInfixOf` s
        , f `notElem` ["src/Match3/Element/Kind.hs", "src/Match3/Element/Rules.hs"]
        ]
  assertBool "at least one Entity record (SnowBoss)" (not (null entityFiles))
  assertEqual "Entity modules wire entityDamage into aSystems" [] [f | (f, s) <- entityFiles, not ("(entityDamage " `isInfixOf` s && "aSystems = [SysNear" `isInfixOf` s)]
  snow <- readFile "src/Match3/Element/Builtin/Obstacle.hs"
  assertBool "SnowBoss wires its entity exactly once"
    (length (filter ("entityDamage snowBossColumn snowBossEntity" `isInfixOf`) (lines snow)) == 1)

-- | 内置关卡级机制的 mechName 两两不同（beatIn / registerMechanic 按名合并；撞名会静默覆盖）。
ec_mechanic_names_unique :: Assertion
ec_mechanic_names_unique = do
  let names = map mechNameOf builtinMechanics
  assertEqual "builtin mechanic names unique" (length names) (length (nub names))
  assertEqual "expected builtins" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop"] names

-- | 关卡级元素是开放的：测试专用「磁铁」在补子之后的节拍（onRefilled）吸走盘上第一颗 C1 宝石；
-- 不改主流程，只 registerMechanic（无状态：开局没有它时用注册的原型值）。第 7 刀 7b 起节拍折叠所有回复者：
-- 保留内置飞碟（本局没有飞碟，回复空）时磁铁照样生效。同一个节拍的多个回复者按注册顺序折叠：
-- 计步器（Pinger，避让格 +1 格）、倍增器（Doubler，避让格翻倍）都回复 avoidCells（内置只有有皮带的关卡回复）。
data Magnet = Magnet
  deriving (Eq, Show)

data Pinger = Pinger
  deriving (Eq, Show)

data Doubler = Doubler
  deriving (Eq, Show)

magnet :: SomeMechanic
magnet = SomeMechanic ((mechanic "magnet") {mechSystems = [OnRefilled absorb]}) Magnet
  where
    absorb b acc m = Just (acc ++ take 1 [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell b p == mkGem C1], m)

pinger :: SomeMechanic
pinger = SomeMechanic ((mechanic "pinger") {mechSystems = [AnswerAvoid (\acc _ -> Just (acc ++ [(0, 0)]))]}) Pinger

doubler :: SomeMechanic
doubler = SomeMechanic ((mechanic "doubler") {mechSystems = [AnswerAvoid (\acc _ -> Just (acc ++ acc))]}) Doubler

ec_mechanics_by_beat :: Assertion
ec_mechanics_by_beat = do
  let world = registerMechanic magnet defaultRegistry
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = setCell stableBoard (1, 0) (mkGem C5)}
      b1 = setCell (setCell stableBoard (1, 0) (mkGem C5)) (1, 1) (mkGem C5)
      (p1, p2) = ((1, 2), (2, 2))
      (_, o, mt) = resolveSwapWith world p1 p2 gs0 {gsBoard = b1}
      (_, oD, mtD) = resolveSwapWith defaultRegistry p1 p2 gs0 {gsBoard = b1}
      ping r = length <$> queryIn r [] (replicate 7 (0, 0)) askAvoid
      withPinger = registerMechanic pinger world
  assertBool "applied" (moveApplied o && moveApplied oD)
  assertBool "magnet adds an absorb wave (ufo also answers)" (length (mtWaves mt) > length (mtWaves mtD))
  assertEqual "magnet does not answer other beats" Nothing (ping world)
  assertEqual "beat folds the only replier" (Just 8) (ping withPinger)
  assertEqual "beat folds all repliers in registration order" (Just 16) (ping (registerMechanic doubler withPinger))
  assertEqual "other order" (Just 15) (ping (registerMechanic pinger (registerMechanic doubler defaultRegistry)))
  assertEqual "nobody answers by default" Nothing (ping defaultRegistry)
  assertEqual "registered after the builtins" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop", "magnet"] (map mechNameOf (mechanicDefs world))

-- | 第 7 刀（7a）验收：带状态的扩展关卡级元素不改主流程就能接入。测试专用「虹吸」开局由 OnStart system 给 2 格电量，
-- 每轮补子之后（OnRefilled system）有电量就吸走盘上最后一颗 C2 宝石并耗 1 格；状态只在 gsLevelElems 里的元素值中，
-- 由结算写回。只 registerMechanic + 用这张表开局 / 走子；去掉注册后状态原样、不再生效。Show 在内置字段后追加
-- gsLevelExtra（内置对局没有这一项，快照不变）。第 7 刀 7b：保留内置飞碟，同一节拍两者都生效（回复折叠：先飞碟、后虹吸）。
newtype Siphon = Siphon Int
  deriving (Eq, Show)

siphon :: Int -> SomeMechanic
siphon = SomeMechanic ((mechanic "siphon") {mechSystems = [OnStart (\_ _ -> Siphon 2), OnRefilled absorb]}) . Siphon
  where
    absorb b acc (Siphon k)
      | k > 0
      , p : _ <- reverse [q | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let q = (r, c), getCell b q == mkGem C2] =
          Just (acc ++ [p], Siphon (k - 1))
      | otherwise = Nothing

ec_mechanic_stateful_extension :: Assertion
ec_mechanic_stateful_extension = do
  let world = registerMechanic (siphon 0) defaultRegistry
      gs0 = newGameAtLevelWith world 0 defaultConfig 7
      charge gs = fmap (\(Siphon k) -> k) (levelState (gsLevelElems gs))
      play r n gs
        | n == (0 :: Int) || gsOver gs /= Nothing = [gs]
        | otherwise = case findHintWith r (gsBoard gs) of
            Nothing -> [gs]
            Just (a, b) -> let (gs', _, _) = resolveSwapWith r a b gs in gs : play r (n - 1) gs'
      states = play world 12 gs0
      final = last states
  assertEqual "opened in registration order + core ground" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop", "siphon", "ground"] (map mechNameOf (gsLevelElems gs0))
  assertEqual "mechStart gives the charge" (Just 2) (charge gs0)
  assertBool "Show appends the extension state" ("gsLevelExtra = [Siphon 2]" `isInfixOf` show gs0)
  assertBool "builtin games show no extras" (not ("gsLevelExtra" `isInfixOf` show (newGameAtLevel 0 defaultConfig 7)))
  assertEqual "charge only goes down, one per absorb" [2 - gsCount CountUfo g | g <- states] (map (maybe (-1) id . charge) states)
  assertEqual "depleted" (Just 0) (charge final)
  assertEqual "absorbed exactly two cells" 2 (gsCount CountUfo final)
  let bare = removeMechanic "siphon" world
      finalBare = last (play bare 12 gs0)
  assertEqual "unregistered: state untouched" (Just 2) (charge finalBare)
  assertEqual "unregistered: nothing absorbed" 0 (gsCount CountUfo finalBare)
  -- 飞碟关（第 13 关）：一次 onRefilled 节拍 = 飞碟的吸收 ++ 虹吸的吸收，两者的状态都推进
  let gsU = newGameAtLevelWith world 12 defaultConfig 1
      gsUD = newGameAtLevel 12 defaultConfig 1
      bU = gsBoard gsU
      (psBoth, hooksBoth) = onAbsorb (levelHooksWith world (gsLevelElems gsU)) bU
      (psUfo, hooksUfo) = onAbsorb (levelHooksWith defaultRegistry (gsLevelElems gsUD)) bU
      lastC2 = last [q | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let q = (r, c), getCell bU q == mkGem C2]
  assertBool "ufo level has ufos" (not (null (levelUfos (gsLevelElems gsU))))
  assertEqual "both absorb in one beat (ufo first)" (psUfo ++ [lastC2]) psBoth
  assertEqual "ufo state advanced as without the siphon" (levelUfos (hookLevel hooksUfo)) (levelUfos (hookLevel hooksBoth))
  assertEqual "siphon state advanced" (Just 1) (fmap (\(Siphon k) -> k) (levelState (hookLevel hooksBoth)))

-- | 自定义元素可以当可匹配的有色宝石：测试专用「星星」（Custom "star" 颜色号，普通棋子组件、按颜色匹配）
-- 与同色宝石成三连被消除并按名字计数、进提示；未注册时是惰性占格（打断连线）。
starArch :: Archetype Color
starArch = (archetype "star" (Column get (\c -> Custom "star" (CustomState (fromEnum c)))))
  { aSpawn = customPlace "star"
  , aTally = const emptyTally {tCounter = Just (CountNamed "star")}
  , aMatch = gemMatch . Just
  , aHit = const gemHit
  , aPhysics = const gemPhysics
  }
  where
    get cell = case cell of
      Custom "star" (CustomState v) -> Just (colorAt v)
      _ -> Nothing

ec_custom_matchable_gem :: Assertion
ec_custom_matchable_gem = do
  let world = register (kindDef starArch) defaultRegistry
      star = Custom "star" (CustomState (fromEnum C5))
      board0 = setCell (setCell stableBoard (1, 0) (mkGem C5)) (1, 1) star
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "star") 1)) 1) {gsBoard = board0}
      (p1, p2) = ((1, 2), (2, 2))
      (gs1, o1, mt1) = resolveSwapWith world p1 p2 gs0
  assertEqual "star matches as C5" (Just C5) (matchColorWith world star)
  assertBool "star can be swapped" (not (blocksSwapWith world star))
  assertBool "move applied" (moveApplied o1)
  w1 <- firstWave mt1
  assertBool "star cleared in the first wave" ((1, 1) `elem` cwCleared w1)
  assertEqual "counted by name" [("star", 1)] (namedCounts (gsCounts gs1))
  assertBool "hint sees the star" (isJust (findHintWith world board0))
  let (_, oD) = trySwap p1 p2 gs0
  assertEqual "unregistered star is inert" Nothing (matchColorWith defaultRegistry star)
  assertBool "unregistered: no match through it" (not (moveApplied oD) || namedCounts (gsCounts (fst (trySwap p1 p2 gs0))) == [])

-- | 注册表检查（由注册表的解码探针推导）：mkRegistryChecked 把重名 / 同一种格子被多个原型认领 /
-- 原型不认领任何格子暴露成值，内置条目表通过检查；mkRegistry 是总函数（空表也能解码，认不出的格子退回惰性占格）。
otherGemArch :: Archetype Color
otherGemArch = (archetype "other_gem" (gemColumn Normal))
  { aMatch = gemMatch . Just
  , aHit = const gemHit
  , aPhysics = const gemPhysics
  }

-- | 什么格子都不认的原型。
strayArch :: Archetype Int
strayArch = archetype "stray" (Column (const Nothing) (colPut (customColumn "stray")))

ec_world_checked_cells :: Assertion
ec_world_checked_cells = do
  let errsOf = either Just (const Nothing) . mkRegistryChecked
  assertEqual "builtin defs pass the check" Nothing (errsOf builtinDefs)
  assertEqual "duplicate name" (Just [DuplicateName "dup"]) (errsOf [inertDef "dup", inertDef "dup"])
  assertEqual "shared cell" (Just [SharedCell "cell 0" ["gem", "other_gem"]]) (errsOf (builtinDefs ++ [kindDef otherGemArch]))
  assertEqual "unclaimed" (Just [Unclaimed "stray"]) (errsOf [kindDef strayArch])
  -- 总函数：register 按名字替换仍可用；空元素世界解码不崩
  assertEqual "empty world decodes to inert" "?" (elementName (mkRegistry []) (mkGem C1))
  assertEqual "empty world: no upper layers" 0 (length (upperOf (mkRegistry []) (mkIceGem C1 2)))
