{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素类（xmonad LayoutClass 风格；元素类重构第 2 刀起是能力类 Match3.Element.Ability + 类型级 Kind / Layer）。
--
-- 阶段 2 删掉了扁平 ElementDef 记录，新旧两条路径不能再在同一进程里并排跑；等价性改由阶段 1（9ae6a7b，
-- 新旧记录并存且逐手相等）上生成的元素查询快照 test/golden/element-queries.txt 锁定：全部 *With 查询 ×
-- 样本格、放置、规则表、规则在样例盘上的输出、40 关逐手对局（见 test/golden/ElementQueries.hs）。
-- 另锁定类本身的约定（Eq / Show、叠层组合、状态在元素值里、关卡级消息）与阶段 2 / 元素类重构消掉的遗留项。
module Spec.ElementClass
  ( tests
  ) where

import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import qualified ElementQueries
import Match3.Board.Grid (setCell)
import Match3.Board.Match (findHintWith)
import Match3.Core
import Match3.Game.Level (newGameAtLevel)
import Match3.Types (boardSize, defaultConfig)
import Match3.Counts (namedCounts)
import Match3.Element
import Match3.Element.Ability
import Match3.Element.Mechanic
  ( Mechanic(..)
  , SomeMechanic(..)
  , fromMechanic
  , mechNameOf
  )
import Match3.Element.Kind (Kind(..), customPlace, fromCustom)
import Match3.Element.Layer (Layer(..), LayerHit(..), Layered(..))
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
  , testCase "ec_some_element_eq_show" ec_some_element_eq_show
  , testCase "ec_some_element_eq_by_type" ec_some_element_eq_by_type
  , testCase "ec_ice_layer_composes" ec_ice_layer_composes
  , testCase "ec_state_lives_in_element_value" ec_state_lives_in_element_value
  , testCase "ec_mechanic_defaults_silent" ec_mechanic_defaults_silent
  , testCase "ec_flat_record_removed" ec_flat_record_removed
  , testCase "ec_level_elements_by_message" ec_level_elements_by_message
  , testCase "ec_level_element_stateful_extension" ec_level_element_stateful_extension
  , testCase "ec_custom_matchable_gem" ec_custom_matchable_gem
  , testCase "ec_registry_checked_slots" ec_registry_checked_slots
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

-- | SomeElement 的 Eq 按类型再比状态，Show 稳定（元素名 + 状态值）。
ec_some_element_eq_show :: Assertion
ec_some_element_eq_show = do
  assertEqual "same name same state" (SomeElement (PlainGem C1)) (SomeElement (PlainGem C1))
  assertBool "same type other state" (SomeElement (PlainGem C1) /= SomeElement (PlainGem C2))
  assertBool "other element" (SomeElement (SpecialGem @'LineH C1) /= SomeElement (SpecialGem @'LineV C1))
  assertBool "other type" (SomeElement (PlainGem C1) /= SomeElement SurpriseEgg)
  assertEqual "show gem" "SomeElement \"gem\" (PlainGem C1)" (show (SomeElement (PlainGem C1)))
  assertEqual "show layered" "SomeElement \"gem\" (Layered (Ice 2) (SomeElement \"gem\" (PlainGem C3)))" (show (iceOn 2 (SomeElement (PlainGem C3))))
  assertEqual "unbox" (Just (PlainGem C4)) (fromElement (SomeElement (PlainGem C4)))
  assertEqual "unbox wrong type" (Nothing :: Maybe SurpriseEgg) (fromElement (SomeElement (PlainGem C4)))
  -- 宝石全用缺省方法（普通棋子），颜色取自写回的格子
  let g = PlainGem C2
  assertEqual "gem color" (Just C2) (color g)
  assertEqual "gem defaults" (False, True, True, True, Destroy, True, True, False, True) (blocksSwap g, fires g, falls g, portal g, struck g, recolorable g, pushable g, keepOnShuffle g, hintable g)
  assertBool "rainbow not hintable" (not (hintable (SpecialGem @'Rainbow C1)))
  assertEqual "egg is an obstacle" (True, False, Nothing, True) (blocksSwap SurpriseEgg, fires SurpriseEgg, color SurpriseEgg, keepOnShuffle SurpriseEgg)
  -- 注册表把格子解码成元素值
  assertEqual "decode gem" (SomeElement (PlainGem C5)) (bodyOf defaultWorld (mkGem C5))
  assertEqual "decode iced line" (iceOn 2 (SomeElement (SpecialGem @'LineV C1))) (elementOf defaultWorld (Gem C1 LineV 2 Nothing))

-- | 冰层包在外面的元素值（与注册表解码出的形状相同）。
iceOn :: Int -> SomeElement -> SomeElement
iceOn n e = SomeElement (Layered (Ice n) e)

-- | 第 6b 刀：SomeElement 的相等按具体类型（Typeable cast）+ 该类型的 Eq，不比较名字字符串。
-- 两个同名（"twin"）而类型不同的测试元素不相等；同类型同状态相等、不同状态不等；名字是 ElementName（newtype）。
ec_some_element_eq_by_type :: Assertion
ec_some_element_eq_by_type = do
  assertEqual "same type same state" (SomeElement (TwinA 1)) (SomeElement (TwinA 1))
  assertBool "same type other state" (SomeElement (TwinA 1) /= SomeElement (TwinA 2))
  assertEqual "names collide" (nameOf (TwinA 1)) (nameOf (TwinB 1))
  assertBool "same name, other type" (SomeElement (TwinA 1) /= SomeElement (TwinB 1))
  assertBool "same name, other type (flipped)" (SomeElement (TwinB 1) /= SomeElement (TwinA 1))
  assertEqual "same cell, still other type" (toCell (TwinA 1)) (toCell (TwinB 1))
  assertEqual "boxed twice" (SomeElement (SomeElement (TwinA 3))) (SomeElement (SomeElement (TwinA 3)))
  assertEqual "layer same" (iceOn 2 (SomeElement (TwinA 1))) (iceOn 2 (SomeElement (TwinA 1)))
  assertBool "layer other state" (iceOn 1 (SomeElement (TwinA 1)) /= iceOn 2 (SomeElement (TwinA 1)))
  assertBool "layer, inner other type" (iceOn 1 (SomeElement (TwinA 1)) /= iceOn 1 (SomeElement (TwinB 1)))
  assertEqual "name is a newtype with String's Show" "\"twin\"" (show (nameOf (TwinA 1)))
  assertEqual "unElementName" "twin" (unElementName (nameOf (TwinB 1)))

-- | 测试专用的两个同名元素类型（只用来检查 SomeElement 的相等不看名字字符串）。
newtype TwinA = TwinA Int
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Obstacle TwinA)

newtype TwinB = TwinB Int
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Obstacle TwinB)

instance Cellular TwinA where
  nameOf _ = "twin"
instance Countable TwinA
instance Renders TwinA

instance Cellular TwinB where
  nameOf _ = "twin"
instance Countable TwinB
instance Renders TwinB

-- | 冰层：包在宝石外面，组合结果与逐层询问一致，写回格子带冰层数。
ec_ice_layer_composes :: Assertion
ec_ice_layer_composes = do
  let iced n = iceOn n (SomeElement (SpecialGem @'Bomb C3))
      plain n = iceOn n (SomeElement (PlainGem C3))
  assertEqual "encode" (Gem C3 Normal 2 Nothing) (toCell (plain 2))
  assertEqual "encode special" (Gem C3 Bomb 1 Nothing) (toCell (iced 1))
  assertEqual "match color passes" (Just C3) (matchColor (plain 2))
  assertEqual "name is the body's" "bomb" (nameOf (iced 1))
  assertEqual "multi-ice does not fire" False (fires (iced 2))
  assertEqual "last ice fires" True (fires (iced 1))
  assertEqual "hit peels one ice" (Absorb (Gem C3 Normal 1 Nothing)) (struck (plain 2))
  assertEqual "last ice shatters with the gem" Destroy (struck (plain 1))
  assertBool "iced normal gem kept on shuffle" (keepOnShuffle (plain 1))
  assertEqual "layer hit" (Keep (Ice 1)) (layerHit (Ice 2))
  forM_ [(c, k, i, ov) | c <- [C1, C4], k <- [Normal, Bomb], i <- [1 .. 3], ov <- [Nothing, Just Grass]] $ \(c, k, i, ov) -> do
    let cell = Gem c k i ov
    assertEqual ("registry directHit " ++ show cell) (if i > 1 then Absorb (Gem c k (i - 1) ov) else Destroy) (directHitWith defaultWorld cell)

-- | 测试专用「鸟窝」：状态（剩余命中数）放在元素值里；受击返回新的元素值，写回 Custom "nest" k；
-- 注册进注册表后，锤子每敲一次减一，最后一下才碎并计数。
newtype Nest = Nest Int
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle Nest)

instance Cellular Nest where
  nameOf _ = "nest"
instance Hittable Nest where
  struck (Nest k) = if k > 1 then Absorb (toCell (Nest (k - 1))) else Destroy
  fires _ = False
instance Countable Nest where
  counter _ = Just (CountNamed "nest")
instance Renders Nest

instance Kind Nest where
  kindName _ = "nest"
  fromCell = fromCustom "nest" Nest
  place _ = customPlace "nest"

ec_state_lives_in_element_value :: Assertion
ec_state_lives_in_element_value = do
  let reg = register (kindDef @Nest) defaultWorld
      p = (3, 3)
      gs0 = (newGame defaultConfig 7) {gsBoard = setCell stableBoard p (Custom "nest" (CustomState 3)), gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith reg p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
      gs3 = hammer gs2
  assertEqual "struck returns the new state" (Absorb (Custom "nest" (CustomState 2))) (struck (Nest 3))
  assertEqual "decoded state" (SomeElement (Nest 3)) (bodyOf reg (Custom "nest" (CustomState 3)))
  assertEqual "first hit" (Custom "nest" (CustomState 2)) (getCell (gsBoard gs1) p)
  assertEqual "second hit" (Custom "nest" (CustomState 1)) (getCell (gsBoard gs2) p)
  assertBool "third hit breaks it" (not (isNest (getCell (gsBoard gs3) p)))
  assertEqual "counted once" [("nest", 1)] (namedCounts (gsCounts gs3))
  assertEqual "placed via the constructor" (Right (Custom "nest" (CustomState 2))) (getCell <$> placeWith reg "nest" [AInt 2] stableBoard [p] <*> pure p)
  where
    isNest cell = case cell of
      Custom "nest" _ -> True
      _ -> False

-- | 关卡级机制的节拍方法缺省都不回复（元素类重构第 5 刀起取代开放消息）：只写名字的机制注册进去，
-- 每个节拍都没有回复，结果与没注册时相同。
newtype Quiet = Quiet ()
  deriving (Eq, Show)

instance Mechanic Quiet where
  mechName _ = "quiet"

ec_mechanic_defaults_silent :: Assertion
ec_mechanic_defaults_silent = do
  let reg = registerMechanic (SomeMechanic (Quiet ())) (foldl (flip removeMechanic) defaultWorld (map mechNameOf builtinMechanics))
      es = [SomeMechanic (Quiet ())]
  assertBool "no beat reply" (isNothing (beatIn reg es [] onEndTick) && isNothing (queryIn reg es [] avoidCells) && isNothing (queryIn reg es [] wallCells))
  assertBool "no query reply" (isNothing (queryIn reg es [] shapes) && isNothing (morphIn reg es stableBoard stableBoard (0, 0) (0, 1)))
  assertEqual "no readings" ([], []) (levelUfos es, levelBelts es)
  assertBool "fromMechanic type check" (fromMechanic (SomeMechanic (Quiet ())) == Just (Quiet ()))

-- | 阶段 2 消掉的遗留项：源码里没有扁平记录 / 封闭钩子；findHint 不再点名彩虹；
-- 主流程不再点名关卡级元素的实现（只经消息）。全部内置元素都是 instance（条目 33 个：新玩法 2 追加魔法石、新玩法 3 追加毛球，名字与阶段 1 相同由快照锁定）。
ec_flat_record_removed :: Assertion
ec_flat_record_removed = do
  srcFiles <- sourcesUnderAll ["src/Match3/Element", "src/Match3/Board", "src/Match3/Game"]
  assertBool "scanned Element / Board / Game" (all (`elem` srcFiles) ["src/Match3/Element/World.hs", "src/Match3/Element/Builtin/Gem.hs", "src/Match3/Board/Cascade.hs", "src/Match3/Game/Resolve.hs"])
  srcs <- mapM (fmap stripStrings . readFile) srcFiles
  -- 按完整标识符比（第 7 刀的钩子记录 LevelHooks 不是段 4 的封闭钩子 LevelHook）
  let bad = [(f, w) | (f, s) <- zip srcFiles srcs, w <- ["ElementDef", "baseDef", "LevelHook", "HookAbsorb", "HookShift", "HookTeleport", "HookCover", "Caps", "capsOf", "Archetype", "SomeModifier", "Modified", "sendMessage", "handleMessage", "customEntry", "bodyEntry", "SomeMessage", "fromMessage", "LevelElement", "SomeLevelElement", "levelReply", "Registry", "HitResult"], mentionsIdent w s]
  assertEqual "no flat record / closed hooks / old element class" [] bad
  match <- readFile "src/Match3/Board/Match.hs"
  assertBool "findHint no longer names the rainbow" (not ("isRainbow" `isInfixOf` stripStrings match) && "Match3.Rainbow" `notElem` importsOf match)
  flowFiles <- pipelineSources
  flow <- mapM readFile flowFiles
  assertEqual "main flow does not call level element implementations" [] [(f, w) | (f, s) <- zip flowFiles flow, w <- ["stepUfos", "beltMoves", "coverCarpets"], mentionsIdent w s]
  assertEqual "builtin entries" builtinEntryCount (length builtinDefs)
  assertEqual "level elements" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop"] (map mechNameOf builtinMechanics)

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

instance Mechanic Magnet where
  mechName _ = "magnet"
  onRefilled m b acc = Just (acc ++ take 1 [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell b p == mkGem C1], m)

instance Mechanic Pinger where
  mechName _ = "pinger"
  avoidCells _ acc = Just (acc ++ [(0, 0)])

instance Mechanic Doubler where
  mechName _ = "doubler"
  avoidCells _ acc = Just (acc ++ acc)

ec_level_elements_by_message :: Assertion
ec_level_elements_by_message = do
  let reg = registerMechanic (SomeMechanic Magnet) defaultWorld
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = setCell stableBoard (1, 0) (mkGem C5)}
      b1 = setCell (setCell stableBoard (1, 0) (mkGem C5)) (1, 1) (mkGem C5)
      (p1, p2) = ((1, 2), (2, 2))
      (_, o, mt) = resolveSwapWith reg p1 p2 gs0 {gsBoard = b1}
      (_, oD, mtD) = resolveSwapWith defaultWorld p1 p2 gs0 {gsBoard = b1}
      ping r = length <$> queryIn r [] (replicate 7 (0, 0)) avoidCells
      withPinger = registerMechanic (SomeMechanic Pinger) reg
  assertBool "applied" (moveApplied o && moveApplied oD)
  assertBool "magnet adds an absorb wave (ufo also answers)" (length (mtWaves mt) > length (mtWaves mtD))
  assertEqual "magnet does not answer other beats" Nothing (ping reg)
  assertEqual "beat folds the only replier" (Just 8) (ping withPinger)
  assertEqual "beat folds all repliers in registration order" (Just 16) (ping (registerMechanic (SomeMechanic Doubler) withPinger))
  assertEqual "other order" (Just 15) (ping (registerMechanic (SomeMechanic Pinger) (registerMechanic (SomeMechanic Doubler) defaultWorld)))
  assertEqual "nobody answers by default" Nothing (ping defaultWorld)
  assertEqual "registered after the builtins" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop", "magnet"] (map mechNameOf (mechanicDefs reg))

-- | 第 7 刀（7a）验收：带状态的扩展关卡级元素不改主流程就能接入。测试专用「虹吸」开局由 mechStart 给 2 格电量，
-- 每轮补子之后（onRefilled）有电量就吸走盘上最后一颗 C2 宝石并耗 1 格；状态只在 gsLevelElems 里的元素值中，
-- 由结算写回。只 registerMechanic + 用这张表开局 / 走子；去掉注册后状态原样、不再生效。Show 在内置字段后追加
-- gsLevelExtra（内置对局没有这一项，快照不变）。第 7 刀 7b：保留内置飞碟，同一节拍两者都生效（回复折叠：先飞碟、后虹吸）。
newtype Siphon = Siphon Int
  deriving (Eq, Show)

instance Mechanic Siphon where
  mechName _ = "siphon"
  onRefilled (Siphon k) b acc
    | k > 0
    , p : _ <- reverse [q | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let q = (r, c), getCell b q == mkGem C2] =
        Just (acc ++ [p], Siphon (k - 1))
    | otherwise = Nothing
  mechStart _ _ = Siphon 2

ec_level_element_stateful_extension :: Assertion
ec_level_element_stateful_extension = do
  let reg = registerMechanic (SomeMechanic (Siphon 0)) defaultWorld
      gs0 = newGameAtLevelWith reg 0 defaultConfig 7
      charge gs = fmap (\(Siphon k) -> k) (levelState (gsLevelElems gs))
      play r n gs
        | n == (0 :: Int) || gsOver gs /= Nothing = [gs]
        | otherwise = case findHintWith r (gsBoard gs) of
            Nothing -> [gs]
            Just (a, b) -> let (gs', _, _) = resolveSwapWith r a b gs in gs : play r (n - 1) gs'
      states = play reg 12 gs0
      final = last states
  assertEqual "opened in registration order + core ground" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop", "siphon", "ground"] (map mechNameOf (gsLevelElems gs0))
  assertEqual "mechStart gives the charge" (Just 2) (charge gs0)
  assertBool "Show appends the extension state" ("gsLevelExtra = [Siphon 2]" `isInfixOf` show gs0)
  assertBool "builtin games show no extras" (not ("gsLevelExtra" `isInfixOf` show (newGameAtLevel 0 defaultConfig 7)))
  assertEqual "charge only goes down, one per absorb" [2 - gsCount CountUfo g | g <- states] (map (maybe (-1) id . charge) states)
  assertEqual "depleted" (Just 0) (charge final)
  assertEqual "absorbed exactly two cells" 2 (gsCount CountUfo final)
  let bare = removeMechanic "siphon" reg
      finalBare = last (play bare 12 gs0)
  assertEqual "unregistered: state untouched" (Just 2) (charge finalBare)
  assertEqual "unregistered: nothing absorbed" 0 (gsCount CountUfo finalBare)
  -- 飞碟关（第 13 关）：一次 onRefilled 节拍 = 飞碟的吸收 ++ 虹吸的吸收，两者的状态都推进
  let gsU = newGameAtLevelWith reg 12 defaultConfig 1
      gsUD = newGameAtLevel 12 defaultConfig 1
      bU = gsBoard gsU
      (psBoth, hooksBoth) = onAbsorb (levelHooksWith reg (gsLevelElems gsU)) bU
      (psUfo, hooksUfo) = onAbsorb (levelHooksWith defaultWorld (gsLevelElems gsUD)) bU
      lastC2 = last [q | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let q = (r, c), getCell bU q == mkGem C2]
  assertBool "ufo level has ufos" (not (null (levelUfos (gsLevelElems gsU))))
  assertEqual "both absorb in one beat (ufo first)" (psUfo ++ [lastC2]) psBoth
  assertEqual "ufo state advanced as without the siphon" (levelUfos (hookLevel hooksUfo)) (levelUfos (hookLevel hooksBoth))
  assertEqual "siphon state advanced" (Just 1) (fmap (\(Siphon k) -> k) (levelState (hookLevel hooksBoth)))

-- | 自定义元素可以当可匹配的有色宝石：测试专用「星星」（Custom "star" 颜色号，原型 Piece、按颜色匹配）
-- 与同色宝石成三连被消除并按名字计数、进提示；未注册时是惰性占格（打断连线）。
newtype Star = Star Color
  deriving (Eq, Show)

instance Cellular Star where
  nameOf _ = "star"
  toCell (Star c) = Custom "star" (CustomState (fromEnum c))
instance Matchable Star where
  color (Star c) = Just c
instance Hittable Star
instance Movable Star
instance Countable Star where
  counter _ = Just (CountNamed "star")
instance Renders Star

instance Kind Star where
  kindName _ = "star"
  fromCell = fromCustom "star" (Star . colorAt)
  place _ = customPlace "star"

ec_custom_matchable_gem :: Assertion
ec_custom_matchable_gem = do
  let reg = register (kindDef @Star) defaultWorld
      star = Custom "star" (CustomState (fromEnum C5))
      board0 = setCell (setCell stableBoard (1, 0) (mkGem C5)) (1, 1) star
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "star") 1)) 1) {gsBoard = board0}
      (p1, p2) = ((1, 2), (2, 2))
      (gs1, o1, mt1) = resolveSwapWith reg p1 p2 gs0
  assertEqual "star matches as C5" (Just C5) (matchColorWith reg star)
  assertBool "star can be swapped" (not (blocksSwapWith reg star))
  assertBool "move applied" (moveApplied o1)
  w1 <- firstWave mt1
  assertBool "star cleared in the first wave" ((1, 1) `elem` cwCleared w1)
  assertEqual "counted by name" [("star", 1)] (namedCounts (gsCounts gs1))
  assertBool "hint sees the star" (isJust (findHintWith reg board0))
  let (_, oD) = trySwap p1 p2 gs0
  assertEqual "unregistered star is inert" Nothing (matchColorWith defaultWorld star)
  assertBool "unregistered: no match through it" (not (moveApplied oD) || namedCounts (gsCounts (fst (trySwap p1 p2 gs0))) == [])

-- | 注册表检查（元素类重构第 2 刀起由世界的解码探针推导）：mkWorldChecked 把重名 / 同一种格子被多个种类认领 /
-- 种类不认领任何格子暴露成值，内置条目表通过检查；mkWorld 是总函数（空表也能解码，认不出的格子退回惰性占格）。
newtype OtherGem = OtherGem Color
  deriving (Eq, Show)

instance Cellular OtherGem where
  nameOf _ = "other_gem"
  toCell (OtherGem c) = Gem c Normal 0 Nothing
instance Matchable OtherGem
instance Hittable OtherGem
instance Movable OtherGem
instance Countable OtherGem
instance Renders OtherGem

instance Kind OtherGem where
  kindName _ = "other_gem"
  fromCell cell = case cell of
    Gem c Normal _ _ -> Just (OtherGem c)
    _ -> Nothing

-- | 什么格子都不认的种类。
newtype Stray = Stray Int
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Obstacle Stray)

instance Cellular Stray where
  nameOf _ = "stray"
instance Countable Stray
instance Renders Stray

instance Kind Stray where
  kindName _ = "stray"
  fromCell _ = Nothing

ec_registry_checked_slots :: Assertion
ec_registry_checked_slots = do
  let errsOf = either Just (const Nothing) . mkWorldChecked
  assertEqual "builtin defs pass the check" Nothing (errsOf builtinDefs)
  assertEqual "duplicate name" (Just [DuplicateName "dup"]) (errsOf [inertDef "dup", inertDef "dup"])
  assertEqual "shared cell" (Just [SharedCell "cell 0" ["gem", "other_gem"]]) (errsOf (builtinDefs ++ [kindDef @OtherGem]))
  assertEqual "unclaimed" (Just [Unclaimed "stray"]) (errsOf [kindDef @Stray])
  -- 总函数：register 按名字替换仍可用；空注册表解码不崩
  assertEqual "empty registry decodes to inert" "?" (elementName (mkWorld []) (mkGem C1))
  assertEqual "empty registry: no upper layers" 0 (length (upperOf (mkWorld []) (mkIceGem C1 2)))
