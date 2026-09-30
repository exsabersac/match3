{-# LANGUAGE OverloadedStrings #-}
-- | 元素类（xmonad LayoutClass 风格，Match3.Element.Class）。
--
-- 阶段 2 删掉了扁平 ElementDef 记录，新旧两条路径不能再在同一进程里并排跑；等价性改由阶段 1（9ae6a7b，
-- 新旧记录并存且逐手相等）上生成的元素查询快照 test/golden/element-queries.txt 锁定：全部 *With 查询 ×
-- 样本格、放置、规则表、规则在样例盘上的输出、40 关逐手对局（见 test/golden/ElementQueries.hs）。
-- 另锁定类本身的约定（Eq / Show、修饰器组合、状态在元素值里、开放消息）与阶段 2 消掉的遗留项。
module Spec.ElementClass
  ( tests
  ) where

import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust)
import qualified ElementQueries
import Match3.Board.Match (findHintWith)
import Match3.Core
import Match3.Element
import Match3.Element.Class
  ( Archetype(..)
  , Element(..)
  , Hit(..)
  , LevelElement(..)
  , Message
  , ModHit(..)
  , Modifier(..)
  , SomeElement(..)
  , SomeLevelElement(..)
  , SomeMessage(..)
  , fromElement
  , fromMessage
  , levelNameOf
  , modify
  , sendMessage
  , activates, archetype, blocksSwap, color, falls, hintable, keepOnShuffle, matchColor, onHit, portal, pushable, recolorable
  )
import Match3.Element.Caps (blocker, colorIs, counts, hit, onMessage, piece)
import qualified Match3.Element.Class as C
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Element.Message (Refilled(..))
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Level (newGameAtLevelWith)
import Match3.Game.Move (resolveSwapWith)
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
  , testCase "ec_ice_modifier_composes" ec_ice_modifier_composes
  , testCase "ec_state_lives_in_element_value" ec_state_lives_in_element_value
  , testCase "ec_open_messages" ec_open_messages
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

-- | SomeElement 的 Eq 先比元素名再比状态，Show 稳定（元素名 + 状态值）。
ec_some_element_eq_show :: Assertion
ec_some_element_eq_show = do
  assertEqual "same name same state" (SomeElement (PlainGem C1)) (SomeElement (PlainGem C1))
  assertBool "same type other state" (SomeElement (PlainGem C1) /= SomeElement (PlainGem C2))
  assertBool "other element" (SomeElement (SpecialGem C1 LineH) /= SomeElement (SpecialGem C1 LineV))
  assertBool "other type" (SomeElement (PlainGem C1) /= SomeElement SurpriseEgg)
  assertEqual "show gem" "SomeElement \"gem\" (PlainGem C1)" (show (SomeElement (PlainGem C1)))
  assertEqual "show modified" "SomeElement \"gem\" (Modified (SomeModifier \"ice\" (Ice 2)) (SomeElement \"gem\" (PlainGem C3)))" (show (modify (Ice 2) (SomeElement (PlainGem C3))))
  assertEqual "unbox" (Just (PlainGem C4)) (fromElement (SomeElement (PlainGem C4)))
  assertEqual "unbox wrong type" (Nothing :: Maybe SurpriseEgg) (fromElement (SomeElement (PlainGem C4)))
  -- 宝石全用默认方法：原型 Piece，颜色取自写回的格子
  let g = PlainGem C2
  assertEqual "gem archetype" Piece (archetype g)
  assertEqual "gem color" (Just C2) (color g)
  assertEqual "gem defaults" (False, Just True, True, True, Destroy, True, True, False, True) (blocksSwap g, activates g, falls g, portal g, onHit g, recolorable g, pushable g, keepOnShuffle g, hintable g)
  assertBool "rainbow not hintable" (not (hintable (SpecialGem C1 Rainbow)))
  assertEqual "egg is a blocker" (True, Just False, Nothing, True) (blocksSwap SurpriseEgg, activates SurpriseEgg, color SurpriseEgg, keepOnShuffle SurpriseEgg)
  -- 注册表把格子解码成元素值
  assertEqual "decode gem" (SomeElement (PlainGem C5)) (bodyOf defaultRegistry (mkGem C5))
  assertEqual "decode iced line" (modify (Ice 2) (SomeElement (SpecialGem C1 LineV))) (elementOf defaultRegistry (Gem C1 LineV 2 Nothing))

-- | 第 6b 刀：SomeElement / SomeModifier 的相等按具体类型（Typeable cast）+ 该类型的 Eq，不再比较名字字符串。
-- 两个同名（"twin"）而类型不同的测试元素不相等；同类型同状态相等、不同状态不等；名字是 ElementName（newtype）。
ec_some_element_eq_by_type :: Assertion
ec_some_element_eq_by_type = do
  assertEqual "same type same state" (SomeElement (TwinA 1)) (SomeElement (TwinA 1))
  assertBool "same type other state" (SomeElement (TwinA 1) /= SomeElement (TwinA 2))
  assertEqual "names collide" (name (TwinA 1)) (name (TwinB 1))
  assertBool "same name, other type" (SomeElement (TwinA 1) /= SomeElement (TwinB 1))
  assertBool "same name, other type (flipped)" (SomeElement (TwinB 1) /= SomeElement (TwinA 1))
  assertEqual "same cell, still other type" (toCell (TwinA 1)) (toCell (TwinB 1))
  assertEqual "boxed twice" (SomeElement (SomeElement (TwinA 3))) (SomeElement (SomeElement (TwinA 3)))
  assertEqual "modifier same" (C.SomeModifier (Ice 2)) (C.SomeModifier (Ice 2))
  assertBool "modifier other state" (C.SomeModifier (Ice 1) /= C.SomeModifier (Ice 2))
  assertEqual "name is a newtype with String's Show" "\"twin\"" (show (name (TwinA 1)))
  assertEqual "unElementName" "twin" (unElementName (name (TwinB 1)))

-- | 测试专用的两个同名元素类型（只用来检查 SomeElement 的相等不看名字字符串）。
newtype TwinA = TwinA Int
  deriving (Eq, Show)

newtype TwinB = TwinB Int
  deriving (Eq, Show)

instance Element TwinA where
  name _ = "twin"
  toCell (TwinA k) = Custom "twin" (CustomState k)
  caps _ = blocker []

instance Element TwinB where
  name _ = "twin"
  toCell (TwinB k) = Custom "twin" (CustomState k)
  caps _ = blocker []

-- | 冰层修饰器：包在宝石外面，组合结果与逐层询问一致，写回格子带冰层数。
ec_ice_modifier_composes :: Assertion
ec_ice_modifier_composes = do
  let iced n k = modify (Ice n) (SomeElement (SpecialGem C3 k))
      plain n = modify (Ice n) (SomeElement (PlainGem C3))
  assertEqual "encode" (Gem C3 Normal 2 Nothing) (toCell (plain 2))
  assertEqual "encode special" (Gem C3 Bomb 1 Nothing) (toCell (iced 1 Bomb))
  assertEqual "match color passes" (Just C3) (matchColor (plain 2))
  assertEqual "name is the body's" "bomb" (C.name (iced 1 Bomb))
  assertEqual "multi-ice does not fire" (Just False) (activates (iced 2 Bomb))
  assertEqual "last ice fires" (Just True) (activates (iced 1 Bomb))
  assertEqual "hit peels one ice" (Absorb (plain 1)) (onHit (plain 2))
  assertEqual "peeled cell" (Gem C3 Normal 1 Nothing) (case onHit (plain 2) of Absorb e -> toCell e; _ -> Surprise)
  assertEqual "last ice shatters with the gem" Destroy (onHit (plain 1))
  assertBool "iced normal gem kept on shuffle" (keepOnShuffle (plain 1))
  assertEqual "modifier hit" (Keep (Ice 1)) (modOnHit (Ice 2))
  forM_ [(c, k, i, ov) | c <- [C1, C4], k <- [Normal, Bomb], i <- [1 .. 3], ov <- [Nothing, Just Grass]] $ \(c, k, i, ov) -> do
    let cell = Gem c k i ov
    assertEqual ("registry directHit " ++ show cell) (if i > 1 then HitAbsorb (Gem c k (i - 1) ov) else HitDestroy) (directHitWith defaultRegistry cell)

-- | 测试专用「鸟窝」：状态（剩余命中数）放在元素值里；受击返回新的元素值，写回 Custom "nest" k；
-- 注册进注册表后，锤子每敲一次减一，最后一下才碎并计数。
newtype Nest = Nest Int
  deriving (Eq, Show)

instance Element Nest where
  name _ = "nest"
  toCell (Nest k) = Custom "nest" (CustomState k)
  caps (Nest k) =
    blocker
      [ hit (if k > 1 then Absorb (SomeElement (Nest (k - 1))) else Destroy)
      , counts (CountNamed "nest")
      , onMessage (\msg -> case fromMessage msg of
          Just (Warm d) -> Just (SomeElement (Nest (k + d)))
          Nothing -> Nothing)
      ]

ec_state_lives_in_element_value :: Assertion
ec_state_lives_in_element_value = do
  let reg = register (customEntry (Nest 1) (Nest . unCustomState)) defaultRegistry
      p = (3, 3)
      gs0 = (newGame defaultConfig 7) {gsBoard = setCell stableBoard p (Custom "nest" (CustomState 3)), gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith reg p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
      gs3 = hammer gs2
  assertEqual "onHit returns the new value" (Absorb (SomeElement (Nest 2))) (onHit (Nest 3))
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

-- | 开放消息：任何模块都能定义消息类型；不认识的消息原样返回，修饰器把消息转给里面。
newtype Warm = Warm Int

instance Message Warm

newtype Other = Other ()

instance Message Other

ec_open_messages :: Assertion
ec_open_messages = do
  assertEqual "handled" (SomeElement (Nest 5)) (sendMessage (Warm 2) (SomeElement (Nest 3)))
  assertEqual "ignored" (SomeElement (Nest 3)) (sendMessage (Other ()) (SomeElement (Nest 3)))
  assertEqual "gem ignores" (SomeElement (PlainGem C1)) (sendMessage (Warm 2) (SomeElement (PlainGem C1)))
  assertEqual "through the ice modifier" (modify (Ice 2) (SomeElement (Nest 4))) (sendMessage (Warm 1) (modify (Ice 2) (SomeElement (Nest 3))))
  assertBool "fromMessage type check" (isJust (fromMessage (SomeMessage (Warm 1)) :: Maybe Warm))
  assertBool "fromMessage wrong type" (not (isJust (fromMessage (SomeMessage (Other ())) :: Maybe Warm)))

-- | 阶段 2 消掉的遗留项：源码里没有扁平记录 / 封闭钩子；findHint 不再点名彩虹；
-- 主流程不再点名关卡级元素的实现（只经消息）。全部内置元素都是 instance（条目 33 个：新玩法 2 追加魔法石、新玩法 3 追加毛球，名字与阶段 1 相同由快照锁定）。
ec_flat_record_removed :: Assertion
ec_flat_record_removed = do
  srcFiles <- sourcesUnderAll ["src/Match3/Element", "src/Match3/Board", "src/Match3/Game"]
  assertBool "scanned Element / Board / Game" (all (`elem` srcFiles) ["src/Match3/Element/Registry.hs", "src/Match3/Element/Builtin/Gem.hs", "src/Match3/Board/Cascade.hs", "src/Match3/Game/Resolve.hs"])
  srcs <- mapM (fmap stripStrings . readFile) srcFiles
  -- 按完整标识符比（第 7 刀的钩子记录 LevelHooks 不是段 4 的封闭钩子 LevelHook）
  let bad = [(f, w) | (f, s) <- zip srcFiles srcs, w <- ["ElementDef", "baseDef", "LevelHook", "HookAbsorb", "HookShift", "HookTeleport", "HookCover"], mentionsIdent w s]
  assertEqual "no flat record / closed hooks" [] bad
  match <- readFile "src/Match3/Board/Match.hs"
  assertBool "findHint no longer names the rainbow" (not ("isRainbow" `isInfixOf` stripStrings match) && "Match3.Rainbow" `notElem` importsOf match)
  flowFiles <- pipelineSources
  flow <- mapM readFile flowFiles
  assertEqual "main flow does not call level element implementations" [] [(f, w) | (f, s) <- zip flowFiles flow, w <- ["stepUfos", "beltMoves", "coverCarpets"], mentionsIdent w s]
  assertEqual "34 builtin entries" 34 (length builtinDefs)
  assertEqual "level elements" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop"] (map levelNameOf builtinLevelDefs)

-- | 关卡级元素是开放的：测试专用「磁铁」在补子之后的节拍（Refilled）吸走盘上第一颗 C1 宝石；
-- 不改主流程，只 registerLevel（无状态：开局没有它时用注册的原型值）。第 7 刀 7b 起节拍折叠所有回复者：
-- 保留内置飞碟（本局没有飞碟，回复空）时磁铁照样生效。新消息类型（Ping）也能经 askLevels 发给关卡级元素，
-- 多个回复者按注册顺序折叠（磁铁 +1、倍增器 ×2）。
data Magnet = Magnet
  deriving (Eq, Show)

data Doubler = Doubler
  deriving (Eq, Show)

newtype Ping = Ping Int

instance Message Ping

instance LevelElement Magnet where
  levelName _ = "magnet"
  levelReply m msg
    | Just (Refilled b acc) <- fromMessage msg =
        Just (SomeMessage (Refilled b (acc ++ take 1 [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell b p == mkGem C1])), m)
    | Just (Ping n) <- fromMessage msg = Just (SomeMessage (Ping (n + 1)), m)
    | otherwise = Nothing

instance LevelElement Doubler where
  levelName _ = "doubler"
  levelReply d msg
    | Just (Ping n) <- fromMessage msg = Just (SomeMessage (Ping (n * 2)), d)
    | otherwise = Nothing

ec_level_elements_by_message :: Assertion
ec_level_elements_by_message = do
  let reg = registerLevel (SomeLevelElement Magnet) defaultRegistry
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = setCell stableBoard (1, 0) (mkGem C5)}
      b1 = setCell (setCell stableBoard (1, 0) (mkGem C5)) (1, 1) (mkGem C5)
      (p1, p2) = ((1, 2), (2, 2))
      (_, o, mt) = resolveSwapWith reg p1 p2 gs0 {gsBoard = b1}
      (_, oD, mtD) = resolveSwapWith defaultRegistry p1 p2 gs0 {gsBoard = b1}
      ping r = fmap (\(Ping n) -> n) (askLevels r (Ping 7))
  assertBool "applied" (moveApplied o && moveApplied oD)
  assertBool "magnet adds an absorb wave (ufo also answers)" (length (mtWaves mt) > length (mtWaves mtD))
  assertEqual "ping folds the only replier" (Just 8) (ping reg)
  assertEqual "ping folds all repliers in registration order" (Just 16) (ping (registerLevel (SomeLevelElement Doubler) reg))
  assertEqual "other order" (Just 15) (ping (registerLevel (SomeLevelElement Magnet) (registerLevel (SomeLevelElement Doubler) defaultRegistry)))
  assertEqual "nobody answers ping by default" Nothing (ping defaultRegistry)
  assertEqual "registered after the builtins" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop", "magnet"] (map levelNameOf (levelDefs reg))

-- | 第 7 刀（7a）验收：带状态的扩展关卡级元素不改主流程就能接入。测试专用「虹吸」开局由 levelStart 给 2 格电量，
-- 每轮补子之后（Refilled）有电量就吸走盘上最后一颗 C2 宝石并耗 1 格；状态只在 gsLevelElems 里的元素值中，
-- 由结算写回。只 registerLevel + 用这张表开局 / 走子；去掉注册后状态原样、不再生效。Show 在内置字段后追加
-- gsLevelExtra（内置对局没有这一项，快照不变）。第 7 刀 7b：保留内置飞碟，同一节拍两者都生效（回复折叠：先飞碟、后虹吸）。
newtype Siphon = Siphon Int
  deriving (Eq, Show)

instance LevelElement Siphon where
  levelName _ = "siphon"
  levelReply (Siphon k) msg
    | k > 0
    , Just (Refilled b acc) <- fromMessage msg
    , p : _ <- reverse [q | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let q = (r, c), getCell b q == mkGem C2] =
        Just (SomeMessage (Refilled b (acc ++ [p])), Siphon (k - 1))
    | otherwise = Nothing
  levelStart _ _ = Siphon 2

ec_level_element_stateful_extension :: Assertion
ec_level_element_stateful_extension = do
  let reg = registerLevel (SomeLevelElement (Siphon 0)) defaultRegistry
      gs0 = newGameAtLevelWith reg 0 defaultConfig 7
      charge gs = fmap (\(Siphon k) -> k) (levelState (gsLevelElems gs))
      play r n gs
        | n == (0 :: Int) || gsOver gs /= Nothing = [gs]
        | otherwise = case findHintWith r (gsBoard gs) of
            Nothing -> [gs]
            Just (a, b) -> let (gs', _, _) = resolveSwapWith r a b gs in gs : play r (n - 1) gs'
      states = play reg 12 gs0
      final = last states
  assertEqual "opened in registration order + core ground" ["ufo", "belt", "portal", "carpet", "bomb_shapes", "rainbow_combos", "cookie_drop", "siphon", "ground"] (map levelNameOf (gsLevelElems gs0))
  assertEqual "levelStart gives the charge" (Just 2) (charge gs0)
  assertBool "Show appends the extension state" ("gsLevelExtra = [Siphon 2]" `isInfixOf` show gs0)
  assertBool "builtin games show no extras" (not ("gsLevelExtra" `isInfixOf` show (newGameAtLevel 0 defaultConfig 7)))
  assertEqual "charge only goes down, one per absorb" [2 - gsCount CountUfo g | g <- states] (map (maybe (-1) id . charge) states)
  assertEqual "depleted" (Just 0) (charge final)
  assertEqual "absorbed exactly two cells" 2 (gsCount CountUfo final)
  let bare = removeLevel "siphon" reg
      finalBare = last (play bare 12 gs0)
  assertEqual "unregistered: state untouched" (Just 2) (charge finalBare)
  assertEqual "unregistered: nothing absorbed" 0 (gsCount CountUfo finalBare)
  -- 飞碟关（第 13 关）：一次 Refilled 节拍 = 飞碟的吸收 ++ 虹吸的吸收，两者的状态都推进
  let gsU = newGameAtLevelWith reg 12 defaultConfig 1
      gsUD = newGameAtLevel 12 defaultConfig 1
      bU = gsBoard gsU
      (psBoth, hooksBoth) = onAbsorb (levelHooksWith reg (gsLevelElems gsU)) bU
      (psUfo, hooksUfo) = onAbsorb (levelHooksWith defaultRegistry (gsLevelElems gsUD)) bU
      lastC2 = last [q | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let q = (r, c), getCell bU q == mkGem C2]
  assertBool "ufo level has ufos" (not (null (levelUfos (gsLevelElems gsU))))
  assertEqual "both absorb in one beat (ufo first)" (psUfo ++ [lastC2]) psBoth
  assertEqual "ufo state advanced as without the siphon" (levelUfos (hookLevel hooksUfo)) (levelUfos (hookLevel hooksBoth))
  assertEqual "siphon state advanced" (Just 1) (fmap (\(Siphon k) -> k) (levelState (hookLevel hooksBoth)))

-- | 自定义元素可以当可匹配的有色宝石：测试专用「星星」（Custom "star" 颜色号，原型 Piece、按颜色匹配）
-- 与同色宝石成三连被消除并按名字计数、进提示；未注册时是惰性占格（打断连线）。
newtype Star = Star Color
  deriving (Eq, Show)

instance Element Star where
  name _ = "star"
  toCell (Star c) = Custom "star" (CustomState (fromEnum c))
  caps (Star c) = piece [colorIs c, counts (CountNamed "star")]

ec_custom_matchable_gem :: Assertion
ec_custom_matchable_gem = do
  let reg = register (customEntry (Star C5) (Star . colorAt . unCustomState)) defaultRegistry
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
  assertEqual "unregistered star is inert" Nothing (matchColorWith defaultRegistry star)
  assertBool "unregistered: no match through it" (not (moveApplied oD) || namedCounts (gsCounts (fst (trySwap p1 p2 gs0))) == [])

-- | 注册表条目的槽位由原型推导；mkRegistryChecked 把重名 / 槽位冲突 / 推不出槽位暴露成值，
-- 内置条目表通过检查；mkRegistry 是总函数（空表也能解码，查不到的槽位退回惰性占格）。
newtype OtherGem = OtherGem Color
  deriving (Eq, Show)

instance Element OtherGem where
  name _ = "other_gem"
  toCell (OtherGem c) = Gem c Normal 0 Nothing

ec_registry_checked_slots :: Assertion
ec_registry_checked_slots = do
  let errsOf = either Just (const Nothing) . mkRegistryChecked
      otherGem = bodyEntry (OtherGem C1) (const Nothing) (\_ _ -> Nothing)
      stray = bodyEntry (C.Inert "stray" (Custom "stray" (CustomState 1))) (const Nothing) (\_ _ -> Nothing)
      slotOf n = [entrySlot e | e <- builtinDefs, entryName e == n]
  assertEqual "builtin defs pass the check" Nothing (errsOf builtinDefs)
  assertEqual "gem slot derived from prototype" [SlotCell 0] (slotOf "gem")
  assertEqual "countdown slot derived from prototype" [SlotCell (cellSlot (Countdown C1 1))] (slotOf "countdown")
  assertEqual "ice slot derived from prototype" [SlotIce] (slotOf "ice")
  assertEqual "steam slot derived from prototype" [SlotOverlay (overlaySlot Steam)] (slotOf "steam")
  assertEqual "duplicate name" (Just [DuplicateName "dup"]) (errsOf [inertEntry "dup", inertEntry "dup"])
  assertEqual "duplicate slot" (Just [DuplicateSlot (SlotCell 0) ["gem", "other_gem"]]) (errsOf (builtinDefs ++ [otherGem]))
  assertEqual "no slot" (Just [NoSlot "stray"]) (errsOf [stray])
  assertEqual "stray entry has no slot" SlotNone (entrySlot stray)
  -- 总函数：register 按名字替换仍可用；空注册表解码不崩
  assertEqual "empty registry decodes to inert" "?" (elementName (mkRegistry []) (mkGem C1))
  assertEqual "empty registry: no upper layers" 0 (length (upperOf (mkRegistry []) (mkIceGem C1 2)))
