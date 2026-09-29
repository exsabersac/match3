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
  , SomeLevel(..)
  , SomeMessage(..)
  , fromElement
  , fromMessage
  , levelNameOf
  , modify
  , sendMessage
  )
import qualified Match3.Element.Class as C
import Match3.Element.Message (Absorbed(..), Refilled(..))
import Match3.Game.Boosters (resolveHammerWith)
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
  , testCase "ec_ice_modifier_composes" ec_ice_modifier_composes
  , testCase "ec_state_lives_in_element_value" ec_state_lives_in_element_value
  , testCase "ec_open_messages" ec_open_messages
  , testCase "ec_flat_record_removed" ec_flat_record_removed
  , testCase "ec_level_elements_by_message" ec_level_elements_by_message
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
  toCell (Nest k) = Custom "nest" k
  archetype _ = Blocker
  onHit (Nest k)
    | k > 1 = Absorb (SomeElement (Nest (k - 1)))
    | otherwise = Destroy
  counter _ = Just (CountNamed "nest")
  handleMessage (Nest k) msg = case fromMessage msg of
    Just (Warm d) -> Just (SomeElement (Nest (k + d)))
    Nothing -> Nothing

ec_state_lives_in_element_value :: Assertion
ec_state_lives_in_element_value = do
  let reg = register (customEntry (Nest 1) Nest) defaultRegistry
      p = (3, 3)
      gs0 = (newGame defaultConfig 7) {gsBoard = setCell stableBoard p (Custom "nest" 3), gsHammers = 5}
      hit gs = let (gs', _, _) = resolveHammerWith reg p gs in gs'
      gs1 = hit gs0
      gs2 = hit gs1
      gs3 = hit gs2
  assertEqual "onHit returns the new value" (Absorb (SomeElement (Nest 2))) (onHit (Nest 3))
  assertEqual "decoded state" (SomeElement (Nest 3)) (bodyOf reg (Custom "nest" 3))
  assertEqual "first hit" (Custom "nest" 2) (getCell (gsBoard gs1) p)
  assertEqual "second hit" (Custom "nest" 1) (getCell (gsBoard gs2) p)
  assertBool "third hit breaks it" (not (isNest (getCell (gsBoard gs3) p)))
  assertEqual "counted once" [("nest", 1)] (namedCounts (gsCounts gs3))
  assertEqual "placed via the constructor" (Custom "nest" 2) (getCell (placeWith reg "nest" [AInt 2] stableBoard [p]) p)
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
-- 主流程不再点名关卡级元素的实现（只经消息）。全部内置元素都是 instance（条目 31 个，名字与阶段 1 相同由快照锁定）。
ec_flat_record_removed :: Assertion
ec_flat_record_removed = do
  srcFiles <- sourcesUnderAll ["src/Match3/Element", "src/Match3/Board", "src/Match3/Game"]
  assertBool "scanned Element / Board / Game" (all (`elem` srcFiles) ["src/Match3/Element/Registry.hs", "src/Match3/Element/Builtin/Gem.hs", "src/Match3/Board/Cascade.hs", "src/Match3/Game/Resolve.hs"])
  srcs <- mapM (fmap stripStrings . readFile) srcFiles
  let bad = [(f, w) | (f, s) <- zip srcFiles srcs, w <- ["ElementDef", "baseDef", "LevelHook", "HookAbsorb", "HookShift", "HookTeleport", "HookCover"], w `isInfixOf` s]
  assertEqual "no flat record / closed hooks" [] bad
  match <- readFile "src/Match3/Board/Match.hs"
  assertBool "findHint no longer names the rainbow" (not ("isRainbow" `isInfixOf` stripStrings match) && "Match3.Rainbow" `notElem` importsOf match)
  flowFiles <- pipelineSources
  flow <- mapM readFile flowFiles
  assertEqual "main flow does not call level element implementations" [] [(f, w) | (f, s) <- zip flowFiles flow, w <- ["stepUfos", "beltMoves", "coverCarpets"], mentionsIdent w s]
  assertEqual "31 builtin entries" 31 (length builtinDefs)
  assertEqual "level elements" ["ufo", "belt", "portal", "carpet"] (map levelNameOf builtinLevelDefs)

-- | 关卡级元素是开放的：测试专用「磁铁」在补子之后的节拍（Refilled）吸走盘上第一颗 C1 宝石；
-- 不改主流程，只 registerLevel。新消息类型（Ping）也能经 askLevel 发给关卡级元素。
data Magnet = Magnet

newtype Ping = Ping Int

newtype Pong = Pong Int

instance Message Ping
instance Message Pong

instance LevelElement Magnet where
  levelName _ = "magnet"
  levelReply _ msg
    | Just (Refilled us b) <- fromMessage msg =
        Just (SomeMessage (Absorbed (take 1 [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell b p == mkGem C1]) us))
    | Just (Ping n) <- fromMessage msg = Just (SomeMessage (Pong (n + 1)))
    | otherwise = Nothing

ec_level_elements_by_message :: Assertion
ec_level_elements_by_message = do
  let reg = registerLevel (SomeLevel Magnet) (removeLevel "ufo" defaultRegistry)
      gs0 = (newGame (GameConfig 5 (goalScore 99999)) 1) {gsBoard = setCell stableBoard (1, 0) (mkGem C5)}
      b1 = setCell (setCell stableBoard (1, 0) (mkGem C5)) (1, 1) (mkGem C5)
      (p1, p2) = ((1, 2), (2, 2))
      (_, o, mt) = resolveSwapWith reg p1 p2 gs0 {gsBoard = b1}
      (_, oD, mtD) = resolveSwapWith defaultRegistry p1 p2 gs0 {gsBoard = b1}
  assertBool "applied" (moveApplied o && moveApplied oD)
  assertBool "magnet adds an absorb wave" (length (mtWaves mt) > length (mtWaves mtD))
  assertEqual "ping / pong" (Just 8) (fmap (\(Pong n) -> n) (askLevel reg (Ping 7)))
  assertEqual "nobody answers ping by default" Nothing (fmap (\(Pong n) -> n) (askLevel defaultRegistry (Ping 7)))
  assertEqual "registered after the builtins" ["belt", "portal", "carpet", "magnet"] (map levelNameOf (levelDefs reg))

-- | 自定义元素可以当可匹配的有色宝石：测试专用「星星」（Custom "star" 颜色号，原型 Piece、按颜色匹配）
-- 与同色宝石成三连被消除并按名字计数、进提示；未注册时是惰性占格（打断连线）。
newtype Star = Star Color
  deriving (Eq, Show)

instance Element Star where
  name _ = "star"
  toCell (Star c) = Custom "star" (fromEnum c)
  color (Star c) = Just c
  counter _ = Just (CountNamed "star")

ec_custom_matchable_gem :: Assertion
ec_custom_matchable_gem = do
  let reg = register (customEntry (Star C5) (Star . colorAt)) defaultRegistry
      star = Custom "star" (fromEnum C5)
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
      stray = bodyEntry (C.Inert "stray" (Custom "stray" 1)) (const Nothing) (\_ _ -> Nothing)
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
