-- | 元素类原型（阶段 1）：宝石 / 彩蛋 / 冰层走新机制（Match3.Element.Class），经适配层桥接成旧记录，
-- 与其余旧记录并存。这里证明新旧两条路径的结果一致，并锁定类本身的约定（Eq / Show、修饰器组合、
-- 状态在元素值里、开放消息）。
module Spec.ElementClass
  ( tests
  ) where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import Match3.Board.Match (findHintWith)
import Match3.Core
import Match3.Element
import Match3.Element.Class
  ( Archetype(..)
  , Element(..)
  , Hit(..)
  , Message
  , ModHit(..)
  , Modifier(..)
  , SomeElement(..)
  , SomeMessage(..)
  , fromElement
  , fromMessage
  , modify
  , sendMessage
  )
import qualified Match3.Element.Class as C
import Match3.Element.Prototype
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Move (resolveSwapWith)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

tests :: [TestTree]
tests =
  [ testCase "ec_bridge_fields_match_legacy" ec_bridge_fields_match_legacy
  , testCase "ec_bridge_rules_match_legacy" ec_bridge_rules_match_legacy
  , testCase "ec_registry_paths_agree_in_play" ec_registry_paths_agree_in_play
  , testCase "ec_some_element_eq_show" ec_some_element_eq_show
  , testCase "ec_ice_modifier_composes" ec_ice_modifier_composes
  , testCase "ec_state_lives_in_element_value" ec_state_lives_in_element_value
  , testCase "ec_open_messages" ec_open_messages
  ]

protoNames :: [ElementName]
protoNames = ["gem", "line_h", "line_v", "bomb", "rainbow", "ice", "surprise"]

defIn :: [ElementDef] -> ElementName -> ElementDef
defIn ds n = case [d | d <- ds, edName d == n] of
  (d : _) -> d
  [] -> error ("no def " ++ n)

-- | 样本格：全部颜色 × 种类 × 冰 0..3 × 若干叠层，加彩蛋和几种障碍。
sampleCells :: [Cell]
sampleCells =
  [ Gem c k i ov
  | c <- [minBound .. maxBound]
  , k <- [Normal, LineH, LineV, Bomb, Rainbow]
  , i <- [0 .. 3]
  , ov <- [Nothing, Just Grass, Just (Chain 2), Just (Freeze 1), Just (Curtain 1), Just Steam]
  ]
    ++ [Surprise, Stone 2, Cookie, Custom "x" 1]

-- | 这个定义在注册表里负责的格（本体按槽位；冰层 = 带冰的宝石）。
cellsOf :: ElementDef -> [Cell]
cellsOf d = case edSlot d of
  SlotCell i -> [cell | cell <- sampleCells, cellSlot cell == i]
  SlotIce -> [cell | cell@(Gem _ _ i _) <- sampleCells, i > 0]
  _ -> []

sampleArgs :: [[Arg]]
sampleArgs = [[], [AInt 1], [AInt 3], [AColor C2], [AColor C1, AColor C4], [AInt 1, AInt 0]]

-- | 逐字段比对：类型级字段直接比，逐格字段在该定义负责的全部样本格上比，放置在全部样本格 × 参数上比。
ec_bridge_fields_match_legacy :: Assertion
ec_bridge_fields_match_legacy = forM_ protoNames $ \n -> do
  let old = defIn legacyBuiltinDefs n
      new = defIn builtinDefs n
      lbl f = n ++ "." ++ f
      cells = cellsOf old
  assertEqual (lbl "bridged def is the prototype one") (map edName prototypeDefs) (filter (`elem` map edName prototypeDefs) (map edName builtinDefs))
  assertBool (lbl "has cells") (not (null cells))
  assertEqual (lbl "slot") (edSlot old) (edSlot new)
  assertEqual (lbl "static") (static old) (static new)
  forM_ cells $ \cell -> do
    let at f = lbl f ++ " @ " ++ show cell
    assertEqual (at "color") (edColor old cell) (edColor new cell)
    assertEqual (at "activates") (edActivates old cell) (edActivates new cell)
    assertEqual (at "onHit") (edOnHit old cell) (edOnHit new cell)
    assertEqual (at "keepOnShuffle") (edKeepOnShuffle old cell) (edKeepOnShuffle new cell)
  forM_ [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]] $ \p ->
    assertEqual (lbl "blast " ++ show p) (fmap ($ p) (edBlast old)) (fmap ($ p) (edBlast new))
  forM_ sampleCells $ \cell -> forM_ sampleArgs $ \args ->
    assertEqual (lbl "place " ++ show args ++ " " ++ show cell) (edPlace old args cell) (edPlace new args cell)
  where
    static d =
      ( (edBlocksMatch d, edBlocksSwap d, edFalls d, edPortal d, edDrains d, edStripOnClear d)
      , (edCounter d, edDiffCounter d, edBonusMoves d, edVacatesCarpet d, edRecolorable d, edPushable d)
      , (fmap arOrder (edAdjacent d), fmap (\r -> (erPhase r, erOrder r)) (edEnd d), isJust (edGround d))
      , (fmap srOrder (edSwap d), isJust (edOpen d))
      )

-- | 规则类字段：成对交换规则在含各种特殊块的盘面上逐对比 srFires / srSeeds；彩蛋的开启规则逐前沿比。
ec_bridge_rules_match_legacy :: Assertion
ec_bridge_rules_match_legacy = do
  let specials = [((2, 2), Gem C1 Rainbow 0 Nothing), ((2, 3), Gem C2 LineH 0 Nothing), ((3, 3), Gem C3 LineV 0 Nothing), ((3, 4), Gem C4 Bomb 0 Nothing), ((4, 4), Gem C5 Rainbow 0 Nothing), ((5, 4), Gem C1 Bomb 1 Nothing)]
      board = foldl (\b (p, c) -> setCell b p c) stableBoard specials
      pairs = [((r, c), q) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], q <- [(r, c + 1), (r + 1, c)], inBounds q]
  forM_ ["line_h", "rainbow"] $ \n -> do
    let Just ro = edSwap (defIn legacyBuiltinDefs n)
        Just rn = edSwap (defIn builtinDefs n)
    assertEqual (n ++ " order") (srOrder ro) (srOrder rn)
    forM_ pairs $ \(p1, p2) -> do
      assertEqual (n ++ " fires " ++ show (p1, p2)) (srFires ro board p1 p2) (srFires rn board p1 p2)
      assertEqual (n ++ " seeds " ++ show (p1, p2)) (srSeeds ro board p1 p2) (srSeeds rn board p1 p2)
  let eggs = foldl (\b p -> setCell b p Surprise) board [(0, 0), (1, 5), (6, 6), (7, 1)]
      Just oo = edOpen (defIn legacyBuiltinDefs "surprise")
      Just on = edOpen (defIn builtinDefs "surprise")
  forM_ [[], [(0, 1)], [(1, 4), (6, 5)], [(7, 0), (7, 2), (0, 0)], [(r, c) | r <- [0 .. 7], c <- [0 .. 7]]] $ \front ->
    assertEqual ("open " ++ show front) (showOpen (orOpen oo eggs front)) (showOpen (orOpen on eggs front))
  where
    showOpen (b, e, s) = (boardRowsText b, e, s)
    boardRowsText b = show [getCell b (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

-- | 新旧两份注册表（旧 = 全部旧记录；新 = 内置表，宝石 / 冰 / 彩蛋走新机制）在全部 40 关 × 种子 1–2 ×
-- 12 手（走提示）上逐手相等：提示、交换后的对局状态与整步记录；每手前再试一次锤子。
ec_registry_paths_agree_in_play :: Assertion
ec_registry_paths_agree_in_play = do
  let legacy = foldl (flip registerLevel) (mkRegistry legacyBuiltinDefs) builtinLevelDefs
  forM_ [0 .. length allLevels - 1] $ \li -> forM_ [1, 2] $ \seed -> do
    let go :: Int -> GameState -> Assertion
        go 0 _ = pure ()
        go k gs
          | gsOver gs /= Nothing = pure ()
          | otherwise = do
              let hint = findHintWith defaultRegistry (gsBoard gs)
                  tag = "L" ++ show (li + 1) ++ " seed " ++ show seed ++ " step " ++ show (12 - k)
              assertEqual (tag ++ " hint") (findHintWith legacy (gsBoard gs)) hint
              let hp = (k `mod` boardSize, (k * 3) `mod` boardSize)
              assertEqual (tag ++ " hammer") (show (resolveHammerWith legacy hp gs)) (show (resolveHammerWith defaultRegistry hp gs))
              case hint of
                Nothing -> pure ()
                Just (a, b) -> do
                  let new = resolveSwapWith defaultRegistry a b gs
                      old = resolveSwapWith legacy a b gs
                  assertEqual (tag ++ " swap") (show old) (show new)
                  let (gs', _, _) = new
                  go (k - 1) gs'
    go 12 (newGameAtLevel li (levelConfig (allLevels !! li)) seed)

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

-- | 冰层修饰器：包在宝石外面，组合结果与旧注册表的逐层询问一致，写回格子带冰层数。
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
  let reg = defaultRegistry
  forM_ [Gem c k i ov | c <- [C1, C4], k <- [Normal, Bomb], i <- [1 .. 3], ov <- [Nothing, Just Grass]] $ \cell ->
    assertEqual ("registry directHit " ++ show cell) (directHitWith (foldl (flip registerLevel) (mkRegistry legacyBuiltinDefs) builtinLevelDefs) cell) (directHitWith reg cell)

-- | 测试专用「鸟窝」：状态（剩余命中数）放在元素值里；受击返回新的元素值，写回 Custom "nest" k；
-- 经适配层注册进注册表后，锤子每敲一次减一，最后一下才碎并计数。
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

decodeNest :: Cell -> Maybe Nest
decodeNest cell = case cell of
  Custom "nest" k -> Just (Nest k)
  _ -> Nothing

ec_state_lives_in_element_value :: Assertion
ec_state_lives_in_element_value = do
  let nestDef = bridgeBody SlotCustom (Nest 1) decodeNest (\args _ -> Just (Custom "nest" (case args of (AInt k : _) -> k; _ -> 1)))
      reg = register nestDef defaultRegistry
      p = (3, 3)
      gs0 = (newGame defaultConfig 7) {gsBoard = setCell stableBoard p (Custom "nest" 3), gsHammers = 5}
      hit gs = let (gs', _, _) = resolveHammerWith reg p gs in gs'
      gs1 = hit gs0
      gs2 = hit gs1
      gs3 = hit gs2
  assertEqual "onHit returns the new value" (Absorb (SomeElement (Nest 2))) (onHit (Nest 3))
  assertEqual "first hit" (Custom "nest" 2) (getCell (gsBoard gs1) p)
  assertEqual "second hit" (Custom "nest" 1) (getCell (gsBoard gs2) p)
  assertBool "third hit breaks it" (getCell (gsBoard gs3) p /= Custom "nest" 1 && not (isNest (getCell (gsBoard gs3) p)))
  assertEqual "counted once" [("nest", 1)] (gsElementCounts gs3)
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
