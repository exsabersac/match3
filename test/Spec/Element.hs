{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 元素注册表：测试专用木箱证明可扩展，内置表与旧谓词逐格一致。
module Spec.Element
  ( tests
  ) where

import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Match3.Board.Grid (setCell)
import Match3.Core
import Match3.Counts (namedCounts)
import Match3.Element
  ( Strike(Absorb, Destroy)
  , activatesWith
  , blocksSwapWith
  , directHitWith
  , hitImmuneWith
  , keepOnShuffleWith
  , lookupDef
  , matchColorWith
  , register
  , worldDefs
  )
import Match3.Element.Event (EventKind(..), Event(..))
import Match3.Element.World (swapBlockedWith)
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Level (newGame)
import Match3.Game.Move (resolveSwapWith, trySwap, trySwapWith)
import Match3.Game.Shuffle (CellDecor(..), extractDecorWith)
import Match3.Game.Trace (traceEventsWith)
import Match3.Types
  ( hasChain
  , hasFreeze
  , isBalloon
  , isBottle
  , isCake
  , isChest
  , isCookie
  , isHoney
  , isMagicHat
  , isMaker
  , isSafe
  , isSnail
  , isStone
  , isSurprise
  , isTimeSpirit
  , specialActivates
  )
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（平铺进顶层 "match3" 组）。
tests :: [TestTree]
tests =
  [ testCase "element_registry_custom_crate_extensibility" element_registry_custom_crate_extensibility
  , testCase "element_registry_matches_legacy_predicates" element_registry_matches_legacy_predicates
  ]

-- | 扩展性验收：测试专用元素只经注册表接入，跑一局并断言它按定义起作用；同时证明主流程没有为它改动。
element_registry_custom_crate_extensibility :: Assertion
element_registry_custom_crate_extensibility = do
  let world = register crateDef defaultWorld
      gs0 = (newGame defaultConfig 1) {gsBoard = crateBoard 2}
  -- 注册表里有它，内置定义一个不少
  assertBool "registered" (isJust (lookupDef world "crate"))
  assertEqual "builtins kept" (length (worldDefs defaultWorld) + 1) (length (worldDefs world))
  -- 挡交换（固定格原型），不可匹配
  let (gsB, oB) = trySwapWith world (0, 1) (0, 2) gs0
  assertEqual "crate blocks swap" NoMatch oB
  assertEqual "blocked swap leaves board" (gsBoard gs0) (gsBoard gsB)
  assertEqual "no match colour" Nothing (matchColorWith world (Custom "crate" (CustomState 2)))
  -- 第 1 手：邻格真消除波及一次 → 耐久 2 → 1，不碎、不计数；木箱不随重力下落（仍在 (0,1)）
  let (gs1, o1, mt1) = resolveSwapWith world (1, 2) (2, 2) gs0
  assertBool "move 1 applied" (moveApplied o1)
  assertEqual "move 1: crate chipped once, stays put" [((0, 1), Custom "crate" (CustomState 1))] (cratesOn (gsBoard gs1))
  assertEqual "move 1: not counted yet" [] (namedCounts (gsCounts gs1))
  w1 <- firstWave mt1
  assertBool "move 1: crate absent from first-wave clears" ((0, 1) `notElem` cwCleared w1)
  assertBool "move 1: EvHit on crate"
    (any (\e -> evKind e == EvHit && evElement e == "crate" && ((0, 1), (0, 1)) `elem` evCells e) (traceEventsWith world mt1))
  -- 第 2 手：同样的局面（耐久 1）再波及一次 → 碎，进入清除格并计数
  let gs1' = gs1 {gsBoard = crateBoard 1}
      (gs2, o2, mt2) = resolveSwapWith world (1, 2) (2, 2) gs1'
  assertBool "move 2 applied" (moveApplied o2)
  assertEqual "move 2: crate broken" [] (cratesOn (gsBoard gs2))
  w2 <- firstWave mt2
  assertBool "move 2: crate in first-wave clears" ((0, 1) `elem` cwCleared w2)
  assertEqual "move 2: counted by name" [("crate", 1)] (namedCounts (gsCounts gs2))
  assertBool "move 2: EvClear of crate"
    (any (\e -> evKind e == EvClear && evElement e == "crate") (traceEventsWith world mt2))
  -- 直接命中（锤子）：耐久 -1；洗牌保留
  let (gsH, oH, _) = resolveHammerWith world (0, 1) gs0
  assertBool "hammer applied" (moveApplied oH)
  assertEqual "hammer chips crate" [((0, 1), Custom "crate" (CustomState 1))] (cratesOn (gsBoard gsH))
  assertBool "shuffle keeps crate" ((0, 1) `elem` map cdPos (extractDecorWith world (gsBoard gs0)))
  -- 对照：不注册时同一局面里它只是惰性占格（打不动、不被波及、不计数），行为完全来自注册
  let (gsD, oD) = trySwap (1, 2) (2, 2) gs0
  assertBool "default applied" (moveApplied oD)
  assertEqual "unregistered: inert, untouched" [Custom "crate" (CustomState 2)] (map snd (cratesOn (gsBoard gsD)))
  assertBool "unregistered: hammer immune" (hitImmuneWith defaultWorld (Custom "crate" (CustomState 2)))
  assertEqual "unregistered: not counted" [] (namedCounts (gsCounts gsD))
  -- 主流程没有为它改动：src/ 与 app/ 下全部源码（含注释）里都没有这个元素名的字面量 "crate" 或「木箱」
  coreFiles <- sourcesUnderAll ["src", "app"]
  assertBool "scanned the core sources" ("src/Match3/Board/Cascade.hs" `elem` coreFiles && "src/Match3/Element/Builtin.hs" `elem` coreFiles)
  srcs <- mapM readFile coreFiles
  let mentions = [f | (f, src) <- zip coreFiles srcs, show ("crate" :: String) `isInfixOf` src || "木箱" `isInfixOf` src]
  assertEqual "core sources do not mention the test element" [] mentions

-- | 注册表的查询与第二刀之前按构造器写死的谓词逐格等价（对所有内置本体 × 冰层 × 叠层）。
element_registry_matches_legacy_predicates :: Assertion
element_registry_matches_legacy_predicates = do
  let world = defaultWorld
      overlays = Nothing : map Just [Grass, Vine, Choco, Fog 1, Fog 2, Chain 1, Chain 2, Freeze 1, Freeze 2, Curtain 1, Curtain 2, Steam]
      gems = [Gem col k ice ov | col <- [C1, C4], k <- [Normal, LineH, LineV, Bomb, Rainbow], ice <- [0, 1, 2], ov <- overlays]
      others =
        [ Stone 1, Stone 3, Chest 1, Chest 2, Honey 1, Honey 2, Balloon C2, Cookie, Cake 1, Cake 3, MagicHat
        , Maker C1 1, Maker C3 3, Snail 0 1, Snail 1 0, Safe 1, Safe 2, Flip C1 C3, Surprise, Bottle C2
        , TimeSpirit, Countdown C4 1, Countdown C2 3 ]
      cells = gems ++ others
      legacyBlock c =
        isStone c || isChest c || isHoney c || isBalloon c || isCookie c
          || isCake c || isMagicHat c || isMaker c || isSnail c || isSafe c
          || isSurprise c || isBottle c || isTimeSpirit c
          || hasChain c || hasFreeze c
      legacyImmune c = isMaker c || isSnail c || isBottle c || isMagicHat c || isCookie c
      legacyFixed c = isBottle c || isMaker c || isMagicHat c || isSnail c
      legacyMatch c = case c of
        Gem _ _ _ (Just (Fog _)) -> Nothing
        Gem _ _ _ (Just (Chain _)) -> Nothing
        Gem _ _ _ (Just (Curtain _)) -> Nothing
        Gem _ _ _ (Just Steam) -> Nothing
        Gem col _ _ _ -> Just col
        Flip col _ -> Just col
        Countdown col _ -> Just col
        _ -> Nothing
      legacyKeep c = case c of
        Gem _ kind ice ov -> kind /= Normal || ice > 0 || ov /= Nothing
        _ -> True
      board c = setCell stableBoard (0, 0) c
      check name f g = assertEqual name [] [show c | c <- cells, f c /= g c]
  check "blocksSwap" (blocksSwapWith world) legacyBlock
  check "swapBlockedWith" (\c -> swapBlockedWith defaultWorld (board c) (0, 0) (0, 1)) legacyBlock
  check "hitImmune" (hitImmuneWith world) legacyImmune
  check "gravityFixed" gravityFixedCell legacyFixed
  check "activates" (activatesWith world) specialActivates
  check "matchColor" (matchColorWith world) legacyMatch
  check "keepOnShuffle" (keepOnShuffleWith world) legacyKeep
  -- 直接命中与第二刀之前的 chipIceOnClear 口径一致（逐格对照几条代表）
  assertEqual "ice2 absorbs" (Absorb (Gem C1 Normal 1 Nothing)) (directHitWith world (Gem C1 Normal 2 Nothing))
  assertEqual "ice1 destroys under chain" Destroy (directHitWith world (Gem C1 Normal 1 (Just (Chain 2))))
  assertEqual "chain2 peels" (Absorb (Gem C1 Normal 0 (Just (Chain 1)))) (directHitWith world (Gem C1 Normal 0 (Just (Chain 2))))
  assertEqual "safe opens" (Absorb Cookie) (directHitWith world (Safe 1))
  assertEqual "flip flips" (Absorb (mkGem C3)) (directHitWith world (Flip C1 C3))
  assertEqual "stone2 chips" (Absorb (Stone 1)) (directHitWith world (Stone 2))
