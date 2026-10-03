{-# LANGUAGE OverloadedStrings #-}
-- | 元素能力记录 Caps。元素类只有 name / toCell / caps，各能力按职责分成五组带默认值的记录
-- （匹配与交换 / 消除与受击 / 重力与移动 / 计数与目标 / 步末与变化），元素只声明自己用到的能力。
--
-- 内置元素逐项的查询与规则输出由元素查询快照（test/golden/element-queries.txt）锁定；这里钉住缺省能力的
-- 固定例子、元素类只剩三个方法（源码扫描）、用 Caps 写的扩展元素不改主流程接入。
module Spec.Caps
  ( tests
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, nub)
import Data.Maybe (isJust)
import Match3.Core (GameState(..), namedCounts, newGame)
import Match3.Board.Grid (getCell, setCell)
import Match3.Element
import Match3.Element.Caps (blocker, counts, hit)
import Match3.Element.Class
import Match3.Game.Boosters (resolveHammerWith)
import Spec.Support (stableBoard)
import Spec.Support.Source (builtinSources, readCode)
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "default_caps_pinned" default_caps_pinned
  , testCase "caps_element_class_is_thin" caps_element_class_is_thin
  , testCase "ext_caps_element_plugs_in" ext_caps_element_plugs_in
  ]

-- | 探针消息：没有内置元素收它（两边的 handleMessage 都应是 Nothing）。
data Probe = Probe

instance Message Probe

blastProbes :: [Pos]
blastProbes = [(0, 0), (3, 4), (7, 7)]

-- | 新写法下一个元素值的全部查询结果（受击后的新元素递归展开）。
newSig :: SomeElement -> [String]
newSig e =
  [ show (name e), show (toCell e), show (archetype e), show (color e), show (matchColor e)
  , show (blocksSwap e), show (activates e), show (falls e), show (portal e), show (drains e)
  , hitSig (onHit e), show (stripOnClear e), show (counter e), show (diffCounter e), show (bonusMoves e)
  , show (vacatesCarpet e), show (keepOnShuffle e), show (fmap (\f -> map (f (boardFromRows (replicate boardSize (replicate boardSize (mkGem C1))))) blastProbes) (blast e))
  , show (recolorable e), show (pushable e), show (hintable e)
  , show (fmap arOrder (adjacentRule e)), show (fmap (\r -> (erPhase r, erOrder r)) (endRule e))
  , show (fmap srOrder (swapRule e)), show (isJust (openRule e)), show (fmap (\g -> map g [0 .. 3]) (groundRule e))
  , show (isJust (handleMessage e (SomeMessage Probe)))
  ]
  where
    hitSig h = case h of
      Absorb e' -> "Absorb " ++ show (newSig e')
      Destroy -> "Destroy"
      Immune -> "Immune"

--------------------------------------------------------------------------------
-- 缺省能力

-- | 只声明原型、其余全用缺省能力的新写法元素。
data NewPlain = NewPlain Archetype Cell
  deriving (Eq, Show)

instance Element NewPlain where
  name _ = "plain"
  toCell (NewPlain _ c) = c
  caps (NewPlain a _) = capsOf a
-- | 缺省能力固定例子的写回格子。
defaultCapsCells :: [Cell]
defaultCapsCells = [Stone 2, Gem C3 Normal 0 Nothing]

-- | 按原型的缺省能力（capsOf 三种原型）与惰性占格 Inert 的全部查询结果写死（newSig，两种写回格子）。
default_caps_pinned :: Assertion
default_caps_pinned = do
  let actual =
        [newSig (SomeElement (NewPlain a cell)) | cell <- defaultCapsCells, a <- [Piece, Blocker, Fixed]]
          ++ [newSig (SomeElement (Inert "inert" cell)) | cell <- defaultCapsCells]
  assertEqual "example count" (length pinnedDefaultSigs) (length actual)
  sequence_
    [ assertEqual ("example " ++ show k ++ " / field " ++ show i) e a
    | (k, expected, got) <- zip3 [0 :: Int ..] pinnedDefaultSigs actual
    , (i, e, a) <- zip3 [0 :: Int ..] expected got
    ]

-- | default_caps_pinned 各例的 newSig（由现实现生成，生成时与删除前的逐字旧副本核对过）。
pinnedDefaultSigs :: [[String]]
pinnedDefaultSigs =
  [ ["\"plain\"","Stone 2","Piece","Nothing","Nothing","False","Just True","True","True","[]","Destroy","False","Nothing","Nothing","0","False","False","Nothing","True","True","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  , ["\"plain\"","Stone 2","Blocker","Nothing","Nothing","True","Just False","True","False","[]","Immune","False","Nothing","Nothing","0","False","True","Nothing","False","False","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  , ["\"plain\"","Stone 2","Fixed","Nothing","Nothing","True","Just False","False","False","[]","Immune","False","Nothing","Nothing","0","False","True","Nothing","False","False","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  , ["\"plain\"","Gem C3 Normal 0 Nothing","Piece","Just C3","Just C3","False","Just True","True","True","[]","Destroy","False","Nothing","Nothing","0","False","False","Nothing","True","True","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  , ["\"plain\"","Gem C3 Normal 0 Nothing","Blocker","Just C3","Just C3","True","Just False","True","False","[]","Immune","False","Nothing","Nothing","0","False","True","Nothing","False","False","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  , ["\"plain\"","Gem C3 Normal 0 Nothing","Fixed","Just C3","Just C3","True","Just False","False","False","[]","Immune","False","Nothing","Nothing","0","False","True","Nothing","False","False","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  , ["\"inert\"","Stone 2","Blocker","Nothing","Nothing","True","Just False","True","False","[]","Immune","False","Nothing","Nothing","0","False","True","Nothing","False","False","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  , ["\"inert\"","Gem C3 Normal 0 Nothing","Blocker","Nothing","Nothing","True","Just False","True","False","[]","Immune","False","Nothing","Nothing","0","False","True","Nothing","False","False","True","Nothing","Nothing","Nothing","False","Nothing","False"]
  ]

--------------------------------------------------------------------------------
-- 类变薄 / 扩展元素

-- | 元素类只剩 name / toCell / caps 三个方法；内置本体的 instance 也只定义这三项。
caps_element_class_is_thin :: Assertion
caps_element_class_is_thin = do
  cls <- readCode "src/Match3/Element/Class.hs"
  let body = takeWhile indented (drop 1 (dropWhile (not . ("class (Show e, Eq e, Typeable e) => Element e where" `isPrefixOf`)) (lines cls)))
  assertEqual "class methods" ["name", "toCell", "caps"] [m | l <- body, (m, rest) <- [span isIdent (dropWhile isSpace l)], not (null m), "::" `isPrefixOf` dropWhile isSpace rest]
  srcs <- builtinSources
  insts <- concat <$> mapM (fmap instanceMethods . readCode) srcs
  assertEqual "builtin body instances" 24 (length insts)
  assertEqual "builtin instances only define name / toCell / caps" [] (filter (`notElem` ["name", "toCell", "caps"]) (nub (concat insts)))
  where
    indented l = case l of
      [] -> True
      ch : _ -> isSpace ch
    isIdent ch = isAlphaNum ch || ch == '_' || ch == '\''
    instanceMethods src = go (lines src)
      where
        go ls = case dropWhile (not . ("instance Element " `isPrefixOf`)) ls of
          [] -> []
          (_ : rest) ->
            let (blk, more) = span indented rest
            in [m | l <- blk, take 2 l == "  ", ch : _ <- [drop 2 l], not (isSpace ch), let m = takeWhile isIdent (drop 2 l), not (null m)] : go more

-- | 用 Caps 写的扩展元素「荆棘」：四行 instance（名字、写回格子、能力 = 占格障碍 + 受击掉耐久 + 计数），
-- customEntry 注册后不改主流程就能被锤子打两下碎掉、按名字计数。
newtype Thorn = Thorn Int
  deriving (Eq, Show)

instance Element Thorn where
  name _ = "thorn"
  toCell (Thorn n) = Custom "thorn" (CustomState n)
  caps (Thorn n) = blocker [hit (if n <= 1 then Destroy else Absorb (SomeElement (Thorn (n - 1)))), counts (CountNamed "thorn")]

ext_caps_element_plugs_in :: Assertion
ext_caps_element_plugs_in = do
  let reg = register (customEntry (Thorn 2) (Thorn . unCustomState)) defaultRegistry
      p = (3, 3)
      thorn = Custom "thorn" (CustomState 2)
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "thorn") 1)) 1) {gsBoard = setCell stableBoard p thorn, gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith reg p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
  assertEqual "decoded through the registry" (SomeElement (Thorn 2)) (bodyOf reg thorn)
  assertEqual "a blocker by default caps" (Blocker, True, Nothing) (archetype (bodyOf reg thorn), blocksSwapWith reg thorn, matchColorWith reg thorn)
  assertEqual "first hit chips it" (Custom "thorn" (CustomState 1)) (getCell (gsBoard gs1) p)
  assertBool "second hit breaks it" (not (isThorn (getCell (gsBoard gs2) p)))
  assertEqual "counted by name" [("thorn", 1)] (namedCounts (gsCounts gs2))
  assertEqual "unknown to the default registry" (Just (Inert "thorn" thorn)) (fromElement (bodyOf defaultRegistry thorn))
  where
    isThorn c = case c of
      Custom "thorn" _ -> True
      _ -> False
