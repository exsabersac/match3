{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | 原型包（元素类重构第 2 刀起取代旧的能力记录 Caps）：能力类的缺省方法 = 普通棋子，
-- @deriving … via Obstacle@ / @via Fixed@ 一次给出占格障碍 / 固定格的整组方法，元素只覆盖自己不同的方法。
--
-- 内置元素逐项的查询与规则输出由元素查询快照（test/golden/element-queries.txt）与元素对照快照
-- （element-oracle.txt）锁定；这里钉住缺省方法与两个原型包的固定例子（与旧 Caps 三种原型 Piece / Blocker /
-- Fixed 的缺省能力逐项对应）、内置本体 instance 的清单数、用原型包写的扩展元素不改主流程接入。
module Spec.Archetype
  ( tests
  ) where

import Match3.Core (GameState(..))
import Match3.Board.Grid (getCell, setCell)
import Match3.Counts (namedCounts)
import Match3.Element
import Match3.Element.Ability
import Match3.Element.Kind (Kind(..), customPlace, fromCustom)
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Level (newGame)
import Spec.Support (builtinBodyInstanceCount, stableBoard)
import Spec.Support.Source (builtinSources, readCode)
import Match3.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "archetype_defaults_pinned" archetype_defaults_pinned
  , testCase "archetype_builtin_kind_inventory" archetype_builtin_kind_inventory
  , testCase "archetype_ext_element_plugs_in" archetype_ext_element_plugs_in
  ]

--------------------------------------------------------------------------------
-- 缺省方法与原型包

-- | 全用缺省方法的棋子（写回给定格子）。
newtype PlainP = PlainP Cell
  deriving (Eq, Show)

instance Cellular PlainP where
  nameOf _ = "plain"
  toCell (PlainP c) = c
instance Matchable PlainP
instance Hittable PlainP
instance Movable PlainP
instance Countable PlainP
instance Renders PlainP

-- | 只声明「占格障碍」原型包的元素。
newtype ObstacleP = ObstacleP Cell
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Obstacle ObstacleP)

instance Cellular ObstacleP where
  nameOf _ = "plain"
  toCell (ObstacleP c) = c
instance Countable ObstacleP
instance Renders ObstacleP

-- | 只声明「固定格」原型包的元素。
newtype FixedP = FixedP Cell
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Fixed FixedP)

instance Cellular FixedP where
  nameOf _ = "plain"
  toCell (FixedP c) = c
instance Countable FixedP
instance Renders FixedP

-- | 固定例子的写回格子。
defaultCells :: [Cell]
defaultCells = [Stone 2, Gem C3 Normal 0 Nothing]

-- | 一组例子的期望读数（'abilityProbe' 的各项，顺序相同）。字段值与旧 Caps 的 capsOf Piece / Blocker / Fixed
-- 及惰性占格 Inert 的同名查询逐项相同（旧值见第 2 刀前 Spec.Caps 的 pinnedDefaultSigs）。
expected :: String -> Cell -> Maybe Color -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [(String, String)]
expected n cell col swapBlocked fire falling port keep move =
  [ ("nameOf", show (ElementName n))
  , ("toCell", show cell)
  , ("color", show col)
  , ("blocksMatch", show False)
  , ("blocksSwap", show swapBlocked)
  , ("hintable", show True)
  , ("struck", show strikeV)
  , ("fires", show fire)
  , ("blast", show (Nothing :: Maybe [Pos]))
  , ("falls", show falling)
  , ("portal", show port)
  , ("drains", show ([] :: [Edge]))
  , ("keepOnShuffle", show keep)
  , ("recolorable", show move)
  , ("pushable", show move)
  , ("counter", show (Nothing :: Maybe CounterKey))
  , ("diffWeight", show (1 :: Int))
  , ("vacatesCarpet", show False)
  , ("face", show ([] :: [(String, FaceValue)]))
  ]
  where
    strikeV = case () of
      _ | fire -> Destroy
        | otherwise -> Immune

archetype_defaults_pinned :: Assertion
archetype_defaults_pinned =
  mapM_
    ( \cell -> do
        let col = case cell of
              Gem c _ _ _ -> Just c
              _ -> Nothing
        assertEqual ("plain " ++ show cell) (expected "plain" cell col False True True True False True) (abilityProbe (PlainP cell))
        assertEqual ("obstacle " ++ show cell) (expected "plain" cell col True False True False True False) (abilityProbe (ObstacleP cell))
        assertEqual ("fixed " ++ show cell) (expected "plain" cell col True False False False True False) (abilityProbe (FixedP cell))
        assertEqual ("inert " ++ show cell) (expected "inert" cell Nothing True False True False True False) (abilityProbe (Inert "inert" cell))
    )
    defaultCells

--------------------------------------------------------------------------------
-- 清单 / 扩展元素

-- | 内置本体的 Kind instance 个数（SpecialGem 一个 instance 覆盖四种特殊块）与清单常量一致。
archetype_builtin_kind_inventory :: Assertion
archetype_builtin_kind_inventory = do
  srcs <- builtinSources
  insts <- concat <$> mapM (fmap (filter isKindInstance . lines) . readCode) srcs
  assertEqual "builtin Kind instances" builtinBodyInstanceCount (length insts)
  where
    -- 去掉 instance 与可选的上下文（… =>）后，头一个词是 Kind
    isKindInstance l = case words l of
      "instance" : rest -> take 1 (afterContext rest) == ["Kind"]
      _ -> False
    afterContext ws = case break (== "=>") ws of
      (_, "=>" : rest) -> rest
      _ -> ws

-- | 用原型包写的扩展元素「荆棘」：占格障碍原型包 + 受击掉耐久 + 计数，kindDef 注册后不改主流程就能被锤子
-- 打两下碎掉、按名字计数。
newtype Thorn = Thorn Int
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle Thorn)

instance Cellular Thorn where
  nameOf _ = "thorn"

instance Hittable Thorn where
  struck (Thorn n) = if n <= 1 then Destroy else Absorb (toCell (Thorn (n - 1)))
  fires _ = False

instance Countable Thorn where
  counter _ = Just (CountNamed "thorn")

instance Renders Thorn

instance Kind Thorn where
  kindName _ = "thorn"
  fromCell = fromCustom "thorn" Thorn
  place _ = customPlace "thorn"

archetype_ext_element_plugs_in :: Assertion
archetype_ext_element_plugs_in = do
  let reg = register (kindDef @Thorn) defaultRegistry
      p = (3, 3)
      thorn = Custom "thorn" (CustomState 2)
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "thorn") 1)) 1) {gsBoard = setCell stableBoard p thorn, gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith reg p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
  assertEqual "decoded through the registry" (Just (Thorn 2)) (fromElement (bodyOf reg thorn))
  assertEqual "an obstacle by its bundle" (True, Nothing) (blocksSwapWith reg thorn, matchColorWith reg thorn)
  assertEqual "first hit chips it" (Custom "thorn" (CustomState 1)) (getCell (gsBoard gs1) p)
  assertBool "second hit breaks it" (not (isThorn (getCell (gsBoard gs2) p)))
  assertEqual "counted by name" [("thorn", 1)] (namedCounts (gsCounts gs2))
  assertEqual "unknown to the default registry" (Just (Inert "thorn" thorn)) (fromElement (bodyOf defaultRegistry thorn))
  where
    isThorn c = case c of
      Custom "thorn" _ -> True
      _ -> False
