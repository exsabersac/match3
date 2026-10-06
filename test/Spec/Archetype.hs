{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | 原型组合子（slim-9 起取代 Ability 的 DerivingVia 原型包）：gemMatch / gemHit / gemPhysics = 普通棋子，
-- obstacleMatch / immuneHit / obstaclePhysics = 占格障碍，fixedPhysics = 固定格；元素只改自己不同的字段。
--
-- 内置元素逐项的查询与规则输出由元素查询快照（test/golden/element-queries.txt）与元素对照快照
-- （element-oracle.txt）锁定；这里钉住三组原型组合子的固定例子（与旧 Caps 三种原型 Piece / Blocker /
-- Fixed 对应）、内置本体 instance 的清单数、用原型组合子写的扩展元素不改主流程接入。
module Spec.Archetype
  ( tests
  ) where
import Data.Proxy (Proxy(..))

import Match3.Core (GameState(..))
import Match3.Board.Grid (getCell, setCell)
import Match3.Counts (namedCounts)
import Match3.Element
import Match3.Element.Phase
import Match3.Element.Kind (customPlace, fromCustom, noHud)
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

-- | 三个原型组合子各写一个元素（写回给定格子）：普通棋子 = gemMatch / gemHit / gemPhysics，
-- 占格障碍 = obstacleMatch / immuneHit / obstaclePhysics，固定格 = 同障碍但 fixedPhysics。
newtype PlainP = PlainP Cell
  deriving (Eq, Show)

newtype ObstacleP = ObstacleP Cell
  deriving (Eq, Show)

newtype FixedP = FixedP Cell
  deriving (Eq, Show)

protoCodec :: (e -> Cell) -> Codec e
protoCodec enc = Codec "plain" enc (const Nothing) (\_ _ -> Nothing) emptyMeta Nothing noHud []

instance Phase PlainP where
  codec = protoCodec (\(PlainP c) -> c)
  onMatch (PlainP c) = gemMatch (case c of Gem col _ _ _ -> Just col; _ -> Nothing)
  onHit _ _ = gemHit
  physics _ = gemPhysics
  view _ = noFace

instance Phase ObstacleP where
  codec = protoCodec (\(ObstacleP c) -> c)
  onMatch _ = obstacleMatch
  onHit _ _ = immuneHit
  physics _ = obstaclePhysics
  view _ = noFace

instance Phase FixedP where
  codec = protoCodec (\(FixedP c) -> c)
  onMatch _ = obstacleMatch
  onHit _ _ = immuneHit
  physics _ = fixedPhysics
  view _ = noFace

-- | 固定例子的写回格子。
defaultCells :: [Cell]
defaultCells = [Stone 2, Gem C3 Normal 0 Nothing]

-- | 一组例子的期望读数（'phaseProbe' 的各项，顺序相同）。与旧 Caps 三种原型 Piece / Blocker / Fixed 及惰性占格
-- 的同名查询对应（占格障碍无色、挡匹配；旧 Obstacle 包颜色取写回格子但实际只用在无色格上）。
expected :: String -> Cell -> Maybe Color -> Bool -> Bool -> Bool -> Physics -> [(String, String)]
expected n cell col blocked fire hint phys =
  [ ("name", show (ElementName n))
  , ("toCell", show cell)
  , ("mColor", show col)
  , ("mBlockMatch", show blocked)
  , ("mBlockSwap", show blocked)
  , ("mHintable", show hint)
  , ("hStrike", show (if fire then Destroy else Immune))
  , ("hFires", show fire)
  , ("hBlast", show (Nothing :: Maybe [Pos]))
  , ("physics", show phys)
  , ("meta", show emptyMeta)
  , ("fBase", show (Nothing :: Maybe (String, [(String, CellField)])))
  , ("fExtras", show ([] :: [(String, FaceValue)]))
  ]

archetype_defaults_pinned :: Assertion
archetype_defaults_pinned = do
  assertEqual "gem physics" (Physics False True True True True False []) gemPhysics
  assertEqual "obstacle physics" (Physics False True False False False True []) obstaclePhysics
  assertEqual "fixed physics" (Physics True False False False False True []) fixedPhysics
  mapM_
    ( \cell -> do
        let col = case cell of
              Gem c _ _ _ -> Just c
              _ -> Nothing
        assertEqual ("plain " ++ show cell) (expected "plain" cell col False True True gemPhysics) (phaseProbe (PlainP cell))
        assertEqual ("obstacle " ++ show cell) (expected "plain" cell Nothing True False True obstaclePhysics) (phaseProbe (ObstacleP cell))
        assertEqual ("fixed " ++ show cell) (expected "plain" cell Nothing True False True fixedPhysics) (phaseProbe (FixedP cell))
        assertEqual ("inert " ++ show cell) (expected "inert" cell Nothing True False True obstaclePhysics) (phaseProbe (Inert "inert" cell))
    )
    defaultCells

--------------------------------------------------------------------------------
-- 清单 / 扩展元素

-- | 内置本体的 Phase instance 个数（slim-10 起本体只有 Phase；SpecialGem 一个 instance 覆盖四种特殊块）与清单常量一致。
archetype_builtin_kind_inventory :: Assertion
archetype_builtin_kind_inventory = do
  srcs <- builtinSources
  insts <- concat <$> mapM (fmap (filter isKindInstance . lines) . readCode) srcs
  assertEqual "builtin Phase instances" builtinBodyInstanceCount (length insts)
  where
    -- 去掉 instance 与可选的上下文（… =>）后，头一个词是 Phase
    isKindInstance l = case words l of
      "instance" : rest -> take 1 (afterContext rest) == ["Phase"]
      _ -> False
    afterContext ws = case break (== "=>") ws of
      (_, "=>" : rest) -> rest
      _ -> ws

-- | 用原型包写的扩展元素「荆棘」：占格障碍原型包 + 受击掉耐久 + 计数，kindDef 注册后不改主流程就能被锤子
-- 打两下碎掉、按名字计数。
newtype Thorn = Thorn Int
  deriving (Eq, Show)

instance Phase Thorn where
  codec = Codec
    { cName = "thorn"
    , cToCell = intCell "thorn"
    , cFromCell = fromCustom "thorn" Thorn
    , cPlace = customPlace "thorn"
    , cMeta = emptyMeta { metaCounter = Just (CountNamed "thorn") }
    , cNear = Nothing
    , cHud = noHud
    , cSystems = []
    }
  onMatch _ = obstacleMatch
  onHit _ (Thorn n) = HitOut (if n <= 1 then Destroy else Absorb (toCell (Thorn (n - 1)))) False Nothing Nothing
  physics _ = obstaclePhysics
  view _ = noFace

archetype_ext_element_plugs_in :: Assertion
archetype_ext_element_plugs_in = do
  let world = register (kindDef @Thorn) defaultRegistry
      p = (3, 3)
      thorn = Custom "thorn" (CustomState 2)
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "thorn") 1)) 1) {gsBoard = setCell stableBoard p thorn, gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith world p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
  assertEqual "decoded through the world" (Just (Thorn 2)) (fromPhase (bodyOf world thorn))
  assertEqual "an obstacle by its bundle" (True, Nothing) (blocksSwapWith world thorn, matchColorWith world thorn)
  assertEqual "first hit chips it" (Custom "thorn" (CustomState 1)) (getCell (gsBoard gs1) p)
  assertBool "second hit breaks it" (not (isThorn (getCell (gsBoard gs2) p)))
  assertEqual "counted by name" [("thorn", 1)] (namedCounts (gsCounts gs2))
  assertEqual "unknown to the default world" (Just (Inert "thorn" thorn)) (fromPhase (bodyOf defaultRegistry thorn))
  where
    isThorn c = case c of
      Custom "thorn" _ -> True
      _ -> False
