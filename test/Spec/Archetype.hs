{-# LANGUAGE OverloadedStrings #-}
-- | 组件组合子（ecs-3 起写在原型记录里）：gemMatch / gemHit / gemPhysics = 普通棋子，
-- obstacleMatch / immuneHit / obstaclePhysics = 占格障碍，fixedPhysics = 固定格；原型只改自己不同的组件。
--
-- 内置元素逐项的查询与规则输出由元素查询快照（test/golden/element-queries.txt）与元素对照快照
-- （element-oracle.txt）锁定；这里钉住三组组件组合子的固定例子（与旧 Caps 三种原型 Piece / Blocker /
-- Fixed 对应）、内置本体原型的清单数、用组件组合子写的扩展元素不改主流程接入。
module Spec.Archetype
  ( tests
  ) where

import Match3.Core (GameState(..))
import Match3.Board.Grid (getCell, setCell)
import Match3.Counts (namedCounts)
import Match3.Element
import Match3.Element.Kind (customPlace)
import Match3.Game.Boosters (resolveHammerWith)
import Match3.Game.Level (newGame)
import Spec.Support (builtinArchetypeCount, stableBoard)
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

-- | 三组组件组合子各写一个原型（状态 = 原格，写回原格）：普通棋子 = gemMatch / gemHit / gemPhysics，
-- 占格障碍 = 缺省原型（obstacleMatch / immuneHit / obstaclePhysics），固定格 = 同障碍但 fixedPhysics。
cellColumn :: Column Cell
cellColumn = Column (const Nothing) id

plainP, obstacleP, fixedP :: Archetype Cell
plainP = (archetype "plain" cellColumn)
  { aMatch = \c -> gemMatch (case c of Gem col _ _ _ -> Just col; _ -> Nothing)
  , aHit = const gemHit
  , aPhysics = const gemPhysics
  }
obstacleP = archetype "plain" cellColumn
fixedP = (archetype "plain" cellColumn) {aPhysics = const fixedPhysics}

-- | 固定例子的写回格子。
defaultCells :: [Cell]
defaultCells = [Stone 2, Gem C3 Normal 0 Nothing]

-- | 一组例子的期望读数（'rowProbe' 的各项，顺序相同）。与旧 Caps 三种原型 Piece / Blocker / Fixed 及惰性占格
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
  , ("tally", show emptyTally)
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
        assertEqual ("plain " ++ show cell) (expected "plain" cell col False True True gemPhysics) (rowProbe (Row plainP cell))
        assertEqual ("obstacle " ++ show cell) (expected "plain" cell Nothing True False True obstaclePhysics) (rowProbe (Row obstacleP cell))
        assertEqual ("fixed " ++ show cell) (expected "plain" cell Nothing True False True fixedPhysics) (rowProbe (Row fixedP cell))
        assertEqual ("inert " ++ show cell) (expected "inert" cell Nothing True False True obstaclePhysics) (rowProbe (Row (inertArch "inert") cell))
    )
    defaultCells

--------------------------------------------------------------------------------
-- 清单 / 扩展元素

-- | 内置本体原型个数：源码里 @名字 :: Archetype …@ 签名声明的原型名（一行可声明多个）与清单常量、
-- 内置注册表里的本体项数一致。
archetype_builtin_kind_inventory :: Assertion
archetype_builtin_kind_inventory = do
  srcs <- builtinSources
  decls <- concat <$> mapM (fmap (concatMap archDecl . lines) . readCode) srcs
  assertEqual "builtin archetypes in source" builtinArchetypeCount (length decls)
  assertEqual "builtin archetypes registered" builtinArchetypeCount (length (registryKinds defaultRegistry))
  where
    -- 「a, b, c :: Archetype X」→ [a, b, c]；只数顶层（行首不缩进）的签名
    archDecl l = case break (== "::") (words l) of
      (names@(_ : _), "::" : "Archetype" : _) | take 1 l /= " " -> filter (not . null) (map (filter (/= ',')) names)
      _ -> []

-- | 用组件组合子写的扩展元素「荆棘」：缺省原型 + 受击掉耐久 + 计数，kindDef 注册后不改主流程就能被锤子
-- 打两下碎掉、按名字计数。
thornArch :: Archetype Int
thornArch = (archetype "thorn" (customColumn "thorn"))
  { aSpawn = customPlace "thorn"
  , aTally = const emptyTally {tCounter = Just (CountNamed "thorn")}
  , aHit = \n -> if n <= 1 then breakHit else absorbHit (Custom "thorn" (CustomState (n - 1)))
  }

archetype_ext_element_plugs_in :: Assertion
archetype_ext_element_plugs_in = do
  let world = register (kindDef thornArch) defaultRegistry
      p = (3, 3)
      thorn = Custom "thorn" (CustomState 2)
      gs0 = (newGame (GameConfig 5 (goalCount (CountNamed "thorn") 1)) 1) {gsBoard = setCell stableBoard p thorn, gsHammers = 5}
      hammer gs = let (gs', _, _) = resolveHammerWith world p gs in gs'
      gs1 = hammer gs0
      gs2 = hammer gs1
  assertEqual "decoded through the world" ("thorn", Just 2) (let r = bodyOf world thorn in (rowName r, colGet (customColumn "thorn") (rowCell r)))
  assertEqual "an obstacle by its bundle" (True, Nothing) (blocksSwapWith world thorn, matchColorWith world thorn)
  assertEqual "first hit chips it" (Custom "thorn" (CustomState 1)) (getCell (gsBoard gs1) p)
  assertBool "second hit breaks it" (not (isThorn (getCell (gsBoard gs2) p)))
  assertEqual "counted by name" [("thorn", 1)] (namedCounts (gsCounts gs2))
  assertEqual "unknown to the default world: inert" ("thorn", thorn, obstacleMatch) (let r = bodyOf defaultRegistry thorn in (rowName r, rowCell r, rowGet r))
  where
    isThorn c = case c of
      Custom "thorn" _ -> True
      _ -> False
