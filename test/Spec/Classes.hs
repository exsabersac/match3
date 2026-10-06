-- 本模块有一处故意的类型错误（见「Int newtype 的写回」一节的 pairCell）：-fdefer-type-errors 把它推迟到运行时，
-- 只有调用 pairCell 时才抛出 TypeError（消息正是编译器本来会报的「缺 Coercible」）。
-- 做法同 Spec.Phase；-Wno-deferred-type-errors 保持 0 警告；-fno-defer-out-of-scope-variables / -fno-defer-typed-holes
-- 让拼错 / 漏导入的名字与类型洞仍是编译错误，其余代码照常完整类型检查。
{-# OPTIONS_GHC -fdefer-type-errors -fno-defer-out-of-scope-variables -fno-defer-typed-holes -Wno-deferred-type-errors #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 类型类与抽象（Haskell 特性第 2 项，docs/haskell-features/02-类型类与抽象.md）。
--
-- * newtype 派生（DerivingStrategies）：ElementName / CustomState 的 Show / IsString 与底层 String / Int 逐字相同。
-- * Int 状态的存储列（'customColumn' / 'intColumn'，Coercible）：四个 Int 状态本体元素的写回格子与第 2 项前手写的 Custom
--   编码相同，注册表解码回来是同一个原型与状态；表示不是 Int 的状态用 intColumn 就是类型错误。
-- * 按组件类型查询（ecs-3）：'Component' 类做存储索引，@rowGet \@Match@ 等与原型字段逐项相同；关卡元素装箱的相等。
-- * Grid 的 Functor / Foldable / Traversable：定律、行主序、形状不变；随机盘（mapAccumL）与旧递归逐种子相同；
--   盘面统计（foldMap Sum）与旧列表推导相同；清除格计数（foldMap Counts）与旧 foldl + bumpCount 相同。
module Spec.Classes
  ( tests
  ) where

import Control.Exception (TypeError(..), evaluate, try)
import Data.Foldable (toList)
import Data.List (isInfixOf, nub)
import Data.String (fromString)
import Data.Traversable (mapAccumL)
import Match3.Board.Grid (chunk, randomColor)
import Match3.Core
import Match3.Levels.Campaign (levelCount)
import Match3.Types (boardDims, maxBoardDim)
import Match3.Counts (Counts, bumpCount, countsFromList, noCounts, singleCount)
import Match3.Board.Random (randomBoardSized)
import Match3.Element
import Match3.Element.Builtin (bubbleArch, chameleonArch, fuzzballArch, magicStoneArch, stoneArch)
import Match3.Types (Grid, boardCells, gridFromRows, gridRows, minBoardDim)
import System.Random (StdGen, mkStdGen, randomR)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testProperty "classes_newtype_show_same_as_underlying" classes_newtype_show_same_as_underlying
  , testCase "classes_default_toCell_same_as_handwritten" classes_default_toCell_same_as_handwritten
  , testCase "classes_default_toCell_needs_coercible" classes_default_toCell_needs_coercible
  , testCase "classes_component_query_by_type" classes_component_query_by_type
  , testProperty "classes_grid_functor_foldable_traversable_laws" classes_grid_laws
  , testCase "classes_random_board_same_as_recursion" classes_random_board_same_as_recursion
  , testCase "classes_board_stats_same_as_list_comprehension" classes_board_stats_same_as_list_comprehension
  , testProperty "classes_foldMap_counts_same_as_foldl_bump" classes_foldMap_counts_same_as_foldl_bump
  ]

--------------------------------------------------------------------------------
-- 1. newtype 派生

-- | newtype 策略的 Show 就是底层的 Show（含 showsPrec 的括号与 showList），IsString 就是 String 的。
classes_newtype_show_same_as_underlying :: Property
classes_newtype_show_same_as_underlying =
  forAll arbitrary $ \(s, n, d) ->
    let prec = d `mod` 12 :: Int
    in conjoin
         [ showsPrec prec (ElementName s) "" === showsPrec prec s ""
         , show [ElementName s, ElementName "x"] === show [s, "x"]
         , fromString s === ElementName s
         , showsPrec prec (CustomState n) "" === showsPrec prec n ""
         , show [CustomState n] === show [n]
         , showsPrec 11 (CustomState (negate (abs n) - 1)) "" === "(" ++ show (negate (abs n) - 1) ++ ")"
         ]

--------------------------------------------------------------------------------
-- 2. 缺省 toCell

-- | 第 2 项前四个本体元素手写的 toCell（逐字副本）与现在存储列的写回相同，经注册表从格子解码回同一个原型与状态。
classes_default_toCell_same_as_handwritten :: Assertion
classes_default_toCell_same_as_handwritten =
  mapM_ one [0 .. 6]
  where
    one k = do
      viaRegistry fuzzballArch k (Custom "fuzzball" (CustomState k))
      viaRegistry bubbleArch k (Custom "bubble" (CustomState k))
      viaRegistry chameleonArch k (Custom "chameleon" (CustomState k))
      viaRegistry magicStoneArch k (Custom "magic_stone" (CustomState k))
      -- 状态同样是 Int、但列是棱镜（不是 Custom 编码）的元素
      encode stoneArch k (Stone k)
    encode :: Archetype Int -> Int -> Cell -> Assertion
    encode a k cell = assertEqual ("colPut " ++ show (aName a, k)) cell (colPut (aColumn a) k)
    viaRegistry a k cell = do
      encode a k cell
      let row = bodyOf defaultRegistry cell
      assertEqual ("decode name " ++ show cell) (aName a) (rowName row)
      assertEqual ("decode state " ++ show cell) (Just k) (colGet (aColumn a) (rowCell row))

-- | 表示不是 Int 的状态（两个字段）用 intColumn 写回：Coercible Pair Int 约束解不出来，是类型错误。
data Pair = Pair Int Int
  deriving (Eq, Show)

-- | 推迟的类型错误放在函数体里：只有调用时才抛出。
pairCell :: () -> Cell
pairCell () = colPut (intColumn "pair") (Pair 1 2)
{-# NOINLINE pairCell #-}

classes_default_toCell_needs_coercible :: Assertion
classes_default_toCell_needs_coercible = do
  r <- try (evaluate (pairCell ()))
  case r of
    Left (TypeError msg) -> assertBool ("names the representation mismatch: " ++ msg) ("Couldn't match representation of type" `isInfixOf` msg && "Pair" `isInfixOf` msg)
    Right cell -> assertFailure ("should not typecheck, got " ++ show cell)

--------------------------------------------------------------------------------
-- 3. 按组件类型查询

-- | 'Component' 类按类型索引原型的派生组件：内置战役盘面上每一格，@rowGet@ 取出的五种组件与直接调原型字段相同；
-- 注册表的 *With 查询就是组件字段（无叠层的格）。关卡元素装箱的相等仍按类型比值。
classes_component_query_by_type :: Assertion
classes_component_query_by_type = do
  sequence_
    [ case bodyOf world cell of
        row@(Row a st) -> do
          assertEqual ("match " ++ show cell) (aMatch a st) (rowGet row)
          assertEqual ("hit " ++ show cell) (aHit a st) (rowGet row)
          assertEqual ("physics " ++ show cell) (aPhysics a st) (rowGet row)
          assertEqual ("tally " ++ show cell) (aTally a st) (rowGet row)
          assertEqual ("face " ++ show cell) (aFace a st) (rowGet row)
          assertEqual ("body @Physics " ++ show cell) (aPhysics a st) (body world cell)
          assertEqual ("fallsWith " ++ show cell) (pFalls (aPhysics a st)) (fallsWith world cell)
          assertEqual ("colorOfWith " ++ show cell) (mColor (aMatch a st)) (colorOfWith world cell)
    | li <- [0 .. levelCount - 1]
    , Just gs <- [campaignGame li 3]
    , cell <- nub (boardCells (gsBoard gs))
    ]
  assertBool "level boxes" (all (\l -> l == l) (gsLevelElems (levelGame0 0)))
  where
    world = defaultRegistry
    levelGame0 li = maybe (error "no level") id (campaignGame li 1)

--------------------------------------------------------------------------------
-- 4. Grid

genGrid :: Gen (Grid Int)
genGrid = do
  r <- choose (0, 6)
  c <- choose (1, 6)
  gridFromRows <$> vectorOf r (vector c)

classes_grid_laws :: Property
classes_grid_laws =
  forAll genGrid $ \g ->
    let f = (* 3)
        h = subtract 7
        idx = snd (mapAccumL (\i _ -> (i + 1, i)) (0 :: Int) g)
    in conjoin
         [ counterexample "fmap id" (fmap id g === g)
         , counterexample "fmap compose" (fmap (f . h) g === (fmap f . fmap h) g)
         , counterexample "fmap keeps shape" (map length (gridRows (fmap f g)) === map length (gridRows g))
         , counterexample "toList = row-major concat" (toList g === concat (gridRows g))
         , counterexample "length / sum" ((length g, sum g) === (length (concat (gridRows g)), sum (concat (gridRows g))))
         , counterexample "foldMap = list foldMap" (foldMap (\x -> [x, x]) g === foldMap (\x -> [x, x]) (concat (gridRows g)))
         , counterexample "traverse Just = Just" (traverse Just g === Just g)
         , counterexample "traverse order is row-major" (toList idx === [0 .. length g - 1])
         , counterexample "traverse keeps shape" (map length (gridRows idx) === map length (gridRows g))
         , counterexample "traverse with effects" (traverse (\x -> ([x], f x)) g === (toList g, fmap f g))
         ]

-- | 第 2 项前 randomBoardSized 的手写递归（逐字副本）。
legacyRandomBoardSized :: Int -> Int -> StdGen -> (Board, StdGen)
legacyRandomBoardSized rows cols g0 =
  let (cells, g') = go (rows * cols) g0
  in (boardFromRows (chunk cols cells), g')
  where
    go :: Int -> StdGen -> ([Cell], StdGen)
    go 0 g = ([], g)
    go n g =
      let (c, g1) = randomColor g
          (rest, g2) = go (n - 1) g1
      in (mkGem c : rest, g2)

-- | mapAccumL 版与旧递归：同一种子、同一尺寸，盘面与推进后的生成器都相同（含 0 行 / 0 列的边角）。
classes_random_board_same_as_recursion :: Assertion
classes_random_board_same_as_recursion =
  sequence_
    [ do
        let (b, g) = randomBoardSized r c (mkStdGen seed)
            (b', g') = legacyRandomBoardSized r c (mkStdGen seed)
        assertEqual ("board " ++ show (seed, r, c)) b' b
        assertEqual ("dims " ++ show (seed, r, c)) (boardDims b') (boardDims b)
        assertEqual ("generator " ++ show (seed, r, c)) (fst (randomR (0, 1000000 :: Int) g')) (fst (randomR (0, 1000000 :: Int) g))
    | seed <- [1 .. 30]
    , (r, c) <- [(rr, cc) | rr <- [minBoardDim .. maxBoardDim], cc <- [minBoardDim .. maxBoardDim]] ++ [(0, 0), (0, 3), (3, 0), (1, 1)]
    ]

-- | 盘面统计（Foldable + Sum）与第 2 项前的列表推导相同：全部战役关卡开局盘面 × 盘上出现的每个名字（另加一个不存在的名字）。
classes_board_stats_same_as_list_comprehension :: Assertion
classes_board_stats_same_as_list_comprehension =
  sequence_
    [ do
        assertEqual ("count " ++ show (li, n)) (length [() | cell <- boardCells b, elementName world cell == n]) (countElementWith world n b)
        assertEqual ("weigh " ++ show (li, n)) (sum [tDiffWeight (rowGet (bodyOf world cell)) | cell <- boardCells b, elementName world cell == n]) (weighElementWith world n b)
    | li <- [0 .. levelCount - 1]
    , Just gs <- [campaignGame li 7]
    , let b = gsBoard gs
    , n <- "no_such_element" : nub (map (elementName world) (boardCells b))
    ]
  where
    world = defaultRegistry

-- | 「每格一份计数，foldMap 合起来」（Cascade.hitsOn / withDrained 的新写法）与旧的「foldl 逐个 bumpCount」相同。
classes_foldMap_counts_same_as_foldl_bump :: Property
classes_foldMap_counts_same_as_foldl_bump =
  forAll (listOf genKey) $ \ks ->
    forAll (listOf genKey) $ \start ->
      let h0 = countsFromList [(k, 1) | k <- start]
      in conjoin
           [ foldMap (\k -> singleCount k 1) ks === legacy noCounts ks
           , h0 <> foldMap (\k -> singleCount k 1) ks === legacy h0 ks
           ]
  where
    legacy :: Counts -> [CounterKey] -> Counts
    legacy = foldl (\h k -> bumpCount k 1 h)
    genKey = elements ([CountStones, CountChests, CountHoney, CountCookies, CountUfo, CountCarpets, CountNamed "x", CountNamed "y"] ++ map CountColor allColors)
