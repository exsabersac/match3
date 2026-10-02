-- | 性能与并发（Haskell 特性第 5 项，docs/haskell-features/05-性能与并发.md）：
--
-- * 匹配扫描改成「先算整盘匹配码（unboxed UArray），再在码上扫」、提示搜索改成「STUArray 上就地换过去查再换回来」之后，
--   findMatchRunsWith / hasAnyMatchWith / findHintWith 与第 5 项前的逐格查注册表写法（Spec.Support.LegacyPerf，逐字副本）
--   逐项相同：全部关卡开局盘及其每一种相邻交换、沿提示走 12 手途经的盘面（按关接上本关注册表），外加随机盘面
--   （任意格、任意行列 1–10）；
-- * 匹配码与 matchColorWith 逐格一致；
-- * 重力改成 STArray 上逐段双指针压实之后，与旧的「逐列取列表、colGravityWith、array 写回」逐格相同：
--   全部关卡开局盘按若干图案挖空（按关接上本关注册表，含不下落的固定格），外加随机可空盘面；
-- * 并行批量求值（Spec.Support.Parallel，STM 领任务 + 每任务一个结果槽）与串行 map 逐项相同、顺序不变，
--   任意工人数（含多于任务数）、空任务表、任务出错时报第一个出错的任务。
module Spec.Perf
  ( tests
  ) where

import Control.Exception (ErrorCall(..), evaluate, try)
import qualified Data.Array as A
import Data.Array.Unboxed ((!))
import Data.Maybe (isJust, isNothing)
import Match3.Board.Gravity (applyGravityWith, gravityFixedCellWith)
import Match3.Board.Grid (mboardFromRows, mboardRows, toM)
import Match3.Board.Match (findHintWith, findMatchRunsWith, hasAnyMatchWith, matchCodesWith)
import Match3.Core
import Match3.Element (defaultRegistry)
import Match3.Element.Level (levelRegistryIn)
import Match3.Element.Registry (Registry, matchColorWith)
import qualified Spec.Support.LegacyPerf as Old
import Spec.Properties (genCell, genPlayBoard)
import Spec.Support.Parallel (forceLines, parallelForce)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testCase "perf_match_scan_same_as_legacy" perf_match_scan_same_as_legacy
  , testProperty "qc_perf_match_scan_random_boards" qc_perf_match_scan_random_boards
  , testCase "perf_gravity_same_as_legacy" perf_gravity_same_as_legacy
  , testProperty "qc_perf_gravity_random_boards" qc_perf_gravity_random_boards
  , testCase "perf_parallel_force_same_as_serial" perf_parallel_force_same_as_serial
  ]

-- | 三个查询与旧写法相同，匹配码与 matchColorWith 一致。返回 (有匹配?, 有提示?)，供覆盖断言用。
sameScan :: String -> Registry -> Board -> IO (Bool, Bool)
sameScan lbl reg b = do
  assertEqual (lbl ++ ": runs") (Old.oldFindMatchRunsWith reg b) (findMatchRunsWith reg b)
  assertEqual (lbl ++ ": any") (Old.oldHasAnyMatchWith reg b) (hasAnyMatchWith reg b)
  assertEqual (lbl ++ ": hint") (Old.oldFindHintWith reg b) (findHintWith reg b)
  let codes = matchCodesWith reg b
  assertBool (lbl ++ ": codes")
    (and [codes ! p == maybe (-1) fromEnum (matchColorWith reg (getCell b p)) | p <- boardPositions b])
  pure (hasAnyMatchWith reg b, isJust (findHintWith reg b))

adjacentSwaps :: Board -> [(Pos, Pos)]
adjacentSwaps b = [(p, q) | p@(r, c) <- boardPositions b, q <- [(r, c + 1), (r + 1, c)], inBounds b q]

perf_match_scan_same_as_legacy :: Assertion
perf_match_scan_same_as_legacy = do
  let levelCases =
        [ (li, seed, reg, gs0)
        | li <- [0 .. levelCount - 1]
        , seed <- [1, 2, 3 :: Int]
        , Just gs0 <- [campaignGame li seed]
        , let reg = levelRegistryIn defaultRegistry (gsLevelElems gs0)
        ]
      -- 开局盘的每一种相邻交换（多数带现成的匹配）
      swapped =
        [ (concat ["L", show li, " s", show seed, " swap ", show pq], reg, swapCells (gsBoard gs0) p q)
        | (li, seed, reg, gs0) <- levelCases
        , pq@(p, q) <- adjacentSwaps (gsBoard gs0)
        ]
      -- 沿提示走 12 手途经的盘面
      played =
        [ (concat ["L", show li, " s", show seed, " move ", show k], reg, gsBoard gs)
        | (li, seed, reg, gs0) <- levelCases
        , (k, gs) <- zip [0 :: Int ..] (take 13 (walk gs0))
        ]
      walk gs = gs : case (gsOver gs, findHint (gsBoard gs)) of
        (Nothing, Just (p, q)) -> let (gs', _) = trySwap p q gs in if gs' == gs then [] else walk gs'
        _ -> []
  results <- mapM (\(lbl, reg, b) -> sameScan lbl reg b) (swapped ++ played)
  assertBool ("many boards: " ++ show (length results)) (length results > 10000)
  assertBool "some boards have matches" (any fst results)
  assertBool "some boards have none" (any (not . fst) results)
  assertBool "some boards have a hint" (any snd results)
  -- 没有可走步的盘（提示为 Nothing）：两份死盘图案
  let stuck = boardFromRows [[mkGem (toEnum ((r + c) `mod` 5)) | c <- [0 .. 7]] | r <- [0 .. 7 :: Int]]
      tiny = boardFromRows [[mkGem C1, mkGem C2], [mkGem C2, mkGem C1]]
  (_, h1) <- sameScan "stuck" defaultRegistry stuck
  (_, h2) <- sameScan "tiny" defaultRegistry tiny
  assertBool "stuck boards have no hint" (not h1 && not h2 && isNothing (findHintWith defaultRegistry stuck))

qc_perf_match_scan_random_boards :: Property
qc_perf_match_scan_random_boards =
  withMaxSuccess 1000 $ forAll genBoard $ \b ->
    let reg = defaultRegistry
    in counterexample (show b) $
         findMatchRunsWith reg b === Old.oldFindMatchRunsWith reg b
           .&&. hasAnyMatchWith reg b === Old.oldHasAnyMatchWith reg b
           .&&. findHintWith reg b === Old.oldFindHintWith reg b
  where
    genBoard =
      oneof
        [ genPlayBoard
        , do
            r <- choose (1, 10)
            c <- choose (1, 10)
            boardFromRows <$> vectorOf r (vectorOf c (frequency [(6, mkGem <$> elements [C1, C2, C3]), (1, genCell)]))
        ]

perf_gravity_same_as_legacy :: Assertion
perf_gravity_same_as_legacy = do
  let cases =
        [ (concat ["L", show li, " s", show seed, " hole ", show k], reg, holed)
        | li <- [0 .. levelCount - 1]
        , seed <- [1, 2 :: Int]
        , Just gs0 <- [campaignGame li seed]
        , let reg = levelRegistryIn defaultRegistry (gsLevelElems gs0)
              b = gsBoard gs0
        , k <- [1 .. 6 :: Int]
        , let holed = toM b A.// [(p, Nothing) | p@(r, c) <- boardPositions b, (r * 7 + c * 3 + k) `mod` (k + 1) == 0]
        ]
  mapM_ (\(lbl, reg, mb) -> assertEqual lbl (Old.oldApplyGravityWith reg mb) (applyGravityWith reg mb)) cases
  -- 用例里确实有固定格把列切成多段、也确实有格子落下
  let fixedCells = [() | (_, reg, mb) <- cases, Just cell <- A.elems mb, gravityFixedCellWith reg cell]
      moved = [() | (_, reg, mb) <- cases, applyGravityWith reg mb /= mb]
  assertBool ("fixed cells present: " ++ show (length fixedCells)) (length fixedCells > 100)
  assertBool "gravity moved cells" (length moved > 100)

qc_perf_gravity_random_boards :: Property
qc_perf_gravity_random_boards =
  withMaxSuccess 1000 $ forAll genHoled $ \mb ->
    counterexample (show (mboardRows mb)) (applyGravityWith defaultRegistry mb === Old.oldApplyGravityWith defaultRegistry mb)
  where
    genHoled = do
      r <- choose (1, 10)
      c <- choose (1, 10)
      mboardFromRows <$> vectorOf r (vectorOf c (frequency [(3, Just <$> genCell), (2, pure Nothing)]))

perf_parallel_force_same_as_serial :: Assertion
perf_parallel_force_same_as_serial = do
  -- 任务：每关开局盘的匹配 / 提示描述（几百个互不依赖的小计算）
  let tasks =
        [ [show li, show seed, show (findMatchRunsWith defaultRegistry b), show (findHintWith defaultRegistry b)]
        | li <- [0 .. levelCount - 1]
        , seed <- [1 .. 6 :: Int]
        , Just gs <- [campaignGame li seed]
        , let b = gsBoard gs
        ]
  mapM_
    (\w -> parallelForce w forceLines tasks >>= assertEqual ("workers " ++ show w) tasks)
    [1, 2, 3, 8, 1000]
  -- 空任务表
  parallelForce 4 forceLines [] >>= assertEqual "no tasks" []
  -- 任务出错：报按顺序第一个出错的任务（与串行求值相同），无论哪个工人先算到哪个
  let bad = [["ok"], ["ok", error "task 3"], ["ok"], [error "task 5"], ["ok"]]
  r <- try (parallelForce 8 forceLines bad >>= evaluate . length)
  case r of
    Left (ErrorCall msg) -> assertEqual "first failing task" "task 3" msg
    Right _ -> assertFailure "expected an error"
