-- | 性能与并发（Haskell 特性第 5 项，docs/haskell-features/05-性能与并发.md）：
--
-- * 匹配扫描（先算整盘匹配码 unboxed UArray，再在码上扫）与提示搜索（STUArray 上就地换过去查再换回来）：
--   findMatchRunsWith / hasAnyMatchWith / findHintWith 在全部关卡开局盘及其每一种相邻交换、沿提示走 12 手途经的盘面
--   （按关接上本关注册表）上的结果写死成指纹；匹配码与 matchColorWith 逐格一致；
-- * 重力（STArray 上逐段双指针压实）：全部关卡开局盘按若干图案挖空（含不下落的固定格）后的结果写死成指纹；
--   （两个指纹生成时与删除前的逐格查注册表写法 / 列表版重力副本逐盘核对过）
-- * 并行批量求值（Spec.Support.Parallel，STM 领任务 + 每任务一个结果槽）与串行 map 逐项相同、顺序不变，
--   任意工人数（含多于任务数）、空任务表、任务出错时报第一个出错的任务。
module Spec.Perf
  ( tests
  ) where

import Control.Exception (ErrorCall(..), evaluate, try)
import qualified Data.Array as A
import Data.Array.Unboxed ((!))
import Data.Maybe (isJust, isNothing)
import Match3.Board.Default (findHint)
import Match3.Board.Gravity (applyGravityWith, gravityFixedCellWith)
import Match3.Board.Grid (MBoard, inBounds, swapCells, toM)
import Match3.Board.Match (findHintWith, findMatchRunsWith, hasAnyMatchWith, matchCodesWith)
import Match3.Core
import Match3.Element.Level (levelWorldIn)
import Match3.Element.World (World, matchColorWith)
import Match3.Game.Move (trySwap)
import Spec.Support (digest)
import Spec.Support.Parallel (forceLines, parallelForce)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "perf_match_scan_pinned" perf_match_scan_pinned
  , testCase "perf_gravity_pinned" perf_gravity_pinned
  , testCase "perf_parallel_force_same_as_serial" perf_parallel_force_same_as_serial
  ]

-- | 三个查询的结果（写进指纹）。
scanResult :: World -> Board -> String
scanResult world b = show (findMatchRunsWith world b, hasAnyMatchWith world b, findHintWith world b)

-- | 匹配码与 matchColorWith 逐格一致。返回 (有匹配?, 有提示?)，供覆盖断言用。
sameScan :: String -> World -> Board -> IO (Bool, Bool)
sameScan lbl world b = do
  let codes = matchCodesWith world b
  assertBool (lbl ++ ": codes")
    (and [codes ! p == maybe (-1) fromEnum (matchColorWith world (getCell b p)) | p <- boardPositions b])
  pure (hasAnyMatchWith world b, isJust (findHintWith world b))

adjacentSwaps :: Board -> [(Pos, Pos)]
adjacentSwaps b = [(p, q) | p@(r, c) <- boardPositions b, q <- [(r, c + 1), (r + 1, c)], inBounds b q]

-- | 扫描用例：全部关卡 × 种子 1–3 的开局盘的每一种相邻交换、沿提示走 12 手途经的盘面，外加两张死盘。
scanCases :: [(String, World, Board)]
scanCases =
  let levelCases =
        [ (li, seed, world, gs0)
        | li <- [0 .. levelCount - 1]
        , seed <- [1, 2, 3 :: Int]
        , Just gs0 <- [campaignGame li seed]
        , let world = levelWorldIn defaultWorld (gsLevelElems gs0)
        ]
      -- 开局盘的每一种相邻交换（多数带现成的匹配）
      swapped =
        [ (concat ["L", show li, " s", show seed, " swap ", show pq], world, swapCells (gsBoard gs0) p q)
        | (li, seed, world, gs0) <- levelCases
        , pq@(p, q) <- adjacentSwaps (gsBoard gs0)
        ]
      -- 沿提示走 12 手途经的盘面
      played =
        [ (concat ["L", show li, " s", show seed, " move ", show k], world, gsBoard gs)
        | (li, seed, world, gs0) <- levelCases
        , (k, gs) <- zip [0 :: Int ..] (take 13 (walk gs0))
        ]
      walk gs = gs : case (gsOver gs, findHint (gsBoard gs)) of
        (Nothing, Just (p, q)) -> let (gs', _) = trySwap p q gs in if gs' == gs then [] else walk gs'
        _ -> []
      -- 没有可走步的盘（提示为 Nothing）：两份死盘图案
      stuck = boardFromRows [[mkGem (toEnum ((r + c) `mod` 5)) | c <- [0 .. 7]] | r <- [0 .. 7 :: Int]]
      tiny = boardFromRows [[mkGem C1, mkGem C2], [mkGem C2, mkGem C1]]
  in swapped ++ played ++ [("stuck", defaultWorld, stuck), ("tiny", defaultWorld, tiny)]

perf_match_scan_pinned :: Assertion
perf_match_scan_pinned = do
  let (levelBoards, deadBoards) = splitAt (length scanCases - 2) scanCases
  assertEqual "scan digest" pinnedScan (length scanCases, digest (concatMap (\(_, world, b) -> scanResult world b) scanCases))
  results <- mapM (\(lbl, world, b) -> sameScan lbl world b) levelBoards
  assertBool ("many boards: " ++ show (length results)) (length results > 10000)
  assertBool "some boards have matches" (any fst results)
  assertBool "some boards have none" (any (not . fst) results)
  assertBool "some boards have a hint" (any snd results)
  deads <- mapM (\(lbl, world, b) -> sameScan lbl world b) deadBoards
  assertBool "stuck boards have no hint" (not (any snd deads) && all (\(_, world, b) -> isNothing (findHintWith world b)) deadBoards)

-- | perf_match_scan_pinned 的期望：(盘数, 指纹)（由现实现生成，生成时与删除前的逐格查注册表写法副本逐盘核对过）。
pinnedScan :: (Int, String)
pinnedScan = (18137, "b1ca0608fe66790c")

-- | 重力用例：49 关 × 2 种子 × 6 种挖空图案（按关接上本关注册表）。
gravityCases :: [(String, World, MBoard)]
gravityCases =
        [ (concat ["L", show li, " s", show seed, " hole ", show k], world, holed)
        | li <- [0 .. levelCount - 1]
        , seed <- [1, 2 :: Int]
        , Just gs0 <- [campaignGame li seed]
        , let world = levelWorldIn defaultWorld (gsLevelElems gs0)
              b = gsBoard gs0
        , k <- [1 .. 6 :: Int]
        , let holed = toM b A.// [(p, Nothing) | p@(r, c) <- boardPositions b, (r * 7 + c * 3 + k) `mod` (k + 1) == 0]
        ]

perf_gravity_pinned :: Assertion
perf_gravity_pinned = do
  let cases = gravityCases
  assertEqual "gravity digest" pinnedGravity (length cases, digest (concat [show (mboardRows (applyGravityWith world mb)) | (_, world, mb) <- cases]))
  -- 用例里确实有固定格把列切成多段、也确实有格子落下
  let fixedCells = [() | (_, world, mb) <- cases, Just cell <- A.elems mb, gravityFixedCellWith world cell]
      moved = [() | (_, world, mb) <- cases, applyGravityWith world mb /= mb]
  assertBool ("fixed cells present: " ++ show (length fixedCells)) (length fixedCells > 100)
  assertBool "gravity moved cells" (length moved > 100)

-- | perf_gravity_pinned 的期望：(盘数, 指纹)（由现实现生成，生成时与删除前的列表版重力副本逐盘核对过）。
pinnedGravity :: (Int, String)
pinnedGravity = (588, "b8ac0687678a7959")

perf_parallel_force_same_as_serial :: Assertion
perf_parallel_force_same_as_serial = do
  -- 任务：每关开局盘的匹配 / 提示描述（几百个互不依赖的小计算）
  let tasks =
        [ [show li, show seed, show (findMatchRunsWith defaultWorld b), show (findHintWith defaultWorld b)]
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
