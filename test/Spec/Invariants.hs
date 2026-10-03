-- | Haskell 特性第 6 项：规则不变量的属性测试（docs/haskell-features/06-测试与光学.md）。
--
-- 用 Spec.Support.Arbitrary 的自定义 Arbitrary（任意行列、带 shrink），补上原有性质没覆盖的几条：
-- 交换是对合、重力幂等且不留悬空、重力 + 补子后满盘、被拒的操作什么都不改；
-- 以及撤销历史（Engine.History）对一个「列表模型」的状态机测试。
module Spec.Invariants
  ( tests
  ) where

import Data.Array (bounds, elems)
import Data.Ix (range)
import Data.List (sort)
import Data.Maybe (catMaybes, isJust, isNothing)
import Engine.Game (Game(..), Step(..))
import Engine.History (History(..), HistoryPolicy(..), Undoable(..), historyDepth, startHistory, withHistory)
import Match3.Board.Default (findHint)
import Match3.Board.Grid (MBoard, inBounds, swapCells)
import Match3.Board.Gravity (applyGravityWith, gravityFixedCellWith, refill)
import Match3.Core
import Match3.Game.Move (trySwap)
import Match3.Types (boardCells)
import Spec.Properties (genPick, genStart, playPicks, startState)
import Spec.Support.Arbitrary (AnyBoard(..), HoledBoard(..))
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.QuickCheck
import Toy

tests :: [TestTree]
tests =
  [ testProperty "qc_inv_swap_is_involution" qc_inv_swap_is_involution
  , testProperty "qc_inv_gravity_idempotent_no_floating" qc_inv_gravity_idempotent_no_floating
  , testProperty "qc_inv_settle_fills_board" qc_inv_settle_fills_board
  , testProperty "qc_inv_rejected_move_changes_nothing" qc_inv_rejected_move_changes_nothing
  , testProperty "qc_inv_history_matches_list_model" qc_inv_history_matches_list_model
  ]

-- | 任意盘、任意格与它的一个邻格（越界则取自身）：换两次 = 没换；换 (p, q) = 换 (q, p)；格子的多重集不变。
qc_inv_swap_is_involution :: Property
qc_inv_swap_is_involution =
  property $ \(AnyBoard b) (NonNegative i) down ->
    let ps = boardPositions b
        p@(r, c) = ps !! (i `mod` length ps)
        q0 = if down then (r + 1, c) else (r, c + 1)
        q = if inBounds b q0 then q0 else p
        b' = swapCells b p q
    in classify (q /= p) "real swap" $
         conjoin
           [ swapCells b' p q === b
           , swapCells b q p === b'
           , sort (boardCells b') === sort (boardCells b)
           , (getCell b' p, getCell b' q) === (getCell b q, getCell b p)
           ]

-- | 重力：再落一次不变（幂等）；固定格不动；每个被固定格切开的段里，实格下面没有空洞；实格的多重集不变。
qc_inv_gravity_idempotent_no_floating :: Property
qc_inv_gravity_idempotent_no_floating =
  property $ \(HoledBoard mb) ->
    let reg = defaultRegistry
        mb' = applyGravityWith reg mb
        fixed = maybe False (gravityFixedCellWith reg)
        ((r0, c0), (r1, c1)) = boundsOf mb
        floating =
          [ (r, c)
          | c <- [c0 .. c1]
          , r <- [r0 .. r1 - 1]
          , isJust (atM mb' (r, c))
          , not (fixed (atM mb' (r, c)))
          , isNothing (atM mb' (r + 1, c))
          ]
    in classify (mb' /= mb) "something fell" $
         conjoin
           [ counterexample "idempotent" (applyGravityWith reg mb' === mb')
           , counterexample ("floating cells " ++ show floating) (null floating)
           , counterexample "fixed cells stay" ([p | p <- positionsOf mb, fixed (atM mb p)] === [p | p <- positionsOf mb', fixed (atM mb' p)])
           , sort (catMaybes (elems mb')) === sort (catMaybes (elems mb))
           ]

-- | 重力之后补子：满盘、没被补的格就是重力的结果、补上的都是普通宝石。
qc_inv_settle_fills_board :: Int -> Property
qc_inv_settle_fills_board seed =
  property $ \(HoledBoard mb) ->
    let fallen = applyGravityWith defaultRegistry mb
        (b, _) = refill (mkStdGen seed) fallen
        ok p = case atM fallen p of
          Just x -> getCell b p == x
          Nothing -> case getCell b p of
            Gem _ Normal 0 Nothing -> True
            _ -> False
    in classify (any isNothing (elems mb)) "had holes" $
         boardDims b === dimsOf mb .&&. counterexample "kept / refilled" (all ok (positionsOf mb))

-- | 局中任意相邻交换：被拒（无匹配 / 非法）时盘面、分数、步数、道具、计数、关卡级元素都不变；
-- 被接受时步数最多少 1（时间精灵等可以加步）、分数不降。
qc_inv_rejected_move_changes_nothing :: Property
qc_inv_rejected_move_changes_nothing =
  forAll genStart $ \start ->
    forAll (choose (0, 2) >>= \k -> vectorOf k genPick) $ \picks ->
      forAll ((,) <$> choose (0, 9) <*> choose (0, 9)) $ \(r, c) ->
        forAll (elements [(0, 1), (1, 0), (0, 2), (1, 1)]) $ \(dr, dc) ->
          forAll arbitrary $ \useHint ->
          let s0 = startState start
              gs = last (s0 : map stepState (snd (playPicks s0 picks)))
              -- 一半用随机的两格（多数被拒），一半用提示给出的一手（多数被接受）
              (p, q) = case findHint (gsBoard gs) of
                Just pq | useHint -> pq
                _ -> ((r, c), (r + dr, c + dc))
              (gs', out) = trySwap p q gs
              frozen g = (gsBoard g, gsScore g, gsMoves g, (gsHammers g, gsFreeSwaps g, gsCrossClears g), gsCounts g, gsLevelElems g)
          in classify (out `elem` [NoMatch, InvalidSwap]) "rejected" $
               if out `elem` [NoMatch, InvalidSwap]
                 then frozen gs' === frozen gs
                 else counterexample (show out) (gsMoves gs' >= gsMoves gs - 1 .&&. gsScore gs' >= gsScore gs)

--------------------------------------------------------------------------------
-- 撤销历史的状态机测试

-- | 命令：走一步（玩具计数器：Inc n / Reset）或撤销。
data Cmd = CAct ToyAction | CUndo
  deriving (Eq, Show)

instance Arbitrary Cmd where
  arbitrary = frequency [(4, CAct . Inc <$> choose (0, 4)), (1, pure (CAct Reset)), (3, pure CUndo)]
  shrink cmd = case cmd of
    CAct (Inc n) -> [CAct (Inc m) | m <- shrink n, m >= 0]
    CAct Reset -> [CAct (Inc 1)]
    CUndo -> []

-- | 模型：当前状态 + 快照栈（新的在前），全部用列表直接写出规则。
data Model = Model ToyState [ToyState]
  deriving (Eq, Show)

-- | 历史规则：Inc 记快照、Reset 不记；快照与撤销各做一个看得见的整理（检查它们确实被调用）。
toyPolicy :: Int -> HistoryPolicy ToyState ToyAction
toyPolicy limit =
  HistoryPolicy
    { hpLimit = limit
    , hpRecord = \a -> a /= Reset
    , hpSnapshot = \s -> s {tsTarget = tsTarget s + 10}
    , hpRestore = \s -> s {tsTurns = tsTurns s + 1}
    }

-- | 模型的一步：返回新模型与「是否接受」。
modelStep :: Int -> Model -> Cmd -> (Model, Bool)
modelStep limit (Model now past) cmd = case cmd of
  CUndo -> case past of
    (p : ps) -> (Model (hpRestore pol p) ps, True)
    [] -> (Model now past, False)
  CAct a ->
    let st = gameStep toyGame now a
        accepted = stepAccepted st
    in if accepted && hpRecord pol a
         then (Model (stepState st) (take limit (hpSnapshot pol now : past)), True)
         else (Model (stepState st) past, accepted)
  where
    pol = toyPolicy limit

-- | 任意命令序列：真实的 withHistory 每一步都与模型相同（当前状态、快照栈、是否接受、能不能撤销、深度不超过上限）。
qc_inv_history_matches_list_model :: Property
qc_inv_history_matches_list_model =
  forAll (choose (1, 4)) $ \limit ->
    property $ \cmds ->
      let g = withHistory (toyPolicy limit) toyGame
          start = gameNew toyGame 4 1
          go _ _ [] = []
          go h m (c : cs) =
            let st = gameStep g h (toUndoable c)
                h' = stepState st
                (m', acc) = modelStep limit m c
            in (c, h', st, m', acc) : go h' m' cs
          trace = go (startHistory start) (Model start []) cmds
          check (c, h', st, Model now past, acc) =
            counterexample (show c) $
              conjoin
                [ histNow h' === now
                , histPast h' === past
                , stepAccepted st === acc
                , (Undo `elem` gameActions g h') === not (null past)
                , property (historyDepth h' <= limit)
                ]
      in classify (any (\(_, h', _, _, _) -> historyDepth h' == limit) trace) "hit the limit" $
           classify (any (\(c, _, _, _, acc) -> c == CUndo && not acc) trace) "undo on empty history" $
             conjoin (map check trace)
  where
    toUndoable (CAct a) = Act a
    toUndoable CUndo = Undo

boundsOf :: MBoard -> (Pos, Pos)
boundsOf = bounds

positionsOf :: MBoard -> [Pos]
positionsOf = range . bounds

dimsOf :: MBoard -> (Int, Int)
dimsOf mb = let ((r0, c0), (r1, c1)) = bounds mb in (r1 - r0 + 1, c1 - c0 + 1)
