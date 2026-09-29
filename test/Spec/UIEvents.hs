{-# LANGUAGE ScopedTypeVariables #-}

-- | 前端反馈与效果事件：失败 / 无效 / 道具空操作 / 撤销洗牌清掉连击反馈，MoveFx 与 gsLastCleared，事件与回放一致。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.UIEvents
  ( tests
  ) where

import Data.List (nub, sort)
import Data.Maybe (isJust)
import Match3.Core
import Match3.Element (defaultRegistry)
import Match3.Element.Event (EventKind(..), Event(..))
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.Trace (traceEvents)
import qualified Match3.Engine as M3E
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "last_cleared_skips_belt_snail" last_cleared_skips_belt_snail
  , testCase "last_cleared_includes_countdown_explode" last_cleared_includes_countdown_explode
  , testCase "failed_swap_resets_combo_feedback" failed_swap_resets_combo_feedback
  , testCase "invalid_swap_resets_combo_feedback" invalid_swap_resets_combo_feedback
  , testCase "booster_noop_resets_combo_feedback" booster_noop_resets_combo_feedback
  , testCase "undo_shuffle_reset_combo_feedback" undo_shuffle_reset_combo_feedback
  , testCase "move_fx_ignores_already_over" move_fx_ignores_already_over
  , testCase "trace_events_consistent_with_trace" trace_events_consistent_with_trace
  ]

-- | gsLastCleared tracks cascade clears, not belt rotation / snail crawl cells.
last_cleared_skips_belt_snail :: Assertion
last_cleared_skips_belt_snail = do
  let belt = [(6, 1), (6, 2), (6, 3)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell stableBoard (6, 1) (mkSnail 0 1))
                      (1, 0)
                      (mkGem C1))
                   (1, 1)
                   (mkGem C1))
                (1, 2)
                (mkGem C2))
             (1, 3)
             (mkGem C1))
          (6, 2)
          (mkGem C4)
      gs0 =
        (newGame defaultConfig 8)
          { gsBoard = board0
          , gsBelts = [belt]
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsUfos = []
          , gsGoal = goalScore 99999
          , gsLastCleared = []
          }
      (gs1, out) = trySwap (1, 2) (1, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let cleared = gsLastCleared gs1
  assertBool "recorded clears" (not (null cleared))
  assertBool "match row cleared" $
    any (`elem` cleared) [(1, 0), (1, 1), (1, 3), (1, 2)]
  assertBool "snail start not a clear site" ((6, 1) `notElem` cleared)
  assertBool "snail belt mid not a clear site" ((6, 2) `notElem` cleared)

-- | Countdown explode footprint lands in gsLastCleared (UI particles), together
-- with the move's match clears. Locks particle × countdown end-of-move explode.
last_cleared_includes_countdown_explode :: Assertion
last_cleared_includes_countdown_explode = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 0) (mkGem C1))
                (0, 1)
                (mkGem C1))
             (0, 2)
             (mkGem C2))
          (0, 3)
          (mkGem C1)
      board = spawnCountdown board0 (4, 4) C5 1
      gs0 =
        (newGame defaultConfig 11)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsBelts = []
          , gsUfos = []
          , gsPortals = []
          , gsLastCleared = []
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "match must apply"
    InvalidSwap -> assertFailure "swap must be valid"
    _ -> pure ()
  assertBool "countdown exploded away" $
    not (isCountdown (getCell (gsBoard gs1) (4, 4)))
  let cleared = gsLastCleared gs1
  assertBool ("explode center in particles, got " ++ show cleared) $
    (4, 4) `elem` cleared
  assertBool ("3×3 corner in particles, got " ++ show cleared) $
    (3, 3) `elem` cleared
  assertBool ("match cells in particles, got " ++ show cleared) $
    all (`elem` cleared) [(0, 0), (0, 1), (0, 2)]


-- | 用户报告：「爆击后，下一次点击没有触发消除，状态重置时会再播放爆击特效」。
-- 连击一步之后做一次无匹配交换：回滚状态里 gsCombo / gsLastCleared 必须归零，
-- moveFx 也不给特效（旧实现 gsCombo 残留 → 前端再置 120 帧连击弹字）。
failed_swap_resets_combo_feedback :: Assertion
failed_swap_resets_combo_feedback = withComboState $ \_ gs1 ->
  case noMatchSwap gs1 of
    Nothing -> assertFailure "need a no-match swap after the combo move"
    Just (p1, p2) -> do
      let (gs2, out) = trySwap p1 p2 gs1
      out @?= NoMatch
      gsBoard gs2 @?= gsBoard gs1
      gsScore gs2 @?= gsScore gs1
      gsMoves gs2 @?= gsMoves gs1
      gsCombo gs2 @?= 0
      gsLastCleared gs2 @?= []
      moveFx gs1 gs2 out @?= noFx
      -- 即使前端不看 moveFx、仍读旧字段，也拿不到上一步的连击
      assertBool "no stale combo in state" (gsCombo gs2 <= 1)

-- | 非相邻 / 越界交换（InvalidSwap）同样清反馈，且规则状态不变。
invalid_swap_resets_combo_feedback :: Assertion
invalid_swap_resets_combo_feedback = withComboState $ \_ gs1 -> do
  let (gs2, out) = trySwap (0, 0) (2, 2) gs1
  out @?= InvalidSwap
  assertBool "rules state unchanged" (gs2 == gs1)
  gsCombo gs2 @?= 0
  gsLastCleared gs2 @?= []
  moveFx gs1 gs2 out @?= noFx
  let (gs3, out3) = trySwap (7, 7) (7, 8) gs1
  out3 @?= InvalidSwap
  gsCombo gs3 @?= 0
  moveFx gs1 gs3 out3 @?= noFx

-- | 道具没有结算（锤子砸免疫格 / 次数用完、自由交换无匹配）也不重播上一步连击。
booster_noop_resets_combo_feedback :: Assertion
booster_noop_resets_combo_feedback = withComboState $ \_ gs1 -> do
  -- 锤子砸饼干：免疫 → NoMatch，不扣次数
  let gsCk = gs1 { gsBoard = setCell (gsBoard gs1) (0, 0) mkCookie }
      (gsH, outH) = useHammer (0, 0) gsCk
  outH @?= NoMatch
  gsHammers gsH @?= gsHammers gsCk
  gsCombo gsH @?= 0
  gsLastCleared gsH @?= []
  moveFx gsCk gsH outH @?= noFx
  -- 锤子次数为 0 → InvalidSwap
  let gsNo = gs1 { gsHammers = 0 }
      (gsH0, outH0) = useHammer (3, 3) gsNo
  outH0 @?= InvalidSwap
  gsCombo gsH0 @?= 0
  moveFx gsNo gsH0 outH0 @?= noFx
  -- 自由交换无匹配 → NoMatch，不扣次数
  case noMatchSwap gs1 of
    Nothing -> assertFailure "need a no-match pair for free-swap"
    Just (p1, p2) -> do
      let (gsF, outF) = useFreeSwap p1 p2 gs1
      outF @?= NoMatch
      gsFreeSwaps gsF @?= gsFreeSwaps gs1
      gsCombo gsF @?= 0
      gsLastCleared gsF @?= []
      moveFx gs1 gsF outF @?= noFx
  -- 十字道具真正结算时 moveFx 取本步结果（不是上一步的）
  let (gsX, outX) = useCrossClear (3, 3) gs1
      fxX = moveFx gs1 gsX outX
  fxCombo fxX @?= gsCombo gsX
  fxCleared fxX @?= gsLastCleared gsX
  assertBool "cross clear has its own sites" (not (null (fxCleared fxX)))

-- | 撤销恢复的快照、洗牌后的盘面都不带「上一步」连击反馈。
undo_shuffle_reset_combo_feedback :: Assertion
undo_shuffle_reset_combo_feedback = withComboState $ \_ gs1 -> do
  -- 再走一步（任意可行步），撤销回 gs1 的快照：gs1 自带连击，但撤销后不应再算本步反馈
  case findHint (gsBoard gs1) of
    Nothing -> assertFailure "need a follow-up move"
    Just (p1, p2) -> do
      let (gs2, _) = trySwap p1 p2 gs1
      case stepThenUndo defaultRegistry gs1 (M3E.Swap p1 p2) of
        Nothing -> assertFailure "undo should succeed"
        Just gsU -> do
          gsBoard gsU @?= gsBoard gs1
          gsCombo gsU @?= 0
          gsLastCleared gsU @?= []
          -- 直接断言 moveFx：哪怕前端误把撤销当成一步已结算的操作，拿到的也是空特效
          moveFx gs2 gsU (MoveApplied 0) @?= noFx
          -- 撤销后紧接着无匹配交换，同样没有特效（不会重播 gs1 那一步的连击）
          case noMatchSwap gsU of
            Nothing -> assertFailure "need a no-match swap after undo"
            Just (q1, q2) -> do
              let (gsU2, outU2) = trySwap q1 q2 gsU
              outU2 @?= NoMatch
              moveFx gsU gsU2 outU2 @?= noFx
  let gsS = shuffleGame gs1
  gsCombo gsS @?= 0
  gsLastCleared gsS @?= []
  moveFx gs1 gsS (MoveApplied 0) @?= noFx
  -- 洗牌后紧接着无匹配交换，仍无特效
  case noMatchSwap gsS of
    Nothing -> assertFailure "need a no-match swap after shuffle"
    Just (p1, p2) -> do
      let (gsS2, outS2) = trySwap p1 p2 gsS
      moveFx gsS gsS2 outS2 @?= noFx

-- | 已结束的局面 trySwap 原样返回旧 Outcome（旧 gsLastCleared / gsCombo 仍在）：
-- moveFx 必须识别为「没有新的一步」，不重播终局前那一步的特效。
move_fx_ignores_already_over :: Assertion
move_fx_ignores_already_over = withComboState $ \_ gs1 -> do
  let gsOverSt = gs1 { gsOver = Just (Won (gsScore gs1)) }
  case findHint (gsBoard gsOverSt) of
    Nothing -> assertFailure "need a hint pair"
    Just (p1, p2) -> do
      let (gs2, out) = trySwap p1 p2 gsOverSt
      out @?= Won (gsScore gs1)
      moveFx gsOverSt gs2 out @?= noFx


-- | 效果事件与回放脚本一致：得分事件之和 = 本步得分；消除事件的格 = 各轮清除格；
-- 步末事件与 mtEnd 一一对应；洗牌事件当且仅当 mtShuffle；爆炸事件只来自直线 / 炸弹。
trace_events_consistent_with_trace :: Assertion
trace_events_consistent_with_trace = do
  let cases =
        [ (li, gs, p1, p2)
        | li <- [0 .. length allLevels - 1]
        , s <- [1, 2]
        , let lvl = allLevels !! li
              gs = newGameAtLevel li (levelConfig lvl) s
        , (p1, p2) <- take 3 [(a, b) | (a, b) <- allSwapsSpec, moveApplied (snd (trySwap a b gs))]
        ]
      problems =
        concat
          [ [ tag "score" | sum [evAmount e | e <- evs, evKind e == EvScore] /= gsScore gs1 - gsScore gs ]
              ++ [ tag "clear" | sort (nub [p | e <- evs, evKind e == EvClear, (p, _) <- evCells e]) /= sort (nub (concatMap cwCleared (mtWaves mt))) ]
              ++ [ tag "end" | length [e | e <- evs, evKind e `elem` [EvTick, EvBelt, EvSpread, EvMove]] /= length (mtEnd mt) ]
              ++ [ tag "shuffle" | any ((== EvShuffle) . evKind) evs /= isJust (mtShuffle mt) ]
              ++ [ tag "blast" | e <- evs, evKind e == EvBlast, evElement e `notElem` ["line_h", "line_v", "bomb"] ]
              -- 逐轮严格相等：该轮 EvClear 的格按事件顺序拼接 == cwCleared（顺序也相同，前端据此画高亮 / 迸粒子），
              -- 该轮 EvScore 之和 == cwScore
              ++ [ tag ("wave-clear-order " ++ show k)
                 | (k, w) <- zip [0 ..] (mtWaves mt)
                 , [p | e <- evs, evWave e == k, evKind e == EvClear, (p, _) <- evCells e] /= cwCleared w
                 ]
              ++ [ tag ("wave-score " ++ show k)
                 | (k, w) <- zip [0 ..] (mtWaves mt)
                 , sum [evAmount e | e <- evs, evWave e == k, evKind e == EvScore] /= cwScore w
                 ]
          | (li, gs, p1, p2) <- cases
          , let (gs1, _, mt) = resolveSwapWith defaultRegistry p1 p2 gs
                evs = traceEvents mt
                tag x = x ++ "@L" ++ show li ++ show (p1, p2)
          ]
  assertBool "enough cases" (length cases > 150)
  assertEqual "event/trace mismatches" [] problems
  where
    allSwapsSpec = [((r, c), p2) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], p2 <- [(r, c + 1), (r + 1, c)], inBounds p2]
