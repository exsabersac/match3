{-# LANGUAGE NamedFieldPuns #-}

-- | 玩家交换：trySwap（= runMove）及其逐轮回放 traceSwap，放在同一模块便于对照同步。
--
-- 依赖：State、Tally、Outcome、Shuffle、Trace、Match3.Board 与各机制模块。
-- 同步（重要）：traceSwap 按 trySwap 的顺序（主连锁 → 倒计时 → 皮带 → 藤 / 巧 / 蒸汽 → 蜗牛 →
-- （成消）再连锁）重算并记录 mtEnd；第一刀只搬移，两者仍是两份平行实现。
-- 护栏 trace_swap_final_equals_trySwap、trace_end_steps_replay_to_trySwap_final、trace_rejected_move_is_empty。
module Match3.Game.Move
  ( trySwap
  , runMove
  , traceSwap
  ) where

import Data.Maybe (isJust)
import Match3.Board (hasAnyMatch, inBounds, adjacent, runCascadeScoredWithUfos, runCascadeScoredFromSeedsWithUfos, runPostBeltCascade, resolveCountdowns, traceCascade, traceCascadeFromSeeds, tracePostBeltCascade, traceCountdowns, swapCells, getCell)
import Match3.Obstacles (swapBlockedByStone)
import Match3.Conveyor (shiftBelts)
import Match3.Grass (spreadVines, spreadChoco, spreadSteam)
import Match3.Carpet (coverCarpets)
import Data.List (nub)
import Match3.Snail (stepSnailsAvoidingBlocked)
import Match3.Countdown (tickCountdowns)
import Match3.Combos (isSpecialCombo, comboClearSeeds)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types
import Match3.Game.Outcome
import Match3.Game.Shuffle
import Match3.Game.State
import Match3.Game.Tally
import Match3.Game.Trace

-- | 玩家相邻交换入口。成功路径固定顺序：
-- 主连锁 → 倒计时 tick/爆炸 → 皮带移位+settle → 藤/巧/蒸汽蔓延 → 蜗牛 →
-- （若蜗牛成消）再连锁一次（不再重复步末效果）→ 结算目标/步数 → ensurePlayable。
-- 无匹配或挡交换返回原盘 + NoMatch；同时清零 gsCombo / gsLastCleared（见 clearMoveFx），
-- 避免前端把上一步的连击当成这一步再播一次。
-- 同步约束：traceSwap 按同样顺序重算并记录 mtEnd；调整这里的步骤顺序、步末效果或蔓延 / 蜗牛
-- 参数时必须同步 traceSwap / traceSpreads / traceSnails / beltMoves / applyEndEffect
-- （护栏：trace_swap_final_equals_trySwap、trace_end_steps_replay_to_trySwap_final）。
trySwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
trySwap p1 p2 gs
  | Just o <- gsOver gs = (gs, o)
  | not (inBounds p1 && inBounds p2) = (clearMoveFx gs, InvalidSwap)
  | not (adjacent p1 p2) = (clearMoveFx gs, InvalidSwap)
  | swapBlockedByStone (gsBoard gs) p1 p2 =
      (rejectMove gs, NoMatch)
  | otherwise =
      let board0 = gsBoard gs
          swapped = swapCells board0 p1 p2
          rainbow = isRainbowSwap board0 p1 p2
          specialCombo = isSpecialCombo board0 p1 p2
      in if not rainbow && not specialCombo && not (hasAnyMatch swapped)
           then (rejectMove gs, NoMatch)
           else
             let ufos0 = gsUfos gs
                 (board0', cleared0, gained0, combo0, tallies0, stones0, chests0, honey0, balloons0, cookies0, cakes0, uAbs0, ufos1, pos0, g0') =
                   if rainbow
                     then
                       let seeds = rainbowClearSeeds swapped p1 p2
                       in runCascadeScoredFromSeedsWithUfos (Just p2) seeds ufos0 (gsPortals gs) (gsGen gs) swapped
                     else if specialCombo
                       then
                         let seeds = comboClearSeeds swapped p1 p2
                         in runCascadeScoredFromSeedsWithUfos (Just p2) seeds ufos0 (gsPortals gs) (gsGen gs) swapped
                       else runCascadeScoredWithUfos (Just p2) ufos0 (gsPortals gs) (gsGen gs) swapped
                 -- Countdown bombs: tick after move; zeros explode 3×3 (keep UFOs + portals)
                 (boardCd, cleared1, gained1, combo1, tallies1, stones1, chests1, honey1, balloons1, cookies1, cakes1, uAbs1, ufosCd, pos1, g1') =
                   resolveCountdowns ufos1 (gsPortals gs) g0' board0'
                 -- Conveyor belts: shift then cascade / settle (drain belt-delivered cookies)
                 boardBelt = shiftBelts boardCd (gsBelts gs)
                 (boardBeltCas, cleared2, gained2, combo2, tallies2, stones2, chests2, honey2, balloons2, cookies2, cakes2, uAbs2, ufos2, pos2, g') =
                   if null (gsBelts gs)
                     then (boardCd, 0, 0, 0, zip allColors (repeat 0), 0, 0, 0, 0, 0, 0, 0, ufosCd, [], g1')
                     else runPostBeltCascade ufosCd (gsPortals gs) g1' boardBelt
                 -- Vine / chocolate / steam, then snails crawl (skip belt; reverse at portals)
                 beltCells = nub (concat (gsBelts gs))
                 portalEnds = nub (concatMap (\(a, b) -> [a, b]) (gsPortals gs))
                 boardSnail =
                   stepSnailsAvoidingBlocked
                     beltCells
                     portalEnds
                     (spreadSteam (spreadChoco (spreadVines boardBeltCas)))
                 -- Snail push can assemble a match after cascades finished; resolve it
                 -- (no second belt/snail/countdown — once-per-move end effects stay once).
                 (board1, cleared3, gained3, combo3, tallies3, stones3, chests3, honey3, balloons3, cookies3, cakes3, uAbs3, ufos3, pos3, gFinal) =
                   if hasAnyMatch boardSnail
                     then runCascadeScoredWithUfos Nothing ufos2 (gsPortals gs) g' boardSnail
                     else (boardSnail, 0, 0, 0, zip allColors (repeat 0), 0, 0, 0, 0, 0, 0, 0, ufos2, [], g')
                 clearedSites = nub (pos0 ++ pos1 ++ pos2 ++ pos3)
                 gained = gained0 + gained1 + gained2 + gained3
                 combo =
                   let c1 = max combo0 (if cleared1 > 0 then combo0 + combo1 else combo0)
                       c2 = max c1 (if cleared2 > 0 then c1 + combo2 else c1)
                   in max c2 (if cleared3 > 0 then c2 + combo3 else c2)
                 tallies = mergeTallies (mergeTallies (mergeTallies tallies0 tallies1) tallies2) tallies3
                 stonesHit = stones0 + stones1 + stones2 + stones3
                 chestsHit = chests0 + chests1 + chests2 + chests3
                 honeyHit = honey0 + honey1 + honey2 + honey3
                 balloonHit = balloons0 + balloons1 + balloons2 + balloons3
                 cookieHit = cookies0 + cookies1 + cookies2 + cookies3
                 cakeHit = cakes0 + cakes1 + cakes2 + cakes3
                 safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
                 spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
                 (carpetOpen', carpetHit) =
                   coverCarpets
                     (gsCarpetOpen gs)
                     (pos0 ++ pos1 ++ pos2 ++ pos3 ++ carpetVacateSeeds board0 board1)
                 uAbs = uAbs0 + uAbs1 + uAbs2 + uAbs3
                 ufoCollected' = gsUfoCollected gs + uAbs
                 cookies' = gsCookiesCollected gs + cookieHit
                 cakes' = gsCakesCleared gs + cakeHit
                 safes' = gsSafesOpened gs + safesHit
                 carpets' = gsCarpetsCovered gs + carpetHit
                 _cleared = cleared0 + cleared1 + cleared2 + cleared3
                 collectDelta = case gsGoal gs of
                   GoalCollect col _ -> lookupColor tallies col
                   GoalCollectMulti _ -> 0
                   GoalClearStone _ -> 0
                   GoalChest _ -> 0
                   GoalHoney _ -> 0
                   GoalBalloon _ -> 0
                   GoalCookie _ -> 0
                   GoalCake _ -> 0
                   GoalSafe _ -> 0
                   GoalCarpet _ -> 0
                   GoalScore _ -> 0
                   GoalUfo _ -> uAbs
                 -- For multi-collect, primary meter = sum of progress toward reqs
                 collected' = case gsGoal gs of
                   GoalCollect _ _ -> gsCollected gs + collectDelta
                   GoalCollectMulti reqs ->
                     let bag' = mergeTallies (gsColorBag gs) tallies
                     in sum [min n (lookupColor bag' c) | (c, n) <- reqs]
                   GoalClearStone _ -> gsStonesCleared gs + stonesHit
                   GoalChest _ -> gsChestsCleared gs + chestsHit
                   GoalHoney _ -> gsHoneyCleared gs + honeyHit
                   GoalBalloon _ -> gsBalloonsPopped gs + balloonHit
                   GoalCookie _ -> cookies'
                   GoalCake _ -> cakes'
                   GoalSafe _ -> safes'
                   GoalCarpet _ -> carpets'
                   GoalScore _ -> gsCollected gs
                   GoalUfo _ -> ufoCollected'
                 score' = gsScore gs + gained
                 moves' = gsMoves gs - 1 + 2 * spiritHit
                 hist = take 20 (snapshot gs : gsHistory gs)
                 gs' =
                   gs
                     { gsBoard = board1
                     , gsScore = score'
                     , gsMoves = moves'
                     , gsCollected = collected'
                     , gsColorBag = mergeTallies (gsColorBag gs) tallies
                     , gsStonesCleared = gsStonesCleared gs + stonesHit
                     , gsChestsCleared = gsChestsCleared gs + chestsHit
                     , gsHoneyCleared = gsHoneyCleared gs + honeyHit
                     , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
                     , gsCookiesCollected = cookies'
                     , gsCakesCleared = cakes'
                     , gsSafesOpened = safes'
                     , gsGen = gFinal
                     , gsHistory = hist
                     , gsHint = Nothing
                     , gsCombo = combo
                     , gsShuffled = False
                     , gsUfos = ufos3
                     , gsUfoCollected = ufoCollected'
                     , gsCarpetOpen = carpetOpen'
                     , gsCarpetsCovered = carpets'
                     , gsLastCleared = clearedSites
                     }
                 outcome = decideOutcome gs' gained
                 gs'' = case outcome of
                   Won s -> gs' { gsOver = Just (Won s) }
                   Lost s -> gs' { gsOver = Just (Lost s) }
                   LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
                   _ -> gs'
                 -- Auto-shuffle if stuck after a non-terminal move
                 gs''' = case outcome of
                   MoveApplied _ -> ensurePlayable gs''
                   _ -> gs''
             in (gs''', outcome)

-- | trySwap 的别名（冻结 API）。
runMove :: Pos -> Pos -> GameState -> (GameState, Outcome)
runMove = trySwap

-- | trySwap 的逐轮回放。步骤顺序与 trySwap 完全一致：
-- 主连锁 → 倒计时 → 皮带 → 藤/巧/蒸汽蔓延 + 蜗牛 →（蜗牛成消）再连锁。
-- 倒计时减一 / 皮带移位 / 蔓延 / 蜗牛这类「非消除」变化不单独成轮，按发生位置记在 mtEnd
-- （EndStep.esAfterWaves）里；自动洗牌不在 mtEnd 中，前端用 mtFinal 与结算后 gsBoard 的差异补播。
traceSwap :: Pos -> Pos -> GameState -> MoveTrace
traceSwap p1 p2 gs
  | isJust (gsOver gs) = emptyTrace gs
  | not (inBounds p1 && inBounds p2) = emptyTrace gs
  | not (adjacent p1 p2) = emptyTrace gs
  | swapBlockedByStone (gsBoard gs) p1 p2 = emptyTrace gs
  | not rainbow && not specialCombo && not (hasAnyMatch swapped) = emptyTrace gs
  | otherwise =
      let portals = gsPortals gs
          (ws0, board0', ufos1, g0') =
            if rainbow
              then traceCascadeFromSeeds (Just p2) (rainbowClearSeeds swapped p1 p2) (gsUfos gs) portals (gsGen gs) swapped
              else if specialCombo
                then traceCascadeFromSeeds (Just p2) (comboClearSeeds swapped p1 p2) (gsUfos gs) portals (gsGen gs) swapped
                else traceCascade (Just p2) (gsUfos gs) portals (gsGen gs) swapped
          (ws1, boardCd, ufosCd, g1') = traceCountdowns ufos1 portals g0' board0'
          -- 倒计时减一（与 resolveCountdowns 内部的 tickCountdowns 相同）
          bTick = tickCountdowns board0'
          ticked = [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell board0' p /= getCell bTick p]
          endTick = [EndStep (length ws0) board0' bTick (EndCountdownTick ticked) | not (null ticked)]
          boardBelt = shiftBelts boardCd (gsBelts gs)
          nBelt = length ws0 + length ws1
          endBelt =
            [ EndStep nBelt boardCd boardBelt (EndBeltShift mv)
            | not (null (gsBelts gs))
            , let mv = beltMoves (gsBelts gs)
            , not (null mv)
            ]
          (ws2, boardBeltCas, ufos2, g') =
            if null (gsBelts gs)
              then ([], boardCd, ufosCd, g1')
              else tracePostBeltCascade ufosCd portals g1' boardBelt
          nEnd = nBelt + length ws2
          beltCells = nub (concat (gsBelts gs))
          portalEnds = nub (concatMap (\(a, b) -> [a, b]) portals)
          (endSpread, boardSpread) = traceSpreads nEnd boardBeltCas
          (snails, boardSnail) = traceSnails beltCells portalEnds boardSpread
          endSnail = [EndStep nEnd boardSpread boardSnail (EndSnail snails) | not (null snails)]
          (ws3, board1, _, _) =
            if hasAnyMatch boardSnail
              then traceCascade Nothing ufos2 portals g' boardSnail
              else ([], boardSnail, ufos2, g')
      in MoveTrace swapped (ws0 ++ ws1 ++ ws2 ++ ws3) board1 (endTick ++ endBelt ++ endSpread ++ endSnail)
  where
    board0 = gsBoard gs
    swapped = swapCells board0 p1 p2
    rainbow = isRainbowSwap board0 p1 p2
    specialCombo = isSpecialCombo board0 p1 p2
