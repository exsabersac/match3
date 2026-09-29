{-# LANGUAGE NamedFieldPuns #-}

-- | 道具：锤子 / 自由交换 / 十字清除的结算（useHammer / useFreeSwap / useCrossClear）
-- 与各自的逐轮回放（traceHammer / traceFreeSwap / traceCrossClear），放在一起便于同步。
--
-- 依赖：State、Tally、Outcome、Shuffle、Trace、Match3.Board、Match3.Boosters（种子几何）。
-- 不变量：道具不耗步、不推进倒计时、没有皮带 / 蜗牛，步末只有蔓延；锤子对免疫格不扣次数。
-- 护栏 trace_boosters_final_equal_result、trace_end_steps_boosters_replay。
module Match3.Game.Boosters
  ( traceFreeSwap
  , traceHammer
  , traceCrossClear
  , traceSeedsThenSpread
  , hammerImmune
  , useHammer
  , useFreeSwap
  , useCrossClear
  ) where

import Data.Maybe (isJust)
import Match3.Board (hasAnyMatch, inBounds, runCascadeScoredWithUfos, runCascadeScoredFromSeedsWithUfos, traceCascade, traceCascadeFromSeeds, swapCells, getCell)
import Match3.Obstacles (swapBlockedByStone)
import Match3.Grass (spreadVines, spreadChoco, spreadSteam)
import Match3.Carpet (coverCarpets)
import Data.List (nub)
import Match3.Combos (isSpecialCombo, comboClearSeeds)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Boosters (crossClearSeeds)
import Match3.Types
import Match3.Game.Outcome
import Match3.Game.Shuffle
import Match3.Game.State
import Match3.Game.Tally
import Match3.Game.Trace

-- | useFreeSwap 的逐轮回放（主连锁 + 蔓延；自由交换不触发倒计时 / 皮带 / 蜗牛）。
traceFreeSwap :: Pos -> Pos -> GameState -> MoveTrace
traceFreeSwap p1 p2 gs
  | isJust (gsOver gs) = emptyTrace gs
  | gsFreeSwaps gs <= 0 = emptyTrace gs
  | not (inBounds p1 && inBounds p2) = emptyTrace gs
  | p1 == p2 = emptyTrace gs
  | swapBlockedByStone (gsBoard gs) p1 p2 = emptyTrace gs
  | not rainbow && not specialCombo && not (hasAnyMatch swapped) = emptyTrace gs
  | otherwise =
      let (ws, boardF, _, _) =
            if rainbow
              then traceCascadeFromSeeds (Just p2) (rainbowClearSeeds swapped p1 p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
              else if specialCombo
                then traceCascadeFromSeeds (Just p2) (comboClearSeeds swapped p1 p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
                else traceCascade (Just p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
          (ends, boardS) = traceSpreads (length ws) boardF
      in MoveTrace swapped ws boardS ends
  where
    board0 = gsBoard gs
    swapped = swapCells board0 p1 p2
    rainbow = isRainbowSwap board0 p1 p2
    specialCombo = isSpecialCombo board0 p1 p2

-- | useHammer 的逐轮回放。
traceHammer :: Pos -> GameState -> MoveTrace
traceHammer p gs
  | isJust (gsOver gs) = emptyTrace gs
  | gsHammers gs <= 0 = emptyTrace gs
  | not (inBounds p) = emptyTrace gs
  | hammerImmune (getCell (gsBoard gs) p) = emptyTrace gs
  | otherwise = traceSeedsThenSpread [p] gs

-- | useCrossClear 的逐轮回放。
traceCrossClear :: Pos -> GameState -> MoveTrace
traceCrossClear p gs
  | isJust (gsOver gs) = emptyTrace gs
  | gsCrossClears gs <= 0 = emptyTrace gs
  | not (inBounds p) = emptyTrace gs
  | otherwise = traceSeedsThenSpread (crossClearSeeds p) gs

-- | 锤子 / 十字共用的回放：种子连锁 → 藤 / 巧 / 蒸汽蔓延（道具没有倒计时、皮带、蜗牛）。
traceSeedsThenSpread :: [Pos] -> GameState -> MoveTrace
traceSeedsThenSpread seeds gs =
  let (ws, boardH, _, _) =
        traceCascadeFromSeeds Nothing seeds (gsUfos gs) (gsPortals gs) (gsGen gs) (gsBoard gs)
      (ends, boardS) = traceSpreads (length ws) boardH
  in MoveTrace (gsBoard gs) ws boardS ends

-- | Cells that chipIceOnClear leaves untouched (no peel / no clear).
hammerImmune :: Cell -> Bool
hammerImmune c = isMaker c || isSnail c || isBottle c || isMagicHat c || isCookie c

-- | Hammer: spend one charge to clear a single in-bounds cell, then cascade.
-- Does not consume a move.
-- Maker / Snail / Bottle / MagicHat / Cookie are immune to direct seeds — reject without spending.
useHammer :: Pos -> GameState -> (GameState, Outcome)
useHammer p gs
  | Just o <- gsOver gs = (gs, o)
  | gsHammers gs <= 0 = (clearMoveFx gs, InvalidSwap)
  | not (inBounds p) = (clearMoveFx gs, InvalidSwap)
  | hammerImmune (getCell (gsBoard gs) p) =
      (rejectMove gs, NoMatch)
  | otherwise =
      let seeds = [p]
          (boardH, _n, gained, combo, tallies, stonesHit, chestsHit, honeyHit, balloonHit, cookieHit, cakeHit, uAbs, ufos', posCleared, g') =
            runCascadeScoredFromSeedsWithUfos Nothing seeds (gsUfos gs) (gsPortals gs) (gsGen gs) (gsBoard gs)
          board1 = spreadSteam (spreadChoco (spreadVines boardH))
          score' = gsScore gs + gained
          hist = take 20 (snapshot gs : gsHistory gs)
          ufoCollected' = gsUfoCollected gs + uAbs
          cookies' = gsCookiesCollected gs + cookieHit
          cakes' = gsCakesCleared gs + cakeHit
          safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
          spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
          safes' = gsSafesOpened gs + safesHit
          (carpetOpen', carpetHit) =
            coverCarpets
              (gsCarpetOpen gs)
              (posCleared ++ carpetVacateSeeds (gsBoard gs) boardH)
          carpets' = gsCarpetsCovered gs + carpetHit
          collectDelta = case gsGoal gs of
            GoalCollect col _ -> lookupColor tallies col
            GoalUfo _ -> uAbs
            _ -> 0
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
          gs' =
            gs
              { gsBoard = board1
              , gsScore = score'
              , gsCollected = collected'
              , gsColorBag = mergeTallies (gsColorBag gs) tallies
              , gsStonesCleared = gsStonesCleared gs + stonesHit
              , gsChestsCleared = gsChestsCleared gs + chestsHit
              , gsHoneyCleared = gsHoneyCleared gs + honeyHit
              , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
              , gsCookiesCollected = cookies'
              , gsCakesCleared = cakes'
              , gsSafesOpened = safes'
              , gsGen = g'
              , gsHistory = hist
              , gsHint = Nothing
              , gsCombo = combo
              , gsShuffled = False
              , gsHammers = gsHammers gs - 1
              , gsMoves = gsMoves gs + 2 * spiritHit
              , gsUfos = ufos'
              , gsUfoCollected = ufoCollected'
              , gsCarpetOpen = carpetOpen'
              , gsCarpetsCovered = carpets'
              , gsLastCleared = nub posCleared
              }
          outcome = decideOutcome gs' gained
          gs'' = case outcome of
            Won s -> gs' { gsOver = Just (Won s) }
            Lost s -> gs' { gsOver = Just (Lost s) }
            LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
            _ -> gs'
          gs''' = case outcome of
            MoveApplied _ -> ensurePlayable gs''
            _ -> gs''
      in (gs''', outcome)

-- | Free-swap: spend one charge to swap any two in-bounds cells (need not be adjacent).
useFreeSwap :: Pos -> Pos -> GameState -> (GameState, Outcome)
useFreeSwap p1 p2 gs
  | Just o <- gsOver gs = (gs, o)
  | gsFreeSwaps gs <= 0 = (clearMoveFx gs, InvalidSwap)
  | not (inBounds p1 && inBounds p2) = (clearMoveFx gs, InvalidSwap)
  | p1 == p2 = (clearMoveFx gs, InvalidSwap)
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
             let (boardF, _c, gained, combo, tallies, stonesHit, chestsHit, honeyHit, balloonHit, cookieHit, cakeHit, uAbs, ufos', posCleared, g') =
                   if rainbow
                     then runCascadeScoredFromSeedsWithUfos (Just p2) (rainbowClearSeeds swapped p1 p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
                     else if specialCombo
                       then runCascadeScoredFromSeedsWithUfos (Just p2) (comboClearSeeds swapped p1 p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
                       else runCascadeScoredWithUfos (Just p2) (gsUfos gs) (gsPortals gs) (gsGen gs) swapped
                 board1 = spreadSteam (spreadChoco (spreadVines boardF))
                 score' = gsScore gs + gained
                 hist = take 20 (snapshot gs : gsHistory gs)
                 ufoCollected' = gsUfoCollected gs + uAbs
                 cookies' = gsCookiesCollected gs + cookieHit
                 cakes' = gsCakesCleared gs + cakeHit
                 safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
                 spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
                 safes' = gsSafesOpened gs + safesHit
                 (carpetOpen', carpetHit) =
                   coverCarpets
                     (gsCarpetOpen gs)
                     (posCleared ++ carpetVacateSeeds (gsBoard gs) boardF)
                 carpets' = gsCarpetsCovered gs + carpetHit
                 collectDelta = case gsGoal gs of
                   GoalCollect col _ -> lookupColor tallies col
                   GoalUfo _ -> uAbs
                   _ -> 0
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
                 gs' =
                   gs
                     { gsBoard = board1
                     , gsScore = score'
                     , gsCollected = collected'
                     , gsColorBag = mergeTallies (gsColorBag gs) tallies
                     , gsStonesCleared = gsStonesCleared gs + stonesHit
                     , gsChestsCleared = gsChestsCleared gs + chestsHit
                     , gsHoneyCleared = gsHoneyCleared gs + honeyHit
                     , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
                     , gsCookiesCollected = cookies'
                     , gsCakesCleared = cakes'
                     , gsSafesOpened = safes'
                     , gsGen = g'
                     , gsHistory = hist
                     , gsHint = Nothing
                     , gsCombo = combo
                     , gsShuffled = False
                     , gsFreeSwaps = gsFreeSwaps gs - 1
                     , gsMoves = gsMoves gs + 2 * spiritHit
                     , gsUfos = ufos'
                     , gsUfoCollected = ufoCollected'
                     , gsCarpetOpen = carpetOpen'
                     , gsCarpetsCovered = carpets'
                     , gsLastCleared = nub posCleared
                     }
                 outcome = decideOutcome gs' gained
                 gs'' = case outcome of
                   Won s -> gs' { gsOver = Just (Won s) }
                   Lost s -> gs' { gsOver = Just (Lost s) }
                   LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
                   _ -> gs'
                 gs''' = case outcome of
                   MoveApplied _ -> ensurePlayable gs''
                   _ -> gs''
             in (gs''', outcome)

-- | Cross clear: spend one charge to clear row+col through a cell, then cascade.
-- Does not consume a move.
useCrossClear :: Pos -> GameState -> (GameState, Outcome)
useCrossClear p gs
  | Just o <- gsOver gs = (gs, o)
  | gsCrossClears gs <= 0 = (clearMoveFx gs, InvalidSwap)
  | not (inBounds p) = (clearMoveFx gs, InvalidSwap)
  | otherwise =
      let seeds = crossClearSeeds p
          (boardH, _n, gained, combo, tallies, stonesHit, chestsHit, honeyHit, balloonHit, cookieHit, cakeHit, uAbs, ufos', posCleared, g') =
            runCascadeScoredFromSeedsWithUfos Nothing seeds (gsUfos gs) (gsPortals gs) (gsGen gs) (gsBoard gs)
          board1 = spreadSteam (spreadChoco (spreadVines boardH))
          score' = gsScore gs + gained
          hist = take 20 (snapshot gs : gsHistory gs)
          ufoCollected' = gsUfoCollected gs + uAbs
          cookies' = gsCookiesCollected gs + cookieHit
          cakes' = gsCakesCleared gs + cakeHit
          safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
          spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
          safes' = gsSafesOpened gs + safesHit
          (carpetOpen', carpetHit) =
            coverCarpets
              (gsCarpetOpen gs)
              (posCleared ++ carpetVacateSeeds (gsBoard gs) boardH)
          carpets' = gsCarpetsCovered gs + carpetHit
          collectDelta = case gsGoal gs of
            GoalCollect col _ -> lookupColor tallies col
            GoalUfo _ -> uAbs
            _ -> 0
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
          gs' =
            gs
              { gsBoard = board1
              , gsScore = score'
              , gsCollected = collected'
              , gsColorBag = mergeTallies (gsColorBag gs) tallies
              , gsStonesCleared = gsStonesCleared gs + stonesHit
              , gsChestsCleared = gsChestsCleared gs + chestsHit
              , gsHoneyCleared = gsHoneyCleared gs + honeyHit
              , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
              , gsCookiesCollected = cookies'
              , gsCakesCleared = cakes'
              , gsSafesOpened = safes'
              , gsGen = g'
              , gsHistory = hist
              , gsHint = Nothing
              , gsCombo = combo
              , gsShuffled = False
              , gsCrossClears = gsCrossClears gs - 1
              , gsMoves = gsMoves gs + 2 * spiritHit
              , gsUfos = ufos'
              , gsUfoCollected = ufoCollected'
              , gsCarpetOpen = carpetOpen'
              , gsCarpetsCovered = carpets'
              , gsLastCleared = nub posCleared
              }
          outcome = decideOutcome gs' gained
          gs'' = case outcome of
            Won s -> gs' { gsOver = Just (Won s) }
            Lost s -> gs' { gsOver = Just (Lost s) }
            LevelClear s n -> gs' { gsOver = Just (LevelClear s n) }
            _ -> gs'
          gs''' = case outcome of
            MoveApplied _ -> ensurePlayable gs''
            _ -> gs''
      in (gs''', outcome)
