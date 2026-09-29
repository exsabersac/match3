{-# LANGUAGE ScopedTypeVariables #-}

-- | 连锁：结算用的 runCascade* / runPostBeltCascade / resolveCountdowns，
-- 与逐轮回放用的 traceCascade* / tracePostBeltCascade / traceCountdowns 放在同一个模块，便于对照同步。
--
-- 依赖：Grid、Match、Clear、Gravity、Countdown、Ufo。
-- 同步（重要）：trace* 与对应的 run* 必须按相同步骤、相同随机数消耗顺序重算，
-- 护栏测试 trace_cascade_final_equals_stabilized / trace_seeds_final_equals_stabilized。
-- 第一刀只搬移代码，两套实现仍是平行的两份（见 docs/architecture.md「逐轮回放与规则的同步」）。
module Match3.Board.Cascade
  ( stepCascade
  , stepCascadeAt
  , stepCascadeDetailed
  , runCascade
  , runCascadeAt
  , runCascadeScored
  , runCascadeScoredWithUfos
  , runCascadeScoredWithUfosFromWave
  , runCascadeScoredFromSeeds
  , runCascadeScoredFromSeedsWithUfos
  , runPostBeltCascade
  , resolveCountdowns
  , CascadeWave(..)
  , traceCascade
  , traceCascadeFromWave
  , traceCascadeFromSeeds
  , tracePostBeltCascade
  , traceCountdowns
  ) where

import Data.List (nub)
import Match3.Countdown
  ( countdownsAtZero
  , explodeSeedsFor
  , tickCountdowns
  )
import Match3.Types
import Match3.Ufo (Ufo, stepUfos)
import System.Random (RandomGen)
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid
import Match3.Board.Match

-- | 一步连锁（无优先生成位）；无匹配时返回 Nothing。
stepCascade :: RandomGen g => g -> Board -> Maybe (Board, Int, g)
stepCascade = stepCascadeAt Nothing

-- | stepCascadeDetailed 的简化版：只返回新盘面、清除数和生成器。
stepCascadeAt :: RandomGen g => Maybe Pos -> g -> Board -> Maybe (Board, Int, g)
stepCascadeAt prefer g b =
  case stepCascadeDetailed prefer [] g b of
    Nothing -> Nothing
    Just (b', n, _, _, _, _, _, _, _, g') -> Just (b', n, g')

-- | Cascade step returning cleared positions for color tallying.
-- Extra ints: stones, chests, honey, balloons, cookies, cakes fully cleared.
stepCascadeDetailed
  :: RandomGen g
  => Maybe Pos
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> Maybe (Board, Int, [Pos], Int, Int, Int, Int, Int, Int, g)
stepCascadeDetailed prefer portals g b
  | not (hasAnyMatch b) = Nothing
  | otherwise =
      let (mb, n, pos) = clearMatchesDetailed prefer b
          stonesHit =
            length [p | p <- pos, isStone (getCell b p)]
          chestsHit =
            length [p | p <- pos, isChest (getCell b p)]
          honeyHit =
            length [p | p <- pos, isHoney (getCell b p)]
          balloonHit =
            length [p | p <- pos, isBalloon (getCell b p)]
          cookiesCleared =
            length [p | p <- pos, isCookie (getCell b p)]
          cakesHit =
            length [p | p <- pos, isCake (getCell b p)]
          (settled, cookiesFallen, cookSites) = settleBoardPortals portals mb
          (b', g') = refill g settled
      in Just (b', n, nub (pos ++ cookSites), stonesHit, chestsHit, honeyHit, balloonHit, cookiesCleared + cookiesFallen, cakesHit, g')

-- | 连锁到稳定（无优先生成位），返回最终盘面、累计清除数、生成器。
runCascade :: RandomGen g => g -> Board -> (Board, Int, g)
runCascade = runCascadeAt Nothing

-- | First cascade step uses prefer (swap dest) for special spawn; later steps don't.
runCascadeAt :: RandomGen g => Maybe Pos -> g -> Board -> (Board, Int, g)
runCascadeAt prefer g b = case stepCascadeAt prefer g b of
  Nothing -> (b, 0, g)
  Just (b', n, g') ->
    let (b'', n', g'') = runCascadeAt Nothing g' b'
    in (b'', n + n', g'')

-- | Cascade with per-wave combo scoring + color tallies from cleared cells.
-- Returns (board, cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, gen).
runCascadeScored
  :: RandomGen g
  => Maybe Pos
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, g)
runCascadeScored prefer g b =
  let (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, _uAbs, _ufos, _cleared, g') =
        runCascadeScoredWithUfos prefer [] [] g b
  in (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, g')


-- | Like runCascadeScored but steps UFOs after each cascade wave (吸同色 + 移格).
-- UFO absorb clears seeds without expandSpecials: absorbing a Bomb/Line/Rainbow
-- removes it (GoalUfo counts) but must not detonate (吸走 ≠ 引爆).
-- portals: bidirectional pairs applied during settle (落入 A 从 B 出).
-- Returns (... stones, chests, honey, balloons, cookies, cakes, ufoAbsorbed, ufos', clearedPos, gen).
-- 同步约束：逐轮回放 traceCascade* 按相同步骤与随机数消耗顺序重算；改这里（及种子 / 倒计时 /
-- 皮带后连锁）时必须同步改 trace 版本（护栏：trace_cascade_final_equals_stabilized 等，
-- 见 docs/architecture.md「逐轮回放与规则的同步」）。
runCascadeScoredWithUfos
  :: RandomGen g
  => Maybe Pos
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredWithUfos prefer ufos0 portals g b =
  runCascadeScoredWithUfosFromWave 0 prefer ufos0 portals g b

-- | Like runCascadeScoredWithUfos but combo wave numbering continues from startW
-- (waves already completed, e.g. seed clear / UFO absorb before match cascades).
runCascadeScoredWithUfosFromWave
  :: RandomGen g
  => Int
  -> Maybe Pos
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredWithUfosFromWave startW prefer ufos0 portals g b =
  go prefer g b 0 0 startW (zip allColors (repeat 0)) 0 0 0 0 0 0 0 ufos0 []
  where
    go pref g' b' cells score maxW tallies stones chests honey balloons cookies cakes uAbs ufos clearedAcc =
      case stepCascadeDetailed pref portals g' b' of
        Nothing -> (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, uAbs, ufos, nub clearedAcc, g')
        Just (b'', n, pos, stn, cht, hny, bal, cok, cak, g'') ->
          let wave = maxW + 1
              score' = score + scoreForWave wave n
              tallies' =
                [ (col, cnt + countColor b' pos col)
                | (col, cnt) <- tallies
                ]
              (absorbed, ufos') = stepUfos b'' ufos
          in if null absorbed
               then go Nothing g'' b'' (cells + n) score' wave tallies' (stones + stn) (chests + cht) (honey + hny) (balloons + bal) (cookies + cok) (cakes + cak) uAbs ufos' (clearedAcc ++ pos)
               else
                 let (mb, n2, pos2) = clearUfoAbsorbed b'' absorbed
                     stn2 = length [p | p <- pos2, isStone (getCell b'' p)]
                     cht2 = length [p | p <- pos2, isChest (getCell b'' p)]
                     hny2 = length [p | p <- pos2, isHoney (getCell b'' p)]
                     bal2 = length [p | p <- pos2, isBalloon (getCell b'' p)]
                     cok2 = length [p | p <- pos2, isCookie (getCell b'' p)]
                     cak2 = length [p | p <- pos2, isCake (getCell b'' p)]
                     (settled, cokFall, cookSites) = settleBoardPortals portals mb
                     (b3, g3) = refill g'' settled
                     score2 = score' + scoreForWave (wave + 1) n2
                     tallies2 =
                       [ (col, cnt + countColor b'' pos2 col)
                       | (col, cnt) <- tallies'
                       ]
                     uAbs' = uAbs + length [p | p <- absorbed, p `elem` pos2]
                 in go Nothing g3 b3 (cells + n + n2) score2 (wave + 1) tallies2 (stones + stn + stn2) (chests + cht + cht2) (honey + hny + hny2) (balloons + bal + bal2) (cookies + cok + cok2 + cokFall) (cakes + cak + cak2) uAbs' ufos' (clearedAcc ++ pos ++ pos2 ++ cookSites)

-- | First wave clears explicit seeds (rainbow etc.), then normal match cascades.
runCascadeScoredFromSeeds
  :: RandomGen g
  => Maybe Pos
  -> [Pos]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, g)
runCascadeScoredFromSeeds prefer seeds g b =
  let (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, _u, _ufos, _cleared, g') =
        runCascadeScoredFromSeedsWithUfos prefer seeds [] [] g b
  in (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, g')

-- | 种子起手的连锁（彩虹 / 特殊合成 / 倒计时爆炸 / 道具）：先清种子并沉降补子、跑飞碟，
-- 再转入普通匹配连锁（波次编号衔接）。回放版本是 traceCascadeFromSeeds，两者步骤与随机数顺序必须一致。
runCascadeScoredFromSeedsWithUfos
  :: RandomGen g
  => Maybe Pos
  -> [Pos]
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredFromSeedsWithUfos prefer seeds ufos0 portals g b
  | null seeds = runCascadeScoredWithUfos prefer ufos0 portals g b
  | otherwise =
      let (mb, n, pos) = clearFromSeedsDetailed prefer b seeds
          stones0 = length [p | p <- pos, isStone (getCell b p)]
          chests0 = length [p | p <- pos, isChest (getCell b p)]
          honey0 = length [p | p <- pos, isHoney (getCell b p)]
          balloons0 = length [p | p <- pos, isBalloon (getCell b p)]
          cookies0 = length [p | p <- pos, isCookie (getCell b p)]
          cakes0 = length [p | p <- pos, isCake (getCell b p)]
          (settled0, cookiesFall0, cookSites0) = settleBoardPortals portals mb
          (b1, g1) = refill g settled0
          score0 = scoreForWave 1 n
          tallies0 = [(col, countColor b pos col) | col <- allColors]
          (absorbed, ufos1) = stepUfos b1 ufos0
          (b1', g1', nU, stonesU, chestsU, honeyU, balloonsU, cookiesU, cakesU, talliesU, uAbs0, ufos2, posU) =
            if null absorbed
              then (b1, g1, 0, 0, 0, 0, 0, 0, 0, zip allColors (repeat 0), 0, ufos1, [])
              else
                let (mb2, n2, pos2) = clearUfoAbsorbed b1 absorbed
                    stn2 = length [p | p <- pos2, isStone (getCell b1 p)]
                    cht2 = length [p | p <- pos2, isChest (getCell b1 p)]
                    hny2 = length [p | p <- pos2, isHoney (getCell b1 p)]
                    bal2 = length [p | p <- pos2, isBalloon (getCell b1 p)]
                    cok2 = length [p | p <- pos2, isCookie (getCell b1 p)]
                    cak2 = length [p | p <- pos2, isCake (getCell b1 p)]
                    (settled2, cokFall2, cookSitesU) = settleBoardPortals portals mb2
                    (b2u, g2u) = refill g1 settled2
                    t2 = [(col, countColor b1 pos2 col) | col <- allColors]
                in (b2u, g2u, n2, stn2, cht2, hny2, bal2, cok2 + cokFall2, cak2, t2, length [p | p <- absorbed, p `elem` pos2], ufos1, nub (pos2 ++ cookSitesU))
          wavesDone = (if n > 0 then 1 else 0) + (if nU > 0 then 1 else 0)
          (b2, cells2, score2, maxW2, tallies2, stones2, chests2, honey2, balloons2, cookies2, cakes2, uAbs2, ufos3, cleared2, g2) =
            -- Continue wave multipliers after seed (+ optional UFO) clear.
            runCascadeScoredWithUfosFromWave wavesDone Nothing ufos2 portals g1' b1'
          mergeT a b' =
            [ (col, lc a col + lc b' col) | col <- allColors ]
          lc xs col = maybe 0 id (lookup col xs)
          maxW = if cells2 > 0 then maxW2 else wavesDone
      in ( b2
         , n + nU + cells2
         , score0 + (if nU > 0 then scoreForWave 2 nU else 0) + score2
         , maxW
         , mergeT (mergeT tallies0 talliesU) tallies2
         , stones0 + stonesU + stones2
         , chests0 + chestsU + chests2
         , honey0 + honeyU + honey2
         , balloons0 + balloonsU + balloons2
         , cookies0 + cookiesFall0 + cookiesU + cookies2
         , cakes0 + cakesU + cakes2
         , uAbs0 + uAbs2
         , ufos3
         , nub (pos ++ cookSites0 ++ posU ++ cleared2)
         , g2
         )

-- | 皮带后：有匹配则全连锁；无匹配仍 settle（收皮带送到底行的饼干）；settle 后再匹配则续连锁。
-- | After conveyor shift: cascade when the shift formed a match (settle inside
-- as usual). When it did *not*, still settle (gravity→drain→portal→gravity→drain)
-- so cookies belt-delivered onto the bottom row collect toward GoalCookie —
-- previously a no-match belt left bottom cookies stranded until a later clear.
-- If that settle creates a match (e.g. portal), cascade it and add drained cookies.
runPostBeltCascade
  :: RandomGen g
  => [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runPostBeltCascade ufos portals g boardBelt
  | hasAnyMatch boardBelt =
      runCascadeScoredWithUfos Nothing ufos portals g boardBelt
  | otherwise =
      let (settled, nCook, cookSites) = settleBoardPortals portals (toM boardBelt)
          (b1, g1) = refill g settled
      in if hasAnyMatch b1
           then
             let (b2, cells, score, maxW, tallies, st, ch, h, bal, cok, cak, uAbs, ufos', cleared, g2) =
                   runCascadeScoredWithUfos Nothing ufos portals g1 b1
             in (b2, cells, score, maxW, tallies, st, ch, h, bal, cok + nCook, cak, uAbs, ufos', nub (cookSites ++ cleared), g2)
           else
             (b1, 0, 0, 0, zip allColors (repeat 0), 0, 0, 0, 0, nCook, 0, 0, ufos, cookSites, g1)

-- | After a successful cascade: tick countdown bombs; any at 0 explode (3×3) + cascade.
-- Threads UFOs + portals so explode settle still teleports / absorbs (到期爆炸不丢飞碟与门).
-- Returns (... stones, chests, honey, balloons, cookies, cakes, ufoAbsorbed, ufos', clearedPos, gen).
resolveCountdowns
  :: RandomGen g
  => [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
resolveCountdowns ufos0 portals g b =
  let bTick = tickCountdowns b
      zeros = countdownsAtZero bTick
  in if null zeros
       then (bTick, 0, 0, 0, zip allColors (repeat 0), 0, 0, 0, 0, 0, 0, 0, ufos0, [], g)
       else
         let seeds = explodeSeedsFor bTick
             (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, uAbs, ufos', cleared, g') =
               runCascadeScoredFromSeedsWithUfos Nothing seeds ufos0 portals g bTick
         in (b', cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, uAbs, ufos', cleared, g')

--------------------------------------------------------------------------------
-- 逐轮回放（纯函数，仅供前端表现层）
--
-- runCascade* 只返回「连锁结束后的稳定盘面 + 累计计数」，前端没法让玩家看清每一轮消了
-- 哪些格。下面这组 trace* 函数与对应的 runCascade* 走**完全相同**的步骤（同一组
-- clear / settle / refill / stepUfos 调用、同样的随机数消耗顺序），额外记录每一轮的
-- 中间快照。它们不改变任何已有函数；结算仍以 runCascade* / trySwap 为准。
-- 一致性由测试保证：最终盘面、随机数生成器、逐轮得分之和、清除格并集都与原函数一致。
--------------------------------------------------------------------------------

-- | 连锁中的一轮（一次「消除 → 下落 → 补子」）。
data CascadeWave = CascadeWave
  { cwBefore  :: Board           -- ^ 本轮消除前的盘面
  , cwCleared :: [Pos]           -- ^ 本轮被消掉的格（真消除 + 被打碎的障碍）；可能为空（仅沉降）
  , cwDrained :: [Pos]           -- ^ 沉降途中底行被收走的饼干位
  , cwHoles   :: [[Maybe Cell]]  -- ^ 消除并放下新特殊块之后、下落之前（Nothing = 空洞）
  , cwAfter   :: Board           -- ^ 重力 / 传送门 / 补子之后
  , cwScore   :: Score           -- ^ 本轮得分（与 runCascade* 的波次计分相同）
  } deriving (Eq, Show)

-- | 与 runCascadeScoredWithUfos 相同的连锁，返回每一轮快照 + 最终盘面 / 飞碟 / 生成器。
traceCascade
  :: RandomGen g
  => Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCascade = traceCascadeFromWave 0

-- | 与 runCascadeScoredWithUfosFromWave 相同（波次编号从 startW 之后继续）。
traceCascadeFromWave
  :: RandomGen g
  => Int -> Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCascadeFromWave startW prefer0 ufos0 portals g0 b0 = go prefer0 g0 b0 startW ufos0
  where
    go pref g b maxW ufos
      | not (hasAnyMatch b) = ([], b, ufos, g)
      | otherwise =
          let (mb, n, pos) = clearMatchesDetailed pref b
              (b', cookSites, g') = settleRefill portals g mb
              wave = maxW + 1
              w1 = CascadeWave b pos cookSites mb b' (scoreForWave wave n)
              (absorbed, ufos') = stepUfos b' ufos
          in if null absorbed
               then
                 let (ws, bF, uF, gF) = go Nothing g' b' wave ufos'
                 in (w1 : ws, bF, uF, gF)
               else
                 -- 飞碟吸收单独算一轮（与 runCascadeScoredWithUfosFromWave 的 wave + 1 一致）
                 let (mb2, n2, pos2) = clearUfoAbsorbed b' absorbed
                     (b3, cook2, g3) = settleRefill portals g' mb2
                     w2 = CascadeWave b' pos2 cook2 mb2 b3 (scoreForWave (wave + 1) n2)
                     (ws, bF, uF, gF) = go Nothing g3 b3 (wave + 1) ufos'
                 in (w1 : w2 : ws, bF, uF, gF)

-- | 与 runCascadeScoredFromSeedsWithUfos 相同：第一轮清种子（彩虹 / 特殊组合 / 道具 / 倒计时爆炸），
-- 可选飞碟吸收一轮，再接普通匹配连锁。
traceCascadeFromSeeds
  :: RandomGen g
  => Maybe Pos -> [Pos] -> [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCascadeFromSeeds prefer seeds ufos0 portals g b
  | null seeds = traceCascade prefer ufos0 portals g b
  | otherwise =
      let (mb, n, pos) = clearFromSeedsDetailed prefer b seeds
          (b1, cook0, g1) = settleRefill portals g mb
          w0 = CascadeWave b pos cook0 mb b1 (scoreForWave 1 n)
          (absorbed, ufos1) = stepUfos b1 ufos0
          (wU, nU, b1', g1') =
            if null absorbed
              then ([], 0, b1, g1)
              else
                let (mb2, n2, pos2) = clearUfoAbsorbed b1 absorbed
                    (b2u, cookU, g2u) = settleRefill portals g1 mb2
                in ([CascadeWave b1 pos2 cookU mb2 b2u (if n2 > 0 then scoreForWave 2 n2 else 0)], n2, b2u, g2u)
          wavesDone = (if n > 0 then 1 else 0) + (if nU > 0 then 1 else 0)
          (ws, bF, uF, gF) = traceCascadeFromWave wavesDone Nothing ufos1 portals g1' b1'
      in (w0 : wU ++ ws, bF, uF, gF)

-- | 与 runPostBeltCascade 相同：皮带移位后有匹配就连锁；否则先沉降（收饼干），沉降后成消再连锁。
tracePostBeltCascade
  :: RandomGen g
  => [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
tracePostBeltCascade ufos portals g boardBelt
  | hasAnyMatch boardBelt = traceCascade Nothing ufos portals g boardBelt
  | otherwise =
      let mb = toM boardBelt
          (b1, cookSites, g1) = settleRefill portals g mb
          settleWave =
            [ CascadeWave boardBelt [] cookSites mb b1 0
            | b1 /= boardBelt || not (null cookSites)
            ]
      in if hasAnyMatch b1
           then
             let (ws, bF, uF, gF) = traceCascade Nothing ufos portals g1 b1
             in (settleWave ++ ws, bF, uF, gF)
           else (settleWave, b1, ufos, g1)

-- | 与 resolveCountdowns 相同：倒计时 -1，归零的 3×3 爆炸后连锁。
-- 返回的最终盘面在没有爆炸时就是 tick 之后的盘面（数字减一，不产生回放轮次）。
traceCountdowns
  :: RandomGen g
  => [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCountdowns ufos0 portals g b =
  let bTick = tickCountdowns b
  in if null (countdownsAtZero bTick)
       then ([], bTick, ufos0, g)
       else traceCascadeFromSeeds Nothing (explodeSeedsFor bTick) ufos0 portals g bTick
