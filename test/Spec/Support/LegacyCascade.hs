{-# LANGUAGE DataKinds #-}
-- | 第 3 项（效果与架构）之前 Match3.Board.Cascade 的连锁核心（逐字副本）：生成器 g 与关卡级钩子手工逐个传递，
-- 回放轮次在反向累积器里攒好再反转。Spec.Effects 用它对照能力类改写后的新路径（盘面、计数、回放、钩子、随机数状态逐项相同）。
-- 数据类型（CascadeRun / CascadeTally / CascadeWave / AfterEntry）与清除 / 沉降 / 补子的底层函数用 src 里的同一份。
module Spec.Support.LegacyCascade
  ( cascadeMatchesWith
  , cascadeMatchesFromWith
  , cascadeSeedsWith
  , cascadeAfterWith
  , cascadeCountdownsWith
  , cascadeCountdownsTracedWith
  , stepCascadeAtWith
  ) where

import Data.List (nub)
import Match3.Board.Cascade (AfterEntry(..), CascadeRun(..), CascadeTally(..), CascadeWave(..), stillRun, zeroTally)
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid
import Match3.Board.Hooks (LevelHooks(..), noHooks)
import Match3.Board.Match
import Match3.Board.Phase (Phase(..), Stage, clearStage, digHoles, fallStage, fullStage, refillStage, stageGrid)
import Match3.Counts (CounterKey(..), Counts, countsFromList, noCounts, singleCount)
import Match3.Element.Event (EndEffect)
import Match3.Element.Registry (Registry, counterWith, endRules, pushableWith)
import Match3.Element.Types (EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import System.Random (RandomGen)

-- | 沉降时被边缘收走的格按各自的 counter 计数（内置只有饼干 → CountCookies，与旧「底行收饼干计入饼干数」相同）。
withDrained :: Registry -> [(Pos, Cell)] -> Counts -> Counts
withDrained reg drained h = h <> foldMap (hitOf . counterWith reg . snd) drained

-- | 把一组计数加进已有计数（皮带后沉降收走的格）。
addHits :: Counts -> CascadeTally -> CascadeTally
addHits h t = t {ctCounts = ctCounts t <> h}

-- | 清除格在消除前盘面上的计数（按本体定义的 counter）。
--
-- 第 2 项（Monoid）：Counts 是交换幺半群（逐键相加、mempty = 什么都没计），所以「逐格 bump 进累积器」
-- 就是「每格一份计数，foldMap 合起来」；和第 2 项前的 foldl + bumpCount 结果相同（加法与顺序无关，Counts 不存 0）。
hitsOn :: Registry -> Board -> [Pos] -> Counts
hitsOn reg b = foldMap (hitOf . counterWith reg . getCell b)

-- | 一个格子的计数：计数键加 1。保险箱 / 时间精灵的键按前后盘面差计（Game.Tally，元素的 diffCounter），
-- 不在清除格里计（与第 4 刀前的 Hits 相同；内置元素没有把这两个键当 counter 的）。
hitOf :: Maybe CounterKey -> Counts
hitOf Nothing = mempty
hitOf (Just CountSafes) = mempty
hitOf (Just CountSpirits) = mempty
hitOf (Just k) = singleCount k 1

-- | 一组格在盘面 b 上按颜色计数（CountColor；第 5 刀前是按 allColors 排的颜色袋列表）。
colorsOn :: Registry -> Board -> [Pos] -> Counts
colorsOn reg b pos = countsFromList [(CountColor col, countColorWith reg b pos col) | col <- allColors]

--------------------------------------------------------------------------------
-- 公共的一轮：沉降 + 补子、整轮吸收

-- | 一轮「挖空 → 沉降（重力 / 边缘收集 / 传送门）→ 补子」的结果。
data Round = Round
  { rdWave  :: CascadeWave  -- ^ 本轮回放（含本轮得分）
  , rdNext  :: Stage 'Full  -- ^ 补子之后的满盘（下一轮从这里开始；= cwAfter rdWave）
  , rdCells :: Int          -- ^ 清除格数
  , rdHits  :: Counts       -- ^ 清除格在消除前盘面上的计数 + 沉降时被边缘收走的格的计数
  , rdSites :: [Pos]        -- ^ 沉降时被边缘收走的格
  }

rdAfter :: Round -> Board
rdAfter = stageGrid . rdNext

-- | 由一轮的三个阶段拼出回放记录。cwHoles 必须是「消除后、下落前」的盘面：
-- 下落后的盘面同样是 MBoard，改动前误传也能编译；现在它是 Stage 'Fallen，传进来编译不过。
waveOf :: Stage 'Full -> [Pos] -> [Pos] -> Stage 'Cleared -> Stage 'Full -> Score -> CascadeWave
waveOf before cleared drained holes after =
  CascadeWave (stageGrid before) cleared drained (stageGrid holes) (stageGrid after)

-- | 所有连锁起手共用的一轮（第 3 刀前在匹配 / 飞碟 / 种子 / 种子后飞碟 / 皮带后 / 步末后 / 单轮里逐行重复 7 份）：
-- 给出消除前盘面 before、本轮清除结果 (挖空盘面, 清除数, 清除格) 与波次 w（得分 = scoreForWave w 清除数），
-- 沉降后按行优先逐个空洞补子（第 8 刀起按补子策略 activeRefill；缺省策略每洞恰好一次 randomColor，随机数顺序与原先相同）。
-- 阶段按类型走：Cleared --fallStage--> Fallen --refillStage--> Full（调换两步的顺序编译不过）。
settleRound :: RandomGen g => Registry -> LevelHooks -> g -> Stage 'Full -> (Stage 'Cleared, Int, [Pos]) -> Int -> (Round, g)
settleRound reg hooks g before (holes, n, pos) w =
  let (fallen, drained) = fallStage reg hooks holes                  -- 消除 → 下落
      (after, g') = refillStage (activeRefill reg hooks) g fallen    -- 下落 → 补子
      sites = map fst drained
      wave = waveOf before pos sites holes after (scoreForWave w n)
  in (Round wave after n (withDrained reg drained (hitsOn reg (stageGrid before) pos)) sites, g')

-- | 补子后的整轮吸收（钩子 onAbsorb：关卡级元素回复 Refilled 消息，内置 = 飞碟）。吸到格子时吸收单独成一轮
-- （clearUfoAbsorbed → settleRound，波次 w）；返回 (推进后的钩子, Just (吸收轮, 其中被吸走的格数), 生成器)。
-- 第 3 刀前匹配连锁与种子起手各有一份。
absorbRound :: RandomGen g => Registry -> LevelHooks -> g -> Board -> Int -> (LevelHooks, Maybe (Round, Int), g)
absorbRound reg hooks g b w =
  let (absorbed, hooks') = onAbsorb hooks b
  in if null absorbed
       then (hooks', Nothing, g)
       else
         let bS = fullStage b
             cr@(_, _, pos) = clearStage (\x -> clearUfoAbsorbedWith reg x absorbed) bS
             (rd, g') = settleRound reg hooks' g bS cr w
         in (hooks', Just (rd, length [p | p <- absorbed, p `elem` pos]), g')

--------------------------------------------------------------------------------
-- 核心：普通匹配连锁

-- | cascadeMatches（指定注册表）。
cascadeMatchesWith :: RandomGen g => Registry -> Maybe Pos -> LevelHooks -> g -> Board -> CascadeRun g
cascadeMatchesWith reg = cascadeMatchesFromWith reg 0

-- | cascadeMatchesFrom（指定注册表）。
-- 每轮：clearMatchesDetailed → settleRound → absorbRound（若飞碟吸到格子，吸收单独算下一轮）。没有匹配时最大波次 = startW。
cascadeMatchesFromWith :: RandomGen g => Registry -> Int -> Maybe Pos -> LevelHooks -> g -> Board -> CascadeRun g
cascadeMatchesFromWith reg startW prefer0 hooks0 g0 b0 =
  go prefer0 g0 b0 0 0 startW noCounts hooks0 [] []
  where
    -- clearedRev / wavesRev：反向累积（按块 / 按轮），收尾时再反转
    go pref g b cells score maxW hits hooks clearedRev wavesRev
      | not (hasAnyMatchWith reg b) =
          CascadeRun b (CascadeTally cells score maxW hits (nub (concat (reverse clearedRev)))) hooks (reverse wavesRev) g
      | otherwise =
          let wave = maxW + 1
              bS = fullStage b
              cr@(_, n, pos) = clearStage (clearMatchesDetailedWith reg pref) bS
              (r1, g1) = settleRound reg hooks g bS cr wave
              b1 = rdAfter r1
              posD = nub (pos ++ rdSites r1)
              score1 = score + cwScore (rdWave r1)
              hits1 = hits <> rdHits r1 <> colorsOn reg b posD
              (hooks', absorbed, g2) = absorbRound reg hooks g1 b1 (wave + 1)
          in case absorbed of
               Nothing ->
                 go Nothing g2 b1 (cells + n) score1 wave hits1
                   hooks' (posD : clearedRev) (rdWave r1 : wavesRev)
               Just (r2, nAbs) ->
                 -- 飞碟吸收单独算一轮（波次 wave + 1）
                 let pos2 = cwCleared (rdWave r2)
                 in go Nothing g2 (rdAfter r2) (cells + n + rdCells r2) (score1 + cwScore (rdWave r2)) (wave + 1)
                      (hits1 <> rdHits r2 <> colorsOn reg b1 pos2 <> singleCount CountUfo nAbs)
                      hooks' (rdSites r2 : pos2 : posD : clearedRev) (rdWave r2 : rdWave r1 : wavesRev)

--------------------------------------------------------------------------------
-- 核心：种子起手

-- | cascadeSeeds（指定注册表）：种子清除一轮（波次 1）→ 整轮吸收（波次 2）→ 普通匹配续连锁。
cascadeSeedsWith :: RandomGen g => Registry -> Maybe Pos -> [Pos] -> LevelHooks -> g -> Board -> CascadeRun g
cascadeSeedsWith reg prefer seeds hooks0 g b
  | null seeds = cascadeMatchesWith reg prefer hooks0 g b
  | otherwise =
      let bS = fullStage b
          cr@(_, n, pos) = clearStage (\x -> clearFromSeedsDetailedWith reg prefer x seeds) bS
          (r0, g1) = settleRound reg hooks0 g bS cr 1
          b1 = rdAfter r0
          (hooks1, absorbed, g1') = absorbRound reg hooks0 g1 b1 2
          (wU, b1', nU, scoreU, hitsU, posU) = case absorbed of
            Nothing -> ([], b1, 0, 0, noCounts, [])
            Just (rU, nAbs) ->
              let pos2 = cwCleared (rdWave rU)
              in ( [rdWave rU], rdAfter rU, rdCells rU, cwScore (rdWave rU)
                 , rdHits rU <> colorsOn reg b1 pos2 <> singleCount CountUfo nAbs, nub (pos2 ++ rdSites rU) )
          wavesDone = (if n > 0 then 1 else 0) + (if nU > 0 then 1 else 0)
          -- 续连锁：波次倍数接在起手轮之后
          rest = cascadeMatchesFromWith reg wavesDone Nothing hooks1 g1' b1'
          t2r = crTally rest
          maxW = if ctCells t2r > 0 then ctMaxWave t2r else wavesDone
          tally =
            CascadeTally
              (n + nU + ctCells t2r)
              (cwScore (rdWave r0) + scoreU + ctScore t2r)
              maxW
              (rdHits r0 <> colorsOn reg b pos <> hitsU <> ctCounts t2r)
              (nub (pos ++ rdSites r0 ++ posU ++ ctCleared t2r))
      in CascadeRun (crBoard rest) tally (crHooks rest) (rdWave r0 : wU ++ crWaves rest) (crGen rest)

--------------------------------------------------------------------------------
-- 核心：皮带移位之后 / 步末之后的补结算

-- | 皮带后 / 步末后的补结算：挖空（步末的空洞；皮带没有）→ 沉降（重力 / 边缘收集 / 传送门）+ 补子；
-- 盘面有变化或收走了格时记一个只有沉降的轮次；之后成消则接普通连锁（波次从 1 起）。
-- 步末入口没有空洞、边上也没有待收格时沉降是恒等、refill 不消耗随机数，结果等于旧的「成消才连锁」：
-- 内置元素的步末从不留下空洞（38 关 × 多种子扫描确认，见 docs/testing.md），金标准因此不变。
cascadeAfterWith :: RandomGen g => Registry -> AfterEntry -> LevelHooks -> g -> Board -> CascadeRun g
cascadeAfterWith reg entry hooks g b = case entry of
  AfterBelt
    | hasAnyMatchWith reg b -> cascadeMatchesWith reg Nothing hooks g b
    | otherwise -> settleThenCascade []
  AfterEnd holes -> settleThenCascade holes
  where
    settleThenCascade holes =
      let bS = fullStage b
          (rd, g1) = settleRound reg hooks g bS (digHoles holes bS, 0, []) 0
          b1 = rdAfter rd
          sites = rdSites rd
          settleWave = [rdWave rd | b1 /= b || not (null sites)]
      in if hasAnyMatchWith reg b1
           then
             let r = cascadeMatchesWith reg Nothing hooks g1 b1
                 t = crTally r
             in r { crTally = (addHits (rdHits rd) t) {ctCleared = nub (sites ++ ctCleared t)}
                  , crWaves = settleWave ++ crWaves r }
           else CascadeRun b1 (addHits (rdHits rd) zeroTally) {ctCleared = sites} hooks settleWave g1

--------------------------------------------------------------------------------
-- 核心：倒计时

-- | cascadeCountdowns（指定注册表）：依次跑 PhaseTick 阶段的步末规则，再合并各规则的引爆种子。
cascadeCountdownsWith :: RandomGen g => Registry -> LevelHooks -> g -> Board -> CascadeRun g
cascadeCountdownsWith reg hooks0 g b = snd (cascadeCountdownsTracedWith reg hooks0 g b)

-- | 带记录的 cascadeCountdownsWith：PhaseTick 规则只跑一遍，同时返回各规则的 (前盘, 后盘, 效果)
-- （空效果不记，按规则顺序）和倒计时连锁。步末记录由调用方按轮次号包成 EndStep。
cascadeCountdownsTracedWith
  :: RandomGen g => Registry -> LevelHooks -> g -> Board -> ([(Board, Board, EndEffect)], CascadeRun g)
cascadeCountdownsTracedWith reg hooks0 g b =
  let rules = endRules reg PhaseTick
      ctx = EndCtx [] [] (pushableWith reg)
      step (accRev, before) r =
        let (eff, after) = erRun r ctx before
        in ([(before, after, e) | Just e <- [eff]] ++ accRev, after)
      (stepsRev, bTick) = foldl step ([], b) rules
      steps = reverse stepsRev
      seeds = nub (concatMap (\r -> erSeeds r bTick) rules)
      run
        | null seeds = stillRun bTick hooks0 g
        | otherwise = cascadeSeedsWith reg Nothing seeds hooks0 g bTick
  in (steps, run)

--------------------------------------------------------------------------------
-- 单轮

-- | 恰好一轮匹配消除 + 沉降补子（指定注册表；没有关卡级钩子：不跑飞碟、无传送门）；无匹配时返回 Nothing。
-- 与 cascadeMatchesFromWith 的单轮是同一组调用：clear → settleRound（沉降 + 补子）；
-- prefer 为第一轮新特殊块的优先生成位。
stepCascadeAtWith :: RandomGen g => Registry -> Maybe Pos -> g -> Board -> Maybe (Board, Int, g)
stepCascadeAtWith reg prefer g b
  | not (hasAnyMatchWith reg b) = Nothing
  | otherwise =
      let bS = fullStage b
          cr@(_, n, _) = clearStage (clearMatchesDetailedWith reg prefer) bS
          (rd, g') = settleRound reg noHooks g bS cr 1
      in Just (rdAfter rd, n, g')
