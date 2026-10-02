{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 连锁：**单一实现**。每一种连锁起手（普通匹配 / 种子 / 皮带后 / 倒计时）只有一个核心函数，
-- 同时产出「结算计数」（CascadeTally）和「逐轮回放」（[CascadeWave]），两者来自同一次计算，
-- 结算与回放因此天然一致（第二刀之前是 runCascade* 与 traceCascade* 两份平行实现）。
--
-- 核心：cascadeMatchesFromWith / cascadeSeedsWith / cascadeAfterWith（皮带后 / 步末后）/ cascadeCountdownsWith（全部收 Registry），返回 CascadeRun。
-- 第 3 刀（审核报告第 11 条）：每一轮的「沉降 + 补子」只在 settleRound、整轮吸收只在 absorbRound 各写一次。
-- 第三刀删除了旧元组兼容层（runCascade* / resolveCountdowns / runPostBeltCascade / traceCascade*），
-- 调用方直接读 CascadeRun / CascadeTally 的字段；stepCascadeAtWith 保留为「恰好一轮」的小工具。段 2c 起本模块不依赖内置注册表（便捷旧名在 Match3.Board.Default）。
--
-- 第 7 刀（7a）：关卡级状态不再作参数（第 3 刀留下的 @[Ufo]@ / 传送门对），只收一个钩子记录 'LevelHooks'
-- （Match3.Board.Hooks；沉降节拍 onSettle、补子后 onAbsorb），推进后的钩子在 CascadeRun 的 crHooks 里。
--
-- 依赖：Grid、Match、Clear、Gravity、Hooks、元素注册表（计数键 counter、倒计时 = PhaseTick 步末规则）。
-- 类型层（Haskell 特性第 1 项）：每一轮的盘面带阶段标签（Match3.Board.Phase）——消除得到 Stage 'Cleared，
-- 下落得到 Stage 'Fallen，补子回到 Stage 'Full；settleRound 与回放记录 waveOf 只收对应阶段的盘面，
-- 「没下落就补子」「cwHoles 记成下落后的盘面」之类的错位编译不过。
-- 效果（Haskell 特性第 3 项，见 docs/haskell-features/03-效果与架构.md）：连锁核心写成只依赖能力类的程序
-- （cascadeMatchesFromM / cascadeSeedsM / cascadeAfterM / cascadeCountdownsM / stepCascadeAtM，约束 MonadCascade m：
-- 补子 MonadRefill、关卡级钩子 MonadLevelHooks、发出回放 MonadWaves，见 Match3.Board.Effect）。对外的 @...With@ 入口签名不变，
-- 用纯解释器 PureCascade 运行（runCascade）；同一个程序换成追踪解释器（runCascadeTraced）就额外得到事件日志。
-- 第 3 项前生成器 g 与钩子 hooks 在每个函数里手工逐个传递（g → g1 → g2 …），回放在反向累积器里攒好再反转。
-- 不变量：每轮 = clear → settleRound（settleDrain → refill）→ 整轮吸收 absorbRound（钩子 onAbsorb，内置 = 飞碟；→ 吸收单独一轮），随机数按此顺序消耗；
-- 计数口径（逐字保持旧实现，由金标准锁定）：
--   * 匹配轮的颜色袋按「清除格 ∪ 本轮底行收饼干位」在消除前盘面上计色；种子轮 / 飞碟轮只按清除格计色；
--   * 障碍计数按清除格在消除前盘面上的格子种类计；饼干 = 被清除的饼干 + 沉降时底行收走的饼干；
--     第 4 刀起这些个数（含飞碟吸收 CountUfo、扩展元素 CountNamed）统一在 ctCounts :: Counts，第 5 刀起颜色袋也在（CountColor）；
--   * 种子起手的最大波次：续连锁有清除时取续连锁的最大波次，否则取「已完成的起手轮数」。
module Match3.Board.Cascade
  ( -- * 记录版（单一实现）
    CascadeTally(..)
  , zeroTally
  , CascadeRun(..)
  , stillRun
    -- * 指定注册表（元素框架；内置注册表的便捷入口见 Match3.Board.Default）
  , cascadeMatchesWith
  , cascadeMatchesFromWith
  , cascadeSeedsWith
  , AfterEntry(..)
  , cascadeAfterWith
  , cascadeCountdownsWith
  , cascadeCountdownsTracedWith
  , endHolesWith
    -- * 回放数据（定义在 Match3.Board.Wave）
  , CascadeWave(..)
    -- * 单轮
  , stepCascadeAtWith
    -- * 连锁程序（第 3 项：只依赖能力类，任选解释器运行；见 Match3.Board.Effect）
  , cascadeMatchesM
  , cascadeMatchesFromM
  , cascadeSeedsM
  , cascadeAfterM
  , cascadeCountdownsM
  , stepCascadeAtM
  , runCascade
  , runCascadeTraced
  ) where

import Control.Monad (when)
import Data.List (nub)
import Match3.Board.Effect
import Match3.Board.Hooks (LevelHooks(..), noHooks)
import Match3.Board.Phase (Phase(..), Stage, clearStage, digHoles, fallStage, fullStage, stageGrid)
import Match3.Board.Wave (CascadeWave(..))
import Match3.Element.Registry (Registry, counterWith, endRules, pushableWith)
import Match3.Element.Event (EndEffect)
import Match3.Counts (CounterKey(..), Counts, countsFromList, noCounts, singleCount)
import Match3.Element.Types (EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import System.Random (RandomGen)
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid
import Match3.Board.Match

--------------------------------------------------------------------------------
-- 数据

-- | 一段连锁的累计计数（替代旧的 15 元元组；第 4 刀起各元素 / 飞碟的个数统一在 ctCounts）。
data CascadeTally = CascadeTally
  { ctCells   :: Int            -- ^ 清除格数（含打碎的障碍）
  , ctScore   :: Score          -- ^ 波次计分之和
  , ctMaxWave :: Int            -- ^ 最大波次（连击数）
  , ctCounts  :: Counts         -- ^ 清除格按本体 counter 计（CountStones … / CountNamed 名字）；各色 CountColor（第 5 刀前的 ctColors）；
                                --   CountCookies 另含沉降时底行收走的饼干；CountUfo = 飞碟吸走的格数（GoalUfo）
  , ctCleared :: [Pos]          -- ^ 清除格 + 收饼干位（GoalCarpet / 前端粒子）
  } deriving (Eq, Show)

-- | 什么都没发生的计数。
zeroTally :: CascadeTally
zeroTally = CascadeTally 0 0 0 noCounts []

-- | 一段连锁的完整结果：终盘、计数、推进后的关卡级钩子、逐轮回放、生成器。
data CascadeRun g = CascadeRun
  { crBoard :: Board
  , crTally :: CascadeTally
  , crHooks :: LevelHooks     -- ^ 推进后的关卡级钩子（第 7 刀前的 crUfos；Game 层从 hookLevel 取回关卡级元素）
  , crWaves :: [CascadeWave]
  , crGen   :: g
  }

-- | 没有任何清除 / 沉降的一段（盘面、钩子、生成器原样）。
stillRun :: Board -> LevelHooks -> g -> CascadeRun g
stillRun b hooks g = CascadeRun b zeroTally hooks [] g

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
--
-- 第 3 项（效果）：签名只说用到哪两种效果——读钩子（沉降节拍、补子策略）与补子（随机数）；不发出回放
-- （是否记这一轮由调用方决定，见 'cascadeAfterM'）。第 3 项前是 @RandomGen g => ... -> LevelHooks -> g -> ... -> (Round, g)@，
-- 生成器由调用方手工传进传出。
settleRoundM :: (MonadRefill m, MonadLevelHooks m) => Registry -> Stage 'Full -> (Stage 'Cleared, Int, [Pos]) -> Int -> m Round
settleRoundM reg before (holes, n, pos) w = do
  hooks <- currentHooks
  let (fallen, drained) = fallStage reg hooks holes                  -- 消除 → 下落
  after <- refillHoles (activeRefill reg hooks) fallen               -- 下落 → 补子
  let sites = map fst drained
      wave = waveOf before pos sites holes after (scoreForWave w n)
  pure (Round wave after n (withDrained reg drained (hitsOn reg (stageGrid before) pos)) sites)

-- | 一轮并发出它的回放记录（除皮带后 / 步末后的「只沉降」一轮外，每一轮都这样记）。
roundM :: MonadCascade m => Registry -> Stage 'Full -> (Stage 'Cleared, Int, [Pos]) -> Int -> m Round
roundM reg before cr w = do
  rd <- settleRoundM reg before cr w
  emitWave (rdWave rd)
  pure rd

-- | 补子后的整轮吸收（钩子 onAbsorb：关卡级元素回复 Refilled 消息，内置 = 飞碟）。吸到格子时吸收单独成一轮
-- （clearUfoAbsorbed → 一轮，波次 w，已发出回放）；返回 Just (吸收轮, 其中被吸走的格数)。钩子由 absorbHooks 推进。
-- 第 3 刀前匹配连锁与种子起手各有一份。
absorbRoundM :: MonadCascade m => Registry -> Board -> Int -> m (Maybe (Round, Int))
absorbRoundM reg b w = do
  absorbed <- absorbHooks b
  if null absorbed
    then pure Nothing
    else do
      let bS = fullStage b
          cr@(_, _, pos) = clearStage (\x -> clearUfoAbsorbedWith reg x absorbed) bS
      rd <- roundM reg bS cr w
      pure (Just (rd, length [p | p <- absorbed, p `elem` pos]))

-- | 用纯解释器运行一段连锁程序，拼成 CascadeRun（各个 @...With@ 入口都经它，签名与第 3 项前相同）。
runCascade :: LevelHooks -> g -> PureCascade g (Board, CascadeTally) -> CascadeRun g
runCascade hooks g m = ranToRun (runPureCascade hooks g m)

-- | 用追踪解释器运行一段连锁程序：CascadeRun 与 'runCascade' 逐项相同，另附事件日志。
runCascadeTraced :: LevelHooks -> g -> TracedCascade g (Board, CascadeTally) -> (CascadeRun g, [CascadeLog])
runCascadeTraced hooks g m = let (r, logs) = runTracedCascade hooks g m in (ranToRun r, logs)

ranToRun :: Ran g (Board, CascadeTally) -> CascadeRun g
ranToRun (Ran (b, t) hooks waves g) = CascadeRun b t hooks waves g

--------------------------------------------------------------------------------
-- 核心：普通匹配连锁

-- | cascadeMatches（指定注册表）。
cascadeMatchesWith :: RandomGen g => Registry -> Maybe Pos -> LevelHooks -> g -> Board -> CascadeRun g
cascadeMatchesWith reg = cascadeMatchesFromWith reg 0

-- | cascadeMatchesFrom（指定注册表）：纯解释器运行 'cascadeMatchesFromM'。
cascadeMatchesFromWith :: RandomGen g => Registry -> Int -> Maybe Pos -> LevelHooks -> g -> Board -> CascadeRun g
cascadeMatchesFromWith reg startW prefer hooks g b = runCascade hooks g (cascadeMatchesFromM reg startW prefer b)

cascadeMatchesM :: MonadCascade m => Registry -> Maybe Pos -> Board -> m (Board, CascadeTally)
cascadeMatchesM reg = cascadeMatchesFromM reg 0

-- | 普通匹配连锁（程序）。每轮：clearMatchesDetailed → 一轮（沉降 + 补子）→ 整轮吸收（若飞碟吸到格子，吸收单独算下一轮）。
-- 没有匹配时最大波次 = startW。返回 (终盘, 计数)；回放、钩子、生成器都在效果里。
-- 第 3 项前的循环带着 g、hooks、wavesRev 三个手工传递的参数（g → g1 → g2、hooks → hooks'），现在只剩计数相关的累积器。
cascadeMatchesFromM :: MonadCascade m => Registry -> Int -> Maybe Pos -> Board -> m (Board, CascadeTally)
cascadeMatchesFromM reg startW prefer0 b0 =
  go prefer0 b0 0 0 startW noCounts []
  where
    -- clearedRev：反向累积（按块），收尾时再反转
    go pref b cells score maxW hits clearedRev
      | not (hasAnyMatchWith reg b) =
          pure (b, CascadeTally cells score maxW hits (nub (concat (reverse clearedRev))))
      | otherwise = do
          let wave = maxW + 1
              bS = fullStage b
              cr@(_, n, pos) = clearStage (clearMatchesDetailedWith reg pref) bS
          r1 <- roundM reg bS cr wave
          let b1 = rdAfter r1
              posD = nub (pos ++ rdSites r1)
              score1 = score + cwScore (rdWave r1)
              hits1 = hits <> rdHits r1 <> colorsOn reg b posD
          absorbed <- absorbRoundM reg b1 (wave + 1)
          case absorbed of
            Nothing ->
              go Nothing b1 (cells + n) score1 wave hits1 (posD : clearedRev)
            Just (r2, nAbs) ->
              -- 飞碟吸收单独算一轮（波次 wave + 1）
              let pos2 = cwCleared (rdWave r2)
              in go Nothing (rdAfter r2) (cells + n + rdCells r2) (score1 + cwScore (rdWave r2)) (wave + 1)
                   (hits1 <> rdHits r2 <> colorsOn reg b1 pos2 <> singleCount CountUfo nAbs)
                   (rdSites r2 : pos2 : posD : clearedRev)

--------------------------------------------------------------------------------
-- 核心：种子起手

-- | cascadeSeeds（指定注册表）：纯解释器运行 'cascadeSeedsM'。
cascadeSeedsWith :: RandomGen g => Registry -> Maybe Pos -> [Pos] -> LevelHooks -> g -> Board -> CascadeRun g
cascadeSeedsWith reg prefer seeds hooks g b = runCascade hooks g (cascadeSeedsM reg prefer seeds b)

-- | 种子起手（程序）：种子清除一轮（波次 1）→ 整轮吸收（波次 2）→ 普通匹配续连锁。
cascadeSeedsM :: MonadCascade m => Registry -> Maybe Pos -> [Pos] -> Board -> m (Board, CascadeTally)
cascadeSeedsM reg prefer seeds b
  | null seeds = cascadeMatchesM reg prefer b
  | otherwise = do
      let bS = fullStage b
          cr@(_, n, pos) = clearStage (\x -> clearFromSeedsDetailedWith reg prefer x seeds) bS
      r0 <- roundM reg bS cr 1
      let b1 = rdAfter r0
      absorbed <- absorbRoundM reg b1 2
      let (b1', nU, scoreU, hitsU, posU) = case absorbed of
            Nothing -> (b1, 0, 0, noCounts, [])
            Just (rU, nAbs) ->
              let pos2 = cwCleared (rdWave rU)
              in ( rdAfter rU, rdCells rU, cwScore (rdWave rU)
                 , rdHits rU <> colorsOn reg b1 pos2 <> singleCount CountUfo nAbs, nub (pos2 ++ rdSites rU) )
          wavesDone = (if n > 0 then 1 else 0) + (if nU > 0 then 1 else 0)
      -- 续连锁：波次倍数接在起手轮之后
      (bRest, t2r) <- cascadeMatchesFromM reg wavesDone Nothing b1'
      let maxW = if ctCells t2r > 0 then ctMaxWave t2r else wavesDone
          tally =
            CascadeTally
              (n + nU + ctCells t2r)
              (cwScore (rdWave r0) + scoreU + ctScore t2r)
              maxW
              (rdHits r0 <> colorsOn reg b pos <> hitsU <> ctCounts t2r)
              (nub (pos ++ rdSites r0 ++ posU ++ ctCleared t2r))
      pure (bRest, tally)

--------------------------------------------------------------------------------
-- 核心：皮带移位之后 / 步末之后的补结算

-- | 非匹配起手的连锁入口（第 3 刀前是 cascadeAfterBeltWith / cascadeAfterEndWith 两份）。
data AfterEntry
  = AfterBelt         -- ^ 皮带移位之后：盘面已成消则直接连锁，否则先沉降补子再看
  | AfterEnd [Pos]    -- ^ 步末之后（段 2c）：先把步末规则声明的空洞挖空，再沉降补子
  deriving (Eq, Show)

-- | 全部步末规则在终盘上声明的空洞（erHoles，去重，按规则顺序）。内置规则恒为 []。
endHolesWith :: Registry -> Board -> [Pos]
endHolesWith reg b = nub (concat [erHoles r b | ph <- [PhaseTick, PhaseSpread, PhaseMove], r <- endRules reg ph])

-- | 皮带后 / 步末后的补结算：挖空（步末的空洞；皮带没有）→ 沉降（重力 / 边缘收集 / 传送门）+ 补子；
-- 盘面有变化或收走了格时记一个只有沉降的轮次；之后成消则接普通连锁（波次从 1 起）。
-- 步末入口没有空洞、边上也没有待收格时沉降是恒等、refill 不消耗随机数，结果等于旧的「成消才连锁」：
-- 内置元素的步末从不留下空洞（38 关 × 多种子扫描确认，见 docs/testing.md），金标准因此不变。
cascadeAfterWith :: RandomGen g => Registry -> AfterEntry -> LevelHooks -> g -> Board -> CascadeRun g
cascadeAfterWith reg entry hooks g b = runCascade hooks g (cascadeAfterM reg entry b)

-- | 皮带后 / 步末后的补结算（程序）。
cascadeAfterM :: MonadCascade m => Registry -> AfterEntry -> Board -> m (Board, CascadeTally)
cascadeAfterM reg entry b = case entry of
  AfterBelt
    | hasAnyMatchWith reg b -> cascadeMatchesM reg Nothing b
    | otherwise -> settleThenCascade []
  AfterEnd holes -> settleThenCascade holes
  where
    settleThenCascade holes = do
      let bS = fullStage b
      rd <- settleRoundM reg bS (digHoles holes bS, 0, []) 0
      let b1 = rdAfter rd
          sites = rdSites rd
      -- 只有沉降的一轮：盘面变了或收走了格才记
      when (b1 /= b || not (null sites)) (emitWave (rdWave rd))
      if hasAnyMatchWith reg b1
        then do
          (b2, t) <- cascadeMatchesM reg Nothing b1
          pure (b2, (addHits (rdHits rd) t) {ctCleared = nub (sites ++ ctCleared t)})
        else pure (b1, (addHits (rdHits rd) zeroTally) {ctCleared = sites})

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
  let r = runPureCascade hooks0 g (cascadeCountdownsM reg b)
      (steps, run) = ranValue r
  in (steps, ranToRun r {ranValue = run})

-- | 倒计时（程序）：步末规则是纯的盘面变换，只有引爆种子之后的连锁用到效果；没有种子时什么效果也不发生
-- （= 第 3 项前的 stillRun：盘面、钩子、生成器原样）。
cascadeCountdownsM :: MonadCascade m => Registry -> Board -> m ([(Board, Board, EndEffect)], (Board, CascadeTally))
cascadeCountdownsM reg b = do
  let rules = endRules reg PhaseTick
      ctx = EndCtx [] [] (pushableWith reg)
      step (accRev, before) r =
        let (eff, after) = erRun r ctx before
        in ([(before, after, e) | Just e <- [eff]] ++ accRev, after)
      (stepsRev, bTick) = foldl step ([], b) rules
      steps = reverse stepsRev
      seeds = nub (concatMap (\r -> erSeeds r bTick) rules)
  run <-
    if null seeds
      then pure (bTick, zeroTally)
      else cascadeSeedsM reg Nothing seeds bTick
  pure (steps, run)

--------------------------------------------------------------------------------
-- 单轮

-- | 恰好一轮匹配消除 + 沉降补子（指定注册表；没有关卡级钩子：不跑飞碟、无传送门）；无匹配时返回 Nothing。
-- 与 cascadeMatchesFromWith 的单轮是同一组调用：clear → settleRound（沉降 + 补子）；
-- prefer 为第一轮新特殊块的优先生成位。
stepCascadeAtWith :: RandomGen g => Registry -> Maybe Pos -> g -> Board -> Maybe (Board, Int, g)
stepCascadeAtWith reg prefer g b =
  let r = runPureCascade noHooks g (stepCascadeAtM reg prefer b)
  in fmap (\(b', n) -> (b', n, ranGen r)) (ranValue r)

-- | 单轮（程序）：发出这一轮的回放（纯入口 'stepCascadeAtWith' 不返回回放，丢弃）。
stepCascadeAtM :: MonadCascade m => Registry -> Maybe Pos -> Board -> m (Maybe (Board, Int))
stepCascadeAtM reg prefer b
  | not (hasAnyMatchWith reg b) = pure Nothing
  | otherwise = do
      let bS = fullStage b
          cr@(_, n, _) = clearStage (clearMatchesDetailedWith reg prefer) bS
      rd <- roundM reg bS cr 1
      pure (Just (rdAfter rd, n))
