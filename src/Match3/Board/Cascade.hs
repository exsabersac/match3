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
    -- * 回放数据
  , CascadeWave(..)
    -- * 单轮
  , stepCascadeAtWith
  ) where

import Data.List (nub)
import Match3.Board.Hooks (LevelHooks(..), noHooks)
import Match3.Board.Refill (refillWith)
import Match3.Element.Registry (Registry, counterWith, endRules, pushableWith)
import Match3.Element.Event (EndEffect)
import Match3.Counts (CounterKey(..), Counts, bumpCount, countsFromList, noCounts, singleCount)
import Match3.Element.Types (EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import System.Random (RandomGen)
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid
import Match3.Board.Match

--------------------------------------------------------------------------------
-- 数据

-- | 连锁中的一轮（一次「消除 → 下落 → 补子」）。
data CascadeWave = CascadeWave
  { cwBefore  :: Board           -- ^ 本轮消除前的盘面
  , cwCleared :: [Pos]           -- ^ 本轮被消掉的格（真消除 + 被打碎的障碍）；可能为空（仅沉降）
  , cwDrained :: [Pos]           -- ^ 沉降途中底行被收走的饼干位
  , cwHoles   :: MBoard          -- ^ 消除并放下新特殊块之后、下落之前（Nothing = 空洞）
  , cwAfter   :: Board           -- ^ 重力 / 传送门 / 补子之后
  , cwScore   :: Score           -- ^ 本轮得分（与结算的波次计分相同）
  } deriving (Eq)

-- | 与派生的 Show 逐字相同，只是 cwHoles 仍按行列表打印（第 3 刀前 MBoard 是 [[Maybe Cell]]；
-- 元素查询快照对 show 的结果取散列，打印形式因此保持不变）。
instance Show CascadeWave where
  showsPrec d w =
    showParen (d >= 11) $
      showString "CascadeWave {cwBefore = " . showsPrec 0 (cwBefore w)
        . showString ", cwCleared = " . showsPrec 0 (cwCleared w)
        . showString ", cwDrained = " . showsPrec 0 (cwDrained w)
        . showString ", cwHoles = " . showsPrec 0 (mboardRows (cwHoles w))
        . showString ", cwAfter = " . showsPrec 0 (cwAfter w)
        . showString ", cwScore = " . showsPrec 0 (cwScore w)
        . showChar '}'


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
withDrained reg drained h = foldl (\hh (_, cell) -> bumpHit (counterWith reg cell) hh) h drained

-- | 把一组计数加进已有计数（皮带后沉降收走的格）。
addHits :: Counts -> CascadeTally -> CascadeTally
addHits h t = t {ctCounts = ctCounts t <> h}

-- | 清除格在消除前盘面上的计数（按本体定义的 counter）。
hitsOn :: Registry -> Board -> [Pos] -> Counts
hitsOn reg b = foldl (\h p -> bumpHit (counterWith reg (getCell b p)) h) noCounts

-- | 一个格子的计数键加 1。保险箱 / 时间精灵的键按前后盘面差计（Game.Tally，元素的 diffCounter），
-- 不在清除格里计（与第 4 刀前的 Hits 相同；内置元素没有把这两个键当 counter 的）。
bumpHit :: Maybe CounterKey -> Counts -> Counts
bumpHit Nothing h = h
bumpHit (Just CountSafes) h = h
bumpHit (Just CountSpirits) h = h
bumpHit (Just k) h = bumpCount k 1 h

-- | 一组格在盘面 b 上按颜色计数（CountColor；第 5 刀前是按 allColors 排的颜色袋列表）。
colorsOn :: Registry -> Board -> [Pos] -> Counts
colorsOn reg b pos = countsFromList [(CountColor col, countColorWith reg b pos col) | col <- allColors]

--------------------------------------------------------------------------------
-- 公共的一轮：沉降 + 补子、整轮吸收

-- | 一轮「挖空 → 沉降（重力 / 边缘收集 / 传送门）→ 补子」的结果。
data Round = Round
  { rdWave  :: CascadeWave  -- ^ 本轮回放（含本轮得分）
  , rdCells :: Int          -- ^ 清除格数
  , rdHits  :: Counts       -- ^ 清除格在消除前盘面上的计数 + 沉降时被边缘收走的格的计数
  , rdSites :: [Pos]        -- ^ 沉降时被边缘收走的格
  }

rdAfter :: Round -> Board
rdAfter = cwAfter . rdWave

-- | 所有连锁起手共用的一轮（第 3 刀前在匹配 / 飞碟 / 种子 / 种子后飞碟 / 皮带后 / 步末后 / 单轮里逐行重复 7 份）：
-- 给出消除前盘面 b、本轮清除结果 (挖空盘面, 清除数, 清除格) 与波次 w（得分 = scoreForWave w 清除数），
-- 沉降后按行优先逐个空洞补子（第 8 刀起按补子策略 activeRefill；缺省策略每洞恰好一次 randomColor，随机数顺序与原先相同）。
settleRound :: RandomGen g => Registry -> LevelHooks -> g -> Board -> (MBoard, Int, [Pos]) -> Int -> (Round, g)
settleRound reg hooks g b (mb, n, pos) w =
  let (settled, drained) = settleDrainWith reg hooks mb
      sites = map fst drained
      (b', g') = refillWith (activeRefill reg hooks) g settled
  in (Round (CascadeWave b pos sites mb b' (scoreForWave w n)) n (withDrained reg drained (hitsOn reg b pos)) sites, g')

-- | 补子后的整轮吸收（钩子 onAbsorb：关卡级元素回复 Refilled 消息，内置 = 飞碟）。吸到格子时吸收单独成一轮
-- （clearUfoAbsorbed → settleRound，波次 w）；返回 (推进后的钩子, Just (吸收轮, 其中被吸走的格数), 生成器)。
-- 第 3 刀前匹配连锁与种子起手各有一份。
absorbRound :: RandomGen g => Registry -> LevelHooks -> g -> Board -> Int -> (LevelHooks, Maybe (Round, Int), g)
absorbRound reg hooks g b w =
  let (absorbed, hooks') = onAbsorb hooks b
  in if null absorbed
       then (hooks', Nothing, g)
       else
         let cr@(_, _, pos) = clearUfoAbsorbedWith reg b absorbed
             (rd, g') = settleRound reg hooks' g b cr w
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
              cr@(_, n, pos) = clearMatchesDetailedWith reg pref b
              (r1, g1) = settleRound reg hooks g b cr wave
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
      let cr@(_, n, pos) = clearFromSeedsDetailedWith reg prefer b seeds
          (r0, g1) = settleRound reg hooks0 g b cr 1
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
cascadeAfterWith reg entry hooks g b = case entry of
  AfterBelt
    | hasAnyMatchWith reg b -> cascadeMatchesWith reg Nothing hooks g b
    | otherwise -> settleThenCascade []
  AfterEnd holes -> settleThenCascade holes
  where
    settleThenCascade holes =
      let mb = setManyM (toM b) [(p, Nothing) | p <- holes]
          (rd, g1) = settleRound reg hooks g b (mb, 0, []) 0
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
      let cr@(_, n, _) = clearMatchesDetailedWith reg prefer b
          (rd, g') = settleRound reg noHooks g b cr 1
      in Just (rdAfter rd, n, g')
