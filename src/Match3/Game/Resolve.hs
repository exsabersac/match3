-- | 一次操作（玩家交换 / 锤子 / 自由交换 / 十字清除）的**公共结算**：主连锁 → 步末效果 →
-- 计数与目标 → 结局判定 → 自动洗牌，并由同一次计算产出回放脚本 MoveTrace。
--
-- 第二刀之前，trySwap 与三种道具各有一份几乎相同的结算代码（计数、目标、结局、洗牌四处重复），
-- 回放 trace* 又各自重算一遍；现在四处入口只负责「校验 + 选择起手方式」，其余全部在这里。
--
-- 依赖：Match3.Board.*（记录版连锁 CascadeRun）、State、Tally、Outcome、Shuffle、Trace、元素注册表
-- （步末阶段 PhaseTick / PhaseSpread / PhaseMove 的规则、按差计数、地毯腾空都查注册表；皮带是关卡特性）。
-- 不变量（逐字保持旧行为，金标准锁定）：
--   * 玩家交换的步末顺序：倒计时 tick / 爆炸 → 皮带移位 + 皮带后连锁 → 藤 / 巧 / 蒸汽蔓延 → 蜗牛 →
--     （蜗牛推出匹配）再连锁一次；道具只有蔓延，没有倒计时 / 皮带 / 蜗牛；
--   * 连击数：第一段的最大波次，之后每段有清除时叠加该段的最大波次；
--   * 交换耗 1 步，道具不耗步但扣对应次数；时间精灵每只 +2 步；
--   * 只有 MoveApplied（未终局）才调用 ensurePlayable；洗牌前的盘面 / 生成器记在 mtFinal / mtGen。
module Match3.Game.Resolve
  ( MoveKind(..)
  , Opening(..)
  , resolveMove
  , resolveMoveWith
  , combineCombo
  ) where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Match3.Board.Cascade
  ( CascadeRun(..)
  , CascadeTally(..)
  , CascadeWave(..)
  , AfterEntry(..)
  , cascadeAfterWith
  , endHolesWith
  , cascadeCountdownsTracedWith
  , cascadeMatchesWith
  , cascadeSeedsWith
  , stillRun
  )
import Match3.Conveyor (applyBeltMoves)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry, beltShiftWith, coverWith, endRules, hitGroundWith, pushableWith)
import Match3.Counts (CounterKey(..), countsFromList, singleCount)
import Match3.Element.Types (EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import Match3.Game.Outcome
import Match3.Game.Shuffle
import Match3.Game.State
import Match3.Game.Tally
import Match3.Game.Trace
import System.Random (StdGen)

-- | 操作种类：决定步末效果、步数 / 道具次数的扣法。
data MoveKind = KindSwap | KindHammer | KindFreeSwap | KindCross
  deriving (Eq, Show)

-- | 主连锁的起手方式。
data Opening
  = OpenMatch (Maybe Pos)        -- ^ 普通匹配连锁（prefer = 新特殊块的优先生成位）
  | OpenSeeds (Maybe Pos) [Pos]  -- ^ 种子起手（彩虹 / 特殊合成 / 锤子 / 十字）
  deriving (Eq, Show)

-- | 多段连锁的连击数：第一段的最大波次，之后每段有清除时把该段的最大波次叠加上去。
combineCombo :: [CascadeTally] -> Int
combineCombo [] = 0
combineCombo (t0 : ts) = foldl step (ctMaxWave t0) ts
  where
    step c t = max c (if ctCells t > 0 then c + ctMaxWave t else c)

-- | 公共结算。start = 第一轮之前的盘面（交换后 / 道具原盘）；调用方已完成全部校验
-- （越界、无次数、挡交换、无匹配等拒绝路径不进这里）。返回 (新状态, 结局, 回放脚本)。
resolveMove :: MoveKind -> Board -> Opening -> GameState -> (GameState, Outcome, MoveTrace)
resolveMove = resolveMoveWith defaultRegistry

-- | 公共结算（指定注册表）：主连锁、步末规则、计数、洗牌都用这张表里的元素定义。
resolveMoveWith :: Registry -> MoveKind -> Board -> Opening -> GameState -> (GameState, Outcome, MoveTrace)
resolveMoveWith reg kind start opening gs =
  let portals = gsPortals gs
      seg0 = case opening of
        OpenMatch prefer -> cascadeMatchesWith reg prefer (gsUfos gs) portals (gsGen gs) start
        OpenSeeds prefer seeds -> cascadeSeedsWith reg prefer seeds (gsUfos gs) portals (gsGen gs) start
      (segs, ends, board1, vacateAfter) =
        if kind == KindSwap then swapEnd reg gs seg0 else boosterEnd reg portals seg0
      finalSeg = NE.last segs
      tallies1 = NE.map crTally segs
      tallies = NE.toList tallies1
      ufosF = crUfos finalSeg
      gFinal = crGen finalSeg
      -- 计数
      total f = sum (map f tallies)
      gained = total ctScore
      clearedAll = concatMap ctCleared tallies
      combo = combineCombo tallies
      -- 按前后盘面差计数（保险箱开启、时间精灵 +2 步、自定义）
      diffs = diffCountsWith reg (gsBoard gs) board1
      bonusMoves = sum (map dcBonus diffs)
      -- 地面层（段 2c）：逐轮被上方消除命中（每轮每格一次）；段 5 起第 39 关（双层果冻）用到，其余内置关卡地面层为空
      (ground', groundCounts) =
        let (gr', countsRev) =
              foldl
                (\(gr, accRev) w -> let (gr1, cs) = hitGroundWith reg (nub (cwCleared w ++ cwDrained w)) gr in (gr1, cs : accRev))
                (gsGround gs, [])
                (concatMap crWaves (NE.toList segs))
        in (gr', concat (reverse countsRev))
      (carpetOpen', carpetHit) =
        coverWith reg (gsCarpetOpen gs) (clearedAll ++ carpetVacateSeedsWith reg (gsBoard gs) vacateAfter)
      -- 计数（第 4 刀：统一进 gsCounts；第 5 刀：颜色袋也在 ctCounts 里、目标进度由目标数据派生）：各段清除格 / 飞碟吸收 + 前后差 + 地面层去层 + 地毯覆盖
      counts' =
        gsCounts gs
          <> mconcat (map ctCounts tallies)
          <> countsFromList [(dcCounter d, dcCount d) | d <- diffs]
          <> countsFromList [(CountNamed n, k) | (n, k) <- groundCounts]
          <> singleCount CountCarpets carpetHit
      -- 步数与道具次数
      spend g = case kind of
        KindSwap -> g {gsMoves = gsMoves gs - 1 + bonusMoves}
        KindHammer -> g {gsMoves = gsMoves gs + bonusMoves, gsHammers = gsHammers gs - 1}
        KindFreeSwap -> g {gsMoves = gsMoves gs + bonusMoves, gsFreeSwaps = gsFreeSwaps gs - 1}
        KindCross -> g {gsMoves = gsMoves gs + bonusMoves, gsCrossClears = gsCrossClears gs - 1}
      gs' =
        spend
          gs
            { gsBoard = board1
            , gsScore = gsScore gs + gained
            , gsCounts = counts'
            , gsGen = gFinal
            , gsHint = Nothing
            , gsCombo = combo
            , gsShuffled = False
            , gsUfos = ufosF
            , gsCarpetOpen = carpetOpen'
            , gsLastCleared = nub clearedAll
            , gsGround = ground'
            }
      outcome = decideOutcome gs' gained
      gs'' = case outcome of
        Won s -> gs' {gsOver = Just (Won s)}
        Lost s -> gs' {gsOver = Just (Lost s)}
        LevelClear s n -> gs' {gsOver = Just (LevelClear s n)}
        _ -> gs'
      -- 未终局时，没有可走步则自动洗牌
      gs''' = case outcome of
        MoveApplied _ -> ensurePlayableWith reg gs''
        _ -> gs''
      trace =
        MoveTrace
          { mtStart = start
          , mtWaves = concatMap crWaves (NE.toList segs)
          , mtFinal = board1
          , mtEnd = ends
          , mtGen = gFinal
          , mtShuffle = if gsShuffled gs''' then Just (gsBoard gs''') else Nothing
          }
  in (gs''', outcome, trace)

-- | 依次跑某阶段的步末规则：返回 (步末记录, 终盘)。空效果不记录。
runPhase :: Registry -> EndPhase -> EndCtx -> Int -> Board -> ([EndStep], Board)
runPhase reg ph ctx k b0 =
  let (stepsRev, b1) = foldl one ([], b0) (endRules reg ph)
  in (reverse stepsRev, b1)
  where
    -- 反向累积，收尾再反转
    one (accRev, before) rule =
      let (eff, after) = erRun rule ctx before
      in ([EndStep k before after e | Just e <- [eff]] ++ accRev, after)

-- | 玩家交换的步末：倒计时（PhaseTick）→ 皮带 → 蔓延（PhaseSpread）→ 会走的元素（PhaseMove）→（成消）再连锁。
-- 返回 (四段连锁, 步末记录, 终盘, 地毯腾空比较用的盘面)。
swapEnd :: Registry -> GameState -> CascadeRun StdGen -> (NonEmpty (CascadeRun StdGen), [EndStep], Board, Board)
swapEnd reg gs seg0 =
  let portals = gsPortals gs
      -- 皮带是关卡级元素：在「倒计时之后」这一节拍发 EndTicked 消息取移位；没人回复时当作没有皮带
      (belts, mvBelt) = case beltShiftWith reg (gsBelts gs) of
        Just mv -> (gsBelts gs, mv)
        Nothing -> ([], [])
      ws0 = crWaves seg0
      board0' = crBoard seg0
      -- 倒计时 tick / 归零爆炸（带飞碟与传送门）
      -- 倒计时规则只跑一遍：同时得到连锁与步末记录
      (tickSteps, seg1) = cascadeCountdownsTracedWith reg (crUfos seg0) portals (crGen seg0) board0'
      endTick = [EndStep (length ws0) before after e | (before, after, e) <- tickSteps]
      -- 皮带：移位后连锁 / 沉降（收皮带送到底行的饼干）
      boardCd = crBoard seg1
      boardBelt = applyBeltMoves boardCd mvBelt
      nBelt = length ws0 + length (crWaves seg1)
      endBelt =
        [ EndStep nBelt boardCd boardBelt (EndBeltShift mvBelt)
        | not (null belts)
        , not (null mvBelt)
        ]
      seg2 =
        if null belts
          then stillRun boardCd (crUfos seg1) (crGen seg1)
          else cascadeAfterWith reg AfterBelt (crUfos seg1) portals (crGen seg1) boardBelt
      -- 蔓延，然后会走的元素（跳过皮带格；传送门端点当墙）
      boardBeltCas = crBoard seg2
      nEnd = nBelt + length (crWaves seg2)
      beltCells = nub (concat belts)
      portalEnds = nub (concatMap (\(a, b) -> [a, b]) portals)
      (endSpread, boardSpread) = traceSpreadsWith reg nEnd boardBeltCas
      (endMove, boardSnail) = runPhase reg PhaseMove (EndCtx beltCells portalEnds (pushableWith reg)) nEnd boardSpread
      -- 步末补结算（段 2c 统一路径）：步末规则声明的空洞挖空 → 沉降 + 补子 → 成消（含蜗牛推出的匹配）再连锁；
      -- 不再重复步末效果。内置元素没有空洞时等于旧的「成消才连锁」。
      seg3 = cascadeAfterWith reg (AfterEnd (endHolesWith reg boardSnail)) (crUfos seg2) portals (crGen seg2) boardSnail
      board1 = crBoard seg3
  in (seg0 :| [seg1, seg2, seg3], endTick ++ endBelt ++ endSpread ++ endMove, board1, board1)

-- | 道具的步末：只有蔓延。地毯腾空比较用蔓延前的盘面（与旧实现一致）。
boosterEnd :: Registry -> [(Pos, Pos)] -> CascadeRun StdGen -> (NonEmpty (CascadeRun StdGen), [EndStep], Board, Board)
boosterEnd reg portals seg0 =
  let boardH = crBoard seg0
      (ends, boardSp) = traceSpreadsWith reg (length (crWaves seg0)) boardH
      -- 步末补结算（同交换的统一路径；蔓延不会造出匹配，内置元素没有空洞时恒等）
      seg1 = cascadeAfterWith reg (AfterEnd (endHolesWith reg boardSp)) (crUfos seg0) portals (crGen seg0) boardSp
  in (seg0 :| [seg1], ends, crBoard seg1, boardH)
