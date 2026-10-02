{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | 一次操作（玩家交换 / 锤子 / 自由交换 / 十字清除）的**公共结算**：主连锁 → 步末效果 →
-- 计数与目标 → 结局判定 → 自动洗牌，并由同一次计算产出回放脚本 MoveTrace。
--
-- 第二刀之前，trySwap 与三种道具各有一份几乎相同的结算代码（计数、目标、结局、洗牌四处重复），
-- 回放 trace* 又各自重算一遍；现在四处入口只负责「校验 + 选择起手方式」，其余全部在这里。
--
-- 依赖：Match3.Board.*（记录版连锁 CascadeRun）、State、Tally、Outcome、Shuffle、Trace、EndPhase（第 7 刀 7b：步末表）、
-- 元素注册表（按差计数、地毯腾空都查注册表）、Element.Level（第 7 刀：
-- 关卡级元素在 gsLevelElems，连锁经钩子 LevelHooks，皮带 / 地毯 / 地面层 / 会走元素的避让格与墙经节拍消息）。
-- 不变量（逐字保持旧行为，金标准锁定）：
--   * 玩家交换的步末顺序：倒计时 tick / 爆炸 → 皮带移位 + 皮带后连锁 → 藤 / 巧 / 蒸汽蔓延 → 蜗牛 →
--     （蜗牛推出匹配）再连锁一次；道具只有蔓延，没有倒计时 / 皮带 / 蜗牛（第 7 刀 7b 起写成 EndPhase 表，见 endTableFor）；
--   * 连击数：第一段的最大波次，之后每段有清除时叠加该段的最大波次；
--   * 交换耗 1 步，道具不耗步但扣对应次数；时间精灵每只 +2 步；
--   * 只有 MoveApplied（未终局）才调用 ensurePlayable；洗牌前的盘面 / 生成器记在 mtFinal / mtGen。
--
-- 类型层（Haskell 特性第 1 项，见 docs/haskell-features/01-类型层.md）：起手盘面带阶段标签（Match3.Board.Phase）。
-- 'MoveKind' 经 DataKinds 提升到类型层，'StartPhase' 算出每种操作该从哪个阶段起手（交换类 = @'Swapped@，
-- 锤子 / 十字 = @'Full@），'SMoveKind' 是把它带到值层的单例；'Opening' 是按起手阶段索引的 GADT
-- （普通匹配 / 变身只能接在交换之后）。于是「锤子拿交换后的盘起手」「道具起手却要求普通匹配」编译不过。
module Match3.Game.Resolve
  ( MoveKind(..)
  , SMoveKind(..)
  , StartPhase
  , moveKind
  , Opening(..)
  , resolveMove
  , resolveMoveWith
  , endTableFor
  , combineCombo
  ) where

import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Match3.Board.Cascade
  ( CascadeRun(..)
  , CascadeTally(..)
  , CascadeWave(..)
  , cascadeMatchesWith
  , cascadeSeedsWith
  )
import Match3.Element.Builtin (defaultRegistry)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Board.Phase (IsFull, Phase(..), Stage, stageBoard)
import Match3.Element.Level (coverIn, hitGroundIn, levelHooksWith, levelRegistryIn)
import Match3.Element.Registry (Registry)
import Match3.Counts (CounterKey(..), countsFromList, singleCount)
import Match3.Game.EndPhase (EndStage, boosterEndTable, runEndTable, swapEndTable)
import Match3.Types
import Match3.Game.Outcome
import Match3.Game.Shuffle
import Match3.Game.State
import Match3.Game.Tally
import Match3.Game.Trace

-- | 操作种类：决定步末效果、步数 / 道具次数的扣法。
-- 开了 DataKinds 后它也是一个种类：@'KindSwap@ 等可以当类型用（见 'StartPhase' / 'SMoveKind'）。
data MoveKind = KindSwap | KindHammer | KindFreeSwap | KindCross
  deriving (Eq, Show)

-- | 每种操作的起手阶段（类型族）：交换类从交换后的盘面起手，锤子 / 十字从静止的原盘起手。
type family StartPhase (k :: MoveKind) :: Phase where
  StartPhase 'KindSwap     = 'Swapped
  StartPhase 'KindFreeSwap = 'Swapped
  StartPhase 'KindHammer   = 'Full
  StartPhase 'KindCross    = 'Full

-- | 'MoveKind' 的单例（singleton）：每个类型 @k@ 恰好有一个值。传 @SKindHammer@ 就把
-- 「这是锤子」同时告诉了值层（'moveKind' 取回 'KindHammer'）和类型层（k ~ 'KindHammer）。
data SMoveKind (k :: MoveKind) where
  SKindSwap     :: SMoveKind 'KindSwap
  SKindHammer   :: SMoveKind 'KindHammer
  SKindFreeSwap :: SMoveKind 'KindFreeSwap
  SKindCross    :: SMoveKind 'KindCross

-- | 单例 → 普通值（步末表、扣步数仍按 'MoveKind' 分派，与改动前相同）。
moveKind :: SMoveKind k -> MoveKind
moveKind SKindSwap = KindSwap
moveKind SKindHammer = KindHammer
moveKind SKindFreeSwap = KindFreeSwap
moveKind SKindCross = KindCross

-- | 主连锁的起手方式（GADT，按起手盘面的阶段 p 索引）。
-- 静止盘面上没有现成的三连，所以普通匹配 / 变身只能接在交换之后（@Opening 'Swapped@）；
-- 种子起手对哪种起手盘都成立（交换出的彩虹 / 特殊合成，或锤子 / 十字的原盘）。
data Opening (p :: Phase) where
  -- | 普通匹配连锁（prefer = 新特殊块的优先生成位）
  OpenMatch :: Maybe Pos -> Opening 'Swapped
  -- | 种子起手（彩虹 / 特殊合成 / 锤子 / 十字）
  OpenSeeds :: Maybe Pos -> [Pos] -> Opening p
  -- | 先变身再种子起手（新玩法 4：关卡级元素回复 Morphing）：变身记成第 0 轮之前的步末效果（esAfterWaves = 0），
  -- mtStart 仍是交换后的盘面，第一轮从变身后的盘面开始
  OpenMorph :: Maybe Pos -> EndEffect -> [Pos] -> Opening 'Swapped

deriving instance Eq (Opening p)
deriving instance Show (Opening p)

-- | 多段连锁的连击数：第一段的最大波次，之后每段有清除时把该段的最大波次叠加上去。
combineCombo :: [CascadeTally] -> Int
combineCombo [] = 0
combineCombo (t0 : ts) = foldl step (ctMaxWave t0) ts
  where
    step c t = max c (if ctCells t > 0 then c + ctMaxWave t else c)

-- | 公共结算。start = 第一轮之前的盘面（交换后 / 道具原盘，阶段由操作种类决定：'StartPhase'）；调用方已完成全部校验
-- （越界、无次数、挡交换、无匹配等拒绝路径不进这里）。返回 (新状态, 结局, 回放脚本)。
resolveMove
  :: IsFull (StartPhase k)
  => SMoveKind k -> Stage (StartPhase k) -> Opening (StartPhase k) -> GameState -> (GameState, Outcome, MoveTrace)
resolveMove = resolveMoveWith defaultRegistry

-- | 公共结算（指定注册表）：主连锁、步末规则、计数、洗牌都用这张表里的元素定义。
-- 三个参数的类型都由同一个 k 决定：操作种类、起手盘面的阶段、起手方式必须彼此吻合。
-- 约束 IsFull (StartPhase k) 在调用处 k 已知时自动成立（起手阶段只有 'Swapped / 'Full，都是满盘）。
resolveMoveWith
  :: IsFull (StartPhase k)
  => Registry -> SMoveKind k -> Stage (StartPhase k) -> Opening (StartPhase k) -> GameState -> (GameState, Outcome, MoveTrace)
resolveMoveWith reg0 sk startS opening gs =
  let kind = moveKind sk
      start = stageBoard startS
      -- 本关的注册表：关卡级元素可以改形状表（规则开关 "bomb_shapes"：L / T 形生成炸弹）；没人回复 = reg0
      reg = levelRegistryIn reg0 (gsLevelElems gs)
      hooks0 = levelHooksWith reg (gsLevelElems gs)
      -- 变身起手（OpenMorph）：第一轮之前先把变身写进盘面，并记一条 esAfterWaves = 0 的步末效果
      (startW, preEnds) = case opening of
        OpenMorph _ eff _ -> let b = applyEndEffect eff start in (b, [EndStep 0 start b eff])
        _ -> (start, [])
      seg0 = case opening of
        OpenMatch prefer -> cascadeMatchesWith reg prefer hooks0 (gsGen gs) startW
        OpenSeeds prefer seeds -> cascadeSeedsWith reg prefer seeds hooks0 (gsGen gs) startW
        OpenMorph prefer _ seeds -> cascadeSeedsWith reg prefer seeds hooks0 (gsGen gs) startW
      -- 步末：按表的顺序执行（第 7 刀 7b）
      (segs, ends0, board1, vacateAfter) = runEndTable reg (endTableFor kind) seg0
      ends = preEnds ++ ends0
      finalSeg = NE.last segs
      tallies1 = NE.map crTally segs
      tallies = NE.toList tallies1
      -- 连锁推进后的关卡级元素（内置：飞碟移动）
      elemsC = hookLevel (crHooks finalSeg)
      gFinal = crGen finalSeg
      -- 计数
      total f = sum (map f tallies)
      gained = total ctScore
      clearedAll = concatMap ctCleared tallies
      combo = combineCombo tallies
      -- 按前后盘面差计数（保险箱开启、时间精灵 +2 步、自定义）
      diffs = diffCountsWith reg (gsBoard gs) board1
      bonusMoves = sum (map dcBonus diffs)
      -- 地面层节拍（段 2c；第 7 刀起地面层是关卡级元素，发 GroundHit）：逐轮被上方消除命中（每轮每格一次）；
      -- 段 5 起第 39 关（双层果冻）用到，其余内置关卡地面层为空
      (elemsG, groundCounts) =
        let (es', countsRev) =
              foldl
                (\(es, accRev) w -> let (cs, es1) = hitGroundIn reg (nub (cwCleared w ++ cwDrained w)) es in (es1, cs : accRev))
                (elemsC, [])
                (concatMap crWaves (NE.toList segs))
        in (es', concat (reverse countsRev))
      -- 地毯节拍（Covering）
      (carpetHit, elems') =
        coverIn reg (clearedAll ++ carpetVacateSeedsWith reg (gsBoard gs) vacateAfter) elemsG
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
            , gsLastCleared = nub clearedAll
            , gsLevelElems = elems'
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

-- | 各操作种类的步末表（Match3.Game.EndPhase）：交换 = tick → belt → spread → move → settle → vacate；道具 = vacate → spread → settle。
endTableFor :: MoveKind -> [EndStage]
endTableFor kind = if kind == KindSwap then swapEndTable else boosterEndTable
