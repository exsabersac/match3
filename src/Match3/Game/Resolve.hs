{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | 一次操作（玩家交换 / 锤子 / 自由交换 / 十字清除）的**公共结算**：主连锁 → 步末效果 →
-- 计数与目标 → 结局判定 → 自动洗牌，并由同一次计算产出回放脚本 MoveTrace。
--
-- trySwap 与三种道具的入口只负责「校验 + 选择起手方式」，结算与回放脚本全部在这里，只写一次。
--
-- 依赖：Match3.Board.*（记录版连锁 CascadeRun）、State、Tally、Outcome、Shuffle、Trace、EndPhase（步末表）、
-- 元素元素世界（按差计数、地毯腾空都查元素世界）、Element.Level（
-- 关卡级元素在 gsLevelElems，连锁经钩子 LevelHooks，皮带 / 地毯 / 地面层 / 会走元素的避让格与墙经 Mechanic 的节拍方法）。
-- 不变量（金标准锁定）：
--   * 玩家交换的步末顺序：倒计时 tick / 爆炸 → 皮带移位 + 皮带后连锁 → 藤 / 巧 / 蒸汽蔓延 → 蜗牛 →
--     （蜗牛推出匹配）再连锁一次；道具只有蔓延，没有倒计时 / 皮带 / 蜗牛（写成 EndPhase 表，见 endTableFor）；
--   * 连击数：第一段的最大波次，之后每段有清除时叠加该段的最大波次；
--   * 交换耗 1 步，道具不耗步但扣对应次数；时间精灵每只 +2 步；
--   * 只有 MoveApplied（未终局）才调用 ensurePlayable；洗牌前的盘面 / 生成器记在 mtFinal / mtGen；
--   * 终局结果（Won / Lost / LevelClear）经 terminalOf 写进 gsOver，其余结果不改 gsOver。
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

import Data.List (mapAccumL, nub)
import Engine.Optics (Traversal', ignored, (%~), (&))
import qualified Data.List.NonEmpty as NE
import Match3.Board.Cascade
  ( CascadeRun(..)
  , CascadeTally(..)
  , CascadeWave(..)
  , cascadeMatchesWith
  , cascadeSeedsWith
  )
import Match3.Element.Builtin (defaultWorld)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Board.Phase (IsFull, Phase(..), Stage, stageBoard)
import Match3.Element.Level (coverIn, hitGroundIn, judgeIn, levelHooksWith, levelWorldIn)
import Match3.Element.World (World)
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

-- | 一次操作消耗的步数：交换 1 步，道具不耗步（Haskell 特性第 6 项）。
kindCost :: MoveKind -> Int
kindCost kind = if kind == KindSwap then 1 else 0

-- | 一次操作消耗的道具次数所在的字段：交换没有（ignored，没有焦点），三种道具各自的次数（Haskell 特性第 6 项）。
kindCharges :: MoveKind -> Traversal' GameState Int
kindCharges kind = case kind of
  KindSwap -> ignored
  KindHammer -> gsHammersL
  KindFreeSwap -> gsFreeSwapsL
  KindCross -> gsCrossClearsL

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
  -- | 先变身再种子起手（新玩法 4：关卡级机制的 morph）：变身记成第 0 轮之前的步末效果（esAfterWaves = 0），
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
resolveMove = resolveMoveWith defaultWorld

-- | 公共结算（指定元素世界）：主连锁、步末规则、计数、洗牌都用这张表里的元素定义。
-- 三个参数的类型都由同一个 k 决定：操作种类、起手盘面的阶段、起手方式必须彼此吻合。
-- 约束 IsFull (StartPhase k) 在调用处 k 已知时自动成立（起手阶段只有 'Swapped / 'Full，都是满盘）。
resolveMoveWith
  :: IsFull (StartPhase k)
  => World -> SMoveKind k -> Stage (StartPhase k) -> Opening (StartPhase k) -> GameState -> (GameState, Outcome, MoveTrace)
resolveMoveWith world0 sk startS opening gs =
  let kind = moveKind sk
      start = stageBoard startS
      -- 本关的元素世界：关卡级元素可以改形状表（规则开关 "bomb_shapes"：L / T 形生成炸弹）；没人回复 = world0
      world = levelWorldIn world0 (gsLevelElems gs)
      hooks0 = levelHooksWith world (gsLevelElems gs)
      -- 变身起手（OpenMorph）：第一轮之前先把变身写进盘面，并记一条 esAfterWaves = 0 的步末效果
      (startW, preEnds) = case opening of
        OpenMorph _ eff _ -> let b = applyEndEffect eff start in (b, [EndStep 0 start b eff])
        _ -> (start, [])
      seg0 = case opening of
        OpenMatch prefer -> cascadeMatchesWith world prefer hooks0 (gsGen gs) startW
        OpenSeeds prefer seeds -> cascadeSeedsWith world prefer seeds hooks0 (gsGen gs) startW
        OpenMorph prefer _ seeds -> cascadeSeedsWith world prefer seeds hooks0 (gsGen gs) startW
      -- 步末：按表的顺序执行
      (segs, ends0, board1, vacateAfter) = runEndTable world (endTableFor kind) seg0
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
      diffs = diffCountsWith world (gsBoard gs) board1
      bonusMoves = sum (map dcBonus diffs)
      -- 地面层节拍（地面层是核心机制，onGroundHit）：逐轮被上方消除命中（每轮每格一次）；
      -- 内置关卡里只有第 39 关（双层果冻）有地面层
      -- （mapAccumL：累积量 = 关卡级元素、每轮输出 = 去层计数）
      (elemsG, groundCounts) =
        let (es', perWave) =
              mapAccumL
                (\es w -> let (cs, es1) = hitGroundIn world (nub (cwCleared w ++ cwDrained w)) es in (es1, cs))
                elemsC
                (concatMap crWaves (NE.toList segs))
        in (es', concat perWave)
      -- 地毯节拍（onCover）
      (carpetHit, elems') =
        coverIn world (clearedAll ++ carpetVacateSeedsWith world (gsBoard gs) vacateAfter) elemsG
      -- 计数（全部进 gsCounts，颜色袋也在 ctCounts 里、目标进度由目标数据派生）：各段清除格 / 飞碟吸收 + 前后差 + 地面层去层 + 地毯覆盖
      counts' =
        gsCounts gs
          <> mconcat (map ctCounts tallies)
          <> countsFromList [(dcCounter d, dcCount d) | d <- diffs]
          <> countsFromList [(CountNamed n, k) | (n, k) <- groundCounts]
          <> singleCount CountCarpets carpetHit
      -- 步数与道具次数
      -- （每种操作「花什么」由 kindCost / kindCharges 给出，这里只写一次）
      spend g = g & gsMovesL %~ (\m -> m - kindCost kind + bonusMoves) & kindCharges kind %~ subtract 1
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
      -- 胜负：内置规则先判，再交关卡级元素复核（胜负节拍 judge；没人回复时原样）
      outcome = judgeIn world elems' board1 (gsScore gs') (gsMoves gs') (decideOutcome gs' gained)
      gs'' = case terminalOf outcome of
        Just t -> gs' {gsOver = Just t}
        Nothing -> gs'
      -- 未终局时，没有可走步则自动洗牌
      gs''' = case outcome of
        MoveApplied _ -> ensurePlayableWith world gs''
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
