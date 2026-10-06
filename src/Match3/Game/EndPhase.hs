{-# LANGUAGE OverloadedStrings #-}
-- | 步末结算表（第 7 刀 7b）：一步操作的主连锁之后，步末各阶段**按表的顺序**执行。
--
-- 第 7 刀前，玩家交换与道具各有一段手写的步末流程（Resolve.swapEnd / boosterEnd）；现在两者都是一张
-- 'EndStage' 表，由 'runEndTable' 统一执行。每行 = 阶段名 + 它跑的元素步末规则阶段（'EndPhase'，没有则 Nothing）
-- + 执行函数（读写累积器 'EndAcc'）：
--
-- * @tick@（PhaseTick）：倒计时减一 / 归零引爆，接倒计时连锁（新的一段连锁）。
-- * @belt@：皮带节拍，问关卡级机制 onEndTick；有人回复时移位并接皮带后连锁，没人回复时是一段空连锁。
-- * @spread@（PhaseSpread）：藤 → 巧 → 蒸汽蔓延（不开新段）。
-- * @move@（PhaseMove）：会走的元素（蜗牛），避让格 / 墙问关卡级元素（不开新段）。
-- * @settle@：步末补结算（步末规则声明的空洞挖空 → 沉降补子 → 成消再连锁；新的一段连锁）。
-- * @vacate@：记下地毯「腾空」比较用的盘面（交换在最后 = 终盘；道具在蔓延之前，与第 7 刀前一致）。
--
-- 表（顺序与第 7 刀前逐字相同，测试 qc_end_table_matches_legacy 锁定）：
--   玩家交换 'swapEndTable'    = tick → belt → spread → move → settle → vacate
--   道具     'boosterEndTable' = vacate → spread → settle
--
-- 随机数：只有开新段的阶段（tick / belt / settle）经连锁消耗生成器，顺序即表的顺序，与旧实现相同。
-- 依赖：Board.Cascade（记录版连锁）、Board.Hooks、Element.Level（关卡级元素节拍）、元素世界、Game.Trace（EndStep）。
module Match3.Game.EndPhase
  ( EndStage(..)
  , EndAcc(..)
  , swapEndTable
  , boosterEndTable
  , tickStage
  , beltStage
  , spreadStage
  , moveStage
  , settleStage
  , vacateStage
  , runEndTable
  , runPhase
  ) where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Match3.Board.Cascade
  ( AfterEntry(..)
  , CascadeRun(..)
  , cascadeAfterWith
  , cascadeCountdownsTracedWith
  , endHolesWith
  , stillRun
  )
import Match3.Board.Grid (getCell)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Conveyor (applyBeltMoves)
import Match3.Element.Event (EndEffect(..), EndItem(..), EventKind(..))
import Match3.Element.Level (avoidCellsIn, beltShiftIn, levelHooksWith, wallCellsIn)
import Match3.ECS.Registry (Registry, pushableWith)
import Match3.Element.Types (EndCtx(..), EndPhase(..))
import Match3.Game.Trace (EndStep(..), runPhaseSteps, traceSpreadsWith)
import Match3.Types
import System.Random (StdGen)

-- | 步末结算的累积器：已有的各段连锁（最新的在前，非空）、按发生顺序的步末记录、当前盘面、地毯腾空比较用的盘面。
data EndAcc = EndAcc
  { eaSegsRev :: NonEmpty (CascadeRun StdGen)
  , eaEnds    :: [EndStep]
  , eaBoard   :: Board
  , eaVacate  :: Maybe Board
  }

-- | 步末表的一行。
data EndStage = EndStage
  { stageName  :: String
  , stagePhase :: Maybe EndPhase  -- ^ 这一行跑的元素步末规则阶段（皮带 / 补结算 / 腾空记录没有）
  , stageRun   :: Registry -> EndAcc -> EndAcc
  }

instance Show EndStage where
  show s = "EndStage " ++ show (stageName s) ++ " " ++ show (stagePhase s)

-- | 玩家交换的步末表。
swapEndTable :: [EndStage]
swapEndTable = [tickStage, beltStage, spreadStage, moveStage, settleStage, vacateStage]

-- | 道具（锤子 / 自由交换 / 十字）的步末表：只有蔓延。
boosterEndTable :: [EndStage]
boosterEndTable = [vacateStage, spreadStage, settleStage]

-- | 按表执行：返回 (各段连锁（主连锁在前）, 步末记录, 终盘, 地毯腾空比较用的盘面（表里没有 vacate 时 = 终盘）)。
runEndTable :: Registry -> [EndStage] -> CascadeRun StdGen -> (NonEmpty (CascadeRun StdGen), [EndStep], Board, Board)
runEndTable world table seg0 =
  let acc = foldl (\a st -> stageRun st world a) (EndAcc (seg0 :| []) [] (crBoard seg0) Nothing) table
  in (NE.reverse (eaSegsRev acc), eaEnds acc, eaBoard acc, maybe (eaBoard acc) id (eaVacate acc))

-- 累积器的小工具 ---------------------------------------------------------------

lastSeg :: EndAcc -> CascadeRun StdGen
lastSeg = NE.head . eaSegsRev

-- | 到目前为止播完的轮数（步末记录的插入点）。
wavesSoFar :: EndAcc -> Int
wavesSoFar = sum . map (length . crWaves) . NE.toList . eaSegsRev

-- | 接上一段新连锁（当前盘面 = 它的终盘）。
pushSeg :: [EndStep] -> CascadeRun StdGen -> EndAcc -> EndAcc
pushSeg steps seg a = a {eaSegsRev = NE.cons seg (eaSegsRev a), eaEnds = eaEnds a ++ steps, eaBoard = crBoard seg}

-- 各阶段 -----------------------------------------------------------------------

-- | 倒计时（PhaseTick 规则只跑一遍：同时得到连锁与步末记录）。
tickStage :: EndStage
tickStage = EndStage "tick" (Just PhaseTick) $ \world a ->
  let seg = lastSeg a
      (tickSteps, seg1) = cascadeCountdownsTracedWith world (crHooks seg) (crGen seg) (eaBoard a)
      k = wavesSoFar a
  in pushSeg [EndStep k before after e | (before, after, e) <- tickSteps] seg1 a

-- | 皮带节拍：关卡级机制（onEndTick）给出移位；没人回复时当作没有皮带（空连锁、不记录）。
beltStage :: EndStage
beltStage = EndStage "belt" Nothing $ \world a ->
  let seg = lastSeg a
      board = eaBoard a
      k = wavesSoFar a
  in case beltShiftIn world (hookLevel (crHooks seg)) of
       Nothing -> pushSeg [] (stillRun board (crHooks seg) (crGen seg)) a
       Just (mv, es) ->
         let board' = applyBeltMoves board mv
             eff = EndEffect EvBelt "belt" [EndItem o d (getCell board o) Nothing | (o, d) <- mv]
             steps = [EndStep k board board' eff | not (null mv)]
         in pushSeg steps (cascadeAfterWith world AfterBelt (levelHooksWith world es) (crGen seg) board') a

-- | 蔓延（PhaseSpread 规则按 erOrder）。
spreadStage :: EndStage
spreadStage = EndStage "spread" (Just PhaseSpread) $ \world a ->
  let (steps, b') = traceSpreadsWith world (wavesSoFar a) (eaBoard a)
  in a {eaEnds = eaEnds a ++ steps, eaBoard = b'}

-- | 会走的元素（PhaseMove）：跳过关卡级元素给的避让格（皮带格），把墙格（传送门端点）当墙。
moveStage :: EndStage
moveStage = EndStage "move" (Just PhaseMove) $ \world a ->
  let elems = hookLevel (crHooks (lastSeg a))
      ctx = EndCtx (nub (avoidCellsIn world elems)) (nub (wallCellsIn world elems)) (pushableWith world)
      (steps, b') = runPhase world PhaseMove ctx (wavesSoFar a) (eaBoard a)
  in a {eaEnds = eaEnds a ++ steps, eaBoard = b'}

-- | 步末补结算（段 2c 统一路径）：步末规则声明的空洞挖空 → 沉降 + 补子 → 成消（含蜗牛推出的匹配）再连锁。
settleStage :: EndStage
settleStage = EndStage "settle" Nothing $ \world a ->
  let seg = lastSeg a
      board = eaBoard a
  in pushSeg [] (cascadeAfterWith world (AfterEnd (endHolesWith world board)) (crHooks seg) (crGen seg) board) a

-- | 记下地毯腾空比较用的盘面。
vacateStage :: EndStage
vacateStage = EndStage "vacate" Nothing $ \_ a -> a {eaVacate = Just (eaBoard a)}

-- | 依次跑某阶段的步末规则：返回 (步末记录, 终盘)。空效果不记录。
-- （第 9 项起 = Game.Trace 的 runPhaseSteps，与蔓延共用同一个 runEndRules；第 9 项前这里另有一份 foldl + reverse。）
runPhase :: Registry -> EndPhase -> EndCtx -> Int -> Board -> ([EndStep], Board)
runPhase = runPhaseSteps
