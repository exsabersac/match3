{-# LANGUAGE NamedFieldPuns #-}

-- | 逐轮回放脚本的数据类型与步末效果：MoveTrace / EndStep，生成步末记录的 traceSpreads（beltMoves 再导出自 Conveyor），
-- 以及从回放脚本派生效果事件的 traceEvents。
--
-- 第二刀 2b：EndEffect / SpreadKind / SnailMove / applyEndEffect / spreadPairs 搬到 Match3.Element.Event，
-- traceSnails 搬到 Match3.Element.Builtin（蜗牛的步末规则），这里原样再导出。蔓延改为依次执行注册表里
-- PhaseSpread 阶段的步末规则。
--
-- 依赖：Match3.Board.*、元素框架（事件词汇 / 注册表）、Conveyor（皮带）。只描述「变了什么」，不结算。
-- 结算直接使用 traceSpreads / traceSnails 返回的盘面（Match3.Game.Resolve），不再另算一遍；
-- traceSnails 与 stepSnailsAvoidingBlocked 逐只调用同一个 stepSnailAtBlocked，结果恒等。
-- 护栏 trace_end_steps_replay_to_trySwap_final、trace_end_snail_push_and_turn、trace_end_spread_from_adjacent_source。
module Match3.Game.Trace
  ( MoveTrace(..)
  , EndStep(..)
  , EndEffect(..)
  , SpreadKind(..)
  , SnailMove(..)
  , applyEndEffect
  , spreadOverlay
  , spreadPairs
  , traceSpreads
  , traceSpreadsWith
  , traceSnails
  , beltMoves
  , emptyTrace
    -- * 效果事件
  , EventKind(..)
  , Event(..)
  , traceEvents
  , traceEventsWith
  ) where

import Match3.Board.Cascade (CascadeWave(..))
import Match3.Board.Grid (getCell)
import Match3.Conveyor (beltMoves)
import Data.List (nub)
import Match3.Element.Builtin (defaultRegistry, traceSnails)
import Match3.Element.Event
import Match3.Element.Registry (Registry, activatesWith, blastWith, elementName, endRules, topLayerName)
import Match3.Element.Types (EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import Match3.Game.State
import System.Random (StdGen)

-- | 一步操作的逐轮回放脚本（纯数据，供前端分轮播放连锁）。
-- 与结算结果由同一次计算产出（Match3.Game.Resolve.resolveMove），天然一致。
-- 被拒的操作（NoMatch / InvalidSwap / 已结束）返回空脚本，前端据此不会播放任何消除或连击。
-- 自动洗牌：mtFinal 是洗牌前的盘面；mtGen 是洗牌前的生成器，mtShuffle 是洗牌后的盘面，
-- 因此 ensurePlayable（mtFinal, mtGen）可逐帧复现洗牌（护栏 trace_shuffle_step_replays）。
data MoveTrace = MoveTrace
  { mtStart :: Board          -- ^ 第一轮之前的盘面（交换后 / 道具作用前）
  , mtWaves :: [CascadeWave]  -- ^ 按时间顺序的每一轮（含倒计时爆炸 / 皮带 / 蜗牛后的续连锁）
  , mtFinal :: Board          -- ^ 所有轮次与步末效果之后的盘面；未触发自动洗牌时 == 结算后的 gsBoard
  , mtEnd   :: [EndStep]      -- ^ 步末效果（非消除的盘面变化），按发生顺序；见 EndStep
  , mtGen   :: StdGen         -- ^ 规则结算结束、自动洗牌之前的生成器（被拒操作 = 原 gsGen）
  , mtShuffle :: Maybe Board  -- ^ 这一步触发了自动洗牌时，洗牌后的盘面（== 结算后的 gsBoard）；否则 Nothing
  } deriving (Show)

-- | 相等：逐字段比较；StdGen 没有 Eq，按 show 比较。
instance Eq MoveTrace where
  a == b =
    mtStart a == mtStart b
      && mtWaves a == mtWaves b
      && mtFinal a == mtFinal b
      && mtEnd a == mtEnd b
      && show (mtGen a) == show (mtGen b)
      && mtShuffle a == mtShuffle b

-- | 一个步末效果：在 mtWaves 的前 esAfterWaves 轮播完之后发生，把 esBefore 变成 esAfter。
-- 时间线 = 轮 0..k-1 → 所有 esAfterWaves == k 的步末效果（按列表顺序）→ 轮 k … → mtFinal。
-- 不变量（测试锁定）：applyEndEffect esEffect esBefore == esAfter，且首尾相接。
data EndStep = EndStep
  { esAfterWaves :: Int
  , esBefore     :: Board
  , esAfter      :: Board
  , esEffect     :: EndEffect
  } deriving (Eq, Show)

-- | 蔓延（藤 → 巧 → 蒸汽）的逐步快照（与 trySwap / 道具里的组合完全相同），空效果不记录。
traceSpreads :: Int -> Board -> ([EndStep], Board)
traceSpreads = traceSpreadsWith defaultRegistry

-- | traceSpreads（指定注册表）：依次执行 PhaseSpread 阶段的步末规则（按 erOrder），
-- 每条规则产出的效果记成一个 EndStep（esAfterWaves = k）。
traceSpreadsWith :: Registry -> Int -> Board -> ([EndStep], Board)
traceSpreadsWith reg k b0 = foldl one ([], b0) (endRules reg PhaseSpread)
  where
    one (acc, before) rule =
      let (eff, after) = erRun rule (EndCtx [] []) before
      in (acc ++ [EndStep k before after e | Just e <- [eff]], after)

-- | 被拒操作的空回放脚本：没有轮次、没有步末效果，前端什么都不播。
emptyTrace :: GameState -> MoveTrace
emptyTrace gs = MoveTrace (gsBoard gs) [] (gsBoard gs) [] (gsGen gs) Nothing

--------------------------------------------------------------------------------
-- 效果事件

-- | 回放脚本 → 效果事件（内置注册表）。
traceEvents :: MoveTrace -> [Event]
traceEvents = traceEventsWith defaultRegistry

-- | 回放脚本 → 按时间顺序的效果事件（纯数据，不参与结算）。时间线与 MoveTrace 相同：
-- 插入点 k 的步末事件（esAfterWaves == k）在第 k 轮之前；自动洗牌在最后。
-- 轮内事件顺序：特殊块爆炸 → 消除（按被消格本体的元素名分组）→ 波及（按最上层元素名分组）→
-- 底收 → 得分 → 连击（本步第 2 次起有消除的轮）。
traceEventsWith :: Registry -> MoveTrace -> [Event]
traceEventsWith reg t =
  concat [endsAt k ++ waveEvents k w | (k, w) <- zip [0 ..] waves]
    ++ endsAt (length waves)
    ++ [Event EvShuffle (length waves) "shuffle" [] 0 | Just _ <- [mtShuffle t]]
  where
    waves = mtWaves t
    endsAt k =
      [ Event (endEffectKind e) k (endEffectElement e) ps (length ps)
      | es <- mtEnd t
      , esAfterWaves es == k
      , let e = esEffect es
            ps = endEffectPairs e
      ]
    clearedWaves = [i | (i, w) <- zip [0 :: Int ..] waves, not (null (cwCleared w))]
    comboRank k = length (takeWhile (<= k) clearedWaves)
    waveEvents k w =
      let before = cwBefore w
          cleared = cwCleared w
          holes = cwHoles w
          hit =
            [ p
            | (r, row) <- zip [0 ..] holes
            , (c, mc) <- zip [0 ..] row
            , let p = (r, c)
            , p `notElem` cleared
            , mc /= Just (getCell before p)
            ]
          touched = cleared ++ hit
          blasts =
            [ Event EvBlast k (elementName reg cell) [(p, q) | q <- fp] (length fp)
            | p <- cleared
            , let cell = getCell before p
            , activatesWith reg cell
            , let fp = [q | q <- blastWith reg cell p, q `elem` touched]
            , not (null fp)
            ]
          grouped kind nameOf ps =
            [ Event kind k n [(p, p) | p <- mine] (length mine)
            | n <- nub (map nameOf ps)
            , let mine = [p | p <- ps, nameOf p == n]
            ]
          rank = comboRank k
      in blasts
           ++ grouped EvClear (elementName reg . getCell before) cleared
           ++ grouped EvHit (topLayerName reg . getCell before) hit
           ++ [Event EvDrain k "cookie" [(p, p) | p <- cwDrained w] (length (cwDrained w)) | not (null (cwDrained w))]
           ++ [Event EvScore k "" [] (cwScore w) | cwScore w > 0]
           ++ [Event EvCombo k "" [] rank | not (null cleared), rank >= 2]
