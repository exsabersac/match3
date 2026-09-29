{-# LANGUAGE NamedFieldPuns #-}

-- | 逐轮回放脚本的数据类型与步末效果：MoveTrace / EndStep / EndEffect / SnailMove，
-- 把步末效果重放回盘面的 applyEndEffect，以及生成步末记录的 traceSpreads / traceSnails / beltMoves。
--
-- 依赖：Match3.Board、Grass（蔓延）、Snail（逐只爬行）、Conveyor（皮带）。只描述「变了什么」，不结算。
-- 同步：traceSpreads / traceSnails / beltMoves 必须与 trySwap 里 spreadVines → spreadChoco →
-- spreadSteam → stepSnailsAvoidingBlocked / shiftBelts 的组合完全一致；
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
  , traceSnails
  , beltMoves
  , emptyTrace
  ) where

import Match3.Board (inBounds, CascadeWave(..), setCell, getCell)
import Match3.Conveyor (Belt)
import Match3.Grass (spreadVines, spreadChoco, spreadSteam)
import Data.List (nub)
import Match3.Snail (stepSnailAtBlocked, snailPositions)
import Match3.Types
import Match3.Game.State

-- | 一步操作的逐轮回放脚本（纯数据，供前端分轮播放连锁）。
-- 与对应的 trySwap / useFreeSwap / useHammer / useCrossClear 走相同的步骤与随机数，
-- 但只记录盘面快照，不做任何结算。被拒的操作（NoMatch / InvalidSwap / 已结束）返回空脚本，
-- 前端据此不会播放任何消除或连击。
data MoveTrace = MoveTrace
  { mtStart :: Board          -- ^ 第一轮之前的盘面（交换后 / 道具作用前）
  , mtWaves :: [CascadeWave]  -- ^ 按时间顺序的每一轮（含倒计时爆炸 / 皮带 / 蜗牛后的续连锁）
  , mtFinal :: Board          -- ^ 所有轮次与步末效果之后的盘面；未触发自动洗牌时 == 结算后的 gsBoard
  , mtEnd   :: [EndStep]      -- ^ 步末效果（非消除的盘面变化），按发生顺序；见 EndStep
  } deriving (Eq, Show)

-- | 一个步末效果：在 mtWaves 的前 esAfterWaves 轮播完之后发生，把 esBefore 变成 esAfter。
-- 时间线 = 轮 0..k-1 → 所有 esAfterWaves == k 的步末效果（按列表顺序）→ 轮 k … → mtFinal。
-- 不变量（测试锁定）：applyEndEffect esEffect esBefore == esAfter，且首尾相接。
data EndStep = EndStep
  { esAfterWaves :: Int
  , esBefore     :: Board
  , esAfter      :: Board
  , esEffect     :: EndEffect
  } deriving (Eq, Show)

-- | 步末效果的种类与播放所需的细节（只描述「变了什么」，规则仍由原函数计算）。
data EndEffect
  = EndCountdownTick [Pos]            -- ^ 倒计时炸弹减一（列出数值真的变了的格）
  | EndBeltShift [(Pos, Pos)]         -- ^ 传送带移位：(原格, 新格)；多条皮带已按顺序合成
  | EndSpread SpreadKind [(Pos, Pos)] -- ^ 藤蔓 / 巧克力 / 蒸汽蔓延：(来源格, 新占的格)
  | EndSnail [SnailMove]              -- ^ 蜗牛爬行（按规则的逐只顺序）
  deriving (Eq, Show)

-- | 会在步末蔓延的叠层种类。
data SpreadKind = SpreadVine | SpreadChoco | SpreadSteam
  deriving (Eq, Show)

-- | 一只蜗牛的一步：smFrom == smTo 表示碰壁掉头；否则爬到 smTo，被推的格子
-- （smPushed，爬之前在 smTo 的内容）换到 smFrom。smDir 是这一步之后的朝向。
data SnailMove = SnailMove
  { smFrom   :: Pos
  , smTo     :: Pos
  , smDir    :: (Int, Int)
  , smPushed :: Maybe Cell
  } deriving (Eq, Show)

-- | 把步末效果的描述重放到盘面上（纯函数，供测试证明描述完整、前端必要时直接用）。
applyEndEffect :: EndEffect -> Board -> Board
applyEndEffect eff b0 = case eff of
  EndCountdownTick ps -> foldl tick b0 ps
  EndBeltShift moves -> foldl (\b (o, d) -> setCell b d (getCell b0 o)) b0 moves
  EndSpread kind pairs -> foldl (\b (_, q) -> plant (spreadOverlay kind) b q) b0 pairs
  EndSnail moves -> foldl snail b0 moves
  where
    tick b p = case getCell b p of
      Countdown col n -> setCell b p (Countdown col (max 0 (n - 1)))
      _ -> b
    plant ov b q = case getCell b q of
      Gem col kind ice Nothing -> setCell b q (Gem col kind ice (Just ov))
      _ -> b
    snail b (SnailMove from to (dr, dc) pushed)
      | from == to = setCell b from (Snail dr dc)
      | otherwise = case pushed of
          Just cell -> setCell (setCell b to (Snail dr dc)) from cell
          Nothing -> setCell b to (Snail dr dc)

-- | SpreadKind 对应的叠层构造器。
spreadOverlay :: SpreadKind -> CellOverlay
spreadOverlay SpreadVine = Vine
spreadOverlay SpreadChoco = Choco
spreadOverlay SpreadSteam = Steam

-- | 蔓延的 (来源, 新格)：新格 = 之前无覆盖层、之后带该覆盖层的格；来源取之前盘面上
-- 与新格正交相邻的第一个同类格（按行优先：上、左、右、下），只用于表现层决定「从哪边长出来」。
spreadPairs :: SpreadKind -> Board -> Board -> [(Pos, Pos)]
spreadPairs kind before after =
  [ (src, q)
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , let q = (r, c)
  , cellOverlay (getCell before q) == Nothing
  , cellOverlay (getCell after q) == Just ov
  , let srcs = [n | n <- [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)], inBounds n, cellOverlay (getCell before n) == Just ov]
  , let src = case srcs of
          (n : _) -> n
          [] -> q
  ]
  where
    ov = spreadOverlay kind

-- | 藤 → 巧 → 蒸汽 → 蜗牛 的逐步快照（与 trySwap / 道具里的组合完全相同），空效果不记录。
traceSpreads :: Int -> Board -> ([EndStep], Board)
traceSpreads k b0 =
  let bV = spreadVines b0
      bC = spreadChoco bV
      bS = spreadSteam bC
      step kind before after =
        [EndStep k before after (EndSpread kind ps) | let ps = spreadPairs kind before after, not (null ps)]
  in (step SpreadVine b0 bV ++ step SpreadChoco bV bC ++ step SpreadSteam bC bS, bS)

-- | stepSnailsAvoidingBlocked 的逐只记录版：对同一快照顺序逐只调用 stepSnailAtBlocked，
-- 结果盘面与原函数完全一致（测试锁定）。
traceSnails :: [Pos] -> [Pos] -> Board -> ([SnailMove], Board)
traceSnails avoid walls b0 = foldl one ([], b0) [p | p <- snailPositions b0, p `notElem` avoid]
  where
    one (acc, board) pos = case getCell board pos of
      Snail dr dc ->
        let board' = stepSnailAtBlocked walls board pos
            next = (fst pos + dr, snd pos + dc)
            mv = case getCell board' pos of
              Snail dr' dc' -> SnailMove pos pos (dr', dc') Nothing
              pushed -> SnailMove pos next (dr, dc) (Just pushed)
        in (acc ++ [mv], board')
      _ -> (acc, board)

-- | 多条皮带按顺序移位后的 (原格, 新格) 映射（只列位置变了的格）。
beltMoves :: [Belt] -> [(Pos, Pos)]
beltMoves belts =
  let cells = nub (concat belts)
      origin0 = [(p, p) | p <- cells]
      shiftOne orig ps
        | length ps < 2 = orig
        | otherwise =
            let prevOf = zip ps (last ps : init ps)
            in [ (p, maybe o (\q -> maybe q id (lookup q orig)) (lookup p prevOf))
               | (p, o) <- orig
               ]
      final = foldl shiftOne origin0 belts
  in [(o, d) | (d, o) <- final, o /= d]

-- | 被拒操作的空回放脚本：没有轮次、没有步末效果，前端什么都不播。
emptyTrace :: GameState -> MoveTrace
emptyTrace gs = MoveTrace (gsBoard gs) [] (gsBoard gs) []
