-- | 元素框架的事件词汇：规则层产出的**纯数据**效果描述，前端按事件类型查表播放。
--
-- 两类：
--   * 步末效果 EndEffect（倒计时 / 皮带 / 蔓延 / 蜗牛）：原在 Match3.Game.Trace，第二刀 2b 搬到这里，
--     因为元素定义（ElementDef 的步末规则）要直接产出它；Game.Trace 原样再导出，旧代码不用改。
--   * 效果事件 Event（消除 / 波及 / 特殊块爆炸 / 收集 / 得分 / 连击 / 步末 / 洗牌）：由回放脚本派生
--     （Match3.Game.Trace.traceEvents），不参与结算。
--
-- 依赖：Match3.Types、Board.Grid。不含规则判断。
module Match3.Element.Event
  ( -- * 步末效果
    EndEffect(..)
  , SpreadKind(..)
  , SnailMove(..)
  , applyEndEffect
  , spreadOverlay
  , spreadPairs
    -- * 效果事件
  , EventKind(..)
  , Event(..)
  , endEffectKind
  , endEffectElement
  , endEffectPairs
  ) where

import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Types

-- | 步末效果的种类与播放所需的细节（只描述「变了什么」，规则仍由元素定义计算）。
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

--------------------------------------------------------------------------------
-- 效果事件

-- | 事件类型。前端的播放表（帧数、粒子、绘制函数）以它为键。
data EventKind
  = EvClear    -- ^ 一轮里被消掉的格（真消除 + 打碎的障碍）
  | EvHit      -- ^ 波及：本轮格子内容变了但没消掉（削层 / 揭叠层 / 变色 / 开箱 / 充能）
  | EvBlast    -- ^ 特殊块爆炸（直线 / 炸弹）：evCells 为 (特殊块位置, 爆炸覆盖格)
  | EvDrain    -- ^ 底行收走饼干
  | EvScore    -- ^ 本轮得分（evAmount）
  | EvCombo    -- ^ 连击：第 2 轮起每轮一个（evAmount = 波次）
  | EvTick     -- ^ 步末：倒计时减一
  | EvBelt     -- ^ 步末：皮带移位
  | EvSpread   -- ^ 步末：蔓延（evElement = vine / choco / steam）
  | EvMove     -- ^ 步末：会走的元素爬行（蜗牛）
  | EvShuffle  -- ^ 自动洗牌（mtShuffle）
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | 一个效果事件。evWave：发生在第几轮之后（与 EndStep.esAfterWaves 同一时间轴；轮内事件 = 轮下标）。
-- evCells：(来源, 目标) 对；单格事件写成 (p, p)。evElement：相关元素名（注册表的键），无则为 ""。
data Event = Event
  { evKind    :: EventKind
  , evWave    :: Int
  , evElement :: String
  , evCells   :: [(Pos, Pos)]
  , evAmount  :: Int
  } deriving (Eq, Show)

-- | 步末效果对应的事件类型。
endEffectKind :: EndEffect -> EventKind
endEffectKind eff = case eff of
  EndCountdownTick _ -> EvTick
  EndBeltShift _ -> EvBelt
  EndSpread _ _ -> EvSpread
  EndSnail _ -> EvMove

-- | 步末效果相关的元素名（注册表的键；皮带是关卡特性，记为 "belt"）。
endEffectElement :: EndEffect -> String
endEffectElement eff = case eff of
  EndCountdownTick _ -> "countdown"
  EndBeltShift _ -> "belt"
  EndSpread SpreadVine _ -> "vine"
  EndSpread SpreadChoco _ -> "choco"
  EndSpread SpreadSteam _ -> "steam"
  EndSnail _ -> "snail"

-- | 步末效果涉及的 (来源, 目标) 对：皮带 = (原格, 新格)；蔓延 = (来源, 新格)；
-- 蜗牛 = (起点, 终点)；倒计时 = (p, p)。
endEffectPairs :: EndEffect -> [(Pos, Pos)]
endEffectPairs eff = case eff of
  EndBeltShift mv -> mv
  EndSpread _ ps -> ps
  EndSnail ms -> [(smFrom m, smTo m) | m <- ms]
  EndCountdownTick ps -> [(p, p) | p <- ps]
