{-# LANGUAGE OverloadedStrings #-}
-- | 元素框架的事件词汇：规则层产出的**纯数据**效果描述，前端按事件类型查表播放。
--
-- 两类：
--   * 步末效果 EndEffect：原在 Match3.Game.Trace，第二刀 2b 搬到这里，因为元素的步末规则（`endRule`）要直接产出它；
--     Game.Trace 原样再导出。第 7 刀（7b）起是**通用形状**：事件类型 + 元素名 + 逐项 EndItem（来源、目标、写入目标格的
--     内容、换回来源格的内容），倒计时 / 皮带 / 藤 / 巧 / 蒸汽 / 蜗牛都是这一种形状（原来的四个构造器、SpreadKind、
--     SnailMove 已删）；新的步末元素不用改这里就能产出可重放、可播放的效果。
--   * 效果事件 Event（消除 / 波及 / 特殊块爆炸 / 收集 / 得分 / 连击 / 步末 / 洗牌）：由回放脚本派生
--     （Match3.Game.Trace.traceEvents），不参与结算。
--
-- 依赖：Match3.Types、Board.Grid。不含规则判断。
module Match3.Element.Event
  ( -- * 步末效果
    EndEffect(..)
  , EndItem(..)
  , applyEndEffect
  , endEffectPairs
  , endItemDir
  , spreadPairs
    -- * 效果事件
  , EventKind(..)
  , Event(..)
  ) where

import Match3.Board.Grid (getCell, neighborsInBounds, setCell)
import Match3.Types

-- | 步末效果（通用形状）：事件类型（EvTick / EvBelt / EvSpread / EvMove …，前端播放表的键）、相关元素名
-- （注册表的键；皮带是关卡级元素，记为 "belt"）、按发生顺序的逐项变化。只描述「变了什么」，规则仍由元素定义计算。
data EndEffect = EndEffect
  { endEffectKind    :: EventKind
  , endEffectElement :: ElementName
  , endEffectItems   :: [EndItem]
  } deriving (Eq)

-- | Show 手写（第 7 刀 7b）：内置的六种步末效果显示成第 7 刀前四个构造器的派生文本（EndCountdownTick /
-- EndBeltShift / EndSpread SpreadVine… / EndSnail [SnailMove {…}]），金标准与元素查询快照里的散列因此逐字不变；
-- 其余（扩展元素的效果）按记录语法显示全部字段。内置形状的文本不含 eiCell（可由前盘重算），Eq 仍比较全部字段。
instance Show EndEffect where
  showsPrec d eff = case (endEffectKind eff, unElementName (endEffectElement eff)) of
    (EvTick, "countdown") -> con "EndCountdownTick " (showsPrec 11 (map eiTo items))
    (EvBelt, "belt") -> con "EndBeltShift " (showsPrec 11 pairs)
    (EvSpread, n) | Just k <- lookup n spreadKinds -> con "EndSpread " (showString k . showChar ' ' . showsPrec 11 pairs)
    (EvMove, "snail") | Just ms <- mapM snailText items -> con "EndSnail " (showChar '[' . foldr (.) id (commas ms) . showChar ']')
    _ ->
      showParen (d >= 11) $
        showString "EndEffect {endEffectKind = " . showsPrec 0 (endEffectKind eff)
          . showString ", endEffectElement = " . showsPrec 0 (endEffectElement eff)
          . showString ", endEffectItems = " . showsPrec 0 items
          . showChar '}'
    where
      items = endEffectItems eff
      pairs = endEffectPairs eff
      con name body = showParen (d >= 11) (showString name . body)
      spreadKinds = [("vine", "SpreadVine"), ("choco", "SpreadChoco"), ("steam", "SpreadSteam")]
      snailText i =
        fmap
          ( \dir ->
              showString "SnailMove {smFrom = " . showsPrec 0 (eiFrom i)
                . showString ", smTo = " . showsPrec 0 (eiTo i)
                . showString ", smDir = " . showsPrec 0 dir
                . showString ", smPushed = " . showsPrec 0 (eiBack i)
                . showChar '}'
          )
          (endItemDir i)
      commas xs = case xs of
        [] -> []
        (x : rest) -> x : map (showChar ',' .) rest

-- | 步末效果的一项：eiFrom → eiTo（单格效果 eiFrom == eiTo）；重放时目标格写成 eiCell，
-- eiBack 为 Just 时来源格写成它（会走的元素推开的格换到原格）。
--
-- 内置：倒计时 = (p, p, 减一后的格)；皮带 = (原格, 新格, 原格的内容)；蔓延 = (来源, 新格, 带叠层的新格)；
-- 蜗牛 = (起点, 终点, 新朝向的蜗牛, 被推开的格)，碰壁掉头时起点 == 终点、eiBack = Nothing。
data EndItem = EndItem
  { eiFrom :: Pos
  , eiTo   :: Pos
  , eiCell :: Cell
  , eiBack :: Maybe Cell
  } deriving (Eq, Show)

-- | 把步末效果的描述重放到盘面上（纯函数，供测试证明描述完整、前端必要时直接用）：逐项、先目标后来源。
applyEndEffect :: EndEffect -> Board -> Board
applyEndEffect eff b0 = foldl one b0 (endEffectItems eff)
  where
    one b (EndItem from to cell back) =
      let b' = setCell b to cell
      in maybe b' (setCell b' from) back

-- | 步末效果涉及的 (来源, 目标) 对：皮带 = (原格, 新格)；蔓延 = (来源, 新格)；
-- 蜗牛 = (起点, 终点)；倒计时 = (p, p)。
endEffectPairs :: EndEffect -> [(Pos, Pos)]
endEffectPairs eff = [(eiFrom i, eiTo i) | i <- endEffectItems eff]

-- | 一项写入的本体朝向（会走的元素：蜗牛）；其余为 Nothing。
endItemDir :: EndItem -> Maybe (Int, Int)
endItemDir i = case eiCell i of
  Snail dr dc -> Just (dr, dc)
  _ -> Nothing

-- | 蔓延的 (来源, 新格)：新格 = 之前无覆盖层、之后带该覆盖层的格；来源取之前盘面上
-- 与新格正交相邻的第一个同类格（按行优先：上、左、右、下 = 'readingOrder'），只用于表现层决定「从哪边长出来」。
-- 注意这里的顺序与邻消 / 蔓延的上、下、左、右不同（第 7 项起两种顺序各有名字）。
spreadPairs :: CellOverlay -> Board -> Board -> [(Pos, Pos)]
spreadPairs ov before after =
  [ (src, q)
  | q <- boardPositions before
  , cellOverlay (getCell before q) == Nothing
  , cellOverlay (getCell after q) == Just ov
  , let srcs = [n | n <- neighborsInBounds readingOrder before q, cellOverlay (getCell before n) == Just ov]
  , let src = case srcs of
          (n : _) -> n
          [] -> q
  ]

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
  , evElement :: ElementName
  , evCells   :: [(Pos, Pos)]
  , evAmount  :: Int
  } deriving (Eq, Show)
