-- | 元素注册表：元素名 → ElementDef，以及主流程查询元素行为的**唯一入口**（各 *With 函数）。
--
-- 分派：内置本体按 cellSlot 编号、叠层按 overlaySlot 编号用数组 O(1) 查；冰层单独一个定义；
-- Custom 按名字查（未注册的名字退回 baseDef：挡交换、打不动、会下落的惰性占格）。
-- 多层询问：冰层 → 叠层 → 本体，自上而下（见 layerDefs）。
--
-- 依赖：Element.Types、Match3.Types、Board.Grid。不含任何具体元素（内置定义见 Element.Builtin）。
module Match3.Element.Registry
  ( Registry
  , mkRegistry
  , register
  , registryDefs
  , lookupElement
  , bodyDef
  , overlayDef
  , iceDef
  , layerDefs
  , elementName
  , topLayerName
    -- * 主流程的查询（钩子）
  , matchColorWith
  , colorOfWith
  , blocksSwapWith
  , upperBlocksSwapWith
  , swapBlockedWith
  , activatesWith
  , fallsWith
  , portalWith
  , drainsWith
  , directHitWith
  , chipOnHitWith
  , hitImmuneWith
  , stripOnClearWith
  , adjacentRules
  , runAdjacentWith
  , counterWith
  , diffDefs
  , countElementWith
  , vacatesCarpetWith
  , keepOnShuffleWith
  , blastWith
  , endRules
  , placeWith
  , placeAllWith
  ) where

import Data.Array (Array, accumArray, (!))
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Match3.Board.Grid (getCell, setCell)
import Match3.Element.Types
import Match3.Types

-- | 注册表。用 mkRegistry / register 构造；字段不导出（分派数组由定义列表派生）。
data Registry = Registry
  { regDefs     :: [ElementDef]              -- 注册顺序
  , regCells    :: Array Int ElementDef      -- 内置本体 0..19
  , regOverlays :: Array Int ElementDef      -- 叠层 0..7
  , regIce      :: ElementDef
  , regCustom   :: [(ElementName, ElementDef)]
  , regAdjacent :: [AdjacentRule]            -- 按 arOrder 排好（稳定）
  , regEnd      :: [EndRule]                 -- 按 (阶段, erOrder) 排好（稳定）
  , regDiff     :: [ElementDef]              -- 带 edDiffCounter 的定义
  }

-- | 由定义列表建表。同一槽位 / 同名的多个定义以后出现的为准。
mkRegistry :: [ElementDef] -> Registry
mkRegistry defs0 =
  let defs = dedupe defs0
      fallback = baseDef "?"
      cells = accumArray (\_ d -> d) fallback (0, 19) [(i, d) | d <- defs, SlotCell i <- [edSlot d]]
      ovs = accumArray (\_ d -> d) fallback (0, 7) [(i, d) | d <- defs, SlotOverlay i <- [edSlot d]]
      ice = fromMaybe fallback (listToMaybe (reverse [d | d <- defs, edSlot d == SlotIce]))
  in Registry
       { regDefs = defs
       , regCells = cells
       , regOverlays = ovs
       , regIce = ice
       , regCustom = [(edName d, d) | d <- defs, edSlot d == SlotCustom]
       , regAdjacent = sortOn arOrder (mapMaybe edAdjacent defs)
       , regEnd = sortOn (\r -> (erPhase r, erOrder r)) (mapMaybe edEnd defs)
       , regDiff = [d | d <- defs, Just _ <- [edDiffCounter d]]
       }
  where
    -- 同名只留最后一个，位置取第一次出现处（注册顺序稳定）
    dedupe ds =
      let names = nub (map edName ds)
      in [last [d | d <- ds, edName d == n] | n <- names]

-- | 往注册表里加（或按名字替换）一个定义。测试专用元素就这样接进来，主流程不用改。
register :: ElementDef -> Registry -> Registry
register d reg = mkRegistry (regDefs reg ++ [d])

-- | 全部定义（注册顺序）。
registryDefs :: Registry -> [ElementDef]
registryDefs = regDefs

-- | 按名字找定义（关卡放置、计数、文档用）。
lookupElement :: Registry -> ElementName -> Maybe ElementDef
lookupElement reg n = listToMaybe [d | d <- regDefs reg, edName d == n]

-- | 本体层的定义。
bodyDef :: Registry -> Cell -> ElementDef
bodyDef reg cell = case cell of
  Custom n _ -> fromMaybe (baseDef n) (lookup n (regCustom reg))
  _ -> regCells reg ! cellSlot cell

-- | 叠层的定义。
overlayDef :: Registry -> CellOverlay -> ElementDef
overlayDef reg ov = regOverlays reg ! overlaySlot ov

-- | 冰层的定义。
iceDef :: Registry -> ElementDef
iceDef = regIce

-- | 本体之上的各层（自上而下）：冰层（ice > 0）、叠层。
upperDefs :: Registry -> Cell -> [ElementDef]
upperDefs reg cell = case cell of
  Gem _ _ ice ov -> [regIce reg | ice > 0] ++ maybe [] (\o -> [overlayDef reg o]) ov
  _ -> []

-- | 格子的全部层（自上而下，本体最后）。
layerDefs :: Registry -> Cell -> [ElementDef]
layerDefs reg cell = upperDefs reg cell ++ [bodyDef reg cell]

-- | 本体的元素名。
elementName :: Registry -> Cell -> ElementName
elementName reg = edName . bodyDef reg

-- | 最上面一层的元素名（事件里「波及了什么」用）。
topLayerName :: Registry -> Cell -> ElementName
topLayerName reg cell = edName (head (layerDefs reg cell))

--------------------------------------------------------------------------------
-- 钩子

-- | 参与匹配的颜色：任一上层挡匹配则 Nothing，否则本体颜色。（匹配、提示的热路径。）
matchColorWith :: Registry -> Cell -> Maybe Color
matchColorWith reg cell = case cell of
  Gem _ _ ice ov
    | ice > 0 && edBlocksMatch (regIce reg) -> Nothing
    | Just o <- ov, edBlocksMatch (overlayDef reg o) -> Nothing
  _ -> edColor (bodyDef reg cell) cell

-- | 本体颜色（颜色袋计数用，不看叠层）。
colorOfWith :: Registry -> Cell -> Maybe Color
colorOfWith reg cell = edColor (bodyDef reg cell) cell

-- | 本格不能被交换（任一层挡）。
blocksSwapWith :: Registry -> Cell -> Bool
blocksSwapWith reg cell = any edBlocksSwap (layerDefs reg cell)

-- | 只看上层（冰 / 叠层）是否挡交换（彩虹 / 特殊合成提示用，本体由它们自己判定）。
upperBlocksSwapWith :: Registry -> Cell -> Bool
upperBlocksSwapWith reg cell = any edBlocksSwap (upperDefs reg cell)

-- | 交换两格是否被挡（任一端挡即挡）。
swapBlockedWith :: Registry -> Board -> Pos -> Pos -> Bool
swapBlockedWith reg b p1 p2 = blocksSwapWith reg (getCell b p1) || blocksSwapWith reg (getCell b p2)

-- | 特殊块能否点火：自上而下第一个 Just 决定；都没有意见则 False。
activatesWith :: Registry -> Cell -> Bool
activatesWith reg cell = fromMaybe False (listToMaybe (mapMaybe (\d -> edActivates d cell) (layerDefs reg cell)))

-- | 本体随重力下落。
fallsWith :: Registry -> Cell -> Bool
fallsWith reg = edFalls . bodyDef reg

-- | 本体能穿过传送门。
portalWith :: Registry -> Cell -> Bool
portalWith reg = edPortal . bodyDef reg

-- | 本体落到底行被收走。
drainsWith :: Registry -> Cell -> Bool
drainsWith reg = edDrains . bodyDef reg

-- | 直接命中：自上而下，第一个不是 HitPierce 的层决定；本体也 Pierce 则视为打不动。
directHitWith :: Registry -> Cell -> HitResult
directHitWith reg cell = go (layerDefs reg cell)
  where
    go [] = HitImmune
    go (d : ds) = case edOnHit d cell of
      HitPierce -> go ds
      r -> r

-- | 对一组种子逐格结算直接命中（去重后按顺序）：返回 (新盘面, 被消除的格)。
-- 被消除的格按「后命中的在前」排列（与旧 chipIceOnClear 相同，下游只当集合用）。
chipOnHitWith :: Registry -> Board -> [Pos] -> (Board, [Pos])
chipOnHitWith reg b seeds = foldl step (b, []) (nub seeds)
  where
    step (board, clearable) p = case directHitWith reg (getCell board p) of
      HitAbsorb cell' -> (setCell board p cell', clearable)
      HitDestroy -> (board, p : clearable)
      HitImmune -> (board, clearable)
      HitPierce -> (board, clearable)

-- | 直接命中打不动（锤子对它拒绝且不扣次数）。
hitImmuneWith :: Registry -> Cell -> Bool
hitImmuneWith reg cell = directHitWith reg cell == HitImmune

-- | 真消除格上随格清掉的叠层（草 / 藤 / 巧）。
stripOnClearWith :: Registry -> Board -> [Pos] -> Board
stripOnClearWith reg b ps = foldl strip b (nub ps)
  where
    strip board p = case getCell board p of
      Gem col kind ice (Just o) | edStripOnClear (overlayDef reg o) -> setCell board p (Gem col kind ice Nothing)
      _ -> board

-- | 邻格波及规则（已按 arOrder 排好）。
adjacentRules :: Registry -> [AdjacentRule]
adjacentRules = regAdjacent

-- | 按顺序跑完一轮的全部邻格波及：返回 (盘面, 打碎的格（按规则顺序拼接）, 新生成需坐住的格)。
-- 每条规则的 acProtect = 起始保护格 ++ 之前各规则的 aoSit。
runAdjacentWith :: Registry -> [Pos] -> [Pos] -> [Pos] -> Board -> (Board, [Pos], [Pos])
runAdjacentWith reg trueClears direct protect0 b0 = foldl one (b0, [], []) (regAdjacent reg)
  where
    one (board, dead, sits) rule =
      let out = arRun rule (AdjCtx trueClears direct (nub (protect0 ++ sits))) board
      in (aoBoard out, dead ++ aoDead out, sits ++ aoSit out)

-- | 本体进入清除格时的计数键。
counterWith :: Registry -> Cell -> Maybe Counter
counterWith reg = edCounter . bodyDef reg

-- | 按前后个数差计数的定义（保险箱、时间精灵、自定义）。
diffDefs :: Registry -> [ElementDef]
diffDefs = regDiff

-- | 盘上本体为该元素的格数。
countElementWith :: Registry -> ElementName -> Board -> Int
countElementWith reg n b = length [() | cell <- boardCells b, elementName reg cell == n]

-- | 本体离开格子（不进清除格）也算覆盖地毯。
vacatesCarpetWith :: Registry -> Cell -> Bool
vacatesCarpetWith reg = edVacatesCarpet . bodyDef reg

-- | 洗牌时原样放回：有冰 / 叠层，或本体要求保留。
keepOnShuffleWith :: Registry -> Cell -> Bool
keepOnShuffleWith reg cell = not (null (upperDefs reg cell)) || edKeepOnShuffle (bodyDef reg cell) cell

-- | 本体被消除且能点火时的爆炸范围（不能点火 / 没有爆炸 → []）。
blastWith :: Registry -> Cell -> Pos -> [Pos]
blastWith reg cell p = case edBlast (bodyDef reg cell) of
  Just f | activatesWith reg cell -> f p
  _ -> []

-- | 某步末阶段的规则（按 erOrder）。
endRules :: Registry -> EndPhase -> [EndRule]
endRules reg ph = [r | r <- regEnd reg, erPhase r == ph]

-- | 按名字放置一个元素到若干格（未注册的名字报错，关卡表写错名字应当立刻暴露）。
placeWith :: Registry -> ElementName -> [Arg] -> Board -> [Pos] -> Board
placeWith reg n args b0 ps = case lookupElement reg n of
  Nothing -> error ("placeWith: unknown element " ++ n)
  Just d -> foldl (\b p -> maybe b (setCell b p) (edPlace d args (getCell b p))) b0 ps

-- | 按顺序应用一张放置表。
placeAllWith :: Registry -> Board -> [Placement] -> Board
placeAllWith reg = foldl (\b (Place n args ps) -> placeWith reg n args b ps)
