-- | 规则的通用驱动（元素类重构第 3 刀）：只调 'Kind' / 'Layer' / 'Entity' 的方法，把「找邻格、去重、跳过直接命中、
-- 按顺序写回」这些样板各写一次（以前散在 Match3.Obstacles / Match3.Grass 里，每种障碍 / 叠层一份）。
--
-- 顺序语义与旧函数逐项相同（元素对照快照 element-oracle.txt 的 AR / ER 行与金标准锁定）：
--
-- * 邻格目标：按真消除格的顺序、每格上 / 下 / 左 / 右，界内、认得出这种元素 / 叠层的格，整体去重；
--   'SkipDirect' 时去掉本轮直接命中的格；
-- * 打碎的格（'Dies'）用 @nub (q : dead)@ 前插——后处理的在前；
-- * 蔓延：来源取步首盘面（新种上的这一步不再蔓延），落点是正交相邻的裸宝石（无叠层）；事件的来源取读序第一个
--   同层邻格；
-- * 多格实体：锚点行优先，伤害 = 身外一圈的真消除格数 + 部件上的直接命中格数，归零时全部部件并入清除格。
module Match3.Element.Rules
  ( -- * 收集
    kindRules
  , layerRules
    -- * 驱动
  , kindNeighbour
  , layerNeighbour
  , layerSpread
  , entityDamage
  ) where

import Data.List (nub)
import Data.Maybe (isJust)
import Match3.Board.Grid (getCell, inBounds, neighborsInBounds, setCell)
import Match3.Element.Ability (toCell)
import Match3.Element.Event (EndEffect(..), EndItem(..), EventKind(..))
import Match3.Element.Kind
import Match3.Element.Layer
import Match3.Element.Types
import Match3.Types

-- | 一种本体的全部规则：方法邻格 → 'entityHit' 扣血 → 逃生口 'boardPasses'（之后按优先级稳定排序）。
kindRules :: Kind e => proxy e -> [BoardPass]
kindRules p =
  [AdjacentPass o (kindNeighbour p) | Just o <- [neighbourPrio p]]
    ++ [AdjacentPass o f | Just (o, f) <- [entityHit p]]
    ++ boardPasses p

-- | 一种叠层的全部规则：邻格规则、蔓延（PhaseSpread）、逃生口 'layerPasses'。
layerRules :: Layer l => proxy l -> [BoardPass]
layerRules p =
  [AdjacentPass o (layerNeighbour p) | Just o <- [layerNeighbourPrio p]]
    ++ [EndPass (spreadRule o (layerSpread p seed)) | Just (o, seed) <- [spreads p]]
    ++ layerPasses p

-- | 邻格目标：与真消除格正交相邻、满足谓词的格（去重；顺序 = 按真消除格、每格上 / 下 / 左 / 右）。
neighbourTargets :: Reach -> (Cell -> Bool) -> AdjCtx -> Board -> [Pos]
neighbourTargets r ok ctx b =
  [q | q <- nub [q' | t <- acTrue ctx, q' <- neighborsInBounds upDownLeftRight b t, ok (getCell b q')], not (skip q)]
  where
    skip q = case r of
      SkipDirect -> q `elem` acDirect ctx
      AllNeighbours -> False

-- | 写回一格的回答。
nudge :: AdjOut -> Pos -> Nudge -> AdjOut
nudge out@(AdjOut b dead sit) q n = case n of
  Untouched -> out
  Becomes c -> AdjOut (setCell b q c) dead sit
  Dies -> AdjOut b (nub (q : dead)) sit

-- | 本体的邻格波及：目标格逐个问 'onNeighbourClear'。
kindNeighbour :: Kind e => proxy e -> AdjCtx -> Board -> AdjOut
kindNeighbour p ctx b0 = foldl one (AdjOut b0 [] []) (neighbourTargets (reach p) (isJust . fromCellAs p) ctx b0)
  where
    one out q = maybe out (nudge out q . onNeighbourClear) (fromCellAs p (getCell (aoBoard out) q))

-- | 叠层的邻格波及：目标格逐个问 'onLayerNeighbourClear'（同一套目标规则）。
layerNeighbour :: Layer l => proxy l -> AdjCtx -> Board -> AdjOut
layerNeighbour p ctx b0 = foldl one (AdjOut b0 [] []) (neighbourTargets (layerReach p) (isJust . peelAs p) ctx b0)
  where
    one out q =
      let cell = getCell (aoBoard out) q
      in maybe out (\(l, _) -> nudge out q (onLayerNeighbourClear l cell)) (peelAs p cell)

-- | 没有叠层的宝石（蔓延的落点）。
bareGem :: Cell -> Bool
bareGem cell = case cell of
  Gem _ _ _ Nothing -> True
  _ -> False

-- | 叠层的步末蔓延：步首盘面上有本层的每一格向正交相邻的裸宝石种上 @seed@；记一条 EvSpread（来源, 新格）。
layerSpread :: Layer l => proxy l -> l -> EndCtx -> Board -> (Maybe EndEffect, Board)
layerSpread p seed _ b =
  let has bd q = isJust (peelAs p (getCell bd q))
      targets = nub [q | s <- boardPositions b, has b s, q <- neighborsInBounds upDownLeftRight b s, bareGem (getCell b q)]
      b' = foldl (\bd q -> if bareGem (getCell bd q) then setCell bd q (putOn seed (getCell bd q)) else bd) b targets
      pairs =
        [ (src, q)
        | q <- boardPositions b
        , cellOverlay (getCell b q) == Nothing
        , has b' q
        , let src = case [n | n <- neighborsInBounds readingOrder b q, has b n] of
                (n : _) -> n
                [] -> q
        ]
  in (if null pairs then Nothing else Just (EndEffect EvSpread (layerName p) [EndItem s q (getCell b' q) Nothing | (s, q) <- pairs]), b')

-- | 多格实体的邻格伤害：每个锚点（行优先）按「身外一圈的真消除 + 部件上的直接命中」扣血，归零则部件并入清除格。
entityDamage :: Entity e => proxy e -> AdjCtx -> Board -> AdjOut
entityDamage p ctx b0 = foldl one (AdjOut b0 [] []) anchors
  where
    at' bd q = fromCellAs p (getCell bd q)
    anchors = [(q, e) | q <- boardPositions b0, Just e <- [at' b0 q], partNo e == 0]
    one out@(AdjOut b dead sit) (anchor, e) =
      let body = footprint p anchor
          parts = [(q, x) | (i, q) <- zip [0 ..] body, inBounds b q, Just x <- [at' b q], partNo x == i]
          ring = foldr (\q acc -> if q `elem` acc then acc else q : acc) [] [q | x <- body, q <- neighborsInBounds upDownLeftRight b x, q `notElem` body]
          dmg = length [q | q <- ring, q `elem` acTrue ctx] + length [q | (q, _) <- parts, q `elem` acDirect ctx]
          hp' = max 0 (hitPoints e - dmg)
      in if dmg == 0
           then out
           else
             if hp' == 0
               then AdjOut b (dead ++ map fst parts) sit
               else AdjOut (foldl (\bd (q, x) -> setCell bd q (toCell (withHp hp' x))) b parts) dead sit
