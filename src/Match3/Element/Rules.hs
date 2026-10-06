{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 规则的通用驱动（元素类重构第 3 刀）：只读 'Phase' / 'Layer' / 'Entity' 给出的数据与方法，把「找邻格、去重、跳过直接命中、
-- 按顺序写回」这些样板各写一次（以前散在 Match3.Obstacles / Match3.Grass 里，每种障碍 / 叠层一份）。
--
-- 顺序语义与旧函数逐项相同（元素对照快照 element-oracle.txt 的 AR / ER 行与金标准锁定）：
--
-- * 邻格目标：按真消除格的顺序、每格上 / 下 / 左 / 右，界内、认得出这种元素 / 叠层的格，整体去重；
--   'SkipDirect' 时去掉本轮直接命中的格；
-- * 打碎的格（'Dies'）：'DiePrepend' 用 @nub (q : dead)@ 前插；'DieAppend' 后插（气球等对齐旧列表序）；
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
import Data.Proxy (Proxy(..))
import Data.Maybe (isJust)
import Match3.Board.Grid (getCell, inBounds, neighborsInBounds, setCell)
import Match3.Element.Phase (toCell)
import Match3.Element.Event (EndEffect(..), EndItem(..), EventKind(..))
import Match3.Element.Kind
import Match3.Element.Phase (Phase, phaseDieOrder, phaseNearPrio, phaseOnNear, phaseReach)
import Match3.Element.Layer
import Match3.ECS.Stage
import Match3.ECS.System (System(..))
import Match3.Element.Types
import Match3.Types

-- | 一种本体的全部规则：方法邻格（'cNear'）→ 逃生口 'cSystems'（多格实体扣血也在这里；之后按优先级稳定排序）。
kindRules :: forall e proxy. Phase e => proxy e -> [SysDef]
kindRules _ =
  [SysNear o (kindNeighbour (Proxy @e)) | Just o <- [phaseNearPrio @e]]
    ++ boardSystems (Proxy @e)

-- | 一种叠层的全部规则：邻格规则、蔓延（PhaseSpread）、逃生口 'layerSystems'（都读自类型级的 'layerCover'）。
layerRules :: Layer l => proxy l -> [SysDef]
layerRules p =
  [SysNear o (layerNeighbour p) | Just o <- [layerNeighbourPrio p]]
    ++ [SysEnd (spreadSys o (layerSpread p seed)) | Just (o, seed) <- [spreads p]]
    ++ layerSystems p

-- | 邻格目标：与真消除格正交相邻、满足谓词的格（去重；顺序 = 按真消除格、每格上 / 下 / 左 / 右）。
neighbourTargets :: Reach -> (Cell -> Bool) -> NearWorld -> Board -> [Pos]
neighbourTargets r ok ctx b =
  [q | q <- nub [q' | t <- nwTrue ctx, q' <- neighborsInBounds upDownLeftRight b t, ok (getCell b q')], not (skip q)]
  where
    skip q = case r of
      SkipDirect -> q `elem` nwDirect ctx
      AllNeighbours -> False

-- | 一个邻格 system 的产出累积（盘面, 打碎的格, 坐住的格）。
data Out = Out Board [Pos] [Pos]

-- | 把产出写回世界。
emit :: NearWorld -> Out -> NearWorld
emit w (Out b dead sit) = w {nwBoard = b, nwDead = dead, nwSit = sit}

-- | 写回一格的回答。
nudge :: DieOrder -> Out -> Pos -> Nudge -> Out
nudge order out@(Out b dead sit) q n = case n of
  Untouched -> out
  Becomes c -> Out (setCell b q c) dead sit
  Dies -> case order of
    DiePrepend -> Out b (nub (q : dead)) sit
    DieAppend -> Out b (dead ++ [q | q `notElem` dead]) sit

-- | 触发消除格颜色（与 Obstacles.isGem+cellColor 一致：含倒计时 / 双面块；其余 Nothing）。
triggerColors :: NearWorld -> Board -> Pos -> [(Pos, Maybe Color)]
triggerColors ctx b self =
  [ (t, cellColor (getCell b t))
  | t <- nwTrue ctx
  , self `elem` neighborsInBounds upDownLeftRight b t
  ]

-- | 本体的邻格波及：目标格逐个问 'onNear'。
kindNeighbour :: forall e proxy. Phase e => proxy e -> System NearWorld
kindNeighbour p = System $ \ctx -> let b0 = nwBoard ctx in emit ctx (foldl (one ctx b0) (Out b0 [] []) (neighbourTargets (phaseReach @e) (isJust . fromCellAs p) ctx b0))
  where
    order = phaseDieOrder @e
    one ctx b0 out@(Out b dead sit) q = case fromCellAs p (getCell b q) of
      Nothing -> out
      Just e ->
        let nctx = NearCtx (triggerColors ctx b0 q) q ctx b
         in case phaseOnNear e nctx of
              NearIdle -> out
              NearNudge n -> nudge order out q n
              NearEdit b' d s -> Out b' (dead ++ [x | x <- d, x `notElem` dead]) (nub (s ++ sit))

-- | 叠层的邻格波及：目标格逐个问 'onLayerNeighbourClear'（= lcOnNear）（同一套目标规则）。
layerNeighbour :: Layer l => proxy l -> System NearWorld
layerNeighbour p = System $ \ctx -> let b0 = nwBoard ctx in emit ctx (foldl one (Out b0 [] []) (neighbourTargets (layerReach p) (isJust . peelAs p) ctx b0))
  where
    one out@(Out bOut _ _) q =
      let cell = getCell bOut q
      in maybe out (\(l, _) -> nudge DiePrepend out q (onLayerNeighbourClear l cell)) (peelAs p cell)

-- | 没有叠层的宝石（蔓延的落点）。
bareGem :: Cell -> Bool
bareGem cell = case cell of
  Gem _ _ _ Nothing -> True
  _ -> False

-- | 叠层的步末蔓延：步首盘面上有本层的每一格向正交相邻的裸宝石种上 @seed@；记一条 EvSpread（来源, 新格）。
layerSpread :: Layer l => proxy l -> l -> System EndWorld
layerSpread p seed = effectSystem (layerSpreadOn p seed . ewBoard)

layerSpreadOn :: Layer l => proxy l -> l -> Board -> (Maybe EndEffect, Board)
layerSpreadOn p seed b =
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
{-# INLINABLE entityDamage #-}
entityDamage :: forall e. Phase e => Entity e -> System NearWorld
entityDamage ent = System $ \ctx -> let b0 = nwBoard ctx in emit ctx (entityDamageOn ent ctx b0)

entityDamageOn :: forall e. Phase e => Entity e -> NearWorld -> Board -> Out
entityDamageOn ent ctx b0 = foldl one (Out b0 [] []) anchors
  where
    at' bd q = fromCell @e (getCell bd q)
    anchors = [(q, e) | q <- boardPositions b0, Just e <- [at' b0 q], partNo ent e == 0]
    one out@(Out b dead sit) (anchor, e) =
      let body = footprint ent anchor
          parts = [(q, x) | (i, q) <- zip [0 ..] body, inBounds b q, Just x <- [at' b q], partNo ent x == i]
          ring = foldr (\q acc -> if q `elem` acc then acc else q : acc) [] [q | x <- body, q <- neighborsInBounds upDownLeftRight b x, q `notElem` body]
          dmg = length [q | q <- ring, q `elem` nwTrue ctx] + length [q | (q, _) <- parts, q `elem` nwDirect ctx]
          hp' = max 0 (hitPoints ent e - dmg)
      in if dmg == 0
           then out
           else
             if hp' == 0
               then Out b (dead ++ map fst parts) sit
               else Out (foldl (\bd (q, x) -> setCell bd q (toCell (withHp ent hp' x))) b parts) dead sit
