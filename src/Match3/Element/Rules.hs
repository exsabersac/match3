-- | 通用 system 构造器：只读原型的存储列（Match3.ECS.Archetype.Column）/ 叠层 'Layer' / 'Entity' 给出的数据，
-- 把「找邻格、去重、跳过直接命中、按顺序写回」这些样板各写一次；元素只给出自己那一格的反应。
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
    layerRules
    -- * 邻格 system 构造器
  , nearBy
  , chipNear
  , layerNeighbour
  , layerSpread
  , entityDamage
    -- * 命中组件构造器
  , chipHit
  ) where

import Data.List (nub)
import Data.Maybe (isJust)
import Match3.Board.Grid (getCell, inBounds, neighborsInBounds, setCell)
import Match3.ECS.Archetype (Column(..), Entity(..))
import Match3.ECS.Component (OnHit, absorbHit, breakHit)
import Match3.Element.Event (EndEffect(..), EndItem(..), EventKind(..))
import Match3.Element.Near
import Match3.Element.Layer
import Match3.ECS.Stage
import Match3.ECS.System (System(..))
import Match3.Element.Types
import Match3.Types

-- | 一种叠层的全部 system：邻格、蔓延（PhaseSpread）、自带的 'layerSystems'（都读自类型级的 'layerCover'）。
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

-- | 本体的邻格 system：目标 = 真消除格邻格里本列认得的格（'Reach' 决定跳不跳过直接命中格），逐格问反应
-- （反应拿到本格状态与 'NearCtx'：触发格颜色、自己的位置、本阶段世界、当前盘面）。
nearBy :: Column s -> Reach -> DieOrder -> (NearCtx -> s -> NearOut) -> System NearWorld
nearBy col reach order react = System $ \ctx ->
  let b0 = nwBoard ctx
  in emit ctx (foldl (one ctx b0) (Out b0 [] []) (neighbourTargets reach (isJust . colGet col) ctx b0))
  where
    one ctx b0 out@(Out b dead sit) q = case colGet col (getCell b q) of
      Nothing -> out
      Just s ->
        let nctx = NearCtx (triggerColors ctx b0 q) q ctx b
         in case react nctx s of
              NearIdle -> out
              NearNudge n -> nudge order out q n
              NearEdit b' d s' -> Out b' (dead ++ [x | x <- d, x `notElem` dead]) (nub (s' ++ sit))

-- | 多层障碍（状态 = 层数）的邻格 system：邻格真消除削一层，末层打碎（并入清除格）；跳过直接命中格。
chipNear :: Column Int -> System NearWorld
chipNear col = nearBy col SkipDirect DiePrepend $ \_ n ->
  NearNudge (if n <= 1 then Dies else Becomes (colPut col (n - 1)))

-- | 多层障碍的命中组件：直接命中削一层，末层消除。
chipHit :: Column Int -> Int -> OnHit
chipHit col n
  | n <= 1 = breakHit
  | otherwise = absorbHit (colPut col (n - 1))

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
entityDamage :: Column s -> Entity s -> System NearWorld
entityDamage col ent = System $ \ctx -> let b0 = nwBoard ctx in emit ctx (entityDamageOn col ent ctx b0)

entityDamageOn :: Column s -> Entity s -> NearWorld -> Board -> Out
entityDamageOn col ent ctx b0 = foldl one (Out b0 [] []) anchors
  where
    at' bd q = colGet col (getCell bd q)
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
               else Out (foldl (\bd (q, x) -> setCell bd q (colPut col (withHp ent hp' x))) b parts) dead sit
