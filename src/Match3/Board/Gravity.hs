{-# LANGUAGE ScopedTypeVariables #-}

-- | 沉降与补子：重力（固定格不动）、底行饼干收集、传送门传送（settleBoardPortals 循环至稳定）、
-- 随机补子（refill）以及回放用的 settleRefill。
--
-- 依赖：Grid（randomColor）、元素注册表（固定格 edFalls、传送门 edPortal、边缘收集 edDrains（方向可配））。
-- 段 2c 起本模块不依赖内置注册表，全部函数收 Registry；不带 With 的旧名在 Match3.Board.Default。
-- 不变量：refill 按行优先顺序逐个空洞消耗随机数；settleRefill 与 stepCascadeDetailed /
-- 种子清除内部用的 settle + refill 完全相同，回放与结算的随机数顺序因此一致。
module Match3.Board.Gravity
  ( gravityFixedCellWith
  , colGravityWith
  , applyGravityWith
  , drainBottomCookiesWith
  , drainEdgesMWith
  , applyPortalTeleportsWith
  , portalTeleport
  , settleBoardPortalsWith
  , settleDrainWith
  , refill
  , settleRefillWith
  ) where

import Data.List (nub, nubBy)
import Match3.Element.Registry (Registry, drainEdgesWith, fallsWith, teleportWith)
import Match3.Element.Types (Edge(..))
import Match3.Types
import System.Random (RandomGen)
import Match3.Board.Grid

-- | 固定格（指定注册表）：本体 edFalls = False。
gravityFixedCellWith :: Registry -> Cell -> Bool
gravityFixedCellWith reg = not . fallsWith reg

-- | colGravity（指定注册表）。
colGravityWith :: Registry -> [Maybe Cell] -> [Maybe Cell]
colGravityWith reg = concatMap packSegment . splitFixed
  where
    isFixed (Just c) = gravityFixedCellWith reg c
    isFixed Nothing = False
    splitFixed [] = []
    splitFixed xs =
      let (seg, rest) = span (not . isFixed) xs
      in case rest of
           (fix : ys) -> seg : [fix] : splitFixed ys
           [] -> [seg]
    packSegment seg =
      let solids = [x | Just x <- seg]
          holes = length seg - length solids
      in replicate holes Nothing ++ map Just solids

-- | applyGravity（指定注册表）。
applyGravityWith :: Registry -> MBoard -> MBoard
applyGravityWith reg mb = transposeM (map (colGravityWith reg) (transposeM mb))

-- | drainBottomCookies（指定注册表）：边缘收集的旧形状（收走个数 + 位置）。
drainBottomCookiesWith :: Registry -> MBoard -> (MBoard, Int, [Pos])
drainBottomCookiesWith reg mb =
  let (mb', drained) = drainEdgesMWith reg mb
  in (mb', length drained, map fst drained)

-- | 边缘收集（段 2c，方向可配）：本体 edDrains 含某条边、且正位于这条边上的格被收走，
-- 然后重力，重复到没有可收的格。返回（盘面，按收集顺序的 (位置, 原格)）。
-- 每一轮按 底 → 左 → 右 → 上 扫四条边（边内按列 / 行升序，角格只收一次）；
-- 内置只有饼干 = [EdgeBottom]，因此与旧的「底行收饼干、重力、再收」逐位相同。
drainEdgesMWith :: Registry -> MBoard -> (MBoard, [(Pos, Cell)])
drainEdgesMWith reg mb =
  let n = boardSize - 1
      edgeCells e = case e of
        EdgeBottom -> [(n, c) | c <- [0 .. n]]
        EdgeLeft -> [(r, 0) | r <- [0 .. n]]
        EdgeRight -> [(r, n) | r <- [0 .. n]]
        EdgeTop -> [(0, c) | c <- [0 .. n]]
      atM (r, c) = (mb !! r) !! c
      hits0 =
        [ (p, cell)
        | e <- [EdgeBottom, EdgeLeft, EdgeRight, EdgeTop]
        , p <- edgeCells e
        , Just cell <- [atM p]
        , e `elem` drainEdgesWith reg cell
        ]
      hits = nubBy (\x y -> fst x == fst y) hits0
  in if null hits
       then (mb, [])
       else
         let mb1 = foldl (\m (p, _) -> setM m p Nothing) mb hits
             (mb2, more) = drainEdgesMWith reg (applyGravityWith reg mb1)
         in (mb2, hits ++ more)

-- | applyPortalTeleports（指定注册表）：段 4 起传送门是关卡级元素，经注册表的 HookTeleport 取实现
-- （内置 = portalTeleport；本体 edPortal 的格可传送）；未注册时不传送。
applyPortalTeleportsWith :: Registry -> [(Pos, Pos)] -> MBoard -> MBoard
applyPortalTeleportsWith = teleportWith

-- | 传送门的实现（内置 LevelDef "portal" 的钩子）：可穿门谓词由注册表给出。
portalTeleport :: (Cell -> Bool) -> [(Pos, Pos)] -> MBoard -> MBoard
portalTeleport canPort portals mb =
  -- Each pair teleports at most one way per settle (A→B else B→A) to avoid bounce-back.
  foldl tryPair mb (nub portals)
  where
    atM m (r, c) = (m !! r) !! c
    transferable (Just cell) = canPort cell
    transferable Nothing = False
    tryPair m (a, b) =
      case (atM m a, atM m b) of
        (ca, Nothing)
          | transferable ca -> setM (setM m a Nothing) b ca
        (Nothing, cb)
          | transferable cb -> setM (setM m b Nothing) a cb
        _ -> m

-- | settleBoardPortals（指定注册表）。
settleBoardPortalsWith :: Registry -> [(Pos, Pos)] -> MBoard -> (MBoard, Int, [Pos])
settleBoardPortalsWith reg portals mb =
  let (mb', drained) = settleDrainWith reg portals mb
  in (mb', length drained, map fst drained)

-- | 沉降（段 2c）：同 settleBoardPortalsWith，但返回被边缘收走的原格（连锁按各自的 edCounter 计数）。
settleDrainWith :: Registry -> [(Pos, Pos)] -> MBoard -> (MBoard, [(Pos, Cell)])
settleDrainWith reg portals mb =
  let fallen = applyGravityWith reg mb
      (drained1, d1) = drainEdgesMWith reg fallen
      ported = applyPortalTeleportsWith reg portals drained1
      fallen2 = if ported == drained1 then ported else applyGravityWith reg ported
      (drained2, d2) = drainEdgesMWith reg fallen2
  in (drained2, d1 ++ d2)

-- | 按行优先顺序把每个空洞补成随机普通宝石；每个洞消耗一次 randomColor。
refill :: RandomGen g => g -> MBoard -> (Board, g)
refill g0 mb =
  let (filled, g') = fillList g0 (concat mb)
  in (boardFromRows (chunk boardSize (map (maybe (error "refill: hole") id) filled)), g')
  where
    fillList g [] = ([], g)
    fillList g (Nothing : xs) =
      let (c, g1) = randomColor g
          (rest, g2) = fillList g1 xs
      in (Just (mkGem c) : rest, g2)
    fillList g (Just x : xs) =
      let (rest, g1) = fillList g xs
      in (Just x : rest, g1)

-- | settleRefill（指定注册表）。
settleRefillWith :: RandomGen g => Registry -> [(Pos, Pos)] -> g -> MBoard -> (Board, [Pos], g)
settleRefillWith reg portals g mb =
  let (settled, _cookies, cookSites) = settleBoardPortalsWith reg portals mb
      (b', g') = refill g settled
  in (b', cookSites, g')
