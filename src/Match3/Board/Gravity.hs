{-# LANGUAGE ScopedTypeVariables #-}

-- | 沉降与补子：重力（固定格不动）、底行饼干收集、沉降节拍的关卡级钩子（onSettle，内置 = 传送门传送）、
-- 随机补子（refill）以及回放用的 settleRefill。
--
-- 依赖：Grid（randomColor）、Board.Hooks（第 7 刀：关卡级钩子取代传送门对参数）、元素注册表（固定格 falls、边缘收集 drains（方向可配））。
-- 段 2c 起本模块不依赖内置注册表，全部函数收 Registry；不带 With 的旧名在 Match3.Board.Default。
-- 不变量：refill 按行优先顺序逐个空洞消耗随机数；settleRefill 与 stepCascadeDetailed /
-- 种子清除内部用的 settle + refill 完全相同，回放与结算的随机数顺序因此一致。
module Match3.Board.Gravity
  ( gravityFixedCellWith
  , colGravityWith
  , applyGravityWith
  , drainBottomCookiesWith
  , drainEdgesMWith
  , settleBoardPortalsWith
  , settleDrainWith
  , refill
  , settleRefillWith
  ) where

import Data.Array (array, bounds, elems, listArray, (!))
import Data.List (nubBy)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Element.Registry (Registry, drainEdgesWith, fallsWith)
import Match3.Element.Types (Edge(..))
import Match3.Types
import System.Random (RandomGen)
import Match3.Board.Grid

-- | 固定格（指定注册表）：本体 falls = False。
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
-- 逐列取出（自上而下）、按列重力、写回；不再转置两次。
applyGravityWith :: Registry -> MBoard -> MBoard
applyGravityWith reg mb =
  let bnds@((r0, c0), (r1, c1)) = bounds mb
      rows = [r0 .. r1]
  in array bnds
       [ ((r, c), v)
       | c <- [c0 .. c1]
       , (r, v) <- zip rows (colGravityWith reg [mb ! (r', c) | r' <- rows])
       ]

-- | drainBottomCookies（指定注册表）：边缘收集的旧形状（收走个数 + 位置）。
drainBottomCookiesWith :: Registry -> MBoard -> (MBoard, Int, [Pos])
drainBottomCookiesWith reg mb =
  let (mb', drained) = drainEdgesMWith reg mb
  in (mb', length drained, map fst drained)

-- | 边缘收集（段 2c，方向可配）：本体 drains 含某条边、且正位于这条边上的格被收走，
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
      hits0 =
        [ (p, cell)
        | e <- [EdgeBottom, EdgeLeft, EdgeRight, EdgeTop]
        , p <- edgeCells e
        , Just cell <- [atM mb p]
        , e `elem` drainEdgesWith reg cell
        ]
      hits = nubBy (\x y -> fst x == fst y) hits0
  in if null hits
       then (mb, [])
       else
         let mb1 = setManyM mb [(p, Nothing) | (p, _) <- hits]
             (mb2, more) = drainEdgesMWith reg (applyGravityWith reg mb1)
         in (mb2, hits ++ more)

-- | settleBoardPortals（指定注册表；传送经钩子 onSettle）。
settleBoardPortalsWith :: Registry -> LevelHooks -> MBoard -> (MBoard, Int, [Pos])
settleBoardPortalsWith reg hooks mb =
  let (mb', drained) = settleDrainWith reg hooks mb
  in (mb', length drained, map fst drained)

-- | 沉降（段 2c）：同 settleBoardPortalsWith，但返回被边缘收走的原格（连锁按各自的 counter 计数）。
settleDrainWith :: Registry -> LevelHooks -> MBoard -> (MBoard, [(Pos, Cell)])
settleDrainWith reg hooks mb =
  let fallen = applyGravityWith reg mb
      (drained1, d1) = drainEdgesMWith reg fallen
      ported = onSettle hooks drained1
      fallen2 = if ported == drained1 then ported else applyGravityWith reg ported
      (drained2, d2) = drainEdgesMWith reg fallen2
  in (drained2, d1 ++ d2)

-- | 按行优先顺序把每个空洞补成随机普通宝石；每个洞消耗一次 randomColor。
refill :: RandomGen g => g -> MBoard -> (Board, g)
refill g0 mb =
  let (filled, g') = fillList g0 (elems mb)
  in (boardFromArray (listArray (bounds mb) filled), g')
  where
    fillList g [] = ([], g)
    fillList g (Nothing : xs) =
      let (c, g1) = randomColor g
          (rest, g2) = fillList g1 xs
      in (mkGem c : rest, g2)
    fillList g (Just x : xs) =
      let (rest, g1) = fillList g xs
      in (x : rest, g1)

-- | settleRefill（指定注册表）。
settleRefillWith :: RandomGen g => Registry -> LevelHooks -> g -> MBoard -> (Board, [Pos], g)
settleRefillWith reg hooks g mb =
  let (settled, _cookies, cookSites) = settleBoardPortalsWith reg hooks mb
      (b', g') = refill g settled
  in (b', cookSites, g')
