{-# LANGUAGE ScopedTypeVariables #-}

-- | 沉降与补子：重力（固定格不动）、底行饼干收集、沉降节拍的关卡级钩子（onSettle，内置 = 传送门传送）、
-- 补子（按补子策略 RefillPolicy，见 Match3.Board.Refill；refill = 缺省策略）以及回放用的 settleRefillWith。
--
-- 依赖：Grid、Board.Refill（补子策略）、Board.Hooks（关卡级钩子）、元素元素世界（固定格 falls、边缘收集 drains（方向可配））。
-- 本模块不依赖内置元素世界，全部函数收 Registry；内置元素世界的短名在 Match3.Board.Default。
-- 不变量：补子按行优先顺序逐个空洞消耗随机数（缺省策略每洞恰好一次 randomColor）；settleRefillWith 与 stepCascadeDetailed /
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
  , activeRefill
  , settleRefillWith
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array (bounds, elems, (!))
import Data.Array.ST (STArray, newListArray, readArray, runSTArray, writeArray)
import Data.List (nubBy)
import Match3.Board.Hooks (LevelHooks(..))
import Data.Maybe (fromMaybe)
import Match3.Board.Refill (RefillPolicy, defaultRefill, refillWith)
import Match3.ECS.Registry (Registry, drainEdgesWith, fallsWith, refillPolicyWith)
import Match3.Element.Types (Edge(..))
import Match3.Types
import System.Random (RandomGen)
import Match3.Board.Grid

-- | 固定格（指定元素世界）：本体 falls = False。
gravityFixedCellWith :: Registry -> Cell -> Bool
gravityFixedCellWith world = not . fallsWith world

-- | 一列的重力（自上而下的列表）：固定格（本体不 falls）原地不动并把列切成段，段内实格保持次序落到段底、空洞在段顶。
colGravityWith :: Registry -> [Maybe Cell] -> [Maybe Cell]
colGravityWith world = concatMap packSegment . splitFixed
  where
    isFixed (Just c) = gravityFixedCellWith world c
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

-- | applyGravity（指定元素世界）：每列与 colGravityWith 相同（固定格不动、把列切成段，段内实格保持次序落到底、空洞在上）。
--
-- 性能（Haskell 特性第 5 项）：在 runSTArray 里复制一份盘面，逐列逐段「双指针」就地压实——读指针自下而上扫，
-- 遇到实格就写到写指针处，最后把段顶剩下的格写成空洞，不为每列建列表、切段、拼接。
-- 对外仍是纯函数（ST 的可变数组出不了 runSTArray）。微基准约 2.5 倍，但重力只占规则总耗时的百分之二三，
-- 整体收益很小（文档 §4 如实给数）。
applyGravityWith :: Registry -> MBoard -> MBoard
applyGravityWith world mb = runSTArray $ do
  let bnds@((r0, c0), (r1, c1)) = bounds mb
  m <- newListArray bnds (elems mb)
  forM_ [c0 .. c1] $ \c -> do
    let fixedAt r = maybe False (gravityFixedCellWith world) (mb ! (r, c))
        -- 固定格把列 [r0 .. r1] 切成若干段（段内没有固定格）
        segments lo [] = [(lo, r1)]
        segments lo (r : rs)
          | fixedAt r = (lo, r - 1) : segments (r + 1) rs
          | otherwise = segments lo rs
    forM_ (segments r0 [r0 .. r1]) (\(lo, hi) -> when (lo <= hi) (compactSegment m c lo hi))
  pure m

-- | 一段 [lo .. hi]（第 c 列，段内没有固定格）：读指针 r 自下而上，实格写到写指针 w，最后 [lo .. w] 是空洞。
compactSegment :: forall s. STArray s Pos (Maybe Cell) -> Int -> Int -> Int -> ST s ()
compactSegment m c lo hi = go hi hi
  where
    go :: Int -> Int -> ST s ()
    go r w
      | r < lo = forM_ [lo .. w] (\k -> writeArray m (k, c) Nothing)
      | otherwise = do
          v <- readArray m (r, c)
          case v of
            Just _ -> writeArray m (w, c) v >> go (r - 1) (w - 1)
            Nothing -> go (r - 1) w

-- | 边缘收集（'drainEdgesMWith'）的计数形状：(盘面, 收走个数, 收走位置)。收走位置按收集顺序，
-- 结算把它们并入清除格（GoalCarpet 的覆盖、前端粒子）。
drainBottomCookiesWith :: Registry -> MBoard -> (MBoard, Int, [Pos])
drainBottomCookiesWith world mb =
  let (mb', drained) = drainEdgesMWith world mb
  in (mb', length drained, map fst drained)

-- | 边缘收集（方向可配）：本体 drains 含某条边、且正位于这条边上的格被收走，
-- 然后重力，重复到没有可收的格。返回（盘面，按收集顺序的 (位置, 原格)）。
-- 每一轮按 底 → 左 → 右 → 上 扫四条边（边内按列 / 行升序，角格只收一次）；
-- 内置只有饼干 = [EdgeBottom]，即「底行收饼干、重力、再收」。
drainEdgesMWith :: Registry -> MBoard -> (MBoard, [(Pos, Cell)])
drainEdgesMWith world mb =
  let ((r0, c0), (r1, c1)) = bounds mb
      edgeCells e = case e of
        EdgeBottom -> [(r1, c) | c <- [c0 .. c1]]
        EdgeLeft -> [(r, c0) | r <- [r0 .. r1]]
        EdgeRight -> [(r, c1) | r <- [r0 .. r1]]
        EdgeTop -> [(r0, c) | c <- [c0 .. c1]]
      hits0 =
        [ (p, cell)
        | e <- [EdgeBottom, EdgeLeft, EdgeRight, EdgeTop]
        , p <- edgeCells e
        , Just cell <- [atM mb p]
        , e `elem` drainEdgesWith world cell
        ]
      hits = nubBy (\x y -> fst x == fst y) hits0
  in if null hits
       then (mb, [])
       else
         let mb1 = setManyM mb [(p, Nothing) | (p, _) <- hits]
             (mb2, more) = drainEdgesMWith world (applyGravityWith world mb1)
         in (mb2, hits ++ more)

-- | settleBoardPortals（指定元素世界；传送经钩子 onSettle）。
settleBoardPortalsWith :: Registry -> LevelHooks -> MBoard -> (MBoard, Int, [Pos])
settleBoardPortalsWith world hooks mb =
  let (mb', drained) = settleDrainWith world hooks mb
  in (mb', length drained, map fst drained)

-- | 沉降：同 settleBoardPortalsWith，但返回被边缘收走的原格（连锁按各自的 counter 计数）。
settleDrainWith :: Registry -> LevelHooks -> MBoard -> (MBoard, [(Pos, Cell)])
settleDrainWith world hooks mb =
  let fallen = applyGravityWith world mb
      (drained1, d1) = drainEdgesMWith world fallen
      ported = onSettle hooks drained1
      fallen2 = if ported == drained1 then ported else applyGravityWith world ported
      (drained2, d2) = drainEdgesMWith world fallen2
  in (drained2, d1 ++ d2)

-- | 按行优先顺序把每个空洞补成随机普通宝石；每个洞消耗一次 randomColor（= 缺省补子策略，见 Match3.Board.Refill）。
refill :: RandomGen g => g -> MBoard -> (Board, g)
refill = refillWith defaultRefill

-- | 本轮用的补子策略：关卡级元素换的（钩子 hookRefill）优先，否则元素世界的（缺省 = 随机五色宝石）。
activeRefill :: Registry -> LevelHooks -> RefillPolicy
activeRefill world hooks = fromMaybe (refillPolicyWith world) (hookRefill hooks)

-- | 回放用的沉降 + 补子：settleBoardPortalsWith 后按本轮补子策略补满，返回 (终盘, 收饼干位, 生成器)。
settleRefillWith :: RandomGen g => Registry -> LevelHooks -> g -> MBoard -> (Board, [Pos], g)
settleRefillWith world hooks g mb =
  let (settled, _cookies, cookSites) = settleBoardPortalsWith world hooks mb
      (b', g') = refillWith (activeRefill world hooks) g settled
  in (b', cookSites, g')
