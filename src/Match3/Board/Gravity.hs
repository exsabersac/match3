{-# LANGUAGE ScopedTypeVariables #-}

-- | 沉降与补子：重力（固定格不动）、底行饼干收集、传送门传送（settleBoardPortals 循环至稳定）、
-- 随机补子（refill）以及回放用的 settleRefill。
--
-- 依赖：Grid（randomColor）。
-- 不变量：refill 按行优先顺序逐个空洞消耗随机数；settleRefill 与 stepCascadeDetailed /
-- 种子清除内部用的 settle + refill 完全相同，回放与结算的随机数顺序因此一致。
module Match3.Board.Gravity
  ( gravityFixedCell
  , colGravity
  , applyGravity
  , drainBottomCookies
  , applyPortalTeleports
  , settleBoardPortals
  , refill
  , settleRefill
  ) where

import Data.List (nub)
import Match3.Types
import System.Random (RandomGen)
import Match3.Board.Grid

-- | Immortal décor that must not fall with gravity. Same class as portal/belt
-- stuck blockers (Bottle / Maker / MagicHat / Snail): they never clear and are
-- not portal-transferable, so gravity-packing them into a portal/belt slot (or
-- off a décor seed via Cross column wipe) permanently soft-locks the layout.
gravityFixedCell :: Cell -> Bool
gravityFixedCell c =
  isBottle c || isMaker c || isMagicHat c || isSnail c

-- | Column gravity: fixed immortals stay put and split the column into segments;
-- fallable cells (gems / Cookie / Countdown / Flip / clearable obstacles) pack
-- down within each segment.
colGravity :: [Maybe Cell] -> [Maybe Cell]
colGravity = concatMap packSegment . splitFixed
  where
    isFixed (Just c) = gravityFixedCell c
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

-- | 整盘按列下落：固定格（染色瓶 / 果汁机 / 魔法帽 / 蜗牛）原地不动并把列分段，段内可下落的格压到段底。
applyGravity :: MBoard -> MBoard
applyGravity mb = transposeM (map colGravity (transposeM mb))

-- | Collect cookies that sit on the bottom row after gravity (开心消消乐饼干掉落收集).
-- Removes them, re-applies gravity, repeats until no bottom-row cookies remain.
-- Returns drained bottom positions so GoalCarpet can cover tiles Cookie only
-- occupied mid-settle (gravity/portal/belt → bottom → drain) — before/after
-- board compare cannot see that intermediate occupancy.
drainBottomCookies :: MBoard -> (MBoard, Int, [Pos])
drainBottomCookies mb =
  let bottom = boardSize - 1
      cols =
        [ c
        | c <- [0 .. boardSize - 1]
        , case (mb !! bottom) !! c of
            Just Cookie -> True
            _ -> False
        ]
      sites = [(bottom, c) | c <- cols]
      n = length cols
  in if n == 0
       then (mb, 0, [])
       else
         let mb1 =
               foldl
                 (\m c -> setM m (bottom, c) Nothing)
                 mb
                 cols
             fallen = applyGravity mb1
             (mb2, n2, sites2) = drainBottomCookies fallen
         in (mb2, n + n2, sites ++ sites2)

-- | Bidirectional portal teleport on MBoard: gem/cookie/countdown on A with hole at B
-- moves A -> B (and reverse). Used after gravity + bottom-cookie drain so clears can
-- open exits without snatching cookies that already touched the bottom row.
applyPortalTeleports :: [(Pos, Pos)] -> MBoard -> MBoard
applyPortalTeleports portals mb =
  -- Each pair teleports at most one way per settle (A→B else B→A) to avoid bounce-back.
  foldl tryPair mb (nub portals)
  where
    atM m (r, c) = (m !! r) !! c
    transferable (Just (Gem _ _ _ _)) = True
    transferable (Just (Countdown _ _)) = True
    transferable (Just Cookie) = True
    transferable (Just (Flip _ _)) = True
    transferable _ = False
    tryPair m (a, b) =
      case (atM m a, atM m b) of
        (ca, Nothing)
          | transferable ca -> setM (setM m a Nothing) b ca
        (Nothing, cb)
          | transferable cb -> setM (setM m b Nothing) a cb
        _ -> m

-- | Gravity, drain bottom cookies, portal teleports (optional), gravity, drain again.
-- Cookies that reach the bottom must collect before a portal can snatch them
-- (触底优先于传送门); cookies that teleport onto a bottom exit still drain after.
-- Third component: bottom cells cookies drained from (Carpet / particle seeds).
settleBoardPortals :: [(Pos, Pos)] -> MBoard -> (MBoard, Int, [Pos])
settleBoardPortals portals mb =
  let fallen = applyGravity mb
      (drained1, n1, sites1) = drainBottomCookies fallen
      ported = applyPortalTeleports portals drained1
      fallen2 = if ported == drained1 then ported else applyGravity ported
      (drained2, n2, sites2) = drainBottomCookies fallen2
  in (drained2, n1 + n2, sites1 ++ sites2)

-- | 按行优先顺序把每个空洞补成随机普通宝石；每个洞消耗一次 randomColor。
refill :: RandomGen g => g -> MBoard -> (Board, g)
refill g0 mb =
  let (filled, g') = fillList g0 (concat mb)
  in (chunk boardSize (map (maybe (error "refill: hole") id) filled), g')
  where
    fillList g [] = ([], g)
    fillList g (Nothing : xs) =
      let (c, g1) = randomColor g
          (rest, g2) = fillList g1 xs
      in (Just (mkGem c) : rest, g2)
    fillList g (Just x : xs) =
      let (rest, g1) = fillList g xs
      in (Just x : rest, g1)

-- | 一轮沉降：settle + refill（与 stepCascadeDetailed / 种子清除用的完全相同）。
settleRefill :: RandomGen g => [(Pos, Pos)] -> g -> MBoard -> (Board, [Pos], g)
settleRefill portals g mb =
  let (settled, _cookies, cookSites) = settleBoardPortals portals mb
      (b', g') = refill g settled
  in (b', cookSites, g')
