-- | 传送带：环路径上格内容循环前移一格。移位后的连锁/收饼由 Game.Resolve + Board.Cascade.cascadeAfterBelt 处理。
--
-- 单一实现（第三刀）：先由 beltMoves 把全部皮带按顺序合成为「原格 → 新格」的置换，
-- 真正移位 shiftBelts 与回放描述 EndBeltShift 都用 applyBeltMoves 执行同一份置换
-- （第二刀之前 beltMoves 与 shiftBelts 是两个独立写法，只靠重放护栏对齐）。
module Match3.Conveyor
  ( Belt
  , beltMoves
  , applyBeltMoves
  , shiftBelt
  , shiftBelts
  ) where

import Data.List (nub)
import Match3.Types

-- | Cyclic belt path (length >= 2). Cells advance toward higher indices;
-- the last cell wraps to the first.
type Belt = [Pos]

-- | 多条皮带依次前移一格后，每个内容变了的格子的 (原格, 新格)。后面的皮带看到的是前面移位后的盘面；
-- 同一条皮带里重复出现的格子以最后一次写入为准（与逐格写回的顺序一致）。
beltMoves :: [Belt] -> [(Pos, Pos)]
beltMoves belts =
  let cells = nub (concat belts)
      origin0 = [(p, p) | p <- cells]
      shiftOne orig ps
        | length ps < 2 = orig
        | otherwise =
            -- 新[i] = 旧[i-1]，新[0] = 旧[last]
            let prevOf = reverse (zip ps (last ps : init ps))
            in [ (p, maybe o (\q -> maybe q id (lookup q orig)) (lookup p prevOf))
               | (p, o) <- orig
               ]
      final = foldl shiftOne origin0 belts
  in [(o, d) | (d, o) <- final, o /= d]

-- | 按 (原格, 新格) 置换盘面：所有读取都来自移位前的盘面。
applyBeltMoves :: Board -> [(Pos, Pos)] -> Board
applyBeltMoves b0 = foldl (\b (o, d) -> setAt b d (at b0 o)) b0
  where
    at = boardAt
    setAt = boardSet

-- | Rotate cells one step along the belt (forward).
shiftBelt :: Board -> Belt -> Board
shiftBelt b p = shiftBelts b [p]

-- | Apply every belt in order (later belts see earlier shifts).
shiftBelts :: Board -> [Belt] -> Board
shiftBelts b belts = applyBeltMoves b (beltMoves belts)
