{-# LANGUAGE RankNTypes #-}

-- | 占格障碍的整盘邻消触发：气球/彩蛋/染色瓶/魔法帽/果汁机（走元素的逃生口 boardPasses）。
-- 一般不可匹配、挡交换；邻消触发效果。不负责连锁循环本身。
--
-- 石头 / 宝箱 / 蜂蜜 / 蛋糕 / 保险箱 / 时间精灵的邻消削层自元素类重构第 3 刀起是元素方法 onNeighbourClear +
-- 通用驱动（Match3.Element.Rules.kindNeighbour），不在这里。「相邻的某种格」都是 'adjacentWhere'，
-- 魔法帽 / 染色瓶的改色用遍历 'cellColorT'（docs/haskell-features/09-规则去重.md）。结果的列表顺序由金标准与
-- Spec.RulesDedup 的固定例子（dedup_obstacle_orders_pinned）锁定。
--
-- except 参数 = 本轮已被直接命中、不再邻消的格。只导出元素定义（Match3.Element.Builtin.*）要用的版本；
-- 测试用的无 except 写法（except = []）在 test/Spec/Support/Obstacles.hs。
module Match3.Obstacles
  ( orthoNeighbors
  , adjacentWhere
    -- * 气球 / 彩蛋
  , chipAdjacentBalloonsExcept
  , openSurprises
    -- * 魔法帽 / 染色瓶 / 果汁机
  , hatsAdjacentTo
  , triggerAdjacentHatsExcept
  , triggerAdjacentHatsBy
  , triggerAdjacentBottlesExcept
  , triggerAdjacentBottlesBy
  , makersAdjacentSameColor
  , chargeAdjacentMakersSit
  ) where

import Data.List (nub, sort)
import Engine.Optics (has, (%~), (&), (.~), (^?))
import Match3.Board.Grid (inBounds)
import Match3.Types
  ( Board
  , boardAt
  , boardSet
  , Cell
  , Color(..)
  , Pos
  , neighborsIn
  , upDownLeftRight
  , isMagicHat
  , isSurprise
  , isBottle
  , CellContents(..)
  , balloonColor
  , makerColor
  , mkMakerCharges
  , GemKind(..)
  , cellColor
  , colorAt
  , isGem
  )
import Match3.Types.Optics (cellAt, cellColorT)

at :: Board -> Pos -> Cell
at = boardAt

setAt :: Board -> Pos -> Cell -> Board
setAt = boardSet

-- | 上 / 下 / 左 / 右的邻格（不查边界）。顺序决定 adjacentWhere 去重后的先后，金标准依赖它（由 'upDownLeftRight' 写明）。
orthoNeighbors :: Pos -> [Pos]
orthoNeighbors = neighborsIn upDownLeftRight


-- | 与给出位置正交相邻、满足谓词的格（去重；顺序 = 按给出位置、每个位置上 / 下 / 左 / 右）。
-- 石头 / 宝箱 / 蜂蜜 / 蛋糕 / 保险箱 / 魔法帽 / 彩蛋 / 染色瓶 / 时间精灵的「相邻的某种格」都是它，只差谓词。
adjacentWhere :: (Cell -> Bool) -> Board -> [Pos] -> [Pos]
adjacentWhere ok b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , ok (at b p)
    ]

-- | Magic hat positions orthogonally adjacent to cleared gems.
hatsAdjacentTo :: Board -> [Pos] -> [Pos]
hatsAdjacentTo = adjacentWhere isMagicHat

-- | Balloon positions orthogonally adjacent to a same-color cleared gem.
balloonsAdjacentSameColor :: Board -> [Pos] -> [Pos]
balloonsAdjacentSameColor b cleared =
  nub
    [ p
    | cpos <- cleared
    , let clearedCell = at b cpos
    , isGem clearedCell
    , Just col <- [cellColor clearedCell]
    , p <- orthoNeighbors cpos
    , inBounds b p
    , balloonColor (at b p) == Just col
    ]

-- | Pop balloons adjacent to same-color clears (single hit; no layers), skipping cells in 'except'.
chipAdjacentBalloonsExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentBalloonsExcept b clearedGems except =
  let dead = [p | p <- balloonsAdjacentSameColor b clearedGems, p `notElem` except]
  in (b, dead)  -- board unchanged until clear pipeline removes them

cycleColor :: Color -> Color
cycleColor c = colorAt (fromEnum c + 1)

-- | Trigger magic hats adjacent to clears: swap colors of two ortho gem neighbors
-- (deterministic: sorted positions). If only one gem neighbor, cycle its color.
-- Hat itself stays. Neighbors in the cleared set and in `protected` are skipped.
-- Surprise-opened specials (saved same wave) must sit unchanged — Hat must not
-- swap/cycle their color before the next move (parity with maker_bomb_survives_wave).
triggerAdjacentHatsExcept :: Board -> [Pos] -> [Pos] -> Board
triggerAdjacentHatsExcept = triggerAdjacentHatsBy isGem

-- | 可改色谓词由调用方给出（元素框架里 = 元素世界的 recolorable；内置等于 isGem）。
triggerAdjacentHatsBy :: (Cell -> Bool) -> Board -> [Pos] -> [Pos] -> Board
triggerAdjacentHatsBy recolorable b cleared protected =
  foldl triggerOne b (hatsAdjacentTo b cleared)
  where
    skip = nub (cleared ++ protected)
    triggerOne board hatPos =
      let nbrs =
            [ p
            | p <- orthoNeighbors hatPos
            , inBounds b p
            , p `notElem` skip
            , let cell = at board p
            , recolorable cell
            ]
          sorted = nub (sort nbrs)
      -- 读颜色与改颜色是同一个遍历 cellColorT
      in case sorted of
           (p1 : p2 : _)
             | Just c1 <- board ^? cellAt p1 . cellColorT
             , Just c2 <- board ^? cellAt p2 . cellColorT ->
                 board & cellAt p1 . cellColorT .~ c2 & cellAt p2 . cellColorT .~ c1
           [p1]
             | has (cellAt p1 . cellColorT) board ->
                 board & cellAt p1 . cellColorT %~ cycleColor
           _ -> board

-- | Maker positions orthogonally adjacent to a same-color cleared gem.
makersAdjacentSameColor :: Board -> [Pos] -> [Pos]
makersAdjacentSameColor b cleared =
  nub
    [ p
    | cpos <- cleared
    , let clearedCell = at b cpos
    , isGem clearedCell
    , Just col <- [cellColor clearedCell]
    , p <- orthoNeighbors cpos
    , inBounds b p
    , makerColor (at b p) == Just col
    ]

-- | Charge juice makers adjacent to same-color clears.
-- Charge 1 -> produce Bomb of maker color in place; n>1 -> decrement (makers never enter the clear-hole set).
-- Also returns positions converted to Bomb this wave.
-- Those Bombs must sit through same-wave Bottle dye (Surprise special parity;
-- Hat runs before Maker so only Bottle can rewrite a freshly produced Bomb).
chargeAdjacentMakersSit :: Board -> [Pos] -> (Board, [Pos])
chargeAdjacentMakersSit b clearedGems =
  foldl chargeOne (b, []) (makersAdjacentSameColor b clearedGems)
  where
    chargeOne (board, saved) p =
      case at board p of
        Maker col n
          | n <= 1 ->
              (setAt board p (Gem col Bomb 0 Nothing), nub (p : saved))
          | otherwise ->
              (setAt board p (mkMakerCharges col (n - 1)), saved)
        _ -> (board, saved)

-- | Surprise box positions orthogonally adjacent to cleared positions.
surprisesAdjacentTo :: Board -> [Pos] -> [Pos]
surprisesAdjacentTo = adjacentWhere isSurprise

-- | Deterministic surprise outcome from board position.
-- 0..2 → become LineH / LineV / Bomb; 3 → 3×3 explosion (box cleared).
surpriseOutcome :: Pos -> Int
surpriseOutcome (r, c) = (r * 8 + c) `mod` 4

surpriseSpecial :: Pos -> Cell
surpriseSpecial (r, c) =
  let col = colorAt (r + 3 * c)
      kind = case surpriseOutcome (r, c) of
        0 -> LineH
        1 -> LineV
        _ -> Bomb
  in Gem col kind 0 Nothing

-- | 3×3 blast centered at pos (same footprint as countdown / bomb).
surpriseBlast :: Board -> Pos -> [Pos]
surpriseBlast b (r, c) =
  [ (rr, cc)
  | rr <- [r - 1 .. r + 1]
  , cc <- [c - 1 .. c + 1]
  , inBounds b (rr, cc)
  ]

-- | Open surprises adjacent to clears *or* sitting on a clear seed
-- (hammer / cross / line / bomb direct hit). Special outcomes replace the box
-- in place; those positions are returned so callers can keep them out of clear
-- holes (otherwise a direct-hit special was spawned then immediately dug away).
-- Explosion outcomes contribute 3×3 clear seeds (box cleared via those seeds).
-- Returns (board, explosion seeds, special-placed positions).
openSurprises :: Board -> [Pos] -> (Board, [Pos], [Pos])
openSurprises b clearedGems =
  foldl openOne (b, [], []) targets
  where
    direct =
      [ p
      | p <- nub clearedGems
      , isSurprise (at b p)
      ]
    targets = nub (surprisesAdjacentTo b clearedGems ++ direct)
    openOne (board, explodes, saved) p =
      case at board p of
        Surprise
          | surpriseOutcome p == 3 ->
              (board, nub (surpriseBlast board p ++ explodes), saved)
          | otherwise ->
              (setAt board p (surpriseSpecial p), explodes, nub (p : saved))
        _ -> (board, explodes, saved)

-- | Dye bottle positions orthogonally adjacent to cleared gems.
bottlesAdjacentTo :: Board -> [Pos] -> [Pos]
bottlesAdjacentTo = adjacentWhere isBottle

-- | Trigger dye bottles: recolor every ortho gem neighbor to the bottle color.
-- Bottle itself stays. Neighbors in the cleared set and in `protected` are skipped.
-- Surprise-opened specials and Maker-produced Bombs sit same-wave; Bottle
-- must not recolor them (parity with maker_bomb_survives_wave / Surprise sit).
triggerAdjacentBottlesExcept :: Board -> [Pos] -> [Pos] -> Board
triggerAdjacentBottlesExcept = triggerAdjacentBottlesBy isGem

-- | 可改色谓词由调用方给出（同 triggerAdjacentHatsBy）。
triggerAdjacentBottlesBy :: (Cell -> Bool) -> Board -> [Pos] -> [Pos] -> Board
triggerAdjacentBottlesBy recolorable b cleared protected =
  foldl dyeOne b (bottlesAdjacentTo b cleared)
  where
    skip = nub (cleared ++ protected)
    dyeOne board bottlePos =
      case at board bottlePos of
        Bottle col ->
          let nbrs =
                [ p
                | p <- orthoNeighbors bottlePos
                , inBounds b p
                , p `notElem` skip
                , let cell = at board p
                , recolorable cell
                ]
          in foldl (\bd p -> bd & cellAt p . cellColorT .~ col) board (nub nbrs)
        _ -> board

