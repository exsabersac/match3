{-# LANGUAGE RankNTypes #-}

-- | 占格障碍与邻消触发：石头/宝箱/蜂蜜/蛋糕/保险箱/气球/彩蛋/染色瓶/时间精灵/魔法帽/果汁机。
-- 一般不可匹配、挡交换；邻消削一层或触发效果。不负责连锁循环本身。
--
-- Haskell 特性第 9 项（docs/haskell-features/09-规则去重.md）：五种带层数障碍的邻消揭层合成一个
-- 'chipAdjacentLayeredExcept'（障碍种类 = 棱镜参数，保险箱末层变饼干 = 另一个参数），九份「相邻的某种格」合成
-- 'adjacentWhere'，魔法帽 / 染色瓶的改色用遍历 'cellColorT'。导出名、签名、结果（含列表顺序）与第 9 项前逐项相同，
-- 对照副本在 test/Spec/Support/LegacyObstacles.hs。
module Match3.Obstacles
  ( orthoNeighbors
  , adjacentWhere
  , chipAdjacentLayeredExcept
  , stonesAdjacentTo
  , chestsAdjacentTo
  , honeysAdjacentTo
  , cakesAdjacentTo
  , safesAdjacentTo
  , chipAdjacentStones
  , chipAdjacentStonesExcept
  , chipAdjacentChests
  , chipAdjacentChestsExcept
  , chipAdjacentHoney
  , chipAdjacentHoneyExcept
  , chipAdjacentCakes
  , chipAdjacentCakesExcept
  , chipAdjacentSafes
  , chipAdjacentSafesExcept
  , chipAdjacentBalloons
  , chipAdjacentBalloonsExcept
  , balloonsAdjacentSameColor
  , hatsAdjacentTo
  , triggerAdjacentHats
  , triggerAdjacentHatsExcept
  , triggerAdjacentHatsBy
  , makersAdjacentSameColor
  , chargeAdjacentMakers
  , chargeAdjacentMakersSit
  , surprisesAdjacentTo
  , openAdjacentSurprises
  , openSurprises
  , bottlesAdjacentTo
  , triggerAdjacentBottles
  , triggerAdjacentBottlesExcept
  , triggerAdjacentBottlesBy
  , spiritsAdjacentTo
  , chipAdjacentTimeSpirits
  , chipAdjacentTimeSpiritsExcept
  , withAdjacentStones
  ) where

import Data.List (nub, sort)
import Engine.Optics (Prism', has, (%~), (&), (.~), (^?))
import Match3.Board.Grid (inBounds)
import Match3.Types
  ( Board
  , boardAt
  , boardSet
  , Cell
  , Color(..)
  , Pos
  , isStone
  , isChest
  , isHoney
  , isCake
  , isMagicHat
  , isSafe
  , isSurprise
  , isBottle
  , isTimeSpirit
  , mkCookie
  , CellContents(..)
  , balloonColor
  , makerColor
  , mkMakerCharges
  , GemKind(..)
  , cellColor
  , colorAt
  , isGem
  )
import Match3.Types.Optics (cellAt, cellColorT, _Cake, _Chest, _Honey, _Safe, _Stone)

at :: Board -> Pos -> Cell
at = boardAt

setAt :: Board -> Pos -> Cell -> Board
setAt = boardSet

-- | Up / down / left / right neighbors (may be out of bounds).
orthoNeighbors :: Pos -> [Pos]
orthoNeighbors (r, c) =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]


-- | 与给出位置正交相邻、满足谓词的格（去重；顺序 = 按给出位置、每个位置上 / 下 / 左 / 右）。
-- 第 9 项前石头 / 宝箱 / 蜂蜜 / 蛋糕 / 保险箱 / 魔法帽 / 彩蛋 / 染色瓶 / 时间精灵各抄一份同样的列表推导，只差谓词。
adjacentWhere :: (Cell -> Bool) -> Board -> [Pos] -> [Pos]
adjacentWhere ok b cleared =
  nub
    [ p
    | cpos <- cleared
    , p <- orthoNeighbors cpos
    , inBounds b p
    , ok (at b p)
    ]

-- | Stone positions orthogonally adjacent to any of the given cleared positions.
stonesAdjacentTo :: Board -> [Pos] -> [Pos]
stonesAdjacentTo = adjacentWhere isStone

-- | Chest positions orthogonally adjacent to cleared positions.
chestsAdjacentTo :: Board -> [Pos] -> [Pos]
chestsAdjacentTo = adjacentWhere isChest

-- | Honey jar positions orthogonally adjacent to cleared positions.
honeysAdjacentTo :: Board -> [Pos] -> [Pos]
honeysAdjacentTo = adjacentWhere isHoney

-- | Cake positions orthogonally adjacent to cleared positions.
cakesAdjacentTo :: Board -> [Pos] -> [Pos]
cakesAdjacentTo = adjacentWhere isCake

-- | Safe / vault positions orthogonally adjacent to cleared positions.
safesAdjacentTo :: Board -> [Pos] -> [Pos]
safesAdjacentTo = adjacentWhere isSafe

-- | Magic hat positions orthogonally adjacent to cleared gems.
hatsAdjacentTo :: Board -> [Pos] -> [Pos]
hatsAdjacentTo = adjacentWhere isMagicHat

-- | 不跳过任何格的版本（except = []）。
noExcept :: (Board -> [Pos] -> [Pos] -> r) -> Board -> [Pos] -> r
noExcept f b clearedGems = f b clearedGems []

-- | 带层数占格障碍的邻消（Haskell 特性第 9 项：第 9 项前石头 / 宝箱 / 蜂蜜 / 蛋糕 / 保险箱五份逐字相同，只差构造器）：
-- 与 clearedGems 正交相邻、不在 except 里（本轮已被直接命中）的这种障碍各削一层。
--
-- * @layer@：哪种障碍（棱镜 '_Stone' / '_Chest' / … ，焦点是层数）；
-- * @onLast@：最后一层被削掉时这一格变成什么——Nothing = 原样留着，由清除管线随本轮清除格移走（石头等）；
--   @Just mkCookie@ = 原地变成饼干（保险箱开启，饼干不在这里移走）。
--
-- 返回（新盘面, 最后一层被削掉的位置）。次序与第 9 项前相同：按 'adjacentWhere' 的顺序 foldl，
-- 末层位置用 @nub (p : dead)@ 前插（即逆序），测试 qc_dedup_obstacles_same_as_legacy 逐项锁定。
chipAdjacentLayeredExcept :: Prism' Cell Int -> Maybe Cell -> Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentLayeredExcept layer onLast b clearedGems except =
  foldl hitOne (b, []) [p | p <- adjacentWhere (has layer) b clearedGems, p `notElem` except]
  where
    hitOne (board, dead) p =
      case at board p ^? layer of
        Just n
          | n <= 1 -> (maybe board (setAt board p) onLast, nub (p : dead))
          | otherwise -> (board & cellAt p . layer .~ n - 1, dead)
        Nothing -> (board, dead)

-- | Chip one layer off each adjacent stone.
-- Returns (board with surviving stones decremented, positions whose last layer was chipped).
chipAdjacentStones :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentStones = noExcept chipAdjacentStonesExcept

-- | Like chipAdjacentStones but skips cells in 'except' (already direct-hit this wave).
chipAdjacentStonesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentStonesExcept = chipAdjacentLayeredExcept _Stone Nothing

-- | Chip one layer off each adjacent treasure chest (宝箱).
chipAdjacentChests :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentChests = noExcept chipAdjacentChestsExcept

chipAdjacentChestsExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentChestsExcept = chipAdjacentLayeredExcept _Chest Nothing

-- | Chip one layer off each adjacent honey jar (蜂蜜罐).
chipAdjacentHoney :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentHoney = noExcept chipAdjacentHoneyExcept

chipAdjacentHoneyExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentHoneyExcept = chipAdjacentLayeredExcept _Honey Nothing

-- | Chip one layer off each adjacent cake (蛋糕). Cleared at 0.
chipAdjacentCakes :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentCakes = noExcept chipAdjacentCakesExcept

chipAdjacentCakesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentCakesExcept = chipAdjacentLayeredExcept _Cake Nothing

-- | Chip one layer off each adjacent safe (保险箱).
-- Last layer opens into a Cookie in place (collectible drop); cookie is NOT removed here.
-- Returns (board, positions that fully opened).
chipAdjacentSafes :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentSafes = noExcept chipAdjacentSafesExcept

chipAdjacentSafesExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentSafesExcept = chipAdjacentLayeredExcept _Safe (Just mkCookie)

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

-- | Pop balloons adjacent to same-color clears (single hit; no layers).
chipAdjacentBalloons :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentBalloons = noExcept chipAdjacentBalloonsExcept

-- | Like chipAdjacentBalloons but skips cells in 'except' (already direct-hit this wave).
chipAdjacentBalloonsExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentBalloonsExcept b clearedGems except =
  let dead = [p | p <- balloonsAdjacentSameColor b clearedGems, p `notElem` except]
  in (b, dead)  -- board unchanged until clear pipeline removes them

cycleColor :: Color -> Color
cycleColor c = colorAt (fromEnum c + 1)

-- | Trigger magic hats adjacent to clears: swap colors of two ortho gem neighbors
-- (deterministic: sorted positions). If only one gem neighbor, cycle its color.
-- Hat itself stays. Neighbors in the cleared set are skipped.
triggerAdjacentHats :: Board -> [Pos] -> Board
triggerAdjacentHats = noExcept triggerAdjacentHatsExcept

-- | Like triggerAdjacentHats, but also skip recoloring `protected` cells.
-- Surprise-opened specials (saved same wave) must sit unchanged — Hat must not
-- swap/cycle their color before the next move (parity with maker_bomb_survives_wave).
triggerAdjacentHatsExcept :: Board -> [Pos] -> [Pos] -> Board
triggerAdjacentHatsExcept = triggerAdjacentHatsBy isGem

-- | 段 4：可改色谓词由调用方给出（元素框架里 = 注册表的 recolorable；内置等于 isGem）。
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
      -- 读颜色与改颜色是同一个遍历 cellColorT（第 9 项前是 cellColor + 手写的 recolorCell 两份分支）
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
-- Charge 1 -> produce Bomb of maker color in place; n>1 -> decrement.
-- Returns board (makers never enter the clear-hole set).
chargeAdjacentMakers :: Board -> [Pos] -> Board
chargeAdjacentMakers b clearedGems = fst (chargeAdjacentMakersSit b clearedGems)

-- | Like chargeAdjacentMakers, also returns positions converted to Bomb this wave.
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

-- | Adjacent-only wrapper (same targets as openSurprises, discards saved list).
openAdjacentSurprises :: Board -> [Pos] -> (Board, [Pos])
openAdjacentSurprises b clearedGems =
  let (b', expl, _) = openSurprises b clearedGems
  in (b', expl)

-- | Dye bottle positions orthogonally adjacent to cleared gems.
bottlesAdjacentTo :: Board -> [Pos] -> [Pos]
bottlesAdjacentTo = adjacentWhere isBottle

-- | Trigger dye bottles: recolor every ortho gem neighbor to the bottle color.
-- Bottle itself stays. Neighbors in the cleared set are skipped.
triggerAdjacentBottles :: Board -> [Pos] -> Board
triggerAdjacentBottles = noExcept triggerAdjacentBottlesExcept

-- | Like triggerAdjacentBottles, but also skip dyeing `protected` cells.
-- Surprise-opened specials and Maker-produced Bombs sit same-wave; Bottle
-- must not recolor them (parity with maker_bomb_survives_wave / Surprise sit).
triggerAdjacentBottlesExcept :: Board -> [Pos] -> [Pos] -> Board
triggerAdjacentBottlesExcept = triggerAdjacentBottlesBy isGem

-- | 段 4：可改色谓词由调用方给出（同 triggerAdjacentHatsBy）。
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


-- | Time spirit positions orthogonally adjacent to cleared gems.
spiritsAdjacentTo :: Board -> [Pos] -> [Pos]
spiritsAdjacentTo = adjacentWhere isTimeSpirit

-- | Remove adjacent time spirits (时间精灵). Dead positions cleared with the wave.
chipAdjacentTimeSpirits :: Board -> [Pos] -> (Board, [Pos])
chipAdjacentTimeSpirits = noExcept chipAdjacentTimeSpiritsExcept

-- | Like chipAdjacentTimeSpirits but skips cells in 'except' (already direct-hit this wave).
chipAdjacentTimeSpiritsExcept :: Board -> [Pos] -> [Pos] -> (Board, [Pos])
chipAdjacentTimeSpiritsExcept b clearedGems except =
  foldl hitOne (b, []) [p | p <- spiritsAdjacentTo b clearedGems, p `notElem` except]
  where
    hitOne (board, dead) p =
      case at board p of
        TimeSpirit -> (board, nub (p : dead))
        _ -> (board, dead)

-- | Legacy helper: positions that should be removed (last-layer stones only).
-- Prefer chipAdjacentStones in clear pipeline.
withAdjacentStones :: Board -> [Pos] -> [Pos]
withAdjacentStones b seeds =
  let (_, dead) = chipAdjacentStones b seeds
  in nub (seeds ++ dead)
