-- | 飞碟：覆盖实体，每波连锁末吸正交同色可吸收格并移格。
-- 跳过锁链/窗帘/雾/蒸汽、多冰、双面以免虚计 GoalUfo；吸走特殊块不引爆（由 Board 掩码）。
module Match3.Ufo
  ( Ufo(..)
  , mkUfo
  , ufoAbsorbTargets
  , moveUfo
  , stepUfo
  , stepUfos
  ) where

import Data.List (nub, sort)
import Match3.Board.Grid (neighborsInBounds)
import Match3.Types

-- | UFO overlay: sits over a cell and targets one gem color.
data Ufo = Ufo
  { ufoCell  :: Pos
  , ufoColor :: Color
  } deriving (Eq, Ord, Show)

mkUfo :: Pos -> Color -> Ufo
mkUfo = Ufo

-- | 界内邻格，上 / 下 / 左 / 右（吸附目标之后还会 sort，顺序不影响结果，照原样保留）。
ortho :: Board -> Pos -> [Pos]
ortho = neighborsInBounds upDownLeftRight

at :: Board -> Pos -> Cell
at = boardAt

-- | True if UFO can absorb this cell as a *full clear* of target color.
-- Skip peel-locks (Chain/Curtain/Fog/Steam), multi-ice (chip-only), and Flip
-- (direct hit only flips face) so GoalUfo cannot phantom-count soft hits.
-- Specials (Line/Bomb/Rainbow) are absorbable; Board.clearUfoAbsorbed masks them
-- so expandSpecials does not detonate on absorb.
matchesTarget :: Board -> Color -> Pos -> Bool
matchesTarget b col p =
  let cell = at b p
  in case cell of
    Gem c _ ice _
      | ice > 1 -> False
      | hasChain cell || hasCurtain cell || hasFog cell || hasSteam cell -> False
      | otherwise -> c == col
    Countdown c _ -> c == col
    Flip _ _ -> False
    _ -> False

-- | Orthogonally adjacent gems / countdowns matching the UFO target color.
ufoAbsorbTargets :: Board -> Ufo -> [Pos]
ufoAbsorbTargets b (Ufo cell col) =
  sort [p | p <- ortho b cell, matchesTarget b col p]

-- | 移一格：优先去吸到的第一格（按坐标排序）；没吸到就按右 → 下 → 左 → 上（'clockwiseFromRight'）取第一个界内邻格。
moveUfo :: Board -> [Pos] -> Ufo -> Ufo
moveUfo b absorbed (Ufo pos col) =
  case sort absorbed of
    (p : _) -> Ufo p col
    [] ->
      let cands = neighborsInBounds clockwiseFromRight b pos
      in case cands of
           (p : _) -> Ufo p col
           [] -> Ufo pos col

-- | Absorb adjacent same-color targets and move one cell.
-- Returns (absorbed positions, relocated UFO). Board mutation is caller's job.
stepUfo :: Board -> Ufo -> ([Pos], Ufo)
stepUfo b u =
  let targets = ufoAbsorbTargets b u
  in (targets, moveUfo b targets u)

-- | Step every UFO on the current board. Absorbed positions are unioned;
-- each UFO moves based on its own targets (using pre-move board).
stepUfos :: Board -> [Ufo] -> ([Pos], [Ufo])
stepUfos b ufos =
  let results = map (stepUfo b) ufos
      absorbed = nub (concatMap fst results)
      moved = map snd results
  in (absorbed, moved)
