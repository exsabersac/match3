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
import Match3.Types

-- | UFO overlay: sits over a cell and targets one gem color.
data Ufo = Ufo
  { ufoCell  :: Pos
  , ufoColor :: Color
  } deriving (Eq, Ord, Show)

mkUfo :: Pos -> Color -> Ufo
mkUfo = Ufo

inBoard :: Pos -> Bool
inBoard (r, c) =
  r >= 0 && r < boardSize && c >= 0 && c < boardSize

ortho :: Pos -> [Pos]
ortho (r, c) =
  filter inBoard [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

at :: Board -> Pos -> Cell
at b (r, c) = (b !! r) !! c

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
  sort [p | p <- ortho cell, matchesTarget b col p]

-- | Relocate one step: prefer first absorbed cell; else cycle right→down→left→up.
moveUfo :: [Pos] -> Ufo -> Ufo
moveUfo absorbed (Ufo pos col) =
  case sort absorbed of
    (p : _) -> Ufo p col
    [] ->
      let (r, c) = pos
          cands =
            filter inBoard [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
      in case cands of
           (p : _) -> Ufo p col
           [] -> Ufo pos col

-- | Absorb adjacent same-color targets and move one cell.
-- Returns (absorbed positions, relocated UFO). Board mutation is caller's job.
stepUfo :: Board -> Ufo -> ([Pos], Ufo)
stepUfo b u =
  let targets = ufoAbsorbTargets b u
  in (targets, moveUfo targets u)

-- | Step every UFO on the current board. Absorbed positions are unioned;
-- each UFO moves based on its own targets (using pre-move board).
stepUfos :: Board -> [Ufo] -> ([Pos], [Ufo])
stepUfos b ufos =
  let results = map (stepUfo b) ufos
      absorbed = nub (concatMap fst results)
      moved = map snd results
  in (absorbed, moved)
