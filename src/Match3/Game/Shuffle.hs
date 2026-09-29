{-# LANGUAGE NamedFieldPuns #-}

-- | 洗牌：保留装饰（障碍 / 特殊 / 叠层）的重洗，以及无可走步时的自动洗牌 ensurePlayable。
--
-- 依赖：State、Match3.Board（shufflePlayable / hasValidMove）。
-- 不变量：只换普通宝石颜色的位置，装饰原样放回；自动洗牌不在回放脚本的 mtEnd 里，
-- 前端用 mtFinal 与结算后 gsBoard 的差异补播（app 的 StShuffle 阶段）。
module Match3.Game.Shuffle
  ( CellDecor(..)
  , extractDecor
  , restoreDecor
  , ensurePlayable
  , shuffleGame
  ) where

import Match3.Board (hasValidMove, shufflePlayable, setCell, getCell)
import Match3.Types
import Match3.Game.State

-- | Snapshot blockers/overlays so reshuffle does not erase level décor.
data CellDecor = CellDecor
  { cdPos :: Pos
  , cdCell :: Cell
  } deriving (Eq, Show)

-- | 记下所有需要在洗牌后原样放回的格（障碍、特殊块、带叠层 / 冰的宝石等）。
extractDecor :: Board -> [CellDecor]
extractDecor b =
  [ CellDecor (r, c) cell
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , let cell = getCell b (r, c)
  , keep cell
  ]
  where
    keep (Stone _) = True
    keep (Chest _) = True
    keep (Honey _) = True
    keep (Balloon _) = True
    keep Cookie = True
    keep (Cake _) = True
    keep MagicHat = True
    keep (Maker _ _) = True
    keep (Snail _ _) = True
    keep (Safe _) = True
    keep (Flip _ _) = True
    keep Surprise = True
    keep (Bottle _) = True
    keep TimeSpirit = True
    keep (Countdown _ _) = True
    keep (Gem _ kind ice ov) =
      kind /= Normal || ice > 0 || ov /= Nothing
    -- Bare Normal gems are shuffled away; Line/Bomb/Rainbow specials stay

-- | 把 extractDecor 记下的格写回新盘面的原位置。
restoreDecor :: Board -> [CellDecor] -> Board
restoreDecor b = foldl (\board (CellDecor p cell) -> setCell board p cell) b

-- | If board has no valid move (and game not over), reshuffle to a playable board.
-- Retries a few times because restoreDecor can recreate a stuck layout.
ensurePlayable :: GameState -> GameState
ensurePlayable gs
  | Just _ <- gsOver gs = gs { gsShuffled = False }
  | hasValidMove (gsBoard gs) = gs { gsShuffled = False }
  | otherwise = go (24 :: Int) gs
  where
    go 0 g =
      let decor = extractDecor (gsBoard g)
          (board0, g') = shufflePlayable (gsGen g)
          board = restoreDecor board0 decor
      in g { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }
    go n g =
      let decor = extractDecor (gsBoard g)
          (board0, g') = shufflePlayable (gsGen g)
          board = restoreDecor board0 decor
          g2 = g { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }
      in if hasValidMove board then g2 else go (n - 1) g2

-- | Force reshuffle (e.g. player key S). Preserves stones / ice / overlays /
-- specials (Line/Bomb/Rainbow) / countdown bombs; keeps UFOs.
shuffleGame :: GameState -> GameState
shuffleGame gs =
  let decor = extractDecor (gsBoard gs)
      (board0, g') = shufflePlayable (gsGen gs)
      board = restoreDecor board0 decor
  -- 洗牌不是一步消除：清掉上一步的连击 / 清除格反馈
  in (clearMoveFx gs) { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True, gsOver = Nothing }
