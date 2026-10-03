{-# LANGUAGE NamedFieldPuns #-}

-- | 洗牌：保留装饰（障碍 / 特殊 / 叠层）的重洗，以及无可走步时的自动洗牌 ensurePlayable。
--
-- 依赖：State、Match3.Board.*（shufflePlayable / hasValidMove）、元素注册表（保留判定 keepOnShuffleWith）。
-- 不变量：只换普通宝石颜色的位置，装饰原样放回；自动洗牌不在回放脚本的 mtEnd 里，
-- 前端用 mtFinal 与结算后 gsBoard 的差异补播（app 的 StShuffle 阶段）。
module Match3.Game.Shuffle
  ( CellDecor(..)
  , extractDecor
  , extractDecorWith
  , restoreDecor
  , ensurePlayable
  , ensurePlayableWith
  , shuffleGame
  , shuffleGameWith
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Engine.Stream (headS, iterateS, splitAtS)
import Match3.Board.Grid (setCell)
import Match3.Board.Match (hasValidMoveWith)
import Match3.Board.Random (shufflePlayableSized)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry, keepOnShuffleWith)
import Match3.Types
import Match3.Game.State

-- | Snapshot blockers/overlays so reshuffle does not erase level décor.
data CellDecor = CellDecor
  { cdPos :: Pos
  , cdCell :: Cell
  } deriving (Eq, Show)

-- | 记下所有需要在洗牌后原样放回的格（障碍、特殊块、带叠层 / 冰的宝石等）。
extractDecor :: Board -> [CellDecor]
extractDecor = extractDecorWith defaultRegistry

-- | extractDecor（指定注册表）：保留判定 = keepOnShuffleWith（有冰 / 叠层，或本体 keepOnShuffle）。
-- 只有普通宝石会被洗走；直线 / 炸弹 / 彩虹特殊块与所有障碍原样放回。
extractDecorWith :: Registry -> Board -> [CellDecor]
extractDecorWith reg = ifoldMap (\p cell -> [CellDecor p cell | keepOnShuffleWith reg cell])

-- | 把 extractDecor 记下的格写回新盘面的原位置。
restoreDecor :: Board -> [CellDecor] -> Board
restoreDecor b = foldl (\board (CellDecor p cell) -> setCell board p cell) b

-- | If board has no valid move (and game not over), reshuffle to a playable board.
-- Retries a few times because restoreDecor can recreate a stuck layout.
ensurePlayable :: GameState -> GameState
ensurePlayable = ensurePlayableWith defaultRegistry

-- | ensurePlayable（指定注册表）。
ensurePlayableWith :: Registry -> GameState -> GameState
ensurePlayableWith reg gs
  | Just _ <- gsOver gs = gs { gsShuffled = False }
  | hasValidMoveWith reg (gsBoard gs) = gs { gsShuffled = False }
  | otherwise = fromMaybe (headS rest) (find (hasValidMoveWith reg . gsBoard) checked)
  where
    -- 第 4 项（惰性）：全部重洗结果是一条无穷流 s1, s2, …（每次从上一次的生成器接着洗）；
    -- 前 24 次逐个检查、取第一个有可走步的，都没有就用第 25 次（不再检查）。
    -- 流是惰性的：第 k 次合格时 s(k+1) 以后根本不会洗，生成器与第 4 项前的计数循环 go 24 … go 0 逐次相同。
    (checked, rest) = splitAtS 24 (iterateS reshuffleOnce (reshuffleOnce gs))
    dims g = boardDims (gsBoard g)
    reshuffle g =
      let (rows, cols) = dims g
          decor = extractDecorWith reg (gsBoard g)
          (board0, g') = shufflePlayableSized rows cols (gsGen g)
          board = restoreDecor board0 decor
      in (board, g')
    reshuffleOnce g =
      let (board, g') = reshuffle g
      in g { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True }

-- | Force reshuffle (e.g. player key S). Preserves stones / ice / overlays /
-- specials (Line/Bomb/Rainbow) / countdown bombs; keeps UFOs.
shuffleGame :: GameState -> GameState
shuffleGame = shuffleGameWith defaultRegistry

-- | 手动洗牌（指定注册表，段 2c）：保留判定 keepOnShuffleWith 用这张表——自定义元素
-- （如测试专用元素）按它自己的 keepOnShuffle 原样放回，不会退回内置表被当普通格洗走。
shuffleGameWith :: Registry -> GameState -> GameState
shuffleGameWith reg gs =
  let (rows, cols) = boardDims (gsBoard gs)
      decor = extractDecorWith reg (gsBoard gs)
      (board0, g') = shufflePlayableSized rows cols (gsGen gs)
      board = restoreDecor board0 decor
  -- 洗牌不是一步消除：清掉上一步的连击 / 清除格反馈
  in (clearMoveFx gs) { gsBoard = board, gsGen = g', gsHint = Nothing, gsShuffled = True, gsOver = Nothing }
