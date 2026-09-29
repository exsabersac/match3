-- | 通用撤销历史（段 3）：任何 "Engine.Game" 的游戏都可以套一层「可撤销」。
--
-- 历史只在这里保存一份（三消的 GameState 不再带 gsHistory）；外壳执行动作只调 gameStep：
-- 普通动作包成 Act a 交给原游戏，Undo 由本层处理。本层**不 import 任何具体游戏**。
--
-- 规则（由 HistoryPolicy 描述，三消的取值见 Match3.Engine.match3History）：
--   * 原游戏接受了动作、且 hpRecord 认为它是「走步」时，先把走步前的状态经 hpSnapshot 记下来
--     （最多 hpLimit 份，最旧的丢掉），再换成新状态；其余动作（被拒 / 提示 / 洗牌……）只换当前状态；
--   * Undo：有历史则回到最近一份快照（经 hpRestore 整理），**终局后同样可以撤销**
--     （先于原游戏的终局拒绝处理）；没有历史时被拒、状态不变。
module Engine.History
  ( History(..)
  , Undoable(..)
  , HistoryPolicy(..)
  , startHistory
  , historyDepth
  , pushHistory
  , replaceNow
  , undoHistory
  , commitStep
  , withHistory
  ) where

import Engine.Game (Game(..), Step(..))

-- | 当前状态 + 撤销快照（新的在前）。
data History s = History
  { histNow  :: s
  , histPast :: [s]
  } deriving (Show)

-- | 可撤销的动作：原游戏的动作，或撤销一步。
data Undoable a = Act a | Undo
  deriving (Eq, Show)

-- | 一种游戏的历史规则。
data HistoryPolicy s a = HistoryPolicy
  { hpLimit    :: Int         -- ^ 最多保留几份快照
  , hpRecord   :: a -> Bool   -- ^ 被接受时要记快照的动作（走步）
  , hpSnapshot :: s -> s      -- ^ 记入前整理（去掉只属于当前时刻的字段）
  , hpRestore  :: s -> s      -- ^ 撤销回去时整理
  }

-- | 开局：没有历史。
startHistory :: s -> History s
startHistory s = History s []

-- | 可撤销的步数。
historyDepth :: History s -> Int
historyDepth = length . histPast

-- | 记下当前状态的快照，再换成新状态。
pushHistory :: HistoryPolicy s a -> History s -> s -> History s
pushHistory p h s' = History s' (take (hpLimit p) (hpSnapshot p (histNow h) : histPast h))

-- | 只换当前状态（不记快照）。
replaceNow :: History s -> s -> History s
replaceNow h s' = h {histNow = s'}

-- | 撤销一步；没有历史时 Nothing。
undoHistory :: HistoryPolicy s a -> History s -> Maybe (History s)
undoHistory p h = case histPast h of
  (prev : rest) -> Just (History (hpRestore p prev) rest)
  [] -> Nothing

-- | 原游戏走完一步之后更新历史。
commitStep :: HistoryPolicy s a -> a -> History s -> Step s e o r -> History s
commitStep p a h st
  | stepAccepted st && hpRecord p a = pushHistory p h (stepState st)
  | otherwise = replaceNow h (stepState st)

-- | 给一种游戏套上撤销历史。
withHistory :: HistoryPolicy s a -> Game cfg s a e o r -> Game cfg (History s) (Undoable a) e o r
withHistory p g =
  Game
    { gameName = gameName g
    , gameNew = \cfg seed -> startHistory (gameNew g cfg seed)
    , gameStep = step
    , gameOutcome = gameOutcome g . histNow
    , gameActions = \h -> map Act (gameActions g (histNow h)) ++ [Undo | not (null (histPast h))]
    , gameStatus = \h -> gameStatus g (histNow h) ++ [("undo", historyDepth h)]
    , gameEffect = gameEffect g
    }
  where
    step h Undo = case undoHistory p h of
      Just h' -> Step h' [] (gameOutcome g (histNow h')) True Nothing
      Nothing -> Step h [] (gameOutcome g (histNow h)) False Nothing
    step h (Act a) =
      let st = gameStep g (histNow h) a
      in st {stepState = commitStep p a h st}
