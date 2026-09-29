{-# LANGUAGE NamedFieldPuns #-}

-- | 对局状态：GameState 及其（部分字段）相等语义、撤销快照、本步特效 MoveFx 的边沿触发、提示与撤销。
--
-- 依赖：Match3.Types、Match3.Board（findHint）、Ufo / Conveyor（字段类型）。
-- 不变量：gsCombo / gsLastCleared 只描述最近一次**真正结算**的一步；任何被拒操作经
-- clearMoveFx / rejectMove 清零，moveFx 对 NoMatch / InvalidSwap / 已终局一律返回空，
-- 前端因此不会重播上一步的连击（护栏 failed_swap_resets_combo_feedback 等）。
module Match3.Game.State
  ( GameState(..)
  , snapshot
  , clearMoveFx
  , rejectMove
  , MoveFx(..)
  , moveFx
  , undoMove
  , applyHint
  ) where

import Data.Maybe (isJust)
import Match3.Board (findHint)
import Match3.Ufo (Ufo(..))
import Match3.Conveyor (Belt)
import Match3.Types
import System.Random (StdGen)

-- | 一局的全部规则状态。前端只读；所有修改都经 trySwap / use* / undoMove / shuffleGame 等纯函数返回新值。
data GameState = GameState
  { gsBoard         :: Board
  , gsScore         :: Score
  , gsMoves         :: MovesLeft
  , gsGoal          :: LevelGoal
  , gsCollected     :: Int          -- primary collect-color cleared (GoalCollect)
  , gsColorBag      :: [(Color, Int)] -- cumulative clears per color
  , gsStonesCleared :: Int          -- fully destroyed stones
  , gsChestsCleared :: Int          -- fully opened treasure chests (宝箱)
  , gsHoneyCleared  :: Int          -- fully smashed honey jars (蜂蜜罐)
  , gsBalloonsPopped :: Int         -- balloons popped (气球)
  , gsCookiesCollected :: Int       -- biscuits collected at bottom (饼干)
  , gsCakesCleared  :: Int          -- cakes fully cleared (蛋糕)
  , gsSafesOpened   :: Int          -- vaults / safes opened (保险箱)
  , gsGen           :: StdGen
  , gsOver          :: Maybe Outcome
  , gsLevel         :: Int
  , gsHistory       :: [GameState]
  , gsHint          :: Maybe (Pos, Pos)
  , gsCombo         :: Int   -- last move max cascade wave (0 if none)
  , gsShuffled      :: Bool  -- True if last ensurePlayable reshuffled
  , gsBelts         :: [Belt] -- conveyor paths (开心消消乐传送带)
  , gsPortals       :: [(Pos, Pos)] -- bidirectional portal pairs (传送门)
  , gsHammers       :: Int    -- hammer booster charges
  , gsFreeSwaps     :: Int    -- free-swap booster charges (any two cells)
  , gsCrossClears   :: Int    -- cross-clear booster charges
  , gsUfos          :: [Ufo]  -- flying saucers (飞碟)
  , gsUfoCollected  :: Int    -- gems absorbed by UFOs
  , gsCarpetOpen    :: [Pos]  -- uncovered carpet / floor tiles (地毯目标)
  , gsCarpetsCovered :: Int   -- carpet tiles covered this level
  , gsLastCleared   :: [Pos]  -- cells cleared last move (UI particles; not belt/snail noise)
  , gsDaily         :: Bool   -- True for date-seeded daily challenge (通关≠战役推进)
  } deriving (Show)

instance Eq GameState where
  a == b =
    gsBoard a == gsBoard b
      && gsScore a == gsScore b
      && gsMoves a == gsMoves b
      && gsGoal a == gsGoal b
      && gsCollected a == gsCollected b
      && gsColorBag a == gsColorBag b
      && gsStonesCleared a == gsStonesCleared b
      && gsChestsCleared a == gsChestsCleared b
      && gsHoneyCleared a == gsHoneyCleared b
      && gsBalloonsPopped a == gsBalloonsPopped b
      && gsCookiesCollected a == gsCookiesCollected b
      && gsCakesCleared a == gsCakesCleared b
      && gsSafesOpened a == gsSafesOpened b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b
      && gsDaily a == gsDaily b
      && gsBelts a == gsBelts b
      && gsPortals a == gsPortals b
      && gsHammers a == gsHammers b
      && gsFreeSwaps a == gsFreeSwaps b
      && gsCrossClears a == gsCrossClears b
      && gsUfos a == gsUfos b
      && gsUfoCollected a == gsUfoCollected b
      && gsCarpetOpen a == gsCarpetOpen b
      && gsCarpetsCovered a == gsCarpetsCovered b

-- | 写入撤销历史的快照：去掉嵌套历史、提示与洗牌标记，避免历史无限嵌套。
snapshot :: GameState -> GameState
snapshot gs = gs { gsHistory = [], gsHint = Nothing, gsShuffled = False }

-- | 清空「上一步」的 UI 反馈字段（连击波数 / 本步清除格）。
-- 这两个字段只描述最近一次**真正结算**的一步；任何没有结算的操作（无匹配回滚、
-- 挡交换、非相邻、道具无效、洗牌、撤销）都必须把它们归零，否则前端会把旧值
-- 当成新一步的结果，再播一遍爆击（连击）特效。规则判定不读这两个字段。
clearMoveFx :: GameState -> GameState
clearMoveFx gs = gs { gsCombo = 0, gsLastCleared = [] }

-- | 交换 / 道具被拒（NoMatch）时的回滚状态：盘面不变，清提示与本步反馈。
rejectMove :: GameState -> GameState
rejectMove gs = (clearMoveFx gs) { gsHint = Nothing, gsShuffled = False }

-- | 一次操作之后，前端该播的特效（边沿触发，只看这一次调用的结果）。
data MoveFx = MoveFx
  { fxCombo   :: Int    -- ^ 本步连击波数；> 1 才播连击 / 爆击特效
  , fxCleared :: [Pos]  -- ^ 本步清除格（闪光 + 粒子）
  } deriving (Eq, Show)

-- | 由「操作前状态、操作后状态、结果」决定要不要播特效。
-- 只有这次调用真正结算了一步（MoveApplied / 本次才产生的 Won / Lost / LevelClear）
-- 才返回 after 的连击与清除格；NoMatch / InvalidSwap / 操作前就已结束（trySwap 原样
-- 返回旧 gsOver）一律返回空，不会重播上一步的爆击特效。
moveFx :: GameState -> GameState -> Outcome -> MoveFx
moveFx before after out
  | isJust (gsOver before) = noFx
  | otherwise = case out of
      NoMatch -> noFx
      InvalidSwap -> noFx
      _ -> MoveFx (gsCombo after) (gsLastCleared after)
  where
    noFx = MoveFx 0 []

-- | 撤销一步：回到上一个快照，清掉本步特效字段与终局标记；无历史时 Nothing。
undoMove :: GameState -> Maybe GameState
undoMove gs = case gsHistory gs of
  (prev : rest) ->
    -- 快照里的 gsCombo / gsLastCleared 属于更早那一步，撤销后不应再当作「本步反馈」
    Just (clearMoveFx prev) { gsHistory = rest, gsHint = Nothing, gsOver = Nothing, gsShuffled = False }
  [] -> Nothing

-- | 计算一手可走的交换并记在 gsHint（不改盘面）。
applyHint :: GameState -> (GameState, Maybe (Pos, Pos))
applyHint gs =
  let h = findHint (gsBoard gs)
  in (gs { gsHint = h }, h)
