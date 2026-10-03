{-# LANGUAGE DeriveGeneric #-}

-- | 一局的基础类型：分数 / 步数、一步的结果 Outcome 与终局 Terminal、地面层 Ground、开局配置 GameConfig。
--
-- 依赖：Match3.Goal、Match3.Types.Board（Pos）、Match3.Types.Name（地面层的元素名）。
module Match3.Types.Game
  ( Score
  , MovesLeft
  , TargetScore
  , Outcome(..)
  , Terminal(..)
  , terminalOf
  , fromTerminal
  , Ground
  , GameConfig(..)
  , defaultConfig
  ) where

import GHC.Generics (Generic)
import Match3.Goal (LevelGoal, goalScore)
import Match3.Types.Board (Pos)
import Match3.Types.Name (ElementName)

type Score = Int
type MovesLeft = Int
type TargetScore = Int

-- | 一步（交换 / 道具）的结果：被拒（InvalidSwap / NoMatch）、走完未结束（MoveApplied）或终局。
-- 终局之后的对局状态只存 'Terminal'（GameState.gsOver），不会存进非终局值。
data Outcome
  = InvalidSwap
  | NoMatch
  | MoveApplied Score
  | Won Score
  | Lost Score
  | LevelClear Score Int  -- score, next level index (0-based)
  deriving (Eq, Show, Generic)

-- | 终局：每日挑战或最后一关过关（TWon）、步数用完（TLost）、战役过关且还有下一关（TLevelClear 分数 下一关下标，从 0 起）。
-- 与 Outcome 的三个终局构造器一一对应（'terminalOf' / 'fromTerminal'）；对外打印（金标准、GameState 的 Show、
-- 网页 JSON）都先换回 Outcome，文本不变。
data Terminal
  = TWon Score
  | TLost Score
  | TLevelClear Score Int
  deriving (Eq, Show, Generic)

-- | 一步的结果是不是终局；是就给出对应的 'Terminal'。
terminalOf :: Outcome -> Maybe Terminal
terminalOf o = case o of
  Won s -> Just (TWon s)
  Lost s -> Just (TLost s)
  LevelClear s n -> Just (TLevelClear s n)
  InvalidSwap -> Nothing
  NoMatch -> Nothing
  MoveApplied _ -> Nothing

-- | 终局换回同名的 Outcome（terminalOf . fromTerminal = Just）。
fromTerminal :: Terminal -> Outcome
fromTerminal t = case t of
  TWon s -> Won s
  TLost s -> Lost s
  TLevelClear s n -> LevelClear s n

-- | 地面层：棋盘格之下的一层元素槽，按位置记（元素名, 层数），位置升序、稀疏。
-- 不占格、不挡交换、不随重力 / 洗牌 / 皮带移动；上方格子被消除时受一次命中（规则见元素定义的 groundRule）。
-- 内置关卡里只有第 39 关（果冻）有地面层。
type Ground = [(Pos, (ElementName, Int))]

data GameConfig = GameConfig
  { cfgMoves :: MovesLeft
  , cfgGoal  :: LevelGoal
  } deriving (Eq, Show)

defaultConfig :: GameConfig
defaultConfig = GameConfig { cfgMoves = 30, cfgGoal = goalScore 500 }
