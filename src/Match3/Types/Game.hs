{-# LANGUAGE DeriveGeneric #-}

-- | 一局的基础类型（第 6 刀从 Match3.Types 拆出）：分数 / 步数、结局 Outcome、地面层 Ground、开局配置 GameConfig。
--
-- 依赖：Match3.Goal、Match3.Types.Board（Pos）。
module Match3.Types.Game
  ( Score
  , MovesLeft
  , TargetScore
  , Outcome(..)
  , Ground
  , GameConfig(..)
  , defaultConfig
  ) where

import GHC.Generics (Generic)
import Match3.Goal (LevelGoal, goalScore)
import Match3.Types.Board (Pos)

type Score = Int
type MovesLeft = Int
type TargetScore = Int

data Outcome
  = InvalidSwap
  | NoMatch
  | MoveApplied Score
  | Won Score
  | Lost Score
  | LevelClear Score Int  -- score, next level index (0-based)
  deriving (Eq, Show, Generic)

-- | 地面层（段 2c）：棋盘格之下的一层元素槽，按位置记（元素名, 层数），位置升序、稀疏。
-- 不占格、不挡交换、不随重力 / 洗牌 / 皮带移动；上方格子被消除时受一次命中（规则见元素定义的 groundRule）。
-- 内置关卡里只有第 39 关（果冻）有地面层。
type Ground = [(Pos, (String, Int))]

data GameConfig = GameConfig
  { cfgMoves :: MovesLeft
  , cfgGoal  :: LevelGoal
  } deriving (Eq, Show)

defaultConfig :: GameConfig
defaultConfig = GameConfig { cfgMoves = 30, cfgGoal = goalScore 500 }
