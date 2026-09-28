{-# LANGUAGE DeriveGeneric #-}
module Match3.Types
  ( Color(..)
  , GemKind(..)
  , Cell(..)
  , mkGem
  , Pos
  , Board
  , boardSize
  , numColors
  , allColors
  , Score
  , MovesLeft
  , TargetScore
  , Outcome(..)
  , LevelGoal(..)
  , goalMet
  , goalProgress
  , goalTarget
  , GameConfig(..)
  , defaultConfig
  , Level(..)
  , allLevels
  , levelConfig
  ) where

import GHC.Generics (Generic)

data Color = C1 | C2 | C3 | C4 | C5
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Normal gem, line clearers (4-match), bomb (5-match).
data GemKind = Normal | LineH | LineV | Bomb
  deriving (Eq, Ord, Show, Generic)

data Cell = Cell
  { cellColor :: Color
  , cellKind  :: GemKind
  } deriving (Eq, Ord, Show, Generic)

mkGem :: Color -> Cell
mkGem c = Cell c Normal

numColors :: Int
numColors = 5

allColors :: [Color]
allColors = [minBound .. maxBound]

type Pos = (Int, Int)
type Board = [[Cell]]

boardSize :: Int
boardSize = 8

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

-- | Level win condition: reach a score, or clear N gems of a color.
data LevelGoal
  = GoalScore TargetScore
  | GoalCollect Color Int
  deriving (Eq, Show, Generic)

-- | Whether the goal is satisfied given current score / collected count.
goalMet :: LevelGoal -> Score -> Int -> Bool
goalMet (GoalScore t) score _ = score >= t
goalMet (GoalCollect _ n) _ collected = collected >= n

-- | Current progress toward the goal (score or collected count).
goalProgress :: LevelGoal -> Score -> Int -> Int
goalProgress (GoalScore _) score _ = score
goalProgress (GoalCollect _ _) _ collected = collected

-- | Target number shown in HUD (score target or collect count).
goalTarget :: LevelGoal -> Int
goalTarget (GoalScore t) = t
goalTarget (GoalCollect _ n) = n

data GameConfig = GameConfig
  { cfgMoves :: MovesLeft
  , cfgGoal  :: LevelGoal
  } deriving (Eq, Show)

defaultConfig :: GameConfig
defaultConfig = GameConfig { cfgMoves = 30, cfgGoal = GoalScore 500 }

data Level = Level
  { lvlIndex :: Int
  , lvlName  :: String
  , lvlMoves :: MovesLeft
  , lvlGoal  :: LevelGoal
  } deriving (Eq, Show)

-- | Mixed campaign: score targets + color-collect stages.
allLevels :: [Level]
allLevels =
  [ Level 0 "入门"   30 (GoalScore 300)
  , Level 1 "采红"   30 (GoalCollect C1 20)
  , Level 2 "热身"   26 (GoalScore 500)
  , Level 3 "采蓝"   26 (GoalCollect C3 22)
  , Level 4 "进阶"   22 (GoalScore 700)
  , Level 5 "采绿"   24 (GoalCollect C2 26)
  , Level 6 "大师"   18 (GoalScore 1100)
  ]

levelConfig :: Level -> GameConfig
levelConfig l = GameConfig { cfgMoves = lvlMoves l, cfgGoal = lvlGoal l }
