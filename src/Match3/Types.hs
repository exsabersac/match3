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

data GameConfig = GameConfig
  { cfgMoves  :: MovesLeft
  , cfgTarget :: TargetScore
  } deriving (Eq, Show)

defaultConfig :: GameConfig
defaultConfig = GameConfig { cfgMoves = 30, cfgTarget = 500 }

data Level = Level
  { lvlIndex  :: Int
  , lvlName   :: String
  , lvlMoves  :: MovesLeft
  , lvlTarget :: TargetScore
  } deriving (Eq, Show)

allLevels :: [Level]
allLevels =
  [ Level 0 "入门" 30 300
  , Level 1 "热身" 28 450
  , Level 2 "进阶" 25 650
  , Level 3 "高手" 22 850
  , Level 4 "大师" 20 1100
  ]

levelConfig :: Level -> GameConfig
levelConfig l = GameConfig { cfgMoves = lvlMoves l, cfgTarget = lvlTarget l }
