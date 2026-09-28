{-# LANGUAGE DeriveGeneric #-}
module Match3.Types
  ( Color(..)
  , GemKind(..)
  , CellContents(..)
  , Cell
  , mkGem
  , mkIceGem
  , iceLayers
  , mkStone
  , mkStoneLayers
  , stoneLayers
  , isStone
  , mkCountdown
  , isCountdown
  , countdownTurns
  , isGem
  , cellColor
  , cellKind
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
  , goalMetEx
  , goalProgress
  , goalProgressEx
  , goalTarget
  , lookupCount
  , GameConfig(..)
  , defaultConfig
  , Level(..)
  , allLevels
  , levelConfig
  ) where

import GHC.Generics (Generic)

data Color = C1 | C2 | C3 | C4 | C5
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Normal gem, line clearers (4-match), bomb, rainbow (5-match color clear).
data GemKind = Normal | LineH | LineV | Bomb | Rainbow
  deriving (Eq, Ord, Show, Generic)

-- | Board cell: gem (optional ice), layered stone crate, or countdown bomb.
-- Stone n = hit points; adjacent clears chip; removed at 0.
-- Countdown c n = colored timer bomb (开心消消乐倒计时炸弹); matches as color c.
data CellContents
  = Gem Color GemKind Int  -- ice layers on gem (0 = none); 开心消消乐冰层
  | Stone Int
  | Countdown Color Int    -- turns remaining; ticks after each successful move
  deriving (Eq, Ord, Show, Generic)

type Cell = CellContents

mkGem :: Color -> Cell
mkGem c = Gem c Normal 0

-- | Gem sealed under N ice layers (must chip ice before the gem clears).
mkIceGem :: Color -> Int -> Cell
mkIceGem c n = Gem c Normal (max 0 n)

iceLayers :: Cell -> Int
iceLayers (Gem _ _ n) = n
iceLayers (Stone _) = 0
iceLayers (Countdown _ _) = 0

-- | Single-layer stone (cleared by one adjacent clear).
mkStone :: Cell
mkStone = Stone 1

-- | Multi-layer stone / crate (开心消消乐-style box).
mkStoneLayers :: Int -> Cell
mkStoneLayers n = Stone (max 1 n)

stoneLayers :: Cell -> Int
stoneLayers (Stone n) = n
stoneLayers _ = 0

isStone :: Cell -> Bool
isStone (Stone _) = True
isStone _ = False

-- | Countdown bomb (倒计时炸弹): colored, matchable; n = turns left.
mkCountdown :: Color -> Int -> Cell
mkCountdown c n = Countdown c (max 1 n)

isCountdown :: Cell -> Bool
isCountdown (Countdown _ _) = True
isCountdown _ = False

countdownTurns :: Cell -> Int
countdownTurns (Countdown _ n) = n
countdownTurns _ = 0

-- | True for ordinary gems and countdown bombs (both match by color).
isGem :: Cell -> Bool
isGem (Gem _ _ _) = True
isGem (Countdown _ _) = True
isGem (Stone _) = False

-- | Color of a gem / countdown cell. Partial on Stone.
cellColor :: Cell -> Color
cellColor (Gem c _ _) = c
cellColor (Countdown c _) = c
cellColor (Stone _) = error "cellColor: Stone has no color"

-- | Kind of a gem cell. Countdown acts as Normal for combo checks.
cellKind :: Cell -> GemKind
cellKind (Gem _ k _) = k
cellKind (Countdown _ _) = Normal
cellKind (Stone _) = error "cellKind: Stone has no kind"

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

-- | Level win condition (GoalCollect shape frozen; new goals are additive).
data LevelGoal
  = GoalScore TargetScore
  | GoalCollect Color Int
  | GoalCollectMulti [(Color, Int)]  -- all color quotas must be met
  | GoalClearStone Int                 -- fully destroy N stone blockers
  deriving (Eq, Show, Generic)

-- | Whether the goal is satisfied given current score / primary collected count.
-- For GoalCollectMulti / GoalClearStone prefer goalMetEx.
goalMet :: LevelGoal -> Score -> Int -> Bool
goalMet (GoalScore t) score _ = score >= t
goalMet (GoalCollect _ n) _ collected = collected >= n
goalMet (GoalCollectMulti _) _ _ = False  -- use goalMetEx
goalMet (GoalClearStone _) _ _ = False

-- | Full goal check with color bag + stones-cleared counters.
goalMetEx :: LevelGoal -> Score -> Int -> [(Color, Int)] -> Int -> Bool
goalMetEx (GoalScore t) score _ _ _ = score >= t
goalMetEx (GoalCollect _ n) _ collected _ _ = collected >= n
goalMetEx (GoalCollectMulti reqs) _ _ bag _ =
  all (\(col, n) -> lookupCount bag col >= n) reqs
goalMetEx (GoalClearStone n) _ _ _ stones = stones >= n

lookupCount :: [(Color, Int)] -> Color -> Int
lookupCount xs col = maybe 0 id (lookup col xs)

-- | Current progress toward the goal (primary meter).
goalProgress :: LevelGoal -> Score -> Int -> Int
goalProgress (GoalScore _) score _ = score
goalProgress (GoalCollect _ _) _ collected = collected
goalProgress (GoalCollectMulti _) _ collected = collected
goalProgress (GoalClearStone _) _ collected = collected

goalProgressEx :: LevelGoal -> Score -> Int -> [(Color, Int)] -> Int -> Int
goalProgressEx (GoalScore _) score _ _ _ = score
goalProgressEx (GoalCollect _ _) _ collected _ _ = collected
goalProgressEx (GoalCollectMulti reqs) _ _ bag _ =
  sum [min n (lookupCount bag c) | (c, n) <- reqs]
goalProgressEx (GoalClearStone _) _ _ _ stones = stones

-- | Target number shown in HUD.
goalTarget :: LevelGoal -> Int
goalTarget (GoalScore t) = t
goalTarget (GoalCollect _ n) = n
goalTarget (GoalCollectMulti reqs) = sum [n | (_, n) <- reqs]
goalTarget (GoalClearStone n) = n

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
  , Level 6 "双采"   28 (GoalCollectMulti [(C1, 12), (C3, 12)])
  , Level 7 "碎石"   26 (GoalClearStone 8)
  , Level 8 "大师"   18 (GoalScore 1100)
  ]

levelConfig :: Level -> GameConfig
levelConfig l = GameConfig { cfgMoves = lvlMoves l, cfgGoal = lvlGoal l }
