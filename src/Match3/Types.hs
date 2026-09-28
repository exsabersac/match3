{-# LANGUAGE DeriveGeneric #-}
module Match3.Types
  ( Color(..)
  , GemKind(..)
  , CellOverlay(..)
  , CellContents(..)
  , Cell
  , mkGem
  , mkIceGem
  , mkGrassGem
  , mkVineGem
  , mkChocoGem
  , iceLayers
  , cellOverlay
  , hasGrass
  , hasVine
  , hasChoco
  , hasFog
  , fogLayers
  , mkFogGem
  , hasChain
  , chainLayers
  , mkChainGem
  , clearOverlay
  , setOverlay
  , mkStone
  , mkStoneLayers
  , stoneLayers
  , isStone
  , mkChest
  , mkChestLayers
  , chestLayers
  , isChest
  , mkHoney
  , mkHoneyLayers
  , honeyLayers
  , isHoney
  , mkBalloon
  , isBalloon
  , balloonColor
  , mkCookie
  , isCookie
  , mkCake
  , mkCakeLayers
  , cakeLayers
  , isCake
  , mkMagicHat
  , isMagicHat
  , mkMaker
  , mkMakerCharges
  , makerColor
  , makerCharges
  , isMaker
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

-- | Overlay on a gem (开心消消乐草 / 藤蔓 / 巧克力 / 迷雾).
-- Grass: clears on match. Vine: spreads after move. Choco: adjacent-clear + spreads.
-- Fog n: layers; adjacent clears peel one layer; fogged gems do not match until clear.
-- Chain n: iron chains (锁链); adjacent clears peel; chained gems cannot swap or match.
data CellOverlay = Grass | Vine | Choco | Fog Int | Chain Int
  deriving (Eq, Ord, Show, Generic)

-- | Board cell: gem (optional ice + overlay), stone, chest, honey, cake, balloon, cookie, magic hat, or countdown.
-- Stone/Chest/Honey/Cake n = hit points; adjacent clears chip; removed at 0.
-- Cookie: falls with gravity; collected when it reaches the bottom row (开心消消乐饼干).
-- Cake: layered obstacle (蛋糕, distinct from Cookie); adjacent clears chip layers.
-- MagicHat: adjacent clear triggers color swap/recolor of neighboring gems (魔法帽).
-- Maker c n: juice maker (果汁机); n adjacent same-color clears produce a Bomb of color c.
-- Countdown c n = colored timer bomb; matches as color c.
-- Gem overlay: Grass on match; Vine spreads; Choco cleared by adjacent match then spreads.
data CellContents
  = Gem Color GemKind Int (Maybe CellOverlay)  -- ice layers; overlay (Grass|Vine|Choco)
  | Stone Int
  | Chest Int   -- treasure chest (宝箱) layers; adjacent clears chip
  | Honey Int   -- honey jar (蜂蜜罐) layers; adjacent clears chip
  | Balloon Color  -- balloon (气球): popped by adjacent same-color clear
  | Cookie        -- biscuit (饼干): falls with gravity; collected on bottom row
  | Cake Int      -- cake layers (蛋糕): adjacent clears chip; not a collectible cookie
  | MagicHat      -- magic hat (魔法帽): adjacent clear swaps/recolors neighbor gem colors
  | Maker Color Int  -- juice maker (果汁机): needs N same-color adjacent clears; produces Bomb
  | Countdown Color Int
  deriving (Eq, Ord, Show, Generic)

type Cell = CellContents

mkGem :: Color -> Cell
mkGem c = Gem c Normal 0 Nothing

-- | Gem sealed under N ice layers (must chip ice before the gem clears).
mkIceGem :: Color -> Int -> Cell
mkIceGem c n = Gem c Normal (max 0 n) Nothing

-- | Gem covered by grass (草坪): match on this cell clears the grass.
mkGrassGem :: Color -> Cell
mkGrassGem c = Gem c Normal 0 (Just Grass)

-- | Gem wrapped by vine (藤蔓): spreads after move unless cleared.
mkVineGem :: Color -> Cell
mkVineGem c = Gem c Normal 0 (Just Vine)

-- | Gem covered by chocolate (巧克力): cleared by adjacent match; spreads after move.
mkChocoGem :: Color -> Cell
mkChocoGem c = Gem c Normal 0 (Just Choco)

-- | Gem covered by fog / cloud (迷雾): adjacent clears peel one layer.
mkFogGem :: Color -> Int -> Cell
mkFogGem c n = Gem c Normal 0 (Just (Fog (max 1 n)))

-- | Gem locked by iron chain (锁链): adjacent clears peel; cannot swap/match while chained.
mkChainGem :: Color -> Int -> Cell
mkChainGem c n = Gem c Normal 0 (Just (Chain (max 1 n)))

iceLayers :: Cell -> Int
iceLayers (Gem _ _ n _) = n
iceLayers (Stone _) = 0
iceLayers (Chest _) = 0
iceLayers (Honey _) = 0
iceLayers (Balloon _) = 0
iceLayers Cookie = 0
iceLayers (Cake _) = 0
iceLayers MagicHat = 0
iceLayers (Maker _ _) = 0
iceLayers (Countdown _ _) = 0

cellOverlay :: Cell -> Maybe CellOverlay
cellOverlay (Gem _ _ _ o) = o
cellOverlay _ = Nothing

hasGrass :: Cell -> Bool
hasGrass c = cellOverlay c == Just Grass

hasVine :: Cell -> Bool
hasVine c = cellOverlay c == Just Vine

hasChoco :: Cell -> Bool
hasChoco c = cellOverlay c == Just Choco

hasFog :: Cell -> Bool
hasFog c = case cellOverlay c of
  Just (Fog _) -> True
  _ -> False

fogLayers :: Cell -> Int
fogLayers c = case cellOverlay c of
  Just (Fog n) -> n
  _ -> 0

hasChain :: Cell -> Bool
hasChain c = case cellOverlay c of
  Just (Chain _) -> True
  _ -> False

chainLayers :: Cell -> Int
chainLayers c = case cellOverlay c of
  Just (Chain n) -> n
  _ -> 0

-- | Strip overlay, keep gem/ice.
clearOverlay :: Cell -> Cell
clearOverlay (Gem col kind ice _) = Gem col kind ice Nothing
clearOverlay x = x

setOverlay :: Maybe CellOverlay -> Cell -> Cell
setOverlay o (Gem col kind ice _) = Gem col kind ice o
setOverlay _ x = x

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

-- | Single-layer treasure chest (宝箱).
mkChest :: Cell
mkChest = Chest 1

-- | Multi-layer treasure chest.
mkChestLayers :: Int -> Cell
mkChestLayers n = Chest (max 1 n)

chestLayers :: Cell -> Int
chestLayers (Chest n) = n
chestLayers _ = 0

isChest :: Cell -> Bool
isChest (Chest _) = True
isChest _ = False

-- | Single-layer honey jar (蜂蜜罐).
mkHoney :: Cell
mkHoney = Honey 1

-- | Multi-layer honey jar.
mkHoneyLayers :: Int -> Cell
mkHoneyLayers n = Honey (max 1 n)

honeyLayers :: Cell -> Int
honeyLayers (Honey n) = n
honeyLayers _ = 0

isHoney :: Cell -> Bool
isHoney (Honey _) = True
isHoney _ = False

-- | Colored balloon (气球): blocks swaps; adjacent same-color clear pops it.
mkBalloon :: Color -> Cell
mkBalloon = Balloon

isBalloon :: Cell -> Bool
isBalloon (Balloon _) = True
isBalloon _ = False

balloonColor :: Cell -> Color
balloonColor (Balloon c) = c
balloonColor _ = error "balloonColor: not a balloon"

-- | Biscuit / cookie (饼干): falls; collected on the bottom row.
mkCookie :: Cell
mkCookie = Cookie

isCookie :: Cell -> Bool
isCookie Cookie = True
isCookie _ = False

-- | Single-layer cake (蛋糕) — layered obstacle, NOT the collectible cookie.
mkCake :: Cell
mkCake = Cake 1

-- | Multi-layer cake (蛋糕).
mkCakeLayers :: Int -> Cell
mkCakeLayers n = Cake (max 1 n)

cakeLayers :: Cell -> Int
cakeLayers (Cake n) = n
cakeLayers _ = 0

isCake :: Cell -> Bool
isCake (Cake _) = True
isCake _ = False

-- | Magic hat (魔法帽): blocks swaps; adjacent clear swaps/recolors neighbor colors.
mkMagicHat :: Cell
mkMagicHat = MagicHat

isMagicHat :: Cell -> Bool
isMagicHat MagicHat = True
isMagicHat _ = False

-- | Juice maker / factory (果汁机): needs N same-color adjacent clears.
mkMaker :: Color -> Cell
mkMaker c = Maker c 3

mkMakerCharges :: Color -> Int -> Cell
mkMakerCharges c n = Maker c (max 1 n)

makerColor :: Cell -> Color
makerColor (Maker c _) = c
makerColor _ = error "makerColor: not a maker"

makerCharges :: Cell -> Int
makerCharges (Maker _ n) = n
makerCharges _ = 0

isMaker :: Cell -> Bool
isMaker (Maker _ _) = True
isMaker _ = False

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
isGem (Gem _ _ _ _) = True
isGem (Countdown _ _) = True
isGem (Stone _) = False
isGem (Chest _) = False
isGem (Honey _) = False
isGem (Balloon _) = False
isGem Cookie = False
isGem (Cake _) = False
isGem MagicHat = False
isGem (Maker _ _) = False

-- | Color of a gem / countdown cell. Partial on Stone.
cellColor :: Cell -> Color
cellColor (Gem c _ _ _) = c
cellColor (Countdown c _) = c
cellColor (Stone _) = error "cellColor: Stone has no color"
cellColor (Chest _) = error "cellColor: Chest has no color"
cellColor (Honey _) = error "cellColor: Honey has no color"
cellColor (Balloon _) = error "cellColor: Balloon has no color (use balloonColor)"
cellColor Cookie = error "cellColor: Cookie has no color"
cellColor (Cake _) = error "cellColor: Cake has no color"
cellColor MagicHat = error "cellColor: MagicHat has no color"
cellColor (Maker _ _) = error "cellColor: Maker has no color (use makerColor)"

-- | Kind of a gem cell. Countdown acts as Normal for combo checks.
cellKind :: Cell -> GemKind
cellKind (Gem _ k _ _) = k
cellKind (Countdown _ _) = Normal
cellKind (Stone _) = error "cellKind: Stone has no kind"
cellKind (Chest _) = error "cellKind: Chest has no kind"
cellKind (Honey _) = error "cellKind: Honey has no kind"
cellKind (Balloon _) = error "cellKind: Balloon has no kind"
cellKind Cookie = error "cellKind: Cookie has no kind"
cellKind (Cake _) = error "cellKind: Cake has no kind"
cellKind MagicHat = error "cellKind: MagicHat has no kind"
cellKind (Maker _ _) = error "cellKind: Maker has no kind"

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
  | GoalChest Int                      -- open N treasure chests (宝箱)
  | GoalHoney Int                      -- smash N honey jars (蜂蜜罐)
  | GoalBalloon Int                    -- pop N balloons (气球)
  | GoalCookie Int                     -- collect N biscuits at bottom (饼干)
  | GoalCake Int                       -- clear N cake layers fully (蛋糕)
  | GoalUfo Int                        -- collect N gems via UFO absorb (飞碟)
  deriving (Eq, Show, Generic)

-- | Whether the goal is satisfied given current score / primary collected count.
-- For GoalCollectMulti / GoalClearStone prefer goalMetEx.
goalMet :: LevelGoal -> Score -> Int -> Bool
goalMet (GoalScore t) score _ = score >= t
goalMet (GoalCollect _ n) _ collected = collected >= n
goalMet (GoalCollectMulti _) _ _ = False  -- use goalMetEx
goalMet (GoalClearStone _) _ _ = False
goalMet (GoalChest _) _ _ = False
goalMet (GoalHoney _) _ _ = False
goalMet (GoalBalloon _) _ _ = False
goalMet (GoalCookie _) _ _ = False
goalMet (GoalCake _) _ _ = False
goalMet (GoalUfo _) _ _ = False

-- | Full goal check with color bag + stones/UFO/chests/honey/balloon/cookie/cake counters.
goalMetEx :: LevelGoal -> Score -> Int -> [(Color, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
goalMetEx (GoalScore t) score _ _ _ _ _ _ _ _ _ = score >= t
goalMetEx (GoalCollect _ n) _ collected _ _ _ _ _ _ _ _ = collected >= n
goalMetEx (GoalCollectMulti reqs) _ _ bag _ _ _ _ _ _ _ =
  all (\(col, n) -> lookupCount bag col >= n) reqs
goalMetEx (GoalClearStone n) _ _ _ stones _ _ _ _ _ _ = stones >= n
goalMetEx (GoalUfo n) _ _ _ _ ufos _ _ _ _ _ = ufos >= n
goalMetEx (GoalChest n) _ _ _ _ _ chests _ _ _ _ = chests >= n
goalMetEx (GoalHoney n) _ _ _ _ _ _ honey _ _ _ = honey >= n
goalMetEx (GoalBalloon n) _ _ _ _ _ _ _ balloons _ _ = balloons >= n
goalMetEx (GoalCookie n) _ _ _ _ _ _ _ _ cookies _ = cookies >= n
goalMetEx (GoalCake n) _ _ _ _ _ _ _ _ _ cakes = cakes >= n

lookupCount :: [(Color, Int)] -> Color -> Int
lookupCount xs col = maybe 0 id (lookup col xs)

-- | Current progress toward the goal (primary meter).
goalProgress :: LevelGoal -> Score -> Int -> Int
goalProgress (GoalScore _) score _ = score
goalProgress (GoalCollect _ _) _ collected = collected
goalProgress (GoalCollectMulti _) _ collected = collected
goalProgress (GoalClearStone _) _ collected = collected
goalProgress (GoalChest _) _ collected = collected
goalProgress (GoalHoney _) _ collected = collected
goalProgress (GoalBalloon _) _ collected = collected
goalProgress (GoalCookie _) _ collected = collected
goalProgress (GoalCake _) _ collected = collected
goalProgress (GoalUfo _) _ collected = collected

goalProgressEx :: LevelGoal -> Score -> Int -> [(Color, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
goalProgressEx (GoalScore _) score _ _ _ _ _ _ _ _ _ = score
goalProgressEx (GoalCollect _ _) _ collected _ _ _ _ _ _ _ _ = collected
goalProgressEx (GoalCollectMulti reqs) _ _ bag _ _ _ _ _ _ _ =
  sum [min n (lookupCount bag c) | (c, n) <- reqs]
goalProgressEx (GoalClearStone _) _ _ _ stones _ _ _ _ _ _ = stones
goalProgressEx (GoalUfo _) _ _ _ _ ufos _ _ _ _ _ = ufos
goalProgressEx (GoalChest _) _ _ _ _ _ chests _ _ _ _ = chests
goalProgressEx (GoalHoney _) _ _ _ _ _ _ honey _ _ _ = honey
goalProgressEx (GoalBalloon _) _ _ _ _ _ _ _ balloons _ _ = balloons
goalProgressEx (GoalCookie _) _ _ _ _ _ _ _ _ cookies _ = cookies
goalProgressEx (GoalCake _) _ _ _ _ _ _ _ _ _ cakes = cakes

-- | Target number shown in HUD.
goalTarget :: LevelGoal -> Int
goalTarget (GoalScore t) = t
goalTarget (GoalCollect _ n) = n
goalTarget (GoalCollectMulti reqs) = sum [n | (_, n) <- reqs]
goalTarget (GoalClearStone n) = n
goalTarget (GoalChest n) = n
goalTarget (GoalHoney n) = n
goalTarget (GoalBalloon n) = n
goalTarget (GoalCookie n) = n
goalTarget (GoalCake n) = n
goalTarget (GoalUfo n) = n

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

-- | Mixed campaign: score / collect / stone / chest / honey / balloon / cookie / cake / hat / chain / maker / portal / UFO / hazards; difficulty ramps.
allLevels :: [Level]
allLevels =
  [ Level 0  "入门"   30 (GoalScore 300)
  , Level 1  "采红"   30 (GoalCollect C1 20)
  , Level 2  "热身"   26 (GoalScore 500)
  , Level 3  "采蓝"   26 (GoalCollect C3 22)
  , Level 4  "进阶"   24 (GoalScore 700)
  , Level 5  "冰绿"   24 (GoalCollect C2 26)
  , Level 6  "双采"   28 (GoalCollectMulti [(C1, 12), (C3, 12)])
  , Level 7  "碎石"   26 (GoalClearStone 8)
  , Level 8  "草场"   24 (GoalScore 600)
  , Level 9  "藤袭"   22 (GoalCollect C1 18)
  , Level 10 "传送"   22 (GoalScore 800)
  , Level 11 "轰炸"   18 (GoalScore 750)
  , Level 12 "飞碟"   24 (GoalUfo 10)
  , Level 13 "碟猎"   20 (GoalUfo 14)
  , Level 14 "压力"   18 (GoalCollectMulti [(C1, 10), (C2, 10), (C3, 8)])
  , Level 15 "大师"   18 (GoalScore 1100)
  , Level 16 "宝箱"   24 (GoalChest 6)
  , Level 17 "巧箱"   22 (GoalChest 5)
  , Level 18 "蜂蜜"   24 (GoalHoney 6)
  , Level 19 "蜜压"   22 (GoalHoney 5)
  , Level 20 "气球"   24 (GoalBalloon 6)
  , Level 21 "饼干"   24 (GoalCookie 6)
  , Level 22 "巧饼"   22 (GoalCookie 5)
  , Level 23 "蛋糕"   24 (GoalCake 6)
  , Level 24 "帽宴"   22 (GoalCake 5)
  , Level 25 "锁链"   22 (GoalScore 900)
  , Level 26 "果汁"   24 (GoalCollect C1 18)
  , Level 27 "终章"   16 (GoalScore 1500)
  ]

levelConfig :: Level -> GameConfig
levelConfig l = GameConfig { cfgMoves = lvlMoves l, cfgGoal = lvlGoal l }
