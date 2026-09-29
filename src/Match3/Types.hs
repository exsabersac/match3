{-# LANGUAGE DeriveGeneric #-}

-- | 领域类型与关卡表：颜色、宝石种类、叠层、单元格内容、目标、结局、40 关配置（段 5 在 38 关之后追加果冻 / 气泡两关）。
-- 提供构造器 / 谓词 / goalMet*；不含交换、连锁或 IO。
-- specialActivates 定义软锁：多冰 / 锁链 / 窗帘下特殊块不点火。
module Match3.Types
  ( Ground
  , Color(..)
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
  , specialActivates
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
  , hasFreeze
  , freezeLayers
  , mkFreezeGem
  , hasCurtain
  , curtainLayers
  , mkCurtainGem
  , mkSnail
  , isSnail
  , snailDir
  , mkSafe
  , mkSafeLayers
  , safeLayers
  , isSafe
  , mkFlip
  , isFlip
  , flipFront
  , flipBack
  , mkSurprise
  , isSurprise
  , mkBottle
  , isBottle
  , bottleColor
  , mkTimeSpirit
  , isTimeSpirit
  , mkSteamGem
  , hasSteam
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
  , mkCustom
  , isCustom
  , isGem
  , cellColor
  , cellKind
  , Pos
  , Board
  , boardFromRows
  , boardRows
  , boardCells
  , boardAssocs
  , boardAt
  , boardSet
  , boardSetMany
  , boardArray
  , boardFromArray
  , mapBoard
  , boardSize
  , numColors
  , allColors
  , colorAt
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

import Data.Array (Array, assocs, bounds, elems, listArray, (!), (//))
import GHC.Generics (Generic)

data Color = C1 | C2 | C3 | C4 | C5
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Normal gem, line clearers (4-match), bomb, rainbow (5-match color clear).
data GemKind = Normal | LineH | LineV | Bomb | Rainbow
  deriving (Eq, Ord, Show, Generic)

-- | Overlay on a gem (开心消消乐草 / 藤蔓 / 巧克力 / 迷雾 / 锁链 / 火箭冰冻 / 窗帘).
-- Grass: clears on match. Vine: spreads after move. Choco: adjacent-clear + spreads.
-- Fog n: layers; adjacent clears peel one layer; fogged gems do not match until clear.
-- Chain n: iron chains (锁链); adjacent clears peel; chained gems cannot swap or match.
-- Freeze n: rocket freeze (火箭冰冻); blocks swap only (NOT match); adjacent clears peel.
-- Curtain n: curtain / roller shade (窗帘); adjacent clears peel; curtained gems do not match.
-- Steam: steam cloud (蒸汽); blocks match; adjacent clear extinguishes; surviving steam spreads.
-- Distinct from Ice (gem ice Int): Ice chips when the gem itself matches.
data CellOverlay = Grass | Vine | Choco | Fog Int | Chain Int | Freeze Int | Curtain Int | Steam
  deriving (Eq, Ord, Show, Generic)

-- | Board cell: gem (optional ice + overlay), stone, chest, honey, cake, balloon, cookie, magic hat, snail, or countdown.
-- Stone/Chest/Honey/Cake n = hit points; adjacent clears chip; removed at 0.
-- Cookie: falls with gravity; collected when it reaches the bottom row (开心消消乐饼干).
-- Cake: layered obstacle (蛋糕, distinct from Cookie); adjacent clears chip layers.
-- MagicHat: adjacent clear triggers color swap/recolor of neighboring gems (魔法帽).
-- Maker c n: juice maker (果汁机); n adjacent same-color clears produce a Bomb of color c.
-- Snail dr dc: crawling snail (蜗牛); blocks swap; after each player move crawls one step (pushes gems).
-- Countdown c n = colored timer bomb; matches as color c.
-- Gem overlay: Grass on match; Vine spreads; Choco cleared by adjacent match then spreads.
data CellContents
  = Gem Color GemKind Int (Maybe CellOverlay)  -- ice layers; overlay (Grass|Vine|Choco|Freeze|Curtain|…)
  | Stone Int
  | Chest Int   -- treasure chest (宝箱) layers; adjacent clears chip
  | Honey Int   -- honey jar (蜂蜜罐) layers; adjacent clears chip
  | Balloon Color  -- balloon (气球): popped by adjacent same-color clear
  | Cookie        -- biscuit (饼干): falls with gravity; collected on bottom row
  | Cake Int      -- cake layers (蛋糕): adjacent clears chip; not a collectible cookie
  | MagicHat      -- magic hat (魔法帽): adjacent clear swaps/recolors neighbor gem colors
  | Maker Color Int  -- juice maker (果汁机): needs N same-color adjacent clears; produces Bomb
  | Snail Int Int    -- snail (蜗牛): direction (dr, dc); crawls after each move
  | Safe Int      -- vault / safe (保险箱): adjacent clears chip; opens into Cookie
  | Flip Color Color  -- dual-face gem (双面块): matches as front; hit flips to back as Normal gem
  | Surprise      -- surprise egg/box (彩蛋): adjacent clear opens → special or 3×3 pop
  | Bottle Color  -- dye bottle (染色瓶): adjacent clear dyes ortho gems to bottle color
  | TimeSpirit   -- time spirit (时间精灵): adjacent clear awards +2 moves
  | Countdown Color Int
  | Custom String Int  -- ^ 元素框架的开放槽：注册表里按名字查定义的自定义元素（名字, 状态值）。
                       -- 只放在末尾，已有构造器的 Show / Ord 不变；内置关卡不使用。
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

-- | Gem sealed under rocket freeze (火箭冰冻): blocks swap only; adjacent clears peel.
-- Unlike Ice (match-chips gem ice) and Chain (blocks match too).
mkFreezeGem :: Color -> Int -> Cell
mkFreezeGem c n = Gem c Normal 0 (Just (Freeze (max 1 n)))

-- | Gem behind a curtain / roller shade (窗帘): adjacent clears peel; no match until clear.
mkCurtainGem :: Color -> Int -> Cell
mkCurtainGem c n = Gem c Normal 0 (Just (Curtain (max 1 n)))

-- | Snail facing (dr, dc); typically (0,1) right or (1,0) down.
mkSnail :: Int -> Int -> Cell
mkSnail dr dc =
  let dr' = if dr == 0 && dc == 0 then 0 else dr
      dc' = if dr == 0 && dc == 0 then 1 else dc
  in Snail dr' dc'

isSnail :: Cell -> Bool
isSnail (Snail _ _) = True
isSnail _ = False

snailDir :: Cell -> (Int, Int)
snailDir (Snail dr dc) = (dr, dc)
snailDir _ = (0, 0)


-- | 软锁纪律：ice>1 只削冰；Chain/Curtain 揭层但不激活特殊；
-- 末层冰（ice==1）清除并激活；Fog/Steam/Freeze 不构成软锁。
-- | True when a Line/Bomb/Rainbow gem would fire on a clear/activation seed.
-- Matches expandSpecials soft-lock discipline: ice>1 chips only; Chain/Curtain
-- peel without activating. Last ice (ice==1) clears and activates. Fog/Steam/
-- Freeze do not soft-lock (blast still fires). Non-gem cells never activate.
specialActivates :: Cell -> Bool
specialActivates (Gem _ _ ice ov)
  | ice > 1 = False
  | ice == 1 = True
  | Just (Chain _) <- ov = False
  | Just (Curtain _) <- ov = False
  | otherwise = True
specialActivates _ = False

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
iceLayers (Snail _ _) = 0
iceLayers (Safe _) = 0
iceLayers (Flip _ _) = 0
iceLayers Surprise = 0
iceLayers (Bottle _) = 0
iceLayers TimeSpirit = 0
iceLayers (Countdown _ _) = 0
iceLayers (Custom _ _) = 0

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

hasFreeze :: Cell -> Bool
hasFreeze c = case cellOverlay c of
  Just (Freeze _) -> True
  _ -> False

freezeLayers :: Cell -> Int
freezeLayers c = case cellOverlay c of
  Just (Freeze n) -> n
  _ -> 0

hasCurtain :: Cell -> Bool
hasCurtain c = case cellOverlay c of
  Just (Curtain _) -> True
  _ -> False

curtainLayers :: Cell -> Int
curtainLayers c = case cellOverlay c of
  Just (Curtain n) -> n
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

balloonColor :: Cell -> Maybe Color
balloonColor (Balloon c) = Just c
balloonColor _ = Nothing

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

makerColor :: Cell -> Maybe Color
makerColor (Maker c _) = Just c
makerColor _ = Nothing

makerCharges :: Cell -> Int
makerCharges (Maker _ n) = n
makerCharges _ = 0

isMaker :: Cell -> Bool
isMaker (Maker _ _) = True
isMaker _ = False

-- | Single-layer vault / safe (保险箱). Opens into a Cookie.
mkSafe :: Cell
mkSafe = Safe 1

mkSafeLayers :: Int -> Cell
mkSafeLayers n = Safe (max 1 n)

safeLayers :: Cell -> Int
safeLayers (Safe n) = n
safeLayers _ = 0

isSafe :: Cell -> Bool
isSafe (Safe _) = True
isSafe _ = False

-- | Dual-face gem (双面块): matches as front color; a clear hit flips to Normal gem of back.
mkFlip :: Color -> Color -> Cell
mkFlip front back = Flip front back

isFlip :: Cell -> Bool
isFlip (Flip _ _) = True
isFlip _ = False

flipFront :: Cell -> Maybe Color
flipFront (Flip f _) = Just f
flipFront _ = Nothing

flipBack :: Cell -> Maybe Color
flipBack (Flip _ b) = Just b
flipBack _ = Nothing

-- | Surprise egg / gift box (彩蛋 / 惊喜盒).
mkSurprise :: Cell
mkSurprise = Surprise

isSurprise :: Cell -> Bool
isSurprise Surprise = True
isSurprise _ = False

-- | Dye bottle (染色瓶): adjacent clear dyes ortho gems to this color.
mkBottle :: Color -> Cell
mkBottle = Bottle

isBottle :: Cell -> Bool
isBottle (Bottle _) = True
isBottle _ = False

bottleColor :: Cell -> Maybe Color
bottleColor (Bottle c) = Just c
bottleColor _ = Nothing

-- | Time spirit (时间精灵): adjacent clear removes it and awards +2 moves.
mkTimeSpirit :: Cell
mkTimeSpirit = TimeSpirit

isTimeSpirit :: Cell -> Bool
isTimeSpirit TimeSpirit = True
isTimeSpirit _ = False

-- | Gem covered by steam (蒸汽): blocks match; adjacent clear extinguishes; spreads after move.
mkSteamGem :: Color -> Cell
mkSteamGem c = Gem c Normal 0 (Just Steam)

hasSteam :: Cell -> Bool
hasSteam c = cellOverlay c == Just Steam

-- | Countdown bomb (倒计时炸弹): colored, matchable; n = turns left.
mkCountdown :: Color -> Int -> Cell
mkCountdown c n = Countdown c (max 1 n)

isCountdown :: Cell -> Bool
isCountdown (Countdown _ _) = True
isCountdown _ = False

countdownTurns :: Cell -> Int
countdownTurns (Countdown _ n) = n
countdownTurns _ = 0

-- | 自定义元素（元素框架开放槽）：名字 + 状态值（如剩余耐久）。
mkCustom :: String -> Int -> Cell
mkCustom = Custom

isCustom :: Cell -> Bool
isCustom (Custom _ _) = True
isCustom _ = False

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
isGem (Snail _ _) = False
isGem (Safe _) = False
isGem (Flip _ _) = True
isGem Surprise = False
isGem (Bottle _) = False
isGem TimeSpirit = False
isGem (Custom _ _) = False

-- | 宝石 / 倒计时 / 双面块（正面）的颜色；其余格没有颜色（Nothing）。
-- 气球 / 榨汁机 / 染色瓶的颜色另见 balloonColor / makerColor / bottleColor。
cellColor :: Cell -> Maybe Color
cellColor cell = case cell of
  Gem c _ _ _ -> Just c
  Countdown c _ -> Just c
  Flip f _ -> Just f
  _ -> Nothing

-- | 宝石的种类；倒计时与双面块按 Normal 参与组合判定；其余格没有种类（Nothing）。
cellKind :: Cell -> Maybe GemKind
cellKind cell = case cell of
  Gem _ k _ _ -> Just k
  Countdown _ _ -> Just Normal
  Flip _ _ -> Just Normal
  _ -> Nothing

numColors :: Int
numColors = 5

allColors :: [Color]
allColors = [minBound .. maxBound]

-- | 第 i 种颜色（按 allColors 顺序，下标对颜色数取模，负数也落在范围内）。
-- 取代 toEnum 构造颜色：总函数，不会因越界报错。
colorAt :: Int -> Color
colorAt i = case drop (i `mod` length allColors) allColors of
  c : _ -> c
  [] -> minBound

type Pos = (Int, Int)
-- | 盘面：以 (行, 列) 为下标的二维数组（第三刀起；之前是 [[Cell]]，读格要走两次 (!!)）。
-- 读格 boardAt 为 O(1)；写格 boardSet 复制一次数组（64 格，与原来重建行列表同量级）。
-- 与行列表互转用 boardFromRows / boardRows（行主序，与旧表示逐格一一对应；Show 仍按行列表打印）。
newtype Board = Board (Array (Int, Int) Cell)
  deriving (Eq)

instance Show Board where
  showsPrec d b = showsPrec d (boardRows b)

-- | 由行列表建盘（每行等长；空列表 = 0×0 盘）。
boardFromRows :: [[Cell]] -> Board
boardFromRows rows =
  let nr = length rows
      nc = case rows of
        [] -> 0
        (r0 : _) -> length r0
  in if any ((/= nc) . length) rows
       then error "boardFromRows: rows of unequal length"
       else Board (listArray ((0, 0), (nr - 1, nc - 1)) (concat rows))

-- | 行列表视图（行主序）。
boardRows :: Board -> [[Cell]]
boardRows (Board a) =
  let ((r0, c0), (r1, c1)) = bounds a
  in [[a ! (r, c) | c <- [c0 .. c1]] | r <- [r0 .. r1]]

-- | 全部格子，行主序。
boardCells :: Board -> [Cell]
boardCells (Board a) = elems a

-- | 全部 (坐标, 格子)，行主序。
boardAssocs :: Board -> [(Pos, Cell)]
boardAssocs (Board a) = assocs a

-- | 读一格，O(1)；越界报错（与旧 (!!) 相同）。
boardAt :: Board -> Pos -> Cell
boardAt (Board a) p = a ! p

-- | 写一格，返回新盘面。
boardSet :: Board -> Pos -> Cell -> Board
boardSet (Board a) p v = Board (a // [(p, v)])

-- | 一次写多格（后写的覆盖先写的）。
boardSetMany :: Board -> [(Pos, Cell)] -> Board
boardSetMany (Board a) kvs = Board (a // kvs)

-- | 底层二维数组（(行, 列) 下标，行主序）。
boardArray :: Board -> Array Pos Cell
boardArray (Board a) = a

-- | 由二维数组建盘（与 boardArray 互逆）。
boardFromArray :: Array Pos Cell -> Board
boardFromArray = Board

-- | 逐格变换。
mapBoard :: (Cell -> Cell) -> Board -> Board
mapBoard f (Board a) = Board (fmap f a)

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

-- | 地面层（段 2c）：棋盘格之下的一层元素槽，按位置记（元素名, 层数），位置升序、稀疏。
-- 不占格、不挡交换、不随重力 / 洗牌 / 皮带移动；上方格子被消除时受一次命中（规则见元素定义的 groundRule）。
-- 内置关卡恒为 []。
type Ground = [(Pos, (String, Int))]

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
  | GoalSafe Int                       -- open N vaults / safes (保险箱)
  | GoalUfo Int                        -- collect N gems via UFO absorb (飞碟)
  | GoalCarpet Int                     -- cover N carpet / floor tiles (地毯)
  | GoalNamed String Int               -- 段 2c：按元素名计数的目标（gsElementCounts 里该名字累计 ≥ N；扩展元素用）
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
goalMet (GoalSafe _) _ _ = False
goalMet (GoalUfo _) _ _ = False
goalMet (GoalCarpet n) _ collected = collected >= n
goalMet (GoalNamed _ n) _ collected = collected >= n

-- | Full goal check: bag + stones/UFO/chests/honey/balloon/cookie/cake/safe counters.
goalMetEx :: LevelGoal -> Score -> Int -> [(Color, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
goalMetEx (GoalScore t) score _ _ _ _ _ _ _ _ _ _ = score >= t
goalMetEx (GoalCollect _ n) _ collected _ _ _ _ _ _ _ _ _ = collected >= n
goalMetEx (GoalCollectMulti reqs) _ _ bag _ _ _ _ _ _ _ _ =
  all (\(col, n) -> lookupCount bag col >= n) reqs
goalMetEx (GoalClearStone n) _ _ _ stones _ _ _ _ _ _ _ = stones >= n
goalMetEx (GoalUfo n) _ _ _ _ ufos _ _ _ _ _ _ = ufos >= n
goalMetEx (GoalChest n) _ _ _ _ _ chests _ _ _ _ _ = chests >= n
goalMetEx (GoalHoney n) _ _ _ _ _ _ honey _ _ _ _ = honey >= n
goalMetEx (GoalBalloon n) _ _ _ _ _ _ _ balloons _ _ _ = balloons >= n
goalMetEx (GoalCookie n) _ _ _ _ _ _ _ _ cookies _ _ = cookies >= n
goalMetEx (GoalCake n) _ _ _ _ _ _ _ _ _ cakes _ = cakes >= n
goalMetEx (GoalSafe n) _ _ _ _ _ _ _ _ _ _ safes = safes >= n
goalMetEx (GoalCarpet n) _ collected _ _ _ _ _ _ _ _ _ = collected >= n
goalMetEx (GoalNamed _ n) _ collected _ _ _ _ _ _ _ _ _ = collected >= n

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
goalProgress (GoalSafe _) _ collected = collected
goalProgress (GoalUfo _) _ collected = collected
goalProgress (GoalCarpet _) _ collected = collected
goalProgress (GoalNamed _ _) _ collected = collected

goalProgressEx :: LevelGoal -> Score -> Int -> [(Color, Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
goalProgressEx (GoalScore _) score _ _ _ _ _ _ _ _ _ _ = score
goalProgressEx (GoalCollect _ _) _ collected _ _ _ _ _ _ _ _ _ = collected
goalProgressEx (GoalCollectMulti reqs) _ _ bag _ _ _ _ _ _ _ _ =
  sum [min n (lookupCount bag c) | (c, n) <- reqs]
goalProgressEx (GoalClearStone _) _ _ _ stones _ _ _ _ _ _ _ = stones
goalProgressEx (GoalUfo _) _ _ _ _ ufos _ _ _ _ _ _ = ufos
goalProgressEx (GoalChest _) _ _ _ _ _ chests _ _ _ _ _ = chests
goalProgressEx (GoalHoney _) _ _ _ _ _ _ honey _ _ _ _ = honey
goalProgressEx (GoalBalloon _) _ _ _ _ _ _ _ balloons _ _ _ = balloons
goalProgressEx (GoalCookie _) _ _ _ _ _ _ _ _ cookies _ _ = cookies
goalProgressEx (GoalCake _) _ _ _ _ _ _ _ _ _ cakes _ = cakes
goalProgressEx (GoalSafe _) _ _ _ _ _ _ _ _ _ _ safes = safes
goalProgressEx (GoalCarpet _) _ collected _ _ _ _ _ _ _ _ _ = collected
goalProgressEx (GoalNamed _ _) _ collected _ _ _ _ _ _ _ _ _ = collected

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
goalTarget (GoalSafe n) = n
goalTarget (GoalUfo n) = n
goalTarget (GoalCarpet n) = n
goalTarget (GoalNamed _ n) = n

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

-- | Mixed campaign: score / collect / stone / chest / honey / balloon / cookie / cake / hat / chain / maker / portal / UFO / snail / freeze / curtain / safe / flip / surprise / bottle / time-spirit / steam / carpet / hazards / jelly / bubble (段 5); difficulty ramps.
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
  , Level 11 "轰炸"   20 (GoalScore 750)
  , Level 12 "飞碟"   24 (GoalUfo 10)
  , Level 13 "碟猎"   20 (GoalUfo 14)
  , Level 14 "压力"   20 (GoalCollectMulti [(C1, 10), (C2, 10), (C3, 8)])
  , Level 15 "大师"   22 (GoalScore 1000)
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
  , Level 27 "终章"   24 (GoalScore 1400)
  , Level 28 "蜗牛"   20 (GoalScore 850)
  , Level 29 "冰冻"   20 (GoalCollect C2 16)
  , Level 30 "窗帘"   22 (GoalCollect C1 16)
  , Level 31 "金库"   22 (GoalSafe 5)
  , Level 32 "惊喜"   22 (GoalScore 900)
  , Level 33 "染色"   22 (GoalCollect C3 16)
  , Level 34 "时灵"   22 (GoalScore 850)
  , Level 35 "蒸汽"   22 (GoalCollect C2 16)
  , Level 36 "地毯"   24 (GoalCarpet 8)
  , Level 37 "织毯"   24 (GoalCarpet 12)
    -- 段 5：追加在 38 关之后（前 38 关不变）；目标按元素名计数（GoalNamed）
  , Level 38 "果冻"   24 (GoalNamed "jelly" 32)
  , Level 39 "气泡"   22 (GoalNamed "bubble" 12)
  ]

levelConfig :: Level -> GameConfig
levelConfig l = GameConfig { cfgMoves = lvlMoves l, cfgGoal = lvlGoal l }
