-- | 各内置本体（石块 / 宝箱 / 蜂蜜 / 气球 / 饼干 / 蛋糕 / 魔法帽 / 果汁机 / 蜗牛 / 保险箱 / 双面块 / 彩蛋 /
-- 染色瓶 / 时间精灵 / 倒计时）的构造、谓词与读数（第 6 刀从 Match3.Types 拆出）。
--
-- 依赖：Match3.Color、Match3.Types.Cell。
module Match3.Types.Body
  ( mkSnail
  , isSnail
  , snailDir
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
  , mkCountdown
  , isCountdown
  , countdownTurns
  ) where

import Match3.Color (Color)
import Match3.Types.Cell

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


-- | Countdown bomb (倒计时炸弹): colored, matchable; n = turns left.
mkCountdown :: Color -> Int -> Cell
mkCountdown c n = Countdown c (max 1 n)

isCountdown :: Cell -> Bool
isCountdown (Countdown _ _) = True
isCountdown _ = False

countdownTurns :: Cell -> Int
countdownTurns (Countdown _ n) = n
countdownTurns _ = 0

