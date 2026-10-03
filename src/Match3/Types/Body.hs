-- | 各内置本体（石块 / 宝箱 / 蜂蜜 / 气球 / 饼干 / 蛋糕 / 魔法帽 / 果汁机 / 蜗牛 / 保险箱 / 双面块 / 彩蛋 /
-- 染色瓶 / 时间精灵 / 倒计时）的构造、谓词与读数（第 6 刀从 Match3.Types 拆出）。
--
-- 第 9 项（docs/haskell-features/09-规则去重.md）：五种带层数的占格障碍（石块 / 宝箱 / 蜂蜜 / 蛋糕 / 保险箱）的
-- mkXLayers / xLayers / isX 原来各是一组只差构造器的 case，现在都由同一个棱镜（Match3.Types.Optics 的 '_Stone' 等）派生：
-- 构造 = @review 棱镜 . max 1@，读数 = 'layersOf' 棱镜，谓词 = @has 棱镜@。
--
-- 依赖：Match3.Color、Match3.Types.Cell、Engine.Optics、Match3.Types.Optics（障碍棱镜）。
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

import Data.Maybe (fromMaybe)
import Data.Monoid (First)
import Engine.Optics (Getting, AReview, has, preview, review)
import Match3.Color (Color)
import Match3.Types.Cell
import Match3.Types.Optics (_Cake, _Chest, _Honey, _Safe, _Stone)

-- | 带层数障碍的构造：至少 1 层（第 9 项前各写一遍 @X (max 1 n)@）。
mkLayers :: AReview Cell Int -> Int -> Cell
mkLayers layer n = review layer (max 1 n)

-- | 带层数障碍的层数：不是这种障碍时 0（第 9 项前各写一遍 @xLayers (X n) = n; xLayers _ = 0@）。
layersOf :: Getting (First Int) Cell Int -> Cell -> Int
layersOf layer = fromMaybe 0 . preview layer

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
mkStoneLayers = mkLayers _Stone

stoneLayers :: Cell -> Int
stoneLayers = layersOf _Stone

isStone :: Cell -> Bool
isStone = has _Stone

-- | Single-layer treasure chest (宝箱).
mkChest :: Cell
mkChest = Chest 1

-- | Multi-layer treasure chest.
mkChestLayers :: Int -> Cell
mkChestLayers = mkLayers _Chest

chestLayers :: Cell -> Int
chestLayers = layersOf _Chest

isChest :: Cell -> Bool
isChest = has _Chest

-- | Single-layer honey jar (蜂蜜罐).
mkHoney :: Cell
mkHoney = Honey 1

-- | Multi-layer honey jar.
mkHoneyLayers :: Int -> Cell
mkHoneyLayers = mkLayers _Honey

honeyLayers :: Cell -> Int
honeyLayers = layersOf _Honey

isHoney :: Cell -> Bool
isHoney = has _Honey

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
mkCakeLayers = mkLayers _Cake

cakeLayers :: Cell -> Int
cakeLayers = layersOf _Cake

isCake :: Cell -> Bool
isCake = has _Cake

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
mkSafeLayers = mkLayers _Safe

safeLayers :: Cell -> Int
safeLayers = layersOf _Safe

isSafe :: Cell -> Bool
isSafe = has _Safe

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

