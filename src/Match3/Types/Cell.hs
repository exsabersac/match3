{-# LANGUAGE DeriveGeneric #-}

-- | 单元格类型（第 6 刀从 Match3.Types 拆出）：宝石种类、叠层、单元格内容（全部内置本体 + 自定义开放槽），
-- 以及对任意格都有定义的通用读数（颜色 / 种类 / 冰层 / 叠层 / 是否宝石 / 软锁）。
-- 叠层的构造与谓词在 Match3.Types.Overlay，各本体的构造与谓词在 Match3.Types.Body。
--
-- 依赖：Match3.Color。不变量：构造器顺序与派生的 Show / Ord 与拆分前逐字相同（金标准与元素查询快照锁定）。
module Match3.Types.Cell
  ( GemKind(..)
  , CellOverlay(..)
  , CellContents(..)
  , Cell
  , mkGem
  , mkIceGem
  , specialActivates
  , iceLayers
  , cellOverlay
  , mkCustom
  , isCustom
  , isGem
  , cellColor
  , cellKind
  ) where

import GHC.Generics (Generic)
import Match3.Color (Color(..))

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
