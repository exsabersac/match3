-- | 关卡目标的前端外观（第 5 刀：一处表）：图标、两套色调，全部按 Match3.Goal.goalView 的
-- 目标形状 / 计数键分派（标题 / 状态栏的文字标签 countTag / colorTag 第 11 刀起在 Match3.View）。进度与目标值不在这里：统一由核心 gsProgress / goalTarget 算（第 5 刀前
-- HudArt / HudPrim / LevelMap / Actions / Input 各有一份「目标构造器 → 计数字段 / 颜色」的 case）。
--
-- 依赖：Match3.Core、UI.Layout（调色板）、UI.Cell.Art（宝石贴图名）。
-- 同步：色值与第 5 刀前各处逐字相同（截图对照 AE=0）；非内置形状（ViewOther、未列出的计数键）落到多色的外观。
module UI.GoalStyle
  ( goalIcon
  , goalTint
  , goalPip
  ) where

import Data.Word (Word8)
import Match3.Core
import SDL (V3 (..), V4 (..))
import UI.Cell.Art (gemSprite)
import UI.Layout (colorRGB, namedRGB)

-- | 目标图标（复用棋子贴图；贴图版 HUD 与地图节点）。
goalIcon :: LevelGoal -> String
goalIcon g = case goalView g of
  ViewScore _ -> "icon_score"
  ViewCollect c _ -> gemSprite c
  ViewCount k _ -> case k of
    CountStones -> "stone_3"
    CountChests -> "chest"
    CountHoney -> "honey"
    CountBalloons -> "balloon_c1"
    CountCookies -> "cookie"
    CountCakes -> "cake_1"
    CountSafes -> "safe"
    CountUfo -> "ufo_c3"
    CountCarpets -> "carpet_covered"
    CountNamed name
      | unElementName name == "chameleon" -> "chameleon_icon" -- 变色龙（新玩法 7）：环贴图单独看不出是宝石，用合成图标
      | otherwise -> unElementName name
    _ -> "icon_multi"
  _ -> "icon_multi"

-- | 贴图版 HUD 进度条色调。
goalTint :: LevelGoal -> V3 Word8
goalTint g = case goalView g of
  ViewScore _ -> V3 110 230 150
  ViewCollect c _ -> let (r, gg, b) = colorRGB c in V3 r gg b
  ViewCount k _ -> case k of
    CountStones -> V3 180 184 200
    CountChests -> V3 230 170 70
    CountHoney -> V3 250 190 50
    CountBalloons -> V3 255 120 160
    CountCookies -> V3 220 160 90
    CountCakes -> V3 255 140 190
    CountSafes -> V3 200 180 90
    CountUfo -> V3 170 130 255
    CountCarpets -> V3 220 90 150
    CountNamed name -> let (r, gg, b) = namedRGB name in V3 r gg b
    _ -> multi
  _ -> multi
  where
    multi = V3 240 190 100

-- | 几何版 HUD 进度条与选关地图节点小点的颜色。
goalPip :: LevelGoal -> V4 Word8
goalPip g = case goalView g of
  ViewScore _ -> V4 100 220 140 255
  ViewCollect c _ -> let (r, gg, b) = colorRGB c in V4 r gg b 255
  ViewCount k _ -> case k of
    CountStones -> V4 160 160 170 255
    CountChests -> V4 220 170 60 255
    CountHoney -> V4 240 180 40 255
    CountBalloons -> V4 255 120 160 255
    CountCookies -> V4 210 160 90 255
    CountCakes -> V4 255 140 180 255
    CountSafes -> V4 200 170 50 255
    CountUfo -> V4 180 120 255 255
    CountCarpets -> V4 180 100 160 255
    CountNamed name -> let (r, gg, b) = namedRGB name in V4 r gg b 255
    _ -> multi
  _ -> multi
  where
    multi = V4 220 180 100 255
