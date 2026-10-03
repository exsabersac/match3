-- | 关卡目标的前端外观（一处表）：图标、两套色调，全部按 Match3.Goal.goalView 的
-- 目标形状 / 计数键分派（标题 / 状态栏的文字标签在 Match3.View：goalBracket / colorTag / goalLabel）。
-- 进度与目标值不在这里：统一由核心 gsProgress / goalTarget 算。
--
-- 依赖：Match3.Core、UI.Layout（调色板）、UI.GoalIcon（目标图标名；纯模块，在 app/pure，网页版 Api 也用它）。
-- 同步：色值由截图对照锁定（AE=0）；非内置形状（ViewOther、未列出的计数键）落到多色的外观。
module UI.GoalStyle
  ( goalIcon
  , goalTint
  , goalPip
  ) where

import Data.Word (Word8)
import Match3.Core
import SDL (V3 (..), V4 (..))
import UI.GoalIcon (goalIcon)
import UI.Layout (colorRGB, namedRGB)

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
