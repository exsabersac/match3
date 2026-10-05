-- | 关卡目标的图标贴图名（纯模块）：网页版 HUD 目标条与选关地图节点
-- （Match3Web.Api 编码进 state.goal.icon）用这一张表，网页端不另写「目标 → 图标」映射。
-- 按 Match3.Goal.goalView 的目标形状 / 计数键分派；图标复用棋子贴图（宝石名 gem_c1..gem_c5，同 web/www/cells.js）。
module UI.GoalIcon
  ( goalIcon
  ) where

import Match3.Core

-- | 目标图标（复用棋子贴图；贴图版 HUD 与地图节点）。
goalIcon :: LevelGoal -> String
goalIcon g = case goalView g of
  ViewScore _ -> "icon_score"
  ViewCollect c _ -> "gem_c" ++ show (fromEnum c + 1)
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
