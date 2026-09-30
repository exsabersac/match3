-- | 目标的中文显示名（合 main 9f5504e 后从 Match3.View 下移到这里）：HUD「目标 …」标签、网页 state.goal.label /
-- m3Levels 的 goal.label、失败提示（Match3.Game.Outcome.loseHint）与桌面窗口标题都取这里，前端不再各自维护
-- 「目标种类 → 中文」的表；与图例（tools/gen_assets.py 的 LEGEND）用词一致。
--
-- 放在 Match3.Game.* 之下、Match3.View 之上（View 重新导出 countLabel / colorLabel / namedGoalLabelTable，
-- 并用 'goalViewLabel' 定义 goalLabel，对外 API 不变），这样 Game.Outcome 也能用而不成环。
--
-- 依赖：Match3.Goal、Match3.Counts、Match3.Color、Match3.Element.Builtin（元素名常量）。
module Match3.GoalLabel
  ( goalViewLabel
  , countLabel
  , colorLabel
  , namedGoalLabelTable
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Match3.Color (Color(..))
import Match3.Counts (CounterKey(..))
import Match3.Element.Builtin (chameleonName, snowBossName)
import Match3.Goal (GoalView(..))
import Match3.Types.Name (ElementName(..))

-- | 目标形状的中文名。名字目标查 'namedGoalLabelTable'；没登记的名字退回元素名本身
-- （不静默丢掉，网页 e2e 与 stack test 会因英文标识符报错）。
goalViewLabel :: GoalView -> String
goalViewLabel v = case v of
  ViewScore _ -> "分数"
  ViewCollect c _ -> "收集" ++ colorLabel c ++ "色宝石"
  ViewCollectMulti cs -> "多色收集（" ++ intercalate " / " [colorLabel c | (c, _) <- cs] ++ "）"
  ViewCount k _ -> countLabel k
  ViewOther _ -> "综合目标"

-- | 计数键的中文名（名字目标查 'namedGoalLabelTable'）。
countLabel :: CounterKey -> String
countLabel k = case k of
  CountStones -> "碎石"
  CountChests -> "宝箱"
  CountHoney -> "蜂蜜罐"
  CountBalloons -> "气球"
  CountCookies -> "饼干"
  CountCakes -> "蛋糕"
  CountSafes -> "保险箱"
  CountUfo -> "飞碟"
  CountCarpets -> "地毯"
  CountSpirits -> "时间精灵"
  CountColor c -> colorLabel c ++ "色宝石"
  CountNamed name -> fromMaybe (unElementName name) (lookup (unElementName name) namedGoalLabelTable)

-- | 按元素名计数的目标（'GoalNamed'）的中文名（键 = 元素名）。新元素做成关卡目标时在这里登记一行
-- （stack test 的 frontends_read_view_model 与 outcome_lose_hint_no_internal_names 核对全部关卡目标都有中文名）。
namedGoalLabelTable :: [(String, String)]
namedGoalLabelTable =
  [ ("jelly", "果冻")          -- 段 5：双层果冻（第 39 关）
  , ("bubble", "气泡")         -- 段 5：气泡（第 40 关）
  , ("magic_stone", "魔法石")  -- 新玩法 2
  , ("fuzzball", "毛球")       -- 新玩法 3：毛球（第 43 关）
  , (unElementName snowBossName, "雪怪")  -- 新玩法 5：雪怪 Boss（第 45 关，目标 = 击败 Boss）
  , (unElementName chameleonName, "变色龙")  -- 新玩法 7：变色龙（第 47 关）
  ]

-- | 颜色的中文名（与图例、ui-art.md 的颜色表一致）。
colorLabel :: Color -> String
colorLabel C1 = "红"
colorLabel C2 = "绿"
colorLabel C3 = "蓝"
colorLabel C4 = "黄"
colorLabel C5 = "紫"
