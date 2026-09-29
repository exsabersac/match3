-- | 关卡目标（第 5 刀：目标数据化）。目标 = 一组配额，每项配额 = 度量（分数或 Counts 的一个计数键）+ 目标值；
-- 进度、是否达成、目标值都由这份数据统一算出，结算、HUD、窗口标题、网页版都读同一组函数
-- （第 5 刀前是 13 个构造器的 LevelGoal、12 个位置参数的 goalMetEx / goalProgressEx，
-- 以及结算 / HUD / 标题 / 网页各自一份「目标 → 计数字段」的 case）。
--
-- 依赖：Match3.Color、Match3.Counts。被 Match3.Types 再导出（关卡表在那里）。
-- 不变量（与第 5 刀前逐字相同，金标准与元素查询快照锁定）：
--   * 达成 ⟺ 每项配额的度量 ≥ 目标值；
--   * 进度：单项目标 = 度量本身（不截断，过关那一步可以超过目标值）；多项 = 各项 min 目标值 之和；
--   * Show 按第 5 刀前的构造器写法打印（GoalScore 300、GoalCollect C1 20、GoalClearStone 8、GoalNamed "jelly" 32 …）。
module Match3.Goal
  ( Meter(..)
  , Quota(..)
  , LevelGoal(..)
  , goalScore
  , goalCollect
  , goalColors
  , goalCount
  , GoalView(..)
  , goalView
  , meterValue
  , goalProgress
  , goalMet
  , goalTarget
  ) where

import Match3.Color (Color)
import Match3.Counts (CounterKey(..), Counts, countOf)

-- | 目标度量：分数，或 Counts 里的一个计数键（颜色 CountColor、石块 CountStones … 名字 CountNamed）。
data Meter = MeterScore | MeterCount CounterKey
  deriving (Eq, Ord, Show)

-- | 一项配额：度量达到目标值。
data Quota = Quota
  { quotaMeter  :: Meter
  , quotaTarget :: Int
  } deriving (Eq, Show)

-- | 关卡目标：全部配额达到即满足。内置关卡除「多色采集」外都只有一项。
newtype LevelGoal = LevelGoal { goalQuotas :: [Quota] }
  deriving (Eq)

-- | 分数达到 t。
goalScore :: Int -> LevelGoal
goalScore t = LevelGoal [Quota MeterScore t]

-- | 清除某色 n 个。
goalCollect :: Color -> Int -> LevelGoal
goalCollect c n = goalCount (CountColor c) n

-- | 多色采集：每色各达到配额（第 5 刀前的 GoalCollectMulti）。
goalColors :: [(Color, Int)] -> LevelGoal
goalColors reqs = LevelGoal [Quota (MeterCount (CountColor c)) n | (c, n) <- reqs]

-- | 某计数键达到 n（石块 / 宝箱 / 蜂蜜罐 / 气球 / 饼干 / 蛋糕 / 保险箱 / 飞碟 / 地毯 / 名字）。
goalCount :: CounterKey -> Int -> LevelGoal
goalCount k n = LevelGoal [Quota (MeterCount k) n]

-- | 前端 / 关卡装饰按目标的「形状」分派（图标、色调、文案、默认摆放）：单项分数、单色、多色、其余单个计数键。
data GoalView
  = ViewScore Int
  | ViewCollect Color Int
  | ViewCollectMulti [(Color, Int)]
  | ViewCount CounterKey Int
  | ViewOther [Quota]  -- ^ 以上都不是（混合分数与计数的多项等；内置关卡没有）
  deriving (Eq, Show)

goalView :: LevelGoal -> GoalView
goalView (LevelGoal qs) = case qs of
  [Quota MeterScore t] -> ViewScore t
  [Quota (MeterCount (CountColor c)) n] -> ViewCollect c n
  [Quota (MeterCount k) n] -> ViewCount k n
  _ | Just cs <- mapM colorQuota qs -> ViewCollectMulti cs
    | otherwise -> ViewOther qs
  where
    colorQuota (Quota (MeterCount (CountColor c)) n) = Just (c, n)
    colorQuota _ = Nothing

-- | 与第 5 刀前派生的 Show 逐字相同（元素查询快照对 GameState 的 show 取散列，网页版按 show 的首词取目标种类）。
instance Show LevelGoal where
  showsPrec d g = case goalView g of
    ViewScore t -> con "GoalScore" (showsPrec 11 t)
    ViewCollect c n -> con "GoalCollect" (showsPrec 11 c . showChar ' ' . showsPrec 11 n)
    ViewCollectMulti cs -> con "GoalCollectMulti" (showsPrec 11 cs)
    ViewCount k n -> case k of
      CountStones -> con "GoalClearStone" (showsPrec 11 n)
      CountChests -> con "GoalChest" (showsPrec 11 n)
      CountHoney -> con "GoalHoney" (showsPrec 11 n)
      CountBalloons -> con "GoalBalloon" (showsPrec 11 n)
      CountCookies -> con "GoalCookie" (showsPrec 11 n)
      CountCakes -> con "GoalCake" (showsPrec 11 n)
      CountSafes -> con "GoalSafe" (showsPrec 11 n)
      CountUfo -> con "GoalUfo" (showsPrec 11 n)
      CountCarpets -> con "GoalCarpet" (showsPrec 11 n)
      CountNamed s -> con "GoalNamed" (showsPrec 11 s . showChar ' ' . showsPrec 11 n)
      _ -> raw
    ViewOther _ -> raw
    where
      con name args = showParen (d >= 11) (showString name . showChar ' ' . args)
      raw = showParen (d >= 11) (showString "LevelGoal {goalQuotas = " . showsPrec 0 (goalQuotas g) . showChar '}')

-- | 一个度量的当前值。
meterValue :: Meter -> Int -> Counts -> Int
meterValue MeterScore score _ = score
meterValue (MeterCount k) _ counts = countOf k counts

-- | 目标进度（HUD / 标题 / 网页的主进度）：单项 = 度量值；多项 = 各项 min 目标值 之和。
goalProgress :: LevelGoal -> Int -> Counts -> Int
goalProgress (LevelGoal [q]) score counts = meterValue (quotaMeter q) score counts
goalProgress (LevelGoal qs) score counts =
  sum [min (quotaTarget q) (meterValue (quotaMeter q) score counts) | q <- qs]

-- | 目标是否达成：每项度量 ≥ 目标值。
goalMet :: LevelGoal -> Int -> Counts -> Bool
goalMet (LevelGoal qs) score counts = all (\q -> meterValue (quotaMeter q) score counts >= quotaTarget q) qs

-- | HUD 上的目标值（各项之和）。
goalTarget :: LevelGoal -> Int
goalTarget = sum . map quotaTarget . goalQuotas
