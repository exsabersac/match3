-- | 特殊块规则表的解释器（第 8 刀）：形状规则表（'ShapeRule'：匹配形状 → 生成哪种特殊块）与
-- 组合规则表（'ComboRule'：两个特殊块交换时的组合效果）。规则表本身是数据，挂在元素世界上
-- （Registry.shapeRules / comboRules；内置表见 Element.Builtin.Gem.builtinShapeRules 与 Match3.Combos.builtinComboRules）。
-- 新形状 / 新组合只需往表里加一条，主流程（Board.Clear 的特殊块生成、Game.Move 的交换起手、提示）不用改。
--
-- 依赖：Element.Types、Match3.Types。不含任何具体规则。
module Match3.Element.Special
  ( -- * 形状规则
    spawnByShapes
  , shapeAnchor
  , runShape
    -- * 组合规则
  , comboOrder
  , comboMatch
  , comboFires
  , comboSeedsFor
  , comboSwapSystem
  ) where

import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Match3.ECS.Stage (SwapSys(..))
import Match3.Element.Types
import Match3.Types

--------------------------------------------------------------------------------
-- 形状规则

-- | 按有序形状规则表生成特殊块：每条连线（按给出的顺序）取第一条认领它的规则的产出，依次拼接。
-- 调用方（Board.Clear）按列表顺序写回，后写的覆盖先写的。
spawnByShapes :: [ShapeRule] -> Maybe Pos -> [MatchRun] -> [Pos] -> [(Pos, Cell)]
spawnByShapes rules prefer runs clearable =
  concat [fromMaybe [] (listToMaybe (mapMaybe (\r -> shapeSpawn r ctx run) rules)) | run <- runs]
  where
    ctx = ShapeCtx prefer runs clearable

-- | 在若干格里挑特殊块的落点（至多一个）：只考虑本轮真正挖空的格（冰 > 1 / 翻转等留在盘面上的格不放）；
-- 玩家交换落点在其中就放那里，否则放这些格的中间一个；没有可放的格时为空。
shapeAnchor :: ShapeCtx -> [Pos] -> [Pos]
shapeAnchor ctx ps = take 1 $ case scPrefer ctx of
  Just p | p `elem` slots -> [p]
  _ -> drop (length slots `div` 2) slots
  where
    slots = filter (`elem` scClearable ctx) ps

-- | 只看单条连线的形状规则：连线满足条件时认领它，在连线的落点（'shapeAnchor'）上放一颗该色的特殊宝石。
runShape :: String -> (MatchRun -> Bool) -> (MatchRun -> GemKind) -> ShapeRule
runShape n ok kind = ShapeRule n spawn
  where
    spawn ctx run
      | ok run = Just [(p, Gem (runColor run) (kind run) 0 Nothing) | p <- shapeAnchor ctx (runPos run)]
      | otherwise = Nothing

--------------------------------------------------------------------------------
-- 组合规则

-- | 组合规则表并进成对交换规则时的次序（彩虹单端交换是 10，先于组合）。
comboOrder :: Int
comboOrder = 20

-- | 第一条对得上的规则与两端的对应：按表顺序，每条先试 (p1, p2) 再试 (p2, p1)。
comboMatch :: [ComboRule] -> Board -> Pos -> Pos -> Maybe (ComboRule, Pos, Pos)
comboMatch rules b p1 p2 =
  listToMaybe
    [ (r, a, c)
    | r <- rules
    , (a, c) <- [(p1, p2), (p2, p1)]
    , comboFirst r (boardAt b a)
    , comboSecond r (boardAt b c)
    ]

-- | 组合是否成立（交换前盘面）：表里有对得上的规则，且两端都能发火（specialActivates：软锁不发火）。
comboFires :: [ComboRule] -> Board -> Pos -> Pos -> Bool
comboFires rules b p1 p2 =
  isJust (comboMatch rules b p1 p2)
    && specialActivates (boardAt b p1)
    && specialActivates (boardAt b p2)

-- | 组合的清除种子（交换后盘面）：第一条对得上的规则给出；没有对得上的为空。
comboSeedsFor :: [ComboRule] -> Board -> Pos -> Pos -> [Pos]
comboSeedsFor rules b p1 p2 = maybe [] (\(r, a, c) -> comboSeeds r b a c) (comboMatch rules b p1 p2)

-- | 整张组合表当作一条成对交换规则（次序 'comboOrder'）。
comboSwapSystem :: [ComboRule] -> SwapSys
comboSwapSystem rules = SwapSys comboOrder (comboFires rules) (comboSeedsFor rules)
