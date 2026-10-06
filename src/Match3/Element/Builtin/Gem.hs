{-# LANGUAGE OverloadedStrings #-}
-- | 宝石：普通宝石与特殊块（直线 / 炸弹 / 彩虹）。
--
-- 共同特征：能力全用「普通宝石」缺省（可交换、按颜色匹配、能点火、会下落、可过传送门、命中即消、可改色 / 推动），
-- 盘面编码都是 Gem 格；特殊块另有爆炸范围与洗牌保留。四种特殊块是同一个原型构造器 'specialArch' 按种类
-- 给出的四个原型（ecs-3 前是类型参数 + SpecialKind 类）。彩虹取色的成对交换 system（swOrder 10）挂在彩虹上；
-- 特殊 × 特殊合成走元素世界的组合表（Match3.Combos.builtinComboRules，并成 swOrder 20）。
-- 特殊块的形状规则表（'builtinShapeRules'：直线 5 → 彩虹、直线 4 → 横 / 竖消）也在这里。
module Match3.Element.Builtin.Gem
  ( gemArch
  , specialArch
  , lineHArch
  , lineVArch
  , bombArch
  , rainbowArch
  , gemColumn
  , specialBlast
  , builtinShapeRules
  , ltBombRule
  , withBombShapes
  ) where

import Data.List (intersect)
import Match3.ECS.Archetype
import Match3.ECS.Component
import Match3.Element.Special (runShape)
import Match3.ECS.Stage (SwapSys(..), SysDef(..))
import Match3.Element.Types
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types

-- | 宝石的存储列（状态 = 颜色）：只认给定种类的宝石格（冰层 / 叠层不看：它们由叠层解码先拆掉）。
gemColumn :: GemKind -> Column Color
gemColumn k = Column get (\c -> Gem c k 0 Nothing)
  where
    get cell = case cell of
      Gem c k' _ _ | k' == k -> Just c
      _ -> Nothing

-- | 普通宝石：普通棋子的组件（可交换、按颜色匹配、能点火、会下落、可过传送门、命中即消、可改色 / 推动）。
-- 宝石不经关卡放置表放置。
gemArch :: Archetype Color
gemArch = (archetype "gem" (gemColumn Normal))
  { aMatch = gemMatch . Just
  , aHit = const gemHit
  , aPhysics = const gemPhysics
  }

-- | 特殊块（直线 / 炸弹 / 彩虹）：种类是原型的参数，状态只是颜色。
-- 洗牌保留；直线 / 炸弹有爆炸范围（'Blast' 数据）；彩虹不进普通匹配提示，带彩虹取色的成对交换 system
-- （先于特殊合成 = 组合表的次序 20）。
specialArch :: GemKind -> Archetype Color
specialArch k = (archetype (specialName k) (gemColumn k))
  { aSpawn = \_ cell -> case cell of
      Gem c _ _ _ -> Just (Gem c k 0 Nothing)
      _ -> Nothing
  , aMatch = \c -> (gemMatch (Just c)) {mHintable = k /= Rainbow}
  , aHit = const gemHit {hBlast = specialBlast k}
  , aPhysics = const gemPhysics {pKeepShuffle = True}
  , aSystems = [SysSwap (SwapSys 10 isRainbowSwap rainbowClearSeeds) | k == Rainbow]
  }

lineHArch, lineVArch, bombArch, rainbowArch :: Archetype Color
lineHArch = specialArch LineH
lineVArch = specialArch LineV
bombArch = specialArch Bomb
rainbowArch = specialArch Rainbow

specialName :: GemKind -> ElementName
specialName k = case k of
  LineH -> "line_h"
  LineV -> "line_v"
  Bomb -> "bomb"
  Rainbow -> "rainbow"
  Normal -> "gem"

-- | 内置特殊块形状规则表（顺序即优先级）：每条连线取第一条认领它的规则——
-- 长度 ≥ 5 → 彩虹；长度 4 横连 → 横消；长度 4 竖连 → 竖消；长度 3 不生成。落点见 Match3.Element.Special.shapeAnchor。
-- 目前没有 L / T 形状的规则（交叉的横竖连线各按直线规则生成，后写的覆盖先写的）。
builtinShapeRules :: [ShapeRule]
builtinShapeRules =
  [ runShape "line5→rainbow" ((>= 5) . runLen) (const Rainbow)
  , runShape "line4h→line_h" (\r -> runLen r >= 4 && runIsH r) (const LineH)
  , runShape "line4v→line_v" (\r -> runLen r >= 4 && not (runIsH r)) (const LineV)
  ]
  where
    runLen = length . runPos

-- | L / T 形 → 炸弹（开心消消乐的「爆炸特效」）：同色的一横一竖两条连线交叉时，横线在交点放一颗炸弹
-- （交点这一轮真被挖空时；否则认领但不生成），竖线认领但不生成（否则竖线的直线规则会在交点上覆盖）。
-- 不交叉的连线不认领，交给后面的直线规则。不在内置表里：只在打开规则开关 "bomb_shapes" 的关卡里
-- 由关卡级元素 'Match3.Element.Builtin.Level.BombShapes' 经 'withBombShapes' 插进本关的形状表。
ltBombRule :: ShapeRule
ltBombRule = ShapeRule "l/t→bomb" spawn
  where
    spawn ctx run =
      case [x | o <- scRuns ctx, runColor o == runColor run, runIsH o /= runIsH run, x <- runPos run `intersect` runPos o] of
        [] -> Nothing
        (x : _)
          | runIsH run -> Just [(x, Gem (runColor run) Bomb 0 Nothing) | x `elem` scClearable ctx]
          | otherwise -> Just []

-- | 把 'ltBombRule' 插进形状表：放在「直线 5 → 彩虹」之后（五连仍出彩虹）、直线 4 规则之前（L / T 里有四连时出炸弹）；
-- 表里没有五连规则时放最前面。
withBombShapes :: [ShapeRule] -> [ShapeRule]
withBombShapes rules = case break ((== "line5→rainbow") . shapeName) rules of
  (pre, r5 : post) -> pre ++ [r5, ltBombRule] ++ post
  _ -> ltBombRule : rules

-- | 按种类的爆炸范围（数据；由 Match3.ECS.Component.blastArea 解释：整行 / 整列 / 3×3 裁到盘内）。
specialBlast :: GemKind -> Maybe Blast
specialBlast k = case k of
  LineH -> Just BlastRow
  LineV -> Just BlastCol
  Bomb -> Just BlastSquare
  _ -> Nothing
