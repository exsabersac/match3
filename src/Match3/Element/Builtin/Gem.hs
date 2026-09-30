{-# LANGUAGE OverloadedStrings #-}
-- | 宝石：普通宝石与特殊块（直线 / 炸弹 / 彩虹）。
--
-- 共同特征：原型 Piece（可交换、按颜色匹配、能点火、会下落、可过传送门、命中即消、可改色 / 推动），
-- 盘面编码都是 Gem 格；特殊块另有爆炸范围与洗牌保留。彩虹取色的成对交换规则（srOrder 10）挂在彩虹上；
-- 第 8 刀起特殊 × 特殊合成不再挂在 line_h 上，改为注册表的组合表（Match3.Combos.builtinComboRules，并成 srOrder 20）。
-- 特殊块的形状规则表（'builtinShapeRules'：直线 5 → 彩虹、直线 4 → 横 / 竖消）也在这里。
module Match3.Element.Builtin.Gem
  ( PlainGem(..)
  , SpecialGem(..)
  , specialBlast
  , builtinShapeRules
  , ltBombRule
  , withBombShapes
  , plainGemEntry
  , specialEntry
  ) where

import Data.List (intersect)
import Match3.Board.Grid (inBounds)
import Match3.Element.Caps
import Match3.Element.Registry
import Match3.Element.Special (runShape)
import Match3.Element.Types
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types

-- | 普通宝石：除名字和写回格子外全用缺省能力（原型 Piece：可交换、能点火、命中即消、洗牌重排……；
-- 颜色缺省取自写回的格子）。
newtype PlainGem = PlainGem Color
  deriving (Eq, Show)

instance Element PlainGem where
  name _ = "gem"
  toCell (PlainGem c) = Gem c Normal 0 Nothing

-- | 特殊块（直线 / 炸弹 / 彩虹）：洗牌保留；直线与炸弹有爆炸范围；彩虹 / 直线各挂一条成对交换规则。
data SpecialGem = SpecialGem Color GemKind
  deriving (Eq, Show)

instance Element SpecialGem where
  name (SpecialGem _ k) = case k of
    LineH -> "line_h"
    LineV -> "line_v"
    Bomb -> "bomb"
    Rainbow -> "rainbow"
    Normal -> "gem"
  toCell (SpecialGem c k) = Gem c k 0 Nothing
  -- 洗牌保留；直线 / 炸弹有爆炸范围；彩虹不进普通匹配提示，挂彩虹取色（先于特殊合成 = 组合表的次序 20）
  caps (SpecialGem _ k) =
    piece $ [keepsOnShuffle | k /= Normal] ++ map explodes (maybe [] pure (specialBlast k))
      ++ [c | k == Rainbow, c <- [notHintable, onSwap (SwapRule 10 isRainbowSwap rainbowClearSeeds)]]

-- | 内置特殊块形状规则表（第 8 刀；顺序即优先级，与旧 spawnSpecials 相同）：每条连线取第一条认领它的规则——
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

-- | 按种类的爆炸范围。
specialBlast :: GemKind -> Maybe (Pos -> [Pos])
specialBlast k = case k of
  LineH -> Just (\(r, _) -> [(r, c) | c <- [0 .. boardSize - 1]])
  LineV -> Just (\(_, c) -> [(r, c) | r <- [0 .. boardSize - 1]])
  Bomb -> Just (\(r, c) -> [(rr, cc) | rr <- [r - 1 .. r + 1], cc <- [c - 1 .. c + 1], inBounds (rr, cc)])
  _ -> Nothing

-- | 条目：普通宝石（槽位 0；宝石不经关卡放置表放置）。
plainGemEntry :: Entry
plainGemEntry = bodyEntry (PlainGem C1) (\cell -> case cell of Gem c _ _ _ -> Just (PlainGem c); _ -> Nothing) noPlace

-- | 条目：按种类的特殊块（槽位由原型推导 = kindSlot）。
specialEntry :: GemKind -> Entry
specialEntry k = bodyEntry (SpecialGem C1 k) (\cell -> case cell of Gem c _ _ _ -> Just (SpecialGem c k); _ -> Nothing) noPlace

noPlace :: Placer
noPlace _ _ = Nothing
