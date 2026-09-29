-- | 宝石：普通宝石与特殊块（直线 / 炸弹 / 彩虹）。
--
-- 共同特征：原型 Piece（可交换、按颜色匹配、能点火、会下落、可过传送门、命中即消、可改色 / 推动），
-- 盘面编码都是 Gem 格；特殊块另有爆炸范围与洗牌保留。两条成对交换规则也挂在这里：
-- 彩虹取色（srOrder 10，挂在彩虹上）→ 特殊 × 特殊合成（srOrder 20，挂在 line_h 上，规则自己检查两端）。
module Match3.Element.Builtin.Gem
  ( PlainGem(..)
  , SpecialGem(..)
  , specialBlast
  , plainGemEntry
  , specialEntry
  ) where

import Match3.Board.Grid (inBounds)
import Match3.Combos (comboClearSeeds, isSpecialCombo)
import Match3.Element.Class
import Match3.Element.Registry
import Match3.Element.Types
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types

-- | 普通宝石：除名字和写回格子外全用默认方法（原型 Piece：可交换、能点火、命中即消、洗牌重排……；
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
  keepOnShuffle (SpecialGem _ k) = k /= Normal
  blast (SpecialGem _ k) = specialBlast k
  -- 彩虹不进普通匹配提示（只经成对交换规则给提示）
  hintable (SpecialGem _ k) = k /= Rainbow
  swapRule (SpecialGem _ k) = case k of
    -- 彩虹取色：由交换对象决定清哪种颜色；先于特殊合成判定
    Rainbow -> Just (SwapRule 10 isRainbowSwap rainbowClearSeeds)
    -- 特殊 × 特殊合成：挂在直线上，规则本身检查两端（直线 / 炸弹 / 彩虹的组合）
    LineH -> Just (SwapRule 20 isSpecialCombo comboClearSeeds)
    _ -> Nothing

-- | 按种类的爆炸范围。
specialBlast :: GemKind -> Maybe (Pos -> [Pos])
specialBlast k = case k of
  LineH -> Just (\(r, _) -> [(r, c) | c <- [0 .. boardSize - 1]])
  LineV -> Just (\(_, c) -> [(r, c) | r <- [0 .. boardSize - 1]])
  Bomb -> Just (\(r, c) -> [(rr, cc) | rr <- [r - 1 .. r + 1], cc <- [c - 1 .. c + 1], inBounds (rr, cc)])
  _ -> Nothing

-- | 条目：普通宝石（槽位 0；宝石不经关卡放置表放置）。
plainGemEntry :: Entry
plainGemEntry = bodyEntry 0 (PlainGem C1) (\cell -> case cell of Gem c _ _ _ -> Just (PlainGem c); _ -> Nothing) noPlace

-- | 条目：按种类的特殊块（槽位 = kindSlot）。
specialEntry :: GemKind -> Entry
specialEntry k = bodyEntry (kindSlot k) (SpecialGem C1 k) (\cell -> case cell of Gem c _ _ _ -> Just (SpecialGem c k); _ -> Nothing) noPlace

noPlace :: Placer
noPlace _ _ = Nothing
