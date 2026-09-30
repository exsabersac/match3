{-# LANGUAGE OverloadedStrings #-}
-- | 打破型障碍：占格本体，被直接命中或邻格真消除时削层 / 打碎 / 变成别的元素。
--
-- 共同特征：原型 Blocker（挡交换、不点火、会下落、洗牌保留），状态是层数或颜色；
-- 石头 / 宝箱 / 蜂蜜 / 蛋糕命中与邻消各削一层、末层消除；气球命中即破、邻格同色真消除打破；
-- 保险箱末层开成饼干；双面块命中翻成背面颜色的普通宝石；彩蛋命中即破，开启规则开出直线 / 炸弹。
-- 邻格规则顺序：石头 10 → 宝箱 20 → 蜂蜜 30 → 蛋糕 40 → 气球 50 → 保险箱 110。
module Match3.Element.Builtin.Obstacle
  ( StoneE(..)
  , ChestE(..)
  , HoneyE(..)
  , CakeE(..)
  , BalloonE(..)
  , SafeE(..)
  , FlipE(..)
  , SurpriseEgg(..)
  , stoneEntry
  , chestEntry
  , honeyEntry
  , cakeEntry
  , balloonEntry
  , safeEntry
  , flipEntry
  , surpriseEntry
  ) where

import Match3.Element.Builtin.Collectible (CookieE(..))
import Match3.Element.Builtin.Common (colorPlace, deadRule)
import Match3.Element.Builtin.Gem (PlainGem(..))
import Match3.Element.Caps
import Match3.Element.Registry
import Match3.Element.Types
import Match3.Obstacles
  ( chipAdjacentBalloonsExcept
  , chipAdjacentCakesExcept
  , chipAdjacentChestsExcept
  , chipAdjacentHoneyExcept
  , chipAdjacentSafesExcept
  , chipAdjacentStonesExcept
  , openSurprises
  )
import Match3.Types

-- | 石头：直接命中削一层、末层消除；邻消同样削层。
newtype StoneE = StoneE Int
  deriving (Eq, Show)

instance Element StoneE where
  name _ = "stone"
  toCell (StoneE n) = Stone n
  caps (StoneE n) = blocker [hit (chip n StoneE), onAdjacent 10 (deadRule chipAdjacentStonesExcept), counts CountStones]

-- | 宝箱：同石头。
newtype ChestE = ChestE Int
  deriving (Eq, Show)

instance Element ChestE where
  name _ = "chest"
  toCell (ChestE n) = Chest n
  caps (ChestE n) = blocker [hit (chip n ChestE), onAdjacent 20 (deadRule chipAdjacentChestsExcept), counts CountChests]

-- | 蜂蜜罐：同石头。
newtype HoneyE = HoneyE Int
  deriving (Eq, Show)

instance Element HoneyE where
  name _ = "honey"
  toCell (HoneyE n) = Honey n
  caps (HoneyE n) = blocker [hit (chip n HoneyE), onAdjacent 30 (deadRule chipAdjacentHoneyExcept), counts CountHoney]

-- | 蛋糕：同石头（层数 = 蛋糕层数）。
newtype CakeE = CakeE Int
  deriving (Eq, Show)

instance Element CakeE where
  name _ = "cake"
  toCell (CakeE n) = Cake n
  caps (CakeE n) = blocker [hit (chip n CakeE), onAdjacent 40 (deadRule chipAdjacentCakesExcept), counts CountCakes]

-- | 气球：命中即破；邻格同色真消除打破。
newtype BalloonE = BalloonE Color
  deriving (Eq, Show)

instance Element BalloonE where
  name _ = "balloon"
  toCell (BalloonE c) = Balloon c
  caps _ = blocker [breaks, onAdjacent 50 (deadRule chipAdjacentBalloonsExcept), counts CountBalloons]

-- | 保险箱：直接命中削一层，末层开成饼干；邻消削层；按个数差计「开启」；离格也算覆盖地毯。
newtype SafeE = SafeE Int
  deriving (Eq, Show)

instance Element SafeE where
  name _ = "safe"
  toCell (SafeE n) = Safe n
  caps (SafeE n) =
    blocker
      [ hit (Absorb (if n <= 1 then SomeElement CookieE else SomeElement (SafeE (n - 1))))
      , onAdjacent 110 (\ctx b -> AdjOut (fst (chipAdjacentSafesExcept b (acTrue ctx) (acDirect ctx))) [] [])
      , countsDiff CountSafes
      , vacates
      ]

-- | 双面块：按正面颜色匹配、可交换 / 改色 / 推动 / 过传送门；命中翻成背面颜色的普通宝石。
data FlipE = FlipE Color Color
  deriving (Eq, Show)

instance Element FlipE where
  name _ = "flip"
  toCell (FlipE f b) = Flip f b
  caps (FlipE f back) = blocker [colorIs f, swappable, teleports, pushes, recolors, hit (Absorb (SomeElement (PlainGem back)))]

-- | 彩蛋：占格障碍；命中即破；开启规则 = 邻格真消除 / 直接命中时开出直线 / 炸弹（本轮坐住）或 3×3 爆炸。
-- 现行规则里彩蛋开一次就开出，没有要跨轮保存的状态，所以值是无字段的。
data SurpriseEgg = SurpriseEgg
  deriving (Eq, Show)

instance Element SurpriseEgg where
  name _ = "surprise"
  toCell _ = Surprise
  caps _ = blocker [breaks, opens openSurprises]

-- | 多层障碍受直接命中：削一层，末层消除。
chip :: Element e => Int -> (Int -> e) -> Hit
chip n con
  | n <= 1 = Destroy
  | otherwise = Absorb (SomeElement (con (n - 1)))

-- | 放置：层数（缺省 1，至少 1）。
layersPlace :: (Int -> Cell) -> Placer
layersPlace con args _ = case args of
  [AInt n] -> Just (con (max 1 n))
  [] -> Just (con 1)
  _ -> Nothing

--------------------------------------------------------------------------------
-- 条目（槽位由原型推导 = cellSlot (toCell 原型)）

stoneEntry, chestEntry, honeyEntry, cakeEntry, balloonEntry, safeEntry, flipEntry, surpriseEntry :: Entry
stoneEntry = bodyEntry (StoneE 1) (\cell -> case cell of Stone n -> Just (StoneE n); _ -> Nothing) (layersPlace Stone)
chestEntry = bodyEntry (ChestE 1) (\cell -> case cell of Chest n -> Just (ChestE n); _ -> Nothing) (layersPlace Chest)
honeyEntry = bodyEntry (HoneyE 1) (\cell -> case cell of Honey n -> Just (HoneyE n); _ -> Nothing) (layersPlace Honey)
balloonEntry = bodyEntry (BalloonE C1) (\cell -> case cell of Balloon c -> Just (BalloonE c); _ -> Nothing) (colorPlace Balloon)
cakeEntry = bodyEntry (CakeE 1) (\cell -> case cell of Cake n -> Just (CakeE n); _ -> Nothing) (layersPlace Cake)
safeEntry = bodyEntry (SafeE 1) (\cell -> case cell of Safe n -> Just (SafeE n); _ -> Nothing) (layersPlace Safe)
flipEntry = bodyEntry (FlipE C1 C2) (\cell -> case cell of Flip f b -> Just (FlipE f b); _ -> Nothing) $ \args _ -> case args of
  [AColor f, AColor b] -> Just (Flip f b)
  _ -> Nothing
surpriseEntry = bodyEntry SurpriseEgg (\cell -> case cell of Surprise -> Just SurpriseEgg; _ -> Nothing) (\_ _ -> Just Surprise)
