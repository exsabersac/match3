{-# LANGUAGE OverloadedStrings #-}
-- | 地面层元素：格子下面的层（SlotGround，在 GameState.gsGround 里）。
--
-- 共同特征：不占格、不挡交换 / 匹配、不随重力 / 洗牌 / 皮带移动；上方格子每被消除（或被边缘收走）一次
-- 受一次命中（groundRule：层数 → 新层数），每去一层按 counter 计 1。写回格子（toCell）只用于显示，
-- 地面层不进盘面。内置只有双层果冻（第 39 关）。
module Match3.Element.Builtin.Ground
  ( Jelly(..)
  , jellyEntry
  ) where

import Match3.Element.Caps
import Match3.Element.Registry
import Match3.Element.Types
import Match3.Types

-- | 双层果冻：地面层（在 gsGround 里，不占格、不挡交换 / 匹配、不随重力 / 洗牌移动）。
-- 上方格子每被消除（或被边缘收走）一次去一层；每去一层按 CountNamed "jelly" 计 1。
-- 值 = 层数（写回格子只用于显示，地面层不进盘面）。
newtype Jelly = Jelly Int
  deriving (Eq, Show)

instance Element Jelly where
  name _ = "jelly"
  toCell (Jelly n) = Custom "jelly" (CustomState n)
  caps _ = piece [ground (\n -> if n > 1 then Just (n - 1) else Nothing), counts (CountNamed "jelly")]

-- | 条目：地面层（关卡的地面层在关卡记录 lvlGround 里，开局时由关卡级元素 GroundLayer 的 levelStart 取进 gsLevelElems（读数 gsGround），不经放置表）。
jellyEntry :: Entry
jellyEntry = groundEntry (Jelly 2)
