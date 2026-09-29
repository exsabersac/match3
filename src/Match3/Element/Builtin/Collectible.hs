{-# LANGUAGE OverloadedStrings #-}
-- | 收集与计数类：离开盘面（被收走 / 打破）时按计数键记一笔，常作关卡目标。
--
-- 共同特征：原型 Blocker 的占格本体，本身不削层、不变形；饼干打不动、落到底边被收走（离格也算覆盖地毯）；
-- 时间精灵命中 / 邻消即破，按步前步后个数差每个奖励 2 步；气泡（Custom "bubble"）命中 / 邻格真消除即破，
-- 按 CountNamed "bubble" 计数。邻格规则顺序：时间精灵 120 → 气泡 170。
module Match3.Element.Builtin.Collectible
  ( CookieE(..)
  , TimeSpiritE(..)
  , Bubble(..)
  , cookieEntry
  , timeSpiritEntry
  , bubbleEntry
  ) where

import Match3.Board.Grid (getCell, inBounds)
import Match3.Element.Builtin.Common (deadRule)
import Match3.Element.Caps
import Match3.Element.Registry
import Match3.Element.Types
import Match3.Obstacles (chipAdjacentTimeSpiritsExcept, orthoNeighbors)
import Match3.Types

-- | 饼干：打不动；随重力下落、可过传送门，落到底边被收走；离格也算覆盖地毯。
data CookieE = CookieE
  deriving (Eq, Show)

instance Element CookieE where
  name _ = "cookie"
  toCell _ = Cookie
  caps _ = blocker [teleports, drainsAt [EdgeBottom], counts CountCookies, vacates]

-- | 时间精灵：命中 / 邻消即破，按个数差每个奖励 2 步。
data TimeSpiritE = TimeSpiritE
  deriving (Eq, Show)

instance Element TimeSpiritE where
  name _ = "time_spirit"
  toCell _ = TimeSpirit
  caps _ = blocker [breaks, onAdjacent 120 (deadRule chipAdjacentTimeSpiritsExcept), countsDiff CountSpirits, bonus 2]

-- | 气泡：占格本体 Custom "bubble" k。无色、挡交换、随重力下落、不穿传送门、洗牌保留；
-- 邻格有真消除（任意颜色）即破，直接命中也破；破掉计 CountNamed "bubble"。
newtype Bubble = Bubble Int
  deriving (Eq, Show)

instance Element Bubble where
  name _ = "bubble"
  toCell (Bubble k) = Custom "bubble" (CustomState k)
  caps _ = blocker [breaks, onAdjacent 170 bubbleAdjacent, counts (CountNamed "bubble")]

bubbleAdjacent :: AdjCtx -> Board -> AdjOut
bubbleAdjacent ctx b =
  let popped =
        [ q
        | q <- nubOrd [q' | p <- acTrue ctx, q' <- orthoNeighbors p, inBounds q']
        , q `notElem` acDirect ctx
        , q `notElem` acTrue ctx
        , isBubble (getCell b q)
        ]
  in AdjOut b popped []
  where
    isBubble cell = case cell of
      Custom "bubble" _ -> True
      _ -> False
    nubOrd = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

--------------------------------------------------------------------------------
-- 条目

cookieEntry, timeSpiritEntry, bubbleEntry :: Entry
cookieEntry = bodyEntry CookieE (\cell -> case cell of Cookie -> Just CookieE; _ -> Nothing) (\_ _ -> Just Cookie)
timeSpiritEntry = bodyEntry TimeSpiritE (\cell -> case cell of TimeSpirit -> Just TimeSpiritE; _ -> Nothing) (\_ _ -> Just TimeSpirit)
-- 气泡：Custom 本体，放置参数 = 值（缺省 1）。
bubbleEntry = customEntry (Bubble 1) (Bubble . unCustomState)
