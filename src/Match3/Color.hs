{-# LANGUAGE DeriveGeneric #-}

-- | 宝石颜色（第 5 刀从 Match3.Types 拆出的叶子模块，Types 原名再导出；第 6 刀 numColors / colorAt 也移到这里）。
-- 拆出来是为了让计数键（Match3.Counts 的 CountColor）与目标（Match3.Goal）能引用颜色，
-- 而 Match3.Types 又能引用目标，不成环。
module Match3.Color
  ( Color(..)
  , allColors
  , numColors
  , colorAt
  ) where

import GHC.Generics (Generic)

data Color = C1 | C2 | C3 | C4 | C5
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

allColors :: [Color]
allColors = [minBound .. maxBound]

numColors :: Int
numColors = 5

-- | 第 i 种颜色（按 allColors 顺序，下标对颜色数取模，负数也落在范围内）。
-- 取代 toEnum 构造颜色：总函数，不会因越界报错。
colorAt :: Int -> Color
colorAt i = case drop (i `mod` length allColors) allColors of
  c : _ -> c
  [] -> minBound
