{-# LANGUAGE DeriveGeneric #-}

-- | 宝石颜色（第 5 刀从 Match3.Types 拆出的叶子模块，Types 原名再导出）。
-- 拆出来是为了让计数键（Match3.Counts 的 CountColor）与目标（Match3.Goal）能引用颜色，
-- 而 Match3.Types 又能引用目标，不成环。
module Match3.Color
  ( Color(..)
  , allColors
  ) where

import GHC.Generics (Generic)

data Color = C1 | C2 | C3 | C4 | C5
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

allColors :: [Color]
allColors = [minBound .. maxBound]
