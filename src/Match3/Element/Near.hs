{-# LANGUAGE OverloadedStrings #-}
-- | 邻格波及的共享类型（Kind 驱动与 Phase API 共用，避免模块环）。
module Match3.Element.Near
  ( Reach(..)
  , Nudge(..)
  , DieOrder(..)
  , NearCtx(..)
  , NearOut(..)
  , NearRule(..)
  ) where

import Match3.Element.Types (AdjCtx)
import Match3.Types (Board, Color, Pos, Cell)

-- | 邻格波及跳不跳过本轮被直接命中的格。
data Reach
  = SkipDirect
  | AllNeighbours
  deriving (Eq, Show)

-- | 邻格有真消除时，这一格怎么变。
data Nudge
  = Untouched
  | Becomes Cell
  | Dies
  deriving (Eq, Show)

-- | 打碎格写入 aoDead 的次序。
data DieOrder = DiePrepend | DieAppend
  deriving (Eq, Show)

data NearCtx = NearCtx
  { ncTriggers :: [(Pos, Maybe Color)]
  , ncSelf :: Pos
  , ncAdj :: AdjCtx
  , ncBoard :: Board
  }

data NearOut
  = NearIdle
  | NearNudge Nudge
  | NearEdit Board [Pos] [Pos]
  deriving (Eq, Show)

data NearRule = NearRule
  { nrPrio :: Int
  , nrReach :: Reach
  , nrDie :: DieOrder
  }
  deriving (Eq, Show)
