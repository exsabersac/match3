{-# LANGUAGE OverloadedStrings #-}
-- | 邻格波及的共享类型（Kind 驱动与 Phase API 共用，避免模块环）。
-- slim-7：优先用 nearSelfBecomes / nearSelfSits / nearLocalEdit；原始 NearEdit 视为逃生口。
module Match3.Element.Near
  ( Reach(..)
  , Nudge(..)
  , DieOrder(..)
  , NearCtx(..)
  , NearOut(..)
  , NearRule(..)
  , nearSelfBecomes
  , nearSelfSits
  , nearLocalEdit
  ) where

import Match3.Board.Grid (setCell)
import Match3.ECS.Stage (NearWorld)
import Match3.Types (Board, Cell, Color, Pos)

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
  , ncWorld :: NearWorld
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

-- | slim-7 白名单组合子：只改自己这一格（Becomes 语义的盘面版，可附 sit）。
nearSelfBecomes :: NearCtx -> Cell -> NearOut
nearSelfBecomes ctx cell = NearEdit (setCell (ncBoard ctx) (ncSelf ctx) cell) [] []

-- | 改自己并坐住（果汁机满充产出等）。
nearSelfSits :: NearCtx -> Cell -> NearOut
nearSelfSits ctx cell = NearEdit (setCell (ncBoard ctx) (ncSelf ctx) cell) [] [ncSelf ctx]

-- | 局部编辑逃生口：调用方必须只改 self 邻接的 recolorable 格或自充能；任意整盘改写仍可编译但应避免新增。
nearLocalEdit :: Board -> [Pos] -> [Pos] -> NearOut
nearLocalEdit = NearEdit
