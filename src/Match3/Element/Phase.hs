{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 按引擎阶段收缩的元素 API：生产行为的唯一来源（slim-3 起 Ability/Kind 邻格薄封装到此）。
-- Meta 并进 'Codec'；'NearEdit' 在 slim-7 收成白名单组合子。
module Match3.Element.Phase
  ( Physics(..)
  , fixedPhysics
  , gemPhysics
  , obstaclePhysics
  , MatchRule(..)
  , obstacleMatch
  , gemMatch
  , Hit(..)
  , Strike(..)
  , HitOut(..)
  , gemHit
  , immuneHit
  , Face(..)
  , emptyFace
  , Meta(..)
  , emptyMeta
  , Codec(..)
  , NearRule(..)
  , Phase(..)
  , noNear
  , phaseNearPrio
  , phaseReach
  , phaseDieOrder
  , phaseOnNear
  , Beat(..)
  , MechLayout(..)
  , emptyLayout
  ) where

import Match3.Element.Near
import Match3.Element.Types (CounterKey, Edge, ElementName, Placer)
import Match3.Types (Board, Cell, Color, MovesLeft, Outcome, Pos, Score)

-- | 直接命中反应（原 Ability.Strike）。
data Strike
  = Absorb Cell
  | Destroy
  | Immune
  deriving (Eq, Show)

data Physics = Physics
  { pFixed :: Bool
  , pFalls :: Bool
  , pPortal :: Bool
  , pRecolor :: Bool
  , pPush :: Bool
  , pKeepShuffle :: Bool
  , pDrains :: [Edge]
  }
  deriving (Eq, Show)

fixedPhysics :: Physics
fixedPhysics = Physics True False False False False True []

gemPhysics :: Physics
gemPhysics = Physics False True True True True False []

-- | 占格障碍：下落、洗牌保留、不过门、不改色 / 不推动。
obstaclePhysics :: Physics
obstaclePhysics = Physics False True False False False True []

data MatchRule = MatchRule
  { mColor :: Maybe Color
  , mBlockMatch :: Bool
  , mBlockSwap :: Bool
  , mHintable :: Bool
  }
  deriving (Eq, Show)

gemMatch :: Maybe Color -> MatchRule
gemMatch mc = MatchRule mc False False True

obstacleMatch :: MatchRule
obstacleMatch = MatchRule Nothing True True False

data Hit = DirectHit
  deriving (Eq, Show)

data HitOut e = HitOut
  { hStrike :: Strike
  , hFires :: Bool
  , hBlast :: Maybe (Board -> Pos -> [Pos])
  , hNext :: Maybe e
  }

gemHit :: HitOut e
gemHit = HitOut Destroy True Nothing Nothing

immuneHit :: HitOut e
immuneHit = HitOut Immune False Nothing Nothing

data Face = Face
  { fTag :: String
  , fFields :: [(String, String)]
  , fLabel :: Maybe String
  , fGoalIcon :: Maybe String
  , fBossHp :: Board -> Maybe Int
  }

emptyFace :: String -> Face
emptyFace t = Face t [] Nothing Nothing (const Nothing)

data Meta = Meta
  { metaCounter :: Maybe CounterKey
  , metaDiffWeight :: Int
  , metaVacatesCarpet :: Bool
  , metaDiffCounter :: Maybe CounterKey
  , metaBonusMoves :: Int
  }
  deriving (Eq, Show)

emptyMeta :: Meta
emptyMeta = Meta Nothing 1 False Nothing 0

data Codec e = Codec
  { cName :: ElementName
  , cToCell :: e -> Cell
  , cFromCell :: Cell -> Maybe e
  , cPlace :: Placer
  , cMeta :: Meta
  , cNear :: Maybe NearRule
  }

class Phase e where
  codec :: Codec e
  onSwap :: e -> [()]
  onMatch :: e -> MatchRule
  onHit :: Hit -> e -> HitOut e
  physics :: e -> Physics
  onNear :: NearRule -> NearCtx -> e -> NearOut
  view :: e -> Face
  onSwap _ = []
  onNear = noNear

noNear :: NearRule -> NearCtx -> e -> NearOut
noNear _ _ _ = NearIdle

phaseNearPrio :: forall e. Phase e => Maybe Int
phaseNearPrio = nrPrio <$> cNear (codec @e)

phaseReach :: forall e. Phase e => Reach
phaseReach = maybe SkipDirect nrReach (cNear (codec @e))

phaseDieOrder :: forall e. Phase e => DieOrder
phaseDieOrder = maybe DiePrepend nrDie (cNear (codec @e))

phaseOnNear :: forall e. Phase e => e -> NearCtx -> NearOut
phaseOnNear e ctx = case cNear (codec @e) of
  Nothing -> NearIdle
  Just rule -> onNear rule ctx e

data Beat r where
  Refilled :: Board -> [Pos] -> Beat [Pos]
  EndTick :: [(Pos, Pos)] -> Beat [(Pos, Pos)]
  AskJudge :: Board -> Score -> MovesLeft -> Outcome -> Beat Outcome

data MechLayout = MechLayout
  { mlName :: ElementName
  , mlCore :: Bool
  }
  deriving (Eq, Show)

emptyLayout :: ElementName -> MechLayout
emptyLayout n = MechLayout n False
