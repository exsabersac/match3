{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 按引擎阶段收缩的元素 API（slim 草图落地）：与 Ability/Kind 并行，逐步迁入后删旧方法。
-- Meta 并进 'Codec'（设计拍板推荐）；Ground 不进本类（纯 Mechanic）。
-- 'NearEdit' 生产路径仍经 Kind.onNear；本模块的 'NearEdit' 仅作阶段 API 形状，白名单组合子后续收紧。
module Match3.Element.Phase
  ( Physics(..)
  , MatchRule(..)
  , Hit(..)
  , HitOut(..)
  , Face(..)
  , Meta(..)
  , Codec(..)
  , NearRule(..)
  , Phase(..)
  , gemMatch
  , gemPhysics
  , fixedPhysics
  , gemHit
  , emptyFace
  , emptyMeta
  , noNear
  , Beat(..)
  , MechLayout(..)
  , emptyLayout
  ) where

import Match3.Element.Ability (Strike(..))
import Match3.Element.Kind (DieOrder(..), NearCtx, NearOut(..), Reach(..))
import Match3.Element.Types (CounterKey, Edge, ElementName, Placer)
import Match3.Types (Board, Cell, Color, MovesLeft, Outcome, Pos, Score)

-- | 物理（pFixed=True 时其余字段无意义，缺省宝石用 gemPhysics）。
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

-- | 匹配规则。
data MatchRule = MatchRule
  { mColor :: Maybe Color
  , mBlockMatch :: Bool
  , mBlockSwap :: Bool
  , mHintable :: Bool
  }
  deriving (Eq, Show)

-- | 命中种类（目前仅直接命中；后续可扩）。
data Hit = DirectHit
  deriving (Eq, Show)

-- | 命中结果。
data HitOut e = HitOut
  { hStrike :: Strike
  , hFires :: Bool
  , hBlast :: Maybe (Board -> Pos -> [Pos])
  , hNext :: Maybe e
  }

-- | 显示 / HUD。
data Face = Face
  { fTag :: String
  , fFields :: [(String, String)]
  , fLabel :: Maybe String
  , fGoalIcon :: Maybe String
  , fBossHp :: Board -> Maybe Int
  }

-- | 计数与目标元数据（并进 Codec）。
data Meta = Meta
  { metaCounter :: Maybe CounterKey
  , metaDiffWeight :: Int
  , metaVacatesCarpet :: Bool
  , metaDiffCounter :: Maybe CounterKey
  , metaBonusMoves :: Int
  }
  deriving (Eq, Show)

-- | 编解码 + Meta。
data Codec e = Codec
  { cName :: ElementName
  , cToCell :: e -> Cell
  , cFromCell :: Cell -> Maybe e
  , cPlace :: Placer
  , cMeta :: Meta
  }

-- | 邻格规则挂件（优先级 / 范围 / 打碎序）。
data NearRule = NearRule
  { nrPrio :: Int
  , nrReach :: Reach
  , nrDie :: DieOrder
  }
  deriving (Eq, Show)

gemMatch :: Maybe Color -> MatchRule
gemMatch mc = MatchRule mc False False True

gemPhysics :: Physics
gemPhysics = Physics False True True True True False []

gemHit :: HitOut e
gemHit = HitOut Destroy True Nothing Nothing

emptyFace :: String -> Face
emptyFace t = Face t [] Nothing Nothing (const Nothing)

emptyMeta :: Meta
emptyMeta = Meta Nothing 1 False Nothing 0

noNear :: NearRule -> NearCtx -> e -> NearOut
noNear _ _ _ = NearIdle

-- | 阶段 API（目标七方法；与 Ability 同义词 Element 并存时本类叫 Phase）。
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

--------------------------------------------------------------------------------
-- Mechanic 节拍（GADT）与布局；生产路径仍走旧 Mechanic 方法，逐步迁入。

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
