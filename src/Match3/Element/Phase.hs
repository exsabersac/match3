{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 按引擎阶段收缩的元素 API：值级行为的唯一来源（slim-9 删除了旧的 Ability 六类）。
-- Meta 并进 'Codec'；邻格编辑优先 Match3.Element.Near 白名单组合子（nearSelf* / nearLocalEdit）。
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
  , noFace
  , baseFace
  , Meta(..)
  , emptyMeta
  , Codec(..)
  , Hud(..)
  , noHud
  , SysDef(..)
  , NearRule(..)
  , Phase(..)
  , noNear
  , phaseNearPrio
  , phaseReach
  , phaseDieOrder
  , phaseOnNear
  , SomePhase(..)
  , phaseName
  , phaseToCell
  , Inert(..)
  , toCell
  , nameOf
  , intCell
  , sameTypeEq
  , fromPhase
  , phaseProbe
  , somePhaseProbe
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Typeable (Typeable, cast)
import Match3.Element.Near
import Match3.ECS.Stage (SysDef(..))
import Match3.Element.Types (CellField, CounterKey, Edge, FaceValue, Placer)
import Match3.Types (Board, Cell, CellContents(..), Color(..), CustomState(..), ElementName(..), GemKind(..), Pos, gridFromRows)

-- | 直接命中反应。
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
obstacleMatch = MatchRule Nothing True True True

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

-- | 显示：前端格子的类型标签与基本字段（Nothing = View 的缺省：Custom 格 ("custom", name / v)，其余 = 元素名）
-- 与附加字段（网页格子 JSON 追加在基本字段之后）。只影响显示，不参与规则。
data Face = Face
  { fBase :: Maybe (String, [(String, CellField)])
  , fExtras :: [(String, FaceValue)]
  }

-- | 没有自己的标签、没有附加字段。
noFace :: Face
noFace = Face Nothing []

-- | 只有基本字段的显示。
baseFace :: String -> [(String, CellField)] -> Face
baseFace t fs = Face (Just (t, fs)) []

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

-- | HUD / 目标显示（slim-10 起由 Kind 的五个方法并来；全是数据，缺省 'noHud' = 都不提供）。
data Hud = Hud
  { hudLabel :: Maybe String               -- ^ 按元素名计数的目标（CountNamed）的中文名（HUD / 网页 / 失败提示）
  , hudLoseHint :: Maybe (Int -> String)   -- ^ 该目标的失败提示（参数 = 目标值）；缺省「消除<中文名>，目标 n 个」
  , hudGoalIcon :: Maybe String            -- ^ 目标图标贴图名（覆盖默认的元素名）
  , hudBossHp :: Maybe (Board -> Int)      -- ^ HUD 血条：盘上剩余 HP（View 按目标名查世界，不点名具体元素）
  }

noHud :: Hud
noHud = Hud Nothing Nothing Nothing Nothing

-- | 一种元素的类型级数据：名字、格子编解码、放置、计数元数据、邻格规则、HUD、自带的 system（Match3.ECS.Stage）。
data Codec e = Codec
  { cName :: ElementName
  , cToCell :: e -> Cell
  , cFromCell :: Cell -> Maybe e
  , cPlace :: Placer
  , cMeta :: Meta
  , cNear :: Maybe NearRule
  , cHud :: Hud
  , cSystems :: [SysDef]
  }

class (Typeable e, Eq e, Show e) => Phase e where
  codec :: Codec e
  onMatch :: e -> MatchRule
  onHit :: Hit -> e -> HitOut e
  physics :: e -> Physics
  onNear :: NearRule -> NearCtx -> e -> NearOut
  view :: e -> Face
  -- | 值级 Meta（缺省 = codec 的 cMeta；雪怪等权重随值变时覆盖）。
  liveMeta :: e -> Meta
  onNear = noNear
  liveMeta _ = cMeta (codec @e)

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

-- | 装箱的 Phase 值（slim-8：Registry 热路径解码）。
data SomePhase = forall e. Phase e => SomePhase e

phaseName :: SomePhase -> ElementName
phaseName (SomePhase e) = phaseNameOf e

phaseToCell :: SomePhase -> Cell
phaseToCell (SomePhase e) = phaseCellOf e

phaseNameOf :: forall e. Phase e => e -> ElementName
phaseNameOf e = case cast e of
  Just (Inert n _) -> n
  Nothing -> cName (codec @e)

phaseCellOf :: forall e. Phase e => e -> Cell
phaseCellOf e = cToCell (codec @e) e

-- | 写回格子（= codec 的 cToCell）。
toCell :: forall e. Phase e => e -> Cell
toCell = cToCell (codec @e)

-- | 元素名（惰性占格取它记下的名字，其余 = codec 的 cName）。
nameOf :: Phase e => e -> ElementName
nameOf = phaseNameOf

-- | Int newtype 的自定义元素写回：@Custom 名字 状态值@。
intCell :: Coercible e Int => ElementName -> e -> Cell
intCell n e = Custom n (CustomState (coerce e))

-- | 同类型才可能相等（装箱值的 Eq）。
sameTypeEq :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
sameTypeEq a b = maybe False (== b) (cast a)

-- | 拆箱（类型对上才有值）。
fromPhase :: Typeable e => SomePhase -> Maybe e
fromPhase (SomePhase e) = cast e

-- | 全部值级方法在一个元素上的读数（透明性测试用：装箱 / 叠层前后逐项比较；爆炸范围在 8×8 空盘的 (3,3) 上取）。
phaseProbe :: Phase e => e -> [(String, String)]
phaseProbe e =
  [ ("name", show (nameOf e))
  , ("toCell", show (toCell e))
  , ("mColor", show (mColor m))
  , ("mBlockMatch", show (mBlockMatch m))
  , ("mBlockSwap", show (mBlockSwap m))
  , ("mHintable", show (mHintable m))
  , ("hStrike", show (hStrike h))
  , ("hFires", show (hFires h))
  , ("hBlast", show (fmap (\f -> f probeBoard (3, 3)) (hBlast h)))
  , ("physics", show (physics e))
  , ("meta", show (liveMeta e))
  , ("fBase", show (fBase (view e)))
  , ("fExtras", show (fExtras (view e)))
  ]
  where
    m = onMatch e
    h = onHit DirectHit e
    probeBoard = gridFromRows (replicate 8 (replicate 8 (Gem C1 Normal 0 Nothing)))

somePhaseProbe :: SomePhase -> [(String, String)]
somePhaseProbe (SomePhase e) = phaseProbe e

instance Eq SomePhase where
  SomePhase a == SomePhase b = sameTypeEq a b

-- | 稳定的显示：元素名 + 状态值的 Show。
instance Show SomePhase where
  showsPrec d (SomePhase e) =
    showParen (d > 10) (showString "SomePhase " . showsPrec 11 (nameOf e) . showChar ' ' . showsPrec 11 e)

-- | 惰性占格：障碍的缺省、无色，写回原来的格子。解码兜底（未注册的 Custom 名字、没有种类认领的格子）。
data Inert = Inert ElementName Cell
  deriving (Eq, Show)

instance Phase Inert where
  codec = Codec
    { cName = ElementName "inert"
    , cToCell = \(Inert _ cell) -> cell
    , cFromCell = const Nothing
    , cPlace = \_ _ -> Nothing
    , cMeta = emptyMeta
    , cNear = Nothing
    , cHud = noHud
    , cSystems = []
    }
  onMatch _ = obstacleMatch
  onHit _ _ = immuneHit
  physics _ = obstaclePhysics
  view _ = noFace
