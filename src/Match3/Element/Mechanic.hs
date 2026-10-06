{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 关卡级机制（飞碟 / 皮带 / 传送门 / 地毯 / 地面层 / 规则开关 / 掉落口）：不在格子里的机制，状态在值里。
--
-- 类只有两个方法：类型级的 'layout'（名字、是否核心、各读数的取法；每种一份静态记录，读名字不分配）
-- 与 'onBeat'（GADT 'Beat' 的每个节拍，含开局 'Start'）。
-- 'mechName' / 'mechCore' / 'mechStart' 是读它们的自由函数。折叠在 Match3.Element.Level。
--
-- 地面层 'GroundLayer' 是核心机制（'mechCore' / 'mlCore'），定义在这里；地面层种类（Match3.Element.Kind 的
-- 'GroundKind' 记录）由它在 'GroundHit' 节拍上消费。
module Match3.Element.Mechanic
  ( Mechanic(..)
  , mechName
  , mechCore
  , mechStart
  , layoutOf
  , SomeMechanic(..)
  , mechNameOf
  , fromMechanic
  , GroundRule
  , Morph(..)
  , GroundLayer(..)
  , Beat(..)
  , MechLayout(..)
  , emptyLayout
  ) where

import Data.Typeable (Typeable, cast)
import Match3.Board.Grid (MBoard)
import Match3.Board.Refill (RefillPolicy)
import Match3.Conveyor (Belt)
import Match3.Element.Phase (sameTypeEq)
import Match3.Element.Types (ShapeRule)
import Match3.Levels.Level (Level(..))
import Match3.Types
import Match3.Ufo (Ufo)

type GroundRule = [Pos] -> Ground -> (Ground, [(ElementName, Int)])

data Morph = Morph
  { morphName  :: ElementName
  , morphCells :: [(Pos, Pos, Cell)]
  , morphSeeds :: [Pos]
  }

-- | 流水线节拍：每种构造子对应原先一个有类型的方法。
data Beat r where
  Start     :: Level -> Beat ()  -- ^ 开局：按关卡记录给出初始状态（不回复 = 原型值原样）
  Refilled  :: Board -> [Pos] -> Beat [Pos]
  EndTick   :: [(Pos, Pos)] -> Beat [(Pos, Pos)]
  Settling  :: (Cell -> Bool) -> MBoard -> Beat MBoard
  Covering  :: [Pos] -> Int -> Beat Int
  GroundHit :: GroundRule -> [Pos] -> [(ElementName, Int)] -> Beat [(ElementName, Int)]
  AskRefill :: RefillPolicy -> Beat RefillPolicy
  AskShapes :: [ShapeRule] -> Beat [ShapeRule]
  AskAvoid  :: [Pos] -> Beat [Pos]
  AskWall   :: [Pos] -> Beat [Pos]
  AskMorph  :: Board -> Board -> Pos -> Pos -> Beat Morph
  AskJudge  :: Board -> Score -> MovesLeft -> Outcome -> Beat Outcome

-- | 机制的身份与读数（类型级：每种机制一份静态记录，读数是取自机制值的函数）。
-- slim-10 起是类型级的：值级记录每次读名字都要分配一份，而名字在每个节拍上都要按名合并（Golden 分配 +5%）。
data MechLayout m = MechLayout
  { mlName :: m -> ElementName
  , mlCore :: Bool
  , mlUfos :: m -> Maybe [Ufo]
  , mlBelts :: m -> Maybe [Belt]
  , mlPortals :: m -> Maybe [(Pos, Pos)]
  , mlCarpetOpen :: m -> Maybe [Pos]
  , mlGround :: m -> Maybe Ground
  , mlDrops :: m -> Maybe [Pos]
  }

-- | 只有名字、不提供任何读数。
emptyLayout :: ElementName -> MechLayout m
emptyLayout n = MechLayout (const n) False none none none none none none
  where
    none = const Nothing

-- | 关卡级机制：'layout' + 'onBeat'。
class (Typeable m, Eq m, Show m) => Mechanic m where
  -- | 身份与读数（名字、是否核心、飞碟 / 皮带 / … 的当前状态怎么取）。
  layout :: MechLayout m
  -- | 流水线节拍（唯一钩子）：Nothing = 本节拍不参与。
  onBeat :: Beat r -> m -> Maybe (r, m)
  onBeat _ _ = Nothing

mechName :: forall m. Mechanic m => m -> ElementName
mechName = mlName (layout @m)

-- | 核心机制（removeMechanic 也去不掉，总参与）。
mechCore :: forall m. Mechanic m => m -> Bool
mechCore _ = mlCore (layout @m)

-- | 按机制值读 layout 的一项：@layoutOf mlUfos m@。
layoutOf :: forall m r. Mechanic m => (MechLayout m -> m -> r) -> m -> r
layoutOf f = f (layout @m)

-- | 开局状态：'Start' 节拍的回复；不回复 = 原值。
mechStart :: Mechanic m => Level -> m -> m
mechStart lvl m = maybe m snd (onBeat (Start lvl) m)


data SomeMechanic = forall m. Mechanic m => SomeMechanic m

instance Eq SomeMechanic where
  SomeMechanic a == SomeMechanic b = sameTypeEq a b

instance Show SomeMechanic where
  showsPrec d (SomeMechanic m) = showsPrec d m

mechNameOf :: SomeMechanic -> ElementName
mechNameOf (SomeMechanic m) = mechName m

fromMechanic :: Mechanic m => SomeMechanic -> Maybe m
fromMechanic (SomeMechanic m) = cast m

newtype GroundLayer = GroundLayer Ground
  deriving (Eq, Show)

instance Mechanic GroundLayer where
  layout = (emptyLayout "ground") { mlCore = True, mlGround = \(GroundLayer g) -> Just g }
  onBeat (Start lvl) _ = Just ((), GroundLayer (lvlGround lvl))
  onBeat (GroundHit hitG hits acc) (GroundLayer g) =
    let (g', cs) = hitG hits g
     in Just (acc ++ cs, GroundLayer g')
  onBeat _ _ = Nothing
