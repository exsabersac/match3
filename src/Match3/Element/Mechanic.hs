{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 关卡级机制（飞碟 / 皮带 / 传送门 / 地毯 / 地面层 / 规则开关 / 掉落口）：不在格子里的机制，状态在值里。
--
-- slim-5：生产节拍收成 'onBeat'（GADT 'Beat'）+ 'layout'；旧的分方法是薄封装（调 onBeat / 读 layout），
-- slim-6 再删。折叠仍在 Match3.Element.Level。
--
-- 地面层 'GroundLayer' 是核心机制（'mechCore' / 'mlCore'），定义在这里。
module Match3.Element.Mechanic
  ( Mechanic(..)
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
import Match3.Element.Ability (sameTypeEq)
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

-- | 流水线节拍（slim-5）：每种构造子对应原先一个有类型的方法。
data Beat r where
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

-- | 机制的静态读数与身份（slim-5）：替代 ufos/belts/… 与 mechName/mechCore 的值级查询。
data MechLayout = MechLayout
  { mlName :: ElementName
  , mlCore :: Bool
  , mlUfos :: Maybe [Ufo]
  , mlBelts :: Maybe [Belt]
  , mlPortals :: Maybe [(Pos, Pos)]
  , mlCarpetOpen :: Maybe [Pos]
  , mlGround :: Maybe Ground
  , mlDrops :: Maybe [Pos]
  }
  deriving (Eq, Show)

emptyLayout :: ElementName -> MechLayout
emptyLayout n = MechLayout n False Nothing Nothing Nothing Nothing Nothing Nothing

-- | 关卡级机制：slim-5 起主 API 是 'onBeat' + 'layout'；下列旧方法缺省转发过去。
class (Typeable m, Eq m, Show m) => Mechanic m where
  -- | 身份：覆盖 'mechName' 或 'layout'（layout 缺省 = emptyLayout (mechName m)）。
  mechName :: m -> ElementName
  mechName = mlName . layout
  layout :: m -> MechLayout
  layout m = emptyLayout (mechName m)
  mechStart :: Level -> m -> m
  mechStart _ m = m
  mechCore :: m -> Bool
  mechCore = mlCore . layout

  onBeat :: Beat r -> m -> Maybe (r, m)
  onBeat _ _ = Nothing

  onRefilled :: m -> Board -> [Pos] -> Maybe ([Pos], m)
  onRefilled m b acc = onBeat (Refilled b acc) m
  onEndTick :: m -> [(Pos, Pos)] -> Maybe ([(Pos, Pos)], m)
  onEndTick m acc = onBeat (EndTick acc) m
  onSettling :: m -> (Cell -> Bool) -> MBoard -> Maybe (MBoard, m)
  onSettling m canPass mb = onBeat (Settling canPass mb) m
  onCover :: m -> [Pos] -> Int -> Maybe (Int, m)
  onCover m hit n = onBeat (Covering hit n) m
  onGroundHit :: m -> GroundRule -> [Pos] -> [(ElementName, Int)] -> Maybe ([(ElementName, Int)], m)
  onGroundHit m hitG hits acc = onBeat (GroundHit hitG hits acc) m

  refillPolicy :: m -> RefillPolicy -> Maybe RefillPolicy
  refillPolicy m p = fmap fst (onBeat (AskRefill p) m)
  shapes :: m -> [ShapeRule] -> Maybe [ShapeRule]
  shapes m rs = fmap fst (onBeat (AskShapes rs) m)
  avoidCells :: m -> [Pos] -> Maybe [Pos]
  avoidCells m acc = fmap fst (onBeat (AskAvoid acc) m)
  wallCells :: m -> [Pos] -> Maybe [Pos]
  wallCells m acc = fmap fst (onBeat (AskWall acc) m)
  morph :: m -> Board -> Board -> Pos -> Pos -> Maybe Morph
  morph m b0 swapped p1 p2 = fmap fst (onBeat (AskMorph b0 swapped p1 p2) m)
  judge :: m -> Board -> Score -> MovesLeft -> Outcome -> Maybe Outcome
  judge m b score moves out = fmap fst (onBeat (AskJudge b score moves out) m)

  ufos :: m -> Maybe [Ufo]
  ufos = mlUfos . layout
  belts :: m -> Maybe [Belt]
  belts = mlBelts . layout
  portals :: m -> Maybe [(Pos, Pos)]
  portals = mlPortals . layout
  carpetOpen :: m -> Maybe [Pos]
  carpetOpen = mlCarpetOpen . layout
  ground :: m -> Maybe Ground
  ground = mlGround . layout
  drops :: m -> Maybe [Pos]
  drops = mlDrops . layout

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
  mechStart lvl _ = GroundLayer (lvlGround lvl)
  layout (GroundLayer g) = (emptyLayout "ground") { mlCore = True, mlGround = Just g }
  onBeat (GroundHit hitG hits acc) (GroundLayer g) =
    let (g', cs) = hitG hits g
     in Just (acc ++ cs, GroundLayer g')
  onBeat _ _ = Nothing
