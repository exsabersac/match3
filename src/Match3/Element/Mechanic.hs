{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 关卡级机制（飞碟 / 皮带 / 传送门 / 地毯 / 地面层 / 规则开关 / 掉落口）：不在格子里的机制，状态是一个值。
--
-- ecs-5 起取代 Mechanic 类与 GADT Beat：一种机制 = 一条**原型记录** 'Mechanic' m ——
--
-- * 名字 / 是否核心（数据）；
-- * 读数组件 'LevelView'（纯数据，由状态算出：飞碟 / 皮带 / 传送门 / 地毯 / 地面层 / 掉落口，前端与查询用）；
-- * 按节拍的 system 列表 'mechSystems'（'MechSys' 的每个构造子是流水线上的一个节拍，类型各自写明：
--   输入累积 → 回复与推进后的状态，Nothing = 本节拍不参与）。开局也是节拍（'OnStart'）。
--
-- 和本体原型的 aSystems（'Match3.ECS.Stage.SysDef'）同一个形状：行为都在 system 里，记录本身只放数据。
-- 折叠（按参与顺序把所有回复者串起来）在 Match3.Element.Level。
--
-- 地面层 'GroundLayer' 是核心机制（'mechCore'），定义在这里；地面层原型（Match3.Element.Kind 的
-- 'GroundArch' 记录）由它在 'OnGroundHit' 节拍上消费。
module Match3.Element.Mechanic
  ( -- * 原型
    Mechanic(..)
  , mechanic
  , MechSys(..)
  , LevelView(..)
  , noView
    -- * 装箱的机制（一局里存的：原型 + 状态）
  , SomeMechanic(..)
  , mechNameOf
  , mechCoreOf
  , mechStartOf
  , viewOf
  , fromMechanic
  , replyOf
    -- * 节拍的载荷类型
  , GroundRule
  , Morph(..)
    -- * 核心机制：地面层
  , GroundLayer(..)
  , groundLayerMech
  , groundLayer
  ) where

import Data.Typeable (Typeable, cast)
import Match3.Board.Grid (MBoard)
import Match3.Board.Refill (RefillPolicy)
import Match3.Conveyor (Belt)
import Match3.Element.Types (ShapeRule)
import Match3.Levels.Level (Level(..))
import Match3.Types
import Match3.Ufo (Ufo)

-- | 地面层被命中的规则（注册表的 hitGroundWith）：命中格 → 地面层 → (新地面层, 按名字的去层数)。
type GroundRule = [Pos] -> Ground -> (Ground, [(ElementName, Int)])

-- | 交换变身（新玩法 4）的回复：元素名、逐格变化 (源, 落点, 新格)、起手种子。
data Morph = Morph
  { morphName  :: ElementName
  , morphCells :: [(Pos, Pos, Cell)]
  , morphSeeds :: [Pos]
  }

-- | 一种机制在某个节拍上的 system（每个构造子 = 流水线上的一个节拍；@m@ = 机制状态）。
-- 推进型节拍（On…）回复并推进状态；问答型节拍（Answer…）只回复、不改状态。Nothing = 不参与本节拍。
data MechSys m
  = OnStart (Level -> m -> m)
    -- ^ 开局：按关卡记录给出初始状态（没有这个 system = 原型值原样）
  | OnRefilled (Board -> [Pos] -> m -> Maybe ([Pos], m))
    -- ^ 补子之后：在累积的吸收格后追加（飞碟）
  | OnEndTick ([(Pos, Pos)] -> m -> Maybe ([(Pos, Pos)], m))
    -- ^ 玩家交换的步末、倒计时之后：在累积的移位后追加（皮带）
  | OnSettling ((Cell -> Bool) -> MBoard -> m -> Maybe (MBoard, m))
    -- ^ 沉降时改写可变盘面（传送门；谓词 = 可穿门）
  | OnCovering ([Pos] -> Int -> m -> Maybe (Int, m))
    -- ^ 步末覆盖：命中格 → 累积的新覆盖数（地毯）
  | OnGroundHit (GroundRule -> [Pos] -> [(ElementName, Int)] -> m -> Maybe ([(ElementName, Int)], m))
    -- ^ 地面层被命中（核心机制地面层）
  | AnswerRefill (RefillPolicy -> m -> Maybe RefillPolicy)
    -- ^ 补子策略（掉落口）
  | AnswerShapes ([ShapeRule] -> m -> Maybe [ShapeRule])
    -- ^ 本步的特殊块形状表（规则开关 L / T 形炸弹）
  | AnswerAvoid ([Pos] -> m -> Maybe [Pos])
    -- ^ 会走的元素要跳过的格（皮带格）
  | AnswerWall ([Pos] -> m -> Maybe [Pos])
    -- ^ 会走的元素当墙的格（传送门端点）
  | AnswerMorph (Board -> Board -> Pos -> Pos -> m -> Maybe Morph)
    -- ^ 交换变身（规则开关「魔力鸟组合增强」）
  | AnswerJudge (Board -> Score -> MovesLeft -> Outcome -> m -> Maybe Outcome)
    -- ^ 胜负复核（内置都不回复）

-- | 读数组件（纯数据，由机制状态算出）：每项 Nothing = 不提供这种读数。
data LevelView = LevelView
  { lvUfos :: Maybe [Ufo]
  , lvBelts :: Maybe [Belt]
  , lvPortals :: Maybe [(Pos, Pos)]
  , lvCarpetOpen :: Maybe [Pos]
  , lvGround :: Maybe Ground
  , lvDrops :: Maybe [Pos]
  }

-- | 不提供任何读数。
noView :: LevelView
noView = LevelView Nothing Nothing Nothing Nothing Nothing Nothing

-- | 关卡级机制原型（状态类型 @m@）。
data Mechanic m = Mechanic
  { mechName :: ElementName        -- ^ 名字（按名合并 / 去掉）
  , mechCore :: Bool               -- ^ 核心机制（removeMechanic 也去不掉，总参与）
  , mechView :: m -> LevelView     -- ^ 读数组件
  , mechSystems :: [MechSys m]     -- ^ 按节拍的 system（同一节拍取第一个）
  }

-- | 缺省原型：只有名字、不核心、无读数、不参与任何节拍。
mechanic :: ElementName -> Mechanic m
mechanic n = Mechanic n False (const noView) []

-- | 一局里的一种机制：原型 + 当前状态（Eq 按状态类型 + 状态值；Show = 状态的 Show）。
data SomeMechanic = forall m. (Typeable m, Eq m, Show m) => SomeMechanic (Mechanic m) m

instance Eq SomeMechanic where
  SomeMechanic _ a == SomeMechanic _ b = maybe False (== b) (cast a)

instance Show SomeMechanic where
  showsPrec d (SomeMechanic _ m) = showsPrec d m

mechNameOf :: SomeMechanic -> ElementName
mechNameOf (SomeMechanic k _) = mechName k

mechCoreOf :: SomeMechanic -> Bool
mechCoreOf (SomeMechanic k _) = mechCore k

-- | 开局：依次跑 'OnStart' system（没有 = 原样）。
mechStartOf :: Level -> SomeMechanic -> SomeMechanic
mechStartOf lvl (SomeMechanic k m) = SomeMechanic k (foldl (\s sys -> case sys of OnStart f -> f lvl s; _ -> s) m (mechSystems k))

-- | 读数组件。
viewOf :: SomeMechanic -> LevelView
viewOf (SomeMechanic k m) = mechView k m

-- | 按状态类型取出状态。
fromMechanic :: Typeable m => SomeMechanic -> Maybe m
fromMechanic (SomeMechanic _ m) = cast m

-- | 在一个节拍上问一种机制：@pick@ 从它的 system 里挑出本节拍的那一个（同一节拍取第一个）。
-- 返回回复与推进后的机制；不参与时 Nothing。
replyOf :: (forall m. MechSys m -> Maybe (q -> m -> Maybe (q, m))) -> q -> SomeMechanic -> Maybe (q, SomeMechanic)
replyOf pick q (SomeMechanic k m) = go (mechSystems k)
  where
    go [] = Nothing
    go (sys : rest) = case pick sys of
      Just f -> (\(q', m') -> (q', SomeMechanic k m')) <$> f q m
      Nothing -> go rest
{-# INLINE replyOf #-}

--------------------------------------------------------------------------------
-- 核心机制：地面层

-- | 地面层的状态：各格的 (名字, 层数)。
newtype GroundLayer = GroundLayer Ground
  deriving (Eq, Show)

-- | 地面层原型：核心机制，开局取关卡记录的地面层，被命中时按注册表的规则去层。
groundLayerMech :: Mechanic GroundLayer
groundLayerMech = (mechanic "ground")
  { mechCore = True
  , mechView = \(GroundLayer g) -> noView {lvGround = Just g}
  , mechSystems =
      [ OnStart (\lvl _ -> GroundLayer (lvlGround lvl))
      , OnGroundHit (\hitG hits acc (GroundLayer g) -> let (g', cs) = hitG hits g in Just (acc ++ cs, GroundLayer g'))
      ]
  }

-- | 装箱的地面层。
groundLayer :: Ground -> SomeMechanic
groundLayer = SomeMechanic groundLayerMech . GroundLayer
