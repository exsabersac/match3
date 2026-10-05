{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | 关卡级机制（飞碟 / 皮带 / 传送门 / 地毯 / 地面层 / 规则开关 / 掉落口）：不在格子里的机制，状态在值里。
--
-- 元素类重构第 5 刀起取代旧的关卡级元素类与开放消息：主流程的每个流水线节拍是
-- 'Mechanic' 的一个**有类型的方法**，缺省 = 不回复（Nothing）。一局的全部机制是 GameState.gsLevelElems ::
-- ['SomeMechanic']；节拍的折叠（按参与顺序，前一个的回复是后一个的输入，推进后的状态写回）在 Match3.Element.Level。
--
-- * 带状态的节拍：回复 = (累加后的结果, 推进后的自身)——补子之后 'onRefilled'（飞碟）、步末倒计时之后 'onEndTick'（皮带）、
--   沉降 'onSettling'（传送门）、步末覆盖 'onCover'（地毯）、每轮之后的地面层命中 'onGroundHit'（地面层）。
-- * 查询（状态不变）：补子策略 'refillPolicy'（掉落口）、形状表 'shapes'（L / T 炸弹）、会走元素的避让格 'avoidCells'
--   （皮带）与墙 'wallCells'（传送门）、交换变身 'morph'（魔力鸟组合）、胜负复核 'judge'。
-- * 读数（前端 / GameState 的派生字段）：'ufos' / 'belts' / 'portals' / 'carpetOpen' / 'ground' / 'drops'，
--   Nothing = 这个机制不提供这种读数（读的一方取第一个提供的）。
--
-- 地面层 'GroundLayer' 是引擎的核心机制（'mechCore'：不经注册开关、总参与），所以定义在这里而不在 Builtin：
-- 每层的行为由世界里的地面层种类（GroundKind）决定，它只是承载状态。
module Match3.Element.Mechanic
  ( Mechanic(..)
  , SomeMechanic(..)
  , mechNameOf
  , fromMechanic
  , GroundRule
  , Morph(..)
  , GroundLayer(..)
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

-- | 地面层规则（世界的 hitGroundWith）：命中格 → 旧地面层 → (新地面层, 按名字的去层数)。
type GroundRule = [Pos] -> Ground -> (Ground, [(ElementName, Int)])

-- | 交换起手前的变身：元素名（步末效果的元素名）、逐格 (来源, 目标, 变身后的格)、变身后的起手种子。
-- 主流程把它记成第 0 轮之前的一条步末效果（EvSpread，前端按「长出新格」播放），再从变身后的盘面按种子起手。
data Morph = Morph
  { morphName  :: ElementName
  , morphCells :: [(Pos, Pos, Cell)]
  , morphSeeds :: [Pos]
  }

-- | 关卡级机制：一个类型 + 一个 instance，自己的状态放在值里。每个节拍方法的缺省都是「不回复」。
class (Typeable m, Eq m, Show m) => Mechanic m where
  -- | 机制身份键（registerMechanic / beatIn 写回按名合并）；内置名两两不同（ec_mechanic_names_unique），扩展时不可撞名。
  mechName :: m -> ElementName
  -- | 开局状态：由关卡记录（lvlGoal 已换成本局目标）给出；缺省 = 原样（没有状态的机制）。
  mechStart :: Level -> m -> m
  mechStart _ m = m
  -- | 核心机制：不经注册开关、只要在 gsLevelElems 里就参与（内置只有地面层）。
  mechCore :: m -> Bool
  mechCore _ = False

  -- | 每轮补子之后：补子后的盘面、已被吸走的格 → 追加自己吸走的格。
  onRefilled :: m -> Board -> [Pos] -> Maybe ([Pos], m)
  onRefilled _ _ _ = Nothing
  -- | 玩家交换的步末、倒计时（PhaseTick）之后、蔓延之前：「原格 → 新格」移位（追加）。没有回复 = 没有皮带后的再连锁。
  onEndTick :: m -> [(Pos, Pos)] -> Maybe ([(Pos, Pos)], m)
  onEndTick _ _ = Nothing
  -- | 沉降时：本体可穿门谓词、落定前的可空盘面 → 传送后的盘面。
  onSettling :: m -> (Cell -> Bool) -> MBoard -> Maybe (MBoard, m)
  onSettling _ _ _ = Nothing
  -- | 步末结算：本步清除 / 腾空的格、已有的新覆盖数 → 累加后的覆盖数。
  onCover :: m -> [Pos] -> Int -> Maybe (Int, m)
  onCover _ _ _ = Nothing
  -- | 每轮之后的地面层命中：地面层规则、本轮命中格、已累计的去层数 → 追加后的去层数。
  onGroundHit :: m -> GroundRule -> [Pos] -> [(ElementName, Int)] -> Maybe ([(ElementName, Int)], m)
  onGroundHit _ _ _ _ = Nothing

  -- | 查询：本关的补子策略（输入 = 世界的策略或前一个回复者换过的）。
  refillPolicy :: m -> RefillPolicy -> Maybe RefillPolicy
  refillPolicy _ _ = Nothing
  -- | 查询：本关的特殊块形状规则表（每步结算开始时问一次）。
  shapes :: m -> [ShapeRule] -> Maybe [ShapeRule]
  shapes _ _ = Nothing
  -- | 查询：会走的元素（PhaseMove）要跳过的格（追加）。
  avoidCells :: m -> [Pos] -> Maybe [Pos]
  avoidCells _ _ = Nothing
  -- | 查询：会走的元素（PhaseMove）当墙的格（追加）。
  wallCells :: m -> [Pos] -> Maybe [Pos]
  wallCells _ _ = Nothing
  -- | 查询（玩家交换成立前）：交换前 / 交换后的盘面与两端 → 变身。第一个回复者为准。
  morph :: m -> Board -> Board -> Pos -> Pos -> Maybe Morph
  morph _ _ _ _ _ = Nothing
  -- | 查询（胜负节拍）：结算后的盘面、总分、剩余步数、（内置规则或前一个回复者）判出的结局 → 换掉的结局。
  judge :: m -> Board -> Score -> MovesLeft -> Outcome -> Maybe Outcome
  judge _ _ _ _ _ = Nothing

  -- | 读数：飞碟 / 传送带路径 / 传送门对 / 未覆盖的地毯格 / 地面层 / 掉落口格。
  ufos :: m -> Maybe [Ufo]
  ufos _ = Nothing
  belts :: m -> Maybe [Belt]
  belts _ = Nothing
  portals :: m -> Maybe [(Pos, Pos)]
  portals _ = Nothing
  carpetOpen :: m -> Maybe [Pos]
  carpetOpen _ = Nothing
  ground :: m -> Maybe Ground
  ground _ = Nothing
  drops :: m -> Maybe [Pos]
  drops _ = Nothing

-- | 装箱的关卡级机制。相等 = 同类型且值相等；Show = 值本身的 Show。
data SomeMechanic = forall m. Mechanic m => SomeMechanic m

instance Eq SomeMechanic where
  SomeMechanic a == SomeMechanic b = sameTypeEq a b

instance Show SomeMechanic where
  showsPrec d (SomeMechanic m) = showsPrec d m

-- | 机制的名字。
mechNameOf :: SomeMechanic -> ElementName
mechNameOf (SomeMechanic m) = mechName m

-- | 拆箱：类型对得上就是 Just。
fromMechanic :: Mechanic m => SomeMechanic -> Maybe m
fromMechanic (SomeMechanic m) = cast m

-- | 地面层（段 2c 的扩展槽，内置有双层果冻）：格子下面的层（元素名 + 层数）。每轮之后按世界的地面层规则被命中；
-- 核心机制。开局 = 关卡记录的地面层。
newtype GroundLayer = GroundLayer Ground
  deriving (Eq, Show)

instance Mechanic GroundLayer where
  mechName _ = "ground"
  onGroundHit (GroundLayer g) hitG hits acc = let (g', cs) = hitG hits g in Just (acc ++ cs, GroundLayer g')
  mechStart lvl _ = GroundLayer (lvlGround lvl)
  mechCore _ = True
  ground (GroundLayer g) = Just g
