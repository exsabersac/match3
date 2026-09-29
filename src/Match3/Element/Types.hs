-- | 元素框架的词汇类型：层（Slot）、命中结果、邻格 / 步末 / 成对交换 / 开启规则、计数键、放置参数。
-- 元素本身是类型类（Match3.Element.Class 的 Element / Modifier / LevelElement），主流程（匹配、挡交换、
-- 直接命中、邻格波及、重力 / 传送门 / 边缘收集、计数、洗牌、步末、关卡放置）只经注册表
-- （Match3.Element.Registry）问它们，不按构造器写死分支。
--
-- 依赖：Match3.Types、Element.Event（步末规则产出 EndEffect）。不含具体元素（见 Element.Builtin）。
--
-- 一个格子最多三层，自上而下：冰层（宝石的 ice Int）→ 叠层（CellOverlay）→ 本体（CellContents 构造器 /
-- 宝石种类 / Custom 名字）。冰层与叠层是修饰器（Modifier），本体是元素（Element）；命中与挡匹配等
-- 按层自上而下组合（见 Class 的 Modified 与 Registry）。
module Match3.Element.Types
  ( ElementName
  , Slot(..)
  , HitResult(..)
  , AdjCtx(..)
  , AdjOut(..)
  , AdjacentRule(..)
  , Counter(..)
  , EndPhase(..)
  , EndCtx(..)
  , EndRule(..)
  , Edge(..)
  , Arg(..)
  , Placement(..)
  , SwapRule(..)
  , OpenRule(..)
  , cellSlot
  , overlaySlot
  , kindSlot
  ) where

import Match3.Element.Event (EndEffect)
import Match3.Types

-- | 元素名：注册表的键，也是关卡放置表、计数键、前端贴图 / 播放表的键。
type ElementName = String

-- | 注册表条目接管格子的哪一层。
data Slot
  = SlotCell Int     -- ^ 内置本体（cellSlot 编号；宝石按种类各占一个编号）
  | SlotOverlay Int  -- ^ 宝石叠层（overlaySlot 编号）
  | SlotIce          -- ^ 宝石冰层
  | SlotCustom       -- ^ 自定义本体：Custom 名字 == 元素名
  | SlotGround       -- ^ 地面层（段 2c）：GameState.gsGround 里名字 == 元素名的格
  | SlotNone         -- ^ 原型推不出内置槽位（构造器用错）：mkRegistryChecked 报错，mkRegistry 不为它分派
  deriving (Eq, Show)

-- | 直接命中（匹配 / 特殊块 / 道具种子落在本格）时这一层的反应。
data HitResult
  = HitPierce       -- ^ 这一层不管，继续问下一层（只对冰层 / 叠层有意义）
  | HitAbsorb Cell  -- ^ 这一层吃掉命中：格子变成给出的新内容，本格不消除
  | HitDestroy      -- ^ 本格被消除（进入清除格）
  | HitImmune       -- ^ 打不动：格子原样，不消除（锤子对它拒绝且不扣次数）
  deriving (Eq, Show)

-- | 邻格波及的上下文：acTrue = 本轮真消除格；acDirect = 本轮已被直接命中的格（不再重复波及）；
-- acProtect = 本轮刚生成、必须原样坐住的格（彩蛋开出的特殊块 + 之前各轮次产出的 aoSit）；
-- acRecolor（段 4）= 注册表给出的「本体可被改色」谓词（魔法帽 / 染色瓶只改这类格，原先写死 isGem）。
data AdjCtx = AdjCtx
  { acTrue    :: [Pos]
  , acDirect  :: [Pos]
  , acProtect :: [Pos]
  , acRecolor :: Cell -> Bool
  }

-- | 一次邻格波及的结果：新盘面、本次打碎（并入清除格）的位置、本次新生成需坐住的位置。
data AdjOut = AdjOut
  { aoBoard :: Board
  , aoDead  :: [Pos]
  , aoSit   :: [Pos]
  }

-- | 邻格波及规则。arOrder 决定在一轮里的先后（小的先；内置 10..160，见 docs/architecture.md）。
data AdjacentRule = AdjacentRule
  { arOrder :: Int
  , arRun   :: AdjCtx -> Board -> AdjOut
  }

-- | 计数键：内置目标用前 8 个；CountNamed 进 GameState.gsElementCounts（按名字累计）。
data Counter
  = CountStones | CountChests | CountHoney | CountBalloons | CountCookies | CountCakes
  | CountSafes | CountSpirits
  | CountNamed String
  deriving (Eq, Ord, Show)

-- | 步末阶段（交换之后；道具只有 PhaseSpread）。皮带是关卡特性，固定夹在 Tick 与 Spread 之间。
data EndPhase = PhaseTick | PhaseSpread | PhaseMove
  deriving (Eq, Ord, Show)

-- | 步末规则的上下文：ecAvoid = 本步已被皮带移动过的格；ecWalls = 传送门端点（会走的元素当墙）；
-- ecPushable（段 4）= 注册表给出的「本体可被推动」谓词（蜗牛只推这类格，原先写死 pushable）。
data EndCtx = EndCtx
  { ecAvoid    :: [Pos]
  , ecWalls    :: [Pos]
  , ecPushable :: Cell -> Bool
  }

-- | 步末规则：erRun 返回（要记录的步末效果，新盘面）；erSeeds 在 PhaseTick 之后给出要引爆的种子
-- （倒计时归零的 3×3），其余阶段为 const []；erHoles（段 2c）在全部步末阶段之后给出要挖空的格
-- （步末补结算：挖空 → 沉降 / 边缘收集 → 补子 → 成消再连锁），内置规则都是 const []。
data EndRule = EndRule
  { erPhase :: EndPhase
  , erOrder :: Int
  , erRun   :: EndCtx -> Board -> (Maybe EndEffect, Board)
  , erSeeds :: Board -> [Pos]
  , erHoles :: Board -> [Pos]
  }

-- | 边缘收集的方向（段 2c）：本体位于这条边上的格子在沉降时被收走。
-- 收集顺序固定为 底 → 左 → 右 → 上，每条边内按行 / 列升序（只有底边时与旧「底行收饼干」逐位相同）。
data Edge = EdgeBottom | EdgeLeft | EdgeRight | EdgeTop
  deriving (Eq, Show)

-- | 关卡放置参数（层数 / 回合数 / 颜色 / 方向分量）。
data Arg = AInt Int | AColor Color
  deriving (Eq, Show)

-- | 关卡放置表的一项：把元素（按名字）以给定参数放到若干格（按列表顺序逐格）。
data Placement = Place ElementName [Arg] [Pos]
  deriving (Eq, Show)

-- | 成对交换规则（段 4）：交换两端的组合直接决定起手种子（彩虹取色、特殊 × 特殊合成）。
-- srFires 看交换前的盘面；srSeeds 在交换后的盘面上给出种子。多条规则按 srOrder 取第一条成立的。
data SwapRule = SwapRule
  { srOrder :: Int
  , srFires :: Board -> Pos -> Pos -> Bool
  , srSeeds :: Board -> Pos -> Pos -> [Pos]
  }

-- | 开启规则（段 4，彩蛋类）：一轮里被命中 / 邻格有真消除时开启，可在同一轮内多次开启（新爆炸再波及）。
-- orOpen 盘面 本批前沿 = (开启后盘面, 要展开的爆炸种子, 开出后本轮必须坐住的格)。
newtype OpenRule = OpenRule
  { orOpen :: Board -> [Pos] -> (Board, [Pos], [Pos])
  }

-- | 内置本体的编号：宝石按种类 0..4（Normal / LineH / LineV / Bomb / Rainbow），其余构造器 5..19；
-- Custom 为 -1（按名字查）。
cellSlot :: Cell -> Int
cellSlot cell = case cell of
  Gem _ k _ _ -> kindSlot k
  Stone _ -> 5
  Chest _ -> 6
  Honey _ -> 7
  Balloon _ -> 8
  Cookie -> 9
  Cake _ -> 10
  MagicHat -> 11
  Maker _ _ -> 12
  Snail _ _ -> 13
  Safe _ -> 14
  Flip _ _ -> 15
  Surprise -> 16
  Bottle _ -> 17
  TimeSpirit -> 18
  Countdown _ _ -> 19
  Custom _ _ -> -1

-- | 宝石种类的编号（cellSlot 的 0..4）。
kindSlot :: GemKind -> Int
kindSlot k = case k of
  Normal -> 0
  LineH -> 1
  LineV -> 2
  Bomb -> 3
  Rainbow -> 4

-- | 叠层的编号 0..7。
overlaySlot :: CellOverlay -> Int
overlaySlot ov = case ov of
  Grass -> 0
  Vine -> 1
  Choco -> 2
  Fog _ -> 3
  Chain _ -> 4
  Freeze _ -> 5
  Curtain _ -> 6
  Steam -> 7
