-- | 元素框架的词汇类型：邻格 / 步末 / 成对交换 / 开启规则、计数键（再导出）、放置参数、格子的分派编号（cellSlot）；
-- 特殊块形状规则（ShapeRule，连同连线 MatchRun）与组合规则（ComboRule）。
-- 元素本身是类型类（Match3.Element.Phase / Kind / Layer；命中结果是 Phase 的 Strike），主流程（匹配、挡交换、
-- 直接命中、邻格波及、重力 / 传送门 / 边缘收集、计数、洗牌、步末、关卡放置）只经元素世界
-- （Match3.Element.World）问它们，不按构造器写死分支。
--
-- 依赖：Match3.Types、Element.Event（步末规则产出 EndEffect）。不含具体元素（见 Element.Builtin）。
--
-- 一个格子最多三层，自上而下：冰层（宝石的 ice Int）→ 叠层（CellOverlay）→ 本体（CellContents 构造器 /
-- 宝石种类 / Custom 名字）。冰层与叠层是叠层种类（Match3.Element.Layer 的 Layer），本体是本体种类（Kind）；
-- 命中与挡匹配等按层自上而下组合（见 Layer 的 Layered 与 World 的解码）。
module Match3.Element.Types
  ( ElementName(..)
  , CustomState(..)
  , AdjCtx(..)
  , AdjOut(..)
  , AdjacentRule(..)
  , CounterKey(..)
  , EndPhase(..)
  , EndCtx(..)
  , EndRule(..)
  , EndRun
  , tickRule
  , spreadRule
  , moveRule
  , runEndRules
  , Edge(..)
  , Arg(..)
  , ArgP(..)
  , argInt
  , argColor
  , exactArgs
  , prefixArgs
  , Placement(..)
  , Placer
  , FaceValue(..)
  , CellField(..)
  , SwapRule(..)
  , OpenRule(..)
  , MatchRun(..)
  , ShapeCtx(..)
  , ShapeRule(..)
  , ComboRule(..)
  , cellSlot
  , overlaySlot
  , kindSlot
  ) where

import Control.Applicative (Alternative(..))
import Data.List (mapAccumL)
import Data.Maybe (catMaybes)
import Match3.Counts (CounterKey(..))
import Match3.Element.Event (EndEffect)
import Match3.Types

-- | 邻格波及的上下文：acTrue = 本轮真消除格；acDirect = 本轮已被直接命中的格（不再重复波及）；
-- acProtect = 本轮刚生成、必须原样坐住的格（彩蛋开出的特殊块 + 之前各轮次产出的 aoSit）；
-- acRecolor = 元素世界给出的「本体可被改色」谓词（魔法帽 / 染色瓶只改这类格；内置等于 isGem）。
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

-- 计数键 CounterKey 定义在 Match3.Counts，这里再导出给元素定义用。

-- | 步末阶段（交换之后；道具只有 PhaseSpread）。皮带是关卡特性，固定夹在 Tick 与 Spread 之间。
data EndPhase = PhaseTick | PhaseSpread | PhaseMove
  deriving (Eq, Ord, Show)

-- | 步末规则的上下文：ecAvoid = 本步已被皮带移动过的格；ecWalls = 传送门端点（会走的元素当墙）；
-- ecPushable = 元素世界给出的「本体可被推动」谓词（蜗牛只推这类格；内置等于 Snail.pushable）。
data EndCtx = EndCtx
  { ecAvoid    :: [Pos]
  , ecWalls    :: [Pos]
  , ecPushable :: Cell -> Bool
  }

-- | 步末规则：erRun 返回（要记录的步末效果，新盘面）；erSeeds 在 PhaseTick 之后给出要引爆的种子
-- （倒计时归零的 3×3），其余阶段为 const []；erHoles 在全部步末阶段之后给出要挖空的格
-- （步末补结算：挖空 → 沉降 / 边缘收集 → 补子 → 成消再连锁），内置规则都是 const []。
data EndRule = EndRule
  { erPhase :: EndPhase
  , erOrder :: Int
  , erRun   :: EndRun
  , erSeeds :: Board -> [Pos]
  , erHoles :: Board -> [Pos]
  }

-- | 一条步末规则的执行：（要记录的步末效果（Nothing = 不记），新盘面）。
type EndRun = EndCtx -> Board -> (Maybe EndEffect, Board)

-- 按阶段的智能构造器（Haskell 特性第 9 项，docs/haskell-features/09-规则去重.md）。
-- erSeeds 只在 PhaseTick 之后被读（Board.Cascade 的倒计时），erHoles 内置规则全是 const []。
-- 三个构造器把「这个阶段有哪些槽」写进参数表——
-- 只有 'tickRule' 收种子；需要声明空洞的扩展规则（测试里的陷坑）仍然直接用 'EndRule' 记录。

-- | 倒计时阶段（PhaseTick）：跑完之后在新盘面上取引爆种子。
tickRule :: Int -> EndRun -> (Board -> [Pos]) -> EndRule
tickRule order run seeds = EndRule PhaseTick order run seeds noCells

-- | 蔓延阶段（PhaseSpread，道具之后也跑）。
spreadRule :: Int -> EndRun -> EndRule
spreadRule order run = EndRule PhaseSpread order run noCells noCells

-- | 会走的元素（PhaseMove）。
moveRule :: Int -> EndRun -> EndRule
moveRule order run = EndRule PhaseMove order run noCells noCells

noCells :: Board -> [Pos]
noCells = const []

-- | 依次执行一串步末规则（Haskell 特性第 9 项）：盘面从一条规则穿到下一条，收集非空效果。
-- 返回（[(规则前盘面, 规则后盘面, 效果)]（按规则顺序，空效果不记）, 终盘）。
--
-- 倒计时（Board.Cascade）、蔓延（Game.Trace）、会走的元素（Game.EndPhase）共用这一份。
-- 写法是 'mapAccumL'：累积量 = 当前盘面，每条规则的输出 = 可能的一条记录（同 randomBoardSized 的写法）；记录按规则顺序。
runEndRules :: EndCtx -> [EndRule] -> Board -> ([(Board, Board, EndEffect)], Board)
runEndRules ctx rules b0 =
  let (b1, recs) = mapAccumL one b0 rules
  in (catMaybes recs, b1)
  where
    one before rule =
      let (eff, after) = erRun rule ctx before
      in (after, fmap (\e -> (before, after, e)) eff)

-- | 边缘收集的方向：本体位于这条边上的格子在沉降时被收走。
-- 收集顺序固定为 底 → 左 → 右 → 上，每条边内按行 / 列升序（只有底边时就是「底行收饼干」）。
data Edge = EdgeBottom | EdgeLeft | EdgeRight | EdgeTop
  deriving (Eq, Show)

-- | 关卡放置参数（层数 / 回合数 / 颜色 / 方向分量）。
data Arg = AInt Int | AColor Color
  deriving (Eq, Show)

-- | 放置参数的小解析器（Haskell 特性第 8 项，docs/haskell-features/08-数据边界.md）：
-- 从参数表头部取值，返回值与剩下的参数。Applicative 依次取（@f <$> argColor <*> argInt@）；
-- Alternative 的 '<|>' 左偏、只在这一步失败时改试右边（@argInt <|> pure 1@ = 「有整数就取，没有就缺省 1」）。
-- 跑法分两种，每个放置函数选哪种见 08 文档的表：
--
-- * 'exactArgs'：必须恰好用完全部参数，多一个也算失败（相当于 @case args of [AInt n] -> …; _ -> Nothing@）；
-- * 'prefixArgs'：只看头部，后面多出的参数忽略（相当于 @(AInt k : _) -> …@）。
newtype ArgP a = ArgP {runArgP :: [Arg] -> Maybe (a, [Arg])}

instance Functor ArgP where
  fmap f (ArgP p) = ArgP (\as -> fmap (\(a, rest) -> (f a, rest)) (p as))

instance Applicative ArgP where
  pure a = ArgP (\as -> Just (a, as))
  ArgP pf <*> ArgP pa = ArgP $ \as -> case pf as of
    Nothing -> Nothing
    Just (f, rest) -> fmap (\(a, rest') -> (f a, rest')) (pa rest)

instance Alternative ArgP where
  empty = ArgP (const Nothing)
  ArgP p <|> ArgP q = ArgP (\as -> maybe (q as) Just (p as))

-- | 取一个整数参数。
argInt :: ArgP Int
argInt = ArgP $ \as -> case as of
  AInt n : rest -> Just (n, rest)
  _ -> Nothing

-- | 取一个颜色参数。
argColor :: ArgP Color
argColor = ArgP $ \as -> case as of
  AColor c : rest -> Just (c, rest)
  _ -> Nothing

-- | 精确匹配：解析成功且参数正好用完。
exactArgs :: ArgP a -> [Arg] -> Maybe a
exactArgs p as = case runArgP p as of
  Just (a, []) -> Just a
  _ -> Nothing

-- | 前缀匹配：解析成功即可，剩下的参数忽略。
prefixArgs :: ArgP a -> [Arg] -> Maybe a
prefixArgs p = fmap fst . runArgP p

-- | 关卡放置：给出参数与原格，返回新格（Nothing = 不放）。
type Placer = [Arg] -> Cell -> Maybe Cell

-- | 关卡放置表的一项：把元素（按名字）以给定参数放到若干格（按列表顺序逐格）。
data Placement = Place ElementName [Arg] [Pos]
  deriving (Eq, Show)

-- | 成对交换规则：交换两端的组合直接决定起手种子（彩虹取色、特殊 × 特殊合成）。
-- srFires 看交换前的盘面；srSeeds 在交换后的盘面上给出种子。多条规则按 srOrder 取第一条成立的。
data SwapRule = SwapRule
  { srOrder :: Int
  , srFires :: Board -> Pos -> Pos -> Bool
  , srSeeds :: Board -> Pos -> Pos -> [Pos]
  }

-- | 开启规则（彩蛋类）：一轮里被命中 / 邻格有真消除时开启，可在同一轮内多次开启（新爆炸再波及）。
-- orOpen 盘面 本批前沿 = (开启后盘面, 要展开的爆炸种子, 开出后本轮必须坐住的格)。
newtype OpenRule = OpenRule
  { orOpen :: Board -> [Pos] -> (Board, [Pos], [Pos])
  }

-- | 一条 ≥3 的同色连线（石头等挡匹配的格打断连线）。定义在这里是因为形状规则要用；Board.Match 再导出。
data MatchRun = MatchRun
  { runColor :: Color
  , runPos   :: [Pos]
  , runIsH   :: Bool  -- True = horizontal
  } deriving (Eq, Show)

-- | 形状规则的上下文：scPrefer = 玩家交换落点（优先放在这里）；scRuns = 本轮全部连线
-- （L / T 这类跨连线的形状要看别的连线）；scClearable = 本轮真正挖空的格（特殊块只放在这些格上）。
data ShapeCtx = ShapeCtx
  { scPrefer    :: Maybe Pos
  , scRuns      :: [MatchRun]
  , scClearable :: [Pos]
  }

-- | 特殊块形状规则：匹配形状 → 生成哪种特殊块。规则表是有序的：每条连线按表顺序问各规则，
-- 取第一条认领它的（Just，可以是空列表 = 认领但不生成）；Nothing = 这条规则不管，问下一条；
-- 全不认领 = 不生成。每条连线的产出按连线顺序依次写回，后写的覆盖先写的。
data ShapeRule = ShapeRule
  { shapeName  :: String
  , shapeSpawn :: ShapeCtx -> MatchRun -> Maybe [(Pos, Cell)]
  }

-- | 特殊块组合规则：两个特殊块交换时的组合效果。规则表是有序的：先按表顺序、每条规则先试
-- (第一端, 第二端) = (p1, p2) 再试 (p2, p1)，取第一条两端谓词都成立的；comboSeeds 收
-- 交换后盘面与 (对上 comboFirst 的一端, 对上 comboSecond 的一端)。一条规则天然对两个方向都成立（对称）；
-- 表里没有的组合不成立（交给后面的成对规则 / 普通三消）。
data ComboRule = ComboRule
  { comboName   :: String
  , comboFirst  :: Cell -> Bool
  , comboSecond :: Cell -> Bool
  , comboSeeds  :: Board -> Pos -> Pos -> [Pos]
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

-- | 元素自带的显示附加字段的值（Match3.Element.Phase 的 view → fExtras）：网页格子 JSON 里按出现顺序
-- 追加在 cellFace 字段之后（FaceInt / FaceColor → 数字（颜色取 1..5），FaceBool → true / false）。
-- | 前端格子的基本字段的值（网页 JSON 的 c / k / i / o / n …；元素类重构第 6 刀从 Match3.View 移来，
-- 元素经 Match3.Element.Phase 的 view → fBase 给出自己的标签与字段）。
data CellField = FieldInt Int | FieldText String | FieldNull
  deriving (Eq, Show)

data FaceValue
  = FaceInt Int
  | FaceBool Bool
  | FaceColor Color
  deriving (Eq, Show)
