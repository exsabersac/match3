-- | 各结算阶段的世界类型（ECS 的「World」）与阶段调度器。
--
-- 每个阶段的世界 = 整盘（实体 = 格；组件存储 = 格子编码）+ 本阶段的黑板资源；该阶段的 system 都是
-- @世界 -> 世界@（Match3.ECS.System）。元素声明自己带哪些 system（'SysDef'），注册表按次序排好、组成流水线：
--
-- * 邻格（'NearWorld'）：一轮消除里与真消除格相邻的反应（石头削层、气球打破、魔法帽换色、雪怪扣血 …）；
-- * 步末（'EndWorld' / 'EndSys'）：玩家交换之后的倒计时 / 蔓延 / 会走的元素；
-- * 开启（'OpenWorld'）：彩蛋类，被命中 / 邻格有真消除时开出；
-- * 成对交换（'SwapSys'）：交换两端的组合直接给出起手种子（是两个只读查询：成立条件 + 种子）。
--
-- 调度器的约定（与重构前的规则驱动逐项相同，三份快照锁定）：
--
-- * 邻格：每个 system 看到的 'nwDead' / 'nwSit' 是空的（只记自己的产出），跑完由调度器按顺序并入总账；
--   'nwProtect' = nub (起始保护格 ++ 之前各 system 的坐住格)；
-- * 步末：每个 system 看到的 'ewEffect' 是 Nothing，跑完若有效果记一条 (前盘, 后盘, 效果)；
-- * 开启：第一个 system 的产出原样，之后的与之 nub 合并。
module Match3.ECS.Stage
  ( -- * 邻格
    NearWorld(..)
  , nearWorld
  , runNearStage
    -- * 步末
  , EndPhase(..)
  , EndWorld(..)
  , endWorld
  , EndSys(..)
  , tickSys
  , spreadSys
  , moveSys
  , effectSystem
  , runEndStage
    -- * 开启
  , OpenWorld(..)
  , runOpenStage
    -- * 成对交换
  , SwapSys(..)
    -- * 元素声明的 system
  , SysDef(..)
  ) where

import Data.List (mapAccumL, nub)
import Data.Maybe (catMaybes)
import Match3.ECS.System
import Match3.Element.Event (EndEffect)
import Match3.Types.Board (Board, Pos)
import Match3.Types.Cell (Cell)

--------------------------------------------------------------------------------
-- 邻格

-- | 邻格阶段的世界：整盘 + 本轮的真消除格 / 直接命中格 / 保护格（本轮刚生成、必须坐住）+ 「本体可被改色」谓词
-- （注册表给出：魔法帽 / 染色瓶只改这类格）+ 本 system 的产出（打碎并入清除格的格、新生成需坐住的格）。
data NearWorld = NearWorld
  { nwBoard   :: Board
  , nwTrue    :: [Pos]
  , nwDirect  :: [Pos]
  , nwProtect :: [Pos]
  , nwRecolor :: Cell -> Bool
  , nwDead    :: [Pos]
  , nwSit     :: [Pos]
  }

-- | 一轮邻格的起始世界（产出为空）。
nearWorld :: (Cell -> Bool) -> [Pos] -> [Pos] -> [Pos] -> Board -> NearWorld
nearWorld recolor trueClears direct protect b = NearWorld b trueClears direct protect recolor [] []

-- | 按次序跑完一轮邻格 system：返回 (盘面, 打碎的格（按 system 顺序拼接）, 新生成需坐住的格)。
runNearStage :: [Scheduled NearWorld] -> NearWorld -> (Board, [Pos], [Pos])
runNearStage systems w0 =
  let ((b', _), outs) = mapAccumL one (nwBoard w0, nub (nwProtect w0)) systems
  in (b', concatMap fst outs, concatMap snd outs)
  where
    -- 累积量 = (盘面, 保护格)；保护格增量维护：nub (xs ++ ys) = nub xs ++ [y | y <- nub ys, y `notElem` xs]
    one (board, protect) s =
      let w = runSystem (schSystem s) w0 {nwBoard = board, nwProtect = protect, nwDead = [], nwSit = []}
          new = nwSit w
      in ((nwBoard w, protect ++ [p | p <- nub new, p `notElem` protect]), (nwDead w, new))

--------------------------------------------------------------------------------
-- 步末

-- | 步末阶段（交换之后；道具只有 PhaseSpread）。皮带是关卡特性，固定夹在 Tick 与 Spread 之间。
data EndPhase = PhaseTick | PhaseSpread | PhaseMove
  deriving (Eq, Ord, Show)

-- | 步末阶段的世界：整盘 + 本步已被皮带移动过的格（避让）+ 传送门端点（会走的元素当墙）+ 「本体可被推动」谓词
-- + 本 system 要记录的步末效果（Nothing = 不记）。
data EndWorld = EndWorld
  { ewBoard    :: Board
  , ewAvoid    :: [Pos]
  , ewWalls    :: [Pos]
  , ewPushable :: Cell -> Bool
  , ewEffect   :: Maybe EndEffect
  }

-- | 步末的起始世界（没有效果）。
endWorld :: [Pos] -> [Pos] -> (Cell -> Bool) -> Board -> EndWorld
endWorld avoid walls pushable b = EndWorld b avoid walls pushable Nothing

-- | 一个步末 system 及其调度信息：阶段、次序、system 本身；'esSeeds' 是 PhaseTick 之后在终盘上取引爆种子的查询
-- （倒计时归零的 3×3、魔法石发射），'esHoles' 是全部步末之后要挖空的格（内置都是空）。
data EndSys = EndSys
  { esPhase  :: EndPhase
  , esOrder  :: Int
  , esSystem :: System EndWorld
  , esSeeds  :: Board -> [Pos]
  , esHoles  :: Board -> [Pos]
  }

-- | 倒计时阶段（PhaseTick）：跑完之后在新盘面上取引爆种子。
tickSys :: Int -> System EndWorld -> (Board -> [Pos]) -> EndSys
tickSys order s seeds = EndSys PhaseTick order s seeds noCells

-- | 蔓延阶段（PhaseSpread，道具之后也跑）。
spreadSys :: Int -> System EndWorld -> EndSys
spreadSys order s = EndSys PhaseSpread order s noCells noCells

-- | 会走的元素（PhaseMove）。
moveSys :: Int -> System EndWorld -> EndSys
moveSys order s = EndSys PhaseMove order s noCells noCells

noCells :: Board -> [Pos]
noCells = const []

-- | 「读世界 → (效果, 新盘面)」写成 system（大多数步末 system 的形状）。
effectSystem :: (EndWorld -> (Maybe EndEffect, Board)) -> System EndWorld
effectSystem f = System (\w -> let (eff, b') = f w in w {ewBoard = b', ewEffect = eff})

-- | 依次跑一串步末 system：返回（[(前盘, 后盘, 效果)]（按顺序，空效果不记）, 终盘）。
runEndStage :: EndWorld -> [EndSys] -> ([(Board, Board, EndEffect)], Board)
runEndStage w0 systems =
  let (b1, recs) = mapAccumL one (ewBoard w0) systems
  in (catMaybes recs, b1)
  where
    one before s =
      let w = runSystem (esSystem s) w0 {ewBoard = before, ewEffect = Nothing}
          after = ewBoard w
      in (after, fmap (\e -> (before, after, e)) (ewEffect w))

--------------------------------------------------------------------------------
-- 开启

-- | 开启阶段的世界：整盘 + 本批前沿（被命中 / 邻格真消除的格）+ 产出（要展开的爆炸种子、开出后本轮必须坐住的格）。
data OpenWorld = OpenWorld
  { owBoard :: Board
  , owFront :: [Pos]
  , owSeeds :: [Pos]
  , owSits  :: [Pos]
  }

-- | 一批前沿上依次跑开启 system：返回 (盘面, 爆炸种子, 本轮坐住的格)。只有一个 system 时就是它自己的产出。
runOpenStage :: [System OpenWorld] -> Board -> [Pos] -> (Board, [Pos], [Pos])
runOpenStage systems b front = case systems of
  [] -> (b, [], [])
  (s : ss) -> foldl step (run s b) ss
  where
    run s b0 = let w = runSystem s (OpenWorld b0 front [] []) in (owBoard w, owSeeds w, owSits w)
    step (b1, e1, s1) s' =
      let (b2, e2, s2) = run s' b1
      in (b2, nub (e1 ++ e2), nub (s1 ++ s2))

--------------------------------------------------------------------------------
-- 成对交换

-- | 成对交换：'swFires' 看交换前的盘面，'swSeeds' 在交换后的盘面上给出起手种子；多条按 'swOrder' 取第一条成立的。
data SwapSys = SwapSys
  { swOrder :: Int
  , swFires :: Board -> Pos -> Pos -> Bool
  , swSeeds :: Board -> Pos -> Pos -> [Pos]
  }

--------------------------------------------------------------------------------
-- 元素声明的 system

-- | 一种元素带来的 system（注册表收集后按阶段 / 次序排好）。没有「逃生口」：雪怪扣血、气泡、毛球、魔法石 tick
-- 与石头削层一样，都是注册在某个阶段上的普通 system。
data SysDef
  = SysNear Int (System NearWorld)  -- ^ 邻格 system（次序小的先；内置 10..200）
  | SysEnd EndSys                   -- ^ 步末 system（阶段 + 次序）
  | SysSwap SwapSys                 -- ^ 成对交换
  | SysOpen (System OpenWorld)      -- ^ 开启（彩蛋类）
