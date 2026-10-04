{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | 一轮连锁的阶段（Haskell 特性展示第 1 项「类型层」，讲解见 docs/haskell-features/01-类型层.md）。
--
-- 一步棋 = 交换 →（消除 → 下落 → 补子）× 若干轮。改动前，「消除后的盘面」和「下落后的盘面」
-- 都是同一个类型 'MBoard'（@Array Pos (Maybe Cell)@），谁先谁后只靠调用顺序和注释；
-- 现在每个盘面带上它所处的阶段（类型层的标签），阶段之间只能经下面这几个转换函数走：
--
-- @
--   Full ──swapStage──▶ Swapped                                （玩家交换 / 自由交换）
--   Full ──clearStage / digHoles──▶ Cleared ──fallStage──▶ Fallen ──refillStage──▶ Full
-- @
--
-- 于是「没下落就补子」「对着有空洞的盘面再消一次」「回放里把下落后的盘面当成消除后的记下来」
-- 这类写法直接编译不过（例子见本模块末尾的注释与 test/Spec/Phase.hs）。
--
-- 用到的特性：
--
-- * DataKinds：普通的 @data Phase = Full | ...@ 被「提升」成一个种类（kind），
--   @'Full@ / @'Cleared@ 等成了类型，可以写进 @Stage 'Cleared@ 这样的类型参数里。
-- * GADTs：'Stage' 的每个构造器各自写死自己的阶段与内部表示（满盘 'Board' / 有空洞的 'MBoard'）；
--   对它做模式匹配时，编译器顺带知道了 @p@ 是哪个阶段。
-- * TypeFamilies：'Repr' 是类型层的函数「阶段 → 内部表示」，让 'stageGrid' 只写一个类型签名；
--   'IsFull' 用它表达「满盘阶段」这个约束。
--
-- 运行时代价：'Stage' 只是给 Board / MBoard 套一层构造器，转换函数内部调用的仍是原来的
-- clear* / settleDrainWith / refillWith，计算与随机数消耗逐字不变（金标准锁定）。
--
-- 依赖：Board.Grid、Board.Gravity（沉降 settleDrainWith）、Board.Refill（补子 refillWith）、Board.Hooks、元素元素世界。
-- 被 Board.Cascade（每一轮 settleRound）与 Game.Resolve / Move / Boosters（一步棋的起手盘面）使用。
module Match3.Board.Phase
  ( -- * 阶段与带阶段的盘面
    Phase(..)
  , Stage
  , Repr
  , IsFull
  , stageGrid
  , stageBoard
    -- * 阶段转换（唯一的入口：构造器不导出）
  , fullStage
  , swapStage
  , clearStage
  , digHoles
  , fallStage
  , refillStage
  ) where

import Data.Kind (Type)
import Match3.Board.Gravity (settleDrainWith)
import Match3.Board.Grid (MBoard, setManyM, swapCells, toM)
import Match3.Board.Hooks (LevelHooks)
import Match3.Board.Refill (RefillPolicy, refillWith)
import Match3.Element.World (World)
import Match3.Types
import System.Random (RandomGen)

-- | 盘面在一步棋里所处的阶段。开了 DataKinds 后，这个类型同时是一个种类：
-- @'Full@、@'Swapped@、@'Cleared@、@'Fallen@ 是这个种类里的四个类型（只当标签用，没有值）。
data Phase
  = Full     -- ^ 满盘、等着开始一轮：静止盘 / 道具的原盘 / 皮带与步末之后的盘 / 补子之后的盘
  | Swapped  -- ^ 玩家（或自由交换道具）刚交换完的满盘；只能由 'swapStage' 产生
  | Cleared  -- ^ 刚消完（或步末挖了洞）：有空洞，还没下落
  | Fallen   -- ^ 下落、边缘收集、传送门之后：空洞都到了上方，等补子
  deriving (Eq, Show)

-- | 带阶段标签的盘面（GADT）。每个构造器的返回类型不同：@AtCleared@ 只能造出 @Stage 'Cleared@，
-- 而且它装的是 'MBoard'；@AtFull@ 只能造出 @Stage 'Full@，装的是没有空洞的 'Board'。
-- 构造器不导出，模块外只能经下面的转换函数得到 @Cleared@ / @Fallen@ / @Swapped@ 阶段的值。
data Stage (p :: Phase) where
  AtFull    :: Board  -> Stage 'Full
  AtSwapped :: Board  -> Stage 'Swapped
  AtCleared :: MBoard -> Stage 'Cleared
  AtFallen  :: MBoard -> Stage 'Fallen

-- | 各阶段盘面的内部表示（封闭类型族 = 类型层的函数，按阶段返回一个类型）。
type family Repr (p :: Phase) :: Type where
  Repr 'Full    = Board
  Repr 'Swapped = Board
  Repr 'Cleared = MBoard
  Repr 'Fallen  = MBoard

-- | 「满盘阶段」约束：内部表示是 'Board' 的阶段（@'Full@ / @'Swapped@）。
-- 对 @'Cleared@ 它化简成 @MBoard ~ Board@，不成立，编译器拒绝。
type IsFull p = (Repr p ~ Board)

-- | 取出内部盘面。一个签名覆盖四个阶段：每个分支里 GADT 匹配让编译器知道 p 是什么，
-- 'Repr' 再把返回类型化简成 Board 或 MBoard。
stageGrid :: Stage p -> Repr p
stageGrid (AtFull b) = b
stageGrid (AtSwapped b) = b
stageGrid (AtCleared mb) = mb
stageGrid (AtFallen mb) = mb

-- | 满盘阶段的盘面（= 'stageGrid'，签名把结果钉成 'Board'）。
stageBoard :: IsFull p => Stage p -> Board
stageBoard = stageGrid

-- | 任何 'Board' 都是满盘（类型 Board 本身就没有空洞），所以这一步总是合法的。
fullStage :: Board -> Stage 'Full
fullStage = AtFull

-- | 交换：满盘 → 交换后。不检查相邻 / 越界（门禁在 Game.Move / Game.Boosters，与 swapCells 相同）。
swapStage :: Pos -> Pos -> Stage 'Full -> Stage 'Swapped
swapStage p1 p2 (AtFull b) = AtSwapped (swapCells b p1 p2)

-- | 消除：满盘 → 有空洞。参数是 Board.Clear 里的某个清除函数（匹配 / 种子 / 飞碟吸收），
-- 返回 (挖空的盘面, 清除数, 清除格)，与原函数相同，只是盘面带上了 @'Cleared@ 标签。
clearStage :: (Board -> (MBoard, Int, [Pos])) -> Stage 'Full -> (Stage 'Cleared, Int, [Pos])
clearStage clear (AtFull b) =
  let (mb, n, pos) = clear b
  in (AtCleared mb, n, pos)

-- | 步末挖洞（段 2c：步末规则声明的空洞）：满盘 → 有空洞，与 clearStage 同处一个阶段。
digHoles :: [Pos] -> Stage 'Full -> Stage 'Cleared
digHoles holes (AtFull b) = AtCleared (setManyM (toM b) [(p, Nothing) | p <- holes])

-- | 下落：有空洞 → 空洞在上方。即 'settleDrainWith'（重力 → 边缘收集 → 传送门 → 再重力 / 收集），
-- 另返回被边缘收走的 (位置, 原格)。
fallStage :: World -> LevelHooks -> Stage 'Cleared -> (Stage 'Fallen, [(Pos, Cell)])
fallStage reg hooks (AtCleared mb) =
  let (settled, drained) = settleDrainWith reg hooks mb
  in (AtFallen settled, drained)

-- | 补子：空洞在上方 → 满盘（下一轮又从 @'Full@ 开始）。即 'refillWith'，随机数消耗不变。
refillStage :: RandomGen g => RefillPolicy -> g -> Stage 'Fallen -> (Stage 'Full, g)
refillStage pol g (AtFallen mb) =
  let (b, g') = refillWith pol g mb
  in (AtFull b, g')

-- 编译器能挡住的写法（这些都编译不过；test/Spec/Phase.hs 用延迟类型错误把它们变成了测试，报错摘自 GHC 9.14）：
--
-- > refillStage pol g cleared   -- 没下落就补子：      Couldn't match type ‘Cleared’ with ‘Fallen’
-- > fallStage reg hooks fallen  -- 下落两次：          Couldn't match type ‘Fallen’ with ‘Cleared’
-- > clearStage f cleared        -- 有空洞还要消：      Couldn't match type ‘Cleared’ with ‘Full’
-- > swapStage p1 p2 swapped     -- 交换两次：          Couldn't match type ‘Swapped’ with ‘Full’
-- > stageBoard cleared          -- 把有空洞的盘当满盘：Couldn't match type ‘Array Pos (Maybe Cell)’ with ‘Board’
