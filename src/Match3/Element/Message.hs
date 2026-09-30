{-# LANGUAGE ExistentialQuantification #-}
-- | 开放消息（同 xmonad 的 Message / SomeMessage / fromMessage）：任何 Typeable 类型声明一个空 instance
-- 就能当消息发，收的一方用 'fromMessage' 按类型认领。
--
-- 另含主流程在流水线节拍上发给关卡级元素的内置消息及其回复（取代段 4 的封闭钩子 LevelHook）：
-- 主流程只在节拍上发消息、按类型收回复；谁在哪个节拍反应由关卡级元素自己决定（见 Match3.Element.Class 的
-- 'LevelElement'）。
--
-- 依赖：Match3.Types、Board.Grid（MBoard）、Board.Refill（补子策略）、Element.Types（形状规则）。
module Match3.Element.Message
  ( Message
  , SomeMessage(..)
  , fromMessage
    -- * 流水线节拍（关卡级元素）
  , Refilled(..)
  , Refilling(..)
  , Shaping(..)
  , EndTicked(..)
  , Settling(..)
  , Covering(..)
  , GroundHit(..)
  , AvoidCells(..)
  , WallCells(..)
  ) where

import Data.Typeable (Typeable, cast)
import Match3.Board.Grid (MBoard)
import Match3.Board.Refill (RefillPolicy)
import Match3.Element.Types (ShapeRule)
import Match3.Types

-- | 消息：任何 Typeable 类型声明一个空 instance 即可。
class Typeable m => Message m

-- | 装箱的消息。
data SomeMessage = forall m. Message m => SomeMessage m

-- | 拆箱：类型对得上就是 Just。
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- 第 7 刀（7a）：关卡级元素的状态在元素值里（GameState.gsLevelElems），消息不再捎带状态；
-- 每条节拍消息的问题与回复是**同一个类型**（累积器）：回复者在收到的消息上累加自己的结果并交回，
-- 回复的同时给出推进后的自身（见 'Match3.Element.Class.levelReply'）。

-- | 每轮补子之后（飞碟节拍）：补子后的盘面、已被吸走的格（回复者追加自己吸走的格）。
data Refilled = Refilled Board [Pos]

-- | 查询（第 8 刀，补子节拍）：本关用的补子策略（初值 = 注册表的策略；回复者可以换掉或包一层再交回）。
-- 没有元素回复 = 用注册表的策略。
newtype Refilling = Refilling RefillPolicy

-- | 查询（新玩法「L / T 形炸弹」）：本关用的特殊块形状规则表（初值 = 注册表的表；回复者可以插入 / 换掉规则再交回）。
-- 没有元素回复 = 用注册表的表。每步结算开始时问一次（Game.Resolve）。
newtype Shaping = Shaping [ShapeRule]

-- | 玩家交换的步末、倒计时（PhaseTick）之后、蔓延之前（皮带节拍）：「原格 → 新格」移位（回复者追加）。
-- 没有元素回复 = 没有皮带（也没有皮带后的再连锁）。
newtype EndTicked = EndTicked [(Pos, Pos)]

-- | 沉降时（传送门节拍）：本体可穿门谓词、落定前的可空盘面（回复者交回传送后的盘面）。
data Settling = Settling (Cell -> Bool) MBoard

-- | 步末结算（地毯节拍）：本步清除 / 腾空的格、新覆盖数（回复者累加）。
data Covering = Covering [Pos] Int

-- | 每轮之后的地面层命中（地面层节拍）：注册表给出的地面层规则（命中格 → 旧地面层 → (新地面层, 按名字的去层数)）、
-- 本轮命中格、已累计的去层数（回复者追加）。
data GroundHit = GroundHit ([Pos] -> Ground -> (Ground, [(ElementName, Int)])) [Pos] [(ElementName, Int)]

-- | 查询：会走的元素（PhaseMove）要跳过的格（皮带格；回复者追加）。
newtype AvoidCells = AvoidCells [Pos]

-- | 查询：会走的元素（PhaseMove）当墙的格（传送门端点；回复者追加）。
newtype WallCells = WallCells [Pos]

instance Message Refilled
instance Message Refilling
instance Message Shaping
instance Message EndTicked
instance Message Settling
instance Message Covering
instance Message GroundHit
instance Message AvoidCells
instance Message WallCells
