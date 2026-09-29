{-# LANGUAGE ExistentialQuantification #-}
-- | 开放消息（同 xmonad 的 Message / SomeMessage / fromMessage）：任何 Typeable 类型声明一个空 instance
-- 就能当消息发，收的一方用 'fromMessage' 按类型认领。
--
-- 另含主流程在流水线节拍上发给关卡级元素的四条内置消息及其回复（取代段 4 的封闭钩子 LevelHook）：
-- 主流程只在节拍上发消息、按类型收回复；谁在哪个节拍反应由关卡级元素自己决定（见 Match3.Element.Class 的
-- 'LevelElement'）。
--
-- 依赖：Match3.Types、Board.Grid（MBoard）、Conveyor（Belt）、Ufo。
module Match3.Element.Message
  ( Message
  , SomeMessage(..)
  , fromMessage
    -- * 流水线节拍（关卡级元素）
  , Refilled(..)
  , Absorbed(..)
  , EndTicked(..)
  , Shifted(..)
  , Settling(..)
  , Settled(..)
  , Covering(..)
  , Covered(..)
  ) where

import Data.Typeable (Typeable, cast)
import Match3.Board.Grid (MBoard)
import Match3.Conveyor (Belt)
import Match3.Types
import Match3.Ufo (Ufo)

-- | 消息：任何 Typeable 类型声明一个空 instance 即可。
class Typeable m => Message m

-- | 装箱的消息。
data SomeMessage = forall m. Message m => SomeMessage m

-- | 拆箱：类型对得上就是 Just。
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- | 每轮补子之后（飞碟节拍）：当前飞碟、补子后的盘面。
data Refilled = Refilled [Ufo] Board

-- | 对 'Refilled' 的回复：吸走的格（≠ 引爆）、新的飞碟。
data Absorbed = Absorbed [Pos] [Ufo]

-- | 玩家交换的步末、倒计时（PhaseTick）之后、蔓延之前（皮带节拍）：当前皮带。
newtype EndTicked = EndTicked [Belt]

-- | 对 'EndTicked' 的回复：「原格 → 新格」移位（皮带）。没有元素回复 = 没有皮带（也没有皮带后的再连锁）。
newtype Shifted = Shifted [(Pos, Pos)]

-- | 沉降时（传送门节拍）：本体可穿门谓词、传送门对、落定前的可空盘面。
data Settling = Settling (Cell -> Bool) [(Pos, Pos)] MBoard

-- | 对 'Settling' 的回复：传送后的可空盘面。
newtype Settled = Settled MBoard

-- | 步末结算（地毯节拍）：未覆盖的目标格、本步清除 / 腾空的格。
data Covering = Covering [Pos] [Pos]

-- | 对 'Covering' 的回复：剩余未覆盖格、新覆盖数。
data Covered = Covered [Pos] Int

instance Message Refilled
instance Message Absorbed
instance Message EndTicked
instance Message Shifted
instance Message Settling
instance Message Settled
instance Message Covering
instance Message Covered
