{-# LANGUAGE ExistentialQuantification #-}
-- | 关卡级元素（飞碟 / 皮带 / 传送门 / 地毯 / 地面层 / 规则开关）：不在格子里的机制，状态在值里，
-- 按消息回复流水线节拍（消息见 Match3.Element.Message）。
--
-- 格子元素（本体 / 叠层 / 地面层）不在这里：值级能力见 Match3.Element.Ability，类型级见
-- Match3.Element.Kind / Match3.Element.Layer，注册与解码见 Match3.Element.World。
module Match3.Element.Class
  ( -- * 关卡级元素
    LevelElement(..)
  , SomeLevelElement(..)
  , levelNameOf
  , fromLevelElement
    -- * 消息（再导出自 Match3.Element.Message）
  , Message
  , SomeMessage(..)
  , fromMessage
  ) where

import Data.Typeable (Typeable, cast)
import Match3.Element.Ability (sameTypeEq)
import Match3.Element.Message (Message, SomeMessage(..), fromMessage)
import Match3.Levels.Level (Level)
import Match3.Types

--------------------------------------------------------------------------------
-- | 关卡级元素：不在格子里的机制（飞碟 / 皮带 / 传送门 / 地毯 / 地面层），与格子元素同一写法：
-- 一种关卡级元素 = 一个类型 + 一个 instance，**自己的状态放在值里**（飞碟位置、皮带路径……），
-- 一局的全部关卡级元素是 GameState.gsLevelElems :: ['SomeLevelElement']。
--
-- 主流程在流水线节拍上发消息（Match3.Element.Message 的 Refilled / EndTicked / Settling / Covering / GroundHit，
-- 也可以是任何新消息类型），元素自己决定回复哪些：回复 = (装箱的回复消息, 推进后的自身)，Nothing = 不关心。
-- 内置节拍消息的问题与回复同类型（累积器）。开局时由关卡记录给出初始状态（'levelStart'）。
class (Typeable l, Eq l, Show l) => LevelElement l where
  levelName :: l -> ElementName
  levelReply :: l -> SomeMessage -> Maybe (SomeMessage, l)
  levelReply _ _ = Nothing
  -- | 开局状态：由关卡记录（lvlGoal 已换成本局目标）给出；缺省 = 原样（没有状态的元素）。
  levelStart :: Level -> l -> l
  levelStart _ l = l
  -- | 核心元素：不经注册表的关卡级开关、只要在 gsLevelElems 里就参与（内置只有地面层：其中每层元素的行为
  -- 已由注册表的地面层条目决定）。缺省 False：没注册（或被 removeLevel 去掉）的名字不生效。
  levelCore :: l -> Bool
  levelCore _ = False

-- | 装箱的关卡级元素。
-- 相等 = 同类型且值相等；Show = 值本身的 Show。
data SomeLevelElement = forall l. LevelElement l => SomeLevelElement l

instance Eq SomeLevelElement where
  SomeLevelElement a == SomeLevelElement b = sameTypeEq a b

instance Show SomeLevelElement where
  showsPrec d (SomeLevelElement l) = showsPrec d l

-- | 关卡级元素的名字。
levelNameOf :: SomeLevelElement -> ElementName
levelNameOf (SomeLevelElement l) = levelName l

-- | 拆箱：类型对得上就是 Just。
fromLevelElement :: LevelElement l => SomeLevelElement -> Maybe l
fromLevelElement (SomeLevelElement l) = cast l
