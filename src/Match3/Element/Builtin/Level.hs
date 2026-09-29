{-# LANGUAGE OverloadedStrings #-}
-- | 关卡级元素：不在格子里、状态在 GameState 专用字段（gsUfos / gsBelts / gsPortals / gsCarpetOpen）的机制。
--
-- 共同特征：都是 LevelElement，主流程在固定的流水线节拍上发消息（Match3.Element.Message），各自只回复
-- 自己关心的那一条：飞碟 = 补子之后（Refilled → Absorbed）、皮带 = 步末倒计时之后（EndTicked → Shifted）、
-- 传送门 = 沉降时（Settling → Settled）、地毯 = 步末结算（Covering → Covered）。去掉（removeLevel）即不生效。
module Match3.Element.Builtin.Level
  ( UfoLevel(..)
  , BeltLevel(..)
  , PortalLevel(..)
  , CarpetLevel(..)
  ) where

import Match3.Board.Gravity (portalTeleport)
import Match3.Carpet (coverCarpets)
import Match3.Conveyor (beltMoves)
import Match3.Element.Class
import Match3.Element.Message
import Match3.Ufo (stepUfos)

-- | 飞碟：每轮补子之后（Refilled）整轮吸收。
data UfoLevel = UfoLevel

instance LevelElement UfoLevel where
  levelName _ = "ufo"
  levelReply _ msg = case fromMessage msg of
    Just (Refilled us b) -> Just (SomeMessage (uncurry Absorbed (stepUfos b us)))
    Nothing -> Nothing

-- | 皮带：玩家交换的步末、倒计时之后（EndTicked）给出移位。
data BeltLevel = BeltLevel

instance LevelElement BeltLevel where
  levelName _ = "belt"
  levelReply _ msg = case fromMessage msg of
    Just (EndTicked belts) -> Just (SomeMessage (Shifted (beltMoves belts)))
    Nothing -> Nothing

-- | 传送门：沉降时（Settling）传送可穿门的本体。
data PortalLevel = PortalLevel

instance LevelElement PortalLevel where
  levelName _ = "portal"
  levelReply _ msg = case fromMessage msg of
    Just (Settling canPass ps mb) -> Just (SomeMessage (Settled (portalTeleport canPass ps mb)))
    Nothing -> Nothing

-- | 地毯：步末结算时（Covering）覆盖目标格。
data CarpetLevel = CarpetLevel

instance LevelElement CarpetLevel where
  levelName _ = "carpet"
  levelReply _ msg = case fromMessage msg of
    Just (Covering open0 hit) -> Just (SomeMessage (uncurry Covered (coverCarpets open0 hit)))
    Nothing -> Nothing
