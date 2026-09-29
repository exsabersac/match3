{-# LANGUAGE OverloadedStrings #-}
-- | 会动或会生成东西的元素：在邻格真消除时改动周围的格子，或在步末自己行动。
--
-- 共同特征：它们的规则改写的是「别的格子」——魔法帽 / 染色瓶给相邻宝石换色 / 染色（只改 recolorable 的格），
-- 果汁机充能后产出炸弹（本轮坐住），蜗牛步末爬行 / 推动（只推 pushable 的格），倒计时步末减一、归零 3×3 爆炸。
-- 魔法帽 / 果汁机 / 蜗牛 / 染色瓶是固定格（原型 Fixed，不随重力下落）；倒计时按颜色匹配、可交换 / 推动。
-- 邻格规则顺序：魔法帽 60 → 果汁机 130 → 染色瓶 140。步末：倒计时（PhaseTick 10）、蜗牛（PhaseMove 10）。
module Match3.Element.Builtin.Actor
  ( MagicHatE(..)
  , MakerE(..)
  , SnailE(..)
  , BottleE(..)
  , CountdownE(..)
  , traceSnails
  , magicHatEntry
  , makerEntry
  , snailEntry
  , bottleEntry
  , countdownEntry
  ) where

import Match3.Board.Grid (getCell)
import Match3.Countdown (explodeSeedsFor, tickCountdowns)
import Match3.Element.Builtin.Common (colorPlace)
import Match3.Element.Class
import Match3.Element.Event
import Match3.Element.Registry
import Match3.Element.Types
import Match3.Obstacles (chargeAdjacentMakersSit, triggerAdjacentBottlesBy, triggerAdjacentHatsBy)
import qualified Match3.Snail as Snail
import Match3.Snail (snailPositions, stepSnailAtBy)
import Match3.Types

-- | 魔法帽（固定格）：邻格真消除时给相邻宝石换色。
data MagicHatE = MagicHatE
  deriving (Eq, Show)

instance Element MagicHatE where
  name _ = "magic_hat"
  toCell _ = MagicHat
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 60 (\ctx b -> AdjOut (triggerAdjacentHatsBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] []))

-- | 果汁机（固定格）：邻格同色真消除充能，满了产出炸弹（本轮坐住）。
data MakerE = MakerE Color Int
  deriving (Eq, Show)

instance Element MakerE where
  name _ = "maker"
  toCell (MakerE c n) = Maker c n
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 130 (\ctx b -> let (b', sit) = chargeAdjacentMakersSit b (acTrue ctx) in AdjOut b' [] sit))

-- | 蜗牛（固定格）：步末爬行 / 推动。
data SnailE = SnailE Int Int
  deriving (Eq, Show)

instance Element SnailE where
  name _ = "snail"
  toCell (SnailE dr dc) = Snail dr dc
  archetype _ = Fixed
  endRule _ = Just (EndRule PhaseMove 10 snailRun (const []) (const []))

-- | 染色瓶（固定格）：邻格真消除时把正交相邻的宝石染成瓶子颜色。
newtype BottleE = BottleE Color
  deriving (Eq, Show)

instance Element BottleE where
  name _ = "bottle"
  toCell (BottleE c) = Bottle c
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 140 (\ctx b -> AdjOut (triggerAdjacentBottlesBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] []))

-- | 倒计时炸弹：按颜色匹配、可交换 / 改色 / 推动 / 过传送门，不点火；步末减一，归零 3×3 爆炸。
data CountdownE = CountdownE Color Int
  deriving (Eq, Show)

instance Element CountdownE where
  name _ = "countdown"
  toCell (CountdownE c n) = Countdown c n
  archetype _ = Blocker
  color (CountdownE c _) = Just c
  blocksSwap _ = False
  portal _ = True
  pushable _ = True
  recolorable _ = True
  onHit _ = Destroy
  endRule _ = Just (EndRule PhaseTick 10 tickRun explodeSeedsFor (const []))

-- | 倒计时减一；列出数值真的变了的格。
tickRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
tickRun _ b =
  let b' = tickCountdowns b
      ticked = [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell b p /= getCell b' p]
  in (if null ticked then Nothing else Just (EndCountdownTick ticked), b')

-- | 蜗牛爬行（跳过本步被皮带移过的格，传送门端点当墙）。
snailRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
snailRun ctx b =
  let (ms, b') = traceSnailsBy (ecPushable ctx) (ecAvoid ctx) (ecWalls ctx) b
  in (if null ms then Nothing else Just (EndSnail ms), b')

-- | stepSnailsAvoidingBlocked 的逐只记录版：对同一快照顺序逐只调用 stepSnailAtBlocked，
-- 结果盘面与原函数完全一致（测试锁定）。
traceSnails :: [Pos] -> [Pos] -> Board -> ([SnailMove], Board)
traceSnails = traceSnailsBy Snail.pushable

-- | traceSnails，可推动谓词由调用方给出（步末上下文 ecPushable = 注册表的 pushable）。
traceSnailsBy :: (Cell -> Bool) -> [Pos] -> [Pos] -> Board -> ([SnailMove], Board)
traceSnailsBy canPush avoid walls b0 =
  let (movesRev, b1) = foldl one ([], b0) [p | p <- snailPositions b0, p `notElem` avoid]
  in (reverse movesRev, b1)
  where
    -- 反向累积，收尾再反转
    one (acc, board) pos = case getCell board pos of
      Snail dr dc ->
        let board' = stepSnailAtBy canPush walls board pos
            next = (fst pos + dr, snd pos + dc)
            mv = case getCell board' pos of
              Snail dr' dc' -> SnailMove pos pos (dr', dc') Nothing
              pushed -> SnailMove pos next (dr, dc) (Just pushed)
        in (mv : acc, board')
      _ -> (acc, board)

--------------------------------------------------------------------------------
-- 条目（槽位由原型推导 = cellSlot (toCell 原型)）

magicHatEntry, makerEntry, snailEntry, bottleEntry, countdownEntry :: Entry
magicHatEntry = bodyEntry MagicHatE (\cell -> case cell of MagicHat -> Just MagicHatE; _ -> Nothing) (\_ _ -> Just MagicHat)
makerEntry = bodyEntry (MakerE C1 3) (\cell -> case cell of Maker c n -> Just (MakerE c n); _ -> Nothing) $ \args _ -> case args of
  [AColor c, AInt n] -> Just (Maker c (max 1 n))
  [AColor c] -> Just (Maker c 3)
  _ -> Nothing
snailEntry = bodyEntry (SnailE 0 1) (\cell -> case cell of Snail dr dc -> Just (SnailE dr dc); _ -> Nothing) $ \args _ -> case args of
  [AInt dr, AInt dc] -> Just (mkSnail dr dc)
  _ -> Nothing
bottleEntry = bodyEntry (BottleE C1) (\cell -> case cell of Bottle c -> Just (BottleE c); _ -> Nothing) (colorPlace Bottle)
-- 倒计时：放置参数 = 初值，颜色取自原格（宝石或倒计时）。
countdownEntry = bodyEntry (CountdownE C1 1) (\cell -> case cell of Countdown c n -> Just (CountdownE c n); _ -> Nothing) $ \args cell -> case (args, cell) of
  ([AInt n], Gem col _ _ _) -> Just (mkCountdown col n)
  ([AInt n], Countdown col _) -> Just (mkCountdown col n)
  _ -> Nothing
