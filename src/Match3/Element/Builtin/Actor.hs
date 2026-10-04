{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 会动或会生成东西的元素：在邻格真消除时改动周围的格子，或在步末自己行动。
--
-- 共同特征：它们的规则改写的是「别的格子」——魔法帽 / 染色瓶给相邻宝石换色 / 染色（只改 recolorable 的格），
-- 果汁机充能后产出炸弹（本轮坐住），蜗牛步末爬行 / 推动（只推 pushable 的格），倒计时步末减一、归零 3×3 爆炸。
-- 魔法帽 / 果汁机 / 蜗牛 / 染色瓶是固定格（原型 Fixed，不随重力下落）；倒计时按颜色匹配、可交换 / 推动。
-- 毛球（新玩法 3，Custom "fuzzball"）：占格障碍，邻格真消除 / 命中即灭，步末跳到相邻的普通宝石格。
-- 邻格规则顺序：魔法帽 60 → 果汁机 130 → 染色瓶 140 → 毛球 190。步末：倒计时（PhaseTick 10）、蜗牛（PhaseMove 10）、
-- 毛球（PhaseMove 20）。
module Match3.Element.Builtin.Actor
  ( MagicHatE(..)
  , MakerE(..)
  , SnailE(..)
  , BottleE(..)
  , CountdownE(..)
  , Fuzzball(..)
  , fuzzballJumps
  , traceSnails
  ) where

import Control.Applicative ((<|>))
import Data.Bits (xor)
import Data.List.NonEmpty (NonEmpty (..))
import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Countdown (explodeSeedsFor, tickCountdowns)
import Match3.Element.Builtin.Common (boardSeed, colorPlace, pickBy, plainGem, posSeed)
import Match3.Element.Ability
import Match3.Element.Event
import Match3.Element.Kind
import Match3.Element.Types
import Match3.Obstacles (chargeAdjacentMakersSit, orthoNeighbors, triggerAdjacentBottlesBy, triggerAdjacentHatsBy)
import qualified Match3.Snail as Snail
import Match3.Snail (snailPositions, stepSnailAtBy)
import Match3.Types

-- | 魔法帽（固定格）：邻格真消除时给相邻宝石换色。
data MagicHatE = MagicHatE
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Fixed MagicHatE)

instance Cellular MagicHatE where
  nameOf _ = "magic_hat"
  toCell _ = MagicHat

instance Countable MagicHatE
instance Renders MagicHatE

instance Kind MagicHatE where
  kindName _ = "magic_hat"
  fromCell cell = case cell of
    MagicHat -> Just (MagicHatE)
    _ -> Nothing
  place _ = \_ _ -> Just MagicHat
  boardPasses _ = [AdjacentPass 60 (\ctx b -> AdjOut (triggerAdjacentHatsBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] [])]

-- | 果汁机（固定格）：邻格同色真消除充能，满了产出炸弹（本轮坐住）。
data MakerE = MakerE Color Int
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Fixed MakerE)

instance Cellular MakerE where
  nameOf _ = "maker"
  toCell (MakerE c n) = Maker c n

instance Countable MakerE
instance Renders MakerE

instance Kind MakerE where
  kindName _ = "maker"
  fromCell cell = case cell of
    Maker c n -> Just (MakerE c n)
    _ -> Nothing
  place _ = \args _ -> exactArgs (Maker <$> argColor <*> (max 1 <$> argInt <|> pure 3)) args
  boardPasses _ = [AdjacentPass 130 (\ctx b -> let (b', sit) = chargeAdjacentMakersSit b (acTrue ctx) in AdjOut b' [] sit)]

-- | 蜗牛（固定格）：步末爬行 / 推动。
data SnailE = SnailE Int Int
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Fixed SnailE)

instance Cellular SnailE where
  nameOf _ = "snail"
  toCell (SnailE dr dc) = Snail dr dc

instance Countable SnailE
instance Renders SnailE

instance Kind SnailE where
  kindName _ = "snail"
  fromCell cell = case cell of
    Snail dr dc -> Just (SnailE dr dc)
    _ -> Nothing
  place _ = \args _ -> exactArgs (mkSnail <$> argInt <*> argInt) args
  boardPasses _ = [EndPass (moveRule 10 snailRun)]

-- | 染色瓶（固定格）：邻格真消除时把正交相邻的宝石染成瓶子颜色。
newtype BottleE = BottleE Color
  deriving (Eq, Show)
  deriving (Matchable, Hittable, Movable) via (Fixed BottleE)

instance Cellular BottleE where
  nameOf _ = "bottle"
  toCell (BottleE c) = Bottle c

instance Countable BottleE
instance Renders BottleE

instance Kind BottleE where
  kindName _ = "bottle"
  fromCell cell = case cell of
    Bottle c -> Just (BottleE c)
    _ -> Nothing
  place _ = colorPlace Bottle
  boardPasses _ = [AdjacentPass 140 (\ctx b -> AdjOut (triggerAdjacentBottlesBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] [])]

-- | 倒计时炸弹：按颜色匹配、可交换 / 改色 / 推动 / 过传送门，不点火；步末减一，归零 3×3 爆炸。
data CountdownE = CountdownE Color Int
  deriving (Eq, Show)

instance Cellular CountdownE where
  nameOf _ = "countdown"
  toCell (CountdownE c n) = Countdown c n

instance Matchable CountdownE where
  color (CountdownE c _) = Just c

instance Hittable CountdownE where
  fires _ = False

instance Movable CountdownE where
  keepOnShuffle _ = True

instance Countable CountdownE
instance Renders CountdownE

instance Kind CountdownE where
  kindName _ = "countdown"
  fromCell cell = case cell of
    Countdown c n -> Just (CountdownE c n)
    _ -> Nothing
  -- 放置：[AInt 回合数]，颜色取原格（宝石 / 倒计时）
  place _ args cell = case cell of
    Gem col _ _ _ -> mkCountdown col <$> exactArgs argInt args
    Countdown col _ -> mkCountdown col <$> exactArgs argInt args
    _ -> Nothing
  boardPasses _ = [EndPass (tickRule 10 tickRun explodeSeedsFor)]

-- | 毛球（新玩法 3，开心消消乐的毛球）：占格本体 Custom "fuzzball"，原型 Blocker（挡交换、无色、随重力下落、洗牌原地保留）。
--
-- * 正交邻格有真消除即被消灭（邻格规则 190），被直接命中（特效 / 道具）也消灭；消灭计 CountNamed "fuzzball"；
-- * 玩家交换的步末（PhaseMove 20，蜗牛之后）：每个毛球跳到一个正交相邻的普通宝石格，和那颗宝石换位；
--   选哪一格由这一步步末开始时的盘面散列决定（伪随机，不消耗 gsGen——没有毛球的关卡随机序列与盘面都不变）；
--   没有可跳的格（四周都是障碍 / 特殊块 / 带冰或叠层的格、皮带本步移过的格、传送门端点）就不动。
-- 步末效果记为 EvBelt "fuzzball"（前端按皮带的平移动画播放：毛球与宝石互换位置）。
newtype Fuzzball = Fuzzball Int
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle Fuzzball)

instance Cellular Fuzzball where
  nameOf _ = "fuzzball"

instance Hittable Fuzzball where
  fires _ = False

instance Countable Fuzzball where
  counter _ = Just (CountNamed "fuzzball")

instance Renders Fuzzball

instance Kind Fuzzball where
  kindName _ = "fuzzball"
  fromCell = fromCustom "fuzzball" Fuzzball
  place _ = customPlace "fuzzball"
  label _ = Just "毛球"
  boardPasses _ = [AdjacentPass 190 fuzzballAdjacent, EndPass (moveRule 20 fuzzballRun)]

isFuzzball :: Cell -> Bool
isFuzzball cell = case cell of
  Custom "fuzzball" _ -> True
  _ -> False

-- | 邻格规则：与本轮真消除格正交相邻的毛球被消灭（并入清除格）；本轮已在清除 / 直接命中格里的不重复算。
fuzzballAdjacent :: AdjCtx -> Board -> AdjOut
fuzzballAdjacent ctx b =
  let dead = foldr (\q acc -> if q `elem` acc then acc else q : acc) []
        [ q
        | p <- acTrue ctx
        , q <- orthoNeighbors p
        , inBounds b q
        , q `notElem` acDirect ctx
        , q `notElem` acTrue ctx
        , isFuzzball (getCell b q)
        ]
  in AdjOut b dead []

-- | 步末跳格（纯函数，测试直接调用）：避让格（皮带本步移过的格）与墙（传送门端点）不跳；
-- 目标格按盘面散列选（'boardSeed' 依赖 @show board@，见 Element.Builtin.Common）；
-- 返回逐个毛球的 (原格, 新格)（行优先）与跳完的盘面。
fuzzballJumps :: [Pos] -> [Pos] -> Board -> ([(Pos, Pos)], Board)
fuzzballJumps avoid walls b0 = (reverse movesRev, bEnd)
  where
    balls = positionsWhere isFuzzball b0
    seed = boardSeed b0
    (movesRev, bEnd, _) = foldl' one ([], b0, []) balls
    one (acc, b, touched) p =
      let cands = [q | q <- orthoNeighbors p, inBounds b q, q `notElem` avoid, q `notElem` walls, q `notElem` touched, plainGem (getCell b q)]
      in case cands of
           [] -> (acc, b, touched)
           c : cs ->
             let q = pickBy (seed `xor` posSeed p) (c :| cs)
                 b' = setCell (setCell b q (getCell b p)) p (getCell b q)
             in ((p, q) : acc, b', p : q : touched)

-- | 步末：毛球跳格，每跳一次记两项（毛球 原格 → 新格、宝石 新格 → 原格）。
fuzzballRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
fuzzballRun ctx b =
  let (ms, b') = fuzzballJumps (ecAvoid ctx) (ecWalls ctx) b
      items = concat [[EndItem p q (getCell b p) Nothing, EndItem q p (getCell b q) Nothing] | (p, q) <- ms]
  in (if null ms then Nothing else Just (EndEffect EvBelt "fuzzball" items), b')

-- | 倒计时减一；列出数值真的变了的格。
tickRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
tickRun _ b =
  let b' = tickCountdowns b
      ticked = [p | p <- boardPositions b, getCell b p /= getCell b' p]
  in (if null ticked then Nothing else Just (EndEffect EvTick "countdown" [EndItem p p (getCell b' p) Nothing | p <- ticked]), b')

-- | 蜗牛爬行（跳过本步被皮带移过的格，传送门端点当墙）。
snailRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
snailRun ctx b =
  let (ms, b') = traceSnailsBy (ecPushable ctx) (ecAvoid ctx) (ecWalls ctx) b
  in (if null ms then Nothing else Just (EndEffect EvMove "snail" ms), b')

-- | stepSnailsAvoidingBlocked 的逐只记录版：对同一快照顺序逐只调用 stepSnailAtBlocked，
-- 结果盘面与原函数完全一致（测试锁定）。
traceSnails :: [Pos] -> [Pos] -> Board -> ([EndItem], Board)
traceSnails = traceSnailsBy Snail.pushable

-- | traceSnails，可推动谓词由调用方给出（步末上下文 ecPushable = 注册表的 pushable）。
traceSnailsBy :: (Cell -> Bool) -> [Pos] -> [Pos] -> Board -> ([EndItem], Board)
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
              Snail dr' dc' -> EndItem pos pos (Snail dr' dc') Nothing
              pushed -> EndItem pos next (Snail dr dc) (Just pushed)
        in (mv : acc, board')
      _ -> (acc, board)

--------------------------------------------------------------------------------
-- 条目（注册项的分派编号由 World 的解码探针推导）
