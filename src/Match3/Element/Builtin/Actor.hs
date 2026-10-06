{-# LANGUAGE OverloadedStrings #-}
-- | 会动或会生成东西的元素：在邻格真消除时改动周围的格子，或在步末自己行动。
--
-- 共同特征：它们的规则改写的是「别的格子」——魔法帽 / 染色瓶给相邻宝石换色 / 染色（只改 recolorable 的格），
-- 果汁机充能后产出炸弹（本轮坐住），蜗牛步末爬行 / 推动（只推 pushable 的格），倒计时步末减一、归零 3×3 爆炸。
-- 魔法帽 / 果汁机 / 蜗牛 / 染色瓶是固定格（fixedPhysics，不随重力下落）；倒计时按颜色匹配、可交换 / 推动。
-- 毛球（新玩法 3，Custom "fuzzball"）：占格障碍，邻格真消除 / 命中即灭，步末跳到相邻的普通宝石格。
-- 邻格 system 次序：魔法帽 60 → 果汁机 130 → 染色瓶 140 → 毛球 190。步末：倒计时（PhaseTick 10）、蜗牛（PhaseMove 10）、
-- 毛球（PhaseMove 20）。
module Match3.Element.Builtin.Actor
  ( magicHatArch
  , makerArch
  , snailArch
  , bottleArch
  , countdownArch
  , fuzzballArch
  , fuzzballAdjacent
  , fuzzballJumps
  , traceSnails
  ) where

import Control.Applicative ((<|>))
import Engine.Optics (has, (%~), (&), (.~), (^?))
import Data.Bits (xor)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Countdown (explodeSeedsFor, tickCountdowns)
import Match3.Element.Builtin.Common (boardSeed, colorField, colorPlace, nField, pickBy, plainGem, posSeed)
import Match3.Element.Event
import Match3.ECS.Archetype
import Match3.ECS.Component
import Match3.Element.Kind (customPlace)
import Match3.Element.Near
import Match3.Element.Rules (nearBy)
import Match3.ECS.Stage
import Match3.ECS.System (System(..))
import Match3.Element.Types
import Match3.Obstacles (orthoNeighbors)
import qualified Match3.Snail as Snail
import Match3.Snail (snailPositions, stepSnailAtBy)
import Match3.Types
import Match3.Types.Optics (cellAt, cellColorT)

-- | 单顶魔法帽（与 Obstacles.triggerAdjacentHatsBy 的 triggerOne 同语义）。
hatTriggerOne :: (Cell -> Bool) -> Board -> [Pos] -> Pos -> Board
hatTriggerOne okRecolor board skip hatPos =
  let nbrs =
        [ p
        | p <- orthoNeighbors hatPos
        , inBounds board p
        , p `notElem` skip
        , okRecolor (getCell board p)
        ]
      sorted = nub (sort nbrs)
      cycleColor c = colorAt (fromEnum c + 1)
  in case sorted of
       (p1 : p2 : _)
         | Just c1 <- board ^? cellAt p1 . cellColorT
         , Just c2 <- board ^? cellAt p2 . cellColorT ->
             board & cellAt p1 . cellColorT .~ c2 & cellAt p2 . cellColorT .~ c1
       [p1]
         | has (cellAt p1 . cellColorT) board ->
             board & cellAt p1 . cellColorT %~ cycleColor
       _ -> board

-- | 单只染色瓶（与 Obstacles.triggerAdjacentBottlesBy 的 dyeOne 同语义）。
bottleDyeOne :: (Cell -> Bool) -> Board -> [Pos] -> Pos -> Color -> Board
bottleDyeOne okRecolor board skip bottlePos col =
  let nbrs =
        [ p
        | p <- orthoNeighbors bottlePos
        , inBounds board p
        , p `notElem` skip
        , okRecolor (getCell board p)
        ]
  in foldl (\bd p -> bd & cellAt p . cellColorT .~ col) board (nub nbrs)

-- | 魔法帽（固定格）：邻格真消除时给相邻宝石换色（邻格 system 60，不跳过直接命中格）。
magicHatArch :: Archetype ()
magicHatArch = (archetype "magic_hat" col)
  { aSpawn = \_ _ -> Just MagicHat
  , aPhysics = const fixedPhysics
  , aFace = const (baseFace "hat" [])
  , aSystems = [SysNear 60 (nearBy col AllNeighbours DiePrepend hatNear)]
  }
  where
    col = unitColumn (== MagicHat) MagicHat
    hatNear ctx () =
      let skip = nub (nwTrue (ncWorld ctx) ++ nwProtect (ncWorld ctx))
          b' = hatTriggerOne (nwRecolor (ncWorld ctx)) (ncBoard ctx) skip (ncSelf ctx)
       in nearLocalEdit b' [] []  -- 局部编辑：hatTriggerOne 只改邻接可改色格

-- | 果汁机（固定格，状态 = (颜色, 剩余充能)）：邻格同色真消除充能，满了产出炸弹（本轮坐住）（邻格 system 130）。
makerArch :: Archetype (Color, Int)
makerArch = (archetype "maker" col)
  { aSpawn = \args _ -> exactArgs (Maker <$> argColor <*> (max 1 <$> argInt <|> pure 3)) args
  , aPhysics = const fixedPhysics
  , aFace = \(c, k) -> baseFace "maker" [colorField c, nField k]
  , aSystems = [SysNear 130 (nearBy col AllNeighbours DiePrepend makerNear)]
  }
  where
    col = Column (\cell -> case cell of Maker c n -> Just (c, n); _ -> Nothing) (uncurry Maker)
    makerNear ctx (c, n)
      | not (any (\(_, mc) -> mc == Just c) (ncTriggers ctx)) = NearIdle
      | n <= 1 = nearSelfSits ctx (Gem c Bomb 0 Nothing)
      | otherwise = NearNudge (Becomes (Maker c (n - 1)))

-- | 蜗牛（固定格，状态 = 方向）：步末爬行 / 推动（PhaseMove 10）。
snailArch :: Archetype (Int, Int)
snailArch = (archetype "snail" (Column get (uncurry Snail)))
  { aSpawn = \args _ -> exactArgs (mkSnail <$> argInt <*> argInt) args
  , aPhysics = const fixedPhysics
  , aFace = \(dr, dc) -> baseFace "snail" [("dr", FieldInt dr), ("dc", FieldInt dc)]
  , aSystems = [SysEnd (moveSys 10 (effectSystem snailRun))]
  }
  where
    get cell = case cell of
      Snail dr dc -> Just (dr, dc)
      _ -> Nothing

-- | 染色瓶（固定格，状态 = 颜色）：邻格真消除时把正交相邻的宝石染成瓶子颜色（邻格 system 140）。
bottleArch :: Archetype Color
bottleArch = (archetype "bottle" col)
  { aSpawn = colorPlace Bottle
  , aPhysics = const fixedPhysics
  , aFace = \c -> baseFace "bottle" [colorField c]
  , aSystems = [SysNear 140 (nearBy col AllNeighbours DiePrepend dyeNear)]
  }
  where
    col = Column (\cell -> case cell of Bottle c -> Just c; _ -> Nothing) Bottle
    dyeNear ctx c =
      let skip = nub (nwTrue (ncWorld ctx) ++ nwProtect (ncWorld ctx))
          b' = bottleDyeOne (nwRecolor (ncWorld ctx)) (ncBoard ctx) skip (ncSelf ctx) c
       in nearLocalEdit b' [] []  -- 局部编辑：bottleDyeOne 只改邻接可改色格

-- | 倒计时炸弹（状态 = (颜色, 剩余步数)）：按颜色匹配、可交换 / 改色 / 推动 / 过传送门，不点火；
-- 步末减一（PhaseTick 10），归零 3×3 爆炸。
countdownArch :: Archetype (Color, Int)
countdownArch = (archetype "countdown" (Column get (uncurry Countdown)))
  { aSpawn = \args cell -> case cell of
      Gem col _ _ _ -> mkCountdown col <$> exactArgs argInt args
      Countdown col _ -> mkCountdown col <$> exactArgs argInt args
      _ -> Nothing
  , aMatch = \(c, _) -> gemMatch (Just c)
  , aHit = const breakHit
  , aPhysics = const gemPhysics {pKeepShuffle = True}
  , aFace = \(c, k) -> baseFace "countdown" [colorField c, nField k]
  , aSystems = [SysEnd (tickSys 10 (effectSystem (tickRun . ewBoard)) explodeSeedsFor)]
  }
  where
    get cell = case cell of
      Countdown c n -> Just (c, n)
      _ -> Nothing

-- | 毛球（新玩法 3，开心消消乐的毛球）：占格本体 Custom "fuzzball"，缺省原型（挡交换、无色、随重力下落、洗牌原地保留）。
--
-- * 正交邻格有真消除即被消灭（邻格 system 190），被直接命中（特效 / 道具）也消灭；消灭计 CountNamed "fuzzball"；
-- * 玩家交换的步末（PhaseMove 20，蜗牛之后）：每个毛球跳到一个正交相邻的普通宝石格，和那颗宝石换位；
--   选哪一格由这一步步末开始时的盘面散列决定（伪随机，不消耗 gsGen——没有毛球的关卡随机序列与盘面都不变）；
--   没有可跳的格（四周都是障碍 / 特殊块 / 带冰或叠层的格、皮带本步移过的格、传送门端点）就不动。
-- 步末效果记为 EvBelt "fuzzball"（前端按皮带的平移动画播放：毛球与宝石互换位置）。
fuzzballArch :: Archetype Int
fuzzballArch = (archetype "fuzzball" (customColumn "fuzzball"))
  { aSpawn = customPlace "fuzzball"
  , aHit = const breakHit
  , aTally = const emptyTally {tCounter = Just (CountNamed "fuzzball")}
  , aHud = noHud {hudLabel = Just "毛球"}
  , aSystems = [SysNear 190 fuzzballAdjacent, SysEnd (moveSys 20 (effectSystem fuzzballRun))]
  }

isFuzzball :: Cell -> Bool
isFuzzball cell = case cell of
  Custom "fuzzball" _ -> True
  _ -> False

-- | 毛球的邻格 system：foldr 去重（列表序与 nub 不同，勿改）；步末跳格另见 fuzzballRun。
fuzzballAdjacent :: System NearWorld
fuzzballAdjacent = System $ \ctx ->
  let b = nwBoard ctx
      dead = foldr (\q acc -> if q `elem` acc then acc else q : acc) []
        [ q
        | p <- nwTrue ctx
        , q <- orthoNeighbors p
        , inBounds b q
        , q `notElem` nwDirect ctx
        , q `notElem` nwTrue ctx
        , isFuzzball (getCell b q)
        ]
  in ctx {nwDead = dead, nwSit = []}

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
fuzzballRun :: EndWorld -> (Maybe EndEffect, Board)
fuzzballRun ctx =
  let b = ewBoard ctx
      (ms, b') = fuzzballJumps (ewAvoid ctx) (ewWalls ctx) b
      items = concat [[EndItem p q (getCell b p) Nothing, EndItem q p (getCell b q) Nothing] | (p, q) <- ms]
  in (if null ms then Nothing else Just (EndEffect EvBelt "fuzzball" items), b')

-- | 倒计时减一；列出数值真的变了的格。
tickRun :: Board -> (Maybe EndEffect, Board)
tickRun b =
  let b' = tickCountdowns b
      ticked = [p | p <- boardPositions b, getCell b p /= getCell b' p]
  in (if null ticked then Nothing else Just (EndEffect EvTick "countdown" [EndItem p p (getCell b' p) Nothing | p <- ticked]), b')

-- | 蜗牛爬行（跳过本步被皮带移过的格，传送门端点当墙）。
snailRun :: EndWorld -> (Maybe EndEffect, Board)
snailRun ctx =
  let (ms, b') = traceSnailsBy (ewPushable ctx) (ewAvoid ctx) (ewWalls ctx) (ewBoard ctx)
  in (if null ms then Nothing else Just (EndEffect EvMove "snail" ms), b')

-- | stepSnailsAvoidingBlocked 的逐只记录版：对同一快照顺序逐只调用 stepSnailAtBlocked，
-- 结果盘面与原函数完全一致（测试锁定）。
traceSnails :: [Pos] -> [Pos] -> Board -> ([EndItem], Board)
traceSnails = traceSnailsBy Snail.pushable

-- | traceSnails，可推动谓词由调用方给出（步末上下文 ecPushable = 元素世界的 pushable）。
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
-- 条目（注册项的分派编号由 Registry 的解码探针推导）
