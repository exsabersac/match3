{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
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
import Match3.Element.Kind
import Match3.Element.Near
import Match3.Element.Phase
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

-- | 魔法帽（固定格）：邻格真消除时给相邻宝石换色。
data MagicHatE = MagicHatE
  deriving (Eq, Show)

instance Phase MagicHatE where
  codec = Codec
    { cName = "magic_hat"
    , cToCell = \_ -> MagicHat
    , cFromCell = \cell -> case cell of MagicHat -> Just MagicHatE; _ -> Nothing
    , cPlace = \_ _ -> Just MagicHat
    , cMeta = emptyMeta
    , cNear = Just (NearRule 60 AllNeighbours DiePrepend)
    , cHud = noHud
    , cPasses = []
    }
  onMatch _ = obstacleMatch
  onHit _ _ = immuneHit
  physics _ = fixedPhysics
  onNear _ ctx _ =
    let skip = nub (acTrue (ncAdj ctx) ++ acProtect (ncAdj ctx))
        b' = hatTriggerOne (acRecolor (ncAdj ctx)) (ncBoard ctx) skip (ncSelf ctx)
     in nearLocalEdit b' [] []  -- 白名单：hatTriggerOne 只改邻接可改色格
  view _ = Face (Just ("hat", [])) []

-- | 果汁机（固定格）：邻格同色真消除充能，满了产出炸弹（本轮坐住）。
data MakerE = MakerE Color Int
  deriving (Eq, Show)

instance Phase MakerE where
  codec = Codec
    { cName = "maker"
    , cToCell = \(MakerE c n) -> Maker c n
    , cFromCell = \cell -> case cell of Maker c n -> Just (MakerE c n); _ -> Nothing
    , cPlace = \args _ -> exactArgs (Maker <$> argColor <*> (max 1 <$> argInt <|> pure 3)) args
    , cMeta = emptyMeta
    , cNear = Just (NearRule 130 AllNeighbours DiePrepend)
    , cHud = noHud
    , cPasses = []
    }
  onMatch _ = obstacleMatch
  onHit _ _ = immuneHit
  physics _ = fixedPhysics
  onNear _ ctx (MakerE c n) =
    if not (any (\(_, mc) -> mc == Just c) (ncTriggers ctx))
      then NearIdle
      else if n <= 1
        then nearSelfSits ctx (Gem c Bomb 0 Nothing)
        else NearNudge (Becomes (Maker c (n - 1)))
  view (MakerE c k) = Face (Just ("maker", [colorField c, nField k])) []

-- | 蜗牛（固定格）：步末爬行 / 推动。
data SnailE = SnailE Int Int
  deriving (Eq, Show)

instance Phase SnailE where
  codec = Codec
    { cName = "snail"
    , cToCell = \(SnailE dr dc) -> Snail dr dc
    , cFromCell = \cell -> case cell of Snail dr dc -> Just (SnailE dr dc); _ -> Nothing
    , cPlace = \args _ -> exactArgs (mkSnail <$> argInt <*> argInt) args
    , cMeta = emptyMeta
    , cNear = Nothing
    , cHud = noHud
    , cPasses = [EndPass (moveRule 10 snailRun)]
    }
  onMatch _ = obstacleMatch
  onHit _ _ = immuneHit
  physics _ = fixedPhysics
  view (SnailE dr dc) = Face (Just ("snail", [("dr", FieldInt dr), ("dc", FieldInt dc)])) []

-- | 染色瓶（固定格）：邻格真消除时把正交相邻的宝石染成瓶子颜色。
newtype BottleE = BottleE Color
  deriving (Eq, Show)

instance Phase BottleE where
  codec = Codec
    { cName = "bottle"
    , cToCell = \(BottleE c) -> Bottle c
    , cFromCell = \cell -> case cell of Bottle c -> Just (BottleE c); _ -> Nothing
    , cPlace = colorPlace Bottle
    , cMeta = emptyMeta
    , cNear = Just (NearRule 140 AllNeighbours DiePrepend)
    , cHud = noHud
    , cPasses = []
    }
  onMatch _ = obstacleMatch
  onHit _ _ = immuneHit
  physics _ = fixedPhysics
  onNear _ ctx (BottleE c) =
    let skip = nub (acTrue (ncAdj ctx) ++ acProtect (ncAdj ctx))
        b' = bottleDyeOne (acRecolor (ncAdj ctx)) (ncBoard ctx) skip (ncSelf ctx) c
     in nearLocalEdit b' [] []  -- 白名单：bottleTriggerOne 只改邻接可改色格
  view (BottleE c) = Face (Just ("bottle", [colorField c])) []

-- | 倒计时炸弹：按颜色匹配、可交换 / 改色 / 推动 / 过传送门，不点火；步末减一，归零 3×3 爆炸。
data CountdownE = CountdownE Color Int
  deriving (Eq, Show)

instance Phase CountdownE where
  codec = Codec
    { cName = "countdown"
    , cToCell = \(CountdownE c n) -> Countdown c n
    , cFromCell = \cell -> case cell of Countdown c n -> Just (CountdownE c n); _ -> Nothing
    , cPlace = \args cell -> case cell of
        Gem col _ _ _ -> mkCountdown col <$> exactArgs argInt args
        Countdown col _ -> mkCountdown col <$> exactArgs argInt args
        _ -> Nothing
    , cMeta = emptyMeta
    , cNear = Nothing
    , cHud = noHud
    , cPasses = [EndPass (tickRule 10 tickRun explodeSeedsFor)]
    }
  onMatch (CountdownE c _) = gemMatch (Just c)
  onHit _ _ = HitOut Destroy False Nothing Nothing
  physics _ = gemPhysics { pKeepShuffle = True }
  view (CountdownE c k) = Face (Just ("countdown", [colorField c, nField k])) []

-- | 毛球（新玩法 3，开心消消乐的毛球）：占格本体 Custom "fuzzball"，原型 Blocker（挡交换、无色、随重力下落、洗牌原地保留）。
--
-- * 正交邻格有真消除即被消灭（邻格规则 190），被直接命中（特效 / 道具）也消灭；消灭计 CountNamed "fuzzball"；
-- * 玩家交换的步末（PhaseMove 20，蜗牛之后）：每个毛球跳到一个正交相邻的普通宝石格，和那颗宝石换位；
--   选哪一格由这一步步末开始时的盘面散列决定（伪随机，不消耗 gsGen——没有毛球的关卡随机序列与盘面都不变）；
--   没有可跳的格（四周都是障碍 / 特殊块 / 带冰或叠层的格、皮带本步移过的格、传送门端点）就不动。
-- 步末效果记为 EvBelt "fuzzball"（前端按皮带的平移动画播放：毛球与宝石互换位置）。
newtype Fuzzball = Fuzzball Int
  deriving (Eq, Show)

instance Phase Fuzzball where
  codec = Codec
    { cName = "fuzzball"
    , cToCell = intCell "fuzzball"
    , cFromCell = fromCustom "fuzzball" Fuzzball
    , cPlace = customPlace "fuzzball"
    , cMeta = emptyMeta { metaCounter = Just (CountNamed "fuzzball") }
    , cNear = Nothing
    , cHud = noHud { hudLabel = Just "毛球" }
    , cPasses = [AdjacentPass 190 fuzzballAdjacent, EndPass (moveRule 20 fuzzballRun)]
    }
  onMatch _ = obstacleMatch
  onHit _ _ = HitOut Destroy False Nothing Nothing
  physics _ = obstaclePhysics
  view _ = noFace

isFuzzball :: Cell -> Bool
isFuzzball cell = case cell of
  Custom "fuzzball" _ -> True
  _ -> False

-- | 毛球邻格（逃生口）：foldr 去重；步末跳格另见 fuzzballRun。
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
