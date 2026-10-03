-- | Haskell 特性第 9 项前的规则折叠与能力声明（逐字副本），只给 Spec.RulesDedup 对照用：
--
-- * 'runPhase'（Match3.Game.EndPhase）与 'traceSpreadsWith'（Match3.Game.Trace）：@foldl one ([], b0) rules@ + 前插 + @reverse@；
-- * 'runAdjacentWith'（Match3.Element.Registry）：四元组累积器 + 两个前插列表 + @reverse@（规则表经导出的 adjacentRules 取）；
-- * Match3.Element.Caps 的能力声明（'withCaps' = @foldl (\c f -> f c)@、5 个 withX、27 个记录更新写入器）：
--   @type Cap = Caps -> Caps@，这里改名 'LCap'，其余逐字。
module Spec.Support.LegacyRules
  ( runPhase
  , traceSpreadsWith
  , runAdjacentWith
  , LCap
  , withCaps
  , piece
  , blocker
  , fixed
  , withMatch
  , withHit
  , withMove
  , withCount
  , withStep
  , colorIs
  , colorless
  , swappable
  , notHintable
  , onSwap
  , hit
  , breaks
  , noFire
  , explodes
  , onAdjacent
  , opens
  , teleports
  , drainsAt
  , keepsOnShuffle
  , recolors
  , pushes
  , reshuffles
  , noRecolor
  , noPush
  , counts
  , countsDiff
  , weighs
  , bonus
  , vacates
  , atEnd
  , ground
  , widens
  , onMessage
  ) where

import Data.List (nub)
import Match3.Element.Class
import Match3.Element.Registry (Registry, adjacentRules, endRules, pushableWith, recolorableWith)
import Match3.Element.Types (AdjCtx(..), AdjOut(..), AdjacentRule(..), CounterKey, Edge, EndCtx(..), EndPhase(..), EndRule(..), OpenRule(..), SwapRule)
import Match3.Game.Trace (EndStep(..))
import Match3.Types

-- | 依次跑某阶段的步末规则：返回 (步末记录, 终盘)。空效果不记录。
runPhase :: Registry -> EndPhase -> EndCtx -> Int -> Board -> ([EndStep], Board)
runPhase reg ph ctx k b0 =
  let (stepsRev, b1) = foldl one ([], b0) (endRules reg ph)
  in (reverse stepsRev, b1)
  where
    -- 反向累积，收尾再反转
    one (accRev, before) rule =
      let (eff, after) = erRun rule ctx before
      in ([EndStep k before after e | Just e <- [eff]] ++ accRev, after)

-- | traceSpreads（指定注册表）：依次执行 PhaseSpread 阶段的步末规则（按 erOrder），
-- 每条规则产出的效果记成一个 EndStep（esAfterWaves = k）。
traceSpreadsWith :: Registry -> Int -> Board -> ([EndStep], Board)
traceSpreadsWith reg k b0 =
  let (stepsRev, b1) = foldl one ([], b0) (endRules reg PhaseSpread)
  in (reverse stepsRev, b1)
  where
    -- 反向累积，收尾再反转
    one (accRev, before) rule =
      let (eff, after) = erRun rule (EndCtx [] [] (pushableWith reg)) before
      in ([EndStep k before after e | Just e <- [eff]] ++ accRev, after)

-- | 按顺序跑完一轮的全部邻格波及：返回 (盘面, 打碎的格（按规则顺序拼接）, 新生成需坐住的格)。
-- 每条规则的 acProtect = 起始保护格 ++ 之前各规则的 aoSit。
runAdjacentWith :: Registry -> [Pos] -> [Pos] -> [Pos] -> Board -> (Board, [Pos], [Pos])
runAdjacentWith reg trueClears direct protect0 b0 =
  let (b', deadRev, sitsRev, _) = foldl one (b0, [], [], nub protect0) (adjacentRules reg)
  in (b', concat (reverse deadRev), concat (reverse sitsRev))
  where
    -- 打碎格 / 坐住格按规则反向累积，收尾再反转拼接；保护格 = nub (起始保护格 ++ 之前的坐住格)，
    -- 增量维护（nub (xs ++ ys) = nub xs ++ [y | y <- nub ys, y `notElem` xs]）
    one (board, deadRev, sitsRev, protect) rule =
      let out = arRun rule (AdjCtx trueClears direct protect (recolorableWith reg)) board
          new = aoSit out
      in (aoBoard out, aoDead out : deadRev, new : sitsRev, protect ++ [p | p <- nub new, p `notElem` protect])

-- | 一条能力声明：改能力记录里的一个（或几个）字段。
type LCap = Caps -> Caps

-- | 按原型的缺省能力，再依次应用各声明（后面的覆盖前面的）。
withCaps :: Archetype -> [LCap] -> Caps
withCaps a = foldl (\c f -> f c) (capsOf a)

-- | 普通棋子：可交换、能点火、会下落、可过传送门、命中即消、可改色 / 推动、洗牌时参与重排。
piece :: [LCap] -> Caps
piece = withCaps Piece

-- | 占格障碍：挡交换、不点火、会下落、打不动、洗牌保留。
blocker :: [LCap] -> Caps
blocker = withCaps Blocker

-- | 固定格：同障碍，但不随重力下落（把列分段）。
fixed :: [LCap] -> Caps
fixed = withCaps Fixed

-- | 直接改某一组能力（字段见 "Match3.Element.Class"）。
withMatch :: (MatchCaps -> MatchCaps) -> LCap
withMatch f c = c {capMatch = f (capMatch c)}

withHit :: (HitCaps -> HitCaps) -> LCap
withHit f c = c {capHit = f (capHit c)}

withMove :: (MoveCaps -> MoveCaps) -> LCap
withMove f c = c {capMove = f (capMove c)}

withCount :: (CountCaps -> CountCaps) -> LCap
withCount f c = c {capCount = f (capCount c)}

withStep :: (StepCaps -> StepCaps) -> LCap
withStep f c = c {capStep = f (capStep c)}

-- | 本体颜色固定为给出的颜色（不看写回的格子）。
colorIs :: Color -> LCap
colorIs col = withMatch (\m -> m {mcColor = const (Just col)})

-- | 无色（即使写回的是宝石格）。
colorless :: LCap
colorless = withMatch (\m -> m {mcColor = const Nothing})

-- | 可以被交换（障碍原型缺省挡交换）。
swappable :: LCap
swappable = withMatch (\m -> m {mcBlocksSwap = False})

-- | 不进普通匹配提示。
notHintable :: LCap
notHintable = withMatch (\m -> m {mcHintable = False})

-- | 成对交换规则。
onSwap :: SwapRule -> LCap
onSwap r = withMatch (\m -> m {mcSwapRule = Just r})

-- | 直接命中的反应。
hit :: Hit -> LCap
hit h = withHit (\x -> x {hcOnHit = h})

-- | 命中即破（= hit Destroy）。
breaks :: LCap
breaks = hit Destroy

-- | 不点火（特殊块被软锁时的意见；本体一般用不到）。
noFire :: LCap
noFire = withHit (\x -> x {hcActivates = Just False})

-- | 被消除且能点火时的爆炸范围。
explodes :: (Board -> Pos -> [Pos]) -> LCap
explodes f = withHit (\x -> x {hcBlast = Just f})

-- | 邻格真消除时的反应（次序、规则）。
onAdjacent :: Int -> (AdjCtx -> Board -> AdjOut) -> LCap
onAdjacent order run = withHit (\x -> x {hcAdjacent = Just (AdjacentRule order run)})

-- | 开启规则（彩蛋类）。
opens :: (Board -> [Pos] -> (Board, [Pos], [Pos])) -> LCap
opens f = withHit (\x -> x {hcOpen = Just (OpenRule f)})

-- | 可过传送门。
teleports :: LCap
teleports = withMove (\m -> m {mvPortal = True})

-- | 到达这些边时被收走。
drainsAt :: [Edge] -> LCap
drainsAt es = withMove (\m -> m {mvDrains = es})

-- | 洗牌时原样放回。
keepsOnShuffle :: LCap
keepsOnShuffle = withMove (\m -> m {mvKeepShuffle = True})

-- | 可被魔法帽 / 染色瓶改色。
recolors :: LCap
recolors = withMove (\m -> m {mvRecolorable = True})

-- | 可被蜗牛推动。
pushes :: LCap
pushes = withMove (\m -> m {mvPushable = True})

-- | 洗牌时参与重排（障碍 / 固定原型缺省原样放回）。
reshuffles :: LCap
reshuffles = withMove (\m -> m {mvKeepShuffle = False})

-- | 不可被改色。
noRecolor :: LCap
noRecolor = withMove (\m -> m {mvRecolorable = False})

-- | 不可被推动。
noPush :: LCap
noPush = withMove (\m -> m {mvPushable = False})

-- | 进入清除格时按这个键计数。
counts :: CounterKey -> LCap
counts k = withCount (\x -> x {ccCounter = Just k})

-- | 按步前 / 步后的个数差计数。
countsDiff :: CounterKey -> LCap
countsDiff k = withCount (\x -> x {ccDiffCounter = Just k})

-- | 按差计数时这一格算几个（缺省 1）。新玩法 5：雪怪 Boss 的左上格 = 当前血量、其余三格 0，
-- 个数差就是本步扣掉的血。
weighs :: Int -> LCap
weighs n = withCount (\x -> x {ccDiffWeight = n})

-- | 每少一个奖励的步数。
bonus :: Int -> LCap
bonus n = withCount (\x -> x {ccBonusMoves = n})

-- | 离开格子也算覆盖地毯。
vacates :: LCap
vacates = withCount (\x -> x {ccVacatesCarpet = True})

-- | 步末规则。
atEnd :: EndRule -> LCap
atEnd r = withStep (\x -> x {stEnd = Just r})

-- | 地面层规则：上方格子被消除一次时，层数 → 新层数（Nothing = 清掉）。
ground :: (Int -> Maybe Int) -> LCap
ground f = withStep (\x -> x {stGround = Just f})

-- | 地面层（新玩法 8）：本格上的特效引爆时，爆炸范围 → 新范围（魔法地格 = 向外扩一圈）。
widens :: (Board -> [Pos] -> [Pos]) -> LCap
widens f = withStep (\x -> x {stWiden = Just f})

-- | 处理消息：Nothing = 不关心；Just = 新的元素值。
onMessage :: (SomeMessage -> Maybe SomeElement) -> LCap
onMessage f = withStep (\x -> x {stMessage = f})
