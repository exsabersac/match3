-- | 元素能力的简写（第 9 刀）：一种元素的能力 = 按原型的缺省（'piece' / 'blocker' / 'fixed'）再加上一串能力声明
-- （'Cap' = 改能力记录里的一个字段），元素只写自己用到的能力：
--
-- > instance Element StoneE where
-- >   name _ = "stone"
-- >   toCell (StoneE n) = Stone n
-- >   caps (StoneE n) = blocker [hit (chip n StoneE), onAdjacent 10 (deadRule chipAdjacentStonesExcept), counts CountStones]
--
-- 能力记录本身（'Caps' 与五组记录）和查询函数在 Match3.Element.Class；这里只是按组排好的字段写入器，
-- 也可以直接用记录更新语法改字段。
--
-- 依赖：Element.Class / Types、Match3.Types。
module Match3.Element.Caps
  ( Cap
  , piece
  , blocker
  , fixed
  , withCaps
    -- * 匹配与交换（MatchCaps）
  , colorIs
  , colorless
  , swappable
  , notHintable
  , onSwap
    -- * 消除与受击（HitCaps）
  , hit
  , breaks
  , noFire
  , explodes
  , onAdjacent
  , opens
    -- * 重力与移动（MoveCaps）
  , teleports
  , drainsAt
  , keepsOnShuffle
  , recolors
  , pushes
  , reshuffles
  , noRecolor
  , noPush
    -- * 计数与目标（CountCaps）
  , counts
  , countsDiff
  , weighs
  , bonus
  , vacates
    -- * 步末与变化（StepCaps）
  , atEnd
  , ground
  , onMessage
    -- * 按组直接改字段（上面的简写不够用时）
  , withMatch
  , withHit
  , withMove
  , withCount
  , withStep
    -- * 再导出
  , module Match3.Element.Class
  ) where

import Match3.Element.Class
import Match3.Element.Types (AdjCtx, AdjOut, AdjacentRule(..), CounterKey, Edge, EndRule, OpenRule(..), SwapRule)
import Match3.Types

-- | 一条能力声明：改能力记录里的一个（或几个）字段。
type Cap = Caps -> Caps

-- | 按原型的缺省能力，再依次应用各声明（后面的覆盖前面的）。
withCaps :: Archetype -> [Cap] -> Caps
withCaps a = foldl (\c f -> f c) (capsOf a)

-- | 普通棋子：可交换、能点火、会下落、可过传送门、命中即消、可改色 / 推动、洗牌时参与重排。
piece :: [Cap] -> Caps
piece = withCaps Piece

-- | 占格障碍：挡交换、不点火、会下落、打不动、洗牌保留。
blocker :: [Cap] -> Caps
blocker = withCaps Blocker

-- | 固定格：同障碍，但不随重力下落（把列分段）。
fixed :: [Cap] -> Caps
fixed = withCaps Fixed

-- | 直接改某一组能力（字段见 "Match3.Element.Class"）。
withMatch :: (MatchCaps -> MatchCaps) -> Cap
withMatch f c = c {capMatch = f (capMatch c)}

withHit :: (HitCaps -> HitCaps) -> Cap
withHit f c = c {capHit = f (capHit c)}

withMove :: (MoveCaps -> MoveCaps) -> Cap
withMove f c = c {capMove = f (capMove c)}

withCount :: (CountCaps -> CountCaps) -> Cap
withCount f c = c {capCount = f (capCount c)}

withStep :: (StepCaps -> StepCaps) -> Cap
withStep f c = c {capStep = f (capStep c)}

-- | 本体颜色固定为给出的颜色（不看写回的格子）。
colorIs :: Color -> Cap
colorIs col = withMatch (\m -> m {mcColor = const (Just col)})

-- | 无色（即使写回的是宝石格）。
colorless :: Cap
colorless = withMatch (\m -> m {mcColor = const Nothing})

-- | 可以被交换（障碍原型缺省挡交换）。
swappable :: Cap
swappable = withMatch (\m -> m {mcBlocksSwap = False})

-- | 不进普通匹配提示。
notHintable :: Cap
notHintable = withMatch (\m -> m {mcHintable = False})

-- | 成对交换规则。
onSwap :: SwapRule -> Cap
onSwap r = withMatch (\m -> m {mcSwapRule = Just r})

-- | 直接命中的反应。
hit :: Hit -> Cap
hit h = withHit (\x -> x {hcOnHit = h})

-- | 命中即破（= hit Destroy）。
breaks :: Cap
breaks = hit Destroy

-- | 不点火（特殊块被软锁时的意见；本体一般用不到）。
noFire :: Cap
noFire = withHit (\x -> x {hcActivates = Just False})

-- | 被消除且能点火时的爆炸范围。
explodes :: (Pos -> [Pos]) -> Cap
explodes f = withHit (\x -> x {hcBlast = Just f})

-- | 邻格真消除时的反应（次序、规则）。
onAdjacent :: Int -> (AdjCtx -> Board -> AdjOut) -> Cap
onAdjacent order run = withHit (\x -> x {hcAdjacent = Just (AdjacentRule order run)})

-- | 开启规则（彩蛋类）。
opens :: (Board -> [Pos] -> (Board, [Pos], [Pos])) -> Cap
opens f = withHit (\x -> x {hcOpen = Just (OpenRule f)})

-- | 可过传送门。
teleports :: Cap
teleports = withMove (\m -> m {mvPortal = True})

-- | 到达这些边时被收走。
drainsAt :: [Edge] -> Cap
drainsAt es = withMove (\m -> m {mvDrains = es})

-- | 洗牌时原样放回。
keepsOnShuffle :: Cap
keepsOnShuffle = withMove (\m -> m {mvKeepShuffle = True})

-- | 可被魔法帽 / 染色瓶改色。
recolors :: Cap
recolors = withMove (\m -> m {mvRecolorable = True})

-- | 可被蜗牛推动。
pushes :: Cap
pushes = withMove (\m -> m {mvPushable = True})

-- | 洗牌时参与重排（障碍 / 固定原型缺省原样放回）。
reshuffles :: Cap
reshuffles = withMove (\m -> m {mvKeepShuffle = False})

-- | 不可被改色。
noRecolor :: Cap
noRecolor = withMove (\m -> m {mvRecolorable = False})

-- | 不可被推动。
noPush :: Cap
noPush = withMove (\m -> m {mvPushable = False})

-- | 进入清除格时按这个键计数。
counts :: CounterKey -> Cap
counts k = withCount (\x -> x {ccCounter = Just k})

-- | 按步前 / 步后的个数差计数。
countsDiff :: CounterKey -> Cap
countsDiff k = withCount (\x -> x {ccDiffCounter = Just k})

-- | 按差计数时这一格算几个（缺省 1）。新玩法 5：雪怪 Boss 的左上格 = 当前血量、其余三格 0，
-- 个数差就是本步扣掉的血。
weighs :: Int -> Cap
weighs n = withCount (\x -> x {ccDiffWeight = n})

-- | 每少一个奖励的步数。
bonus :: Int -> Cap
bonus n = withCount (\x -> x {ccBonusMoves = n})

-- | 离开格子也算覆盖地毯。
vacates :: Cap
vacates = withCount (\x -> x {ccVacatesCarpet = True})

-- | 步末规则。
atEnd :: EndRule -> Cap
atEnd r = withStep (\x -> x {stEnd = Just r})

-- | 地面层规则：上方格子被消除一次时，层数 → 新层数（Nothing = 清掉）。
ground :: (Int -> Maybe Int) -> Cap
ground f = withStep (\x -> x {stGround = Just f})

-- | 处理消息：Nothing = 不关心；Just = 新的元素值。
onMessage :: (SomeMessage -> Maybe SomeElement) -> Cap
onMessage f = withStep (\x -> x {stMessage = f})
