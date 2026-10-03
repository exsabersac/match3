{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}

-- | 元素能力的简写（第 9 刀）：一种元素的能力 = 按原型的缺省（'piece' / 'blocker' / 'fixed'）再加上一串能力声明
-- （'Cap' = 改能力记录里的一个（或几个）字段），元素只写自己用到的能力：
--
-- > instance Element StoneE where
-- >   name _ = "stone"
-- >   toCell (StoneE n) = Stone n
-- >   caps (StoneE n) = blocker [hit (chip n StoneE), onAdjacent 10 (deadRule chipAdjacentStonesExcept), counts CountStones]
--
-- 能力记录本身（'Caps' 与五组记录）和查询函数在 Match3.Element.Class；这里只是按组排好的字段写入器，
-- 也可以直接用记录更新语法改字段。
--
-- Haskell 特性第 9 项（docs/haskell-features/09-规则去重.md）：
--
-- * 每个字段一个透镜（组透镜 'matchL' / 'hitL' / … 与字段透镜 'mcColorL' / 'hcOnHitL' / …，Engine.Optics），
--   字段写入器都是「把某个透镜设成某个值」（'setCap'），第 9 项前是 27 个手写的嵌套记录更新；
--   'withMatch' 等五个按组改字段的写入器 = @over 组透镜@。
-- * 'Cap' 是 @Caps -> Caps@ 的 newtype，'Semigroup' / 'Monoid' 实例经 DerivingVia 取自 @Dual (Endo Caps)@：
--   @a <> b@ = 先做 a 再做 b，所以一串声明里**后面的覆盖前面的**——第 9 项前 'withCaps' 的 @foldl (\c f -> f c)@
--   是同一个意思，现在写进了类型（'Endo' 的复合是「先右后左」，'Dual' 把它翻过来）。
--   元素定义的写法（@blocker [hit …, counts …]@）不变。
--
-- 依赖：Element.Class / Types、Match3.Types、Engine.Optics。
module Match3.Element.Caps
  ( Cap(..)
  , applyCap
  , setCap
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
  , widens
  , onMessage
    -- * 按组直接改字段（上面的简写不够用时）
  , withMatch
  , withHit
  , withMove
  , withCount
  , withStep
    -- * 透镜（第 9 项）
  , matchL
  , hitL
  , moveL
  , countL
  , stepL
  , mcColorL
  , mcBlocksSwapL
  , mcHintableL
  , mcSwapRuleL
  , hcOnHitL
  , hcActivatesL
  , hcBlastL
  , hcAdjacentL
  , hcOpenL
  , mvPortalL
  , mvDrainsL
  , mvKeepShuffleL
  , mvRecolorableL
  , mvPushableL
  , ccCounterL
  , ccDiffCounterL
  , ccDiffWeightL
  , ccBonusMovesL
  , ccVacatesCarpetL
  , stEndL
  , stGroundL
  , stWidenL
  , stMessageL
    -- * 再导出
  , module Match3.Element.Class
  ) where

import Data.Monoid (Dual(..), Endo(..))
import Engine.Optics (ASetter, Lens', lens, over, set)
import Match3.Element.Class
import Match3.Element.Types (AdjCtx, AdjOut, AdjacentRule(..), CounterKey, Edge, EndRule, OpenRule(..), SwapRule)
import Match3.Types

-- | 一条能力声明：改能力记录里的一个（或几个）字段。
--
-- 拼接 = 依次应用、后面的覆盖前面的：@applyCap (a <> b) = applyCap b . applyCap a@，'mempty' = 什么都不改。
-- 实例经 DerivingVia 取自 @Dual (Endo Caps)@（'Endo' 的 @f <> g = f . g@ 先做右边，'Dual' 交换左右）。
newtype Cap = Cap (Caps -> Caps)
  deriving (Semigroup, Monoid) via Dual (Endo Caps)

-- | 把声明作用到能力记录上。
applyCap :: Cap -> Caps -> Caps
applyCap (Cap f) = f

-- | 「把这个字段设成这个值」的声明。
setCap :: ASetter Caps Caps a a -> a -> Cap
setCap l v = Cap (set l v)

-- | 按原型的缺省能力，再依次应用各声明（后面的覆盖前面的：'Cap' 的 'Monoid'）。
withCaps :: Archetype -> [Cap] -> Caps
withCaps a cs = applyCap (mconcat cs) (capsOf a)

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
withMatch = Cap . over matchL

withHit :: (HitCaps -> HitCaps) -> Cap
withHit = Cap . over hitL

withMove :: (MoveCaps -> MoveCaps) -> Cap
withMove = Cap . over moveL

withCount :: (CountCaps -> CountCaps) -> Cap
withCount = Cap . over countL

withStep :: (StepCaps -> StepCaps) -> Cap
withStep = Cap . over stepL

-- | 本体颜色固定为给出的颜色（不看写回的格子）。
colorIs :: Color -> Cap
colorIs col = setCap (matchL . mcColorL) (const (Just col))

-- | 无色（即使写回的是宝石格）。
colorless :: Cap
colorless = setCap (matchL . mcColorL) (const Nothing)

-- | 可以被交换（障碍原型缺省挡交换）。
swappable :: Cap
swappable = setCap (matchL . mcBlocksSwapL) False

-- | 不进普通匹配提示。
notHintable :: Cap
notHintable = setCap (matchL . mcHintableL) False

-- | 成对交换规则。
onSwap :: SwapRule -> Cap
onSwap r = setCap (matchL . mcSwapRuleL) (Just r)

-- | 直接命中的反应。
hit :: Hit -> Cap
hit = setCap (hitL . hcOnHitL)

-- | 命中即破（= hit Destroy）。
breaks :: Cap
breaks = hit Destroy

-- | 不点火（特殊块被软锁时的意见；本体一般用不到）。
noFire :: Cap
noFire = setCap (hitL . hcActivatesL) (Just False)

-- | 被消除且能点火时的爆炸范围。
explodes :: (Board -> Pos -> [Pos]) -> Cap
explodes f = setCap (hitL . hcBlastL) (Just f)

-- | 邻格真消除时的反应（次序、规则）。
onAdjacent :: Int -> (AdjCtx -> Board -> AdjOut) -> Cap
onAdjacent order run = setCap (hitL . hcAdjacentL) (Just (AdjacentRule order run))

-- | 开启规则（彩蛋类）。
opens :: (Board -> [Pos] -> (Board, [Pos], [Pos])) -> Cap
opens f = setCap (hitL . hcOpenL) (Just (OpenRule f))

-- | 可过传送门。
teleports :: Cap
teleports = setCap (moveL . mvPortalL) True

-- | 到达这些边时被收走。
drainsAt :: [Edge] -> Cap
drainsAt = setCap (moveL . mvDrainsL)

-- | 洗牌时原样放回。
keepsOnShuffle :: Cap
keepsOnShuffle = setCap (moveL . mvKeepShuffleL) True

-- | 可被魔法帽 / 染色瓶改色。
recolors :: Cap
recolors = setCap (moveL . mvRecolorableL) True

-- | 可被蜗牛推动。
pushes :: Cap
pushes = setCap (moveL . mvPushableL) True

-- | 洗牌时参与重排（障碍 / 固定原型缺省原样放回）。
reshuffles :: Cap
reshuffles = setCap (moveL . mvKeepShuffleL) False

-- | 不可被改色。
noRecolor :: Cap
noRecolor = setCap (moveL . mvRecolorableL) False

-- | 不可被推动。
noPush :: Cap
noPush = setCap (moveL . mvPushableL) False

-- | 进入清除格时按这个键计数。
counts :: CounterKey -> Cap
counts k = setCap (countL . ccCounterL) (Just k)

-- | 按步前 / 步后的个数差计数。
countsDiff :: CounterKey -> Cap
countsDiff k = setCap (countL . ccDiffCounterL) (Just k)

-- | 按差计数时这一格算几个（缺省 1）。新玩法 5：雪怪 Boss 的左上格 = 当前血量、其余三格 0，
-- 个数差就是本步扣掉的血。
weighs :: Int -> Cap
weighs = setCap (countL . ccDiffWeightL)

-- | 每少一个奖励的步数。
bonus :: Int -> Cap
bonus = setCap (countL . ccBonusMovesL)

-- | 离开格子也算覆盖地毯。
vacates :: Cap
vacates = setCap (countL . ccVacatesCarpetL) True

-- | 步末规则。
atEnd :: EndRule -> Cap
atEnd r = setCap (stepL . stEndL) (Just r)

-- | 地面层规则：上方格子被消除一次时，层数 → 新层数（Nothing = 清掉）。
ground :: (Int -> Maybe Int) -> Cap
ground f = setCap (stepL . stGroundL) (Just f)

-- | 地面层（新玩法 8）：本格上的特效引爆时，爆炸范围 → 新范围（魔法地格 = 向外扩一圈）。
widens :: (Board -> [Pos] -> [Pos]) -> Cap
widens f = setCap (stepL . stWidenL) (Just f)

-- | 处理消息：Nothing = 不关心；Just = 新的元素值。
onMessage :: (SomeMessage -> Maybe SomeElement) -> Cap
onMessage = setCap (stepL . stMessageL)

--------------------------------------------------------------------------------
-- 透镜（Haskell 特性第 9 项）：五组能力各一个、每组字段各一个（只列写入器用到的字段）。

matchL :: Lens' Caps MatchCaps
matchL = lens capMatch (\c v -> c {capMatch = v})

hitL :: Lens' Caps HitCaps
hitL = lens capHit (\c v -> c {capHit = v})

moveL :: Lens' Caps MoveCaps
moveL = lens capMove (\c v -> c {capMove = v})

countL :: Lens' Caps CountCaps
countL = lens capCount (\c v -> c {capCount = v})

stepL :: Lens' Caps StepCaps
stepL = lens capStep (\c v -> c {capStep = v})

mcColorL :: Lens' MatchCaps (Cell -> Maybe Color)
mcColorL = lens mcColor (\m v -> m {mcColor = v})

mcBlocksSwapL, mcHintableL :: Lens' MatchCaps Bool
mcBlocksSwapL = lens mcBlocksSwap (\m v -> m {mcBlocksSwap = v})
mcHintableL = lens mcHintable (\m v -> m {mcHintable = v})

mcSwapRuleL :: Lens' MatchCaps (Maybe SwapRule)
mcSwapRuleL = lens mcSwapRule (\m v -> m {mcSwapRule = v})

hcOnHitL :: Lens' HitCaps Hit
hcOnHitL = lens hcOnHit (\h v -> h {hcOnHit = v})

hcActivatesL :: Lens' HitCaps (Maybe Bool)
hcActivatesL = lens hcActivates (\h v -> h {hcActivates = v})

hcBlastL :: Lens' HitCaps (Maybe (Board -> Pos -> [Pos]))
hcBlastL = lens hcBlast (\h v -> h {hcBlast = v})

hcAdjacentL :: Lens' HitCaps (Maybe AdjacentRule)
hcAdjacentL = lens hcAdjacent (\h v -> h {hcAdjacent = v})

hcOpenL :: Lens' HitCaps (Maybe OpenRule)
hcOpenL = lens hcOpen (\h v -> h {hcOpen = v})

mvPortalL, mvKeepShuffleL, mvRecolorableL, mvPushableL :: Lens' MoveCaps Bool
mvPortalL = lens mvPortal (\m v -> m {mvPortal = v})
mvKeepShuffleL = lens mvKeepShuffle (\m v -> m {mvKeepShuffle = v})
mvRecolorableL = lens mvRecolorable (\m v -> m {mvRecolorable = v})
mvPushableL = lens mvPushable (\m v -> m {mvPushable = v})

mvDrainsL :: Lens' MoveCaps [Edge]
mvDrainsL = lens mvDrains (\m v -> m {mvDrains = v})

ccCounterL, ccDiffCounterL :: Lens' CountCaps (Maybe CounterKey)
ccCounterL = lens ccCounter (\c v -> c {ccCounter = v})
ccDiffCounterL = lens ccDiffCounter (\c v -> c {ccDiffCounter = v})

ccDiffWeightL, ccBonusMovesL :: Lens' CountCaps Int
ccDiffWeightL = lens ccDiffWeight (\c v -> c {ccDiffWeight = v})
ccBonusMovesL = lens ccBonusMoves (\c v -> c {ccBonusMoves = v})

ccVacatesCarpetL :: Lens' CountCaps Bool
ccVacatesCarpetL = lens ccVacatesCarpet (\c v -> c {ccVacatesCarpet = v})

stEndL :: Lens' StepCaps (Maybe EndRule)
stEndL = lens stEnd (\s v -> s {stEnd = v})

stGroundL :: Lens' StepCaps (Maybe (Int -> Maybe Int))
stGroundL = lens stGround (\s v -> s {stGround = v})

stWidenL :: Lens' StepCaps (Maybe (Board -> [Pos] -> [Pos]))
stWidenL = lens stWiden (\s v -> s {stWiden = v})

stMessageL :: Lens' StepCaps (SomeMessage -> Maybe SomeElement)
stMessageL = lens stMessage (\s v -> s {stMessage = v})
