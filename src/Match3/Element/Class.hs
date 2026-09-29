{-# LANGUAGE ExistentialQuantification #-}
-- | 元素类（xmonad LayoutClass 风格）：一种元素 = 一个类型 + 一个 'Element' instance。
-- 每项能力是一个带默认实现的方法，元素只覆盖自己用到的；默认实现由 'archetype'（原型：普通棋子 /
-- 障碍 / 固定格）推出，所以普通宝石除了名字和写回格子以外全用默认方法。
--
-- * 元素自己的状态放在 instance 的值里（石头的层数、保险箱的层数……）；受击（'onHit'）和消息处理
--   （'handleMessage'）返回新的元素值（'SomeElement'，可以换成别的元素，如保险箱开成饼干）。
-- * 叠层（冰、叠层类）是修饰器 'Modifier'（对应 xmonad 的 LayoutModifier）：包在本体外面，
--   'Modified' 把修饰器和被修饰的元素合成一个元素，各方法按「修饰器先说，没意见再问里面」组合。
-- * 开放消息：'SomeMessage' + 'fromMessage'（Typeable），任何模块都能定义新消息类型。
--
-- * 关卡级元素（飞碟 / 皮带 / 传送门 / 地毯）是 'LevelElement'：不在格子里，按消息回复流水线节拍。
--
-- 盘面仍以 'Cell' 存储（稳定的编码：金标准、前端、机制模块都按它读写）；'toCell' 把元素值写回格子，
-- 注册表的构造器负责从格子解码出元素值（见 Match3.Element.Registry）。
module Match3.Element.Class
  ( -- * 元素
    Element(..)
  , Archetype(..)
  , Hit(..)
  , SomeElement(..)
  , fromElement
    -- * 修饰器（叠层）
  , Modifier(..)
  , ModHit(..)
  , SomeModifier(..)
  , fromModifier
  , Modified(..)
  , modify
  , Inert(..)
    -- * 关卡级元素
  , LevelElement(..)
  , SomeLevel(..)
  , levelNameOf
    -- * 消息（再导出自 Match3.Element.Message）
  , Message
  , SomeMessage(..)
  , fromMessage
  , sendMessage
  ) where

import Data.Typeable (Typeable, cast)
import Match3.Element.Message (Message, SomeMessage(..), fromMessage)
import Match3.Element.Types
  ( AdjacentRule
  , CounterKey
  , Edge
  , EndRule
  , OpenRule
  , SwapRule
  )
import Match3.Types

--------------------------------------------------------------------------------
-- 元素

-- | 元素的原型：决定各方法的默认实现（旧 ElementDef 的 gemDef / blocker / fixed 三个模板）。
data Archetype
  = Piece    -- ^ 普通棋子：可交换、能点火、会下落、可过传送门、命中即消、可改色 / 推动、洗牌时参与重排
  | Blocker  -- ^ 占格障碍：挡交换、不点火、会下落、打不动、洗牌保留
  | Fixed    -- ^ 固定格：同障碍，但不随重力下落（把列分段）
  deriving (Eq, Show)

-- | 直接命中时本体的反应（返回新的元素值，可以换成别的元素）。
data Hit
  = Absorb SomeElement  -- ^ 吃掉命中，格子变成给出的元素，本格不消除
  | Destroy             -- ^ 本格被消除（进入清除格）
  | Immune              -- ^ 打不动
  deriving (Eq, Show)

-- | 一种元素的全部能力。只有 'name' 和 'toCell' 必须写，其余都有默认实现。
-- 规则类能力（'adjacentRule' / 'endRule' / 'swapRule' / 'openRule' / 'groundRule'）描述「这类元素」在一轮里怎么作用于盘面，
-- 注册表在注册时从构造器给出的原型值上取一次。
class (Show e, Eq e, Typeable e) => Element e where
  -- | 元素名：注册表的键，也是关卡放置表、计数键、前端贴图的键。
  name :: e -> ElementName
  -- | 把元素值写回格子（盘面的存储编码）。
  toCell :: e -> Cell

  archetype :: e -> Archetype
  archetype _ = Piece

  -- | 本体颜色（参与匹配与颜色袋计数）；缺省取写回格子后的宝石颜色（只有宝石格有）。
  color :: e -> Maybe Color
  color e = case toCell e of
    Gem c _ _ _ -> Just c
    _ -> Nothing
  -- | 参与匹配的颜色；缺省 = 'color'（修饰器可以挡住）。
  matchColor :: e -> Maybe Color
  matchColor = color
  blocksSwap :: e -> Bool
  blocksSwap e = archetype e /= Piece
  -- | 特殊块能否点火（自上而下第一个 Just 决定）。
  activates :: e -> Maybe Bool
  activates e = Just (archetype e == Piece)
  falls :: e -> Bool
  falls e = archetype e /= Fixed
  portal :: e -> Bool
  portal e = archetype e == Piece
  -- | 边缘收集：本体到达这些边时被收走。
  drains :: e -> [Edge]
  drains _ = []
  onHit :: e -> Hit
  onHit e = if archetype e == Piece then Destroy else Immune
  -- | 真消除时随格清掉的上层（修饰器用；本体恒 False）。
  stripOnClear :: e -> Bool
  stripOnClear _ = False
  counter :: e -> Maybe CounterKey
  counter _ = Nothing
  -- | 按步前 / 步后盘面上的个数差计数（保险箱开启、时间精灵）。
  diffCounter :: e -> Maybe CounterKey
  diffCounter _ = Nothing
  bonusMoves :: e -> Int
  bonusMoves _ = 0
  -- | 离开格子（不进清除格）也算覆盖地毯。
  vacatesCarpet :: e -> Bool
  vacatesCarpet _ = False
  keepOnShuffle :: e -> Bool
  keepOnShuffle e = archetype e /= Piece
  -- | 被消除且能点火时的爆炸范围。
  blast :: e -> Maybe (Pos -> [Pos])
  blast _ = Nothing
  recolorable :: e -> Bool
  recolorable e = archetype e == Piece
  pushable :: e -> Bool
  pushable e = archetype e == Piece
  -- | 普通匹配提示是否试这个格（彩虹 = False：它只经成对交换规则给提示）。
  hintable :: e -> Bool
  hintable _ = True

  -- 规则类能力（在原型值上取）
  adjacentRule :: e -> Maybe AdjacentRule
  adjacentRule _ = Nothing
  endRule :: e -> Maybe EndRule
  endRule _ = Nothing
  swapRule :: e -> Maybe SwapRule
  swapRule _ = Nothing
  openRule :: e -> Maybe OpenRule
  openRule _ = Nothing
  -- | 地面层：上方格子被消除一次时，层数 → 新层数（Nothing = 清掉）。
  groundRule :: e -> Maybe (Int -> Maybe Int)
  groundRule _ = Nothing

  -- | 处理一条消息：Nothing = 不关心；Just = 新的元素值。
  handleMessage :: e -> SomeMessage -> Maybe SomeElement
  handleMessage _ _ = Nothing

-- | 装箱的元素（同 xmonad 的 Layout）。
data SomeElement = forall e. Element e => SomeElement e

-- | 按具体类型比较（第 6b 刀起不再比较名字字符串）：两边的元素类型不同即不等（Typeable 的 cast 失败），
-- 类型相同再用该类型的 Eq 比状态。名字是元素值的函数（name :: e -> ElementName），同类型同值必然同名，
-- 所以这比「名字相同且状态相同」更强：名字相同而类型不同的两个元素仍然不等。
instance Eq SomeElement where
  SomeElement a == SomeElement b = maybe False (== b) (cast a)

-- | 稳定的显示：元素名 + 状态值的 Show。
instance Show SomeElement where
  showsPrec d (SomeElement e) =
    showParen (d > 10) (showString "SomeElement " . showsPrec 11 (name e) . showChar ' ' . showsPrec 11 e)

instance Element SomeElement where
  name (SomeElement e) = name e
  toCell (SomeElement e) = toCell e
  archetype (SomeElement e) = archetype e
  color (SomeElement e) = color e
  matchColor (SomeElement e) = matchColor e
  blocksSwap (SomeElement e) = blocksSwap e
  activates (SomeElement e) = activates e
  falls (SomeElement e) = falls e
  portal (SomeElement e) = portal e
  drains (SomeElement e) = drains e
  onHit (SomeElement e) = onHit e
  stripOnClear (SomeElement e) = stripOnClear e
  counter (SomeElement e) = counter e
  diffCounter (SomeElement e) = diffCounter e
  bonusMoves (SomeElement e) = bonusMoves e
  vacatesCarpet (SomeElement e) = vacatesCarpet e
  keepOnShuffle (SomeElement e) = keepOnShuffle e
  blast (SomeElement e) = blast e
  recolorable (SomeElement e) = recolorable e
  pushable (SomeElement e) = pushable e
  hintable (SomeElement e) = hintable e
  adjacentRule (SomeElement e) = adjacentRule e
  endRule (SomeElement e) = endRule e
  swapRule (SomeElement e) = swapRule e
  openRule (SomeElement e) = openRule e
  groundRule (SomeElement e) = groundRule e
  handleMessage (SomeElement e) = handleMessage e

-- | 拆箱。
fromElement :: Element e => SomeElement -> Maybe e
fromElement (SomeElement e) = cast e

-- | 给元素发一条消息（不关心时原样返回）。
sendMessage :: Message m => m -> SomeElement -> SomeElement
sendMessage m e = maybe e id (handleMessage e (SomeMessage m))

--------------------------------------------------------------------------------
-- 修饰器

-- | 修饰器受直接命中的反应。
data ModHit m
  = Pierce        -- ^ 本层不管，问里面
  | Keep m        -- ^ 吃掉命中，本层换成新值（冰 3 → 2），里面不动，本格不消除
  | Remove        -- ^ 吃掉命中，本层揭掉（锁链 / 窗帘末层），本格不消除
  | Shatter       -- ^ 本格被消除（末层冰随宝石一起碎）
  deriving (Eq, Show)

-- | 叠在本体之上的一层（冰、叠层类），对应 xmonad 的 LayoutModifier：只覆盖要改的方法。
class (Show m, Eq m, Typeable m) => Modifier m where
  modName :: m -> ElementName
  -- | 把本层写回格子（冰：设冰层数；叠层：设 overlay）。
  modApply :: m -> Cell -> Cell
  -- | 盖住的宝石不参与匹配。
  modBlocksMatch :: m -> Bool
  modBlocksMatch _ = False
  modBlocksSwap :: m -> Bool
  modBlocksSwap _ = False
  -- | 点火意见：Nothing = 没意见（问里面）。
  modActivates :: m -> Maybe Bool
  modActivates _ = Nothing
  modOnHit :: m -> ModHit m
  modOnHit _ = Pierce
  -- | 本格真消除时随格清掉（草 / 藤 / 巧）。
  modStripOnClear :: m -> Bool
  modStripOnClear _ = False
  modAdjacent :: m -> Maybe AdjacentRule
  modAdjacent _ = Nothing
  modEnd :: m -> Maybe EndRule
  modEnd _ = Nothing
  -- | 处理一条消息：Nothing = 不关心；Just Nothing = 揭掉本层；Just (Just m') = 换成新值。
  modHandleMessage :: m -> SomeMessage -> Maybe (Maybe m)
  modHandleMessage _ _ = Nothing

-- | 装箱的修饰器。
data SomeModifier = forall m. Modifier m => SomeModifier m

instance Eq SomeModifier where
  SomeModifier a == SomeModifier b = maybe False (== b) (cast a)

instance Show SomeModifier where
  showsPrec d (SomeModifier m) =
    showParen (d > 10) (showString "SomeModifier " . showsPrec 11 (modName m) . showChar ' ' . showsPrec 11 m)

-- | 拆箱。
fromModifier :: Modifier m => SomeModifier -> Maybe m
fromModifier (SomeModifier m) = cast m

-- | 修饰过的元素（同 xmonad 的 ModifiedLayout）：修饰器在外，被修饰的元素（本体或更内层的修饰）在里。
data Modified = Modified SomeModifier SomeElement
  deriving (Eq, Show)

-- | 给元素套一层修饰器。
modify :: Modifier m => m -> SomeElement -> SomeElement
modify m e = SomeElement (Modified (SomeModifier m) e)

-- 组合规则与旧注册表的逐层询问一致：挡匹配 / 挡交换任一层即挡；点火与命中自上而下第一个有意见的层决定；
-- 名字 / 颜色 / 计数 / 下落等本体属性取最里面的本体；有上层就洗牌保留。
instance Element Modified where
  name (Modified _ e) = name e
  toCell (Modified (SomeModifier m) e) = modApply m (toCell e)
  archetype (Modified _ e) = archetype e
  color (Modified _ e) = color e
  matchColor (Modified (SomeModifier m) e)
    | modBlocksMatch m = Nothing
    | otherwise = matchColor e
  blocksSwap (Modified (SomeModifier m) e) = modBlocksSwap m || blocksSwap e
  activates (Modified (SomeModifier m) e) = maybe (activates e) Just (modActivates m)
  falls (Modified _ e) = falls e
  portal (Modified _ e) = portal e
  drains (Modified _ e) = drains e
  onHit (Modified (SomeModifier m) e) = case modOnHit m of
    Pierce -> case onHit e of
      Absorb e' -> Absorb (modify m e')
      r -> r
    Keep m' -> Absorb (modify m' e)
    Remove -> Absorb e
    Shatter -> Destroy
  stripOnClear (Modified (SomeModifier m) _) = modStripOnClear m
  counter (Modified _ e) = counter e
  diffCounter (Modified _ e) = diffCounter e
  bonusMoves (Modified _ e) = bonusMoves e
  vacatesCarpet (Modified _ e) = vacatesCarpet e
  keepOnShuffle _ = True
  blast (Modified _ e) = blast e
  recolorable (Modified _ e) = recolorable e
  pushable (Modified _ e) = pushable e
  hintable (Modified _ e) = hintable e
  handleMessage (Modified sm@(SomeModifier m) e) msg = case modHandleMessage m msg of
    Just Nothing -> Just e
    Just (Just m') -> Just (modify m' e)
    Nothing -> fmap (SomeElement . Modified sm) (handleMessage e msg)

--------------------------------------------------------------------------------
-- 惰性占格

-- | 惰性占格：挡交换、无色、会下落、打不动、洗牌保留，写回原来的格子。注册表用它兜底
-- （未注册的 Custom 名字、内置槽位没有注册定义），测试 / 扩展也可以直接注册它（旧 baseDef 的等价物）。
data Inert = Inert ElementName Cell
  deriving (Eq, Show)

instance Element Inert where
  name (Inert n _) = n
  toCell (Inert _ cell) = cell
  archetype _ = Blocker
  color _ = Nothing

--------------------------------------------------------------------------------
-- 关卡级元素

-- | 关卡级元素：不在格子里、状态在 GameState 专用字段的机制（飞碟 / 皮带 / 传送门 / 地毯）。
-- 主流程在流水线节拍上发消息（Match3.Element.Message 的 Refilled / EndTicked / Settling / Covering，
-- 也可以是任何新消息类型），元素自己决定回复哪些：回复 = 装箱的回复消息，Nothing = 不关心。
class Typeable l => LevelElement l where
  levelName :: l -> ElementName
  levelReply :: l -> SomeMessage -> Maybe SomeMessage
  levelReply _ _ = Nothing

-- | 装箱的关卡级元素。
data SomeLevel = forall l. LevelElement l => SomeLevel l

-- | 关卡级元素的名字。
levelNameOf :: SomeLevel -> ElementName
levelNameOf (SomeLevel l) = levelName l
