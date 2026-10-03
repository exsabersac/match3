{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | 元素类（xmonad LayoutClass 风格）：一种元素 = 一个类型 + 一个 'Element' instance。
-- 类只有 name / toCell / caps 三个方法：能力按职责分成五组带默认值的记录（'Caps'：匹配与交换 'MatchCaps'、
-- 消除与受击 'HitCaps'、重力与移动 'MoveCaps'、计数与目标 'CountCaps'、步末与变化 'StepCaps'），缺省值由原型
-- （'Archetype'：普通棋子 / 障碍 / 固定格）推出，元素只声明自己用到的能力（简写见 Match3.Element.Caps）；
-- 普通宝石除了名字和写回格子以外全用缺省。各能力的查询是普通函数（color / onHit / counter …）。
--
-- * 元素自己的状态放在 instance 的值里（石头的层数、保险箱的层数……）；受击（'onHit'）和消息处理
--   （'handleMessage'）返回新的元素值（'SomeElement'，可以换成别的元素，如保险箱开成饼干）。
-- * 叠层（冰、叠层类）是修饰器 'Modifier'（对应 xmonad 的 LayoutModifier）：包在本体外面，
--   'Modified' 把修饰器和被修饰的元素合成一个元素，各方法按「修饰器先说，没意见再问里面」组合。
-- * 开放消息：'SomeMessage' + 'fromMessage'（Typeable），任何模块都能定义新消息类型。
--
-- * 关卡级元素（飞碟 / 皮带 / 传送门 / 地毯 / 地面层）是 'LevelElement'：不在格子里，状态在值里，按消息回复流水线节拍。
--
-- 盘面仍以 'Cell' 存储（稳定的编码：金标准、前端、机制模块都按它读写）；'toCell' 把元素值写回格子，
-- 注册表的构造器负责从格子解码出元素值（见 Match3.Element.Registry）。
--
-- 三个装箱类型（'SomeElement' / 'SomeModifier' / 'SomeLevelElement'）与 Match3.Element.Message 的 'SomeMessage'
-- 是同一个套路：存在类型把「某个实现了类的类型」装进一个统一的类型，Typeable 的 cast 负责安全拆箱与按类型比较
-- （公共的 'sameTypeEq' / 'showsBoxed'）。与 xmonad 的对照、各方法的组合规则见 docs/haskell-features/02-类型类与抽象.md §1.3。
module Match3.Element.Class
  ( -- * 元素
    Element(..)
  , Archetype(..)
    -- * 能力记录
  , Caps(..)
  , MatchCaps(..)
  , HitCaps(..)
  , MoveCaps(..)
  , CountCaps(..)
  , StepCaps(..)
  , capsOf
  , matchCaps
  , hitCaps
  , moveCaps
  , countCaps
  , stepCaps
  , cellGemColor
    -- * 能力查询
  , archetype
  , color
  , matchColor
  , blocksSwap
  , hintable
  , swapRule
  , activates
  , onHit
  , blast
  , stripOnClear
  , adjacentRule
  , openRule
  , falls
  , portal
  , drains
  , keepOnShuffle
  , recolorable
  , pushable
  , counter
  , diffCounter
  , diffWeight
  , bonusMoves
  , vacatesCarpet
  , endRule
  , groundRule
  , widenRule
  , handleMessage
  , Hit(..)
  , SomeElement(..)
  , fromElement
    -- * 修饰器（叠层）
  , Modifier(..)
  , ModHit(..)
  , SomeModifier(..)
  , Modified(..)
  , modify
  , Inert(..)
    -- * 关卡级元素
  , LevelElement(..)
  , SomeLevelElement(..)
  , levelNameOf
  , fromLevelElement
    -- * 消息（再导出自 Match3.Element.Message）
  , Message
  , SomeMessage(..)
  , fromMessage
  , sendMessage
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Typeable (Typeable, cast)
import Match3.Element.Message (Message, SomeMessage(..), fromMessage)
import Match3.Levels.Level (Level)
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

-- | 元素的原型：决定各组能力的缺省值（普通棋子 / 障碍 / 固定格三个模板）。
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

-- | 匹配与交换：颜色、挡匹配 / 挡交换、进不进普通匹配提示、成对交换规则。
data MatchCaps = MatchCaps
  { mcColor       :: Cell -> Maybe Color
    -- ^ 本体颜色（参与匹配与颜色袋计数），参数是元素写回的格子；缺省取宝石格的颜色（'cellGemColor'）
  , mcBlocksMatch :: Bool             -- ^ 不参与匹配（修饰器挡住；本体缺省 False）
  , mcBlocksSwap  :: Bool             -- ^ 本格不能被交换（缺省：原型不是 Piece 即挡）
  , mcHintable    :: Bool             -- ^ 普通匹配提示是否试这个格（彩虹 = False：它只经成对交换规则给提示）
  , mcSwapRule    :: Maybe SwapRule   -- ^ 成对交换规则（在原型值上取）
  }

-- | 消除与受击：点火、直接命中、爆炸范围、上层随格清掉、邻格波及与开启规则。
data HitCaps = HitCaps
  { hcActivates :: Maybe Bool                -- ^ 特殊块能否点火（自上而下第一个 Just 决定；缺省 Just (原型 == Piece)）
  , hcOnHit     :: Hit                       -- ^ 直接命中（缺省：Piece 命中即消，其余打不动）
  , hcBlast     :: Maybe (Board -> Pos -> [Pos])  -- ^ 被消除且能点火时的爆炸范围（由盘面行列决定）
  , hcStrip     :: Bool                      -- ^ 真消除时随格清掉的上层（修饰器用；本体恒 False）
  , hcAdjacent  :: Maybe AdjacentRule        -- ^ 邻格真消除时的反应（在原型值上取）
  , hcOpen      :: Maybe OpenRule            -- ^ 开启规则（彩蛋类；在原型值上取）
  }

-- | 重力与移动：下落、传送门、边缘收集、洗牌、被改色 / 推动。
data MoveCaps = MoveCaps
  { mvFalls       :: Bool    -- ^ 随重力下落（缺省：原型不是 Fixed）
  , mvPortal      :: Bool    -- ^ 可过传送门（缺省：Piece）
  , mvDrains      :: [Edge]  -- ^ 到达这些边时被收走（缺省不收）
  , mvKeepShuffle :: Bool    -- ^ 洗牌时原样放回（缺省：原型不是 Piece）
  , mvRecolorable :: Bool    -- ^ 可被魔法帽 / 染色瓶改色（缺省：Piece）
  , mvPushable    :: Bool    -- ^ 可被蜗牛推动（缺省：Piece）
  }

-- | 计数与目标：进入清除格计数、按个数差计数、每少一个奖励步数、离格覆盖地毯。
data CountCaps = CountCaps
  { ccCounter       :: Maybe CounterKey
  , ccDiffCounter   :: Maybe CounterKey  -- ^ 按步前 / 步后盘面上的个数差计数（保险箱开启、时间精灵）
  , ccDiffWeight    :: Int               -- ^ 按差计数时这一格算几个（缺省 1 = 数格子；新玩法 5 雪怪 Boss：左上格 = 血量、其余 0）
  , ccBonusMoves    :: Int
  , ccVacatesCarpet :: Bool              -- ^ 离开格子（不进清除格）也算覆盖地毯
  }

-- | 步末与变化：步末规则、地面层规则、消息。
data StepCaps = StepCaps
  { stEnd     :: Maybe EndRule                 -- ^ 步末规则（在原型值上取）
  , stGround  :: Maybe (Int -> Maybe Int)      -- ^ 地面层：上方格子被消除一次时，层数 → 新层数（Nothing = 清掉）
  , stWiden   :: Maybe (Board -> [Pos] -> [Pos])  -- ^ 地面层（新玩法 8）：本格上的特效引爆时，爆炸范围 → 新范围（Nothing = 不改）
  , stMessage :: SomeMessage -> Maybe SomeElement  -- ^ 处理一条消息：Nothing = 不关心；Just = 新的元素值
  }

-- | 一种元素的全部能力：原型 + 五组能力记录。缺省值由原型推出（'capsOf'），元素只改自己用到的字段
-- （写法见 Match3.Element.Caps 的 piece / blocker / fixed 与各能力声明）。
data Caps = Caps
  { capArchetype :: Archetype
  , capMatch     :: MatchCaps
  , capHit       :: HitCaps
  , capMove      :: MoveCaps
  , capCount     :: CountCaps
  , capStep      :: StepCaps
  }

-- | 宝石格的颜色（'mcColor' 的缺省：只有宝石格有颜色）。
cellGemColor :: Cell -> Maybe Color
cellGemColor cell = case cell of
  Gem c _ _ _ -> Just c
  _ -> Nothing

-- | 各组能力按原型的缺省值。
matchCaps :: Archetype -> MatchCaps
matchCaps a = MatchCaps {mcColor = cellGemColor, mcBlocksMatch = False, mcBlocksSwap = a /= Piece, mcHintable = True, mcSwapRule = Nothing}

hitCaps :: Archetype -> HitCaps
hitCaps a =
  HitCaps
    { hcActivates = Just (a == Piece)
    , hcOnHit = if a == Piece then Destroy else Immune
    , hcBlast = Nothing
    , hcStrip = False
    , hcAdjacent = Nothing
    , hcOpen = Nothing
    }

moveCaps :: Archetype -> MoveCaps
moveCaps a =
  MoveCaps
    { mvFalls = a /= Fixed
    , mvPortal = a == Piece
    , mvDrains = []
    , mvKeepShuffle = a /= Piece
    , mvRecolorable = a == Piece
    , mvPushable = a == Piece
    }

countCaps :: CountCaps
countCaps = CountCaps {ccCounter = Nothing, ccDiffCounter = Nothing, ccDiffWeight = 1, ccBonusMoves = 0, ccVacatesCarpet = False}

stepCaps :: StepCaps
stepCaps = StepCaps {stEnd = Nothing, stGround = Nothing, stWiden = Nothing, stMessage = const Nothing}

-- | 按原型的全套缺省能力。
capsOf :: Archetype -> Caps
capsOf a = Caps a (matchCaps a) (hitCaps a) (moveCaps a) countCaps stepCaps

-- | 元素类（三个方法）：名字、写回格子、能力记录。只有 'name' 和 'toCell' 必须写；
-- 'caps' 缺省 = 普通棋子（capsOf Piece）。能力可以依赖元素值（石头的层数决定受击结果）。
-- 各能力的查询（'color' / 'blocksSwap' / 'onHit' / 'counter' …）是下面的普通函数。
class (Show e, Eq e, Typeable e) => Element e where
  -- | 元素名：注册表的键，也是关卡放置表、计数键、前端贴图的键。
  name :: e -> ElementName
  -- | 把元素值写回格子（盘面的存储编码）。
  --
  -- 缺省实现（DefaultSignatures，Haskell 特性第 2 项）：状态就是一个 Int 的 newtype 元素（毛球、泡泡、变色龙、
  -- 果冻、魔法地格、魔法石）写成 @Custom (name e) (CustomState n)@，名字只在 'name' 里写一次。
  -- 缺省带额外约束 Coercible e Int：只有表示与 Int 相同的元素能用它；表示不是 Int 的元素（多字段等）不写 toCell
  -- 就是编译错误。注意表示是 Int、但写回内置格子构造器的元素（StoneE → Stone n 等）必须自己写 toCell，
  -- 否则会悄悄用上缺省的 Custom 编码（取舍见文档 §5）。
  toCell :: e -> Cell
  default toCell :: Coercible e Int => e -> Cell
  toCell e = Custom (name e) (CustomState (coerce e))
  -- | 元素的能力。
  caps :: e -> Caps
  caps _ = capsOf Piece

-- 能力查询

archetype :: Element e => e -> Archetype
archetype = capArchetype . caps

-- | 本体颜色（参与匹配与颜色袋计数）。
color :: Element e => e -> Maybe Color
color e = mcColor (capMatch (caps e)) (toCell e)

-- | 参与匹配的颜色：挡匹配时 Nothing，否则 'color'。
matchColor :: Element e => e -> Maybe Color
matchColor e
  | mcBlocksMatch (capMatch (caps e)) = Nothing
  | otherwise = color e

blocksSwap :: Element e => e -> Bool
blocksSwap = mcBlocksSwap . capMatch . caps

hintable :: Element e => e -> Bool
hintable = mcHintable . capMatch . caps

swapRule :: Element e => e -> Maybe SwapRule
swapRule = mcSwapRule . capMatch . caps

activates :: Element e => e -> Maybe Bool
activates = hcActivates . capHit . caps

onHit :: Element e => e -> Hit
onHit = hcOnHit . capHit . caps

blast :: Element e => e -> Maybe (Board -> Pos -> [Pos])
blast = hcBlast . capHit . caps

stripOnClear :: Element e => e -> Bool
stripOnClear = hcStrip . capHit . caps

adjacentRule :: Element e => e -> Maybe AdjacentRule
adjacentRule = hcAdjacent . capHit . caps

openRule :: Element e => e -> Maybe OpenRule
openRule = hcOpen . capHit . caps

falls :: Element e => e -> Bool
falls = mvFalls . capMove . caps

portal :: Element e => e -> Bool
portal = mvPortal . capMove . caps

drains :: Element e => e -> [Edge]
drains = mvDrains . capMove . caps

keepOnShuffle :: Element e => e -> Bool
keepOnShuffle = mvKeepShuffle . capMove . caps

recolorable :: Element e => e -> Bool
recolorable = mvRecolorable . capMove . caps

pushable :: Element e => e -> Bool
pushable = mvPushable . capMove . caps

counter :: Element e => e -> Maybe CounterKey
counter = ccCounter . capCount . caps

diffCounter :: Element e => e -> Maybe CounterKey
diffCounter = ccDiffCounter . capCount . caps

-- | 按差计数时这一格的权重（缺省 1；见 'ccDiffWeight'）。
diffWeight :: Element e => e -> Int
diffWeight = ccDiffWeight . capCount . caps

bonusMoves :: Element e => e -> Int
bonusMoves = ccBonusMoves . capCount . caps

vacatesCarpet :: Element e => e -> Bool
vacatesCarpet = ccVacatesCarpet . capCount . caps

endRule :: Element e => e -> Maybe EndRule
endRule = stEnd . capStep . caps

groundRule :: Element e => e -> Maybe (Int -> Maybe Int)
groundRule = stGround . capStep . caps

-- | 地面层（新玩法 8）：本格上的特效引爆时改写爆炸范围（Nothing = 不改；内置只有魔法地格）。
widenRule :: Element e => e -> Maybe (Board -> [Pos] -> [Pos])
widenRule = stWiden . capStep . caps

-- | 处理一条消息：Nothing = 不关心；Just = 新的元素值。
handleMessage :: Element e => e -> SomeMessage -> Maybe SomeElement
handleMessage = stMessage . capStep . caps

-- | 装箱的元素（同 xmonad 的 Layout）。
data SomeElement = forall e. Element e => SomeElement e

-- | 按具体类型比较（不比较名字字符串）：两边的元素类型不同即不等（Typeable 的 cast 失败），
-- 类型相同再用该类型的 Eq 比状态。名字是元素值的函数（name :: e -> ElementName），同类型同值必然同名，
-- 所以这比「名字相同且状态相同」更强：名字相同而类型不同的两个元素仍然不等。
instance Eq SomeElement where
  SomeElement a == SomeElement b = sameTypeEq a b

-- | 稳定的显示：元素名 + 状态值的 Show。
instance Show SomeElement where
  showsPrec d (SomeElement e) = showsBoxed "SomeElement" (name e) e d

instance Element SomeElement where
  name (SomeElement e) = name e
  toCell (SomeElement e) = toCell e
  caps (SomeElement e) = caps e

-- | 拆箱。
fromElement :: Element e => SomeElement -> Maybe e
fromElement (SomeElement e) = cast e

-- | 装箱值的相等（三个装箱类型共用）：两边的具体类型不同即不等（cast 失败），相同再用该类型自己的 Eq。
-- 类型参数各自独立（a、b 是两个盒子里各自的类型），这正是存在类型拆开后编译器知道的全部信息。
sameTypeEq :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
sameTypeEq a b = maybe False (== b) (cast a)

-- | 装箱值的稳定显示：@标签 名字 值@（优先级 > 10 时加括号；SomeElement / SomeModifier 共用）。
showsBoxed :: Show v => String -> ElementName -> v -> Int -> ShowS
showsBoxed tag n v d =
  showParen (d > 10) (showString tag . showChar ' ' . showsPrec 11 n . showChar ' ' . showsPrec 11 v)

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
  SomeModifier a == SomeModifier b = sameTypeEq a b

instance Show SomeModifier where
  showsPrec d (SomeModifier m) = showsBoxed "SomeModifier" (modName m) m d

-- | 修饰过的元素（同 xmonad 的 ModifiedLayout）：修饰器在外，被修饰的元素（本体或更内层的修饰）在里。
data Modified = Modified SomeModifier SomeElement
  deriving (Eq, Show)

-- | 给元素套一层修饰器。
modify :: Modifier m => m -> SomeElement -> SomeElement
modify m e = SomeElement (Modified (SomeModifier m) e)

-- 组合规则（逐层询问）：挡匹配 / 挡交换任一层即挡；点火与命中自上而下第一个有意见的层决定；
-- 名字 / 颜色 / 计数 / 下落等本体属性取最里面的本体；有上层就洗牌保留；规则类能力（邻格 / 开启 / 成对交换 /
-- 步末 / 地面层）只在原型本体上取，修饰过的元素值上没有。
instance Element Modified where
  name (Modified _ e) = name e
  toCell (Modified (SomeModifier m) e) = modApply m (toCell e)
  caps (Modified sm@(SomeModifier m) e) =
    let c = caps e
        mc = capMatch c
        hc = capHit c
    in c
         { capMatch =
             mc
               { mcColor = const (color e)
               , mcBlocksMatch = modBlocksMatch m || mcBlocksMatch mc
               , mcBlocksSwap = modBlocksSwap m || mcBlocksSwap mc
               , mcSwapRule = Nothing
               }
         , capHit =
             hc
               { hcActivates = maybe (hcActivates hc) Just (modActivates m)
               , hcOnHit = case modOnHit m of
                   Pierce -> case hcOnHit hc of
                     Absorb e' -> Absorb (modify m e')
                     r -> r
                   Keep m' -> Absorb (modify m' e)
                   Remove -> Absorb e
                   Shatter -> Destroy
               , hcStrip = modStripOnClear m
               , hcAdjacent = Nothing
               , hcOpen = Nothing
               }
         , capMove = (capMove c) {mvKeepShuffle = True}
         , capStep =
             StepCaps
               { stEnd = Nothing
               , stGround = Nothing
               , stWiden = Nothing
               , stMessage = \msg -> case modHandleMessage m msg of
                   Just Nothing -> Just e
                   Just (Just m') -> Just (modify m' e)
                   Nothing -> fmap (SomeElement . Modified sm) (stMessage (capStep c) msg)
               }
         }

--------------------------------------------------------------------------------
-- 惰性占格

-- | 惰性占格：挡交换、无色、会下落、打不动、洗牌保留，写回原来的格子。注册表用它兜底
-- （未注册的 Custom 名字、内置槽位没有注册定义），测试 / 扩展也可以直接注册它。
data Inert = Inert ElementName Cell
  deriving (Eq, Show)

instance Element Inert where
  name (Inert n _) = n
  toCell (Inert _ cell) = cell
  caps _ = inertCaps

-- | 惰性占格的能力：障碍的缺省，另外无色。
inertCaps :: Caps
inertCaps = let c = capsOf Blocker in c {capMatch = (capMatch c) {mcColor = const Nothing}}

--------------------------------------------------------------------------------
-- 关卡级元素

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
