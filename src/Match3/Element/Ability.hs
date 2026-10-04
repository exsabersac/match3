{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | 元素的值级能力（xmonad LayoutClass 风格）：引擎对「一格」的全部需求拆成六个能力小类，
-- 每个类都带「普通宝石」的默认方法，元素只覆盖自己不同的那几个：
--
-- * 'Cellular'  —— 名字与写回存储（盘面仍以 'Cell' 存储，见 docs/guide/04）；
-- * 'Matchable' —— 颜色、挡匹配、挡交换、进不进普通提示；
-- * 'Hittable'  —— 直接命中的反应（'Strike'）、能否点火、爆炸范围；
-- * 'Movable'   —— 下落、传送门、边缘收集、洗牌保留、被改色 / 推动；
-- * 'Countable' —— 进入清除格的计数键、按差计数的权重、离格覆盖地毯；
-- * 'Renders'   —— 显示：基础字形与数值字段（faceBase）、前端要的附加显示字段（face）。
--
-- 类同义词 'Element' 把六个类捆在一起；存在类型 'SomeElement' 只带 'Element' 这一个约束，逐类转发每个方法
-- （新增方法要动四处：类里加方法与默认值、'SomeElement' 转发一行、Match3.Element.Layer 的 'Layered' 合成一行、
-- 'abilityProbe' 加一项；透明性测试 Spec.ElementClass 会按源码核对前三处）。
--
-- 原型包（DerivingVia）：'Obstacle'（占格障碍：挡交换、不点火、打不动、洗牌保留、不过门、不被改色 / 推动）
-- 与 'Fixed'（同障碍，另外不下落），写法 @deriving (Matchable, Hittable, Movable) via Obstacle StoneE@。
module Match3.Element.Ability
  ( -- * 能力小类
    Cellular(..)
  , Matchable(..)
  , matchColor
  , Hittable(..)
  , Strike(..)
  , Movable(..)
  , Countable(..)
  , Renders(..)
    -- * 类同义词与存在类型
  , Element
  , SomeElement(..)
  , fromElement
  , abilityProbe
    -- * 原型包与惰性占格
  , Obstacle(..)
  , Fixed(..)
  , Inert(..)
  , cellGemColor
  , sameTypeEq
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Typeable (Typeable, cast)
import Match3.Counts (CounterKey)
import Match3.Element.Types (CellField, Edge, FaceValue)
import Match3.Types

--------------------------------------------------------------------------------
-- 能力小类

-- | 名字与写回存储。
class Cellular e where
  -- | 元素名：关卡放置表、计数键、前端贴图的键（与该类型 'Match3.Element.Kind.kindName' 相同）。
  nameOf :: e -> ElementName
  -- | 把元素值写回格子（盘面的存储编码）。缺省（DefaultSignatures）：状态就是一个 Int 的 newtype 元素
  -- 写成 @Custom (nameOf e) (CustomState n)@；表示不是 Int 的元素不写 toCell 就是编译错误。
  toCell :: e -> Cell
  default toCell :: Coercible e Int => e -> Cell
  toCell e = Custom (nameOf e) (CustomState (coerce e))

-- | 匹配与交换（缺省 = 普通宝石：颜色取写回的宝石格、不挡匹配、可交换、进普通提示）。
class Cellular e => Matchable e where
  -- | 本体颜色（参与匹配与颜色袋计数）。
  color :: e -> Maybe Color
  color = cellGemColor . toCell
  -- | 不参与匹配（叠层挡住；本体缺省 False）。
  blocksMatch :: e -> Bool
  blocksMatch _ = False
  -- | 本格不能被交换。
  blocksSwap :: e -> Bool
  blocksSwap _ = False
  -- | 普通匹配提示是否试这个格（彩虹 = False：它只经成对交换规则给提示）。
  hintable :: e -> Bool
  hintable _ = True

-- | 参与匹配的颜色：挡匹配时 Nothing，否则 'color'。
matchColor :: Matchable e => e -> Maybe Color
matchColor e
  | blocksMatch e = Nothing
  | otherwise = color e

-- | 直接命中（匹配 / 特殊块 / 道具种子落在本格）时的反应。
data Strike
  = Absorb Cell  -- ^ 吃掉命中：格子变成给出的新内容，本格不消除
  | Destroy      -- ^ 本格被消除（进入清除格）
  | Immune       -- ^ 打不动：格子原样，不消除（锤子对它拒绝且不扣次数）
  deriving (Eq, Show)

-- | 受击（缺省 = 普通宝石：命中即消、能点火、没有爆炸范围）。
class Hittable e where
  struck :: e -> Strike
  struck _ = Destroy
  -- | 被消除时能否点火（特殊块引爆的前提；叠层可以否决，见 Layer 的 layerFires）。
  fires :: e -> Bool
  fires _ = True
  -- | 被消除且能点火时的爆炸范围（由盘面行列决定）。
  blast :: e -> Maybe (Board -> Pos -> [Pos])
  blast _ = Nothing

-- | 重力与移动（缺省 = 普通宝石）。
class Movable e where
  -- | 随重力下落。
  falls :: e -> Bool
  falls _ = True
  -- | 可过传送门。
  portal :: e -> Bool
  portal _ = True
  -- | 到达这些边时被收走（缺省不收）。
  drains :: e -> [Edge]
  drains _ = []
  -- | 洗牌时原样放回。
  keepOnShuffle :: e -> Bool
  keepOnShuffle _ = False
  -- | 可被魔法帽 / 染色瓶改色。
  recolorable :: e -> Bool
  recolorable _ = True
  -- | 可被蜗牛推动。
  pushable :: e -> Bool
  pushable _ = True

-- | 计数（缺省：不计数、按差计数时算 1 个、离格不算覆盖地毯）。
class Countable e where
  -- | 本体进入清除格时的计数键。
  counter :: e -> Maybe CounterKey
  counter _ = Nothing
  -- | 按差计数时这一格算几个（雪怪：左上格 = 血量，其余 0）。
  diffWeight :: e -> Int
  diffWeight _ = 1
  -- | 离开格子（不进清除格）也算覆盖地毯。
  vacatesCarpet :: e -> Bool
  vacatesCarpet _ = False

-- | 显示：前端要的附加字段（按顺序；网页格子 JSON 追加在 cellFace 字段之后）。只影响显示，不参与规则。
class Renders e where
  face :: e -> [(String, FaceValue)]
  face _ = []
  -- | 前端格子的类型标签与基本字段（网页 JSON 的 t 与其后的字段，Match3.View.cellFace）。Nothing = 缺省：
  -- Custom 格 = ("custom", name / v)，其余 = (元素名, 无字段)；宝石格（含冰层 / 叠层）由 View 按存储编码给出。
  faceBase :: e -> Maybe (String, [(String, CellField)])
  faceBase _ = Nothing

-- | 宝石格的颜色（'color' 的缺省：只有宝石格有颜色）。
cellGemColor :: Cell -> Maybe Color
cellGemColor cell = case cell of
  Gem c _ _ _ -> Just c
  _ -> Nothing

--------------------------------------------------------------------------------
-- 类同义词与存在类型

-- | 引擎看到的「一格」= 全部值级能力（类同义词，ConstraintKinds：任何实现了六个类的类型自动满足 Element）。
type Element e = (Cellular e, Matchable e, Hittable e, Movable e, Countable e, Renders e, Show e, Eq e, Typeable e)

-- | 装箱的元素（同 xmonad 的 Layout）：解码一格得到的值。
data SomeElement = forall e. Element e => SomeElement e

-- | 按具体类型比较：两边类型不同即不等（Typeable 的 cast 失败），相同再用该类型的 Eq。
instance Eq SomeElement where
  SomeElement a == SomeElement b = sameTypeEq a b

-- | 稳定的显示：元素名 + 状态值的 Show。
instance Show SomeElement where
  showsPrec d (SomeElement e) =
    showParen (d > 10) (showString "SomeElement " . showsPrec 11 (nameOf e) . showChar ' ' . showsPrec 11 e)

instance Cellular SomeElement where
  nameOf (SomeElement e) = nameOf e
  toCell (SomeElement e) = toCell e

instance Matchable SomeElement where
  color (SomeElement e) = color e
  blocksMatch (SomeElement e) = blocksMatch e
  blocksSwap (SomeElement e) = blocksSwap e
  hintable (SomeElement e) = hintable e

instance Hittable SomeElement where
  struck (SomeElement e) = struck e
  fires (SomeElement e) = fires e
  blast (SomeElement e) = blast e

instance Movable SomeElement where
  falls (SomeElement e) = falls e
  portal (SomeElement e) = portal e
  drains (SomeElement e) = drains e
  keepOnShuffle (SomeElement e) = keepOnShuffle e
  recolorable (SomeElement e) = recolorable e
  pushable (SomeElement e) = pushable e

instance Countable SomeElement where
  counter (SomeElement e) = counter e
  diffWeight (SomeElement e) = diffWeight e
  vacatesCarpet (SomeElement e) = vacatesCarpet e

instance Renders SomeElement where
  face (SomeElement e) = face e
  faceBase (SomeElement e) = faceBase e

-- | 拆箱。
fromElement :: Typeable e => SomeElement -> Maybe e
fromElement (SomeElement e) = cast e

-- | 装箱值的相等：两边的具体类型不同即不等（cast 失败），相同再用该类型自己的 Eq。
sameTypeEq :: (Typeable a, Typeable b, Eq b) => a -> b -> Bool
sameTypeEq a b = maybe False (== b) (cast a)

-- | 全部值级方法在一个元素上的读数（透明性测试用：装箱 / 叠层前后逐项比较；爆炸范围在 8×8 空盘的 (3,3) 上取）。
abilityProbe :: Element e => e -> [(String, String)]
abilityProbe e =
  [ ("nameOf", show (nameOf e))
  , ("toCell", show (toCell e))
  , ("color", show (color e))
  , ("blocksMatch", show (blocksMatch e))
  , ("blocksSwap", show (blocksSwap e))
  , ("hintable", show (hintable e))
  , ("struck", show (struck e))
  , ("fires", show (fires e))
  , ("blast", show (fmap (\f -> f probeBoard (3, 3)) (blast e)))
  , ("falls", show (falls e))
  , ("portal", show (portal e))
  , ("drains", show (drains e))
  , ("keepOnShuffle", show (keepOnShuffle e))
  , ("recolorable", show (recolorable e))
  , ("pushable", show (pushable e))
  , ("counter", show (counter e))
  , ("diffWeight", show (diffWeight e))
  , ("vacatesCarpet", show (vacatesCarpet e))
  , ("face", show (face e))
  , ("faceBase", show (faceBase e))
  ]
  where
    probeBoard = gridFromRows (replicate 8 (replicate 8 (Gem C1 Normal 0 Nothing)))

--------------------------------------------------------------------------------
-- 原型包

-- | 占格障碍的原型包：挡交换、不点火、打不动、洗牌保留、不过门、不被改色 / 推动（颜色仍按写回的格子取）。
newtype Obstacle e = Obstacle e

instance Cellular e => Cellular (Obstacle e) where
  nameOf (Obstacle e) = nameOf e
  toCell (Obstacle e) = toCell e

instance Cellular e => Matchable (Obstacle e) where
  blocksSwap _ = True

instance Hittable (Obstacle e) where
  struck _ = Immune
  fires _ = False

instance Movable (Obstacle e) where
  portal _ = False
  keepOnShuffle _ = True
  recolorable _ = False
  pushable _ = False

-- | 固定格的原型包：同障碍，另外不随重力下落（把列分段）。
newtype Fixed e = Fixed e

instance Cellular e => Cellular (Fixed e) where
  nameOf (Fixed e) = nameOf e
  toCell (Fixed e) = toCell e

instance Cellular e => Matchable (Fixed e) where
  blocksSwap _ = True

instance Hittable (Fixed e) where
  struck _ = Immune
  fires _ = False

instance Movable (Fixed e) where
  falls _ = False
  portal _ = False
  keepOnShuffle _ = True
  recolorable _ = False
  pushable _ = False

-- | 惰性占格：障碍的缺省、无色，写回原来的格子。解码兜底（未注册的 Custom 名字、没有种类认领的格子）。
data Inert = Inert ElementName Cell
  deriving (Eq, Show)
  deriving (Hittable, Movable) via (Obstacle Inert)

instance Cellular Inert where
  nameOf (Inert n _) = n
  toCell (Inert _ cell) = cell

instance Matchable Inert where
  color _ = Nothing
  blocksSwap _ = True

instance Countable Inert

instance Renders Inert
