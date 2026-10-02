{-# LANGUAGE RankNTypes #-}

-- | 手写的 van Laarhoven 光学（Haskell 特性第 6 项，见 docs/haskell-features/06-测试与光学.md）。
--
-- 只依赖 base，不引 lens / microlens：
--
-- * Lens s t a b = 「s 里恰好有一个 a」：forall f. Functor f => (a -> f b) -> s -> f t；
-- * Traversal s t a b = 「s 里有零到多个 a」：同样的形状，但要求 Applicative；
-- * Prism s t a b = 「s 可能是 a 这一种情形（和类型的一个构造器）」：在 Choice 范畴上的同一形状，
--   取 p = (->) 时就是一个 Traversal，所以三者都能直接用函数复合 (.) 拼起来。
--
-- 读写全靠选不同的函子：view 用 Const a，over / set 用 Identity，preview 用 Const (First a)，
-- toListOf 用 Const (Endo [a])，review 用 Tagged（只构造、不拆）。
-- 属于通用层：不 import 任何 Match3 模块。定律（get-put / put-get / put-put、遍历的恒等与合成、棱镜的往返）
-- 在 test/Spec/Optics.hs 里用 QuickCheck 检查。
module Engine.Optics
  ( -- * 光学的类型
    Lens
  , Lens'
  , Traversal
  , Traversal'
  , Prism
  , Prism'
  , Getting
  , ASetter
  , AReview
  , Choice(..)
  , Tagged(..)
    -- * 构造
  , lens
  , prism
  , prism'
  , only
  , ignored
  , _Just
    -- * 使用
  , view
  , (^.)
  , over
  , (%~)
  , set
  , (.~)
  , preview
  , (^?)
  , has
  , toListOf
  , (^..)
  , review
  , (&)
  ) where

import Data.Function ((&))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Any(..), Endo(..), First(..))

infixl 8 ^., ^?, ^..
infixr 4 %~, .~

-- | 恰好一个焦点（可以换类型：s 里的 a 换成 b 得到 t）。
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

-- | 零到多个焦点，按固定顺序。
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

-- | 和类型的一种情形：能拆（preview，可能失败）也能造（review）。
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

-- | 读：把任何光学在 Const r 上实例化。
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | 写：把任何光学在 Identity 上实例化。
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

-- | 只构造：把棱镜在 Tagged 上实例化。
type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

-- | 棱镜需要的那一点 profunctor 结构：两头能接函数（dimapP），能只作用在 Either 的右边（right'）。
class Choice p where
  dimapP :: (a -> b) -> (c -> d) -> p b c -> p a d
  right' :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
  dimapP f g h = g . h . f
  right' = fmap

-- | 只有输出、忽略输入的「箭头」：review 用它把棱镜倒着跑。
newtype Tagged a b = Tagged { unTagged :: b }

instance Choice Tagged where
  dimapP _ g (Tagged b) = Tagged (g b)
  right' (Tagged b) = Tagged (Right b)

-- | 由读、写两个函数造透镜。
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt f s = sbt s <$> f (sa s)

-- | 由「造」和「拆（拆不了就原样给出 t）」造棱镜。
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimapP seta (either pure (fmap bt)) . right'

-- | 不换类型的棱镜：拆不了时 Nothing。
prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

-- | 「恰好等于 x」这一情形（没有参数的构造器，如 Grass）。
only :: Eq a => a -> Prism' a ()
only x = prism' (const x) (\y -> if y == x then Just () else Nothing)

-- | 没有焦点的遍历（「这种情形下没有可改的东西」）。
ignored :: Traversal s s a b
ignored _ = pure

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

view :: Getting a s a -> s -> a
view l = getConst . l Const

(^.) :: s -> Getting a s a -> a
s ^. l = view l s

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over

set :: ASetter s t a b -> b -> s -> t
set l b = over l (const b)

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set

-- | 第一个焦点（没有则 Nothing）。
preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)

(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = preview l s

-- | 有没有焦点。
has :: Getting Any s a -> s -> Bool
has l = getAny . getConst . l (const (Const (Any True)))

-- | 全部焦点，按遍历顺序。
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l s = appEndo (getConst (l (\a -> Const (Endo (a :))) s)) []

(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s

-- | 用棱镜造一个 s。
review :: AReview t b -> b -> t
review p = runIdentity . unTagged . p . Tagged . Identity
