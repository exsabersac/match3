-- | 惰性无限流（Haskell 特性第 4 项，见 docs/haskell-features/04-惰性与递归模式.md）。
--
-- 和列表的区别只有一处：没有 [] 构造器，所以 'headS' / 'findS' / 'splitAtS' 都是全函数，
-- 不需要「列表空了怎么办」的分支（GHC 9.8 起对 head / tail 的 -Wx-partial 警告也就无从谈起）。
-- 流本身永远不会「算完」：只有被消费到的前缀才会被求值，其余部分停在一个未求值的 thunk 里。
--
-- 用法：拒绝采样（Match3.Board.Random）= 「无穷多次独立抽样」的流上取第一个合格的；
-- 自动洗牌的有限重试（Match3.Game.Shuffle）= 同一条流先 'splitAtS' 出前 24 次，再兜底取第 25 次。
-- 生成器的推进与手写递归完全一样：第 k 次抽样只在前 k−1 次都被拒绝时才会被求值。
--
-- 通用层：不 import 任何 Match3 模块（engine_layer_is_game_agnostic）。
module Engine.Stream
  ( Stream(..)
  , unfoldS
  , iterateS
  , draws
  , headS
  , takeS
  , splitAtS
  , findS
  ) where

infixr 5 :>

-- | 无穷流：一个元素接着一条流。没有空流。
data Stream a = a :> Stream a

-- | 展开（anamorphism）：种子 s 每次产出一个元素和下一个种子。
-- 结果是「余递归」的：构造器 :> 先返回，尾部留作 thunk，所以无穷展开也能立刻拿到头部。
unfoldS :: (s -> (a, s)) -> s -> Stream a
unfoldS step = go
  where
    go s = let (a, s') = step s in a :> go s'

-- | x, f x, f (f x), …
iterateS :: (a -> a) -> a -> Stream a
iterateS f = unfoldS (\x -> (x, f x))

-- | 用同一个「抽一次」函数反复抽样：第 k 项是第 k 次抽样的结果和抽完之后的生成器。
-- 例：draws randomBoard g = (b1, g1) :> (b2, g2) :> …，其中 (b1, g1) = randomBoard g、(b2, g2) = randomBoard g1。
draws :: (g -> (a, g)) -> g -> Stream (a, g)
draws sample = unfoldS (\g -> let x@(_, g') = sample g in (x, g'))

headS :: Stream a -> a
headS (a :> _) = a

-- | 前 n 项（n ≤ 0 时为空）。
takeS :: Int -> Stream a -> [a]
takeS n = fst . splitAtS n

-- | 前 n 项与剩下的流（n ≤ 0 时前缀为空）。
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS n s | n <= 0 = ([], s)
splitAtS n (a :> rest) = let (xs, s') = splitAtS (n - 1) rest in (a : xs, s')

-- | 第一个满足条件的元素（没有满足的就一直找下去——拒绝采样的语义本来就是这样）。
findS :: (a -> Bool) -> Stream a -> a
findS ok = go
  where
    go (a :> rest)
      | ok a = a
      | otherwise = go rest
