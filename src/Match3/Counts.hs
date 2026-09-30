-- | 计数：一局 / 一段连锁里按计数键累计的个数（第 4 刀：替代 GameState 里 10 个专用计数字段、
-- CascadeTally 里 8 个计数字段与连锁内部的 Hits 记录；第 5 刀：颜色袋也并入，键 CountColor）。
--
-- 依赖：containers、Match3.Color。计数键 CounterKey 也在这里（元素经 Element 类的 counter / diffCounter 声明，
-- 由 Match3.Element.Types 再导出）。
-- 不变量：Counts 里不存 0（加 0 不插入，归零即删除），因此两份计数相等 ⟺ 每个键的个数相等；
-- 缺省键读作 0。个数只增不减（结算只做加法）。
module Match3.Counts
  ( CounterKey(..)
  , Counts
  , noCounts
  , countOf
  , bumpCount
  , singleCount
  , plusCounts
  , countsFromList
  , countsToList
  , namedCounts
  , colorBag
  ) where

import qualified Data.Map.Strict as M
import Match3.Color (Color, allColors)
import Match3.Types.Name (ElementName)

-- | 计数键。前 8 个是内置元素（counter / diffCounter），CountUfo / CountCarpets 是关卡特性
-- （飞碟吸走的格 / 地毯覆盖的格），CountNamed 是扩展元素按名字计数，CountColor 是各色被清除的格数
-- （第 5 刀前的颜色袋 gsColorBag / ctColors）。
-- 构造器名与第 4 刀前的 Counter 相同（元素查询快照打印 counter 的 show，逐字不变）。
data CounterKey
  = CountStones | CountChests | CountHoney | CountBalloons | CountCookies | CountCakes
  | CountSafes | CountSpirits
  | CountNamed ElementName
  | CountUfo
  | CountCarpets
  | CountColor Color
  deriving (Eq, Ord, Show)

-- | 按键累计的个数（稀疏，不含 0）。
newtype Counts = Counts (M.Map CounterKey Int)
  deriving (Eq, Ord)

instance Show Counts where
  showsPrec d c = showParen (d > 10) (showString "countsFromList " . showsPrec 11 (countsToList c))

instance Semigroup Counts where
  (<>) = plusCounts

instance Monoid Counts where
  mempty = noCounts

-- | 什么都没计。
noCounts :: Counts
noCounts = Counts M.empty

-- | 某键的个数（缺省 0）。
countOf :: CounterKey -> Counts -> Int
countOf k (Counts m) = M.findWithDefault 0 k m

-- | 某键加 n（n = 0 时不变；结果为 0 时删除该键）。
bumpCount :: CounterKey -> Int -> Counts -> Counts
bumpCount _ 0 c = c
bumpCount k n (Counts m) = Counts (M.alter step k m)
  where
    step old = case maybe n (+ n) old of
      0 -> Nothing
      v -> Just v

-- | 只有一个键的计数。
singleCount :: CounterKey -> Int -> Counts
singleCount k n = bumpCount k n noCounts

-- | 逐键相加。
plusCounts :: Counts -> Counts -> Counts
plusCounts (Counts a) (Counts b) = Counts (M.filter (/= 0) (M.unionWith (+) a b))

-- | 由 (键, 个数) 列表逐项累加（同键相加，0 忽略）。
countsFromList :: [(CounterKey, Int)] -> Counts
countsFromList = foldl (\c (k, n) -> bumpCount k n c) noCounts

-- | 全部非零计数，按键升序。
countsToList :: Counts -> [(CounterKey, Int)]
countsToList (Counts m) = M.toAscList m

-- | 扩展元素的按名字计数（CountNamed），按名字升序（第 4 刀前的 gsElementCounts 按首次出现排序；
-- 内置与测试关卡同一局最多出现一个名字，两种排序一致）。
namedCounts :: Counts -> [(ElementName, Int)]
namedCounts c = [(n, v) | (CountNamed n, v) <- countsToList c]

-- | 各色清除数（CountColor），按 allColors 顺序、含 0（第 5 刀前的颜色袋 gsColorBag / ctColors 的形状）。
colorBag :: Counts -> [(Color, Int)]
colorBag c = [(col, countOf (CountColor col) c) | col <- allColors]
