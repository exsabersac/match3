{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 元素名与自定义状态值（第 6b 刀：从 type 同义词改成 newtype）。
--
-- * ElementName —— 注册表的键，也是关卡放置表、地面层、Custom 格、前端贴图 / 播放表的键。
--   有 IsString（开 OverloadedStrings 的模块里可以直接写 "stone"），但不再和任意 String 混用；
--   计数键 CountNamed 仍是 String（计数名不一定是元素名），两者之间显式用 unElementName 转。
-- * CustomState —— Custom 格的状态值（例如剩余耐久）；只有自定义元素的编码 / 解码边界（toCell、
--   fromCustom 的解码）在它和 Int 之间转换。
--
-- 两者的 Show 都与底层值相同（"x" / 3），因此 Cell、GameState 等派生的 Show 与第 6b 刀前逐字相同
-- （金标准、元素查询快照、新旧整局对照锁定）。Ord 与底层 String / Int 相同。
--
-- Haskell 特性第 2 项（类型类与抽象，见 docs/haskell-features/02-类型类与抽象.md §1.1）：
-- 这些「和底层一样」的实例原来是手写的转发（showsPrec d (ElementName s) = showsPrec d s 等），
-- 现在用 DerivingStrategies 写明来源：Eq / Ord / Show / IsString 走 newtype 策略（GeneralizedNewtypeDeriving），
-- 编译器直接复用 String / Int 的实例字典（零开销 coerce），不可能写错成别的输出。
-- 注意 newtype 策略的 Show 不带构造器名（与 stock 的 "ElementName {unElementName = ...}" 不同），这正是这里要的。
--
-- 依赖：无（类型层最底层）。
module Match3.Types.Name
  ( ElementName(..)
  , CustomState(..)
  ) where

import Data.String (IsString)

-- | 元素名。Show 与 String 相同（带引号、转义）；IsString 即 String 自己的 fromString（= 'ElementName'）。
newtype ElementName = ElementName { unElementName :: String }
  deriving newtype (Eq, Ord, Show, IsString)

-- | 自定义格（Custom 名字 状态）的状态值。Show 与 Int 相同（负数在 showsPrec 11 下加括号）。
newtype CustomState = CustomState { unCustomState :: Int }
  deriving newtype (Eq, Ord, Show)
