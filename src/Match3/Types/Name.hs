-- | 元素名与自定义状态值（第 6b 刀：从 type 同义词改成 newtype）。
--
-- * ElementName —— 注册表的键，也是关卡放置表、地面层、Custom 格、前端贴图 / 播放表的键。
--   有 IsString（开 OverloadedStrings 的模块里可以直接写 "stone"），但不再和任意 String 混用；
--   计数键 CountNamed 仍是 String（计数名不一定是元素名），两者之间显式用 unElementName 转。
-- * CustomState —— Custom 格的状态值（例如剩余耐久）；只有自定义元素的编码 / 解码边界（toCell、
--   customEntry 的解码函数）在它和 Int 之间转换。
--
-- 两者的 Show 都手写成与底层值相同（"x" / 3），因此 Cell、GameState 等派生的 Show 与第 6b 刀前逐字相同
-- （金标准、元素查询快照、新旧整局对照锁定）。Ord 与底层 String / Int 相同。
--
-- 依赖：无（类型层最底层）。
module Match3.Types.Name
  ( ElementName(..)
  , CustomState(..)
  ) where

import Data.String (IsString(..))

-- | 元素名。
newtype ElementName = ElementName { unElementName :: String }
  deriving (Eq, Ord)

-- | 与 String 的 Show 相同（带引号、转义）。
instance Show ElementName where
  showsPrec d (ElementName s) = showsPrec d s

instance IsString ElementName where
  fromString = ElementName

-- | 自定义格（Custom 名字 状态）的状态值。
newtype CustomState = CustomState { unCustomState :: Int }
  deriving (Eq, Ord)

-- | 与 Int 的 Show 相同（负数在 showsPrec 11 下加括号）。
instance Show CustomState where
  showsPrec d (CustomState n) = showsPrec d n
