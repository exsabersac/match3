-- | 内置内容的数量清单：加元素、加关卡时只改这里。
-- 各测试引用这里的常量，不再各自写死数字（以前「关卡数」写在 8 处）。
module Spec.Support.Inventory
  ( campaignLevelCount
  , builtinEntryCount
  , builtinBodyInstanceCount
  ) where

-- | 战役关卡数（'Match3.Levels.Campaign.allLevels' 的长度）。
campaignLevelCount :: Int
campaignLevelCount = 49

-- | 内置注册表条目数（'Match3.Element.Builtin.builtinDefs' 的长度）。
builtinEntryCount :: Int
builtinEntryCount = 36

-- | 内置本体元素的 instance 个数（'Spec.Caps.caps_element_class_is_thin' 在 Element/Builtin 源码里数出来的）。
builtinBodyInstanceCount :: Int
builtinBodyInstanceCount = 24
