-- | 内置内容的数量清单：加元素、加关卡时只改这里。
-- 各测试引用这里的常量，不再各自写死数字（以前「关卡数」写在 8 处）。
module Spec.Support.Inventory
  ( campaignLevelCount
  , builtinEntryCount
  , builtinArchetypeCount
  ) where

-- | 战役关卡数（'Match3.Levels.Campaign.allLevels' 的长度）。
campaignLevelCount :: Int
campaignLevelCount = 49

-- | 内置元素世界条目数（'Match3.Element.Builtin.builtinDefs' 的长度）。
builtinEntryCount :: Int
builtinEntryCount = 36

-- | 内置本体原型个数（ecs-3 起本体 = 'Match3.ECS.Archetype.Archetype' 值；'Spec.Archetype.archetype_builtin_kind_inventory'
-- 在 Element/Builtin 源码里数 @… :: Archetype …@ 签名声明的原型名，四种特殊块各算一个）。
builtinArchetypeCount :: Int
builtinArchetypeCount = 25
