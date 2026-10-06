{-# LANGUAGE TypeApplications #-}
-- | 内置元素的汇总：内置类型表 'builtinDefs'、关卡级元素表 'builtinMechanics' 与 'defaultRegistry'。
--
-- 本体元素 = 一个原型值（Match3.ECS.Archetype：存储列 + 纯数据组件 + 自带 system）；
-- 叠层 = 'Layer' instance；地面层 = 一条 'GroundKind' 记录；关卡级元素 = Mechanic instance。
-- 按功能分组放在 Match3.Element.Builtin.* 里：
--
-- * Gem          普通宝石、特殊块（直线 / 炸弹 / 彩虹），彩虹取色的成对交换规则，特殊块形状规则表（第 8 刀）
-- * Layer        冰层与 8 种叠层（Layer）
-- * Obstacle     打破型障碍：石头、宝箱、蜂蜜、蛋糕、气球、保险箱、双面块、彩蛋、魔法石（新玩法 2）、雪怪 Boss（新玩法 5，2×2）
-- * Collectible  收集与计数类：饼干、时间精灵、气泡、变色龙（新玩法 7）
-- * Actor        会动或会生成东西的：魔法帽、果汁机、蜗牛、染色瓶、倒计时、毛球（新玩法 3）
-- * Ground       地面层：果冻、魔法地格（新玩法 8：本格上的特效引爆时范围扩一圈）
-- * Level        关卡级元素：飞碟、皮带、传送门、地毯、规则开关 L / T 炸弹（可注册 / 去掉）与地面层（核心元素）；状态在元素值里（第 7 刀）
-- * Common       跨分组共用的规则 / 放置辅助函数
--
-- 具体效果仍复用原机制模块（Obstacles / Grass / Snail / Countdown / Rainbow / Combos / Ufo / Conveyor /
-- Carpet / Gravity）的函数，行为逐字不变（金标准与元素查询快照锁定）。
--
-- 邻格 system 的次序（SysNear）：石头 10 → 宝箱 20 → 蜂蜜 30 → 蛋糕 40 → 气球 50 → 魔法帽 60 → 迷雾 70 →
-- 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 保险箱 110 → 时间精灵 120 → 果汁机 130 → 染色瓶 140 → 巧克力 150 →
-- 蒸汽 160 → 气泡 170 → 魔法石 180 → 毛球 190 → 雪怪 200。步末：倒计时 10 → 魔法石 20（PhaseTick）；藤 10 → 巧 20 → 蒸汽 30（PhaseSpread）；
-- 蜗牛 10 → 毛球 20 → 雪怪 30 → 变色龙 40（PhaseMove）。
-- 成对交换：彩虹取色 10 → 彩虹 × 变色龙 15（新玩法 7）→ 特殊合成 20（第 8 刀起 = 组合表 Match3.Combos.builtinComboRules 并成的一条）。关卡级机制（飞碟 / 皮带 / 传送门 / 地毯 …）实现各自的节拍方法（Match3.Element.Mechanic）。
module Match3.Element.Builtin
  ( defaultRegistry
  , builtinDefs
  , builtinMechanics
  , builtinShapeRules
  , builtinComboRules
  , traceSnails
    -- * 内置原型（测试 / 扩展用）
  , gemArch
  , gemColumn
  , specialArch
  , lineHArch
  , lineVArch
  , bombArch
  , rainbowArch
  , stoneArch
  , chestArch
  , honeyArch
  , cakeArch
  , balloonArch
  , safeArch
  , flipArch
  , surpriseArch
  , cookieArch
  , timeSpiritArch
  , magicHatArch
  , makerArch
  , snailArch
  , bottleArch
  , countdownArch
  , magicStoneArch
  , fuzzballArch
  , bubbleArch
  , snowBossArch
  , chameleonArch
  , magicStoneCharge
  , magicStoneFull
  , magicStoneFiring
  , magicStoneSeeds
  , fuzzballJumps
  , SnowBoss(..)
  , snowBossName
  , snowBossEvery
  , snowBossCells
  , snowBossColumn
  , snowBossEntity
  , snowBosses
  , snowBossHp
  , snowBossSpawn
  , decodeBoss
  , Ice(..)
  , jelly
  , magicGround
  , magicGroundName
  , magicWiden
  , chameleonName
  , chameleonCell
  , chameleonColor
  , chameleonNext
  , chameleonShift
  , UfoLevel(..)
  , BeltLevel(..)
  , PortalLevel(..)
  , CarpetLevel(..)
  , GroundLayer(..)
  , BombShapes(..)
  , RainbowCombos(..)
  , CookieDrop(..)
  , dropRefill
  , ltBombRule
  , withBombShapes
  , portalTeleport
  , specialBlast
  , putOverlay
  ) where

import Match3.Element.Builtin.Actor
import Match3.Element.Builtin.Collectible
import Match3.Element.Builtin.Gem
import Match3.Element.Builtin.Ground
import Match3.Element.Builtin.Layer
import Match3.Element.Builtin.Level
import Match3.Element.Builtin.Obstacle
import Match3.Element.Mechanic (SomeMechanic(..))
import Match3.Combos (builtinComboRules)
import Match3.ECS.Registry (Registry, mkRegistry, registerMechanic, setComboRules, setShapeRules)
import Match3.ECS.Registry (Def, groundDef, kindDef, layerDef)

-- | 内置元素世界：全部内置元素 + 内置规则表（第 8 刀：形状规则 builtinShapeRules、组合表 builtinComboRules；
-- 补子策略是 mkRegistry 的缺省 defaultRefill）。主流程的旧函数名（不带 With）都用它。
defaultRegistry :: Registry
defaultRegistry =
  setShapeRules builtinShapeRules . setComboRules builtinComboRules $
    foldl (flip registerMechanic) (mkRegistry builtinDefs) builtinMechanics

-- | 全部内置元素（注册顺序 = 文档里的清单顺序，也是元素查询快照 R 行锁定的顺序；与分组无关，不要重排）：
-- 本体是原型值，叠层仍是类型（ecs-4 改成 Cover 数据）。
builtinDefs :: [Def]
builtinDefs =
  [ kindDef gemArch                                      -- Gem
  , kindDef lineHArch
  , kindDef lineVArch
  , kindDef bombArch
  , kindDef rainbowArch
  , layerDef @Ice                                        -- Layer
  , layerDef @GrassL
  , layerDef @VineL
  , layerDef @ChocoL
  , layerDef @FogL
  , layerDef @ChainL
  , layerDef @FreezeL
  , layerDef @CurtainL
  , layerDef @SteamL
  , kindDef stoneArch                                    -- Obstacle
  , kindDef chestArch
  , kindDef honeyArch
  , kindDef balloonArch
  , kindDef cookieArch                                   -- Collectible
  , kindDef cakeArch                                     -- Obstacle
  , kindDef magicHatArch                                 -- Actor
  , kindDef makerArch
  , kindDef snailArch
  , kindDef safeArch                                     -- Obstacle
  , kindDef flipArch
  , kindDef surpriseArch
  , kindDef bottleArch                                   -- Actor
  , kindDef timeSpiritArch                               -- Collectible
  , kindDef countdownArch                                -- Actor
  , groundDef jelly                                      -- Ground
  , kindDef bubbleArch                                   -- Collectible
  , kindDef magicStoneArch                               -- Obstacle（新玩法 2）
  , kindDef fuzzballArch                                 -- Actor（新玩法 3）
  , kindDef snowBossArch                                 -- Obstacle（新玩法 5）
  , kindDef chameleonArch                                -- Collectible（新玩法 7）
  , groundDef magicGround                                -- Ground（新玩法 8）
  ]


-- | 内置关卡级元素的种类（原型值 = 空状态；开局状态由 mechStart 按关卡记录给出）：实现各自关心的节拍方法；去掉某项（removeMechanic）即该机制不生效。
builtinMechanics :: [SomeMechanic]
builtinMechanics = [SomeMechanic (UfoLevel []), SomeMechanic (BeltLevel []), SomeMechanic (PortalLevel []), SomeMechanic (CarpetLevel []), SomeMechanic (BombShapes False), SomeMechanic (RainbowCombos False), SomeMechanic (CookieDrop [])]
