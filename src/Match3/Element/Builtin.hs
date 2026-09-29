-- | 内置元素的汇总：注册表条目表 'builtinDefs'、关卡级元素表 'builtinLevelDefs' 与 'defaultRegistry'。
--
-- 每种元素一个类型 + 一个 Element / Modifier / LevelElement instance（xmonad LayoutClass 风格，
-- 见 Match3.Element.Class），按功能分组放在 Match3.Element.Builtin.* 里：
--
-- * Gem          普通宝石、特殊块（直线 / 炸弹 / 彩虹），彩虹取色与特殊合成的成对交换规则
-- * Layer        冰层与 8 种叠层（修饰器）
-- * Obstacle     打破型障碍：石头、宝箱、蜂蜜、蛋糕、气球、保险箱、双面块、彩蛋
-- * Collectible  收集与计数类：饼干、时间精灵、气泡
-- * Actor        会动或会生成东西的：魔法帽、果汁机、蜗牛、染色瓶、倒计时
-- * Ground       地面层：果冻
-- * Level        关卡级元素：飞碟、皮带、传送门、地毯
-- * Common       跨分组共用的规则 / 放置辅助函数
--
-- 具体效果仍复用原机制模块（Obstacles / Grass / Snail / Countdown / Rainbow / Combos / Ufo / Conveyor /
-- Carpet / Gravity）的函数，行为逐字不变（金标准与元素查询快照锁定）。
--
-- 邻格波及的顺序（arOrder）：石头 10 → 宝箱 20 → 蜂蜜 30 → 蛋糕 40 → 气球 50 → 魔法帽 60 → 迷雾 70 →
-- 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 保险箱 110 → 时间精灵 120 → 果汁机 130 → 染色瓶 140 → 巧克力 150 →
-- 蒸汽 160 → 气泡 170。步末：倒计时（PhaseTick）；藤 10 → 巧 20 → 蒸汽 30（PhaseSpread）；蜗牛（PhaseMove）。
-- 成对交换：彩虹取色 10 → 特殊合成 20。关卡级元素（飞碟 / 皮带 / 传送门 / 地毯）按消息回复流水线节拍。
module Match3.Element.Builtin
  ( defaultRegistry
  , builtinDefs
  , builtinLevelDefs
  , traceSnails
    -- * 元素类型（测试 / 扩展用）
  , PlainGem(..)
  , SpecialGem(..)
  , SurpriseEgg(..)
  , Ice(..)
  , Jelly(..)
  , Bubble(..)
  , UfoLevel(..)
  , BeltLevel(..)
  , PortalLevel(..)
  , CarpetLevel(..)
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
import Match3.Element.Class (SomeLevel(..))
import Match3.Element.Registry (Entry, Registry, mkRegistry, registerLevel)
import Match3.Types (GemKind(..))

-- | 内置注册表：全部内置元素。主流程的旧函数名（不带 With）都用它。
defaultRegistry :: Registry
defaultRegistry = foldl (flip registerLevel) (mkRegistry builtinDefs) builtinLevelDefs

-- | 全部内置条目（注册顺序 = 文档里的清单顺序，也是元素查询快照 R 行锁定的顺序；与分组无关，不要重排）：
-- 名字 → 构造器（原型值、从格子解码、关卡放置）。
builtinDefs :: [Entry]
builtinDefs =
  [ plainGemEntry                                       -- Gem
  , specialEntry LineH
  , specialEntry LineV
  , specialEntry Bomb
  , specialEntry Rainbow
  , iceEntry                                            -- Layer
  , grassEntry
  , vineEntry
  , chocoEntry
  , fogEntry
  , chainEntry
  , freezeEntry
  , curtainEntry
  , steamEntry
  , stoneEntry                                          -- Obstacle
  , chestEntry
  , honeyEntry
  , balloonEntry
  , cookieEntry                                         -- Collectible
  , cakeEntry                                           -- Obstacle
  , magicHatEntry                                       -- Actor
  , makerEntry
  , snailEntry
  , safeEntry                                           -- Obstacle
  , flipEntry
  , surpriseEntry
  , bottleEntry                                         -- Actor
  , timeSpiritEntry                                     -- Collectible
  , countdownEntry                                      -- Actor
  , jellyEntry                                          -- Ground
  , bubbleEntry                                         -- Collectible
  ]

-- | 内置关卡级元素：按消息回复流水线节拍；去掉某项（removeLevel）即该机制不生效。
builtinLevelDefs :: [SomeLevel]
builtinLevelDefs = [SomeLevel UfoLevel, SomeLevel BeltLevel, SomeLevel PortalLevel, SomeLevel CarpetLevel]
