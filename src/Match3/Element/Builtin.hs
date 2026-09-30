-- | 内置元素的汇总：注册表条目表 'builtinDefs'、关卡级元素表 'builtinLevelDefs' 与 'defaultRegistry'。
--
-- 每种元素一个类型 + 一个 Element / Modifier / LevelElement instance（xmonad LayoutClass 风格，
-- 见 Match3.Element.Class），按功能分组放在 Match3.Element.Builtin.* 里：
--
-- * Gem          普通宝石、特殊块（直线 / 炸弹 / 彩虹），彩虹取色的成对交换规则，特殊块形状规则表（第 8 刀）
-- * Layer        冰层与 8 种叠层（修饰器）
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
-- 邻格波及的顺序（arOrder）：石头 10 → 宝箱 20 → 蜂蜜 30 → 蛋糕 40 → 气球 50 → 魔法帽 60 → 迷雾 70 →
-- 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 保险箱 110 → 时间精灵 120 → 果汁机 130 → 染色瓶 140 → 巧克力 150 →
-- 蒸汽 160 → 气泡 170 → 魔法石 180 → 毛球 190 → 雪怪 200。步末：倒计时 10 → 魔法石 20（PhaseTick）；藤 10 → 巧 20 → 蒸汽 30（PhaseSpread）；
-- 蜗牛 10 → 毛球 20 → 雪怪 30 → 变色龙 40（PhaseMove）。
-- 成对交换：彩虹取色 10 → 彩虹 × 变色龙 15（新玩法 7）→ 特殊合成 20（第 8 刀起 = 组合表 Match3.Combos.builtinComboRules 并成的一条）。关卡级元素（飞碟 / 皮带 / 传送门 / 地毯）按消息回复流水线节拍。
module Match3.Element.Builtin
  ( defaultRegistry
  , builtinDefs
  , builtinLevelDefs
  , builtinShapeRules
  , builtinComboRules
  , traceSnails
    -- * 元素类型（测试 / 扩展用）
  , PlainGem(..)
  , SpecialGem(..)
  , SurpriseEgg(..)
  , MagicStone(..)
  , magicStoneFull
  , magicStoneFiring
  , magicStoneSeeds
  , Fuzzball(..)
  , fuzzballJumps
  , SnowBoss(..)
  , snowBossName
  , snowBossEvery
  , snowBossCells
  , snowBosses
  , snowBossHp
  , snowBossSpawn
  , decodeBoss
  , Ice(..)
  , Jelly(..)
  , MagicGround(..)
  , magicGroundName
  , magicWiden
  , Bubble(..)
  , Chameleon(..)
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
import Match3.Element.Class (SomeLevelElement(..))
import Match3.Combos (builtinComboRules)
import Match3.Element.Registry (Entry, Registry, mkRegistry, registerLevel, setComboRules, setShapeRules)
import Match3.Types (GemKind(..))

-- | 内置注册表：全部内置元素 + 内置规则表（第 8 刀：形状规则 builtinShapeRules、组合表 builtinComboRules；
-- 补子策略是 mkRegistry 的缺省 defaultRefill）。主流程的旧函数名（不带 With）都用它。
defaultRegistry :: Registry
defaultRegistry =
  setShapeRules builtinShapeRules . setComboRules builtinComboRules $
    foldl (flip registerLevel) (mkRegistry builtinDefs) builtinLevelDefs

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
  , magicStoneEntry                                     -- Obstacle（新玩法 2）
  , fuzzballEntry                                       -- Actor（新玩法 3）
  , snowBossEntry                                       -- Obstacle（新玩法 5）
  , chameleonEntry                                      -- Collectible（新玩法 7）
  , magicGroundEntry                                    -- Ground（新玩法 8）
  ]

-- | 内置关卡级元素的种类（原型值 = 空状态；开局状态由 levelStart 按关卡记录给出）：按消息回复流水线节拍；去掉某项（removeLevel）即该机制不生效。
builtinLevelDefs :: [SomeLevelElement]
builtinLevelDefs = [SomeLevelElement (UfoLevel []), SomeLevelElement (BeltLevel []), SomeLevelElement (PortalLevel []), SomeLevelElement (CarpetLevel []), SomeLevelElement (BombShapes False), SomeLevelElement (RainbowCombos False), SomeLevelElement (CookieDrop [])]
