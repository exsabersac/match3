-- | 领域类型的门面（第 6 刀按职责拆成小模块，这里原名再导出，调用方不用改 import）：
--
--   * Match3.Color —— 颜色、colorAt；
--   * Match3.Types.Name —— 元素名 ElementName 与自定义状态 CustomState（第 6b 刀 newtype）；
--   * Match3.Types.Cell —— 宝石种类、叠层、单元格内容与通用读数；
--   * Match3.Types.Overlay / Match3.Types.Body —— 叠层 / 各本体的构造与谓词；
--   * Match3.Types.Board —— 坐标与盘面；
--   * Match3.Types.Game —— 分数 / 步数、结局、地面层、开局配置；
--   * Match3.Goal —— 目标数据（第 5 刀）。
--
-- 关卡记录与关卡表不在这里（它们要引用放置表 / 飞碟 / 皮带，这些模块又依赖本门面）：
-- 见 Match3.Levels.Level 与 Match3.Levels.Campaign（Match3.Core 一并再导出）。
-- specialActivates 定义软锁：多冰 / 锁链 / 窗帘下特殊块不点火。
module Match3.Types
  ( Ground
  , Color(..)
  , GemKind(..)
  , CellOverlay(..)
  , CellContents(..)
  , Cell
  , ElementName(..)
  , CustomState(..)
  , mkGem
  , mkIceGem
  , mkGrassGem
  , mkVineGem
  , mkChocoGem
  , iceLayers
  , specialActivates
  , cellOverlay
  , hasGrass
  , hasVine
  , hasChoco
  , hasFog
  , fogLayers
  , mkFogGem
  , hasChain
  , chainLayers
  , mkChainGem
  , hasFreeze
  , freezeLayers
  , mkFreezeGem
  , hasCurtain
  , curtainLayers
  , mkCurtainGem
  , mkSnail
  , isSnail
  , snailDir
  , mkSafe
  , mkSafeLayers
  , safeLayers
  , isSafe
  , mkFlip
  , isFlip
  , flipFront
  , flipBack
  , mkSurprise
  , isSurprise
  , mkBottle
  , isBottle
  , bottleColor
  , mkTimeSpirit
  , isTimeSpirit
  , mkSteamGem
  , hasSteam
  , clearOverlay
  , setOverlay
  , mkStone
  , mkStoneLayers
  , stoneLayers
  , isStone
  , mkChest
  , mkChestLayers
  , chestLayers
  , isChest
  , mkHoney
  , mkHoneyLayers
  , honeyLayers
  , isHoney
  , mkBalloon
  , isBalloon
  , balloonColor
  , mkCookie
  , isCookie
  , mkCake
  , mkCakeLayers
  , cakeLayers
  , isCake
  , mkMagicHat
  , isMagicHat
  , mkMaker
  , mkMakerCharges
  , makerColor
  , makerCharges
  , isMaker
  , mkCountdown
  , isCountdown
  , countdownTurns
  , mkCustom
  , isCustom
  , isGem
  , cellColor
  , cellKind
  , Pos
  , Board
  , boardFromRows
  , boardRows
  , boardCells
  , boardAssocs
  , boardAt
  , boardSet
  , boardSetMany
  , boardArray
  , boardFromArray
  , mapBoard
  , boardSize
  , numColors
  , allColors
  , colorAt
  , Score
  , MovesLeft
  , TargetScore
  , Outcome(..)
  , Meter(..)
  , Quota(..)
  , LevelGoal(..)
  , goalScore
  , goalCollect
  , goalColors
  , goalCount
  , GoalView(..)
  , goalView
  , meterValue
  , goalProgress
  , goalMet
  , goalTarget
  , GameConfig(..)
  , defaultConfig
  ) where

import Match3.Color
import Match3.Goal
import Match3.Types.Board
import Match3.Types.Body
import Match3.Types.Cell
import Match3.Types.Game
import Match3.Types.Overlay
