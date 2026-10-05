-- | 前端 API：网页版（web/hs 与它编译的 app/pure）用到的类型、构造函数、查询和对局操作。
-- 本模块只再导出，不定义任何东西。
--
-- 规则内部（障碍邻消、蔓延、连锁各阶段、道具清除范围等）不从这里导出：库内模块和测试直接 import
-- 所在的子模块（Match3.Types、Match3.Board.*、Match3.Game.*、Match3.Obstacles 等）。
-- 前端要用新名字时在这里加上；这里的每个名字都必须有前端在用（Spec.SourceScan 的 core_exports_used_by_frontends）。
module Match3.Core
  ( -- * 格子
    Color(..)
  , allColors
  , GemKind(..)
  , CellContents(..)
  , Cell
  , CellOverlay(..)
  , ElementName(..)
  , CustomState(..)
    -- ** 构造（图例、几何版示意格）
  , mkGem
  , mkIceGem
  , mkGrassGem
  , mkVineGem
  , mkChocoGem
  , mkFogGem
  , mkChainGem
  , mkFreezeGem
  , mkCurtainGem
  , mkSteamGem
  , mkSnail
  , mkSafeLayers
  , mkFlip
  , mkSurprise
  , mkBottle
  , mkTimeSpirit
  , mkStoneLayers
  , mkChestLayers
  , mkHoneyLayers
  , mkBalloon
  , mkCookie
  , mkCakeLayers
  , mkMagicHat
  , mkMakerCharges
  , mkCountdown
    -- * 盘面与坐标
  , Pos
  , Board
  , boardFromRows
  , boardRows
  , boardRowIndices
  , boardColIndices
  , getCell
  , gravityFixedCell
    -- ** 可空盘面（连锁波次里带空洞的盘面）
  , atM
  , mboardRows
    -- * 关卡与目标
  , GameConfig(..)
  , Level(..)
  , clampLevelIndex
  , LevelGoal(..)
  , CounterKey(..)
  , GoalView(..)
  , goalView
  , Outcome(..)
  , Terminal(..)
  , fromTerminal
    -- * 对局
  , GameState(..)
  , MoveFx(..)
  , Ufo(..)
  , setUfos
  , setBelts
  , setPortals
  , setCarpetOpen
  , campaignGame
  , newDailyGame
  , restartLevel
  , nextLevel
  , unlockAfterOutcome
  , mapClickJump
    -- * 一步的轨迹（回放用）
  , MoveTrace(..)
  , emptyTrace
  , CascadeWave(..)
  , EndStep(..)
  , EndEffect(..)
  , EndItem(..)
  , endEffectPairs
  , endItemDir
    -- * 每日挑战
  , Year(..)
  , Month(..)
  , Day(..)
  , starRating
  ) where

import Match3.Board.Cascade (CascadeWave(..))
import Match3.Board.Default (gravityFixedCell)
import Match3.Board.Grid (atM, getCell, mboardRows)
import Match3.Counts (CounterKey(..))
import Match3.Daily (Day(..), Month(..), Year(..), starRating)
import Match3.Game.Level (campaignGame, newDailyGame, nextLevel, restartLevel)
import Match3.Game.Outcome (mapClickJump, unlockAfterOutcome)
import Match3.Game.State (GameState(..), MoveFx(..), setBelts, setCarpetOpen, setPortals, setUfos)
import Match3.Game.Trace (EndEffect(..), EndItem(..), EndStep(..), MoveTrace(..), emptyTrace, endEffectPairs, endItemDir)
import Match3.Levels.Campaign (clampLevelIndex)
import Match3.Levels.Level (Level(..))
import Match3.Types
  ( Board
  , Cell
  , CellContents(..)
  , CellOverlay(..)
  , Color(..)
  , CustomState(..)
  , ElementName(..)
  , GameConfig(..)
  , GemKind(..)
  , GoalView(..)
  , LevelGoal(..)
  , Outcome(..)
  , Pos
  , Terminal(..)
  , allColors
  , boardColIndices
  , boardFromRows
  , boardRowIndices
  , boardRows
  , fromTerminal
  , goalView
  , mkBalloon
  , mkBottle
  , mkCakeLayers
  , mkChainGem
  , mkChestLayers
  , mkChocoGem
  , mkCookie
  , mkCountdown
  , mkCurtainGem
  , mkFlip
  , mkFogGem
  , mkFreezeGem
  , mkGem
  , mkGrassGem
  , mkHoneyLayers
  , mkIceGem
  , mkMagicHat
  , mkMakerCharges
  , mkSafeLayers
  , mkSnail
  , mkSteamGem
  , mkStoneLayers
  , mkSurprise
  , mkTimeSpirit
  , mkVineGem
  )
import Match3.Ufo (Ufo(..))
