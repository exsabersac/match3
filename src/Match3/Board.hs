-- | 棋盘纯规则的外观模块：按原有导出列表重新导出 Match3.Board.* 子模块，
-- 让 Match3.Core、Match3.Game 和测试的 import 保持不变。本模块自身不含实现。
--
-- 子模块（依赖自下而上，单向）：
--
-- * "Match3.Board.Grid"    —— 坐标、读写格、交换、MBoard、随机颜色
-- * "Match3.Board.Match"   —— 匹配检测、findHint / hasValidMove
-- * "Match3.Board.Clear"   —— 一轮消除、特殊扩展 / 生成、邻格削层、飞碟吸收、计分
-- * "Match3.Board.Gravity" —— 重力、底行饼干、传送门沉降、补子
-- * "Match3.Board.Cascade" —— 连锁的单一实现（记录版 cascade* 同时产出计数与逐轮回放；runCascade* / traceCascade* 为兼容投影）
-- * "Match3.Board.Random"  —— 随机 / 稳定 / 可玩盘面与洗牌
--
-- 元素行为（哪些格参与匹配、挡交换、命中反应、邻格波及、重力、计数……）查 "Match3.Element" 的注册表；
-- 带 With 的函数接受任意注册表，旧名 = 内置注册表 defaultRegistry。
--
-- 不拥有：步数与目标结算、道具扣次、战役装饰（见 Match3.Game）。
module Match3.Board
  ( getCell
  , setCell
  , swapCells
  , inBounds
  , adjacent
  , findMatches
  , findMatchRuns
  , hasAnyMatch
  , clearMatches
  , clearMatchesAt
  , applyGravity
  , refill
  , stepCascade
  , stepCascadeAt
  , runCascade
  , runCascadeAt
  , runCascadeScored
  , runCascadeScoredWithUfos
  , runCascadeScoredFromSeeds
  , runCascadeScoredFromSeedsWithUfos
  , runPostBeltCascade
  , randomBoard
  , randomStableBoard
  , randomPlayableBoard
  , shufflePlayable
  , hasValidMove
  , scoreForCleared
  , scoreForWave
  , findHint
  , MatchRun(..)
  , countColor
  , resolveCountdowns
  , applyPortalTeleports
  , settleBoardPortals
  , expandSpecials
    -- * 连锁（记录版，单一实现）
  , CascadeTally(..)
  , zeroTally
  , CascadeRun(..)
  , cascadeMatches
  , cascadeMatchesFrom
  , cascadeSeeds
  , cascadeAfterBelt
  , cascadeCountdowns
  , stillRun
    -- * 指定注册表的版本（元素框架；不带 With 的旧名 = 内置注册表）
  , findMatchRunsWith
  , hasAnyMatchWith
  , hasValidMoveWith
  , findHintWith
  , expandSpecialsWith
  , countColorWith
  , clearMatchesDetailedWith
  , clearFromSeedsDetailedWith
  , gravityFixedCell
  , gravityFixedCellWith
  , settleBoardPortalsWith
  , cascadeMatchesWith
  , cascadeMatchesFromWith
  , cascadeSeedsWith
  , cascadeAfterBeltWith
  , cascadeCountdownsWith
    -- * 逐轮回放（兼容投影）
  , CascadeWave(..)
  , traceCascade
  , traceCascadeFromWave
  , traceCascadeFromSeeds
  , tracePostBeltCascade
  , traceCountdowns
  ) where

import Match3.Board.Cascade
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid
import Match3.Board.Match
import Match3.Board.Random
