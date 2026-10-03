{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 界面动作（IO）：刷新窗口标题、关卡重置、走步（交换 / 三种道具）的执行、回放加速、过关后前进 / 重试、解锁记录。
--
-- 依赖：UI.Types、UI.Playback、UI.MoveText（走步文案）、Match3.Engine（动作执行）、Match3.View（标题 titleLine）、Match3.Core、SDL（窗口标题）。
-- 规则结果一律来自通用接口 gameStep（M3E.match3Shell；每个动作只结算一次，整步报告里有状态、Outcome、MoveFx、回放脚本、效果事件），
-- 这里只更新 App。
module UI.Actions
  ( updateTitle
  , stepShell
  , playMove
  , playbackOf
  , colorTag
  , freshLevelUi
  , Booster(..)
  , applyBooster
  , speedUp
  , withUnlock
  , advanceOrMsg
  ) where

import Data.IORef
import Data.Maybe (fromMaybe)
import Engine.Game (Game (..), Step (..))
import Engine.History (History, Undoable (..), startHistory)
import qualified Match3.Element.Event as Ev
import Engine.Playback (Player (..))
import qualified Data.Text as T
import Match3.Core
import qualified Match3.Engine as M3E
import SDL hiding (Normal)
import System.Random (randomIO)
import Match3.View (colorTag, gameView, titleLine)
import UI.Audio (beginLevel)
import UI.MoveText (MoveUi (..), keepsTool, moveMsg)
import UI.Playback
import UI.Types

-- | 把关卡、步数、分数、道具次数、状态与最近提示写到窗口标题（调试 / 无贴图时也能看到）。
updateTitle :: Window -> App -> IO ()
updateTitle window app = do
  -- 标题读视图模型（Match3.View.titleLine），再接本次提示
  let title = T.pack (titleLine (gameView (appGame app)) ++ "  |  " ++ T.unpack (appMsg app))
  windowTitle window $= title

-- | 外壳执行一个动作：只调通用接口 M3E.match3Shell 的 gameStep（撤销历史由 Engine.History 维护）。
stepShell :: Undoable M3E.Action -> App -> Step (History GameState) Ev.Event Outcome M3E.Played
stepShell act app = gameStep M3E.match3Shell (appHist app) act

-- | 执行一个走步动作（交换 / 道具）：整步报告（stepReport）、Outcome 与新的历史。
playMove :: M3E.Action -> App -> (M3E.Played, Outcome, History GameState)
playMove act app =
  let st = stepShell (Act act) app
      pd = fromMaybe (M3E.rejectedPlayed (appGame app)) (stepReport st)
  in (pd, fromMaybe InvalidSwap (M3E.pdOutcome pd), stepState st)

-- | 走步之后的表现编排：MoveFx、回放脚本与效果事件都取自同一次 gameStep 的报告。
playbackOf :: GameState -> M3E.Played -> Maybe (Pos, Pos) -> App -> App
playbackOf before pd =
  withMovePlayback before (M3E.pdState pd) (M3E.pdFx pd) (M3E.pdTrace pd) (M3E.pdEvents pd)

-- | Reset tip/help for a (re)started level; auto-hint on level 1 (index 0).
freshLevelUi :: GameState -> App -> App
freshLevelUi gs app =
  let (gs', _) =
        if gsLevel gs == 0
          then applyHint gs
          else (gs { gsHint = Nothing }, Nothing)
      tip = if gsLevel gs' == 0 then 240 else 0
  in app
       { appHist = startHistory gs'
       , appSel = Nothing
       , appMsg = helpKeysMsg
       , appFlash = []
       , appAnim = AnimNone
       , appComboShow = 0
       , appComboBest = 0
       , appPops = []
       , appShake = 0
       , appParticles = []
       , appTipFrames = tip
       , appStartMoves = gsMoves gs
       , appHelpFrames = 300
       , appPaused = False
       , appTool = ToolNone
       , appDragFrom = Nothing
       , appMapOpen = False
       , appMaxReached = max (appMaxReached app) (gsLevel gs')
       }


-- | 三种道具：锤子 / 十字打一格，自由交换换任意两格。
data Booster
  = BoostHammer Pos
  | BoostCross Pos
  | BoostFreeSwap Pos Pos

-- | 执行一次道具：经通用接口结算一次（playMove），文案与之后的点选模式查 UI.MoveText，
-- 编排回放（自由交换带交换补间的两格）、记录解锁，写回 App 并刷新窗口标题。
-- 特效只看本次调用的 MoveFx（边沿触发），道具无效时不重播上一步连击。
applyBooster :: IORef App -> Window -> App -> Booster -> IO ()
applyBooster ref window app b = do
  let (act, ui, pair) = case b of
        BoostHammer p -> (M3E.Hammer p, UiHammer, Nothing)
        BoostCross p -> (M3E.CrossClear p, UiCross, Nothing)
        BoostFreeSwap p1 p2 -> (M3E.FreeSwap p1 p2, UiFreeSwap, Just (p1, p2))
      before = appGame app
      (pd, out, h') = playMove act app
      tool'
        | keepsTool ui out = ToolFreeSwap Nothing
        | otherwise = ToolNone
      app' =
        withUnlock
          ( playbackOf before pd pair
              app
                { appHist = h'
                , appSel = Nothing
                , appTool = tool'
                , appDragFrom = Nothing
                , appMsg = T.pack (moveMsg ui before (M3E.pdState pd) (M3E.pdFx pd) out)
                }
          )
          out
  writeIORef ref app'
  updateTitle window app'

-- | 回放中按下鼠标 / 空格：加速并提示。
speedUp :: IORef App -> Window -> IO ()
speedUp ref window = do
  app <- readIORef ref
  case playingPlayer app of
    Just p | not (plFast p) -> do
      let app' = app {appAnim = accelerate (appAnim app), appMsg = "Fast-forward combo"}
      writeIORef ref app'
      updateTitle window app'
    _ -> pure ()

-- | Bump map unlock after LevelClear / Won (daily Won must not unlock campaign).
withUnlock :: App -> Outcome -> App
withUnlock app out =
  app { appMaxReached = unlockAfterOutcome (appGame app) (appMaxReached app) out }

-- | N / Enter / Space / click-on-clear: next level or new campaign.
advanceOrMsg :: IORef App -> Window -> IO ()
advanceOrMsg ref window = do
  seed <- randomIO
  app <- readIORef ref
  case gsOver (appGame app) of
    Just (LevelClear _ n) -> do
      beginLevel
      let gs = nextLevel (appGame app) seed
          -- Stars rate vs printed level moves; carry must not inflate the denominator.
          baseMoves = maybe (gsMoves gs) lvlMoves (lookupLevel (gsLevel gs))
          app' =
            (freshLevelUi gs app)
              { appMsg = "Next level!"
              , appMaxReached = max (appMaxReached app) n
              , appStartMoves = baseMoves
              }
      writeIORef ref app'
      updateTitle window app'
    Just (Won _) -> case campaignGame 0 seed of
      -- 通关后从第 1 关重开（关卡表恒非空；空表时不动）
      Just gs -> do
        beginLevel
        let app' = (freshLevelUi gs app) { appMsg = "New campaign" }
        writeIORef ref app'
        updateTitle window app'
      Nothing -> pure ()
    Just (Lost _) -> do
      beginLevel
      let gs0 = appGame app
          gs =
            if gsDaily gs0
              then newDailyGame (GameConfig (appStartMoves app) (gsGoal gs0)) seed
              else restartLevel gs0 seed
          app' = (freshLevelUi gs app) { appMsg = "Retry!" }
      writeIORef ref app'
      updateTitle window app'
    _ -> do
      let app' = app { appMsg = "Clear the level first (or finish)" }
      writeIORef ref app'
      updateTitle window app'
