{-# LANGUAGE OverloadedStrings #-}

-- | 三消插件（第三刀）：把三消的界面接到通用外壳 Shell.Loop 上。
--
-- 世界状态是 IORef App；各钩子：
--   * 初始化：加载贴图、建立初始 App（开局提示 / 展示模式）、写窗口标题；
--   * 帧首：同步渲染倍率（UI.Env.syncScale）；
--   * 事件：输入映射 UI.Input.foldEvents（规则经 Match3.Engine.play）；
--   * 推进：UI.Playback.tickAnim（逐轮回放用通用播放器 Engine.Playback）；
--   * 绘制：UI.Draw.draw。
module UI.Plugin
  ( Match3Opts(..)
  , match3Plugin
  , match3ShellConfig
  ) where

import Art
import Data.IORef
import Foreign.C.Types (CInt)
import Match3.Core
import SDL (V2 (..))
import Shell.Loop
import UI.Actions
import UI.Draw
import UI.Env
import UI.Input
import UI.Layout
import UI.Playback
import UI.Types

-- | 启动参数（来自环境变量，见 UI.Env）。
data Match3Opts = Match3Opts
  { moSeed      :: Int
  , moStart     :: Int   -- ^ 起始关卡下标
  , moShowcase  :: Bool  -- ^ 元素展示盘（截图用）
  , moWinScale  :: CInt  -- ^ 窗口放大倍数（MATCH3_SCALE）
  }

-- | 外壳配置：窗口按逻辑尺寸 × 放大倍数，固定 16 ms 步长。
match3ShellConfig :: Match3Opts -> ShellConfig
match3ShellConfig o =
  ShellConfig
    { scTitle = "Match-3"
    , scSize = V2 (winW * moWinScale o) (winH * moWinScale o)
    , scFrameMs = 16
    }

-- | 三消插件。
match3Plugin :: Match3Opts -> Plugin (IORef App)
match3Plugin o =
  Plugin
    { plugInit = \window renderer -> do
        art <- loadArt renderer
        ref <- newIORef (initialApp o art)
        updateTitle window =<< readIORef ref
        pure ref
    , plugBegin = \window renderer ref ->
        -- 每帧同步倍率（两次查询很便宜）：窗口拖到不同 DPI 的显示器上也能立刻跟上
        syncScale window renderer ref
    , plugEvents = \window events ref -> foldEvents ref window events
    , plugTick = \ref -> modifyIORef' ref tickAnim
    , plugDraw = \renderer ref -> draw renderer =<< readIORef ref
    }

-- | 开局界面状态：第 1 关自动提示；展示模式换成元素展示盘。
initialApp :: Match3Opts -> Maybe Art -> App
initialApp o art =
  let startIdx = moStart o
      showcase = moShowcase o
      lvl = allLevels !! startIdx
      gs0 = newGameAtLevel startIdx (levelConfig lvl) (moSeed o)
      (gsHinted0, _) = applyHint gs0
      gsHinted = if showcase then showcaseState gsHinted0 else gsHinted0
  in App
       { appGame = gsHinted
       , appSel = Nothing
       , appMsg = helpKeysMsg
       , appFlash = []
       , appPulse = 0
       , appAnim = AnimNone
       , appComboShow = 0
       , appComboBest = 0
       , appPops = []
       , appShake = 0
       , appShakeAmp = 0
       , appParticles = []
       , appTipFrames = if startIdx == 0 && not showcase then 240 else 0
       , appHelpFrames = if showcase then 0 else 300
       , appPaused = False
       , appStartMoves = lvlMoves lvl
       , appDragFrom = Nothing
       , appTool = ToolNone
       , appMapOpen = False
       , appMaxReached = startIdx
       , appArt = art
       , appScale = 0
       , appMouseScale = 1
       }
