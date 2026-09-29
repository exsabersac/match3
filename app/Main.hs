{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SDL2 前端入口：初始化窗口与渲染器、加载贴图、建立初始 App，运行固定步长（≈60 fps）主循环：
-- 同步倍率 → 处理事件（UI.Input）→ 推进动画（UI.Playback）→ 绘制（UI.Draw）。
--
-- 规则一律经 Match3.Core；前端不改写 trySwap 结果，只展示。
-- 连锁按轮回放（高亮 → 消失 → 下落补子 → 下一轮），时间线与阶段机见 ComboFx。
-- 模块分工见 docs/architecture.md 的「前端模块」。
module Main
  ( main
  ) where

import Control.Concurrent (threadDelay)
import Art
import Control.Monad (unless)
import Data.IORef
import Data.Maybe (isJust)
import Match3.Core
import SDL hiding (Normal)
import System.Environment (lookupEnv)
import UI.Actions
import UI.Draw
import UI.Env
import UI.Input
import UI.Layout
import UI.Playback
import UI.Types

main :: IO ()
main = do
  initializeAll
  -- 线性过滤：2x 贴图缩到 56px 格子时更平滑（须在创建纹理之前设置）
  HintRenderScaleQuality $= ScaleLinear
  seed <- envSeed
  startIdx <- envStartLevel
  showcase <- isJust <$> lookupEnv "MATCH3_SHOWCASE"
  winScale <- envWindowScale
  let lvl = allLevels !! startIdx
      gs0 = newGameAtLevel startIdx (levelConfig lvl) seed
  -- 高分屏：windowHighDPI 让 macOS Retina 给出 2x 物理像素的绘制表面（窗口坐标仍是逻辑点）。
  -- MATCH3_SCALE=N（测试用）把窗口本身放大 N 倍，在 Xvfb 等没有 HiDPI 的环境里模拟 Retina。
  window <-
    createWindow
      "Match-3"
      defaultWindow
        { windowInitialSize = V2 (winW * winScale) (winH * winScale)
        , windowHighDPI = True
        }
  renderer <- createRenderer window (-1) defaultRenderer
  -- 开启 alpha 混合：面板、遮罩、粒子的半透明才生效
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  art <- loadArt renderer
  let (gsHinted0, _) = applyHint gs0
      gsHinted = if showcase then showcaseState gsHinted0 else gsHinted0
  ref <-
    newIORef
      App
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
  updateTitle window =<< readIORef ref
  let loop = do
        t0 <- ticks
        -- 每帧同步倍率（两次查询很便宜）：窗口拖到不同 DPI 的显示器上也能立刻跟上
        syncScale window renderer ref
        events <- pollEvents
        shouldQuit <- foldEvents ref window events
        modifyIORef' ref tickAnim
        app <- readIORef ref
        draw renderer app
        present renderer
        -- 固定步长 ≈ 60 fps：扣掉本帧已花的时间，保证动画时间线（按帧计）接近真实毫秒
        t1 <- ticks
        let spent = fromIntegral (t1 - t0) :: Int
        threadDelay (max 1000 ((16 - spent) * 1000))
        unless shouldQuit loop
  loop
  destroyRenderer renderer
  destroyWindow window
  quit
