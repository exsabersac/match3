{-# LANGUAGE OverloadedStrings #-}

-- | 通用 SDL 外壳（第三刀）：与具体游戏无关的主循环骨架。不 import 任何 Match3 模块
-- （测试 engine_layer_is_game_agnostic 检查）。
--
-- 外壳负责：SDL 初始化、线性缩放提示、带 HiDPI 的窗口、渲染器（alpha 混合）、
-- 固定步长（默认 16 ms ≈ 60 fps）主循环与退出清理。每帧的顺序固定为：
--   帧首钩子（同步倍率等）→ 取事件 → 插件处理事件（返回是否退出）→ 插件推进一帧 → 插件绘制 → present → 补足帧时长。
-- 具体游戏以 Plugin 接入：世界状态 w 由插件在窗口 / 渲染器建好之后创建（贴图要在此之后加载），
-- 之后外壳只把 w 原样交回插件的各个钩子。播放器的帧节拍与加速见 Engine.Playback（纯，不在此处）。
module Shell.Loop
  ( ShellConfig(..)
  , Plugin(..)
  , runShell
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Text (Text)
import Foreign.C.Types (CInt)
import SDL

-- | 窗口与帧节拍配置。
data ShellConfig = ShellConfig
  { scTitle   :: Text     -- ^ 初始窗口标题
  , scSize    :: V2 CInt  -- ^ 窗口初始大小（逻辑点；HiDPI 下绘制表面由系统给 2x）
  , scFrameMs :: Int      -- ^ 固定步长（毫秒）
  }

-- | 游戏插件：外壳在固定的时刻调用这些钩子。
data Plugin w = Plugin
  { plugInit   :: Window -> Renderer -> IO w          -- ^ 窗口与渲染器就绪后建立世界状态（加载贴图、初始标题等）
  , plugBegin  :: Window -> Renderer -> w -> IO ()    -- ^ 每帧开始（取事件之前），如同步渲染倍率
  , plugEvents :: Window -> [Event] -> w -> IO Bool   -- ^ 处理本帧全部事件；True = 退出
  , plugTick   :: w -> IO ()                          -- ^ 推进一帧（动画 / 计时）
  , plugDraw   :: Renderer -> w -> IO ()              -- ^ 绘制本帧（外壳随后 present）
  }

-- | 运行外壳直到插件要求退出。
runShell :: ShellConfig -> Plugin w -> IO ()
runShell cfg plug = do
  initializeAll
  -- 线性过滤：高倍贴图缩小时更平滑（须在创建纹理之前设置）
  HintRenderScaleQuality $= ScaleLinear
  -- windowHighDPI 让 macOS Retina 给出 2x 物理像素的绘制表面（窗口坐标仍是逻辑点）
  window <-
    createWindow
      (scTitle cfg)
      defaultWindow
        { windowInitialSize = scSize cfg
        , windowHighDPI = True
        }
  renderer <- createRenderer window (-1) defaultRenderer
  -- 开启 alpha 混合：半透明面板 / 遮罩 / 粒子才生效
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  w <- plugInit plug window renderer
  let loop = do
        t0 <- ticks
        plugBegin plug window renderer w
        events <- pollEvents
        shouldQuit <- plugEvents plug window events w
        plugTick plug w
        plugDraw plug renderer w
        present renderer
        -- 固定步长：扣掉本帧已花的时间，保证按帧计的动画时间线接近真实毫秒
        t1 <- ticks
        let spent = fromIntegral (t1 - t0) :: Int
        threadDelay (max 1000 ((scFrameMs cfg - spent) * 1000))
        unless shouldQuit loop
  loop
  destroyRenderer renderer
  destroyWindow window
  quit
