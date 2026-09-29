-- | SDL2 前端入口：读环境变量，把三消插件（UI.Plugin）交给通用外壳（Shell.Loop）运行。
--
-- 外壳负责窗口 / 渲染器 / 固定步长（≈60 fps）主循环：帧首同步倍率 → 处理事件（UI.Input）→
-- 推进动画（UI.Playback）→ 绘制（UI.Draw）。
-- 规则一律经 Match3.Engine（三消的通用接口实例）；前端不改写结算结果，只展示。
-- 连锁按轮回放（高亮 → 消失 → 下落补子 → 下一轮），阶段机见 ComboFx，时钟见 Engine.Playback。
-- 模块分工见 docs/architecture.md 的「前端模块」与「多游戏接口」。
module Main
  ( main
  ) where

import Data.Maybe (isJust)
import Shell.Loop (runShell)
import System.Environment (lookupEnv)
import UI.Env
import UI.Plugin

main :: IO ()
main = do
  seed <- envSeed
  startIdx <- envStartLevel
  showcase <- isJust <$> lookupEnv "MATCH3_SHOWCASE"
  -- MATCH3_SCALE=N（测试用）把窗口本身放大 N 倍，在 Xvfb 等没有 HiDPI 的环境里模拟 Retina。
  winScale <- envWindowScale
  let opts = Match3Opts seed startIdx showcase winScale
  runShell (match3ShellConfig opts) (match3Plugin opts)
