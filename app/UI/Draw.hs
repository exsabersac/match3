{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 一帧的总绘制，层次自下而上：背景 → HUD → 棋盘 / 粒子 / 浮字（同一震屏视口）→ 提示 / 道具横幅 →
-- 键位条 → 结算面板 → 暂停帮助 → 选关地图，
-- 按是否加载了贴图在 *Art 与几何降级版本之间分派。
--
-- 依赖：UI.Cascade、UI.BoardArt、UI.HudArt、UI.HudPrim、UI.LevelMap、UI.Types。
module UI.Draw
  ( draw
  ) where

import Art
import Control.Monad (forM_)
import SDL hiding (Normal)
import UI.BoardArt
import UI.Cascade
import UI.HudArt
import UI.HudPrim
import UI.LevelMap
import UI.Types

-- | 画一帧（见模块说明的层次顺序）。
draw :: Renderer -> App -> IO ()
draw ren app = do
  rendererDrawColor ren $= V4 28 28 38 255
  clear ren
  case appArt app of
    -- 贴图模式：背景图 + 圆角面板 HUD + 精灵棋子
    Just art -> do
      forM_ (artBg art) $ \bg -> copy ren bg Nothing Nothing
      drawHudArt ren art app
      withShake ren app $ do
        drawBoard ren app
        drawParticlesAny ren app
        drawPopsArt ren art app
      drawTipBannerArt ren art app
      drawToolBannerArt ren art app
      drawHelpStripArt ren art app
      drawOverlayArt ren art app
      drawPauseHelpArt ren art app
      drawLevelMapArt ren art app
    -- 回退：无资源时沿用原有矩形 / 位图字绘制
    Nothing -> do
      drawHud ren app
      withShake ren app $ do
        drawBoard ren app
        drawParticlesAny ren app
        drawPopsPrim ren app
      drawTipBanner ren app
      drawHelpStrip ren app
      drawOverlay ren app
      drawPauseHelp ren app
      drawLevelMap ren app
