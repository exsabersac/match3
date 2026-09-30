{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HUD 与叠层的几何降级绘制：顶部信息栏、键位条、首关提示、暂停帮助、弹字、结算面板、进度条。
--
-- 依赖：Match3.View（gameView）、UI.HudBlocks（HUD 各区块与进度条 drawMeter，本模块再导出 drawMeter）、UI.Glyph、UI.Types、UI.Layout、ComboFx（弹字曲线）。
-- 不变量：结算面板在回放播完（animBusy 为假）后才画，不挡住最后几轮。
module UI.HudPrim
  ( drawKeyChip
  , drawHelpStrip
  , drawTipBanner
  , drawPauseHelp
  , drawHud
  , drawPopsPrim
  , drawMeter -- 再导出自 UI.HudBlocks
  , drawOverlay
  , drawOverlayNow
  ) where

import ComboFx
import Control.Monad (forM_)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import Match3.View (GameView (..), gameView)
import UI.Glyph
import UI.HudBlocks
import UI.Layout
import UI.Presentation (scorePopRGB)
import UI.Types

--------------------------------------------------------------------------------
-- Help / tip / pause overlays (bitmap, no TTF)
--------------------------------------------------------------------------------

drawKeyChip :: Renderer -> CInt -> CInt -> Char -> V4 Word8 -> IO ()
drawKeyChip ren x y ch col = do
  rendererDrawColor ren $= V4 30 30 45 255
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 18 18)))
  rendererDrawColor ren $= col
  drawRect ren (Just (Rectangle (P (V2 x y)) (V2 18 18)))
  drawGlyph ren (x + 4) (y + 3) 2 col ch

-- | Brief key strip along bottom of HUD (start / after unpause).
drawHelpStrip :: Renderer -> App -> IO ()
drawHelpStrip ren app
  | appPaused app = pure ()
  | appHelpFrames app <= 0 = pure ()
  | otherwise = do
      let y = hudH - 22
          keys =
            [ ('H', V4 255 220 100 255)
            , ('1', V4 255 160 100 255)
            , ('2', V4 160 220 255 255)
            , ('3', V4 240 140 240 255)
            , ('U', V4 180 200 255 255)
            , ('S', V4 200 160 255 255)
            , ('D', V4 140 220 200 255)
            , ('M', V4 180 200 255 255)
            , ('R', V4 255 160 140 255)
            , ('N', V4 140 220 160 255)
            , ('P', V4 255 200 120 255)
            ]
      rendererDrawColor ren $= V4 20 20 32 220
      fillRect ren (Just (Rectangle (P (V2 0 y)) (V2 winW 22)))
      forM_ (zip [0 :: CInt ..] keys) $ \(i, (ch, col)) ->
        drawKeyChip ren (8 + i * 22) (y + 2) ch col

-- | First-level tip banner over the board top edge.
drawTipBanner :: Renderer -> App -> IO ()
drawTipBanner ren app
  | appPaused app = pure ()
  | appTipFrames app <= 0 = pure ()
  | gsLevel (appGame app) /= 0 = pure ()
  | otherwise = do
      let y = hudH + 6
          pulse = appPulse app
          bright = fromIntegral (200 + (pulse `mod` 30)) :: Word8
      rendererDrawColor ren $= V4 40 35 15 230
      fillRect ren (Just (Rectangle (P (V2 24 y)) (V2 (winW - 48) 28)))
      rendererDrawColor ren $= V4 255 bright 80 255
      drawRect ren (Just (Rectangle (P (V2 24 y)) (V2 (winW - 48) 28)))
      drawBannerWord ren 40 (y + 6) 2 (V4 255 230 120 255) "TIP"
      drawKeyChip ren 120 (y + 5) 'H' (V4 255 220 100 255)
      drawBannerWord ren 150 (y + 8) 2 (V4 220 220 200 255) "HINT"

-- | Full pause overlay with key legend.
drawPauseHelp :: Renderer -> App -> IO ()
drawPauseHelp ren app
  | not (appPaused app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 8 8 16 200
      fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW winH)))
      let panelY = hudH + 40
          panelH = 430 :: CInt
      rendererDrawColor ren $= V4 32 32 48 245
      fillRect ren (Just (Rectangle (P (V2 32 panelY)) (V2 (winW - 64) panelH)))
      rendererDrawColor ren $= V4 255 200 80 255
      drawRect ren (Just (Rectangle (P (V2 32 panelY)) (V2 (winW - 64) panelH)))
      drawBannerWord ren 100 (panelY + 16) 4 (V4 255 220 100 255) "PAUSE"
      -- Mechanism reminder strip (carpet / steam)
      drawBannerWord ren 60 (panelY + 52) 2 (V4 200 140 180 255) "CARPET"
      drawBannerWord ren 220 (panelY + 52) 2 (V4 180 190 200 255) "STEAM"
      let rows :: [(Int, Char, String)]
          rows =
            [ (0, 'H', "HINT")
            , (1, '1', "HAMMER")
            , (2, '2', "SWAP")
            , (3, '3', "CROSS")
            , (4, 'U', "UNDO")
            , (5, 'S', "SHUFFLE")
            , (6, 'D', "DAILY")
            , (7, 'M', "MAP")
            , (8, 'R', "RETRY")
            , (9, 'N', "NEXT")
            , (10, 'P', "PLAY")
            ]
      forM_ rows $ \(i, ch, label) -> do
        let yy = panelY + 72 + fromIntegral i * 28
        drawKeyChip ren 80 yy ch (V4 255 220 120 255)
        drawBannerWord ren 120 yy 2 (V4 210 210 230 255) label

-- | 几何降级版 HUD：按区块依次绘制（各区块在 UI.HudBlocks；顺序即层次）。
drawHud :: Renderer -> App -> IO ()
drawHud ren app = do
  let gv = gameView (appGame app)  -- 第 11 刀：各区块读视图模型
  hudFrame ren
  hudLevel ren gv
  hudGoal ren (gvGoal gv)
  hudGoalSwatch ren (gvGoal gv)
  hudMoves ren gv
  hudBoosters ren app (gvBoosters gv)
  hudComboBadge ren app gv
  hudStatus ren (gvStatus gv)


-- | 退回画法（无贴图）的浮字：「COMBO N」+ 本轮得分「+N」，同样放大弹出再淡出。
drawPopsPrim :: Renderer -> App -> IO ()
drawPopsPrim ren app
  | appMapOpen app || appPaused app = pure ()
  | otherwise =
      forM_ (reverse (appPops app)) $ \p -> do
        let a = popAlpha p
            cx = round (tpX p) :: CInt
            cy = round (tpY p - popRise p) :: CInt
        case tpKind p of
          PopCombo k -> do
            let st = comboStyle k
                (r, g, b) = styleRGB st (appPulse app)
                px = max 2 (round (fromIntegral (csHeight st) * comboPopScale (tpAge p) / 7 :: Double)) :: CInt
                w = 5 * 5 * px + 2 * px + 4 * px * fromIntegral (length (show k))
                x0 = popLeft w cx
            rendererDrawColor ren $= V4 20 10 5 (a `div` 2 + a `div` 4)
            fillRect ren (Just (Rectangle (P (V2 (x0 - 8) (cy - 3 * px - 6))) (V2 (w + 16) (5 * px + 12))))
            drawBannerWord ren x0 (cy - 3 * px) px (V4 r g b a) "COMBO"
            drawNumber ren (x0 + 5 * 5 * px + 2 * px) (cy - 3 * px) px (V4 r g b a) k
          PopScore n k -> do
            let (r, g, b) = scorePopRGB k (appPulse app) -- 表现表 EvScore 行（连击轮用等级色）
                px = 3 :: CInt
                w = 4 * px * fromIntegral (length (show n) + 1)
                x0 = popLeft w cx
                col = V4 r g b a
            rendererDrawColor ren $= col
            -- 「+」
            fillRect ren (Just (Rectangle (P (V2 x0 (cy + px))) (V2 (3 * px) px)))
            fillRect ren (Just (Rectangle (P (V2 (x0 + px) cy)) (V2 px (3 * px))))
            drawNumber ren (x0 + 4 * px) (cy - px) px col n


--------------------------------------------------------------------------------
-- Fullscreen outcome overlay
--------------------------------------------------------------------------------

drawOverlay :: Renderer -> App -> IO ()
drawOverlay ren app
  -- 结算面板等连锁回放播完再出，别挡住最后几轮
  | animBusy app = pure ()
  | otherwise = drawOverlayNow ren app

drawOverlayNow :: Renderer -> App -> IO ()
drawOverlayNow ren app = case gsOver (appGame app) of
  Nothing -> pure ()
  Just outcome -> do
    -- Dim board
    rendererDrawColor ren $= V4 10 10 18 180
    fillRect ren (Just (Rectangle (P (V2 0 hudH)) (V2 winW (winH - hudH))))
    -- Banner panel
    let panelH = 120 :: CInt
        panelY = hudH + (boardPx - panelH) `div` 2
    case outcome of
      LevelClear _ nextIdx -> do
        rendererDrawColor ren $= V4 40 50 20 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 255 220 80 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawRect ren (Just (Rectangle (P (V2 26 (panelY + 2))) (V2 (winW - 52) (panelH - 4))))
        drawBannerWord ren 80 (panelY + 18) 5 (V4 255 230 100 255) "CLEAR!"
        let stars = starRating (appStartMoves app) (gsMoves (appGame app))
        drawNumber ren 200 (panelY + 22) 3 (V4 255 220 80 255) stars
        -- Clearer next-level prompt: NEXT L# + N key chip (click / N / Space)
        drawBannerWord ren 60 (panelY + 70) 3 (V4 200 220 180 255) "NEXT"
        drawNumber ren 180 (panelY + 68) 3 (V4 200 220 180 255) (nextIdx + 1)
        drawKeyChip ren (winW - 100) (panelY + 70) 'N' (V4 140 220 160 255)
      Won s -> do
        rendererDrawColor ren $= V4 20 50 30 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 80 220 120 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawBannerWord ren 110 (panelY + 18) 5 (V4 120 255 160 255) "WIN!"
        let stars = starRating (appStartMoves app) (gsMoves (appGame app))
        drawNumber ren 200 (panelY + 22) 3 (V4 255 220 80 255) stars
        drawNumber ren 160 (panelY + 70) 3 (V4 200 255 210 255) s
      Lost s -> do
        rendererDrawColor ren $= V4 50 20 20 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 220 80 80 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawBannerWord ren 100 (panelY + 18) 5 (V4 255 120 120 255) "LOSE"
        drawBannerWord ren 90 (panelY + 70) 3 (V4 255 180 180 255) "RETRY"
        drawNumber ren (winW - 140) (panelY + 68) 3 (V4 255 180 180 255) s
      _ -> pure ()
