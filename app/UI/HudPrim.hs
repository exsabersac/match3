{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HUD 与叠层的几何降级绘制：顶部信息栏、键位条、首关提示、暂停帮助、弹字、结算面板、进度条。
--
-- 依赖：UI.Glyph、UI.Types、UI.Layout、ComboFx（弹字曲线）。
-- 不变量：结算面板在回放播完（animBusy 为假）后才画，不挡住最后几轮。
module UI.HudPrim
  ( drawKeyChip
  , drawHelpStrip
  , drawTipBanner
  , drawPauseHelp
  , drawHud
  , drawPopsPrim
  , drawMeter
  , drawOverlay
  , drawOverlayNow
  ) where

import ComboFx
import Control.Monad (forM_)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.Glyph
import UI.Layout
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

-- | 几何降级版 HUD：关卡、目标进度、步数、分数、道具次数与连击徽章。
drawHud :: Renderer -> App -> IO ()
drawHud ren app = do
  let gs = appGame app
      lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
      white = V4 230 230 245 255 :: V4 Word8
      dim = V4 140 140 170 255
      _accent = V4 255 200 80 255 :: V4 Word8
  rendererDrawColor ren $= V4 42 42 58 255
  fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW hudH)))

  -- Level label
  drawNumber ren 10 8 3 white (gsLevel gs + 1)
  forM_ (zip [0 :: Int ..] allLevels) $ \(i, _) -> do
    let col =
          if i == gsLevel gs
            then V4 255 200 80 255
            else if i < gsLevel gs then V4 80 180 120 255 else V4 60 60 80 255
        -- 38 levels fit in HUD: 7px stride
        xDot = 60 + fromIntegral i * 7
    rendererDrawColor ren $= col
    fillRect ren (Just (Rectangle (P (V2 xDot 10)) (V2 7 14)))

  -- Goal meter (score or collect)
  let prog = goalProgress (gsGoal gs) (gsScore gs) (gsCollected gs)
      targ = goalTarget (gsGoal gs)
      meterCol = case gsGoal gs of
        GoalScore _ -> V4 100 220 140 255
        GoalCollectMulti _ -> V4 220 180 100 255
        GoalClearStone _ -> V4 160 160 170 255
        GoalChest _ -> V4 220 170 60 255
        GoalHoney _ -> V4 240 180 40 255
        GoalBalloon _ -> V4 255 120 160 255
        GoalCookie _ -> V4 210 160 90 255
        GoalCake _ -> V4 255 140 180 255
        GoalSafe _ -> V4 200 170 50 255
        GoalUfo _ -> V4 180 120 255 255
        GoalCarpet _ -> V4 180 100 160 255
        GoalCollect col _ ->
          let (r, g, b) = colorRGB col in V4 r g b 255
  drawMeter ren 10 36 prog targ meterCol
  drawNumber ren 10 40 2 white prog
  rendererDrawColor ren $= dim
  fillRect ren (Just (Rectangle (P (V2 78 48)) (V2 8 2)))
  drawNumber ren 90 40 2 dim targ

  -- Collect color swatch
  case gsGoal gs of
    GoalCollectMulti _ -> pure ()
    GoalClearStone _ -> pure ()
    GoalChest _ -> do
      rendererDrawColor ren $= V4 220 170 60 255
      fillRect ren (Just (Rectangle (P (V2 200 42)) (V2 20 16)))
      rendererDrawColor ren $= V4 180 120 40 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 12 6)))
    GoalHoney _ -> do
      rendererDrawColor ren $= V4 240 180 40 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 18)))
      rendererDrawColor ren $= V4 200 140 20 255
      fillRect ren (Just (Rectangle (P (V2 206 36)) (V2 8 6)))
    GoalBalloon _ -> do
      rendererDrawColor ren $= V4 255 120 160 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 14 16)))
      rendererDrawColor ren $= V4 200 80 120 255
      fillRect ren (Just (Rectangle (P (V2 209 54)) (V2 4 6)))
    GoalCookie _ -> do
      rendererDrawColor ren $= V4 210 160 90 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 16)))
      rendererDrawColor ren $= V4 90 50 30 255
      fillRect ren (Just (Rectangle (P (V2 206 44)) (V2 3 3)))
      fillRect ren (Just (Rectangle (P (V2 212 48)) (V2 3 3)))
    GoalCake _ -> do
      -- Pink frosted cake swatch (distinct from tan cookie)
      rendererDrawColor ren $= V4 255 140 180 255
      fillRect ren (Just (Rectangle (P (V2 202 44)) (V2 16 14)))
      rendererDrawColor ren $= V4 255 220 230 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 12 8)))
      rendererDrawColor ren $= V4 255 80 120 255
      fillRect ren (Just (Rectangle (P (V2 208 36)) (V2 4 4)))
    GoalSafe _ -> do
      -- Brass vault door swatch
      rendererDrawColor ren $= V4 200 170 50 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 18)))
      rendererDrawColor ren $= V4 80 80 90 255
      fillRect ren (Just (Rectangle (P (V2 208 46)) (V2 6 6)))
    GoalUfo _ -> do
      rendererDrawColor ren $= V4 180 120 255 255
      fillRect ren (Just (Rectangle (P (V2 200 42)) (V2 20 16)))
      rendererDrawColor ren $= V4 220 200 255 255
      fillRect ren (Just (Rectangle (P (V2 204 40)) (V2 12 6)))
    GoalCarpet _ -> do
      -- Magenta weave swatch (地毯)
      rendererDrawColor ren $= V4 180 100 160 255
      fillRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
      rendererDrawColor ren $= V4 220 140 200 255
      fillRect ren (Just (Rectangle (P (V2 204 44)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 212 44)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 208 50)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 204 52)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 212 52)) (V2 4 4)))
    GoalCollect col _ -> do
      let (r, g, b) = colorRGB col
      rendererDrawColor ren $= V4 r g b 255
      fillRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
      rendererDrawColor ren $= white
      drawRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
    GoalScore _ -> pure ()

  -- Moves meter
  let moveCap = max (gsMoves gs) (lvlMoves lvl)
  drawMeter ren 10 68 (gsMoves gs) (max 1 moveCap) (V4 100 160 240 255)
  drawNumber ren 10 72 2 white (gsMoves gs)


  -- Booster charges + tool mode
  do
    let hx = winW - 200
    rendererDrawColor ren $= V4 255 140 80 255
    fillRect ren (Just (Rectangle (P (V2 hx 8)) (V2 14 14)))
    drawNumber ren (hx + 18) 8 2 white (gsHammers gs)
    rendererDrawColor ren $= V4 100 180 255 255
    fillRect ren (Just (Rectangle (P (V2 (hx + 50) 8)) (V2 14 14)))
    drawNumber ren (hx + 68) 8 2 white (gsFreeSwaps gs)
    rendererDrawColor ren $= V4 220 80 220 255
    fillRect ren (Just (Rectangle (P (V2 (hx + 100) 8)) (V2 14 14)))
    drawNumber ren (hx + 118) 8 2 white (gsCrossClears gs)
    case appTool app of
      ToolHammer -> do
        rendererDrawColor ren $= V4 255 180 80 255
        drawBannerWord ren (hx) 72 2 (V4 255 200 100 255) "HAMMER"
      ToolFreeSwap _ -> do
        drawBannerWord ren (hx) 72 2 (V4 140 200 255 255) "SWAP"
      ToolCross -> do
        drawBannerWord ren (hx) 72 2 (V4 240 140 240 255) "CROSS"
      ToolNone -> pure ()

  -- Combo badge (连击反馈)：回放中显示当前轮连击，播完后短暂显示本步最高连击
  let badge = case playingCascade app of
        Just c | cCombo c >= 2 -> Just (cCombo c, False)
        Just _ -> Nothing
        Nothing
          | appComboShow app > 0 && appComboBest app >= 2 -> Just (appComboBest app, True)
          | otherwise -> Nothing
  forM_ badge $ \(n, summary) -> do
    let (cr, cg, cb) = styleRGB (comboStyle n) (appPulse app)
        pulseBright = fromIntegral (200 + (appPulse app `mod` 40)) :: Word8
        badgeCol = V4 cr cg cb 255
    rendererDrawColor ren $= V4 50 20 10 255
    fillRect ren (Just (Rectangle (P (V2 (winW - 130) 30)) (V2 110 50)))
    rendererDrawColor ren $= badgeCol
    drawRect ren (Just (Rectangle (P (V2 (winW - 130) 30)) (V2 110 50)))
    if summary
      then do
        -- 「N COMBO!」总结
        drawNumber ren (winW - 124) 38 4 badgeCol n
        drawBannerWord ren (winW - 96) 42 2 (V4 255 pulseBright 80 255) "COMBO!"
      else do
        drawBannerWord ren (winW - 124) 34 2 (V4 255 pulseBright 80 255) "COMBO"
        drawNumber ren (winW - 70) 52 3 badgeCol n

  -- Status strip
  case gsOver gs of
    Just (Won _) -> rendererDrawColor ren $= V4 60 180 90 255
    Just (LevelClear _ _) -> rendererDrawColor ren $= V4 220 180 60 255
    Just (Lost _) -> rendererDrawColor ren $= V4 200 70 70 255
    _ ->
      rendererDrawColor ren $=
        if gsShuffled gs then V4 180 140 220 255 else V4 70 70 90 255
  fillRect ren (Just (Rectangle (P (V2 (winW - 24) 8)) (V2 16 (hudH - 16))))


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
            let (r, g, b) = if k >= 2 then styleRGB (comboStyle k) (appPulse app) else (255, 244, 200)
                px = 3 :: CInt
                w = 4 * px * fromIntegral (length (show n) + 1)
                x0 = popLeft w cx
                col = V4 r g b a
            rendererDrawColor ren $= col
            -- 「+」
            fillRect ren (Just (Rectangle (P (V2 x0 (cy + px))) (V2 (3 * px) px)))
            fillRect ren (Just (Rectangle (P (V2 (x0 + px) cy)) (V2 px (3 * px))))
            drawNumber ren (x0 + 4 * px) (cy - px) px col n

drawMeter :: Renderer -> CInt -> CInt -> Int -> Int -> V4 Word8 -> IO ()
drawMeter ren x y value cap col = do
  let maxW = winW - 48
  rendererDrawColor ren $= V4 20 20 30 255
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 maxW 24)))
  rendererDrawColor ren $= col
  let w =
        if cap <= 0
          then 0
          else min maxW (max 0 (fromIntegral value * maxW `div` fromIntegral (max 1 cap)))
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 w 24)))
  rendererDrawColor ren $= V4 200 200 220 255
  drawRect ren (Just (Rectangle (P (V2 x y)) (V2 maxW 24)))

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
