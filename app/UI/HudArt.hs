{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HUD 与叠层的贴图绘制：九宫格面板、目标图标与进度、道具次数、连击徽章与「N 连击！」总结、
-- 提示 / 道具横幅、键位条、暂停帮助、结算面板、「连击 xN」弹字与得分浮字。
--
-- 依赖：UI.TextArt、UI.BoardArt（宝石小图标）、UI.GoalStyle（目标图标 / 色调）、Art、ComboFx（样式与浮字曲线）、UI.Types、UI.Layout。
-- 不变量：结算面板在回放播完后才画；连击总结只在最高连击 ≥ 2 时显示。
module UI.HudArt
  ( drawHudArt
  , drawComboSummaryArt
  , drawTipBannerArt
  , drawToolBannerArt
  , drawHelpStripArt
  , drawPauseHelpArt
  , drawOverlayArt
  , drawOverlayArtNow
  , drawPopsArt
  ) where

import Art
import ComboFx
import Control.Monad (forM_, void)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.BoardArt
import UI.GoalStyle (goalIcon, goalTint)
import UI.Layout
import UI.Presentation (comboPopSprite, scorePopRGB)
import UI.TextArt
import UI.Types

--------------------------------------------------------------------------------
-- 贴图版 HUD / 横幅 / 暂停 / 结算 / 地图
--------------------------------------------------------------------------------

-- | 贴图版 HUD：关卡徽章、目标与进度条、步数、分数（回放中滚动）、道具次数、连击徽章 / 总结。
drawHudArt :: Renderer -> Art -> App -> IO ()
drawHudArt ren art app = do
  let gs = appGame app
      li = min (gsLevel gs) (levelCount - 1)
      mlvl = lookupLevel li
      white = V4 245 245 255 255
      dim = V4 150 145 190 255
      gold = V4 255 214 90 255
  _ <- drawPanel ren art "panel_dark" (rect 8 6 464 96) 14
  -- 关卡徽章 + 名称
  _ <- drawSprite ren art "medal" (rect 14 11 44 44)
  textAC ren art 36 24 3 white (show (li + 1))
  _ <- if gsDaily gs
    then zhA ren art "zh_daily" 66 12 22
    else zhA ren art ("name_" ++ show li) 66 11 24
  -- 关卡进度点：已过绿、当前金、未解锁暗
  -- 间距 6（38 关时与段 5 之前逐像素相同）；关卡更多时收窄，保证最后一个点不钻到道具面板（x = 298）下面
  let dotStep = min 6 (228 `div` max 1 levelCount) :: Int
  forM_ [0 .. levelCount - 1] $ \i -> do
    let xD = 66 + fromIntegral (i * dotStep)
        (col, yy, hh)
          | i == li = (V4 255 214 90 255, 38, 10)
          | i < li = (V4 90 210 130 255, 40, 6)
          | i <= appMaxReached app = (V4 120 180 140 255, 40, 6)
          | otherwise = (V4 80 72 130 255, 40, 6)
    rendererDrawColor ren $= col
    fillRect ren (Just (rect xD yy 4 hh))
  -- 道具：锤子 / 自由交换 / 十字消（当前模式金框）
  let chips =
        [ ("icon_hammer", gsHammers gs, appTool app == ToolHammer)
        , ("icon_swap", gsFreeSwaps gs, case appTool app of ToolFreeSwap _ -> True; _ -> False)
        , ("icon_cross", gsCrossClears gs, appTool app == ToolCross)
        ]
  forM_ (zip [0 :: CInt ..] chips) $ \(i, (ic, n, active)) -> do
    let cx = 298 + i * 57
    _ <- drawPanel ren art (if active then "panel_gold" else "panel_chip") (rect cx 11 53 32) 10
    _ <- drawSprite ren art ic (rect (cx + 4) 15 24 24)
    textA ren art (cx + 30) 18 3 (if n > 0 then white else dim) (show n)
  -- 目标条
  let goal = gsGoal gs
      prog = gsProgress gs  -- 第 5 刀：与窗口标题 / 网页版同一个数（第 5 刀前是本模块的 hudProgress）
      targ = goalTarget goal
  _ <- drawSprite ren art (goalIcon goal) (rect 12 50 26 26)
  meterA ren art 42 53 332 prog targ (goalTint goal) (show prog ++ "/" ++ show targ)
  -- 步数条（≤5 步时变红并闪烁）
  let mv = gsMoves gs
      moveCap = max mv (maybe mv lvlMoves mlvl)
      low = mv <= 5
      tintMv
        | low = let k = round (160 + 95 * breathe (appPulse app) 40) :: Int in V3 255 (fromIntegral (k `div` 2)) 80
        | otherwise = V3 90 165 255
  _ <- drawSprite ren art "icon_moves" (rect 13 77 24 24)
  meterA ren art 42 79 332 mv (max 1 moveCap) tintMv (show mv)
  -- 右下：回放中显示当前轮「连击 xN」；播完后短暂显示本步总结「N 连击！」；否则得分。
  -- 回放期间分数随每一轮消失逐步滚动上涨（结算值早已写入 gsScore，这里只是显示）。
  case playingCascade app of
    Just c
      | cCombo c >= 2 -> do
          let (r, g, b) = styleRGB (comboStyle (cCombo c)) (appPulse app)
          _ <- drawPanel ren art "panel_gold" (rect 382 52 84 48) 12
          zhAC ren art comboPopSprite 424 56 18
          textAC ren art 424 76 3 (V4 r g b 255) ("x" ++ show (cCombo c))
      | otherwise -> do
          _ <- drawPanel ren art "panel_chip" (rect 382 52 84 48) 12
          zhAC ren art "zh_score" 424 56 18
          textAC ren art 424 76 3 gold (show (cBase c + cGain c))
    Nothing
      | appComboShow app > 0 && appComboBest app >= 2 -> drawComboSummaryArt ren art app
      | otherwise -> do
          _ <- drawPanel ren art "panel_chip" (rect 382 52 84 48) 12
          zhAC ren art (if gsShuffled gs then "zh_shuffle" else "zh_score") 424 56 18
          textAC ren art 424 76 3 gold (show (gsScore gs))

-- | HUD 右下「N 连击！」总结：放大弹入 + 等级色光晕，最后 16 帧淡出（回到得分）。
drawComboSummaryArt :: Renderer -> Art -> App -> IO ()
drawComboSummaryArt ren art app = do
  let n = appComboBest app
      age = comboSummaryFrames - appComboShow app
      s = comboPopScale age
      left = appComboShow app
      a = if left >= 16 then 255 else fromIntegral (left * 255 `div` 16) :: Word8
      (r, g, b) = styleRGB (comboStyle n) (appPulse app)
      h = round (20 * s) :: CInt
      gh = round (26 * s) :: CInt
      num = show n
      nw = glyphTextW gh num
      zw = zhW art "zh_combo_end" h
      gap = 2 :: CInt
      total = nw + gap + zw
      cx = 424
      cy = 76
      x0 = cx - total `div` 2
  _ <- drawPanel ren art "panel_gold" (rect 382 52 84 48) 12
  void (drawSpriteAdd ren art "spark" (rect (cx - 60) (cy - 34) 120 68) (V3 r g b) (a `div` 2))
  glyphText ren art x0 (cy - gh `div` 2) gh (V4 r g b a) num
  void (drawSpriteMod ren art "zh_combo_end" (rect (x0 + nw + gap) (cy - h `div` 2) zw h) (V3 255 255 255) a)

-- | 首关提示横幅（棋盘上沿）。
drawTipBannerArt :: Renderer -> Art -> App -> IO ()
drawTipBannerArt ren art app
  | appPaused app || appMapOpen app = pure ()
  | appTipFrames app <= 0 = pure ()
  | gsLevel (appGame app) /= 0 = pure ()
  | appTool app /= ToolNone = pure ()
  | otherwise = do
      let y = hudH + padPx + 4
          w = 40 + zhW art "zh_tip" 20
          x = (winW - w) `div` 2
      _ <- drawPanel ren art "panel_gold" (rect x y w 30) 12
      keyChipA ren art (x + 10) (y + 5) 'H' (V4 255 220 100 255)
      void (zhA ren art "zh_tip" (x + 34) (y + 5) 20)

-- | 道具点选模式横幅：告诉玩家下一步要点哪里。
drawToolBannerArt :: Renderer -> Art -> App -> IO ()
drawToolBannerArt ren art app
  | appPaused app || appMapOpen app = pure ()
  | otherwise = case appTool app of
      ToolNone -> pure ()
      tool -> do
        let (ic, key) = case tool of
              ToolHammer -> ("icon_hammer", "zh_tool_hammer")
              ToolFreeSwap _ -> ("icon_swap", "zh_tool_swap")
              _ -> ("icon_cross", "zh_tool_cross")
            y = hudH + padPx + 4
            w = 44 + zhW art key 20
            x = (winW - w) `div` 2
        _ <- drawPanel ren art "panel_gold" (rect x y w 30) 12
        _ <- drawSprite ren art ic (rect (x + 10) (y + 4) 22 22)
        void (zhA ren art key (x + 36) (y + 5) 20)

-- | 开局 / 取消暂停后的按键条。
drawHelpStripArt :: Renderer -> Art -> App -> IO ()
drawHelpStripArt ren art app
  | appPaused app || appMapOpen app = pure ()
  | appHelpFrames app <= 0 = pure ()
  | otherwise = do
      -- 放在棋盘底部浮层，避免遮住 HUD 的步数行
      let y = winH - padPx - 34
          keys = "H123USDMRNP"
      _ <- drawPanel ren art "panel_chip" (rect 8 y 464 28) 10
      forM_ (zip [0 :: CInt ..] keys) $ \(i, ch) ->
        keyChipA ren art (13 + i * 24) (y + 3) ch (V4 255 220 120 255)
      void (zhA ren art "zh_help_more" (13 + 11 * 24 + 4) (y + 6) 16)

-- | 全屏暂停：按键说明（中文）+ 形状图例。
drawPauseHelpArt :: Renderer -> Art -> App -> IO ()
drawPauseHelpArt ren art app
  | not (appPaused app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 8 6 20 200
      fillRect ren (Just (rect 0 0 winW winH))
      let px0 = 40
          py0 = 40
          pw = winW - 80
          ph = winH - 80
      _ <- drawPanel ren art "panel_gold" (rect px0 py0 pw ph) 18
      zhAC ren art "zh_pause" (winW `div` 2) (py0 + 14) 32
      let rows :: [(Char, String, String)]
          rows =
            [ ('H', "zh_k_hint", "HINT"), ('1', "zh_k_hammer", "HAMMER"), ('2', "zh_k_swap", "SWAP")
            , ('3', "zh_k_cross", "CROSS"), ('U', "zh_k_undo", "UNDO"), ('S', "zh_k_shuffle", "SHUFFLE")
            , ('D', "zh_k_daily", "DAILY"), ('M', "zh_k_map", "MAP"), ('R', "zh_k_retry", "RETRY")
            , ('N', "zh_k_next", "NEXT"), ('P', "zh_k_play", "PLAY")
            ]
      forM_ (zip [0 :: CInt ..] rows) $ \(i, (ch, key, en)) -> do
        let yy = py0 + 60 + i * 30
        keyChipA ren art (px0 + 40) yy ch (V4 255 220 120 255)
        _ <- zhA ren art key (px0 + 72) yy 20
        textA ren art (px0 + pw - 40 - textW 3 en) (yy + 2) 3 (V4 190 185 240 255) en
      -- 图例：颜色 × 形状
      let ly = py0 + ph - 62
      _ <- zhA ren art "zh_legend" (px0 + 40) (ly + 8) 20
      forM_ (zip [0 :: CInt ..] allColors) $ \(i, c) ->
        void (drawSprite ren art (gemSprite c) (rect (px0 + 100 + i * 50) ly 40 40))

-- | 结算面板：过关 / 胜利 / 失败 + 星级 + 分数 + 下一步提示。
drawOverlayArt :: Renderer -> Art -> App -> IO ()
drawOverlayArt ren art app
  -- 结算面板等连锁回放播完再出，别挡住最后几轮
  | animBusy app = pure ()
  | otherwise = drawOverlayArtNow ren art app

drawOverlayArtNow :: Renderer -> Art -> App -> IO ()
drawOverlayArtNow ren art app = case gsOver (appGame app) of
  Nothing -> pure ()
  Just outcome -> do
    rendererDrawColor ren $= V4 10 8 24 170
    fillRect ren (Just (rect 0 hudH winW (winH - hudH)))
    let pw = 380
        ph = 180
        px0 = (winW - pw) `div` 2
        py0 = hudH + padPx + (boardPx - ph) `div` 2
        cx = winW `div` 2
        stars = starRating (appStartMoves app) (gsMoves (appGame app))
        drawStars = forM_ [0 .. 2 :: Int] $ \i ->
          void (drawSprite ren art (if i < stars then "star_on" else "star_off")
                  (rect (cx - 72 + fromIntegral i * 48) (py0 + 58) 48 48))
        white = V4 255 255 255 255
    _ <- drawPanel ren art "panel_gold" (rect px0 py0 pw ph) 18
    case outcome of
      LevelClear s nextIdx -> do
        zhAC ren art "zh_clear" cx (py0 + 12) 38
        drawStars
        -- 得分：图标 + 金色数字
        let sw = textW 3 (show s)
            sx = cx - (sw + 30) `div` 2
        _ <- drawSprite ren art "icon_score" (rect sx (py0 + 108) 24 24)
        textA ren art (sx + 30) (py0 + 111) 3 (V4 255 220 120 255) (show s)
        let w = zhW art "zh_next" 22
            rowX = cx - (w + 60) `div` 2
        _ <- zhA ren art "zh_next" rowX (py0 + 142) 22
        textA ren art (rowX + w + 6) (py0 + 144) 3 white (show (nextIdx + 1))
        keyChipA ren art (px0 + pw - 40) (py0 + 144) 'N' (V4 140 230 160 255)
      Won s -> do
        zhAC ren art "zh_win" cx (py0 + 12) 38
        drawStars
        _ <- drawSprite ren art "icon_score" (rect (cx - 60) (py0 + 120) 32 32)
        textA ren art (cx - 20) (py0 + 124) 4 white (show s)
      Lost s -> do
        zhAC ren art "zh_lose" cx (py0 + 12) 38
        _ <- drawSprite ren art "icon_score" (rect (cx - 60) (py0 + 64) 32 32)
        textA ren art (cx - 20) (py0 + 68) 4 white (show s)
        zhAC ren art "zh_retry" cx (py0 + 130) 26
      _ -> pure ()

-- | 浮字：「连击 xN」放大弹出（等级越高字越大、颜色越暖、x5+ 彩色流转）+ 本轮得分「+N」飘起。
drawPopsArt :: Renderer -> Art -> App -> IO ()
drawPopsArt ren art app
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
                sc = comboPopScale (tpAge p)
                h = round (fromIntegral (csHeight st) * sc) :: CInt
                gh = round (fromIntegral h * 1.2 :: Double) :: CInt
                numS = "x" ++ show k
                zw = zhW art comboPopSprite h
                nw = glyphTextW gh numS
                gap = h `div` 6
                total = zw + gap + nw
                x0 = popLeft total cx
                hx = x0 + total `div` 2
                haloW = total + h * 2
                haloH = h * 3
            -- 柔光底：等级色，让文字在任何宝石颜色上都读得清
            void (drawSpriteMod ren art "spark" (rect (hx - haloW `div` 2) (cy - haloH `div` 2) haloW haloH) (V3 20 10 40) (a `div` 2 + a `div` 4))
            void (drawSpriteAdd ren art "spark" (rect (hx - haloW `div` 2) (cy - haloH `div` 2) haloW haloH) (V3 r g b) (a `div` 3))
            void (drawSpriteMod ren art comboPopSprite (rect x0 (cy - h `div` 2) zw h) (V3 r g b) a)
            glyphText ren art (x0 + zw + gap) (cy - gh `div` 2) gh (V4 r g b a) numS
          PopScore n k -> do
            let (r, g, b) = scorePopRGB k (appPulse app) -- 表现表 EvScore 行（连击轮用等级色）
                h = round ((20 + 2 * fromIntegral (min 4 (max 0 (k - 1)))) * scorePopScale (tpAge p) :: Double) :: CInt
                str = "+" ++ show n
                w = glyphTextW h str
            glyphText ren art (popLeft w cx) (cy - h `div` 2) h (V4 r g b a) str
