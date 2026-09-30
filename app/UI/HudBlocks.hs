{-# LANGUAGE OverloadedStrings #-}

-- | 几何降级版 HUD 的各个区块（第 10 刀从 UI.HudPrim.drawHud 按区块拆出，函数体逐字搬运）：
-- 底板、关卡号与进度点、目标进度条、收集目标色块、步数条、道具次数与工具模式、连击徽章、结局色条。
-- UI.HudPrim.drawHud 按固定顺序依次调用它们（顺序即绘制层次，不要调换）。
--
-- 依赖：Match3.View（第 11 刀起各区块读视图模型，不再从 GameState 现算）、UI.Glyph（点阵数字 / 字）、UI.GoalStyle（进度条颜色）、UI.Presentation（连击等级色）、UI.Layout、UI.Types。
-- 新增一个 HUD 区块：在这里写一个 @hudXxx :: Renderer -> …@，再在 drawHud 的顺序表里加一行。
module UI.HudBlocks
  ( hudWhite
  , hudDim
  , hudFrame
  , hudLevel
  , hudGoal
  , hudBoss
  , hudGoalSwatch
  , hudMoves
  , hudBoosters
  , hudComboBadge
  , hudStatus
  , hudSound
  , drawMeter
  ) where

import ComboFx (Cascade (..))
import Control.Monad (forM_)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import Match3.View
import UI.Audio (bgmEnabled, sfxEnabled)
import UI.Glyph
import UI.GoalStyle (goalPip)
import UI.Layout
import UI.Presentation (comboStyle, styleRGB)
import UI.Types

-- | HUD 正文色（数字 / 边框）。
hudWhite :: V4 Word8
hudWhite = V4 230 230 245 255

-- | HUD 次要色（目标值、分隔线）。
hudDim :: V4 Word8
hudDim = V4 140 140 170 255

-- | 顶部信息栏底板。
hudFrame :: Renderer -> IO ()
hudFrame ren = do
  rendererDrawColor ren $= V4 42 42 58 255
  fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW hudH)))

-- | 关卡号 + 各关进度点（当前关金色、已过绿色、未到灰色；38 关以 7px 间距排进 HUD）。
hudLevel :: Renderer -> GameView -> IO ()
hudLevel ren gv = do
  let white = hudWhite
  drawNumber ren 10 8 3 white (gvLevel gv + 1)
  -- 几何版按 gsLevel 原值画点，只分当前 / 已过 / 其余（不区分已解锁）
  forM_ (zip [0 :: Int ..] (levelDots (gvLevel gv) (-1))) $ \(i, ld) -> do
    let col = case ld of
          DotCurrent -> V4 255 200 80 255
          DotDone -> V4 80 180 120 255
          _ -> V4 60 60 80 255
        -- 38 levels fit in HUD: 7px stride
        xDot = 60 + fromIntegral i * 7
    rendererDrawColor ren $= col
    fillRect ren (Just (Rectangle (P (V2 xDot 10)) (V2 7 14)))

-- | 目标进度条（分数或收集），下方「当前 / 目标」数字。
hudGoal :: Renderer -> GoalInfo -> IO ()
hudGoal ren gi = do
  let white = hudWhite
      dim = hudDim
  let prog = giProgress gi
      targ = giTarget gi
      meterCol = goalPip (giGoal gi)
  drawMeter ren 10 36 prog targ meterCol
  drawNumber ren 10 40 2 white prog
  rendererDrawColor ren $= dim
  fillRect ren (Just (Rectangle (P (V2 78 48)) (V2 8 2)))
  drawNumber ren 90 40 2 dim targ

-- | 雪怪 Boss 血条（新玩法 5，几何版）：目标是「击败 Boss」时盖在目标条上——红色剩余血量 + 剩余 / 满血数字。
hudBoss :: Renderer -> Maybe BossView -> IO ()
hudBoss ren mbv = case mbv of
  Nothing -> pure ()
  Just bv -> do
    drawMeter ren 10 36 (bvHp bv) (max 1 (bvMax bv)) (V4 235 70 80 255)
    drawNumber ren 10 40 2 hudWhite (bvHp bv)
    rendererDrawColor ren $= hudDim
    fillRect ren (Just (Rectangle (P (V2 78 48)) (V2 8 2)))
    drawNumber ren 90 40 2 hudDim (bvMax bv)

-- | 收集目标的色块（宝箱 / 蜂蜜 / 气球 / 饼干 / 蛋糕 / 保险箱 / UFO / 地毯 / 名字目标 / 颜色）；分数等目标不画。
hudGoalSwatch :: Renderer -> GoalInfo -> IO ()
hudGoalSwatch ren gi = do
  let white = hudWhite
  case giView gi of
    ViewCount CountChests _ -> do
      rendererDrawColor ren $= V4 220 170 60 255
      fillRect ren (Just (Rectangle (P (V2 200 42)) (V2 20 16)))
      rendererDrawColor ren $= V4 180 120 40 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 12 6)))
    ViewCount CountHoney _ -> do
      rendererDrawColor ren $= V4 240 180 40 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 18)))
      rendererDrawColor ren $= V4 200 140 20 255
      fillRect ren (Just (Rectangle (P (V2 206 36)) (V2 8 6)))
    ViewCount CountBalloons _ -> do
      rendererDrawColor ren $= V4 255 120 160 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 14 16)))
      rendererDrawColor ren $= V4 200 80 120 255
      fillRect ren (Just (Rectangle (P (V2 209 54)) (V2 4 6)))
    ViewCount CountCookies _ -> do
      rendererDrawColor ren $= V4 210 160 90 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 16)))
      rendererDrawColor ren $= V4 90 50 30 255
      fillRect ren (Just (Rectangle (P (V2 206 44)) (V2 3 3)))
      fillRect ren (Just (Rectangle (P (V2 212 48)) (V2 3 3)))
    ViewCount CountCakes _ -> do
      -- Pink frosted cake swatch (distinct from tan cookie)
      rendererDrawColor ren $= V4 255 140 180 255
      fillRect ren (Just (Rectangle (P (V2 202 44)) (V2 16 14)))
      rendererDrawColor ren $= V4 255 220 230 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 12 8)))
      rendererDrawColor ren $= V4 255 80 120 255
      fillRect ren (Just (Rectangle (P (V2 208 36)) (V2 4 4)))
    ViewCount CountSafes _ -> do
      -- Brass vault door swatch
      rendererDrawColor ren $= V4 200 170 50 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 18)))
      rendererDrawColor ren $= V4 80 80 90 255
      fillRect ren (Just (Rectangle (P (V2 208 46)) (V2 6 6)))
    ViewCount CountUfo _ -> do
      rendererDrawColor ren $= V4 180 120 255 255
      fillRect ren (Just (Rectangle (P (V2 200 42)) (V2 20 16)))
      rendererDrawColor ren $= V4 220 200 255 255
      fillRect ren (Just (Rectangle (P (V2 204 40)) (V2 12 6)))
    ViewCount CountCarpets _ -> do
      -- Magenta weave swatch (地毯)
      rendererDrawColor ren $= V4 180 100 160 255
      fillRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
      rendererDrawColor ren $= V4 220 140 200 255
      fillRect ren (Just (Rectangle (P (V2 204 44)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 212 44)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 208 50)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 204 52)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 212 52)) (V2 4 4)))
    ViewCount (CountNamed name) _ -> do
      let (r, g, b) = namedRGB name
      rendererDrawColor ren $= V4 r g b 255
      fillRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
    ViewCollect col _ -> do
      let (r, g, b) = colorRGB col
      rendererDrawColor ren $= V4 r g b 255
      fillRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
      rendererDrawColor ren $= white
      drawRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
    _ -> pure ()  -- 分数 / 多色 / 石块 / 其余：无色块

-- | 剩余步数条（满格 = 该关印制步数）。
hudMoves :: Renderer -> GameView -> IO ()
hudMoves ren gv = do
  let white = hudWhite
  let moveCap = gvMoveCap gv
  drawMeter ren 10 68 (gvMoves gv) (max 1 moveCap) (V4 100 160 240 255)
  drawNumber ren 10 72 2 white (gvMoves gv)

-- | 道具次数（锤子 / 自由交换 / 十字）与当前工具模式字样。
hudBoosters :: Renderer -> App -> Boosters -> IO ()
hudBoosters ren app bs = do
  let white = hudWhite
  let hx = winW - 200
  rendererDrawColor ren $= V4 255 140 80 255
  fillRect ren (Just (Rectangle (P (V2 hx 8)) (V2 14 14)))
  drawNumber ren (hx + 18) 8 2 white (bHammers bs)
  rendererDrawColor ren $= V4 100 180 255 255
  fillRect ren (Just (Rectangle (P (V2 (hx + 50) 8)) (V2 14 14)))
  drawNumber ren (hx + 68) 8 2 white (bFreeSwaps bs)
  rendererDrawColor ren $= V4 220 80 220 255
  fillRect ren (Just (Rectangle (P (V2 (hx + 100) 8)) (V2 14 14)))
  drawNumber ren (hx + 118) 8 2 white (bCrossClears bs)
  case appTool app of
    ToolHammer -> do
      rendererDrawColor ren $= V4 255 180 80 255
      drawBannerWord ren (hx) 72 2 (V4 255 200 100 255) "HAMMER"
    ToolFreeSwap _ -> do
      drawBannerWord ren (hx) 72 2 (V4 140 200 255 255) "SWAP"
    ToolCross -> do
      drawBannerWord ren (hx) 72 2 (V4 240 140 240 255) "CROSS"
    ToolNone -> pure ()

-- | 连击徽章（连击反馈）：回放中显示当前轮连击，播完后短暂显示本步最高连击。
hudComboBadge :: Renderer -> App -> GameView -> IO ()
hudComboBadge ren app gv = do
  let replay = fmap (\c -> ReplayView (cCombo c) (cBase c + cGain c)) (playingCascade app)
      badge = case scoreBadge replay (appComboShow app) (appComboBest app) gv of
        BadgeCombo n -> Just (n, False)
        BadgeSummary n -> Just (n, True)
        _ -> Nothing  -- 几何版不画滚动分数 / 得分徽章
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

-- | 右侧结局色条（胜 / 过关 / 负 / 已洗牌 / 进行中）。
hudStatus :: Renderer -> PlayStatus -> IO ()
hudStatus ren st = do
  rendererDrawColor ren $= case st of
    PlayWon _ -> V4 60 180 90 255
    PlayCleared _ _ -> V4 220 180 60 255
    PlayLost _ -> V4 200 70 70 255
    PlayShuffled -> V4 180 140 220 255
    PlayOn -> V4 70 70 90 255
  fillRect ren (Just (Rectangle (P (V2 (winW - 24) 8)) (V2 16 (hudH - 16))))

-- | 进度条：底 + 按 value / cap 填充 + 边框。
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

-- | 音效 / BGM 两枚独立芯片（几何降级用拉丁 X/M；静音为 S）。
hudSound :: Renderer -> IO ()
hudSound ren = do
  sfxOn <- sfxEnabled
  bgmOn <- bgmEnabled
  let drawOne (x, y, w, h) ch = do
        rendererDrawColor ren $= V4 30 24 60 200
        fillRect ren (Just (Rectangle (P (V2 x y)) (V2 w h)))
        drawGlyph ren (x + 16) (y + 8) 3 (V4 255 224 130 255) ch
  drawOne sfxChipRect (if sfxOn then 'X' else 'S')
  drawOne bgmChipRect (if bgmOn then 'M' else 'S')
