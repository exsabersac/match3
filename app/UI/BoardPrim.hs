{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 棋盘几何降级绘制：没有贴图时用矩形 / 线条画宝石、障碍、叠层、传送门、飞碟、皮带、蔓延预告和粒子。
--
-- 依赖：UI.Types、UI.Layout、Match3.Core、SDL。
-- 同步：新增棋盘元素时这里与 UI.BoardArt（贴图版）都要画，保证缺图时仍可玩。
module UI.BoardPrim
  ( drawParticles
  , drawGemAt
  , drawStaticPrim
  , drawVineSpreadHints
  , drawChocoSpreadHints
  , drawPortal
  , drawUfo
  , drawBelt
  ) where

import Control.Monad (forM_, unless, when)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.Layout
import UI.Types

--------------------------------------------------------------------------------
-- Particles
--------------------------------------------------------------------------------

drawParticles :: Renderer -> [Particle] -> IO ()
drawParticles ren = mapM_ drawOne
  where
    drawOne p = do
      let fade =
            if pMax p <= 0
              then 255
              else fromIntegral (255 * pLife p `div` pMax p) :: Word8
      rendererDrawColor ren $= V4 (pR p) (pG p) (pB p) fade
      let s = pSize p
          x = round (pX p) - s `div` 2
          y = round (pY p) - s `div` 2
      fillRect ren (Just (Rectangle (P (V2 x y)) (V2 s s)))

-- | 几何降级版的单格绘制：按格子内容画宝石 / 特殊块 / 障碍 / 叠层 / 层数（约 540 行的大 case，第二刀拆分）。
drawGemAt :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
drawGemAt ren x y cell flashing = case cell of
  Stone layers -> do
    let gap = 3 :: CInt
        (cr, cg, cb) = if flashing then (200, 200, 200) else (90, 90, 100)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Speckle to look rocky
    rendererDrawColor ren $= V4 60 60 70 255
    fillRect ren (Just (Rectangle (P (V2 (x + 12) (y + 14))) (V2 8 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + 28) (y + 30))) (V2 10 7)))
    -- Layer pips (开心消消乐箱子层数感)
    rendererDrawColor ren $= V4 220 200 120 255
    forM_ [0 .. min 3 layers - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Chest layers -> do
    let gap = 3 :: CInt
        (cr, cg, cb) = if flashing then (255, 230, 140) else (200, 150, 50)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 6)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 6))))
    rendererDrawColor ren $= V4 230 180 70 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) 14)))
    rendererDrawColor ren $= V4 80 160 220 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + cellPx `div` 2 - 2))) (V2 10 10)))
    rendererDrawColor ren $= V4 255 240 180 255
    forM_ [0 .. min 3 layers - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Honey layers -> do
    -- Amber honey jar (蜂蜜罐): round body + lid + drip, distinct from chest
    let gap = 4 :: CInt
        (cr, cg, cb) = if flashing then (255, 230, 120) else (230, 170, 35)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap + 4) (y + gap + 10)))
            (V2 (cellPx - 2 * gap - 8) (cellPx - 2 * gap - 12))))
    -- Lid
    rendererDrawColor ren $= V4 180 120 30 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 8) (y + gap + 2))) (V2 (cellPx - 2 * gap - 16) 10)))
    -- Highlight drip
    rendererDrawColor ren $= V4 255 220 100 220
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + cellPx `div` 2))) (V2 8 14)))
    -- Layer pips
    rendererDrawColor ren $= V4 255 240 160 255
    forM_ [0 .. min 3 layers - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Balloon col -> do
    let (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
        gap = 6 :: CInt
    -- Roundish body
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 8))))
    -- Highlight
    rendererDrawColor ren $= V4 255 255 255 160
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 4) (y + gap + 4))) (V2 8 8)))
    -- String
    rendererDrawColor ren $= V4 220 220 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 1) (y + cellPx - 14))) (V2 2 10)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Cookie -> do
    -- Tan biscuit with chocolate chips (饼干)
    let gap = 5 :: CInt
        (cr, cg, cb) = if flashing then (255, 220, 160) else (210, 160, 90)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    rendererDrawColor ren $= V4 90 50 30 255
    fillRect ren (Just (Rectangle (P (V2 (x + 14) (y + 14))) (V2 6 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + 28) (y + 22))) (V2 5 5)))
    fillRect ren (Just (Rectangle (P (V2 (x + 18) (y + 32))) (V2 5 5)))
    fillRect ren (Just (Rectangle (P (V2 (x + 32) (y + 12))) (V2 4 4)))
    rendererDrawColor ren $= V4 180 120 60 255
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Cake layers -> do
    -- Pink layered cake (蛋糕) — frosted tiers, distinct from tan Cookie
    let gap = 4 :: CInt
        (cr, cg, cb) = if flashing then (255, 200, 220) else (255, 140, 180)
    -- Bottom tier
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 18)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 18))))
    -- Mid frosting
    rendererDrawColor ren $= V4 255 220 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 4) (y + gap + 10))) (V2 (cellPx - 2 * gap - 8) 10)))
    -- Top cherry
    rendererDrawColor ren $= V4 220 40 80 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + gap + 2))) (V2 8 8)))
    -- Layer pips
    rendererDrawColor ren $= V4 255 240 250 255
    forM_ [0 .. min 3 layers - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  MagicHat -> do
    -- Purple magic hat (魔法帽): brim + cone
    let gap = 5 :: CInt
        (cr, cg, cb) = if flashing then (200, 160, 255) else (120, 60, 180)
    rendererDrawColor ren $= V4 cr cg cb 255
    -- Brim
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + cellPx - 18))) (V2 (cellPx - 2 * gap) 10)))
    -- Cone
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap + 8) (y + gap + 4)))
            (V2 (cellPx - 2 * gap - 16) (cellPx - 2 * gap - 14))))
    -- Band
    rendererDrawColor ren $= V4 255 220 80 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 6) (y + cellPx - 24))) (V2 (cellPx - 2 * gap - 12) 5)))
    -- Star tip
    rendererDrawColor ren $= V4 255 255 200 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + gap))) (V2 6 6)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Maker col charges -> do
    -- Juice maker / factory (果汁机): metallic body + color spout + charge pips
    let gap = 4 :: CInt
        (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
    rendererDrawColor ren $= V4 90 100 120 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 8)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 8))))
    -- Spout / hopper tinted with target color
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 8) (y + gap))) (V2 (cellPx - 2 * gap - 16) 12)))
    rendererDrawColor ren $= V4 200 210 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 6) (y + gap + 14))) (V2 12 8)))
    -- Charge pips
    rendererDrawColor ren $= V4 255 220 80 255
    forM_ [0 .. min 3 charges - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Snail dr dc -> do
    -- Snail (蜗牛): olive body + shell spiral; tip shows crawl direction
    let gap = 5 :: CInt
        (cr, cg, cb) = if flashing then (180, 230, 140) else (90, 150, 60)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap + 4) (y + gap + 12)))
            (V2 (cellPx - 2 * gap - 8) (cellPx - 2 * gap - 16))))
    rendererDrawColor ren $= V4 60 110 40 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 10) (y + gap + 4))) (V2 (cellPx - 2 * gap - 20) 14)))
    rendererDrawColor ren $= V4 200 230 120 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + gap + 8))) (V2 8 8)))
    -- Direction tick
    rendererDrawColor ren $= V4 255 255 200 255
    let (tx, ty) =
          if abs dr >= abs dc
            then if dr >= 0
                   then (x + cellPx `div` 2 - 3, y + cellPx - 14)
                   else (x + cellPx `div` 2 - 3, y + 8)
            else if dc >= 0
                   then (x + cellPx - 14, y + cellPx `div` 2 - 3)
                   else (x + 8, y + cellPx `div` 2 - 3)
    fillRect ren (Just (Rectangle (P (V2 tx ty)) (V2 6 6)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Safe layers -> do
    -- Vault / safe (保险箱): dark steel door + gold dial; distinct from Chest
    let gap = 3 :: CInt
        (cr, cg, cb) = if flashing then (220, 200, 120) else (70, 75, 85)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    rendererDrawColor ren $= V4 200 170 50 255
    drawRect ren (Just (Rectangle (P (V2 (x + gap + 2) (y + gap + 2))) (V2 (cellPx - 2 * gap - 4) (cellPx - 2 * gap - 4))))
    -- Dial
    rendererDrawColor ren $= V4 220 190 60 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 8) (y + cellPx `div` 2 - 8))) (V2 16 16)))
    rendererDrawColor ren $= V4 40 40 50 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + cellPx `div` 2 - 3))) (V2 6 6)))
    rendererDrawColor ren $= V4 255 230 120 255
    forM_ [0 .. min 3 layers - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Flip front back -> do
    -- Dual-face gem (双面块): front color body + back color corner triangle
    let gap = 4 :: CInt
        (fr, fg, fb) = colorRGB front
        (br, bg, bb) = colorRGB back
        (cr, cg, cb) = if flashing then (255, 255, 255) else (fr, fg, fb)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Back-face wedge (top-right)
    rendererDrawColor ren $= V4 br bg bb 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2) (y + gap))) (V2 (cellPx `div` 2 - gap) (cellPx `div` 2 - gap))))
    rendererDrawColor ren $= V4 255 255 255 200
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Split line
    rendererDrawColor ren $= V4 30 30 40 220
    drawLine ren (P (V2 (x + gap) (y + cellPx - gap))) (P (V2 (x + cellPx - gap) (y + gap)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Surprise -> do
    -- Surprise egg / gift box (彩蛋): pink package + gold bow
    let gap = 4 :: CInt
        (cr, cg, cb) = if flashing then (255, 200, 220) else (255, 90, 150)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    rendererDrawColor ren $= V4 255 210 80 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + gap))) (V2 6 (cellPx - 2 * gap))))
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + cellPx `div` 2 - 3))) (V2 (cellPx - 2 * gap) 6)))
    rendererDrawColor ren $= V4 255 255 255 220
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Bottle col -> do
    -- Dye bottle (染色瓶): body tinted with bottle color + neck
    let gap = 6 :: CInt
        (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 10)))
            (V2 (cellPx - 2 * gap) (cellPx - gap - 14))))
    -- Neck
    rendererDrawColor ren $= V4 220 220 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + gap))) (V2 10 12)))
    rendererDrawColor ren $= V4 40 40 50 255
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap + 10))) (V2 (cellPx - 2 * gap) (cellPx - gap - 14))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  TimeSpirit -> do
    -- Time spirit (时间精灵): cyan orb + hourglass ticks; adjacent clear → +2 moves
    rendererDrawColor ren $= V4 40 180 220 255
    fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 10))) (V2 (cellPx - 20) (cellPx - 20))))
    rendererDrawColor ren $= V4 200 250 255 255
    fillRect ren (Just (Rectangle (P (V2 (x + 16) (y + 14))) (V2 (cellPx - 32) 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + 16) (y + cellPx - 20))) (V2 (cellPx - 32) 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + 18))) (V2 6 (cellPx - 36))))
    rendererDrawColor ren $= V4 255 240 100 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 8) (y + cellPx `div` 2 - 4))) (V2 16 8)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Countdown col turns -> do
    let (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
        gap = 3 :: CInt
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Dark fuse / bomb body
    rendererDrawColor ren $= V4 20 20 20 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 10) (y + cellPx `div` 2 - 10))) (V2 20 20)))
    rendererDrawColor ren $= V4 255 180 40 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + cellPx `div` 2 - 5))) (V2 10 10)))
    -- Turn pips (up to 9)
    rendererDrawColor ren $= V4 255 255 255 255
    forM_ [0 .. min 8 turns - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 6 + fromIntegral (i `mod` 3) * 10) (y + 6 + fromIntegral (i `div` 3) * 8)))
              (V2 6 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Gem _ _ ice ov -> do
    let (cr0, cg0, cb0) = colorRGB (cellColor cell)
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
        gap = 3 :: CInt
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
    -- Ice overlay: cyan frames + crack scratches (开心消消乐冰裂纹)
    when (ice > 0) $ do
      rendererDrawColor ren $= V4 140 220 255 220
      drawRect ren (Just (Rectangle (P (V2 (x + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
      rendererDrawColor ren $= V4 200 240 255 200
      -- Diagonal crack lines
      drawLine ren (P (V2 (x + 8) (y + 12))) (P (V2 (x + cellPx - 10) (y + cellPx - 14)))
      drawLine ren (P (V2 (x + cellPx - 12) (y + 10))) (P (V2 (x + 14) (y + cellPx - 12)))
      when (ice > 1) $ do
        drawRect ren (Just (Rectangle (P (V2 (x + 5) (y + 5))) (V2 (cellPx - 10) (cellPx - 10))))
        drawLine ren (P (V2 (x + 10) (y + cellPx `div` 2))) (P (V2 (x + cellPx - 10) (y + cellPx `div` 2 + 4)))
    -- Grass / vine / chocolate overlays (开心消消乐草·藤蔓·巧克力)
    case ov of
      Just Grass -> do
        rendererDrawColor ren $= V4 40 140 50 200
        fillRect ren (Just (Rectangle (P (V2 (x + 6) (y + cellPx - 14))) (V2 (cellPx - 12) 8)))
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + cellPx - 20))) (V2 8 8)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx - 18) (y + cellPx - 18))) (V2 8 6)))
      Just Vine -> do
        rendererDrawColor ren $= V4 20 100 40 230
        drawRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
        drawLine ren (P (V2 (x + 8) (y + 8))) (P (V2 (x + cellPx - 10) (y + cellPx - 12)))
        drawLine ren (P (V2 (x + cellPx - 12) (y + 10))) (P (V2 (x + 12) (y + cellPx - 10)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 6))) (V2 8 8)))
      Just Choco -> do
        -- Brown chocolate slab with bite notches (不改六色宝石本体)
        rendererDrawColor ren $= V4 110 60 30 220
        fillRect ren (Just (Rectangle (P (V2 (x + 5) (y + 5))) (V2 (cellPx - 10) (cellPx - 10))))
        rendererDrawColor ren $= V4 80 40 20 255
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + 8))) (V2 (cellPx - 16) 4)))
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx `div` 2 - 2))) (V2 (cellPx - 16) 4)))
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx - 16))) (V2 (cellPx - 16) 4)))
        rendererDrawColor ren $= V4 150 90 50 200
        fillRect ren (Just (Rectangle (P (V2 (x + 12) (y + 14))) (V2 6 6)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx - 20) (y + cellPx - 22))) (V2 6 6)))
      Just (Fog layers) -> do
        -- Soft white/gray cloud veil (迷雾); layer pips
        rendererDrawColor ren $= V4 200 210 230 200
        fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
        rendererDrawColor ren $= V4 240 245 255 180
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 10))) (V2 14 10)))
        fillRect ren (Just (Rectangle (P (V2 (x + 22) (y + 18))) (V2 16 12)))
        fillRect ren (Just (Rectangle (P (V2 (x + 12) (y + 26))) (V2 18 10)))
        rendererDrawColor ren $= V4 120 140 180 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just (Chain layers) -> do
        -- Iron chain lock (锁链): gray links over gem
        rendererDrawColor ren $= V4 70 75 90 220
        drawRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
        rendererDrawColor ren $= V4 140 150 170 255
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + cellPx `div` 2 - 4))) (V2 (cellPx - 20) 8)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 10))) (V2 8 (cellPx - 20))))
        rendererDrawColor ren $= V4 200 210 230 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 6) (y + cellPx `div` 2 - 6))) (V2 12 12)))
        rendererDrawColor ren $= V4 180 190 210 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just (Freeze layers) -> do
        -- Rocket freeze (火箭冰冻): deep-blue glaze + snowflake ticks; ≠ cyan ice cracks
        rendererDrawColor ren $= V4 40 90 200 180
        fillRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
        rendererDrawColor ren $= V4 180 220 255 230
        drawRect ren (Just (Rectangle (P (V2 (x + 5) (y + 5))) (V2 (cellPx - 10) (cellPx - 10))))
        -- Snowflake cross
        drawLine ren (P (V2 (x + cellPx `div` 2) (y + 10))) (P (V2 (x + cellPx `div` 2) (y + cellPx - 10)))
        drawLine ren (P (V2 (x + 10) (y + cellPx `div` 2))) (P (V2 (x + cellPx - 10) (y + cellPx `div` 2)))
        drawLine ren (P (V2 (x + 14) (y + 14))) (P (V2 (x + cellPx - 14) (y + cellPx - 14)))
        drawLine ren (P (V2 (x + cellPx - 14) (y + 14))) (P (V2 (x + 14) (y + cellPx - 14)))
        rendererDrawColor ren $= V4 220 240 255 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just (Curtain layers) -> do
        -- Curtain / roller shade (窗帘): vertical fabric stripes; ≠ soft Fog clouds
        rendererDrawColor ren $= V4 160 50 90 200
        fillRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
        rendererDrawColor ren $= V4 200 80 120 220
        forM_ [0 .. 3 :: Int] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + 6)))
                  (V2 5 (cellPx - 14))))
        -- Rod
        rendererDrawColor ren $= V4 220 180 100 255
        fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) 5)))
        rendererDrawColor ren $= V4 255 220 180 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just Steam -> do
        -- Steam cloud (蒸汽): soft gray wisps; blocks match until adjacent clear
        rendererDrawColor ren $= V4 170 180 190 190
        fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
        rendererDrawColor ren $= V4 220 230 240 200
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + 8))) (V2 16 10)))
        fillRect ren (Just (Rectangle (P (V2 (x + 20) (y + 16))) (V2 18 12)))
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 28))) (V2 20 10)))
        rendererDrawColor ren $= V4 140 160 180 255
        drawRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
      Nothing -> pure ()
    case cellKind cell of
      Normal -> pure ()
      LineH -> do
        rendererDrawColor ren $= V4 255 255 255 230
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx `div` 2 - 4))) (V2 (cellPx - 16) 8)))
        rendererDrawColor ren $= V4 255 200 80 255
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx `div` 2 - 1))) (V2 (cellPx - 16) 2)))
      LineV -> do
        rendererDrawColor ren $= V4 255 255 255 230
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 8))) (V2 8 (cellPx - 16))))
        rendererDrawColor ren $= V4 255 200 80 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 1) (y + 8))) (V2 2 (cellPx - 16))))
      Bomb -> do
        rendererDrawColor ren $= V4 20 20 20 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 10) (y + cellPx `div` 2 - 10))) (V2 20 20)))
        rendererDrawColor ren $= V4 255 220 80 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + cellPx `div` 2 - 5))) (V2 10 10)))
        rendererDrawColor ren $= V4 255 80 40 255
        drawRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 12) (y + cellPx `div` 2 - 12))) (V2 24 24)))
      Rainbow -> do
        let cx = x + cellPx `div` 2
            cy = y + cellPx `div` 2
        rendererDrawColor ren $= V4 255 80 80 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 14) (cy - 6))) (V2 10 12)))
        rendererDrawColor ren $= V4 80 220 100 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 4) (cy - 14))) (V2 10 12)))
        rendererDrawColor ren $= V4 80 140 255 255
        fillRect ren (Just (Rectangle (P (V2 (cx + 4) (cy - 6))) (V2 10 12)))
        rendererDrawColor ren $= V4 255 220 60 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 4) (cy + 2))) (V2 10 12)))
        rendererDrawColor ren $= V4 255 255 255 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 4) (cy - 4))) (V2 8 8)))

-- | 原有矩形版棋盘绘制（无贴图时的回退）。
drawStaticPrim :: Renderer -> App -> Board -> CInt -> IO ()
drawStaticPrim ren app board yOff = do
  let sel = appSel app
      hint = gsHint (appGame app)
      flashSet = map fst (appFlash app)
      pulse = appPulse app
  mapM_
    ( \(r, c) -> do
        let pos = (r, c)
            cell = getCell board pos
            (x0, y0) = cellOrigin pos
            y = y0 + yOff
            flashing = pos `elem` flashSet
            -- Soft checkerboard under gems; carpet weave if open / covered target
            carpetOpen = pos `elem` gsCarpetOpen (appGame app)
            carpetCovered =
              pos `elem` levelCarpets (gsLevel (appGame app))
                && not carpetOpen
                && not (null (levelCarpets (gsLevel (appGame app))))
            (br, bg, bb) =
              if carpetOpen
                then (90, 40, 80)  -- uncovered target (magenta base)
                else if carpetCovered
                  then (140, 70, 120)  -- covered weave
                  else if even (r + c) then (36, 36, 48) else (28, 28, 40)
        rendererDrawColor ren $= V4 br bg bb 255
        fillRect ren (Just (Rectangle (P (V2 x0 y)) (V2 cellPx cellPx)))
        -- Carpet weave ticks (目标地砖底纹)
        when (carpetOpen || carpetCovered) $ do
          let tick = if carpetCovered then V4 200 120 180 220 else V4 160 80 140 200
          rendererDrawColor ren $= tick
          fillRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 10))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 22) (y + 10))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 36) (y + 10))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 15) (y + 24))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 29) (y + 24))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 38))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 22) (y + 38))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 36) (y + 38))) (V2 6 6)))
          when carpetCovered $ do
            rendererDrawColor ren $= V4 255 200 230 180
            drawRect ren (Just (Rectangle (P (V2 (x0 + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
        drawGemAt ren x0 y cell flashing
        when (sel == Just pos) $ do
          let bright = fromIntegral (180 + (pulse `mod` 40) * 2) :: Word8
              (sr, sg, sb) = case appTool app of
                ToolHammer -> (255, 160, 80)
                ToolFreeSwap _ -> (100, 180, 255)
                ToolCross -> (220, 80, 220)
                ToolNone -> (255, bright, bright)
          -- Outer glow ring
          rendererDrawColor ren $= V4 sr sg sb 120
          drawRect ren (Just (Rectangle (P (V2 (x0 - 1) (y - 1))) (V2 (cellPx + 2) (cellPx + 2))))
          rendererDrawColor ren $= V4 sr sg sb 255
          drawRect ren (Just (Rectangle (P (V2 (x0 + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
          drawRect ren (Just (Rectangle (P (V2 (x0 + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
        case hint of
          Just (h1, h2)
            | pos == h1 || pos == h2 -> do
                rendererDrawColor ren $= V4 255 255 100 255
                drawRect ren (Just (Rectangle (P (V2 x0 y)) (V2 cellPx cellPx)))
          _ -> pure ()
    )
    [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
  -- Conveyor belt path markers (teal chevrons)
  mapM_ (drawBelt ren yOff) (gsBelts (appGame app))
  -- Portal pair markers (violet rings)
  mapM_ (drawPortal ren yOff) (gsPortals (appGame app))
  -- Vine / chocolate spread preview pulses（只在静止时画，理由同贴图版）
  unless (animBusy app) $ do
    drawVineSpreadHints ren yOff pulse (gsBoard (appGame app))
    drawChocoSpreadHints ren yOff pulse (gsBoard (appGame app))
  -- UFO overlays
  mapM_ (drawUfo ren yOff pulse) (gsUfos (appGame app))

-- | Pulse outline on cells a vine would spread onto next move.
drawVineSpreadHints :: Renderer -> CInt -> Int -> Board -> IO ()
drawVineSpreadHints ren yOff pulse board = do
  let sources =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , hasVine (getCell board (r, c))
        ]
      neigh (r, c) =
        filter
          (\(rr, cc) -> rr >= 0 && rr < boardSize && cc >= 0 && cc < boardSize)
          [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
      targets =
        [ q
        | p <- sources
        , q <- neigh p
        , case getCell board q of
            Gem _ _ _ Nothing -> True
            _ -> False
        ]
      alpha = fromIntegral (100 + (pulse `mod` 30) * 4) :: Word8
  rendererDrawColor ren $= V4 40 200 80 alpha
  mapM_
    ( \pos -> do
        let (x0, y0) = cellOrigin pos
            y = y0 + yOff
        drawRect ren (Just (Rectangle (P (V2 (x0 + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
    )
    targets

-- | Draw flying saucer overlay at its cell (飞碟).
drawChocoSpreadHints :: Renderer -> CInt -> Int -> Board -> IO ()
drawChocoSpreadHints ren yOff pulse board = do
  let sources =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , hasChoco (getCell board (r, c))
        ]
      neigh (r, c) =
        filter
          (\(rr, cc) -> rr >= 0 && rr < boardSize && cc >= 0 && cc < boardSize)
          [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
      targets =
        [ q
        | p <- sources
        , q <- neigh p
        , case getCell board q of
            Gem _ _ _ Nothing -> True
            _ -> False
        ]
      alpha = fromIntegral (100 + (pulse `mod` 30) * 4) :: Word8
  rendererDrawColor ren $= V4 160 90 40 alpha
  mapM_
    ( \pos -> do
        let (x0, y0) = cellOrigin pos
            y = y0 + yOff
        drawRect ren (Just (Rectangle (P (V2 (x0 + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
    )
    targets


drawPortal :: Renderer -> CInt -> (Pos, Pos) -> IO ()
drawPortal ren yOff (a, b) = do
  let mark pos = do
        let (x0, y0) = cellOrigin pos
            y = y0 + yOff
        rendererDrawColor ren $= V4 160 80 220 220
        drawRect ren (Just (Rectangle (P (V2 (x0 + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
        rendererDrawColor ren $= V4 220 160 255 180
        drawRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 8))) (V2 (cellPx - 16) (cellPx - 16))))
  mark a
  mark b

drawUfo :: Renderer -> CInt -> Int -> Ufo -> IO ()
drawUfo ren yOff pulse (Ufo cell col) = do
  let (x0, y0) = cellOrigin cell
      y = y0 + yOff
      (cr, cg, cb) = colorRGB col
      bob = fromIntegral ((pulse `mod` 20) - 10) :: CInt
  -- dome
  rendererDrawColor ren $= V4 220 220 240 230
  fillRect ren (Just (Rectangle (P (V2 (x0 + 14) (y + 10 + bob))) (V2 (cellPx - 28) 12)))
  -- saucer body tinted by target color
  rendererDrawColor ren $= V4 cr cg cb 240
  fillRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 20 + bob))) (V2 (cellPx - 16) 10)))
  rendererDrawColor ren $= V4 255 255 255 200
  fillRect ren (Just (Rectangle (P (V2 (x0 + 18) (y + 22 + bob))) (V2 (cellPx - 36) 4)))
  -- beam hint downward
  rendererDrawColor ren $= V4 cr cg cb 100
  drawLine ren (P (V2 (x0 + cellPx `div` 2) (y + 30 + bob))) (P (V2 (x0 + cellPx `div` 2) (y + cellPx - 6)))

drawBelt :: Renderer -> CInt -> [Pos] -> IO ()
drawBelt _ _ [] = pure ()
drawBelt ren yOff belt = do
  rendererDrawColor ren $= V4 40 200 180 220
  let pairs = zip belt (tail belt ++ [head belt])
  mapM_
    ( \(a, b) -> do
        let (x0, y0) = cellOrigin a
            (x1, y1) = cellOrigin b
            y0' = y0 + yOff
            y1' = y1 + yOff
            cx0 = x0 + cellPx `div` 2
            cy0 = y0' + cellPx `div` 2
            cx1 = x1 + cellPx `div` 2
            cy1 = y1' + cellPx `div` 2
        drawLine ren (P (V2 cx0 cy0)) (P (V2 cx1 cy1))
        -- small chevron near destination
        fillRect ren (Just (Rectangle (P (V2 (cx1 - 3) (cy1 - 3))) (V2 6 6)))
    )
    pairs
