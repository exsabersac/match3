{-# LANGUAGE OverloadedStrings #-}

-- | 几何降级版的宝石覆盖层（第 10 刀从 UI.Cell.Prim.primOverlay 的大 case 拆出，函数体逐字搬运）：
-- 每种覆盖层一个函数，'primOverlay' 只按构造子分派。带层数的覆盖层（迷雾 / 锁链 / 冰冻 / 窗帘）
-- 共用 'overlayLayerPips' 画底部层数点。
--
-- 依赖：UI.Layout（格子尺寸）、Match3.Core、SDL。UI.Cell.Prim 再导出 primOverlay。
-- 新增一种几何版覆盖层：写一个 @overlayXxx ren x y …@，再在 'primOverlay' 的分派里加一行。
module UI.Cell.PrimOverlay
  ( primOverlay
  , overlayGrass
  , overlayVine
  , overlayChoco
  , overlayFog
  , overlayChain
  , overlayFreeze
  , overlayCurtain
  , overlaySteam
  , overlayLayerPips
  ) where

import Control.Monad (forM_)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.Layout

-- | 几何版：宝石上的覆盖层（草 / 藤 / 巧克力 / 迷雾 / 锁链 / 冰冻 / 窗帘 / 蒸汽）；无覆盖层不画。
primOverlay :: Renderer -> CInt -> CInt -> Maybe CellOverlay -> IO ()
primOverlay ren x y ov = case ov of
  Just Grass -> overlayGrass ren x y
  Just Vine -> overlayVine ren x y
  Just Choco -> overlayChoco ren x y
  Just (Fog layers) -> overlayFog ren x y layers
  Just (Chain layers) -> overlayChain ren x y layers
  Just (Freeze layers) -> overlayFreeze ren x y layers
  Just (Curtain layers) -> overlayCurtain ren x y layers
  Just Steam -> overlaySteam ren x y
  Nothing -> pure ()

-- | 草：底部几丛深绿草叶。
overlayGrass :: Renderer -> CInt -> CInt -> IO ()
overlayGrass ren x y = do
  rendererDrawColor ren $= V4 40 140 50 200
  fillRect ren (Just (Rectangle (P (V2 (x + 6) (y + cellPx - 14))) (V2 (cellPx - 12) 8)))
  fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + cellPx - 20))) (V2 8 8)))
  fillRect ren (Just (Rectangle (P (V2 (x + cellPx - 18) (y + cellPx - 18))) (V2 8 6)))

-- | 藤蔓：深绿框 + 交叉藤条 + 顶部叶片。
overlayVine :: Renderer -> CInt -> CInt -> IO ()
overlayVine ren x y = do
  rendererDrawColor ren $= V4 20 100 40 230
  drawRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
  drawLine ren (P (V2 (x + 8) (y + 8))) (P (V2 (x + cellPx - 10) (y + cellPx - 12)))
  drawLine ren (P (V2 (x + cellPx - 12) (y + 10))) (P (V2 (x + 12) (y + cellPx - 10)))
  fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 6))) (V2 8 8)))

-- | 巧克力：棕色板 + 三道刻槽 + 两处高光（不改六色宝石本体）。
overlayChoco :: Renderer -> CInt -> CInt -> IO ()
overlayChoco ren x y = do
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

-- | 迷雾：柔白云纱 + 层数点。
overlayFog :: Renderer -> CInt -> CInt -> Int -> IO ()
overlayFog ren x y layers = do
  -- Soft white/gray cloud veil (迷雾); layer pips
  rendererDrawColor ren $= V4 200 210 230 200
  fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
  rendererDrawColor ren $= V4 240 245 255 180
  fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 10))) (V2 14 10)))
  fillRect ren (Just (Rectangle (P (V2 (x + 22) (y + 18))) (V2 16 12)))
  fillRect ren (Just (Rectangle (P (V2 (x + 12) (y + 26))) (V2 18 10)))
  rendererDrawColor ren $= V4 120 140 180 255
  overlayLayerPips ren x y layers

-- | 锁链：灰色铁链十字锁 + 层数点。
overlayChain :: Renderer -> CInt -> CInt -> Int -> IO ()
overlayChain ren x y layers = do
  -- Iron chain lock (锁链): gray links over gem
  rendererDrawColor ren $= V4 70 75 90 220
  drawRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
  rendererDrawColor ren $= V4 140 150 170 255
  fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + cellPx `div` 2 - 4))) (V2 (cellPx - 20) 8)))
  fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 10))) (V2 8 (cellPx - 20))))
  rendererDrawColor ren $= V4 200 210 230 255
  fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 6) (y + cellPx `div` 2 - 6))) (V2 12 12)))
  rendererDrawColor ren $= V4 180 190 210 255
  overlayLayerPips ren x y layers

-- | 冰冻（火箭冰冻）：深蓝釉 + 雪花线 + 层数点（区别于青色冰裂纹）。
overlayFreeze :: Renderer -> CInt -> CInt -> Int -> IO ()
overlayFreeze ren x y layers = do
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
  overlayLayerPips ren x y layers

-- | 窗帘：竖条布纹 + 金色卷杆 + 层数点（区别于柔和的迷雾）。
overlayCurtain :: Renderer -> CInt -> CInt -> Int -> IO ()
overlayCurtain ren x y layers = do
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
  overlayLayerPips ren x y layers

-- | 蒸汽：灰色云团 + 外框（相邻消除前挡住匹配）。
overlaySteam :: Renderer -> CInt -> CInt -> IO ()
overlaySteam ren x y = do
  -- Steam cloud (蒸汽): soft gray wisps; blocks match until adjacent clear
  rendererDrawColor ren $= V4 170 180 190 190
  fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
  rendererDrawColor ren $= V4 220 230 240 200
  fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + 8))) (V2 16 10)))
  fillRect ren (Just (Rectangle (P (V2 (x + 20) (y + 16))) (V2 18 12)))
  fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 28))) (V2 20 10)))
  rendererDrawColor ren $= V4 140 160 180 255
  drawRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))

-- | 底部层数点（最多 3 个，颜色由调用方先设好）。
overlayLayerPips :: Renderer -> CInt -> CInt -> Int -> IO ()
overlayLayerPips ren x y layers =
  forM_ [0 .. min 3 layers - 1] $ \i ->
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
            (V2 7 5)))
