{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 贴图文字：预烘焙的拉丁字形 / 中文标签贴图的定位、测宽与居中绘制，键帽、进度条等小部件。
--
-- 依赖：Art（图集查找）、UI.Glyph（缺字形时退回像素字）、UI.Layout。
-- 不变量：字形按游戏内实际字号 2x 烘焙，调用方传入的是逻辑字号，不做非整数倍缩放。
module UI.TextArt
  ( textA
  , textW
  , textAC
  , zhW
  , zhA
  , zhAC
  , drawPanelTint
  , keyChipA
  , meterA
  , glyphText
  , glyphW1
  , glyphTextW
  ) where

import Art
import Control.Monad (forM_, unless, void, when)
import Data.Char (ord, toUpper)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL hiding (Normal)
import UI.Glyph
import UI.Layout

--------------------------------------------------------------------------------
-- 贴图字形 / 中文标签 / 面板工具
--------------------------------------------------------------------------------

-- | 贴图字形文字：每字 4*px 宽、6*px 高（与位图字体步幅一致），颜色经 colorMod 着色。
textA :: Renderer -> Art -> CInt -> CInt -> CInt -> V4 Word8 -> String -> IO ()
textA ren art x0 y0 px col@(V4 r g b a) s =
  forM_ (zip [0 :: CInt ..] s) $ \(i, ch0) -> do
    let ch = if ch0 == 'x' then 'x' else toUpper ch0
        x = x0 + i * 4 * px
    unless (ch == ' ') $ do
      ok <- drawSpriteMod ren art ("g_" ++ show (ord ch)) (rect x y0 (4 * px) (6 * px)) (V3 r g b) a
      unless ok $ drawGlyph ren x (y0 + px) px col ch

textW :: CInt -> String -> CInt
textW px s = 4 * px * fromIntegral (length s)

-- | 水平居中文字。
textAC :: Renderer -> Art -> CInt -> CInt -> CInt -> V4 Word8 -> String -> IO ()
textAC ren art cx y px col s = textA ren art (cx - textW px s `div` 2) y px col s

-- | 中文标签贴图宽度（按目标高度 h 等比；用实际选中的尺寸变体计算，保证与绘制一致）。
zhW :: Art -> String -> CInt -> CInt
zhW art key h = maybe 0 (\(w0, h0) -> w0 * h `div` max 1 h0) (spriteSizeAt art key h)

-- | 画中文标签：按游戏内实际高度 h 烘焙了 2x 变体（见 tools/gen_assets.py 的 ZH_SIZES），
-- Retina 上贴图像素与物理像素 1:1；返回宽度。
zhA :: Renderer -> Art -> String -> CInt -> CInt -> CInt -> IO CInt
zhA ren art key x y h = do
  let w = zhW art key h
  when (w > 0) $ void (drawSprite ren art key (rect x y w h))
  pure w

zhAC :: Renderer -> Art -> String -> CInt -> CInt -> CInt -> IO ()
zhAC ren art key cx y h = void (zhA ren art key (cx - zhW art key h `div` 2) y h)

-- | 着色九宫格（进度条填充）。
drawPanelTint :: Renderer -> Art -> String -> Rectangle CInt -> CInt -> V3 Word8 -> IO ()
drawPanelTint ren art name r c tint = void (drawPanelMod ren art name r c tint)

-- | 按键小方块。
keyChipA :: Renderer -> Art -> CInt -> CInt -> Char -> V4 Word8 -> IO ()
keyChipA ren art x y ch col = do
  _ <- drawPanel ren art "panel_chip" (rect x y 22 22) 7
  textA ren art (x + 5) (y + 2) 3 col [ch]

-- | 进度条：底槽 + 着色填充 + 右对齐数字。
meterA :: Renderer -> Art -> CInt -> CInt -> CInt -> Int -> Int -> V3 Word8 -> String -> IO ()
meterA ren art x y w value cap tint label = do
  _ <- drawPanel ren art "panel_bar" (rect x y w 20) 9
  let inner = w - 4
      fw
        | cap <= 0 = 0
        | otherwise = min inner (max 0 (fromIntegral value * inner `div` fromIntegral (max 1 cap)))
  when (fw > 0) $ drawPanelTint ren art "panel_fill" (rect (x + 2) (y + 2) (max 16 fw) 16) 8 tint
  textA ren art (x + w - 8 - textW 3 label) (y + 1) 3 (V4 255 255 255 255) label

-- | 任意字高的描边字形（贴图 g_<码点> 及其 @变体）；每字宽 = 高 × 2/3，与 textA 一致。
-- 缺字形时退回位图字。
glyphText :: Renderer -> Art -> CInt -> CInt -> CInt -> V4 Word8 -> String -> IO ()
glyphText ren art x0 y0 h col@(V4 r g b a) s =
  forM_ (zip [0 :: CInt ..] s) $ \(i, ch0) -> do
    let ch = if ch0 == 'x' then 'x' else toUpper ch0
        w = glyphW1 h
        x = x0 + i * w
    unless (ch == ' ') $ do
      ok <- drawSpriteMod ren art ("g_" ++ show (ord ch)) (rect x y0 w h) (V3 r g b) a
      unless ok $ drawGlyph ren x (y0 + h `div` 6) (max 1 (h `div` 6)) col (toUpper ch)

glyphW1 :: CInt -> CInt
glyphW1 h = h * 2 `div` 3

glyphTextW :: CInt -> String -> CInt
glyphTextW h s = glyphW1 h * fromIntegral (length s)
