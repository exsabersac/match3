{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 几何降级用的像素字形：缺贴图时用矩形拼出数字与少量英文大写字母。
--
-- 依赖：SDL。只在 appArt = Nothing 或缺少对应文字贴图时使用。
module UI.Glyph
  ( digitGlyph
  , drawDigit
  , drawNumber
  , drawBannerWord
  , drawGlyph
  ) where

import Control.Monad (forM_, when)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL hiding (Normal)

--------------------------------------------------------------------------------
-- Bitmap 3x5 digits (no TTF)
--------------------------------------------------------------------------------

digitGlyph :: Int -> [[Word8]]
digitGlyph d = case d of
  0 -> [[1,1,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]
  1 -> [[0,1,0],[1,1,0],[0,1,0],[0,1,0],[1,1,1]]
  2 -> [[1,1,1],[0,0,1],[1,1,1],[1,0,0],[1,1,1]]
  3 -> [[1,1,1],[0,0,1],[1,1,1],[0,0,1],[1,1,1]]
  4 -> [[1,0,1],[1,0,1],[1,1,1],[0,0,1],[0,0,1]]
  5 -> [[1,1,1],[1,0,0],[1,1,1],[0,0,1],[1,1,1]]
  6 -> [[1,1,1],[1,0,0],[1,1,1],[1,0,1],[1,1,1]]
  7 -> [[1,1,1],[0,0,1],[0,0,1],[0,0,1],[0,0,1]]
  8 -> [[1,1,1],[1,0,1],[1,1,1],[1,0,1],[1,1,1]]
  9 -> [[1,1,1],[1,0,1],[1,1,1],[0,0,1],[1,1,1]]
  _ -> digitGlyph 0

drawDigit :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> Int -> IO ()
drawDigit ren x0 y0 px col d = do
  rendererDrawColor ren $= col
  let g = digitGlyph (abs d `mod` 10)
  forM_ (zip [0 :: CInt ..] g) $ \(ry, row) ->
    forM_ (zip [0 :: CInt ..] row) $ \(cx, bit) ->
      when (bit == 1) $
        fillRect
          ren
          (Just (Rectangle (P (V2 (x0 + cx * px) (y0 + ry * px))) (V2 px px)))

drawNumber :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> Int -> IO ()
drawNumber ren x0 y0 px col n =
  let s = show (max 0 n)
      step = 3 * px + px
  in forM_ (zip [0 :: CInt ..] s) $ \(i, ch) ->
       drawDigit ren (x0 + i * step) y0 px col (fromEnum ch - fromEnum '0')

-- | Tiny letter blocks for CLEAR / WIN / LOSE / NEXT / RETRY banners.
-- Only a few needed glyphs (pure rect approximations).
drawBannerWord :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> String -> IO ()
drawBannerWord ren x0 y0 px col word = do
  rendererDrawColor ren $= col
  let gap = 4 * px + px
  forM_ (zip [0 :: CInt ..] word) $ \(i, ch) ->
    drawGlyph ren (x0 + i * gap) y0 px col ch

drawGlyph :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> Char -> IO ()
drawGlyph ren x y px col ch = do
  rendererDrawColor ren $= col
  let spit :: [[Int]] -> IO ()
      spit bits =
        forM_ (zip [0 :: CInt ..] bits) $ \(ry, row) ->
          forM_ (zip [0 :: CInt ..] row) $ \(cx, bit) ->
            when (bit == 1) $
              fillRect
                ren
                (Just (Rectangle (P (V2 (x + cx * px) (y + ry * px))) (V2 px px)))
  spit $ case ch of
    'C' -> [[1,1,1],[1,0,0],[1,0,0],[1,0,0],[1,1,1]]
    'L' -> [[1,0,0],[1,0,0],[1,0,0],[1,0,0],[1,1,1]]
    'E' -> [[1,1,1],[1,0,0],[1,1,0],[1,0,0],[1,1,1]]
    'A' -> [[0,1,0],[1,0,1],[1,1,1],[1,0,1],[1,0,1]]
    'R' -> [[1,1,0],[1,0,1],[1,1,0],[1,0,1],[1,0,1]]
    'W' -> [[1,0,1],[1,0,1],[1,0,1],[1,1,1],[1,0,1]]
    'I' -> [[1,1,1],[0,1,0],[0,1,0],[0,1,0],[1,1,1]]
    'N' -> [[1,0,1],[1,1,1],[1,1,1],[1,0,1],[1,0,1]]
    'O' -> [[1,1,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]
    'S' -> [[1,1,1],[1,0,0],[1,1,1],[0,0,1],[1,1,1]]
    'X' -> [[1,0,1],[1,0,1],[0,1,0],[1,0,1],[1,0,1]]
    'T' -> [[1,1,1],[0,1,0],[0,1,0],[0,1,0],[0,1,0]]
    'Y' -> [[1,0,1],[1,0,1],[0,1,0],[0,1,0],[0,1,0]]
    'P' -> [[1,1,0],[1,0,1],[1,1,0],[1,0,0],[1,0,0]]
    'H' -> [[1,0,1],[1,0,1],[1,1,1],[1,0,1],[1,0,1]]
    'U' -> [[1,0,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]
    'G' -> [[1,1,1],[1,0,0],[1,0,1],[1,0,1],[1,1,1]]
    'M' -> [[1,0,1],[1,1,1],[1,1,1],[1,0,1],[1,0,1]]
    'K' -> [[1,0,1],[1,0,1],[1,1,0],[1,0,1],[1,0,1]]
    'B' -> [[1,1,0],[1,0,1],[1,1,0],[1,0,1],[1,1,0]]
    'D' -> [[1,1,0],[1,0,1],[1,0,1],[1,0,1],[1,1,0]]
    'F' -> [[1,1,1],[1,0,0],[1,1,0],[1,0,0],[1,0,0]]
    'Q' -> [[1,1,1],[1,0,1],[1,0,1],[1,1,1],[0,0,1]]
    '!' -> [[0,1,0],[0,1,0],[0,1,0],[0,0,0],[0,1,0]]
    ' ' -> [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0]]
    d | d >= '0' && d <= '9' ->
      map (map fromIntegral) (digitGlyph (fromEnum d - fromEnum '0'))
    _   -> [[1,1,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]
