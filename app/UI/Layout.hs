{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 布局常量与几何：逻辑像素尺寸（格 56 px、边距、HUD 高度、窗口大小）、格子坐标换算、
-- 矩形与插值小工具、调色板（宝石颜色 / 叠层蔓延色）、浮字左边界。
--
-- 依赖：只依赖 Match3.Core 与 SDL 类型。所有绘制与点选都以这里的逻辑坐标为准，
-- 物理像素倍率由 UI.Env.syncScale 交给 SDL 缩放。
-- 同步：colorRGB 必须与 tools/gen_assets.py 的调色板一致。
module UI.Layout
  ( cellPx
  , padPx
  , hudH
  , boardPx
  , winW
  , winH
  , colorRGB
  , elementRGBTable
  , namedRGB
  , cellRGB
  , pixelToCell
  , cellOrigin
  , lerpI
  , boardRect
  , allCells
  , smoothT
  , easeOutT
  , lerpC
  , rect
  , cellRect
  , clampI
  , popLeft
  ) where

import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)

-- | 逻辑像素布局：格 56、边距 16、HUD 高 108；窗口 = 棋盘 + 两侧边距 + HUD。
cellPx, padPx, hudH, boardPx, winW, winH :: CInt
cellPx = 56
padPx = 16
hudH = 108
boardPx = cellPx * fromIntegral boardSize
winW = padPx * 2 + boardPx
winH = padPx * 2 + boardPx + hudH

-- | 五色的主色（与 tools/gen_assets.py 调色板一致）。
colorRGB :: Color -> (Word8, Word8, Word8)
colorRGB C1 = (236, 62, 78)   -- 红·圆（与 tools/gen_assets.py 调色板一致）
colorRGB C2 = (52, 196, 96)   -- 绿·方
colorRGB C3 = (56, 128, 246)  -- 蓝·菱
colorRGB C4 = (255, 194, 36)  -- 黄·星
colorRGB C5 = (172, 88, 236)  -- 紫·三角


-- | 名字目标（goalCount (CountNamed …)） / 自定义元素按名字取色；表里没有的名字为灰蓝。
namedRGB :: ElementName -> (Word8, Word8, Word8)
namedRGB n = maybe (200, 200, 220) id (lookup n elementRGBTable)

-- | 按元素名取色：步末效果（事件 evElement / endEffectElement 的键）藤 / 巧 / 蒸汽的蔓延色；
-- 段 5 起也给 名字目标与自定义格取色（果冻 / 气泡，见 namedRGB）。
elementRGBTable :: [(ElementName, (Word8, Word8, Word8))]
elementRGBTable =
  [ ("vine", (110, 220, 90))
  , ("choco", (150, 90, 45))
  , ("steam", (225, 225, 235))
  , ("jelly", (240, 110, 180))
  , ("bubble", (150, 215, 250))
  ]

-- | 格子对应的粒子 / 退回画法颜色。
cellRGB :: Cell -> (Word8, Word8, Word8)
cellRGB cell = case cell of
  Stone _ -> (120, 120, 130)
  Chest _ -> (220, 170, 60)
  Honey _ -> (240, 180, 40)
  Balloon col -> colorRGB col
  Cookie -> (210, 160, 90)
  Cake _ -> (255, 140, 180)
  MagicHat -> (140, 90, 200)
  Maker col _ -> colorRGB col
  Snail _ _ -> (90, 160, 70)
  Safe _ -> (180, 150, 40)
  Flip f _ -> colorRGB f
  Surprise -> (255, 100, 160)
  Bottle col -> colorRGB col
  TimeSpirit -> (80, 220, 255)
  Countdown col _ -> colorRGB col
  Gem col _ _ _ -> colorRGB col
  Custom n _ -> maybe (160, 160, 170) id (lookup n elementRGBTable)

-- | 逻辑坐标 → 棋盘格；棋盘外返回 Nothing。
pixelToCell :: Int32 -> Int32 -> Maybe Pos
pixelToCell mx my =
  let x = fromIntegral mx - padPx
      y = fromIntegral my - padPx - hudH
  in if x < 0 || y < 0 || x >= boardPx || y >= boardPx
       then Nothing
       else
         let c = fromIntegral (x `div` cellPx)
             r = fromIntegral (y `div` cellPx)
         in if inBounds (r, c) then Just (r, c) else Nothing

cellOrigin :: Pos -> (CInt, CInt)
cellOrigin (r, c) =
  ( padPx + fromIntegral c * cellPx
  , padPx + hudH + fromIntegral r * cellPx
  )

-- | 整数坐标线性插值（t ∈ [0,1]）。
lerpI :: CInt -> CInt -> Int -> Int -> CInt
lerpI a b frame maxF
  | maxF <= 0 = b
  | otherwise =
      let t = fromIntegral frame :: Double
          m = fromIntegral maxF :: Double
          u = t / m
      in a + round (fromIntegral (b - a) * u)

-- | 整个棋盘区域的矩形。
boardRect :: Rectangle CInt
boardRect = rect padPx (padPx + hudH) boardPx boardPx

-- | 8×8 全部坐标（行优先）。
allCells :: [Pos]
allCells = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

-- | smoothstep 缓动（两端慢）。
smoothT :: Double -> Double
smoothT x = let y = max 0 (min 1 x) in y * y * (3 - 2 * y)

-- | 先快后慢的缓动。
easeOutT :: Double -> Double
easeOutT x = let y = max 0 (min 1 x) in 1 - (1 - y) * (1 - y)

-- | 颜色 / 数值的线性插值。
lerpC :: CInt -> CInt -> Double -> CInt
lerpC a b e = a + round (fromIntegral (b - a) * e)

-- | SDL 矩形的简写。
rect :: CInt -> CInt -> CInt -> CInt -> Rectangle CInt
rect x y w h = Rectangle (P (V2 x y)) (V2 w h)

-- | 整格矩形（逻辑像素）。
cellRect :: CInt -> CInt -> Rectangle CInt
cellRect x y = rect x y cellPx cellPx

-- | 把整数夹到区间内。
clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

-- | 浮字左边界：以 cx 居中，但整体夹在棋盘左右边框内（放大到峰值时也不出窗口）。
popLeft :: CInt -> CInt -> CInt
popLeft w cx =
  let lo = padPx + 4
      hi = padPx + boardPx - 4 - w
  in if hi < lo then lo else max lo (min hi (cx - w `div` 2))
