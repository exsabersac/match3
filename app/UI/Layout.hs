{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 布局常量与几何：逻辑像素尺寸（格 56 px、边距、HUD 高度、窗口大小）、格子坐标换算、
-- 矩形与插值小工具、调色板（宝石颜色 / 叠层蔓延色）、浮字左边界。
--
-- 依赖：只依赖 Match3.Core、Engine.GridUI（第 11 刀：像素 ↔ 格经通用网格几何 boardGrid）与 SDL 类型。所有绘制与点选都以这里的逻辑坐标为准，
-- 物理像素倍率由 UI.Env.syncScale 交给 SDL 缩放。
-- 同步：colorRGB 必须与 tools/gen_assets.py 的调色板一致。
module UI.Layout
  ( cellPx
  , padPx
  , hudH
  , boardPx
  , boardWPx
  , boardHPx
  , winW
  , winH
  , colorRGB
  , elementRGBTable -- 再导出自 UI.Presentation（第 10 刀起元素色在表现表模块）
  , namedRGB
  , cellRGB
  , boardGrid
  , boardGridFor
  , pixelToCell
  , pixelToCellOn
  , cellOrigin
  , cellOriginOn
  , lerpI
  , boardRect
  , boardRectOn
  , allCells
  , allCellsOn
  , smoothT  -- 再导出自 UI.Presentation
  , easeOutT
  , lerpC
  , rect
  , cellRect
  , clampI
  , popLeft
  , sfxChipRect
  , bgmChipRect
  , hitChip
  ) where

import Data.Int (Int32)
import Data.Word (Word8)
import Engine.GridUI (GridGeom (..), PxX (..), PxY (..), gridCellAt, gridCellOrigin, gridCells, pxXY)
import Foreign.C.Types (CInt)
import Match3.Core
import Match3.Element.Builtin (chameleonColor)
import SDL hiding (Normal)
import UI.Presentation (easeOutT, elementRGBTable, smoothT)

-- | 逻辑像素布局：格 56、边距 16、HUD 高 108；窗口按允许的最大盘面（maxBoardDim）留足空间，不假设正方形棋盘。
cellPx, padPx, hudH, boardPx, winW, winH :: CInt
cellPx = 56
padPx = 16
hudH = 108
-- | 最大边长像素（窗口 / 居中参照）；实际棋盘宽高用 boardWPx / boardHPx。
boardPx = cellPx * fromIntegral maxBoardDim
winW = padPx * 2 + boardPx
winH = padPx * 2 + boardPx + hudH

-- | 实际棋盘宽 / 高（列数 / 行数 × 格宽）；可矩形。
boardWPx, boardHPx :: Int -> CInt
boardWPx cols = cellPx * fromIntegral cols
boardHPx rows = cellPx * fromIntegral rows

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
  Custom n _
    | Just col <- chameleonColor cell -> colorRGB col -- 变色龙（新玩法 7）：当前颜色
    | otherwise -> maybe (160, 160, 170) id (lookup n elementRGBTable)

-- | 按行列数的棋盘网格几何（可不正方形）。
boardGridFor :: Int -> Int -> GridGeom CInt
boardGridFor rows cols = GridGeom {ggLeft = padPx, ggTop = padPx + hudH, ggCell = cellPx, ggRows = rows, ggCols = cols}

-- | 缺省 8×8 网格（测试 / 未指定关卡尺寸时）；对局绘制与点选请用 boardGridFor / *On。
boardGrid :: GridGeom CInt
boardGrid = boardGridFor boardSize boardSize

-- | 逻辑坐标 → 棋盘格（缺省 8×8）；棋盘外返回 Nothing。
pixelToCell :: Int32 -> Int32 -> Maybe Pos
pixelToCell mx my = gridCellAt boardGrid (PxX (fromIntegral mx)) (PxY (fromIntegral my))

-- | 按实际行列点选。
pixelToCellOn :: Int -> Int -> Int32 -> Int32 -> Maybe Pos
pixelToCellOn rows cols mx my = gridCellAt (boardGridFor rows cols) (PxX (fromIntegral mx)) (PxY (fromIntegral my))

-- | 格子 → 左上角 (x, y)。第 7 项起 Engine.GridUI 回的是 (PxX, PxY)，在这里拆成 SDL 用的二元组。
cellOrigin :: Pos -> (CInt, CInt)
cellOrigin = pxXY . gridCellOrigin boardGrid

cellOriginOn :: Int -> Int -> Pos -> (CInt, CInt)
cellOriginOn rows cols = pxXY . gridCellOrigin (boardGridFor rows cols)

-- | 整数坐标线性插值（t ∈ [0,1]）。
lerpI :: CInt -> CInt -> Int -> Int -> CInt
lerpI a b frame maxF
  | maxF <= 0 = b
  | otherwise =
      let t = fromIntegral frame :: Double
          m = fromIntegral maxF :: Double
          u = t / m
      in a + round (fromIntegral (b - a) * u)

-- | 整个棋盘区域的矩形（缺省按最大边长的正方形窗口区；对局绘制用 boardRectOn）。
boardRect :: Rectangle CInt
boardRect = rect padPx (padPx + hudH) boardPx boardPx

-- | 按实际行列的棋盘矩形（可矩形）。
boardRectOn :: Int -> Int -> Rectangle CInt
boardRectOn rows cols = rect padPx (padPx + hudH) (boardWPx cols) (boardHPx rows)

-- | 缺省 8×8 全部坐标（行优先）。
allCells :: [Pos]
allCells = gridCells boardGrid

-- | 按实际行列的全部坐标。
allCellsOn :: Int -> Int -> [Pos]
allCellsOn rows cols = gridCells (boardGridFor rows cols)

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

-- | HUD 音效 / BGM 开关芯片（逻辑像素；两枚并排贴在 HUD 右下，不挡棋盘格）。
sfxChipRect, bgmChipRect :: (CInt, CInt, CInt, CInt)
sfxChipRect = (374, 100, 46, 28)
bgmChipRect = (424, 100, 46, 28)

-- | 点是否落在芯片矩形内（鼠标逻辑坐标）。
hitChip :: (CInt, CInt, CInt, CInt) -> Int32 -> Int32 -> Bool
hitChip (x, y, w, h) mx my =
  mx >= fromIntegral x && mx < fromIntegral (x + w)
    && my >= fromIntegral y && my < fromIntegral (y + h)
