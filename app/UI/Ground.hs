{-# LANGUAGE OverloadedStrings #-}
-- | 地面层（GameState.gsGround，段 2c 的扩展槽；段 5 起内置有双层果冻）的绘制查表：
-- 键 = 地面层元素名，值 = 几何降级版与贴图版两个画法（参数：格左上角、剩余层数）。
-- 贴图版画在棋盘格之上、棋子之下；几何版画在棋子之上（框）。表里没有的名字画一道淡灰框。
-- 贴图版主贴图缺失时逐格退回几何版。某格的地面层第 11 刀起由视图模型 Match3.View.groundAtView 取。
--
-- 新增地面层元素：在这里加一行（贴图由 tools/gen_assets.py 生成，名字 = 元素名 / 元素名_层数）。
--
-- 依赖：Art、UI.Layout、Match3.Core、SDL。
module UI.Ground
  ( groundTable
  , drawGroundPrimAt
  , drawGroundArtAt
  ) where

import Art
import Control.Monad (void, when)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.Layout

-- | 名字 → (几何版, 贴图名(层数))。
groundTable :: [(ElementName, (Renderer -> CInt -> CInt -> Int -> IO (), Int -> String))]
groundTable =
  [ ("jelly", (primJelly, \n -> if n >= 2 then "jelly_2" else "jelly"))
  , ("magic", (primMagic, const "magic"))
  ]

-- | 几何版：按名字查表，查不到画淡灰底。
drawGroundPrimAt :: Renderer -> CInt -> CInt -> (ElementName, Int) -> IO ()
drawGroundPrimAt ren x y (name, n) = case lookup name groundTable of
  Just (prim, _) -> prim ren x y n
  Nothing -> do
    rendererDrawColor ren $= V4 170 170 180 200
    drawRect ren (Just (Rectangle (P (V2 (x + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))

-- | 贴图版：主贴图缺失时退回几何版。
drawGroundArtAt :: Renderer -> Art -> CInt -> CInt -> (ElementName, Int) -> IO ()
drawGroundArtAt ren art x y g@(name, n) = case lookup name groundTable of
  Just (_, sprite) | hasSprite art (sprite n) -> void (drawSprite ren art (sprite n) (cellRect x y))
  _ -> drawGroundPrimAt ren x y g

-- | 几何版果冻：几何版棋子几乎占满整格，所以果冻画成**棋子之上**的粉色框（调用方在棋子之后画）：
-- 单层一道细框，双层粗框 + 一道内框（层线）。
primJelly :: Renderer -> CInt -> CInt -> Int -> IO ()
primJelly ren x y n = do
  let box i = Rectangle (P (V2 (x + i) (y + i))) (V2 (cellPx - 2 * i) (cellPx - 2 * i))
  rendererDrawColor ren $= if n >= 2 then V4 240 80 165 255 else V4 255 160 210 235
  mapM_ (drawRect ren . Just . box) (if n >= 2 then [1, 2, 3, 4] else [2, 3])
  when (n >= 2) $ do
    rendererDrawColor ren $= V4 255 215 238 235
    drawRect ren (Just (box 9))

-- | 几何版魔法地格（新玩法 8）：棋子之上的紫色双线框 + 四角小方点（永久，不随层数变化）。
primMagic :: Renderer -> CInt -> CInt -> Int -> IO ()
primMagic ren x y _ = do
  let box i = Rectangle (P (V2 (x + i) (y + i))) (V2 (cellPx - 2 * i) (cellPx - 2 * i))
      corner dx dy = fillRect ren (Just (Rectangle (P (V2 (x + dx) (y + dy))) (V2 5 5)))
  rendererDrawColor ren $= V4 170 90 255 245
  mapM_ (drawRect ren . Just . box) [1, 2]
  rendererDrawColor ren $= V4 225 190 255 235
  drawRect ren (Just (box 5))
  mapM_ (uncurry corner) [(4, 4), (cellPx - 9, 4), (4, cellPx - 9), (cellPx - 9, cellPx - 9)]
