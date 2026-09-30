{-# LANGUAGE OverloadedStrings #-}

-- | 几何降级版的单格绘制（第三刀从 UI.BoardPrim.drawGemAt 的约 540 行大 case 拆出，函数体逐字搬运）：
-- 每种元素一个函数，签名相同（渲染器、格左上角、格子内容、是否闪白），由 UI.CellTable 按元素名查表分派。
-- 函数只认自己的构造子，其它格子什么也不画。
--
-- 依赖：UI.Layout（格子尺寸 / 颜色）、UI.Cell.PrimOverlay（覆盖层，本模块再导出 primOverlay）、Match3.Core、SDL。没有贴图时使用，保证缺图时仍可玩。
module UI.Cell.Prim
  ( primCustom
  , primBubble
  , primMagicStone
  , primFuzzball
  , primStone
  , primChest
  , primHoney
  , primBalloon
  , primCookie
  , primCake
  , primMagicHat
  , primMaker
  , primSnail
  , primSafe
  , primFlip
  , primSurprise
  , primBottle
  , primTimeSpirit
  , primCountdown
  , primGem
  , primIce
  , primOverlay -- 再导出自 UI.Cell.PrimOverlay
  , primMark
  ) where

import Control.Monad (forM_, when)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.Cell.PrimOverlay (primOverlay)
import UI.Layout

-- | 几何版：自定义元素。
primCustom :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primCustom ren x y cell flashing = case cell of
  Custom _ _ -> do
    -- 自定义元素（元素框架开放槽，内置关卡不出现）：灰色方块
    let gap = 4 :: CInt
        v = if flashing then 230 else 150
    rendererDrawColor ren $= V4 v v (v + 10) 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
  _ -> pure ()

-- | 几何版：气泡（段 5）——浅蓝方块 + 亮边 + 左上高光（无颜色徽记）。
primBubble :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primBubble ren x y cell flashing = case cell of
  Custom _ _ -> do
    let gap = 6 :: CInt
        body = Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))
    rendererDrawColor ren $= if flashing then V4 235 250 255 230 else V4 120 190 240 130
    fillRect ren (Just body)
    rendererDrawColor ren $= V4 200 240 255 255
    drawRect ren (Just body)
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 6) (y + gap + 6))) (V2 10 6)))
  _ -> pure ()

-- | 几何版：魔法石（新玩法 2）——紫色方块 + 底部 3 个充能格（点亮的金色）；满格时亮紫边。
primMagicStone :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primMagicStone ren x y cell flashing = case cell of
  Custom _ (CustomState k) -> do
    let gap = 5 :: CInt
        body = Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))
    rendererDrawColor ren $= if flashing then V4 230 210 255 255 else V4 92 60 160 255
    fillRect ren (Just body)
    rendererDrawColor ren $= if k >= 3 then V4 255 140 255 255 else V4 40 24 80 255
    drawRect ren (Just body)
    forM_ [0 .. 2 :: Int] $ \i -> do
      rendererDrawColor ren $= if i < k then V4 255 200 60 255 else V4 40 26 70 255
      fillRect ren (Just (Rectangle (P (V2 (x + 14 + fromIntegral i * 11) (y + cellPx - 18))) (V2 7 7)))
  _ -> pure ()

-- | 几何版：毛球（新玩法 3）——灰粉色毛团（大方块 + 四角小方块当绒毛）+ 两只白眼黑瞳。
primFuzzball :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primFuzzball ren x y cell flashing = case cell of
  Custom _ _ -> do
    let gap = 9 :: CInt
        s = cellPx - 2 * gap
    rendererDrawColor ren $= if flashing then V4 255 230 240 255 else V4 196 150 170 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 s s)))
    forM_ [(x + gap - 4, y + gap - 4), (x + cellPx - gap - 4, y + gap - 4), (x + gap - 4, y + cellPx - gap - 4), (x + cellPx - gap - 4, y + cellPx - gap - 4)] $ \(fx, fy) ->
      fillRect ren (Just (Rectangle (P (V2 fx fy)) (V2 8 8)))
    forM_ [x + cellPx `div` 2 - 12, x + cellPx `div` 2 + 3] $ \eyeX -> do
      rendererDrawColor ren $= V4 255 255 255 255
      fillRect ren (Just (Rectangle (P (V2 eyeX (y + cellPx `div` 2 - 8))) (V2 9 10)))
      rendererDrawColor ren $= V4 30 20 30 255
      fillRect ren (Just (Rectangle (P (V2 (eyeX + 3) (y + cellPx `div` 2 - 4))) (V2 4 5)))
  _ -> pure ()

-- | 几何版：石头。
primStone :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primStone ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：宝箱。
primChest :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primChest ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：蜂蜜罐。
primHoney :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primHoney ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：气球。
primBalloon :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primBalloon ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：饼干。
primCookie :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primCookie ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：蛋糕。
primCake :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primCake ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：魔法帽。
primMagicHat :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primMagicHat ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：果汁机。
primMaker :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primMaker ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：蜗牛。
primSnail :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primSnail ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：保险箱。
primSafe :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primSafe ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：双面块。
primFlip :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primFlip ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：彩蛋。
primSurprise :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primSurprise ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：染色瓶。
primBottle :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primBottle ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：时间精灵。
primTimeSpirit :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primTimeSpirit ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：倒计时炸弹。
primCountdown :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primCountdown ren x y cell flashing = case cell of
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
  _ -> pure ()

-- | 几何版：宝石（含特殊块 / 冰 / 覆盖层）。底色块 → 冰 → 覆盖层 → 特殊块标记。
primGem :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
primGem ren x y cell flashing = case cell of
  Gem col kind ice ov -> do
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
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
    primIce ren x y ice
    primOverlay ren x y ov
    primMark ren x y kind
  _ -> pure ()

-- | 几何版：冰层（青色框 + 裂纹，两层以上加内框）。
primIce :: Renderer -> CInt -> CInt -> Int -> IO ()
primIce ren x y ice = do
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

-- | 几何版：特殊块标记（横 / 竖直线、炸弹、彩虹）。
primMark :: Renderer -> CInt -> CInt -> GemKind -> IO ()
primMark ren x y kind = do
  case kind of
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
