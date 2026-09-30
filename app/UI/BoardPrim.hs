{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 棋盘几何降级绘制：没有贴图时用矩形 / 线条画宝石、障碍、叠层、传送门、飞碟、皮带、蔓延预告和粒子。
--
-- 单格绘制 drawGemAt 经 UI.CellTable 分派到 UI.Cell.Prim（按元素拆开的几何渲染器）。
--
-- 依赖：UI.CellTable、Match3.View（地毯 / 地面层 / 关卡级元素读数）、Engine.GridUI（高亮）、UI.Types、UI.Layout、Match3.Core、SDL。
-- 同步：新增棋盘元素时 UI.Cell.Prim 与 UI.Cell.Art 各写一个函数并在 UI.CellTable 登记，保证缺图时仍可玩。
module UI.BoardPrim
  ( drawParticles
  , drawGemAt
  , drawStaticPrim
  , drawVineSpreadHints
  , drawChocoSpreadHints
  , drawPortal
  , drawUfo
  , drawDropMark
  , drawBelt
  ) where

import Control.Monad (unless, when)
import Data.Word (Word8)
import Engine.GridUI (isFlashing, isHinted, isSelected)
import Foreign.C.Types (CInt)
import Match3.Core
import Match3.View (BoardView (..), CarpetMark (..), boardView, carpetAt, groundAtView)
import SDL hiding (Normal)
import UI.CellTable (CellRenderer (..), cellRenderer)
import UI.Ground (drawGroundPrimAt)
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
-- | 几何降级版的单格绘制：按元素名查 UI.CellTable，交给该元素的几何渲染器（UI.Cell.Prim）。
drawGemAt :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
drawGemAt ren x y cell flashing = crPrim (cellRenderer cell) ren x y cell flashing

-- | 原有矩形版棋盘绘制（无贴图时的回退）。
drawStaticPrim :: Renderer -> App -> Board -> CInt -> IO ()
drawStaticPrim ren app board yOff = do
  -- 第 11 刀：高亮读 Engine.GridUI.Highlight，地毯 / 地面层读视图模型 Match3.View.BoardView
  let hl = appHighlight app
      bv = boardView (appGame app)
      pulse = appPulse app
  mapM_
    ( \(r, c) -> do
        let pos = (r, c)
            cell = getCell board pos
            (x0, y0) = cellOrigin pos
            y = y0 + yOff
            flashing = isFlashing hl pos
            -- Soft checkerboard under gems; carpet weave if open / covered target
            carpet = carpetAt bv pos
            carpetOpen = carpet == CarpetOpen
            carpetCovered = carpet == CarpetCovered
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
        -- 地面层（段 5：双层果冻）：几何版画在棋子之上（框），否则会被整格的色块盖住
        mapM_ (drawGroundPrimAt ren x0 y) (groundAtView bv pos)
        when (isSelected hl pos) $ do
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
        when (isHinted hl pos) $ do
          rendererDrawColor ren $= V4 255 255 100 255
          drawRect ren (Just (Rectangle (P (V2 x0 y)) (V2 cellPx cellPx)))
    )
    (boardPositions (bvBoard bv))
  -- Conveyor belt path markers (teal chevrons)
  mapM_ (drawBelt ren yOff) (bvBelts bv)
  -- Portal pair markers (violet rings)
  mapM_ (drawPortal ren yOff) (bvPortals bv)
  -- Vine / chocolate spread preview pulses（只在静止时画，理由同贴图版）
  unless (animBusy app) $ do
    drawVineSpreadHints ren yOff pulse (bvBoard bv)
    drawChocoSpreadHints ren yOff pulse (bvBoard bv)
  -- UFO overlays
  mapM_ (drawUfo ren yOff pulse) (bvUfos bv)
  -- 掉落口（新玩法 6）：格子上沿的漏斗
  mapM_ (drawDropMark ren) (bvDrops bv)

-- | Pulse outline on cells a vine would spread onto next move.
drawVineSpreadHints :: Renderer -> CInt -> Int -> Board -> IO ()
drawVineSpreadHints ren yOff pulse board = do
  let sources = [p | p <- boardPositions board, hasVine (getCell board p)]
      neigh (r, c) =
        filter
          (inBounds board)
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
  let sources = [p | p <- boardPositions board, hasChoco (getCell board p)]
      neigh (r, c) =
        filter
          (inBounds board)
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

-- | 掉落口标记（新玩法 6，几何版）：格子上沿一条金色漏斗（上宽下窄的三级台阶）+ 中间白色向下箭头；固定不随下落偏移。
drawDropMark :: Renderer -> Pos -> IO ()
drawDropMark ren pos = do
  let (x, y0) = cellOrigin pos
      y = y0 - 3
      box bx by bw bh = fillRect ren (Just (Rectangle (P (V2 bx by)) (V2 bw bh)))
  rendererDrawColor ren $= V4 120 70 20 255
  box (x + 4) y (cellPx - 8) 3
  rendererDrawColor ren $= V4 240 190 90 255
  box (x + 6) (y + 3) (cellPx - 12) 4
  box (x + 11) (y + 7) (cellPx - 22) 4
  rendererDrawColor ren $= V4 200 130 50 255
  box (x + 16) (y + 11) (cellPx - 32) 3
  rendererDrawColor ren $= V4 255 250 230 255
  box (x + cellPx `div` 2 - 2) (y + 3) 4 5
  box (x + cellPx `div` 2 - 5) (y + 8) 10 2
  box (x + cellPx `div` 2 - 2) (y + 10) 4 2

drawBelt :: Renderer -> CInt -> [Pos] -> IO ()
drawBelt _ _ [] = pure ()
drawBelt ren yOff belt@(b0 : bs) = do
  rendererDrawColor ren $= V4 40 200 180 220
  let pairs = zip belt (bs ++ [b0])
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
