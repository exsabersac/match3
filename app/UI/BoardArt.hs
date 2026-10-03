{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 棋盘贴图绘制与分派：宝石 / 障碍贴图、层数角标、棋盘底纹与皮带、飞碟、蔓延预告，
-- 以及「有贴图画贴图、否则退回几何」的分派函数（drawCellAny / drawStatic / drawUfosAny / drawParticlesAny），
-- 和回放 / 步末绘制共用的底盘与格子部件（drawBoardBase / drawCellsExcept / drawCellScaled / waveTint）。
--
-- 单格绘制经 UI.CellTable 按元素名分派到 UI.Cell.Art（贴图）/ UI.Cell.Prim（几何）；colorKey / gemSprite /
-- breathe / 角标 / primarySprite 从那里再导出，原调用方不变。
--
-- 依赖：UI.BoardPrim（降级）、Match3.View（棋盘底层读数）、Engine.GridUI（高亮）、UI.CellTable、UI.Cell.Art、Art、UI.Types、UI.Layout。
module UI.BoardArt
  ( drawBoardBase
  , waveTint
  , drawCellsExcept
  , drawCellScaled
  , drawStatic
  , colorKey
  , gemSprite
  , breathe
  , drawCellAny
  , primarySprite
  , drawCellArt
  , drawLayerBadge
  , drawBadgeAt
  , beltAngles
  , drawBoardBgArt
  , drawStaticArt
  , drawUfosArt
  , drawUfosAny
  , drawDropsArt
  , drawDropsAny
  , spreadTargets
  , drawParticlesAny
  ) where

import Art
import Control.Monad (forM_, unless, void, when)
import Data.Word (Word8)
import Engine.GridUI (Highlight (..), isFlashing)
import Foreign.C.Types (CDouble, CInt)
import Match3.Core
import Match3.View (BoardView (..), CarpetMark (..), boardView, carpetAt, groundAtView)
import SDL hiding (Normal)
import UI.BoardPrim
import UI.Cell.Art (breathe, colorKey, drawBadgeAt, drawLayerBadge, gemSprite)
import UI.CellTable (CellRenderer (..), cellRenderer, primarySprite)
import UI.Ground (drawGroundArtAt)
import UI.Layout
import UI.Presentation (clearTint)
import UI.Types

-- | 棋盘底层（格子 / 地毯 / 地面层 / 传送带 / 传送门），不画棋子。
drawBoardBase :: Renderer -> App -> IO ()
drawBoardBase ren app = case appArt app of
  Just art -> drawBoardBgArt ren art app
  Nothing ->
    forM_ (boardPositions (gsBoard (appGame app))) $ \pos@(r, c) -> do
      let (x, y) = cellOrigin pos
      rendererDrawColor ren $= if even (r + c) then V4 36 36 48 255 else V4 28 28 40 255
      fillRect ren (Just (cellRect x y))

-- | 高亮 / 光圈颜色：查表现表（第 1 轮取 EvClear 行的柔白，连击轮用等级色；见 UI.Presentation.clearTint）。
waveTint :: App -> Int -> V3 Word8
waveTint app k = let (r, g, b) = clearTint k (appPulse app) in V3 r g b

-- | 棋盘底 + 除 hidden 以外的所有格（移动中的格由调用方另画）。
drawCellsExcept :: Renderer -> App -> Board -> [Pos] -> IO ()
drawCellsExcept ren app board hidden = do
  drawBoardBase ren app
  forM_ (boardPositions board) $ \pos ->
    unless (pos `elem` hidden) $ do
      let (x, y) = cellOrigin pos
      drawCellAny ren app x y (getCell board pos) False
  drawUfosAny ren app
  drawDropsAny ren app

-- | 以格子中心 (cx, cy) 按比例 s、透明度 a 画一格（缩放用简化贴图：主贴图 + 特殊标记）。
drawCellScaled :: Renderer -> App -> CInt -> CInt -> Double -> Word8 -> Cell -> IO ()
drawCellScaled ren app cx cy s a cell
  | s <= 0.03 || a == 0 = pure ()
  | otherwise = do
      let sz = max 1 (round (fromIntegral cellPx * s)) :: CInt
          dst = rect (cx - sz `div` 2) (cy - sz `div` 2) sz sz
          white = V3 255 255 255
      case appArt app of
        Just art | hasSprite art (primarySprite cell) -> do
          void (drawSpriteMod ren art (primarySprite cell) dst white a)
          case cell of
            Gem _ LineH _ _ -> void (drawSpriteMod ren art "line_h" dst white a)
            Gem _ LineV _ _ -> void (drawSpriteMod ren art "line_v" dst white a)
            Gem _ Bomb _ _ -> void (drawSpriteMod ren art "bomb_mark" dst white a)
            _ -> pure ()
        _ -> do
          let (r, g, b) = cellRGB cell
          rendererDrawColor ren $= V4 r g b a
          fillRect ren (Just dst)

drawStatic :: Renderer -> App -> Board -> CInt -> IO ()
drawStatic ren app board yOff = case appArt app of
  Just art -> drawStaticArt ren art app board yOff
  Nothing -> drawStaticPrim ren app board yOff

-- | 单格绘制入口：有贴图走精灵，否则走原矩形版。
drawCellAny :: Renderer -> App -> CInt -> CInt -> Cell -> Bool -> IO ()
drawCellAny ren app x y cell flashing = case appArt app of
  Just art -> drawCellArt ren art (appPulse app) x y cell flashing
  Nothing -> drawGemAt ren x y cell flashing

-- | 精灵版单格：按元素名查 UI.CellTable 交给该元素的贴图渲染器（UI.Cell.Art），再统一闪白；
-- 主贴图缺失时逐格退回几何版。
drawCellArt :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> Bool -> IO ()
drawCellArt ren art pulse x y cell flashing
  | not (hasSprite art (primarySprite cell)) = drawGemAt ren x y cell flashing
  | otherwise = do
      crArt (cellRenderer cell) ren art pulse x y cell
      when flashing $
        void (drawSpriteAdd ren art "spark" (rect (x - 10) (y - 10) (cellPx + 20) (cellPx + 20)) (V3 255 255 230) 210)

-- | 传送带每格的朝向角度（右 0 / 下 90 / 左 180 / 上 270）。
beltAngles :: [Pos] -> [(Pos, CDouble)]
beltAngles belt = go Nothing (zip belt (drop 1 belt ++ take 1 belt))
  where
    go _ [] = []
    go prev ((a, b) : rest) =
      let ang = case dirAngle a b of
            Just d -> d
            Nothing -> maybe 0 id prev
      in (a, ang) : go (Just ang) rest
    dirAngle (r1, c1) (r2, c2)
      | r1 == r2 && c2 == c1 + 1 = Just 0
      | r1 == r2 && c2 == c1 - 1 = Just 180
      | c1 == c2 && r2 == r1 + 1 = Just 90
      | c1 == c2 && r2 == r1 - 1 = Just 270
      | otherwise = Nothing

-- | 棋盘底层：圆角框 + 棋盘格 + 地毯 + 地面层（果冻）+ 传送带 + 传送门（都在棋子下面）。
drawBoardBgArt :: Renderer -> Art -> App -> IO ()
drawBoardBgArt ren art app = do
  let bv = boardView (appGame app)  -- 第 11 刀：地毯 / 地面层 / 传送带 / 传送门读视图模型
      (nr, nc) = boardDims (gsBoard (appGame app))
  _ <- drawPanel ren art "panel_dark" (rect (padPx - 8) (hudH + padPx - 8) (boardWPx nc + 16) (boardHPx nr + 16)) 16
  forM_ (allCellsOn nr nc) $ \pos@(r, c) -> do
    let (x, y) = cellOriginOn nr nc pos
        carpet = carpetAt bv pos
        carpetOpen = carpet == CarpetOpen
        carpetCovered = carpet == CarpetCovered
    void (drawSprite ren art (if even (r + c) then "tile_a" else "tile_b") (cellRect x y))
    when carpetCovered $ void (drawSprite ren art "carpet_covered" (cellRect x y))
    when carpetOpen $ void (drawSprite ren art "carpet_open" (cellRect x y))
    -- 地面层（段 5：双层果冻）：棋盘格之上、棋子之下
    mapM_ (drawGroundArtAt ren art x y) (groundAtView bv pos)
  forM_ (bvBelts bv) $ \belt ->
    forM_ (beltAngles belt) $ \(pos, ang) -> do
      let (x, y) = cellOrigin pos
      void (drawSpriteEx ren art "belt" (cellRect x y) ang False)
  forM_ (bvPortals bv) $ \(a, b) ->
    forM_ [a, b] $ \pos -> do
      let (x, y) = cellOrigin pos
          spin = fromIntegral (appPulse app * 3 `mod` 360) :: CDouble
      void (drawSpriteEx ren art "portal" (cellRect x y) spin False)

-- | 精灵版棋盘：底层 → 提示光 → 棋子（下落时带 yOff）→ 选中框 → 蔓延预告 → 飞碟。
drawStaticArt :: Renderer -> Art -> App -> Board -> CInt -> IO ()
drawStaticArt ren art app board yOff = do
  let bv = boardView (appGame app)
      pulse = appPulse app
      hl = appHighlight app  -- 第 11 刀：选中 / 提示 / 闪光 / 固定高亮读 Engine.GridUI.Highlight
      hintCells = hlHint hl
      hintA = round (120 + 135 * breathe pulse 60) :: Int
  drawBoardBgArt ren art app
  forM_ hintCells $ \pos -> do
    let (x, y) = cellOrigin pos
    void (drawSpriteMod ren art "hint_glow" (rect (x - 3) (y - 3) (cellPx + 6) (cellPx + 6)) (V3 255 255 255) (fromIntegral hintA))
  forM_ (boardPositions board) $ \pos -> do
    let (x, y) = cellOrigin pos
    drawCellArt ren art pulse x (y + yOff) (getCell board pos) (isFlashing hl pos)
  -- 提示格再叠一层淡淡的加色光，便于一眼看到
  forM_ hintCells $ \pos -> do
    let (x, y) = cellOrigin pos
    void (drawSpriteAdd ren art "hint_glow" (cellRect x y) (V3 255 230 150) (fromIntegral (hintA `div` 3)))
  forM_ (hlSelected hl) $ \pos -> do
    let (x, y) = cellOrigin pos
        tint = case appTool app of
          ToolHammer -> V3 255 170 80
          ToolFreeSwap _ -> V3 110 190 255
          ToolCross -> V3 235 110 235
          ToolNone -> V3 255 255 255
        grow = round (2 * breathe pulse 30) :: CInt
    void (drawSpriteMod ren art "sel_ring" (rect (x - 2 - grow) (y - 2 - grow) (cellPx + 4 + 2 * grow) (cellPx + 4 + 2 * grow)) tint 255)
  -- 自由交换第一格：保持高亮
  forM_ (hlPinned hl) $ \p -> do
    let (x, y) = cellOrigin p
    void (drawSpriteMod ren art "sel_ring" (cellRect x y) (V3 110 190 255) 220)
  -- 藤蔓 / 巧克力下一步可能蔓延到的格子：绿 / 棕色柔光呼吸。
  -- 只在静止时画：预告基于结算后的盘面，回放 / 步末动画中画出来会和正在长出的格子混淆。
  let spreadA = fromIntegral (round (70 + 110 * breathe pulse 60) :: Int) :: Word8
  unless (animBusy app) $ do
    forM_ (spreadTargets hasVine (bvBoard bv)) $ \pos -> do
      let (x, y) = cellOrigin pos
      void (drawSpriteMod ren art "hint_glow" (cellRect x (y + yOff)) (V3 90 255 120) spreadA)
    forM_ (spreadTargets hasChoco (bvBoard bv)) $ \pos -> do
      let (x, y) = cellOrigin pos
      void (drawSpriteMod ren art "hint_glow" (cellRect x (y + yOff)) (V3 210 120 60) spreadA)
  drawUfosArt ren art app yOff
  drawDropsArt ren art app

-- | 掉落口标记（新玩法 6，贴图版 cookie_drop，缺图时退回几何画法）：画在棋子之上、掉落口格的上沿（固定不随下落偏移）。
drawDropsArt :: Renderer -> Art -> App -> IO ()
drawDropsArt ren art app =
  forM_ (bvDrops (boardView (appGame app))) $ \pos -> do
    let (x, y) = cellOrigin pos
    ok <- drawSprite ren art "cookie_drop" (rect x (y - 6) cellPx cellPx)
    unless ok $ drawDropMark ren pos

drawDropsAny :: Renderer -> App -> IO ()
drawDropsAny ren app = case appArt app of
  Just art -> drawDropsArt ren art app
  Nothing -> mapM_ (drawDropMark ren) (bvDrops (boardView (appGame app)))

-- | 飞碟（贴图版，缺图时退回几何画法）。
drawUfosArt :: Renderer -> Art -> App -> CInt -> IO ()
drawUfosArt ren art app yOff = do
  let pulse = appPulse app
  forM_ (gsUfos (appGame app)) $ \(Ufo pos col) -> do
    let (x, y) = cellOrigin pos
        bob = round (3 * sin (fromIntegral pulse / 10 :: Double)) :: CInt
    ok <- drawSprite ren art ("ufo_" ++ colorKey col) (rect x (y + yOff - 12 + bob) cellPx cellPx)
    unless ok $ drawUfo ren yOff pulse (Ufo pos col)

drawUfosAny :: Renderer -> App -> IO ()
drawUfosAny ren app = case appArt app of
  Just art -> drawUfosArt ren art app 0
  Nothing -> mapM_ (drawUfo ren 0 (appPulse app)) (gsUfos (appGame app))

-- | 与 drawVineSpreadHints 相同的判定：源格正交相邻、且无覆盖层的普通宝石格。
spreadTargets :: (Cell -> Bool) -> Board -> [Pos]
spreadTargets isSource board =
  [ q
  | p <- positionsWhere isSource board
  , q <- neighborsInBounds upDownLeftRight board p
  , case getCell board q of
      Gem _ _ _ Nothing -> True
      _ -> False
  ]

-- | 粒子：有贴图时用柔光圆点（按颜色着色、随寿命淡出）。
drawParticlesAny :: Renderer -> App -> IO ()
drawParticlesAny ren app = case appArt app of
  Nothing -> drawParticles ren (appParticles app)
  Just art ->
    forM_ (appParticles app) $ \p -> do
      let fade = if pMax p <= 0 then 255 else fromIntegral (255 * pLife p `div` pMax p) :: Word8
          s = pSize p * 3
          x = round (pX p) - s `div` 2
          y = round (pY p) - s `div` 2
      void (drawSpriteMod ren art "spark" (rect x y s s) (V3 (pR p) (pG p) (pB p)) fade)
