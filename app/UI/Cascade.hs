{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 棋盘动画绘制：静止盘、交换补间、轻落，以及逐轮回放的高亮 → 消失 → 下落补子（和步末阶段的分派），
-- 震屏视口偏移 withShake。
--
-- 依赖：ComboFx（阶段机与下落映射）、UI.Presentation（光圈贴图）、UI.EndStage、UI.BoardArt、UI.BoardPrim、UI.Types、UI.Layout。
module UI.Cascade
  ( drawBoard
  , withShake
  , drawCascade
  , drawCascadeWave
  , drawWaveFlash
  , drawWavePop
  , drawWaveFall
  , drawSwap
  , drawFall
  ) where

import Art
import ComboFx
import Control.Monad (forM_, unless, void)
import Data.Word (Word8)
import Engine.Playback (Player (..))
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.BoardArt
import UI.BoardPrim
import UI.EndStage
import UI.Layout
import UI.Presentation (clearSprite)
import UI.Types

-- | 按当前动画分派棋盘绘制：交换 / 轻落 / 逐轮回放 / 静止。
drawBoard :: Renderer -> App -> IO ()
drawBoard ren app = case appAnim app of
  AnimSwap SwapAnim { asP1, asP2, asBefore, asFrame } ->
    drawSwap ren app asBefore asP1 asP2 asFrame
  AnimFall FallAnim { afBoard, afFrame } ->
    drawFall ren app afBoard afFrame
  AnimCascade p ->
    drawCascade ren app p
  AnimNone ->
    drawStatic ren app (gsBoard (appGame app)) 0

--------------------------------------------------------------------------------
-- 逐轮连锁回放绘制（阶段机见 ComboFx）
--------------------------------------------------------------------------------

-- | 震屏：把整个棋盘层（棋盘 / 粒子 / 弹字）的视口平移几像素，振幅线性衰减。HUD 不动。
withShake :: Renderer -> App -> IO () -> IO ()
withShake ren app act
  | appShake app <= 0 || appShakeAmp app <= 0 || appPaused app = act
  | otherwise = do
      let k = fromIntegral (appShake app) / fromIntegral shakeFrames :: Double
          amp = fromIntegral (appShakeAmp app) * k
          ph = fromIntegral (appPulse app) :: Double
          dx = round (amp * sin (ph * 2.3)) :: CInt
          dy = round (amp * cos (ph * 3.1)) :: CInt
      rendererViewport ren $= Just (rect dx dy winW winH)
      act
      rendererViewport ren $= Nothing

-- | 回放中：步末阶段交给 UI.EndStage，其余按轮绘制。
drawCascade :: Renderer -> App -> CascadePlayer -> IO ()
drawCascade ren app p = case (cPhase c, cStages c) of
  (PhEnd, st : _) -> drawEndStage ren app (phaseT p) st
  _ -> drawCascadeWave ren app p
  where
    c = plStage p

-- | 按当前轮的阶段（高亮 / 消失 / 下落 / 落定）绘制。
drawCascadeWave :: Renderer -> App -> CascadePlayer -> IO ()
drawCascadeWave ren app p = case cWaves c of
  [] -> drawStatic ren app (cShown c) 0
  (v : _) -> case cPhase c of
    PhStart -> drawStatic ren app (cwBefore (wvWave v)) 0
    PhFlash -> drawWaveFlash ren app p v
    PhPop -> drawWavePop ren app p v
    PhFall -> drawWaveFall ren app p v
    PhRest -> drawStatic ren app (cwAfter (wvWave v)) 0
    PhEnd -> drawStatic ren app (cShown c) 0
  where
    c = plStage p

-- | 高亮：整盘压暗，被消格提到暗幕之上，闪两下 + 轻微弹跳 + 等级色光圈。
drawWaveFlash :: Renderer -> App -> CascadePlayer -> WaveView -> IO ()
drawWaveFlash ren app p v = do
  let t = phaseT p
      w = wvWave v
      (nr, nc) = boardDims (cwBefore w)
      tint@(V3 tr tg tb) = waveTint app (cCombo (plStage p))
      veilA = round (min 1 (t * 4) * 120 :: Double) :: Word8
      blink = 0.5 + 0.5 * cos (t * 2 * pi * 2) :: Double
      bounce = round (3 * sin (t * pi) :: Double) :: CInt
  drawStatic ren app (cwBefore w) 0
  rendererDrawColor ren $= V4 8 6 24 veilA
  fillRect ren (Just (boardRectOn nr nc))
  forM_ (wvCleared v) $ \pos -> do
    let (x, y0) = cellOrigin pos
        y = y0 - bounce
        cell = getCell (cwBefore w) pos
    case appArt app of
      Just art -> do
        void (drawSpriteAdd ren art clearSprite (rect (x - 12) (y - 12) (cellPx + 24) (cellPx + 24)) tint (round (90 + 120 * blink)))
        drawCellArt ren art (appPulse app) x y cell False
        void (drawSpriteAdd ren art clearSprite (cellRect x y) (V3 255 255 255) (round (80 * blink)))
        void (drawSpriteMod ren art "sel_ring" (rect (x - 2) (y - 2) (cellPx + 4) (cellPx + 4)) tint 235)
      Nothing -> do
        rendererDrawColor ren $= V4 60 54 84 255
        fillRect ren (Just (cellRect x y))
        drawGemAt ren x y cell (blink > 0.5)
        rendererDrawColor ren $= V4 tr tg tb 255
        drawRect ren (Just (cellRect x y))
        drawRect ren (Just (rect (x + 1) (y + 1) (cellPx - 2) (cellPx - 2)))

-- | 消失：被消格缩小淡出 + 光环外扩；本轮生成的特殊块从中心放大出现；其它格保持不动。
drawWavePop :: Renderer -> App -> CascadePlayer -> WaveView -> IO ()
drawWavePop ren app p v = do
  let t = phaseT p
      w = wvWave v
      (nr, nc) = boardDims (cwBefore w)
      tint = waveTint app (cCombo (plStage p))
      cleared = wvCleared v
      veilA = round ((1 - t) * 120) :: Word8
  drawBoardBase ren app
  forM_ (boardPositions (cwBefore w)) $ \pos -> do
    let (x, y) = cellOrigin pos
    case holeAt w pos of
      Just cell | pos `notElem` cleared -> drawCellAny ren app x y cell False
      _ -> pure ()
  rendererDrawColor ren $= V4 8 6 24 veilA
  fillRect ren (Just (boardRectOn nr nc))
  forM_ cleared $ \pos -> do
    let (x, y) = cellOrigin pos
        cx = x + cellPx `div` 2
        cy = y + cellPx `div` 2
        s = 1.1 * (1 - t) * (1 - t) + 0.02
        a = round (255 * (1 - t)) :: Word8
        ring = round (fromIntegral cellPx * (1 + 0.9 * t)) :: CInt
    forM_ (appArt app) $ \art ->
      void (drawSpriteAdd ren art clearSprite (rect (cx - ring `div` 2) (cy - ring `div` 2) ring ring) tint a)
    drawCellScaled ren app cx cy s a (getCell (cwBefore w) pos)
    case holeAt w pos of
      Just cell -> drawCellScaled ren app cx cy (max 0.05 t) 255 cell
      Nothing -> pure ()

-- | 下落 + 补子：按列复现重力（ComboFx.fallTable），加速度下落；新格从棋盘上沿外落入（裁剪）。
drawWaveFall :: Renderer -> App -> CascadePlayer -> WaveView -> IO ()
drawWaveFall ren app p v = do
  let t = phaseT p
      w = wvWave v
      (nr, nc) = boardDims (cwAfter w)
      e = t * t
      table = fallTable w
  drawBoardBase ren app
  rendererClipRect ren $= Just (boardRectOn nr nc)
  forM_ (boardPositions (cwAfter w)) $ \pos -> do
    let (d, _new) = fallAt table pos
        (x, y) = cellOrigin pos
        off = round (fromIntegral (fromIntegral d * cellPx) * (1 - e)) :: CInt
    drawCellAny ren app x (y - off) (getCell (cwAfter w) pos) False
  rendererClipRect ren $= Nothing
  drawUfosAny ren app

-- | 交换补间：两格沿直线互换位置。
drawSwap :: Renderer -> App -> Board -> Pos -> Pos -> Int -> IO ()
drawSwap ren app board p1 p2 frame = do
  let (x1, y1) = cellOrigin p1
      (x2, y2) = cellOrigin p2
      xa = lerpI x1 x2 frame swapFrames
      ya = lerpI y1 y2 frame swapFrames
      xb = lerpI x2 x1 frame swapFrames
      yb = lerpI y2 y1 frame swapFrames
      c1 = getCell board p1
      c2 = getCell board p2
      flashSet = map fst (appFlash app)
  -- 贴图模式先画棋盘底（格子 / 地毯 / 传送带 / 传送门），交换中也不露底色
  forM_ (appArt app) $ \art -> drawBoardBgArt ren art app
  mapM_
    ( \(r, c) -> do
        let pos = (r, c)
        unless (pos == p1 || pos == p2) $ do
          let (x0, y0) = cellOrigin pos
          drawCellAny ren app x0 y0 (getCell board pos) (pos `elem` flashSet)
    )
    (boardPositions board)
  drawCellAny ren app xa ya c1 (p1 `elem` flashSet)
  drawCellAny ren app xb yb c2 (p2 `elem` flashSet)

-- | 轻落：整盘从上方约 0.35 格处落下（洗牌与回放兜底用）。
drawFall :: Renderer -> App -> Board -> Int -> IO ()
drawFall ren app board frame = do
  let t = fromIntegral frame / fromIntegral fallFrames :: Double
      ease = 1 - (1 - t) * (1 - t)
      offset = round (fromIntegral cellPx * (1 - ease) * (-0.35)) :: CInt
  drawStatic ren app board offset
