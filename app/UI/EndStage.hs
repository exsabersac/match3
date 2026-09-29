{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 步末阶段绘制（ComboFx 的 PhEnd）：倒计时减一、传送带滑动、藤 / 巧 / 蒸汽蔓延生长、蜗牛爬行、自动洗牌。
--
-- 依赖：ComboFx（EndStage / stageMoves）、UI.BoardArt、UI.BoardPrim、UI.Layout、UI.Types。
-- 只画 stBefore → stAfter 的插值，不计算规则；时长由 ComboFx.endStageBase / endBudgetFrames 决定。
module UI.EndStage
  ( drawEndStage
  , drawEndTick
  , drawEndBelt
  , drawEndSpread
  , drawEndSnail
  , drawSnailAt
  , drawEndShuffle
  ) where

import Art
import ComboFx
import Control.Monad (forM_, void)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import Match3.Element.Event (endEffectElement, endEffectPairs)
import SDL hiding (Normal)
import UI.BoardArt
import UI.BoardPrim
import UI.Layout
import UI.Types

--------------------------------------------------------------------------------
-- 步末效果绘制（阶段划分见 ComboFx.arrive / EndStage）
--------------------------------------------------------------------------------

drawEndStage :: Renderer -> App -> Double -> EndStage -> IO ()
drawEndStage ren app t st =
  maybe (drawStatic ren app (stAfter st) 0) (\f -> f ren app t st) (lookup (stKind st) endStageDrawers)

-- | 步末绘制表：表现段种类（由事件类型经 ComboFx.endStageTable 得到）→ 绘制函数。
endStageDrawers :: [(StageKind, Renderer -> App -> Double -> EndStage -> IO ())]
endStageDrawers =
  [ (StTick, drawEndTick)
  , (StBelt, drawEndBelt)
  , (StSpread, drawEndSpread)
  , (StSnail, drawEndSnail)
  , (StShuffle, drawEndShuffle)
  ]

-- | 蔓延的生长曲线（按元素名）：藤蔓分 4 段一节一节伸长；巧克力先快后慢；蒸汽匀速。
spreadProgress :: [(ElementName, Double -> Double)]
spreadProgress =
  [ ( "vine"
    , \t ->
        let u = t * 4
            seg = fromIntegral (floor u :: Int)
        in min 1 ((seg + smoothT (u - seg)) / 4)
    )
  , ("choco", easeOutT)
  , ("steam", id)
  ]

-- | 倒计时减一：前半段旧数字、后半段新数字，炸弹格红光脉冲 + 数字放大回弹。
drawEndTick :: Renderer -> App -> Double -> EndStage -> IO ()
drawEndTick ren app t st = do
  let board = if t < 0.5 then stBefore st else stAfter st
      k = sin (pi * t)
  drawStatic ren app board 0
  forM_ (map fst (stageMoves st)) $ \pos -> do
    let (x, y) = cellOrigin pos
        grow = round (10 * k) :: CInt
    case appArt app of
      Just art -> do
        void (drawSpriteAdd ren art "spark" (rect (x - 8) (y - 8) (cellPx + 16) (cellPx + 16)) (V3 255 90 60) (round (200 * k)))
        case getCell board pos of
          Countdown _ n ->
            void (drawSprite ren art ("countdown_" ++ show (clampI 1 9 n)) (rect (x - grow) (y - grow) (cellPx + 2 * grow) (cellPx + 2 * grow)))
          _ -> pure ()
      Nothing -> do
        rendererDrawColor ren $= V4 255 90 60 (round (255 * k))
        drawRect ren (Just (rect (x - grow `div` 2) (y - grow `div` 2) (cellPx + grow) (cellPx + grow)))

-- | 皮带移位：相邻格平滑滑过去；首尾相接的那一格在终点缩放淡入。
drawEndBelt :: Renderer -> App -> Double -> EndStage -> IO ()
drawEndBelt ren app t st = do
  let moves = stageMoves st
      e = smoothT t
  drawCellsExcept ren app (stBefore st) (map snd moves)
  rendererClipRect ren $= Just boardRect
  forM_ moves $ \(o, d) -> do
    let cell = getCell (stBefore st) o
        (x0, y0) = cellOrigin o
        (x1, y1) = cellOrigin d
    if adjacent o d
      then drawCellAny ren app (lerpC x0 x1 e) (lerpC y0 y1 e) cell False
      else drawCellScaled ren app (x1 + cellPx `div` 2) (y1 + cellPx `div` 2) (max 0.05 e) (round (255 * e)) cell
  rendererClipRect ren $= Nothing

-- | 蔓延：新格的覆盖层从来源格那一侧「长」过来（按方向逐渐露出新状态）。
-- 藤蔓分 4 段一节一节伸长；巧克力先快后慢地涂抹铺开；蒸汽匀速漫开并淡入。生长前沿带同色柔光。
drawEndSpread :: Renderer -> App -> Double -> EndStage -> IO ()
drawEndSpread ren app t st = do
  drawStatic ren app (stBefore st) 0
  forM_ [(endEffectElement (esEffect e), pr) | e <- stSteps st, pr <- endEffectPairs (esEffect e)] $ \(name, (src, q)) -> do
    let (x, y) = cellOrigin q
        prog = maybe t ($ t) (lookup name spreadProgress)
        w = max 1 (round (fromIntegral cellPx * prog)) :: CInt
        (dr, dc) = (fst q - fst src, snd q - snd src)
        (clip, front)
          | dc == 1 = (rect x y w cellPx, rect (x + w - 10) (y - 4) 20 (cellPx + 8))
          | dc == -1 = (rect (x + cellPx - w) y w cellPx, rect (x + cellPx - w - 10) (y - 4) 20 (cellPx + 8))
          | dr == 1 = (rect x y cellPx w, rect (x - 4) (y + w - 10) (cellPx + 8) 20)
          | dr == -1 = (rect x (y + cellPx - w) cellPx w, rect (x - 4) (y + cellPx - w - 10) (cellPx + 8) 20)
          | otherwise =
              let h = w `div` 2
                  cx = x + cellPx `div` 2
                  cy = y + cellPx `div` 2
              in (rect (cx - h) (cy - h) (2 * h) (2 * h), rect (cx - h) (cy - h) (2 * h) (2 * h))
        (cr, cg, cb) = maybe (255, 255, 255) id (lookup name elementRGBTable)
    rendererClipRect ren $= Just clip
    drawCellAny ren app x y (getCell (stAfter st) q) False
    rendererClipRect ren $= Nothing
    let glowA = round (220 * (1 - t) + 30) :: Word8
    case appArt app of
      Just art -> void (drawSpriteAdd ren art "spark" front (V3 cr cg cb) glowA)
      Nothing -> do
        rendererDrawColor ren $= V4 cr cg cb glowA
        fillRect ren (Just front)

-- | 蜗牛：沿爬行方向平滑挪一格（轻微一拱），被推的宝石同时退到蜗牛原格；碰壁的蜗牛原地翻身掉头。
drawEndSnail :: Renderer -> App -> Double -> EndStage -> IO ()
drawEndSnail ren app t st = do
  let ms = [m | es <- stSteps st, EndSnail xs <- [esEffect es], m <- xs]
      e = smoothT t
  drawCellsExcept ren app (stBefore st) (concat [[smFrom m, smTo m] | m <- ms])
  forM_ ms $ \m -> do
    let (x0, y0) = cellOrigin (smFrom m)
        (x1, y1) = cellOrigin (smTo m)
    if smFrom m == smTo m
      then do
        -- 掉头：横向压扁到 0 再展开，中点换朝向
        let sq = abs (cos (pi * t))
            w = max 2 (round (fromIntegral cellPx * sq)) :: CInt
            hop = round (4 * sin (pi * t)) :: CInt
            dir = if t < 0.5 then oldDir m else smDir m
        drawSnailAt ren app (x0 + (cellPx - w) `div` 2) (y0 - hop) w dir
      else do
        -- 被推的宝石交错时往侧面让一点，两者都看得见
        let side = round (9 * sin (pi * t)) :: CInt
            (sx, sy) = if y0 == y1 then (0, side) else (side, 0)
        forM_ (smPushed m) $ \cell -> drawCellAny ren app (lerpC x1 x0 e + sx) (lerpC y1 y0 e + sy) cell False
        let hop = round (5 * sin (pi * t)) :: CInt
        drawSnailAt ren app (lerpC x0 x1 e) (lerpC y0 y1 e - hop) cellPx (smDir m)
  where
    oldDir m = case getCell (stBefore st) (smFrom m) of
      Snail dr dc -> (dr, dc)
      _ -> smDir m

-- | 按朝向画蜗牛（宽度可压扁，用于掉头翻身）。
drawSnailAt :: Renderer -> App -> CInt -> CInt -> CInt -> (Int, Int) -> IO ()
drawSnailAt ren app x y w (dr, dc) = case appArt app of
  Just art | hasSprite art "snail" -> do
    let (ang, flipH)
          | abs dc >= abs dr && dc >= 0 = (0, False)
          | abs dc >= abs dr = (0, True)
          | dr > 0 = (90, False)
          | otherwise = (-90, False)
    void (drawSpriteEx ren art "snail" (rect x y w cellPx) ang flipH)
  _ -> drawGemAt ren x y (Snail dr dc) False

-- | 自动洗牌（无可走步时规则层重排）：旧盘向中心收拢并被暗幕盖住 → 新盘从中心散开、暗幕褪去。
-- 全程用完整的格子画法（覆盖层 / 角标不会突然消失），t = 0.5 时完全被暗幕盖住再换盘。
drawEndShuffle :: Renderer -> App -> Double -> EndStage -> IO ()
drawEndShuffle ren app t st = do
  let (board, k)
        | t < 0.5 = (stBefore st, smoothT (t * 2))
        | otherwise = (stAfter st, 1 - smoothT (t * 2 - 1))
      (mx, my) = cellOrigin (3, 3)
      cx = mx + cellPx `div` 2
      cy = my + cellPx `div` 2
  drawBoardBase ren app
  forM_ allCells $ \pos -> do
    let (x, y) = cellOrigin pos
        x' = lerpC x (cx - cellPx `div` 2) (0.3 * k)
        y' = lerpC y (cy - cellPx `div` 2) (0.3 * k)
    drawCellAny ren app x' y' (getCell board pos) False
  drawUfosAny ren app
  rendererDrawColor ren $= V4 20 12 40 (round (230 * k))
  fillRect ren (Just boardRect)
  forM_ (appArt app) $ \art -> do
    let sz = round (fromIntegral boardPx * (0.2 + 0.5 * k)) :: CInt
    void (drawSpriteAdd ren art "spark" (rect (cx - sz `div` 2) (cy - sz `div` 2) sz sz) (V3 200 150 255) (round (160 * k)))
