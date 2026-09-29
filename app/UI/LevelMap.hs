{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 选关地图：章节分隔、节点位置、点击命中，以及地图的几何 / 贴图两种绘制。
--
-- 依赖：UI.TextArt、UI.Glyph、UI.HudArt（目标图标）、UI.BoardArt（呼吸光）、UI.Types、UI.Layout。
-- 同步：mapNodePos 与 mapHitTest 共用同一套坐标；章节起点 chapterStarts 要与 allLevels 的章节划分一致。
module UI.LevelMap
  ( chapterStarts
  , chapterLabel
  , chapterGapBefore
  , mapNodePos
  , mapHitTest
  , drawLevelMap
  , drawLevelMapArt
  ) where

import Art
import Control.Monad (forM_, void, when)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.BoardArt
import UI.Glyph
import UI.HudArt
import UI.Layout
import UI.TextArt
import UI.Types

-- | Chapter boundaries (0-based level index starts). Light map separators.
chapterStarts :: [Int]
chapterStarts = [0, 7, 14, 21, 28, 34, 36]

chapterLabel :: Int -> String
chapterLabel 0 = "CH1"
chapterLabel 7 = "CH2"
chapterLabel 14 = "CH3"
chapterLabel 21 = "CH4"
chapterLabel 28 = "CH5"
chapterLabel 34 = "CH6"
chapterLabel 36 = "CH7"
chapterLabel _ = ""

-- | Extra vertical gap before chapter-start nodes.
chapterGapBefore :: Int -> CInt
chapterGapBefore i
  | i `elem` drop 1 chapterStarts = 14
  | otherwise = 0

-- | Simplified campaign map (选关): zig-zag nodes + chapter gaps.
mapNodePos :: Int -> (CInt, CInt)
mapNodePos i =
  let cols = 6 :: Int
      row = i `div` cols
      col = i `mod` cols
      col' = if even row then col else (cols - 1 - col)
      -- Accumulate chapter gaps for rows that contain a chapter start
      -- Compact spacing so all 38 nodes + CH1–CH7 labels fit in winH.
      gapY =
        sum
          [ chapterGapBefore j
          | j <- [0 .. i]
          ]
      x = 40 + fromIntegral col' * 72
      y = hudH + 36 + fromIntegral row * 52 + gapY
  in (x, y)

-- | 地图点击命中哪个关卡节点（与 mapNodePos 共用坐标）。
mapHitTest :: Int32 -> Int32 -> Maybe Int
mapHitTest mx my =
  let hits =
        [ i
        | i <- [0 .. length allLevels - 1]
        , let (nx, ny) = mapNodePos i
              r = 22 :: CInt
        , fromIntegral mx >= nx - r
        , fromIntegral mx <= nx + r
        , fromIntegral my >= ny - r
        , fromIntegral my <= ny + r
        ]
  in case hits of
       (i : _) -> Just i
       [] -> Nothing

-- | 几何降级版选关地图。
drawLevelMap :: Renderer -> App -> IO ()
drawLevelMap ren app
  | not (appMapOpen app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 12 18 28 230
      fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW winH)))
      drawBannerWord ren 80 20 4 (V4 255 220 100 255) "MAP"
      drawBannerWord ren 250 28 2 (V4 180 200 220 255) "M"
      -- Chapter separators / labels
      forM_ chapterStarts $ \ci -> do
        let lab = chapterLabel ci
        when (not (null lab)) $ do
          let (_nx, ny) = mapNodePos ci
          rendererDrawColor ren $= V4 90 110 140 255
          fillRect ren (Just (Rectangle (P (V2 16 (ny - 36))) (V2 (winW - 32) 2)))
          drawBannerWord ren 20 (ny - 32) 2 (V4 160 190 220 255) lab
      -- Path lines between consecutive nodes (skip visual break at chapter edges)
      rendererDrawColor ren $= V4 60 80 100 255
      forM_ [0 .. length allLevels - 2] $ \i -> do
        let (x0, y0) = mapNodePos i
            (x1, y1) = mapNodePos (i + 1)
        drawLine ren (P (V2 x0 y0)) (P (V2 x1 y1))
      let reached = appMaxReached app
          cur = gsLevel (appGame app)
      forM_ (zip [0 :: Int ..] allLevels) $ \(i, lvl) -> do
        let (nx, ny) = mapNodePos i
            unlocked = i <= reached
            isCur = i == cur
            body
              | isCur = V4 255 200 60 255
              | unlocked = V4 80 180 120 255
              | otherwise = V4 50 50 70 255
        rendererDrawColor ren $= body
        fillRect ren (Just (Rectangle (P (V2 (nx - 18) (ny - 18))) (V2 36 36)))
        rendererDrawColor ren $= V4 230 230 245 255
        drawRect ren (Just (Rectangle (P (V2 (nx - 18) (ny - 18))) (V2 36 36)))
        -- Pulse ring on current level so map focus is obvious
        when isCur $ do
          let bright = fromIntegral (200 + (appPulse app `mod` 50)) :: Word8
          rendererDrawColor ren $= V4 255 bright 60 255
          drawRect ren (Just (Rectangle (P (V2 (nx - 22) (ny - 22))) (V2 44 44)))
          drawRect ren (Just (Rectangle (P (V2 (nx - 20) (ny - 20))) (V2 40 40)))
        drawNumber ren (nx - 10) (ny - 8) 2 (V4 240 240 255 255) (i + 1)
        -- Tiny goal color pip
        let pip = case lvlGoal lvl of
              GoalScore _ -> V4 100 220 140 255
              GoalCollect c _ -> let (r,g,b) = colorRGB c in V4 r g b 255
              GoalCollectMulti _ -> V4 220 180 100 255
              GoalClearStone _ -> V4 160 160 170 255
              GoalChest _ -> V4 220 170 60 255
              GoalHoney _ -> V4 240 180 40 255
              GoalBalloon _ -> V4 255 120 160 255
              GoalCookie _ -> V4 210 160 90 255
              GoalCake _ -> V4 255 140 180 255
              GoalSafe _ -> V4 200 170 50 255
              GoalUfo _ -> V4 180 120 255 255
              GoalCarpet _ -> V4 180 100 160 255
        rendererDrawColor ren $= pip
        fillRect ren (Just (Rectangle (P (V2 (nx - 6) (ny + 22))) (V2 12 6)))

-- | 贴图版选关地图（节点坐标 / 点击判定与原版一致）。
drawLevelMapArt :: Renderer -> Art -> App -> IO ()
drawLevelMapArt ren art app
  | not (appMapOpen app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 12 10 32 235
      fillRect ren (Just (rect 0 0 winW winH))
      zhAC ren art "zh_map" (winW `div` 2) 16 32
      zhAC ren art "zh_map_hint" (winW `div` 2) 56 18
      forM_ chapterStarts $ \ci -> do
        let (_nx, ny) = mapNodePos ci
        rendererDrawColor ren $= V4 110 100 190 160
        fillRect ren (Just (rect 16 (ny - 36) (winW - 32) 1))
      rendererDrawColor ren $= V4 140 130 220 200
      forM_ [0 .. length allLevels - 2] $ \i -> do
        let (x0, y0) = mapNodePos i
            (x1, y1) = mapNodePos (i + 1)
        forM_ [-1, 0, 1] $ \d -> drawLine ren (P (V2 x0 (y0 + d))) (P (V2 x1 (y1 + d)))
      let reached = appMaxReached app
          cur = gsLevel (appGame app)
      forM_ (zip [0 :: Int ..] allLevels) $ \(i, lvl) -> do
        let (nx, ny) = mapNodePos i
            kind
              | i == cur = "node_cur"
              | i <= reached = "node_done"
              | otherwise = "node_lock"
        when (i == cur) $ do
          let a = round (120 + 120 * breathe (appPulse app) 50) :: Int
          void (drawSpriteAdd ren art "spark" (rect (nx - 34) (ny - 34) 68 68) (V3 255 210 90) (fromIntegral a))
        _ <- drawSprite ren art kind (rect (nx - 20) (ny - 20) 40 40)
        textAC ren art nx (ny - 9) 3 (if i <= reached then V4 255 255 255 255 else V4 170 170 200 255) (show (i + 1))
        void (drawSprite ren art (goalIcon (lvlGoal lvl)) (rect (nx + 10) (ny + 6) 18 18))
      -- 章节标签最后画（小底板），避免被节点遮住
      forM_ (zip [0 :: Int ..] chapterStarts) $ \(k, ci) -> do
        let (nx, ny) = mapNodePos ci
            key = "zh_ch" ++ show (k + 1)
            w = zhW art key 14
            lx = max 8 (nx - (w + 12) `div` 2)
        _ <- drawPanel ren art "panel_gold" (rect lx (ny - 45) (w + 12) 20) 7
        void (zhA ren art key (lx + 6) (ny - 42) 14)
