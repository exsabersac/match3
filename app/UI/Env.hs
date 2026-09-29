{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 启动环境与高分屏倍率：读取 MATCH3_LEVEL / MATCH3_SEED / MATCH3_SCALE / MATCH3_SHOWCASE，
-- 展示用盘面，以及渲染器输出尺寸 → 渲染倍率、鼠标窗口坐标 → 逻辑坐标的换算。
--
-- 依赖：UI.Types、UI.Layout、Art、SDL。
module UI.Env
  ( rendererOutputPixels
  , ratioOf
  , syncScale
  , mouseToLogical
  , envWindowScale
  , envStartLevel
  , envSeed
  , showcaseState
  , showcaseBoard
  ) where

import Art
import Control.Monad (when)
import Data.IORef
import Data.Int (Int32)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Match3.Core
import SDL hiding (Normal)
import qualified SDL.Internal.Types as SI
import qualified SDL.Raw as Raw
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Random (randomIO)
import Text.Read (readMaybe)
import UI.Layout
import UI.Types

--------------------------------------------------------------------------------
-- 高分屏（Retina）倍率
--------------------------------------------------------------------------------

-- | 渲染器输出尺寸（物理像素）。Retina + windowHighDPI 下是窗口点数的 2 倍。
rendererOutputPixels :: Renderer -> IO (V2 CInt)
rendererOutputPixels (SI.Renderer raw) =
  alloca $ \pw -> alloca $ \ph -> do
    rc <- Raw.getRendererOutputSize raw pw ph
    if rc /= 0 then pure (V2 winW winH) else V2 <$> peek pw <*> peek ph

-- | 由「尺寸 / 逻辑尺寸」求倍率；宽高取较小者，保证整个逻辑画面都放得下。
ratioOf :: V2 CInt -> Float
ratioOf (V2 w h) =
  max 0.25 (min (fromIntegral w / fromIntegral winW) (fromIntegral h / fromIntegral winH))

-- | 查询当前倍率，变化时更新 SDL 渲染缩放和贴图变体选择用的 artScale。
-- 游戏逻辑与所有绘制坐标始终是 480x588 逻辑单位；SDL_RenderSetScale 负责乘上物理倍率。
syncScale :: Window -> Renderer -> IORef App -> IO ()
syncScale window ren ref = do
  out <- rendererOutputPixels ren
  win <- get (windowSize window)
  let ps = ratioOf out
      ms = ratioOf win
  app <- readIORef ref
  when (ps /= appScale app || ms /= appMouseScale app) $ do
    rendererScale ren $= V2 (realToFrac ps) (realToFrac ps)
    hPutStrLn stderr $
      "match3-sdl: render scale " ++ show ps ++ " (output " ++ showV2 out
        ++ ", window " ++ showV2 win ++ ", logical " ++ showV2 (V2 winW winH) ++ ")"
    writeIORef ref app
      { appScale = ps
      , appMouseScale = ms
      , appArt = fmap (\a -> a {artScale = ps}) (appArt app)
      }
  where
    showV2 (V2 a b) = show a ++ "x" ++ show b

-- | 鼠标事件：窗口坐标 → 逻辑坐标。macOS Retina 上 SDL 给的已是逻辑点（倍率 1，不变）；
-- MATCH3_SCALE=N 放大窗口时窗口坐标 = 物理像素，需要除以 N。
mouseToLogical :: Float -> Event -> Event
mouseToLogical s ev
  | s == 1 = ev
  | otherwise = case eventPayload ev of
      MouseButtonEvent me ->
        ev {eventPayload = MouseButtonEvent me {mouseButtonEventPos = conv (mouseButtonEventPos me)}}
      MouseMotionEvent mm ->
        ev {eventPayload = MouseMotionEvent mm {mouseMotionEventPos = conv (mouseMotionEventPos mm)}}
      _ -> ev
  where
    conv (P (V2 x y)) = P (V2 (d x) (d y))
    d :: Int32 -> Int32
    d v = floor (fromIntegral v / s :: Float)

-- | MATCH3_SCALE=N（1..4，测试用）：窗口按 N 倍逻辑尺寸创建。默认 1。
envWindowScale :: IO CInt
envWindowScale = do
  v <- lookupEnv "MATCH3_SCALE"
  pure $ case v >>= readMaybe of
    Just n | n >= 1 && n <= (4 :: Int) -> fromIntegral n
    _ -> 1

-- | 开发 / 截图用：MATCH3_LEVEL=N（1 起）直接从第 N 关开始；仅前端，不改规则。
envStartLevel :: IO Int
envStartLevel = do
  v <- lookupEnv "MATCH3_LEVEL"
  pure $ case v >>= readMaybe of
    Just n | n >= 1 && n <= length allLevels -> n - 1
    _ -> 0

-- | 开发 / 复现用：MATCH3_SEED=N 固定开局随机种子（便于 Xvfb 下按固定坐标复现问题）；
-- 未设置时仍随机。只影响首局，重开 / 下一关照旧随机。
envSeed :: IO Int
envSeed = do
  v <- lookupEnv "MATCH3_SEED"
  case v >>= readMaybe of
    Just n -> pure n
    Nothing -> randomIO

-- | MATCH3_SHOWCASE=1：把棋盘换成「全部棋子一览」，用于检查贴图（仅展示，不影响规则模块）。
showcaseState :: GameState -> GameState
showcaseState gs =
  gs
    { gsBoard = showcaseBoard
    , gsHint = Nothing
    , gsUfos = [Ufo (7, 7) C3]
    , gsPortals = [((7, 5), (7, 6))]
    , gsBelts = [[(7, 0), (7, 1), (7, 2), (7, 3)]]
    , gsCarpetOpen = [(7, 4)]
    }

showcaseBoard :: Board
showcaseBoard =
  [ [mkGem C1, mkGem C2, mkGem C3, mkGem C4, mkGem C5, Gem C1 LineH 0 Nothing, Gem C2 LineV 0 Nothing, Gem C3 Bomb 0 Nothing]
  , [Gem C4 Rainbow 0 Nothing, mkFlip C1 C3, mkCountdown C2 3, mkIceGem C5 1, mkIceGem C1 2, mkIceGem C2 3, mkGrassGem C3, mkVineGem C4]
  , [mkChocoGem C5, mkFogGem C1 1, mkFogGem C2 3, mkChainGem C3 1, mkChainGem C4 2, mkFreezeGem C5 1, mkFreezeGem C1 2, mkCurtainGem C2 1]
  , [mkCurtainGem C3 2, mkSteamGem C4, mkStoneLayers 1, mkStoneLayers 2, mkStoneLayers 3, mkChestLayers 1, mkChestLayers 2, mkHoneyLayers 2]
  , [mkBalloon C1, mkBalloon C2, mkBalloon C3, mkBalloon C4, mkBalloon C5, mkCookie, mkCakeLayers 1, mkCakeLayers 3]
  , [mkMagicHat, mkMakerCharges C1 3, mkMakerCharges C3 2, mkSnail 0 1, mkSnail 1 0, mkSafeLayers 2, mkSurprise, mkTimeSpirit]
  , [mkBottle C1, mkBottle C2, mkBottle C3, mkBottle C4, mkBottle C5, mkSnail 0 (-1), mkSnail (-1) 0, mkGem C1]
  , [mkGem C2, mkGem C3, mkGem C4, mkGem C5, mkGem C1, mkGem C2, mkGem C3, mkGem C4]
  ]
