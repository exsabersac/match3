{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | 贴图版的单格绘制（第三刀从 UI.BoardArt.drawCellArt 拆出，函数体逐字搬运）：每种元素一个函数，
-- 签名相同（渲染器、贴图集、呼吸计数、格左上角、格子内容），由 UI.CellTable 按元素名查表分派；
-- 闪白与「缺图退回几何版」由分派方（UI.BoardArt.drawCellArt）统一处理。
-- 另含贴图命名（colorKey / gemSprite）、呼吸曲线 breathe 与层数角标。
--
-- 依赖：Art（贴图集与绘制）、UI.Layout、Match3.Core、SDL。
module UI.Cell.Art
  ( colorKey
  , gemSprite
  , breathe
  , drawLayerBadge
  , drawBadgeAt
  , artGem
  , artStone
  , artChest
  , artHoney
  , artBalloon
  , artCookie
  , artCake
  , artMagicHat
  , artMaker
  , artSnail
  , artSafe
  , artFlip
  , artSurprise
  , artBottle
  , artTimeSpirit
  , artCountdown
  , artCustom
  ) where

import Art
import Control.Monad (void, when)
import Foreign.C.Types (CDouble, CInt)
import Match3.Core
import SDL hiding (Normal)
import UI.Layout

-- | 颜色 → 贴图后缀（c1..c5）。
colorKey :: Color -> String
colorKey C1 = "c1"
colorKey C2 = "c2"
colorKey C3 = "c3"
colorKey C4 = "c4"
colorKey C5 = "c5"

gemSprite :: Color -> String
gemSprite c = "gem_" ++ colorKey c

-- | 0..1 正弦呼吸，周期约 period 帧。
breathe :: Int -> Double -> Double
breathe pulse period = 0.5 + 0.5 * sin (fromIntegral pulse * 2 * pi / period)

-- | 各元素贴图函数共用的小工具（原 drawCellArt 里的 let 绑定）。
data Kit = Kit
  { dst    :: Rectangle CInt    -- ^ 本格矩形
  , spr    :: String -> IO ()   -- ^ 整格画一张贴图
  , sprBob :: String -> IO ()   -- ^ 轻微上下浮动地画（气球 / 精灵）
  , badge  :: Int -> IO ()      -- ^ 层数 ≥ 2 时画右下角角标
  }

cellKit :: Renderer -> Art -> Int -> CInt -> CInt -> Kit
cellKit ren art pulse x y =
  let dst = cellRect x y
      spr n = void (drawSprite ren art n dst)
      -- 气球 / 精灵轻微上下浮动
      bob = round (2 * sin (fromIntegral pulse / 9 :: Double)) :: CInt
      sprBob n = void (drawSprite ren art n (cellRect x (y + bob)))
      badge = drawLayerBadge ren art x y
  in Kit {..}

-- | 层数 ≥ 2 时右下角数字角标。
drawLayerBadge :: Renderer -> Art -> CInt -> CInt -> Int -> IO ()
drawLayerBadge ren art x y n = when (n >= 2) $ drawBadgeAt ren art x y n

drawBadgeAt :: Renderer -> Art -> CInt -> CInt -> Int -> IO ()
drawBadgeAt ren art x y n =
  void (drawSprite ren art ("badge_" ++ show (clampI 1 9 n)) (rect (x + cellPx - 23) (y + cellPx - 23) 23 23))

-- | 贴图版：宝石（特殊块 / 冰 / 覆盖层 / 层数角标）。
artGem :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artGem ren art pulse x y cell = case cell of
  Gem col kind ice ov -> do
    -- 炸弹：身后橙色呼吸光晕
    when (kind == Bomb) $ do
      let a = round (140 + 110 * breathe pulse 50) :: Int
      void (drawSpriteMod ren art "bomb_glow" dst (V3 255 255 255) (fromIntegral a))
    if kind == Rainbow
      then void (drawSpriteEx ren art "rainbow" dst (fromIntegral (pulse * 2 `mod` 360) :: CDouble) False)
      else spr (gemSprite col)
    case kind of
      LineH -> spr "line_h"
      LineV -> spr "line_v"
      Bomb -> spr "bomb_mark"
      _ -> pure ()
    when (ice > 0) $ spr ("ice_" ++ show (clampI 1 3 ice))
    layers <- case ov of
      Just Grass -> spr "grass" >> pure 0
      Just Vine -> spr "vine" >> pure 0
      Just Choco -> spr "choco" >> pure 0
      Just (Fog n) -> spr ("fog_" ++ show (clampI 1 2 n)) >> pure n
      Just (Chain n) -> spr ("chain_" ++ show (clampI 1 2 n)) >> pure n
      Just (Freeze n) -> spr ("freeze_" ++ show (clampI 1 2 n)) >> pure n
      Just (Curtain n) -> spr ("curtain_" ++ show (clampI 1 2 n)) >> pure n
      Just Steam -> spr "steam" >> pure 0
      Nothing -> pure 0
    badge (if layers > 0 then layers else ice)
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：石头。
artStone :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artStone ren art pulse x y cell = case cell of
  Stone n -> spr ("stone_" ++ show (clampI 1 3 n)) >> badge n
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：宝箱。
artChest :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artChest ren art pulse x y cell = case cell of
  Chest n -> spr "chest" >> badge n
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：蜂蜜罐。
artHoney :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artHoney ren art pulse x y cell = case cell of
  Honey n -> spr "honey" >> badge n
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：气球（上下浮动）。
artBalloon :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artBalloon ren art pulse x y cell = case cell of
  Balloon c -> sprBob ("balloon_" ++ colorKey c)
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：饼干。
artCookie :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artCookie ren art pulse x y cell = case cell of
  Cookie -> spr "cookie"
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：蛋糕。
artCake :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artCake ren art pulse x y cell = case cell of
  Cake n -> spr ("cake_" ++ show (clampI 1 3 n)) >> badge n
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：魔法帽。
artMagicHat :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artMagicHat ren art pulse x y cell = case cell of
  MagicHat -> spr "magic_hat"
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：果汁机（剩余次数角标）。
artMaker :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artMaker ren art pulse x y cell = case cell of
  Maker c n -> do
    spr ("maker_" ++ colorKey c)
    -- 果汁机是计数器：剩余次数始终显示
    drawBadgeAt ren art x y (max 1 n)
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：蜗牛（按爬行方向旋转）。
artSnail :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artSnail ren art pulse x y cell = case cell of
  Snail dr dc -> do
    -- 贴图朝右；按爬行方向旋转 / 翻转
    let (ang, flipH)
          | abs dc >= abs dr && dc >= 0 = (0, False)
          | abs dc >= abs dr = (0, True)
          | dr > 0 = (90, False)
          | otherwise = (-90, False)
    void (drawSpriteEx ren art "snail" dst ang flipH)
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：保险箱。
artSafe :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artSafe ren art pulse x y cell = case cell of
  Safe n -> spr "safe" >> badge n
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：双面块。
artFlip :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artFlip ren art pulse x y cell = case cell of
  Flip f b -> do
    spr (gemSprite f)
    -- 右上角小图 = 翻面后的颜色；左下角双箭头标记
    void (drawSprite ren art (gemSprite b) (rect (x + cellPx - 25) (y + 1) 24 24))
    spr "flip_mark"
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：彩蛋。
artSurprise :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artSurprise ren art pulse x y cell = case cell of
  Surprise -> spr "surprise"
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：染色瓶。
artBottle :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artBottle ren art pulse x y cell = case cell of
  Bottle c -> spr ("bottle_" ++ colorKey c)
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：时间精灵（上下浮动）。
artTimeSpirit :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artTimeSpirit ren art pulse x y cell = case cell of
  TimeSpirit -> sprBob "time_spirit"
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：倒计时炸弹。
artCountdown :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artCountdown ren art pulse x y cell = case cell of
  Countdown c n -> do
    spr (gemSprite c)
    spr ("countdown_" ++ show (clampI 1 9 n))
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y

-- | 贴图版：自定义元素（贴图名 = 元素名）。
artCustom :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO ()
artCustom ren art pulse x y cell = case cell of
  Custom n k -> spr n >> badge k
  _ -> pure ()
  where
    Kit {..} = cellKit ren art pulse x y
