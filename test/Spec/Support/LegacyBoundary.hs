{-# LANGUAGE OverloadedStrings #-}

-- | 第 8 项（数据边界）之前的代码逐字副本，只给 Spec.DataBoundary 做新旧对照。
--
-- 放置函数照抄 e548ace（feat/hs-grid-geometry）里手写的 @case args of …@：
-- Registry.customEntry、Obstacle 的 layersPlace / flip / magic_stone / snow_boss、Actor 的 maker / snail / countdown、
-- Common.colorPlace、Layer 的 ice / layeredOverlay、Collectible 的 chameleon。按元素名列成一张表，
-- 只改了写法上的外壳（从 Entry 里拆出来、按名字配对），case 分支本身不动。
-- 另有 Engine.Effect.beats 的旧版（每组是普通列表）。
module Spec.Support.LegacyBoundary
  ( legacyPlacers
  , beats
  ) where

import Engine.Effect (Effect (..))

import Match3.Element.Builtin.Collectible (chameleonCell)
import Match3.Element.Builtin.Obstacle (MagicStone (..), SnowBoss (..), magicStoneFull)
import Match3.Element.Class (toCell)
import Match3.Element.Types (Arg (..))
import Match3.Types

-- | 旧放置函数：元素名 → (参数, 原格) → 新格（Nothing = 该格不变）。
legacyPlacers :: [(ElementName, [Arg] -> Cell -> Maybe Cell)]
legacyPlacers =
  [ ("stone", layersPlace Stone)
  , ("chest", layersPlace Chest)
  , ("honey", layersPlace Honey)
  , ("cake", layersPlace Cake)
  , ("safe", layersPlace Safe)
  , ("balloon", colorPlace Balloon)
  , ("bottle", colorPlace Bottle)
  , ("flip", \args _ -> case args of
      [AColor f, AColor b] -> Just (Flip f b)
      _ -> Nothing)
  , ("magic_stone", \args _ ->
      Just (toCell (MagicStone (case args of (AInt k : _) -> max 0 (min magicStoneFull k); _ -> 0))))
  , ("snow_boss", \args _ -> case args of
      [AInt hp, AInt q] | hp > 0, hp <= 255, q >= 0, q < 4 -> Just (toCell (SnowBoss hp hp 0 q))
      _ -> Nothing)
  , ("maker", \args _ -> case args of
      [AColor c, AInt n] -> Just (Maker c (max 1 n))
      [AColor c] -> Just (Maker c 3)
      _ -> Nothing)
  , ("snail", \args _ -> case args of
      [AInt dr, AInt dc] -> Just (mkSnail dr dc)
      _ -> Nothing)
  , ("countdown", \args cell -> case (args, cell) of
      ([AInt n], Gem col _ _ _) -> Just (mkCountdown col n)
      ([AInt n], Countdown col _) -> Just (mkCountdown col n)
      _ -> Nothing)
  , ("ice", \args cell -> case (args, cell) of
      ([AInt n], Gem col kind _ ov) -> Just (Gem col kind n ov)
      _ -> Nothing)
  , ("fog", layeredOverlay Fog)
  , ("chain", layeredOverlay Chain)
  , ("freeze", layeredOverlay Freeze)
  , ("curtain", layeredOverlay Curtain)
  , ("bubble", customPlace "bubble")
  , ("fuzzball", customPlace "fuzzball")
  , ("chameleon", \args cell -> case (args, cell) of
      (AColor c : _, _) -> Just (chameleonCell c)
      (_, Gem c _ _ _) -> Just (chameleonCell c)
      _ -> Nothing)
  ]

layersPlace :: (Int -> Cell) -> [Arg] -> Cell -> Maybe Cell
layersPlace con args _ = case args of
  [AInt n] -> Just (con (max 1 n))
  [] -> Just (con 1)
  _ -> Nothing

colorPlace :: (Color -> Cell) -> [Arg] -> Cell -> Maybe Cell
colorPlace con args _ = case args of
  [AColor c] -> Just (con c)
  _ -> Nothing

layeredOverlay :: (Int -> CellOverlay) -> [Arg] -> Cell -> Maybe Cell
layeredOverlay con args cell = case (args, cell) of
  ([AInt n], Gem col kind ice _) -> Just (Gem col kind ice (Just (con n)))
  _ -> Nothing

customPlace :: ElementName -> [Arg] -> Cell -> Maybe Cell
customPlace n args _ = Just (Custom n (CustomState (case args of (AInt k : _) -> k; _ -> 1)))

-- | 旧 Engine.Effect.beats：按节拍把相邻的效果分组（保持原顺序）。
beats :: [Effect] -> [(Int, [Effect])]
beats [] = []
beats (e : es) =
  let (same, rest) = span ((== efBeat e) . efBeat) es
  in (efBeat e, e : same) : beats rest
