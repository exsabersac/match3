{-# LANGUAGE OverloadedStrings #-}
-- | 冰层与叠层：盖在宝石上的叠层原型（'Cover'，ecs-4 起是数据）。
--
-- 共同特征：不占格，只在 Gem 格的冰层数 / overlay 字段里；解码时由注册表剥在本体外面，合成规则
-- （本层先回答，没意见再问里面）只写在注册表的 wholeMatch / wholeHit 里一次。冰层削层点火；草 / 藤 / 巧随格清掉；
-- 迷雾 / 锁链 / 火箭冰冻 / 窗帘带层数、邻消揭一层；巧克力 / 蒸汽被邻格真消除清掉；藤 10 → 巧 20 → 蒸汽 30 步末蔓延
-- （PhaseSpread）。邻格 system 次序：迷雾 70 → 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 巧克力 150 → 蒸汽 160。
-- 状态：带层数的叠层 = 层数（Int），无层数的 = ()。
module Match3.Element.Builtin.Layer
  ( iceCover
  , grassCover
  , vineCover
  , chocoCover
  , fogCover
  , chainCover
  , freezeCover
  , curtainCover
  , steamCover
  , putOverlay
  ) where

import Match3.ECS.Cover
import Match3.ECS.Stage (SysDef(..), spreadSys)
import Match3.Element.Kind (Nudge(..), Reach(..))
import Match3.Element.Rules (coverNear, coverSpread)
import Match3.Element.Types
import Match3.Types

-- | 冰层：不挡匹配 / 交换；多层冰只削一层（不点火），末层冰随宝石一起碎（并点火）。状态 = 冰层数。
iceCover :: Cover Int
iceCover = (mkCover "ice" iceColumn)
  { cvShield = \n -> openShield {sFires = Just (n <= 1), sHit = if n > 1 then Keep (n - 1) else Shatter}
    -- 放置：设冰层数（精确一个整数参数）；原格不是宝石时不放
  , cvSpawn = \args cell -> case cell of
      Gem col kind _ ov -> (\n -> Gem col kind n ov) <$> exactArgs argInt args
      _ -> Nothing
  }
  where
    iceColumn = CoverColumn peel put
    peel cell = case cell of
      Gem c k n ov | n > 0 -> Just (n, Gem c k 0 ov)
      _ -> Nothing
    put n cell = case cell of
      Gem c k _ ov -> Gem c k n ov
      _ -> cell

-- | 无层数叠层的列。
markColumn :: CellOverlay -> CoverColumn ()
markColumn ov = overlayColumn (\o -> if o == ov then Just () else Nothing) (const ov)

-- | 带层数叠层的列（状态 = 层数）。
countColumn :: (CellOverlay -> Maybe Int) -> (Int -> CellOverlay) -> CoverColumn Int
countColumn = overlayColumn

-- | 草：真消除时随格清掉。
grassCover :: Cover ()
grassCover = (mkCover "grass" (markColumn Grass))
  { cvSpawn = overlayPlace Grass
  , cvShield = const openShield {sStrips = True}
  }

-- | 藤：真消除时随格清掉；步末向相邻裸宝石蔓延。
vineCover :: Cover ()
vineCover = (mkCover "vine" (markColumn Vine))
  { cvSpawn = overlayPlace Vine
  , cvShield = const openShield {sStrips = True}
  , cvSystems = [SysEnd (spreadSys 10 (coverSpread vineCover ()))]
  }

-- | 巧克力：真消除时随格清掉；邻格真消除清掉它；步末蔓延。
chocoCover :: Cover ()
chocoCover = (mkCover "choco" (markColumn Choco))
  { cvSpawn = overlayPlace Choco
  , cvShield = const openShield {sStrips = True}
  , cvSystems =
      [ SysNear 150 (coverNear chocoCover AllNeighbours (\_ cell -> Becomes (stripOverlay cell)))
      , SysEnd (spreadSys 20 (coverSpread chocoCover ()))
      ]
  }

-- | 迷雾：挡匹配，邻消揭一层。
fogCover :: Cover Int
fogCover = (mkCover "fog" col)
  { cvSpawn = layeredPlace Fog
  , cvShield = const openShield {sBlocksMatch = True}
  , cvSystems = [SysNear 70 (coverNear fogCover SkipDirect (chipLayer col))]
  }
  where
    col = countColumn (\o -> case o of Fog n -> Just n; _ -> Nothing) Fog

-- | 锁链：挡匹配 / 交换、不点火；直接命中与邻消各揭一层。
chainCover :: Cover Int
chainCover = (mkCover "chain" col)
  { cvSpawn = layeredPlace Chain
  , cvShield = \n -> openShield {sBlocksMatch = True, sBlocksSwap = True, sFires = Just False, sHit = peelHit n}
  , cvSystems = [SysNear 80 (coverNear chainCover SkipDirect (chipLayer col))]
  }
  where
    col = countColumn (\o -> case o of Chain n -> Just n; _ -> Nothing) Chain

-- | 火箭冰冻：不挡匹配、挡交换；邻消揭一层。
freezeCover :: Cover Int
freezeCover = (mkCover "freeze" col)
  { cvSpawn = layeredPlace Freeze
  , cvShield = const openShield {sBlocksSwap = True}
  , cvSystems = [SysNear 90 (coverNear freezeCover SkipDirect (chipLayer col))]
  }
  where
    col = countColumn (\o -> case o of Freeze n -> Just n; _ -> Nothing) Freeze

-- | 窗帘：挡匹配、不点火；直接命中与邻消各揭一层。
curtainCover :: Cover Int
curtainCover = (mkCover "curtain" col)
  { cvSpawn = layeredPlace Curtain
  , cvShield = \n -> openShield {sBlocksMatch = True, sFires = Just False, sHit = peelHit n}
  , cvSystems = [SysNear 100 (coverNear curtainCover SkipDirect (chipLayer col))]
  }
  where
    col = countColumn (\o -> case o of Curtain n -> Just n; _ -> Nothing) Curtain

-- | 蒸汽：挡匹配；邻格真消除清掉它；步末蔓延。
steamCover :: Cover ()
steamCover = (mkCover "steam" (markColumn Steam))
  { cvSpawn = overlayPlace Steam
  , cvShield = const openShield {sBlocksMatch = True}
  , cvSystems =
      [ SysNear 160 (coverNear steamCover AllNeighbours (\_ cell -> Becomes (stripOverlay cell)))
      , SysEnd (spreadSys 30 (coverSpread steamCover ()))
      ]
  }

-- | 直接命中揭一层（锁链 / 窗帘）：宝石留下，不消除。
peelHit :: Int -> LayerHit Int
peelHit n
  | n <= 1 = Peel
  | otherwise = Keep (n - 1)

-- | 邻格真消除揭一层（迷雾 / 锁链 / 火箭冰冻 / 窗帘）：末层去掉叠层，宝石留下。
chipLayer :: CoverColumn Int -> Int -> Cell -> Nudge
chipLayer col n cell
  | n <= 1 = Becomes (stripOverlay cell)
  | otherwise = Becomes (ccPut col (n - 1) cell)

-- | 去掉宝石上的叠层（非宝石格不变）。
stripOverlay :: Cell -> Cell
stripOverlay cell = case cell of
  Gem c k i _ -> Gem c k i Nothing
  _ -> cell

-- | 放置一种无层数的叠层：换掉原格的叠层（原格不是宝石时不放）。
overlayPlace :: CellOverlay -> Placer
overlayPlace ov _ cell = case cell of
  Gem col kind ice _ -> Just (Gem col kind ice (Just ov))
  _ -> Nothing

-- | 放置一种带层数的叠层（精确一个整数参数）。
layeredPlace :: (Int -> CellOverlay) -> Placer
layeredPlace con args cell = case cell of
  Gem col kind ice _ -> (\n -> Gem col kind ice (Just (con n))) <$> exactArgs argInt args
  _ -> Nothing
