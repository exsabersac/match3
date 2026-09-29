{-# LANGUAGE ScopedTypeVariables #-}

-- | 一轮消除：匹配清除（clearMatchesDetailed）与种子清除（clearFromSeedsDetailed），
-- 特殊块扩展（expandSpecials）、新特殊块生成（spawnSpecials）、彩蛋、邻格障碍削层 / 触发，
-- 飞碟吸收（maskUfoAbsorbSpecials / clearUfoAbsorbed：吸走 ≠ 引爆）以及计分公式。
--
-- 依赖：Grid、Match、Ice、Grass、Obstacles。
-- 同步：这里的函数只被 Match3.Board.Cascade 的单一连锁实现调用（结算与回放同一次计算），
-- 返回 (挖空后盘面, 清除数, 清除格)；Cascade 再按清除格在消除前盘面上统计障碍计数。
module Match3.Board.Clear
  ( expandSpecials
  , spawnSpecials
  , countColor
  , clearMatches
  , clearMatchesAt
  , surpriseClearPass
  , clearMatchesDetailed
  , scoreForCleared
  , scoreForWave
  , maskUfoAbsorbSpecials
  , clearUfoAbsorbed
  , clearFromSeedsDetailed
  ) where

import Data.List (foldl', nub)
import Match3.Ice (chipIceOnClear)
import Match3.Grass (clearOverlaysOn, clearChocoAdjacent, clearSteamAdjacent, chipAdjacentFogExcept, chipAdjacentChainExcept, chipAdjacentFreezeExcept, chipAdjacentCurtainExcept)
import Match3.Obstacles
  ( chipAdjacentStonesExcept
  , chipAdjacentChestsExcept
  , chipAdjacentHoneyExcept
  , chipAdjacentCakesExcept
  , chipAdjacentSafesExcept
  , chipAdjacentBalloonsExcept
  , chipAdjacentTimeSpiritsExcept
  , triggerAdjacentHatsExcept
  , chargeAdjacentMakersSit
  , openSurprises
  , triggerAdjacentBottlesExcept
  )
import Match3.Types
import Match3.Board.Grid
import Match3.Board.Match

-- | Expand clears: LineH/LineV/Bomb effects when those gem cells are in the seed set.
-- Soft-locked specials do not fire: ice>1 only chips; Chain/Curtain peel without
-- clearing (same discipline as chipIceOnClear). Last ice (ice==1) clears + activates.
-- Rainbow is a no-op here (partner color comes from rainbowClearSeeds only).
expandSpecials :: Board -> [Pos] -> [Pos]
expandSpecials b seeds = go (nub seeds) (nub seeds)
  where
    go acc [] = acc
    go acc (p : ps) =
      let cell = getCell b p
          extra = case cell of
            Gem _ LineH _ _ | specialActivates cell ->
              [(fst p, c) | c <- [0 .. boardSize - 1]]
            Gem _ LineV _ _ | specialActivates cell ->
              [(r, snd p) | r <- [0 .. boardSize - 1]]
            Gem _ Bomb _ _ | specialActivates cell ->
              [ (r, c)
              | r <- [fst p - 1 .. fst p + 1]
              , c <- [snd p - 1 .. snd p + 1]
              , inBounds (r, c)
              ]
            Gem _ LineH _ _ -> []
            Gem _ LineV _ _ -> []
            Gem _ Bomb _ _ -> []
            -- Rainbow activation is only via rainbowClearSeeds (swap partner color).
            -- Expanding own color here double-cleared partner+own on every Rainbow×gem swap.
            Gem _ Rainbow _ _ -> []
            Gem _ Normal _ _ -> []
            Stone _ -> []
            Chest _ -> []
            Honey _ -> []
            Balloon _ -> []
            Cookie -> []
            Cake _ -> []
            MagicHat -> []
            Maker _ _ -> []
            Snail _ _ -> []
            Safe _ -> []
            Flip _ _ -> []
            Surprise -> []
            Bottle _ -> []
            TimeSpirit -> []
            Countdown _ _ -> []
          new = filter (`notElem` acc) extra
      in go (acc ++ new) (ps ++ new)

-- | Specials spawned from runs: len>=5 Rainbow, len==4 Line (orient by run).
-- Only place onto positions that actually clear (holes). Flip / ice>1 seeds stay
-- on the board, so prefer/middle must fall back to a clearable cell in the run
-- (otherwise a 4-match with Flip/ice at the anchor silently drops the special).
spawnSpecials :: Maybe Pos -> [MatchRun] -> [Pos] -> [(Pos, Cell)]
spawnSpecials prefer runs clearable =
  [ (pos, Gem (runColor run) kind 0 Nothing)
  | run <- runs
  , let n = length (runPos run)
  , n >= 4
  , let kind
          | n >= 5 = Rainbow
          | runIsH run = LineH
          | otherwise = LineV
        slots = filter (`elem` clearable) (runPos run)
  , not (null slots)
  , let pos = case prefer of
          Just p | p `elem` slots -> p
          _ -> slots !! (length slots `div` 2)
  ]

-- | Count how many cleared positions have a given color (pre-clear board; stones skip).
countColor :: Board -> [Pos] -> Color -> Int
countColor b ps col =
  length
    [ p
    | p <- ps
    , case getCell b p of
        Gem c _ _ _ -> c == col
        Flip c _ -> c == col
        Countdown c _ -> c == col
        Stone _ -> False
        Chest _ -> False
        Honey _ -> False
        Balloon _ -> False
        Cookie -> False
        Cake _ -> False
        MagicHat -> False
        Maker _ _ -> False
        Snail _ _ -> False
        Safe _ -> False
        Surprise -> False
        Bottle _ -> False
        TimeSpirit -> False
    ]

-- | Clear matches (+ special expansions + adjacent stones), place new specials.
clearMatches :: Board -> (MBoard, Int)
clearMatches b = clearMatchesAt Nothing b

-- | 只要「挖空后的盘面 + 清除数」的 clearMatchesDetailed 简化版；prefer 为新特殊块的优先生成位（交换目标格）。
clearMatchesAt :: Maybe Pos -> Board -> (MBoard, Int)
clearMatchesAt prefer b =
  let (mb, n, _) = clearMatchesDetailed prefer b
  in (mb, n)

-- | Open Surprises against clear seeds; explode blasts expand specials + chip ice
-- and re-open nested Surprises until the frontier is quiet.
-- Bomb parity: Bomb→Surprise opens to special/explode; Surprise explode must
-- likewise open nested boxes instead of hole-deleting them via chipIce alone.
-- Saved specials (Bomb/Line from Surprise) must not activate in this pass — mask
-- them as Normal before expandSpecials. Same-pass placement and later re-explode
-- of an unconsumed Surprise center would otherwise fire-and-survive the special.
-- Pre-existing specials (not in saved) still expand. chipIce runs on bOpen so
-- saved specials remain on the board while explode centers clear.
-- Returns (board, trueClears, surpriseDirectHits, savedSpecialPositions).
surpriseClearPass :: Board -> [Pos] -> (Board, [Pos], [Pos], [Pos])
surpriseClearPass b0 seeds0 =
  go b0 (nub seeds0) [] [] []
  where
    -- Mask saved Surprise-specials so expandSpecials cannot activate them.
    maskSaved b ps =
      foldl' (\b' p -> setCell b' p (mkGem C1)) b ps
    go board front trueAcc directAcc saved
      | null front =
          ( board
          , nub (filter (`notElem` saved) trueAcc)
          , nub directAcc
          , nub saved
          )
      | otherwise =
          let (bOpen, expl, savedNew) = openSurprises board front
              saved' = nub (saved ++ savedNew)
              kept = filter (`notElem` saved') front
          in if null expl
               then go bOpen [] (trueAcc ++ kept) directAcc saved'
               else
                 let expanded = expandSpecials (maskSaved board saved') expl
                     (bChip, free1) = chipIceOnClear bOpen expanded
                     processed = nub (front ++ trueAcc)
                     front' = filter (`notElem` processed) free1
                 in go bChip front' (trueAcc ++ kept ++ free1) (directAcc ++ expanded) saved'

-- | Like clearMatchesAt but also returns the cleared positions (pre-spawn).
clearMatchesDetailed :: Maybe Pos -> Board -> (MBoard, Int, [Pos])
clearMatchesDetailed prefer b =
  let runs = findMatchRuns b
      base = nub (concatMap runPos runs)
      expanded = expandSpecials b base
      -- Ice chips first: iced gems stay, ice-free positions may clear.
      -- Do NOT strip Grass/Vine/Choco on raw expand seeds — soft hits (ice>1 /
      -- Flip / Chain peel) keep on-cell overlays (same discipline as adjacent).
      (bIced, iceFree) = chipIceOnClear b expanded
      -- Surprise opens against match/special clears *before* adjacent peels, so a
      -- 3×3 explode contributes to trueClears (Bomb-parity for stone/fog/chain/…).
      -- Nested Surprises inside an explode footprint also open (Bomb parity).
      (bSurp2, trueClears, surpDirect, surpSaved) = surpriseClearPass bIced iceFree
      -- Strip Grass/Vine/Choco only on true clear holes (cannot spread from ghosts)
      bClearedOv = clearOverlaysOn bSurp2 trueClears
      -- Cells that already took a direct-hit peel/chip (expand → chipIce) must not
      -- also receive an ortho adjacent peel this wave (Chain2/Stone2/Safe2 on a
      -- Line/Bomb path were double-chipped via neighbor clears).
      directHits = nub (expanded ++ surpDirect)
      -- Adjacent obstacle peels / charges against *all* true clear holes (once)
      (bChipped, deadStones) = chipAdjacentStonesExcept bClearedOv trueClears directHits
      (bChest, deadChests) = chipAdjacentChestsExcept bChipped trueClears directHits
      (bHoney, deadHoney) = chipAdjacentHoneyExcept bChest trueClears directHits
      (bCake, deadCakes) = chipAdjacentCakesExcept bHoney trueClears directHits
      (bBal, deadBalloons) = chipAdjacentBalloonsExcept bCake trueClears directHits
      -- Protect Surprise-opened specials from same-wave Hat/Bottle mutate
      bHat = triggerAdjacentHatsExcept bBal trueClears surpSaved
      (bFog, _fogCleared) = chipAdjacentFogExcept bHat trueClears directHits
      (bChain, _chainCleared) = chipAdjacentChainExcept bFog trueClears directHits
      (bFreeze, _freezeCleared) = chipAdjacentFreezeExcept bChain trueClears directHits
      (bCurtain, _curtainCleared) = chipAdjacentCurtainExcept bFreeze trueClears directHits
      (bSafe, _openedSafes) = chipAdjacentSafesExcept bCurtain trueClears directHits
      (bSpirit, deadSpirits) = chipAdjacentTimeSpiritsExcept bSafe trueClears directHits
      (bMaker, makerSaved) = chargeAdjacentMakersSit bSpirit trueClears
      -- Protect Surprise specials + Maker-produced Bombs from same-wave Bottle dye
      bBottle = triggerAdjacentBottlesExcept bMaker trueClears (nub (surpSaved ++ makerSaved))
      -- Chocolate / steam: only true clears extinguish (not soft hits)
      bNoChoco = clearChocoAdjacent bBottle trueClears
      bNoSteam = clearSteamAdjacent bNoChoco trueClears
      allPos = nub (trueClears ++ deadStones ++ deadChests ++ deadHoney ++ deadCakes ++ deadBalloons ++ deadSpirits)
      n = length allPos
      mb0 = foldl' (\m p -> setM m p Nothing) (toM bNoSteam) allPos
      spawns = spawnSpecials prefer runs allPos
      mb1 =
        foldl'
          ( \m (p, cell) ->
              if p `elem` allPos then setM m p (Just cell) else m
          )
          mb0
          spawns
  in (mb1, n, allPos)

-- | 旧计分：每格 10 分（不带波次倍数）。
scoreForCleared :: Int -> Score
scoreForCleared n = n * 10

-- | Wave 1 = 1x, wave 2 = 2x, ... (cells * 10 * wave).
scoreForWave :: Int -> Int -> Score
scoreForWave wave n = n * 10 * max 1 wave

-- | UFO 吸收前把特殊降为 Normal，清除时不走 expandSpecials（吸走 ≠ 引爆）。
-- | Demote Line/Bomb/Rainbow at UFO absorb seeds to Normal so clearFromSeedsDetailed
-- removes them without expandSpecials detonation (吸走 ≠ 引爆). Ice / overlays kept.
maskUfoAbsorbSpecials :: Board -> [Pos] -> Board
maskUfoAbsorbSpecials b ps =
  foldl' maskOne b (nub ps)
  where
    maskOne board p =
      case getCell board p of
        Gem c k ice ov
          | k /= Normal ->
              setCell board p (Gem c Normal ice ov)
        _ -> board

-- | Clear UFO-absorbed cells: mask specials first, then normal seed clear.
clearUfoAbsorbed :: Board -> [Pos] -> (MBoard, Int, [Pos])
clearUfoAbsorbed b absorbed =
  clearFromSeedsDetailed Nothing (maskUfoAbsorbSpecials b absorbed) absorbed

-- | Clear an explicit seed set (expand specials + adjacent stones).
clearFromSeedsDetailed :: Maybe Pos -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearFromSeedsDetailed prefer b seeds0 =
  let runs = findMatchRuns b
      base = nub seeds0
      expanded = expandSpecials b base
      -- Soft-hit safe: chipIce before overlay strip (same as clearMatchesDetailed)
      (bIced, iceFree) = chipIceOnClear b expanded
      -- Surprise before adjacent peels (same as clearMatchesDetailed / Bomb parity),
      -- including nested Surprises inside explode footprints.
      (bSurp2, trueClears, surpDirect, surpSaved) = surpriseClearPass bIced iceFree
      bClearedOv = clearOverlaysOn bSurp2 trueClears
      -- Exclude direct-hit cells from adjacent peels (same as clearMatchesDetailed).
      directHits = nub (expanded ++ surpDirect)
      (bChipped, deadStones) = chipAdjacentStonesExcept bClearedOv trueClears directHits
      (bChest, deadChests) = chipAdjacentChestsExcept bChipped trueClears directHits
      (bHoney, deadHoney) = chipAdjacentHoneyExcept bChest trueClears directHits
      (bCake, deadCakes) = chipAdjacentCakesExcept bHoney trueClears directHits
      (bBal, deadBalloons) = chipAdjacentBalloonsExcept bCake trueClears directHits
      bHat = triggerAdjacentHatsExcept bBal trueClears surpSaved
      (bFog, _) = chipAdjacentFogExcept bHat trueClears directHits
      (bChain, _) = chipAdjacentChainExcept bFog trueClears directHits
      (bFreeze, _) = chipAdjacentFreezeExcept bChain trueClears directHits
      (bCurtain, _) = chipAdjacentCurtainExcept bFreeze trueClears directHits
      (bSafe, _) = chipAdjacentSafesExcept bCurtain trueClears directHits
      (bSpirit, deadSpirits) = chipAdjacentTimeSpiritsExcept bSafe trueClears directHits
      (bMaker, makerSaved) = chargeAdjacentMakersSit bSpirit trueClears
      bBottle = triggerAdjacentBottlesExcept bMaker trueClears (nub (surpSaved ++ makerSaved))
      bNoChoco = clearChocoAdjacent bBottle trueClears
      bNoSteam = clearSteamAdjacent bNoChoco trueClears
      allPos = nub (trueClears ++ deadStones ++ deadChests ++ deadHoney ++ deadCakes ++ deadBalloons ++ deadSpirits)
      n = length allPos
      mb0 = foldl' (\m p -> setM m p Nothing) (toM bNoSteam) allPos
      spawns = spawnSpecials prefer runs allPos
      mb1 =
        foldl'
          ( \m (p, cell) ->
              if p `elem` allPos then setM m p (Just cell) else m
          )
          mb0
          spawns
  in (mb1, n, allPos)
