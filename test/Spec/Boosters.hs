{-# LANGUAGE ScopedTypeVariables #-}

-- | 道具：锤子、自由交换、十字清除的命中 / 免疫 / 扣次数，以及道具不触发的步末阶段。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Boosters
  ( tests
  ) where

import Match3.Core
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "booster_hammer_clears_cell" booster_hammer_clears_cell
  , testCase "booster_free_swap_any_cells" booster_free_swap_any_cells
  , testCase "booster_cross_clears_row_col" booster_cross_clears_row_col
  , testCase "hammer_peels_chain_not_gem" hammer_peels_chain_not_gem
  , testCase "hammer_peels_curtain_not_gem" hammer_peels_curtain_not_gem
  , testCase "hammer_chips_stone_layer" hammer_chips_stone_layer
  , testCase "cross_peels_chain_on_seed" cross_peels_chain_on_seed
  , testCase "freeswap_blocked_by_stone_chain" freeswap_blocked_by_stone_chain
  , testCase "hammer_clears_grass_vine" hammer_clears_grass_vine
  , testCase "hammer_immune_no_spend" hammer_immune_no_spend
  , testCase "cross_keeps_maker_in_place" cross_keeps_maker_in_place
  , testCase "booster_freeswap_skips_countdown_tick" booster_freeswap_skips_countdown_tick
  ]

--------------------------------------------------------------------------------
-- Boosters + lose hint
--------------------------------------------------------------------------------

booster_hammer_clears_cell :: Assertion
booster_hammer_clears_cell = do
  let board = setCell stableBoard (2, 2) (mkGem C1)
      gs0 =
        (newGame defaultConfig 1)
          { gsBoard = board
          , gsHammers = 2
          , gsMoves = 10
          , gsScore = 0
          , gsOver = Nothing
          }
      (gs1, out) = useHammer (2, 2) gs0
  case out of
    NoMatch -> assertFailure "hammer should apply"
    InvalidSwap -> assertFailure "hammer should be valid"
    MoveApplied g -> assertBool "scored" (g >= 10)
    _ -> pure ()
  assertEqual "hammer spent" (1 :: Int) (gsHammers gs1)
  assertEqual "moves untouched" (10 :: Int) (gsMoves gs1)
  -- No charges left after spending both
  let (gs2, _) = useHammer (0, 0) gs1
      (gs3, out3) = useHammer (1, 1) gs2
  assertEqual "empty" (0 :: Int) (gsHammers gs2)
  out3 @?= InvalidSwap
  assertEqual "unchanged hammers" (0 :: Int) (gsHammers gs3)

booster_free_swap_any_cells :: Assertion
booster_free_swap_any_cells = do
  -- Non-adjacent swap that forms C1 C1 C1 on row 3
  let board =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (5, 5)
          (mkGem C1)
      gs0 =
        (newGame defaultConfig 2)
          { gsBoard = board
          , gsFreeSwaps = 1
          , gsMoves = 10
          , gsScore = 0
          , gsOver = Nothing
          }
  -- Adjacent would be trySwap; here (3,2) and (5,5) are NOT adjacent
  assertBool "not adjacent" (not (adjacent (3, 2) (5, 5)))
  let (gs1, out) = useFreeSwap (3, 2) (5, 5) gs0
  case out of
    NoMatch -> assertFailure "free swap must apply"
    InvalidSwap -> assertFailure "free swap must be valid"
    MoveApplied g -> assertBool "scored" (g >= 30)
    _ -> pure ()
  assertEqual "charge spent" (0 :: Int) (gsFreeSwaps gs1)
  assertEqual "moves free" (10 :: Int) (gsMoves gs1)
  -- No charge: reject
  let (_, out2) = useFreeSwap (0, 0) (0, 1) gs1
  out2 @?= InvalidSwap

--------------------------------------------------------------------------------
-- Cross clear booster (十字清除)
--------------------------------------------------------------------------------

booster_cross_clears_row_col :: Assertion
booster_cross_clears_row_col = do
  let seeds = crossClearSeeds (boardFromRows (replicate boardSize (replicate boardSize (mkGem C1)))) (3, 4)
  assertEqual "row+col size" (15 :: Int) (length seeds)  -- 8+8-1
  assertBool "has row" (all (\c -> (3, c) `elem` seeds) [0 .. boardSize - 1])
  assertBool "has col" (all (\r -> (r, 4) `elem` seeds) [0 .. boardSize - 1])
  let gs0 =
        (newGame defaultConfig 5)
          { gsCrossClears = 1
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = useCrossClear (3, 3) gs0
  case out of
    InvalidSwap -> assertFailure "expected cross to apply"
    NoMatch -> assertFailure "expected cascade"
    _ -> pure ()
  assertEqual "charge spent" (0 :: Int) (gsCrossClears gs1)
  let (_, out2) = useCrossClear (1, 1) gs1
  out2 @?= InvalidSwap
  assertEqual "unchanged" (0 :: Int) (gsCrossClears gs1)

--------------------------------------------------------------------------------
-- Booster × Stone/Chain/Curtain + Daily décor + Undo/Shuffle progress
--------------------------------------------------------------------------------

-- | Hammer on a chained gem peels one chain layer; gem stays (not cleared).
hammer_peels_chain_not_gem :: Assertion
hammer_peels_chain_not_gem = do
  let board0 = setCell stableBoard (3, 3) (mkChainGem C2 2)
      gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 3)
          { gsBoard = board0
          , gsHammers = 2
          , gsOver = Nothing
          , gsHint = Nothing
          })
      (gs1, out) = useHammer (3, 3) gs0
  case out of
    InvalidSwap -> assertFailure "hammer should apply"
    NoMatch -> assertFailure "hammer should apply"
    _ -> pure ()
  let cell = getCell (gsBoard gs1) (3, 3)
  assertBool "gem remains" (isGem cell)
  assertBool "chain peeled to 1" (hasChain cell && chainLayers cell == 1)
  assertEqual "color kept" (Just C2) (cellColor cell)
  -- second hammer unlocks fully (gem may reshuffle if board was stuck)
  let (gs2, _) = useHammer (3, 3) gs1 { gsOver = Nothing }
      cell2 = getCell (gsBoard gs2) (3, 3)
  assertBool "chain fully peeled" (not (hasChain cell2))

-- | Hammer on curtain peels one layer; gem stays.
hammer_peels_curtain_not_gem :: Assertion
hammer_peels_curtain_not_gem = do
  let board0 = setCell stableBoard (2, 4) (mkCurtainGem C3 2)
      gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 4)
          { gsBoard = board0
          , gsHammers = 2
          , gsOver = Nothing
          })
      (gs1, _) = useHammer (2, 4) gs0
      cell = getCell (gsBoard gs1) (2, 4)
  assertBool "gem remains" (isGem cell)
  assertBool "curtain -> 1" (hasCurtain cell && curtainLayers cell == 1)

-- | Hammer on multi-layer stone chips one layer (does not nuke all).
hammer_chips_stone_layer :: Assertion
hammer_chips_stone_layer = do
  let board0 = setCell stableBoard (5, 5) (mkStoneLayers 3)
      gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 5)
          { gsBoard = board0
          , gsHammers = 2
          , gsGoal = goalCount CountStones 8
          , gsOver = Nothing
          })
      (gs1, _) = useHammer (5, 5) gs0
      cell = getCell (gsBoard gs1) (5, 5)
  assertBool "still stone" (isStone cell)
  assertEqual "3 -> 2" (2 :: Int) (stoneLayers cell)
  assertEqual "not counted until last layer" (0 :: Int) (gsCount CountStones gs1)
  -- chip down to 1 then clear
  let (gs2, _) = useHammer (5, 5) gs1 { gsHammers = 2, gsOver = Nothing }
  assertEqual "2 -> 1" (1 :: Int) (stoneLayers (getCell (gsBoard gs2) (5, 5)))
  let (gs3, _) = useHammer (5, 5) gs2 { gsHammers = 2, gsOver = Nothing }
  assertBool "removed" (not (isStone (getCell (gsBoard gs3) (5, 5))))
  assertEqual "cleared counted" (1 :: Int) (gsCount CountStones gs3)

-- | Cross clear seed on a chain cell peels (does not clear gem).
cross_peels_chain_on_seed :: Assertion
cross_peels_chain_on_seed = do
  let board0 = setCell stableBoard (3, 3) (mkChainGem C1 1)
      gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 6)
          { gsBoard = board0
          , gsCrossClears = 1
          , gsOver = Nothing
          })
      (gs1, out) = useCrossClear (3, 3) gs0
  case out of
    InvalidSwap -> assertFailure "cross should apply"
    _ -> pure ()
  let cell = getCell (gsBoard gs1) (3, 3)
  -- Chain 1 peeled → unlocked gem; cross also clears other row/col gems so
  -- this cell may refill — assert either unlocked gem or a refill cell, not chain.
  assertBool "chain gone from seed cell" (not (hasChain cell))

-- | Free-swap refuses stone and chained endpoints (same gate as trySwap).
freeswap_blocked_by_stone_chain :: Assertion
freeswap_blocked_by_stone_chain = do
  let bStone = setCell stableBoard (1, 1) mkStone
      bChain = setCell stableBoard (2, 2) (mkChainGem C2 1)
      gsS =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 7)
          { gsBoard = bStone, gsFreeSwaps = 1, gsOver = Nothing})
      gsC =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 8)
          { gsBoard = bChain, gsFreeSwaps = 1, gsOver = Nothing})
  let (_, outS) = useFreeSwap (1, 1) (1, 3) gsS
      (_, outC) = useFreeSwap (2, 2) (2, 4) gsC
  assertEqual "stone blocks free-swap" NoMatch outS
  assertEqual "chain blocks free-swap" NoMatch outC
  assertEqual "charge kept (stone)" (1 :: Int) (gsFreeSwaps (fst (useFreeSwap (1, 1) (1, 3) gsS)))
  assertEqual "charge kept (chain)" (1 :: Int) (gsFreeSwaps (fst (useFreeSwap (2, 2) (2, 4) gsC)))

-- | Hammer on Grass/Vine clears the gem (overlays strip; ≠ Chain/Curtain peel-lock).
hammer_clears_grass_vine :: Assertion
hammer_clears_grass_vine = do
  let bg = setCell stableBoard (2, 2) (Gem C3 Normal 0 (Just Grass))
      gsG0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 2)
          { gsBoard = bg
          , gsHammers = 2
          , gsOver = Nothing
          })
      (gsG1, _) = useHammer (2, 2) gsG0
  assertBool "grass cell cleared or refilled (not peel-locked)" $
    not (hasGrass (getCell (gsBoard gsG1) (2, 2)))
  let bv = setCell stableBoard (4, 4) (Gem C4 Normal 0 (Just Vine))
      gsV0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 3)
          { gsBoard = bv
          , gsHammers = 2
          , gsOver = Nothing
          })
      (gsV1, _) = useHammer (4, 4) gsV0
  assertBool "vine cell cleared or refilled" $
    not (hasVine (getCell (gsBoard gsV1) (4, 4)))

-- | Hammer on Maker/Snail/Bottle/Hat/Cookie is a no-op: reject without spending a charge.
hammer_immune_no_spend :: Assertion
hammer_immune_no_spend = do
  let mkGs cell =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 11)
          { gsBoard = setCell stableBoard (3, 3) cell
          , gsHammers = 2
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
      check tag cell = do
        let gs0 = mkGs cell
            (gs1, out) = useHammer (3, 3) gs0
        out @?= NoMatch
        assertEqual (tag ++ " hammers kept") (2 :: Int) (gsHammers gs1)
        assertEqual (tag ++ " board unchanged") (gsBoard gs0) (gsBoard gs1)
  check "maker" (mkMakerCharges C1 2)
  check "snail" (mkSnail 0 1)
  check "bottle" (mkBottle C2)
  check "hat" mkMagicHat
  check "cookie" mkCookie
  -- Control: bare gem still spends
  let gsG0 = mkGs (mkGem C1)
      (gsG1, outG) = useHammer (3, 3) gsG0
  case outG of
    NoMatch -> assertFailure "bare gem hammer should apply"
    InvalidSwap -> assertFailure "bare gem hammer should apply"
    _ -> pure ()
  assertEqual "bare spends hammer" (1 :: Int) (gsHammers gsG1)

-- | Cross clear through a Maker must not gravity-pack it off its seed cell.
-- Regression: colGravity treated Maker as a fallable solid, so wiping the column
-- below relocated the immortal (and refill covered the décor slot).
cross_keeps_maker_in_place :: Assertion
cross_keeps_maker_in_place = do
  let board = setCell stableBoard (3, 3) (mkMakerCharges C5 5)
      gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 3)
          { gsBoard = board
          , gsCrossClears = 2
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          , gsMoves = 10
          })
      (gs1, out) = useCrossClear (3, 3) gs0
  case out of
    NoMatch -> assertFailure "cross should apply"
    InvalidSwap -> assertFailure "cross should be valid"
    _ -> pure ()
  assertEqual "cross spent" (1 :: Int) (gsCrossClears gs1)
  assertBool "Maker still at seed" (isMaker (getCell (gsBoard gs1) (3, 3)))
  -- Ortho cross seeds may same-color charge once; décor slot must not relocate.
  assertBool "Maker still charged maker" $
    makerCharges (getCell (gsBoard gs1) (3, 3)) >= 4

--------------------------------------------------------------------------------
-- Stability cruise: booster / countdown / particle boundaries
--------------------------------------------------------------------------------

-- | Free-swap does not consume a move, so surviving countdowns must NOT tick.
-- trySwap (move-costing) still ticks  N→N-1 on the same board. Locks Booster ×
-- Countdown asymmetry (boosters skip belt/snail/countdown end-of-move effects).
booster_freeswap_skips_countdown_tick :: Assertion
booster_freeswap_skips_countdown_tick = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 0) (mkGem C1))
                (0, 1)
                (mkGem C1))
             (0, 2)
             (mkGem C2))
          (0, 3)
          (mkGem C1)
      board = spawnCountdown board0 (5, 5) C5 3
      mkGs free =
        (setBelts [] . setUfos [] . setPortals [] $ (newGame defaultConfig 9)
          { gsBoard = board
          , gsFreeSwaps = free
          , gsOver = Nothing
          , gsMoves = 10
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
      (gsFree, outFree) = useFreeSwap (0, 2) (0, 3) (mkGs 1)
      (gsMove, outMove) = trySwap (0, 2) (0, 3) (mkGs 0)
  case outFree of
    NoMatch -> assertFailure "free-swap match must apply"
    InvalidSwap -> assertFailure "free-swap must be valid"
    _ -> pure ()
  case outMove of
    NoMatch -> assertFailure "trySwap match must apply"
    InvalidSwap -> assertFailure "trySwap must be valid"
    _ -> pure ()
  assertEqual "free-swap leaves countdown at 3" (3 :: Int) $
    countdownTurns (getCell (gsBoard gsFree) (5, 5))
  assertEqual "free-swap does not spend a move" (10 :: Int) (gsMoves gsFree)
  assertEqual "free-swap spends charge" (0 :: Int) (gsFreeSwaps gsFree)
  assertEqual "trySwap ticks countdown 3→2" (2 :: Int) $
    countdownTurns (getCell (gsBoard gsMove) (5, 5))
  assertEqual "trySwap spends a move" (9 :: Int) (gsMoves gsMove)
