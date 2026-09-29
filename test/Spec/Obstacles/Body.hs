{-# LANGUAGE ScopedTypeVariables #-}

-- | 本体障碍（按元素）：石头、宝箱、蜂蜜、气球、饼干、蛋糕、魔法帽、果汁机、保险箱、双面、彩蛋、染色瓶、时间精灵。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Obstacles.Body
  ( tests
  ) where

import Match3.Board.Default (cascadeMatches, cascadeSeeds, clearMatches)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crTally, crBoard), CascadeTally(CascadeTally, ctCookies, ctChests, ctBalloons, ctCakes, ctScore, ctMaxWave, ctCells, ctHoney))
import Match3.Core
import Match3.Element (defaultRegistry)
import Match3.Element.Registry (swapBlockedWith)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "stone_blocks_swap" stone_blocks_swap
  , testCase "stone_cleared_by_adjacent" stone_cleared_by_adjacent
  , testCase "stone_not_in_match" stone_not_in_match
  , testCase "stone_layer_decrement" stone_layer_decrement
  , testCase "stone_layer_clears_at_zero" stone_layer_clears_at_zero
  , testCase "chest_blocks_swap" chest_blocks_swap
  , testCase "chest_cleared_by_adjacent" chest_cleared_by_adjacent
  , testCase "chest_layer_decrement" chest_layer_decrement
  , testCase "honey_blocks_swap" honey_blocks_swap
  , testCase "honey_cleared_by_adjacent" honey_cleared_by_adjacent
  , testCase "honey_layer_decrement" honey_layer_decrement
  , testCase "balloon_blocks_swap" balloon_blocks_swap
  , testCase "balloon_popped_by_same_color" balloon_popped_by_same_color
  , testCase "balloon_ignores_other_color" balloon_ignores_other_color
  , testCase "cookie_blocks_swap" cookie_blocks_swap
  , testCase "cookie_falls_with_gravity" cookie_falls_with_gravity
  , testCase "cookie_collected_at_bottom" cookie_collected_at_bottom
  , testCase "cake_blocks_swap" cake_blocks_swap
  , testCase "cake_layer_decrement" cake_layer_decrement
  , testCase "cake_clears_at_zero" cake_clears_at_zero
  , testCase "hat_triggered_by_adjacent" hat_triggered_by_adjacent
  , testCase "hat_swaps_colors" hat_swaps_colors
  , testCase "maker_blocks_swap" maker_blocks_swap
  , testCase "maker_charges_on_same_color" maker_charges_on_same_color
  , testCase "maker_produces_bomb" maker_produces_bomb
  , testCase "safe_blocks_swap" safe_blocks_swap
  , testCase "safe_opens_to_cookie" safe_opens_to_cookie
  , testCase "safe_layer_decrement" safe_layer_decrement
  , testCase "flip_matches_front" flip_matches_front
  , testCase "flip_becomes_back_on_clear" flip_becomes_back_on_clear
  , testCase "surprise_blocks_swap" surprise_blocks_swap
  , testCase "surprise_opens_to_special" surprise_opens_to_special
  , testCase "surprise_explodes_small" surprise_explodes_small
  , testCase "bottle_blocks_swap" bottle_blocks_swap
  , testCase "bottle_dyes_neighbors" bottle_dyes_neighbors
  , testCase "time_spirit_blocks_swap" time_spirit_blocks_swap
  , testCase "time_spirit_awards_moves" time_spirit_awards_moves
  , testCase "time_spirit_rescues_last_move" time_spirit_rescues_last_move
  , testCase "flip_four_match_spawns_line" flip_four_match_spawns_line
  , testCase "surprise_blast_expands_bomb" surprise_blast_expands_bomb
  , testCase "maker_bomb_survives_wave" maker_bomb_survives_wave
  , testCase "honey_balloon_same_clear" honey_balloon_same_clear
  , testCase "safe_bottom_cookie_collected" safe_bottom_cookie_collected
  , testCase "bottle_dye_followup_match" bottle_dye_followup_match
  , testCase "hat_recolor_followup_match" hat_recolor_followup_match
  , testCase "maker_multi_adjacent_charges_once" maker_multi_adjacent_charges_once
  , testCase "surprise_direct_seed_opens" surprise_direct_seed_opens
  , testCase "surprise_blast_peels_adjacent" surprise_blast_peels_adjacent
  , testCase "blast_chips_layered_obstacles_once" blast_chips_layered_obstacles_once
  , testCase "hat_immune_to_direct_clear" hat_immune_to_direct_clear
  , testCase "surprise_blast_opens_nested" surprise_blast_opens_nested
  , testCase "surprise_nested_special_no_fire" surprise_nested_special_no_fire
  , testCase "surprise_special_sits_hat_bottle" surprise_special_sits_hat_bottle
  , testCase "maker_bomb_sits_bottle" maker_bomb_sits_bottle
  , testCase "cookie_immune_to_direct_clear" cookie_immune_to_direct_clear
  ]

-- | trySwap involving a Stone returns NoMatch; board/moves/score unchanged.
stone_blocks_swap :: Assertion
stone_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkStone
      gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsShuffled = False
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0
  gsScore gs1 @?= gsScore gs0
  gsMoves gs1 @?= gsMoves gs0
  let (gs2, out2) = trySwap (3, 4) (3, 3) gs0
  out2 @?= NoMatch
  gsBoard gs2 @?= gsBoard gs0
  gsMoves gs2 @?= gsMoves gs0

-- | Clearing gems next to a stone also removes that stone.
stone_cleared_by_adjacent :: Assertion
stone_cleared_by_adjacent = do
  -- Row 3: C1 C1 C2 C1 ... — swap (3,2)<->(3,3) yields C1 C1 C1
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      board = setCell board0 (2, 1) mkStone
  assertBool "stone placed" (isStone (getCell board (2, 1)))
  assertBool "no match yet" (not (hasAnyMatch board))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    MoveApplied _ -> pure ()
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("expected applied/terminal, got " ++ show other)
  assertBool
    "stone cleared by adjacent match"
    (not (isStone (getCell (gsBoard gs1) (2, 1))))

-- | Stones never form matches; they interrupt color runs.
stone_not_in_match :: Assertion
stone_not_in_match = do
  let bStones =
        setCell
          (setCell (setCell stableBoard (0, 0) mkStone) (0, 1) mkStone)
          (0, 2)
          mkStone
  assertBool "three stones not a match" (null (findMatches bStones))
  assertBool "hasAnyMatch false for stones" (not (hasAnyMatch bStones))
  -- C1 C1 Stone C1 C1 on a stable base — stone breaks the run
  let bBreak =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (1, 0) (mkGem C1))
                   (1, 1)
                   (mkGem C1))
                (1, 2)
                mkStone)
             (1, 3)
             (mkGem C1))
          (1, 4)
          (mkGem C1)
  assertBool "stone breaks C1 run" (null (findMatches bBreak))
  -- Gem triple with a stone beside still matches only the gems
  let bOk =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (2, 0) (mkGem C1))
                (2, 1)
                (mkGem C1))
             (2, 2)
             (mkGem C1))
          (2, 3)
          mkStone
      ms = findMatches bOk
  assertBool "gem triple still matches" ((2, 0) `elem` ms && (2, 1) `elem` ms && (2, 2) `elem` ms)
  assertBool "stone itself not in match set" ((2, 3) `notElem` ms)

-- | Multi-layer stone loses one layer on a single adjacent clear, stays on board.
-- Stone placed *below* the match so gravity does not relocate it.
stone_layer_decrement :: Assertion
stone_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkStoneLayers 3)
  assertEqual "3 layers" (3 :: Int) (stoneLayers (getCell board0 (4, 1)))
  assertBool "has match" (hasAnyMatch board0)
  let (b1, dead) = chipAdjacentStones board0 [(3, 0), (3, 1), (3, 2)]
  assertBool "not removed on first chip" (null dead)
  assertBool "still stone" (isStone (getCell b1 (4, 1)))
  assertEqual "3 -> 2" (2 :: Int) (stoneLayers (getCell b1 (4, 1)))
  let g = mkStdGen 1
  case stepCascade g board0 of
    Nothing -> assertFailure "expected a cascade step"
    Just (b2, _n, _) -> do
      assertBool "stone survives one wave" (isStone (getCell b2 (4, 1)))
      assertEqual "one wave chips once" (2 :: Int) (stoneLayers (getCell b2 (4, 1)))

-- | Last layer chip removes the stone (Stone 1, or Stone 2 hit twice).
stone_layer_clears_at_zero :: Assertion
stone_layer_clears_at_zero = do
  -- First: Stone 1 clears in one adjacent hit (existing behavior)
  let board1 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C2))
             (3, 3)
             (mkGem C1))
          (2, 1)
          (mkStoneLayers 1)
      gsA0 =
        (newGame defaultConfig 7)
          { gsBoard = board1
          , gsOver = Nothing
          }
      (gsA1, _) = trySwap (3, 2) (3, 3) gsA0
  assertBool "Stone 1 cleared" (not (isStone (getCell (gsBoard gsA1) (2, 1))))

  -- Second: Stone 2 needs two adjacent clears to vanish
  let board2 = setCell board1 (2, 1) (mkStoneLayers 2)
      -- First hit via clearMatches pipeline directly for control
      (_, dead1) = chipAdjacentStones board2 [(3, 1)]
  assertBool "first hit does not remove" (null dead1)
  let (bMid, _) = chipAdjacentStones board2 [(3, 1)]
  assertEqual "after first chip" (1 :: Int) (stoneLayers (getCell bMid (2, 1)))
  let (bEnd, dead2) = chipAdjacentStones bMid [(3, 1)]
  assertEqual "second hit removes" [(2, 1)] dead2
  assertBool "board still has stone until clear applied" (isStone (getCell bEnd (2, 1)))

--------------------------------------------------------------------------------
-- Treasure chests / 宝箱
--------------------------------------------------------------------------------

chest_blocks_swap :: Assertion
chest_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkChest
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  assertBool "is chest" (isChest (getCell board (3, 3)))

chest_cleared_by_adjacent :: Assertion
chest_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkChest
  assertBool "chest present" (isChest (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, dead) = chipAdjacentChests board0 ms
  assertEqual "last layer dead" [(2, 1)] dead
  assertBool "still on board until remove" (isChest (getCell b1 (2, 1)))
  let seeds = findMatches board0
      CascadeRun {crBoard = board1, crTally = CascadeTally {ctChests = chests}} = cascadeSeeds Nothing seeds [] [] (mkStdGen 1) board0
  assertBool "chest opened" (chests >= 1)
  assertBool "chest gone" (not (isChest (getCell board1 (2, 1))))

chest_layer_decrement :: Assertion
chest_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkChestLayers 2)
  let ms = findMatches board0
      (b1, dead) = chipAdjacentChests board0 ms
  assertEqual "no full clear yet" ([] :: [Pos]) dead
  assertEqual "layers 2->1" (1 :: Int) (chestLayers (getCell b1 (4, 1)))
  let (_b2, dead2) = chipAdjacentChests b1 ms
  assertEqual "now dead" [(4, 1)] dead2

-- Treasure honey jars / 蜂蜜罐
--------------------------------------------------------------------------------

honey_blocks_swap :: Assertion
honey_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkHoney
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  assertBool "is honey" (isHoney (getCell board (3, 3)))

honey_cleared_by_adjacent :: Assertion
honey_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkHoney
  assertBool "honey present" (isHoney (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, dead) = chipAdjacentHoney board0 ms
  assertEqual "last layer dead" [(2, 1)] dead
  assertBool "still on board until remove" (isHoney (getCell b1 (2, 1)))
  let seeds = findMatches board0
      CascadeRun {crBoard = board1, crTally = CascadeTally {ctHoney = honey}} = cascadeSeeds Nothing seeds [] [] (mkStdGen 1) board0
  assertBool "honey smashed" (honey >= 1)
  assertBool "honey gone" (not (isHoney (getCell board1 (2, 1))))

honey_layer_decrement :: Assertion
honey_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkHoneyLayers 2)
  let ms = findMatches board0
      (b1, dead) = chipAdjacentHoney board0 ms
  assertEqual "no full clear yet" ([] :: [Pos]) dead
  assertEqual "layers 2->1" (1 :: Int) (honeyLayers (getCell b1 (4, 1)))
  let (_b2, dead2) = chipAdjacentHoney b1 ms
  assertEqual "now dead" [(4, 1)] dead2

--------------------------------------------------------------------------------
-- Balloons / 气球 (same-color adjacent pop)
--------------------------------------------------------------------------------

balloon_blocks_swap :: Assertion
balloon_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkBalloon C1)
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  assertBool "is balloon" (isBalloon (getCell board (3, 3)))

balloon_popped_by_same_color :: Assertion
balloon_popped_by_same_color = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkBalloon C1)
  assertBool "balloon present" (isBalloon (getCell board0 (2, 1)))
  let ms = findMatches board0
      (_b1, dead) = chipAdjacentBalloons board0 ms
  assertEqual "same color pops" [(2, 1)] dead
  let seeds = findMatches board0
      CascadeRun {crBoard = board1, crTally = CascadeTally {ctBalloons = balloons}} = cascadeSeeds Nothing seeds [] [] (mkStdGen 1) board0
  assertBool "balloon counted" (balloons >= 1)
  assertBool "balloon gone" (not (isBalloon (getCell board1 (2, 1))))

balloon_ignores_other_color :: Assertion
balloon_ignores_other_color = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkBalloon C3)  -- different color
  let ms = findMatches board0
      (_, dead) = chipAdjacentBalloons board0 ms
  assertEqual "other color ignored" ([] :: [Pos]) dead

--------------------------------------------------------------------------------
-- Cookies / 饼干 (fall to bottom collect)
--------------------------------------------------------------------------------

cookie_blocks_swap :: Assertion
cookie_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkCookie
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  assertBool "is cookie" (isCookie (getCell board (3, 3)))

cookie_falls_with_gravity :: Assertion
cookie_falls_with_gravity = do
  -- Cookie at (0,1); clear horizontal match on row 3 including (3,1) so col 1 has a hole
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 1) mkCookie)
                (3, 0)
                (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
  assertBool "cookie at top" (isCookie (getCell board0 (0, 1)))
  let seeds = findMatches board0
  assertBool "match under cookie col" ((3, 1) `elem` seeds)
  let CascadeRun {crBoard = board1, crTally = CascadeTally {ctCookies = cookies}} = cascadeSeeds Nothing seeds [] [] (mkStdGen 1) board0
      cookiePos =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCookie (getCell board1 (r, c))
        ]
  assertBool
    ("cookie fell or collected, pos=" ++ show cookiePos ++ " collected=" ++ show cookies)
    (cookies >= 1 || null cookiePos || any (\(r, c) -> c == 1 && r > 0) cookiePos)

cookie_collected_at_bottom :: Assertion
cookie_collected_at_bottom = do
  -- Place cookie already on bottom; clearing elsewhere should drain it
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (boardSize - 1, 4) mkCookie)
                (3, 0)
                (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
  assertBool "cookie on bottom" (isCookie (getCell board0 (boardSize - 1, 4)))
  let seeds = findMatches board0
      CascadeRun {crBoard = board1, crTally = CascadeTally {ctCookies = cookies}} = cascadeSeeds Nothing seeds [] [] (mkStdGen 1) board0
  assertBool ("cookie collected, got " ++ show cookies) (cookies >= 1)
  assertBool "cookie gone" (not (isCookie (getCell board1 (boardSize - 1, 4))))

--------------------------------------------------------------------------------
-- Cake / 蛋糕 (layered obstacle; distinct from Cookie drop-collect)
--------------------------------------------------------------------------------

cake_blocks_swap :: Assertion
cake_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkCake
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  assertBool "is cake" (isCake (getCell board (3, 3)))
  assertBool "not cookie" (not (isCookie (getCell board (3, 3))))

cake_layer_decrement :: Assertion
cake_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkCakeLayers 2)
  let ms = findMatches board0
      (b1, dead) = chipAdjacentCakes board0 ms
  assertEqual "no full clear yet" ([] :: [Pos]) dead
  assertEqual "layers 2->1" (1 :: Int) (cakeLayers (getCell b1 (4, 1)))
  let (_b2, dead2) = chipAdjacentCakes b1 ms
  assertEqual "now dead" [(4, 1)] dead2

cake_clears_at_zero :: Assertion
cake_clears_at_zero = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkCake
  assertBool "cake present" (isCake (getCell board0 (2, 1)))
  let seeds = findMatches board0
      CascadeRun {crBoard = board1, crTally = CascadeTally {ctCakes = cakes}} = cascadeSeeds Nothing seeds [] [] (mkStdGen 1) board0
  assertBool "cake cleared count" (cakes >= 1)
  assertBool "cake gone" (not (isCake (getCell board1 (2, 1))))

--------------------------------------------------------------------------------
-- Magic hat / 魔法帽 (adjacent trigger swaps neighbor colors)
--------------------------------------------------------------------------------

hat_triggered_by_adjacent :: Assertion
hat_triggered_by_adjacent = do
  -- Stones block side neighbors so only (1,1) gem remains for the hat
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell stableBoard (3, 0) (mkGem C1))
                         (3, 1)
                         (mkGem C1))
                      (3, 2)
                      (mkGem C1))
                   (2, 1)
                   mkMagicHat)
                (1, 1)
                (mkGem C2))
             (2, 0)
             mkStone)
          (2, 2)
          mkStone
  assertBool "hat present" (isMagicHat (getCell board0 (2, 1)))
  let ms = findMatches board0
      hats = hatsAdjacentTo board0 ms
  assertEqual "hat adjacent to match" [(2, 1)] hats
  let board1 = triggerAdjacentHats board0 ms
  assertBool "hat still there" (isMagicHat (getCell board1 (2, 1)))
  assertEqual "cycled neighbor" C3 (cellColor (getCell board1 (1, 1)))

hat_swaps_colors :: Assertion
hat_swaps_colors = do
  -- Hat at (2,1); match on row 3; stone above so only left/right gems swap
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell stableBoard (3, 0) (mkGem C1))
                         (3, 1)
                         (mkGem C1))
                      (3, 2)
                      (mkGem C1))
                   (2, 1)
                   mkMagicHat)
                (2, 0)
                (mkGem C4))
             (2, 2)
             (mkGem C5))
          (1, 1)
          mkStone
  let ms = findMatches board0
      cLeft0 = cellColor (getCell board0 (2, 0))
      cRight0 = cellColor (getCell board0 (2, 2))
  assertEqual "left before" C4 cLeft0
  assertEqual "right before" C5 cRight0
  let board1 = triggerAdjacentHats board0 ms
      cLeft1 = cellColor (getCell board1 (2, 0))
      cRight1 = cellColor (getCell board1 (2, 2))
  assertEqual "left got right" C5 cLeft1
  assertEqual "right got left" C4 cRight1
  assertBool "hat remains" (isMagicHat (getCell board1 (2, 1)))


--------------------------------------------------------------------------------
-- Maker / 果汁机 (same-color adjacent charge -> Bomb)
--------------------------------------------------------------------------------

maker_blocks_swap :: Assertion
maker_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkMaker C1)
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  assertBool "is maker" (isMaker (getCell board (3, 3)))
  assertEqual "default charges" (3 :: Int) (makerCharges (getCell board (3, 3)))

maker_charges_on_same_color :: Assertion
maker_charges_on_same_color = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkMakerCharges C1 3)
  let ms = findMatches board0
      b1 = chargeAdjacentMakers board0 ms
  assertBool "still maker" (isMaker (getCell b1 (2, 1)))
  assertEqual "charges 3->2" (2 :: Int) (makerCharges (getCell b1 (2, 1)))
  -- Wrong color adjacent clear does not charge
  let boardWrong =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C2))
                (3, 1)
                (mkGem C2))
             (3, 2)
             (mkGem C2))
          (2, 1)
          (mkMakerCharges C1 2)
      ms2 = findMatches boardWrong
      b2 = chargeAdjacentMakers boardWrong ms2
  assertEqual "wrong color no charge" (2 :: Int) (makerCharges (getCell b2 (2, 1)))

maker_produces_bomb :: Assertion
maker_produces_bomb = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkMakerCharges C1 1)
  let ms = findMatches board0
      b1 = chargeAdjacentMakers board0 ms
  assertBool "became gem" (isGem (getCell b1 (2, 1)))
  assertEqual "bomb kind" Bomb (cellKind (getCell b1 (2, 1)))
  assertEqual "bomb color" C1 (cellColor (getCell b1 (2, 1)))
  let gs = newGameAtLevel 26 (levelConfig (allLevels !! 26)) 42
      nMaker =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isMaker (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor makers >= 3, got " ++ show nMaker) (nMaker >= 3)
  assertBool "level has portals" (not (null (gsPortals gs)))

--------------------------------------------------------------------------------
-- Safe / 保险箱 (layered vault; opens into Cookie; GoalSafe)
--------------------------------------------------------------------------------

safe_blocks_swap :: Assertion
safe_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkSafe
  assertBool "is safe" (isSafe (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0

safe_opens_to_cookie :: Assertion
safe_opens_to_cookie = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkSafe
  assertBool "safe present" (isSafe (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, opened) = chipAdjacentSafes board0 ms
  assertEqual "opened" (1 :: Int) (length opened)
  assertBool "became cookie" (isCookie (getCell b1 (2, 1)))
  assertBool "not still safe" (not (isSafe (getCell b1 (2, 1))))
  -- Cascade path: open + cookie may fall/collect
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board0
          , gsScore = 0
          , gsMoves = 20
          , gsGoal = GoalSafe 1
          , gsSafesOpened = 0
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "goal progress" (gsSafesOpened gs1 >= 1)
  assertBool "safe gone from board" $
    not (any (\r -> any (\c -> isSafe (getCell (gsBoard gs1) (r, c))) [0 .. boardSize - 1]) [0 .. boardSize - 1])

safe_layer_decrement :: Assertion
safe_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkSafeLayers 2)
  let ms = findMatches board0
      (b1, opened) = chipAdjacentSafes board0 ms
  assertEqual "not open yet" (0 :: Int) (length opened)
  assertEqual "layers 2->1" (1 :: Int) (safeLayers (getCell b1 (4, 1)))
  let (b2, opened2) = chipAdjacentSafes b1 ms
  assertEqual "opened" (1 :: Int) (length opened2)
  assertBool "cookie" (isCookie (getCell b2 (4, 1)))

--------------------------------------------------------------------------------
-- Flip / 双面块 (matches as front; clear hit flips to back Normal gem)
--------------------------------------------------------------------------------

flip_matches_front :: Assertion
flip_matches_front = do
  let boardM =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFlip C1 C3))
          (3, 2)
          (mkGem C1)
  assertBool "flip participates" (not (null (findMatches boardM)))
  assertBool "is flip" (isFlip (getCell boardM (3, 1)))
  assertEqual "front" C1 (flipFront (getCell boardM (3, 1)))
  assertEqual "back" C3 (flipBack (getCell boardM (3, 1)))
  -- Can swap like a gem
  assertBool "not blocked" (not (swapBlockedWith defaultRegistry boardM (3, 1) (3, 3)))

flip_becomes_back_on_clear :: Assertion
flip_becomes_back_on_clear = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFlip C1 C4))
          (3, 2)
          (mkGem C1)
  assertBool "has match" (not (null (findMatches board0)))
  let ms = findMatches board0
      (b1, iceFree) = chipIceOnClear board0 ms
      cellAfter = getCell b1 (3, 1)
  assertBool "became gem" (isGem cellAfter)
  assertBool "not still flip" (not (isFlip cellAfter))
  assertEqual "back color" C4 (cellColor cellAfter)
  -- Flip stays on board (not listed as clearable hole)
  assertBool "flip not cleared away" ((3, 1) `notElem` iceFree)
  -- Full cascade also leaves a gem (possibly later matched as C4)
  let CascadeRun {crBoard = board1} = cascadeMatches Nothing [] [] (mkStdGen 2) board0
  assertBool "no flip remains at seed" (not (isFlip (getCell board1 (3, 1))))

--------------------------------------------------------------------------------
-- Surprise / 彩蛋 (adjacent open → special or 3×3 pop)
--------------------------------------------------------------------------------

surprise_blocks_swap :: Assertion
surprise_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkSurprise
  assertBool "is surprise" (isSurprise (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0

surprise_opens_to_special :: Assertion
surprise_opens_to_special = do
  -- (4,0): outcome 0 → LineH special
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 0)
          mkSurprise
  assertBool "surprise present" (isSurprise (getCell board0 (4, 0)))
  let ms = findMatches board0
      (b1, expl) = openAdjacentSurprises board0 ms
  assertEqual "no explode" (0 :: Int) (length expl)
  let cell = getCell b1 (4, 0)
  assertBool "became special gem" (isGem cell && cellKind cell /= Normal)
  assertEqual "LineH" LineH (cellKind cell)
  let gs = newGameAtLevel 32 (levelConfig (allLevels !! 32)) 42
      nSur =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isSurprise (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor surprises >= 8, got " ++ show nSur) (nSur >= 8)
  assertEqual "campaign levels" (38 :: Int) (length allLevels)

surprise_explodes_small :: Assertion
surprise_explodes_small = do
  -- (3,3): outcome 3 → 3×3 blast; ortho-adjacent to match at (3,2)
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (3, 3)
          mkSurprise
  let ms = findMatches board0
      (b1, expl) = openAdjacentSurprises board0 ms
  assertBool "explode seeds" (length expl >= 5)
  assertBool "center in blast" ((3, 3) `elem` expl)
  -- Still Surprise on board until clear holes applied
  assertBool "still surprise until clear" (isSurprise (getCell b1 (3, 3)))
  let gs0 =
        (newGame defaultConfig 11)
          { gsBoard = board0
          , gsScore = 0
          , gsMoves = 20
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "surprise cleared by blast" $
    not (isSurprise (getCell (gsBoard gs1) (3, 3)))

--------------------------------------------------------------------------------
-- Bottle / 染色瓶 (adjacent clear dyes ortho gems)
--------------------------------------------------------------------------------

bottle_blocks_swap :: Assertion
bottle_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkBottle C2)
  assertBool "is bottle" (isBottle (getCell board (3, 3)))
  assertEqual "color" C2 (bottleColor (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))

bottle_dyes_neighbors :: Assertion
bottle_dyes_neighbors = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             (mkBottle C3))
          (2, 2)
          (mkGem C5)
  let ms = findMatches board0
      b1 = triggerAdjacentBottles board0 ms
  assertBool "bottle stays" (isBottle (getCell b1 (2, 1)))
  assertEqual "dyed neighbor" C3 (cellColor (getCell b1 (2, 2)))
  let gs = newGameAtLevel 33 (levelConfig (allLevels !! 33)) 42
      nBot =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isBottle (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor bottles >= 6, got " ++ show nBot) (nBot >= 6)

--------------------------------------------------------------------------------
-- TimeSpirit / 时间精灵 (+2 moves) & Steam / 蒸汽 & carry bonus
--------------------------------------------------------------------------------

time_spirit_blocks_swap :: Assertion
time_spirit_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkTimeSpirit
  assertBool "is spirit" (isTimeSpirit (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))

time_spirit_awards_moves :: Assertion
time_spirit_awards_moves = do
  -- Form match via swap (0,2)<->(0,3): C1 C1 C2 C1 → C1 C1 C1 C2; spirit at (1,1) adjacent
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (0, 0) (mkGem C1))
                   (0, 1)
                   (mkGem C1))
                (0, 2)
                (mkGem C2))
             (0, 3)
             (mkGem C1))
          (1, 1)
          mkTimeSpirit
      gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board0
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "spirit cleared" (not (isTimeSpirit (getCell (gsBoard gs1) (1, 1))))
  -- spent 1 move, gained +2 → net +1 from 10 → 11
  assertEqual "moves +2 net" (11 :: Int) (gsMoves gs1)
  let gsL = newGameAtLevel 34 (levelConfig (allLevels !! 34)) 42
      nSp =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isTimeSpirit (getCell (gsBoard gsL) (r, c))
          ]
  assertBool ("decor spirits >= 6, got " ++ show nSp) (nSp >= 6)

-- | TimeSpirit adjacent-clear on the final move: -1+2 keeps the player alive.
time_spirit_rescues_last_move :: Assertion
time_spirit_rescues_last_move = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (0, 0) (mkGem C1))
                   (0, 1)
                   (mkGem C1))
                (0, 2)
                (mkGem C2))
             (0, 3)
             (mkGem C1))
          (1, 1)
          mkTimeSpirit
      gs0 =
        (newGame defaultConfig 8)
          { gsBoard = board0
          , gsMoves = 1
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    Lost _ -> assertFailure "spirit should rescue last move"
    _ -> pure ()
  assertBool "spirit gone" (not (isTimeSpirit (getCell (gsBoard gs1) (1, 1))))
  assertEqual "net +1 from last move" (2 :: Int) (gsMoves gs1)
  assertEqual "not over" Nothing (gsOver gs1)

-- | Flip at the natural 4-match spawn anchor: Line must still appear on a hole.
-- Locks Flip+Match: non-clearing Flip seeds must not swallow special spawns.
flip_four_match_spawns_line :: Assertion
flip_four_match_spawns_line = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkFlip C1 C4))
          (3, 3)
          (mkGem C1)
  assertEqual "4-run" (4 :: Int) (maximum (0 : map (length . runPos) (findMatchRuns board0)))
  let (mb, n) = clearMatches board0
  assertEqual "three holes (flip stays)" (3 :: Int) n
  assertBool "flip became back gem" $
    case (mb !! 3) !! 2 of
      Just c -> isGem c && cellColor c == C4 && cellKind c == Normal
      Nothing -> False
  let specials =
        [ (r, c, cellKind cell)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [((mb !! r) !! c)]
        , isGem cell
        , cellKind cell /= Normal
        ]
  assertBool ("expected Line spawn, got " ++ show specials) $
    any (\(_, _, k) -> k == LineH || k == LineV) specials
  -- ice>1 at middle is the same class of non-hole spawn anchor
  let boardIce =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (5, 0) (mkGem C2))
                (5, 1)
                (mkGem C2))
             (5, 2)
             (mkIceGem C2 2))
          (5, 3)
          (mkGem C2)
      (mbI, _) = clearMatches boardIce
      specsI =
        [ cellKind cell
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [((mbI !! r) !! c)]
        , isGem cell
        , cellKind cell /= Normal
        ]
  assertBool ("ice mid still spawns, got " ++ show specsI) (LineH `elem` specsI || LineV `elem` specsI)

-- | Surprise 3×3 blast that hits a Bomb must expand the Bomb (same as countdown blasts).
-- Locks Surprise+Cascade: explode seeds go through expandSpecials before chipIce.
surprise_blast_expands_bomb :: Assertion
surprise_blast_expands_bomb = do
  -- (3,3) surpriseOutcome == 3 → explode; Bomb at (2,3) sits inside the 3×3.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 3)
          (Gem C5 Bomb 0 Nothing)
  assertBool "match opens surprise" (not (null (findMatches board0)))
  let (mb, n) = clearMatches board0
      holes =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mb !! r) !! c) == Nothing
        ]
  assertBool "bomb cell cleared" ((2, 3) `elem` holes)
  -- Bomb at (2,3) expands to row1; (1,3) is outside surprise 3×3 alone.
  assertBool "bomb expansion reached (1,3)" ((1, 3) `elem` holes)
  assertBool "cleared well beyond match+surprise" (n >= 12)

-- | Maker charge-1 → Bomb in place; Bomb is not consumed by the producing wave.
-- Locks Maker+Bomb: produced Bomb sits until a later match/swap.
maker_bomb_survives_wave :: Assertion
maker_bomb_survives_wave = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkMakerCharges C1 1)
      gs0 =
        (newGame defaultConfig 11)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let cell = getCell (gsBoard gs1) (2, 1)
  -- trySwap path: Maker is gone (converted); Bomb may still sit or cascade-detonate.
  assertBool "maker converted away" (not (isMaker cell))
  -- Unit path: same-wave clearMatches leaves Bomb in place (not consumed by producing wave)
  let (mb, _) = clearMatches board0
  case (mb !! 2) !! 1 of
    Just c -> do
      assertBool "unit bomb gem" (isGem c)
      assertEqual "unit Bomb kind" Bomb (cellKind c)
      assertEqual "unit bomb color" C1 (cellColor c)
    Nothing -> assertFailure "maker cell must not hole"

-- | One match adjacent to Honey and same-color Balloon clears both in the wave.
-- Locks Honey+Balloon: independent adjacency rules share iceFree seeds.
honey_balloon_same_clear :: Assertion
honey_balloon_same_clear = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             mkHoney)
          (4, 1)
          (mkBalloon C1)
      gs0 =
        (newGame defaultConfig 12)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHoneyCleared = 0
          , gsBalloonsPopped = 0
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "honey counted" (gsHoneyCleared gs1 >= 1)
  assertBool "balloon counted" (gsBalloonsPopped gs1 >= 1)
  assertBool "no honey left" $
    not (any (\r -> any (\c -> isHoney (getCell (gsBoard gs1) (r, c))) [0 .. boardSize - 1]) [0 .. boardSize - 1])
  assertBool "no balloon left" $
    not (any (\r -> any (\c -> isBalloon (getCell (gsBoard gs1) (r, c))) [0 .. boardSize - 1]) [0 .. boardSize - 1])

-- | Safe on the bottom row opens to Cookie and collects in the same settle.
-- Locks Safe→Cookie: open-in-place + drainBottomCookies in one wave.
safe_bottom_cookie_collected :: Assertion
safe_bottom_cookie_collected = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (6, 0) (mkGem C1))
                (6, 1)
                (mkGem C1))
             (6, 2)
             (mkGem C1))
          (7, 1)
          mkSafe
      (mb, _) = clearMatches board0
  assertBool "opened to cookie pre-settle" $
    case (mb !! 7) !! 1 of
      Just Cookie -> True
      _ -> False
  let (settled, fallen, _) = settleBoardPortals [] mb
  assertEqual "cookie drained" (1 :: Int) fallen
  assertBool "bottom no longer cookie" $
    case (settled !! 7) !! 1 of
      Just Cookie -> False
      _ -> True
  let gs0 =
        (newGame defaultConfig 13)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsSafesOpened = 0
          , gsCookiesCollected = 0
          , gsGoal = GoalSafe 1
          }
      (gs1, out) = trySwap (6, 1) (6, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "safe opened" (gsSafesOpened gs1 >= 1)
  assertBool "cookie collected" (gsCookiesCollected gs1 >= 1)

-- | Dye bottle recolors neighbors mid-clear; a new match must cascade (not stall).
bottle_dye_followup_match :: Assertion
bottle_dye_followup_match = do
  let bSafe = boardFromRows $
        [ [mkGem (toEnum ((r * 3 + c) `mod` 5)) | c <- [0 .. 7]]
        | r <- [0 .. 7]
        ]
      board =
        foldl
          (\b (p, c) -> setCell b p c)
          bSafe
          [ ((6, 0), mkGem C1)
          , ((6, 1), mkGem C1)
          , ((6, 2), mkGem C1)
          , ((5, 1), mkBottle C3)
          , ((5, 0), mkGem C4)
          , ((5, 2), mkGem C4)
          , ((4, 2), mkGem C3)
          , ((3, 2), mkGem C3)
          ]
      ms = findMatches board
  assertBool "seed match includes row6" $
    all (`elem` ms) [(6, 0), (6, 1), (6, 2)]
  let dyed = triggerAdjacentBottles board ms
  assertEqual "dyed (5,2)" C3 (cellColor (getCell dyed (5, 2)))
  assertBool "dye created vertical C3" $
    all (`elem` findMatches dyed) [(3, 2), (4, 2), (5, 2)]
  let CascadeRun {crTally = CascadeTally {ctCells = cells, ctScore = scored, ctMaxWave = maxW}} = cascadeMatches Nothing [] [] (mkStdGen 42) board
  assertBool ("follow-up cascade cells>=6 got " ++ show cells) (cells >= 6)
  assertBool ("maxW>=2 got " ++ show maxW) (maxW >= 2)
  assertBool "scored" (scored >= scoreForWave 1 3 + scoreForWave 2 3)

-- | Magic hat recolor of neighbors can create a match; cascade must clear it.
hat_recolor_followup_match :: Assertion
hat_recolor_followup_match = do
  let bSafe = boardFromRows $
        [ [mkGem (toEnum ((r * 3 + c) `mod` 5)) | c <- [0 .. 7]]
        | r <- [0 .. 7]
        ]
      board =
        foldl
          (\b (p, c) -> setCell b p c)
          bSafe
          [ ((6, 0), mkGem C1)
          , ((6, 1), mkGem C1)
          , ((6, 2), mkGem C1)
          , ((5, 1), mkMagicHat)
          , ((4, 1), mkStone) -- only left/right gem neighbors
          , ((5, 0), mkGem C2)
          , ((5, 2), mkGem C3)
          , ((4, 0), mkGem C3)
          , ((3, 0), mkGem C3)
          ]
      ms = findMatches board
      hatted = triggerAdjacentHats board ms
  assertEqual "hat swapped left to C3" C3 (cellColor (getCell hatted (5, 0)))
  assertEqual "hat swapped right to C2" C2 (cellColor (getCell hatted (5, 2)))
  assertBool "hat created col0 C3 match" $
    all (`elem` findMatches hatted) [(3, 0), (4, 0), (5, 0)]
  let CascadeRun {crTally = CascadeTally {ctCells = cells, ctScore = scored, ctMaxWave = maxW}} = cascadeMatches Nothing [] [] (mkStdGen 7) board
  assertBool ("hat follow-up cells>=6 got " ++ show cells) (cells >= 6)
  assertBool ("maxW>=2 got " ++ show maxW) (maxW >= 2)
  let CascadeRun {crBoard = bAfter} = cascadeMatches Nothing [] [] (mkStdGen 7) board
      hatLeft =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isMagicHat (getCell bAfter (r, c))
        ]
  -- Hat is gravity-fixed (immortal class); stays put through clears below.
  assertEqual "hat still on board" (1 :: Int) (length hatLeft)
  assertEqual "hat stayed put" [(5, 1)] hatLeft
  assertBool "scored" (scored >= scoreForWave 1 3)


-- | Three same-color adjacent clears in one wave charge a maker only once.
maker_multi_adjacent_charges_once :: Assertion
maker_multi_adjacent_charges_once = do
  let board =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (2, 2) (mkMakerCharges C1 2))
                (2, 1)
                (mkGem C1))
             (2, 3)
             (mkGem C1))
          (1, 2)
          (mkGem C1)
      cleared = [(2, 1), (2, 3), (1, 2)]
      adj = makersAdjacentSameColor board cleared
  assertEqual "nub single maker" [(2, 2)] adj
  let b1 = chargeAdjacentMakers board cleared
  assertBool "still maker" (isMaker (getCell b1 (2, 2)))
  assertEqual "2->1 once" (1 :: Int) (makerCharges (getCell b1 (2, 2)))
  let bLow = setCell board (2, 2) (mkMakerCharges C1 1)
      bBomb = chargeAdjacentMakers bLow cleared
  assertBool "became bomb" (cellKind (getCell bBomb (2, 2)) == Bomb)
  assertBool "not maker" (not (isMaker (getCell bBomb (2, 2))))

--------------------------------------------------------------------------------
-- Surprise direct-seed open (hammer / cross): special survives; explode blasts
--------------------------------------------------------------------------------

-- | Direct-hit Surprise must open like adjacent open — not spawn-then-hole or
-- single-cell delete. Seed cascade keeps special; explode hammers 3×3.
surprise_direct_seed_opens :: Assertion
surprise_direct_seed_opens = do
  -- Unit: openSurprises on a direct-seed iceFree list saves special-outcome cells.
  let boardSpecial = setCell stableBoard (4, 0) mkSurprise
      (bOpen, expl0, saved0) = openSurprises boardSpecial [(4, 0)]
  assertEqual "no explode for outcome 0" (0 :: Int) (length expl0)
  assertEqual "saved special cell" [(4, 0)] saved0
  let cellU = getCell bOpen (4, 0)
  assertBool "opened to special" (isGem cellU && cellKind cellU /= Normal)
  assertEqual "LineH" LineH (cellKind cellU)
  -- Seed cascade (hammer path): special sits; not dug by iceFree hole.
  let g0 = mkStdGen 11
      CascadeRun {crBoard = bCas, crTally = CascadeTally {ctCells = nCleared}} = cascadeSeeds Nothing [(4, 0)] [] [] g0 boardSpecial
  assertEqual "special open clears no hole" (0 :: Int) nCleared
  let cellC = getCell bCas (4, 0)
  assertBool "cascade kept special" (isGem cellC && cellKind cellC /= Normal)
  assertEqual "cascade LineH" LineH (cellKind cellC)
  -- Direct-hit alongside another seed: special is saved (not spawn-then-hole).
  let (bOpen2, _, saved2) = openSurprises boardSpecial [(4, 0), (4, 1)]
  assertBool "saved when co-seeded" ((4, 0) `elem` saved2)
  assertEqual "still LineH when co-seeded" LineH (cellKind (getCell bOpen2 (4, 0)))
  -- (3,3) explode-outcome: hammer 3×3 scores >= 90 (single-cell would be 10).
  let boardBoom = setCell stableBoard (3, 3) mkSurprise
      gsB0 =
        (newGame defaultConfig 11)
          { gsBoard = boardBoom
          , gsHammers = 2
          , gsMoves = 20
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gsB1, outB) = useHammer (3, 3) gsB0
  case outB of
    NoMatch -> assertFailure "hammer explode surprise should apply"
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsB1)
  assertBool "surprise gone after explode" $
    not (isSurprise (getCell (gsBoard gsB1) (3, 3)))
  assertBool ("explode score >= 90, got " ++ show (gsScore gsB1)) (gsScore gsB1 >= 90)


-- | Surprise 3×3 explode must peel/chip adjacent obstacles like Bomb (trueClears).
-- Regression: surpFree used to skip stone/fog/chain/freeze/curtain/honey peels.
surprise_blast_peels_adjacent :: Assertion
surprise_blast_peels_adjacent = do
  -- (3,3) outcome 3 → explode; obstacles sit just outside the 3×3 blast.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell
                               (setCell stableBoard (3, 0) (mkGem C1))
                               (3, 1)
                               (mkGem C1))
                            (3, 2)
                            (mkGem C1))
                         (3, 3)
                         mkSurprise)
                      (2, 5)
                      (mkStoneLayers 2))
                   (1, 4)
                   (mkFogGem C4 2))
                (4, 5)
                (Gem C3 Normal 0 (Just (Chain 2))))
             (5, 3)
             (Gem C2 Normal 0 (Just (Freeze 2))))
          (3, 5)
          (Gem C1 Normal 0 (Just (Curtain 2)))
  assertEqual "explode outcome" (3 :: Int) (((3 * 8 + 3) `mod` 4))
  let (mb, _) = clearMatches board0
      stone = case (mb !! 2) !! 5 of
        Just c -> c
        Nothing -> error "stone must remain"
      fog = case (mb !! 1) !! 4 of
        Just c -> c
        Nothing -> error "fog gem must remain"
      chain = case (mb !! 4) !! 5 of
        Just c -> c
        Nothing -> error "chain gem must remain"
      freeze = case (mb !! 5) !! 3 of
        Just c -> c
        Nothing -> error "freeze gem must remain"
      curtain = case (mb !! 3) !! 5 of
        Just c -> c
        Nothing -> error "curtain gem must remain"
  assertEqual "stone chipped 2→1" (1 :: Int) (stoneLayers stone)
  assertEqual "fog peeled 2→1" (Just (Fog 1)) (cellOverlay fog)
  assertEqual "chain peeled 2→1" (Just (Chain 1)) (cellOverlay chain)
  assertEqual "freeze peeled 2→1" (Just (Freeze 1)) (cellOverlay freeze)
  assertEqual "curtain peeled 2→1" (Just (Curtain 1)) (cellOverlay curtain)
  -- Honey outside blast, ortho-adjacent to a blast cell, chips one layer.
  let boardH =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 5)
          (mkHoneyLayers 2)
      (mbH, _) = clearMatches boardH
  case (mbH !! 2) !! 5 of
    Just h -> assertEqual "honey chipped 2→1" (1 :: Int) (honeyLayers h)
    Nothing -> assertFailure "honey must remain (not last layer)"
  -- Control: Bomb in-match 3×3 still peels the same way (parity sanity).
  let boardB =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (Gem C1 Bomb 0 Nothing))
          (2, 4)
          (mkStoneLayers 2)
      (mbB, _) = clearMatches boardB
  case (mbB !! 2) !! 4 of
    Just s -> assertEqual "bomb still chips stone" (1 :: Int) (stoneLayers s)
    Nothing -> assertFailure "bomb stone must remain"

--------------------------------------------------------------------------------
-- Line/Bomb/Hammer direct hit must single-chip Chest/Honey/Cake (Stone parity)
--------------------------------------------------------------------------------

-- | Multi-layer Chest/Honey/Cake on a Line blast path (or hammer seed) must chip
-- one layer — not fully wipe. Regression: chipIceOnClear listed them clearable
-- regardless of layers while Stone/Safe correctly decremented, so LineH/Bomb/
-- Hammer erased Honey2/Chest2/Cake2 in one hit (Except peels were moot).
blast_chips_layered_obstacles_once :: Assertion
blast_chips_layered_obstacles_once = do
  let lineBoard lock =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell stableBoard (3, 0) (mkGem C1))
                            (3, 1)
                            (Gem C1 LineH 0 Nothing))
                         (3, 2)
                         (mkGem C1))
                      (3, 3)
                      (mkGem C3))
                   (3, 4)
                   (mkGem C4))
                (3, 5)
                lock)
             (3, 6)
             (mkGem C5))
          (3, 7)
          (mkGem C3)
  assertBool "line match" (not (null (findMatches (lineBoard (mkGem C2)))))
  -- Honey 2 on blast path: chip once → Honey 1 (not removed).
  let CascadeRun {crBoard = bH} = cascadeMatches Nothing [] [] (mkStdGen 51) (lineBoard (mkHoneyLayers 2))
      cellH = getCell bH (3, 5)
  assertBool "honey survives" (isHoney cellH)
  assertEqual "honey chipped once 2→1" (1 :: Int) (honeyLayers cellH)
  -- Chest 2: same single chip.
  let CascadeRun {crBoard = bC} = cascadeMatches Nothing [] [] (mkStdGen 52) (lineBoard (mkChestLayers 2))
      cellC = getCell bC (3, 5)
  assertBool "chest survives" (isChest cellC)
  assertEqual "chest chipped once 2→1" (1 :: Int) (chestLayers cellC)
  -- Cake 2: same single chip.
  let CascadeRun {crBoard = bK} = cascadeMatches Nothing [] [] (mkStdGen 53) (lineBoard (mkCakeLayers 2))
      cellK = getCell bK (3, 5)
  assertBool "cake survives" (isCake cellK)
  assertEqual "cake chipped once 2→1" (1 :: Int) (cakeLayers cellK)
  -- Control: Honey 1 on path fully clears (last layer).
  let CascadeRun {crBoard = bH1, crTally = CascadeTally {ctHoney = honeyHit}} = cascadeMatches Nothing [] [] (mkStdGen 54) (lineBoard (mkHoneyLayers 1))
  assertBool "honey1 cleared" (not (isHoney (getCell bH1 (3, 5))))
  assertBool "honey1 counted" (honeyHit >= 1)
  -- Hammer on Honey 3: chip 3→2, charge spent, not goal-counted yet.
  let boardHam = setCell stableBoard (5, 5) (mkHoneyLayers 3)
      gs0 =
        (newGame defaultConfig 12)
          { gsBoard = boardHam
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalHoney 8
          , gsMoves = 20
          , gsScore = 0
          , gsHoneyCleared = 0
          }
      (gs1, outH) = useHammer (5, 5) gs0
  case outH of
    NoMatch -> assertFailure "hammer chip should apply"
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cellHam = getCell (gsBoard gs1) (5, 5)
  assertBool "honey remains after hammer" (isHoney cellHam)
  assertEqual "hammer chips 3→2" (2 :: Int) (honeyLayers cellHam)
  assertEqual "not counted until last" (0 :: Int) (gsHoneyCleared gs1)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gs1)
  -- chipIceOnClear unit: Chest2/Cake2 not clearable; layers decremented.
  let (bChest, freeChest) = chipIceOnClear (setCell stableBoard (1, 1) (mkChestLayers 2)) [(1, 1)]
      (bCake, freeCake) = chipIceOnClear (setCell stableBoard (2, 2) (mkCakeLayers 2)) [(2, 2)]
  assertEqual "chest2 not clearable" ([] :: [Pos]) freeChest
  assertEqual "chest 2→1" (1 :: Int) (chestLayers (getCell bChest (1, 1)))
  assertEqual "cake2 not clearable" ([] :: [Pos]) freeCake
  assertEqual "cake 2→1" (1 :: Int) (cakeLayers (getCell bCake (2, 2)))

--------------------------------------------------------------------------------
-- MagicHat survives Line/Bomb/Hammer direct seeds (Maker/Bottle parity)
--------------------------------------------------------------------------------

-- | Hats only recolor via adjacent clears and must stay on the board. chipIceOnClear
-- used to list MagicHat as clearable, so LineH/Bomb/Hammer/Cross wiped hats without
-- a useful trigger (row neighbors already in the clear set). Align with Maker /
-- Snail / Bottle immunity; hammer rejects without spending. Adjacent trigger still
-- works. Regression: hat_immune_to_direct_clear.
hat_immune_to_direct_clear :: Assertion
hat_immune_to_direct_clear = do
  -- Unit: direct seed does not mark hat clearable; cell unchanged.
  let (bU, freeU) = chipIceOnClear (setCell stableBoard (2, 2) mkMagicHat) [(2, 2)]
  assertEqual "hat not clearable" ([] :: [Pos]) freeU
  assertBool "hat stays on chipIce" (isMagicHat (getCell bU (2, 2)))
  -- LineH blast through a hat: hat survives (Maker parity).
  let lineBoard lock =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell stableBoard (3, 0) (mkGem C1))
                            (3, 1)
                            (Gem C1 LineH 0 Nothing))
                         (3, 2)
                         (mkGem C1))
                      (3, 3)
                      (mkGem C3))
                   (3, 4)
                   (mkGem C4))
                (3, 5)
                lock)
             (3, 6)
             (mkGem C5))
          (3, 7)
          (mkGem C3)
  assertBool "line match" (not (null (findMatches (lineBoard (mkGem C2)))))
  let CascadeRun {crBoard = bLine} = cascadeMatches Nothing [] [] (mkStdGen 61) (lineBoard mkMagicHat)
  assertBool "hat survives line blast" (isMagicHat (getCell bLine (3, 5)))
  -- Hammer on hat: NoMatch, charge kept, board unchanged.
  let gs0 =
        (newGame defaultConfig 12)
          { gsBoard = setCell stableBoard (4, 4) mkMagicHat
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
      (gs1, outH) = useHammer (4, 4) gs0
  outH @?= NoMatch
  assertEqual "hammer not spent" (2 :: Int) (gsHammers gs1)
  assertBool "hat remains after hammer" (isMagicHat (getCell (gsBoard gs1) (4, 4)))
  -- Adjacent clear still triggers hat (recolor) and hat stays.
  let boardAdj =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (2, 0) (mkGem C1))
                (2, 1)
                (mkGem C1))
             (2, 2)
             (mkGem C1))
          (1, 1)
          mkMagicHat
      ms = [(2, 0), (2, 1), (2, 2)]
  assertBool "hat present" (isMagicHat (getCell boardAdj (1, 1)))
  let boardTrig = triggerAdjacentHats boardAdj ms
  assertBool "hat still after adj trigger" (isMagicHat (getCell boardTrig (1, 1)))

--------------------------------------------------------------------------------
-- Surprise explode must open nested Surprises (Bomb parity)
--------------------------------------------------------------------------------

-- | Surprise 3×3 explode that hits another Surprise must open it (special or
-- chained explode) — not hole-delete via chipIce alone. Bomb→Surprise already
-- opened; Surprise→Surprise lacked the second openSurprises pass.
surprise_blast_opens_nested :: Assertion
surprise_blast_opens_nested = do
  -- (3,3) outcome 3 → explode; (2,2) outcome 2 → Bomb special inside the 3×3.
  let boardSpecial =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 2)
          mkSurprise
  assertEqual "outer explode" (3 :: Int) (((3 * 8 + 3) `mod` 4))
  assertEqual "nested special" (2 :: Int) (((2 * 8 + 2) `mod` 4))
  let (mbS, _) = clearMatches boardSpecial
  case (mbS !! 2) !! 2 of
    Just c -> do
      assertBool "nested opened to gem" (isGem c)
      assertEqual "nested Bomb special" Bomb (cellKind c)
      assertBool "not still Surprise" (not (isSurprise c))
    Nothing -> assertFailure "nested Surprise must open to special, not hole-delete"
  -- Same-pass special must sit (Bomb parity): must NOT fire-and-survive.
  -- Outer explode footprint is (2..4,2..4); Bomb@ (2,2) firing would hole (1,1).
  let holesS =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mbS !! r) !! c) == Nothing
        ]
  assertBool "nested Bomb must not fire (1,1)" ((1, 1) `notElem` holesS)
  assertBool "nested Bomb must not fire (2,1)" ((2, 1) `notElem` holesS)
  -- Control: Bomb match hitting Surprise at (2,2) also opens (parity sanity).
  let boardBomb =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (Gem C1 Bomb 0 Nothing))
          (2, 2)
          mkSurprise
      (mbB, _) = clearMatches boardBomb
  case (mbB !! 2) !! 2 of
    Just c -> assertEqual "bomb-hit nested Bomb" Bomb (cellKind c)
    Nothing -> assertFailure "bomb-hit Surprise must open"
  -- Nested explode at (2,3): chain must reach (1,3) outside outer 3×3 alone.
  let boardChain =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 3)
          mkSurprise
  assertEqual "nested explode" (3 :: Int) (((2 * 8 + 3) `mod` 4))
  let (mbC, nC) = clearMatches boardChain
      holesC =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mbC !! r) !! c) == Nothing
        ]
  assertBool "nested explode center cleared" ((2, 3) `elem` holesC)
  assertBool "chained blast reached (1,3)" ((1, 3) `elem` holesC)
  assertBool ("chained clear count >= 12, got " ++ show nC) (nC >= 12)

--------------------------------------------------------------------------------
-- Nested Surprise special must sit (not fire-and-survive)
--------------------------------------------------------------------------------

-- | When an exploding Surprise and a special-outcome Surprise open in the same
-- openSurprises pass (both ortho-adjacent to the match), expandSpecials must
-- use the pre-open board so the newly placed Bomb/Line does not activate while
-- also being saved from holes. Pre-existing Bombs in the blast still expand.
surprise_nested_special_no_fire :: Assertion
surprise_nested_special_no_fire = do
  -- (3,3) explode + (2,2) Bomb special, both ortho-adj to match row.
  let board =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 2)
          mkSurprise
  assertEqual "outer explode" (3 :: Int) (((3 * 8 + 3) `mod` 4))
  assertEqual "nested Bomb" (2 :: Int) (((2 * 8 + 2) `mod` 4))
  let (mb, n) = clearMatches board
      holes =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mb !! r) !! c) == Nothing
        ]
  case (mb !! 2) !! 2 of
    Just c -> assertEqual "special sits" Bomb (cellKind c)
    Nothing -> assertFailure "nested special must survive"
  assertBool "no fire beyond outer blast (1,1)" ((1, 1) `notElem` holes)
  assertBool "no fire beyond outer blast (1,2)" ((1, 2) `notElem` holes)
  assertBool "no fire beyond outer blast (2,1)" ((2, 1) `notElem` holes)
  -- Outer explode + match still clear the expected footprint.
  assertBool "outer center cleared" ((3, 3) `elem` holes)
  assertBool ("clear count in outer range, got " ++ show n) (n >= 9 && n <= 12)
  -- Control: pre-existing Bomb inside Surprise explode MUST still expand.
  let boardPreBomb =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 2)
          (Gem C4 Bomb 0 Nothing)
      (mbP, _) = clearMatches boardPreBomb
      holesP =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mbP !! r) !! c) == Nothing
        ]
  assertBool "pre-existing Bomb fires (1,1)" ((1, 1) `elem` holesP)
  assertBool "pre-existing Bomb consumed" (((mbP !! 2) !! 2) == Nothing)

--------------------------------------------------------------------------------
-- Surprise-opened special must sit through same-wave Hat / Bottle
--------------------------------------------------------------------------------

-- | Surprise opens to a special *before* Hat/Bottle side-effects. That special
-- must not be recolored/swapped same wave (sit until next move — same discipline
-- as maker_bomb_survives_wave / surprise_nested_special_no_fire).
-- Regression: Hat/Bottle skipped only trueClears, so a special ortho to both the
-- clear and a Hat/Bottle was mutated in-place (e.g. C4 Bomb → C3 Bomb by Bottle).
surprise_special_sits_hat_bottle :: Assertion
surprise_special_sits_hat_bottle = do
  -- (2,2) outcome 2 → Bomb C4; match row (3,1..3) triggers Surprise + Bottle/Hat
  assertEqual "special outcome" (2 :: Int) (((2 * 8 + 2) `mod` 4))
  let matchRow =
        setCell
          (setCell
             (setCell stableBoard (3, 1) (mkGem C1))
             (3, 2)
             (mkGem C1))
          (3, 3)
          (mkGem C1)
      expectedSpecial = Gem C4 Bomb 0 Nothing
  -- Unit: Bottle would dye (2,2) without protection
  let boardBot =
        setCell (setCell matchRow (2, 2) mkSurprise) (2, 3) (mkBottle C3)
      clears = [(3, 1), (3, 2), (3, 3)]
      (bOpen, _, saved) = openSurprises boardBot clears
  assertEqual "saved special pos" [(2, 2)] saved
  assertEqual "opened Bomb C4" expectedSpecial (getCell bOpen (2, 2))
  let dyedBare = triggerAdjacentBottles bOpen clears
  assertEqual "unprotected Bottle dyes special" C3 (cellColor (getCell dyedBare (2, 2)))
  let dyedProt = triggerAdjacentBottlesExcept bOpen clears saved
  assertEqual "protected Bottle skips special" expectedSpecial (getCell dyedProt (2, 2))
  -- Unit: Hat would swap special color without protection
  let boardHat =
        setCell
          (setCell
             (setCell matchRow (2, 2) mkSurprise)
             (2, 3)
             mkMagicHat)
          (1, 3)
          (mkGem C1)
      (hOpen, _, hSaved) = openSurprises boardHat clears
      hattedBare = triggerAdjacentHats hOpen clears
      hattedProt = triggerAdjacentHatsExcept hOpen clears hSaved
  assertEqual "unprotected Hat recolors special" C1 (cellColor (getCell hattedBare (2, 2)))
  assertEqual "protected Hat skips special" expectedSpecial (getCell hattedProt (2, 2))
  -- Integration: clearMatches keeps opened special through Hat+Bottle.
  -- Hat (2,1) with stones so only neighbor is Surprise special → would cycleColor;
  -- Bottle (2,3) would dye special to C3. Both must leave C4 Bomb intact.
  let boardBoth =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell matchRow (2, 2) mkSurprise)
                      (2, 3)
                      (mkBottle C3))
                   (2, 1)
                   mkMagicHat)
                (2, 0)
                mkStone)
             (1, 1)
             mkStone)
          (1, 2)
          (mkGem C5)
      (mb, _) = clearMatches boardBoth
  case (mb !! 2) !! 2 of
    Just c -> do
      assertEqual "cascade keeps Bomb kind" Bomb (cellKind c)
      assertEqual "cascade keeps special color (not Bottle/Hat)" C4 (cellColor c)
    Nothing -> assertFailure "Surprise special must sit, not hole"

-- | Maker-produced Bomb must sit through same-wave Bottle dye (Surprise special
-- parity). Regression: chargeAdjacentMakers ran before Bottle, and surpSaved did
-- not cover Maker bomb sites, so Bottle recolored C1 Bomb → C3 Bomb in-place.
maker_bomb_sits_bottle :: Assertion
maker_bomb_sits_bottle = do
  -- Match row (3,0..2) C1; Maker C1@1 at (2,1) → Bomb; Bottle C3 at (2,2) ortho.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             (mkMakerCharges C1 1))
          (2, 2)
          (mkBottle C3)
      clears = [(3, 0), (3, 1), (3, 2)]
  -- Unit: unprotected Bottle dyes freshly produced Maker Bomb
  let (bCharged, saved) = chargeAdjacentMakersSit board0 clears
  assertEqual "maker bomb sites" [(2, 1)] saved
  assertEqual "produced C1 Bomb" (Gem C1 Bomb 0 Nothing) (getCell bCharged (2, 1))
  let dyedBare = triggerAdjacentBottles bCharged clears
  assertEqual "unprotected Bottle dyes maker bomb" C3 (cellColor (getCell dyedBare (2, 1)))
  let dyedProt = triggerAdjacentBottlesExcept bCharged clears saved
  assertEqual "protected Bottle skips maker bomb" (Gem C1 Bomb 0 Nothing) (getCell dyedProt (2, 1))
  -- Integration: clearMatches keeps Maker Bomb color through Bottle
  let (mb, _) = clearMatches board0
  case (mb !! 2) !! 1 of
    Just c -> do
      assertEqual "cascade keeps Bomb kind" Bomb (cellKind c)
      assertEqual "cascade keeps maker color (not Bottle)" C1 (cellColor c)
    Nothing -> assertFailure "Maker Bomb must sit, not hole"

-- | Cookie mid-board is immune to Line/Bomb/Hammer direct seeds: must not wipe
-- or count toward GoalCookie. Cookies only collect via bottom-row drain.
-- Regression: chipIceOnClear listed Cookie clearable → blast "collected" mid-air.
cookie_immune_to_direct_clear :: Assertion
cookie_immune_to_direct_clear = do
  -- Unit: chipIceOnClear leaves Cookie, not clearable
  let boardC = setCell stableBoard (3, 3) mkCookie
      (bIce, free) = chipIceOnClear boardC [(3, 3)]
  assertEqual "cookie not clearable" ([] :: [Pos]) free
  assertBool "cookie persists after chipIce" (isCookie (getCell bIce (3, 3)))
  -- Bomb footprint covers Cookie: cookie survives; GoalCookie count stays 0
  let boardBomb =
        setCell
          (setCell stableBoard (3, 3) mkCookie)
          (3, 4)
          (Gem C1 Bomb 0 Nothing)
      seeds = [(3, 4)]
      CascadeRun {crBoard = b1, crTally = CascadeTally {ctCookies = cookies}} = cascadeSeeds Nothing seeds [] [] (mkStdGen 1) boardBomb
      cookieLeft =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCookie (getCell b1 (r, c))
        ]
  assertEqual "bomb must not count mid-board cookie" (0 :: Int) cookies
  assertBool ("cookie still on board after bomb, left=" ++ show cookieLeft) (not (null cookieLeft))
  -- LineH through cookie: same immunity
  let boardLine =
        setCell
          (setCell stableBoard (4, 2) mkCookie)
          (4, 4)
          (Gem C2 LineH 0 Nothing)
      CascadeRun {crBoard = bL, crTally = CascadeTally {ctCookies = cookiesL}} = cascadeSeeds Nothing [(4, 4)] [] [] (mkStdGen 2) boardLine
  assertEqual "line must not count mid-board cookie" (0 :: Int) cookiesL
  assertBool "cookie survives line" $
    any (\(r, c) -> isCookie (getCell bL (r, c)))
      [ (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] ]
  -- trySwap bomb path: GoalCookie meter unchanged when cookie not at bottom
  let gs0 =
        (newGame defaultConfig 21)
          { gsBoard = boardBomb
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsPortals = []
          , gsCookiesCollected = 0
          , gsGoal = GoalCookie 5
          , gsHammers = 2
          }
      -- Force bomb activation via useHammer on bomb cell (cookie neighbor)
      (gsH, outH) = useHammer (3, 4) gs0
  case outH of
    NoMatch -> assertFailure "hammer on bomb should apply"
    InvalidSwap -> assertFailure "hammer on bomb should apply"
    _ -> pure ()
  assertEqual "bomb hammer must not collect mid cookie" (0 :: Int) (gsCookiesCollected gsH)
  assertBool "cookie still present after bomb hammer" $
    any (\(r, c) -> isCookie (getCell (gsBoard gsH) (r, c)))
      [ (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] ]
  -- Control: bottom-row cookie still drains on unrelated clear
  let bottom = boardSize - 1
      boardBot =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (bottom, 4) mkCookie)
                (2, 0)
                (mkGem C1))
             (2, 1)
             (mkGem C1))
          (2, 2)
          (mkGem C1)
      seedsBot = findMatches boardBot
      CascadeRun {crTally = CascadeTally {ctCookies = cookiesBot}} = cascadeSeeds Nothing seedsBot [] [] (mkStdGen 3) boardBot
  assertBool ("bottom cookie still drains, got " ++ show cookiesBot) (cookiesBot >= 1)
