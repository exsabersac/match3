{-# LANGUAGE ScopedTypeVariables #-}

-- | 冰层与叠层（对应 Element/Builtin/Layer）：冰、草、藤、巧克力、迷雾、锁链、冰冻、窗帘、蒸汽，以及软命中不伤同格叠层。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Builtin.Layer
  ( tests
  ) where

import Match3.Board.Default (cascadeMatches, cascadeSeeds, clearMatches)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crBoard))
import Match3.Core
import Match3.Board.Grid (atM)
import Match3.Element (defaultRegistry)
import Match3.Element.Registry (swapBlockedWith)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "ice_layer_blocks_clear" ice_layer_blocks_clear
  , testCase "ice_layer_chips_then_clears" ice_layer_chips_then_clears
  , testCase "grass_cleared_by_match_above" grass_cleared_by_match_above
  , testCase "vine_spreads_after_move" vine_spreads_after_move
  , testCase "vine_blocked_by_clear" vine_blocked_by_clear
  , testCase "choco_spreads_after_move" choco_spreads_after_move
  , testCase "choco_cleared_by_adjacent" choco_cleared_by_adjacent
  , testCase "choco_blocked_by_clear" choco_blocked_by_clear
  , testCase "fog_blocks_match" fog_blocks_match
  , testCase "fog_cleared_by_adjacent" fog_cleared_by_adjacent
  , testCase "fog_layer_decrement" fog_layer_decrement
  , testCase "chain_blocks_match" chain_blocks_match
  , testCase "chain_blocks_swap" chain_blocks_swap
  , testCase "chain_cleared_by_adjacent" chain_cleared_by_adjacent
  , testCase "chain_layer_decrement" chain_layer_decrement
  , testCase "freeze_blocks_swap" freeze_blocks_swap
  , testCase "freeze_cleared_by_adjacent" freeze_cleared_by_adjacent
  , testCase "freeze_layer_decrement" freeze_layer_decrement
  , testCase "curtain_blocks_match" curtain_blocks_match
  , testCase "curtain_cleared_by_adjacent" curtain_cleared_by_adjacent
  , testCase "curtain_layer_decrement" curtain_layer_decrement
  , testCase "steam_blocks_match" steam_blocks_match
  , testCase "steam_cleared_by_adjacent" steam_cleared_by_adjacent
  , testCase "steam_spreads_after_move" steam_spreads_after_move
  , testCase "chain_freeze_both_peel" chain_freeze_both_peel
  , testCase "curtain_allows_swap_blocks_match" curtain_allows_swap_blocks_match
  , testCase "freeze_blocks_freeswap_and_swap" freeze_blocks_freeswap_and_swap
  , testCase "soft_hit_preserves_choco_steam" soft_hit_preserves_choco_steam
  , testCase "soft_hit_preserves_oncell_overlays" soft_hit_preserves_oncell_overlays
  , testCase "soft_hit_no_adj_side_effects" soft_hit_no_adj_side_effects
  , testCase "soft_hit_preserves_oncell_fog_steam" soft_hit_preserves_oncell_fog_steam
  ]

--------------------------------------------------------------------------------
-- Ice layers (开心消消乐冰层)
--------------------------------------------------------------------------------

-- | ice>=2: match chips one layer and gem stays; ice-free neighbors clear.
ice_layer_blocks_clear :: Assertion
ice_layer_blocks_clear = do
  let board =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkIceGem C1 2))
          (3, 2)
          (mkGem C1)
  assertBool "has match" (hasAnyMatch board)
  let (b1, free) = chipIceOnClear board [(3, 0), (3, 1), (3, 2)]
  assertBool "iced pos not free" ((3, 1) `notElem` free)
  assertBool "plain gems free" ((3, 0) `elem` free && (3, 2) `elem` free)
  assertEqual "ice 2 -> 1" (1 :: Int) (iceLayers (getCell b1 (3, 1)))
  assertBool "gem still there" (isGem (getCell b1 (3, 1)))

-- | ice-2 chips to ice-1 (stays); next chip (last layer) clears the gem.
ice_layer_chips_then_clears :: Assertion
ice_layer_chips_then_clears = do
  let board = setCell stableBoard (1, 1) (mkIceGem C4 2)
      (b1, free1) = chipIceOnClear board [(1, 1)]
  assertBool "still blocked" ((1, 1) `notElem` free1)
  assertEqual "2 -> 1" (1 :: Int) (iceLayers (getCell b1 (1, 1)))
  let (_b2, free2) = chipIceOnClear b1 [(1, 1)]
  assertBool "last ice clears gem" ((1, 1) `elem` free2)
  -- ice-1 alone also clears in one chip
  let (_b3, free3) = chipIceOnClear (setCell stableBoard (2, 2) (mkIceGem C3 1)) [(2, 2)]
  assertBool "ice-1 clears immediately" ((2, 2) `elem` free3)

--------------------------------------------------------------------------------
-- Grass / Vine overlays (开心消消乐草·藤蔓)
--------------------------------------------------------------------------------

-- | Match on a grass-covered cell clears the Grass overlay (gem may stay if iced).
grass_cleared_by_match_above :: Assertion
grass_cleared_by_match_above = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 Normal 2 (Just Grass)))
          (3, 2)
          (mkGem C1)
  assertBool "grass present" (hasGrass (getCell board0 (3, 1)))
  assertBool "has match" (hasAnyMatch board0)
  -- Unit: clearOverlaysOn strips grass on match seeds
  let stripped = clearOverlaysOn board0 [(3, 0), (3, 1), (3, 2)]
  assertBool "grass cleared by match" (not (hasGrass (getCell stripped (3, 1))))
  assertEqual "ice untouched by overlay clear" (2 :: Int) (iceLayers (getCell stripped (3, 1)))
  -- Match path: strip overlays then chip ice → grass gone, ice 2→1, gem stays
  let ms = findMatches board0
      b1 = clearOverlaysOn board0 ms
      (b2, free) = chipIceOnClear b1 ms
  assertBool "grass gone after match path" (not (hasGrass (getCell b2 (3, 1))))
  assertEqual "ice 2->1" (1 :: Int) (iceLayers (getCell b2 (3, 1)))
  assertBool "iced gem not free yet" ((3, 1) `notElem` free)

-- | Surviving vines spread onto adjacent bare gems after a successful move.
vine_spreads_after_move :: Assertion
vine_spreads_after_move = do
  -- Vine at (5,5); bare neighbors. Match on row 0 so vine survives.
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
          (5, 5)
          (mkVineGem C4)
  assertBool "vine placed" (hasVine (getCell board0 (5, 5)))
  assertBool "neighbor bare" (cellOverlay (getCell board0 (5, 4)) == Nothing)
  let cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalScore 99999 }
      gs0 =
        (newGameAtLevel 0 cfg 12)
          { gsBoard = board0
          , gsBelts = []
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  assertBool "source vine remains or was shifted" $
    hasVine (getCell b1 (5, 5))
      || any hasVine [getCell b1 p | r <- [4 .. 6], c <- [4 .. 6], let p = (r, c)]
  -- At least one orthogonal neighbor of original vine should now have vine
  -- (unless cascade destroyed the area — keep vine far from match)
  let neighbors = [(5, 4), (5, 6), (4, 5), (6, 5)]
  assertBool
    ("vine spread to a neighbor, board snippet: " ++ show [(p, cellOverlay (getCell b1 p)) | p <- (5, 5) : neighbors])
    (any (\p -> hasVine (getCell b1 p)) neighbors
       || (hasVine (getCell b1 (5, 5)) && any (\p -> hasVine (getCell b1 p)) neighbors))
  -- Pure spreadVines unit check
  let pureB = spreadVines board0
  assertBool "pure spread" (any (\p -> hasVine (getCell pureB p)) neighbors)

-- | A vine cleared by the move does not spread; uncleared vines still may.
vine_blocked_by_clear :: Assertion
vine_blocked_by_clear = do
  -- Vine ON the match at (3,1): cleared with the match → must not spread.
  -- Isolated vine at (6,6) far away: should still spread.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkVineGem C1))
                (3, 2)
                (mkGem C1))
             (6, 6)
             (mkVineGem C5))
          (0, 0)
          (mkGem C2)  -- keep stable-ish
  assertBool "match vine" (hasVine (getCell board0 (3, 1)))
  assertBool "safe vine" (hasVine (getCell board0 (6, 6)))
  -- clearOverlaysOn on match seeds strips the match vine
  let ms = findMatches board0
  assertBool "vine in match" ((3, 1) `elem` ms)
  let stripped = clearOverlaysOn board0 ms
  assertBool "cleared vine stripped" (not (hasVine (getCell stripped (3, 1))))
  assertBool "safe vine kept" (hasVine (getCell stripped (6, 6)))
  -- After strip + clear of match cells, spread only from remaining vines
  let afterClear =
        -- simulate: match cells become empty of vine; then if we spread on stripped
        -- before removing gems, match vine already gone so cannot spread from (3,1)
        spreadVines stripped
  -- Neighbors of (3,1) should NOT have gained vine from the cleared vine.
  -- (safe vine at 6,6 may spread to its own neighbors)
  -- (3,0) and (3,2) still C1 gems with no overlay after strip — would have been
  -- spread targets IF vine at (3,1) survived; it did not.
  assertBool "no spread from cleared vine onto (3,0)" (not (hasVine (getCell afterClear (3, 0))))
  assertBool "no spread from cleared vine onto (3,2)" (not (hasVine (getCell afterClear (3, 2))))
  -- Safe vine still spreads
  let safeNeighbors = [(6, 5), (6, 7), (5, 6), (7, 6)]
  assertBool
    "safe vine spreads"
    (any (\p -> hasVine (getCell afterClear p)) safeNeighbors)



--------------------------------------------------------------------------------
-- Chocolate overlays (开心消消乐巧克力)
--------------------------------------------------------------------------------

-- | Surviving chocolate spreads onto adjacent bare gems after a successful move.
choco_spreads_after_move :: Assertion
choco_spreads_after_move = do
  -- Choco at (5,5); bare neighbors. Match on row 0 so chocolate survives.
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
          (5, 5)
          (mkChocoGem C4)
  assertBool "choco placed" (hasChoco (getCell board0 (5, 5)))
  assertBool "neighbor bare" (cellOverlay (getCell board0 (5, 4)) == Nothing)
  let cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalScore 99999 }
      gs0 =
        (newGameAtLevel 0 cfg 12)
          { gsBoard = board0
          , gsBelts = []
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
      neighbors = [(5, 4), (5, 6), (4, 5), (6, 5)]
  assertBool
    ("choco spread to a neighbor: " ++ show [(p, cellOverlay (getCell b1 p)) | p <- (5, 5) : neighbors])
    (any (\p -> hasChoco (getCell b1 p)) neighbors
       || (hasChoco (getCell b1 (5, 5)) && any (\p -> hasChoco (getCell b1 p)) neighbors))
  -- Pure spreadChoco unit check
  let pureB = spreadChoco board0
  assertBool "pure spread" (any (\p -> hasChoco (getCell pureB p)) neighbors)

-- | Chocolate adjacent to a match is cleared; the gem underneath stays.
choco_cleared_by_adjacent :: Assertion
choco_cleared_by_adjacent = do
  -- Match at (3,0)(3,1)(3,2); chocolate on (2,1) and (4,1) adjacent, not in match.
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
             (mkChocoGem C3))
          (4, 1)
          (mkChocoGem C4)
  assertBool "choco above" (hasChoco (getCell board0 (2, 1)))
  assertBool "choco below" (hasChoco (getCell board0 (4, 1)))
  let ms = findMatches board0
  assertBool "match on row" (all (`elem` ms) [(3, 0), (3, 1), (3, 2)])
  assertBool "choco not in match" ((2, 1) `notElem` ms && (4, 1) `notElem` ms)
  -- Unit: clearChocoAdjacent strips neighbors
  let cleared = clearChocoAdjacent board0 ms
  assertBool "adj choco above cleared" (not (hasChoco (getCell cleared (2, 1))))
  assertBool "adj choco below cleared" (not (hasChoco (getCell cleared (4, 1))))
  assertEqual "gem color kept above" (Just C3) (cellColor (getCell cleared (2, 1)))
  assertEqual "gem color kept below" (Just C4) (cellColor (getCell cleared (4, 1)))
  -- Far choco untouched
  let withFar = setCell board0 (6, 6) (mkChocoGem C5)
      cleared2 = clearChocoAdjacent withFar ms
  assertBool "far choco kept" (hasChoco (getCell cleared2 (6, 6)))

-- | Chocolate cleared by the move does not spread; uncleared chocolate still may.
choco_blocked_by_clear :: Assertion
choco_blocked_by_clear = do
  -- Choco adjacent to match at (2,1): cleared by adjacent → must not spread.
  -- Isolated choco at (6,6): should still spread.
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
             (mkChocoGem C2))
          (6, 6)
          (mkChocoGem C5)
  assertBool "adj choco" (hasChoco (getCell board0 (2, 1)))
  assertBool "safe choco" (hasChoco (getCell board0 (6, 6)))
  let ms = findMatches board0
  assertBool "match present" ((3, 1) `elem` ms)
  let stripped = clearChocoAdjacent (clearOverlaysOn board0 ms) ms
  assertBool "adj choco stripped" (not (hasChoco (getCell stripped (2, 1))))
  assertBool "safe choco kept" (hasChoco (getCell stripped (6, 6)))
  let spread = spreadChoco stripped
  -- Neighbors of cleared (2,1) must NOT gain choco from that cell
  assertBool "no spread onto (2,0)" (not (hasChoco (getCell spread (2, 0))))
  assertBool "no spread onto (2,2)" (not (hasChoco (getCell spread (2, 2))))
  assertBool "no spread onto (1,1)" (not (hasChoco (getCell spread (1, 1))))
  -- Safe choco still spreads
  let safeNeighbors = [(6, 5), (6, 7), (5, 6), (7, 6)]
  assertBool
    "safe choco spreads"
    (any (\p -> hasChoco (getCell spread p)) safeNeighbors)


--------------------------------------------------------------------------------
-- Fog / 迷雾 (adjacent peel layers; fogged gems do not match)
--------------------------------------------------------------------------------

fog_blocks_match :: Assertion
fog_blocks_match = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFogGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "fog present" (hasFog (getCell board0 (3, 1)))
  assertBool "fogged gem breaks run" (null (findMatches board0))
  assertEqual "fog layers" (1 :: Int) (fogLayers (getCell board0 (3, 1)))

fog_cleared_by_adjacent :: Assertion
fog_cleared_by_adjacent = do
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
          (mkFogGem C2 1)
  assertBool "fog above match" (hasFog (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFog board0 ms
  assertEqual "one fog fully peeled" (1 :: Int) cleared
  assertBool "fog gone" (not (hasFog (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  -- Via cascade clear path
  let CascadeRun {crBoard = board1} = cascadeSeeds Nothing ms [] [] (mkStdGen 1) board0
  assertBool "fog cleared in cascade" (not (hasFog (getCell board1 (2, 1))))

fog_layer_decrement :: Assertion
fog_layer_decrement = do
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
          (mkFogGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFog board0 ms
  assertEqual "no full clear yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (fogLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentFog b1 ms
  assertEqual "now fully peeled" (1 :: Int) cleared2
  assertBool "fog gone" (not (hasFog (getCell b2 (4, 1))))
  -- Campaign décor includes fog on 巧饼 / 终章
  let gs = newGameAtLevel 22 (levelConfig (allLevels !! 22)) 42
      nFog =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasFog (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor fog >= 4, got " ++ show nFog) (nFog >= 4)


--------------------------------------------------------------------------------
-- Chain / 锁链 (locks gem: no swap/match; adjacent peel)
--------------------------------------------------------------------------------

chain_blocks_match :: Assertion
chain_blocks_match = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkChainGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "chain present" (hasChain (getCell board0 (3, 1)))
  assertBool "chained gem breaks run" (null (findMatches board0))
  assertEqual "chain layers" (1 :: Int) (chainLayers (getCell board0 (3, 1)))

chain_blocks_swap :: Assertion
chain_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkChainGem C2 1)
      gs0 =
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
  gsMoves gs1 @?= gsMoves gs0
  assertBool "swapBlocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))

chain_cleared_by_adjacent :: Assertion
chain_cleared_by_adjacent = do
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
          (mkChainGem C2 1)
  assertBool "chain above match" (hasChain (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentChain board0 ms
  assertEqual "one chain unlocked" (1 :: Int) cleared
  assertBool "chain gone" (not (hasChain (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  let CascadeRun {crBoard = board1} = cascadeSeeds Nothing ms [] [] (mkStdGen 1) board0
  assertBool "chain cleared in cascade" (not (hasChain (getCell board1 (2, 1))))

chain_layer_decrement :: Assertion
chain_layer_decrement = do
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
          (mkChainGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentChain board0 ms
  assertEqual "no full unlock yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (chainLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentChain b1 ms
  assertEqual "now unlocked" (1 :: Int) cleared2
  assertBool "chain gone" (not (hasChain (getCell b2 (4, 1))))
  let gs = newGameAtLevel 25 (levelConfig (allLevels !! 25)) 42
      nChain =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasChain (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor chain >= 8, got " ++ show nChain) (nChain >= 8)
  assertEqual "campaign levels" (40 :: Int) (length allLevels)

--------------------------------------------------------------------------------
-- Freeze / 火箭冰冻 (blocks swap only; adjacent peel; ≠ Ice match-chip)
--------------------------------------------------------------------------------

freeze_blocks_swap :: Assertion
freeze_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkFreezeGem C2 1)
  assertBool "has freeze" (hasFreeze (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedWith defaultRegistry board (3, 3) (3, 4))
  -- Frozen gem CAN still participate in matches (unlike Chain)
  let boardM =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFreezeGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "freeze does not break match" (not (null (findMatches boardM)))
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

freeze_cleared_by_adjacent :: Assertion
freeze_cleared_by_adjacent = do
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
          (mkFreezeGem C2 1)
  assertBool "freeze above match" (hasFreeze (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFreeze board0 ms
  assertEqual "fully thawed" (1 :: Int) cleared
  assertBool "freeze gone" (not (hasFreeze (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  -- Cascade path also peels
  let CascadeRun {crBoard = board1} = cascadeMatches Nothing [] [] (mkStdGen 1) board0
  assertBool "freeze cleared in cascade" (not (hasFreeze (getCell board1 (2, 1))))

freeze_layer_decrement :: Assertion
freeze_layer_decrement = do
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
          (mkFreezeGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFreeze board0 ms
  assertEqual "no full thaw yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (freezeLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentFreeze b1 ms
  assertEqual "now thawed" (1 :: Int) cleared2
  assertBool "freeze gone" (not (hasFreeze (getCell b2 (4, 1))))
  -- Distinct from Ice: ice layers live on Gem Int field, not Freeze overlay
  let iced = mkIceGem C1 2
  assertEqual "ice layers" (2 :: Int) (iceLayers iced)
  assertBool "ice is not freeze overlay" (not (hasFreeze iced))
  let gs = newGameAtLevel 29 (levelConfig (allLevels !! 29)) 42
      nFreeze =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasFreeze (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor freeze >= 8, got " ++ show nFreeze) (nFreeze >= 8)
  assertEqual "campaign levels" (40 :: Int) (length allLevels)

--------------------------------------------------------------------------------
-- Curtain / 窗帘 (blocks match; adjacent peel; ≠ Fog soft cloud)
--------------------------------------------------------------------------------

curtain_blocks_match :: Assertion
curtain_blocks_match = do
  let boardM =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkCurtainGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "curtain breaks match" (null (findMatches boardM))
  assertBool "has curtain" (hasCurtain (getCell boardM (3, 1)))

curtain_cleared_by_adjacent :: Assertion
curtain_cleared_by_adjacent = do
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
          (mkCurtainGem C2 1)
  assertBool "curtain above match" (hasCurtain (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentCurtain board0 ms
  assertEqual "fully opened" (1 :: Int) cleared
  assertBool "curtain gone" (not (hasCurtain (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  let CascadeRun {crBoard = board1} = cascadeMatches Nothing [] [] (mkStdGen 1) board0
  assertBool "curtain cleared in cascade" (not (hasCurtain (getCell board1 (2, 1))))

curtain_layer_decrement :: Assertion
curtain_layer_decrement = do
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
          (mkCurtainGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentCurtain board0 ms
  assertEqual "no full clear yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (curtainLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentCurtain b1 ms
  assertEqual "now clear" (1 :: Int) cleared2
  assertBool "curtain gone" (not (hasCurtain (getCell b2 (4, 1))))
  let gs = newGameAtLevel 30 (levelConfig (allLevels !! 30)) 42
      nCurt =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasCurtain (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor curtain >= 8, got " ++ show nCurt) (nCurt >= 8)
  assertEqual "campaign levels" (40 :: Int) (length allLevels)

steam_blocks_match :: Assertion
steam_blocks_match = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 Normal 0 (Just Steam)))
          (3, 2)
          (mkGem C1)
  assertBool "has steam" (hasSteam (getCell board0 (3, 1)))
  assertBool "no match through steam" (null (findMatches board0))

steam_cleared_by_adjacent :: Assertion
steam_cleared_by_adjacent = do
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
          (Gem C2 Normal 0 (Just Steam))
  assertBool "steam present" (hasSteam (getCell board0 (2, 1)))
  let b1 = clearSteamAdjacent board0 (findMatches board0)
  assertBool "steam extinguished" (not (hasSteam (getCell b1 (2, 1))))
  assertEqual "gem remains" (Just C2) (cellColor (getCell b1 (2, 1)))

steam_spreads_after_move :: Assertion
steam_spreads_after_move = do
  let board0 =
        setCell
          (setCell stableBoard (3, 3) (Gem C1 Normal 0 (Just Steam)))
          (3, 4)
          (mkGem C5)
  assertBool "neighbor bare" (cellOverlay (getCell board0 (3, 4)) == Nothing)
  let b1 = spreadSteam board0
  assertBool "steam spread" (hasSteam (getCell b1 (3, 4)))
  let gs = newGameAtLevel 35 (levelConfig (allLevels !! 35)) 42
      nSt =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasSteam (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor steam >= 8, got " ++ show nSt) (nSt >= 8)
  assertEqual "campaign levels" (40 :: Int) (length allLevels)

-- | Same clear peels an adjacent Chain and an adjacent Freeze.
-- Locks Chain+Freeze: independent overlays on different cells both respond.
chain_freeze_both_peel :: Assertion
chain_freeze_both_peel = do
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
             (mkChainGem C2 1))
          (4, 1)
          (mkFreezeGem C3 1)
      ms = findMatches board0
      (b1, nChain) = chipAdjacentChain board0 ms
      (b2, nFreeze) = chipAdjacentFreeze b1 ms
  assertEqual "chain peeled" (1 :: Int) nChain
  assertEqual "freeze peeled" (1 :: Int) nFreeze
  assertBool "chain gone" (not (hasChain (getCell b2 (2, 1))))
  assertBool "freeze gone" (not (hasFreeze (getCell b2 (4, 1))))
  -- Full clearMatches also leaves both cells as bare gems (not holes)
  let (mb, _) = clearMatches board0
  assertBool "chain cell not holed" $
    case atM mb (2, 1) of
      Just c -> isGem c && not (hasChain c)
      Nothing -> False
  assertBool "freeze cell not holed" $
    case atM mb (4, 1) of
      Just c -> isGem c && not (hasFreeze c)
      Nothing -> False

-- | Curtain: may swap (≠ Chain/Freeze) but curtained gem breaks match runs.
curtain_allows_swap_blocks_match :: Assertion
curtain_allows_swap_blocks_match = do
  let board =
        setCell
          (setCell (setCell stableBoard (5, 2) (mkGem C2)) (5, 3) (mkCurtainGem C2 1))
          (5, 4)
          (mkGem C2)
  assertBool "curtain does not block swap" $
    not (swapBlockedWith defaultRegistry board (5, 3) (5, 2))
  assertBool "no H match through curtain" $
    not ((5, 2) `elem` findMatches board)
      && not ((5, 3) `elem` findMatches board)
      && not ((5, 4) `elem` findMatches board)
  -- Open curtain then the same triple can match
  let open = setCell board (5, 3) (mkGem C2)
  assertBool "open triple matches" $
    all (`elem` findMatches open) [(5, 2), (5, 3), (5, 4)]

-- | Freeze blocks both adjacent trySwap (click/drag) and free-swap booster.
freeze_blocks_freeswap_and_swap :: Assertion
freeze_blocks_freeswap_and_swap = do
  let board =
        setCell
          (setCell stableBoard (2, 2) (mkFreezeGem C1 1))
          (2, 3)
          (mkGem C2)
      gs0 =
        (newGame defaultConfig 5)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsFreeSwaps = 2
          , gsBelts = []
          , gsUfos = []
          }
  assertBool "swapBlocked" (swapBlockedWith defaultRegistry board (2, 2) (2, 3))
  let (gs1, out1) = trySwap (2, 2) (2, 3) gs0
  assertEqual "trySwap NoMatch" NoMatch out1
  assertEqual "moves kept" (gsMoves gs0) (gsMoves gs1)
  let (gs2, out2) = useFreeSwap (2, 2) (5, 5) gs0
  assertEqual "free-swap NoMatch" NoMatch out2
  assertEqual "free-swap charge kept" (gsFreeSwaps gs0) (gsFreeSwaps gs2)


--------------------------------------------------------------------------------
-- Soft-hit must not strip adjacent Choco / Steam (ice chip / Flip face-flip)
--------------------------------------------------------------------------------

-- | ice>1 chip and Flip face-flip are soft hits (not clear holes). They must NOT
-- extinguish orthogonally adjacent Chocolate or Steam — only true clears do.
-- Regression: clear pipeline used raw expand seeds, so soft hits stripped décor.
soft_hit_preserves_choco_steam :: Assertion
soft_hit_preserves_choco_steam = do
  -- ice=2 mid of a 3-match: chips to ice=1, stays; adjacent choco must survive.
  let boardIce =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkIceGem C1 2))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkChocoGem C2)
  assertBool "ice match present" (not (null (findMatches boardIce)))
  let expanded = expandSpecials boardIce (findMatches boardIce)
      (_, iceFree) = chipIceOnClear boardIce expanded
  assertBool "soft ice not a hole" ((3, 1) `notElem` iceFree)
  let CascadeRun {crBoard = boardI1} = cascadeMatches Nothing [] [] (mkStdGen 21) boardIce
  assertEqual "ice chipped once" (1 :: Int) (iceLayers (getCell boardI1 (3, 1)))
  assertBool "choco survives ice soft-hit" (hasChoco (getCell boardI1 (2, 1)))
  -- Control: same layout with bare mid gem — true clear *does* strip choco.
  let boardHard =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkChocoGem C2)
      CascadeRun {crBoard = boardH1} = cascadeMatches Nothing [] [] (mkStdGen 22) boardHard
  assertBool "true clear strips choco" (not (hasChoco (getCell boardH1 (2, 1))))
  -- Flip in a 3-match: flips to back, stays; adjacent steam must survive.
  let boardFlip =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (4, 0) (mkGem C2))
                (4, 1)
                (mkFlip C2 C5))
             (4, 2)
             (mkGem C2))
          (5, 1)
          (mkSteamGem C3)
  assertBool "flip match present" (not (null (findMatches boardFlip)))
  let (_, iceF) = chipIceOnClear boardFlip (expandSpecials boardFlip (findMatches boardFlip))
  assertBool "flip not a hole" ((4, 1) `notElem` iceF)
  let CascadeRun {crBoard = boardF1} = cascadeMatches Nothing [] [] (mkStdGen 23) boardFlip
  assertBool "became back gem" $
    isGem (getCell boardF1 (4, 1)) && not (isFlip (getCell boardF1 (4, 1)))
  assertEqual "back color C5" (Just C5) (cellColor (getCell boardF1 (4, 1)))
  assertBool "steam survives flip soft-hit" (hasSteam (getCell boardF1 (5, 1)))

--------------------------------------------------------------------------------
-- Soft-hit must keep on-cell Grass/Vine/Choco (ice>1 hammer / Line path)
--------------------------------------------------------------------------------

-- | ice>1 soft chip must not strip Grass/Vine/Choco on the same cell.
-- Regression: clearOverlaysOn ran on raw expand seeds before chipIce, so a
-- hammer (or Line blast path) on ice=2+Choco wiped the overlay while the gem
-- stayed — breaking soft-hit / Choco-adjacent-only discipline.
soft_hit_preserves_oncell_overlays :: Assertion
soft_hit_preserves_oncell_overlays = do
  let mkGs board =
        (newGame defaultConfig 11)
          { gsBoard = board
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
  -- Hammer ice=2 + Choco: chip ice, Choco stays (no true clear / no adj clear).
  let boardCh = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Choco))
      (gsCh, outCh) = useHammer (4, 4) (mkGs boardCh)
  case outCh of
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cCh = getCell (gsBoard gsCh) (4, 4)
  assertEqual "choco ice 2→1" (1 :: Int) (iceLayers cCh)
  assertBool "on-cell choco survives soft hammer" (hasChoco cCh)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsCh)
  -- Hammer ice=2 + Grass / Vine: same soft-hit keep.
  let boardGr = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Grass))
      (gsGr, _) = useHammer (4, 4) (mkGs boardGr)
      cGr = getCell (gsBoard gsGr) (4, 4)
  assertEqual "grass ice 2→1" (1 :: Int) (iceLayers cGr)
  assertBool "on-cell grass survives soft hammer" (hasGrass cGr)
  let boardVi = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Vine))
      (gsVi, _) = useHammer (4, 4) (mkGs boardVi)
      cVi = getCell (gsBoard gsVi) (4, 4)
  assertEqual "vine ice 2→1" (1 :: Int) (iceLayers cVi)
  assertBool "on-cell vine survives soft hammer" (hasVine cVi)
  -- Control: last ice (ice==1) + Choco clears the gem (refill has no Choco).
  let boardLast = setCell stableBoard (4, 4) (Gem C2 Normal 1 (Just Choco))
      (gsLast, _) = useHammer (4, 4) (mkGs boardLast)
  assertBool "last-ice choco gone" (not (hasChoco (getCell (gsBoard gsLast) (4, 4))))
  assertBool "last-ice layer gone" (iceLayers (getCell (gsBoard gsLast) (4, 4)) == 0)

--------------------------------------------------------------------------------
-- Soft-hit must not fire adjacent side-effects (Fog/locks/Maker/Bottle/Hat/Balloon)
--------------------------------------------------------------------------------

-- | ice>1 chip and Flip face-flip are not true clear holes. They must NOT peel
-- adjacent Fog/Chain/Freeze/Curtain, charge Maker, trigger Bottle/Hat, or pop
-- Balloon — same trueClears discipline as soft_hit_preserves_choco_steam.
soft_hit_no_adj_side_effects :: Assertion
soft_hit_no_adj_side_effects = do
  -- Soft ice match: adjacent peel-locks / Maker stay untouched.
  let boardIce lock =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkIceGem C1 2))
             (3, 2)
             (mkGem C1))
          (2, 1)
          lock
  assertBool "ice match" (not (null (findMatches (boardIce (mkGem C2)))))
  let msIce = findMatches (boardIce (mkFogGem C2 2))
      expandedIce = expandSpecials (boardIce (mkFogGem C2 2)) msIce
      (_, iceFree) = chipIceOnClear (boardIce (mkFogGem C2 2)) expandedIce
      (bFogU, nFog) = chipAdjacentFog (boardIce (mkFogGem C2 2)) iceFree
  assertBool "soft ice not a hole" ((3, 1) `notElem` iceFree)
  assertEqual "soft ice peels no Fog" (0 :: Int) nFog
  assertEqual "Fog2 unchanged" (Just (Fog 2)) (cellOverlay (getCell bFogU (2, 1)))
  let (bChU, nCh) = chipAdjacentChain (boardIce (mkChainGem C2 2)) iceFree
  assertEqual "soft ice peels no Chain" (0 :: Int) nCh
  assertEqual "Chain2 unchanged" (2 :: Int) (chainLayers (getCell bChU (2, 1)))
  let (bFrU, nFr) = chipAdjacentFreeze (boardIce (mkFreezeGem C2 2)) iceFree
  assertEqual "soft ice peels no Freeze" (0 :: Int) nFr
  assertEqual "Freeze2 unchanged" (2 :: Int) (freezeLayers (getCell bFrU (2, 1)))
  let (bCuU, nCu) = chipAdjacentCurtain (boardIce (mkCurtainGem C2 2)) iceFree
  assertEqual "soft ice peels no Curtain" (0 :: Int) nCu
  assertEqual "Curtain2 unchanged" (2 :: Int) (curtainLayers (getCell bCuU (2, 1)))
  -- Live cascade: Maker of match color must not charge on soft ice wave.
  let CascadeRun {crBoard = bMk} = cascadeMatches Nothing [] [] (mkStdGen 72) (boardIce (mkMakerCharges C1 3))
  assertBool "Maker still maker" (isMaker (getCell bMk (2, 1)))
  assertEqual "Maker uncharged" (3 :: Int) (makerCharges (getCell bMk (2, 1)))
  -- Soft Flip match: Balloon same front-color must not pop; Bottle must not dye.
  let boardFlip =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (4, 0) (mkGem C2))
                   (4, 1)
                   (mkFlip C2 C5))
                (4, 2)
                (mkGem C2))
             (5, 1)
             (mkBalloon C2))
          (5, 2)
          (mkGem C3)
  assertBool "flip match" (not (null (findMatches boardFlip)))
  let boardFlipBot = setCell boardFlip (3, 1) (mkBottle C4)
      CascadeRun {crBoard = bFlip} = cascadeMatches Nothing [] [] (mkStdGen 73) boardFlipBot
  assertBool "Balloon survives soft Flip" (isBalloon (getCell bFlip (5, 1)))
  assertEqual "Bottle did not dye neighbor" (Just C3) (cellColor (getCell bFlip (5, 2)))
  -- Control: true clear mid gem *does* peel Fog / charge Maker.
  let boardHard' =
        setCell (boardIce (mkFogGem C2 2)) (3, 1) (mkGem C1)  -- bare mid
      msH = findMatches boardHard'
      (_, iceH) = chipIceOnClear boardHard' (expandSpecials boardHard' msH)
      (bFogH, _) = chipAdjacentFog boardHard' iceH
  assertBool "true clear is a hole" ((3, 1) `elem` iceH)
  assertEqual "true clear peels Fog 2→1" (Just (Fog 1)) (cellOverlay (getCell bFogH (2, 1)))
  -- Unit: trueClears charge Maker (avoid gravity moving the maker off-cell).
  let boardMkH = setCell (boardIce (mkMakerCharges C1 3)) (3, 1) (mkGem C1)
      msMk = findMatches boardMkH
      (_, iceMk) = chipIceOnClear boardMkH (expandSpecials boardMkH msMk)
      bMkH = chargeAdjacentMakers boardMkH iceMk
  assertEqual "true clear charges Maker 3→2" (2 :: Int) (makerCharges (getCell bMkH (2, 1)))

--------------------------------------------------------------------------------
-- Soft-hit must keep on-cell Fog / Steam (ice>1 hammer)
--------------------------------------------------------------------------------

-- | ice>1 soft chip must not strip on-cell Fog or Steam (peel-locks / match
-- blockers ride with the gem until a true clear). Extends
-- soft_hit_preserves_oncell_overlays beyond Grass/Vine/Choco.
soft_hit_preserves_oncell_fog_steam :: Assertion
soft_hit_preserves_oncell_fog_steam = do
  let mkGs board =
        (newGame defaultConfig 13)
          { gsBoard = board
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
  -- Hammer ice=2 + Fog2: chip ice, Fog stays.
  let boardFog = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just (Fog 2)))
      (gsFog, outFog) = useHammer (4, 4) (mkGs boardFog)
  case outFog of
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cFog = getCell (gsBoard gsFog) (4, 4)
  assertEqual "fog ice 2→1" (1 :: Int) (iceLayers cFog)
  assertEqual "on-cell Fog2 survives soft hammer" (Just (Fog 2)) (cellOverlay cFog)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsFog)
  -- Hammer ice=2 + Steam: same soft-hit keep.
  let boardSteam = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Steam))
      (gsSt, _) = useHammer (4, 4) (mkGs boardSteam)
      cSt = getCell (gsBoard gsSt) (4, 4)
  assertEqual "steam ice 2→1" (1 :: Int) (iceLayers cSt)
  assertBool "on-cell Steam survives soft hammer" (hasSteam cSt)
  -- Control: bare Fog (ice 0) hammer clears through Fog (direct-hit ≠ peel).
  let boardBare = setCell stableBoard (4, 4) (mkFogGem C2 2)
      (gsBare, _) = useHammer (4, 4) (mkGs boardBare)
  assertBool "bare Fog direct-cleared" (not (hasFog (getCell (gsBoard gsBare) (4, 4))))
  assertEqual "bare Fog hammer spent" (1 :: Int) (gsHammers gsBare)
