{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 会动或会生成东西的元素（对应 Element/Builtin/Actor）：魔法帽、果汁机、染色瓶、倒计时、蜗牛。
-- （第 1 刀由 Spec.Obstacles.Body / Features 按 Builtin 分组纯搬家而来；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Builtin.Actor
  ( tests
  ) where

import Data.List (nub)
import Match3.Board.Default (cascadeCountdowns, cascadeMatches, clearMatches, noHooks, builtinHooks, hookLevel)
import Match3.Element.Level (levelUfos)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crTally, crHooks, crBoard), CascadeTally(CascadeTally, ctCounts, ctScore, ctMaxWave, ctCells))
import Match3.Core
import Match3.Board.Grid (atM, setM, mboardFromRows)
import Match3.Element (defaultRegistry)
import Match3.Element.Registry (swapBlockedWith)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "hat_triggered_by_adjacent" hat_triggered_by_adjacent
  , testCase "hat_swaps_colors" hat_swaps_colors
  , testCase "maker_blocks_swap" maker_blocks_swap
  , testCase "maker_charges_on_same_color" maker_charges_on_same_color
  , testCase "maker_produces_bomb" maker_produces_bomb
  , testCase "bottle_blocks_swap" bottle_blocks_swap
  , testCase "bottle_dyes_neighbors" bottle_dyes_neighbors
  , testCase "maker_bomb_survives_wave" maker_bomb_survives_wave
  , testCase "bottle_dye_followup_match" bottle_dye_followup_match
  , testCase "hat_recolor_followup_match" hat_recolor_followup_match
  , testCase "maker_multi_adjacent_charges_once" maker_multi_adjacent_charges_once
  , testCase "hat_immune_to_direct_clear" hat_immune_to_direct_clear
  , testCase "maker_bomb_sits_bottle" maker_bomb_sits_bottle
  , testCase "countdown_bomb_spawns" countdown_bomb_spawns
  , testCase "countdown_bomb_ticks_after_move" countdown_bomb_ticks_after_move
  , testCase "countdown_bomb_explodes_at_zero" countdown_bomb_explodes_at_zero
  , testCase "countdown_bomb_cleared_disarms" countdown_bomb_cleared_disarms
  , testCase "snail_moves_after_move" snail_moves_after_move
  , testCase "snail_blocks_swap" snail_blocks_swap
  , testCase "countdown_explode_keeps_ufo_portals" countdown_explode_keeps_ufo_portals
  , testCase "snail_belt_no_double_step" snail_belt_no_double_step
  , testCase "snail_crawl_resolves_match" snail_crawl_resolves_match
  , testCase "snail_reverses_at_portal_endpoint" snail_reverses_at_portal_endpoint
  ]

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
  assertEqual "cycled neighbor" (Just C3) (cellColor (getCell board1 (1, 1)))

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
  assertEqual "left before" (Just C4) cLeft0
  assertEqual "right before" (Just C5) cRight0
  let board1 = triggerAdjacentHats board0 ms
      cLeft1 = cellColor (getCell board1 (2, 0))
      cRight1 = cellColor (getCell board1 (2, 2))
  assertEqual "left got right" (Just C5) cLeft1
  assertEqual "right got left" (Just C4) cRight1
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
  assertEqual "bomb kind" (Just Bomb) (cellKind (getCell b1 (2, 1)))
  assertEqual "bomb color" (Just C1) (cellColor (getCell b1 (2, 1)))
  let gs = levelGame 26 42
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
-- Bottle / 染色瓶 (adjacent clear dyes ortho gems)
--------------------------------------------------------------------------------

bottle_blocks_swap :: Assertion
bottle_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkBottle C2)
  assertBool "is bottle" (isBottle (getCell board (3, 3)))
  assertEqual "color" (Just C2) (bottleColor (getCell board (3, 3)))
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
  assertEqual "dyed neighbor" (Just C3) (cellColor (getCell b1 (2, 2)))
  let gs = levelGame 33 42
      nBot =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isBottle (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor bottles >= 6, got " ++ show nBot) (nBot >= 6)

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
        (setBelts [] . setUfos [] $ (newGame defaultConfig 11)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
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
  case atM mb (2, 1) of
    Just c -> do
      assertBool "unit bomb gem" (isGem c)
      assertEqual "unit Bomb kind" (Just Bomb) (cellKind c)
      assertEqual "unit bomb color" (Just C1) (cellColor c)
    Nothing -> assertFailure "maker cell must not hole"

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
  assertEqual "dyed (5,2)" (Just C3) (cellColor (getCell dyed (5, 2)))
  assertBool "dye created vertical C3" $
    all (`elem` findMatches dyed) [(3, 2), (4, 2), (5, 2)]
  let CascadeRun {crTally = CascadeTally {ctCells = cells, ctScore = scored, ctMaxWave = maxW}} = cascadeMatches Nothing noHooks (mkStdGen 42) board
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
  assertEqual "hat swapped left to C3" (Just C3) (cellColor (getCell hatted (5, 0)))
  assertEqual "hat swapped right to C2" (Just C2) (cellColor (getCell hatted (5, 2)))
  assertBool "hat created col0 C3 match" $
    all (`elem` findMatches hatted) [(3, 0), (4, 0), (5, 0)]
  let CascadeRun {crTally = CascadeTally {ctCells = cells, ctScore = scored, ctMaxWave = maxW}} = cascadeMatches Nothing noHooks (mkStdGen 7) board
  assertBool ("hat follow-up cells>=6 got " ++ show cells) (cells >= 6)
  assertBool ("maxW>=2 got " ++ show maxW) (maxW >= 2)
  let CascadeRun {crBoard = bAfter} = cascadeMatches Nothing noHooks (mkStdGen 7) board
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
  assertBool "became bomb" (cellKind (getCell bBomb (2, 2)) == Just Bomb)
  assertBool "not maker" (not (isMaker (getCell bBomb (2, 2))))

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
  let CascadeRun {crBoard = bLine} = cascadeMatches Nothing noHooks (mkStdGen 61) (lineBoard mkMagicHat)
  assertBool "hat survives line blast" (isMagicHat (getCell bLine (3, 5)))
  -- Hammer on hat: NoMatch, charge kept, board unchanged.
  let gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 12)
          { gsBoard = setCell stableBoard (4, 4) mkMagicHat
          , gsHammers = 2
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          , gsMoves = 20
          , gsScore = 0
          })
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
  assertEqual "unprotected Bottle dyes maker bomb" (Just C3) (cellColor (getCell dyedBare (2, 1)))
  let dyedProt = triggerAdjacentBottlesExcept bCharged clears saved
  assertEqual "protected Bottle skips maker bomb" (Gem C1 Bomb 0 Nothing) (getCell dyedProt (2, 1))
  -- Integration: clearMatches keeps Maker Bomb color through Bottle
  let (mb, _) = clearMatches board0
  case atM mb (2, 1) of
    Just c -> do
      assertEqual "cascade keeps Bomb kind" (Just Bomb) (cellKind c)
      assertEqual "cascade keeps maker color (not Bottle)" (Just C1) (cellColor c)
    Nothing -> assertFailure "Maker Bomb must sit, not hole"

--------------------------------------------------------------------------------
-- Countdown bombs (开心消消乐倒计时炸弹)
--------------------------------------------------------------------------------

-- | spawnCountdown / mkCountdown places a colored timer on the board.
countdown_bomb_spawns :: Assertion
countdown_bomb_spawns = do
  let b0 = spawnCountdown stableBoard (2, 2) C1 5
  assertBool "is countdown" (isCountdown (getCell b0 (2, 2)))
  assertEqual "turns" (5 :: Int) (countdownTurns (getCell b0 (2, 2)))
  assertEqual "color" (Just C1) (cellColor (getCell b0 (2, 2)))
  assertBool "counts as gem" (isGem (getCell b0 (2, 2)))
  -- Participates in a same-color run
  let b1 =
        setCell
          (setCell b0 (2, 0) (mkGem C1))
          (2, 1)
          (mkGem C1)
      ms = findMatches b1
  assertBool "countdown in match" ((2, 2) `elem` ms)

-- | Successful move ticks remaining countdowns by 1.
countdown_bomb_ticks_after_move :: Assertion
countdown_bomb_ticks_after_move = do
  let bPure = spawnCountdown stableBoard (2, 2) C3 5
  assertEqual "pure tick 5->4" (4 :: Int) (countdownTurns (getCell (tickCountdowns bPure) (2, 2)))
  let CascadeRun {crBoard = bRes, crTally = CascadeTally {ctCells = nClear}} = cascadeCountdowns noHooks (mkStdGen 0) bPure
  assertEqual "resolve ticks" (4 :: Int) (countdownTurns (getCell bRes (2, 2)))
  assertEqual "no explode when >0" (0 :: Int) nClear
  -- trySwap path: use a tiny score goal so outcome is terminal (skips ensurePlayable shuffle)
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
      board = spawnCountdown board0 (5, 5) C5 5
      cfg = GameConfig { cfgMoves = 10, cfgGoal = goalScore 1 }
      gs0 =
        (newGameAtLevel 0 cfg 11)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  assertBool "terminal or applied" $
    case out of
      MoveApplied _ -> True
      LevelClear _ _ -> True
      Won _ -> True
      Lost _ -> True
      _ -> False
  assertBool "not reshuffled away" (not (gsShuffled gs1))
  let cds =
        [ countdownTurns (getCell (gsBoard gs1) (r, c))
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCountdown (getCell (gsBoard gs1) (r, c))
        ]
  assertEqual "countdown ticked 5->4 after move" [4] cds

-- | Countdown at 1 ticks to 0 and explodes clearing the 3×3 neighborhood.
countdown_bomb_explodes_at_zero :: Assertion
countdown_bomb_explodes_at_zero = do
  -- Countdown at (4,4) with 1 turn; match elsewhere on row 0 area via swap
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 0) (mkGem C2))
                (0, 1)
                (mkGem C2))
             (0, 2)
             (mkGem C3))
          (0, 3)
          (mkGem C2)
      board = spawnCountdown board0 (4, 4) C5 1
      -- Marker gem adjacent that should vanish in 3×3 explosion
      board' = setCell board (4, 5) (mkGem C1)
      gs0 =
        (newGame defaultConfig 13)
          { gsBoard = board'
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  -- Countdown itself must be gone (exploded)
  let cds =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCountdown (getCell (gsBoard gs1) (r, c))
        ]
  assertBool ("countdown exploded away, leftover " ++ show cds) (null cds)
  -- Score should reflect explosion clear (at least some points beyond tiny match)
  assertBool ("explosion scored, got " ++ show (gsScore gs1)) (gsScore gs1 >= 30)

-- | Matching a countdown disarms it (removed, no zero-explosion).
countdown_bomb_cleared_disarms :: Assertion
countdown_bomb_cleared_disarms = do
  -- Countdown C1 at (3,2) with 1 turn — would explode if not matched.
  -- Setup: C1 C1 Countdown(C1) via swap of (3,2)<->(3,3) where (3,3) is C1
  -- Start: (3,0)=C1 (3,1)=C1 (3,2)=Countdown C1 1 (3,3)=C2 — already a match including countdown!
  -- Better: no initial match; swap brings countdown into a triple.
  -- board: (3,0)=C1 (3,1)=C1 (3,2)=C2 (3,3)=Countdown C1 1
  -- swap (3,2)<->(3,3) => C1 C1 Countdown C2 — wait that matches countdown with C1s.
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C2)
      board = spawnCountdown board0 (3, 3) C1 1
      -- Witness gem at (5,5) far from (3,3); if countdown exploded (3×3 around 3,3)
      -- it would NOT reach (5,5). Place witness inside explosion radius instead:
      -- (3,4) is in 3×3 of (3,3). If disarmed by match, explosion shouldn't fire,
      -- but match clear may still remove neighbors via specials — use plain match.
      -- After disarm+ cascade, countdown gone; tick of other bombs N/A.
      -- Place a second countdown at (6,6) with 3 turns — should tick to 2, not explode.
      board' = spawnCountdown board (6, 6) C4 3
      gs0 =
        (newGame defaultConfig 17)
          { gsBoard = board'
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match to disarm"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  -- Matched countdown disarmed (no C1 countdown left)
  let c1cds =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , case getCell (gsBoard gs1) (r, c) of
            Countdown C1 _ -> True
            _ -> False
        ]
  assertBool ("C1 countdown disarmed, leftover " ++ show c1cds) (null c1cds)
  -- Other countdown ticked 3 -> 2 (survived, not exploded)
  let other =
        [ countdownTurns (getCell (gsBoard gs1) (r, c))
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCountdown (getCell (gsBoard gs1) (r, c))
        ]
  assertEqual "other countdown ticked once" [2] other

snail_moves_after_move :: Assertion
snail_moves_after_move = do
  -- Match far from snail: row 3 swap like stone_cleared; snail on bottom row
  -- so gravity cannot drop it. Faces right and pushes the gem at (7,2).
  let board0 =
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
          (7, 1)
          (mkSnail 0 1)
  assertBool "snail at start" (isSnail (getCell board0 (7, 1)))
  assertBool "gem to the right" (isGem (getCell board0 (7, 2)))
  -- Pure unit: stepSnails crawls without a full move
  let stepped = stepSnails board0
  let gemRight = getCell board0 (7, 2)
  assertBool "pure left start" (not (isSnail (getCell stepped (7, 1))))
  assertBool "pure at (7,2)" (isSnail (getCell stepped (7, 2)))
  assertEqual "pure pushed gem back" gemRight (getCell stepped (7, 1))
  let gs0 =
        (setBelts [] . setUfos [] $ (newGame defaultConfig 11)
          { gsBoard = board0
          , gsScore = 0
          , gsMoves = 20
          , gsGoal = goalScore 99999
          , gsOver = Nothing
          , gsHint = Nothing
          })
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  -- After cascades/refill colors may change; only require snail relocated one step
  assertBool "snail left start" (not (isSnail (getCell b1 (7, 1))))
  assertBool "snail crawled right" (isSnail (getCell b1 (7, 2)))
  assertBool "cell behind is gem (pushed)" (isGem (getCell b1 (7, 1)))
  -- Edge reverse: snail at right edge facing right flips dir
  let edgeB = setCell stableBoard (0, 7) (mkSnail 0 1)
      edge1 = stepSnailAt edgeB (0, 7)
  assertEqual "reversed at edge" (0, -1) (snailDir (getCell edge1 (0, 7)))
  assertBool "still at edge" (isSnail (getCell edge1 (0, 7)))
  let gsL = levelGame 28 42
      nSnail =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isSnail (getCell (gsBoard gsL) (r, c))
          ]
  assertBool ("decor snails >= 4, got " ++ show nSnail) (nSnail >= 4)

--------------------------------------------------------------------------------
-- Snail / 蜗牛 (crawls after move; blocks swap; pushes gems)
--------------------------------------------------------------------------------

snail_blocks_swap :: Assertion
snail_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkSnail 0 1)
  assertBool "is snail" (isSnail (getCell board (3, 3)))
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
  gsMoves gs1 @?= gsMoves gs0

-- | Countdown explode cascade still threads UFOs + portals (not dropped as []).
-- Locks 倒计时到期 × UFO/Portal: resolveCountdowns keeps overlays during blast settle.
countdown_explode_keeps_ufo_portals :: Assertion
countdown_explode_keeps_ufo_portals = do
  let bottom = boardSize - 1
      portals = [((0, 1), (bottom, 6))]
      -- Cookie on bottom portal; countdown far away ticks to 0 and explodes
      board0 =
        spawnCountdown (setCell stableBoard (bottom, 6) mkCookie) (4, 4) C5 1
      u0 = mkUfo (2, 2) C1
      CascadeRun {crBoard = bRes, crTally = CascadeTally {ctCounts = (countOf CountCookies -> cookies)}, crHooks = (levelUfos . hookLevel -> ufos')} = cascadeCountdowns (builtinHooks [u0] portals) (mkStdGen 5) board0
  assertBool ("explode settle collected bottom cookie, got " ++ show cookies) (cookies >= 1)
  assertBool "cookie not left on bottom portal" (not (isCookie (getCell bRes (bottom, 6))))
  assertEqual "UFO list preserved through resolve" (1 :: Int) (length ufos')
  -- trySwap path: countdown expires; UFO + portals remain on game state
  let boardT =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 0) (mkGem C2))
                (0, 1)
                (mkGem C2))
             (0, 2)
             (mkGem C3))
          (0, 3)
          (mkGem C2)
      boardT' = spawnCountdown boardT (5, 5) C4 1
      gs0 =
        (setUfos [u0] . setPortals portals . setBelts [] $ (newGame defaultConfig 19)
          { gsBoard = boardT'
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          })
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertEqual "UFO still on game after countdown explode" (1 :: Int) (length (gsUfos gs1))
  assertEqual "portals kept on state" portals (gsPortals gs1)
  assertBool "countdown gone after expire" $
    null
      [ ()
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , isCountdown (getCell (gsBoard gs1) (r, c))
      ]

--------------------------------------------------------------------------------
-- Stability cruise: snail×belt, maker charge edge, particle sites, map unlock
--------------------------------------------------------------------------------

-- | Snail sitting on a belt must not crawl after the belt shift (no double-step).
snail_belt_no_double_step :: Assertion
snail_belt_no_double_step = do
  let belt = [(4, 1), (4, 2), (4, 3), (4, 4)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (4, 1) (mkSnail 0 1))
                   (0, 0)
                   (mkGem C1))
                (0, 1)
                (mkGem C1))
             (0, 2)
             (mkGem C2))
          (0, 3)
          (mkGem C1)
      afterBelt = shiftBelts board0 [belt]
  assertBool "belt moved snail to (4,2)" (isSnail (getCell afterBelt (4, 2)))
  let skipped = stepSnailsAvoiding (concat [belt]) afterBelt
      crawled = stepSnails afterBelt
  assertBool "avoid: still at (4,2)" (isSnail (getCell skipped (4, 2)))
  assertBool "avoid: not at (4,3)" (not (isSnail (getCell skipped (4, 3))))
  assertBool "raw crawl would reach (4,3)" (isSnail (getCell crawled (4, 3)))
  let gs0 =
        (setBelts [belt] . setUfos [] $ (newGame defaultConfig 9)
          { gsBoard = board0
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
      snails =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isSnail (getCell b1 (r, c))
        ]
  assertEqual "one snail survives" (1 :: Int) (length snails)
  assertEqual "belt-only step lands on (4,2)" [(4, 2)] snails

-- | Snail crawl at end of move can assemble a 3-match; must cascade to stable
-- (regression: trySwap left hasAnyMatch after snail push).
snail_crawl_resolves_match :: Assertion
snail_crawl_resolves_match = do
  -- Trigger match on cols 4/5/7 (away from snail cols 0-3) so gravity does not
  -- disturb the snail row before crawl.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell
                               (setCell stableBoard (1, 4) (mkGem C1))
                               (1, 5)
                               (mkGem C1))
                            (1, 6)
                            (mkGem C2))
                         (1, 7)
                         (mkGem C1))
                      (2, 3)
                      (mkGem C2))
                   (3, 0)
                   (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkSnail 0 1))
          (3, 3)
          (mkGem C1)
  assertBool "pre-move stable" (not (hasAnyMatch board0))
  let afterCrawl = stepSnails board0
  assertBool "crawl creates match" (hasAnyMatch afterCrawl)
  assertBool "snail at (3,3)" (isSnail (getCell afterCrawl (3, 3)))
  assertEqual "C1 pushed to (3,2)" (Just C1) (cellColor (getCell afterCrawl (3, 2)))
  let gs0 =
        (setBelts [] . setPortals [] . setUfos [] $ (newGame (GameConfig 20 (goalScore 99999)) 7)
          { gsBoard = board0
          , gsOver = Nothing
          , gsMoves = 20
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          , gsLastCleared = []
          })
      (gs1, out) = trySwap (1, 6) (1, 7) gs0
  case out of
    NoMatch -> assertFailure "expected match swap"
    InvalidSwap -> assertFailure "expected valid swap"
    _ -> pure ()
  assertBool "post-move stable" (not (hasAnyMatch (gsBoard gs1)))
  assertBool "snail still on board" $
    any
      (\(r, c) -> isSnail (getCell (gsBoard gs1) (r, c)))
      [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
  -- Snail-assembled match cells should appear in clear particles
  assertBool "snail match cleared in particles" $
    any (`elem` gsLastCleared gs1) [(3, 0), (3, 1), (3, 2)]
  assertEqual "one move spent" (19 :: Int) (gsMoves gs1)

snail_reverses_at_portal_endpoint :: Assertion
snail_reverses_at_portal_endpoint = do
  let portals = [((3, 3), (6, 6))]
      walls = nub (concatMap (\(a, b) -> [a, b]) portals)
      -- Snail left of portal A, facing right toward it; gem on portal
      board0 =
        setCell
          (setCell stableBoard (3, 2) (mkSnail 0 1))
          (3, 3)
          (mkGem C2)
  assertBool "pre: snail left of portal" (isSnail (getCell board0 (3, 2)))
  assertBool "pre: portal has gem" (isGem (getCell board0 (3, 3)))
  -- Raw crawl (no walls) would push onto the portal
  let raw = stepSnails board0
  assertBool "raw crawl occupies portal" (isSnail (getCell raw (3, 3)))
  -- Gated crawl reverses at portal wall
  let gated = stepSnailsAvoidingBlocked [] walls board0
  assertBool "gated: still left of portal" (isSnail (getCell gated (3, 2)))
  assertBool "gated: portal not snail" (not (isSnail (getCell gated (3, 3))))
  assertEqual "gated: reversed dir" (0, -1) (snailDir (getCell gated (3, 2)))
  assertEqual "gated: portal gem kept" (Just C2) (cellColor (getCell gated (3, 3)))
  -- Full move with portals: snail must not land on endpoint after crawl
  let boardTrap =
        -- Cookie right of snail forces reverse toward portal (old finale pattern)
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell stableBoard (0, 3) (mkGem C4))
                      (0, 4)
                      (mkSnail 0 1))
                   (0, 5)
                   mkCookie)
                (1, 0)
                (mkGem C1))
             (1, 1)
             (mkGem C1))
          (1, 2)
          (mkGem C2)
      -- Make a match away from snail so trySwap succeeds and end-of-move crawls
      boardMove =
        setCell boardTrap (1, 3) (mkGem C1)
      gs0 =
        (setBelts [] . setPortals [((0, 3), (7, 4))] . setUfos [] $ (newGame (GameConfig 20 (goalScore 99999)) 11)
          { gsBoard = boardMove
          , gsOver = Nothing
          , gsMoves = 20
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          , gsLastCleared = []
          })
      -- Two crawls: first reverses at Cookie, second would enter portal without walls
      b1 = stepSnails (gsBoard gs0)
      b2 = stepSnails b1
  assertBool "unit: 2 raw crawls park on portal" (isSnail (getCell b2 (0, 3)))
  let (gs1, out1) = trySwap (1, 2) (1, 3) gs0
  case out1 of
    NoMatch -> assertFailure "expected match for crawl turn 1"
    InvalidSwap -> assertFailure "expected valid swap turn 1"
    _ -> pure ()
  assertBool "after move1 snail not on portal A" (not (isSnail (getCell (gsBoard gs1) (0, 3))))
  assertBool "after move1 snail not on portal B" (not (isSnail (getCell (gsBoard gs1) (7, 4))))
  -- Second move: still must not occupy portal
  let board2 = gsBoard gs1
      -- Ensure a legal match remains for a second crawl tick
      board2' =
        setCell
          (setCell
             (setCell
                (setCell board2 (2, 0) (mkGem C3))
                (2, 1)
                (mkGem C3))
             (2, 2)
             (mkGem C4))
          (2, 3)
          (mkGem C3)
      gs2 = gs1 { gsBoard = board2', gsOver = Nothing, gsHint = Nothing }
      (gs3, out2) = trySwap (2, 2) (2, 3) gs2
  case out2 of
    NoMatch -> assertFailure "expected match for crawl turn 2"
    InvalidSwap -> assertFailure "expected valid swap turn 2"
    _ -> pure ()
  assertBool "after move2 snail not on portal A" (not (isSnail (getCell (gsBoard gs3) (0, 3))))
  assertBool "after move2 snail not on portal B" (not (isSnail (getCell (gsBoard gs3) (7, 4))))
  -- Portal still transferable after two crawls
  let setMBoard = setM
      fill = Just (mkGem C5)
      mb0 = mboardFromRows (replicate boardSize (replicate boardSize fill))
      mb1 = setMBoard mb0 (0, 3) (Just (mkGem C1))
      mb2 = setMBoard mb1 (7, 4) Nothing
      mb3 = applyPortalTeleports (builtinHooks [] (gsPortals gs3)) mb2
  assertEqual "portal A still empties" Nothing (atM mb3 (0, 3))
  case atM mb3 (7, 4) of
    Just cell -> assertEqual "portal B still receives" (Just C1) (cellColor cell)
    Nothing -> assertFailure "expected gem at portal B after crawls"
