{-# LANGUAGE ScopedTypeVariables #-}

-- | 特殊块与合成：直线 / 炸弹 / 彩虹的生成与引爆、特殊 × 特殊合成、彩虹按交换对象取色、软锁纪律。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Specials
  ( tests
  ) where

import Match3.Board.Default (cascadeMatches, clearMatches)
import Data.List (sort)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crBoard, crTally), CascadeTally(CascadeTally, ctCells))
import Match3.Core
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "special_line_from_4" special_line_from_4
  , testCase "special_rainbow_from_5" special_rainbow_from_5
  , testCase "rainbow_clears_color" rainbow_clears_color
  , testCase "rainbow_swap_without_match" rainbow_swap_without_match
  , testCase "special_combo_line_bomb" special_combo_line_bomb
  , testCase "special_combo_rainbow_line" special_combo_rainbow_line
  , testCase "special_combo_bomb_bomb" special_combo_bomb_bomb
  , testCase "special_combo_line_line" special_combo_line_line
  , testCase "rainbow_expand_noop" rainbow_expand_noop
  , testCase "rainbow_swap_partner_not_own_color" rainbow_swap_partner_not_own_color
  , testCase "rainbow_x_bomb_blast_expands" rainbow_x_bomb_blast_expands
  , testCase "rainbow_swap_flip_partner" rainbow_swap_flip_partner
  , testCase "soft_lock_blocks_special_expand" soft_lock_blocks_special_expand
  , testCase "line_blast_no_double_peel" line_blast_no_double_peel
  , testCase "soft_lock_blocks_rainbow_swap" soft_lock_blocks_rainbow_swap
  , testCase "soft_lock_blocks_special_combo" soft_lock_blocks_special_combo
  , testCase "soft_lock_blocks_freeswap_activation" soft_lock_blocks_freeswap_activation
  , testCase "soft_lock_blocks_double_rainbow" soft_lock_blocks_double_rainbow
  ]

-- | 4-in-a-row spawns a Line special.
special_line_from_4 :: Assertion
special_line_from_4 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row2 = map mkGem [C1, C1, C1, C1, C2, C3, C4, C2]
      b = boardFromRows $ take 2 b0 ++ [row2] ++ drop 3 b0
      (mb, n) = clearMatches b
  assertBool "cleared" (n >= 4)
  let specials =
        [ (r, c, k)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [ (mb !! r) !! c ]
        , Just k <- [cellKind cell]
        , k /= Normal
        ]
  assertBool ("spawned line: " ++ show specials) $
    any (\(_, _, k) -> k == LineH || k == LineV) specials

-- | 5-in-a-row spawns a Rainbow.
special_rainbow_from_5 :: Assertion
special_rainbow_from_5 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row1 = map mkGem [C1, C1, C1, C1, C1, C2, C3, C2]
      b = boardFromRows $ take 1 b0 ++ [row1] ++ drop 2 b0
      (mb, _) = clearMatches b
      rainbows =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [ (mb !! r) !! c ]
        , cellKind cell == Just Rainbow
        ]
  assertBool ("rainbow spawned: " ++ show rainbows) (not (null rainbows))

--------------------------------------------------------------------------------
-- Rainbow (color bomb)
--------------------------------------------------------------------------------

-- | Swapping a Rainbow with a color clears every gem of that color.
rainbow_clears_color :: Assertion
rainbow_clears_color = do
  let board0 =
        setCell stableBoard (4, 4) (Gem C1 Rainbow 0 Nothing)
      -- Ensure neighbor (4,5) is C2 (stableBoard already has variety)
      board = setCell board0 (4, 5) (mkGem C2)
      -- Count C2 before
      c2before =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , case getCell board (r, c) of
              Gem C2 _ _ Nothing -> True
              _ -> False
          ]
  assertBool "have some C2" (c2before >= 1)
  let gs0 =
        (newGame defaultConfig 3)
          { gsBoard = board
          , gsOver = Nothing
          , gsHint = Nothing
          , gsScore = 0
          , gsMoves = 10
          }
      (gs1, out) = trySwap (4, 4) (4, 5) gs0
  case out of
    MoveApplied gained -> assertBool "scored" (gained > 0)
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("expected applied/terminal, got " ++ show other)
  let c2after =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , case getCell (gsBoard gs1) (r, c) of
              Gem C2 _ _ Nothing -> True
              _ -> False
          ]
  -- After cascade refill, leftover C2 may appear from refill; the rainbow itself must be gone
  assertBool "rainbow consumed" (not (isRainbow (getCell (gsBoard gs1) (4, 4))))
  assertBool "rainbow consumed at partner" (not (isRainbow (getCell (gsBoard gs1) (4, 5))))
  -- Moves decremented
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)
  -- At least the clear scored for the C2 count (wave1 * 10); allow cascades
  assertBool
    ("cleared many cells relative to C2 count " ++ show c2before ++ " after=" ++ show c2after)
    (gsScore gs1 >= c2before * 10)

-- | Rainbow swap works even when it would not create a classic 3-match.
rainbow_swap_without_match :: Assertion
rainbow_swap_without_match = do
  let board =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 Nothing))
          (0, 1)
          (mkGem C4)
  assertBool "no classic match after swap alone"
    (not (hasAnyMatch (swapCells board (0, 0) (0, 1)))
       || isRainbowSwap board (0, 0) (0, 1))
  assertBool "is rainbow swap" (isRainbowSwap board (0, 0) (0, 1))
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 8
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 0) (0, 1) gs0
  case out of
    NoMatch -> assertFailure "rainbow swap must not roll back as NoMatch"
    InvalidSwap -> assertFailure "rainbow swap must be valid"
    MoveApplied g -> assertBool "gained" (g > 0)
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
  assertEqual "moves spent" (gsMoves gs0 - 1) (gsMoves gs1)

--------------------------------------------------------------------------------
-- Special combos (Line × Bomb)
--------------------------------------------------------------------------------

-- | Swapping Line + Bomb clears a 3-row × 3-col cross without needing a 3-match.
special_combo_line_bomb :: Assertion
special_combo_line_bomb = do
  let board =
        setCell
          (setCell stableBoard (4, 3) (Gem C1 LineH 0 Nothing))
          (4, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "is line+bomb combo" (isLineBombCombo board (4, 3) (4, 4))
  assertBool "special combo" (isSpecialCombo board (4, 3) (4, 4))
  -- Seeds on post-swap board: bomb moves to (4,3)
  let swapped = swapCells board (4, 3) (4, 4)
      seeds = comboClearSeeds swapped (4, 3) (4, 4)
  -- At least 3 full rows + 3 full cols = 3*8 + 3*8 - 9 overlap = 39
  assertBool ("big clear seeds " ++ show (length seeds)) (length seeds >= 39)
  let gs0 =
        (newGame defaultConfig 2)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (4, 3) (4, 4) gs0
  case out of
    NoMatch -> assertFailure "combo must not roll back"
    InvalidSwap -> assertFailure "combo must be valid adjacent swap"
    MoveApplied g -> assertBool ("big score, got " ++ show g) (g >= 39 * 10)
    LevelClear s _ -> assertBool "scored" (s >= 0)
    Won s -> assertBool "scored" (s >= 0)
    Lost _ -> pure ()
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)
  assertBool "board stable" (not (hasAnyMatch (gsBoard gs1)))

-- | Rainbow × Line clears all gems of the line's color.
special_combo_rainbow_line :: Assertion
special_combo_rainbow_line = do
  let board =
        setCell
          (setCell stableBoard (5, 2) (Gem C2 Rainbow 0 Nothing))
          (5, 3)
          (Gem C4 LineV 0 Nothing)
  assertBool "rainbow+line" (isRainbowLineCombo board (5, 2) (5, 3))
  assertBool "special" (isSpecialCombo board (5, 2) (5, 3))
  let gs0 =
        (newGame defaultConfig 4)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (5, 2) (5, 3) gs0
  case out of
    NoMatch -> assertFailure "must apply"
    InvalidSwap -> assertFailure "must be valid"
    MoveApplied g -> assertBool ("scored " ++ show g) (g >= 40)
    _ -> pure ()
  assertEqual "moves" (gsMoves gs0 - 1) (gsMoves gs1)

special_combo_bomb_bomb :: Assertion
special_combo_bomb_bomb = do
  let board =
        setCell
          (setCell stableBoard (3, 3) (Gem C1 Bomb 0 Nothing))
          (3, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "bomb×bomb" (isBombBombCombo board (3, 3) (3, 4))
  let seeds = comboClearSeeds (swapCells board (3, 3) (3, 4)) (3, 3) (3, 4)
  assertBool ("5x5-ish seeds " ++ show (length seeds)) (length seeds >= 25)
  let (gs1, out) = trySwap (3, 3) (3, 4) (newGame defaultConfig 1) { gsBoard = board, gsOver = Nothing, gsMoves = 5 }
  case out of
    NoMatch -> assertFailure "must apply"
    MoveApplied g -> assertBool "big" (g >= 200)
    _ -> pure ()
  assertBool "stable" (not (hasAnyMatch (gsBoard gs1)))

special_combo_line_line :: Assertion
special_combo_line_line = do
  let board =
        setCell
          (setCell stableBoard (2, 2) (Gem C3 LineH 0 Nothing))
          (2, 3)
          (Gem C4 LineV 0 Nothing)
  assertBool "line×line" (isLineLineCombo board (2, 2) (2, 3))
  let (gs1, out) = trySwap (2, 2) (2, 3) (newGame defaultConfig 1) { gsBoard = board, gsOver = Nothing, gsMoves = 5 }
  case out of
    NoMatch -> assertFailure "must apply"
    MoveApplied g -> assertBool ("row+col score " ++ show g) (g >= 150)
    _ -> pure ()
  assertEqual "moves" (4 :: Int) (gsMoves gs1)

--------------------------------------------------------------------------------
-- Stability cruise: Rainbow×special / carry·stars / curtain / grass·vine / Freeze
--------------------------------------------------------------------------------

-- | Rainbow must not expandSpecials into its own spawn color (activation = rainbowClearSeeds).
rainbow_expand_noop :: Assertion
rainbow_expand_noop = do
  let board = setCell stableBoard (3, 3) (Gem C1 Rainbow 0 Nothing)
      -- Sprinkle other C1 so a buggy expand would pick them up
      board' =
        foldl
          (\b p -> setCell b p (mkGem C1))
          board
          [(0, 0), (0, 2), (2, 0), (7, 7)]
      expanded = expandSpecials board' [(3, 3)]
  assertEqual "rainbow expand is identity" [(3, 3)] expanded

-- | Rainbow×Normal: seeds + expandSpecials stay on partner color (+ rainbow cell), not own color.
rainbow_swap_partner_not_own_color :: Assertion
rainbow_swap_partner_not_own_color = do
  let board0 =
        foldl
          (\b p -> setCell b p (mkGem C1))
          stableBoard
          [(0, 0), (0, 2), (2, 0), (7, 6)]
      board =
        setCell
          (setCell board0 (4, 4) (Gem C1 Rainbow 0 Nothing))
          (4, 5)
          (mkGem C2)
      swapped = swapCells board (4, 4) (4, 5)
      seeds = rainbowClearSeeds swapped (4, 4) (4, 5)
      expanded = expandSpecials swapped seeds
      ownExtras =
        [ p
        | p <- expanded
        , p `notElem` seeds
        , case getCell swapped p of
            Gem C1 _ _ _ -> True
            _ -> False
        ]
  assertBool "seeds include rainbow" $
    any (\p -> isRainbow (getCell swapped p)) seeds
  assertEqual "no own-color extras from expand" ([] :: [Pos]) ownExtras
  assertEqual "expand == seeds (no rainbow self-blast)" (length seeds) (length expanded)
  -- Live swap still applies and consumes rainbow
  let gs0 =
        (newGame defaultConfig 3)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 8
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (4, 4) (4, 5) gs0
  case out of
    NoMatch -> assertFailure "rainbow swap must apply"
    InvalidSwap -> assertFailure "rainbow swap must be valid"
    _ -> pure ()
  assertBool "rainbow gone" $
    not (isRainbow (getCell (gsBoard gs1) (4, 4)))
      && not (isRainbow (getCell (gsBoard gs1) (4, 5)))

-- | Rainbow×Bomb: partner-color clear seeds expand the Bomb into a 3×3 blast.
rainbow_x_bomb_blast_expands :: Assertion
rainbow_x_bomb_blast_expands = do
  let board =
        setCell
          (setCell stableBoard (3, 3) (Gem C5 Rainbow 0 Nothing))
          (3, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "rainbow swap" (isRainbowSwap board (3, 3) (3, 4))
  assertBool "not a listed specialCombo (goes rainbow path)" $
    not (isSpecialCombo board (3, 3) (3, 4))
  let swapped = swapCells board (3, 3) (3, 4)
      seeds = rainbowClearSeeds swapped (3, 3) (3, 4)
      expanded = expandSpecials swapped seeds
      -- After swap Bomb sits at (3,3); its 3×3 should appear in expanded
      bomb3x3 =
        [ (r, c)
        | r <- [2 .. 4]
        , c <- [2 .. 4]
        ]
  assertBool "bomb in seeds" ((3, 3) `elem` seeds)
  assertBool ("3x3 subset of expanded, got " ++ show (length expanded)) $
    all (`elem` expanded) bomb3x3
  assertBool "expanded beyond color seeds" (length expanded > length seeds)
  let gs0 =
        (newGame defaultConfig 4)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 6
          , gsBelts = []
          , gsUfos = []
          , gsScore = 0
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  case out of
    NoMatch -> assertFailure "must apply"
    InvalidSwap -> assertFailure "must be valid"
    MoveApplied g -> assertBool ("scored " ++ show g) (g >= 40)
    _ -> pure ()
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)

-- | Rainbow×Flip activates like Rainbow×Countdown: partner front color is the
-- clear target even when the swap forms no classic 3-match. Partner Flip flips
-- to its back face (does not hole); other front-color gems clear.
rainbow_swap_flip_partner :: Assertion
rainbow_swap_flip_partner = do
  let board =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 Nothing))
          (0, 1)
          (mkFlip C4 C1)
  assertBool "is rainbow×flip swap" (isRainbowSwap board (0, 0) (0, 1))
  assertBool "symmetric flip×rainbow" (isRainbowSwap board (0, 1) (0, 0))
  assertBool "no classic match required" $
    not (hasAnyMatch (swapCells board (0, 0) (0, 1)))
  let swapped = swapCells board (0, 0) (0, 1)
      seeds = rainbowClearSeeds swapped (0, 0) (0, 1)
  assertBool "seeds include rainbow" $
    any (\p -> isRainbow (getCell swapped p)) seeds
  assertBool "seeds include partner flip" ((0, 0) `elem` seeds)  -- Flip landed at (0,0)
  assertBool "seeds include other C4" $
    any
      ( \p ->
          p /= (0, 0)
            && case getCell swapped p of
              Gem C4 _ _ _ -> True
              Flip C4 _ -> True
              _ -> False
      )
      seeds
  -- Unit: direct seed hit flips partner (gem stays as back color)
  let (bIced, iceFree) = chipIceOnClear swapped [(0, 0)]
  assertBool "partner not holed" ((0, 0) `notElem` iceFree)
  assertEqual "flipped to back C1" (Just C1) (cellColor (getCell bIced (0, 0)))
  assertBool "no longer flip" (not (isFlip (getCell bIced (0, 0))))
  -- Live trySwap must apply (regression: used to NoMatch-rollback)
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 8
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (0, 0) (0, 1) gs0
  case out of
    NoMatch -> assertFailure "rainbow×flip must not roll back as NoMatch"
    InvalidSwap -> assertFailure "rainbow×flip must be valid"
    MoveApplied g -> assertBool "gained" (g > 0)
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
  assertBool "rainbow consumed" $
    not (isRainbow (getCell (gsBoard gs1) (0, 0)))
      && not (isRainbow (getCell (gsBoard gs1) (0, 1)))
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)

--------------------------------------------------------------------------------
-- Soft-locked Line/Bomb must not expand (ice>1 / Chain / Curtain)
--------------------------------------------------------------------------------

-- | ice>1 and Chain/Curtain peel-locks must suppress Line/Bomb expandSpecials.
-- Regression: expand fired while chipIce only soft-chipped, clearing whole rows
-- / 3×3 around still-locked specials (and Chain+Line hammer double-peeled via
-- adjacent after the illicit row clear).
soft_lock_blocks_special_expand :: Assertion
soft_lock_blocks_special_expand = do
  -- ice=2 LineH in a 3-match: chips ice only; does NOT clear the rest of the row.
  let boardIce =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 LineH 2 Nothing))
          (3, 2)
          (mkGem C1)
  assertBool "ice match" (not (null (findMatches boardIce)))
  let expIce = expandSpecials boardIce (findMatches boardIce)
  assertEqual "ice>1 LineH does not expand row" (sort (findMatches boardIce)) (sort expIce)
  let CascadeRun {crBoard = bIce, crTally = CascadeTally {ctCells = nIce}} = cascadeMatches Nothing [] [] (mkStdGen 31) boardIce
  assertEqual "only two match partners clear" (2 :: Int) nIce
  assertEqual "LineH survives" (Just LineH) (cellKind (getCell bIce (3, 1)))
  assertEqual "ice chipped 2→1" (1 :: Int) (iceLayers (getCell bIce (3, 1)))
  assertBool "far cell (3,7) untouched kind" $
    isGem (getCell bIce (3, 7)) && cellKind (getCell bIce (3, 7)) == Just Normal
  -- Control: last ice (ice==1) LineH still expands the row.
  let boardLast =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 LineH 1 Nothing))
          (3, 2)
          (mkGem C1)
      expLast = expandSpecials boardLast (findMatches boardLast)
  assertEqual "last-ice expands full row" (8 :: Int) (length [c | (3, c) <- expLast])
  -- ice=2 Bomb: no 3×3 while soft-locked.
  let boardBomb =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 Bomb 2 Nothing))
          (3, 2)
          (mkGem C1)
      expBomb = expandSpecials boardBomb (findMatches boardBomb)
  assertEqual "ice>1 Bomb does not blast" (sort (findMatches boardBomb)) (sort expBomb)
  -- Hammer on Chain+LineH: peel one layer only (no row clear / no double peel).
  let boardCh = setCell stableBoard (4, 4) (Gem C2 LineH 0 (Just (Chain 2)))
      gsCh0 =
        (newGame defaultConfig 11)
          { gsBoard = boardCh
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
      (gsCh1, outCh) = useHammer (4, 4) gsCh0
  case outCh of
    NoMatch -> assertFailure "hammer peel should apply"
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cellCh = getCell (gsBoard gsCh1) (4, 4)
  assertEqual "LineH kept" (Just LineH) (cellKind cellCh)
  assertBool "chain peeled 2→1" (hasChain cellCh && chainLayers cellCh == 1)
  assertEqual "no illicit row score" (0 :: Int) (gsScore gsCh1)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsCh1)
  -- Curtain+Bomb: expandSpecials is identity on the seed.
  let boardCu = setCell stableBoard (5, 5) (Gem C3 Bomb 0 (Just (Curtain 2)))
      expCu = expandSpecials boardCu [(5, 5)]
  assertEqual "curtain Bomb does not blast" [(5, 5)] expCu

--------------------------------------------------------------------------------
-- Line/Bomb blast must not double-peel direct-hit layered obstacles
--------------------------------------------------------------------------------

-- | Chain/Curtain/Stone/Safe on a Line clear path already take one direct chip
-- via chipIceOnClear; ortho adjacent peels from neighbor holes must not chip
-- them again (regression: Chain2/Stone2/Safe2 fully cleared in one Line wave).
line_blast_no_double_peel :: Assertion
line_blast_no_double_peel = do
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
  -- Chain 2 on blast path: peel once → Chain 1 (not fully unlocked).
  let CascadeRun {crBoard = bCh} = cascadeMatches Nothing [] [] (mkStdGen 41) (lineBoard (Gem C2 Normal 0 (Just (Chain 2))))
      cellCh = getCell bCh (3, 5)
  assertBool "chain survives" (hasChain cellCh)
  assertEqual "chain peeled once 2→1" (1 :: Int) (chainLayers cellCh)
  -- Curtain 2: same single peel.
  let CascadeRun {crBoard = bCu} = cascadeMatches Nothing [] [] (mkStdGen 42) (lineBoard (Gem C2 Normal 0 (Just (Curtain 2))))
      cellCu = getCell bCu (3, 5)
  assertBool "curtain survives" (hasCurtain cellCu)
  assertEqual "curtain peeled once 2→1" (1 :: Int) (curtainLayers cellCu)
  -- Stone 2: chip once → Stone 1 (not removed).
  let CascadeRun {crBoard = bSt} = cascadeMatches Nothing [] [] (mkStdGen 43) (lineBoard (mkStoneLayers 2))
      cellSt = getCell bSt (3, 5)
  assertBool "stone survives" (isStone cellSt)
  assertEqual "stone chipped once 2→1" (1 :: Int) (stoneLayers cellSt)
  -- Safe 2: chip once → Safe 1 (not opened to Cookie).
  let CascadeRun {crBoard = bSa} = cascadeMatches Nothing [] [] (mkStdGen 44) (lineBoard (mkSafeLayers 2))
      cellSa = getCell bSa (3, 5)
  assertBool "safe survives" (isSafe cellSa)
  assertEqual "safe chipped once 2→1" (1 :: Int) (safeLayers cellSa)
  -- Control: Chain 1 on path fully unlocks (single peel strips last layer).
  let CascadeRun {crBoard = bC1} = cascadeMatches Nothing [] [] (mkStdGen 45) (lineBoard (Gem C2 Normal 0 (Just (Chain 1))))
  assertBool "chain1 unlocked" (not (hasChain (getCell bC1 (3, 5))))
  -- Control: adjacent-only Chain 2 (not on blast) still peels once.
  let boardAdj =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (Gem C2 Normal 0 (Just (Chain 2)))
      CascadeRun {crBoard = bAdj} = cascadeMatches Nothing [] [] (mkStdGen 46) boardAdj
      cellAdj = getCell bAdj (4, 1)
  assertBool "adj chain survives" (hasChain cellAdj)
  assertEqual "adj chain peeled 2→1" (1 :: Int) (chainLayers cellAdj)

--------------------------------------------------------------------------------
-- Soft-locked Rainbow / special combo must not fire (parity with expandSpecials)
--------------------------------------------------------------------------------

-- | ice>1 / Curtain Rainbow×gem must not color-clear (Line/Bomb soft-lock parity).
-- Regression: isRainbowSwap ignored soft-lock, so ice=2 Rainbow cleared partner
-- color while surviving (fire-and-survive). Last-ice Rainbow still activates.
soft_lock_blocks_rainbow_swap :: Assertion
soft_lock_blocks_rainbow_swap = do
  let mkGs board =
        (newGame defaultConfig 11)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
      countC4 b =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , case getCell b (r, c) of
              Gem C4 _ _ _ -> True
              Flip C4 _ -> True
              Countdown C4 _ -> True
              _ -> False
          ]
  -- ice=2 Rainbow × C4: not a rainbow activation; NoMatch if swap forms no match.
  let boardIce =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 2 Nothing))
          (0, 1)
          (mkGem C4)
  assertBool "ice>1 Rainbow does not soft-activate" $
    not (isRainbowSwap boardIce (0, 0) (0, 1))
  assertBool "no classic match after swap" $
    not (hasAnyMatch (swapCells boardIce (0, 0) (0, 1)))
  let (gsIce, outIce) = trySwap (0, 0) (0, 1) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    InvalidSwap -> assertFailure "swap geometry ok"
    MoveApplied _ -> assertFailure "ice>1 Rainbow must not fire color clear"
    other -> assertFailure ("unexpected: " ++ show other)
  assertEqual "board unchanged on rollback" boardIce (gsBoard gsIce)
  assertEqual "moves unchanged" (10 :: Int) (gsMoves gsIce)
  -- Curtain Rainbow × C4: same soft-lock (Curtain allows swap, blocks activate).
  let boardCu =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 (Just (Curtain 2))))
          (0, 1)
          (mkGem C4)
  assertBool "curtain Rainbow does not activate" $
    not (isRainbowSwap boardCu (0, 0) (0, 1))
  let (gsCu, outCu) = trySwap (0, 0) (0, 1) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain Rainbow must not fire"
    _ -> assertFailure "curtain Rainbow must NoMatch-rollback"
  assertEqual "curtain board unchanged" boardCu (gsBoard gsCu)
  -- Control: ice==1 Rainbow still activates and clears partner color.
  let boardLast =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 1 Nothing))
          (0, 1)
          (mkGem C4)
  assertBool "last-ice Rainbow activates" (isRainbowSwap boardLast (0, 0) (0, 1))
  let c4Before = countC4 boardLast
      (gsLast, outLast) = trySwap (0, 0) (0, 1) (mkGs boardLast)
  case outLast of
    NoMatch -> assertFailure "last-ice Rainbow must apply"
    InvalidSwap -> assertFailure "last-ice Rainbow must be valid"
    MoveApplied g -> assertBool "gained" (g > 0)
    _ -> pure ()
  assertBool "partner color reduced" (countC4 (gsBoard gsLast) < c4Before)
  assertEqual "move spent" (9 :: Int) (gsMoves gsLast)

-- | ice>1 / Curtain Line×Bomb must not fire combo geometry (soft-lock parity).
-- Regression: isSpecialCombo was kind-only, so soft-locked Line×Bomb still
-- cleared the full cross via comboClearSeeds while Line survived.
soft_lock_blocks_special_combo :: Assertion
soft_lock_blocks_special_combo = do
  let mkGs board =
        (newGame defaultConfig 13)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
  -- ice=2 LineH × Bomb: combo blocked.
  let boardIce =
        setCell
          (setCell stableBoard (3, 3) (Gem C1 LineH 2 Nothing))
          (3, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "kinds look like line-bomb" (isLineBombCombo boardIce (3, 3) (3, 4))
  assertBool "soft ice blocks combo" $
    not (isSpecialCombo boardIce (3, 3) (3, 4))
  assertBool "no classic match" $
    not (hasAnyMatch (swapCells boardIce (3, 3) (3, 4)))
  let (gsIce, outIce) = trySwap (3, 3) (3, 4) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 Line×Bomb must not fire combo"
    _ -> assertFailure "ice>1 Line×Bomb must NoMatch-rollback"
  assertEqual "ice combo board unchanged" boardIce (gsBoard gsIce)
  -- Curtain Line × Bomb: combo blocked.
  let boardCu =
        setCell
          (setCell stableBoard (4, 3) (Gem C1 LineH 0 (Just (Curtain 2))))
          (4, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "curtain blocks combo" $
    not (isSpecialCombo boardCu (4, 3) (4, 4))
  let (gsCu, outCu) = trySwap (4, 3) (4, 4) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain Line×Bomb must not fire"
    _ -> assertFailure "curtain Line×Bomb must NoMatch-rollback"
  assertEqual "curtain combo board unchanged" boardCu (gsBoard gsCu)
  -- Control: unlocked Line×Bomb still fires.
  let boardOk =
        setCell
          (setCell stableBoard (5, 3) (Gem C1 LineH 0 Nothing))
          (5, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "unlocked combo" (isSpecialCombo boardOk (5, 3) (5, 4))
  let (gsOk, outOk) = trySwap (5, 3) (5, 4) (mkGs boardOk)
  case outOk of
    NoMatch -> assertFailure "unlocked Line×Bomb must apply"
    InvalidSwap -> assertFailure "unlocked Line×Bomb must be valid"
    MoveApplied g -> assertBool "combo score" (g > 0)
    _ -> pure ()
  assertEqual "move spent" (9 :: Int) (gsMoves gsOk)

--------------------------------------------------------------------------------
-- Soft-lock FreeSwap entry + asymmetric double-Rainbow (boundary)
--------------------------------------------------------------------------------

-- | useFreeSwap must honor the same soft-lock gates as trySwap (isRainbowSwap /
-- isSpecialCombo). ice>1 / Curtain Rainbow×gem and ice>1 Line×Bomb must NoMatch
-- without spending the free-swap charge.
soft_lock_blocks_freeswap_activation :: Assertion
soft_lock_blocks_freeswap_activation = do
  let mkGs board =
        (newGame defaultConfig 17)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsFreeSwaps = 2
          }
  -- ice=2 Rainbow × C4 via free-swap (non-adjacent also OK for booster).
  let boardIce =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 2 Nothing))
          (2, 2)
          (mkGem C4)
  assertBool "ice>1 Rainbow not soft-activate" $
    not (isRainbowSwap boardIce (0, 0) (2, 2))
  let (gsIce, outIce) = useFreeSwap (0, 0) (2, 2) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 Rainbow free-swap must not color-clear"
    _ -> assertFailure "ice>1 Rainbow free-swap must NoMatch"
  assertEqual "board unchanged" boardIce (gsBoard gsIce)
  assertEqual "charge kept (ice RB)" (2 :: Int) (gsFreeSwaps gsIce)
  -- Curtain Rainbow × gem (Curtain allows geometry; soft-lock blocks activate).
  let boardCu =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 (Just (Curtain 2))))
          (2, 2)
          (mkGem C4)
  assertBool "curtain Rainbow gated" $
    not (isRainbowSwap boardCu (0, 0) (2, 2))
  let (gsCu, outCu) = useFreeSwap (0, 0) (2, 2) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain Rainbow free-swap must not fire"
    _ -> assertFailure "curtain Rainbow free-swap must NoMatch"
  assertEqual "curtain board unchanged" boardCu (gsBoard gsCu)
  assertEqual "charge kept (curtain RB)" (2 :: Int) (gsFreeSwaps gsCu)
  -- ice=2 LineH × Bomb free-swap: combo blocked, charge kept.
  let boardCombo =
        setCell
          (setCell stableBoard (1, 1) (Gem C1 LineH 2 Nothing))
          (5, 5)
          (Gem C2 Bomb 0 Nothing)
  assertBool "kinds look like line-bomb" (isLineBombCombo boardCombo (1, 1) (5, 5))
  assertBool "soft ice blocks combo" $
    not (isSpecialCombo boardCombo (1, 1) (5, 5))
  let (gsCo, outCo) = useFreeSwap (1, 1) (5, 5) (mkGs boardCombo)
  case outCo of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 Line×Bomb free-swap must not combo"
    _ -> assertFailure "ice>1 Line×Bomb free-swap must NoMatch"
  assertEqual "combo board unchanged" boardCombo (gsBoard gsCo)
  assertEqual "charge kept (combo)" (2 :: Int) (gsFreeSwaps gsCo)

-- | Double-Rainbow requires BOTH endpoints to specialActivates. ice>1 on one
-- side must not clear the board (conjunction branch of isRainbowSwap).
soft_lock_blocks_double_rainbow :: Assertion
soft_lock_blocks_double_rainbow = do
  let mkGs board =
        (newGame defaultConfig 19)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
      countGems b =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isGem (getCell b (r, c))
          ]
  -- ice=2 Rainbow × unlocked Rainbow: gated.
  let boardIce =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 2 Nothing))
          (0, 1)
          (Gem C4 Rainbow 0 Nothing)
  assertBool "asymmetric dbl soft-locked" $
    not (isRainbowSwap boardIce (0, 0) (0, 1))
  assertBool "symmetric order also gated" $
    not (isRainbowSwap boardIce (0, 1) (0, 0))
  let gemsBefore = countGems boardIce
      (gsIce, outIce) = trySwap (0, 0) (0, 1) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 double-Rainbow must not board-wipe"
    _ -> assertFailure "ice>1 double-Rainbow must NoMatch-rollback"
  assertEqual "board unchanged" boardIce (gsBoard gsIce)
  assertEqual "moves unchanged" (10 :: Int) (gsMoves gsIce)
  assertEqual "gems preserved" gemsBefore (countGems (gsBoard gsIce))
  -- Curtain on one Rainbow: same conjunction gate.
  let boardCu =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 (Just (Curtain 1))))
          (0, 1)
          (Gem C4 Rainbow 0 Nothing)
  assertBool "curtain dbl soft-locked" $
    not (isRainbowSwap boardCu (0, 0) (0, 1))
  let (gsCu, outCu) = trySwap (0, 0) (0, 1) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain double-Rainbow must not fire"
    _ -> assertFailure "curtain double-Rainbow must NoMatch"
  assertEqual "curtain board unchanged" boardCu (gsBoard gsCu)
  -- Control: both unlocked double-Rainbow still activates.
  let boardOk =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 Nothing))
          (0, 1)
          (Gem C4 Rainbow 0 Nothing)
  assertBool "unlocked dbl activates" (isRainbowSwap boardOk (0, 0) (0, 1))
  let (gsOk, outOk) = trySwap (0, 0) (0, 1) (mkGs boardOk)
  case outOk of
    NoMatch -> assertFailure "unlocked double-Rainbow must apply"
    InvalidSwap -> assertFailure "unlocked double-Rainbow must be valid"
    MoveApplied g -> assertBool "board-wipe score" (g >= 100)
    _ -> pure ()
  assertEqual "move spent" (9 :: Int) (gsMoves gsOk)
  assertBool "score reflects wipe" (gsScore gsOk >= 100)
  -- Refill restores gem count; assert the swapped rainbows were consumed.
  assertBool "endpoint no longer Rainbow" $
    not (isRainbow (getCell (gsBoard gsOk) (0, 0)))
      && not (isRainbow (getCell (gsBoard gsOk) (0, 1)))
