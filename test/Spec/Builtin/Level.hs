{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 关卡级元素（对应 Element/Builtin/Level）：皮带、传送门、飞碟、地毯。
-- （第 1 刀由 Spec.Obstacles.Body / Features 按 Builtin 分组纯搬家而来；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Builtin.Level
  ( tests
  ) where

import Control.Monad (when)
import Data.List (nub, sort)
import Match3.Board.Default (cascadeMatches, clearMatches, builtinHooks)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crTally, crBoard), CascadeTally(CascadeTally, ctCounts))
import Match3.Core
import Match3.Board.Grid (atM, setM, mboardFromRows)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Match3.Counts (noCounts, singleCount)
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "conveyor_cycle_preserves_cells" conveyor_cycle_preserves_cells
  , testCase "conveyor_shifts_after_move" conveyor_shifts_after_move
  , testCase "conveyor_can_create_match" conveyor_can_create_match
  , testCase "portal_teleports_gem" portal_teleports_gem
  , testCase "ufo_collects_target_color" ufo_collects_target_color
  , testCase "ufo_moves_each_cascade" ufo_moves_each_cascade
  , testCase "ufo_goal_counts" ufo_goal_counts
  , testCase "carpet_covers_on_clear" carpet_covers_on_clear
  , testCase "carpet_already_covered_noop" carpet_already_covered_noop
  , testCase "carpet_ice_partial_no_cover" carpet_ice_partial_no_cover
  , testCase "carpet_ice_last_layer_covers" carpet_ice_last_layer_covers
  , testCase "portal_after_belt_match_teleports" portal_after_belt_match_teleports
  , testCase "cookie_bottom_portal_collects" cookie_bottom_portal_collects
  , testCase "ufo_skips_peel_locks" ufo_skips_peel_locks
  , testCase "belt_delivers_cookie_bottom_drains" belt_delivers_cookie_bottom_drains
  , testCase "portal_teleports_flip" portal_teleports_flip
  , testCase "portal_endpoints_not_immortal_blocked" portal_endpoints_not_immortal_blocked
  , testCase "belt_cells_not_stuck_immortal" belt_cells_not_stuck_immortal
  , testCase "ufo_absorb_no_special_expand" ufo_absorb_no_special_expand
  , testCase "carpet_covers_on_cookie_vacate" carpet_covers_on_cookie_vacate
  , testCase "carpet_covers_on_safe_open" carpet_covers_on_safe_open
  , testCase "carpet_covers_on_cookie_bottom_drain" carpet_covers_on_cookie_bottom_drain
  , testCase "carpet_covers_on_portal_cookie_drain" carpet_covers_on_portal_cookie_drain
  , testCase "carpet_covers_on_surprise_safe_bottom" carpet_covers_on_surprise_safe_bottom
  ]

--------------------------------------------------------------------------------
-- Conveyor belts (开心消消乐传送带)
--------------------------------------------------------------------------------

-- | Shifting a cycle permutes cells; multiset of belt cells is unchanged.
conveyor_cycle_preserves_cells :: Assertion
conveyor_cycle_preserves_cells = do
  let belt = [(0, 0), (0, 1), (0, 2), (1, 2)]
      b0 = stableBoard
      cellsBefore = map (getCell b0) belt
      b1 = shiftBelt b0 belt
      cellsAfter = map (getCell b1) belt
  assertEqual "same multiset" (sort cellsBefore) (sort cellsAfter)
  -- Forward: new[i] = old[i-1]
  assertEqual "wrap" (take 1 (reverse cellsBefore)) (take 1 cellsAfter)
  assertEqual "step" (cellsBefore !! 0) (cellsAfter !! 1)
  -- Identity on empty / singleton
  assertEqual "empty" b0 (shiftBelt b0 [])
  assertEqual "singleton" b0 (shiftBelt b0 [(3, 3)])

-- | After a successful move, belt cells advance one step.
conveyor_shifts_after_move :: Assertion
conveyor_shifts_after_move = do
  let belt = [(5, 0), (5, 1), (5, 2)]
      board0 =
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
      -- Distinct colors on belt so we can see the rotation
      board1 =
        setCell
          (setCell
             (setCell board0 (5, 0) (mkGem C1))
             (5, 1)
             (mkGem C2))
          (5, 2)
          (mkGem C3)
      cfg = GameConfig { cfgMoves = 10, cfgGoal = goalScore 1 }  -- terminal => no shuffle
      gs0 =
        (setBelts [belt] $ (newGameAtLevel 0 cfg 3)
          { gsBoard = board1
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          })
      before = map (getCell (gsBoard gs0)) belt
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  assertBool "move ok" $
    case out of
      NoMatch -> False
      InvalidSwap -> False
      _ -> True
  assertBool "not shuffled" (not (gsShuffled gs1))
  let afterCells = map (getCell (gsBoard gs1)) belt
      expected = shiftBelt board1 belt
      expectCells = map (getCell expected) belt
  assertEqual "belt advanced one step" expectCells afterCells
  assertEqual "multiset kept" (sort before) (sort afterCells)

-- | Belt shift can assemble a 3-match that then clears.
conveyor_can_create_match :: Assertion
conveyor_can_create_match = do
  -- Belt moves C1 into a row of two C1s to form a triple.
  -- Row 4: C1, C1, C2 at (4,0)(4,1)(4,2). Belt on col2: (6,2)->(5,2)->(4,2)
  -- Put C1 at (5,2); after shift forward on [(6,2),(5,2),(4,2)]:
  --   new(6,2)=old(4,2)=C2, new(5,2)=old(6,2)=?, new(4,2)=old(5,2)=C1
  -- So we need old(5,2)=C1 to land on (4,2), making (4,0)(4,1)(4,2) all C1.
  let belt = [(6, 2), (5, 2), (4, 2)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (4, 0) (mkGem C1))
                   (4, 1)
                   (mkGem C1))
                (4, 2)
                (mkGem C2))
             (5, 2)
             (mkGem C1))
          (6, 2)
          (mkGem C3)
  assertBool "no match yet" (not (hasAnyMatch board0))
  let shifted = shiftBelt board0 belt
  assertEqual "C1 arrived" (Just C1) (cellColor (getCell shifted (4, 2)))
  assertBool "match formed" (hasAnyMatch shifted)
  -- Via trySwap: match elsewhere + belt creates extra clear
  let boardM =
        setCell
          (setCell
             (setCell
                (setCell board0 (0, 0) (mkGem C4))
                (0, 1)
                (mkGem C4))
             (0, 2)
             (mkGem C5))
          (0, 3)
          (mkGem C4)
      cfg = GameConfig { cfgMoves = 10, cfgGoal = goalScore 99999 }
      gs0 =
        (setBelts [belt] $ (newGameAtLevel 0 cfg 9)
          { gsBoard = boardM
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          })
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    MoveApplied g -> assertBool ("belt cascade scored extra, got " ++ show g) (g >= 60)
    _ -> pure ()
  assertBool "stable after" (not (hasAnyMatch (gsBoard gs1)))

--------------------------------------------------------------------------------
-- Portal / 传送门 (gem on A with hole at B teleports)
--------------------------------------------------------------------------------

portal_teleports_gem :: Assertion
portal_teleports_gem = do
  -- Build MBoard: gem at (0,0), hole at (7,7)
  let setMBoard = setM
      fill = Just (mkGem C5)
      mb0 = mboardFromRows (replicate boardSize (replicate boardSize fill))
      mb1 = setMBoard mb0 (0, 0) (Just (mkGem C1))
      mb2 = setMBoard mb1 (7, 7) Nothing
      portals = [((0, 0), (7, 7))]
      mb3 = applyPortalTeleports (builtinHooks [] portals) mb2
  assertEqual "entrance emptied" Nothing (atM mb3 (0, 0))
  case atM mb3 (7, 7) of
    Just cell -> do
      assertBool "exit got gem" (isGem cell)
      assertEqual "teleported color" (Just C1) (cellColor cell)
    Nothing -> assertFailure "expected gem at exit"
  assertBool "identity on full board" (applyPortalTeleports (builtinHooks [] portals) mb0 == mb0)
  let gs = levelGame 26 42
  assertEqual "two portal pairs" (2 :: Int) (length (gsPortals gs))

-- UFO / 飞碟 (absorb same-color neighbors; move each cascade wave)

-- | UFO absorbs only orthogonally adjacent gems of its target color.
ufo_collects_target_color :: Assertion
ufo_collects_target_color = do
  let b0 = fst (randomStableBoard (mkStdGen 77))
      b1 = setCell b0 (3, 3) (mkGem C5)
      b2 = setCell b1 (3, 4) (mkGem C1)
      b3 = setCell b2 (3, 2) (mkGem C1)
      b4 = setCell b3 (2, 3) (mkGem C2)
      b5 = setCell b4 (4, 3) (mkGem C1)
      ufo = mkUfo (3, 3) C1
      targets = ufoAbsorbTargets b5 ufo
      (absorbed, ufo') = stepUfo b5 ufo
  assertBool "absorbs left" ((3, 2) `elem` targets)
  assertBool "absorbs right" ((3, 4) `elem` targets)
  assertBool "absorbs down" ((4, 3) `elem` targets)
  assertBool "ignores wrong color up" ((2, 3) `notElem` targets)
  assertEqual "step absorbs same set" (sort targets) (sort absorbed)
  assertEqual "3 targets" (3 :: Int) (length targets)
  assertEqual "moved onto first absorbed" (take 1 (sort absorbed)) [ufoCell ufo']
  assertEqual "color preserved" C1 (ufoColor ufo')

-- | Each cascade wave relocates the UFO.
ufo_moves_each_cascade :: Assertion
ufo_moves_each_cascade = do
  let b0 = fst (randomPlayableBoard (mkStdGen 88))
      u0 = mkUfo (0, 0) C1
      paint b ps col = foldl (\bd p -> setCell bd p (mkGem col)) b ps
      b1 = paint b0 [(0, 0), (0, 1), (1, 0)] C2
      (abs1, u1) = stepUfo b1 u0
      (_, u2) = stepUfo b1 u1
  assertEqual "no absorb when no C1 neighbor" ([] :: [Pos]) abs1
  assertBool "first step moves" (ufoCell u1 /= ufoCell u0)
  assertBool "second step moves again" (ufoCell u2 /= ufoCell u1)
  let cfg = GameConfig 20 (goalCount CountUfo 99)
      gs0 = (setUfos [u0] $ (newGameAtLevel 12 cfg 91) { gsBoard = b1, gsCounts = noCounts })
  case findHint (gsBoard gs0) of
    Nothing -> assertFailure "board should be playable"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      assertBool "move applied or terminal" $ case out of
        MoveApplied _ -> True
        LevelClear _ _ -> True
        Won _ -> True
        Lost _ -> True
        _ -> False
      assertBool "UFO relocated after cascade" $
        case gsUfos gs1 of
          (u : _) -> ufoCell u /= ufoCell u0
          [] -> False

-- | GoalUfo progress increments with UFO absorbs.
ufo_goal_counts :: Assertion
ufo_goal_counts = do
  assertBool "goal unmet at 0" (not (goalMet (goalCount CountUfo 2) 0 noCounts))
  assertBool "goal met at 2" (goalMet (goalCount CountUfo 2) 0 (singleCount CountUfo 2))
  assertEqual "progress" (2 :: Int) (goalProgress (goalCount CountUfo 5) 0 (singleCount CountUfo 2))
  assertEqual "target" (5 :: Int) (goalTarget (goalCount CountUfo 5))
  let b0 = fst (randomStableBoard (mkStdGen 101))
      b1 = setCell b0 (3, 3) (mkGem C5)
      b2 = setCell b1 (3, 2) (mkGem C1)
      b3 = setCell b2 (3, 4) (mkGem C1)
      b4 = setCell b3 (4, 3) (mkGem C1)
      ufo = mkUfo (3, 3) C1
      targets = ufoAbsorbTargets b4 ufo
      n = length targets
  assertBool "has absorb targets" (n >= 2)
  let cfg = GameConfig 15 (goalCount CountUfo n)
      gs0 =
        (setUfos [ufo] $ (newGameAtLevel 12 cfg 55)
          { gsBoard = b4
          , gsCounts = noCounts
          })
      (_, ufo') = stepUfo b4 ufo
      gsSim =
        (setUfos [ufo'] $ gs0
          { gsCounts = singleCount CountUfo n
          })
  assertBool
    "simulated goal met"
    (gsGoalMet gsSim)
  -- Level table includes GoalUfo stages
  assertBool
    "campaign has GoalUfo"
    (any (\g -> case goalView g of ViewCount CountUfo _ -> True; _ -> False) (map lvlGoal allLevels))

--------------------------------------------------------------------------------
-- Carpet / 地毯 (floor tiles covered when gems on them clear)
--------------------------------------------------------------------------------

carpet_covers_on_clear :: Assertion
carpet_covers_on_clear = do
  -- Horizontal match on row 3 cols 0-2; carpet targets under those cells
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
      cfg = GameConfig 20 (goalCount CountCarpets 8)
      gs0 =
        (setBelts [] . setCarpetOpen [(3, 0), (3, 1), (3, 2), (4, 4)] $ (newGameAtLevel 0 cfg 7)
          { gsBoard = board0
          , gsOver = Nothing
          , gsMoves = 20
          , gsScore = 0
          , gsCounts = noCounts
          , gsGoal = goalCount CountCarpets 8
          })
  assertBool "match ready" (hasAnyMatch board0)
  -- Force cascade via trySwap of a neighboring pair that creates/uses the match
  -- Board already has match; use hammer on one carpet cell to clear seeds then cascade
  let (gs1, out) = useHammer (3, 1) gs0 { gsHammers = 2 }
  case out of
    InvalidSwap -> assertFailure "hammer should apply"
    _ -> pure ()
  assertBool
    ("carpet covered some of match cells, covered=" ++ show (gsCount CountCarpets gs1)
       ++ " open=" ++ show (gsCarpetOpen gs1))
    (gsCount CountCarpets gs1 >= 1)
  assertBool "covered cells removed from open" $
    all (`notElem` gsCarpetOpen gs1) [(3, 0), (3, 1), (3, 2)]
      || gsCount CountCarpets gs1 >= 1
  -- Pure unit
  let (open', n) = coverCarpets [(3, 0), (3, 1), (4, 4)] [(3, 0), (3, 1), (5, 5)]
  assertEqual "pure cover count" (2 :: Int) n
  assertEqual "pure remain" [(4, 4)] open'

carpet_already_covered_noop :: Assertion
carpet_already_covered_noop = do
  -- Covering the same cell twice must not double-count
  let open0 = [(2, 2), (2, 3)]
      (open1, n1) = coverCarpets open0 [(2, 2)]
      (open2, n2) = coverCarpets open1 [(2, 2), (2, 3)]
  assertEqual "first hit" (1 :: Int) n1
  assertEqual "second hit only new" (1 :: Int) n2
  assertEqual "all covered" ([] :: [Pos]) open2
  -- Re-feeding already-covered positions into coverCarpets is a no-op
  let (open3, n3) = coverCarpets [] [(2, 2), (9, 9)]
  assertEqual "empty open stays empty" ([] :: [Pos]) open3
  assertEqual "no spurious covers" (0 :: Int) n3
  -- Game path: cover once, then clear same cell again with empty open → count unchanged
  let cfg = GameConfig 15 (goalCount CountCarpets 3)
      gs0 =
        (setCarpetOpen [(5, 5)] . setBelts [] . setUfos [] $ (newGameAtLevel 0 cfg 3)
          { gsCounts = noCounts
          , gsGoal = goalCount CountCarpets 3
          , gsHammers = 3
          , gsBoard = setCell stableBoard (5, 5) (mkGem C2)
          })
      (gs1, _) = useHammer (5, 5) gs0
  assertEqual "covered once" (1 :: Int) (gsCount CountCarpets gs1)
  assertEqual "open emptied" ([] :: [Pos]) (gsCarpetOpen gs1)
  let (gs2, _) = useHammer (5, 5) gs1 { gsHammers = 2, gsOver = Nothing }
  assertEqual "second clear does not double-count" (1 :: Int) (gsCount CountCarpets gs2)
  assertEqual "still empty open" ([] :: [Pos]) (gsCarpetOpen gs2)

--------------------------------------------------------------------------------
-- Fragile interaction boundaries (stability cruise)
--------------------------------------------------------------------------------

-- | Ice layers >1 on a carpet tile: chip does NOT cover; only ice-free clears count.
carpet_ice_partial_no_cover :: Assertion
carpet_ice_partial_no_cover = do
  -- Unit: ice>1 excluded from iceFree → coverCarpets ignores that tile.
  let board =
        setCell
          (setCell stableBoard (3, 0) (mkGem C1))
          (3, 3)
          (mkIceGem C1 2)
      seeds = [(3, 0), (3, 1), (3, 3)]
      (bIced, iceFree) = chipIceOnClear board seeds
  assertBool "(3,3) not cleared" ((3, 3) `notElem` iceFree)
  assertEqual "ice chipped to 1" (1 :: Int) (iceLayers (getCell bIced (3, 3)))
  assertBool "(3,0) cleared" ((3, 0) `elem` iceFree)
  let (open', n) = coverCarpets [(3, 3), (3, 0)] iceFree
  assertEqual "one cover" (1 :: Int) n
  assertBool "iced tile still open" ((3, 3) `elem` open')
  assertBool "cleared tile covered" ((3, 0) `notElem` open')
  -- trySwap: carpet under stationary iced gem (3,0); swap forms match including it.
  -- Row3: C1(ice2) C1 C2 C1 — swap (3,2)<->(3,3) → C1(ice2) C1 C1 C2
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkIceGem C1 2))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      gs0 =
        (setBelts [] . setUfos [] . setCarpetOpen [(3, 0)] $ (newGame defaultConfig 3)
          { gsBoard = board0
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsCounts = noCounts
          , gsGoal = goalCount CountCarpets 1
          })
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  -- Partial ice must not cover on the chip-only wave; covered stays 0 if gem survived.
  -- Cascades may later clear the cell — only assert we never over-count past 1 open tile.
  assertBool "covered in {0,1}" (gsCount CountCarpets gs1 <= 1)
  assertBool "open consistent" (length (gsCarpetOpen gs1) + gsCount CountCarpets gs1 == 1)

-- | Last ice layer (ice==1) on carpet: gem clears and carpet covers.
carpet_ice_last_layer_covers :: Assertion
carpet_ice_last_layer_covers = do
  let board = setCell stableBoard (5, 3) (mkIceGem C2 1)
      (bIced, iceFree) = chipIceOnClear board [(5, 3)]
  assertEqual "last ice clears" [(5, 3)] iceFree
  assertEqual "board unchanged pre-hole" (mkIceGem C2 1) (getCell bIced (5, 3))
  let (open', n) = coverCarpets [(5, 3)] iceFree
  assertEqual "covers" (1 :: Int) n
  assertEqual "open empty" ([] :: [Pos]) open'
  -- trySwap: carpet under stationary ice==1 at (5,0)
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (5, 0) (mkIceGem C2 1))
                (5, 1)
                (mkGem C2))
             (5, 2)
             (mkGem C3))
          (5, 3)
          (mkGem C2)
      gs0 =
        (setBelts [] . setUfos [] . setCarpetOpen [(5, 0)] $ (newGame defaultConfig 4)
          { gsBoard = board0
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsCounts = noCounts
          , gsGoal = goalCount CountCarpets 1
          })
      (gs1, out) = trySwap (5, 2) (5, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertEqual "last ice covers carpet" (1 :: Int) (gsCount CountCarpets gs1)
  assertEqual "open emptied" ([] :: [Pos]) (gsCarpetOpen gs1)

-- | Belt shift forms a match that clears portal A; portal teleports B→A before re-gravity.
-- Locks Portal+Belt: belt-created clears are visible to applyPortalTeleports / settle.
portal_after_belt_match_teleports :: Assertion
portal_after_belt_match_teleports = do
  let portals = [((0, 2), (7, 2))]
      belt = [(0, 0), (0, 1), (0, 2), (0, 3)]
      -- Before belt: C1 C1 C2 C1 — after forward shift: C1 C1 C1 C2 (triple includes A)
      board0 =
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
          (7, 2)
          (mkGem C5)
      shifted = shiftBelts board0 [belt]
  assertBool "belt assembled match" (hasAnyMatch shifted)
  assertEqual "A is C1" (Just C1) (cellColor (getCell shifted (0, 2)))
  assertEqual "B is C5" (Just C5) (cellColor (getCell shifted (7, 2)))
  let (mb, n) = clearMatches shifted
  assertBool "cleared triple+" (n >= 3)
  assertEqual "A hole pre-portal" Nothing (atM mb (0, 2))
  assertEqual "B still C5" (Just (mkGem C5)) (atM mb (7, 2))
  -- Direct portal step (pre re-gravity): B→A
  let ported = applyPortalTeleports (builtinHooks [] portals) mb
  assertEqual "C5 teleported to A" (Just (mkGem C5)) (atM ported (0, 2))
  assertEqual "B emptied by portal" Nothing (atM ported (7, 2))
  -- Full settle: gravity may repack column, but B must not keep C5
  let (settled, _, _) = settleBoardPortals (builtinHooks [] portals) mb
  assertBool "B no longer holds C5 after settle" $
    case atM settled (7, 2) of
      Just c -> not (isGem c && cellColor c == Just C5) || False
      Nothing -> True
  assertBool "C5 still somewhere in col 2" $
    any
      ( \r ->
          case atM settled (r, 2) of
            Just c -> isGem c && cellColor c == Just C5
            Nothing -> False
      )
      [0 .. boardSize - 1]
  -- Full trySwap path with belts+portals stays stable and spends a move
  let boardSwap =
        setCell
          (setCell
             (setCell
                (setCell board0 (2, 0) (mkGem C3))
                (2, 1)
                (mkGem C3))
             (2, 2)
             (mkGem C4))
          (2, 3)
          (mkGem C3)
      gs0 =
        (setBelts [belt] . setPortals portals . setUfos [] $ (newGame defaultConfig 9)
          { gsBoard = boardSwap
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsGoal = goalScore 99999
          })
      (gs1, out) = trySwap (2, 2) (2, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match on row2"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "post-move stable" (not (hasAnyMatch (gsBoard gs1)))
  assertEqual "spent one move" (11 :: Int) (gsMoves gs1)

-- | Cookie on a bottom-row portal entrance collects; portal must not snatch it first.
-- Locks Cookie触底 × Portal: drainBottomCookies before applyPortalTeleports.
cookie_bottom_portal_collects :: Assertion
cookie_bottom_portal_collects = do
  let bottom = boardSize - 1
      fill = Just (mkGem C5)
      mb0 = mboardFromRows (replicate boardSize (replicate boardSize fill))
      setMB = setM
      -- Cookie already on bottom portal A; exit B empty (buggy order teleports up)
      mb = setMB (setMB mb0 (bottom, 1) (Just Cookie)) (0, 6) Nothing
      portals = [((0, 6), (bottom, 1))]
      (settled, fallen, _) = settleBoardPortals (builtinHooks [] portals) mb
      cookieLeft =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , case atM settled (r, c) of
            Just Cookie -> True
            _ -> False
        ]
  assertEqual "cookie collected at bottom portal" (1 :: Int) fallen
  assertEqual "no cookie left on board" ([] :: [(Int, Int)]) cookieLeft
  assertBool "exit not holding snatch" $
    case atM settled (0, 6) of
      Just Cookie -> False
      _ -> True
  -- trySwap path with portals: cookie on bottom portal drains into GoalCookie
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (bottom, 1) mkCookie)
                (3, 0)
                (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
      gs0 =
        (setBelts [] . setUfos [] . setPortals portals $ (newGame defaultConfig 23)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsCounts = noCounts
          , gsGoal = goalCount CountCookies 1
          })
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool ("trySwap collected cookie, got " ++ show (gsCount CountCookies gs1))
    (gsCount CountCookies gs1 >= 1)
  assertBool "trySwap cookie gone from bottom" (not (isCookie (getCell (gsBoard gs1) (bottom, 1))))
  assertBool "trySwap cookie not at portal exit" (not (isCookie (getCell (gsBoard gs1) (0, 6))))

-- | UFO must not target peel-locks / multi-ice / Flip (no GoalUfo phantom counts).
ufo_skips_peel_locks :: Assertion
ufo_skips_peel_locks = do
  let row = replicate boardSize (mkGem C5)
      base = boardFromRows $ replicate boardSize row
      u = mkUfo (7, 3) C1
      near lock =
        setCell (setCell base (7, 3) (mkGem C5)) (7, 4) lock
  assertEqual "skips chain" [] (ufoAbsorbTargets (near (mkChainGem C1 2)) u)
  assertEqual "skips curtain" [] (ufoAbsorbTargets (near (mkCurtainGem C1 1)) u)
  assertEqual "skips fog" [] (ufoAbsorbTargets (near (mkFogGem C1 1)) u)
  assertEqual "skips steam" [] (ufoAbsorbTargets (near (mkSteamGem C1)) u)
  assertEqual "skips ice>1" [] (ufoAbsorbTargets (near (mkIceGem C1 2)) u)
  assertEqual "skips flip" [] (ufoAbsorbTargets (near (mkFlip C1 C4)) u)
  assertEqual "takes bare" [(7, 4)] (ufoAbsorbTargets (near (mkGem C1)) u)
  assertEqual "takes freeze" [(7, 4)] (ufoAbsorbTargets (near (mkFreezeGem C1 1)) u)
  assertEqual "takes last-ice" [(7, 4)] (ufoAbsorbTargets (near (mkIceGem C1 1)) u)
  -- Defense-in-depth: if a peel-lock somehow entered absorbed, clear ∩ count is 0.
  -- Seed-clear a chain cell via fromSeeds path is N/A (UFO skips targeting);
  -- verify wrong-color bare still ignored.
  assertEqual "ignores wrong color" [] (ufoAbsorbTargets (near (mkGem C2)) u)

-- | Belt delivers Cookie onto bottom with no follow-up match → must still drain.
-- Regression: runCascadeScoredWithUfos no-ops without settle, stranding GoalCookie.
belt_delivers_cookie_bottom_drains :: Assertion
belt_delivers_cookie_bottom_drains = do
  let belt = [(5, 2), (6, 2), (7, 2), (7, 3)]
      -- Cookie one step before bottom entrance; one belt tick parks it on (7,2)
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (6, 2) mkCookie)
                   (0, 0)
                   (mkGem C1))
                (0, 1)
                (mkGem C1))
             (0, 2)
             (mkGem C2))
          (0, 3)
          (mkGem C1)
      afterBelt = shiftBelts board0 [belt]
  assertBool "belt parks cookie on bottom" (isCookie (getCell afterBelt (7, 2)))
  assertBool "belt alone forms no match" (not (hasAnyMatch afterBelt))
  let gs0 =
        (setBelts [belt] . setUfos [] . setPortals [] $ (newGame defaultConfig 9)
          { gsBoard = board0
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsCounts = noCounts
          , gsGoal = goalCount CountCookies 1
          })
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "row0 match should apply"
    InvalidSwap -> assertFailure "row0 swap valid"
    _ -> pure ()
  let left =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCookie (getCell (gsBoard gs1) (r, c))
        ]
  assertBool ("cookie must drain after belt, left=" ++ show left) (null left)
  assertBool
    ("GoalCookie must count belt-delivered cookie, got " ++ show (gsCount CountCookies gs1))
    (gsCount CountCookies gs1 >= 1)

--------------------------------------------------------------------------------
-- Stability cruise: GoalCookie/Carpet décor seed + portal Flip
--------------------------------------------------------------------------------

-- | Portal teleports Flip (dual-face) the same as gems/countdowns/cookies.
portal_teleports_flip :: Assertion
portal_teleports_flip = do
  let setMBoard = setM
      fill = Just (mkGem C5)
      mb0 = mboardFromRows (replicate boardSize (replicate boardSize fill))
      mb1 = setMBoard mb0 (0, 0) (Just (mkFlip C1 C2))
      mb2 = setMBoard mb1 (7, 7) Nothing
      portals = [((0, 0), (7, 7))]
      mb3 = applyPortalTeleports (builtinHooks [] portals) mb2
  assertEqual "Flip left entrance" Nothing (atM mb3 (0, 0))
  case atM mb3 (7, 7) of
    Just cell -> do
      assertBool "exit is Flip" (isFlip cell)
      assertEqual "front color" (Just C1) (flipFront cell)
      assertEqual "back color" (Just C2) (flipBack cell)
    Nothing -> assertFailure "expected Flip at portal exit"
  -- Countdown still teleports (sibling transferable)
  let mbC =
        setMBoard
          (setMBoard mb0 (1, 1) (Just (Countdown C3 2)))
          (6, 6)
          Nothing
      mbC' = applyPortalTeleports (builtinHooks [] [((1, 1), (6, 6))]) mbC
  assertEqual "CD entrance empty" Nothing (atM mbC' (1, 1))
  case atM mbC' (6, 6) of
    Just (Countdown C3 2) -> pure ()
    other -> assertFailure ("expected Countdown at exit, got " ++ show other)

-- | Campaign portal endpoints must not host immortal blockers (Bottle / Maker /
-- MagicHat / Snail). Those never become holes and are not portal-transferable,
-- so a portal pair with either end occupied forever is dead décor (终章 regression:
-- Bottle was seeded on portal A at (0,3)).
portal_endpoints_not_immortal_blocked :: Assertion
portal_endpoints_not_immortal_blocked = do
  let portalLevels =
        [ (li, gs)
        | li <- [0 .. length allLevels - 1]
        , let gs = levelGame li 42
        , not (null (gsPortals gs))
        ]
  assertBool "campaign has portal levels" (not (null portalLevels))
  mapM_
    ( \(li, gs) -> do
        let endpoints = nub (concatMap (\(a, b) -> [a, b]) (gsPortals gs))
            immortal c =
              isBottle c || isMaker c || isMagicHat c || isSnail c
        mapM_
          ( \p -> do
              let cell = getCell (gsBoard gs) p
              assertBool
                ( "L"
                    ++ show li
                    ++ " portal "
                    ++ show p
                    ++ " immortal blocker: "
                    ++ show cell
                )
                (not (immortal cell))
          )
          endpoints
        -- Finale: portal A must be able to teleport a gem into an empty B
        when (li == 27) $ do
          assertBool "finale bottle relocated off portal+belt" (isBottle (getCell (gsBoard gs) (2, 7)))
          assertBool "finale portal A not bottle" (not (isBottle (getCell (gsBoard gs) (0, 3))))
          assertBool "finale snail off portal row" (isSnail (getCell (gsBoard gs) (2, 0)))
          assertBool "finale portal A not snail" (not (isSnail (getCell (gsBoard gs) (0, 3))))
          let portals = gsPortals gs
              setMBoard = setM
              fill = Just (mkGem C5)
              mb0 = mboardFromRows (replicate boardSize (replicate boardSize fill))
              mb1 = setMBoard mb0 (0, 3) (Just (mkGem C1))
              mb2 = setMBoard mb1 (7, 4) Nothing
              mb3 = applyPortalTeleports (builtinHooks [] portals) mb2
          assertEqual "finale A emptied" Nothing (atM mb3 (0, 3))
          case atM mb3 (7, 4) of
            Just cell -> do
              assertBool "finale B got gem" (isGem cell)
              assertEqual "finale teleported C1" (Just C1) (cellColor cell)
            Nothing -> assertFailure "finale expected gem at portal B"
    )
    portalLevels

-- | Snail must reverse at portal endpoints (immortal + not transferable).
-- Finale regression: snail at (0,4) facing right hit Cookie at (0,5), reversed,
-- then crawled onto portal A (0,3) and permanently killed the pair.
-- | Campaign belt cells must not host stuck immortals (Bottle / Maker / MagicHat /
-- Snail). Those never clear, so one belt slot stays permanently occupied (终章
-- regression: Bottle was seeded on belt cell (1,7) after being moved off portal A).
belt_cells_not_stuck_immortal :: Assertion
belt_cells_not_stuck_immortal = do
  let beltLevels =
        [ (li, gs)
        | li <- [0 .. length allLevels - 1]
        , let gs = levelGame li 42
        , not (null (gsBelts gs))
        ]
  assertBool "campaign has belt levels" (not (null beltLevels))
  mapM_
    ( \(li, gs) -> do
        let cells = nub (concat (gsBelts gs))
            stuck c =
              isBottle c || isMaker c || isMagicHat c || isSnail c
        mapM_
          ( \p -> do
              let cell = getCell (gsBoard gs) p
              assertBool
                ( "L"
                    ++ show li
                    ++ " belt "
                    ++ show p
                    ++ " stuck immortal: "
                    ++ show cell
                )
                (not (stuck cell))
          )
          cells
        when (li == 27) $ do
          assertBool "finale bottle off belt" (isBottle (getCell (gsBoard gs) (2, 7)))
          assertBool "finale belt end not bottle" $
            not (isBottle (getCell (gsBoard gs) (1, 7)))
          assertBool "(2,7) not on finale belt" $
            (2, 7) `notElem` nub (concat (gsBelts gs))
    )
    beltLevels

-- | UFO absorb of Bomb/Line/Rainbow must remove the special without expandSpecials
-- detonation (吸走 ≠ 引爆). Regression: clearFromSeedsDetailed expanded absorbed
-- Bombs into 3×3 / Lines into full rows, wiping cells UFO never targeted.
ufo_absorb_no_special_expand :: Assertion
ufo_absorb_no_special_expand = do
  let paint (r, c) = if even (r + c) then mkGem C4 else mkGem C5
      base = boardFromRows $
        [[paint (r, c) | c <- [0 .. boardSize - 1]] | r <- [0 .. boardSize - 1]]
      boardBomb =
        setCell
          (setCell base (3, 3) (Gem C1 Bomb 0 Nothing))
          (3, 1)
          (mkGem C3)
      u = mkUfo (3, 2) C1
  assertBool "bomb board stable" (not (hasAnyMatch boardBomb))
  assertEqual "absorbs bomb" [(3, 3)] (ufoAbsorbTargets boardBomb u)
  -- Unit: raw Bomb/Line seeds expand; Normal-masked seeds do not
  -- (same mask Board.clearUfoAbsorbed applies before clearFromSeedsDetailed).
  let rawBomb = expandSpecials boardBomb [(3, 3)]
      maskedBomb = setCell boardBomb (3, 3) (Gem C1 Normal 0 Nothing)
  assertBool "raw bomb expands beyond seed" (length rawBomb > 1)
  assertEqual "masked bomb is seed-only" [(3, 3)] (expandSpecials maskedBomb [(3, 3)])
  let boardLine =
        foldl
          (\b (p, c) -> setCell b p c)
          base
          [ ((2, 2), mkGem C2)
          , ((2, 3), Gem C1 LineH 0 Nothing)
          , ((2, 4), mkGem C3)
          ]
      rawLine = expandSpecials boardLine [(2, 3)]
      maskedLine = setCell boardLine (2, 3) (Gem C1 Normal 0 Nothing)
  assertBool "raw LineH expands full row" $
    all (`elem` rawLine) [(2, c) | c <- [0 .. boardSize - 1]]
  assertEqual "masked LineH seed-only" [(2, 3)] (expandSpecials maskedLine [(2, 3)])
  -- Live cascade: UFO absorbs Bomb; GoalUfo counts; cell is no longer Bomb
  let boardMatch =
        setCell
          (setCell
             (setCell
                (setCell boardBomb (0, 0) (mkGem C3))
                (0, 1)
                (mkGem C3))
             (0, 2)
             (mkGem C2))
          (0, 3)
          (mkGem C3)
      swapped = swapCells boardMatch (0, 2) (0, 3)
  assertBool "setup match" (hasAnyMatch swapped)
  let CascadeRun {crBoard = b2, crTally = CascadeTally {ctCounts = (countOf CountUfo -> uAbs2)}} = cascadeMatches (Just (0, 3)) (builtinHooks [u] []) (mkStdGen 5) swapped
  assertBool "UFO absorbed bomb" (uAbs2 >= 1)
  assertBool "bomb cell no longer Bomb" $
    case getCell b2 (3, 3) of
      Gem _ Bomb _ _ -> False
      _ -> True
  -- Live LineH absorb: special gone, GoalUfo counts
  let uL = mkUfo (2, 2) C1
  assertEqual "absorbs line" [(2, 3)] (ufoAbsorbTargets boardLine uL)
  assertBool "line board stable" (not (hasAnyMatch boardLine))
  let boardLM =
        setCell
          (setCell
             (setCell
                (setCell boardLine (6, 0) (mkGem C3))
                (6, 1)
                (mkGem C3))
             (6, 2)
             (mkGem C4))
          (6, 3)
          (mkGem C3)
      swappedL = swapCells boardLM (6, 2) (6, 3)
  assertBool "line setup match" (hasAnyMatch swappedL)
  let CascadeRun {crBoard = bL, crTally = CascadeTally {ctCounts = (countOf CountUfo -> uAbsL)}} = cascadeMatches (Just (6, 3)) (builtinHooks [uL] []) (mkStdGen 11) swappedL
  assertBool "absorbed line" (uAbsL >= 1)
  assertBool "line cell no longer LineH" $
    case getCell bL (2, 3) of
      Gem _ LineH _ _ -> False
      _ -> True

--------------------------------------------------------------------------------
-- Carpet × Cookie vacate / Safe open (GoalCarpet soft-lock fix)
--------------------------------------------------------------------------------

-- | Cookie on a carpet tile that falls away (column clear → gravity → drain)
-- must cover that carpet — Cookie never enters clear-hole lists.
carpet_covers_on_cookie_vacate :: Assertion
carpet_covers_on_cookie_vacate = do
  let board = setCell stableBoard (3, 3) Cookie
      gs0 =
        (setCarpetOpen [(3, 3)] . setBelts [] . setPortals [] . setUfos [] $ (newGameAtLevel 0 (GameConfig 20 (goalCount CountCarpets 1)) 1)
          { gsBoard = board
          , gsCounts = noCounts
          , gsOver = Nothing
          , gsCrossClears = 1
          , gsMoves = 20
          , gsGoal = goalCount CountCarpets 1
          })
      (gs1, out) = useCrossClear (5, 3) gs0
  case out of
    InvalidSwap -> assertFailure "cross should fire"
    NoMatch -> assertFailure "cross should apply"
    _ -> pure ()
  assertBool "cookie drained or left (3,3)" $
    not (isCookie (getCell (gsBoard gs1) (3, 3)))
  assertEqual "carpet covered by cookie vacate" (1 :: Int) (gsCount CountCarpets gs1)
  assertEqual "no open carpets" ([] :: [Pos]) (gsCarpetOpen gs1)
  assertEqual "GoalCarpet meter" (1 :: Int) (gsCollected gs1)

-- | Safe opening to Cookie on a carpet tile covers that carpet (Safe→Cookie
-- never digs a clear-hole either).
carpet_covers_on_safe_open :: Assertion
carpet_covers_on_safe_open = do
  let board = setCell stableBoard (3, 3) (mkSafeLayers 1)
      gs0 =
        (setCarpetOpen [(3, 3)] . setBelts [] . setPortals [] . setUfos [] $ (newGameAtLevel 0 (GameConfig 20 (goalCount CountCarpets 1)) 2)
          { gsBoard = board
          , gsCounts = noCounts
          , gsOver = Nothing
          , gsHammers = 2
          , gsMoves = 20
          , gsGoal = goalCount CountCarpets 1
          })
      -- Hammer an orthogonal neighbor: adj peel opens Safe → Cookie
      (gs1, out) = useHammer (3, 2) gs0
  case out of
    InvalidSwap -> assertFailure "hammer should fire"
    NoMatch -> assertFailure "hammer should apply"
    _ -> pure ()
  assertBool "safe opened to cookie" $
    isCookie (getCell (gsBoard gs1) (3, 3)) || not (isSafe (getCell (gsBoard gs1) (3, 3)))
  assertEqual "safes opened" (1 :: Int) (gsCount CountSafes gs1)
  assertEqual "carpet covered on safe open" (1 :: Int) (gsCount CountCarpets gs1)
  assertEqual "carpet closed" ([] :: [Pos]) (gsCarpetOpen gs1)

--------------------------------------------------------------------------------
-- Carpet × Cookie mid-settle bottom drain (GoalCarpet soft-lock fix)
--------------------------------------------------------------------------------

-- | Cookie that only lands on a bottom-row carpet mid-settle (belt delivery /
-- gravity into drainBottomCookies) must cover that tile. before/after board
-- compare misses intermediate occupancy; drain positions must seed coverCarpets.
carpet_covers_on_cookie_bottom_drain :: Assertion
carpet_covers_on_cookie_bottom_drain = do
  let bottom = boardSize - 1
      -- Unit: settle drains Cookie that fell onto bottom; sites include that cell
      setMB = setM
      fill = Just (mkGem C5)
      mb0 = mboardFromRows (replicate boardSize (replicate boardSize fill))
      -- Hole under Cookie at (3,4) so gravity packs Cookie to bottom col 4
      mb =
        foldl
          (\m r -> setMB m (r, 4) Nothing)
          (setMB mb0 (3, 4) (Just Cookie))
          [4 .. bottom]
      (_, fallen, sites) = settleBoardPortals (builtinHooks [] []) mb
  assertEqual "one cookie drained" (1 :: Int) fallen
  assertEqual "drain site is bottom carpet col" [(bottom, 4)] sites
  -- Live: Cookie starts OFF the bottom so the match-cascade settle cannot
  -- drain it early; belt then parks it on the carpet cell (not a clear-hole).
  let belt = [(5, 4), (6, 4), (bottom, 4)]
      board =
        foldl
          (\b (p, cell) -> setCell b p cell)
          stableBoard
          [ ((6, 4), Cookie)
          , ((0, 0), mkGem C1)
          , ((0, 1), mkGem C1)
          , ((0, 2), mkGem C2)
          , ((0, 3), mkGem C1)
          ]
  assertBool "pre stable" (not (hasAnyMatch board))
  assertBool "cookie not already bottom" (not (isCookie (getCell board (bottom, 4))))
  assertBool "belt parks cookie on bottom carpet" $
    isCookie (getCell (shiftBelts board [belt]) (bottom, 4))
  assertBool "belt alone forms no match" $
    not (hasAnyMatch (shiftBelts board [belt]))
  let gs0 =
        (setCarpetOpen [(bottom, 4)] . setBelts [belt] . setPortals [] . setUfos [] $ (newGameAtLevel 0 (GameConfig 20 (goalCount CountCarpets 1)) 3)
          { gsBoard = board
          , gsCounts = noCounts
          , gsOver = Nothing
          , gsMoves = 20
          , gsGoal = goalCount CountCarpets 1
          , gsLastCleared = []
          })
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match swap"
    InvalidSwap -> assertFailure "expected valid swap"
    _ -> pure ()
  assertBool ("cookie collected via belt drain, got " ++ show (gsCount CountCookies gs1)) (gsCount CountCookies gs1 >= 1)
  assertBool "cookie not left on carpet" $
    not (isCookie (getCell (gsBoard gs1) (bottom, 4)))
  assertBool ("drain site in lastCleared, got " ++ show (gsLastCleared gs1)) $
    (bottom, 4) `elem` gsLastCleared gs1
  assertEqual "bottom carpet covered by drain" (1 :: Int) (gsCount CountCarpets gs1)
  assertEqual "no open carpets" ([] :: [Pos]) (gsCarpetOpen gs1)
  assertEqual "GoalCarpet meter" (1 :: Int) (gsCollected gs1)

--------------------------------------------------------------------------------
-- Portal / Surprise × Cookie mid-settle bottom drain → GoalCarpet (boundary)
--------------------------------------------------------------------------------

-- | Portal teleport onto an empty bottom-row carpet must drain the Cookie and
-- cover that tile (same cookSites → coverCarpets path as belt/gravity drain).
-- Unit: settleBoardPortals sites; live: cross-clear empties the exit column so
-- the portal can deliver mid-settle.
carpet_covers_on_portal_cookie_drain :: Assertion
carpet_covers_on_portal_cookie_drain = do
  let bottom = boardSize - 1
      setMB = setM
      fill = Just (mkGem C5)
      mb0 = mboardFromRows (replicate boardSize (replicate boardSize fill))
      -- Empty exit column 6; Cookie at portal A (4,2) → B (bottom,6)
      mb1 = foldl (\m r -> setMB m (r, 6) Nothing) mb0 [0 .. bottom]
      mb = setMB mb1 (4, 2) (Just Cookie)
      (_, fallen, sites) = settleBoardPortals (builtinHooks [] [((4, 2), (bottom, 6))]) mb
  assertEqual "portal-delivered cookie drained" (1 :: Int) fallen
  assertEqual "drain site is portal B bottom" [(bottom, 6)] sites
  let (open', hit) = coverCarpets [(bottom, 6)] sites
  assertEqual "coverCarpets hits portal drain" (1 :: Int) hit
  assertEqual "carpet closed after portal drain" ([] :: [Pos]) open'
  -- Live: Cookie at A; cross-clear empties exit column so settle can teleport+drain
  let board = setCell stableBoard (5, 3) Cookie
      gs0 =
        (setCarpetOpen [(bottom, 6)] . setBelts [] . setPortals [((5, 3), (bottom, 6))] . setUfos [] $ (newGameAtLevel 0 (GameConfig 20 (goalCount CountCarpets 1)) 4)
          { gsBoard = board
          , gsCounts = noCounts
          , gsOver = Nothing
          , gsCrossClears = 2
          , gsMoves = 20
          , gsGoal = goalCount CountCarpets 1
          , gsLastCleared = []
          })
      (gs1, out) = useCrossClear (3, 6) gs0
  case out of
    InvalidSwap -> assertFailure "cross should fire"
    NoMatch -> assertFailure "cross should apply"
    _ -> pure ()
  assertBool ("portal cookie collected, got " ++ show (gsCount CountCookies gs1)) $
    gsCount CountCookies gs1 >= 1
  assertBool "cookie not left on board" $
    null
      [ (r, c)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , isCookie (getCell (gsBoard gs1) (r, c))
      ]
  assertBool ("portal drain in lastCleared, got " ++ show (gsLastCleared gs1)) $
    (bottom, 6) `elem` gsLastCleared gs1
  assertEqual "bottom carpet covered by portal drain" (1 :: Int) (gsCount CountCarpets gs1)
  assertEqual "no open carpets" ([] :: [Pos]) (gsCarpetOpen gs1)
  assertEqual "GoalCarpet meter" (1 :: Int) (gsCollected gs1)

-- | Surprise 3×3 explode that opens a bottom-row Safe must drain the Cookie and
-- cover that carpet (Safe→Cookie then settle drainSites — Surprise blast path).
-- surpriseOutcome (6,3) == 3 (explode); blast includes (bottom,3).
carpet_covers_on_surprise_safe_bottom :: Assertion
carpet_covers_on_surprise_safe_bottom = do
  let bottom = boardSize - 1
  assertEqual "fixture Surprise explodes" (3 :: Int) (((6 * 8 + 3) `mod` 4) :: Int)
  let board =
        setCell
          (setCell stableBoard (6, 3) mkSurprise)
          (bottom, 3)
          (mkSafeLayers 1)
      gs0 =
        (setCarpetOpen [(bottom, 3)] . setBelts [] . setPortals [] . setUfos [] $ (newGameAtLevel 0 (GameConfig 20 (goalCount CountCarpets 1)) 5)
          { gsBoard = board
          , gsCounts = noCounts
          , gsOver = Nothing
          , gsHammers = 2
          , gsMoves = 20
          , gsGoal = goalCount CountCarpets 1
          , gsLastCleared = []
          })
      (gs1, out) = useHammer (6, 3) gs0
  case out of
    InvalidSwap -> assertFailure "hammer Surprise should fire"
    NoMatch -> assertFailure "hammer Surprise should apply"
    _ -> pure ()
  assertEqual "safe opened" (1 :: Int) (gsCount CountSafes gs1)
  assertBool ("cookie drained after safe open, got " ++ show (gsCount CountCookies gs1)) $
    gsCount CountCookies gs1 >= 1
  assertBool "no cookie left on bottom carpet" $
    not (isCookie (getCell (gsBoard gs1) (bottom, 3)))
  assertBool ("drain site in lastCleared, got " ++ show (gsLastCleared gs1)) $
    (bottom, 3) `elem` gsLastCleared gs1
  assertEqual "bottom carpet covered" (1 :: Int) (gsCount CountCarpets gs1)
  assertEqual "no open carpets" ([] :: [Pos]) (gsCarpetOpen gs1)
  assertEqual "GoalCarpet meter" (1 :: Int) (gsCollected gs1)
