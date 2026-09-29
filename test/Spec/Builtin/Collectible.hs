{-# LANGUAGE ScopedTypeVariables #-}

-- | 收集物（对应 Element/Builtin/Collectible）：饼干、时间精灵（气泡见 Spec.JellyBubble）。
-- （第 1 刀由 Spec.Obstacles.Body / Features 按 Builtin 分组纯搬家而来；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Builtin.Collectible
  ( tests
  ) where

import Match3.Board.Default (cascadeSeeds)
import Match3.Board.Cascade (CascadeRun(CascadeRun, crTally, crBoard), CascadeTally(CascadeTally, ctCookies))
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
  [ testCase "cookie_blocks_swap" cookie_blocks_swap
  , testCase "cookie_falls_with_gravity" cookie_falls_with_gravity
  , testCase "cookie_collected_at_bottom" cookie_collected_at_bottom
  , testCase "time_spirit_blocks_swap" time_spirit_blocks_swap
  , testCase "time_spirit_awards_moves" time_spirit_awards_moves
  , testCase "time_spirit_rescues_last_move" time_spirit_rescues_last_move
  , testCase "cookie_immune_to_direct_clear" cookie_immune_to_direct_clear
  ]

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
