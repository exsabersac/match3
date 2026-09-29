{-# LANGUAGE ScopedTypeVariables #-}

-- | 重力与补子：下落 + 补子、固定格（不死装饰）不随重力移动。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Gravity
  ( tests
  , gravity_then_refill
  ) where

import Match3.Board.Cascade (cascadeSeeds, CascadeRun(CascadeRun, crTally, crBoard), CascadeTally(CascadeTally, ctCookies))
import Match3.Board.Clear (clearMatches)
import Match3.Board.Gravity (applyGravity, refill)
import Match3.Core
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Support

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "gravity_then_refill" gravity_then_refill
  , testCase "immortal_no_gravity_fall" immortal_no_gravity_fall
  ]

gravity_then_refill :: Assertion
gravity_then_refill = do
  let b = boardFromRows $
        [ [ if c == 0
              then if r >= 5 then mkGem C1 else mkGem (toEnum (r `mod` 5))
              else mkGem C5
          | c <- [0 .. boardSize - 1]
          ]
        | r <- [0 .. boardSize - 1]
        ]
  assertBool "has match" (hasAnyMatch b)
  let (mb, n) = clearMatches b
  assertBool "cleared >= 3" (n >= 3)
  let fallen = applyGravity mb
      colsOk =
        all
          ( \c ->
              let col = map (!! c) fallen
                  (holes, rest) = span (== Nothing) col
              in all (/= Nothing) rest && length holes + length rest == boardSize
          )
          [0 .. boardSize - 1]
  assertBool "gravity: holes on top" colsOk
  let g = mkStdGen 7
      (b', _) = refill g fallen
  assertEqual "rows" boardSize (length (boardRows b'))
  assertBool "full rows" (all ((== boardSize) . length) (boardRows b'))

--------------------------------------------------------------------------------
-- Immortal décor must not fall with gravity (portal/belt soft-lock class)
--------------------------------------------------------------------------------

-- | Bottle / Maker / MagicHat / Snail never clear and are not portal-transferable.
-- If gravity packs them into holes they can land on a portal/belt slot (or leave
-- a décor seed via Cross column wipe) and permanently soft-lock the layout —
-- same immortal class as portal_endpoints_not_immortal_blocked /
-- belt_cells_not_stuck_immortal. Gems / Cookie still fall.
immortal_no_gravity_fall :: Assertion
immortal_no_gravity_fall = do
  let seeds = [(6, 3), (7, 3)]  -- far below (3,3); buffer gems avoid adj peels
      check name cell isImm = do
        let board = setCell stableBoard (3, 3) cell
            CascadeRun {crBoard = b1} = cascadeSeeds Nothing seeds [] [] (mkStdGen 0) board
            positions =
              [ (r, c)
              | r <- [0 .. boardSize - 1]
              , c <- [0 .. boardSize - 1]
              , isImm (getCell b1 (r, c))
              ]
        assertEqual (name ++ " stayed put") [(3, 3)] positions
  check "Maker" (mkMakerCharges C5 5) isMaker
  check "Bottle" (mkBottle C2) isBottle
  check "Hat" mkMagicHat isMagicHat
  check "Snail" (mkSnail 1 0) isSnail
  -- Control: Cookie still falls toward bottom (may drain).
  let boardC = setCell stableBoard (3, 3) mkCookie
      CascadeRun {crBoard = bC, crTally = CascadeTally {ctCookies = cookies}} = cascadeSeeds Nothing [(4, 3), (5, 3), (6, 3), (7, 3)] [] [] (mkStdGen 1) boardC
  assertBool "cookie left (3,3)" (not (isCookie (getCell bC (3, 3))))
  assertBool ("cookie fell or drained, cookies=" ++ show cookies) $
    cookies >= 1
      || any
           (\(r, c) -> isCookie (getCell bC (r, c)) && r > 3)
           [ (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] ]
