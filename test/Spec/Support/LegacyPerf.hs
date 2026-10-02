-- | 第 5 项（性能与并发）改写前的写法，逐字副本（只改了函数名前缀与 import），供 Spec.Perf 逐项对照：
--
-- * Match3.Board.Match 的匹配扫描与提示搜索：每次扫描逐格问注册表 matchColorWith（行一遍、列一遍），
--   提示搜索对每个候选 swapCells 出一张新盘再查；
-- * Match3.Board.Gravity.applyGravityWith：逐列取出成列表、colGravityWith（列表版，未改）、再 array 写回。
module Spec.Support.LegacyPerf
  ( oldFindMatchRunsWith
  , oldHasAnyMatchWith
  , oldFindHintWith
  , oldApplyGravityWith
  ) where

import Data.Array (Array, array, bounds, listArray, (!))
import Match3.Board.Gravity (colGravityWith)
import Data.List (nub)
import Data.Maybe (isJust)
import Match3.Board.Grid
import Match3.Element.Registry (Registry, blocksSwapWith, colorOfWith, hintableWith, matchColorWith, swapRules, upperBlocksSwapWith)
import Match3.Element.Types (MatchRun(..), SwapRule(..))
import Match3.Types

oldFindMatchRunsWith :: Registry -> Board -> [MatchRun]
oldFindMatchRunsWith reg b = filter ((>= 3) . length . runPos) (hRuns ++ vRuns)
  where
    rows = boardRowIndices b
    cols = boardColIndices b
    hRuns =
      [ MatchRun col ps True
      | r <- rows
      , let rowPs = [(r, c) | c <- cols]
      , (col, ps) <- groupGemRunsWith reg b rowPs
      ]
    vRuns =
      [ MatchRun col ps False
      | c <- cols
      , let colPs = [(r, c) | r <- rows]
      , (col, ps) <- groupGemRunsWith reg b colPs
      ]

-- | 连续同色段：matchColorWith 为 Nothing 的格（障碍、被迷雾 / 锁链 / 窗帘 / 蒸汽盖住的宝石等）打断连线。
-- 火箭冰冻不挡匹配（冻住的宝石照样成消）。
groupGemRunsWith :: Registry -> Board -> [Pos] -> [(Color, [Pos])]
groupGemRunsWith _ _ [] = []
groupGemRunsWith reg b (p : ps) = case matchColorWith reg (getCell b p) of
  Nothing -> groupGemRunsWith reg b ps
  Just col -> go [p] col ps
  where
    go run col [] = [(col, reverse run)]
    go run col (q : qs) = case matchColorWith reg (getCell b q) of
      Just col' | col' == col -> go (q : run) col qs
      _ -> (col, reverse run) : groupGemRunsWith reg b (q : qs)

-- | hasAnyMatch（指定注册表）。
oldHasAnyMatchWith :: Registry -> Board -> Bool
oldHasAnyMatchWith reg = not . null . oldFindMatchRunsWith reg

-- | findHint（指定注册表）。普通匹配提示只试「有色且能交换」的格；成对交换规则（段 4：注册表的成对交换规则，
-- 内置 = 彩虹、特殊合成，按 srOrder 逐条）的提示只排除上层（锁链 / 火箭冰冻）挡交换的格，本体由规则自己判定。
oldFindHintWith :: Registry -> Board -> Maybe (Pos, Pos)
oldFindHintWith reg b =
  case matchHints ++ concatMap ruleHints (swapRules reg) of
    (x : _) -> Just x
    [] -> Nothing
  where
    rows = boardRowIndices b
    cols = boardColIndices b
    matchHints =
      [ (p1, p2)
      | r <- rows
      , c <- cols
      , let p1 = (r, c)
      , hintable (getCell b p1)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds b p2
      , hintable (getCell b p2)
      , hintableWith reg (getCell b p1) && hintableWith reg (getCell b p2)
      , swapMakesMatch p1 p2
      ]
    ruleHints rule =
      [ (p1, p2)
      | r <- rows
      , c <- cols
      , let p1 = (r, c)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds b p2
      , not (upperLocked (getCell b p1) || upperLocked (getCell b p2))
      , srFires rule b p1 p2
      ]
    hintable cell = isJust (colorOfWith reg cell) && not (blocksSwapWith reg cell)
    upperLocked = upperBlocksSwapWith reg
    -- 局部检查（第 3 刀）：交换后的盘面有匹配 ⇔ 没被交换触及的行 / 列在原盘上已有 ≥3 连，
    -- 或交换两格所在的行 / 列在交换后有 ≥3 连。与整盘 hasAnyMatchWith (swapCells b p1 p2) 逐格等价
    -- （匹配只看各格自己的 matchColorWith，连线按行 / 列独立），遍历顺序与返回结果不变。
    rowPs r = [(r, c) | c <- cols]
    colPs c = [(r, c) | r <- rows]
    rowHas = listArray (0, boardNRows b - 1) [lineHasRunWith reg b (rowPs r) | r <- rows] :: Array Int Bool
    colHas = listArray (0, boardNCols b - 1) [lineHasRunWith reg b (colPs c) | c <- cols] :: Array Int Bool
    swapMakesMatch p1@(r1, c1) p2@(r2, c2) =
      let b' = swapCells b p1 p2
          rs = nub [r1, r2]
          cs = nub [c1, c2]
      in any (lineHasRunWith reg b' . rowPs) rs
           || any (lineHasRunWith reg b' . colPs) cs
           || or [rowHas ! r | r <- rows, r `notElem` rs]
           || or [colHas ! c | c <- cols, c `notElem` cs]

-- | 一条线（按给定顺序的格）上是否有 ≥3 的同色连续段。
lineHasRunWith :: Registry -> Board -> [Pos] -> Bool
lineHasRunWith reg b ps = any ((>= 3) . length . snd) (groupGemRunsWith reg b ps)


oldApplyGravityWith :: Registry -> MBoard -> MBoard
oldApplyGravityWith reg mb =
  let bnds@((r0, c0), (r1, c1)) = bounds mb
      rows = [r0 .. r1]
  in array bnds
       [ ((r, c), v)
       | c <- [c0 .. c1]
       , (r, v) <- zip rows (colGravityWith reg [mb ! (r', c) | r' <- rows])
       ]
