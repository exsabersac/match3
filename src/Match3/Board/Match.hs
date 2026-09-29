{-# LANGUAGE ScopedTypeVariables #-}

-- | 匹配检测与提示：找 ≥3 连（MatchRun）、是否存在匹配、是否存在可走的一手（findHint）。
--
-- 依赖：Grid、元素注册表（哪些格参与匹配 / 能交换 / 进普通匹配提示；成对交换规则 = 彩虹与特殊合成也算可走）。只读盘面。
-- 第二刀 2b：「哪些格打断连线」不再按构造器列举，改为查注册表 matchColorWith
-- （本体颜色 + 挡匹配的叠层）；新增元素只需在它的 instance 里声明。元素类迁移起普通匹配提示排除彩虹本体
-- 改查元素的 hintable（原先直接调 Rainbow.isRainbow）。段 2c 起本模块不依赖内置注册表，全部函数收 Registry；不带 With 的旧名在 Match3.Board.Default。
module Match3.Board.Match
  ( MatchRun(..)
  , findMatchRunsWith
  , groupGemRunsWith
  , findMatchesWith
  , hasAnyMatchWith
  , hasValidMoveWith
  , findHintWith
  ) where

import Data.List (nub)
import Data.Maybe (isJust)
import Match3.Element.Registry (Registry, blocksSwapWith, colorOfWith, hintableWith, matchColorWith, swapRules, upperBlocksSwapWith)
import Match3.Element.Types (SwapRule(..))
import Match3.Types
import Match3.Board.Grid

-- | A contiguous same-color gem run of length >= 3 (stones break runs).
data MatchRun = MatchRun
  { runColor :: Color
  , runPos   :: [Pos]
  , runIsH   :: Bool  -- True = horizontal
  } deriving (Eq, Show)

-- | findMatchRuns（指定注册表）。
findMatchRunsWith :: Registry -> Board -> [MatchRun]
findMatchRunsWith reg b = filter ((>= 3) . length . runPos) (hRuns ++ vRuns)
  where
    hRuns =
      [ MatchRun col ps True
      | r <- [0 .. boardSize - 1]
      , let rowPs = [(r, c) | c <- [0 .. boardSize - 1]]
      , (col, ps) <- groupGemRunsWith reg b rowPs
      ]
    vRuns =
      [ MatchRun col ps False
      | c <- [0 .. boardSize - 1]
      , let colPs = [(r, c) | r <- [0 .. boardSize - 1]]
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
hasAnyMatchWith :: Registry -> Board -> Bool
hasAnyMatchWith reg = not . null . findMatchRunsWith reg

-- | hasValidMove（指定注册表）。
hasValidMoveWith :: Registry -> Board -> Bool
hasValidMoveWith reg = maybe False (const True) . findHintWith reg

-- | findHint（指定注册表）。普通匹配提示只试「有色且能交换」的格；成对交换规则（段 4：注册表的成对交换规则，
-- 内置 = 彩虹、特殊合成，按 srOrder 逐条）的提示只排除上层（锁链 / 火箭冰冻）挡交换的格，本体由规则自己判定。
findHintWith :: Registry -> Board -> Maybe (Pos, Pos)
findHintWith reg b =
  case matchHints ++ concatMap ruleHints (swapRules reg) of
    (x : _) -> Just x
    [] -> Nothing
  where
    matchHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , hintable (getCell b p1)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , hintable (getCell b p2)
      , hintableWith reg (getCell b p1) && hintableWith reg (getCell b p2)
      , hasAnyMatchWith reg (swapCells b p1 p2)
      ]
    ruleHints rule =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , not (upperLocked (getCell b p1) || upperLocked (getCell b p2))
      , srFires rule b p1 p2
      ]
    hintable cell = isJust (colorOfWith reg cell) && not (blocksSwapWith reg cell)
    upperLocked = upperBlocksSwapWith reg

-- | 所有匹配格（指定注册表）：各连线位置的去重并集。
findMatchesWith :: Registry -> Board -> [Pos]
findMatchesWith reg b = nub (concatMap runPos (findMatchRunsWith reg b))
