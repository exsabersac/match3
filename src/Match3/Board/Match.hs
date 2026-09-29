{-# LANGUAGE ScopedTypeVariables #-}

-- | 匹配检测与提示：找 ≥3 连（MatchRun）、是否存在匹配、是否存在可走的一手（findHint）。
--
-- 依赖：Grid、Rainbow / Combos（彩虹与特殊合成也算可走）。只读盘面，不修改。
-- 同步：groupGemRuns 里「哪些格打断连线」必须和 Obstacles / Grass 对占格障碍与叠层的定义一致；
-- 新增占格障碍时要在这里补一行，否则会被当成普通宝石参与匹配。
module Match3.Board.Match
  ( MatchRun(..)
  , findMatchRuns
  , groupGemRuns
  , findMatches
  , hasAnyMatch
  , hasValidMove
  , findHint
  ) where

import Data.List (nub)
import Match3.Combos (isSpecialCombo)
import Match3.Rainbow (isRainbow, isRainbowSwap)
import Match3.Types
import Match3.Board.Grid

-- | A contiguous same-color gem run of length >= 3 (stones break runs).
data MatchRun = MatchRun
  { runColor :: Color
  , runPos   :: [Pos]
  , runIsH   :: Bool  -- True = horizontal
  } deriving (Eq, Show)

-- | 全部横、竖 ≥3 同色连线（先横后竖，行 / 列优先）；被障碍 / 叠层打断的不算。
findMatchRuns :: Board -> [MatchRun]
findMatchRuns b = filter ((>= 3) . length . runPos) (hRuns ++ vRuns)
  where
    hRuns =
      [ MatchRun col ps True
      | r <- [0 .. boardSize - 1]
      , let rowPs = [(r, c) | c <- [0 .. boardSize - 1]]
      , (col, ps) <- groupGemRuns b rowPs
      ]
    vRuns =
      [ MatchRun col ps False
      | c <- [0 .. boardSize - 1]
      , let colPs = [(r, c) | r <- [0 .. boardSize - 1]]
      , (col, ps) <- groupGemRuns b colPs
      ]

-- | Group contiguous same-color *gems*; stones and color changes break runs.
groupGemRuns :: Board -> [Pos] -> [(Color, [Pos])]
groupGemRuns _ [] = []
groupGemRuns b (p : ps) = case getCell b p of
  Stone _ -> groupGemRuns b ps
  Chest _ -> groupGemRuns b ps
  Honey _ -> groupGemRuns b ps
  Balloon _ -> groupGemRuns b ps
  Cookie -> groupGemRuns b ps
  Cake _ -> groupGemRuns b ps
  MagicHat -> groupGemRuns b ps
  Maker _ _ -> groupGemRuns b ps
  Snail _ _ -> groupGemRuns b ps
  Safe _ -> groupGemRuns b ps
  Surprise -> groupGemRuns b ps
  Bottle _ -> groupGemRuns b ps
  TimeSpirit -> groupGemRuns b ps
  Gem _ _ _ (Just (Fog _)) -> groupGemRuns b ps  -- fog hides gem from matches
  Gem _ _ _ (Just (Chain _)) -> groupGemRuns b ps  -- chain locks gem from matches
  Gem _ _ _ (Just (Curtain _)) -> groupGemRuns b ps  -- curtain hides gem from matches
  Gem _ _ _ (Just Steam) -> groupGemRuns b ps  -- steam hides gem from matches
  -- Freeze does NOT break runs: frozen gems still match (vs Chain/Fog/Curtain/Steam)
  Gem col _ _ _ -> go [p] col ps
  Flip col _ -> go [p] col ps
  Countdown col _ -> go [p] col ps
  where
    go run col [] = [(col, reverse run)]
    go run col (q : qs) = case getCell b q of
      Gem _ _ _ (Just (Fog _)) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem _ _ _ (Just (Chain _)) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem _ _ _ (Just (Curtain _)) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem _ _ _ (Just Steam) -> (col, reverse run) : groupGemRuns b (q : qs)
      Gem col' _ _ _ | col' == col -> go (q : run) col qs
      Flip col' _ | col' == col -> go (q : run) col qs
      Countdown col' _ | col' == col -> go (q : run) col qs
      _ -> (col, reverse run) : groupGemRuns b (q : qs)

-- | 所有匹配格：各连线位置的去重并集。
findMatches :: Board -> [Pos]
findMatches b = nub (concatMap runPos (findMatchRuns b))

-- | 盘面上是否存在任意 ≥3 连。
hasAnyMatch :: Board -> Bool
hasAnyMatch = not . null . findMatchRuns

-- | True if some adjacent gem-gem swap would create a match.
hasValidMove :: Board -> Bool
hasValidMove = maybe False (const True) . findHint

-- | First adjacent swap that would create a match or activate a rainbow (for hint).
findHint :: Board -> Maybe (Pos, Pos)
findHint b =
  case matchHints ++ rainbowHints ++ comboHints of
    (x : _) -> Just x
    [] -> Nothing
  where
    matchHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , isGem (getCell b p1)
      , not (hasChain (getCell b p1) || hasFreeze (getCell b p1))
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , isGem (getCell b p2)
      , not (hasChain (getCell b p2) || hasFreeze (getCell b p2))
      , not (isRainbow (getCell b p1) || isRainbow (getCell b p2))
      , hasAnyMatch (swapCells b p1 p2)
      ]
    rainbowHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , not (hasChain (getCell b p1) || hasChain (getCell b p2)
               || hasFreeze (getCell b p1) || hasFreeze (getCell b p2))
      , isRainbowSwap b p1 p2
      ]
    comboHints =
      [ (p1, p2)
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , let p1 = (r, c)
      , p2 <- [(r, c + 1), (r + 1, c)]
      , inBounds p2
      , not (hasChain (getCell b p1) || hasChain (getCell b p2)
               || hasFreeze (getCell b p1) || hasFreeze (getCell b p2))
      , isSpecialCombo b p1 p2
      ]
