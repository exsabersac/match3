{-# LANGUAGE NamedFieldPuns #-}

-- | 结算计数辅助：颜色袋累加、「按前后盘面差计数」（保险箱开启 / 时间精灵 / 自定义）、地毯可覆盖的腾空格。
--
-- 依赖：Match3.Types、Match3.Board.*、元素注册表。被公共结算 Match3.Game.Resolve.resolveMove 使用
-- （交换与三种道具共用一处）。第二刀 2b：哪些元素按差计数（元素类的 diffCounter / bonusMoves）、
-- 哪些元素离格算地毯覆盖（vacatesCarpet）改由元素自己声明；旧的 countSafes / countTimeSpirits /
-- carpetVacateSeeds 保留为内置注册表上的同名入口。
module Match3.Game.Tally
  ( lookupColor
  , mergeTallies
  , countSafes
  , countTimeSpirits
  , carpetVacateSeeds
  , carpetVacateSeedsWith
  , DiffCount(..)
  , diffCountsWith
  ) where

import Data.Maybe (fromMaybe)
import Match3.Board.Grid (getCell)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry, countElementWith, diffCountersWith, elementName, vacatesCarpetWith)
import Match3.Element.Types (Counter, ElementName)
import Match3.Types

-- | 颜色袋里某色的计数（缺省 0）。
lookupColor :: [(Color, Int)] -> Color -> Int
lookupColor tallies col = fromMaybe 0 (lookup col tallies)

-- | 两个颜色袋逐色相加（结果按 allColors 顺序）。
mergeTallies :: [(Color, Int)] -> [(Color, Int)] -> [(Color, Int)]
mergeTallies a b =
  [(col, lookupColor a col + lookupColor b col) | col <- allColors]

-- | 盘上保险箱个数（结算时用前后差计「开启数」）。
countSafes :: Board -> Int
countSafes = countElementWith defaultRegistry "safe"

-- | 盘上时间精灵个数（前后差 = 本步命中数，每只 +2 步）。
countTimeSpirits :: Board -> Int
countTimeSpirits = countElementWith defaultRegistry "time_spirit"

-- | 一个「按前后差计数」元素本步的结果。
data DiffCount = DiffCount
  { dcElement :: ElementName
  , dcCounter :: Counter
  , dcCount   :: Int  -- ^ max 0 (步前个数 - 步后个数)
  , dcBonus   :: Int  -- ^ 奖励步数 = dcCount * bonusMoves
  } deriving (Eq, Show)

-- | 注册表里所有带 diffCounter 的元素，按步前 / 步后盘面算个数差。
diffCountsWith :: Registry -> Board -> Board -> [DiffCount]
diffCountsWith reg before after =
  [ DiffCount n k cnt (cnt * bonus)
  | (n, k, bonus) <- diffCountersWith reg
  , let cnt = max 0 (countElementWith reg n before - countElementWith reg n after)
  ]

-- | 地毯补充种子：饼干腾空或保险箱开启离开格子时，即使未进 clear-holes 也要计入覆盖。
-- 沉降中途才落到地毯再底收的饼干由 Board drain 位并入清除列表。
-- | Carpet seeds when Cookie / Safe leave a cell without entering clear-holes.
-- Cookie is immune to mid-board wipe (only gravity + bottom drain); Safe opens
-- in place to Cookie. Start-of-move Cookie occupancy / Safe→Cookie would miss
-- GoalCarpet unless we treat the vacate as a cover seed. (Cookies that only
-- arrive on a tile mid-settle then drain are covered via Board drain positions
-- in cascade clear lists — see settleBoardPortals.)
-- Compare pre-move board to final post-move board (after snail + follow-up cascade).
carpetVacateSeeds :: Board -> Board -> [Pos]
carpetVacateSeeds = carpetVacateSeedsWith defaultRegistry

-- | carpetVacateSeeds（指定注册表）：步前本体 vacatesCarpet 的格，步后本体换成了别的元素。
carpetVacateSeedsWith :: Registry -> Board -> Board -> [Pos]
carpetVacateSeedsWith reg before after =
  [ (r, c)
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , let cell0 = getCell before (r, c)
        cell1 = getCell after (r, c)
  , vacatesCarpetWith reg cell0
  , elementName reg cell1 /= elementName reg cell0
  ]
