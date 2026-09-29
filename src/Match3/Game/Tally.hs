{-# LANGUAGE NamedFieldPuns #-}

-- | 结算计数辅助：颜色袋累加、保险箱 / 时间精灵计数、地毯可覆盖的腾空格。
--
-- 依赖：Match3.Types、Match3.Board。被 trySwap 与三种道具共同使用。
-- 同步：新增需要「按前后盘面差计数」的目标时，trySwap / useHammer / useFreeSwap / useCrossClear
-- 四处结算都要接上（第二刀计划把四处结算合并）。
module Match3.Game.Tally
  ( lookupColor
  , mergeTallies
  , countSafes
  , countTimeSpirits
  , carpetVacateSeeds
  ) where

import Data.Maybe (fromMaybe)
import Match3.Board (getCell)
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
countSafes b =
  length
    [ ()
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , isSafe (getCell b (r, c))
    ]

-- | 盘上时间精灵个数（前后差 = 本步命中数，每只 +2 步）。
countTimeSpirits :: Board -> Int
countTimeSpirits b =
  length
    [ ()
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , isTimeSpirit (getCell b (r, c))
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
carpetVacateSeeds before after =
  [ (r, c)
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , let cell0 = getCell before (r, c)
        cell1 = getCell after (r, c)
  , (isCookie cell0 && not (isCookie cell1))
      || (isSafe cell0 && not (isSafe cell1))
  ]
