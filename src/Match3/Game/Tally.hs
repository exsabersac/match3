{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | 结算计数辅助：「按前后盘面差计数」（保险箱开启 / 时间精灵 / 自定义）、地毯可覆盖的腾空格。
--
-- 依赖：Match3.Types、Match3.Board.*、元素注册表。被公共结算 Match3.Game.Resolve.resolveMove 使用
-- （交换与三种道具共用一处）。哪些元素按差计数（能力 diffCounter / bonusMoves）、
-- 哪些元素离格算地毯覆盖（vacatesCarpet）由元素自己声明；盘上某种元素的个数用注册表的 countElementWith。
module Match3.Game.Tally
  ( carpetVacateSeedsWith
  , DiffCount(..)
  , diffCountsWith
  ) where

import Match3.Board.Grid (getCell)
import Match3.Element.Registry (Registry, diffCountersWith, elementName, vacatesCarpetWith, weighElementWith)
import Match3.Element.Types (CounterKey)
import Match3.Types

-- | 一个「按前后差计数」元素本步的结果。
data DiffCount = DiffCount
  { dcElement :: ElementName
  , dcCounter :: CounterKey
  , dcCount   :: Int  -- ^ max 0 (步前个数 - 步后个数)
  , dcBonus   :: Int  -- ^ 奖励步数 = dcCount * bonusMoves
  } deriving (Eq, Show)

-- | 注册表里所有带 diffCounter 的元素，按步前 / 步后盘面算个数差（格子按 diffWeight 加权，缺省每格 1）。
diffCountsWith :: Registry -> Board -> Board -> [DiffCount]
diffCountsWith reg before after =
  [ DiffCount n k cnt (cnt * bonus)
  | (n, k, bonus) <- diffCountersWith reg
  , let cnt = max 0 (weighElementWith reg n before - weighElementWith reg n after)
  ]

-- | 地毯补充种子：步前本体 vacatesCarpet 的格，步后本体换成了别的元素。
-- 内置是饼干与保险箱：饼干不怕盘中清除（只随重力下落、在底边被收走），保险箱原地开成饼干；
-- 它们离开起始格时不进清除格，不当作覆盖种子的话 GoalCarpet 会漏掉这些格。
-- 沉降中途才落到地毯上、随后被收走的饼干不在这里：它们的收集位已并入连锁的清除格（见 settleBoardPortalsWith）。
-- 比较的是步前盘面与步后终盘（蜗牛与后续连锁之后）。
carpetVacateSeedsWith :: Registry -> Board -> Board -> [Pos]
carpetVacateSeedsWith reg before after =
  [ p
  | p <- boardPositions before
  , let cell0 = getCell before p
        cell1 = getCell after p
  , vacatesCarpetWith reg cell0
  , elementName reg cell1 /= elementName reg cell0
  ]
