{-# LANGUAGE ScopedTypeVariables #-}

-- | 一轮消除：匹配清除（clearMatchesDetailed）与种子清除（clearFromSeedsDetailed），两者共用同一个
-- 一轮流水线 clearWaveWith；特殊块扩展（expandSpecials）、新特殊块生成（spawnSpecials）、彩蛋、
-- 邻格波及、飞碟吸收（maskUfoAbsorbSpecials / clearUfoAbsorbed：吸走 ≠ 引爆）以及计分公式。
--
-- 第二刀 2b：直接命中、叠层随格清除、邻格波及、特殊块爆炸范围、计色都查元素注册表
-- （Match3.Element.Registry）；邻格波及按各元素 AdjacentRule 的 arOrder 依次执行（顺序见 Element.Builtin）。
-- 段 4：彩蛋开启改为注册表的开启规则（openRule），彩虹取色 / 特殊合成改为成对交换规则（swapRule，在 Game.Move）。
-- 段 2c 起本模块不依赖内置注册表，全部函数收 Registry；不带 With 的旧名在 Match3.Board.Default。
--
-- 依赖：Grid、Match、元素注册表。
-- 同步：这里的函数只被 Match3.Board.Cascade 的单一连锁实现调用（结算与回放同一次计算），
-- 返回 (挖空后盘面, 清除数, 清除格)；Cascade 再按清除格在消除前盘面上统计障碍计数。
module Match3.Board.Clear
  ( expandSpecialsWith
  , spawnSpecials
  , clearMatchesAtWith
  , countColorWith
  , surpriseClearPassWith
  , clearMatchesDetailedWith
  , scoreForCleared
  , scoreForWave
  , maskUfoAbsorbSpecials
  , clearUfoAbsorbedWith
  , clearFromSeedsDetailedWith
  ) where

import Data.List (nub)
import Match3.Element.Registry (Registry, blastWith, chipOnHitWith, colorOfWith, openWith, runAdjacentWith, stripOnClearWith)
import Match3.Types
import Match3.Board.Grid
import Match3.Board.Match

-- | expandSpecials（指定注册表）：爆炸范围 = 本体定义的 blast，能否点火 = 各层 activates（软锁纪律）。
-- 彩虹没有 blast（只经 rainbowClearSeeds 按交换对象取色，否则彩虹 × 宝石会重复清两色）。
expandSpecialsWith :: Registry -> Board -> [Pos] -> [Pos]
expandSpecialsWith reg b seeds = go (nub seeds) (nub seeds)
  where
    go acc [] = acc
    go acc (p : ps) =
      let extra = blastWith reg (getCell b p) p
          new = filter (`notElem` acc) extra
      in go (acc ++ new) (ps ++ new)

-- | Specials spawned from runs: len>=5 Rainbow, len==4 Line (orient by run).
-- Only place onto positions that actually clear (holes). Flip / ice>1 seeds stay
-- on the board, so prefer/middle must fall back to a clearable cell in the run
-- (otherwise a 4-match with Flip/ice at the anchor silently drops the special).
spawnSpecials :: Maybe Pos -> [MatchRun] -> [Pos] -> [(Pos, Cell)]
spawnSpecials prefer runs clearable =
  [ (pos, Gem (runColor run) kind 0 Nothing)
  | run <- runs
  , let n = length (runPos run)
  , n >= 4
  , let kind
          | n >= 5 = Rainbow
          | runIsH run = LineH
          | otherwise = LineV
        slots = filter (`elem` clearable) (runPos run)
  , pos <- take 1 $ case prefer of
      Just p | p `elem` slots -> [p]
      _ -> drop (length slots `div` 2) slots
  ]

-- | countColor（指定注册表）：按本体颜色 color 计（不看叠层）。
countColorWith :: Registry -> Board -> [Pos] -> Color -> Int
countColorWith reg b ps col = length [p | p <- ps, colorOfWith reg (getCell b p) == Just col]

-- | surpriseClearPass（指定注册表）：一轮内的多轮开启（段 4 起是通用的「开启类元素」流程：开启规则来自
-- 注册表的 openRule，内置只有彩蛋；开出的格本轮坐住、屏蔽后再展开爆炸，新命中的格进入下一批前沿）。
surpriseClearPassWith :: Registry -> Board -> [Pos] -> (Board, [Pos], [Pos], [Pos])
surpriseClearPassWith reg b0 seeds0 =
  go b0 (nub seeds0) [] [] []
  where
    -- Mask saved Surprise-specials so expandSpecials cannot activate them.
    maskSaved b ps =
      foldl' (\b' p -> setCell b' p (mkGem C1)) b ps
    go board front trueAcc directAcc saved
      | null front =
          ( board
          , nub (filter (`notElem` saved) trueAcc)
          , nub directAcc
          , nub saved
          )
      | otherwise =
          let (bOpen, expl, savedNew) = openWith reg board front
              saved' = nub (saved ++ savedNew)
              kept = filter (`notElem` saved') front
          in if null expl
               then go bOpen [] (trueAcc ++ kept) directAcc saved'
               else
                 let expanded = expandSpecialsWith reg (maskSaved board saved') expl
                     (bChip, free1) = chipOnHitWith reg bOpen expanded
                     processed = nub (front ++ trueAcc)
                     front' = filter (`notElem` processed) free1
                 in go bChip front' (trueAcc ++ kept ++ free1) (directAcc ++ expanded) saved'

-- | 匹配清除一轮（指定注册表）：种子 = 全部匹配格。
clearMatchesDetailedWith :: Registry -> Maybe Pos -> Board -> (MBoard, Int, [Pos])
clearMatchesDetailedWith reg prefer b =
  let runs = findMatchRunsWith reg b
  in clearWaveWith reg prefer runs b (nub (concatMap runPos runs))

-- | 一轮消除的**唯一流水线**（匹配清除与种子清除共用；第二刀之前是两份逐行相同的代码）：
--
--   1. expandSpecials：种子里能点火的特殊块展开爆炸范围（= 直接命中格）；
--   2. chipOnHitWith：直接命中按层结算（冰 → 叠层 → 本体：削层 / 揭层 / 消除 / 免疫）；
--   3. surpriseClearPass：彩蛋在邻格波及之前开启（3×3 爆炸计入真消除，与炸弹同口径）；
--   4. stripOnClearWith：真消除格上的草 / 藤 / 巧随格清掉（空洞不能再蔓延）；
--   5. runAdjacentWith：按 arOrder 跑各元素的邻格波及（已被直接命中的格不再重复波及；
--      彩蛋开出的特殊块与果汁机刚产出的炸弹本轮坐住，不被魔法帽 / 染色瓶改色）；
--   6. 清除格 = 真消除 ∪ 波及打碎的格（按规则顺序）；挖空后在清除格上放新特殊块。
clearWaveWith :: Registry -> Maybe Pos -> [MatchRun] -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearWaveWith reg prefer runs b base =
  let expanded = expandSpecialsWith reg b base
      -- Ice chips first: iced gems stay, ice-free positions may clear.
      -- Do NOT strip Grass/Vine/Choco on raw expand seeds — soft hits (ice>1 /
      -- Flip / Chain peel) keep on-cell overlays (same discipline as adjacent).
      (bIced, iceFree) = chipOnHitWith reg b expanded
      (bSurp2, trueClears, surpDirect, surpSaved) = surpriseClearPassWith reg bIced iceFree
      bClearedOv = stripOnClearWith reg bSurp2 trueClears
      -- Cells that already took a direct-hit peel/chip must not also receive an
      -- ortho adjacent peel this wave (Chain2/Stone2/Safe2 on a Line/Bomb path).
      directHits = nub (expanded ++ surpDirect)
      (bAdj, dead, _sits) = runAdjacentWith reg trueClears directHits surpSaved bClearedOv
      allPos = nub (trueClears ++ dead)
      n = length allPos
      mb0 = setManyM (toM bAdj) [(p, Nothing) | p <- allPos]
      spawns = spawnSpecials prefer runs allPos
      mb1 = setManyM mb0 [(p, Just cell) | (p, cell) <- spawns, p `elem` allPos]
  in (mb1, n, allPos)

-- | 旧计分：每格 10 分（不带波次倍数）。
scoreForCleared :: Int -> Score
scoreForCleared n = n * 10

-- | Wave 1 = 1x, wave 2 = 2x, ... (cells * 10 * wave).
scoreForWave :: Int -> Int -> Score
scoreForWave wave n = n * 10 * max 1 wave

-- | UFO 吸收前把特殊降为 Normal，清除时不走 expandSpecials（吸走 ≠ 引爆）。
-- | Demote Line/Bomb/Rainbow at UFO absorb seeds to Normal so clearFromSeedsDetailed
-- removes them without expandSpecials detonation (吸走 ≠ 引爆). Ice / overlays kept.
maskUfoAbsorbSpecials :: Board -> [Pos] -> Board
maskUfoAbsorbSpecials b ps =
  foldl' maskOne b (nub ps)
  where
    maskOne board p =
      case getCell board p of
        Gem c k ice ov
          | k /= Normal ->
              setCell board p (Gem c Normal ice ov)
        _ -> board

-- | clearUfoAbsorbed（指定注册表）。
clearUfoAbsorbedWith :: Registry -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearUfoAbsorbedWith reg b absorbed =
  clearFromSeedsDetailedWith reg Nothing (maskUfoAbsorbSpecials b absorbed) absorbed

-- | 种子清除一轮（指定注册表）：流水线同 clearMatchesDetailedWith，种子由调用方给出；
-- 新特殊块仍按本盘的匹配段生成。
clearFromSeedsDetailedWith :: Registry -> Maybe Pos -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearFromSeedsDetailedWith reg prefer b seeds0 =
  clearWaveWith reg prefer (findMatchRunsWith reg b) b (nub seeds0)

-- | 只要「挖空后的盘面 + 清除数」的 clearMatchesDetailedWith 简化版（指定注册表）；
-- prefer 为新特殊块的优先生成位（交换目标格）。
clearMatchesAtWith :: Registry -> Maybe Pos -> Board -> (MBoard, Int)
clearMatchesAtWith reg prefer b =
  let (mb, n, _) = clearMatchesDetailedWith reg prefer b
  in (mb, n)
