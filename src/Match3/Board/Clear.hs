{-# LANGUAGE ScopedTypeVariables #-}

-- | 一轮消除：匹配清除（clearMatchesDetailedWith）与种子清除（clearFromSeedsDetailedWith），两者共用同一个
-- 一轮流水线 clearWaveWith；特殊块扩展（expandSpecials）、新特殊块生成（spawnSpecialsWith，查形状规则表）、彩蛋、
-- 邻格波及、飞碟吸收（maskUfoAbsorbSpecials / clearUfoAbsorbedWith：吸走 ≠ 引爆）以及计分公式。
--
-- 直接命中、叠层随格清除、邻格波及、特殊块爆炸范围、计色都查元素元素世界
-- （Match3.Element.World）；邻格波及按各元素 AdjacentRule 的 arOrder 依次执行（顺序见 Element.Builtin）。
-- 彩蛋开启走元素世界的开启规则（openRule），彩虹取色 / 特殊合成是成对交换规则（swapRule，在 Game.Move）。
-- 本模块不依赖内置元素世界，全部函数收 World；内置元素世界的短名在 Match3.Board.Default。
--
-- 依赖：Grid、Match、元素元素世界。
-- 同步：这里的函数只被 Match3.Board.Cascade 的单一连锁实现调用（结算与回放同一次计算），
-- 返回 (挖空后盘面, 清除数, 清除格)；Cascade 再按清除格在消除前盘面上统计障碍计数。
module Match3.Board.Clear
  ( expandSpecialsWith
  , spawnSpecialsWith
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
import Match3.Element.World (World, blastWith, chipOnHitWith, colorOfWith, openWith, runAdjacentWith, shapeRules, stripOnClearWith)
import Match3.Element.Special (spawnByShapes)
import Match3.Types
import Match3.Board.Grid
import Match3.Board.Match

-- | expandSpecials（指定元素世界）：爆炸范围 = 本体定义的 blast，能否点火 = 各层 activates（软锁纪律）。
-- 彩虹没有 blast（只经 rainbowClearSeeds 按交换对象取色，否则彩虹 × 宝石会重复清两色）。
expandSpecialsWith :: World -> Board -> [Pos] -> [Pos]
expandSpecialsWith world b seeds = go (nub seeds) (nub seeds)
  where
    go acc [] = acc
    go acc (p : ps) =
      let extra = blastWith world b (getCell b p) p
          new = filter (`notElem` acc) extra
      in go (acc ++ new) (ps ++ new)

-- | 新特殊块（按元素世界的有序形状规则表 shapeRules，解释器 Match3.Element.Special.spawnByShapes）：
-- 每条连线取第一条认领它的规则的产出（内置：长度 ≥5 彩虹，长度 4 按方向横 / 竖消）；只放在真正挖空的格上
-- （Flip / ice>1 的格留在盘面上，落点退到连线里别的可清格，否则 4 连在这类格上会悄悄丢掉特殊块）。
spawnSpecialsWith :: World -> Maybe Pos -> [MatchRun] -> [Pos] -> [(Pos, Cell)]
spawnSpecialsWith world = spawnByShapes (shapeRules world)

-- | countColor（指定元素世界）：按本体颜色 color 计（不看叠层）。
countColorWith :: World -> Board -> [Pos] -> Color -> Int
countColorWith world b ps col = length [p | p <- ps, colorOfWith world (getCell b p) == Just col]

-- | 一轮内的多轮开启（通用的「开启类元素」流程：开启规则来自元素世界的 openRule，内置只有彩蛋；
-- 开出的格本轮坐住、屏蔽后再展开爆炸，新命中的格进入下一批前沿）。
-- 返回 (盘面, 真消除格, 开启带来的直接命中格, 本轮坐住的格)。
surpriseClearPassWith :: World -> Board -> [Pos] -> (Board, [Pos], [Pos], [Pos])
surpriseClearPassWith world b0 seeds0 =
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
          let (bOpen, expl, savedNew) = openWith world board front
              saved' = nub (saved ++ savedNew)
              kept = filter (`notElem` saved') front
          in if null expl
               then go bOpen [] (trueAcc ++ kept) directAcc saved'
               else
                 let expanded = expandSpecialsWith world (maskSaved board saved') expl
                     (bChip, free1) = chipOnHitWith world bOpen expanded
                     processed = nub (front ++ trueAcc)
                     front' = filter (`notElem` processed) free1
                 in go bChip front' (trueAcc ++ kept ++ free1) (directAcc ++ expanded) saved'

-- | 匹配清除一轮（指定元素世界）：种子 = 全部匹配格。
clearMatchesDetailedWith :: World -> Maybe Pos -> Board -> (MBoard, Int, [Pos])
clearMatchesDetailedWith world prefer b =
  let runs = findMatchRunsWith world b
  in clearWaveWith world prefer runs b (nub (concatMap runPos runs))

-- | 一轮消除的**唯一流水线**（匹配清除与种子清除共用）：
--
--   1. expandSpecials：种子里能点火的特殊块展开爆炸范围（= 直接命中格）；
--   2. chipOnHitWith：直接命中按层结算（冰 → 叠层 → 本体：削层 / 揭层 / 消除 / 免疫）；
--   3. surpriseClearPassWith：彩蛋在邻格波及之前开启（3×3 爆炸计入真消除，与炸弹同口径）；
--   4. stripOnClearWith：真消除格上的草 / 藤 / 巧随格清掉（空洞不能再蔓延）；
--   5. runAdjacentWith：按 arOrder 跑各元素的邻格波及（已被直接命中的格不再重复波及；
--      彩蛋开出的特殊块与果汁机刚产出的炸弹本轮坐住，不被魔法帽 / 染色瓶改色）；
--   6. 清除格 = 真消除 ∪ 波及打碎的格（按规则顺序）；挖空后在清除格上放新特殊块。
clearWaveWith :: World -> Maybe Pos -> [MatchRun] -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearWaveWith world prefer runs b base =
  let expanded = expandSpecialsWith world b base
      -- Ice chips first: iced gems stay, ice-free positions may clear.
      -- Do NOT strip Grass/Vine/Choco on raw expand seeds — soft hits (ice>1 /
      -- Flip / Chain peel) keep on-cell overlays (same discipline as adjacent).
      (bIced, iceFree) = chipOnHitWith world b expanded
      (bSurp2, trueClears, surpDirect, surpSaved) = surpriseClearPassWith world bIced iceFree
      bClearedOv = stripOnClearWith world bSurp2 trueClears
      -- Cells that already took a direct-hit peel/chip must not also receive an
      -- ortho adjacent peel this wave (Chain2/Stone2/Safe2 on a Line/Bomb path).
      directHits = nub (expanded ++ surpDirect)
      (bAdj, dead, _sits) = runAdjacentWith world trueClears directHits surpSaved bClearedOv
      allPos = nub (trueClears ++ dead)
      n = length allPos
      mb0 = setManyM (toM bAdj) [(p, Nothing) | p <- allPos]
      spawns = spawnSpecialsWith world prefer runs allPos
      mb1 = setManyM mb0 [(p, Just cell) | (p, cell) <- spawns, p `elem` allPos]
  in (mb1, n, allPos)

-- | 不带波次倍数的计分：每格 10 分。
scoreForCleared :: Int -> Score
scoreForCleared n = n * 10

-- | Wave 1 = 1x, wave 2 = 2x, ... (cells * 10 * wave).
scoreForWave :: Int -> Int -> Score
scoreForWave wave n = n * 10 * max 1 wave

-- | 飞碟吸收前把吸收格上的直线 / 炸弹 / 彩虹降为 Normal，种子清除（clearFromSeedsDetailedWith）
-- 就不会走 expandSpecials 引爆它们（吸走 ≠ 引爆）。冰与叠层保留。
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

-- | 飞碟吸收一轮：吸收格上的特殊块先降级（'maskUfoAbsorbSpecials'），再以吸收格为种子做种子清除。
clearUfoAbsorbedWith :: World -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearUfoAbsorbedWith world b absorbed =
  clearFromSeedsDetailedWith world Nothing (maskUfoAbsorbSpecials b absorbed) absorbed

-- | 种子清除一轮（指定元素世界）：流水线同 clearMatchesDetailedWith，种子由调用方给出；
-- 新特殊块仍按本盘的匹配段生成。
clearFromSeedsDetailedWith :: World -> Maybe Pos -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearFromSeedsDetailedWith world prefer b seeds0 =
  clearWaveWith world prefer (findMatchRunsWith world b) b (nub seeds0)

-- | 只要「挖空后的盘面 + 清除数」的 clearMatchesDetailedWith 简化版（指定元素世界）；
-- prefer 为新特殊块的优先生成位（交换目标格）。
clearMatchesAtWith :: World -> Maybe Pos -> Board -> (MBoard, Int)
clearMatchesAtWith world prefer b =
  let (mb, n, _) = clearMatchesDetailedWith world prefer b
  in (mb, n)
