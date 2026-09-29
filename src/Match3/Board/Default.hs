-- | 内置注册表的便捷入口（段 2c）：Board.{Match, Clear, Gravity, Cascade} 自身**不再依赖**
-- defaultRegistry，全部函数都收一个 Registry（*With）；这里把不带 With 的旧名定义为
-- 「*With defaultRegistry」，给只跑内置元素的调用方（Core 再导出、随机开局、前端、测试）用。
--
-- 依赖方向：Board.* ← 本模块 → Element.Builtin；主流程（Game.Resolve 等）一律把 reg 传下去，不经本模块。
module Match3.Board.Default
  ( gravityFixedCell
  , colGravity
  , applyGravity
  , drainBottomCookies
  , applyPortalTeleports
  , settleBoardPortals
  , settleRefill
  , findMatchRuns
  , groupGemRuns
  , findMatches
  , hasAnyMatch
  , hasValidMove
  , findHint
  , expandSpecials
  , countColor
  , clearMatches
  , clearMatchesAt
  , surpriseClearPass
  , clearMatchesDetailed
  , clearUfoAbsorbed
  , clearFromSeedsDetailed
  , cascadeMatches
  , cascadeMatchesFrom
  , cascadeSeeds
  , cascadeAfterBelt
  , cascadeCountdowns
  , stepCascade
  , stepCascadeAt
  ) where

import Match3.Board.Cascade
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid (MBoard)
import Match3.Board.Match
import Match3.Element.Builtin (defaultRegistry)
import Match3.Types
import Match3.Ufo (Ufo)
import System.Random (RandomGen)

--------------------------------------------------------------------------------
-- Match3.Board.Gravity

-- | Immortal décor that must not fall with gravity. Same class as portal/belt
-- stuck blockers (Bottle / Maker / MagicHat / Snail): they never clear and are
-- not portal-transferable, so gravity-packing them into a portal/belt slot (or
-- off a décor seed via Cross column wipe) permanently soft-locks the layout.
gravityFixedCell :: Cell -> Bool
gravityFixedCell = gravityFixedCellWith defaultRegistry

-- | Column gravity: fixed immortals stay put and split the column into segments;
-- fallable cells (gems / Cookie / Countdown / Flip / clearable obstacles) pack
-- down within each segment.
colGravity :: [Maybe Cell] -> [Maybe Cell]
colGravity = colGravityWith defaultRegistry

-- | 整盘按列下落：固定格（染色瓶 / 果汁机 / 魔法帽 / 蜗牛）原地不动并把列分段，段内可下落的格压到段底。
applyGravity :: MBoard -> MBoard
applyGravity = applyGravityWith defaultRegistry

-- | Collect cookies that sit on the bottom row after gravity (开心消消乐饼干掉落收集).
-- Removes them, re-applies gravity, repeats until no bottom-row cookies remain.
-- Returns drained bottom positions so GoalCarpet can cover tiles Cookie only
-- occupied mid-settle (gravity/portal/belt → bottom → drain) — before/after
-- board compare cannot see that intermediate occupancy.
drainBottomCookies :: MBoard -> (MBoard, Int, [Pos])
drainBottomCookies = drainBottomCookiesWith defaultRegistry

-- | Bidirectional portal teleport on MBoard: gem/cookie/countdown on A with hole at B
-- moves A -> B (and reverse). Used after gravity + bottom-cookie drain so clears can
-- open exits without snatching cookies that already touched the bottom row.
applyPortalTeleports :: [(Pos, Pos)] -> MBoard -> MBoard
applyPortalTeleports = applyPortalTeleportsWith defaultRegistry

-- | Gravity, drain bottom cookies, portal teleports (optional), gravity, drain again.
-- Cookies that reach the bottom must collect before a portal can snatch them
-- (触底优先于传送门); cookies that teleport onto a bottom exit still drain after.
-- Third component: bottom cells cookies drained from (Carpet / particle seeds).
settleBoardPortals :: [(Pos, Pos)] -> MBoard -> (MBoard, Int, [Pos])
settleBoardPortals = settleBoardPortalsWith defaultRegistry

-- | 一轮沉降：settle + refill（与 stepCascadeDetailed / 种子清除用的完全相同）。
settleRefill :: RandomGen g => [(Pos, Pos)] -> g -> MBoard -> (Board, [Pos], g)
settleRefill = settleRefillWith defaultRegistry

--------------------------------------------------------------------------------
-- Match3.Board.Match

-- | 全部横、竖 ≥3 同色连线（先横后竖，行 / 列优先）；被障碍 / 叠层打断的不算。
findMatchRuns :: Board -> [MatchRun]
findMatchRuns = findMatchRunsWith defaultRegistry

-- | Group contiguous same-color *gems*; stones and color changes break runs.
groupGemRuns :: Board -> [Pos] -> [(Color, [Pos])]
groupGemRuns = groupGemRunsWith defaultRegistry

-- | 所有匹配格：各连线位置的去重并集。
findMatches :: Board -> [Pos]
findMatches = findMatchesWith defaultRegistry

-- | 盘面上是否存在任意 ≥3 连。
hasAnyMatch :: Board -> Bool
hasAnyMatch = hasAnyMatchWith defaultRegistry

-- | True if some adjacent gem-gem swap would create a match.
hasValidMove :: Board -> Bool
hasValidMove = hasValidMoveWith defaultRegistry

-- | First adjacent swap that would create a match or activate a rainbow (for hint).
findHint :: Board -> Maybe (Pos, Pos)
findHint = findHintWith defaultRegistry

--------------------------------------------------------------------------------
-- Match3.Board.Clear

-- | Expand clears: LineH/LineV/Bomb effects when those gem cells are in the seed set.
-- Soft-locked specials do not fire: ice>1 only chips; Chain/Curtain peel without
-- clearing (same discipline as chipIceOnClear). Last ice (ice==1) clears + activates.
-- Rainbow is a no-op here (partner color comes from rainbowClearSeeds only).
expandSpecials :: Board -> [Pos] -> [Pos]
expandSpecials = expandSpecialsWith defaultRegistry

-- | Count how many cleared positions have a given color (pre-clear board; stones skip).
countColor :: Board -> [Pos] -> Color -> Int
countColor = countColorWith defaultRegistry

-- | Clear matches (+ special expansions + adjacent stones), place new specials.
clearMatches :: Board -> (MBoard, Int)
clearMatches = clearMatchesAtWith defaultRegistry Nothing

-- | 只要「挖空后的盘面 + 清除数」的 clearMatchesDetailed 简化版；prefer 为新特殊块的优先生成位（交换目标格）。
clearMatchesAt :: Maybe Pos -> Board -> (MBoard, Int)
clearMatchesAt = clearMatchesAtWith defaultRegistry

-- | Open Surprises against clear seeds; explode blasts expand specials + chip ice
-- and re-open nested Surprises until the frontier is quiet.
-- Bomb parity: Bomb→Surprise opens to special/explode; Surprise explode must
-- likewise open nested boxes instead of hole-deleting them via chipIce alone.
-- Saved specials (Bomb/Line from Surprise) must not activate in this pass — mask
-- them as Normal before expandSpecials. Same-pass placement and later re-explode
-- of an unconsumed Surprise center would otherwise fire-and-survive the special.
-- Pre-existing specials (not in saved) still expand. chipIce runs on bOpen so
-- saved specials remain on the board while explode centers clear.
-- Returns (board, trueClears, surpriseDirectHits, savedSpecialPositions).
surpriseClearPass :: Board -> [Pos] -> (Board, [Pos], [Pos], [Pos])
surpriseClearPass = surpriseClearPassWith defaultRegistry

-- | Like clearMatchesAt but also returns the cleared positions (pre-spawn).
clearMatchesDetailed :: Maybe Pos -> Board -> (MBoard, Int, [Pos])
clearMatchesDetailed = clearMatchesDetailedWith defaultRegistry

-- | Clear UFO-absorbed cells: mask specials first, then normal seed clear.
clearUfoAbsorbed :: Board -> [Pos] -> (MBoard, Int, [Pos])
clearUfoAbsorbed = clearUfoAbsorbedWith defaultRegistry

-- | Clear an explicit seed set (expand specials + adjacent stones).
clearFromSeedsDetailed :: Maybe Pos -> Board -> [Pos] -> (MBoard, Int, [Pos])
clearFromSeedsDetailed = clearFromSeedsDetailedWith defaultRegistry

--------------------------------------------------------------------------------
-- Match3.Board.Cascade

-- | 普通匹配连锁到稳定（波次从 1 开始）。prefer 只作用于第一轮的特殊块生成位。
cascadeMatches :: RandomGen g => Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeMatches = cascadeMatchesWith defaultRegistry

-- | 普通匹配连锁，波次编号从 startW 之后继续（种子起手 / 飞碟吸收已占用的轮数）。
cascadeMatchesFrom :: RandomGen g => Int -> Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeMatchesFrom = cascadeMatchesFromWith defaultRegistry

-- | 种子起手的连锁（彩虹 / 特殊合成 / 道具 / 倒计时爆炸）：第一轮清种子（波次 1）并沉降补子，
-- 跑一次飞碟（吸到则单独一轮，波次 2），再接普通匹配连锁（波次编号衔接「已完成的起手轮数」）。
-- 种子为空时等同 cascadeMatches。
cascadeSeeds :: RandomGen g => Maybe Pos -> [Pos] -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeSeeds = cascadeSeedsWith defaultRegistry

-- | 皮带移位之后：成消则整段连锁；否则仍沉降一次（收皮带送到底行的饼干，回放记为一个
-- 只有沉降的轮次，盘面没变且没收饼干时不记），沉降后成消再接连锁并补上饼干数与收饼干位。
cascadeAfterBelt :: RandomGen g => [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeAfterBelt = cascadeAfterBeltWith defaultRegistry

-- | 一步之后倒计时 -1；归零的 3×3 爆炸走种子连锁（带飞碟与传送门）。
-- 没有归零时终盘就是 tick 之后的盘面（数字减一），不产生回放轮次。
cascadeCountdowns :: RandomGen g => [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeCountdowns = cascadeCountdownsWith defaultRegistry

-- | 恰好一轮匹配消除 + 沉降补子（不跑飞碟、无传送门）；无匹配时返回 Nothing。
-- 与 cascadeMatchesFrom 的单轮是同一组调用：clear → settleBoardPortals → refill。
stepCascade :: RandomGen g => g -> Board -> Maybe (Board, Int, g)
stepCascade = stepCascadeAtWith defaultRegistry Nothing

-- | 第一轮在 prefer 处优先生成特殊块的 stepCascade。
stepCascadeAt :: RandomGen g => Maybe Pos -> g -> Board -> Maybe (Board, Int, g)
stepCascadeAt = stepCascadeAtWith defaultRegistry
