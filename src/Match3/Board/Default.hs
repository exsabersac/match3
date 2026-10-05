-- | 内置元素世界的便捷入口：Board.{Match, Clear, Gravity, Cascade} 自身不依赖 defaultWorld，
-- 全部函数都收一个 World（*With）；这里只把仍有调用方的几个定义为「*With defaultWorld」的短名，
-- 给只跑内置元素的调用方（Core 再导出、随机开局、前端、测试）用。没有调用方的短名不在这里，直接用 *With。
--
-- 依赖方向：Board.* ← 本模块 → Element.Builtin；主流程（Game.Resolve 等）一律把 world 传下去，不经本模块。
module Match3.Board.Default
  ( gravityFixedCell
  , applyGravity
  , applyPortalTeleports
  , settleBoardPortals
  , findMatchRuns
  , findMatches
  , hasAnyMatch
  , hasValidMove
  , findHint
  , expandSpecials
  , countColor
  , clearMatches
  , cascadeMatches
  , cascadeSeeds
  , cascadeCountdowns
  , stepCascade
    -- * 关卡级钩子
  , LevelHooks(..)
  , noHooks
  , builtinHooks
  ) where

import Match3.Board.Cascade
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid (MBoard)
import Match3.Board.Match
import Match3.Board.Hooks (LevelHooks(..), noHooks)
import Match3.Element.Builtin (PortalLevel(..), UfoLevel(..), defaultWorld)
import Match3.Element.Mechanic (SomeMechanic(..))
import Match3.Element.Level (levelHooksWith)
import Match3.Types
import Match3.Ufo (Ufo)
import System.Random (RandomGen)

--------------------------------------------------------------------------------
-- 关卡级钩子

-- | 内置元素世界下、只有飞碟与传送门两种关卡级元素的钩子。
builtinHooks :: [Ufo] -> [(Pos, Pos)] -> LevelHooks
builtinHooks ufos portals = levelHooksWith defaultWorld [SomeMechanic (UfoLevel ufos), SomeMechanic (PortalLevel portals)]

--------------------------------------------------------------------------------
-- Match3.Board.Gravity

-- | Immortal décor that must not fall with gravity. Same class as portal/belt
-- stuck blockers (Bottle / Maker / MagicHat / Snail): they never clear and are
-- not portal-transferable, so gravity-packing them into a portal/belt slot (or
-- off a décor seed via Cross column wipe) permanently soft-locks the layout.
gravityFixedCell :: Cell -> Bool
gravityFixedCell = gravityFixedCellWith defaultWorld

-- | 整盘按列下落：固定格（染色瓶 / 果汁机 / 魔法帽 / 蜗牛）原地不动并把列分段，段内可下落的格压到段底。
applyGravity :: MBoard -> MBoard
applyGravity = applyGravityWith defaultWorld

-- | Bidirectional portal teleport on MBoard: gem/cookie/countdown on A with hole at B
-- moves A -> B (and reverse). Used after gravity + bottom-cookie drain so clears can
-- open exits without snatching cookies that already touched the bottom row.
-- 传送门是关卡级元素，传送经钩子（builtinHooks [] portals）的 onSettle。
applyPortalTeleports :: LevelHooks -> MBoard -> MBoard
applyPortalTeleports = onSettle

-- | Gravity, drain bottom cookies, portal teleports (optional), gravity, drain again.
-- Cookies that reach the bottom must collect before a portal can snatch them
-- (触底优先于传送门); cookies that teleport onto a bottom exit still drain after.
-- Third component: bottom cells cookies drained from (Carpet / particle seeds).
settleBoardPortals :: LevelHooks -> MBoard -> (MBoard, Int, [Pos])
settleBoardPortals = settleBoardPortalsWith defaultWorld

--------------------------------------------------------------------------------
-- Match3.Board.Match

-- | 全部横、竖 ≥3 同色连线（先横后竖，行 / 列优先）；被障碍 / 叠层打断的不算。
findMatchRuns :: Board -> [MatchRun]
findMatchRuns = findMatchRunsWith defaultWorld

-- | 所有匹配格：各连线位置的去重并集。
findMatches :: Board -> [Pos]
findMatches = findMatchesWith defaultWorld

-- | 盘面上是否存在任意 ≥3 连。
hasAnyMatch :: Board -> Bool
hasAnyMatch = hasAnyMatchWith defaultWorld

-- | True if some adjacent gem-gem swap would create a match.
hasValidMove :: Board -> Bool
hasValidMove = hasValidMoveWith defaultWorld

-- | First adjacent swap that would create a match or activate a rainbow (for hint).
findHint :: Board -> Maybe (Pos, Pos)
findHint = findHintWith defaultWorld

--------------------------------------------------------------------------------
-- Match3.Board.Clear

-- | Expand clears: LineH/LineV/Bomb effects when those gem cells are in the seed set.
-- Soft-locked specials do not fire: ice>1 only chips; Chain/Curtain peel without
-- clearing (same discipline as chipIceOnClear). Last ice (ice==1) clears + activates.
-- Rainbow is a no-op here (partner color comes from rainbowClearSeeds only).
expandSpecials :: Board -> [Pos] -> [Pos]
expandSpecials = expandSpecialsWith defaultWorld

-- | Count how many cleared positions have a given color (pre-clear board; stones skip).
countColor :: Board -> [Pos] -> Color -> Int
countColor = countColorWith defaultWorld

-- | Clear matches (+ special expansions + adjacent stones), place new specials.
clearMatches :: Board -> (MBoard, Int)
clearMatches = clearMatchesAtWith defaultWorld Nothing

--------------------------------------------------------------------------------
-- Match3.Board.Cascade

-- | 普通匹配连锁到稳定（波次从 1 开始）。prefer 只作用于第一轮的特殊块生成位。
cascadeMatches :: RandomGen g => Maybe Pos -> LevelHooks -> g -> Board -> CascadeRun g
cascadeMatches = cascadeMatchesWith defaultWorld

-- | 种子起手的连锁（彩虹 / 特殊合成 / 道具 / 倒计时爆炸）：第一轮清种子（波次 1）并沉降补子，
-- 跑一次飞碟（吸到则单独一轮，波次 2），再接普通匹配连锁（波次编号衔接「已完成的起手轮数」）。
-- 种子为空时等同 cascadeMatches。
cascadeSeeds :: RandomGen g => Maybe Pos -> [Pos] -> LevelHooks -> g -> Board -> CascadeRun g
cascadeSeeds = cascadeSeedsWith defaultWorld

-- | 一步之后倒计时 -1；归零的 3×3 爆炸走种子连锁（带飞碟与传送门）。
-- 没有归零时终盘就是 tick 之后的盘面（数字减一），不产生回放轮次。
cascadeCountdowns :: RandomGen g => LevelHooks -> g -> Board -> CascadeRun g
cascadeCountdowns = cascadeCountdownsWith defaultWorld

-- | 恰好一轮匹配消除 + 沉降补子（不跑飞碟、无传送门）；无匹配时返回 Nothing。
-- 与 cascadeMatchesFromWith 的单轮是同一组调用：clear → settleBoardPortals → refill。
stepCascade :: RandomGen g => g -> Board -> Maybe (Board, Int, g)
stepCascade = stepCascadeAtWith defaultWorld Nothing
