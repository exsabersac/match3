{-# LANGUAGE NamedFieldPuns #-}

-- | 对局状态：GameState 及其（部分字段）相等语义、撤销快照、本步特效 MoveFx 的边沿触发、提示与撤销。
--
-- 依赖：Match3.Types、Match3.Board.*（findHint）、Ufo / Conveyor（字段类型）。
-- 不变量：gsCombo / gsLastCleared 只描述最近一次**真正结算**的一步；任何被拒操作经
-- clearMoveFx / rejectMove 清零，moveFx 对 NoMatch / InvalidSwap / 已终局一律返回空，
-- 前端因此不会重播上一步的连击（护栏 failed_swap_resets_combo_feedback 等）。
module Match3.Game.State
  ( GameState(..)
  , gsCount
  , gsProgress
  , gsGoalMet
  , gsCollected
  , gsColorBag
  , clearMoveFx
  , rejectMove
  , MoveFx(..)
  , moveFx
  , applyHint
  , applyHintWith
  ) where

import Data.Maybe (isJust)
import Match3.Counts (CounterKey(..), Counts, colorBag, countOf, namedCounts)
import Match3.Board.Match (findHintWith)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry)
import Match3.Ufo (Ufo(..))
import Match3.Conveyor (Belt)
import Match3.Types
import System.Random (StdGen)

-- | 一局的全部规则状态。前端只读；所有修改都经 trySwap / use* / shuffleGame 等纯函数返回新值；撤销历史不在这里（段 3 起由 Engine.History 持有）。
data GameState = GameState
  { gsBoard         :: Board
  , gsScore         :: Score
  , gsMoves         :: MovesLeft
  , gsGoal          :: LevelGoal
  , gsCounts        :: Counts       -- 第 4 刀：按计数键累计（各色清除 CountColor / 石块 / 宝箱 / 蜂蜜罐 / 气球 / 饼干 / 蛋糕 /
                                    -- 保险箱 / 时间精灵 / 飞碟吸收 CountUfo / 地毯覆盖 CountCarpets / 扩展元素 CountNamed 名字），
                                    -- 读法见 gsCount；第 5 刀起颜色袋也在这里（gsColorBag / gsCollected 改为派生读数）
  , gsGen           :: StdGen
  , gsOver          :: Maybe Outcome
  , gsLevel         :: Int
  , gsHint          :: Maybe (Pos, Pos)
  , gsCombo         :: Int   -- last move max cascade wave (0 if none)
  , gsShuffled      :: Bool  -- True if last ensurePlayable reshuffled
  , gsBelts         :: [Belt] -- conveyor paths (开心消消乐传送带)
  , gsPortals       :: [(Pos, Pos)] -- bidirectional portal pairs (传送门)
  , gsHammers       :: Int    -- hammer booster charges
  , gsFreeSwaps     :: Int    -- free-swap booster charges (any two cells)
  , gsCrossClears   :: Int    -- cross-clear booster charges
  , gsUfos          :: [Ufo]  -- flying saucers (飞碟)
  , gsCarpetOpen    :: [Pos]  -- uncovered carpet / floor tiles (地毯目标)
  , gsLastCleared   :: [Pos]  -- cells cleared last move (UI particles; not belt/snail noise)
  , gsDaily         :: Bool   -- True for date-seeded daily challenge (通关≠战役推进)
  , gsGround        :: Ground -- 地面层（段 2c，元素名 + 层数；内置关卡恒为 []）
  }

-- | 与第 4 刀前派生的 Show 逐字相同：各计数仍按旧字段名、旧位置打印（元素查询快照对 show 取散列）。
-- gsElementCounts 打印 namedCounts（按名字升序；旧实现按首次出现，快照里每局至多一个名字）。
-- 没有旧字段的键（CountSpirits、扩展元素借用的其余内置键）不打印，相等判断仍比较全部计数。
instance Show GameState where
  showsPrec d gs =
    showParen (d >= 11) $
      showString "GameState {"
        . field "gsBoard" (gsBoard gs) . sep
        . field "gsScore" (gsScore gs) . sep
        . field "gsMoves" (gsMoves gs) . sep
        . field "gsGoal" (gsGoal gs) . sep
        . field "gsCollected" (gsCollected gs) . sep
        . field "gsColorBag" (gsColorBag gs) . sep
        . field "gsStonesCleared" (cnt CountStones) . sep
        . field "gsChestsCleared" (cnt CountChests) . sep
        . field "gsHoneyCleared" (cnt CountHoney) . sep
        . field "gsBalloonsPopped" (cnt CountBalloons) . sep
        . field "gsCookiesCollected" (cnt CountCookies) . sep
        . field "gsCakesCleared" (cnt CountCakes) . sep
        . field "gsSafesOpened" (cnt CountSafes) . sep
        . field "gsGen" (gsGen gs) . sep
        . field "gsOver" (gsOver gs) . sep
        . field "gsLevel" (gsLevel gs) . sep
        . field "gsHint" (gsHint gs) . sep
        . field "gsCombo" (gsCombo gs) . sep
        . field "gsShuffled" (gsShuffled gs) . sep
        . field "gsBelts" (gsBelts gs) . sep
        . field "gsPortals" (gsPortals gs) . sep
        . field "gsHammers" (gsHammers gs) . sep
        . field "gsFreeSwaps" (gsFreeSwaps gs) . sep
        . field "gsCrossClears" (gsCrossClears gs) . sep
        . field "gsUfos" (gsUfos gs) . sep
        . field "gsUfoCollected" (cnt CountUfo) . sep
        . field "gsCarpetOpen" (gsCarpetOpen gs) . sep
        . field "gsCarpetsCovered" (cnt CountCarpets) . sep
        . field "gsLastCleared" (gsLastCleared gs) . sep
        . field "gsDaily" (gsDaily gs) . sep
        . field "gsElementCounts" (namedCounts (gsCounts gs)) . sep
        . field "gsGround" (gsGround gs)
        . showChar '}'
    where
      cnt k = gsCount k gs
      sep = showString ", "
      field :: Show a => String -> a -> ShowS
      field name v = showString name . showString " = " . showsPrec 0 v

instance Eq GameState where
  a == b =
    gsBoard a == gsBoard b
      && gsScore a == gsScore b
      && gsMoves a == gsMoves b
      && gsGoal a == gsGoal b
      && gsCounts a == gsCounts b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b
      && gsDaily a == gsDaily b
      && gsBelts a == gsBelts b
      && gsPortals a == gsPortals b
      && gsHammers a == gsHammers b
      && gsFreeSwaps a == gsFreeSwaps b
      && gsCrossClears a == gsCrossClears b
      && gsUfos a == gsUfos b
      && gsCarpetOpen a == gsCarpetOpen b
      && gsGround a == gsGround b

-- | 某计数键的累计个数（缺省 0；第 4 刀前是各自的字段，如 gsStonesCleared = gsCount CountStones）。
gsCount :: CounterKey -> GameState -> Int
gsCount k = countOf k . gsCounts

-- | 目标进度（HUD / 标题 / 网页的主进度，Match3.Goal.goalProgress）。
gsProgress :: GameState -> Int
gsProgress gs = goalProgress (gsGoal gs) (gsScore gs) (gsCounts gs)

-- | 当前计数是否满足关卡目标。
gsGoalMet :: GameState -> Bool
gsGoalMet gs = goalMet (gsGoal gs) (gsScore gs) (gsCounts gs)

-- | 第 5 刀前的 gsCollected 字段（派生读数）：不计分数的目标进度——分数目标恒 0，其余等于 gsProgress。
-- 旧字段只在结算时按目标种类更新、开局为 0，数值与此处逐步相同（金标准 col= 锁定）。
gsCollected :: GameState -> Int
gsCollected gs = goalProgress (gsGoal gs) 0 (gsCounts gs)

-- | 第 5 刀前的 gsColorBag 字段（派生读数）：各色累计清除数，按 allColors 顺序、含 0。
gsColorBag :: GameState -> [(Color, Int)]
gsColorBag = colorBag . gsCounts

-- | 清空「上一步」的 UI 反馈字段（连击波数 / 本步清除格）。
-- 这两个字段只描述最近一次**真正结算**的一步；任何没有结算的操作（无匹配回滚、
-- 挡交换、非相邻、道具无效、洗牌、撤销）都必须把它们归零，否则前端会把旧值
-- 当成新一步的结果，再播一遍爆击（连击）特效。规则判定不读这两个字段。
clearMoveFx :: GameState -> GameState
clearMoveFx gs = gs { gsCombo = 0, gsLastCleared = [] }

-- | 交换 / 道具被拒（NoMatch）时的回滚状态：盘面不变，清提示与本步反馈。
rejectMove :: GameState -> GameState
rejectMove gs = (clearMoveFx gs) { gsHint = Nothing, gsShuffled = False }

-- | 一次操作之后，前端该播的特效（边沿触发，只看这一次调用的结果）。
data MoveFx = MoveFx
  { fxCombo   :: Int    -- ^ 本步连击波数；> 1 才播连击 / 爆击特效
  , fxCleared :: [Pos]  -- ^ 本步清除格（闪光 + 粒子）
  } deriving (Eq, Show)

-- | 由「操作前状态、操作后状态、结果」决定要不要播特效。
-- 只有这次调用真正结算了一步（MoveApplied / 本次才产生的 Won / Lost / LevelClear）
-- 才返回 after 的连击与清除格；NoMatch / InvalidSwap / 操作前就已结束（trySwap 原样
-- 返回旧 gsOver）一律返回空，不会重播上一步的爆击特效。
moveFx :: GameState -> GameState -> Outcome -> MoveFx
moveFx before after out
  | isJust (gsOver before) = noFx
  | otherwise = case out of
      NoMatch -> noFx
      InvalidSwap -> noFx
      _ -> MoveFx (gsCombo after) (gsLastCleared after)
  where
    noFx = MoveFx 0 []

-- | 计算一手可走的交换并记在 gsHint（不改盘面）。
applyHint :: GameState -> (GameState, Maybe (Pos, Pos))
applyHint = applyHintWith defaultRegistry

-- | applyHint（指定注册表）：可走判定用这张表里的挡交换 / 匹配色定义。
applyHintWith :: Registry -> GameState -> (GameState, Maybe (Pos, Pos))
applyHintWith reg gs =
  let h = findHintWith reg (gsBoard gs)
  in (gs { gsHint = h }, h)
