-- | 三消作为通用游戏接口 "Engine.Game" 的第一个实现（第三刀）。
--
-- 动作：交换、三种道具（锤子 / 自由交换 / 十字）、撤销、提示、手动洗牌；
-- 事件：规则层的效果事件 Match3.Element.Event.Event（由回放脚本 traceEventsWith 展开），
-- 经 toEffect 映射成通用 Effect；结局：gsOver（Won / Lost / LevelClear）。
--
-- 每个动作只算一次：play 调用 resolveSwap / resolveHammer / … 一次，同时得到结算后的状态、
-- 结局、回放脚本、边沿特效 MoveFx 与效果事件；trySwap / traceSwap 等旧入口是同一计算的投影，
-- 因此经本模块与直接调用旧入口的结果逐位相同（测试 engine_match3_instance_matches_direct_api）。
--
-- 依赖：Engine.*（通用层）、Match3.Game.*、Match3.Element.*。前端的三消插件经本模块执行动作。
module Match3.Engine
  ( -- * 动作与开局
    Action(..)
  , Setup(..)
    -- * 执行
  , Played(..)
  , play
  , playWith
    -- * 通用接口实例
  , match3Game
  , match3GameWith
  , match3Status
  , toEffect
  , eventKindTag
  ) where

import Data.Maybe (isJust)
import Engine.Effect (Effect(..))
import Engine.Game (Game(..), Step(..))
import Match3.Board.Grid (adjacent, inBounds)
import Match3.Daily (dailyConfig, dailySeed)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Event (Event(..), EventKind(..))
import Match3.Element.Registry (Registry)
import Match3.Game.Boosters (resolveCrossClearWith, resolveFreeSwapWith, resolveHammerWith)
import Match3.Game.Level (newDailyGame, newGame, newGameAtLevel)
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.Shuffle (shuffleGameWith)
import Match3.Game.State (GameState(..), MoveFx(..), applyHintWith, moveFx, undoMove)
import Match3.Game.Trace (MoveTrace, emptyTrace, traceEventsWith)
import Match3.Types

-- | 三消的动作。
data Action
  = Swap Pos Pos        -- ^ 交换相邻两格
  | Hammer Pos          -- ^ 锤子
  | FreeSwap Pos Pos    -- ^ 自由交换（不要求相邻）
  | CrossClear Pos      -- ^ 十字清除
  | Undo                -- ^ 撤销一步
  | Hint                -- ^ 请求提示（写入 gsHint）
  | Shuffle             -- ^ 手动洗牌（保留装饰）
  deriving (Eq, Show)

-- | 开局配置。
data Setup
  = Campaign Int          -- ^ 战役第 n 关（0 起）
  | CustomLevel GameConfig     -- ^ 任意配置（不带战役装饰）
  | Daily Int Int Int     -- ^ 每日挑战（年 月 日；种子由日期决定，gameNew 的种子参数不用）
  deriving (Eq, Show)

-- | 一个动作的完整结果（三消外壳需要的全部数据）。
data Played = Played
  { pdState    :: GameState      -- ^ 结算后的状态（被拒时为规则层给出的回滚状态）
  , pdOutcome  :: Maybe Outcome  -- ^ 交换 / 道具的 Outcome；撤销 / 提示 / 洗牌为 Nothing
  , pdTrace    :: MoveTrace      -- ^ 回放脚本（非走步动作为空脚本）
  , pdFx       :: MoveFx         -- ^ 本次操作的边沿特效（被拒时为空）
  , pdEvents   :: [Event]        -- ^ 效果事件（按时间顺序）
  , pdHint     :: Maybe (Pos, Pos) -- ^ Hint 动作找到的提示
  , pdAccepted :: Bool           -- ^ 动作是否被接受
  }

-- | 用内置注册表执行一个动作。
play :: Action -> GameState -> Played
play = playWith defaultRegistry

-- | 用指定注册表执行一个动作。
playWith :: Registry -> Action -> GameState -> Played
playWith reg act gs = case act of
  Swap p q -> move (resolveSwapWith reg p q gs)
  Hammer p -> move (resolveHammerWith reg p gs)
  FreeSwap p q -> move (resolveFreeSwapWith reg p q gs)
  CrossClear p -> move (resolveCrossClearWith reg p gs)
  Undo -> case undoMove gs of
    Just gs' -> other gs' [] True
    Nothing -> other gs [] False
  Hint ->
    let (gs', h) = applyHintWith reg gs
    in (other gs' [] True) {pdHint = h}
  Shuffle
    | isJust (gsOver gs) -> other gs [] False
    | otherwise -> other (shuffleGameWith reg gs) [Event EvShuffle 0 "shuffle" [] 0] True
  where
    move (gs', out, mt) =
      let fx = moveFx gs gs' out
          ok = out /= NoMatch && out /= InvalidSwap
      in Played gs' (Just out) mt fx (if ok then traceEventsWith reg mt else []) Nothing ok
    other gs' evs ok = Played gs' Nothing (emptyTrace gs') (MoveFx 0 []) evs Nothing ok

-- | 通用接口实例（内置注册表）。
match3Game :: Game Setup GameState Action Event Outcome
match3Game = match3GameWith defaultRegistry

-- | 通用接口实例（指定注册表）。
match3GameWith :: Registry -> Game Setup GameState Action Event Outcome
match3GameWith reg =
  Game
    { gameName = "match3"
    , gameNew = newFrom
    , gameStep = \s a ->
        if isJust (gsOver s)
          then Step s [] (gsOver s) False
          else
            let p = playWith reg a s
            in Step (pdState p) (pdEvents p) (gsOver (pdState p)) (pdAccepted p)
    , gameOutcome = gsOver
    , gameActions = \s ->
        [ Swap p q
        | not (isJust (gsOver s))
        , r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , q <- [(r, c + 1), (r + 1, c)]
        , inBounds q
        , adjacent p q
        , pdAccepted (playWith reg (Swap p q) s)
        ]
    , gameStatus = match3Status
    , gameEffect = toEffect
    }
  where
    newFrom setup seed = case setup of
      Campaign li -> newGameAtLevel li (levelConfig (allLevels !! li)) seed
      CustomLevel cfg -> newGame cfg seed
      Daily y m d -> newDailyGame (dailyConfig y m d) (dailySeed y m d)

-- | 外壳用的具名数值（标题栏 / HUD）。
match3Status :: GameState -> [(String, Int)]
match3Status gs =
  [ ("level", gsLevel gs + 1)
  , ("score", gsScore gs)
  , ("moves", gsMoves gs)
  , ("combo", gsCombo gs)
  , ("hammers", gsHammers gs)
  , ("freeSwaps", gsFreeSwaps gs)
  , ("crossClears", gsCrossClears gs)
  ]

-- | 规则层事件 → 通用效果：节拍 = 轮次（evWave），格 = 每对的目标格。
toEffect :: Event -> Effect
toEffect e = Effect (evWave e) (eventKindTag (evKind e)) (evElement e) (map snd (evCells e)) (evAmount e)

-- | 事件种类的字符串标签。
eventKindTag :: EventKind -> String
eventKindTag k = case k of
  EvClear -> "clear"
  EvHit -> "hit"
  EvBlast -> "blast"
  EvDrain -> "drain"
  EvScore -> "score"
  EvCombo -> "combo"
  EvTick -> "tick"
  EvBelt -> "belt"
  EvSpread -> "spread"
  EvMove -> "move"
  EvShuffle -> "shuffle"
