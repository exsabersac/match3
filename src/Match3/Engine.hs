-- | 三消作为通用游戏接口 "Engine.Game" 的实现。
--
-- 动作：交换、三种道具（锤子 / 自由交换 / 十字）、提示、手动洗牌；撤销不在这里——历史由通用层
-- "Engine.History" 持有（match3Shell = withHistory match3History match3Game），GameState 不带历史；
-- 事件：规则层的效果事件 Match3.Element.Event.Event（由回放脚本 traceEventsWith 展开），
-- 经 toEffect 映射成通用 Effect；结局类型是 Terminal（gsOver：TWon / TLost / TLevelClear）。
--
-- 每个动作只算一次：play 调用 resolveSwap / resolveHammer / … 一次，同时得到结算后的状态、
-- 结局、回放脚本、边沿特效 MoveFx 与效果事件；trySwap / traceSwap 等直接入口是同一计算的投影，
-- 因此经本模块与直接调用的结果逐位相同（测试 engine_match3_instance_matches_direct_api）。
--
-- 整步报告：gameStep 的 stepReport 是本次的 Played（状态 / 本步 Outcome / 回放脚本 / MoveFx / 事件 / 提示），
-- 前端只调 match3Shell 的 gameStep 就拿到全部表现数据，不直接调 play（测试 engine_frontend_steps_only_via_gameStep）。
--
-- 依赖：Engine.*（通用层）、Match3.Game.*、Match3.Element.*。前端的三消插件经本模块执行动作。
module Match3.Engine
  ( -- * 动作与开局
    Action(..)
  , Setup(..)
    -- * 执行
  , Played(..)
  , rejectedPlayed
  , play
  , playWith
    -- * 通用接口实例
  , match3Game
  , match3GameWith
    -- * 带撤销历史的实例（外壳用）
  , match3History
  , match3Shell
  , match3ShellWith
  , match3Status
  , toEffect
  , eventKindTag
  ) where

import Data.Maybe (fromMaybe, isJust)
import Engine.Effect (Effect(..))
import Engine.Game (Game(..), Step(..))
import Engine.History (History, HistoryPolicy(..), Undoable, withHistory)
import Match3.Board.Grid (neighborsInBounds)
import Match3.Daily (Day, Month, Year, dailyConfig, dailySeed)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Event (Event(..), EventKind(..))
import Match3.Element.Level (levelRegistryIn)
import Match3.Element.Registry (Registry)
import Match3.Game.Boosters (resolveCrossClearWith, resolveFreeSwapWith, resolveHammerWith)
import Match3.Game.Level (campaignGame, newDailyGame, newGame, newGameAtLevel)
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.Shuffle (shuffleGameWith)
import Match3.Game.State (GameState(..), MoveFx(..), applyHintWith, clearMoveFx, moveFx)
import Match3.Game.Trace (MoveTrace, emptyTrace, traceEventsWith)
import Match3.Types

-- | 三消的动作。
data Action
  = Swap Pos Pos        -- ^ 交换相邻两格
  | Hammer Pos          -- ^ 锤子
  | FreeSwap Pos Pos    -- ^ 自由交换（不要求相邻）
  | CrossClear Pos      -- ^ 十字清除
  | Hint                -- ^ 请求提示（写入 gsHint）
  | Shuffle             -- ^ 手动洗牌（保留装饰）
  deriving (Eq, Show)

-- | 开局配置。
data Setup
  = Campaign Int          -- ^ 战役第 n 关（0 起）
  | CustomLevel GameConfig     -- ^ 任意配置（不带战役装饰）
  | Daily Year Month Day  -- ^ 每日挑战（年 月 日；种子由日期决定，gameNew 的种子参数不用）
  deriving (Eq, Show)

-- | 一个动作的完整结果（三消外壳需要的全部数据）。
data Played = Played
  { pdState    :: GameState      -- ^ 结算后的状态（被拒时为规则层给出的回滚状态）
  , pdOutcome  :: Maybe Outcome  -- ^ 交换 / 道具的 Outcome；提示 / 洗牌为 Nothing
  , pdTrace    :: MoveTrace      -- ^ 回放脚本（非走步动作为空脚本）
  , pdFx       :: MoveFx         -- ^ 本次操作的边沿特效（被拒时为空）
  , pdEvents   :: [Event]        -- ^ 效果事件（按时间顺序）
  , pdHint     :: Maybe (Pos, Pos) -- ^ Hint 动作找到的提示
  , pdAccepted :: Bool           -- ^ 动作是否被接受
  }

-- | 被拒动作的报告：状态不变、没有结局 / 脚本 / 特效 / 事件。
rejectedPlayed :: GameState -> Played
rejectedPlayed s = Played s Nothing (emptyTrace s) (MoveFx 0 []) [] Nothing False

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
  Hint ->
    let (gs', h) = applyHintWith reg gs
    in (other gs' [] True) {pdHint = h}
  Shuffle
    | isJust (gsOver gs) -> other gs [] False
    | otherwise -> other (shuffleGameWith reg gs) [Event EvShuffle 0 (ElementName "shuffle") [] 0] True
  where
    move (gs', out, mt) =
      let fx = moveFx gs gs' out
          ok = out /= NoMatch && out /= InvalidSwap
      -- 事件按本步结算用的注册表展开（levelRegistryIn：新玩法 8 魔法地格的扩爆格要算进 EvBlast 的范围；
      -- 其余关卡它只可能换形状表，而事件展开不读形状表，与用 reg 逐项相同）
      in Played gs' (Just out) mt fx (if ok then traceEventsWith (levelRegistryIn reg (gsLevelElems gs)) mt else []) Nothing ok
    other gs' evs ok = Played gs' Nothing (emptyTrace gs') (MoveFx 0 []) evs Nothing ok

-- | 通用接口实例（内置注册表）。
match3Game :: Game Setup GameState Action Event Terminal Played
match3Game = match3GameWith defaultRegistry

-- | 通用接口实例（指定注册表）。
-- 终局后拒绝一切走步 / 洗牌；提示除外（只写 gsHint、不推进对局，与原前端「终局后按 H 仍给提示」一致）。
match3GameWith :: Registry -> Game Setup GameState Action Event Terminal Played
match3GameWith reg =
  Game
    { gameName = "match3"
    , gameNew = newFrom
    , gameStep = \s a ->
        if isJust (gsOver s) && a /= Hint
          then Step s [] (gsOver s) False (Just (rejectedPlayed s))
          else
            let p = playWith reg a s
            in Step (pdState p) (pdEvents p) (gsOver (pdState p)) (pdAccepted p) (Just p)
    , gameOutcome = gsOver
    , gameActions = \s ->
        [ Swap p q
        | not (isJust (gsOver s))
        , let b = gsBoard s
        , p <- boardPositions b
        , q <- neighborsInBounds rightAndDown b p
        , pdAccepted (playWith reg (Swap p q) s)
        ]
    , gameStatus = match3Status
    , gameEffect = toEffect
    }
  where
    newFrom setup seed = case setup of
      -- 没有这一关（越界下标）时按默认配置开局，不报错
      Campaign li -> fromMaybe (newGameAtLevel li defaultConfig seed) (campaignGame li seed)
      CustomLevel cfg -> newGame cfg seed
      Daily y m d -> newDailyGame (dailyConfig y m d) (dailySeed y m d)

-- | 三消的撤销规则：交换与三种道具被接受时记快照（最多 20 份，与原 gsHistory 相同）；
-- 快照去掉提示与洗牌标记，撤销回去时再清掉本步特效字段与终局标记（原 snapshot / undoMove 的逐字搬迁）。
match3History :: HistoryPolicy GameState Action
match3History =
  HistoryPolicy
    { hpLimit = 20
    , hpRecord = isMove
    , hpSnapshot = \gs -> gs {gsHint = Nothing, gsShuffled = False}
    , hpRestore = \prev -> (clearMoveFx prev) {gsHint = Nothing, gsOver = Nothing, gsShuffled = False}
    }
  where
    isMove a = case a of
      Swap _ _ -> True
      Hammer _ -> True
      FreeSwap _ _ -> True
      CrossClear _ -> True
      Hint -> False
      Shuffle -> False

-- | 外壳用的实例：内置注册表 + 撤销历史。
match3Shell :: Game Setup (History GameState) (Undoable Action) Event Terminal Played
match3Shell = match3ShellWith defaultRegistry

-- | 外壳用的实例（指定注册表）。
match3ShellWith :: Registry -> Game Setup (History GameState) (Undoable Action) Event Terminal Played
match3ShellWith reg = withHistory match3History (match3GameWith reg)

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
toEffect e = Effect (evWave e) (eventKindTag (evKind e)) (unElementName (evElement e)) (map snd (evCells e)) (evAmount e)

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
