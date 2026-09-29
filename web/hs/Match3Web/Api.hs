-- | 网页端与纯核心之间的「薄接口层」（纯函数，不依赖 JSFFI，原生 GHC 也能编译测试）。
--
-- 设计原则：
--   * 一切规则判定（能否交换、消除、下落、补子、连锁、计分、胜负、撤销）都走通用接口
--     "Engine.Game" 的三消外壳实例 match3Shell（= withHistory match3History match3Game），
--     与桌面版 SDL 外壳（app/UI/Plugin.hs）是同一条路径：前端只调 gameStep，这里只做序列化。
--   * 每一步的表现数据全部来自 stepReport（Played）：回放脚本 pdTrace、效果事件 pdEvents、Outcome；
--     规则只算一次（不再像 spike 初版那样 traceSwap + trySwap 各算一遍）。
--   * 输出是手写的最小 JSON（不引入 aeson，减小 wasm 体积与依赖面）。
module Match3Web.Api
  ( WebGame
  , apiNew
  , apiSwap
  , apiUndo
  , apiState
  , apiLevels
  , webState
  , encodeState
  , encodeOutcome
  , encodeTrace
  , encodeEvent
  , jsonString
  ) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)

import Engine.Game (Game(..), Step(..))
import Engine.History (History, Undoable(..), histNow, historyDepth)
import Match3.Core
import Match3.Element.Event (Event(..))
import Match3.Engine (Action(..), Played(..), Setup(..), eventKindTag, match3Shell)
import Match3.Game.Trace (emptyTrace)

-- ---------------------------------------------------------------------------
-- 对外接口（被 WebMain 的 JSFFI 导出包装）

-- | 网页端持有的一局：当前状态 + 撤销历史（与桌面外壳相同，历史只在 Engine.History 里存一份）。
type WebGame = History GameState

-- | 当前局面。
webState :: WebGame -> GameState
webState = histNow

-- | 新开一局：关卡序号（0 起）+ 随机种子。越界关卡号夹到合法范围。
apiNew :: Int -> Int -> (WebGame, String)
apiNew li seed =
  let i = max 0 (min (length allLevels - 1) li)
      h = gameNew match3Shell (Campaign i) seed
  in (h, obj [("ok", "true"), ("state", encodeState h)])

-- | 交换 (r1,c1) 与 (r2,c2)。返回新状态和 JSON：
--   { ok, accepted, outcome, trace:{start,waves,end,final,shuffle}, events:[...], state }
-- 被拒（NoMatch / InvalidSwap / 已结束）时 accepted=false、trace.waves 与 events 为空，状态不变（NoMatch 也不扣步）。
apiSwap :: Pos -> Pos -> WebGame -> (WebGame, String)
apiSwap p1 p2 = runStep (Act (Swap p1 p2))

-- | 撤销一步（终局后也可撤销；没有历史时 accepted=false、状态不变）。JSON 形状同 apiSwap。
apiUndo :: WebGame -> (WebGame, String)
apiUndo = runStep Undo

-- | 执行一个动作：只调 gameStep，表现数据取自 stepReport。
runStep :: Undoable Action -> WebGame -> (WebGame, String)
runStep act h =
  let st = gameStep match3Shell h act
      h' = stepState st
      gs' = histNow h'
      rep = stepReport st
      -- 走步的 Outcome 来自报告；撤销 / 终局后被拒没有走步结果，退回当前结局（未结束则 null）
      outcome = case rep >>= pdOutcome of
        Just o -> encodeOutcome o
        Nothing -> maybe "null" encodeOutcome (stepOutcome st)
      -- 撤销没有报告：给一个「盘面不动」的空脚本，前端无需特判
      trace = encodeTrace (maybe (emptyTrace gs') pdTrace rep)
  in ( h'
     , obj
         [ ("ok", "true")
         , ("accepted", bool (stepAccepted st))
         , ("outcome", outcome)
         , ("trace", trace)
         , ("events", arr (map encodeEvent (maybe [] pdEvents rep)))
         , ("state", encodeState h')
         ]
     )

-- | 仅序列化当前状态。
apiState :: WebGame -> String
apiState h = obj [("ok", "true"), ("state", encodeState h)]

-- | 关卡列表（序号、中文名、步数、目标描述），给选关界面用。
apiLevels :: String
apiLevels =
  arr
    [ obj
        [ ("index", int (lvlIndex l))
        , ("name", str (lvlName l))
        , ("moves", int (lvlMoves l))
        , ("goal", encodeGoal (lvlGoal l))
        ]
    | l <- allLevels
    ]

-- ---------------------------------------------------------------------------
-- 状态 / 结果 / 回放脚本

encodeState :: WebGame -> String
encodeState h =
  obj
    [ ("level", int (gsLevel gs))
    , ("name", str (levelName (gsLevel gs)))
    , ("score", int (gsScore gs))
    , ("moves", int (gsMoves gs))
    , ("goal", encodeGoal (gsGoal gs))
    , ("progress", int (progress gs))
    , ("target", int (goalTarget (gsGoal gs)))
    , ("over", maybe "null" encodeOutcome (gsOver gs))
    , ("loseHint", str (loseHint (gsGoal gs)))
    , ("combo", int (gsCombo gs))
    , ("shuffled", bool (gsShuffled gs))
    , ("undo", int (historyDepth h))
    , ("hint", maybe "null" encodePair (findHint (gsBoard gs)))
    , ("lastCleared", arr (map encodePos (gsLastCleared gs)))
    , ("ground", arr [obj [("p", encodePos p), ("name", str n), ("layers", int k)] | (p, (n, k)) <- gsGround gs])
    , ("board", encodeBoard (gsBoard gs))
    ]
  where
    gs = histNow h
    levelName i = case [lvlName l | l <- allLevels, lvlIndex l == i] of
      (n : _) -> n
      [] -> "?"

-- | 目标进度：参数顺序与核心 goalSatisfied 调用 goalMetEx 的顺序一致。
progress :: GameState -> Int
progress gs =
  goalProgressEx
    (gsGoal gs) (gsScore gs) (gsCollected gs) (gsColorBag gs)
    (gsStonesCleared gs) (gsUfoCollected gs) (gsChestsCleared gs)
    (gsHoneyCleared gs) (gsBalloonsPopped gs) (gsCookiesCollected gs)
    (gsCakesCleared gs) (gsSafesOpened gs)

encodeOutcome :: Outcome -> String
encodeOutcome o = case o of
  InvalidSwap -> tag "InvalidSwap" []
  NoMatch -> tag "NoMatch" []
  MoveApplied s -> tag "MoveApplied" [("score", int s)]
  Won s -> tag "Won" [("score", int s)]
  Lost s -> tag "Lost" [("score", int s)]
  LevelClear s n -> tag "LevelClear" [("score", int s), ("next", int n)]
  where
    tag t kvs = obj (("tag", str t) : kvs)

encodeGoal :: LevelGoal -> String
encodeGoal g =
  obj $
    [("kind", str (goalKind g)), ("text", str (show g)), ("target", int (goalTarget g))]
      ++ [("name", str n) | GoalNamed n _ <- [g]]   -- 段 5：按元素名计数的目标（jelly / bubble）
  where
    goalKind x = takeWhile (/= ' ') (show x)

-- | 一步的逐轮回放：start → waves[0..] → end（步末效果，按 afterWaves 插在第 k 轮之后）→ final
--   → shuffle（本步触发自动洗牌时的洗牌后盘面，否则 null）。
encodeTrace :: MoveTrace -> String
encodeTrace mt =
  obj
    [ ("start", encodeBoard (mtStart mt))
    , ("waves", arr (map encodeWave (mtWaves mt)))
    , ("end", arr (map encodeEnd (mtEnd mt)))
    , ("final", encodeBoard (mtFinal mt))
    , ("shuffle", maybe "null" encodeBoard (mtShuffle mt))
    ]

encodeWave :: CascadeWave -> String
encodeWave w =
  obj
    [ ("before", encodeBoard (cwBefore w))
    , ("cleared", arr (map encodePos (cwCleared w)))
    , ("drained", arr (map encodePos (cwDrained w)))
    , ("holes", arr [arr (map (maybe "null" encodeCell) row) | row <- cwHoles w])
    , ("after", encodeBoard (cwAfter w))
    , ("score", int (cwScore w))
    ]

encodeEnd :: EndStep -> String
encodeEnd e =
  obj
    [ ("afterWaves", int (esAfterWaves e))
    , ("before", encodeBoard (esBefore e))
    , ("after", encodeBoard (esAfter e))
    , ("effect", encodeEndEffect (esEffect e))
    ]

-- | 步末效果（结构化）：{type:"tick",cells} / {type:"belt",pairs} / {type:"spread",kind,pairs} / {type:"snail",moves}
--   pairs 为 [[来源],[目标]]。
encodeEndEffect :: EndEffect -> String
encodeEndEffect eff = case eff of
  EndCountdownTick ps -> obj [("type", str "tick"), ("cells", arr (map encodePos ps))]
  EndBeltShift ps -> obj [("type", str "belt"), ("pairs", arr (map encodePair ps))]
  EndSpread k ps -> obj [("type", str "spread"), ("kind", str (spreadName k)), ("pairs", arr (map encodePair ps))]
  EndSnail ms -> obj [("type", str "snail"), ("moves", arr (map snail ms))]
  where
    spreadName k = case k of
      SpreadVine -> "vine"
      SpreadChoco -> "choco"
      SpreadSteam -> "steam"
    snail m =
      obj
        [ ("from", encodePos (smFrom m))
        , ("to", encodePos (smTo m))
        , ("dir", encodePos (smDir m))
        , ("pushed", maybe "null" encodeCell (smPushed m))
        ]

-- | 效果事件（规则层 Match3.Element.Event.Event，按时间顺序）：
--   {kind, beat, subject, pairs:[[来源],[目标]], amount}
--   kind 与 Match3.Engine.eventKindTag 相同（clear / hit / blast / drain / score / combo / tick / belt / spread / move / shuffle）；
--   beat = evWave（与 trace.end 的 afterWaves 同一时间轴，同 beat 的事件同时播放）；subject = 元素名（无则 ""）。
--   通用 Engine.Effect 只保留目标格（toEffect 取 snd）；这里保留 (来源, 目标) 对，
--   方便前端画爆炸方向、皮带 / 蔓延 / 蜗牛的移动轨迹。
encodeEvent :: Event -> String
encodeEvent e =
  obj
    [ ("kind", str (eventKindTag (evKind e)))
    , ("beat", int (evWave e))
    , ("subject", str (evElement e))
    , ("pairs", arr (map encodePair (evCells e)))
    , ("amount", int (evAmount e))
    ]

-- ---------------------------------------------------------------------------
-- 盘面编码：Board 按 boardRows 展开成行列表（行优先，8×8）
--   宝石：{"t":"G","c":1..5,"k":"N|H|V|B|R","i":冰层,"o":覆盖物或 null}
--   其他格：{"t":"X","s":"<核心 show 文本>","c":颜色或 0}
-- 渲染层只需按 t/c/k 画色块；非宝石格先用文字占位，完整版再逐种出图。

encodeBoard :: Board -> String
encodeBoard b = arr [arr (map encodeCell row) | row <- boardRows b]

encodeCell :: Cell -> String
encodeCell cell = case cell of
  Gem c k ice ov ->
    obj
      [ ("t", str "G")
      , ("c", int (colorNum c))
      , ("k", str (kindCode k))
      , ("i", int ice)
      , ("o", maybe "null" (str . show) ov)
      ]
  Countdown c n -> other (colorNum c) ("Countdown " ++ show n)
  Flip f _ -> other (colorNum f) (show cell)
  Balloon c -> other (colorNum c) (show cell)
  Bottle c -> other (colorNum c) (show cell)
  Maker c _ -> other (colorNum c) (show cell)
  Custom name v -> obj [("t", str "X"), ("c", int 0), ("s", str (name ++ " " ++ show v)), ("name", str name), ("v", int v)]
  _ -> other 0 (show cell)
  where
    other c s = obj [("t", str "X"), ("c", int c), ("s", str s)]

colorNum :: Color -> Int
colorNum c = fromEnum c + 1

kindCode :: GemKind -> String
kindCode k = case k of
  Normal -> "N"
  LineH -> "H"
  LineV -> "V"
  Bomb -> "B"
  Rainbow -> "R"

encodePos :: Pos -> String
encodePos (r, c) = arr [int r, int c]

encodePair :: (Pos, Pos) -> String
encodePair (a, b) = arr [encodePos a, encodePos b]

-- ---------------------------------------------------------------------------
-- 极简 JSON 构造器

obj :: [(String, String)] -> String
obj kvs = "{" ++ intercalate "," [str k ++ ":" ++ v | (k, v) <- kvs] ++ "}"

arr :: [String] -> String
arr xs = "[" ++ intercalate "," xs ++ "]"

int :: Int -> String
int = show

bool :: Bool -> String
bool True = "true"
bool False = "false"

-- | 对外暴露的 JSON 字符串编码（WebMain 的错误信息用）。
jsonString :: String -> String
jsonString = str

-- | JSON 字符串转义；非 ASCII（中文关卡名等）原样输出，由 toJSString 负责 UTF-8。
str :: String -> String
str s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc ch
      | ord ch < 0x20 = "\\u" ++ pad4 (showHex (ord ch) "")
      | otherwise = [ch]
    pad4 h = replicate (4 - length h) '0' ++ h
