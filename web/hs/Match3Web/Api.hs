-- | 网页端与纯核心之间的「薄接口层」（纯函数，不依赖 JSFFI，原生 GHC 也能编译测试）。
--
-- 设计原则：
--   * 一切规则判定（能否交换、消除、下落、补子、连锁、计分、胜负、撤销）都走通用接口
--     "Engine.Game" 的三消外壳实例 match3Shell（= withHistory match3History match3Game），
--     与桌面版 SDL 外壳（app/UI/Plugin.hs）是同一条路径：前端只调 gameStep，这里只做序列化。
--   * 每一步的表现数据全部来自 stepReport（Played）：回放脚本 pdTrace、效果事件 pdEvents、Outcome；
--     规则只算一次（不再像 spike 初版那样 traceSwap + trySwap 各算一遍）。
--   * 输出是手写的最小 JSON（Match3Web.Json，不引入 aeson，减小 wasm 体积与依赖面）。
--   * 逐轮回放（ComboFx 阶段机）见 Match3Web.Anim；apiSwapAnim 额外返回建立回放所需的 AnimSeed。
module Match3Web.Api
  ( WebGame
  , apiNew
  , apiSwap
  , apiSwapAnim
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

import Engine.Game (Game(..), Step(..))
import Engine.History (History, Undoable(..), histNow, historyDepth)
import Match3.Board.Grid (mboardRows)
import Match3.Core
import Match3.Element.Event (Event(..))
import Match3.Engine (Action(..), Played(..), Setup(..), eventKindTag, match3Shell)
import Match3.Game.Trace (emptyTrace)
import Match3Web.Anim (AnimSeed, seedOf)
import Match3Web.Json

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
apiSwap p1 p2 h = let (h', _, j) = apiSwapAnim p1 p2 h in (h', j)

-- | 同 apiSwap，另返回本步的回放输入（本步不需要回放时 Nothing，见 Match3Web.Anim.seedOf）。
apiSwapAnim :: Pos -> Pos -> WebGame -> (WebGame, Maybe AnimSeed, String)
apiSwapAnim p1 p2 = runStep (Act (Swap p1 p2))

-- | 撤销一步（终局后也可撤销；没有历史时 accepted=false、状态不变）。JSON 形状同 apiSwap。
apiUndo :: WebGame -> (WebGame, String)
apiUndo h = let (h', _, j) = runStep Undo h in (h', j)

-- | 执行一个动作：只调 gameStep，表现数据取自 stepReport。
runStep :: Undoable Action -> WebGame -> (WebGame, Maybe AnimSeed, String)
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
     , rep >>= seedOf (histNow h)
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
      -- 关卡级元素（棋盘底层 / 飞碟），渲染层按它们画传送带、传送门、地毯与飞碟
    , ("belts", arr [arr (map encodePos b) | b <- gsBelts gs])
    , ("portals", arr (map encodePair (gsPortals gs)))
    , ("ufos", arr [obj [("p", encodePos (ufoCell u)), ("c", int (colorNum (ufoColor u)))] | u <- gsUfos gs])
    , ("carpets", arr (map encodePos (levelCarpets (gsLevel gs))))
    , ("carpetOpen", arr (map encodePos (gsCarpetOpen gs)))
    , ("board", encodeBoard (gsBoard gs))
    ]
  where
    gs = histNow h
    levelName i = case [lvlName l | l <- allLevels, lvlIndex l == i] of
      (n : _) -> n
      [] -> "?"

-- | 目标进度（第 5 刀：即核心 gsProgress，与桌面 HUD / 标题同一个数；由目标数据统一算）。
progress :: GameState -> Int
progress = gsProgress

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
      ++ [("name", str n) | ViewCount (CountNamed n) _ <- [goalView g]]   -- 段 5：按元素名计数的目标（jelly / bubble）
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
    , ("holes", arr [arr (map (maybe "null" encodeCell) row) | row <- mboardRows (cwHoles w)])
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
-- 盘面编码：Board 按 boardRows 展开成行列表（行优先，8×8），每格一个结构化对象：
--   宝石  {"t":"G","c":1..5,"k":"N|H|V|B|R","i":冰层,"o":覆盖物名或 null,"n":覆盖物层数}
--         覆盖物名：grass / vine / choco / fog / chain / freeze / curtain / steam（无层数的为 0）
--   其他  {"t":<元素>, ...}：stone/chest/honey/cake/safe {"n"}；balloon/bottle {"c"}；cookie / hat / surprise / spirit；
--         maker {"c","n"}；snail {"dr","dc"}；flip {"c":正面,"b":背面}；countdown {"c","n"}；custom {"name","v"}
--   每格另带 "s"（核心 show 文本，调试 / 未知元素占位用）。渲染层按 t 查表（www/cells.js，对应桌面 UI.CellTable）。

encodeBoard :: Board -> String
encodeBoard b = arr [arr (map encodeCell row) | row <- boardRows b]

encodeCell :: Cell -> String
encodeCell cell = obj (fields ++ [("s", str (show cell))])
  where
    t x = ("t", str x)
    n k = ("n", int k)
    col c = ("c", int (colorNum c))
    fields = case cell of
      Gem c k ice ov ->
        [ t "G", col c, ("k", str (kindCode k)), ("i", int ice)
        , ("o", maybe "null" (str . overlayName) ov), ("n", int (maybe 0 overlayLayers ov)) ]
      Stone k -> [t "stone", n k]
      Chest k -> [t "chest", n k]
      Honey k -> [t "honey", n k]
      Balloon c -> [t "balloon", col c]
      Cookie -> [t "cookie"]
      Cake k -> [t "cake", n k]
      MagicHat -> [t "hat"]
      Maker c k -> [t "maker", col c, n k]
      Snail dr dc -> [t "snail", ("dr", int dr), ("dc", int dc)]
      Safe k -> [t "safe", n k]
      Flip f b -> [t "flip", col f, ("b", int (colorNum b))]
      Surprise -> [t "surprise"]
      Bottle c -> [t "bottle", col c]
      TimeSpirit -> [t "spirit"]
      Countdown c k -> [t "countdown", col c, n k]
      Custom name v -> [t "custom", ("name", str name), ("v", int v)]

overlayName :: CellOverlay -> String
overlayName ov = case ov of
  Grass -> "grass"
  Vine -> "vine"
  Choco -> "choco"
  Fog _ -> "fog"
  Chain _ -> "chain"
  Freeze _ -> "freeze"
  Curtain _ -> "curtain"
  Steam -> "steam"

overlayLayers :: CellOverlay -> Int
overlayLayers ov = case ov of
  Fog k -> k
  Chain k -> k
  Freeze k -> k
  Curtain k -> k
  _ -> 0

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
