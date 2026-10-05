-- | 网页端与纯核心之间的「薄接口层」（纯函数，不依赖 JSFFI，原生 GHC 也能编译测试）。
--
-- 设计原则：
--   * 一切规则判定（能否交换、消除、下落、补子、连锁、计分、胜负、撤销）都走通用接口
--     "Engine.Game" 的三消外壳实例 match3Shell（= withHistory match3History match3Game），
--     网页是唯一前端（原 SDL 桌面版已移除）：前端只调 gameStep，这里只做序列化。
--   * 每一步的表现数据全部来自 stepReport（Played）：回放脚本 pdTrace、效果事件 pdEvents、Outcome；
--     规则只算一次（不分别调 traceSwap 与 trySwap）。
--   * 输出是手写的最小 JSON（Match3Web.Json，不引入 aeson，减小 wasm 体积与依赖面）。
--   * 逐轮回放（ComboFx 阶段机）见 Match3Web.Anim；apiSwapAnim 额外返回建立回放所需的 AnimSeed。
module Match3Web.Api
  ( WebGame
  , apiNew
  , apiSwap
  , apiSwapAnim
  , apiUndo
  , apiHammer
  , apiCross
  , apiFreeSwap
  , apiShuffle
  , apiDaily
  , apiRestart
  , apiAdvance
  , apiShowcase
  , apiProgress
  , apiMapJump
  , apiBadge
  , apiState
  , apiLevels
  , apiMeta
  , webState
  , encodeState
  , encodeOutcome
  , encodeTrace
  , encodeEvent
  , jsonString
  ) where

import Engine.Game (Game(..), Step(..))
import Engine.History (History, Undoable(..), histNow, historyDepth, startHistory)
import Match3.Core
import Match3.Element.Event (Event(..), EventKind(..))
import Match3.Engine (Action(..), Played(..), Setup(..), eventKindTag, match3Shell)
import Match3.View
import Match3Web.Anim (AnimSeed, seedOf)
import Match3Web.Json
import UI.Chapters (chapterLabel, chapterStarts, chapterTitle)
import UI.GoalIcon (goalIcon)
import UI.MoveText (MoveUi(..), keepsTool)
import UI.Presentation (Curve(..), RGB)
import UI.Restart (restartSame)
import UI.Showcase (showcaseState)
import UI.WebMeta (WebMeta(..), webMeta)

-- ---------------------------------------------------------------------------
-- 对外接口（被 WebMain 的 JSFFI 导出包装）

-- | 网页端持有的一局：当前状态 + 撤销历史（与原桌面版外壳相同，历史只在 Engine.History 里存一份）。
type WebGame = History GameState

-- | 当前局面。
webState :: WebGame -> GameState
webState = histNow

-- | 新开一局：关卡序号（0 起）+ 随机种子。越界关卡号夹到合法范围。
apiNew :: Int -> Int -> (WebGame, String)
apiNew li seed =
  let i = clampLevelIndex li
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

-- | 三种道具（同原桌面版 UI.Actions.applyBooster，经 gameStep 的 Hammer / CrossClear / FreeSwap）：JSON 形状同 m3Swap，
-- 另加 keepTool（UI.MoveText.keepsTool：之后是否留在点选模式——只有自由交换换不掉、不扣次数时留下）。
apiHammer :: Pos -> WebGame -> (WebGame, Maybe AnimSeed, String)
apiHammer p = runStepWith (Just UiHammer) (Act (Hammer p))

apiCross :: Pos -> WebGame -> (WebGame, Maybe AnimSeed, String)
apiCross p = runStepWith (Just UiCross) (Act (CrossClear p))

apiFreeSwap :: Pos -> Pos -> WebGame -> (WebGame, Maybe AnimSeed, String)
apiFreeSwap p q = runStepWith (Just UiFreeSwap) (Act (FreeSwap p q))

-- | 手动洗牌（同原桌面版 S 键 UI.Input.keyShuffle：gameStep 的 Shuffle，终局后被拒）；没有回放脚本，前端播一段轻落。
apiShuffle :: WebGame -> (WebGame, Maybe AnimSeed, String)
apiShuffle = runStep (Act Shuffle)

-- | 每日挑战（同原桌面版 D 键：Setup Daily，种子由日期决定）。
apiDaily :: Int -> Int -> Int -> (WebGame, String)
apiDaily y m d = fresh (gameNew match3Shell (Daily (Year y) (Month m) (Day d)) 0)

-- | 重开本关（同原桌面版 R 键 UI.Input.restartSame）：每日挑战按开局步数与原目标换种子重开，战役关 restartLevel。
apiRestart :: Int -> Int -> WebGame -> (WebGame, String)
apiRestart startMoves seed h = fresh (startHistory (restartSame startMoves seed (histNow h)))

-- | 结局后前进（同原桌面版 N / 空格 / 回车 / 点结算面板，UI.Actions.advanceOrMsg）：过关 → nextLevel（携带剩余步数，最多 3 步），
-- 通关 → 从第 1 关重开战役，失败 → 同 apiRestart；未结束时 accepted=false、状态不变。
-- 返回 {ok, accepted, startMoves, state}：startMoves 是新一局的「开局步数」（三星分母；过关进下一关时取该关印制步数，不含携带）。
apiAdvance :: Int -> Int -> WebGame -> (WebGame, String)
apiAdvance startMoves seed h = case gvOver (gameView gs) of
  Just (TLevelClear _ _) ->
    let gs' = nextLevel gs seed
        gv' = gameView gs'
    in done gs' (maybe (gvMoves gv') lvMoves (lookup (gvLevel gv') [(lvIndex l, l) | l <- levelViews]))
  Just (TWon _) -> case campaignGame 0 seed of
    Just gs' -> done gs' (gvMoves (gameView gs'))
    Nothing -> rejected
  Just (TLost _) -> done (restartSame startMoves seed gs) startMoves
  Nothing -> rejected
  where
    gs = histNow h
    done gs' sm =
      let h' = startHistory gs'
      in (h', obj [("ok", "true"), ("accepted", "true"), ("startMoves", int sm), ("state", encodeState h')])
    rejected = (h, obj [("ok", "true"), ("accepted", "false"), ("startMoves", int startMoves), ("state", encodeState h)])

-- | 元素展示盘（同原桌面版 MATCH3_SHOWCASE：UI.Showcase.showcaseState，清空撤销历史）。
apiShowcase :: WebGame -> (WebGame, String)
apiShowcase h = fresh (startHistory (showcaseState (histNow h)))

fresh :: WebGame -> (WebGame, String)
fresh h = (h, obj [("ok", "true"), ("state", encodeState h)])

-- | 选关进度与结算星级（原桌面版 App 的 appMaxReached / appStartMoves 由网页持有，传进来）：
--   {reached, stars, dots}——reached = 开局记到当前关（freshLevelUi 的 max appMaxReached 当前关），
--   终局后再按 unlockAfterOutcome 解锁（每日挑战不解锁）；stars = starRating 开局步数 剩余步数；
--   dots = 每关一个字符的进度点（Match3.View.levelDots：C 当前 / D 已过 / U 已解锁 / L 未解锁）。
apiProgress :: Int -> Int -> WebGame -> String
apiProgress reached startMoves h =
  obj [("reached", int r2), ("stars", int (starRating startMoves (gvMoves gv))), ("dots", str (map dot (levelDots (gvLevelIndex gv) r2)))]
  where
    gs = histNow h
    gv = gameView gs
    r1 = max reached (gvLevel gv)
    r2 = maybe r1 (unlockAfterOutcome gs r1 . fromTerminal) (gvOver gv)
    dot d = case d of
      DotCurrent -> 'C'
      DotDone -> 'D'
      DotUnlocked -> 'U'
      DotLocked -> 'L'

-- | 选关地图点击（同原桌面版 UI.Input.mapClick 的 mapClickJump）：{jump: 关卡下标 | null}——null = 当前关或未解锁（关地图、保留进度）。
apiMapJump :: Int -> Int -> WebGame -> String
apiMapJump reached clicked h = obj [("jump", maybe "null" int (mapClickJump (gvLevel (gameView (histNow h))) reached clicked))]

-- | HUD 右下角分数徽章（同原桌面版 drawHudArt 的 Match3.View.scoreBadge）：
-- 回放中（replaying ≠ 0）传当前轮连击与滚动中的分数；播完后传总结剩余帧数与本步最高连击。
--   {kind:"combo"|"rolling"|"summary"|"score", n, shuffled}
apiBadge :: Int -> Int -> Int -> Int -> Int -> WebGame -> String
apiBadge replaying combo shown summaryLeft best h = case scoreBadge replay summaryLeft best (gameView (histNow h)) of
  BadgeCombo n -> badge "combo" n False
  BadgeRolling n -> badge "rolling" n False
  BadgeSummary n -> badge "summary" n False
  BadgeScore sh n -> badge "score" n sh
  where
    replay = if replaying /= 0 then Just (ReplayView combo shown) else Nothing
    badge k n sh = obj [("kind", str k), ("n", int n), ("shuffled", bool sh)]

-- | 执行一个动作：只调 gameStep，表现数据取自 stepReport。
runStep :: Undoable Action -> WebGame -> (WebGame, Maybe AnimSeed, String)
runStep = runStepWith Nothing

-- | 同 runStep；给出道具的界面路径时 JSON 另带 keepTool（UI.MoveText.keepsTool）。
runStepWith :: Maybe MoveUi -> Undoable Action -> WebGame -> (WebGame, Maybe AnimSeed, String)
runStepWith ui act h =
  let st = gameStep match3Shell h act
      h' = stepState st
      gs' = histNow h'
      rep = stepReport st
      -- 走步的 Outcome 来自报告；撤销 / 终局后被拒没有走步结果，退回当前结局（未结束则 null）
      outcome = case rep >>= pdOutcome of
        Just o -> encodeOutcome o
        Nothing -> maybe "null" (encodeOutcome . fromTerminal) (stepOutcome st)
      -- 撤销没有报告：给一个「盘面不动」的空脚本，前端无需特判
      trace = encodeTrace (maybe (emptyTrace gs') pdTrace rep)
  in ( h'
     , rep >>= seedOf (histNow h)
     , obj $
         [ ("ok", "true")
         , ("accepted", bool (stepAccepted st))
         , ("outcome", outcome)
         , ("trace", trace)
         , ("events", arr (map encodeEvent (maybe [] pdEvents rep)))
         , ("state", encodeState h')
         ]
         ++ [("keepTool", bool (keepsTool u (maybe InvalidSwap id (rep >>= pdOutcome)))) | Just u <- [ui]]
     )

-- | 仅序列化当前状态。
apiState :: WebGame -> String
apiState h = obj [("ok", "true"), ("state", encodeState h)]

-- | 关卡列表（序号、中文名、步数、目标描述），给选关界面用（读 Match3.View.levelViews）。
apiLevels :: String
apiLevels =
  arr
    [ obj
        [ ("index", int (lvIndex l))
        , ("name", str (lvName l))
        , ("moves", int (lvMoves l))
        , ("goal", encodeGoal (lvGoal l))
        ]
    | l <- levelViews
    ]

-- | m3Meta()：网页启动时读一次的表现表（UI.WebMeta：颜色、格子取色规则、碎屑色、生长曲线、帧数、音效名），
-- 网页不再手抄这些表。
--   {"colorRGB":{"1":[r,g,b],…},"elementRGB":{名字:[r,g,b]},"colorTags":[标签],"tagRGB":{标签:[r,g,b]},
--    "fallbackRGB":[r,g,b],"spreadCrumbRGB":{名字:[r,g,b]},"tickCrumbRGB":[r,g,b]|null,"spreadGlow":[r,g,b],
--    "spreadCurves":{名字:{"curve":"linear"|"easeOut"|"segments","n"?:段数}},"frames":{名字:帧数},"sounds":[名字]}
apiMeta :: String
apiMeta =
  obj
    [ ("colorRGB", table [(show k, v) | (k, v) <- wmColorRGB m])
    , ("elementRGB", table (wmElementRGB m))
    , ("colorTags", arr (map str (wmColorTags m)))
    , ("tagRGB", table (wmTagRGB m))
    , ("fallbackRGB", rgb (wmFallbackRGB m))
    , ("spreadCrumbRGB", table (wmSpreadCrumbRGB m))
    , ("tickCrumbRGB", maybe "null" rgb (wmTickCrumbRGB m))
    , ("spreadGlow", rgb (wmSpreadGlow m))
    , ("spreadCurves", obj [(n, curve c) | (n, c) <- wmSpreadCurves m])
    , ("frames", obj [(n, int f) | (n, f) <- wmFrames m])
    , ("sounds", arr (map str (wmSounds m)))
      -- 选关地图的章节（UI.Chapters，与原桌面版 UI.LevelMap 同一张表）：[{start: 章节第一关的下标, label: CH1…, title: 第一章…}]
    , ("chapters", arr [obj [("start", int i), ("label", str (chapterLabel i)), ("title", str (chapterTitle k))] | (k, i) <- zip [0 ..] chapterStarts])
    ]
  where
    m = webMeta
    table kvs = obj [(k, rgb v) | (k, v) <- kvs]
    curve c = case c of
      CurveLinear -> obj [("curve", str "linear")]
      CurveEaseOut -> obj [("curve", str "easeOut")]
      CurveSegments n -> obj [("curve", str "segments"), ("n", int n)]

rgb :: RGB -> String
rgb (r, g, b) = arr [int (fromIntegral r), int (fromIntegral g), int (fromIntegral b)]

-- ---------------------------------------------------------------------------
-- 状态 / 结果 / 回放脚本

-- | 局面 JSON：全部读视图模型 Match3.View（与原桌面版 HUD / 标题同一份读数），不从 GameState 现算。
encodeState :: WebGame -> String
encodeState h =
  obj
    [ ("level", int (gvLevel gv))
    , ("name", str (gvRawName gv))
      -- 每日挑战（gvDaily；HUD 关名画「每日挑战」、没有规则角标）与窗口标题（Match3.View.titleLine，同原桌面版标题栏，网页写进 document.title）
    , ("daily", bool (gvDaily gv))
    , ("title", str (titleLine gv))
      -- 道具剩余次数（gvBoosters，同原桌面版 HUD 的三枚道具芯片）
    , ("boosters", obj [("hammer", int (bHammers bs)), ("swap", int (bFreeSwaps bs)), ("cross", int (bCrossClears bs))])
      -- 本关打开的规则开关角标（视图模型 gvRules 查 Match3.View.ruleBadge，与原桌面版 HUD 同一张表）：
      -- [{name, text, icons}]，前端 HUD 按列表逐个画，新规则登记进 ruleBadgeTable 就自动显示
    , ("rules", arr (map encodeRuleBadge (ruleBadges gv)))
    , ("score", int (gvScore gv))
    , ("moves", int (gvMoves gv))
    , ("goal", encodeGoal goal)
    , ("progress", int (giProgress goal))
    , ("target", int (giTarget goal))
      -- 雪怪 Boss 血条（新玩法 5，视图模型 gvBoss，与原桌面版 HUD 同一份读数）：{hp: 剩余, max: 满血}；目标不是「击败 Boss」时为 null
    , ("boss", maybe "null" encodeBoss (gvBoss gv))
    , ("over", maybe "null" (encodeOutcome . fromTerminal) (gvOver gv))
    , ("loseHint", str (giLoseHint goal))
    , ("combo", int (gvCombo gv))
    , ("shuffled", bool (gvShuffled gv))
    , ("undo", int (historyDepth h))
    , ("hint", maybe "null" encodePair (bvFoundHint bv))
    , ("lastCleared", arr (map encodePos (bvLastCleared bv)))
    , ("ground", arr [obj [("p", encodePos p), ("name", str (unElementName n)), ("layers", int k)] | (p, (n, k)) <- bvGround bv])
      -- 关卡级元素（棋盘底层 / 飞碟），渲染层按它们画传送带、传送门、地毯与飞碟
    , ("belts", arr [arr (map encodePos b) | b <- bvBelts bv])
    , ("portals", arr (map encodePair (bvPortals bv)))
    , ("ufos", arr [obj [("p", encodePos (ufoCell u)), ("c", int (colorNum (ufoColor u)))] | u <- bvUfos bv])
    , ("carpets", arr (map encodePos (bvCarpets bv)))
    , ("carpetOpen", arr (map encodePos (bvCarpetOpen bv)))
      -- 饼干掉落口格（新玩法 6，视图模型 bvDrops，与原桌面版 UI.BoardArt.drawDropsArt 同一份读数）；没有掉落口为 []
    , ("drops", arr (map encodePos (bvDrops bv)))
    , ("board", encodeBoard (bvBoard bv))
    ]
  where
    gv = gameView (histNow h)
    goal = gvGoal gv
    bv = gvBoard gv
    bs = gvBoosters gv

-- | 规则开关角标：{name: 规则开关名, text: 角标文字, icons: [图标贴图名，从下往上叠画]}。
encodeRuleBadge :: RuleBadge -> String
encodeRuleBadge rb =
  obj [("name", str (rbRule rb)), ("text", str (rbText rb)), ("icons", arr (map str (rbIcons rb)))]

-- | Boss 血条：{hp, max}（Match3.View.BossView）。
encodeBoss :: BossView -> String
encodeBoss bv = obj [("hp", int (bvHp bv)), ("max", int (bvMax bv))]

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

encodeGoal :: GoalInfo -> String
encodeGoal gi =
  obj $
    [("kind", str (giKind gi)), ("text", str (giText gi)), ("target", int (giTarget gi))]
      ++ [("name", str (unElementName n)) | Just n <- [giName gi]]   -- 按元素名计数的目标（jelly / bubble 等）
      -- 中文显示名（视图模型 Match3.View.goalLabel，唯一来源）：HUD「目标 …」直接画它，前端不自带映射表
      ++ [("label", str (goalLabel gi))]
      -- 目标图标贴图名（UI.GoalIcon.goalIcon，app/pure 里与原桌面版 HUD / 选关地图共用的一张表；如第 47 关 chameleon_icon）
      ++ [("icon", str (goalIcon (giGoal gi)))]

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
--   pairs 为 [[来源],[目标]]。EndEffect 是通用形状（事件类型 + 元素名 + 逐项 EndItem），
--   这里按事件类型编码成上面几种固定形状（网页 JS 与 web/test 的一致性快照依赖它）；其余事件类型编码为 {type:<eventKindTag>,kind:<元素名>,pairs}。
encodeEndEffect :: EndEffect -> String
encodeEndEffect eff = case endEffectKind eff of
  EvTick -> obj [("type", str "tick"), ("cells", arr (map (encodePos . eiTo) items))]
  EvBelt -> obj [("type", str "belt"), ("pairs", arr (map encodePair pairs))]
  EvSpread -> obj [("type", str "spread"), ("kind", str name), ("pairs", arr (map encodePair pairs))]
  EvMove -> obj [("type", str "snail"), ("moves", arr (map snail items))]
  k -> obj [("type", str (eventKindTag k)), ("kind", str name), ("pairs", arr (map encodePair pairs))]
  where
    items = endEffectItems eff
    pairs = endEffectPairs eff
    name = unElementName (endEffectElement eff)
    snail m =
      obj
        [ ("from", encodePos (eiFrom m))
        , ("to", encodePos (eiTo m))
        , ("dir", encodePos (maybe (0, 0) id (endItemDir m)))
        , ("pushed", maybe "null" encodeCell (eiBack m))
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
    , ("subject", str (unElementName (evElement e)))
    , ("pairs", arr (map encodePair (evCells e)))
    , ("amount", int (evAmount e))
    ]

-- ---------------------------------------------------------------------------
-- 盘面编码：Board 按 boardRows 展开成行列表（行优先，8×8），每格一个结构化对象：
--   宝石  {"t":"G","c":1..5,"k":"N|H|V|B|R","i":冰层,"o":覆盖物名或 null,"n":覆盖物层数}
--         覆盖物名：grass / vine / choco / fog / chain / freeze / curtain / steam（无层数的为 0）
--   其他  {"t":<元素>, ...}：stone/chest/honey/cake/safe {"n"}；balloon/bottle {"c"}；cookie / hat / surprise / spirit；
--         maker {"c","n"}；snail {"dr","dc"}；flip {"c":正面,"b":背面}；countdown {"c","n"}；custom {"name","v"}
--         元素自带的显示附加字段（Match3.View.cellExtras，元素 caps 的 displays）按顺序追加在后面，前端不自己拆 v：
--         雪怪 Boss（custom "snow_boss"）{"q":象限 0–3,"hurt":血量是否过半,"turn":召唤计数,"every":召唤周期}；
--         变色龙（custom "chameleon"，v = 颜色下标 0..4）{"c":当前颜色 1..5（同宝石的 "c"）}
--   每格另带 "s"（核心 show 文本，调试 / 未知元素占位用）。渲染层按 t 查表（www/cells.js，对应原桌面版 UI.CellTable）。

encodeBoard :: Board -> String
encodeBoard b = arr [arr (map encodeCell row) | row <- boardRows b]

-- | 单格：Match3.View.cellFace 的类型标签与字段，外加 "s"。
encodeCell :: Cell -> String
encodeCell cell = obj (("t", str tag) : map field fields ++ map extra (cellExtras cell) ++ [("s", str (show cell))])
  where
    extra (k, v) = (k, case v of
      FaceInt i -> int i
      FaceBool b -> bool b
      FaceColor c -> int (colorNum c))
    (tag, fields) = cellFace cell
    field (k, v) = (k, case v of
      FieldInt i -> int i
      FieldText x -> str x
      FieldNull -> "null")

encodePos :: Pos -> String
encodePos (r, c) = arr [int r, int c]

encodePair :: (Pos, Pos) -> String
encodePair (a, b) = arr [encodePos a, encodePos b]
