# 架构

## 分层

```
app/（可执行文件 match3-sdl，依赖 SDL2；图中箭头 = 依赖）
┌──────────────────────────────────────────────────────────────┐
│ Main（读环境变量）──→ Shell.Loop（通用外壳：窗口 / 固定步长主循环）│
│   └─ UI.Plugin（三消插件：把下面这些接到外壳的钩子上）           │
│        ├─ UI.Input ──→ UI.Actions ──→ UI.Playback              │  输入映射 → 动作 → 表现编排
│        ├─ UI.Draw ──→ UI.Cascade / UI.EndStage / UI.HudArt …   │  一帧绘制
│        │     └─ UI.BoardArt / UI.BoardPrim ──→ UI.CellTable     │  单格：元素名 → 贴图 / 几何两个后端
│        │                                   └─ UI.Cell.Art / UI.Cell.Prim
│        └─ UI.Env（环境变量 / 高分屏倍率）                        │
│ UI.Types / UI.Layout（状态类型、布局常量，被上面所有模块依赖）  │
│ ComboFx（纯阶段机 cascadeStages）   Art（贴图图集）              │
└───────────────┬──────────────────────────────┬───────────────┘
                │ 规则：Match3.Core / Match3.Engine │ 时钟：Engine.Playback
┌───────────────▼──────────────────────────────▼───────────────┐
│ Match3.Engine（三消 = 通用接口的第一个实现）  Match3.Core（再导出）│
└──────────────┬──────────────────────────────┬────────────────┘
               ▼                              ▼
   Match3.Game.*                        Match3.Board.*
     State  Tally                       Grid
     Outcome  Shuffle  Trace            Match   Gravity
     Resolve（公共结算）                 Clear   Random
     Level  Move  Boosters
               │                        Cascade
               └──── 直接 import 子模块 ───┤
                                            ▼
 Match3.Element（元素框架：Types / Registry / Builtin / Event；默认注册表 defaultRegistry）
 Obstacles Rainbow Combos Ice Grass Carpet Snail Ufo Countdown Conveyor Boosters Daily
 Match3.Types（类型 / 关卡表；Board = Array 盘面）
 （纯函数机制模块；无 IO）

 Engine.Game / Engine.Effect / Engine.Playback（通用层：不 import 任何 Match3 模块）
```

**依赖方向（硬约束）**

- 纯核心 / library ← 应用：`match3-sdl` 依赖 `match3` 库；库**不**依赖 SDL。`app/` 里的模块通过 `Match3.Core`（类型与查询）和 `Match3.Engine`（执行动作）使用规则。
- 通用层 ← 具体游戏：`Engine.*` 与 `app/Shell/Loop.hs` 不 import 任何 `Match3` 模块；`Match3.Engine` 实现通用接口，三消前端作为插件接入外壳（见[多游戏接口](#多游戏接口)）。测试 `engine_layer_is_game_agnostic` 检查这一方向。
- `Core` 把各子模块符号汇总导出，便于前端与测试只 import 一处。第三刀删掉了 `Match3.Board` / `Match3.Game` 两个外观模块：Game 子模块、`Core`、测试直接 import 各子模块。
- 子模块之间单向依赖、无环（下文 `A ← B` 表示 B 依赖 A）：Board 内 `Grid ← Match ← Clear`、`Grid ← Gravity`、`Match ← Random`，`Cascade` 依赖 Grid / Match / Clear / Gravity；Game 内 `State ← Outcome / Shuffle / Trace`、`Shuffle ← Level`，`Resolve`（公共结算）依赖 State / Tally / Outcome / Shuffle / Trace，`Move` / `Boosters` 只做校验与起手选择、依赖 `Resolve`。Game 子模块直接 import 所需的 Board 子模块。
- 元素框架 `Match3.Element.*` 位于 Board / Game 之下：`Element.Event ← Element.Types`、`Element.Message` → `Element.Class`（元素类）→ `Element.Registry` → `Element.Builtin.*`（按功能分组的 instance）→ `Element.Builtin`（汇总）；各分组里的 instance 调用各机制子模块实现具体反应。Board / Game 只通过注册表查询「这个格子怎么反应」，不再按构造器写死（见[元素框架与事件](#元素框架与事件)）。
- 机制子模块（障碍、彩虹、合成、冰、草系、地毯、蜗牛、飞碟、倒计时、传送带、道具种子、每日）尽量只依赖 `Types`（及彼此必要的窄依赖），由 `Board.*` / `Game.*` 编排调用顺序。
- 回放方向单向：`Match3.Game.Resolve.resolveMove`（经 `Match3.Board.Cascade` 的记录版连锁）与结算结果一起产出 `MoveTrace` → `app/ComboFx.hs`（纯阶段机，按时间线把它拆成帧）→ `UI.Playback`（阶段事件 → 弹字 / 粒子 / 震屏）→ `UI.Cascade` / `UI.EndStage`（绘制）。核心**不**知道帧、阶段或样式；`ComboFx` 不 import SDL，也不调用 `trySwap` 等规则入口，只读 `MoveTrace` 与前端传入的结算后盘面。

## 模块地图

### 核心库（`src/Match3/`）

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Match3.Types` | `Color` / `GemKind` / `CellOverlay` / `CellContents`（含 `Custom 名字 值`，供注册表扩展元素）、构造器与谓词、`LevelGoal` / `Outcome` / `allLevels`；`Board = Board (Array (Int,Int) Cell)`（O(1) 读格，`boardFromRows` / `boardRows` / `boardAt` / `boardSet` / `mapBoard` 等；`Show` 按行列表打印，与旧列表盘输出相同） | 连锁、交换、IO |
| `Match3.Element` | 门面：再导出 `Types` / `Registry` / `Builtin` / `Event`（元素类 `Class` 与 `Message` 单独 import，方法名 `name` / `color` / `pushable` 等较通用，避免与使用方撞名） | 自身无实现 |
| `Match3.Element.Types` | 规则与查询结果的数据类型：`Slot`、`HitResult`、`AdjacentRule` / `EndRule` / `SwapRule` / `OpenRule`、`Counter`、`Arg` / `Placement`、`cellSlot` | 调用顺序 |
| `Match3.Element.Registry` | `Registry`（名字 → 构造器 `Entry`；按层数组 O(1) 取解码器 + 自定义元素表 + 已排序的邻格 / 步末规则）、`register` / `lookupElement`、解码 `elementOf`、各能力的查询函数 `*With`、关卡级元素 `registerLevel` / `askLevel` | 具体元素 |
| `Match3.Element.Builtin` | 汇总：条目表 `builtinDefs`（注册顺序固定，快照锁定）、`builtinLevelDefs` 与 `defaultRegistry`；再导出测试 / 扩展用的元素类型 | 具体元素的定义 |
| `Match3.Element.Builtin.Gem` | 宝石：`PlainGem`、`SpecialGem`（直线 / 炸弹 / 彩虹），彩虹取色与特殊合成的成对交换规则、`specialBlast` | 障碍与叠层 |
| `Match3.Element.Builtin.Layer` | 冰层 `Ice` 与 8 种叠层修饰器（草 / 藤 / 巧 / 迷雾 / 锁链 / 火箭冰冻 / 窗帘 / 蒸汽），蔓延规则 | 本体 |
| `Match3.Element.Builtin.Obstacle` | 打破型障碍：石头、宝箱、蜂蜜、蛋糕、气球、保险箱、双面块、彩蛋 | 收走 / 按名字计数的元素（`Collectible`） |
| `Match3.Element.Builtin.Collectible` | 收集与计数类：饼干、时间精灵、气泡 | 削层 / 变形（`Obstacle`） |
| `Match3.Element.Builtin.Actor` | 会动或会生成东西的：魔法帽、果汁机、蜗牛（含 `traceSnails`）、染色瓶、倒计时 | 被动障碍（`Obstacle`） |
| `Match3.Element.Builtin.Ground` | 地面层：果冻 | 占格本体 |
| `Match3.Element.Builtin.Level` | 关卡级元素：飞碟、皮带、传送门、地毯（`LevelElement`，按节拍消息回复） | 关卡状态的存储（仍在 `GameState`） |
| `Match3.Element.Builtin.Common` | 跨分组共用的辅助：`deadRule`（邻消打碎并入清除格）、`colorPlace`（按颜色放置） | 只在一组里用的辅助 |
| `Match3.Element.Class` | 元素类（xmonad LayoutClass 风格）：`Element`（带默认实现的能力方法，默认由 `archetype` 推出）、`SomeElement`、修饰器 `Modifier` / `Modified`、惰性占格 `Inert`、关卡级元素 `LevelElement` / `SomeLevel` | 具体元素 |
| `Match3.Element.Message` | 开放消息 `Message` / `SomeMessage` / `fromMessage`；流水线节拍消息及回复（`Refilled` → `Absorbed`、`EndTicked` → `Shifted`、`Settling` → `Settled`、`Covering` → `Covered`） | 谁回复 |
| `Match3.Element.Event` | `EndEffect` / `SpreadKind` / `SnailMove`、`applyEndEffect`、效果事件 `EventKind` / `Event` | 帧与样式 |
| `Match3.Core` | 再导出公共 API | 自身几乎无逻辑 |
| `Match3.Board.Grid` | 坐标边界、读写格（`getCell` = `boardAt`，O(1)）、交换、相邻、可空盘面 `MBoard`（仍是行列表，只在一轮消除 / 沉降内部使用）、`randomColor` | 任何规则 |
| `Match3.Board.Match` | `MatchRun` / `findMatchRuns` / `hasAnyMatch`、`findHint` / `hasValidMove` | 修改盘面 |
| `Match3.Board.Clear` | 一轮消除（匹配 / 种子）、特殊扩展与生成、彩蛋、邻格削层与触发、飞碟吸收（吸走 ≠ 引爆）、计分公式 | 沉降、连锁循环 |
| `Match3.Board.Gravity` | 重力（固定格分段）、边缘收集（`drainEdgesMWith`：按元素的 `drains` 方向，底 → 左 → 右 → 上，收完再落，内置只有饼干 = 底边）、传送门沉降、补子、`settleRefill` / `settleDrainWith` | 消除 |
| `Match3.Board.Cascade` | 连锁的**单一实现**：`cascadeMatches` / `cascadeMatchesFrom` / `cascadeSeeds` / `cascadeAfterBelt` / `cascadeCountdowns` 返回 `CascadeRun`（终盘 + `CascadeTally` 计数记录 + `[CascadeWave]` + 飞碟 + 生成器），调用方直接读字段（第三刀删掉了元组兼容层 `runCascade*` / `resolveCountdowns` / `runPostBeltCascade` 与 `traceCascade*`）；`stepCascade` 保留为「恰好一轮」的小工具；段 2c 起另有 `cascadeAfterEndWith`（步末补结算：挖 `erHoles` 空洞 → 边缘收集 + 补子 → 再连锁） | 步数 / 目标结算、道具扣次 |
| `Match3.Board.Default` | 段 2c：不带 `With` 的旧名（`cascadeMatches` / `clearMatches` / `applyGravity` / `findHint` …）= `*With defaultRegistry`。`Board.{Match,Clear,Gravity,Cascade}` 自身不再 import `Element.Builtin`，只收 `Registry` 参数；主流程一律把 `reg` 往下传，不经本模块 | 规则 |
| `Match3.Board.Random` | 随机盘、稳定盘、可玩盘、`shufflePlayable` | 保留装饰（见 `Game.Shuffle`） |
| `Match3.Game.State` | `GameState`（段 3 起不含撤销历史）、`MoveFx` / `moveFx` / `clearMoveFx`（边沿触发）、`applyHint` / `applyHintWith` | 结算、撤销历史（在 `Engine.History`） |
| `Match3.Game.Tally` | 结算计数辅助：颜色袋、保险箱 / 时间精灵计数、地毯腾空格 | 结局判定 |
| `Match3.Game.Outcome` | 目标满足、`decideOutcome`、`checkOutcome`、选关解锁、地图跳转、失败提示 | 盘面 |
| `Match3.Game.Shuffle` | 保装饰洗牌 `shuffleGame`、自动洗牌 `ensurePlayable` | 回放（洗牌不在 `mtEnd`） |
| `Match3.Game.Level` | 开局 / 每日 / 重开 / 下一关、战役装饰 `decorateLevel`、皮带 / 传送门 / 飞碟布局、步数携带 | 走步 |
| `Match3.Game.Trace` | `MoveTrace`（含 `mtGen` / `mtShuffle`）/ `EndStep`（`EndEffect` 等再导出自 `Element.Event`）、`traceSpreads`（跑注册表的蔓延规则）、`beltMoves`（再导出自 `Conveyor`）、效果事件 `traceEvents` | 结算 |
| `Match3.Game.Resolve` | 交换与三种道具的**公共结算** `resolveMove`：主连锁 → 步末（交换：倒计时 / 皮带 / 蔓延 / 蜗牛 / 再连锁；道具：蔓延）→ 计数与目标 → 结局 → 自动洗牌，同时产出 `MoveTrace` | 入口校验（在 `Move` / `Boosters`） |
| `Match3.Game.Move` | `resolveSwap`（校验 + 起手选择）及其投影 `trySwap`（= `runMove`）/ `traceSwap` | 道具 |
| `Match3.Game.Boosters` | `resolveHammer` / `resolveFreeSwap` / `resolveCrossClear` 及其投影 `use*` / `trace*` | 种子几何（见 `Match3.Boosters`） |
| `Match3.Obstacles` | 石头/宝箱/蜂蜜/蛋糕/保险箱/气球/彩蛋/瓶子/精灵/魔法帽/果汁机的邻消削层与触发 | 连锁循环 |
| `Match3.Rainbow` | 彩虹判定与清色种子 | 合成几何（见 Combos） |
| `Match3.Combos` | 特殊×特殊合成种子 | 普通三消 |
| `Match3.Ice` | 匹配时削冰层 | overlay（Freeze/Chain…） |
| `Match3.Grass` | 草/藤/巧/雾/链/冻/帘/蒸汽的清除与蔓延 | 蜗牛爬行 |
| `Match3.Carpet` | 地毯覆盖计数、关卡地毯布局 | 饼干底行收集逻辑（在 `Board.Gravity` / `Game.Tally`） |
| `Match3.Snail` | 蜗牛一步爬行 / 掉头 | 步末其它效果编排 |
| `Match3.Ufo` | 飞碟吸色目标与移格 | 棋盘清除（由 `Board.Clear` 掩码后清） |
| `Match3.Countdown` | 倒计时 tick / 归零爆炸种子 | 爆炸后连锁（`Board.Cascade`） |
| `Match3.Conveyor` | 传送带移位的**单一实现**：`beltMoves`（「原格 → 新格」描述）→ `applyBeltMoves`（按描述移位）；`shiftBelts = applyBeltMoves b (beltMoves belts)`，结算、回放描述与 `applyEndEffect` 重放共用 | 移位后再连锁（`Game.Resolve`） |
| `Match3.Boosters` | 锤子/十字**种子位置**（纯几何） | 扣次数与连锁（`Game.Boosters`） |
| `Match3.Daily` | 日期种子、每日配置、三星公式 | 每日盘面装饰（`Game.Level`） |
| `Match3.Engine` | 三消作为通用接口的实现：`Action`（交换 / 锤子 / 自由交换 / 十字 / 提示 / 洗牌）、`Setup`、`play`（一次结算得到 `Played`：状态 / `Outcome` / `MoveTrace` / `MoveFx` / 事件 / 提示，作为 `gameStep` 的 `stepReport`）、`match3Game`、撤销规则 `match3History`、外壳实例 `match3Shell = withHistory match3History match3Game`、`toEffect` | 帧与绘制、撤销历史的存放 |

### 通用层（`src/Engine/`）

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Engine.Game` | 通用游戏接口 `Game cfg s a e o r`（record-of-functions；`r` 是整步报告）、`Step`（含 `stepReport :: Maybe r`）、`runActions` / `finalState` / `stepEffects` / `rejectedStep`、种子约定 | 任何具体规则 |
| `Engine.History` | 段 3：通用撤销历史 `History{histNow, histPast}`、`Undoable a = Act a \| Undo`、`HistoryPolicy{hpLimit, hpRecord, hpSnapshot, hpRestore}`、`withHistory`（给任意 `Game` 套一层撤销；终局后仍可撤销）、`pushHistory` / `replaceNow` / `undoHistory` / `commitStep` | 哪些动作算走步（由游戏的 policy 给出） |
| `Engine.Effect` | 通用效果事件 `Effect{efBeat, efKind, efSubject, efSpots, efAmount}`、按节拍分组 `beats` | 帧数与样式 |
| `Engine.Playback` | 纯播放层：阶段机 `Stages`、播放器 `Player`（帧号 / 加速）、`stepPlayer` / `playerProgress` / `runPlayer`；固定队列 `Cue` / `cueStages` / `effectCues` | 阶段内容（由游戏给出）、SDL |

### 前端模块（`app/`）

| 模块 | 职责 |
|------|------|
| `Main` | 入口：读环境变量（种子 / 起始关 / 展示盘 / 窗口倍数），`runShell (match3ShellConfig o) (match3Plugin o)` |
| `Shell.Loop` | 通用 SDL 外壳（不 import Match3）：初始化、HiDPI 窗口、渲染器、alpha 混合、固定步长主循环（帧首钩子 → 取事件 → 事件钩子 → 推进 → 绘制 → present → 补足 16 ms）、`Plugin` 钩子 |
| `UI.Plugin` | 三消插件：初始 `App`（开局提示 / 展示盘）、加载贴图、各钩子接到 `syncScale` / `foldEvents` / `tickAnim` / `draw` |
| `UI.Types` | `App`、`Anim`（`AnimCascade` 持有 `Player Cascade`）、`Particle`、`ToolMode`，帧数常量，`animBusy` / `playingPlayer` / `playingCascade` |
| `UI.Layout` | 逻辑像素布局常量、格子坐标换算、矩形 / 插值工具、调色板 |
| `UI.Env` | 环境变量（`MATCH3_LEVEL` / `SEED` / `SCALE` / `SHOWCASE`）、展示盘、高分屏倍率与鼠标坐标换算 |
| `UI.Input` | 输入映射：`handleEvent` 分派到 `handleKey`（每键一个函数）/ `handleMouseUp`（拖拽交换）/ `handleMouseDown`（地图 / 加速 / 结束浮层 / 点格）；规则一律经通用接口 `gameStep`（`UI.Actions.stepShell`，实例 `Match3.Engine.match3Shell`；撤销是 `Undo`，由 `Engine.History` 处理）；播放锁定（见 [ui-controls.md](ui-controls.md#播放锁定animbusy)） |
| `UI.Actions` | 标题栏（连击数经 `gameStatus` 取）、`stepShell`（外壳执行动作的唯一入口：`gameStep M3E.match3Shell`）、`playMove` / `playbackOf`（一次 `gameStep` 的整步报告 → 表现编排）、关卡重置、三种道具执行、回放加速、过关前进 / 重试 |
| `UI.Playback` | 纯函数：每帧推进动画；按本次 `MoveFx` / `MoveTrace` 编排回放，阶段事件产生弹字 / 浮字 / 震屏 / 粒子 |
| `UI.Draw` | 一帧的层次与贴图 / 几何分派 |
| `UI.Cascade` | 静止盘、交换补间、轻落、逐轮回放（高亮 / 消失 / 下落）、震屏视口 |
| `UI.EndStage` | 步末阶段绘制：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌 |
| `UI.BoardArt` | 棋盘贴图绘制与分派（`drawCellAny` / `drawCellArt` / `drawStatic` 等）、回放共用的底盘部件 |
| `UI.BoardPrim` | 棋盘几何降级绘制（`drawGemAt` 查表分派、底盘 / 传送门 / 飞碟 / 皮带 / 蔓延预告 / 粒子） |
| `UI.CellTable` | 单格绘制的元素查表：元素名（注册表）→ `CellRenderer{crPrim, crArt, crSprite}`；宝石 5 个名字共用一个渲染器；`Custom` 先查按名字的 `customTable`（段 5：气泡），查不到走自定义渲染器 |
| `UI.Ground` | 段 5：地面层（`gsGround`）的绘制查表：名字 → 几何版 / 贴图名(层数)；贴图版画在棋子之下，几何版画在棋子之上（框） |
| `UI.Cell.Prim` / `UI.Cell.Art` | 每种元素一个几何 / 贴图渲染函数（从原 `drawGemAt` / `drawCellArt` 的大 case 逐字拆出）；`Cell.Art` 另含 `colorKey` / `gemSprite` / `breathe` / 角标 |
| `UI.HudArt` / `UI.HudPrim` | HUD、横幅、键位条、暂停帮助、结算面板、弹字的贴图版 / 几何降级版 |
| `UI.TextArt` / `UI.Glyph` | 烘焙文字 / 中文标签贴图的排版；缺字形时的像素字 |
| `UI.LevelMap` | 选关地图：章节、节点坐标、点击命中、两种绘制 |
| `ComboFx` | 连锁逐轮回放的纯逻辑（步末阶段种类查 `endStageTable`，按事件种类分派）：阶段机 `cascadeStages`（高亮→消失→下落→落定，以及步末阶段：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌；帧号与加速交给 `Engine.Playback.Player`）、波次视图 `WaveView`（快照 + 本轮效果事件）、时间线常量、连击等级样式、下落映射、浮字曲线；只消费 `MoveTrace` 与效果事件，不绘制 |
| `Art` | 贴图图集（BMP + 索引）加载、路径查找、九宫格面板、染色/加色绘制；缺资源时各绘制模块退回几何版 |

前端依赖同样单向无环：`UI.Types` / `UI.Layout` 在最底层；`UI.Glyph ← UI.TextArt ← UI.HudArt ← UI.LevelMap`，`UI.Cell.Prim / UI.Cell.Art ← UI.CellTable ← UI.BoardPrim ← UI.BoardArt ← UI.EndStage ← UI.Cascade ← UI.Draw`，`UI.Playback ← UI.Actions ← UI.Input ← UI.Plugin ← Main`，`Shell.Loop ← UI.Plugin`（`A ← B` 表示 B 依赖 A）。

## 构建工具链

| 项 | 值 |
|----|-----|
| 构建 | Stack（`package.yaml` → hpack → `match3.cabal`） |
| Resolver | **lts-21.25** |
| GHC | **9.4.8**（`stack.yaml`：`system-ghc: true`） |
| 库名 | `match3` |
| 可执行文件 | `match3-sdl` |
| 测试套件 | `match3-test`（入口 `test/Spec.hs` 汇总 `test/Spec/*.hs` 各功能模块，tasty + HUnit + QuickCheck） |

库依赖：`base`、`array`、`random`。可执行文件额外：`sdl2`、`text`，以及 GHC 自带的 `containers`、`directory`、`filepath`（贴图加载）。贴图由 `tools/gen_assets.py` 生成到 `assets/`，详见 [ui-art.md](ui-art.md)。

## 网页版（技术验证）

分支 `web-wasm-spike` 上的 `web/` 目录用 GHC wasm 后端把核心（`Engine.*` / `Match3.*`）和 `ComboFx` 编成 wasm，
接口层 `web/hs/Match3Web/Api.hs` 与桌面外壳一样只调 `gameStep match3Shell`，JS 只负责绘制与输入，核心源码不改。
元素框架（`Match3.Element.Class` 的 `Element` / `Modifier` / `LevelElement` 与 `SomeElement` 等存在类型、`Match3.Element.Message`、
`Match3.Element.Builtin.*` 分文件）整体编进 wasm；网页接口层不直接调用元素类，盘面按 `Cell` 构造器编码成 JSON，
所以元素迁移不改变网页端的 JSON 与贴图映射。核心新增模块时要同步到 `web/match3-web.cabal`（`web/build.sh` 会核对）。
结构、导出接口、渲染器、自适应布局、资源管线、构建、部署与测试见 [web.md](web.md)。

## 状态边界

- **规则已结算**：`trySwap` / `useHammer` / `useFreeSwap` / `useCrossClear` 返回的 `GameState` 已是稳定盘（或终局），前端只做展示与补间。
- **随机**：`StdGen` 存在 `gsGen`；洗牌 / 补子推进生成器，测试用固定种子。
- **历史**：段 3 起撤销历史只有一份，放在通用层 `Engine.History.History GameState`（前端 `App.appHist`，当前局面 `appGame = histNow . appHist`），`GameState` 不再带 `gsHistory`。规则 `match3History`：交换 / 三种道具被接受时记走步前的快照（去掉提示与洗牌标记），最多 20 份；提示 / 洗牌 / 被拒动作不记；撤销回到快照并清掉本步特效字段与终局标记——终局后同样可以撤销（`Undo` 先于三消的终局拒绝处理）。这是原 `snapshot` / `undoMove` 的逐字搬迁（测试 `engine_undo_after_terminal_matches_legacy_play` 与 `13094d1` 上直接调 `play` 的结果逐位比对；金标准 `hist=` / `undo=` 两列改从 `History` 取数，2344 行全等）；UI 粒子用 `gsLastCleared`，不参与规则；前端经 `moveFx` 边沿触发特效，失败操作不会重播上一步连击。
- **回放脚本**：`MoveTrace { mtStart, mtWaves :: [CascadeWave], mtFinal, mtEnd :: [EndStep], mtGen, mtShuffle }` 是纯数据，与结算结果由同一次 `resolveMove` 计算产出，不写回 `GameState`。`mtFinal` / `mtGen` 是 `ensurePlayable` 之前的稳定盘与生成器：没有自动洗牌时等于结算后的 `gsBoard` / `gsGen`；发生洗牌时 `mtShuffle = Just 洗牌后盘面`，从 `(mtFinal, mtGen)` 重放 `ensurePlayable` 可逐帧复现（`ComboFx` 在最后追加 `StShuffle` 阶段；洗牌**不在** `mtEnd` 里）。前端只调通用接口 `gameStep`（`match3Shell`），整步报告 `stepReport = Just Played` 里一次拿到结算结果、脚本、`MoveFx` 与效果事件（与分别调 `trySwap` / `use*` 和 `trace*` 逐位相同，测试 `engine_match3_instance_matches_direct_api`；`app/` 不再直接调 `play`，测试 `engine_frontend_steps_only_via_gameStep`），`MoveFx` 为空时直接丢弃脚本。

## 逐轮回放与规则的同步

第二刀起，结算与回放是**同一份实现**的两个投影，不再需要人工同步：

| 层 | 单一实现 | 结算投影 | 回放投影 | 护栏测试（现在天然成立，保留作回归） |
|----|----------|----------|----------|----------|
| 连锁 | `Match3.Board.Cascade` 的 `cascadeMatchesFrom` / `cascadeSeeds` / `cascadeAfterBelt` / `cascadeCountdowns`（`CascadeRun`：每轮一个 `CascadeWave`，飞碟吸收单独一轮） | `crTally`（`CascadeTally` 记录） | `crWaves` | `trace_cascade_final_equals_stabilized`、`trace_seeds_final_equals_stabilized`、`trace_multi_wave_each_round_visible` |
| 一步操作 | `Match3.Game.Resolve.resolveMove`（交换与三种道具共用；`Move.resolveSwap` / `Boosters.resolve*` 只做校验与起手选择） | `trySwap` / `use*` = 取 `(GameState, Outcome)` | `trace*` = 取 `MoveTrace` | `trace_swap_final_equals_trySwap`、`trace_boosters_final_equal_result`、`trace_rejected_move_is_empty`、`trace_shuffle_step_replays` |
| 步末描述 | 结算直接使用 `Match3.Game.Trace` 的 `traceSpreads` / `traceSnails` 返回的盘面；`applyEndEffect` 把 `EndEffect` 重放回盘面 | — | `mtEnd` | `trace_end_steps_replay_to_trySwap_final`、`trace_end_steps_boosters_replay`、`trace_end_snail_push_and_turn`、`trace_end_spread_from_adjacent_source` |

皮带：第三刀起 `Conveyor.beltMoves` 是唯一实现，结算（`applyBeltMoves`）、回放描述（`EndBeltShift`）与重放（`applyEndEffect`）共用同一份「原格 → 新格」表。行为金标准见 [testing.md](testing.md#行为金标准golden)；护栏细节与底线见 [testing.md](testing.md#逐轮回放护栏)；前端时间线见 [ui-art.md](ui-art.md#连击表现逐轮回放)。

## 元素框架与事件

第二刀 2b 起，「某个格子在某个时机怎么反应」不再散落在 Board / Game 各处的 `case` 里，而是由**元素**描述、**注册表**（`Registry`）分派（段 2b–5 是扁平记录 `ElementDef`，元素类迁移后是类型类 `Element` 的 instance，见下文「元素类」两节）。旧的函数名保留为 `defaultRegistry` 下的包装；新增的 `*With reg` 版本接受任意注册表（测试里用它接入样例元素）。

### 一个格子的层

一格自上而下是：叠层（`CellOverlay`，草 / 藤 / 巧 / 雾 / 链 / 冻 / 帘 / 蒸汽）→ 冰层（`iceLayers`）→ 本体（宝石各种类、石头、宝箱……、`Custom 名字 值`）→ 地面层（段 2c，`GameState.gsGround :: [(Pos,(名字, 层数))]`，不在 `Cell` 里、不占格、不随重力 / 洗牌移动；内置关卡恒为空）。`Slot` 标出一个定义占哪一层：`SlotCell i` / `SlotOverlay i`（内置，数组下标）、`SlotIce`、`SlotCustom`（按 `Custom` 的名字查表）、`SlotGround`（按 `gsGround` 里的名字查表）。查询时按层合成：挡交换 = 任一层挡；点火 = 自上而下第一个 `Just`；直接命中 = 最上面一个非穿透层先吸收。

### 元素类的方法与调用时机

一种元素 = 一个类型 + 一个 `Element` instance（`Match3.Element.Class`）。只有 `name` / `toCell` 必须写，其余方法都有默认实现，默认值由 `archetype` 推出：`Piece`（普通棋子：可交换、能点火、会下落、可过传送门、命中即消、可改色 / 推动、洗牌时参与重排）、`Blocker`（占格障碍：挡交换、不点火、会下落、打不动、洗牌保留）、`Fixed`（同障碍，但不随重力下落）。颜色缺省取 `toCell` 写回的宝石格。元素自己的状态放在值里（石头 `StoneE 层数`、保险箱 `SafeE 层数`……），受击返回新值。

| 方法 | 含义 | 调用点（经注册表的 `*With` 查询） |
|------|------|--------|
| `name` / `toCell` | 元素名（注册表的键、贴图名、计数名、事件里的 `evElement`）/ 把值写回格子（盘面的存储编码） | 注册、分派、放置 |
| `archetype` | 原型，决定下面各方法的默认值 | — |
| `color` / `matchColor` | 本体颜色（颜色袋计数）/ 参与匹配的颜色（修饰器可挡住，`modBlocksMatch`） | `Board.Match.groupGemRuns`、`Clear.countColor`、提示 |
| `blocksSwap` | 本格不能被交换（任一层挡即挡） | `Move` / `Boosters` 校验、`findHint` |
| `swapRule` | 成对交换规则 `SwapRule{srOrder, srFires 交换前盘, srSeeds 交换后盘}`：交换两端的组合直接给出起手种子；多条按 `srOrder` 取第一条成立的。内置：彩虹取色（rainbow，10）、特殊 × 特殊合成（挂在 line_h 上，20） | `Registry.swapOpeningWith`、`Board.Match.findHintWith` |
| `hintable` | 普通匹配提示是否试这个格（缺省 True；彩虹 = False，只经成对交换规则给提示） | `Board.Match.findHintWith`（取代原先写死的 `isRainbow`） |
| `activates` | 特殊块能否点火（自上而下第一个 `Just` 决定，软锁纪律） | `Clear.expandSpecials` |
| `falls` / `portal` / `drains` | 随重力下落（否则把列分段）/ 可穿传送门 / 到达哪些边时被收走（`[Edge]`；内置饼干 = `[EdgeBottom]`） | `Board.Gravity.drainEdgesMWith` |
| `onHit` | 直接命中（锤子、爆炸、十字）：`Absorb 新元素` 吸收 / `Destroy` 打碎 / `Immune` 免疫；修饰器用 `modOnHit`：`Pierce` / `Keep m` / `Remove` / `Shatter` | `Clear.clearWaveWith`、`Ice.chipIceOnClear`、`hammerImmune` |
| `adjacentRule` | 邻格真消除时的反应（`AdjacentRule 顺序 规则`，规则拿到 `AdjCtx{真消除格, 直接命中格, 保护格, 可改色谓词 acRecolor}`，返回 `AdjOut{新盘, 打碎格, 生成格}`） | `Clear.clearWaveWith`（按 `arOrder` 依次跑） |
| `openRule` | 开启规则 `OpenRule{orOpen}`：一轮内可多次开启，开出的格本轮坐住。内置：彩蛋 | `Clear.surpriseClearPassWith`（经 `Registry.openWith`） |
| `recolorable` / `pushable` | 可被魔法帽 / 染色瓶改色、可被蜗牛推动（内置 = 宝石各种类 + 倒计时 + 双面块） | `AdjCtx.acRecolor`、`EndCtx.ecPushable` |
| `stripOnClear` | 本格真消除时上层随格清掉（修饰器 `modStripOnClear`） | `Clear` |
| `counter` / `diffCounter` / `bonusMoves` | 进入清除格计数 / 按步前步后个数差计数 / 每少一个奖励步数 | `Board.Cascade`（`ctNamed`）、`Game.Resolve`、`Game.Tally`（`diffCountersWith`） |
| `vacatesCarpet` | 离开格子也算覆盖地毯 | `Game.Tally.carpetVacateSeedsWith` |
| `keepOnShuffle` | 洗牌时原样放回 | `Game.Shuffle.extractDecorWith` / `ensurePlayableWith` |
| `blast` | 被消除且能点火时的爆炸范围 | `Clear.expandSpecials` |
| `groundRule` | 地面层（`SlotGround`）：上方格子每被消除 / 收走一次，层数 → 新层数（`Nothing` = 清掉）；每去掉一层按 `counter` 计 1 | `Registry.hitGroundWith`（`Game.Resolve` 逐轮调用） |
| `endRule` | 步末规则 `EndRule{erPhase, erOrder, erRun, erHoles}`：`PhaseTick`（倒计时）→ 皮带 → `PhaseSpread`（蔓延）→ `PhaseMove`（蜗牛）→ 再连锁；`erHoles` 在全部步末阶段之后给出要挖空的格，由 `cascadeAfterEndWith` 补结算 | `Game.Resolve.runPhase`、`Cascade.cascadeCountdownsWith`、`Trace.traceSpreadsWith` |
| `handleMessage` | 开放消息：`Nothing` = 不关心，`Just` = 新的元素值（可以换成别的元素） | `sendMessage`（内置格内元素目前都不收消息；护栏见 `ec_open_messages`） |

规则类方法（`adjacentRule` / `endRule` / `swapRule` / `openRule` / `groundRule`）描述「这类元素」在一轮里怎么作用于盘面，注册表在注册时从原型值上取一次；其余方法逐格调用：先把格子解码成元素值（`elementOf`），再调方法。

**修饰器**（`Modifier`，对应 xmonad 的 `LayoutModifier`）：冰层（`Ice 层数`）和八种叠层（`GrassL` / `VineL` / `ChocoL` / `FogL` / `ChainL` / `FreezeL` / `CurtainL` / `SteamL`）。`Modified` 把修饰器和里面的元素合成一个元素：挡匹配 / 挡交换任一层即挡；点火与命中自上而下第一个有意见的层决定；名字 / 颜色 / 计数 / 下落等取本体；有上层就洗牌保留。一格解码为「冰 → 叠层 → 本体」的嵌套 `Modified`。

**注册表条目**（`Registry.Entry`，名字 → 构造器）：`bodyEntry 槽位 原型 解码 放置`（内置本体）、`customEntry 原型 (Int → 元素)`（`Custom 名字 k`）、`modifierEntry 槽位 原型 解码 放置`（冰 / 叠层）、`groundEntry 原型`（地面层）、`inertEntry 名字`（`Inert`：挡交换、无色、会下落、打不动、洗牌保留的惰性占格，即旧 `baseDef` 的等价物；未注册的 `Custom` 名字也按它处理）。放置函数 `Placer = [Arg] -> Cell -> Maybe Cell` 由关卡放置表 `Place 名字 参数 坐标` 调用（`Game.Level.decorateLevelWith`）。

**关卡级元素**（`LevelElement`：`levelName`、`levelReply :: l -> SomeMessage -> Maybe SomeMessage`）：飞碟 / 皮带 / 传送门 / 地毯。主流程在固定的流水线节拍上发消息，关卡级元素自己决定回复哪条（`Registry.askLevel`，取第一个类型对得上的回复）：

| 节拍消息 → 回复 | 时机 | 内置回复者 | 主流程入口 |
|------|------|------|------|
| `Refilled 飞碟 盘` → `Absorbed 吸走格 新飞碟` | 每轮补子之后 | `UfoLevel` | `Registry.absorbWith`（`Cascade`） |
| `EndTicked 皮带` → `Shifted 移位表` | 玩家交换的步末，倒计时之后、蔓延之前 | `BeltLevel` | `Registry.beltShiftWith`（`Resolve`） |
| `Settling 可穿门谓词 门对 可空盘` → `Settled 可空盘` | 沉降时 | `PortalLevel` | `Registry.teleportWith`（`Gravity`） |
| `Covering 未覆盖格 本步格` → `Covered 剩余 新覆盖数` | 步末结算 | `CarpetLevel` | `Registry.coverWith`（`Resolve`） |

没有元素回复 = 该机制不生效（`removeLevel`；测试 `br_level_hooks_*` 在 38 关实测）。消息是开放的：任何模块都能定义新的消息类型和新的关卡级元素（`ec_level_elements_by_message` 用测试专用的「磁铁」演示）。

### 邻格规则顺序

`Clear.clearWaveWith` 每轮在真消除之后按 `arOrder` 从小到大跑邻格规则（与第二刀之前写死的顺序一致，改顺序会改变行为，金标准会报）：

| 顺序 | 元素 | 顺序 | 元素 |
|------|------|------|------|
| 10 | stone（削层） | 90 | freeze |
| 20 | chest | 100 | curtain |
| 30 | honey | 110 | safe |
| 40 | cake | 120 | time_spirit |
| 50 | balloon | 130 | maker（产出生成格 sit） |
| 60 | magic_hat | 140 | bottle |
| 70 | fog | 150 | choco |
| 80 | chain | 160 | steam |
|  |  | 170 | bubble（段 5：邻格真消除即破） |

步末规则：countdown（`PhaseTick`）；vine 10 / choco 20 / steam 30（`PhaseSpread`）；snail（`PhaseMove`）。

### 效果事件

`Match3.Game.Trace.traceEvents :: MoveTrace -> [Event]`（`traceEventsWith reg`）把回放脚本按时间线展开成事件，`Event{evKind, evWave, evElement, evCells, evAmount}`：

| `EventKind` | 来源 | `evElement` | `evCells` / `evAmount` |
|-------------|------|-------------|------------------------|
| `EvBlast` | 每轮被点火的直线 / 炸弹 | line_h / line_v / bomb | (特殊块, 覆盖格) |
| `EvClear` | 每轮清除格，按 `cwCleared` 的顺序把**连续同名**（本体名）的格分为一组，按事件顺序拼接即 `cwCleared` | gem / stone / cookie / … / 自定义名 | (格, 格) |
| `EvHit` | 本轮内容变了但没消掉的格，按最上层名分组 | ice / chain / stone / … | (格, 格) |
| `EvDrain` | 底行收走 | cookie | (格, 格) |
| `EvScore` | 每轮得分 | — | `evAmount` = 分 |
| `EvCombo` | 第 2 轮起每轮一个 | — | `evAmount` = 波次 |
| `EvTick` / `EvBelt` / `EvSpread` / `EvMove` | `mtEnd` 的每个 `EndStep` | countdown / belt / vine·choco·steam / snail | 原格 → 新格 |
| `EvShuffle` | `mtShuffle` | — | — |

前端步末阶段按事件种类查表分派：`ComboFx.endStageTable`（种类 → 阶段与基础时长）、`UI.Playback.endCrumbTable`（阶段 → 粒子）、`UI.EndStage.endStageDrawers`（阶段 → 绘制）、`UI.Layout.elementRGBTable`（元素名 → 颜色）。波次级的高亮 / 消失 / 粒子 / 得分浮字读 `ComboFx.WaveView` 里本轮的效果事件（`wvCleared` = EvClear 格、`wvScore` = EvScore 之和）；底图快照（消除前 / 挖洞 / 落定盘面）与下落映射仍取自 `CascadeWave`（事件是差量描述，不含整盘快照）。护栏：`trace_events_consistent_with_trace`（含逐轮严格相等：EvClear 格序 = `cwCleared`、EvScore 和 = `cwScore`）。

### 扩展钩子（段 2c）

段 2c 把「新增元素只经注册表接入」补齐到下列类别，主流程（`Game.Resolve` / `Board.*`）不再需要为新元素改代码。`Engine.*` 在 2c 中**没有改动**。

| 钩子 | 类型 / 入口 | 用途 | 护栏测试（`test/Spec/Extension.hs`，样例元素只在测试里） |
|------|-------------|------|------|
| 注册表下传到底 | `Board.*With reg`；旧名在 `Board.Default` | Board 层不依赖内置表 | `ext_board_modules_take_registry`（源码扫描） |
| 按名字的目标 | `LevelGoal` 新增 `GoalNamed 名字 N`（读 `gsElementCounts`，由 `counter` / `diffCounter = CountNamed 名字` 累加）；HUD / 标题 / 失败提示 / 选关已接 | 自定义元素当关卡目标 | `ext_goal_named_counts_crate` |
| 地面层 | `SlotGround` + `groundRule` + `gsGround`（`Level.levelGround`） | 果冻类「格子下面的层」 | `ext_ground_layer_test_element` |
| 边缘收集 | `drains :: e -> [Edge]` | 任意方向的收集物 | `ext_edge_drain_side_collectible` |
| 步末补结算 | `EndRule.erHoles` + `Cascade.cascadeAfterEndWith` | 步末阶段挖掉格子后的沉降 / 补子 / 再连锁 | `ext_post_end_settle_hole_element` |
| 洗牌走注册表 | `Game.Shuffle.shuffleGameWith reg`、`Game.State.applyHintWith reg`（`Engine.playWith` 的 Shuffle / Hint 分支） | 自定义元素的洗牌保留 / 提示 | `ext_manual_shuffle_keeps_crate_via_engine` |

**步末补结算选统一路径（无开关）**：交换与道具的步末之后一律经 `cascadeAfterEndWith`：先挖 `erHoles` 的空洞，再做边缘收集 + 补子；盘面有变化或有格被收走时记一个只含沉降的轮次，之后成消再接普通连锁；什么都没发生时原样返回、**不消耗随机数、不加轮次**。对内置元素它恒为空操作，依据：① 类型层面——`Board` 不能表示空洞，内置 `erHoles` 全是 `const []`；步末阶段（倒计时 / 皮带 / 蔓延 / 蜗牛）不移动饼干（蜗牛把饼干当障碍），皮带后的再连锁本身已含沉降；`refill` 在没有空洞时不取随机数。② 实测——38 关 × 种子 1..100 × 15 步，每步检查主交换、三种道具与全部可成交的交换对，共 **610,751** 手，步末终盘上待挖空洞 / 待收边缘 / 沉降变化全部为 0；金标准 2344 行全等。

### 新增一种元素的步骤

1. 选层：本体用 `Custom "名字" 值`（值自定义，例如耐久；存储编码只能是一个 `Int`）；格子下面的层用 `SlotGround`（放进 `gsGround`）；需要新的内置层时才动 `Types`。
2. 写 instance：定义一个类型（状态放在值里），写 `instance Element 类型`（测试 / 扩展元素写在自己的模块里；新的**内置**元素放进 `src/Match3/Element/Builtin/` 下功能最接近的分组文件——宝石 `Gem`、冰 / 叠层 `Layer`、打破型障碍 `Obstacle`、收集计数 `Collectible`、会动 / 会生成的 `Actor`、地面层 `Ground`、关卡级 `Level`——条目函数写在同一文件，跨分组共用的辅助放 `Common`；再在 `Element.Builtin.builtinDefs` 末尾追加条目，已有条目不要重排），只写 `name` / `toCell` 和要改的方法（例如 `archetype`、`onHit`、`adjacentRule`、`counter`、`falls`、`drains`、`groundRule`、`color`）。邻格规则选一个不和现有顺序冲突的 `arOrder`；步末要挖掉格子时给 `EndRule` 填 `erHoles`，补结算自动发生。叠层类写 `instance Modifier`；不在格子里的机制写 `instance LevelElement`，回复流水线节拍消息（或自定义新消息）。
   - 只经注册表即可接入的类别：本体 `Custom`（削层 / 打碎 / 免疫 / 挡交换 / 下落 / 洗牌保留，也可以是按颜色匹配的有色棋子：给 `color`、原型 `Piece`，见 `ec_custom_matchable_gem`）、叠层与冰的命中规则、地面层、任意方向的边缘收集物、带 `erHoles` 的步末元素、以 `CountNamed` 计数并用 `GoalNamed` 当目标的元素、成对交换规则（`swapRule`）、开启类元素（`openRule`）、可被改色 / 推动（`recolorable` / `pushable`）、不进普通匹配提示（`hintable`）、在已有节拍上反应的关卡级元素（`LevelElement`，见 `ec_level_elements_by_message`）。
   - 段 5 的双层果冻（`Jelly`，在 `Element.Builtin.Ground`：地面层 + `groundRule` + `CountNamed`）与气泡（`Bubble`，在 `Element.Builtin.Collectible`：`Custom` + `onHit` + `adjacentRule` + `CountNamed`）就是这样接入的内置元素：规则只在 instance 里，关卡数据在 `Types.allLevels` / `Game.Level` 的放置 / 地面表里，主流程没有改动（`jb_main_flow_untouched_scan`）。
   - 仍需改主流程的：需要**新节拍**的关卡级元素（节拍由主流程在固定位置发出）、需要存进 `GameState` 的关卡级状态（见下节「遗留」）、补子时生成自定义棋子。
3. 注册：内置元素 = 在 `Element.Builtin.builtinDefs` 里加一行（关卡级元素加进 `builtinLevelDefs`）；测试 / 扩展元素 = `register (customEntry 原型 (Int → 元素)) defaultRegistry`（地面层用 `groundEntry`，关卡级元素用 `registerLevel (SomeLevel 值)`），把注册表传给 `*With` 入口（`trySwapWith` / `resolveSwapWith` / `resolveHammerWith` / `ensurePlayableWith` / `shuffleGameWith` / `applyHintWith` / `decorateLevelWith` / `traceEventsWith`），或整体用 `Match3.Engine.match3GameWith reg`。
4. 放置：在关卡放置表里写 `Place "名字" [参数] [坐标]`，由条目的放置函数落格（`customEntry` 缺省 = `Custom 名字 第一个整数参数`；要别的解析用 `customEntryWith`）。
5. 表现：贴图名即元素名（`assets/` 里放同名贴图，缺图时画灰块）；要专门画法的 `Custom` 在 `UI.CellTable.customTable` 加一行，地面层元素在 `UI.Ground.groundTable` 加一行，颜色在 `UI.Layout.elementRGBTable`（HUD 目标 / 地图 / 几何版共用）；步末有新效果时在前端各查找表里加一行。
6. 测试：参照 `element_registry_custom_crate_extensibility`、`test/Spec/Extension.hs` 与 `test/Spec/ElementClass.hs`（测试专用「木箱」`Crate` 只定义在测试辅助 `test/Spec/Support.hs`，断言它削层、打碎、计数、挡交换、被锤、洗牌保留，并断言核心源码里没有它的名字）。

### 专门分支的收编（段 4）

段 4 把原先写死在主流程里的专门分支收进元素框架：主流程只经注册表取规则，内置实现挪到 `Element.Builtin` 的定义里。金标准 2344 行全等、三场景截图 AE=0。**段 4 没有改动 `Engine.*`**（只动了 `Match3.*`）。

| 原专门分支 | 段 4 之后（元素类迁移后的写法） | 主流程入口 |
|------------|-----------|------------|
| 彩虹取色（`Move` / `Boosters` / `findHint` 直接调 `isRainbowSwap` / `rainbowClearSeeds`） | `SpecialGem _ Rainbow` 的 `swapRule = SwapRule 10 isRainbowSwap rainbowClearSeeds` | `Registry.swapOpeningWith`、`findHintWith` 逐条 `swapRules` |
| 特殊 × 特殊合成（`isSpecialCombo` / `comboClearSeeds`） | `SwapRule 20`，挂在 `SpecialGem _ LineH` 上（规则自己检查两端） | 同上 |
| 彩蛋开启（`Clear` 直接调 `Obstacles.openSurprises`） | `SurpriseEgg` 的 `openRule = OpenRule openSurprises` | `Registry.openWith`（`surpriseClearPassWith` 成为通用的「开启类元素」流程） |
| 魔法帽 / 染色瓶只改 `isGem` 的格 | `recolorable`（`Piece` 缺省 True：宝石各种类、倒计时、双面块） | `AdjCtx.acRecolor` → `triggerAdjacentHatsBy` / `triggerAdjacentBottlesBy` |
| 蜗牛只推 `Snail.pushable` 的格 | `pushable`（同上） | `EndCtx.ecPushable` → `stepSnailAtBy` / `traceSnailsBy` |
| 飞碟（`Cascade` 直接调 `stepUfos`） | `UfoLevel` 回复 `Refilled` → `Absorbed` | `Registry.absorbWith` |
| 皮带（`Resolve` 直接调 `beltMoves`） | `BeltLevel` 回复 `EndTicked` → `Shifted` | `Registry.beltShiftWith` |
| 传送门（`Gravity` 里写死实现） | `PortalLevel` 回复 `Settling` → `Settled` | `Registry.teleportWith` |
| 地毯（`Resolve` 直接调 `coverCarpets`） | `CarpetLevel` 回复 `Covering` → `Covered` | `Registry.coverWith` |
| 提示排除彩虹本体（`findHint` 直接调 `Rainbow.isRainbow`；元素类迁移收编） | `SpecialGem _ Rainbow` 的 `hintable = False` | `Board.Match.findHintWith`（`hintableWith`） |

段 4 当时关卡级元素是 `LevelDef{ldName, ldHook}` + 封闭和类型 `LevelHook`（`HookAbsorb` / `HookShift` / `HookTeleport` / `HookCover`）；元素类迁移后换成 `LevelElement` + 节拍消息（见上节「元素类的方法与调用时机」），`registerLevel` / `removeLevel` / `levelDefs` 保留。未注册即不生效（测试 `br_level_hooks_*` 在 38 关实测）。

护栏（`test/Spec/Branches.hs`，样例元素拉杆 / 豆荚 / 小车只在测试里）：测试专用成对规则、开启规则、可推动元素只经注册表生效；内置谓词与原写死谓词逐格相同；去掉关卡级元素后飞碟 / 皮带 / 地毯不生效；源码扫描 `br_main_flow_no_special_branches`（`Move` / `Boosters` / `Match` 不点名彩虹取色与特殊合成，`Clear` 不 import `Obstacles`，`Cascade` 不用 `stepUfos`，`Resolve` 不用 `coverCarpets` / `beltMoves`，`Gravity` 不用 `portalWith`）。

**遗留（抽不干净的，及原因；元素类迁移后更新）**：

| 遗留 | 状态 | 原因 |
|------|------|------|
| `findHint` 用 `Rainbow.isRainbow` 排除彩虹本体 | **已消掉**（元素类迁移） | 换成元素方法 `hintable`（彩虹 = False），提示顺序逐字不变（快照 M 行 + 金标准锁定） |
| `LevelHook` 封闭和类型 | **已消掉**（元素类迁移） | 换成 `LevelElement` + 开放消息；新关卡级元素只要 `registerLevel`（`ec_level_elements_by_message`） |
| 自定义元素不能是可匹配的有色宝石 | **已消掉**（元素类迁移） | `Custom` 本体的颜色来自元素的 `color` 方法，注册 `Piece` 原型 + 颜色即参与匹配 / 提示 / 计数（`ec_custom_matchable_gem`）；补子仍只生成普通宝石 |
| 钩子调用时机写死在主流程（吸收在补子后、移位在 Tick 与 Spread 之间……） | 部分消掉 | 发消息的节拍仍是流水线的固定位置（改节拍 = 改规则流水线）；但哪个元素在哪个节拍反应、回复什么，由元素自己决定，新消息类型也能随时定义 |
| 关卡级元素的状态仍在 `GameState` 专用字段（`gsUfos` / `gsBelts` / `gsPortals` / `gsCarpetOpen` 与 `gsUfoCollected` / `gsCarpetsCovered`）；关卡搭建（`Game.Level`）直接构造这些字段 | 保留 | 字段参与撤销快照、存档、HUD、目标判定与金标准投影，测试里有 200 多处读取与记录更新；移进元素值要改所有这些读点，却不带来新行为。节拍消息把状态作为参数带给元素、回复里带回新状态 |
| 自定义元素的存储编码仍是 `Custom 名字 Int` | 保留 | 盘面以 `Cell` 存储（金标准、前端、机制模块都按它读写）；元素值由构造器从这个 `Int` 解码，所以自定义元素的状态只能编码成一个整数 |
| 地面层元素的 `toCell` 只用于显示 | 保留 | 地面层在 `gsGround` 里、不进盘面，`toCell` 写回的格子不会落到 `Board` 上 |
| 冰层 / 叠层槽位没有注册条目时的兜底 | 行为略有不同 | 新版直接跳过这一层（不当修饰器询问）；旧版按 `baseDef` 缺省把它当一层惰性层（挡交换、打不动）。内置注册表恒含全部冰层 / 叠层条目，不影响内置元素与金标准 |
| `Resolve` 仍 import `Conveyor.applyBeltMoves`；`Trace` 重放 `EndBeltShift` 时也直接用它；`Trace` 再导出 `beltMoves` | 保留 | `applyBeltMoves` 是「按 (原格, 新格) 列表移格」的通用搬运，与皮带语义无关；再导出只为兼容旧 import |
| `Rainbow` / `Combos` / `Obstacles` / `Snail` 里的规则实现本身 | 保留 | 实现仍在各自模块，只是改由 `Element.Builtin` 的 instance 引用；旧的 `*Except` / `stepSnailAtBlocked` 保留为 `By isGem` / `By pushable` 的包装（测试与 `Board.Default` 在用） |
| `LevelGoal` / `SpreadKind` | 保留 | 封闭 ADT，被关卡表、目标判定、HUD / 标题文案穷举匹配；新元素用 `GoalNamed 名字 N` 作目标，不必再加构造器 |

### 元素类（阶段 1 原型 → 阶段 2 迁移）

元素框架从扁平的 `ElementDef` 记录（段 2b–5，一个元素 = 一条带 20 多个字段的记录，从 `baseDef` / `gemDef` / `blocker` / `fixed` / `layered` 等模板改字段）改成了 xmonad `LayoutClass` 风格的类型类，分两阶段：

- **阶段 1（原型）**：宝石（`PlainGem` / `SpecialGem`）、彩蛋（`SurpriseEgg`）、冰层（`Ice`）先写成 instance，经适配层 `bridgeBody` / `bridgeModifier` 桥接回旧记录，与其余旧记录并存；`ec_bridge_*` 与 `ec_registry_paths_agree_in_play` 逐字段 / 逐手比对新旧两条路径。彩蛋在现行规则里没有跨轮状态、`GameState` 里也没有彩蛋专用字段，「状态放在元素值里」改由测试专用「鸟窝」`Nest` 演示。
- **阶段 2（迁移）**：全部 31 个内置条目都是 instance（19 个本体类型 + 1 个冰层 + 8 个叠层修饰器；`SpecialGem` 一个类型对应 line_h / line_v / bomb / rainbow 四个条目），四个关卡级元素是 `LevelElement`。`ElementDef` / `baseDef` / `LevelDef` / `LevelHook` 与适配层 `Match3.Element.Prototype` 一起删除；注册表改为「名字 → 构造器」（`Entry`），一格解码成嵌套的元素值后直接调类方法。
- **分文件**（阶段 2 之后的纯搬家）：原来 640 行的 `Element.Builtin` 按功能拆到 `src/Match3/Element/Builtin/` 下的 `Gem` / `Layer` / `Obstacle` / `Collectible` / `Actor` / `Ground` / `Level` / `Common`（各文件开头的注释写明这一组的共同特征），每个元素的类型、instance、条目函数放在同一文件；`Element.Builtin` 只按原注册顺序汇总条目，对外导出不变。彩蛋归 `Obstacle`：它是原型 `Blocker`、命中即破的占格本体，没有计数（不是收集类），也不按颜色匹配（不是宝石）。只在一组里用到的辅助留在组内（`chip` / `layersPlace` 在 `Obstacle`，`peel` / `layerChip` / `spreadRule` 在 `Layer`，`tickRun` / `snailRun` / `traceSnails` 在 `Actor`），跨组共用的才进 `Common`。依赖只朝一个方向：`Obstacle` 引用 `Gem`（双面块翻成宝石）和 `Collectible`（保险箱开成饼干），其余分组互不引用。

**等价性依据**：阶段 2 删掉旧记录之后，新旧两条路径无法在同一进程里并排比对，所以先在阶段 1 的代码上生成元素查询快照 `test/golden/element-queries.txt`（1648 行，生成器 `test/golden/ElementQueries.hs`；用阶段 1 的旧记录注册表生成的结果与之逐字相同），阶段 2 一字不改地比对它：Q 行 = 全部格子组合上的逐格查询，P = 放置，R = 规则表 / 条目名 / 个数差计数 / 关卡级元素清单，A / E / S / O / C / G = 邻格 / 步末 / 成对交换 / 开启规则、直接命中、地面层在样例盘上的输出，M = 40 关 × 种子 1–2 × 12 手（提示、锤子、十字、交换）的逐手散列。三个 `ec_*_snapshot` 测试取代了阶段 1 的三个 bridge 测试。另有金标准 2534 行全等、三场景截图 AE=0。

### 与 xmonad LayoutClass 的对照

| xmonad | 本项目 | 说明 |
|--------|--------|------|
| `class LayoutClass layout a`（`doLayout` / `handleMessage` / `description` 等方法都有默认实现） | `class Element e`（`name` / `toCell` 必写，其余 25 个方法有默认实现） | 一种元素 = 一个类型 + 一个 instance，只覆盖用到的方法 |
| 默认方法之间互相推（`doLayout` 默认调 `pureLayout`） | 默认值由 `archetype`（`Piece` / `Blocker` / `Fixed`）推出，颜色缺省取 `toCell` | 取代旧的 `gemDef` / `blocker` / `fixed` 模板 |
| 布局的状态在值里（`Tall nmaster delta frac`），`handleMessage` 返回新布局 | 元素的状态在值里（`StoneE 层数`），`onHit` / `handleMessage` 返回新元素值（可换成别的元素：保险箱开成饼干） | 盘面仍存 `Cell`，`toCell` 写回、构造器解码 |
| `data Layout a = forall l. LayoutClass l a => Layout (l a)` | `data SomeElement = forall e. Element e => SomeElement e` | 存在类型装箱；Eq 先比名字再比状态，Show 稳定 |
| `LayoutModifier` / `ModifiedLayout m l` | `Modifier` / `Modified SomeModifier SomeElement`（冰层、叠层） | 修饰器先说，没意见再问里面；`Pierce` / `Keep` / `Remove` / `Shatter` 对应命中时的四种组合 |
| `Message` / `SomeMessage` / `fromMessage`（Typeable） | `Match3.Element.Message` 同名三件套 | 开放消息：任何模块都能定义新消息，收的一方按类型认领 |
| xmonad 在事件循环里把 X 事件转成消息发给当前布局 | 主流程在流水线节拍上发 `Refilled` / `EndTicked` / `Settling` / `Covering`，`LevelElement` 回复 `Absorbed` / `Shifted` / `Settled` / `Covered` | 回复是消息而不是新布局：关卡级状态仍在 `GameState` 字段里，由消息带进带出 |
| 布局组合子（`|||`、`Choose`）在运行时切换布局 | 无对应 | 一格同一时刻只有一种本体；注册表按槽位 / 名字选构造器 |
| `description` 用于显示与序列化 | `name`（贴图名、计数键、放置表键、事件里的 `evElement`） | — |

与 xmonad 的主要不同：xmonad 的布局值本身就是状态的存储；这里盘面的存储编码仍是 `Cell`（金标准、前端、机制模块都按它读写），元素值在每次查询时从格子解码、改完用 `toCell` 写回。

## 多游戏接口

第三刀起，「回合制、纯函数、固定种子可复现」的游戏被抽象成与三消无关的通用层；三消是它的第一个实现。**不做第二个游戏**，测试里只有一个一维计数器玩具（`test/Toy.hs`）证明接口可以脱离三消编译、跑通。

### 分层

```
┌─ 通用层（不 import 任何 Match3 模块；engine_layer_is_game_agnostic 检查）────────────────┐
│ src/Engine/Game.hs      Game cfg s a e o r、Step、runActions / finalState / stepEffects     │
│ src/Engine/History.hs   History / Undoable / withHistory：通用撤销历史（段 3）              │
│ src/Engine/Effect.hs    Effect（节拍 / 种类 / 主体 / 格 / 数量）、beats                      │
│ src/Engine/Playback.hs  Stages / Player / Tick：帧节拍、分段推进、加速、进度；Cue 队列        │
│ app/Shell/Loop.hs       SDL 外壳：初始化 / HiDPI 窗口 / 渲染器 / 固定 16 ms 主循环；Plugin   │
└──────────────▲───────────────────────────────▲──────────────────────────▲────────────────┘
               │ 实现 Game                      │ 用 Player + 自己的 Stages  │ 实现 Plugin
┌──────────────┴─────────────┐  ┌──────────────┴──────────────┐  ┌────────┴──────────────────┐
│ 三消实现（库）              │  │ 三消回放（app/ComboFx）       │  │ 三消插件（app/UI.*）        │
│ Match3.Engine：match3Game、│  │ cascadeStages：高亮→消失→下落 │  │ UI.Plugin 钩子、UI.Input   │
│ Action、match3Shell、toEffect│  │ →落定 / 步末阶段；WaveView    │  │ 输入映射、UI.Draw /        │
│   ▲ Match3.Game.* / Board.* │  │                              │  │ UI.CellTable 绘制          │
│   │ / Element.*（规则）      │  │                              │  │                            │
└────────────────────────────┘  └──────────────────────────────┘  └────────────────────────────┘
测试：test/Toy.hs（只 import Engine.*）+ engine_toy_counter_game
```

### 接口字段

`Engine.Game.Game cfg s a e o r`（cfg 开局配置、s 状态、a 动作、e 本游戏的效果事件、o 结局、r 整步报告）：

| 字段 | 类型 | 说明 | 三消（`Match3.Engine.match3Game`） |
|------|------|------|------------------------------------|
| `gameName` | `String` | 名字 | `"match3"` |
| `gameNew` | `cfg -> Seed -> s` | 开局；**唯一**接受外部种子的地方 | `Setup`：`Campaign 关卡下标` / `CustomLevel 配置` / `Daily 年 月 日`（每日的种子由日期决定） |
| `gameStep` | `s -> a -> Step s e o r` | 纯函数推进一步，随机数只来自 `s` | 终局时拒绝走步与洗牌（提示除外：只写 `gsHint`，与原前端终局后按 H 的行为一致）；否则 `playWith reg`，`stepReport = Just Played` |
| `gameOutcome` | `s -> Maybe o` | 结局判定 | `gsOver`（`Won` / `Lost` / `LevelClear`） |
| `gameActions` | `s -> [a]` | 当前会被接受的动作（测试 / 自动演示） | 所有会成交的相邻交换 |
| `gameStatus` | `s -> [(String, Int)]` | 给外壳的具名数值 | level / score / moves / combo / hammers / freeSwaps / crossClears（标题栏的连击数从这里取） |
| `gameEffect` | `e -> Effect` | 本游戏事件 → 通用效果 | `toEffect`：节拍 = `evWave`，种类 = 事件种类标签，主体 = `evElement`，格 = 每对的目标格，数量 = `evAmount` |

`Step{stepState, stepEvents, stepOutcome, stepAccepted, stepReport}`：被拒时状态不变、没有事件。`stepReport :: Maybe r`（段 3）放本游戏自己的前端才需要的整步数据（三消：`Played`），通用层原样带出、不解释——有了它，外壳执行动作只调 `gameStep`，不必绕过接口调游戏自己的入口。

**撤销历史（`Engine.History`，段 3）**：`withHistory policy game` 把 `Game cfg s a e o r` 变成 `Game cfg (History s) (Undoable a) e o r`。`Act a` 交给原游戏，被接受且 `hpRecord a` 时先记 `hpSnapshot` 过的旧状态（最多 `hpLimit` 份）；`Undo` 由本层处理，有历史就回到最近一份快照（经 `hpRestore`），**不经原游戏的终局拒绝**，所以终局后仍可撤销；`gameActions` 在有历史时多一个 `Undo`，`gameStatus` 多一项 `undo`（可撤销步数）。组合子：`runActions`（依次执行，遇到结局即停）、`finalState`、`stepEffects`、`rejectedStep`。

`Engine.Effect.Effect{efBeat, efKind, efSubject, efSpots, efAmount}`：节拍相同的效果同时播放（`beats` 按连续节拍分组）。

`Engine.Playback`：`Stages{stageLen, stageNext}` 由游戏给出（阶段多长；播完后 `Right (下一阶段, 进入时触发的事件)` 或 `Left 最终状态`）；`Player{plStage, plFrame, plFast}` 只管时钟：每帧推进 1 帧、加速后推进 `fastStep` 帧，到达阶段长度就切换、帧归零。`playerProgress` 给出阶段进度 0..1。简单游戏可直接用 `effectCues framesFor effects` + `cueStages`。

`Shell.Loop.Plugin w{plugInit, plugBegin, plugEvents, plugTick, plugDraw}`：外壳在窗口 / 渲染器建好后调 `plugInit` 建世界状态 `w`，之后每帧依次调 `plugBegin` → 取事件 → `plugEvents`（返回是否退出）→ `plugTick` → `plugDraw` → present → 补足 `scFrameMs`。

**取舍：record-of-functions，而不是带关联类型的 typeclass。** 同一种游戏可以有多份配置不同的实例（三消：`match3GameWith reg` 接任意元素注册表），记录是一等值，类型类按类型只能有一个实例；不需要 `TypeFamilies` / 孤儿实例，玩具实现在测试里写一个值即可；状态、动作、事件、结局都是普通类型参数，推断直接。代价是没有「按类型自动找实例」，调用处要显式传 `Game` 值——对只有少数几个游戏的项目这是好事。

**随机数与种子约定。** 随机数生成器放在状态里（三消是 `gsGen :: StdGen`），`gameStep` 是纯函数：同一状态 + 同一动作 ⇒ 同一结果；只有 `gameNew` 接受外部种子（`Seed = Int`），由外壳 / 测试决定，规则层从不读时钟或做 IO。

### 三消的动作映射

| `Match3.Engine.Action` | 规则入口（每个动作只结算一次） | 被拒条件 | 事件 |
|------------------------|--------------------------------|----------|------|
| `Swap p q` | `resolveSwapWith` | `NoMatch` / `InvalidSwap` | `traceEventsWith reg`（回放脚本展开） |
| `Hammer p` / `FreeSwap p q` / `CrossClear p` | `resolveHammerWith` / `resolveFreeSwapWith` / `resolveCrossClearWith` | 同上 | 同上 |
| `Hint` | `applyHint`（写 `gsHint`，`pdHint` 带回提示） | 从不 | 无 |
| `Shuffle` | `shuffleGame` | 已结束 | `EvShuffle` |

撤销不是三消的动作：外壳用 `match3Shell`（`withHistory match3History match3Game`），动作是 `Act (Swap p q)` / … / `Undo`。整步报告 `Played{pdState, pdOutcome, pdTrace, pdFx, pdEvents, pdHint, pdAccepted}` 经 `stepReport` 带回；界面行为（终局后仍可撤销、终局后按 H 仍给提示）与原来直接调 `play` 完全相同。测试：`engine_match3_instance_matches_direct_api`（`gameStep` 与直接调旧入口逐位相同）、`engine_undo_after_terminal_matches_legacy_play`（终局后撤销 = `13094d1` 的 `play Undo`）、`engine_frontend_steps_only_via_gameStep`（`app/` 源码扫描）。

**段 3 对 `Engine.*` 的改动**：`Engine.Game` 的 `Game` / `Step` 多一个类型参数 `r` 与字段 `stepReport`（原因：前端要的回放脚本 / 特效 / 提示原来只能从 `play` 拿，要让外壳只调 `gameStep`，接口必须能带出整步报告）；新增 `Engine.History`。玩具 `test/Toy.hs` 的 `r = ()`，其余不变。

### 接入一个新游戏的步骤清单

1. **类型**：定义开局配置 `cfg`、状态 `s`（含随机数生成器）、动作 `a`、效果事件 `e`（纯数据）、结局 `o`。模块放在自己的命名空间（如 `src/Foo/`），**不** import `Match3.*`。
2. **写 `Game` 值**：`gameNew`（只在这里用种子）、`gameStep`（纯；非法动作返回 `rejectedStep` 式的结果）、`gameOutcome`、`gameActions`、`gameStatus`、`gameEffect`。
3. **纯测试**：参照 `test/Toy.hs` 与 `engine_toy_counter_game`——`runActions` 走到胜 / 负、非法动作被拒且不改状态、结局后拒绝一切、`gameActions` 全部被接受、效果按节拍播放的帧数（含加速）。
4. **回放**：时间线简单就用 `effectCues` + `cueStages`；复杂时间线自己写 `Stages`（参照 `ComboFx.cascadeStages`），交给 `Player`，绘制时用 `playerProgress` 取进度。
5. **外壳**：写一个 `Plugin`（`plugInit` 加载资源、建世界状态；`plugEvents` 把 SDL 事件映射成动作并调 `gameStep`（要撤销就用 `withHistory` 套一层，历史不要放进游戏状态；前端要的额外数据放进 `stepReport`）；`plugTick` 推进 `Player`；`plugDraw` 绘制），`main = runShell cfg plugin`。
6. **依赖检查**：新游戏与通用层之间只允许「新游戏 → Engine.* / Shell.Loop」；需要时把新的通用文件加入 `engine_layer_is_game_agnostic` 的检查列表。
7. **文档**：在本节的分层图与模块地图里登记新模块。
