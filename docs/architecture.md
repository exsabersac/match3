# 架构

## 分层

```
app/（可执行文件 match3-sdl，依赖 SDL2；图中箭头 = 依赖）
┌──────────────────────────────────────────────────────────────┐
│ Main（入口 / 固定步长主循环）                                  │
│   ├─ UI.Input ──→ UI.Actions ──→ UI.Playback                 │  输入 → 动作 → 表现编排
│   ├─ UI.Draw ──→ UI.Cascade / UI.EndStage / UI.HudArt …      │  一帧绘制
│   └─ UI.Env（环境变量 / 高分屏倍率）                          │
│ UI.Types / UI.Layout（状态类型、布局常量，被上面所有模块依赖）  │
│ ComboFx（纯阶段机）   Art（贴图图集）                          │
└──────────────────────────────┬───────────────────────────────┘
                               │ 只 import Match3.Core
┌──────────────────────────────▼───────────────────────────────┐
│ Match3.Core（library 门面，再导出稳定公开 API）                │
└──────────────┬──────────────────────────────┬────────────────┘
               ▼                              ▼
   Match3.Game（外观，再导出）        Match3.Board（外观，再导出）
     State  Tally                       Grid
     Outcome  Shuffle  Trace            Match   Gravity
     Resolve（公共结算）                 Clear   Random
     Level  Move  Boosters
               │                        Cascade
               └──── 调用 Match3.Board ─────┤
                                            ▼
 Match3.Element（元素框架：Types / Registry / Builtin / Event；默认注册表 defaultRegistry）
 Obstacles Rainbow Combos Ice Grass Carpet Snail Ufo Countdown Conveyor Boosters Daily
 Match3.Types（类型 / 关卡表）
 （纯函数机制模块；无 IO）
```

**依赖方向（硬约束）**

- 纯核心 / library ← 应用：`match3-sdl` 依赖 `match3` 库；库**不**依赖 SDL。`app/` 里的模块只通过 `Match3.Core` 使用规则。
- `Core` 是门面：把各子模块符号汇总导出，便于前端与测试只 import 一处。`Match3.Board` / `Match3.Game` 也是外观模块，保持原有导出列表，实现都在各自的子模块里；测试和 `Core` 的 import 不需要改。
- 子模块之间单向依赖、无环（下文 `A ← B` 表示 B 依赖 A）：Board 内 `Grid ← Match ← Clear`、`Grid ← Gravity`、`Match ← Random`，`Cascade` 依赖 Grid / Match / Clear / Gravity；Game 内 `State ← Outcome / Shuffle / Trace`、`Shuffle ← Level`，`Resolve`（公共结算）依赖 State / Tally / Outcome / Shuffle / Trace，`Move` / `Boosters` 只做校验与起手选择、依赖 `Resolve`。Game 子模块只经 `Match3.Board` 外观使用棋盘函数。
- 元素框架 `Match3.Element.*` 位于 Board / Game 之下：`Element.Types ← Element.Registry ← Element.Builtin`，`Element.Event` 只依赖 `Types`；`Builtin` 调用各机制子模块实现具体反应。Board / Game 只通过注册表查询「这个格子怎么反应」，不再按构造器写死（见[元素框架与事件](#元素框架与事件)）。
- 机制子模块（障碍、彩虹、合成、冰、草系、地毯、蜗牛、飞碟、倒计时、传送带、道具种子、每日）尽量只依赖 `Types`（及彼此必要的窄依赖），由 `Board.*` / `Game.*` 编排调用顺序。
- 回放方向单向：`Match3.Game.Resolve.resolveMove`（经 `Match3.Board.Cascade` 的记录版连锁）与结算结果一起产出 `MoveTrace` → `app/ComboFx.hs`（纯阶段机，按时间线把它拆成帧）→ `UI.Playback`（阶段事件 → 弹字 / 粒子 / 震屏）→ `UI.Cascade` / `UI.EndStage`（绘制）。核心**不**知道帧、阶段或样式；`ComboFx` 不 import SDL，也不调用 `trySwap` 等规则入口，只读 `MoveTrace` 与前端传入的结算后盘面。

## 模块地图

### 核心库（`src/Match3/`）

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Match3.Types` | `Color` / `GemKind` / `CellOverlay` / `CellContents`（含 `Custom 名字 值`，供注册表扩展元素）、构造器与谓词、`LevelGoal` / `Outcome` / `allLevels` | 连锁、交换、IO |
| `Match3.Element` | 门面：再导出下列四个模块 | 自身无实现 |
| `Match3.Element.Types` | `ElementDef`（一个元素的全部钩子）、`Slot`、`HitResult`、`AdjacentRule`、`EndRule`、`Counter`、`Placement`、`baseDef` | 调用顺序 |
| `Match3.Element.Registry` | `Registry`（按层数组 O(1) 分派 + 自定义元素表 + 已排序的邻格 / 步末规则）、`register` / `lookupElement`、各钩子的查询函数 `*With` | 具体元素 |
| `Match3.Element.Builtin` | 全部内置元素的 `ElementDef` 与 `defaultRegistry` | 流水线 |
| `Match3.Element.Event` | `EndEffect` / `SpreadKind` / `SnailMove`、`applyEndEffect`、效果事件 `EventKind` / `Event` | 帧与样式 |
| `Match3.Core` | 再导出公共 API | 自身几乎无逻辑 |
| `Match3.Board` | 外观：按原导出列表再导出下列子模块 | 自身无实现 |
| `Match3.Board.Grid` | 坐标边界、读写格、交换、相邻、可空盘面 `MBoard`、`randomColor` | 任何规则 |
| `Match3.Board.Match` | `MatchRun` / `findMatchRuns` / `hasAnyMatch`、`findHint` / `hasValidMove` | 修改盘面 |
| `Match3.Board.Clear` | 一轮消除（匹配 / 种子）、特殊扩展与生成、彩蛋、邻格削层与触发、飞碟吸收（吸走 ≠ 引爆）、计分公式 | 沉降、连锁循环 |
| `Match3.Board.Gravity` | 重力（固定格分段）、底行饼干、传送门沉降、补子、`settleRefill` | 消除 |
| `Match3.Board.Cascade` | 连锁的**单一实现**：`cascadeMatches` / `cascadeMatchesFrom` / `cascadeSeeds` / `cascadeAfterBelt` / `cascadeCountdowns` 返回 `CascadeRun`（终盘 + `CascadeTally` 计数记录 + `[CascadeWave]` + 飞碟 + 生成器）；旧的 `runCascade*` / `resolveCountdowns` / `runPostBeltCascade`（元组）与 `traceCascade*` 都是它的兼容投影 | 步数 / 目标结算、道具扣次 |
| `Match3.Board.Random` | 随机盘、稳定盘、可玩盘、`shufflePlayable` | 保留装饰（见 `Game.Shuffle`） |
| `Match3.Game` | 外观：按原导出列表再导出下列子模块 | 自身无实现 |
| `Match3.Game.State` | `GameState`、撤销快照、`MoveFx` / `moveFx` / `clearMoveFx`（边沿触发）、`undoMove`、`applyHint` | 结算 |
| `Match3.Game.Tally` | 结算计数辅助：颜色袋、保险箱 / 时间精灵计数、地毯腾空格 | 结局判定 |
| `Match3.Game.Outcome` | 目标满足、`decideOutcome`、`checkOutcome`、选关解锁、地图跳转、失败提示 | 盘面 |
| `Match3.Game.Shuffle` | 保装饰洗牌 `shuffleGame`、自动洗牌 `ensurePlayable` | 回放（洗牌不在 `mtEnd`） |
| `Match3.Game.Level` | 开局 / 每日 / 重开 / 下一关、战役装饰 `decorateLevel`、皮带 / 传送门 / 飞碟布局、步数携带 | 走步 |
| `Match3.Game.Trace` | `MoveTrace`（含 `mtGen` / `mtShuffle`）/ `EndStep`（`EndEffect` 等再导出自 `Element.Event`）、`traceSpreads`（跑注册表的蔓延规则）/ `beltMoves`、效果事件 `traceEvents` | 结算 |
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
| `Match3.Conveyor` | 传送带循环移位 | 移位后再连锁（`Game.Move`） |
| `Match3.Boosters` | 锤子/十字**种子位置**（纯几何） | 扣次数与连锁（`Game.Boosters`） |
| `Match3.Daily` | 日期种子、每日配置、三星公式 | 每日盘面装饰（`Game.Level`） |

### 前端模块（`app/`）

| 模块 | 职责 |
|------|------|
| `Main` | 入口：窗口 / 渲染器 / 贴图初始化、初始 `App`、固定步长（≈60 fps）主循环：倍率同步 → 事件 → 动画推进 → 绘制 |
| `UI.Types` | `App`、`Anim`、`Particle`、`ToolMode`，帧数常量，`animBusy` / `playingCascade` |
| `UI.Layout` | 逻辑像素布局常量、格子坐标换算、矩形 / 插值工具、调色板 |
| `UI.Env` | 环境变量（`MATCH3_LEVEL` / `SEED` / `SCALE` / `SHOWCASE`）、展示盘、高分屏倍率与鼠标坐标换算 |
| `UI.Input` | SDL 事件 → 动作；播放锁定（见 [ui-controls.md](ui-controls.md#播放锁定animbusy)） |
| `UI.Actions` | 标题栏、关卡重置、三种道具执行、回放加速、过关前进 / 重试 |
| `UI.Playback` | 纯函数：每帧推进动画；按本次 `MoveFx` / `MoveTrace` 编排回放，阶段事件产生弹字 / 浮字 / 震屏 / 粒子 |
| `UI.Draw` | 一帧的层次与贴图 / 几何分派 |
| `UI.Cascade` | 静止盘、交换补间、轻落、逐轮回放（高亮 / 消失 / 下落）、震屏视口 |
| `UI.EndStage` | 步末阶段绘制：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌 |
| `UI.BoardArt` | 棋盘贴图绘制与分派（`drawCellAny` / `drawStatic` 等）、回放共用的底盘部件 |
| `UI.BoardPrim` | 棋盘几何降级绘制（`drawGemAt` 等） |
| `UI.HudArt` / `UI.HudPrim` | HUD、横幅、键位条、暂停帮助、结算面板、弹字的贴图版 / 几何降级版 |
| `UI.TextArt` / `UI.Glyph` | 烘焙文字 / 中文标签贴图的排版；缺字形时的像素字 |
| `UI.LevelMap` | 选关地图：章节、节点坐标、点击命中、两种绘制 |
| `ComboFx` | 连锁逐轮回放的纯逻辑（步末阶段种类查 `endStageTable`，按事件种类分派）：阶段机（高亮→消失→下落→落定，以及步末阶段：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌）、时间线常量、连击等级样式、下落映射、浮字曲线；只消费 `MoveTrace`，不绘制 |
| `Art` | 贴图图集（BMP + 索引）加载、路径查找、九宫格面板、染色/加色绘制；缺资源时各绘制模块退回几何版 |

前端依赖同样单向无环：`UI.Types` / `UI.Layout` 在最底层；`UI.Glyph ← UI.TextArt ← UI.HudArt ← UI.LevelMap`，`UI.BoardPrim ← UI.BoardArt ← UI.EndStage ← UI.Cascade ← UI.Draw`，`UI.Playback ← UI.Actions ← UI.Input ← Main`（`A ← B` 表示 B 依赖 A）。

## 构建工具链

| 项 | 值 |
|----|-----|
| 构建 | Stack（`package.yaml` → hpack → `match3.cabal`） |
| Resolver | **lts-21.25** |
| GHC | **9.4.8**（`stack.yaml`：`system-ghc: true`） |
| 库名 | `match3` |
| 可执行文件 | `match3-sdl` |
| 测试套件 | `match3-test`（`test/Spec.hs`，tasty + HUnit + QuickCheck） |

库依赖：`base`、`array`、`random`。可执行文件额外：`sdl2`、`text`，以及 GHC 自带的 `containers`、`directory`、`filepath`（贴图加载）。贴图由 `tools/gen_assets.py` 生成到 `assets/`，详见 [ui-art.md](ui-art.md)。

## 状态边界

- **规则已结算**：`trySwap` / `useHammer` / `useFreeSwap` / `useCrossClear` 返回的 `GameState` 已是稳定盘（或终局），前端只做展示与补间。
- **随机**：`StdGen` 存在 `gsGen`；洗牌 / 补子推进生成器，测试用固定种子。
- **历史**：`gsHistory` 最多保留约 20 步快照供撤销；UI 粒子用 `gsLastCleared`，不参与规则；前端经 `moveFx` 边沿触发特效，失败操作不会重播上一步连击。
- **回放脚本**：`MoveTrace { mtStart, mtWaves :: [CascadeWave], mtFinal, mtEnd :: [EndStep], mtGen, mtShuffle }` 是纯数据，与结算结果由同一次 `resolveMove` 计算产出，不写回 `GameState`。`mtFinal` / `mtGen` 是 `ensurePlayable` 之前的稳定盘与生成器：没有自动洗牌时等于结算后的 `gsBoard` / `gsGen`；发生洗牌时 `mtShuffle = Just 洗牌后盘面`，从 `(mtFinal, mtGen)` 重放 `ensurePlayable` 可逐帧复现（`ComboFx` 在最后追加 `StShuffle` 阶段；洗牌**不在** `mtEnd` 里）。前端调用顺序：先 `trySwap` / `use*` 拿结算结果，再用操作前的状态调 `trace*` 拿脚本（同一纯函数的另一投影），`moveFx` 为空时直接丢弃脚本。

## 逐轮回放与规则的同步

第二刀起，结算与回放是**同一份实现**的两个投影，不再需要人工同步：

| 层 | 单一实现 | 结算投影 | 回放投影 | 护栏测试（现在天然成立，保留作回归） |
|----|----------|----------|----------|----------|
| 连锁 | `Match3.Board.Cascade` 的 `cascadeMatchesFrom` / `cascadeSeeds` / `cascadeAfterBelt` / `cascadeCountdowns`（`CascadeRun`：每轮一个 `CascadeWave`，飞碟吸收单独一轮） | `crTally`（`CascadeTally` 记录；旧元组 API `runCascade*` / `resolveCountdowns` / `runPostBeltCascade` 由它投影） | `crWaves`（旧 `traceCascade*` 由它投影） | `trace_cascade_final_equals_stabilized`、`trace_seeds_final_equals_stabilized`、`trace_multi_wave_each_round_visible` |
| 一步操作 | `Match3.Game.Resolve.resolveMove`（交换与三种道具共用；`Move.resolveSwap` / `Boosters.resolve*` 只做校验与起手选择） | `trySwap` / `use*` = 取 `(GameState, Outcome)` | `trace*` = 取 `MoveTrace` | `trace_swap_final_equals_trySwap`、`trace_boosters_final_equal_result`、`trace_rejected_move_is_empty`、`trace_shuffle_step_replays` |
| 步末描述 | 结算直接使用 `Match3.Game.Trace` 的 `traceSpreads` / `traceSnails` 返回的盘面；`applyEndEffect` 把 `EndEffect` 重放回盘面 | — | `mtEnd` | `trace_end_steps_replay_to_trySwap_final`、`trace_end_steps_boosters_replay`、`trace_end_snail_push_and_turn`、`trace_end_spread_from_adjacent_source` |

仍需注意的一处：`beltMoves`（皮带「原格 → 新格」的描述）与 `Conveyor.shiftBelts`（真正移位）仍是两个函数，由 `applyEndEffect` 重放护栏锁定。行为金标准见 [testing.md](testing.md#行为金标准golden)；护栏细节与底线见 [testing.md](testing.md#逐轮回放护栏)；前端时间线见 [ui-art.md](ui-art.md#连击表现逐轮回放)。

## 元素框架与事件

第二刀 2b 起，「某个格子在某个时机怎么反应」不再散落在 Board / Game 各处的 `case` 里，而是由**元素定义**（`ElementDef`）描述、**注册表**（`Registry`）分派。旧的函数名保留为 `defaultRegistry` 下的包装；新增的 `*With reg` 版本接受任意注册表（测试里用它接入样例元素）。

### 一个格子的层

一格自上而下是：叠层（`CellOverlay`，草 / 藤 / 巧 / 雾 / 链 / 冻 / 帘 / 蒸汽）→ 冰层（`iceLayers`）→ 本体（宝石各种类、石头、宝箱……、`Custom 名字 值`）。`Slot` 标出一个定义占哪一层：`SlotCell i` / `SlotOverlay i`（内置，数组下标）、`SlotIce`、`SlotCustom`（按 `Custom` 的名字查表）。查询时按层合成：挡交换 = 任一层挡；点火 = 自上而下第一个 `Just`；直接命中 = 最上面一个非穿透层先吸收。

### `ElementDef` 字段与调用时机

| 字段 | 含义 | 调用点 |
|------|------|--------|
| `edName` / `edSlot` | 元素名（贴图名、计数名、事件里的 `evElement`）/ 所在层 | 注册、分派 |
| `edColor` | 本体颜色（无挡匹配的上层时参与匹配；颜色袋计数） | `Board.Match.groupGemRuns`、`Clear.countColor`、提示 |
| `edBlocksMatch` | 冰 / 叠层盖住的宝石不参与匹配 | `matchColorWith` |
| `edBlocksSwap` | 本格不能被交换 | `Move` / `Boosters` 校验、`findHint` |
| `edActivates` | 特殊块能否点火（软锁纪律） | `Clear.expandSpecials` |
| `edFalls` / `edPortal` / `edDrains` | 随重力下落（否则把列分段）/ 可穿传送门 / 落到底行被收走 | `Board.Gravity` |
| `edOnHit` | 直接命中（锤子、爆炸、十字）：`HitPierce` 穿过 / `HitAbsorb 新格` 吸收 / `HitDestroy` 打碎 / `HitImmune` 免疫 | `Clear.clearWaveWith`、`Ice.chipIceOnClear`、`hammerImmune` |
| `edAdjacent` | 邻格真消除时的反应（`AdjacentRule 顺序 规则`，规则拿到 `AdjCtx{真消除格, 直接命中格, 保护格}`，返回 `AdjOut{新盘, 打碎格, 生成格}`） | `Clear.clearWaveWith`（按 `arOrder` 依次跑） |
| `edStripOnClear` | 本格真消除时叠层随格清掉 | `Clear` |
| `edCounter` / `edDiffCounter` / `edBonusMoves` | 进入清除格计数 / 按步前步后个数差计数 / 每少一个奖励步数 | `Board.Cascade`（`ctNamed`）、`Game.Resolve`、`Game.Tally.diffCountsWith` |
| `edVacatesCarpet` | 离开格子也算覆盖地毯 | `Game.Tally.carpetVacateSeedsWith` |
| `edKeepOnShuffle` | 洗牌时原样放回 | `Game.Shuffle.extractDecorWith` / `ensurePlayableWith` |
| `edBlast` | 被消除且能点火时的爆炸范围 | `Clear.expandSpecials` |
| `edEnd` | 步末规则 `EndRule{erPhase, erOrder, erRun}`：`PhaseTick`（倒计时）→ 皮带（关卡特性）→ `PhaseSpread`（蔓延）→ `PhaseMove`（蜗牛）→ 再连锁 | `Game.Resolve.runPhase`、`Cascade.cascadeCountdownsWith`、`Trace.traceSpreadsWith` |
| `edPlace` | 关卡放置：`Place 名字 参数 坐标` 经它落到格子上 | `Game.Level.decorateLevelWith`（`levelPlacements` 放置表） |

`baseDef name` 是自定义元素的缺省：挡交换、会下落、直接命中免疫、洗牌保留、无邻格 / 步末规则、放置结果为 `Custom name n`——即「注册了但什么都不做」的惰性占格。未注册的 `Custom` 也按它处理。

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

步末规则：countdown（`PhaseTick`）；vine 10 / choco 20 / steam 30（`PhaseSpread`）；snail（`PhaseMove`）。

### 效果事件

`Match3.Game.Trace.traceEvents :: MoveTrace -> [Event]`（`traceEventsWith reg`）把回放脚本按时间线展开成事件，`Event{evKind, evWave, evElement, evCells, evAmount}`：

| `EventKind` | 来源 | `evElement` | `evCells` / `evAmount` |
|-------------|------|-------------|------------------------|
| `EvBlast` | 每轮被点火的直线 / 炸弹 | line_h / line_v / bomb | (特殊块, 覆盖格) |
| `EvClear` | 每轮清除格，按本体名分组 | gem / stone / cookie / … / 自定义名 | (格, 格) |
| `EvHit` | 本轮内容变了但没消掉的格，按最上层名分组 | ice / chain / stone / … | (格, 格) |
| `EvDrain` | 底行收走 | cookie | (格, 格) |
| `EvScore` | 每轮得分 | — | `evAmount` = 分 |
| `EvCombo` | 第 2 轮起每轮一个 | — | `evAmount` = 波次 |
| `EvTick` / `EvBelt` / `EvSpread` / `EvMove` | `mtEnd` 的每个 `EndStep` | countdown / belt / vine·choco·steam / snail | 原格 → 新格 |
| `EvShuffle` | `mtShuffle` | — | — |

前端步末阶段按事件种类查表分派：`ComboFx.endStageTable`（种类 → 阶段与基础时长）、`UI.Playback.endCrumbTable`（阶段 → 粒子）、`UI.EndStage.endStageDrawers`（阶段 → 绘制）、`UI.Layout.elementRGBTable`（元素名 → 颜色）。波次级的高亮 / 消失仍直接读 `CascadeWave`。护栏：`trace_events_consistent_with_trace`。

### 新增一种元素的步骤

1. 选层：本体用 `Custom "名字" 值`（值自定义，例如耐久）；需要新的内置层时才动 `Types`。
2. 写定义：从 `baseDef "名字"` 起，只改需要的字段（例如 `edOnHit`、`edAdjacent`、`edCounter`、`edFalls`）。邻格规则选一个不和现有顺序冲突的 `arOrder`。
3. 注册：`register def defaultRegistry`，把注册表传给 `*With` 入口（`trySwapWith` / `resolveSwapWith` / `resolveHammerWith` / `ensurePlayableWith` / `decorateLevelWith` / `traceEventsWith`）。
4. 放置：在关卡放置表里写 `Place "名字" [参数] [坐标]`，由 `edPlace` 落格。
5. 表现：贴图名即元素名（`assets/` 里放同名贴图，缺图时画灰块）；步末有新效果时在前端各查找表里加一行。
6. 测试：参照 `element_registry_custom_crate_extensibility`（测试专用「木箱」只定义在 `test/Spec.hs`，断言它削层、打碎、计数、挡交换、被锤、洗牌保留，并断言核心源码里没有它的名字）。

仍保留专门分支的元素（第三刀再看）：彩蛋（多轮开启、开出的特殊块要保存）、彩虹（按交换对象取色）、特殊×特殊合成、飞碟（整轮吸收）、皮带 / 传送门 / 地毯（关卡特性，不在格子里）、`LevelGoal` 与 `SpreadKind`（封闭 ADT）、魔法帽 / 染色瓶 / 蜗牛内部仍用 `isGem` / 可推判断；自定义元素目前不能作为可匹配的有色宝石。
