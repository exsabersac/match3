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
               │                        Cascade  Hooks（第 7 刀）  Refill（第 8 刀）
               └──── 直接 import 子模块 ───┤
                                            ▼
 Match3.Element（元素框架：Types / Registry / Builtin / Event / Level / Special（第 8 刀规则表解释器）；默认注册表 defaultRegistry）
 Obstacles Rainbow Combos Ice Grass Carpet Snail Ufo Countdown Conveyor Boosters Daily
 Match3.Levels.Campaign（48 关关卡表 / lookupLevel，第 6 刀） ← Match3.Levels.Level（关卡记录）
 Match3.Types（门面，再导出 Types.Name / Cell / Overlay / Body / Board / Game；第 6 刀拆分）  Match3.Goal（目标数据，第 5 刀）
   └─ Match3.Counts（计数键与 Counts，第 4 刀） ← Match3.Color（颜色，第 5 刀从 Types 拆出）
 （纯函数机制模块；无 IO）

 Engine.Game / Engine.Effect / Engine.Playback（通用层：不 import 任何 Match3 模块）
```

**依赖方向（硬约束）**

- 纯核心 / library ← 应用：`match3-sdl` 依赖 `match3` 库；库**不**依赖 SDL。`app/` 里的模块通过 `Match3.Core`（类型与查询）和 `Match3.Engine`（执行动作）使用规则。
- 通用层 ← 具体游戏：`Engine.*` 与 `app/Shell/Loop.hs` 不 import 任何 `Match3` 模块；`Match3.Engine` 实现通用接口，三消前端作为插件接入外壳（见[多游戏接口](#多游戏接口)）。测试 `engine_layer_is_game_agnostic` 检查这一方向。
- `Core` 把各子模块符号汇总导出，便于前端与测试只 import 一处。第三刀删掉了 `Match3.Board` / `Match3.Game` 两个外观模块：Game 子模块、`Core`、测试直接 import 各子模块。
- 子模块之间单向依赖、无环（下文 `A ← B` 表示 B 依赖 A）：Board 内 `Grid ← Match ← Clear`、`Grid ← Gravity`、`Match ← Random`，`Cascade` 依赖 Grid / Match / Clear / Gravity；Game 内 `State ← Outcome / Shuffle / Trace`、`Shuffle ← Level`，`Resolve`（公共结算）依赖 State / Tally / Outcome / Shuffle / Trace，`Move` / `Boosters` 只做校验与起手选择、依赖 `Resolve`。Game 子模块直接 import 所需的 Board 子模块。
- 元素框架 `Match3.Element.*` 位于 Board / Game 之下：`Element.Event ← Element.Types`、`Element.Message` → `Element.Class`（元素类；`SomeElement` / `SomeModifier` 的相等第 6b 刀起按具体类型（`Typeable` 的 `cast`）+ 该类型的 `Eq`，不再比较名字字符串）→ `Element.Registry` → `Element.Builtin.*`（按功能分组的 instance）→ `Element.Builtin`（汇总）；各分组里的 instance 调用各机制子模块实现具体反应。Board / Game 只通过注册表查询「这个格子怎么反应」，不再按构造器写死（见[元素框架与事件](#元素框架与事件)）。
- 关卡级元素（第 7 刀）：`Element.Class`（`SomeLevelElement`）← `Board.Hooks`（钩子记录 `LevelHooks`，只把元素列表当不透明载荷）← `Board.Gravity` / `Board.Cascade`；`Element.Registry` / `Element.Builtin.Level` / `Board.Hooks` ← `Element.Level`（开局、节拍、造钩子）← `Board.Default`（`builtinHooks`）/ `Game.State` / `Game.Level` / `Game.Resolve`。`Element.Class` 为 `levelStart` 依赖 `Levels.Level`（关卡记录）。
- 步末表（第 7b 刀）：`Board.Cascade` / `Element.Level` / `Game.Trace`（`EndStep`、`traceSpreadsWith`）← `Game.EndPhase`（`runEndTable`）← `Game.Resolve`（`endTableFor`）；步末效果的通用形状在 `Element.Event`。
- 类型层（第 6 刀拆分）：`Match3.Color` / `Types.Name` ← `Types.Cell ← Types.Overlay / Types.Body`、`Types.Cell ← Types.Board ← Types.Game`（`Types.Game` 另依赖 `Match3.Goal`；`Types.Name` 无依赖，`Match3.Counts` 的 `CountNamed` 也用它），`Match3.Types` 只再导出这六个模块（导出列表与拆分前相同，只少了移走的关卡表、多了第 6b 刀的 `ElementName` / `CustomState`）；关卡层 `Levels.Level ← Levels.Campaign` 在 `Types` / `Element.Types`（放置表）/ `Ufo` / `Conveyor`（`Belt`）之上、`Game.Level` / `Daily` / `Game.Outcome` 之下。
- 机制子模块（障碍、彩虹、合成、冰、草系、地毯、蜗牛、飞碟、倒计时、传送带、道具种子、每日）尽量只依赖 `Types`（及彼此必要的窄依赖），由 `Board.*` / `Game.*` 编排调用顺序。
- 回放方向单向：`Match3.Game.Resolve.resolveMove`（经 `Match3.Board.Cascade` 的记录版连锁）与结算结果一起产出 `MoveTrace` → `app/pure/ComboFx.hs`（纯阶段机，按时间线把它拆成帧；帧数读表现表 `UI.Presentation`）→ `UI.Playback`（阶段事件 → 弹字 / 粒子 / 震屏 / 音效队列）→ `UI.Cascade` / `UI.EndStage`（绘制，颜色 / 贴图读表现表）。核心**不**知道帧、阶段或样式；`ComboFx` 不 import SDL，也不调用 `trySwap` 等规则入口，只读 `MoveTrace` 与前端传入的结算后盘面。

## 模块地图

### 核心库（`src/Match3/`）

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Match3.Types` | 门面（第 6 刀起无实现）：再导出下面六个 `Types.*` 模块、`Match3.Color` 与 `Match3.Goal` 的 API，导出列表与拆分前相同（关卡表移到 `Match3.Levels.*`） | 连锁、交换、IO |
| `Match3.Types.Name` | 第 6b 刀：`newtype ElementName`（元素名：注册表 / 放置表 / 地面层 / `Custom` 格 / 效果事件 / `CountNamed` 的键；有 `IsString`）与 `newtype CustomState`（`Custom` 格的状态值）；两者的 `Show` 手写成与底层 `String` / `Int` 相同，`Cell` / `GameState` 的 `Show` 因此逐字不变 | 名字 → 元素的查表（`Element.Registry`） |
| `Match3.Types.Cell` | `GemKind` / `CellOverlay` / `CellContents`（含 `Custom ElementName CustomState`，供注册表扩展元素）/ `Cell`，宝石构造与格子取值（`mkGem` / `cellColor` / `cellKind` / `isGem` / `isCustom` …） | 叠层与本体的具体构造器 |
| `Match3.Types.Overlay` | 叠层（草 / 藤 / 巧 / 雾 / 链 / 冻 / 帘 / 蒸汽）的构造、谓词与层数，`setOverlay` / `clearOverlay` | 叠层的清除与蔓延（`Match3.Grass`） |
| `Match3.Types.Body` | 本体（石头 / 宝箱 / 蜂蜜 / 气球 / 饼干 / 蛋糕 / 魔法帽 / 果汁机 / 蜗牛 / 保险箱 / 双面 / 彩蛋 / 瓶子 / 精灵 / 倒计时）的构造、谓词与层数 | 本体的反应（`Element.Builtin.*`） |
| `Match3.Types.Board` | `Pos`、`Board = Board (Array (Int,Int) Cell)`（O(1) 读格，`boardFromRows` / `boardRows` / `boardAt` / `boardSet` / `mapBoard` 等；`Show` 按行列表打印，与旧列表盘输出相同）、`boardSize` | 可变盘（`Board.Grid`） |
| `Match3.Types.Game` | `Score` / `MovesLeft` / `TargetScore`、`Outcome`、地面层 `Ground`、`GameConfig` / `defaultConfig` | 关卡表 |
| `Match3.Levels.Level` | 第 6 刀：关卡记录 `Level`（`lvlIndex` / `lvlName` / `lvlMoves` / `lvlGoal` 与原先分散在按下标 case 的并行表里的 `lvlPlacements` / `lvlBelts` / `lvlPortals` / `lvlUfos` / `lvlCarpets` / `lvlGround`；另有 `lvlRules` 规则开关、新玩法 6 的掉落口 `lvlDrops :: [DropSpec]`）、`level`（不带装饰的关）、`levelConfig`、放置表辅助 `placeEach` / `layersAt` | 放置表的解释（`Game.Level`） |
| `Match3.Levels.Campaign` | 第 6 刀：48 关 `allLevels`（2026-09-30 起新玩法关卡追加在末尾）（每关一条完整记录）、`lookupLevel :: Int -> Maybe Level`（取代各处的 `allLevels !! i`）、`levelCount`、`clampLevelIndex`、`levelCarpets` | 开局（`Game.Level`） |
| `Match3.Color` | 第 5 刀：`Color`（`C1`–`C5`）与 `allColors`，从 `Types` 拆出，让 `Counts` 能有颜色键而不成环 | 颜色的显示 |
| `Match3.Counts` | 第 4 刀：计数键 `CounterKey`（内置 8 个元素键 + `CountUfo` / `CountCarpets` + `CountNamed 名字`，第 5 刀加 `CountColor 颜色`）与 `Counts`（`Map CounterKey Int` 的 newtype，稀疏、不存 0；`countOf` / `bumpCount` / `plusCounts`（也是 `<>`）/ `countsFromList` / `countsToList` / `namedCounts` / `colorBag`）；`GameState.gsCounts` 与 `CascadeTally.ctCounts` 都是它（第 5 刀起颜色袋也在里面） | 哪个键算哪个目标（`Match3.Goal`） |
| `Match3.Goal` | 第 5 刀：目标数据 `LevelGoal { goalQuotas :: [Quota] }`，`Quota { quotaMeter :: Meter, quotaTarget :: Int }`，`Meter = MeterScore \| MeterCount CounterKey`；构造函数 `goalScore` / `goalCollect` / `goalColors` / `goalCount`；统一计算 `goalProgress` / `goalMet` / `goalTarget` / `meterValue`；前端分派用的形状 `goalView :: LevelGoal -> GoalView`（`ViewScore` / `ViewCollect` / `ViewCollectMulti` / `ViewCount 键` / `ViewOther`）；手写 `Show` 按第 5 刀前的构造器写法打印 | 图标 / 文案（前端 `UI.GoalStyle`） |
| `Match3.GoalLabel` | 目标的中文显示名（合 main 9f5504e 后从 `Match3.View` 下移）：`goalViewLabel :: GoalView -> String`、`countLabel`、`colorLabel`、`namedGoalLabelTable`（名字目标登记表：果冻 / 气泡 / 魔法石 / 毛球 / 雪怪 / 变色龙）。`Match3.View` 重新导出并用它定义 `goalLabel`（对外 API 不变）；`Game.Outcome.loseHint` 与桌面标题目标段 `goalLine` / `goalBracket` 也读它，所以放在 `Game.*` 之下、`View` 之上 |
| `Match3.Element` | 门面：再导出 `Types` / `Registry` / `Builtin` / `Event` / `Level`（第 7 刀）、`Special` / `Board.Refill`（第 8 刀；元素类 `Class`、能力声明 `Caps`（第 9 刀）与 `Message` 单独 import，查询名 `name` / `color` / `pushable` 等较通用，避免与使用方撞名） | 自身无实现 |
| `Match3.Element.Types` | 规则与查询结果的数据类型：`Slot`、`HitResult`、`AdjacentRule` / `EndRule` / `SwapRule` / `OpenRule`、第 8 刀的特殊块形状规则 `ShapeRule { shapeName, shapeSpawn :: ShapeCtx -> MatchRun -> Maybe [(Pos, Cell)] }`（上下文 `ShapeCtx { scPrefer, scRuns, scClearable }`）与组合规则 `ComboRule { comboName, comboFirst, comboSecond, comboSeeds }`、连线 `MatchRun`（第 8 刀从 `Board.Match` 移来，原处再导出）、`CounterKey`（再导出自 `Match3.Counts`，第 4 刀前叫 `Counter`）、`Arg` / `Placement`、`cellSlot` | 调用顺序 |
| `Match3.Element.Registry` | `Registry`（名字 → 构造器 `Entry`；按层数组 O(1) 取解码器 + 自定义元素表 + 已排序的邻格 / 步末规则）、`register` / `lookupElement`、解码 `elementOf`、各能力的查询函数 `*With`（新玩法 5 起另有 `weighElementWith`：按元素名对盘面求 `diffWeight` 之和）、关卡级元素的种类表 `registerLevel` / `removeLevel` / `levelDefs` / `askLevels`（问注册的原型值，第 7 刀 7b 起折叠所有回复者；第 7 刀删掉了带状态参数的 `absorbWith` / `beltShiftWith` / `teleportWith` / `coverWith`）；第 8 刀的三张规则表 `shapeRules` / `setShapeRules`、`comboRules` / `setComboRules`（非空时并成一条次序 20 的成对交换规则，`swapRules` = 元素声明的 `elementSwapRules` + 它）、`refillPolicyWith` / `setRefillPolicy`（`mkRegistry` 建出的表：形状 / 组合为空，补子 = `defaultRefill`；`register` 保留三张表） | 具体元素、一局的关卡级状态（`Element.Level`） |
| `Match3.Element.Builtin` | 汇总：条目表 `builtinDefs`（注册顺序固定，快照锁定）、`builtinLevelDefs` 与 `defaultRegistry`（第 8 刀起另装上 `builtinShapeRules` / `builtinComboRules`，均再导出）；再导出测试 / 扩展用的元素类型 | 具体元素的定义 |
| `Match3.Element.Builtin.Gem` | 宝石：`PlainGem`、`SpecialGem`（直线 / 炸弹 / 彩虹），彩虹取色的成对交换规则、`specialBlast`、第 8 刀的内置形状规则表 `builtinShapeRules`（特殊合成不再挂在 line_h 上）；新玩法 1 的 L / T 形规则 `ltBombRule` 与插表函数 `withBombShapes`（不在内置表里，由规则开关 `BombShapes` 按关插入） | 障碍与叠层 |
| `Match3.Element.Builtin.Layer` | 冰层 `Ice` 与 8 种叠层修饰器（草 / 藤 / 巧 / 迷雾 / 锁链 / 火箭冰冻 / 窗帘 / 蒸汽），蔓延规则 | 本体 |
| `Match3.Element.Builtin.Obstacle` | 打破型障碍：石头、宝箱、蜂蜜、蛋糕、气球、保险箱、双面块、彩蛋；魔法石（新玩法 2）；雪怪 Boss `SnowBoss`（新玩法 5：2×2 四格 `Custom "snow_boss"`，邻格规则 200 扣血、步末 `PhaseMove` 30 召唤雪块，`snowBosses` / `snowBossHp` / `snowBossSpawn` / `decodeBoss`） | 收走 / 按名字计数的元素（`Collectible`） |
| `Match3.Element.Builtin.Collectible` | 收集与计数类：饼干、时间精灵、气泡；变色龙 `Chameleon`（新玩法 7：`Custom "chameleon" k`，普通棋子原型、按当前颜色匹配；步末 `PhaseMove` 40 换色 `chameleonShift`、彩虹 × 变色龙成对规则 15；`chameleonCell` / `chameleonColor` 给前端用） | 削层 / 变形（`Obstacle`） |
| `Match3.Element.Builtin.Actor` | 会动或会生成东西的：魔法帽、果汁机、蜗牛（含 `traceSnails`）、染色瓶、倒计时 | 被动障碍（`Obstacle`） |
| `Match3.Element.Builtin.Ground` | 地面层：果冻；魔法地格 `MagicGround`（新玩法 8：`"magic"`，没有 `groundRule`，不被消耗、不计数；能力 `widens magicWiden`——特效在这一格上引爆时爆炸范围扩一圈，`magicWiden` = 原范围 ++ 八邻格按行优先） | 占格本体 |
| `Match3.Element.Builtin.Level` | 关卡级元素：飞碟 `UfoLevel [Ufo]`、皮带 `BeltLevel [Belt]`、传送门 `PortalLevel [(Pos,Pos)]`、地毯 `CarpetLevel [Pos]`、地面层 `GroundLayer Ground`、规则开关 `BombShapes` / `RainbowCombos`、掉落口 `CookieDrop [DropSpec]`（新玩法 6：回复 `Refilling`，把补子策略包一层 `dropRefill`；新玩法 7 起名额按「同种」数：`Custom` 按名字、其余按相等）（第 7 刀：状态在元素值里；按节拍消息回复并交回推进后的自身，开局状态 `levelStart` 取自关卡记录，飞碟 / 地毯的目标补齐也在这里）；`portalTeleport`（第 7 刀前在 `Board.Gravity`） | 一局里有哪些元素（`GameState.gsLevelElems`） |
| `Match3.Element.Level` | 第 7 刀（7a）：一局的关卡级元素 `gsLevelElems`——开局 `startLevelsWith`（注册的各种 + 核心元素地面层）、每个节拍参与的元素 `activeLevels`（注册顺序取同名状态，没有则用原型；未注册的不参与，核心元素总参与）、`askLevelsIn`（发消息，第 7 刀 7b 起按参与顺序折叠所有回复者并依次写回推进后的状态）、读写 `levelState` / `putLevel` 与内置读数 `levelUfos` / `levelBelts` / `levelPortals` / `levelCarpetOpen` / `levelGround` / `levelDrops`（新玩法 6：掉落口格）、给 Board 层的钩子 `levelHooksWith`、本步注册表 `levelRegistryIn`（`Shaping` 换形状表；新玩法 8：地面层里有带 `widenRule` 的格时再 `setWidening`，没有时注册表原样）、Game 层的节拍 `beltShiftIn` / `avoidCellsIn` / `wallCellsIn` / `coverIn` / `hitGroundIn` | 连锁顺序（`Board.Cascade`） |
| `Match3.Element.Builtin.Common` | 跨分组共用的辅助：`deadRule`（邻消打碎并入清除格）、`colorPlace`（按颜色放置） | 只在一组里用的辅助 |
| `Match3.Element.Special` | 第 8 刀：规则表的解释器（不含具体规则）——形状表 `spawnByShapes`（每条连线取第一条认领它的规则）、落点 `shapeAnchor`、单连线规则的构造器 `runShape`；组合表 `comboMatch`（按表顺序、每条先试 (p1,p2) 再试 (p2,p1)）/ `comboFires`（另要求两端 `specialActivates`）/ `comboSeedsFor` / `comboSwapRule`（次序 `comboOrder` = 20） | 具体规则（`Builtin.Gem` / `Combos`） |
| `Match3.Element.Class` | 元素类（xmonad LayoutClass 风格）：`Element`（第 9 刀起只剩 `name` / `toCell` / `caps` 三个方法）、能力记录 `Caps`（五组带默认值的记录 `MatchCaps` / `HitCaps` / `MoveCaps` / `CountCaps` / `StepCaps`，按原型的缺省 `capsOf`）与 27 个同名查询函数（第 9 刀前是类方法，签名不变）、`SomeElement`、修饰器 `Modifier` / `Modified`、惰性占格 `Inert`、关卡级元素 `LevelElement`（`levelName` / `levelReply` 返回（回复，推进后的自身）/ `levelStart` / `levelCore`）与存在类型 `SomeLevelElement`（第 7 刀前的 `SomeLevel` 没有状态；相等 = 同类型且值相等） | 具体元素 |
| `Match3.Element.Caps` | 第 9 刀：写元素用的能力声明——`piece` / `blocker` / `fixed :: [Cap] -> Caps`（按原型的缺省能力再依次应用声明）、每项能力一个简写（`colorIs` / `swappable` / `hit` / `breaks` / `onAdjacent` / `teleports` / `counts` / `atEnd` / `onMessage` …，见「元素的能力」）、按组直接改字段的 `withMatch` / `withHit` / `withMove` / `withCount` / `withStep`；再导出 `Element.Class` | 内置本体、测试 / 扩展元素 |
| `Match3.Element.Message` | 开放消息 `Message` / `SomeMessage` / `fromMessage`；流水线节拍消息（第 7 刀起问题与回复同类型，回复者在上面累加：`Refilled`、`Refilling`（第 8 刀，补子策略）、`Shaping`（新玩法 1，本关形状表）、`Morphing`（新玩法 4，交换变身，回复 `Morph`）、`EndTicked`、`Settling`、`Covering`、`GroundHit`，查询 `AvoidCells` / `WallCells`；第 7 刀前的 `Absorbed` / `Shifted` / `Settled` / `Covered` 已删） | 谁回复 |
| `Match3.Element.Event` | 通用步末效果 `EndEffect { endEffectKind, endEffectElement, endEffectItems }` / `EndItem { eiFrom, eiTo, eiCell, eiBack }`（第 7 刀 7b 取代四个构造器与 `SpreadKind` / `SnailMove`；`Show` 手写成旧构造器文本）、`applyEndEffect` / `endEffectPairs` / `endItemDir` / `spreadPairs`、效果事件 `EventKind` / `Event` | 帧与样式 |
| `Match3.Core` | 再导出公共 API | 自身几乎无逻辑 |
| `Match3.Board.Grid` | 坐标边界、读写格（`getCell` = `boardAt`，O(1)）、交换、相邻、可空盘面 `MBoard = Array Pos (Maybe Cell)`（第 3 刀起与 `Board` 同形的二维数组，`atM` / `setM` / `setManyM` 读写、`mboardRows` 转行列表；只在一轮消除 / 沉降内部使用，重力按列取出不再转置）、`randomColor` | 任何规则 |
| `Match3.Board.Match` | `MatchRun`（第 8 刀起定义在 `Element.Types`，这里再导出）/ `findMatchRuns` / `hasAnyMatch`、`findHint` / `hasValidMove`（第 3 刀起提示只对交换两格所在的行 / 列做局部匹配检查，其余行列用原盘的结果；遍历顺序与返回值不变，性质 `qc_find_hint_local_matches_reference` 与旧实现对照） | 修改盘面 |
| `Match3.Board.Clear` | 一轮消除（匹配 / 种子）、特殊扩展与生成（第 8 刀起 `spawnSpecialsWith reg` 查注册表的形状规则表；不带 With 的 `spawnSpecials` 移到 `Board.Default`）、彩蛋、邻格削层与触发、飞碟吸收（吸走 ≠ 引爆）、计分公式 | 沉降、连锁循环 |
| `Match3.Board.Gravity` | 重力（固定格分段）、边缘收集（`drainEdgesMWith`：按元素的 `drains` 方向，底 → 左 → 右 → 上，收完再落，内置只有饼干 = 底边）、沉降节拍的钩子（`onSettle`，内置 = 传送门）、补子（第 8 刀起按补子策略：`activeRefill reg hooks` = 钩子 `hookRefill` 优先、否则注册表的策略；`refill` = 缺省策略）、`settleRefill` / `settleDrainWith`（第 7 刀起收 `LevelHooks`，不再收传送门对） | 消除 |
| `Match3.Board.Refill` | 第 8 刀：补子策略 `RefillPolicy { refillName, refillCell :: RandomGen g => RefillCtx -> g -> (Cell, g) }`（上下文 `RefillCtx { rcPos, rcBoard }`）、缺省 `defaultRefill`（随机五色普通宝石，每洞一次 `randomColor`）、关卡颜色数 `colorsRefill n`、`refillWith`（行优先逐个空洞问策略）；不依赖注册表 | 策略从哪来（`Gravity.activeRefill`） |
| `Match3.Board.Hooks` | 第 7 刀（7a）：Board 层的关卡级钩子记录 `LevelHooks { onSettle, onAbsorb, hookRefill（第 8 刀）, hookLevel }` 与空钩子 `noHooks`；由 `Element.Level.levelHooksWith` 从注册表 + `gsLevelElems` 造出，Board 层不看背后是哪些元素 | 关卡级元素本身 |
| `Match3.Board.Cascade` | 连锁的**单一实现**：`cascadeMatches` / `cascadeMatchesFrom` / `cascadeSeeds` / `cascadeAfterWith`（`AfterBelt` / `AfterEnd 空洞`）/ `cascadeCountdowns` 返回 `CascadeRun`（终盘 + `CascadeTally` 计数记录 + `[CascadeWave]` + 推进后的钩子 `crHooks`（第 7 刀前是飞碟 `crUfos`）+ 生成器；第 7 刀起全部入口只收一个 `LevelHooks`，不再收 `[Ufo]` / 传送门对），调用方直接读字段（第三刀删掉了元组兼容层 `runCascade*` / `resolveCountdowns` / `runPostBeltCascade` 与 `traceCascade*`）；`stepCascade` 保留为「恰好一轮」的小工具；段 2c 的步末补结算（挖 `erHoles` 空洞 → 边缘收集 + 补子 → 再连锁）与皮带后连锁在第 3 刀合并为 `cascadeAfterWith`；每轮的「沉降 + 补子」只在 `settleRound`、整轮吸收（飞碟）只在 `absorbRound` 各写一次 | 步数 / 目标结算、道具扣次 |
| `Match3.Board.Default` | 段 2c：不带 `With` 的旧名（`cascadeMatches` / `clearMatches` / `applyGravity` / `findHint` …）= `*With defaultRegistry`；第 7 刀起连锁 / 沉降的旧名也收 `LevelHooks`，`builtinHooks 飞碟 传送门对` 造出内置注册表下的钩子（再导出 `LevelHooks(..)` / `noHooks`）。`Board.{Match,Clear,Gravity,Cascade}` 自身不再 import `Element.Builtin`，只收 `Registry` 参数；主流程一律把 `reg` 往下传，不经本模块 | 规则 |
| `Match3.Board.Random` | 随机盘、稳定盘、可玩盘、`shufflePlayable` | 保留装饰（见 `Game.Shuffle`） |
| `Match3.Game.State` | `GameState`（段 3 起不含撤销历史；第 7 刀起关卡级元素收在 `gsLevelElems :: [SomeLevelElement]`，第 7 刀前的 `gsBelts` / `gsPortals` / `gsUfos` / `gsCarpetOpen` / `gsGround` 改为派生读数，写入用 `setLevelElem` / `setUfos` / `setBelts` / `setPortals` / `setCarpetOpen` / `setGround`；`Show` 仍按旧字段名、旧位置打印，内置之外的元素才追加 `gsLevelExtra`）、`MoveFx` / `moveFx` / `clearMoveFx`（边沿触发）、`applyHint` / `applyHintWith` | 结算、撤销历史（在 `Engine.History`） |
| `Match3.Game.Tally` | 结算计数辅助：颜色袋、保险箱 / 时间精灵计数（`diffCountsWith` 按步前 / 步后盘面的加权个数差，新玩法 5 起经 `weighElementWith`，权重缺省 1 时即个数差）、地毯腾空格 | 结局判定 |
| `Match3.Game.Outcome` | 目标满足、`decideOutcome`、`checkOutcome`、选关解锁、地图跳转、失败提示 | 盘面 |
| `Match3.Game.Shuffle` | 保装饰洗牌 `shuffleGame`、自动洗牌 `ensurePlayable` | 回放（洗牌不在 `mtEnd`） |
| `Match3.Game.Level` | 开局 / 每日 / 重开 / 下一关（越界的关卡下标夹到关卡表范围）、`campaignGame :: Int -> Int -> Maybe GameState`、按关卡记录铺装饰 `decorateLevel`、关卡级元素由 `startLevelsWith` 按记录开出（第 7 刀；`newGameAtLevelWith reg` 用指定注册表开局）、目标补齐 `ensureGoalDecor`（新玩法 6 起有掉落口 `lvlDrops` 的关卡跳过）、步数携带；放置表返回 `Either PlaceError`，静态数据在 `placeStatic` 这一处转成带关卡名的 error | 关卡数据（`Levels.Campaign`）、走步 |
| `Match3.Game.Trace` | `MoveTrace`（含 `mtGen` / `mtShuffle`）/ `EndStep`（`EndEffect` 等再导出自 `Element.Event`）、`traceSpreads`（跑注册表的蔓延规则）、`beltMoves`（再导出自 `Conveyor`）、效果事件 `traceEvents` | 结算 |
| `Match3.Game.EndPhase` | 第 7 刀 7b：步末表 `EndStage { stageName, stagePhase, stageRun }`、累积器 `EndAcc`、`swapEndTable`（tick → belt → spread → move → settle → vacate）/ `boosterEndTable`（vacate → spread → settle）、`runEndTable`、各行 `tickStage` … `vacateStage`、`runPhase` | 计数、结局（在 `Resolve`） |
| `Match3.Game.Resolve` | 交换与三种道具的**公共结算** `resolveMove`：主连锁 → 步末（按 `endTableFor` 选的 EndPhase 表执行）→ 计数与目标 → 结局 → 自动洗牌，同时产出 `MoveTrace` | 入口校验（在 `Move` / `Boosters`）、步末各阶段（在 `EndPhase`） |
| `Match3.Game.Move` | `resolveSwap`（校验 + 起手选择；新玩法 4 起先问关卡级元素的交换变身 `morphIn`，有回复时起手为 `OpenMorph`）及其投影 `trySwap`（= `runMove`）/ `traceSwap` | 道具 |
| `Match3.Game.Boosters` | `resolveHammer` / `resolveFreeSwap` / `resolveCrossClear` 及其投影 `use*` / `trace*` | 种子几何（见 `Match3.Boosters`） |
| `Match3.Obstacles` | 石头/宝箱/蜂蜜/蛋糕/保险箱/气球/彩蛋/瓶子/精灵/魔法帽/果汁机的邻消削层与触发 | 连锁循环 |
| `Match3.Rainbow` | 彩虹判定与清色种子 | 合成几何（见 Combos） |
| `Match3.Combos` | 特殊×特殊合成：第 8 刀起是内置组合表 `builtinComboRules`（炸弹 × 炸弹 → 直线 × 直线 → 直线 × 炸弹 → 彩虹 × 直线），`isSpecialCombo` / `comboClearSeeds` 是这张表的判定 / 清种子；新玩法 4 的 `rainbowComboMorph`（彩虹 × 直线 / 炸弹的变身格与种子，只经规则开关 `rainbow_combos` 用）；各组合的种类谓词与爆炸几何 `bigBomb` / `fullRowCol` / `lineBombCross` | 普通三消、组合表的解释（`Element.Special`） |
| `Match3.Ice` | 匹配时削冰层 | overlay（Freeze/Chain…） |
| `Match3.Grass` | 草/藤/巧/雾/链/冻/帘/蒸汽的清除与蔓延 | 蜗牛爬行 |
| `Match3.Carpet` | 地毯覆盖计数（各关的地毯布局第 6 刀起在关卡记录 `lvlCarpets` 里） | 饼干底行收集逻辑（在 `Board.Gravity` / `Game.Tally`） |
| `Match3.Snail` | 蜗牛一步爬行 / 掉头 | 步末其它效果编排 |
| `Match3.Ufo` | 飞碟吸色目标与移格 | 棋盘清除（由 `Board.Clear` 掩码后清） |
| `Match3.Countdown` | 倒计时 tick / 归零爆炸种子 | 爆炸后连锁（`Board.Cascade`） |
| `Match3.Conveyor` | 传送带移位的**单一实现**：`beltMoves`（「原格 → 新格」描述）→ `applyBeltMoves`（按描述移位）；`shiftBelts = applyBeltMoves b (beltMoves belts)`，结算、回放描述与 `applyEndEffect` 重放共用 | 移位后再连锁（`Game.Resolve`） |
| `Match3.Boosters` | 锤子/十字**种子位置**（纯几何） | 扣次数与连锁（`Game.Boosters`） |
| `Match3.Daily` | 日期种子、每日配置、三星公式 | 每日盘面装饰（`Game.Level`） |
| `Match3.Engine` | 三消作为通用接口的实现：`Action`（交换 / 锤子 / 自由交换 / 十字 / 提示 / 洗牌）、`Setup`、`play`（一次结算得到 `Played`：状态 / `Outcome` / `MoveTrace` / `MoveFx` / 事件 / 提示，作为 `gameStep` 的 `stepReport`；新玩法 8 起效果事件按本步注册表 `levelRegistryIn` 展开，`EvBlast` 含魔法地格扩出来的一圈）、`match3Game`、撤销规则 `match3History`、外壳实例 `match3Shell = withHistory match3History match3Game`、`toEffect` | 帧与绘制、撤销历史的存放 |
| `Match3.View` | 第 11 刀：视图模型（纯函数）。`gameView :: GameState -> GameView`（关卡 / 夹紧下标 / 关名、每日、分数、步数与步数上限、道具 `Boosters`、连击（经 `gameStatus`）、洗牌 / 结局 / `PlayStatus`、目标 `GoalInfo`、棋盘 `BoardView`）、`titleLine` / `goalLine` / `goalBracket`（标题与提示文字）、`carpetAt` / `groundAtView`（逐格底层）、`levelDots`（进度点）、`scoreBadge`（回放 / 连击总结 / 得分徽章）、`levelViews`（关卡列表）、`cellFace`（单格结构化描述）、文字标签 `countTag` / `colorTag`；新玩法 5 起 `gvBoss :: Maybe BossView`（目标是「击败 Boss」时的剩余 / 满血）与 `bossPart :: Cell -> Maybe BossPart`（雪怪格的象限 / 受伤 / 召唤进度）；桌面版与网页版都读它，见[视图模型](#视图模型第-11-刀) | 坐标、颜色、贴图（前端） |

### 通用层（`src/Engine/`）

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Engine.Game` | 通用游戏接口 `Game cfg s a e o r`（record-of-functions；`r` 是整步报告）、`Step`（含 `stepReport :: Maybe r`）、`runActions` / `finalState` / `stepEffects` / `rejectedStep`、种子约定 | 任何具体规则 |
| `Engine.History` | 段 3：通用撤销历史 `History{histNow, histPast}`、`Undoable a = Act a \| Undo`、`HistoryPolicy{hpLimit, hpRecord, hpSnapshot, hpRestore}`、`withHistory`（给任意 `Game` 套一层撤销；终局后仍可撤销）、`pushHistory` / `replaceNow` / `undoHistory` / `commitStep` | 哪些动作算走步（由游戏的 policy 给出） |
| `Engine.Effect` | 通用效果事件 `Effect{efBeat, efKind, efSubject, efSpots, efAmount}`、按节拍分组 `beats` | 帧数与样式 |
| `Engine.GridUI` | 第 11 刀：通用网格 UI 组件（不绑定三消）：网格几何 `GridGeom{ggLeft, ggTop, ggCell, ggRows, ggCols}` 与 `gridCellAt`（像素 → 格）/ `gridCellOrigin`（格 → 像素）/ `gridCells`（行优先）/ `orthoAdjacent`；两步点选 `gridClick :: Maybe c -> c -> Click c`（`ClickSelect` / `ClickDeselect` / `ClickPair`）；拖动松手 `gridDragRelease adj from mTo`；高亮集合 `Highlight{hlSelected, hlHint, hlFlash, hlPinned}` 与 `isSelected` / `isHinted` / `isFlashing` | 什么算合法交换（由游戏传入相邻判定）、绘制 |
| `Engine.Playback` | 纯播放层：阶段机 `Stages`、播放器 `Player`（帧号 / 加速）、`stepPlayer` / `playerProgress` / `runPlayer`；固定队列 `Cue` / `cueStages` / `effectCues` | 阶段内容（由游戏给出）、SDL |

### 前端模块（`app/`）

第 10 刀起 `app/pure/` 放**不依赖 SDL 的纯前端模块**（`ComboFx`、`UI.Presentation`、`UI.Sound`）：可执行文件、测试套件（`package.yaml` 的 test `source-dirs` 含 `app/pure`）与网页版（`web/match3-web.cabal` 的 `hs-source-dirs` 含 `../app/pure`）共用同一份源码；其余 `app/` 模块只进可执行文件。

| 模块 | 职责 |
|------|------|
| `Main` | 入口：读环境变量（种子 / 起始关 / 展示盘 / 窗口倍数），`runShell (match3ShellConfig o) (match3Plugin o)` |
| `Shell.Loop` | 通用 SDL 外壳（不 import Match3）：初始化、HiDPI 窗口、渲染器、alpha 混合、固定步长主循环（帧首钩子 → 取事件 → 事件钩子 → 推进 → 绘制 → present → 补足 16 ms）、`Plugin` 钩子 |
| `UI.Plugin` | 三消插件：初始 `App`（开局提示 / 展示盘）、加载贴图、各钩子接到 `syncScale` / `foldEvents` / `tickAnim` / `draw` |
| `UI.Types` | `App`、`Anim`（`AnimCascade` 持有 `Player Cascade`）、`Particle`、`ToolMode`，帧数常量，`animBusy` / `playingPlayer` / `playingCascade`，第 11 刀起 `appHighlight :: App -> Highlight Pos`（选中 / 提示 / 闪光 / 自由交换第一格） |
| `UI.Layout` | 逻辑像素布局常量、格子坐标换算（第 11 刀起 `boardGrid :: GridGeom CInt`，`pixelToCell` / `cellOrigin` / `allCells` 签名不变、经 `Engine.GridUI` 算）、矩形 / 插值工具、调色板（`elementRGBTable` / `smoothT` / `easeOutT` 第 10 刀起定义在 `UI.Presentation`，这里再导出） |
| `UI.Env` | 环境变量（`MATCH3_LEVEL` / `SEED` / `SCALE` / `SHOWCASE`）、展示盘、高分屏倍率与鼠标坐标换算 |
| `UI.Input` | 输入映射：`handleEvent` 分派到 `handleKey`（每键一个函数）/ `handleMouseUp`（拖拽交换）/ `handleMouseDown`（地图 / 加速 / 结束浮层 / 点格）；点选与拖动判定经 `Engine.GridUI.gridClick` / `gridDragRelease`；规则一律经通用接口 `gameStep`（`UI.Actions.stepShell`，实例 `Match3.Engine.match3Shell`；撤销是 `Undo`，由 `Engine.History` 处理）；播放锁定（见 [ui-controls.md](ui-controls.md#播放锁定animbusy)） |
| `UI.Actions` | 标题栏（第 11 刀起 = `Match3.View.titleLine` + 提示消息）、`stepShell`（外壳执行动作的唯一入口：`gameStep M3E.match3Shell`）、`playMove` / `playbackOf`（一次 `gameStep` 的整步报告 → 表现编排）、关卡重置、三种道具执行、回放加速、过关前进 / 重试 |
| `UI.Playback` | 纯函数：每帧推进动画；按本次 `MoveFx` / `MoveTrace` 编排回放，阶段事件产生弹字 / 浮字 / 震屏 / 粒子；步末碎屑按表现表的 `prCrumbs` 解释（`endCrumbs`）；音效名排进 `appSounds` |
| `UI.Draw` | 一帧的层次与贴图 / 几何分派 |
| `UI.Cascade` | 静止盘、交换补间、轻落、逐轮回放（高亮 / 消失 / 下落）、震屏视口 |
| `UI.EndStage` | 步末阶段绘制：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌（绘制表 `endStageDrawers` 按 `StageKind` 查；颜色、光效贴图、蔓延生长曲线读表现表） |
| `UI.BoardArt` | 棋盘贴图绘制与分派（`drawCellAny` / `drawCellArt` / `drawStatic` 等）、回放共用的底盘部件（地毯 / 地面层 / 皮带 / 传送门读 `BoardView`，选中 / 提示 / 闪光读 `appHighlight`）；新玩法 6 的掉落口标记 `drawDropsArt` / `drawDropsAny`（读 `bvDrops`，画在棋子之上） |
| `UI.BoardPrim` | 棋盘几何降级绘制（`drawGemAt` 查表分派、底盘 / 传送门 / 飞碟 / 皮带 / 掉落口 `drawDropMark` / 蔓延预告 / 粒子；底层与高亮同贴图版读 `BoardView` / `appHighlight`） |
| `UI.CellTable` | 单格绘制的元素查表：元素名（注册表）→ `CellRenderer{crPrim, crArt, crSprite}`；宝石 5 个名字共用一个渲染器；`Custom` 先查按名字的 `customTable`（段 5：气泡；新玩法 5：雪怪 `snow_boss` 按象限画四分之一身体；新玩法 7：变色龙 `chameleon` = 当前颜色宝石 + 五色环），查不到走自定义渲染器 |
| `UI.Ground` | 段 5：地面层的绘制查表（某格的地面层第 11 刀起由 `Match3.View.groundAtView` 取）：名字 → 几何版 / 贴图名(层数)；贴图版画在棋子之下，几何版画在棋子之上（框） |
| `UI.Cell.Prim` / `UI.Cell.Art` | 每种元素一个几何 / 贴图渲染函数（从原 `drawGemAt` / `drawCellArt` 的大 case 逐字拆出）；`Cell.Art` 另含 `colorKey` / `gemSprite` / `breathe` / 角标 |
| `UI.Cell.PrimOverlay` | 第 10 刀：几何版宝石覆盖层，每种一个函数（`overlayGrass` / `Vine` / `Choco` / `Fog` / `Chain` / `Freeze` / `Curtain` / `Steam`，层数点共用 `overlayLayerPips`），`primOverlay` 只按构造子分派（`UI.Cell.Prim` 再导出） |
| `UI.HudArt` / `UI.HudPrim` | HUD、横幅、键位条、暂停帮助、结算面板、弹字的贴图版 / 几何降级版（第 11 刀起 HUD 读数全部来自 `Match3.View`：关卡下标、进度点 `levelDots`、目标、步数上限、道具、右下角 `scoreBadge`；新玩法 5 起 `gvBoss` 为 `Just` 时目标条换成 Boss 血条；得分浮字色、连击贴图名读表现表）；几何版 `drawHud` 只按顺序调用 `UI.HudBlocks` 的区块 |
| `UI.HudBlocks` | 第 10 刀：几何版 HUD 的区块（`hudFrame` 底板、`hudLevel` 关卡号与进度点、`hudGoal` 目标条、`hudGoalSwatch` 收集色块、`hudMoves` 步数条、`hudBoosters` 道具与工具模式、`hudComboBadge` 连击徽章、`hudStatus` 结局色条；新玩法 5 的 `hudBoss` 血条收 `Maybe BossView`，在 `hudGoal` 之后画；第 11 刀起参数是视图模型：`hudLevel` / `hudMoves` 收 `GameView`、`hudGoal` / `hudGoalSwatch` 收 `GoalInfo`、`hudBoosters` 收 `Boosters`、`hudComboBadge` 多收 `GameView`、`hudStatus` 收 `PlayStatus`）与进度条 `drawMeter`（`UI.HudPrim` 再导出） |
| `UI.GoalStyle` | 第 5 刀：目标外观的唯一一张表（图标 `goalIcon`、贴图版色调 `goalTint`、几何版 / 地图小点颜色 `goalPip`），按 `goalView` 分派（新玩法 7：`CountNamed "chameleon"` 的图标是合成图 `chameleon_icon`）；HUD、选关地图读它（标题 / 状态文字标签 `countTag` / `colorTag` 第 11 刀起在 `Match3.View`） |
| `UI.TextArt` / `UI.Glyph` | 烘焙文字 / 中文标签贴图的排版；缺字形时的像素字 |
| `UI.LevelMap` | 选关地图：章节、节点坐标、点击命中、两种绘制 |
| `ComboFx`（`app/pure`） | 连锁逐轮回放的纯逻辑（步末阶段种类与基础帧数、高亮 / 浮字 / 弹字帧数都查表现表 `UI.Presentation`，按事件种类分派；`StageKind` / 连击等级样式从那里再导出）：阶段机 `cascadeStages`（高亮→消失→下落→落定，以及步末阶段：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌；帧号与加速交给 `Engine.Playback.Player`）、波次视图 `WaveView`（快照 + 本轮效果事件）、时间线常量、连击等级样式、下落映射、浮字曲线；只消费 `MoveTrace` 与效果事件，不绘制 |
| `UI.Presentation`（`app/pure`） | 第 10 刀：效果事件 → 前端表现的唯一一张表 `presentationTable`（表现方式、帧数、主色、贴图、步末碎屑、音效名），按元素名细分的生长曲线 `spreadCurves` 与颜色 `elementRGBTable`、缺省表现、连击等级样式、缓动；纯数据，不 import SDL，见[前端表现表](#前端表现表第-10-刀) |
| `UI.Sound`（`app/pure`） | 第 10 刀：音效钩子。`cascadeEventKinds`（回放阶段事件 → 效果种类）、`cascadeSounds`（查表现表的 `effectSound`）、`playSounds`（预留，空操作，不引入音频依赖） |
| `Art` | 贴图图集（BMP + 索引）加载、路径查找、九宫格面板、染色/加色绘制；缺资源时各绘制模块退回几何版 |

前端依赖同样单向无环：纯模块 `UI.Presentation ← ComboFx ← UI.Sound` 在最底层（只依赖核心库），其上 `UI.Types` / `UI.Layout`；`UI.HudBlocks ← UI.HudPrim`、`UI.Cell.PrimOverlay ← UI.Cell.Prim`；`UI.Glyph ← UI.TextArt ← UI.HudArt`，`UI.GoalStyle ← UI.HudArt / UI.HudBlocks / UI.LevelMap`；核心库的 `Match3.View` 与 `Engine.GridUI` 被 HUD / 棋盘 / 标题 / 输入 / `UI.Layout` / `UI.Types` 读，`UI.Cell.Prim / UI.Cell.Art ← UI.CellTable ← UI.BoardPrim ← UI.BoardArt ← UI.EndStage ← UI.Cascade ← UI.Draw`，`UI.Playback ← UI.Actions ← UI.Input ← UI.Plugin ← Main`，`Shell.Loop ← UI.Plugin`（`A ← B` 表示 B 依赖 A）。

## 构建工具链

| 项 | 值 |
|----|-----|
| 构建 | Stack（`package.yaml` → hpack → `match3.cabal`） |
| Resolver | **lts-24.60** + `compiler: ghc-9.14.1`（Stackage 暂无 9.14 快照；`extra-deps` 钉 random 1.2.1.1 / splitmix 0.1.0.5 等，见 `stack.yaml`） |
| GHC | **9.14.1**（`stack.yaml`：`system-ghc: true`，用 ghcup 安装） |
| 库名 | `match3` |
| 可执行文件 | `match3-sdl` |
| 测试套件 | `match3-test`（入口 `test/Spec.hs` 汇总 `test/Spec/*.hs` 各功能模块，tasty + HUnit + QuickCheck） |

库依赖：`base`、`array`、`random`，以及 GHC 自带的 `containers`（第 4 刀起 `Match3.Counts` 用 `Data.Map.Strict`；网页版 `web/match3-web.cabal` 同步加了这一项）。可执行文件额外：`sdl2`、`text`，以及 GHC 自带的 `directory`、`filepath`（贴图加载）与 `containers`。贴图由 `tools/gen_assets.py` 生成到 `assets/`，详见 [ui-art.md](ui-art.md)。

## 网页版（技术验证）

`web/` 目录（已合入 main，`59f1e53`）用 GHC wasm 后端把核心（`Engine.*` / `Match3.*`）和 `ComboFx` 编成 wasm，
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
| 连锁 | `Match3.Board.Cascade` 的 `cascadeMatchesFrom` / `cascadeSeeds` / `cascadeAfterWith` / `cascadeCountdowns`（`CascadeRun`：每轮一个 `CascadeWave`，飞碟吸收单独一轮） | `crTally`（`CascadeTally` 记录） | `crWaves` | `trace_cascade_final_equals_stabilized`、`trace_seeds_final_equals_stabilized`、`trace_multi_wave_each_round_visible` |
| 一步操作 | `Match3.Game.Resolve.resolveMove`（交换与三种道具共用；`Move.resolveSwap` / `Boosters.resolve*` 只做校验与起手选择） | `trySwap` / `use*` = 取 `(GameState, Outcome)` | `trace*` = 取 `MoveTrace` | `trace_swap_final_equals_trySwap`、`trace_boosters_final_equal_result`、`trace_rejected_move_is_empty`、`trace_shuffle_step_replays` |
| 步末描述 | 结算直接使用 `Match3.Game.Trace` 的 `traceSpreads` / `traceSnails` 返回的盘面；`applyEndEffect` 把 `EndEffect` 重放回盘面 | — | `mtEnd` | `trace_end_steps_replay_to_trySwap_final`、`trace_end_steps_boosters_replay`、`trace_end_snail_push_and_turn`、`trace_end_spread_from_adjacent_source` |

皮带：第三刀起 `Conveyor.beltMoves` 是唯一实现，结算（`applyBeltMoves`）、回放描述（`EvBelt` 步末效果）与重放（`applyEndEffect`）共用同一份「原格 → 新格」表。行为金标准见 [testing.md](testing.md#行为金标准golden)；护栏细节与底线见 [testing.md](testing.md#逐轮回放护栏)；前端时间线见 [ui-art.md](ui-art.md#连击表现逐轮回放)。

## 元素框架与事件

第二刀 2b 起，「某个格子在某个时机怎么反应」不再散落在 Board / Game 各处的 `case` 里，而是由**元素**描述、**注册表**（`Registry`）分派（段 2b–5 是扁平记录 `ElementDef`，元素类迁移后是类型类 `Element` 的 instance，见下文「元素类」两节）。旧的函数名保留为 `defaultRegistry` 下的包装；新增的 `*With reg` 版本接受任意注册表（测试里用它接入样例元素）。

### 一个格子的层

一格自上而下是：叠层（`CellOverlay`，草 / 藤 / 巧 / 雾 / 链 / 冻 / 帘 / 蒸汽）→ 冰层（`iceLayers`）→ 本体（宝石各种类、石头、宝箱……、`Custom 名字 值`）→ 地面层（段 2c，`GameState.gsGround :: [(Pos,(ElementName, 层数))]`，第 7 刀起是关卡级元素 `GroundLayer` 的状态、`gsGround` 为派生读数；不在 `Cell` 里、不占格、不随重力 / 洗牌移动；内置关卡恒为空）。`Slot` 标出一个定义占哪一层：`SlotCell i` / `SlotOverlay i`（内置，数组下标）、`SlotIce`、`SlotCustom`（按 `Custom` 的名字查表）、`SlotGround`（按 `gsGround` 里的名字查表）。查询时按层合成：挡交换 = 任一层挡；点火 = 自上而下第一个 `Just`；直接命中 = 最上面一个非穿透层先吸收。

### 元素的能力（Caps）与调用时机

一种元素 = 一个类型 + 一个 `Element` instance（`Match3.Element.Class`）。第 9 刀起类只有三个方法：`name`（元素名）、`toCell`（把值写回格子）、`caps :: e -> Caps`（能力记录，缺省 = `capsOf Piece`）。能力按职责分成五组带默认值的记录，元素只声明自己用到的几项（`Match3.Element.Caps` 的简写，每项一个 `Cap = Caps -> Caps`）；缺省值由原型推出：`Piece`（普通棋子：可交换、能点火、会下落、可过传送门、命中即消、可改色 / 推动、洗牌时参与重排）、`Blocker`（占格障碍：挡交换、不点火、会下落、打不动、洗牌保留）、`Fixed`（同障碍，但不随重力下落）。颜色缺省取 `toCell` 写回的宝石格。元素自己的状态放在值里（石头 `StoneE 层数`、保险箱 `SafeE 层数`……），受击返回新值。

```haskell
instance Element StoneE where
  name _ = "stone"
  toCell (StoneE n) = Stone n
  caps (StoneE n) = blocker [hit (chip n StoneE), onAdjacent 10 (deadRule chipAdjacentStonesExcept), counts CountStones]
```

| 组（记录） | 字段 → 查询函数 | 声明简写 | `Piece` / `Blocker` / `Fixed` 的缺省 |
|------------|-----------------|----------|------------------------------|
| 原型 | `capArchetype` → `archetype` | `piece` / `blocker` / `fixed [..]`（`withCaps 原型 [..]`） | — |
| 匹配与交换（`MatchCaps`） | `mcColor :: Cell -> Maybe Color` → `color`（作用在 `toCell` 上）；`mcBlocksMatch` → `matchColor`；`mcBlocksSwap` → `blocksSwap`；`mcHintable` → `hintable`；`mcSwapRule` → `swapRule` | `colorIs c`、`colorless`、`swappable`、`notHintable`、`onSwap 规则` | 颜色取 `toCell` 写回的宝石格（三种原型相同；`Inert` 无色）；不挡匹配；挡交换 否 / 是 / 是；可提示；无规则 |
| 消除与受击（`HitCaps`） | `hcActivates` → `activates`；`hcOnHit` → `onHit`；`hcBlast` → `blast`；`hcStrip` → `stripOnClear`；`hcAdjacent` → `adjacentRule`；`hcOpen` → `openRule` | `hit h`、`breaks`（= `hit Destroy`）、`noFire`、`explodes 范围`、`onAdjacent 次序 规则`、`opens 规则` | 点火 `Just True` / `Just False` / `Just False`；命中 `Destroy` / `Immune` / `Immune`；无爆炸；不剥层；无规则 |
| 重力与移动（`MoveCaps`） | `mvFalls` → `falls`；`mvPortal` → `portal`；`mvDrains` → `drains`；`mvKeepShuffle` → `keepOnShuffle`；`mvRecolorable` → `recolorable`；`mvPushable` → `pushable` | `teleports`、`drainsAt [边]`、`keepsOnShuffle` / `reshuffles`、`recolors` / `noRecolor`、`pushes` / `noPush` | 下落 是 / 是 / 否；传送门 是 / 否 / 否；不收走；洗牌保留 否 / 是 / 是；可改色、可推 是 / 否 / 否 |
| 计数与目标（`CountCaps`） | `ccCounter` → `counter`；`ccDiffCounter` → `diffCounter`；`ccDiffWeight` → `diffWeight`（新玩法 5）；`ccBonusMoves` → `bonusMoves`；`ccVacatesCarpet` → `vacatesCarpet` | `counts 键`、`countsDiff 键`、`weighs n`、`bonus 步数`、`vacates` | 不计数；差计权重 1；奖励 0；不算覆盖地毯 |
| 步末与变化（`StepCaps`） | `stEnd` → `endRule`；`stGround` → `groundRule`；`stWiden` → `widenRule`（新玩法 8）；`stMessage` → `handleMessage` | `atEnd 规则`、`ground 层数变化`、`widens 改写`、`onMessage 处理` | 无规则；不扩爆；不收消息 |

简写不够用时用 `withMatch` / `withHit` / `withMove` / `withCount` / `withStep` 直接改一组的字段。27 个查询函数（新玩法 5 起多一个 `diffWeight`，共 28 个）与第 9 刀前的同名类方法签名相同（`Element e => e -> …`），注册表的 `*With` 查询、`SomeElement`、修饰器组合 `Modified`（自己的 `caps` 按原规则组合修饰器与里面的元素）与消息机制都没有变；下表是各查询的含义与调用点。

| 查询 | 含义 | 调用点（经注册表的 `*With` 查询） |
|------|------|--------|
| `name` / `toCell` | 元素名（注册表的键、贴图名、计数名、事件里的 `evElement`）/ 把值写回格子（盘面的存储编码） | 注册、分派、放置 |
| `archetype` | 原型，决定各组能力的缺省值 | — |
| `color` / `matchColor` | 本体颜色（颜色袋计数）/ 参与匹配的颜色（修饰器可挡住，`modBlocksMatch`） | `Board.Match.groupGemRuns`、`Clear.countColor`、提示 |
| `blocksSwap` | 本格不能被交换（任一层挡即挡） | `Move` / `Boosters` 校验、`findHint` |
| `swapRule` | 成对交换规则 `SwapRule{srOrder, srFires 交换前盘, srSeeds 交换后盘}`：交换两端的组合直接给出起手种子；多条按 `srOrder` 取第一条成立的。内置：彩虹取色（rainbow，10）、彩虹 × 变色龙（chameleon，15，新玩法 7）；特殊 × 特殊合成第 8 刀起不再是元素规则，而是注册表的组合表并成的一条（20，见「规则表」） | `Registry.swapOpeningWith`、`Board.Match.findHintWith` |
| `hintable` | 普通匹配提示是否试这个格（缺省 True；彩虹 = False，只经成对交换规则给提示） | `Board.Match.findHintWith`（取代原先写死的 `isRainbow`） |
| `activates` | 特殊块能否点火（自上而下第一个 `Just` 决定，软锁纪律） | `Clear.expandSpecials` |
| `falls` / `portal` / `drains` | 随重力下落（否则把列分段）/ 可穿传送门 / 到达哪些边时被收走（`[Edge]`；内置饼干 = `[EdgeBottom]`） | `Board.Gravity.drainEdgesMWith` |
| `onHit` | 直接命中（锤子、爆炸、十字）：`Absorb 新元素` 吸收 / `Destroy` 打碎 / `Immune` 免疫；修饰器用 `modOnHit`：`Pierce` / `Keep m` / `Remove` / `Shatter` | `Clear.clearWaveWith`、`Ice.chipIceOnClear`、`hammerImmune` |
| `adjacentRule` | 邻格真消除时的反应（`AdjacentRule 顺序 规则`，规则拿到 `AdjCtx{真消除格, 直接命中格, 保护格, 可改色谓词 acRecolor}`，返回 `AdjOut{新盘, 打碎格, 生成格}`） | `Clear.clearWaveWith`（按 `arOrder` 依次跑） |
| `openRule` | 开启规则 `OpenRule{orOpen}`：一轮内可多次开启，开出的格本轮坐住。内置：彩蛋 | `Clear.surpriseClearPassWith`（经 `Registry.openWith`） |
| `recolorable` / `pushable` | 可被魔法帽 / 染色瓶改色、可被蜗牛推动（内置 = 宝石各种类 + 倒计时 + 双面块） | `AdjCtx.acRecolor`、`EndCtx.ecPushable` |
| `stripOnClear` | 本格真消除时上层随格清掉（修饰器 `modStripOnClear`） | `Clear` |
| `counter` / `diffCounter` / `bonusMoves` | 进入清除格计数 / 按步前步后个数差计数 / 每少一个奖励步数 | `Board.Cascade`（`ctCounts`）、`Game.Resolve`、`Game.Tally`（`diffCountersWith`） |
| `diffWeight` | 新玩法 5：差计时这一格算几个（缺省 1；雪怪左上格 = 剩余血量、其余三格 0） | `Registry.weighElementWith` → `Game.Tally.diffCountsWith` |
| `vacatesCarpet` | 离开格子也算覆盖地毯 | `Game.Tally.carpetVacateSeedsWith` |
| `keepOnShuffle` | 洗牌时原样放回 | `Game.Shuffle.extractDecorWith` / `ensurePlayableWith` |
| `blast` | 被消除且能点火时的爆炸范围（新玩法 8：`Registry.blastWith` 再按本步扩爆格改写，引爆格不是扩爆格时原样） | `Clear.expandSpecials` |
| `groundRule` | 地面层（`SlotGround`）：上方格子每被消除 / 收走一次，层数 → 新层数（`Nothing` = 清掉）；每去掉一层按 `counter` 计 1 | `Registry.hitGroundWith`（`Game.Resolve` 逐轮调用） |
| `widenRule` | 地面层（新玩法 8）：本格上的特效引爆时，爆炸范围 → 新范围（`Nothing` = 不改；内置只有魔法地格 `magicWiden`） | `Registry.groundWideningWith` → `setWidening`（每步 `Element.Level.levelRegistryIn`）→ `blastWith` / `widenAtWith` |
| `endRule` | 步末规则 `EndRule{erPhase, erOrder, erRun, erHoles}`：`PhaseTick`（倒计时）→ 皮带 → `PhaseSpread`（蔓延）→ `PhaseMove`（蜗牛）→ 再连锁；`erHoles` 在全部步末阶段之后给出要挖空的格，由 `cascadeAfterEndWith` 补结算 | `Game.EndPhase`（步末表的 `tick` / `spread` / `move` 行、`runPhase`）、`Cascade.cascadeCountdownsWith`、`Trace.traceSpreadsWith` |
| `handleMessage` | 开放消息：`Nothing` = 不关心，`Just` = 新的元素值（可以换成别的元素） | `sendMessage`（内置格内元素目前都不收消息；护栏见 `ec_open_messages`） |

规则类能力（`adjacentRule` / `endRule` / `swapRule` / `openRule` / `groundRule`）描述「这类元素」在一轮里怎么作用于盘面，注册表在注册时从原型值上取一次；其余能力逐格查询：先把格子解码成元素值（`elementOf`），再取 `caps`。

**第 9 刀的等价性**：`test/Spec/Support/LegacyElement.hs` 是第 9 刀前 `Element` 类（27 个方法）与全部 19 个内置本体 instance 的逐字副本（只改名为 `LElement` / `LSome`），`Spec.Caps` 用它对照：`qc_caps_match_legacy_elements`（任意格，含冰 / 叠层组合与受击后的新元素，27 项查询逐项相同）、`qc_caps_rules_match_legacy`（邻格 / 步末 / 成对交换 / 开启规则在随机盘面与上下文上的输出相同）、`qc_default_caps_match_legacy_defaults`（三种原型与 `Inert` 的缺省能力等于旧类的缺省方法）、`caps_element_class_is_thin`（类只剩三个方法、内置 instance 只写这三项）、`ext_caps_element_plugs_in`（四行 instance 的扩展元素「荆棘」不改主流程接入）。

**修饰器**（`Modifier`，对应 xmonad 的 `LayoutModifier`）：冰层（`Ice 层数`）和八种叠层（`GrassL` / `VineL` / `ChocoL` / `FogL` / `ChainL` / `FreezeL` / `CurtainL` / `SteamL`）。`Modified` 把修饰器和里面的元素合成一个元素：挡匹配 / 挡交换任一层即挡；点火与命中自上而下第一个有意见的层决定；名字 / 颜色 / 计数 / 下落等取本体；有上层就洗牌保留。一格解码为「冰 → 叠层 → 本体」的嵌套 `Modified`。

**注册表条目**（`Registry.Entry`，名字 → 构造器）：`bodyEntry 原型 解码 放置`（内置本体；槽号由原型推导 = `cellSlot (toCell 原型)`）、`customEntry 原型 (CustomState → 元素)`（`Custom 名字 (CustomState k)`；第 6b 刀前收 `Int`）、`modifierEntry 原型 解码 放置`（冰 / 叠层；槽位由原型写到裸宝石上的结果推导：叠层 → `SlotOverlay (overlaySlot …)`，冰 → `SlotIce`）、`groundEntry 原型`（地面层）、`inertEntry 名字`（`Inert`：挡交换、无色、会下落、打不动、洗牌保留的惰性占格，即旧 `baseDef` 的等价物；未注册的 `Custom` 名字也按它处理）。建表：`mkRegistry`（总函数：同名 / 同槽以后出现的为准，分派数组边界由条目的槽号算出，查不到的槽号退回惰性占格）；`mkRegistryChecked :: [Entry] -> Either [RegistryError] Registry` 把重名（`DuplicateName`）、槽位冲突（`DuplicateSlot`）、推不出槽位（`NoSlot`，原型写回 `Custom` 或修饰器不落任何层）暴露成值，内置条目表由测试 `ec_registry_checked_slots` 保证通过检查。放置函数 `Placer = [Arg] -> Cell -> Maybe Cell` 由关卡放置表 `Place 名字 参数 坐标` 调用（`Game.Level.decorateLevelWith`）；第 6 刀起 `placeWith` / `placeAllWith` 返回 `Either PlaceError Board`（`UnknownElement 名字` / `PlaceOutOfBounds 名字 格`），内置关卡与每日挑战全部放置成功由 `level_placements_all_right` / `daily_placements_all_right` 锁定。

**关卡级元素**（`LevelElement`：`levelName`、`levelReply :: l -> SomeMessage -> Maybe (SomeMessage, l)`、`levelStart :: Level -> l -> l`、`levelCore :: l -> Bool`）：飞碟 / 皮带 / 传送门 / 地毯 / 地面层。第 7 刀（7a）起与格子元素同一写法：**状态在元素值里**（`UfoLevel [Ufo]`、`BeltLevel [Belt]`、`PortalLevel [(Pos,Pos)]`、`CarpetLevel [Pos]`、`GroundLayer Ground`），一局的全部关卡级元素是 `GameState.gsLevelElems :: [SomeLevelElement]`（取代第 7 刀前的五个专用字段；旧名 `gsUfos` 等改为派生读数）。开局由 `Element.Level.startLevelsWith reg 关卡记录` 开出：注册表里的各种（注册顺序）+ 核心元素（地面层），各自的 `levelStart` 从关卡记录取初始状态（飞碟 / 地毯没有放置而目标需要时的补齐也在 `levelStart` 里）。

主流程在固定的流水线节拍上发消息，关卡级元素自己决定回复哪条；回复 = （同类型的消息，回复者在上面累加自己的结果；推进后的自身），推进后的状态写回 `gsLevelElems`。每个节拍参与的元素（`activeLevels`）= 注册顺序的各种（取 `gsLevelElems` 里同名的状态，没有则用注册的原型值）+ `gsLevelElems` 里的核心元素；**未注册（或 `removeLevel` 去掉）的名字即使状态在 `gsLevelElems` 里也不参与**（同未注册的 `Custom` 格按惰性占格处理）。第 7 刀 7b 起**折叠所有回复者**（`askLevelsIn`：按参与顺序，前一个的回复是后一个的问题，各自推进后的状态依次写回；7a 取第一个回复者，内置元素每种消息只有一个回复者，结果相同）：

| 节拍消息（问题 = 回复的类型） | 时机 | 内置回复者 | 主流程入口 |
|------|------|------|------|
| `Refilled 盘 吸走格` | 每轮补子之后 | `UfoLevel`（`stepUfos`，飞碟移动） | 钩子 `onAbsorb`（`Board.Cascade.absorbRound`） |
| `Refilling 补子策略` | 第 8 刀：补子时取策略（初值 = 注册表的策略） | 无（内置都不回复 = 用注册表的缺省策略） | 钩子 `hookRefill`（`Board.Gravity.activeRefill`） |
| `Shaping 形状表` | 新玩法 1：每步结算开始时（初值 = 注册表的形状表） | `BombShapes`（规则开关 `bomb_shapes`；只在 `lvlRules` 含它的关卡打开，打开时插入 `ltBombRule`） | `Element.Level.levelRegistryIn`（`Game.Resolve.resolveMoveWith` 开头换上本关的注册表） |
| `Morphing 交换前盘 交换后盘 两端 变身` | 新玩法 4：玩家交换成立前（初值 Nothing，第一个回复者填上 `Morph { morphName, morphCells, morphSeeds }`） | `RainbowCombos`（规则开关 `rainbow_combos`；彩虹 × 直线 / 炸弹，`Combos.rainbowComboMorph`） | `Element.Level.morphIn`（`Game.Move.resolveSwapWith`：有回复时起手为 `OpenMorph`，`Game.Resolve` 先把变身写进盘面并记一条 `esAfterWaves = 0` 的步末效果） |
| `EndTicked 移位表` | 玩家交换的步末，倒计时之后、蔓延之前；没有皮带时不回复 | `BeltLevel`（`beltMoves`） | `Element.Level.beltShiftIn`（步末表 `belt` 行） |
| `AvoidCells 格` / `WallCells 格` | 会走的元素（PhaseMove）之前 | `BeltLevel`（皮带格）/ `PortalLevel`（门端点） | `avoidCellsIn` / `wallCellsIn`（步末表 `move` 行的 `EndCtx`） |
| `Settling 可穿门谓词 可空盘` | 沉降时 | `PortalLevel`（`portalTeleport`） | 钩子 `onSettle`（`Board.Gravity.settleDrainWith`） |
| `Covering 本步格 新覆盖数` | 步末结算 | `CarpetLevel`（`coverCarpets`） | `coverIn`（`Resolve`） |
| `GroundHit 规则 命中格 去层数` | 每轮之后（按轮） | `GroundLayer`（规则 = 注册表的 `hitGroundWith`；核心元素） | `hitGroundIn`（`Resolve`） |

**Board 层只收钩子记录**（第 7 刀，`Match3.Board.Hooks`）：`LevelHooks { onSettle :: MBoard -> MBoard, onAbsorb :: Board -> ([Pos], LevelHooks), hookRefill :: Maybe RefillPolicy（第 8 刀）, hookLevel :: [SomeLevelElement] }`，由 `levelHooksWith reg (gsLevelElems gs)` 造出；连锁把 `onAbsorb` 交回的钩子一路传下去（`CascadeRun.crHooks`），Game 层最后从 `hookLevel` 取回推进后的关卡级元素。第 3 刀留下的 `[Ufo]` / 传送门对参数全部删掉，Board 核心模块（Match / Clear / Cascade / Gravity / Hooks / Grid）不 import 飞碟 / 皮带 / 地毯模块，也不碰 `GameState`（`br_board_takes_hooks_only` 扫描）。

没有元素回复 = 该机制不生效（`removeLevel`；测试 `br_level_hooks_*` 在 38 关实测）。消息是开放的：任何模块都能定义新的消息类型和新的关卡级元素（`ec_level_elements_by_message` 用无状态的「磁铁」演示只 `registerLevel` 即可接入；`ec_level_element_stateful_extension` 用带状态的「虹吸」演示开局 `levelStart`、状态写回 `gsLevelElems`、去掉注册后状态原样不生效，都不改主流程）。

### 规则表：特殊块形状、特殊块组合、补子策略（第 8 刀）

三处原先写死在主流程里的规则改成注册表上的数据，扩展只改表、不改主流程（`br_rule_tables_out_of_main_flow` 扫描：Board 核心与 Game 的 Move / Resolve / Boosters 不点名特殊块种类、不直接随机选色、不用组合几何）：

| 规则表 | 数据 | 内置（顺序即优先级） | 主流程入口 | 语义 |
|------|------|------|------|------|
| 特殊块形状 | `[ShapeRule]`（`shapeRules` / `setShapeRules`） | `builtinShapeRules`：line5→rainbow（长度 ≥5）→ line4h→line_h（横 4）→ line4v→line_v（竖 4）；长度 3 不生成；内置表没有 L / T 规则（新玩法 1：规则开关 `bomb_shapes` 打开的关卡经 `Shaping` 把 `ltBombRule` 插在 line5 之后） | `Board.Clear.spawnSpecialsWith`（`Element.Special.spawnByShapes`） | 每条连线（横线在前、竖线在后）按表顺序问各规则，取第一条认领它的（`Just`，可为空 = 认领但不生成）；各连线的产出依次写回，后写的覆盖先写的；落点 `shapeAnchor`：交换落点在可清格里就放那里，否则放可清格的中间一个 |
| 特殊块组合 | `[ComboRule]`（`comboRules` / `setComboRules`） | `builtinComboRules`：bomb×bomb（两个 5×5）→ line×line（两端整行整列）→ line×bomb（炸弹端 3 行 + 3 列）→ rainbow×line（彩虹取色；实际总被先于它的彩虹规则 10 接走） | 整张表并成一条 `SwapRule 20`（`comboSwapRule`），经 `swapOpeningWith` / `findHintWith` | 成立 = 表里有对得上的规则（按表顺序，每条先试 (p1,p2) 再试 (p2,p1)，所以天然对称）且两端都 `specialActivates`（软锁不发火）；种子取第一条对得上的规则、参数是（对上第一端谓词的格, 对上第二端的格）；表里没有的组合（彩虹 × 炸弹、彩虹 × 彩虹、普通宝石）不成立，交给后面的规则 / 普通三消 |
| 补子策略 | `RefillPolicy`（`refillPolicyWith` / `setRefillPolicy`；关卡级元素回复 `Refilling` 可换掉） | `defaultRefill`：每个空洞随机选一色补普通宝石（每洞恰好一次 `randomColor`）；另有关卡颜色数 `colorsRefill n` | `Board.Gravity.activeRefill` → `refillWith`（`Cascade.settleRound`、`settleRefillWith`） | 行优先逐个空洞问策略，策略看到空洞位置与已部分补上的盘面，随机数只经给出的生成器消耗 |

等价性：内置表与第 8 刀前的 `spawnSpecials` / `isSpecialCombo` / `comboClearSeeds` / `refill` 逐字相同（性质 `qc_shape_table_matches_legacy` / `qc_combo_table_matches_legacy` / `qc_refill_policy_default_matches_legacy`，旧实现逐字保留在测试里；缺省补子的生成器状态也相同，随机数消费顺序不变），组合表对称（`qc_combo_table_symmetric`）；元素查询快照的 `R swap [10,20]` 与各 S 行不变，金标准 2534 行不变。

`mkRegistry` 建出的注册表形状 / 组合表为空（只有 `defaultRegistry` 装上内置表），`register` 保留三张表（以及新玩法 8 的本步扩爆格 `regWiden`）：从 `defaultRegistry` 扩展的注册表行为不变；直接用 `mkRegistry` 建的非内置注册表不再生成特殊块、也没有特殊合成（第 8 刀前这两处写死、与注册表无关），需要时 `setShapeRules builtinShapeRules` / `setComboRules builtinComboRules`。

**扩爆格（新玩法 8，`regWiden :: [(Pos, [Pos] -> [Pos])]`）**：注册表上的第四项数据，缺省 `[]`（`mkRegistry` / `defaultRegistry` 都是空的），`register` 保留。它不是常驻表，而是「本步」的：每步结算开始时 `Element.Level.levelRegistryIn` 取地面层（`levelGround`）里有 `widenRule` 的格（`groundWideningWith`）写进去（`setWidening`；一格也没有时注册表原样返回），`blastWith reg cell p` 在 `p` 是扩爆格时对本体的 `blast p` 再套一次改写（`widenAtWith`）。`Engine.playWith` 展开效果事件时也用 `levelRegistryIn reg (gsLevelElems 步前状态)`，所以 `EvBlast` 的覆盖格包含扩出来的一圈；其余关卡 `levelRegistryIn` 只可能换形状表，而事件展开不读形状表，与原先用 `reg` 逐项相同。只看引爆格：成对交换规则（彩虹取色、组合表）给的种子、十字道具、魔法石 / 倒计时 / 彩蛋的爆炸都不经 `blast`，不扩（种子里的直线 / 炸弹照常逐个引爆，落在扩爆格上的那枚照样扩）。

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
|  |  | 180 | magic_stone（新玩法 2：邻格真消除充能，本轮被直接命中的不充） |
|  |  | 190 | fuzzball（新玩法 3：邻格真消除即消灭，本轮已被直接命中的不重复算） |
|  |  | 200 | snow_boss（新玩法 5：身外一圈的真消除 + 直接命中的 Boss 格各扣 1 血，四格同改；归零四格并入清除格） |

步末规则：countdown（`PhaseTick` 10）、magic_stone（`PhaseTick` 20，新玩法 2：满格转发射中，种子 = 整行 + 整列）；vine 10 / choco 20 / steam 30（`PhaseSpread`）；snail（`PhaseMove` 10）、fuzzball（`PhaseMove` 20，新玩法 3：跳到相邻普通宝石格，记 `EvBelt "fuzzball"`；按盘面散列选格，不耗 `gsGen`）、snow_boss（`PhaseMove` 30，新玩法 5：召唤计数 +1，每 3 次把身外一圈的一颗普通宝石变成 1 层石头，记 `EvTick "snow_boss"`；同样按盘面散列选格）、chameleon（`PhaseMove` 40，新玩法 7：每只按固定顺序换到下一种不会立刻连成三消的颜色，记 `EvTick "chameleon"`；纯按盘面）。

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

前端按事件种类查**表现表** `UI.Presentation.presentationTable`（第 10 刀起的唯一一张表，取代原来的 `ComboFx.endStageTable`（种类 → 阶段与基础时长）、`UI.Playback.endCrumbTable`（阶段 → 粒子）、`UI.EndStage.spreadProgress`（生长曲线）以及散在 `UI.BoardArt.waveTint` / `UI.HudArt` / `UI.HudPrim` / `UI.EndStage` 的颜色与贴图常量）；步末阶段怎么画仍由 `UI.EndStage.endStageDrawers`（阶段 → 绘制）分派，元素名 → 颜色在 `UI.Presentation.elementRGBTable`。波次级的高亮 / 消失 / 粒子 / 得分浮字读 `ComboFx.WaveView` 里本轮的效果事件（`wvCleared` = EvClear 格、`wvScore` = EvScore 之和）；底图快照（消除前 / 挖洞 / 落定盘面）与下落映射仍取自 `CascadeWave`（事件是差量描述，不含整盘快照）。护栏：`trace_events_consistent_with_trace`（含逐轮严格相等：EvClear 格序 = `cwCleared`、EvScore 和 = `cwScore`）。

### 前端表现表（第 10 刀）

`app/pure/UI/Presentation.hs` 把「效果事件 → 前端表现」收成一张表，按 `EventKind` 查：

```haskell
data Presentation = Presentation
  { prLook   :: Look              -- LookClear | LookScore | LookCombo | LookWithClear | LookStage StageKind
  , prFrames :: Int               -- 基础帧数（1 帧 ≈ 16.7 ms；0 = 不单独占时长）
  , prColor  :: Maybe RGB         -- 固定主色（Nothing = 按格子 / 连击等级 / 元素名取色）
  , prSprite :: Maybe SpriteName  -- 贴图版用到的光效 / 精灵
  , prCrumbs :: Crumbs            -- NoCrumbs | CrumbsAtSources RGB | CrumbsByElement
  , prSound  :: Maybe SoundName   -- 音效钩子（内置全部 Nothing）
  }
```

| 事件 | 表现方式 | 帧数 | 主色 | 贴图 | 碎屑 | 读取方 |
|------|----------|------|------|------|------|--------|
| `EvClear` | `LookClear`（高亮 → 消失） | 12 | (255,250,220)（第 1 轮；连击轮用等级色） | `spark` | — | `ComboFx.waveFlashFrames`、`UI.BoardArt.waveTint`（`clearTint`）、`UI.Cascade`（`clearSprite`） |
| `EvHit` / `EvBlast` / `EvDrain` | `LookWithClear`（随消除一起表现） | 0 | — | — | — | — |
| `EvScore` | `LookScore`（得分浮字） | 48 | (255,244,200)（第 1 轮；连击轮用等级色） | — | — | `ComboFx.scorePopLife`、`UI.HudArt` / `UI.HudPrim`（`scorePopRGB`） |
| `EvCombo` | `LookCombo`（「连击 xN」弹字 + 震屏） | 54 | 等级色 | `zh_combo` | — | `ComboFx.comboPopLife`、`UI.HudArt`（`comboPopSprite`） |
| `EvTick` | `LookStage StTick` | 10 | (255,90,60) | `spark` | 来源格 (255,110,70) | `ComboFx.endStageBase`、`UI.EndStage.drawEndTick`、`UI.Playback.endCrumbs` |
| `EvBelt` | `LookStage StBelt` | 14 | — | — | — | 同上 |
| `EvSpread` | `LookStage StSpread` | 18 | 按元素名（`spreadGlowFor`） | `spark` | 按元素名（`CrumbsByElement`） | 同上；生长曲线 `spreadCurveFor`（vine 分 4 段、choco 先快后慢、steam 匀速） |
| `EvMove` | `LookStage StSnail` | 18 | — | `snail` | — | 同上 |
| `EvShuffle` | `LookStage StShuffle` | 22 | (200,150,255) | `spark` | — | 同上 |

查询函数：`presentationFor`（查内置表）/ `presentationIn`（查给定的表）、`stageKindOf`（事件 → 步末段，非步末种类按蔓延段）、`stagePresentation` / `stageFrames`（段 → 那一行 / 基础帧数）、`presentationRGB`、`effectSound`。几何版不用贴图，只用颜色与帧数。

**缺省表现（扩展元素）**：`EventKind` 是封闭的，扩展元素的步末效果也落在某个已有种类上（例如测试里的 `hopper` 产出 `EvMove`，按蜗牛段播放）；按元素名细分的表里没有的名字用明确的缺省——生长曲线 `defaultSpreadCurve = CurveLinear`（匀速）、前沿柔光 `defaultSpreadGlow = (255,255,255)`（白）、`CrumbsByElement` 查不到颜色时不迸碎屑。整张表里查不到的种类（将来新增 `EventKind` 忘了加行）用 `defaultPresentation`：蔓延段、18 帧、无颜色 / 贴图 / 碎屑 / 音效（与第 10 刀前 `stageKindFor` / `endStageBase` 的缺省相同）。测试 `presentation_extension_defaults` 锁定这些缺省。

**音效钩子**：`effectSound :: EventKind -> Maybe SoundName` = 表项的 `prSound`，内置全部 `Nothing`。回放每切换一个阶段，`UI.Playback.applyCascadeEvent` 经 `UI.Sound.cascadeSounds`（高亮 = `EvCombo`（连击轮）；消失 = 本轮的轮内事件种类；步末段 = 该段每步的种类）把查到的音效名追加到 `App.appSounds`；`UI.Plugin` 每帧推进后把队列交给 `UI.Sound.playSounds`（空操作）并清空。不引入音频依赖、不播放；内置表下队列恒为空，画面与帧序不变（`effect_sound_defaults_to_nothing`、`cascade_sounds_silent_on_real_moves`）。

**给新元素加表现和音效**：

1. 新元素的步末效果选一个已有 `EventKind`（蔓延类用 `EvSpread`、会走的用 `EvMove`……），不用改表就能按那一行播放；
2. 蔓延类要自己的颜色 / 生长节奏：在 `elementRGBTable` 加 `("名字", (r, g, b))`（同时决定前沿柔光、碎屑、HUD 目标色块与几何版 `Custom` 格颜色），在 `spreadCurves` 加 `("名字", CurveSegments n | CurveEaseOut | CurveLinear)`；
3. 要音效：给那一行填 `prSound = Just "名字"`（按事件种类，不按元素）；接入真实音频时只替换 `UI.Sound.playSounds` 的实现；
4. 真正新的表现方式（新的 `StageKind`）才需要：`StageKind` 加构造子 → 表里加一行 `LookStage 新段` → `UI.EndStage.endStageDrawers` 加绘制函数；测试 `presentation_table_covers_every_event_kind` 会检查每种事件恰有一行、每个段恰有一种事件使用。

### 视图模型（第 11 刀）

`src/Match3/View.hs` 是「从 `GameState` / 回放状态算前端要画的东西」的唯一一处，纯函数、字段惰性（不用的读数不算）。桌面版的 HUD（贴图 / 几何）、窗口标题、提示后缀、棋盘底层，网页版的 `encodeState` / `encodeGoal` / `encodeCell` / `apiLevels` 都读它；各字段是第 11 刀前各前端现算式的逐字搬迁（`view_*` 测试对照字面副本）。

```haskell
data GameView = GameView
  { gvLevel, gvLevelIndex :: Int      -- gsLevel 原值（标题 / 网页 / 几何版进度点）；夹紧下标（贴图版徽章、步数上限）
  , gvLevelName, gvRawName :: String  -- 夹紧下标的关名（标题）；原下标关名，无此关 "?"（网页）
  , gvDaily :: Bool, gvScore, gvMoves, gvMoveCap :: Int
  , gvBoosters :: Boosters            -- bHammers / bFreeSwaps / bCrossClears
  , gvCombo :: Int                    -- 经通用接口 gameStatus 取
  , gvShuffled :: Bool, gvOver :: Maybe Outcome
  , gvStatus :: PlayStatus            -- PlayWon s | PlayCleared s n | PlayLost s | PlayShuffled | PlayOn
  , gvGoal :: GoalInfo                -- giGoal / giView / giProgress / giTarget / giKind / giText / giName / giLoseHint
  , gvBoard :: BoardView              -- bvBoard / bvHint（gsHint）/ bvFoundHint（findHint，网页）/ bvLastCleared / bvGround
  }                                   --   / bvBelts / bvPortals / bvUfos / bvCarpets / bvCarpetOpen
```

| 函数 | 用途 | 读取方 |
|------|------|--------|
| `titleLine gv` | 窗口标题（不含 `"  \|  "` 与消息） | `UI.Actions.updateTitle` |
| `goalLine` / `goalBracket` | 标题目标段 / 提示消息里的收集进度 `[RED 3/20]` | `titleLine`、`UI.Input.collectMsg` |
| `carpetAt bv pos` / `groundAtView bv pos` | 逐格地毯标记（`CarpetNone` / `CarpetCovered` / `CarpetOpen`）/ 地面层 | `UI.BoardPrim` / `UI.BoardArt` |
| `bvDrops bv` | 新玩法 6：掉落口格（`Element.Level.levelDrops`；没有掉落口为空） | `UI.BoardArt.drawDropsArt` / `UI.BoardPrim.drawDropMark` |
| `levelDots cur maxReached` | 各关进度点 `DotCurrent` / `DotDone` / `DotUnlocked` / `DotLocked`（贴图版传夹紧下标与最高解锁；几何版传原值与 −1） | `UI.HudArt` / `UI.HudBlocks.hudLevel` |
| `scoreBadge replay summaryLeft best gv` | 右下角：`BadgeCombo n`（回放中连击 ≥2）/ `BadgeRolling 分`（回放中）/ `BadgeSummary n`（播完后的总结）/ `BadgeScore 洗牌? 分`；回放状态由前端从 `Cascade` 换成 `ReplayView{rvCombo, rvShownScore}` | `UI.HudArt`、`UI.HudBlocks.hudComboBadge`（只画 `BadgeCombo` / `BadgeSummary`） |
| `levelViews` | 关卡列表（序号 / 名字 / 步数 / 目标） | 网页 `apiLevels` |
| `cellFace cell` | 单格结构化描述（类型标签 + 按固定顺序的 `CellField` 字段） | 网页 `encodeCell`（桌面按 `UI.CellTable` 画，不读它） |

网格交互在通用层 `Engine.GridUI`（见[模块地图](#通用层srcengine)）：桌面的 `UI.Layout.boardGrid` 描述棋盘在窗口里的位置，`pixelToCell` / `cellOrigin` / `allCells` 签名不变；`UI.Input` 的普通点选与自由交换两步点选都是 `gridClick`，拖动交换是 `gridDragRelease adjacent`；`UI.Types.appHighlight` 给出本帧高亮，两套棋盘绘制按它画选中环 / 提示光 / 闪光 / 自由交换第一格。

**加一个要显示的读数**：在 `GameView`（或 `GoalInfo` / `BoardView`）加字段并在 `gameView` 里算，桌面与网页都从视图读；不要在前端再从 `GameState` 现算（`frontends_read_view_model` 扫描 HUD / 棋盘 / 网页 API）。

### 扩展钩子（段 2c）

段 2c 把「新增元素只经注册表接入」补齐到下列类别，主流程（`Game.Resolve` / `Board.*`）不再需要为新元素改代码。`Engine.*` 在 2c 中**没有改动**。

| 钩子 | 类型 / 入口 | 用途 | 护栏测试（`test/Spec/Extension.hs`，样例元素只在测试里） |
|------|-------------|------|------|
| 注册表下传到底 | `Board.*With reg`；旧名在 `Board.Default` | Board 层不依赖内置表 | `ext_board_modules_take_registry`（源码扫描） |
| 按名字的目标 | `LevelGoal` 新增 `GoalNamed 名字 N`（第 5 刀起写作 `goalCount (CountNamed 名字) N`；读 `gsCount (CountNamed 名字)`，由 `counter` / `diffCounter = CountNamed 名字` 累加）；HUD / 标题 / 失败提示 / 选关已接 | 自定义元素当关卡目标 | `ext_goal_named_counts_crate` |
| 地面层 | `SlotGround` + `groundRule` + `gsGround`（关卡记录的 `lvlGround`） | 果冻类「格子下面的层」 | `ext_ground_layer_test_element` |
| 扩爆（新玩法 8） | 地面层元素的能力 `widens 改写`（`StepCaps.stWiden`）→ 每步 `levelRegistryIn` 写 `regWiden` → `blastWith` | 魔法地格类「在这格引爆的特效范围变大」 | `mg_blast_widened_only_at_magic_cell`、`mg_other_levels_unchanged`（`test/Spec/MagicGround.hs`） |
| 边缘收集 | `drains :: e -> [Edge]`（声明 `drainsAt`） | 任意方向的收集物 | `ext_edge_drain_side_collectible` |
| 步末补结算 | `EndRule.erHoles` + `Cascade.cascadeAfterWith (AfterEnd …)` | 步末阶段挖掉格子后的沉降 / 补子 / 再连锁 | `ext_post_end_settle_hole_element` |
| 洗牌走注册表 | `Game.Shuffle.shuffleGameWith reg`、`Game.State.applyHintWith reg`（`Engine.playWith` 的 Shuffle / Hint 分支） | 自定义元素的洗牌保留 / 提示 | `ext_manual_shuffle_keeps_crate_via_engine` |

**步末补结算选统一路径（无开关）**：交换与道具的步末之后一律经 `cascadeAfterWith (AfterEnd 空洞)`：先挖 `erHoles` 的空洞，再做边缘收集 + 补子；盘面有变化或有格被收走时记一个只含沉降的轮次，之后成消再接普通连锁；什么都没发生时原样返回、**不消耗随机数、不加轮次**。对内置元素它恒为空操作，依据：① 类型层面——`Board` 不能表示空洞，内置 `erHoles` 全是 `const []`；步末阶段（倒计时 / 皮带 / 蔓延 / 蜗牛）不移动饼干（蜗牛把饼干当障碍），皮带后的再连锁本身已含沉降；`refill` 在没有空洞时不取随机数。② 实测——38 关 × 种子 1..100 × 15 步，每步检查主交换、三种道具与全部可成交的交换对，共 **610,751** 手，步末终盘上待挖空洞 / 待收边缘 / 沉降变化全部为 0；金标准 2344 行全等。

### 新增一种元素的步骤

1. 选层：本体用 `Custom "名字" (CustomState 值)`（值自定义，例如耐久；存储编码只能是一个 `Int`，第 6b 刀起包在 `CustomState` 里，在元素的 `toCell` / 解码函数这一处转换）；格子下面的层用 `SlotGround`（放进 `gsGround`）；需要新的内置层时才动 `Types`。
2. 写 instance：定义一个类型（状态放在值里），写 `instance Element 类型`（测试 / 扩展元素写在自己的模块里；新的**内置**元素放进 `src/Match3/Element/Builtin/` 下功能最接近的分组文件——宝石 `Gem`、冰 / 叠层 `Layer`、打破型障碍 `Obstacle`、收集计数 `Collectible`、会动 / 会生成的 `Actor`、地面层 `Ground`、关卡级 `Level`——条目函数写在同一文件，跨分组共用的辅助放 `Common`；再在 `Element.Builtin.builtinDefs` 末尾追加条目，已有条目不要重排），只写 `name` / `toCell` 和 `caps`：`caps x = blocker [能力, …]`（原型选 `piece` / `blocker` / `fixed`，`import Match3.Element.Caps`；能力简写见上文「元素的能力」表，例如 `hit`、`breaks`、`onAdjacent`、`counts`、`drainsAt`、`ground`、`colorIs`、`swappable`；全用缺省时可以不写 `caps`（= 普通棋子）或写 `blocker []`）。邻格规则选一个不和现有顺序冲突的 `arOrder`；步末要挖掉格子时给 `EndRule` 填 `erHoles`，补结算自动发生。叠层类写 `instance Modifier`；不在格子里的机制写 `instance LevelElement`，回复流水线节拍消息（或自定义新消息）。
   - 只经注册表即可接入的类别：本体 `Custom`（削层 / 打碎 / 免疫 / 挡交换 / 下落 / 洗牌保留，也可以是按颜色匹配的有色棋子：`piece [colorIs c, …]`，见 `ec_custom_matchable_gem`）、叠层与冰的命中规则、地面层、任意方向的边缘收集物、带 `erHoles` 的步末元素、以 `CountNamed` 计数并用 `GoalNamed` 当目标的元素、成对交换规则（`onSwap`）、开启类元素（`opens`）、可被改色 / 推动（`recolors` / `pushes`）、不进普通匹配提示（`notHintable`）、在已有节拍上反应的关卡级元素（`LevelElement`，见 `ec_level_elements_by_message`）；第 8 刀起还有特殊块形状规则（`setShapeRules`，见 `ext_shape_rule_lt_bomb`）、特殊块组合规则（`setComboRules`，见 `ext_combo_rule_line_gem`）、补子策略（注册表 `setRefillPolicy` 或关卡级元素回复 `Refilling`，见 `ext_refill_policy_level_colors` / `ext_refill_policy_level_element`）。
   - 段 5 的双层果冻（`Jelly`，在 `Element.Builtin.Ground`：地面层 + `piece [ground …, counts (CountNamed "jelly")]`）与气泡（`Bubble`，在 `Element.Builtin.Collectible`：`Custom` + `blocker [breaks, onAdjacent 170 …, counts (CountNamed "bubble")]`）就是这样接入的内置元素：规则只在 instance 里，关卡数据在 `Levels.Campaign` 的关卡记录里（放置表 `lvlPlacements` / 地面层 `lvlGround`），主流程没有改动（`jb_main_flow_untouched_scan`）。新玩法 2 的魔法石（`MagicStone`，在 `Element.Builtin.Obstacle`：`Custom` + `fixed [hit …, colorless, onAdjacent 180 …, atEnd (EndRule PhaseTick 20 …)]`，命中反应随状态变：平时 `Immune`、发射中 `Absorb` 归零）同样只加 instance 与条目；前端在 `UI.CellTable.customTable` 加一行。新玩法 3 的毛球（`Fuzzball`，在 `Element.Builtin.Actor`：`Custom` + `blocker [breaks, onAdjacent 190 …, counts (CountNamed "fuzzball"), atEnd (EndRule PhaseMove 20 …)]`）也一样：步末效果借用 `EvBelt` 的形状（前端按皮带平移播放），`app/pure` 不改。
   - 新玩法 5 的雪怪 Boss（`SnowBoss`，在 `Element.Builtin.Obstacle`）：多格元素用四个固定格表达（`fixed [hit (Absorb 自身), colorless, notHintable, onAdjacent 200 …, countsDiff (CountNamed "snow_boss"), weighs 血量, atEnd (EndRule PhaseMove 30 …)]`），主流程唯一的改动是通用的差计权重（`CountCaps.ccDiffWeight` / `weighs`，`Game.Tally.diffCountsWith` 求加权和；缺省 1 时与原来的个数差相同）
   - 新玩法 6 的饼干掉落口（`CookieDrop`，在 `Element.Builtin.Level`）：关卡级元素回复已有的补子策略查询 `Refilling`（第 8 刀），把策略包一层「掉落口格补收集物」（`dropRefill`，随机数照常消耗）；配置在关卡记录的新字段 `lvlDrops`（缺省 `[]`）。主流程只多一个条件：有掉落口的关卡开局跳过目标补齐（`Game.Level.newGameAtLevelWith`）
   - 新玩法 7 的变色龙（`Chameleon`，在 `Element.Builtin.Collectible`）：`Custom` 本体 + `piece [colorIs (colorAt k), keepsOnShuffle, noRecolor, counts (CountNamed "chameleon"), onSwap (SwapRule 15 …), atEnd (EndRule PhaseMove 40 …)]`，主流程不改；第 47 关用新玩法 6 的掉落口（`DropSpec … (Custom "chameleon" 0) 2`）在补子时补进变色龙
   - 新玩法 8 的魔法地格（`MagicGround`，在 `Element.Builtin.Ground`）：地面层 `groundEntry (MagicGround 1)`，`caps _ = piece [widens magicWiden]`（没有 `ground` 规则 = 不被消耗、不计数）；为它加了一个缺省什么都不做的通用钩子（能力 `widens` / 注册表 `regWiden` / `blastWith` 改写，见「规则表」后的扩爆格一段）。主流程的改动只有 `Element.Level.levelRegistryIn` 多写一项与 `Engine.playWith` 展开事件改用 `levelRegistryIn`；第 48 关的地面层写在关卡记录 `lvlGround`
   - 仍需改主流程的：需要**新节拍**的关卡级元素（节拍由主流程在固定位置发出）、需要存进 `GameState` 的关卡级状态（见下节「遗留」）。（补子时生成自定义棋子已可经掉落口 `lvlDrops` 做到，见新玩法 7。）
3. 注册：内置元素 = 在 `Element.Builtin.builtinDefs` 里加一行（关卡级元素加进 `builtinLevelDefs`）；测试 / 扩展元素 = `register (customEntry 原型 (元素 . unCustomState)) defaultRegistry`（地面层用 `groundEntry`，关卡级元素用 `registerLevel (SomeLevelElement 原型值)`，开局状态写在 `levelStart` 里，开局 / 走子用 `newGameAtLevelWith reg` 与 `*With reg` 入口），把注册表传给 `*With` 入口（`trySwapWith` / `resolveSwapWith` / `resolveHammerWith` / `ensurePlayableWith` / `shuffleGameWith` / `applyHintWith` / `decorateLevelWith` / `traceEventsWith`），或整体用 `Match3.Engine.match3GameWith reg`。
4. 放置：在关卡放置表里写 `Place "名字" [参数] [坐标]`，由条目的放置函数落格（`customEntry` 缺省 = `Custom 名字 第一个整数参数`；要别的解析用 `customEntryWith`）。
5. 表现：贴图名即元素名（`assets/` 里放同名贴图，缺图时画灰块）；要专门画法的 `Custom` 在 `UI.CellTable.customTable` 加一行，地面层元素在 `UI.Ground.groundTable` 加一行，颜色在 `UI.Presentation.elementRGBTable`（HUD 目标 / 地图 / 几何版 / 步末前沿与碎屑共用）；步末效果的播放、生长曲线与音效见[前端表现表](#前端表现表第-10-刀)的「给新元素加表现和音效」。
6. 测试：参照 `ext_caps_element_plugs_in`（`test/Spec/Caps.hs`，最短的完整例子）、`element_registry_custom_crate_extensibility`、`test/Spec/Extension.hs` 与 `test/Spec/ElementClass.hs`（测试专用「木箱」`Crate` 只定义在测试辅助 `test/Spec/Support.hs`，断言它削层、打碎、计数、挡交换、被锤、洗牌保留，并断言核心源码里没有它的名字）。

### 专门分支的收编（段 4）

段 4 把原先写死在主流程里的专门分支收进元素框架：主流程只经注册表取规则，内置实现挪到 `Element.Builtin` 的定义里。金标准 2344 行全等、三场景截图 AE=0。**段 4 没有改动 `Engine.*`**（只动了 `Match3.*`）。

| 原专门分支 | 段 4 之后（元素类迁移后的写法） | 主流程入口 |
|------------|-----------|------------|
| 彩虹取色（`Move` / `Boosters` / `findHint` 直接调 `isRainbowSwap` / `rainbowClearSeeds`） | `SpecialGem _ Rainbow` 的 `swapRule = SwapRule 10 isRainbowSwap rainbowClearSeeds` | `Registry.swapOpeningWith`、`findHintWith` 逐条 `swapRules` |
| 特殊 × 特殊合成（`isSpecialCombo` / `comboClearSeeds`） | `SwapRule 20`，挂在 `SpecialGem _ LineH` 上（规则自己检查两端）；第 8 刀起改为注册表的组合表 `comboRules`（内置 `builtinComboRules`），整张表并成一条 `SwapRule 20` | 同上 |
| 彩蛋开启（`Clear` 直接调 `Obstacles.openSurprises`） | `SurpriseEgg` 的 `openRule = OpenRule openSurprises` | `Registry.openWith`（`surpriseClearPassWith` 成为通用的「开启类元素」流程） |
| 魔法帽 / 染色瓶只改 `isGem` 的格 | `recolorable`（`Piece` 缺省 True：宝石各种类、倒计时、双面块） | `AdjCtx.acRecolor` → `triggerAdjacentHatsBy` / `triggerAdjacentBottlesBy` |
| 蜗牛只推 `Snail.pushable` 的格 | `pushable`（同上） | `EndCtx.ecPushable` → `stepSnailAtBy` / `traceSnailsBy` |
| 飞碟（`Cascade` 直接调 `stepUfos`） | `UfoLevel [Ufo]` 回复 `Refilled`（第 7 刀起状态在值里） | 钩子 `onAbsorb`（第 7 刀前 `Registry.absorbWith`） |
| 皮带（`Resolve` 直接调 `beltMoves`） | `BeltLevel [Belt]` 回复 `EndTicked` / `AvoidCells` | `Element.Level.beltShiftIn`（第 7 刀前 `Registry.beltShiftWith`） |
| 传送门（`Gravity` 里写死实现） | `PortalLevel [(Pos,Pos)]` 回复 `Settling` / `WallCells` | 钩子 `onSettle`（第 7 刀前 `Registry.teleportWith`） |
| 地毯（`Resolve` 直接调 `coverCarpets`） | `CarpetLevel [Pos]` 回复 `Covering` | `Element.Level.coverIn`（第 7 刀前 `Registry.coverWith`） |
| 提示排除彩虹本体（`findHint` 直接调 `Rainbow.isRainbow`；元素类迁移收编） | `SpecialGem _ Rainbow` 的 `hintable = False` | `Board.Match.findHintWith`（`hintableWith`） |

段 4 当时关卡级元素是 `LevelDef{ldName, ldHook}` + 封闭和类型 `LevelHook`（`HookAbsorb` / `HookShift` / `HookTeleport` / `HookCover`）；元素类迁移后换成 `LevelElement` + 节拍消息（见上节「元素的能力（Caps）与调用时机」），`registerLevel` / `removeLevel` / `levelDefs` 保留。未注册即不生效（测试 `br_level_hooks_*` 在 38 关实测）。

护栏（`test/Spec/Branches.hs`，样例元素拉杆 / 豆荚 / 小车只在测试里）：测试专用成对规则、开启规则、可推动元素只经注册表生效；内置谓词与原写死谓词逐格相同；去掉关卡级元素后飞碟 / 皮带 / 地毯不生效；源码扫描 `br_main_flow_no_special_branches`（`Move` / `Boosters` / `Match` 不点名彩虹取色与特殊合成，`Clear` 不 import `Obstacles`，`Cascade` 不用 `stepUfos`，`Resolve` 不用 `coverCarpets` / `beltMoves`，`Gravity` 不用 `portalWith`）。

**遗留（抽不干净的，及原因；元素类迁移后更新）**：

| 遗留 | 状态 | 原因 |
|------|------|------|
| `findHint` 用 `Rainbow.isRainbow` 排除彩虹本体 | **已消掉**（元素类迁移） | 换成元素方法 `hintable`（彩虹 = False），提示顺序逐字不变（快照 M 行 + 金标准锁定） |
| `LevelHook` 封闭和类型 | **已消掉**（元素类迁移） | 换成 `LevelElement` + 开放消息；新关卡级元素只要 `registerLevel`（`ec_level_elements_by_message`） |
| 自定义元素不能是可匹配的有色宝石 | **已消掉**（元素类迁移） | `Custom` 本体的颜色来自元素的 `color` 方法，注册 `Piece` 原型 + 颜色即参与匹配 / 提示 / 计数（`ec_custom_matchable_gem`）；补子仍只生成普通宝石 |
| 钩子调用时机写死在主流程（吸收在补子后、移位在 Tick 与 Spread 之间……） | 部分消掉 | 发消息的节拍仍是流水线的固定位置（改节拍 = 改规则流水线）；但哪个元素在哪个节拍反应、回复什么，由元素自己决定，新消息类型也能随时定义 |
| 关卡级元素的状态在 `GameState` 专用字段（`gsUfos` / `gsBelts` / `gsPortals` / `gsCarpetOpen` / `gsGround`）；关卡搭建（`Game.Level`）直接构造这些字段；Board 层收 `[Ufo]` / 传送门对参数 | **已消掉**（第 7 刀 7a） | 收进 `gsLevelElems :: [SomeLevelElement]`（状态在元素值里，开局由 `levelStart` 从关卡记录取），旧名改为派生读数（HUD / 前端 / 网页 / 快照投影不变），测试里的记录更新改成 `setUfos` 等写入函数；Board 层只收 `LevelHooks`。飞碟吸收 / 地毯覆盖的个数仍在 `gsCounts` 的 `CountUfo` / `CountCarpets` |
| 传送门端点当会走元素的墙（`WallCells`）只问注册了的传送门 | 行为差异（仅非内置注册表） | 第 7 刀前 `Resolve` 直接读 `gsPortals`，去掉 `portal` 注册后端点仍当墙；现在与传送本身一致：去掉即不生效。内置注册表不受影响（整局对照逐字相同） |
| 自定义元素的存储编码仍是 `Custom 名字 Int`（第 6b 刀起是 `Custom ElementName CustomState`，两个 newtype 各包一个 `String` / `Int`） | 保留 | 盘面以 `Cell` 存储（金标准、前端、机制模块都按它读写）；元素值由构造器从这个 `Int` 解码，所以自定义元素的状态只能编码成一个整数 |
| 地面层元素的 `toCell` 只用于显示 | 保留 | 地面层在 `GroundLayer` 的状态里（`gsGround` 读数）、不进盘面，`toCell` 写回的格子不会落到 `Board` 上 |
| 地面层是核心关卡级元素（`levelCore`），不在 `levelDefs` 里 | 保留（第 7 刀） | 地面层里每层元素的行为已由注册表的地面层条目（`groundEntry`）决定，关卡级这一层只是承载；把它加进 `levelDefs` 会改变元素查询快照的 `R level` 行 |
| 冰层 / 叠层槽位没有注册条目时的兜底 | 行为略有不同 | 新版直接跳过这一层（不当修饰器询问）；旧版按 `baseDef` 缺省把它当一层惰性层（挡交换、打不动）。内置注册表恒含全部冰层 / 叠层条目，不影响内置元素与金标准 |
| `Game.EndPhase`（皮带行）仍 import `Conveyor.applyBeltMoves`；`Trace` 再导出 `beltMoves` | 保留 | `applyBeltMoves` 是「按 (原格, 新格) 列表移格」的通用搬运，与皮带语义无关；再导出只为兼容旧 import |
| `Rainbow` / `Combos` / `Obstacles` / `Snail` 里的规则实现本身 | 保留 | 实现仍在各自模块，只是改由 `Element.Builtin` 的 instance 引用；旧的 `*Except` / `stepSnailAtBlocked` 保留为 `By isGem` / `By pushable` 的包装（测试与 `Board.Default` 在用） |
| `EventKind` | 保留 | 封闭枚举，是前端播放表的键；第 7 刀 7b 起步末效果本身是通用形状（`SpreadKind` 已删，蔓延种类即元素名），新步末元素复用已有事件类型（如 `EvMove`）即可接入 |

### 元素类（阶段 1 原型 → 阶段 2 迁移）

元素框架从扁平的 `ElementDef` 记录（段 2b–5，一个元素 = 一条带 20 多个字段的记录，从 `baseDef` / `gemDef` / `blocker` / `fixed` / `layered` 等模板改字段）改成了 xmonad `LayoutClass` 风格的类型类，分两阶段：

- **阶段 1（原型）**：宝石（`PlainGem` / `SpecialGem`）、彩蛋（`SurpriseEgg`）、冰层（`Ice`）先写成 instance，经适配层 `bridgeBody` / `bridgeModifier` 桥接回旧记录，与其余旧记录并存；`ec_bridge_*` 与 `ec_registry_paths_agree_in_play` 逐字段 / 逐手比对新旧两条路径。彩蛋在现行规则里没有跨轮状态、`GameState` 里也没有彩蛋专用字段，「状态放在元素值里」改由测试专用「鸟窝」`Nest` 演示。
- **阶段 2（迁移）**：全部 31 个内置条目都是 instance（19 个本体类型 + 1 个冰层 + 8 个叠层修饰器；`SpecialGem` 一个类型对应 line_h / line_v / bomb / rainbow 四个条目），四个关卡级元素是 `LevelElement`。`ElementDef` / `baseDef` / `LevelDef` / `LevelHook` 与适配层 `Match3.Element.Prototype` 一起删除；注册表改为「名字 → 构造器」（`Entry`），一格解码成嵌套的元素值后直接调类方法。
- **分文件**（阶段 2 之后的纯搬家）：原来 640 行的 `Element.Builtin` 按功能拆到 `src/Match3/Element/Builtin/` 下的 `Gem` / `Layer` / `Obstacle` / `Collectible` / `Actor` / `Ground` / `Level` / `Common`（各文件开头的注释写明这一组的共同特征），每个元素的类型、instance、条目函数放在同一文件；`Element.Builtin` 只按原注册顺序汇总条目，对外导出不变。彩蛋归 `Obstacle`：它是原型 `Blocker`、命中即破的占格本体，没有计数（不是收集类），也不按颜色匹配（不是宝石）。只在一组里用到的辅助留在组内（`chip` / `layersPlace` 在 `Obstacle`，`peel` / `layerChip` / `spreadRule` 在 `Layer`，`tickRun` / `snailRun` / `traceSnails` 在 `Actor`），跨组共用的才进 `Common`。依赖只朝一个方向：`Obstacle` 引用 `Gem`（双面块翻成宝石）和 `Collectible`（保险箱开成饼干），其余分组互不引用。

**等价性依据**：阶段 2 删掉旧记录之后，新旧两条路径无法在同一进程里并排比对，所以先在阶段 1 的代码上生成元素查询快照 `test/golden/element-queries.txt`（1648 行，生成器 `test/golden/ElementQueries.hs`；用阶段 1 的旧记录注册表生成的结果与之逐字相同），阶段 2 一字不改地比对它：Q 行 = 全部格子组合上的逐格查询，P = 放置，R = 规则表 / 条目名 / 个数差计数 / 关卡级元素清单，A / E / S / O / C / G = 邻格 / 步末 / 成对交换 / 开启规则、直接命中、地面层在样例盘上的输出，M = 40 关 × 种子 1–2 × 12 手（提示、锤子、十字、交换）的逐手散列。三个 `ec_*_snapshot` 测试取代了阶段 1 的三个 bridge 测试。另有金标准 2534 行全等、三场景截图 AE=0。

### 与 xmonad LayoutClass 的对照

| xmonad | 本项目 | 说明 |
|--------|--------|------|
| `class LayoutClass layout a`（`doLayout` / `handleMessage` / `description` 等方法都有默认实现） | `class Element e`（第 9 刀起只有 `name` / `toCell` / `caps`；第 9 刀前是 27 个带默认实现的方法） | 一种元素 = 一个类型 + 一个 instance，只声明用到的能力 |
| 默认方法之间互相推（`doLayout` 默认调 `pureLayout`） | 缺省能力由原型（`Piece` / `Blocker` / `Fixed`，`capsOf`）推出，颜色缺省取 `toCell`；第 9 刀起能力是带默认值的记录，按组覆盖 | 取代旧的 `gemDef` / `blocker` / `fixed` 模板 |
| 布局的状态在值里（`Tall nmaster delta frac`），`handleMessage` 返回新布局 | 元素的状态在值里（`StoneE 层数`），`onHit` / `handleMessage` 返回新元素值（可换成别的元素：保险箱开成饼干） | 盘面仍存 `Cell`，`toCell` 写回、构造器解码 |
| `data Layout a = forall l. LayoutClass l a => Layout (l a)` | `data SomeElement = forall e. Element e => SomeElement e` | 存在类型装箱；Eq 先比名字再比状态，Show 稳定 |
| `LayoutModifier` / `ModifiedLayout m l` | `Modifier` / `Modified SomeModifier SomeElement`（冰层、叠层） | 修饰器先说，没意见再问里面；`Pierce` / `Keep` / `Remove` / `Shatter` 对应命中时的四种组合 |
| `Message` / `SomeMessage` / `fromMessage`（Typeable） | `Match3.Element.Message` 同名三件套 | 开放消息：任何模块都能定义新消息，收的一方按类型认领 |
| xmonad 在事件循环里把 X 事件转成消息发给当前布局 | 主流程在流水线节拍上发 `Refilled` / `EndTicked` / `Settling` / `Covering` / `GroundHit`，`LevelElement` 回复同类型的消息并交回新值 | 第 7 刀起同 `handleMessage` 返回新布局：关卡级元素的状态在值里（`gsLevelElems`），回复时交回推进后的自身 |
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
│ src/Engine/GridUI.hs    GridGeom / gridCellAt / gridClick / gridDragRelease / Highlight：网格 UI│
│ app/Shell/Loop.hs       SDL 外壳：初始化 / HiDPI 窗口 / 渲染器 / 固定 16 ms 主循环；Plugin   │
└──────────────▲───────────────────────────────▲──────────────────────────▲────────────────┘
               │ 实现 Game                      │ 用 Player + 自己的 Stages  │ 实现 Plugin
┌──────────────┴─────────────┐  ┌──────────────┴──────────────┐  ┌────────┴──────────────────┐
│ 三消实现（库）              │  │ 三消回放（app/pure/ComboFx）  │  │ 三消插件（app/UI.*）        │
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
5. **前端**：网格类游戏直接用 `Engine.GridUI`（`GridGeom` 做像素 ↔ 格、`gridClick` / `gridDragRelease` 做点选 / 拖动、`Highlight` 做高亮）；前端要画的读数写成一个纯的视图模型模块（参照 `Match3.View`），桌面 / 网页都读它。
6. **外壳**：写一个 `Plugin`（`plugInit` 加载资源、建世界状态；`plugEvents` 把 SDL 事件映射成动作并调 `gameStep`（要撤销就用 `withHistory` 套一层，历史不要放进游戏状态；前端要的额外数据放进 `stepReport`）；`plugTick` 推进 `Player`；`plugDraw` 绘制），`main = runShell cfg plugin`。
7. **依赖检查**：新游戏与通用层之间只允许「新游戏 → Engine.* / Shell.Loop」；需要时把新的通用文件加入 `engine_layer_is_game_agnostic` 的检查列表。
8. **文档**：在本节的分层图与模块地图里登记新模块。
