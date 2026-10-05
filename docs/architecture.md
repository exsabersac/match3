# 架构

## 分层

```
web/（唯一前端：GHC wasm + JS；SDL2 桌面版已于 2026-10-05 移除，refactor/web-only；图中箭头 = 依赖）
┌──────────────────────────────────────────────────────────────┐
│ web/www/*.js（Canvas 绘制、输入、音效、浮层）                    │
│   └─ 经 wasm 导出调 Match3Web.Api（m3State / m3Swap / m3Meta …）│
│ web/hs/Match3Web.Api / Anim / Json（接口层：JSON 编码、回放帧）   │
│ app/pure（纯前端模块：ComboFx 阶段机、UI.Presentation 表现表、   │
│   UI.Palette / UI.CellFace / UI.WebMeta、UI.GoalIcon、UI.Chapters、│
│   UI.Showcase、UI.Restart、UI.MoveText.keepsTool）               │
└───────────────┬──────────────────────────────┬───────────────┘
                │ 规则：Match3.Core / Match3.Engine │ 时钟：Engine.Playback
┌───────────────▼──────────────────────────────▼───────────────┐
│ Match3.Engine（三消 = 通用接口的第一个实现）  Match3.Core（前端用）│
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
 Match3.Element（元素框架：Types / Ability / Kind / Layer / Rules / World / Mechanic / Builtin / Event / Level / Special；默认世界 defaultWorld）
 Obstacles Rainbow Combos Ice Grass Carpet Snail Ufo Countdown Conveyor Boosters Daily
 Match3.Levels.Campaign（49 关关卡表 / lookupLevel，第 6 刀） ← Match3.Levels.Level（关卡记录）
 Match3.Types（门面，再导出 Types.Name / Cell / Overlay / Body / Board / Game；第 6 刀拆分）  Match3.Goal（目标数据，第 5 刀）
   └─ Match3.Counts（计数键与 Counts，第 4 刀） ← Match3.Color（颜色，第 5 刀从 Types 拆出）
 （纯函数机制模块；无 IO）

 Engine.Game / Engine.Effect / Engine.Playback / Engine.Stream / Engine.Optics（通用层：不 import 任何 Match3 模块）
```

**依赖方向（硬约束）**

- 纯核心 / library ← 前端：网页版（`web/match3-web.cabal`）按源码编核心库与 `app/pure`；库不依赖任何图形库。前端（`app/pure` 与 `web/hs`）从库里只 import 前端 API：`Match3.Core`（类型、查询与对局操作）、`Match3.Engine`（执行动作）、`Match3.View`（视图模型）、`Match3.Element.Event`（效果事件）与通用层 `Engine.*`；测试 `frontends_import_core_api` 扫描这一条。
- 通用层 ← 具体游戏：`Engine.*` 不 import 任何 `Match3` 模块（原 SDL 外壳 `app/Shell/Loop.hs` 也受这条约束，已随桌面版移除）；`Match3.Engine` 实现通用接口，前端只经 `gameStep match3Shell` 执行动作（见[多游戏接口](#多游戏接口)）。测试 `engine_layer_is_game_agnostic` 检查这一方向。
- `Match3.Core` 只再导出前端用到的名字（SDL2 前端移除后又删去 25 个只有桌面在用的导出），每个函数都有前端在用（同一测试检查），前端要用新名字时在这里加。库内模块（含 `Match3.View`）与测试直接 import 所在的子模块（`Match3.Types`、`Match3.Board.*`、`Match3.Game.*`、各机制模块），不经 `Core`。没有 `Match3.Board` / `Match3.Game` 这样的外观模块。
- 子模块之间单向依赖、无环（下文 `A ← B` 表示 B 依赖 A）：Board 内 `Grid ← Match ← Clear`、`Grid ← Gravity`、`Match ← Random`，`Cascade` 依赖 Grid / Match / Clear / Gravity / Effect（第 3 项：`Phase` / `Refill` / `Hooks` / `Wave` ← `Effect` ← `Cascade`，效果层不依赖元素世界）；Game 内 `State ← Outcome / Shuffle / Trace`、`Shuffle ← Level`，`Resolve`（公共结算）依赖 State / Tally / Outcome / Shuffle / Trace，`Move` / `Boosters` 只做校验与起手选择、依赖 `Resolve`。Game 子模块直接 import 所需的 Board 子模块。
- 元素框架 `Match3.Element.*` 位于 Board / Game 之下：`Element.Event ← Element.Types` → `Element.Ability`（值级能力类；`SomeElement` 的相等按具体类型（`Typeable` 的 `cast`）+ 该类型的 `Eq`）→ `Element.Kind` / `Element.Layer`（类型级）→ `Element.Rules`（通用驱动）→ `Element.Mechanic`（关卡级机制类）→ `Element.World`（解码与全部 `*With` 查询）→ `Element.Builtin.*`（按功能分组的 instance）→ `Element.Builtin`（汇总）；各分组里的 instance 调用各机制子模块实现具体反应。Board / Game 只通过元素世界查询「这个格子怎么反应」，不再按构造器写死（见[元素框架与事件](#元素框架与事件)）。
- 关卡级机制（第 7 刀；元素类重构第 5 刀起是 `class Mechanic`）：`Element.Mechanic`（`SomeMechanic`）← `Board.Hooks`（钩子记录 `LevelHooks`，只把机制列表当不透明载荷）← `Board.Gravity` / `Board.Cascade`；`Element.World` / `Element.Mechanic` / `Board.Hooks` ← `Element.Level`（开局、节拍折叠、造钩子；不依赖任何内置机制）← `Board.Default`（`builtinHooks`）/ `Game.State` / `Game.Level` / `Game.Resolve`。`Element.Mechanic` 为 `mechStart` 依赖 `Levels.Level`（关卡记录）。
- 步末表（第 7b 刀）：`Board.Cascade` / `Element.Level` / `Game.Trace`（`EndStep`、`traceSpreadsWith`）← `Game.EndPhase`（`runEndTable`）← `Game.Resolve`（`endTableFor`）；步末效果的通用形状在 `Element.Event`。
- 类型层（第 6 刀拆分）：`Match3.Color` / `Types.Name` ← `Types.Cell ← Types.Overlay / Types.Body`、`Types.Cell ← Types.Board ← Types.Game`（`Types.Game` 另依赖 `Match3.Goal`；`Types.Name` 无依赖，`Match3.Counts` 的 `CountNamed` 也用它），`Match3.Types` 只再导出这六个模块（导出列表与拆分前相同，只少了移走的关卡表、多了第 6b 刀的 `ElementName` / `CustomState`）；关卡层 `Levels.Level ← Levels.Campaign` 在 `Types` / `Element.Types`（放置表）/ `Ufo` / `Conveyor`（`Belt`）之上、`Game.Level` / `Daily` / `Game.Outcome` 之下。
- 机制子模块（障碍、彩虹、合成、冰、草系、地毯、蜗牛、飞碟、倒计时、传送带、道具种子、每日）尽量只依赖 `Types`（及彼此必要的窄依赖），由 `Board.*` / `Game.*` 编排调用顺序。
- 回放方向单向：`Match3.Game.Resolve.resolveMove`（经 `Match3.Board.Cascade` 的记录版连锁）与结算结果一起产出 `MoveTrace` → `app/pure/ComboFx.hs`（纯阶段机，按时间线把它拆成帧；帧数读表现表 `UI.Presentation`）→ `web/hs/Match3Web/Anim.hs`（帧序列编码成 JSON）→ `web/www/render.js`（绘制，颜色 / 帧数经 `m3Meta` 读表现表）。核心**不**知道帧、阶段或样式；`ComboFx` 不 import SDL，也不调用 `trySwap` 等规则入口，只读 `MoveTrace` 与前端传入的结算后盘面。

## 模块地图

### 核心库（`src/Match3/`）

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Match3.Types` | 门面（第 6 刀起无实现）：再导出下面六个 `Types.*` 模块、`Match3.Color` 与 `Match3.Goal` 的 API，导出列表与拆分前相同（关卡表移到 `Match3.Levels.*`） | 连锁、交换、IO |
| `Match3.Types.Name` | 第 6b 刀：`newtype ElementName`（元素名：元素世界 / 放置表 / 地面层 / `Custom` 格 / 效果事件 / `CountNamed` 的键；有 `IsString`）与 `newtype CustomState`（`Custom` 格的状态值）；两者的 `Show` 与底层 `String` / `Int` 相同（Haskell 特性第 2 项起用 `deriving newtype` 派生，不再手写转发），`Cell` / `GameState` 的 `Show` 因此逐字不变 | 名字 → 元素的查表（`Element.World`） |
| `Match3.Types.Cell` | `GemKind` / `CellOverlay` / `CellContents`（含 `Custom ElementName CustomState`，供元素世界扩展元素）/ `Cell`，宝石构造与格子取值（`mkGem` / `cellColor` / `cellKind` / `isGem` / `isCustom` …） | 叠层与本体的具体构造器 |
| `Match3.Types.Overlay` | 叠层（草 / 藤 / 巧 / 雾 / 链 / 冻 / 帘 / 蒸汽）的构造、谓词与层数，`setOverlay` / `clearOverlay`（Haskell 特性第 6 项起谓词与层数用 `Match3.Types.Optics` 的光学写，语义不变） | 叠层的清除与蔓延（`Match3.Grass`） |
| `Match3.Types.Optics` | Haskell 特性第 6 项：盘面与单元格的光学——透镜 `cellAt p :: Lens' Board Cell`、遍历 `cells` / `gemOverlay`（宝石的叠层槽）/ `overlay`（宝石现有的叠层）、棱镜 `_Gem` / `_Fog` / `_Chain` / `_Freeze` / `_Curtain`；可复合成 `cellAt p . overlay . _Fog`；第 9 项加占格障碍棱镜 `_Stone` / `_Chest` / `_Honey` / `_Cake` / `_Safe :: Prism' Cell Int` 并再导出改色遍历 `cellColorT`（定义在 `Types.Cell`，`cellColor = preview cellColorT`），见 [haskell-features/09-规则去重.md](haskell-features/09-规则去重.md) | 叠层的规则（`Match3.Grass`） |
| `Match3.Types.Body` | 本体（石头 / 宝箱 / 蜂蜜 / 气球 / 饼干 / 蛋糕 / 魔法帽 / 果汁机 / 蜗牛 / 保险箱 / 双面 / 彩蛋 / 瓶子 / 精灵 / 倒计时）的构造、谓词与层数（Haskell 特性第 9 项起五种带层数障碍的 `mkXLayers` / `xLayers` / `isX` 由棱镜派生，语义不变）；第 7 项加蜗牛的方向写法 `mkSnailFacing :: Dir -> Cell` / `snailFacing`（`Snail dr dc` 构造器与 Show 不变） | 本体的反应（`Element.Builtin.*`） |
| `Match3.Types.Board` | `Pos`、`newtype Grid a = Grid (Array Pos a)` 与 `type Board = Grid Cell`（Haskell 特性第 2 项：`Grid` 有 Functor / Foldable / Traversable，逐格变换 = `fmap`、逐格统计 = `foldMap`、随机盘 = `mapAccumL`；O(1) 读格，`boardFromRows` / `boardRows` / `boardAt` / `boardSet` / `mapBoard`（= `fmap`）等；`Show` 按行列表打印，与旧列表盘输出相同）、`boardSize`；Haskell 特性第 7 项：四个正交方向 `Dir`（`stepDir` / `dirDelta` / `dirBetween` / `neighborsIn`）与四种具名邻格顺序 `upDownLeftRight` / `readingOrder` / `clockwiseFromRight` / `rightAndDown`，带坐标的折叠 `ifoldMap` / `ifoldr` / `ifoldl'` / `positionsWhere`（行主序，同 `boardPositions`），见 [haskell-features/07-网格几何.md](haskell-features/07-网格几何.md) | 可变盘（`Board.Grid`） |
| `Match3.Types.Game` | `Score` / `MovesLeft` / `TargetScore`、每步结果 `Outcome`、终局 `Terminal`（`TWon` / `TLost` / `TLevelClear`）与 `terminalOf` / `fromTerminal`、地面层 `Ground`、`GameConfig` / `defaultConfig` | 关卡表 |
| `Match3.Levels.Level` | 第 6 刀：关卡记录 `Level`（`lvlIndex` / `lvlName` / `lvlMoves` / `lvlGoal` 与原先分散在按下标 case 的并行表里的 `lvlPlacements` / `lvlBelts` / `lvlPortals` / `lvlUfos` / `lvlCarpets` / `lvlGround`；另有 `lvlRules` 规则开关、新玩法 6 的掉落口 `lvlDrops :: [DropSpec]`）、`level`（不带装饰的关）、`levelConfig`、放置表辅助 `placeEach` / `layersAt`；Haskell 特性第 8 项：Applicative 校验 `Validation`（无 Monad 实例）、`LevelIssue` / `renderIssue`、`validateLevel` / `checkLevel` / `assertLevel`（皮带 / 传送门 / 飞碟 / 地毯 / 地面层 / 掉落口 / 放置表一次报全；`checkLevelDims` 文字不变），见 [haskell-features/08-数据边界.md](haskell-features/08-数据边界.md) | 放置表的解释（`Game.Level`） |
| `Match3.Levels.Campaign` | 第 6 刀：49 关 `allLevels`（2026-09-30 起新玩法关卡追加在末尾）（每关一条完整记录）、`lookupLevel :: Int -> Maybe Level`（取代各处的 `allLevels !! i`；第 8 项起取关时跑全部校验 `assertLevel`）、`levelCount`、`clampLevelIndex`、`levelCarpets` | 开局（`Game.Level`） |
| `Match3.Color` | 第 5 刀：`Color`（`C1`–`C5`）与 `allColors`，从 `Types` 拆出，让 `Counts` 能有颜色键而不成环 | 颜色的显示 |
| `Match3.Counts` | 第 4 刀：计数键 `CounterKey`（内置 8 个元素键 + `CountUfo` / `CountCarpets` + `CountNamed 名字`，第 5 刀加 `CountColor 颜色`）与 `Counts`（`Map CounterKey Int` 的 newtype，稀疏、不存 0；`countOf` / `bumpCount` / `plusCounts`（也是 `<>`）/ `countsFromList` / `countsToList` / `namedCounts` / `colorBag`）；`GameState.gsCounts` 与 `CascadeTally.ctCounts` 都是它（第 5 刀起颜色袋也在里面） | 哪个键算哪个目标（`Match3.Goal`） |
| `Match3.Goal` | 第 5 刀：目标数据 `LevelGoal { goalQuotas :: [Quota] }`，`Quota { quotaMeter :: Meter, quotaTarget :: Int }`，`Meter = MeterScore \| MeterCount CounterKey`；构造函数 `goalScore` / `goalCollect` / `goalColors` / `goalCount`；统一计算 `goalProgress` / `goalMet` / `goalTarget` / `meterValue`；前端分派用的形状 `goalView :: LevelGoal -> GoalView`（`ViewScore` / `ViewCollect` / `ViewCollectMulti` / `ViewCount 键` / `ViewOther`）；手写 `Show` 按第 5 刀前的构造器写法打印 | 图标 / 文案（前端 `UI.GoalStyle`） |
| `Match3.GoalLabel` | 目标的中文显示名（合 main 9f5504e 后从 `Match3.View` 下移）：`goalViewLabel :: GoalView -> String`、`countLabel`、`colorLabel`、`namedGoalLabelTable`（名字目标的中文名，由 `Kind.label` / `GroundKind.groundLabel` 推出：果冻 / 气泡 / 魔法石 / 毛球 / 雪怪 / 变色龙）、`namedLoseHint`（`Kind.loseHint`，雪怪）。`Match3.View` 重新导出并用它定义 `goalLabel`（对外 API 不变）；`Game.Outcome.loseHint` 与标题目标段 `goalLine` 也读它，所以放在 `Game.*` 之下、`View` 之上 |
| `Match3.Element` | 门面：再导出 `Types` / `World` / `Builtin` / `Event` / `Level`、`Special` / `Board.Refill`（第 8 刀）；能力类 `Ability`、类型级 `Kind` / `Layer`、关卡级 `Mechanic` 单独 import（方法名 `color` / `pushable` / `falls` 等较通用，避免与使用方撞名） | 自身无实现 |
| `Match3.Element.Types` | 规则与查询结果的数据类型：`AdjacentRule` / `EndRule` / `SwapRule` / `OpenRule`、第 8 刀的特殊块形状规则 `ShapeRule { shapeName, shapeSpawn :: ShapeCtx -> MatchRun -> Maybe [(Pos, Cell)] }`（上下文 `ShapeCtx { scPrefer, scRuns, scClearable }`）与组合规则 `ComboRule { comboName, comboFirst, comboSecond, comboSeeds }`、连线 `MatchRun`（第 8 刀从 `Board.Match` 移来，原处再导出）、`CounterKey`（再导出自 `Match3.Counts`，第 4 刀前叫 `Counter`）、`Arg` / `Placement`、放置参数解析器 `ArgP`（Applicative / Alternative；`argInt` / `argColor`，跑法 `exactArgs` 精确匹配 / `prefixArgs` 前缀匹配，Haskell 特性第 8 项）、`cellSlot`；Haskell 特性第 9 项：步末规则的智能构造器 `tickRule` / `spreadRule` / `moveRule` 与共用折叠 `runEndRules`（`mapAccumL`），见 [haskell-features/09-规则去重.md](haskell-features/09-规则去重.md) | 调用顺序 |
| `Match3.Element.Ability` | 元素类重构第 1 刀：值级能力的六个小类 `Cellular` / `Matchable` / `Hittable` / `Movable` / `Countable` / `Renders`（每个方法带「普通宝石」缺省；`Renders.faceBase` 第 6 刀）、类同义词 `Element`、装箱 `SomeElement`（逐类转发）、命中结果 `Strike`、DerivingVia 原型包 `Obstacle` / `Fixed`、惰性占格 `Inert`、`abilityProbe`（逐方法取值，透明性测试用）、`sameTypeEq` | 具体元素 |
| `Match3.Element.Kind` | 元素类重构第 1 / 3 刀：类型级 `Kind`（`kindName` / `fromCell` / `place` / `label` / `loseHint` / `diffCounter` / `bonusMoves`，规则方法 `neighbourPrio` / `reach` / `onNeighbourClear`，逃生口 `boardPasses :: [BoardPass]`）、多格实体 `Entity`、地面层 `GroundKind`（不带值）、`Reach` / `Nudge` / `BoardPass`、`customPlace` / `fromCustom` | 怎么执行规则（`Rules`） |
| `Match3.Element.Layer` | 叠层类 `Layer`（`peel` / `putOn` / `layerHit :: LayerHit` / `layerFires` / 规则方法 / `spreads` / `layerPasses`）与通用包装 `Layered l e`（合成规则只写一次） | 具体叠层 |
| `Match3.Element.Rules` | 元素类重构第 3 刀：规则的通用驱动——`kindRules` / `layerRules`（建世界时收集方法规则与逃生口）、`kindNeighbour` / `layerNeighbour`（找邻格、去重、跳过直接命中、按顺序写回）、`layerSpread`（步末蔓延）、`entityDamage`（多格实体扣血） | 具体元素 |
| `Match3.Element.World` | 元素类重构第 4 刀起取代旧注册表：注册项 `Def`（`kindDef @T` / `layerDef` / `groundDef` / `inertDef`）、`World`（有序类型列表 + 按 `cellSlot` / 叠层编号 / `Custom` 名字的解码缓存 + 排好序的规则）、`mkWorld` / `mkWorldChecked`（`WorldError` = `DuplicateName` / `SharedCell` / `Unclaimed`）/ `register`、解码 `decode` / `elementOf` / `bodyOf` / `upperOf`、主流程的全部 `*With` 查询（`matchColorWith` / `blocksSwapWith` / `directHitWith` / `fallsWith` / `counterWith` / `weighElementWith` / `blastWith` …）、显示查询 `faceFieldsWith` / `displayLabelWith` / `loseHintWith` / `displayLabels`、本步上下文 `StepCtx`（`noStep` / `setWidening` / `widenedCells`）、第 8 刀的三张规则表 `shapeRules` / `comboRules` / `refillPolicyWith`（各有 `set…`；`mkWorld` 建出的表：形状 / 组合为空，补子 = `defaultRefill`；`register` 保留三张表）、关卡级机制的种类表 `registerMechanic` / `removeMechanic` / `mechanicDefs` | 具体元素、一局的关卡级状态（`Element.Level`） |
| `Match3.Element.Mechanic` | 元素类重构第 5 刀：关卡级机制类 `Mechanic`（`mechName` / `mechStart` / `mechCore`；带状态的节拍 `onRefilled` / `onEndTick` / `onSettling` / `onCover` / `onGroundHit`；查询 `refillPolicy` / `shapes` / `avoidCells` / `wallCells` / `morph` / `judge`；读数 `ufos` / `belts` / `portals` / `carpetOpen` / `ground` / `drops`，缺省都是 `Nothing`）、`SomeMechanic` / `fromMechanic` / `mechNameOf`、`Morph`、`GroundRule`，以及核心机制地面层 `GroundLayer` | 节拍的折叠（`Element.Level`） |
| `Match3.Element.Builtin` | 汇总：类型列表 `builtinDefs`（36 项，注册顺序固定，快照锁定）、`builtinMechanics` 与 `defaultWorld`（第 8 刀起另装上 `builtinShapeRules` / `builtinComboRules`，均再导出）；再导出测试 / 扩展用的元素类型 | 具体元素的定义 |
| `Match3.Element.Builtin.Gem` | 宝石：`PlainGem`、`SpecialGem`（直线 / 炸弹 / 彩虹），彩虹取色的成对交换规则、`specialBlast`、第 8 刀的内置形状规则表 `builtinShapeRules`（特殊合成不再挂在 line_h 上）；新玩法 1 的 L / T 形规则 `ltBombRule` 与插表函数 `withBombShapes`（不在内置表里，由规则开关 `BombShapes` 按关插入） | 障碍与叠层 |
| `Match3.Element.Builtin.Layer` | 冰层 `Ice` 与 8 种叠层（`Layer` instance）（草 / 藤 / 巧 / 迷雾 / 锁链 / 火箭冰冻 / 窗帘 / 蒸汽），蔓延规则 | 本体 |
| `Match3.Element.Builtin.Obstacle` | 打破型障碍：石头、宝箱、蜂蜜、蛋糕、气球、保险箱、双面块、彩蛋；魔法石（新玩法 2）；雪怪 Boss `SnowBoss`（新玩法 5：2×2 四格 `Custom "snow_boss"`，邻格规则 200 扣血、步末 `PhaseMove` 30 召唤雪块，`snowBosses` / `snowBossHp` / `snowBossSpawn` / `decodeBoss`） | 收走 / 按名字计数的元素（`Collectible`） |
| `Match3.Element.Builtin.Collectible` | 收集与计数类：饼干、时间精灵、气泡；变色龙 `Chameleon`（新玩法 7：`Custom "chameleon" k`，普通棋子原型、按当前颜色匹配；步末 `PhaseMove` 40 换色 `chameleonShift`、彩虹 × 变色龙成对规则 15；`chameleonCell` / `chameleonColor` 给前端用） | 削层 / 变形（`Obstacle`） |
| `Match3.Element.Builtin.Actor` | 会动或会生成东西的：魔法帽、果汁机、蜗牛（含 `traceSnails`）、染色瓶、倒计时 | 被动障碍（`Obstacle`） |
| `Match3.Element.Builtin.Ground` | 地面层：果冻；魔法地格 `MagicGround`（新玩法 8：`"magic"`，没有 `groundHit`，不被消耗、不计数；`groundWiden = Just magicWiden`——特效在这一格上引爆时爆炸范围扩一圈，`magicWiden` = 原范围 ++ 八邻格按行优先） | 占格本体 |
| `Match3.Element.Builtin.Level` | 关卡级机制（`Mechanic` instance）：飞碟 `UfoLevel [Ufo]`、皮带 `BeltLevel [Belt]`、传送门 `PortalLevel [(Pos,Pos)]`、地毯 `CarpetLevel [Pos]`、规则开关 `BombShapes` / `RainbowCombos`、掉落口 `CookieDrop [DropSpec]`（新玩法 6：实现 `refillPolicy`，把补子策略包一层 `dropRefill`；新玩法 7 起名额按「同种」数：`Custom` 按名字、其余按相等）（状态在值里；实现各自的节拍方法并交回推进后的自身，开局状态 `mechStart` 取自关卡记录，飞碟 / 地毯的目标补齐也在这里；核心机制地面层在 `Element.Mechanic`）；传送实现 `portalTeleport` | 一局里有哪些元素（`GameState.gsLevelElems`） |
| `Match3.Element.Level` | 第 7 刀（7a）：一局的关卡级机制 `gsLevelElems`——开局 `startLevelsWith`（注册的各种 + 核心机制地面层）、每个节拍参与的机制（内部函数 `active`：注册顺序取同名状态，没有则用原型；未注册的不参与，核心机制总参与）、节拍折叠 `beatIn`（元素类重构第 5 刀起取代发消息的 `askLevelsIn`：按参与顺序折叠所有回复者并依次写回推进后的状态）/ `queryIn`（状态不变）、读写 `levelState` / `putLevel` 与内置读数 `levelUfos` / `levelBelts` / `levelPortals` / `levelCarpetOpen` / `levelGround` / `levelDrops`（新玩法 6：掉落口格）、给 Board 层的钩子 `levelHooksWith`、本步世界 `levelWorldIn`（`shapes` 节拍换形状表；新玩法 8：地面层里有 `groundWiden` 的格时再 `setWidening`，没有时世界原样）、交换变身 `morphIn`、胜负复核 `judgeIn`、Game 层的节拍 `beltShiftIn` / `avoidCellsIn` / `wallCellsIn` / `coverIn` / `hitGroundIn` | 连锁顺序（`Board.Cascade`） |
| `Match3.Element.Builtin.Common` | 跨分组共用的辅助：`colorPlace`（按颜色放置）、显示字段 `nField` / `colorField`（第 6 刀，`faceBase` 用）、毛球跳格与雪怪召唤共用的选格散列 `boardSeed`（= `show board` 的 64 位 FNV-1a，**依赖派生 Show**）/ `posSeed` / `pickBy`（非空候选里按散列取模）/ `plainGem` | 只在一组里用的辅助 |
| `Match3.Element.Special` | 第 8 刀：规则表的解释器（不含具体规则）——形状表 `spawnByShapes`（每条连线取第一条认领它的规则）、落点 `shapeAnchor`、单连线规则的构造器 `runShape`；组合表 `comboMatch`（按表顺序、每条先试 (p1,p2) 再试 (p2,p1)）/ `comboFires`（另要求两端 `specialActivates`）/ `comboSeedsFor` / `comboSwapRule`（次序 `comboOrder` = 20） | 具体规则（`Builtin.Gem` / `Combos`） |
| `Match3.Element.Event` | 通用步末效果 `EndEffect { endEffectKind, endEffectElement, endEffectItems }` / `EndItem { eiFrom, eiTo, eiCell, eiBack }`（第 7 刀 7b 取代四个构造器与 `SpreadKind` / `SnailMove`；`Show` 手写成旧构造器文本）、`applyEndEffect` / `endEffectPairs` / `endItemDir` / `spreadPairs`、效果事件 `EventKind` / `Event` | 帧与样式 |
| `Match3.Core` | 前端 API：只再导出 `app/` 与 `web/hs` 用到的名字（格子与构造、盘面、关卡与目标、对局操作、回放轨迹、每日挑战） | 规则内部（库内模块与测试直接 import 子模块） |
| `Match3.Board.Grid` | 坐标边界、读写格（`getCell` = `boardAt`，O(1)）、交换、相邻、可空盘面 `MBoard = Array Pos (Maybe Cell)`（第 3 刀起与 `Board` 同形的二维数组，`atM` / `setM` / `setManyM` 读写、`mboardRows` 转行列表；只在一轮消除 / 沉降内部使用，重力按列取出不再转置）、`randomColor`；第 7 项起 `adjacent = isJust . dirBetween`，界内邻格 `neighborsInBounds 顺序 b p` | 任何规则 |
| `Match3.Board.Match` | `MatchRun`（第 8 刀起定义在 `Element.Types`，这里再导出）/ `findMatchRuns` / `hasAnyMatch`、`findHint` / `hasValidMove`（第 3 刀起提示只对交换两格所在的行 / 列做局部匹配检查，其余行列用原盘的结果；遍历顺序与返回值不变，性质 `qc_find_hint_local_matches_reference` 与旧实现对照；Haskell 特性第 5 项起扫描先建整盘的 unboxed 匹配码 `matchCodesWith :: UArray Pos Int`，每格只问一次元素世界，提示搜索读码时对调两个下标、不再复制盘面，见 [haskell-features/05-性能与并发.md](haskell-features/05-性能与并发.md)） | 修改盘面 |
| `Match3.Board.Clear` | 一轮消除（匹配 / 种子）、特殊扩展与生成（第 8 刀起 `spawnSpecialsWith world` 查元素世界的形状规则表）、彩蛋、邻格削层与触发、飞碟吸收（吸走 ≠ 引爆）、计分公式 | 沉降、连锁循环 |
| `Match3.Board.Gravity` | 重力（固定格分段）、边缘收集（`drainEdgesMWith`：按元素的 `drains` 方向，底 → 左 → 右 → 上，收完再落，内置只有饼干 = 底边）、沉降节拍的钩子（`onSettle`，内置 = 传送门）、补子（第 8 刀起按补子策略：`activeRefill world hooks` = 钩子 `hookRefill` 优先、否则元素世界的策略；`refill` = 缺省策略）、`settleRefillWith` / `settleDrainWith`（第 7 刀起收 `LevelHooks`，不再收传送门对）；第 5 项起 `applyGravityWith` 在 `runSTArray` 里逐段双指针压实，对外仍是纯函数 | 消除 |
| `Match3.Board.Refill` | 第 8 刀：补子策略 `RefillPolicy { refillName, refillCell :: RandomGen g => RefillCtx -> g -> (Cell, g) }`（上下文 `RefillCtx { rcPos, rcBoard }`）、缺省 `defaultRefill`（随机五色普通宝石，每洞一次 `randomColor`）、关卡颜色数 `colorsRefill n`、`refillWith`（行优先逐个空洞问策略）；不依赖元素世界 | 策略从哪来（`Gravity.activeRefill`） |
| `Match3.Board.Phase` | Haskell 特性第 1 项（类型层，见 [docs/haskell-features/01-类型层.md](haskell-features/01-类型层.md)）：盘面阶段 `Phase`（`Full` / `Swapped` / `Cleared` / `Fallen`）与带标签的盘面 GADT `Stage p`；阶段转换 `fullStage` / `swapStage` / `clearStage` / `digHoles` / `fallStage` / `refillStage`；类型族 `Repr` / 约束 `IsFull`。运行时代价为零，金标准不变 | 连锁循环本身（在 `Cascade`） |
| `Match3.Board.Hooks` | 第 7 刀（7a）：Board 层的关卡级钩子记录 `LevelHooks { onSettle, onAbsorb, hookRefill（第 8 刀）, hookLevel }` 与空钩子 `noHooks`；由 `Element.Level.levelHooksWith` 从元素世界 + `gsLevelElems` 造出，Board 层不看背后是哪些元素 | 关卡级元素本身 |
| `Match3.Board.Wave` | Haskell 特性第 3 项从 `Cascade` 拆出：一轮回放 `CascadeWave`（`Cascade` 原样再导出） | — |
| `Match3.Board.Effect` | Haskell 特性第 3 项（效果与架构，见 [docs/haskell-features/03-效果与架构.md](haskell-features/03-效果与架构.md)）：连锁用到的三种能力 `MonadRefill`（补子 = 唯一的随机数消耗点）/ `MonadLevelHooks`（读钩子、整轮吸收推进钩子）/ `MonadWaves`（发出一轮回放），约束同义词 `MonadCascade`；两个解释器：纯 `PureCascade`（State：生成器、钩子、回放）与追踪 `TracedCascade`（纯解释器外叠 `StateT` 事件日志 `CascadeLog`），运行结果 `Ran` | 元素世界、具体元素 |
| `Match3.Board.Cascade` | 连锁的**单一实现**（效果：核心写成只依赖能力类的程序 `cascadeMatchesFromM` / `cascadeSeedsM` / `cascadeAfterM` / `cascadeCountdownsM` / `stepCascadeAtM`，对外 `...With` 入口签名不变、经纯解释器 `runCascade` 运行，`runCascadeTraced` 另附事件日志；类型层：每轮 `settleRound` 按 `Stage 'Cleared → fallStage → 'Fallen → refillStage → 'Full` 走，回放 `waveOf` 只收 `Stage 'Cleared` 作 `cwHoles`）：`cascadeMatches` / `cascadeMatchesFromWith` / `cascadeSeeds` / `cascadeAfterWith`（`AfterBelt` / `AfterEnd 空洞`）/ `cascadeCountdowns` 返回 `CascadeRun`（终盘 + `CascadeTally` 计数记录 + `[CascadeWave]` + 推进后的钩子 `crHooks` + 生成器；全部入口只收一个 `LevelHooks`），调用方直接读字段；`stepCascade` 是「恰好一轮」的小工具；步末补结算（挖 `erHoles` 空洞 → 边缘收集 + 补子 → 再连锁）与皮带后连锁都走 `cascadeAfterWith`；每轮的「沉降 + 补子」只在 `settleRound`、整轮吸收（飞碟）只在 `absorbRound` 各写一次 | 步数 / 目标结算、道具扣次 |
| `Match3.Board.Default` | 不带 `With` 的简写（`cascadeMatches` / `clearMatches` / `applyGravity` / `findHint` …）= `*With defaultWorld`；连锁 / 沉降的简写也收 `LevelHooks`，`builtinHooks 飞碟 传送门对` 造出内置世界下的钩子（再导出 `LevelHooks(..)` / `noHooks`）。`Board.{Match,Clear,Gravity,Cascade}` 自身不再 import `Element.Builtin`，只收 `World` 参数；主流程一律把世界往下传，不经本模块 | 规则 |
| `Match3.Board.Random` | 随机盘、稳定盘、可玩盘、`shufflePlayable`（拒绝采样 = 惰性无穷抽样流 `Engine.Stream.draws` 上 `findS`，Haskell 特性第 4 项） | 保留装饰（见 `Game.Shuffle`） |
| `Match3.Game.State` | `GameState`（不含撤销历史；终局 `gsOver :: Maybe Terminal`，只能存终局值；关卡级机制收在 `gsLevelElems :: [SomeMechanic]`，`gsBelts` / `gsPortals` / `gsUfos` / `gsCarpetOpen` / `gsGround` 是派生读数，写入用 `setLevelElem` / `setUfos` / `setBelts` / `setPortals` / `setCarpetOpen` / `setGround`；`Show` 按固定字段名与位置打印（`gsOver` 经 `fromTerminal` 按 `Outcome` 打印），内置之外的元素才追加 `gsLevelExtra`）、`MoveFx` / `moveFx` / `clearMoveFx`（边沿触发）、`applyHint` / `applyHintWith`；Haskell 特性第 6 项起有字段透镜 `gsBoardL` / `gsMovesL` / `gsHammersL` / `gsFreeSwapsL` / `gsCrossClearsL` 与五个派生读数的透镜 `gsUfosL` 等（`Game.Resolve` 的步数 / 道具次数结算用它们） | 结算、撤销历史（在 `Engine.History`） |
| `Match3.Game.Tally` | 结算计数辅助：颜色袋、保险箱 / 时间精灵计数（`diffCountsWith` 按步前 / 步后盘面的加权个数差，新玩法 5 起经 `weighElementWith`，权重缺省 1 时即个数差）、地毯腾空格 | 结局判定 |
| `Match3.Game.Outcome` | 目标满足、`decideOutcome`、`checkOutcome`、选关解锁、地图跳转、失败提示 | 盘面 |
| `Match3.Game.Shuffle` | 保装饰洗牌 `shuffleGame`、自动洗牌 `ensurePlayable` | 回放（洗牌不在 `mtEnd`） |
| `Match3.Game.Level` | 开局 / 每日 / 重开 / 下一关（越界的关卡下标夹到关卡表范围）、`campaignGame :: Int -> Int -> Maybe GameState`、按关卡记录铺装饰 `decorateLevel`、关卡级元素由 `startLevelsWith` 按记录开出（第 7 刀；`newGameAtLevelWith world` 用指定元素世界开局）、目标补齐 `goalDecorWith`（新玩法 6 起有掉落口 `lvlDrops` 的关卡跳过）、步数携带；放置表返回 `Either PlaceError`，静态数据在 `placeStatic` 这一处转成带关卡名的 error | 关卡数据（`Levels.Campaign`）、走步 |
| `Match3.Game.Trace` | `MoveTrace`（含 `mtGen` / `mtShuffle`）/ `EndStep`（`EndEffect` 等再导出自 `Element.Event`）、`traceSpreadsWith`（跑元素世界的蔓延规则）、`beltMoves`（再导出自 `Conveyor`）、效果事件 `traceEvents` | 结算 |
| `Match3.Game.EndPhase` | 第 7 刀 7b：步末表 `EndStage { stageName, stagePhase, stageRun }`、累积器 `EndAcc`、`swapEndTable`（tick → belt → spread → move → settle → vacate）/ `boosterEndTable`（vacate → spread → settle）、`runEndTable`、各行 `tickStage` … `vacateStage`、`runPhase` | 计数、结局（在 `Resolve`） |
| `Match3.Game.Resolve` | 交换与三种道具的**公共结算** `resolveMove`：主连锁 → 步末（按 `endTableFor` 选的 EndPhase 表执行）→ 计数与目标 → 结局 → 自动洗牌，同时产出 `MoveTrace`。类型层：`StartPhase` / `SMoveKind` / 按阶段索引的 `Opening`（见 [01-类型层.md](haskell-features/01-类型层.md)） | 入口校验（在 `Move` / `Boosters`）、步末各阶段（在 `EndPhase`） |
| `Match3.Game.Move` | `resolveSwap`（校验 + 起手选择；新玩法 4 起先问关卡级元素的交换变身 `morphIn`，有回复时起手为 `OpenMorph`）及其投影 `trySwap`（= `runMove`）/ `traceSwap` | 道具 |
| `Match3.Game.Boosters` | `resolveHammer` / `resolveFreeSwap` / `resolveCrossClear` 及其投影 `use*` / `trace*` | 种子几何（见 `Match3.Boosters`） |
| `Match3.Obstacles` | 气球 / 彩蛋 / 染色瓶 / 魔法帽 / 果汁机的整盘邻消触发（元素逃生口 `boardPasses` 引用；相邻查询共用 `adjacentWhere`，气球的同色相邻 `balloonsAdjacentSameColor` 由 `Element.Builtin.Obstacle.balloonPop` 包成邻格规则）；石头 / 宝箱 / 蜂蜜 / 蛋糕 / 保险箱 / 时间精灵的邻消削层自元素类重构第 3 刀起是 `onNeighbourClear` + 通用驱动，不在这里；无 except 的测试写法在 `test/Spec/Support/Obstacles.hs` | 连锁循环 |
| `Match3.Rainbow` | 彩虹判定与清色种子 | 合成几何（见 Combos） |
| `Match3.Combos` | 特殊×特殊合成：第 8 刀起是内置组合表 `builtinComboRules`（炸弹 × 炸弹 → 直线 × 直线 → 直线 × 炸弹 → 彩虹 × 直线），`isSpecialCombo` / `comboClearSeeds` 是这张表的判定 / 清种子；新玩法 4 的 `rainbowComboMorph`（彩虹 × 直线 / 炸弹的变身格与种子，只经规则开关 `rainbow_combos` 用）；各组合的种类谓词与爆炸几何 `bigBomb` / `fullRowCol` / `lineBombCross` | 普通三消、组合表的解释（`Element.Special`） |
| `Match3.Ice` | 匹配时削冰层 | overlay（Freeze/Chain…） |
| `Match3.Grass` | 草/藤/巧/雾/链/冻/帘/蒸汽的清除与蔓延（Haskell 特性第 6 项起四种揭层叠层共用 `chipAdjacentLayerExcept`（传棱镜）、三种蔓延叠层共用 `spreadLayer`、两种邻消叠层共用 `clearAdjacentOverlay`；导出与语义不变） | 蜗牛爬行 |
| `Match3.Carpet` | 地毯覆盖计数（各关的地毯布局第 6 刀起在关卡记录 `lvlCarpets` 里） | 饼干底行收集逻辑（在 `Board.Gravity` / `Game.Tally`） |
| `Match3.Snail` | 蜗牛一步爬行 / 掉头 | 步末其它效果编排 |
| `Match3.Ufo` | 飞碟吸色目标与移格 | 棋盘清除（由 `Board.Clear` 掩码后清） |
| `Match3.Countdown` | 倒计时 tick / 归零爆炸种子 | 爆炸后连锁（`Board.Cascade`） |
| `Match3.Conveyor` | 传送带移位的**单一实现**：`beltMoves`（「原格 → 新格」描述）→ `applyBeltMoves`（按描述移位）；`shiftBelts = applyBeltMoves b (beltMoves belts)`，结算、回放描述与 `applyEndEffect` 重放共用 | 移位后再连锁（`Game.Resolve`） |
| `Match3.Boosters` | 锤子/十字**种子位置**（纯几何） | 扣次数与连锁（`Game.Boosters`） |
| `Match3.Daily` | 日期种子、每日配置、三星公式；第 7 项起年 / 月 / 日是 newtype `Year` / `Month` / `Day`（`dailySeed :: Year -> Month -> Day -> Int`，月日写反是类型错误） | 每日盘面装饰（`Game.Level`） |
| `Match3.Engine` | 三消作为通用接口的实现：`Action`（交换 / 锤子 / 自由交换 / 十字 / 提示 / 洗牌）、`Setup`、`play`（一次结算得到 `Played`：状态 / `Outcome` / `MoveTrace` / `MoveFx` / 事件 / 提示，作为 `gameStep` 的 `stepReport`；通用接口的结局类型 `o = Terminal`，`stepOutcome` / `gameOutcome` 是 `Maybe Terminal`；新玩法 8 起效果事件按本步世界 `levelWorldIn` 展开，`EvBlast` 含魔法地格扩出来的一圈）、`match3Game`、撤销规则 `match3History`、外壳实例 `match3Shell = withHistory match3History match3Game`、`toEffect` | 帧与绘制、撤销历史的存放 |
| `Match3.View` | 第 11 刀：视图模型（纯函数）。`gameView :: GameState -> GameView`（关卡 / 夹紧下标 / 关名、每日、分数、步数、道具 `Boosters`、连击（经 `gameStatus`）、洗牌 / 结局 / `PlayStatus`、目标 `GoalInfo`、棋盘 `BoardView`）、`titleLine` / `goalLine`（标题文字）、`levelDots`（进度点）、`scoreBadge`（回放 / 连击总结 / 得分徽章）、`levelViews`（关卡列表）、`cellFace` / `cellFaceWith`（单格结构化描述，元素类重构第 6 刀起由 `Renders.faceBase` 给出，宝石格按存储编码）；新玩法 5 起 `gvBoss :: Maybe BossView`（目标是「击败 Boss」时的剩余 / 满血）；`cellExtras`（单格的显示附加字段，元素的 `Renders.face` 给出，如雪怪的 q / hurt / turn / every、变色龙的 c，View 不点名元素）；网页读它（原桌面专用的 `goalBracket` / `carpetAt` / `groundAtView` / `colorTag`、`gvMoveCap` / `bvHint` 等已于 `refactor/web-only-2` 删除），见[视图模型](#视图模型第-11-刀) | 坐标、颜色、贴图（前端） |

### 通用层（`src/Engine/`）

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Engine.Game` | 通用游戏接口 `Game cfg s a e o r`（record-of-functions；`r` 是整步报告）、`Step`（含 `stepReport :: Maybe r`）、`runActions` / `finalState` / `stepEffects` / `rejectedStep`、种子约定 | 任何具体规则 |
| `Engine.History` | 段 3：通用撤销历史 `History{histNow, histPast}`、`Undoable a = Act a \| Undo`、`HistoryPolicy{hpLimit, hpRecord, hpSnapshot, hpRestore}`、`withHistory`（给任意 `Game` 套一层撤销；终局后仍可撤销）、`pushHistory` / `replaceNow` / `undoHistory` / `commitStep` | 哪些动作算走步（由游戏的 policy 给出） |
| `Engine.Effect` | 通用效果事件 `Effect{efBeat, efKind, efSubject, efSpots, efAmount}`、按节拍分组 `beats`（第 8 项起每组是 `NonEmpty Effect`） | 帧数与样式 |
| `Engine.Stream` | Haskell 特性第 4 项：没有空构造器的惰性无穷流 `Stream a = a :> Stream a`（`unfoldS` / `iterateS` / `draws` / `headS` / `takeS` / `splitAtS` / `findS`，全是全函数）；拒绝采样（`Board.Random`）与自动洗牌的有限重试（`Game.Shuffle.ensurePlayableWith`）用它 | 采样什么、何时算合格 |
| `Engine.Optics` | Haskell 特性第 6 项：手写 van Laarhoven 光学（只依赖 base）：`Lens` / `Traversal` / `Prism`（最小的 `Choice` profunctor）、`lens` / `prism` / `prism'` / `only` / `ignored` / `_Just`、`view` / `over` / `set` / `preview` / `has` / `toListOf` / `review` 与中缀 `^.` / `%~` / `.~` / `^?` / `^..` / `&`；定律在 `test/Spec/Optics.hs` | 具体游戏的光学（`Match3.Types.Optics`、`Match3.Game.State`） |
| `Engine.Playback` | 纯播放层：阶段机 `Stages`、播放器 `Player`（帧号 / 加速）、`stepPlayer` / `playerProgress` / `runPlayer`（第 4 项起帧计数严格、空事件表不入累积器）；固定队列 `Cue` / `cueStages` / `effectCues` | 阶段内容（由游戏给出）、SDL |

### 前端模块（`app/pure` 与 `web/hs`）

网页是唯一前端。原 SDL2 桌面版（`app/Main.hs`、`app/Shell/Loop.hs`、`app/UI/**` 25 个模块、`app/Art.hs`、`app/pure/UI/Sound.hs`，可执行文件 `match3-sdl`）已于 2026-10-05 移除（`refactor/web-only`）；它的模块说明见 git 历史（本文件在 `origin/main` 45844bf 的版本）。

`app/pure/` 放**纯前端模块**（不依赖任何图形库），是 `package.yaml` 的内部库 `match3-pure`（模块清单在那里，`web/build.sh` 核对网页用到的模块都在清单里）：测试套件直接编译 `app/pure` 源码（它也直接编译 `src`，经内部库用会得到两份不同的 `Match3.*` 类型）；网页版按源码编（`web/match3-web.cabal` 的 `hs-source-dirs` 含 `../app/pure`，`other-modules` 列出全部 10 个）。

| 模块 | 职责 |
|------|------|
| `ComboFx`（`app/pure`） | 连锁逐轮回放的纯逻辑（步末阶段种类与基础帧数、高亮 / 浮字 / 弹字帧数都查表现表 `UI.Presentation`，按事件种类分派；`StageKind` / 连击等级样式从那里再导出）：阶段机 `cascadeStages`（高亮→消失→下落→落定，以及步末阶段：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌；帧号与加速交给 `Engine.Playback.Player`）、波次视图 `WaveView`（快照 + 本轮效果事件）、时间线常量、连击等级样式、下落映射、浮字曲线；只消费 `MoveTrace` 与效果事件，不绘制 |
| `UI.Presentation`（`app/pure`） | 第 10 刀：效果事件 → 前端表现的唯一一张表 `presentationTable`（表现方式、帧数、主色、贴图、步末碎屑、音效名），按元素名细分的生长曲线 `spreadCurves` 与颜色 `elementRGBTable`、缺省表现、连击等级样式、缓动；纯数据，见[前端表现表](#前端表现表第-10-刀) |
| `UI.Palette`（`app/pure`） | 调色板：五色主色 `colorRGB`、名字目标 / 自定义元素取色 `namedRGB`、格子的粒子 / 退回画法颜色 `cellRGB`（元素名的颜色查 `UI.Presentation.elementRGBTable`，元素给出当前颜色时按它）；网页经 `UI.WebMeta` / `m3Meta` 读 |
| `UI.CellFace`（`app/pure`） | 按名字读单格的显示附加字段（`Match3.View.cellExtras`）：`bossPart`（q / hurt / turn / every → `BossPart`）、`faceColor`（c）；不认元素名；`UI.Palette` 用 |
| `UI.WebMeta`（`app/pure`） | 网页启动时读的表现表（`m3Meta`）：颜色、格子取色规则、碎屑色、生长曲线、帧数、音效名，全由 `UI.Palette` / `UI.Presentation` / `ComboFx` 推出；`metaCellRGB` 按网页取色规则算颜色（`Spec.WebColors` 核对 = `cellRGB`） |
| `UI.GoalIcon`（`app/pure`） | 关卡目标 → 图标贴图名 `goalIcon` 的唯一一张表（按 `goalView` 分派，复用棋子贴图）；`Match3Web.Api` 编进 `state.goal.icon` |
| `UI.MoveText`（`app/pure`） | 道具点选模式规则 `keepsTool`（只有自由交换换不掉时保持点选模式；网页 `m3Hammer` / `m3Cross` / `m3FreeSwap` 的 `keepTool` 字段）与界面路径 `MoveUi`；原桌面英文走步文案 `moveMsg` 已随 SDL2 前端移除 |
| `UI.Chapters`（`app/pure`） | 选关地图的章节划分（CH1–CH7），网页 `m3Meta.chapters` |
| `UI.Showcase`（`app/pure`） | 元素展示盘（网页 `?showcase=1`） |
| `UI.Restart`（`app/pure`） | 「重开本关」规则 `restartSame`（每日挑战按开局步数与原目标换种子重开，战役关 `restartLevel`） |
| `Match3Web.Api`（`web/hs`） | wasm 导出：对局状态 JSON、交换 / 道具 / 撤销 / 提示 / 切关 / 重开（一律经 `gameStep match3Shell`）、`m3Meta`；见 [web.md](web.md) |
| `Match3Web.Anim` / `Match3Web.Json`（`web/hs`） | 回放帧序列编码、最小 JSON 编码器 |

前端依赖同样单向无环：`UI.Presentation ← ComboFx`、`UI.CellFace ← UI.Palette`（另依赖 `UI.Presentation`）`← UI.WebMeta`（另依赖 `UI.Presentation` / `ComboFx`），`UI.GoalIcon` / `UI.MoveText` / `UI.Chapters` / `UI.Showcase` / `UI.Restart` 只依赖核心库；`web/hs` 在最上层（`A ← B` 表示 B 依赖 A）。

## 构建工具链

| 项 | 值 |
|----|-----|
| 构建 | Stack（`package.yaml` → hpack → `match3.cabal`） |
| Resolver | **lts-24.60** + `compiler: ghc-9.14.1`（Stackage 暂无 9.14 快照；`extra-deps` 钉 random 1.2.1.1 / splitmix 0.1.0.5 等，见 `stack.yaml`） |
| GHC | **9.14.1**（`stack.yaml`：`system-ghc: true`，用 ghcup 安装） |
| 库名 | `match3` |
| 内部库 | `match3-pure`（`app/pure`；模块清单是网页版与测试共用的权威列表） |
| 可执行文件 | 无（原 `match3-sdl` 已移除；网页版由 `web/build.sh` 用 wasm32-wasi-cabal 构建） |
| 测试套件 | `match3-test`（入口 `test/Spec.hs` 汇总 `test/Spec/*.hs` 各功能模块，tasty + HUnit + QuickCheck；直接编译 `src/`，不依赖库，见 [testing.md「套件结构」](testing.md#套件结构)） |

库依赖：`base`、`array`、`random`，以及 GHC 自带的 `containers`（`Match3.Counts` 用 `Data.Map.Strict`）与 `transformers`（网页版 `web/match3-web.cabal` 同样列出这几项）。内部库 `match3-pure`：顶层公共依赖之外只加 `match3`。（原可执行文件的 `sdl2`、`text`、`vector`、`bytestring`、`filepath` 与只经 SDL2 依赖链引入的 `stack.yaml` extra-deps `parallel` / `unordered-containers` 已随桌面版移除。）测试组件直接编译 `src/`，所以列出库的依赖（`array`、`containers`、`transformers`、`random`），另用 `stm`、`tasty` 系列与 `directory`。各组件的依赖都经 `-Wunused-packages` 核对过，没有多余项。贴图由 `tools/gen_assets.py` 生成到 `assets/`，详见 [ui-art.md](ui-art.md)。

## 网页版（技术验证）

`web/` 目录（已合入 main，`59f1e53`）用 GHC wasm 后端把核心（`Engine.*` / `Match3.*`）和 `ComboFx` 编成 wasm，
接口层 `web/hs/Match3Web/Api.hs` 只调 `gameStep match3Shell`（网页是唯一前端），JS 只负责绘制与输入，核心源码不改。
元素框架（`Match3.Element.Ability` / `Kind` / `Layer` / `World` / `Mechanic` 与 `SomeElement` 等存在类型、
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
| 连锁 | `Match3.Board.Cascade` 的 `cascadeMatchesFromWith` / `cascadeSeeds` / `cascadeAfterWith` / `cascadeCountdowns`（`CascadeRun`：每轮一个 `CascadeWave`，飞碟吸收单独一轮） | `crTally`（`CascadeTally` 记录） | `crWaves` | `trace_cascade_final_equals_stabilized`、`trace_seeds_final_equals_stabilized`、`trace_multi_wave_each_round_visible` |
| 一步操作 | `Match3.Game.Resolve.resolveMove`（交换与三种道具共用；`Move.resolveSwap` / `Boosters.resolve*` 只做校验与起手选择） | `trySwap` / `use*` = 取 `(GameState, Outcome)` | `trace*` = 取 `MoveTrace` | `trace_swap_final_equals_trySwap`、`trace_boosters_final_equal_result`、`trace_rejected_move_is_empty`、`trace_shuffle_step_replays` |
| 步末描述 | 结算直接使用 `Match3.Game.Trace` 的 `traceSpreadsWith` / `traceSnails` 返回的盘面；`applyEndEffect` 把 `EndEffect` 重放回盘面 | — | `mtEnd` | `trace_end_steps_replay_to_trySwap_final`、`trace_end_steps_boosters_replay`、`trace_end_snail_push_and_turn`、`trace_end_spread_from_adjacent_source` |

皮带：第三刀起 `Conveyor.beltMoves` 是唯一实现，结算（`applyBeltMoves`）、回放描述（`EvBelt` 步末效果）与重放（`applyEndEffect`）共用同一份「原格 → 新格」表。行为金标准见 [testing.md](testing.md#行为金标准golden)；护栏细节与底线见 [testing.md](testing.md#逐轮回放护栏)；前端时间线见 [ui-art.md](ui-art.md#连击表现逐轮回放)。

## 元素框架与事件

「某个格子在某个时机怎么反应」不写在 Board / Game 各处的 `case` 里，而是由**元素**（能力类与类型级类的 instance，见下文）描述、**元素世界**（`World`，`Match3.Element.World`）解码与分派。不带 `With` 的简写（`Board.Default`）= `defaultWorld` 下的包装；`*With w` 版本接受任意世界（测试里用它接入样例元素）。入门讲解见 [guide/04-元素框架.md](guide/04-元素框架.md)。

### 一个格子的层

一格自上而下是：冰层（宝石的 `ice` 层数）→ 叠层（`CellOverlay`，草 / 藤 / 巧 / 雾 / 链 / 冻 / 帘 / 蒸汽）→ 本体（宝石各种类、石头、宝箱……、`Custom 名字 值`）→ 地面层（段 2c，`GameState.gsGround :: [(Pos,(ElementName, 层数))]`，第 7 刀起是关卡级机制 `GroundLayer` 的状态、`gsGround` 为派生读数；不在 `Cell` 里、不占格、不随重力 / 洗牌移动）。冰层与叠层是叠层种类（`Layer`），本体是本体种类（`Kind`），地面层是地面层种类（`GroundKind`）。解码（`elementOf`）：按注册的叠层由外向内 `peel`，剩下的交给本体的 `fromCell`，都不认识时得到惰性占格 `Inert`；分派缓存按 `cellSlot` / 叠层编号 / `Custom` 名字在建世界时算好（同名 / 同格以后注册的为准）。查询时按层合成（`Layered`）：挡匹配 / 挡交换 = 任一层挡；点火 = 本层有意见（`layerFires = Just`）就听本层；直接命中 = 最外层先回答（`layerHit`）。

### 元素的能力与调用时机

元素类重构（第 0–6 刀，2026-10）起，一种本体元素 = 一个类型 + 六个值级能力类的 instance（`Match3.Element.Ability`，每个方法都带「普通宝石」的缺省，元素只覆盖不同的）+ 一个类型级 `Kind` instance（`Match3.Element.Kind`）。`Element` 是六个能力类加 `Show` / `Eq` / `Typeable` 的类同义词，`SomeElement` 是装箱的元素（逐类转发每个方法）。常见组合用 DerivingVia 原型包：`Obstacle`（挡交换、不点火、打不动、洗牌保留、不过门、不被改色 / 推动，会下落）与 `Fixed`（同障碍，另外不下落）。元素自己的状态放在值里（石头 `StoneE 层数`、保险箱 `SafeE 层数`……），受击返回新格子。

```haskell
newtype StoneE = StoneE Int
  deriving (Eq, Show)
  deriving (Matchable, Movable) via (Obstacle StoneE)

instance Cellular StoneE where { nameOf _ = "stone"; toCell (StoneE n) = Stone n }
instance Hittable StoneE where { struck (StoneE n) = chip n Stone; fires _ = False }
instance Countable StoneE where counter _ = Just CountStones
instance Renders StoneE where faceBase (StoneE k) = Just ("stone", [nField k])

instance Kind StoneE where
  kindName _ = "stone"
  fromCell cell = case cell of { Stone n -> Just (StoneE n); _ -> Nothing }
  place _ = layersPlace Stone
  neighbourPrio _ = Just 10
  onNeighbourClear (StoneE n) = chipNudge n Stone
```

| 类 | 方法 → 世界查询 | 普通宝石的缺省（`Obstacle` / `Fixed` 的不同处） | 调用点 |
|----|-----------------|----------------------------|--------|
| `Cellular` | `nameOf` / `toCell`（`Int` newtype 元素缺省写成 `Custom 名字 (CustomState n)`） | — | 注册、分派、放置、计数名、事件里的 `evElement` |
| `Matchable` | `color` → `colorOfWith` / `matchColorWith`；`blocksMatch`；`blocksSwap` → `blocksSwapWith` / `swapBlockedWith`；`hintable` → `hintableWith` | 颜色取宝石格；不挡匹配；不挡交换（挡）；可提示 | `Board.Match.groupGemRunsWith`、`Clear.countColor`、`Move` / `Boosters` 校验、`findHintWith` |
| `Hittable` | `struck :: e -> Strike`（`Absorb Cell` / `Destroy` / `Immune`）→ `directHitWith` / `hitImmuneWith`；`fires` → `activatesWith`；`blast` → `blastWith` | `Destroy`（`Immune`）；能点火（不点火）；无爆炸范围 | `Clear.clearWaveWith`、`Clear.expandSpecials`、锤子 |
| `Movable` | `falls` → `fallsWith`；`portal` → `portalWith`；`drains` → `drainsWith`；`keepOnShuffle` → `keepOnShuffleWith`；`recolorable` / `pushable` → `recolorableWith` / `pushableWith` | 下落（`Fixed` 不落）；过门（不过）；不收走；洗牌重排（保留）；可改色 / 可推（不可） | `Board.Gravity`、`Game.Shuffle`、`AdjCtx.acRecolor`、`EndCtx.ecPushable` |
| `Countable` | `counter` → `counterWith`；`diffWeight` → `weighElementWith`（新玩法 5）；`vacatesCarpet` → `vacatesCarpetWith` | 不计数；差计权重 1；不算覆盖地毯 | `Board.Cascade`（`ctCounts`）、`Game.Tally` |
| `Renders`（不参与规则） | `face` → `faceFieldsWith`（`View.cellExtras`：网页 JSON 按序追加的附加字段）；`faceBase`（第 6 刀，`View.cellFaceWith`：网页 JSON 的类型标签 `t` 与基本字段） | 无附加字段；`faceBase = Nothing` → `Custom` 格 `("custom", name / v)`、其余 `(元素名, [])`；宝石格（含冰 / 叠层）由 View 按存储编码给出 | 网页 `Api.encodeCell`、桌面 HUD |

类型级 `Kind` 的方法都带 `proxy e` 参数：`kindName`、`fromCell`、`place`（关卡放置）、`label` / `loseHint`（`CountNamed` 目标的中文名 / 失败提示 → `displayLabelWith` / `loseHintWith`）、`diffCounter` / `bonusMoves`（→ `diffCountersWith`）、规则方法 `neighbourPrio` / `reach`（`SkipDirect` / `AllNeighbours`）/ `onNeighbourClear :: e -> Nudge`（`Untouched` / `Becomes Cell` / `Dies`），以及逃生口 `boardPasses :: proxy e -> [BoardPass]`（`AdjacentPass 次序 规则` / `EndPass EndRule` / `SwapPass SwapRule` / `OpenPass OpenRule`）。多格实体另写 `Entity`（`footprint` / `partNo` / `hitPoints` / `withHp`）。地面层 `GroundKind` 是不带值的类型：`groundName`、`groundHit :: proxy g -> Int -> Maybe Int`（→ `hitGroundWith`，`Game.Resolve` 逐轮调用；每去一层按 `groundCounter` 计 1）、`groundWiden`（新玩法 8 魔法地格扩爆 → `groundWideningWith`）、`groundLabel` / `groundLoseHint`。

**规则 = 方法 + 通用驱动 + 逃生口**（第 3 刀，`Match3.Element.Rules`）：建世界时 `kindRules` / `layerRules` 把方法给出的规则与逃生口收集一次、按次序排好。邻格规则的驱动 `kindNeighbour` / `layerNeighbour` 统一做「按真消除格顺序找上下左右、界内且认得出这种元素的格、整体去重、`SkipDirect` 去掉直接命中格、按顺序写回、`Dies` 前插进打碎格」；叠层的步末蔓延由 `spreads` 方法经 `layerSpread` 驱动；多格实体扣血由 `entityDamage` 算。读整盘、按自己顺序写回的规则（步末 `EndRule{erPhase, erOrder, erRun, erSeeds, erHoles}`、成对交换 `SwapRule{srOrder, srFires, srSeeds}`、开启 `OpenRule`、魔法帽 / 果汁机 / 染色瓶这类改邻格的规则）照旧写成函数挂在 `boardPasses` / `layerPasses` 上。驱动的顺序语义与第 3 刀前的手写函数逐项相同，元素对照快照 `element-oracle.txt` 的 `AR` / `ER` 行与金标准锁定；`ab_rule_methods_pinned` 钉住每种内置元素的方法值。

**叠层**（`Layer`，对应 xmonad 的 `LayoutModifier`）：冰层（`Ice 层数`）和八种叠层（`GrassL` / `VineL` / `ChocoL` / `FogL` / `ChainL` / `FreezeL` / `CurtainL` / `SteamL`）。方法：`peel` / `putOn`、`layerBlocksMatch` / `layerBlocksSwap`、`layerFires :: l -> Maybe Bool`、`layerHit :: l -> LayerHit l`（`Pierce` 问里面 / `Keep l'` 换层 / `Peel` 揭掉 / `Shatter` 连本格一起碎）、`layerStripsOnClear`（→ `stripOnClearWith`）、`layerPlace`，规则方法 `layerNeighbourPrio` / `layerReach` / `onLayerNeighbourClear` / `spreads` 与逃生口 `layerPasses`。`Layered l e` 把叠层和里面的元素合成一个元素，合成规则只在 `Layered` 的 instance 里写一次：挡匹配 / 挡交换任一层即挡；点火本层有意见就听本层；命中本层先回答，穿透时问里层、里层吃掉命中就把本层盖回去；名字 / 颜色 / 计数 / 下落等取里层；有任何一层就洗牌保留。

**注册项**（`Def`）：`kindDef @T`（本体）、`layerDef @L`（冰 / 叠层）、`groundDef @G`（地面层）、`inertDef 名字`（只登记名字的惰性占格；未注册的 `Custom` 名字也解码成 `Inert`：挡交换、无色、会下落、打不动、洗牌保留）。建世界：`mkWorld :: [Def] -> World`（总函数：同名 / 同格以后出现的为准）；`mkWorldChecked :: [Def] -> Either [WorldError] World` 把重名（`DuplicateName`）、同一种格子被多个种类认领（`SharedCell`）、种类不认领任何格子（`Unclaimed`，解码永远轮不到它）暴露成值；`register` 往已有世界追加一项。放置函数 `Placer = [Arg] -> Cell -> Maybe Cell` 由关卡放置表 `Place 名字 参数 坐标` 调用（`Game.Level.decorateLevelWith`）；`placeWith` / `placeAllWith` 返回 `Either PlaceError Board`（`UnknownElement 名字` / `PlaceOutOfBounds 名字 格`），内置关卡与每日挑战全部放置成功由 `level_placements_all_right` / `daily_placements_all_right` 锁定。

**本步上下文**（第 4 刀，`StepCtx { stepWiden }`，World 的 `wStep` 字段，`noStep` 为空）：只在一步结算期间有意义的数据，目前只有魔法地格的扩爆格（见「规则表」后的扩爆格一段）。放在 World 里而不另穿参数，Clear / Cascade / Trace 的签名不用改。

**关卡级机制**（第 5 刀，`class Mechanic`，`Match3.Element.Mechanic`）：飞碟 / 皮带 / 传送门 / 地毯 / 地面层 / 规则开关 / 掉落口。与格子元素同一写法：**状态在值里**（`UfoLevel [Ufo]`、`BeltLevel [Belt]`、`PortalLevel [(Pos,Pos)]`、`CarpetLevel [Pos]`、`GroundLayer Ground`），一局的全部机制是 `GameState.gsLevelElems :: [SomeMechanic]`（`gsUfos` 等是派生读数，来自读数方法 `ufos` / `belts` / `portals` / `carpetOpen` / `ground` / `drops`，返回 `Maybe`，读的一方取第一个提供的）。基本方法 `mechName`、`mechStart :: Level -> m -> m`（从关卡记录取开局状态；飞碟 / 地毯没有放置而目标需要时的补齐也在这里）、`mechCore`（核心机制：不经注册、总参与，只有地面层）。开局由 `Element.Level.startLevelsWith w 关卡记录` 开出：世界里的各种（注册顺序）+ 核心机制。

主流程在固定的流水线节拍上调**有类型的方法**（缺省都是 `Nothing` = 不回复）。每个节拍参与的机制（`Element.Level` 内部的 `active`）= 注册顺序的各种（取 `gsLevelElems` 里同名的状态，没有则用注册的原型值）+ 核心机制；**未注册（或 `removeMechanic` 去掉）的名字即使状态在 `gsLevelElems` 里也不参与**。带状态的节拍经 `beatIn` **折叠所有回复者**（按参与顺序，前一个的回复是后一个的输入，各自推进后的状态依次写回；原来没有的只在状态真的变了时追加），查询节拍经 `queryIn`（状态不变）：

| 节拍方法 | 时机 | 内置回复者 | 主流程入口 |
|------|------|------|------|
| `onRefilled 盘 吸走格` | 每轮补子之后 | `UfoLevel`（`stepUfos`，飞碟移动） | 钩子 `onAbsorb`（`Board.Cascade.absorbRound`） |
| `refillPolicy 策略` | 第 8 刀：补子时取策略（初值 = 世界的策略） | `CookieDrop`（新玩法 6 掉落口；其余关不回复 = 缺省策略） | 钩子 `hookRefill`（`Board.Gravity.activeRefill`） |
| `shapes 形状表` | 新玩法 1：每步结算开始时（初值 = 世界的形状表） | `BombShapes`（规则开关 `bomb_shapes`；打开时插入 `ltBombRule`） | `Element.Level.levelWorldIn`（`Game.Resolve.resolveMoveWith` 开头换上本关的世界） |
| `morph 交换前盘 交换后盘 两端` | 新玩法 4：玩家交换成立前（第一个回复者为准，回复 `Morph { morphName, morphCells, morphSeeds }`） | `RainbowCombos`（规则开关 `rainbow_combos`；彩虹 × 直线 / 炸弹） | `Element.Level.morphIn`（`Game.Move.resolveSwapWith`：有回复时起手为 `OpenMorph`） |
| `onEndTick 移位表` | 玩家交换的步末，倒计时之后、蔓延之前 | `BeltLevel`（`beltMoves`） | `Element.Level.beltShiftIn`（步末表 `belt` 行） |
| `avoidCells 格` / `wallCells 格` | 会走的元素（PhaseMove）之前 | `BeltLevel`（皮带格）/ `PortalLevel`（门端点） | `avoidCellsIn` / `wallCellsIn`（步末表 `move` 行的 `EndCtx`） |
| `onSettling 可穿门谓词 可变盘` | 沉降时 | `PortalLevel`（`portalTeleport`） | 钩子 `onSettle`（`Board.Gravity.settleDrainWith`） |
| `onCover 本步格 新覆盖数` | 步末结算 | `CarpetLevel`（`coverCarpets`） | `coverIn`（`Resolve`） |
| `onGroundHit 规则 命中格 去层数` | 每轮之后（按轮） | `GroundLayer`（规则 = 世界的 `hitGroundWith`；核心机制） | `hitGroundIn`（`Resolve`） |
| `judge 盘面 总分 剩余步数 结局` | 一步结算之后、写进 `gsOver` 之前（初值 = 内置规则判出的结局）；只查询 | 无（内置都不回复 = 原有结局） | `judgeIn`（`Resolve`）；限时关、倒计时归零即输这类输法只加机制，见 `ext_judging_level_element` |

**Board 层只收钩子记录**（第 7 刀，`Match3.Board.Hooks`）：`LevelHooks { onSettle :: MBoard -> MBoard, onAbsorb :: Board -> ([Pos], LevelHooks), hookRefill :: Maybe RefillPolicy（第 8 刀）, hookLevel :: [SomeMechanic] }`，由 `levelHooksWith w (gsLevelElems gs)` 造出；连锁把 `onAbsorb` 交回的钩子一路传下去（`CascadeRun.crHooks`），Game 层最后从 `hookLevel` 取回推进后的机制。Board 核心模块（Match / Clear / Cascade / Gravity / Hooks / Grid）不 import 飞碟 / 皮带 / 地毯模块，也不碰 `GameState`（`br_board_takes_hooks_only` 扫描）。

没有机制回复 = 该机制不生效（`removeMechanic`；测试 `br_level_hooks_*` 在 38 关实测）。任何模块都能定义新的机制（`ec_mechanics_by_beat` 用无状态的「磁铁」演示只 `registerMechanic` 即可接入；`ec_mechanic_stateful_extension` 用带状态的「虹吸」演示开局 `mechStart`、状态写回 `gsLevelElems`、去掉注册后状态原样不生效，都不改主流程）。需要新时机的机制要在 `Mechanic` 加一个带 `Nothing` 缺省的方法，并在主流程对应位置调一次 `beatIn` / `queryIn`。

### 规则表：特殊块形状、特殊块组合、补子策略（第 8 刀）

三处原先写死在主流程里的规则改成元素世界上的数据，扩展只改表、不改主流程（`br_rule_tables_out_of_main_flow` 扫描：Board 核心与 Game 的 Move / Resolve / Boosters 不点名特殊块种类、不直接随机选色、不用组合几何）：

| 规则表 | 数据 | 内置（顺序即优先级） | 主流程入口 | 语义 |
|------|------|------|------|------|
| 特殊块形状 | `[ShapeRule]`（`shapeRules` / `setShapeRules`） | `builtinShapeRules`：line5→rainbow（长度 ≥5）→ line4h→line_h（横 4）→ line4v→line_v（竖 4）；长度 3 不生成；内置表没有 L / T 规则（新玩法 1：规则开关 `bomb_shapes` 打开的关卡经节拍 `shapes` 把 `ltBombRule` 插在 line5 之后） | `Board.Clear.spawnSpecialsWith`（`Element.Special.spawnByShapes`） | 每条连线（横线在前、竖线在后）按表顺序问各规则，取第一条认领它的（`Just`，可为空 = 认领但不生成）；各连线的产出依次写回，后写的覆盖先写的；落点 `shapeAnchor`：交换落点在可清格里就放那里，否则放可清格的中间一个 |
| 特殊块组合 | `[ComboRule]`（`comboRules` / `setComboRules`） | `builtinComboRules`：bomb×bomb（两个 5×5）→ line×line（两端整行整列）→ line×bomb（炸弹端 3 行 + 3 列）→ rainbow×line（彩虹取色；实际总被先于它的彩虹规则 10 接走） | 整张表并成一条 `SwapRule 20`（`comboSwapRule`），经 `swapOpeningWith` / `findHintWith` | 成立 = 表里有对得上的规则（按表顺序，每条先试 (p1,p2) 再试 (p2,p1)，所以天然对称）且两端都 `specialActivates`（软锁不发火）；种子取第一条对得上的规则、参数是（对上第一端谓词的格, 对上第二端的格）；表里没有的组合（彩虹 × 炸弹、彩虹 × 彩虹、普通宝石）不成立，交给后面的规则 / 普通三消 |
| 补子策略 | `RefillPolicy`（`refillPolicyWith` / `setRefillPolicy`；关卡级机制的 `refillPolicy` 节拍可换掉） | `defaultRefill`：每个空洞随机选一色补普通宝石（每洞恰好一次 `randomColor`）；另有关卡颜色数 `colorsRefill n` | `Board.Gravity.activeRefill` → `refillWith`（`Cascade.settleRound`、`settleRefillWith`） | 行优先逐个空洞问策略，策略看到空洞位置与已部分补上的盘面，随机数只经给出的生成器消耗 |

等价性：内置表与第 8 刀前的 `spawnSpecials` / `isSpecialCombo` / `comboClearSeeds` / `refill` 逐字相同（性质 `qc_shape_table_matches_legacy` / `qc_combo_table_matches_legacy` / `qc_refill_policy_default_matches_legacy`，旧实现逐字保留在测试里；缺省补子的生成器状态也相同，随机数消费顺序不变），组合表对称（`qc_combo_table_symmetric`）；元素查询快照的 `R swap [10,20]` 与各 S 行不变，金标准 2534 行不变。

`mkWorld` 建出的世界形状 / 组合表为空（只有 `defaultWorld` 装上内置表），`register` 保留三张表（以及本步上下文 `StepCtx`）：从 `defaultWorld` 扩展的世界行为不变；直接用 `mkWorld` 建的非内置世界不生成特殊块、也没有特殊合成（第 8 刀前这两处写死、与注册表无关），需要时 `setShapeRules builtinShapeRules` / `setComboRules builtinComboRules`。

**扩爆格（新玩法 8，本步上下文 `StepCtx { stepWiden :: [(Pos, Board -> [Pos] -> [Pos])] }`）**：世界的 `wStep` 字段，缺省 `noStep`（`mkWorld` / `defaultWorld` 都是空的），`register` 保留；第 4 刀前是注册表字段 `regWiden`。它不是常驻表，而是「本步」的：每步结算开始时 `Element.Level.levelWorldIn` 取地面层（`levelGround`）里 `groundWiden` 为 `Just` 的格（`groundWideningWith`）写进去（`setWidening`；一格也没有时世界原样返回），`blastWith world cell p` 在 `p` 是扩爆格时对本体的 `blast p` 再套一次改写（`widenAtWith`）。`Engine.playWith` 展开效果事件时也用 `levelWorldIn w (gsLevelElems 步前状态)`，所以 `EvBlast` 的覆盖格包含扩出来的一圈；其余关卡 `levelWorldIn` 只可能换形状表，而事件展开不读形状表，结果逐项相同。只看引爆格：成对交换规则（彩虹取色、组合表）给的种子、十字道具、魔法石 / 倒计时 / 彩蛋的爆炸都不经 `blast`，不扩（种子里的直线 / 炸弹照常逐个引爆，落在扩爆格上的那枚照样扩）。

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
|  |  | 180 | magic_stone（新玩法 2：邻格真消除充能，本轮被直接命中的不充；命名清理起是方法 `onNeighbourClear` + 通用驱动） |
|  |  | 190 | fuzzball（新玩法 3：邻格真消除即消灭，本轮已被直接命中的不重复算） |
|  |  | 200 | snow_boss（新玩法 5：身外一圈的真消除 + 直接命中的 Boss 格各扣 1 血，四格同改；归零四格并入清除格） |

步末规则：countdown（`PhaseTick` 10）、magic_stone（`PhaseTick` 20，新玩法 2：满格转发射中，种子 = 整行 + 整列）；vine 10 / choco 20 / steam 30（`PhaseSpread`）；snail（`PhaseMove` 10）、fuzzball（`PhaseMove` 20，新玩法 3：跳到相邻普通宝石格，记 `EvBelt "fuzzball"`；按盘面散列选格，不耗 `gsGen`）、snow_boss（`PhaseMove` 30，新玩法 5：召唤计数 +1，每 3 次把身外一圈的一颗普通宝石变成 1 层石头，记 `EvTick "snow_boss"`；同样按盘面散列选格）、chameleon（`PhaseMove` 40，新玩法 7：每只按固定顺序换到下一种不会立刻连成三消的颜色，记 `EvTick "chameleon"`；纯按盘面）。

### 效果事件

`Match3.Game.Trace.traceEvents :: MoveTrace -> [Event]`（`traceEventsWith world`）把回放脚本按时间线展开成事件，`Event{evKind, evWave, evElement, evCells, evAmount}`：

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

前端按事件种类查**表现表** `UI.Presentation.presentationTable`（第 10 刀起的唯一一张表，取代原来的 `ComboFx.endStageTable`（种类 → 阶段与基础时长）、`UI.Playback.endCrumbTable`（阶段 → 粒子）、`UI.EndStage.spreadProgress`（生长曲线）以及散在 `UI.BoardArt.waveTint` / `UI.HudArt` / `UI.HudPrim` / `UI.EndStage` 的颜色常量；这些桌面模块已随 SDL2 前端移除）；步末阶段怎么画由网页 `web/www/render.js` 的 `drawEndStage`（阶段 → 绘制）分派，元素名 → 颜色在 `UI.Presentation.elementRGBTable`。波次级的高亮 / 消失 / 粒子 / 得分浮字读 `ComboFx.WaveView` 里本轮的效果事件（`wvCleared` = EvClear 格、`wvScore` = EvScore 之和）；底图快照（消除前 / 挖洞 / 落定盘面）与下落映射仍取自 `CascadeWave`（事件是差量描述，不含整盘快照）。护栏：`trace_events_consistent_with_trace`（含逐轮严格相等：EvClear 格序 = `cwCleared`、EvScore 和 = `cwScore`）。

### 前端表现表（第 10 刀）

`app/pure/UI/Presentation.hs` 把「效果事件 → 前端表现」收成一张表，按 `EventKind` 查：

```haskell
data Presentation = Presentation
  { prLook   :: Look              -- LookClear | LookScore | LookCombo | LookWithClear | LookStage StageKind
  , prFrames :: Int               -- 基础帧数（1 帧 ≈ 16.7 ms；0 = 不单独占时长）
  , prColor  :: Maybe RGB         -- 固定主色（Nothing = 按格子 / 连击等级 / 元素名取色）
  , prCrumbs :: Crumbs            -- NoCrumbs | CrumbsAtSources RGB | CrumbsByElement
  , prSound  :: Maybe SoundName   -- 音效名（内置：EvClear = "clear"、EvBlast = "special"，其余 Nothing）
  }
```

「读取方」一列：现行实现写在前面；括号内「原桌面 …」是第 10 刀时的读表位置（`app/UI` 的 `UI.*`），已随 SDL2 桌面版移除。网页经 `m3Meta`（`UI.WebMeta`）读同一张表、在 `web/www/render.js` / `hud.js` 里画。原桌面贴图版专用的贴图名列 `prSprite`（`spark` / `snail` / `zh_combo`）与 `clearTint` / `scorePopRGB` / `clearSprite` / `comboPopSprite` 已于 `refactor/web-only-2` 删除。

| 事件 | 表现方式 | 帧数 | 主色 | 碎屑 | 读取方 |
|------|----------|------|------|------|--------|
| `EvClear` | `LookClear`（高亮 → 消失） | 12 | (255,250,220)（第 1 轮；连击轮用等级色） | — | `ComboFx.waveFlashFrames`；网页 `render.js`（原桌面 `UI.BoardArt.waveTint`，已移除） |
| `EvHit` / `EvBlast` / `EvDrain` | `LookWithClear`（随消除一起表现） | 0 | — | — | — |
| `EvScore` | `LookScore`（得分浮字） | 48 | (255,244,200)（第 1 轮；连击轮用等级色） | — | `ComboFx.scorePopLife`；网页 `hud.js`（原桌面 `UI.HudArt` / `UI.HudPrim`，已移除） |
| `EvCombo` | `LookCombo`（「连击 xN」弹字 + 震屏） | 54 | 等级色 | — | `ComboFx.comboPopLife`；网页 `hud.js`（原桌面 `UI.HudArt`，已移除） |
| `EvTick` | `LookStage StTick` | 10 | (255,90,60) | 来源格 (255,110,70) | `ComboFx.endStageBase`；网页 `render.js` `drawEndStage`（原桌面 `UI.EndStage` / `UI.Playback.endCrumbs`，已移除） |
| `EvBelt` | `LookStage StBelt` | 14 | — | — | 同上 |
| `EvSpread` | `LookStage StSpread` | 18 | 按元素名（`spreadGlowFor`） | 按元素名（`CrumbsByElement`） | 同上；生长曲线 `spreadCurveFor`（vine 分 4 段、choco 先快后慢、steam 匀速） |
| `EvMove` | `LookStage StSnail` | 18 | — | — | 同上 |
| `EvShuffle` | `LookStage StShuffle` | 22 | (200,150,255) | — | 同上 |

查询函数：`presentationFor`（查内置表）/ `presentationIn`（查给定的表）、`stageKindOf`（事件 → 步末段，非步末种类按蔓延段）、`stagePresentation` / `stageFrames`（段 → 那一行 / 基础帧数）、`presentationRGB`、`effectSound`。

**缺省表现（扩展元素）**：`EventKind` 是封闭的，扩展元素的步末效果也落在某个已有种类上（例如测试里的 `hopper` 产出 `EvMove`，按蜗牛段播放）；按元素名细分的表里没有的名字用明确的缺省——生长曲线 `defaultSpreadCurve = CurveLinear`（匀速）、前沿柔光 `defaultSpreadGlow = (255,255,255)`（白）、`CrumbsByElement` 查不到颜色时不迸碎屑。整张表里查不到的种类（将来新增 `EventKind` 忘了加行）用 `defaultPresentation`：蔓延段、18 帧、无颜色 / 贴图 / 碎屑 / 音效（与第 10 刀前 `stageKindFor` / `endStageBase` 的缺省相同）。测试 `presentation_extension_defaults` 锁定这些缺省。

**音效**：`effectSound :: EventKind -> Maybe SoundName` = 表项的 `prSound`；内置表只有消除 `EvClear` = `"clear"`、爆炸 `EvBlast` = `"special"`，其余无声。网页经 `m3Meta` 取音效名（`UI.WebMeta`，`web/www/audio.js`），回放时按同一张表选音效；音效只在前端，不影响规则与帧序（`effect_sound_names_clear_and_special`）。（原桌面 `UI.Sound.cascadeSounds` / `UI.Playback` / `UI.Audio.cue` 的音效队列已随 SDL2 前端移除。）

**给新元素加表现和音效**：

1. 新元素的步末效果选一个已有 `EventKind`（蔓延类用 `EvSpread`、会走的用 `EvMove`……），不用改表就能按那一行播放；
2. 蔓延类要自己的颜色 / 生长节奏：在 `elementRGBTable` 加 `("名字", (r, g, b))`（同时决定前沿柔光、碎屑、HUD 目标色块与 `Custom` 格的退回颜色），在 `spreadCurves` 加 `("名字", CurveSegments n | CurveEaseOut | CurveLinear)`；
3. 要音效：给那一行填 `prSound = Just "名字"`（按事件种类，不按元素），并在 `assets/sfx/` 放同名 `.wav`、把名字加进 `UI.Presentation.soundNames`（网页经 `m3Meta` 取，JS 不用改）；
4. 真正新的表现方式（新的 `StageKind`）才需要：`StageKind` 加构造子 → 表里加一行 `LookStage 新段` → 网页 `web/www/render.js` 的 `drawEndStage` 加绘制分支；测试 `presentation_table_covers_every_event_kind` 会检查每种事件恰有一行、每个段恰有一种事件使用。

### 视图模型（第 11 刀）

`src/Match3/View.hs` 是「从 `GameState` / 回放状态算前端要画的东西」的唯一一处，纯函数、字段惰性（不用的读数不算）。网页版的 `encodeState` / `encodeGoal` / `encodeCell` / `apiLevels` 都读它；各字段是第 11 刀前各前端现算式的逐字搬迁（`view_*` 测试对照字面副本）。

```haskell
data GameView = GameView
  { gvLevel, gvLevelIndex :: Int      -- gsLevel 原值（标题 / 网页）；夹紧下标（关名图、进度点）
  , gvLevelName, gvRawName :: String  -- 夹紧下标的关名（标题）；原下标关名，无此关 "?"（网页）
  , gvDaily :: Bool, gvRules :: [String], gvScore, gvMoves :: Int
  , gvBoosters :: Boosters            -- bHammers / bFreeSwaps / bCrossClears
  , gvCombo :: Int                    -- 经通用接口 gameStatus 取
  , gvShuffled :: Bool, gvOver :: Maybe Terminal
  , gvStatus :: PlayStatus            -- PlayWon s | PlayCleared s n | PlayLost s | PlayShuffled | PlayOn
  , gvGoal :: GoalInfo                -- giGoal / giView / giProgress / giTarget / giKind / giText / giName / giLoseHint
  , gvBoss :: Maybe BossView          -- 雪怪血条（新玩法 5）
  , gvBoard :: BoardView              -- bvBoard / bvFoundHint（findHint）/ bvLastCleared / bvGround
  }                                   --   / bvBelts / bvPortals / bvUfos / bvCarpets / bvCarpetOpen / bvDrops
```

| 函数 | 用途 | 读取方 |
|------|------|--------|
| `titleLine gv` | 标题（不含 `"  \|  "` 与消息） | `Match3Web.Api`（原桌面窗口标题） |
| `goalLine` | 标题目标段 | `titleLine` |
| `bvDrops bv` | 新玩法 6：掉落口格（`Element.Level.levelDrops`；没有掉落口为空） | 网页 `state.drops`（原桌面 `drawDropsArt` / `drawDropMark`） |
| `levelDots cur maxReached` | 各关进度点 `DotCurrent` / `DotDone` / `DotUnlocked` / `DotLocked`（网页传夹紧下标与最高解锁；原桌面几何版传原值与 −1） | 网页 `m3Progress` 的 `dots`（原桌面 `UI.HudArt` / `UI.HudBlocks.hudLevel`） |
| `scoreBadge replay summaryLeft best gv` | 右下角：`BadgeCombo n`（回放中连击 ≥2）/ `BadgeRolling 分`（回放中）/ `BadgeSummary n`（播完后的总结）/ `BadgeScore 洗牌? 分`；回放状态由前端从 `Cascade` 换成 `ReplayView{rvCombo, rvShownScore}` | 网页 `m3Badge`（原桌面 `UI.HudArt` / `UI.HudBlocks.hudComboBadge`） |
| `levelViews` | 关卡列表（序号 / 名字 / 步数 / 目标） | 网页 `apiLevels` |
| `cellFace cell` | 单格结构化描述（类型标签 + 按固定顺序的 `CellField` 字段） | 网页 `encodeCell` |

网格交互（像素 ↔ 格、点选 / 拖划、高亮）在网页 `web/www/main.js` / `layout.js` 里；原通用层组件 `Engine.GridUI` 只给 SDL 桌面版用，已于 `refactor/web-only-2` 删除。

**加一个要显示的读数**：在 `GameView`（或 `GoalInfo` / `BoardView`）加字段并在 `gameView` 里算，网页 `Match3Web.Api` 从视图读；不要在前端再从 `GameState` 现算（`frontends_read_view_model` 扫描网页 API）。

### 扩展钩子（段 2c）

段 2c 把「新增元素只经注册表（今 `World`）接入」补齐到下列类别，主流程（`Game.Resolve` / `Board.*`）不再需要为新元素改代码。`Engine.*` 在 2c 中**没有改动**。

| 钩子 | 类型 / 入口 | 用途 | 护栏测试（`test/Spec/Extension.hs`，样例元素只在测试里） |
|------|-------------|------|------|
| 世界下传到底 | `Board.*With w`；旧名在 `Board.Default` | Board 层不依赖内置表 | `ext_board_modules_take_world`（源码扫描） |
| 按名字的目标 | `LevelGoal` 新增 `GoalNamed 名字 N`（第 5 刀起写作 `goalCount (CountNamed 名字) N`；读 `gsCount (CountNamed 名字)`，由 `counter` / `diffCounter = CountNamed 名字` 累加）；HUD / 标题 / 失败提示 / 选关已接 | 自定义元素当关卡目标 | `ext_goal_named_counts_crate` |
| 地面层 | `GroundKind`（`groundHit`，元素类重构前是 `SlotGround` + `groundRule`）+ `gsGround`（关卡记录的 `lvlGround`） | 果冻类「格子下面的层」 | `ext_ground_layer_test_element` |
| 扩爆（新玩法 8） | 地面层种类的 `groundWiden`（元素类重构前是能力 `widens`）→ 每步 `levelWorldIn` 写本步上下文 `StepCtx` → `blastWith` | 魔法地格类「在这格引爆的特效范围变大」 | `mg_blast_widened_only_at_magic_cell`、`mg_other_levels_unchanged`（`test/Spec/MagicGround.hs`） |
| 边缘收集 | `Movable.drains :: e -> [Edge]` | 任意方向的收集物 | `ext_edge_drain_side_collectible` |
| 步末补结算 | `EndRule.erHoles` + `Cascade.cascadeAfterWith (AfterEnd …)` | 步末阶段挖掉格子后的沉降 / 补子 / 再连锁 | `ext_post_end_settle_hole_element` |
| 洗牌走世界 | `Game.Shuffle.shuffleGameWith w`、`Game.State.applyHintWith w`（`Engine.playWith` 的 Shuffle / Hint 分支） | 自定义元素的洗牌保留 / 提示 | `ext_manual_shuffle_keeps_crate_via_engine` |

**步末补结算选统一路径（无开关）**：交换与道具的步末之后一律经 `cascadeAfterWith (AfterEnd 空洞)`：先挖 `erHoles` 的空洞，再做边缘收集 + 补子；盘面有变化或有格被收走时记一个只含沉降的轮次，之后成消再接普通连锁；什么都没发生时原样返回、**不消耗随机数、不加轮次**。对内置元素它恒为空操作，依据：① 类型层面——`Board` 不能表示空洞，内置 `erHoles` 全是 `const []`；步末阶段（倒计时 / 皮带 / 蔓延 / 蜗牛）不移动饼干（蜗牛把饼干当障碍），皮带后的再连锁本身已含沉降；`refill` 在没有空洞时不取随机数。② 实测——38 关 × 种子 1..100 × 15 步，每步检查主交换、三种道具与全部可成交的交换对，共 **610,751** 手，步末终盘上待挖空洞 / 待收边缘 / 沉降变化全部为 0；金标准 2344 行全等。

### 新增一种元素的步骤

1. 选层：本体用 `Custom "名字" (CustomState 值)`（值自定义，例如耐久；存储编码只能是一个 `Int`，在元素的 `toCell` / `fromCell` 这一处转换；元素类型就是 `newtype X = X Int` 时可以不写 `toCell`，缺省实现写成 `Custom (nameOf x) (CustomState n)`，解码用 `fromCustom "名字" X`，见 [haskell-features/02 §1.2](haskell-features/02-类型类与抽象.md#12-defaultsignatures)）；格子下面的层写 `GroundKind`（层数放进地面层状态）；需要新的内置层时才动 `Types`。
2. 写 instance：定义一个类型（状态放在值里），写能力 instance 与 `instance Kind`（测试 / 扩展元素写在自己的模块里；新的**内置**元素放进 `src/Match3/Element/Builtin/` 下功能最接近的分组文件——宝石 `Gem`、冰 / 叠层 `Layer`、打破型障碍 `Obstacle`、收集计数 `Collectible`、会动 / 会生成的 `Actor`、地面层 `Ground`、关卡级 `Level`——跨分组共用的辅助放 `Common`）。`Cellular` 必写（`nameOf`，非 `Int` 表示时还有 `toCell`）；其余能力类只覆盖与普通宝石不同的方法，障碍类用 `deriving (Matchable, Movable) via (Obstacle 类型)`（不下落用 `Fixed`），没有要覆盖的写空 instance（`instance Renders X`）。`Kind` 写 `kindName`（与 `nameOf` 相同，测试钉住）/ `fromCell` / `place`（`Custom` 用 `customPlace "名字"`），做成目标时 `label` / `loseHint`。邻格规则：能写成「邻格真消除时这一格怎么变」的用方法 `neighbourPrio`（选一个不和现有次序冲突的数，见上文「邻格规则顺序」）/ `reach` / `onNeighbourClear`；其余规则（步末、成对交换、开启、读整盘的邻格规则）挂在 `boardPasses`。步末要挖掉格子时给 `EndRule` 填 `erHoles`，补结算自动发生。叠层写 `instance Layer`；不在格子里的机制写 `instance Mechanic`，实现需要的节拍方法。
   - 只经元素世界即可接入的类别：本体 `Custom`（削层 / 打碎 / 免疫 / 挡交换 / 下落 / 洗牌保留，也可以是按颜色匹配的有色棋子：覆盖 `color`，见 `ec_custom_matchable_gem`）、叠层与冰的命中规则、地面层、任意方向的边缘收集物（`drains`）、带 `erHoles` 的步末元素、以 `CountNamed` 计数并用 `GoalNamed` 当目标的元素、成对交换规则（`SwapPass`）、开启类元素（`OpenPass`）、可被改色 / 推动（`recolorable` / `pushable`）、不进普通匹配提示（`hintable`）、在已有节拍上反应的关卡级机制（`Mechanic`，见 `ec_mechanics_by_beat`）；第 8 刀起还有特殊块形状规则（`setShapeRules`，见 `ext_shape_rule_lt_bomb`）、特殊块组合规则（`setComboRules`，见 `ext_combo_rule_line_gem`）、补子策略（`setRefillPolicy` 或机制的 `refillPolicy` 节拍，见 `ext_refill_policy_level_colors` / `ext_refill_policy_level_element`）。
   - 段 5 的双层果冻（`Jelly`，在 `Element.Builtin.Ground`：`GroundKind`，`groundHit` 去层、`groundCounter = CountNamed "jelly"`）与气泡（`Bubble`，在 `Element.Builtin.Collectible`：`Custom` 障碍，命中即碎、邻消 170、`counter = CountNamed "bubble"`）就是这样接入的内置元素：规则只在 instance 里，关卡数据在 `Levels.Campaign` 的关卡记录里（放置表 `lvlPlacements` / 地面层 `lvlGround`），主流程没有改动（`jb_main_flow_untouched_scan`）。新玩法 2 的魔法石（`MagicStone`，`Fixed` 原型包，命中反应随状态变：平时 `Immune`、发射中 `Absorb` 归零；邻格充能 180、步末 `tickRule 20`）、新玩法 3 的毛球（`Fuzzball`，`Obstacle` 原型包，`boardPasses = [AdjacentPass 190 …, EndPass (moveRule 20 …)]`）同样只加 instance 与类型列表一项；要专门画法时在网页 `web/www/cells.js` 的 `CUSTOM_ART` 加一项（见下文步骤 5 与 [web.md §2.3](web.md#23-js-渲染器)）。
   - 新玩法 5 的雪怪 Boss（`SnowBoss`，在 `Element.Builtin.Obstacle`）：多格元素用四个固定格表达，元素类重构第 3 刀起写成 `Entity`（`footprint` / `partNo` / `hitPoints` / `withHp`），扣血由通用驱动 `entityDamage` 算；主流程唯一的改动是通用的差计权重（`Countable.diffWeight`，`Game.Tally.diffCountsWith` 求加权和；缺省 1 时与原来的个数差相同）
   - 新玩法 6 的饼干掉落口（`CookieDrop`，在 `Element.Builtin.Level`）：机制实现补子策略节拍 `refillPolicy`（第 8 刀），把策略包一层「掉落口格补收集物」（`dropRefill`，随机数照常消耗）；配置在关卡记录的新字段 `lvlDrops`（缺省 `[]`）。主流程只多一个条件：有掉落口的关卡开局跳过目标补齐（`Game.Level.newGameAtLevelWith`）
   - 新玩法 7 的变色龙（`Chameleon`，在 `Element.Builtin.Collectible`）：`Custom` 本体，覆盖 `color` / `keepOnShuffle` / `recolorable` / `counter`，`boardPasses = [SwapPass (SwapRule 15 …), EndPass (moveRule 40 …)]`，主流程不改；第 47 关用新玩法 6 的掉落口在补子时补进变色龙
   - 新玩法 8 的魔法地格（`MagicGround`，在 `Element.Builtin.Ground`）：`GroundKind`，只写 `groundWiden = Just magicWiden`（没有 `groundHit` = 不被消耗、不计数）；为它加了一个缺省什么都不做的通用钩子（`groundWiden` / 本步上下文 `StepCtx` / `blastWith` 改写，见「规则表」后的扩爆格一段）。主流程的改动只有 `Element.Level.levelWorldIn` 多写一项与 `Engine.playWith` 展开事件改用 `levelWorldIn`；第 48 关的地面层写在关卡记录 `lvlGround`
   - 仍需改主流程的：需要**新节拍**的关卡级机制（在 `Mechanic` 加带缺省的方法，并在主流程固定位置调用）、需要存进 `GameState` 的关卡级状态（见下节「遗留」）。（补子时生成自定义棋子已可经掉落口 `lvlDrops` 做到，见新玩法 7。）
   - 显示：有状态的 `Custom` 元素要给前端额外字段（象限、当前颜色……）写 `Renders.face`，要换网页 JSON 的类型标签 / 基本字段写 `Renders.faceBase`（第 6 刀）；做成关卡目标要中文名 / 失败提示时写 `Kind.label` / `loseHint`；`View` / 网页 `Api` / `GoalLabel` / `Game.Outcome` 不改（`ext_element_display_fields`）。
   - 胜负条件：内置规则判出结局后经机制的 `judge` 节拍复核（`judgeIn`），新的输赢法（限时、某物落底即输……）只写一个实现 `judge` 的 `Mechanic`，不改 `Game.Outcome`。
3. 注册：内置元素 = 在 `Element.Builtin.builtinDefs` 末尾加一行 `kindDef @类型`（已有项不要重排：注册顺序被元素查询快照的 `R` 行锁定；关卡级机制加进 `builtinMechanics`）；测试 / 扩展元素 = `register (kindDef @类型) defaultWorld`（叠层 `layerDef`，地面层 `groundDef`，关卡级机制 `registerMechanic (SomeMechanic 原型值)`，开局状态写在 `mechStart` 里，开局 / 走子用 `newGameAtLevelWith w` 与 `*With w` 入口），把世界传给 `*With` 入口（`trySwapWith` / `resolveSwapWith` / `resolveHammerWith` / `ensurePlayableWith` / `shuffleGameWith` / `applyHintWith` / `decorateLevelWith` / `traceEventsWith`），或整体用 `Match3.Engine.match3GameWith w`。
4. 放置：在关卡放置表里写 `Place "名字" [参数] [坐标]`，由 `Kind.place` 落格（`customPlace "名字"` = `Custom 名字 第一个整数参数`；要别的解析就自己写 `Placer`，参数解析器见 `Element.Types` 的 `ArgP`）。
5. 表现：贴图名即元素名（`assets/` 里放同名贴图，缺图时画灰块）；要专门画法的 `Custom` 在网页 `web/www/cells.js` 加画法，颜色在 `UI.Presentation.elementRGBTable`（HUD 目标 / 地图 / 步末前沿与碎屑共用，网页经 `m3Meta` 读）；步末效果的播放、生长曲线与音效见[前端表现表](#前端表现表第-10-刀)的「给新元素加表现和音效」；网页端什么时候要改 `web/www/cells.js` 见 [web.md §2.3](web.md#23-js-渲染器)。
6. 测试：参照 `archetype_ext_element_plugs_in`（`test/Spec/Archetype.hs`，最短的完整例子：障碍「荆棘」`Thorn` 不改主流程接入）、`element_world_custom_crate_extensibility`、`test/Spec/Extension.hs` 与 `test/Spec/ElementClass.hs`（测试专用「木箱」`Crate` 只定义在测试辅助 `test/Spec/Support.hs`，断言它削层、打碎、计数、挡交换、被锤、洗牌保留，并断言核心源码里没有它的名字）。

### 专门分支的收编（段 4）

段 4 把原先写死在主流程里的专门分支收进元素框架：主流程只经注册表（今 `World`）取规则，内置实现挪到 `Element.Builtin` 的定义里。金标准 2344 行全等、三场景截图 AE=0。**段 4 没有改动 `Engine.*`**（只动了 `Match3.*`）。

| 原专门分支 | 段 4 之后（元素类重构后的写法） | 主流程入口 |
|------------|-----------|------------|
| 彩虹取色（`Move` / `Boosters` / `findHint` 直接调 `isRainbowSwap` / `rainbowClearSeeds`） | `SpecialGem 'Rainbow` 的 `boardPasses = [SwapPass (SwapRule 10 isRainbowSwap rainbowClearSeeds)]` | `World.swapOpeningWith`、`findHintWith` 逐条 `swapRules` |
| 特殊 × 特殊合成（`isSpecialCombo` / `comboClearSeeds`） | `SwapRule 20`，挂在 `SpecialGem 'LineH` 上（规则自己检查两端）；第 8 刀起改为世界的组合表 `comboRules`（内置 `builtinComboRules`），整张表并成一条 `SwapRule 20` | 同上 |
| 彩蛋开启（`Clear` 直接调 `Obstacles.openSurprises`） | `SurpriseEgg` 的 `boardPasses = [OpenPass (OpenRule openSurprises)]` | `World.openWith`（`surpriseClearPassWith` 成为通用的「开启类元素」流程） |
| 魔法帽 / 染色瓶只改 `isGem` 的格 | `Movable.recolorable`（缺省 True：宝石各种类、倒计时、双面块；`Obstacle` / `Fixed` 为 False） | `AdjCtx.acRecolor` → `triggerAdjacentHatsBy` / `triggerAdjacentBottlesBy` |
| 蜗牛只推 `Snail.pushable` 的格 | `Movable.pushable`（同上） | `EndCtx.ecPushable` → `stepSnailAtBy` / `traceSnailsBy` |
| 飞碟（`Cascade` 直接调 `stepUfos`） | `UfoLevel [Ufo]` 实现节拍 `onRefilled`（状态在值里） | 钩子 `onAbsorb` |
| 皮带（`Resolve` 直接调 `beltMoves`） | `BeltLevel [Belt]` 实现 `onEndTick` / `avoidCells` | `Element.Level.beltShiftIn` |
| 传送门（`Gravity` 里写死实现） | `PortalLevel [(Pos,Pos)]` 实现 `onSettling` / `wallCells` | 钩子 `onSettle` |
| 地毯（`Resolve` 直接调 `coverCarpets`） | `CarpetLevel [Pos]` 实现 `onCover` | `Element.Level.coverIn` |
| 提示排除彩虹本体（`findHint` 直接调 `Rainbow.isRainbow`；元素类迁移收编） | `SpecialGem 'Rainbow` 的 `hintable = False` | `Board.Match.findHintWith`（`hintableWith`） |

关卡级机制是 `Mechanic` + 有类型的节拍方法（见上节「元素的能力与调用时机」），经 `registerMechanic` / `removeMechanic` / `mechanicDefs` 增删查；未注册即不生效（测试 `br_level_hooks_*`）。

护栏（`test/Spec/Branches.hs`，样例元素拉杆 / 豆荚 / 小车只在测试里）：测试专用成对规则、开启规则、可推动元素只经元素世界生效；内置谓词与原写死谓词逐格相同；去掉关卡级机制后飞碟 / 皮带 / 地毯不生效；源码扫描 `br_main_flow_no_special_branches`（`Move` / `Boosters` / `Match` 不点名彩虹取色与特殊合成，`Clear` 不 import `Obstacles`，`Cascade` 不用 `stepUfos`，`Resolve` 不用 `coverCarpets` / `beltMoves`，`Gravity` 不用 `portalWith`）。

**遗留（抽不干净的，及原因）**：

| 遗留 | 状态 | 原因 |
|------|------|------|
| 钩子调用时机写死在主流程（吸收在补子后、移位在 Tick 与 Spread 之间……） | 部分消掉 | 节拍仍是流水线的固定位置（改节拍 = 改规则流水线）；但哪个机制在哪个节拍反应、回复什么，由机制自己的方法决定；元素类重构第 5 刀起没有开放消息，新节拍 = `Mechanic` 加一个带缺省的方法 |
| 传送门端点当会走元素的墙（`wallCells`）只问注册了的传送门 | 有意如此（仅影响非内置世界） | 与传送本身一致：去掉 `portal` 注册后端点不再当墙。内置世界不受影响 |
| 自定义元素的存储编码是 `Custom ElementName CustomState`（两个 newtype 各包一个 `String` / `Int`） | 保留 | 盘面以 `Cell` 存储（金标准、前端、机制模块都按它读写）；元素值由构造器从这个 `Int` 解码，所以自定义元素的状态只能编码成一个整数 |
| 地面层种类不带值 | 元素类重构第 2 刀起 | 地面层在 `GroundLayer` 的状态里（`gsGround` 读数）、不进盘面，`GroundKind` 是不带值的类型（方法吃 `proxy`），层数只在状态里 |
| 地面层是核心关卡级机制（`mechCore`，定义在 `Element.Mechanic`），不在 `mechanicDefs` 里 | 保留（第 7 刀） | 地面层里每层的行为已由世界里的地面层种类（`groundDef`）决定，关卡级这一层只是承载；把它加进 `mechanicDefs` 会改变元素查询快照的 `R level` 行 |
| 冰层 / 叠层没有注册种类时的兜底 | 行为略有不同 | 解码时没人 `peel` 这一层，它原样留在格子里交给本体解码（`decodeLayers`），不当叠层询问。内置世界恒含全部冰层 / 叠层种类，不影响内置元素与金标准 |
| `Game.EndPhase`（皮带行）仍 import `Conveyor.applyBeltMoves`；`Trace` 再导出 `beltMoves` | 保留 | `applyBeltMoves` 是「按 (原格, 新格) 列表移格」的通用搬运，与皮带语义无关；`Trace` 的再导出给回放与测试用 |
| `Rainbow` / `Combos` / `Obstacles` / `Snail` 里的规则实现本身 | 保留 | 实现仍在各自模块，只是改由 `Element.Builtin` 的 instance 引用；第 3 刀起能写成方法的邻格 / 蔓延规则已由通用驱动 `Match3.Element.Rules` 执行，旧的 `chipAdjacent*Except` / `spreadLayer` 等删除（只剩气球的 `chipAdjacentBalloonsExcept`；测试包装在 `Spec.Support.Obstacles` / `Spec.Support.Layers`） |
| `EventKind` | 保留 | 封闭枚举，是前端播放表的键；步末效果本身是通用形状（蔓延种类即元素名），新步末元素复用已有事件类型（如 `EvMove`）即可接入 |

### 元素类

元素是 xmonad `LayoutClass` 风格的类型类：一种元素 = 一个类型 + 几个 instance，状态放在元素值里（测试专用「鸟窝」`Nest` 演示跨轮状态）。

- **类型列表**：内置世界 `builtinDefs` 是 36 项有序的类型列表（22 个本体类型，`SpecialGem` 用 DataKinds 一个类型覆盖 line_h / line_v / bomb / rainbow 四项；冰层与 8 个叠层是 `Layer`；果冻、魔法地格是 `GroundKind`），关卡级机制是 `Mechanic`。一格解码成 `Layered … (SomeElement 本体)` 后直接调能力方法。
- **分文件**：内置元素按功能分在 `src/Match3/Element/Builtin/` 下的 `Gem` / `Layer` / `Obstacle` / `Collectible` / `Actor` / `Ground` / `Level` / `Common`（各文件开头的注释写明这一组的共同特征），每个元素的类型与 instance 放在同一文件；`Element.Builtin` 只按注册顺序汇总类型列表。彩蛋归 `Obstacle`：它是 `Obstacle` 原型包、命中即破的占格本体，没有计数（不是收集类），也不按颜色匹配（不是宝石）。只在一组里用到的辅助留在组内（`chip` / `layersPlace` 在 `Obstacle`，`spreadRule` 在 `Layer`，`tickRun` / `snailRun` / `traceSnails` 在 `Actor`），跨组共用的才进 `Common`（`nField` / `colorField` 等显示字段辅助也在这里）。依赖只朝一个方向：`Obstacle` 引用 `Gem`（双面块翻成宝石）和 `Collectible`（保险箱开成饼干），`Level` 引用 `Gem`（规则开关 `BombShapes` 用 `withBombShapes`），其余分组互不引用（`Common` 除外）。
- **透明性护栏**：`Spec.ElementAbility` 按源码核对 `SomeElement` 转发、`Layered` 合成每个能力方法并都进了 `abilityProbe`，装箱前后逐项相等；`Spec.Archetype` 钉住普通宝石 / `Obstacle` / `Fixed` / `Inert` 的缺省方法值。

**护栏**：元素查询快照 `test/golden/element-queries.txt`（1666 行，生成器 `test/golden/ElementQueries.hs`）锁定全部内置元素的行为：Q 行 = 全部格子组合上的逐格查询，P = 放置，R = 规则表 / 条目名 / 个数差计数 / 关卡级机制清单，A / E / S / O / C / G = 邻格 / 步末 / 成对交换 / 开启规则、直接命中、地面层在样例盘上的输出，M = 每关 × 种子 1–2 × 12 手（提示、锤子、十字、交换）的逐手散列；由三个 `ec_*_snapshot` 测试比对。这份快照最初在扁平记录 `ElementDef` 改成类型类时生成。元素类重构第 0 刀另加元素对照快照 `test/golden/element-oracle.txt`（3623 行：更多样本格 × 全部值查询、逐条规则在 400 张随机盘上的散列与起作用盘数、整轮查询，`Spec.ElementOracle` 的 4 个用例），第 1–6 刀全程逐字节不变。

### 与 xmonad LayoutClass 的对照

| xmonad | 本项目 | 说明 |
|--------|--------|------|
| `class LayoutClass layout a`（`doLayout` / `handleMessage` / `description` 等方法都有默认实现） | 六个能力小类（`Cellular` / `Matchable` / `Hittable` / `Movable` / `Countable` / `Renders`，类同义词 `Element`）+ 类型级 `Kind` | 一种元素 = 一个类型 + 几个 instance，只覆盖与普通宝石不同的方法 |
| 默认方法之间互相推（`doLayout` 默认调 `pureLayout`） | 每个方法带「普通宝石」缺省；障碍类经 DerivingVia 原型包（`Obstacle` / `Fixed`）一次拿到另一组缺省；颜色缺省取 `toCell` 写回的宝石格 | 实例只写与缺省不同的能力 |
| 布局的状态在值里（`Tall nmaster delta frac`），`handleMessage` 返回新布局 | 元素的状态在值里（`StoneE 层数`），`struck` 返回 `Absorb 新格子`（可换成别的元素：保险箱开成饼干） | 盘面仍存 `Cell`，`toCell` 写回、`fromCell` 解码 |
| `data Layout a = forall l. LayoutClass l a => Layout (l a)` | `data SomeElement = forall e. Element e => SomeElement e`、`SomeMechanic` | 存在类型装箱；Eq 按具体类型（`cast`）比、类型相同再比状态（共用 `sameTypeEq`）；详见 [haskell-features/02 §1.3](haskell-features/02-类型类与抽象.md#13-存在类型与-xmonad-风格的元素类) |
| `LayoutModifier` / `ModifiedLayout m l` | `Layer` / `Layered l e`（冰层、叠层） | 本层先说，没意见再问里面；`Pierce` / `Keep` / `Peel` / `Shatter` 对应命中时的四种组合 |
| `layoutHook`（配置里的布局列表） | `World`（有序的类型列表 `[Def]`，`kindDef @T` …）+ `defaultWorld` | 世界决定解码顺序与规则次序 |
| `Message` / `SomeMessage` / `fromMessage`（Typeable） + 事件循环发消息 | 元素类重构第 5 刀起改为 `Mechanic` 的**有类型方法**（`onRefilled` / `onEndTick` / `onSettling` / `onCover` / `onGroundHit` / `shapes` / `morph` / `judge` …），缺省 `Nothing` | 主流程在流水线节拍上调方法（`beatIn` / `queryIn` 折叠所有回复者），机制交回推进后的自身；不再有开放消息，新节拍 = 新方法 |
| 布局组合子（`|||`、`Choose`）在运行时切换布局 | 无对应 | 一格同一时刻只有一种本体；世界按格子编号 / 名字选种类 |
| `description` 用于显示与序列化 | `nameOf` / `kindName`（贴图名、计数键、放置表键、事件里的 `evElement`） | — |

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
└──────────────▲───────────────────────────────▲──────────────────────────▲────────────────┘
               │ 实现 Game                      │ 用 Player + 自己的 Stages  │ 只调 gameStep match3Shell
┌──────────────┴─────────────┐  ┌──────────────┴──────────────┐  ┌────────┴──────────────────┐
│ 三消实现（库）              │  │ 三消回放（app/pure/ComboFx）  │  │ 网页接口层（web/hs）        │
│ Match3.Engine：match3Game、│  │ cascadeStages：高亮→消失→下落 │  │ Match3Web.Api / Anim：     │
│ Action、match3Shell、toEffect│  │ →落定 / 步末阶段；WaveView    │  │ JSON 编码，JS 绘制与输入   │
│   ▲ Match3.Game.* / Board.* │  │                              │  │（原 SDL 外壳 Shell.Loop 与 │
│   │ / Element.*（规则）      │  │                              │  │  插件 app/UI.* 已移除）    │
└────────────────────────────┘  └──────────────────────────────┘  └────────────────────────────┘
测试：test/Toy.hs（只 import Engine.*）+ engine_toy_counter_game
```

### 接口字段

`Engine.Game.Game cfg s a e o r`（cfg 开局配置、s 状态、a 动作、e 本游戏的效果事件、o 结局、r 整步报告）：

| 字段 | 类型 | 说明 | 三消（`Match3.Engine.match3Game`） |
|------|------|------|------------------------------------|
| `gameName` | `String` | 名字 | `"match3"` |
| `gameNew` | `cfg -> Seed -> s` | 开局；**唯一**接受外部种子的地方 | `Setup`：`Campaign 关卡下标` / `CustomLevel 配置` / `Daily 年 月 日`（每日的种子由日期决定）/ `LevelSetup 关卡记录`（任意完整关卡记录，按记录铺装饰、开关卡级元素、定行列，用本实例的元素世界；`Game.Level.newGameForLevelWith`） |
| `gameStep` | `s -> a -> Step s e o r` | 纯函数推进一步，随机数只来自 `s` | 终局时拒绝走步与洗牌（提示除外：只写 `gsHint`，与原前端终局后按 H 的行为一致）；否则 `playWith world`，`stepReport = Just Played` |
| `gameOutcome` | `s -> Maybe o` | 结局判定 | `gsOver`（`o = Terminal`：`TWon` / `TLost` / `TLevelClear`） |
| `gameActions` | `s -> [a]` | 当前会被接受的动作（测试 / 自动演示） | 所有会成交的相邻交换 |
| `gameStatus` | `s -> [(String, Int)]` | 给外壳的具名数值 | level / score / moves / combo / hammers / freeSwaps / crossClears（标题栏的连击数从这里取） |
| `gameEffect` | `e -> Effect` | 本游戏事件 → 通用效果 | `toEffect`：节拍 = `evWave`，种类 = 事件种类标签，主体 = `evElement`，格 = 每对的目标格，数量 = `evAmount` |

`Step{stepState, stepEvents, stepOutcome, stepAccepted, stepReport}`：被拒时状态不变、没有事件。`stepReport :: Maybe r`（段 3）放本游戏自己的前端才需要的整步数据（三消：`Played`），通用层原样带出、不解释——有了它，外壳执行动作只调 `gameStep`，不必绕过接口调游戏自己的入口。

**撤销历史（`Engine.History`，段 3）**：`withHistory policy game` 把 `Game cfg s a e o r` 变成 `Game cfg (History s) (Undoable a) e o r`。`Act a` 交给原游戏，被接受且 `hpRecord a` 时先记 `hpSnapshot` 过的旧状态（最多 `hpLimit` 份）；`Undo` 由本层处理，有历史就回到最近一份快照（经 `hpRestore`），**不经原游戏的终局拒绝**，所以终局后仍可撤销；`gameActions` 在有历史时多一个 `Undo`，`gameStatus` 多一项 `undo`（可撤销步数）。组合子：`runActions`（依次执行，遇到结局即停）、`finalState`、`stepEffects`、`rejectedStep`。

`Engine.Effect.Effect{efBeat, efKind, efSubject, efSpots, efAmount}`：节拍相同的效果同时播放（`beats` 按连续节拍分组）。

`Engine.Playback`：`Stages{stageLen, stageNext}` 由游戏给出（阶段多长；播完后 `Right (下一阶段, 进入时触发的事件)` 或 `Left 最终状态`）；`Player{plStage, plFrame, plFast}` 只管时钟：每帧推进 1 帧、加速后推进 `fastStep` 帧，到达阶段长度就切换、帧归零。`playerProgress` 给出阶段进度 0..1。简单游戏可直接用 `effectCues framesFor effects` + `cueStages`。

（原 SDL 外壳 `Shell.Loop` 的插件记录 `Plugin w{plugInit, plugBegin, plugEvents, plugTick, plugDraw}` 已随桌面版移除；网页前端的帧循环在 `web/www/main.js`，每步经 wasm 导出调 `gameStep match3Shell`。）

**取舍：record-of-functions，而不是带关联类型的 typeclass。** 同一种游戏可以有多份配置不同的实例（三消：`match3GameWith world` 接任意元素世界），记录是一等值，类型类按类型只能有一个实例；不需要 `TypeFamilies` / 孤儿实例，玩具实现在测试里写一个值即可；状态、动作、事件、结局都是普通类型参数，推断直接。代价是没有「按类型自动找实例」，调用处要显式传 `Game` 值——对只有少数几个游戏的项目这是好事。

**随机数与种子约定。** 随机数生成器放在状态里（三消是 `gsGen :: StdGen`），`gameStep` 是纯函数：同一状态 + 同一动作 ⇒ 同一结果；只有 `gameNew` 接受外部种子（`Seed = Int`），由外壳 / 测试决定，规则层从不读时钟或做 IO。

### 三消的动作映射

| `Match3.Engine.Action` | 规则入口（每个动作只结算一次） | 被拒条件 | 事件 |
|------------------------|--------------------------------|----------|------|
| `Swap p q` | `resolveSwapWith` | `NoMatch` / `InvalidSwap` | `traceEventsWith world`（回放脚本展开） |
| `Hammer p` / `FreeSwap p q` / `CrossClear p` | `resolveHammerWith` / `resolveFreeSwapWith` / `resolveCrossClearWith` | 同上 | 同上 |
| `Hint` | `applyHint`（写 `gsHint`，`pdHint` 带回提示） | 从不 | 无 |
| `Shuffle` | `shuffleGame` | 已结束 | `EvShuffle` |

撤销不是三消的动作：外壳用 `match3Shell`（`withHistory match3History match3Game`），动作是 `Act (Swap p q)` / … / `Undo`。整步报告 `Played{pdState, pdOutcome, pdTrace, pdFx, pdEvents, pdHint, pdAccepted}` 经 `stepReport` 带回；界面行为（终局后仍可撤销、终局后按 H 仍给提示）与原来直接调 `play` 完全相同。测试：`engine_match3_instance_matches_direct_api`（`gameStep` 与直接调旧入口逐位相同）、`engine_undo_after_terminal_matches_legacy_play`（终局后撤销 = `13094d1` 的 `play Undo`）、`engine_frontend_steps_only_via_gameStep`（`app/pure` + `web/hs` 源码扫描）。

**段 3 对 `Engine.*` 的改动**：`Engine.Game` 的 `Game` / `Step` 多一个类型参数 `r` 与字段 `stepReport`（原因：前端要的回放脚本 / 特效 / 提示原来只能从 `play` 拿，要让外壳只调 `gameStep`，接口必须能带出整步报告）；新增 `Engine.History`。玩具 `test/Toy.hs` 的 `r = ()`，其余不变。

### 接入一个新游戏的步骤清单

1. **类型**：定义开局配置 `cfg`、状态 `s`（含随机数生成器）、动作 `a`、效果事件 `e`（纯数据）、结局 `o`。模块放在自己的命名空间（如 `src/Foo/`），**不** import `Match3.*`。
2. **写 `Game` 值**：`gameNew`（只在这里用种子）、`gameStep`（纯；非法动作返回 `rejectedStep` 式的结果）、`gameOutcome`、`gameActions`、`gameStatus`、`gameEffect`。
3. **纯测试**：参照 `test/Toy.hs` 与 `engine_toy_counter_game`——`runActions` 走到胜 / 负、非法动作被拒且不改状态、结局后拒绝一切、`gameActions` 全部被接受、效果按节拍播放的帧数（含加速）。
4. **回放**：时间线简单就用 `effectCues` + `cueStages`；复杂时间线自己写 `Stages`（参照 `ComboFx.cascadeStages`），交给 `Player`，绘制时用 `playerProgress` 取进度。
5. **前端**：前端要画的读数写成一个纯的视图模型模块（参照 `Match3.View`），前端只读它。
6. **外壳**：参照 `web/hs/Match3Web/Api.hs` 写接口层：把输入映射成动作并调 `gameStep`（要撤销就用 `withHistory` 套一层，历史不要放进游戏状态；前端要的额外数据放进 `stepReport`），状态与回放帧编码给 JS 绘制。
7. **依赖检查**：新游戏与通用层之间只允许「新游戏 → Engine.*」；需要时把新的通用文件加入 `engine_layer_is_game_agnostic` 的检查列表。
8. **文档**：在本节的分层图与模块地图里登记新模块。
