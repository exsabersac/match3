# 重构总结（2026-09，11 刀）

2026-09-30 01:10 – 08:34（北京时间）在 `main` 上做完的一轮纯重构：拆了 11 刀（第 6、7 刀各分 a / b 两步，共 13 个提交），每刀单独推送。**内置游戏的规则、随机数消费顺序、网页 JSON、桌面画面都没变**；少数行为差异只出现在「非内置注册表」和「越界关卡下标」上，见[行为差异汇总](#行为差异汇总)。本轮没有加新玩法。

起点是 `870748d`（GHC 9.14.1 升级与网页版合入之后），测试 262 个；终点 `8f153c7`，测试 331 个。

## 每刀一览

| 刀 | 提交 | 要点 | 测试数 |
|----|------|------|-------:|
| 1 | `05281e0` | 测试基建：源码扫描工具集中到 `test/Spec/Support/Source.hs`（去注释 / 去字符串 / import 解析 / 标识符匹配，按目录列文件）；辅助函数去重；去掉 `-Wno-x-partial`（16 处 `head` / `last` 改掉）；`test/Spec/Obstacles/*` 按 `Element/Builtin` 分组拆成 `test/Spec/Builtin/*`；新增 8 条 QuickCheck 性质（固定种子 20260930） | 271 |
| 2 | `1088131` | 类型安全小修：注册表槽号由原型推导，新增 `mkRegistryChecked`；`cellColor` 等格子取值函数改返回 `Maybe`；去掉 `!!` / `error` / `last`（`NonEmpty`、`atM`）；倒计时规则每步只跑一遍 | 273 |
| 3 | `4b83dc9` | 性能与去重：Cascade 抽出 `settleRound` / `absorbRound`；`MBoard` 改为数组；`findHint` 只检查交换涉及的行和列；`acc ++ [x]` 改为反向累积 | 274 |
| 4 | `47485c3` | 计数统一：新模块 `Match3.Counts`（`CounterKey` + 稀疏 `Counts`），`GameState` 的 10 个计数字段合成 `gsCounts` | 276 |
| 5 | `024d421` | 目标数据化：`Match3.Goal`，目标 = `[Quota{计数键或分数, 目标值}]`，进度 / 达成 / 目标值统一计算，前端按 `goalView` 分派；新增 `Match3.Color`、前端表 `UI.GoalStyle` | 279 |
| 6a | `ebff019` | `Match3.Types` 按职责拆成 `Types.*`；关卡数据并进关卡记录 `Level`（放置表 / 皮带 / 传送门 / 飞碟 / 地毯 / 地面层）；`Match3.Levels.Campaign` 与 `lookupLevel`（取代所有 `allLevels !!`）；`placeWith` 返回 `Either` | 288 |
| 6b | `420fce0` | `ElementName` / `CustomState` 改为 newtype（`Show` 不变）；`SomeElement` 的相等改为按类型比较 | 290 |
| 7a | `ac015c0` | 关卡级状态收进 `gsLevelElems :: [SomeLevelElement]`（旧的五个字段变成派生读数 + setter）；带状态的 `LevelElement`；Board 层只接收钩子记录 `LevelHooks` | 294 |
| 7b | `ade2775` | 步末结算改为表 `Match3.Game.EndPhase`（tick → belt → spread → move → settle → vacate）；`askLevels` 依次问所有回复者；`EndEffect` 改为通用形状（事件类型 + 元素名 + `EndItem`） | 298 |
| 8 | `0e91565` | 规则表：补子策略 `RefillPolicy`（`Match3.Board.Refill`）、特殊块形状表与特殊块组合表（`Match3.Element.Special`），都挂在注册表上 | 307 |
| 9 | `ac211d8` | 元素类只剩 `name` / `toCell` / `caps`；能力分成五组带缺省值的记录（`Caps`），简写在 `Match3.Element.Caps`；内置 instance 平均从 7.21 行降到 4.63 行 | 312 |
| 10 | `0230893` | 前端表现表 `UI.Presentation`（效果事件 → 表现方式 / 帧数 / 颜色 / 贴图 / 碎屑 / 音效）与音效钩子 `UI.Sound`（预留）；新目录 `app/pure/`（桌面 / 测试 / 网页共用的纯前端模块）；`drawHud` 拆成 `UI.HudBlocks` 各区块，`primOverlay` 拆到 `UI.Cell.PrimOverlay` | 322 |
| 11 | `8f153c7` | 视图模型 `Match3.View`（`GameView` / `GoalInfo` / `BoardView`、标题、进度点、分数徽章、关卡列表、单格描述），桌面 HUD / 标题 / 棋盘底层和网页 JSON 都读它；通用网格 UI 组件 `Engine.GridUI`（像素 ↔ 格、点选、拖动、高亮） | 331 |

## 怎样验收的

- 每刀都做：全量重编 0 警告；`stack test` 全过；两份文本快照 `test/golden/golden.txt`（2534 行，md5 `776bb12a…`）和 `test/golden/element-queries.txt`（1648 行，md5 `fb99c871…`）从第 1 刀到第 11 刀逐字不变。
- 第 1–10 刀另外做了：与上一刀对照整局（18383 行）、关卡数据（640 行）、回放脚本（17353 行），逐字相同；`make check` 的网页 JSON 22 组逐字节相同（state 12/12、anim 10/10）。凡是动了 `app/` 的刀，桌面版多场景截图与上一刀逐帧相同（AE=0），窗口标题逐字相同。第 10 刀共拍了 44 个场景（静态 22、步末 6、动画 16）。
- 第 11 刀按 yu 的精简验收：编译 0 警告 + `stack test` 331 全过（含两份快照）；因为改了 `web/hs`，跑了 `make check`（331 全过，state 12/12、anim 10/10 一致，e2e 通过）；桌面版拍了 1 张图，肉眼确认显示正常。新增的 `test/Spec/View.hs` 拿第 11 刀前各前端现算式的字面副本做对照。
- 收尾：在 `/tmp` 下新 clone `main`（`8f153c7`），全新 `stack build` + `stack test` 通过（331，0 警告，工作区干净），说明构建不依赖未跟踪文件。

## 行为差异汇总

内置注册表 + 内置关卡：**无差异**（快照、整局对照、网页 JSON、截图都能证明）。有差异的只有下面几处：

| 刀 | 差异 | 影响范围 |
|----|------|----------|
| 2 | `cellColor` / `cellKind` / `balloonColor` 等取值函数改返回 `Maybe`（非对应格为 `Nothing`）；`mkRegistry` 去重不再依赖 `last` | 只改 API；调用方已收窄，结果不变 |
| 6a | 关卡下标越界时：重开 / 下一关 / `Campaign` 开局夹到关卡表范围内，或按默认配置开局；选关地图跳到不存在的关时关闭地图 | 以前直接崩溃（`!!` 越界）；内置流程到不了这里 |
| 7a | `WallCells` 只问已注册的 portal 元素：对非内置注册表 `removeLevel "portal"` 之后，传送门端点不再当墙 | 只影响自定义注册表 |
| 7b | 关卡级消息改为折叠所有回复者（前一个的回复就是后一个的问题）；以前只取第一个 | 内置元素每种消息只有一个回复者，结果不变；扩展测试「虹吸」改为和内置飞碟同一节拍都生效 |
| 8 | 直接用 `mkRegistry` 建的注册表不带形状表和组合表，所以不生成特殊块、没有特殊合成（以前这两处写死在主流程里，和注册表无关）；`defaultRegistry` 及从它扩展的注册表不变 | 只影响从零建注册表的扩展；需要时 `setShapeRules builtinShapeRules` / `setComboRules builtinComboRules` |
| 10 | 前端表现改为查表；表里没有的扩展元素：蔓延匀速、白色前沿光、不迸碎屑；表里没有的事件种类按 `defaultPresentation`（蔓延段 18 帧） | 与第 10 刀前的缺省相同；内置画面 AE=0 |
| 11 | 无 | — |

随机数消费顺序：每刀都不变（金标准里记录了生成器状态；第 8 刀的缺省补子策略逐洞调用 `randomColor`，与旧 `refill` 相同）。

## 现在的结构

```
src/Engine/          通用层（不 import Match3）：Game（接口）、History（撤销）、Effect、Playback（播放器）、Stream（无穷流）、Optics（透镜）、GridUI（网格 UI）
src/Match3/          三消规则库：Types.* / Counts / Goal / Color / Levels.* / Board.* / Game.* / Element.*
                     Engine（三消 = 通用接口的实现）、View（视图模型）、Core（再导出门面）
app/Shell/Loop.hs    通用 SDL 外壳
app/pure/            纯前端（桌面 / 测试 / 网页共用）：ComboFx、UI.Presentation、UI.Sound、UI.GoalIcon、UI.MoveText、UI.Palette
app/UI/              三消插件（SDL）：输入、动作、回放编排、绘制（CellTable / Ground / HudBlocks / GoalStyle 等查表）、音效（Audio）
web/hs/              网页接口层：gameStep match3Shell → JSON（读 Match3.View）
```

各模块职责见 [architecture.md § 模块地图](architecture.md#模块地图)，测试清单见 [testing.md](testing.md)。

## 现在怎样扩展

> 2026-10 的元素类重构（第 0–6 刀）改了下面的写法：能力记录 `Caps`、`Modifier`、`LevelElement` 与开放消息、注册表 `Registry` 都已删除，
> 现在的写法是能力类 + `Kind` / `Layer` / `Mechanic` instance + `kindDef @T`，见 [guide/04-元素框架.md](guide/04-元素框架.md) 与 [architecture.md「新增一种元素的步骤」](architecture.md#新增一种元素的步骤)。以下保留第 9 刀时的原文。

### 新增一种元素（格子里的东西）

1. 写一个类型和 `instance Element`：只写 `name`、`toCell`、`caps`，例如 `caps (Thorn n) = blocker [hit …, counts (CountNamed "thorn")]`（原型选 `piece` / `blocker` / `fixed`，能力简写在 `Match3.Element.Caps`）。内置元素放进 `src/Match3/Element/Builtin/` 下最接近的分组文件；测试 / 扩展元素放在自己的模块里。叠层写 `instance Modifier`。
2. 注册：内置的在 `Element.Builtin.builtinDefs` 末尾加一行；扩展的用 `register (customEntry 原型 解码) defaultRegistry`，再把注册表传给各个 `*With` 入口，或直接用 `match3GameWith reg`。
3. 放进关卡：在关卡记录的 `lvlPlacements` 里写 `Place "名字" [参数] [坐标]`。
4. 计数与目标：`counts (CountNamed "名字")` + 目标 `goalCount (CountNamed "名字") n`，HUD、标题、网页自动显示（经 `Match3.View`）。
5. 表现：贴图名就是元素名；需要专门画法的在 `UI.CellTable.customTable` 加一行；颜色在 `UI.Presentation.elementRGBTable` 加一行。网页端：单格、不带颜色、桌面也没有专门画法的 `Custom` 不用改 `web/www/cells.js`（通用画法 = 元素名贴图 + 层数角标，重新生成网页图集即可）；新的 `Cell` 构造器、桌面 `customTable` 里有专门画法的元素、占多格或带颜色的元素、要专门降级色的元素才要改 `cells.js`（详见 [web.md §2.3](web.md#23-js-渲染器)；e2e 的逐关降级护栏会报漏掉的）。
6. 测试：照 `ext_caps_element_plugs_in`（`test/Spec/Caps.hs`）的写法。

详见 [architecture.md § 新增一种元素的步骤](architecture.md#新增一种元素的步骤)。

### 新增关卡级元素（不在格子里的机制：皮带 / 传送门 / 飞碟 / 地毯 这类）

写一个带状态的 `instance LevelElement`：用 `levelStart` 从关卡记录取开局状态，用 `levelReply` 回复流水线的节拍消息（`Refilled` / `EndTicked` / `Settling` / `Covering` / `GroundHit` / `AvoidCells` / `WallCells` / `Refilling`，也可以自定义消息）。然后用 `registerLevel (SomeLevelElement 原型值)` 注册（内置的加进 `builtinLevelDefs`）。状态存在 `gsLevelElems` 里，撤销、`Show`、`Eq` 自动覆盖；同一条消息有多个回复者时按顺序折叠。需要主流程发出**新节拍**时才动主流程。例子：`ec_level_element_stateful_extension`（虹吸）、`ext_refill_policy_level_element`（金币雨）。

### 新增关卡

在 `src/Match3/Levels/Campaign.hs` 的 `allLevels` 末尾追加 `level 下标 "名字" 步数 目标`，再用记录更新写 `lvlPlacements` / `lvlBelts` / `lvlPortals` / `lvlUfos` / `lvlCarpets` / `lvlGround`。放置表写错时，`placeWith` 会带着关卡名报错（`Levels` 测试会检查全部关卡）。桌面版还要做三件事：用 `tools/gen_assets.py` 生成关名贴图 `name_<下标>`；需要的话调整 `UI.LevelMap.chapterStarts`；金标准只覆盖已有关卡。

### 新增规则

- 特殊块形状（如 L / T → 炸弹）：往注册表的形状表加一条 `ShapeRule`（`setShapeRules`），例子 `ext_shape_rule_lt_bomb`。
- 特殊块组合（如 直线 × 普通宝石）：加一条 `ComboRule`（`setComboRules`），例子 `ext_combo_rule_line_gem`。
- 补子策略：`setRefillPolicy`（例如 `colorsRefill 3`），或者由关卡级元素回复 `Refilling`。
- 步末阶段：在 `Match3.Game.EndPhase` 的表里加一行 `EndStage`，或者让元素产出通用的 `EndEffect`（例子「跳跳虫」`ext_end_effect_generic_hopper`）。
- 成对交换 / 开启 / 改色 / 推动：写成元素的能力（`onSwap` / `opens` / `recolors` / `pushes`）。

### 新增表现

- 效果事件的播放方式、帧数、颜色、贴图、碎屑、音效：改 `app/pure/UI/Presentation.hs` 的 `presentationTable`（按 `EventKind` 一行）；蔓延类元素的节奏和颜色在 `spreadCurves` / `elementRGBTable` 里配。确实需要新的步末段时，才加 `StageKind` 并在 `UI.EndStage.endStageDrawers` 里加绘制函数。
- 音效：填表项的 `prSound`（按事件种类），在 `assets/sfx/` 放同名 `.wav` 并加进 `UI.Audio` 的加载表；桌面由 `UI.Audio.cue` 播放。
- HUD 或网页要显示新读数：在 `Match3.View` 的 `GameView` / `GoalInfo` / `BoardView` 里加字段，桌面和网页都从视图读，不要在前端再从 `GameState` 现算（`frontends_read_view_model` 会扫描）。几何版 HUD 新区块：在 `UI.HudBlocks` 写一个 `hudXxx`，在 `drawHud` 里加一行。
- 网格交互（其他网格类游戏也能用）：`Engine.GridUI` 提供 `GridGeom` / `gridCellAt` / `gridCellOrigin`、`gridClick`（两步点选）、`gridDragRelease`（拖动松手）、`Highlight`（高亮）。

### 接入另一个游戏

按 [architecture.md § 接入一个新游戏的步骤清单](architecture.md#接入一个新游戏的步骤清单)：实现 `Engine.Game`，要撤销就用 `withHistory` 套一层，回放用 `Engine.Playback`，网格交互用 `Engine.GridUI`，写一个纯视图模型，再写一个 `Shell.Loop` 插件。新游戏只能依赖 `Engine.*` / `Shell.Loop`，不能 import `Match3.*`。
