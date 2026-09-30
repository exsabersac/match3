# SDL 键位与操作（match3-sdl）

前端：`app/UI/Input.hs`（三消插件的输入映射：`handleKey` 按键表每键一个函数，`handleMouseUp` 拖拽交换，`handleMouseDown` 地图 / 加速 / 结束浮层 / 点格）与 `app/UI/Actions.hs`（道具 / 过关等动作）；规则一律经通用接口 `gameStep`（`UI.Actions.stepShell` → `Match3.Engine.match3Shell`；撤销历史在 `Engine.History`）。点选（含自由交换的两步点选）与拖动松手的判定第 11 刀起用通用网格组件 `Engine.GridUI`（`gridClick` / `gridDragRelease adjacent`，像素 ↔ 格经 `UI.Layout.boardGrid`）。规则侧不读键盘；此处仅描述 UI 绑定。

## 基本操作

| 输入 | 行为 |
|------|------|
| 左键点两格（相邻） | 调用 `trySwap` |
| 拖拽到相邻格 | 同上 |
| 无三连 / 挡交换 | 回滚；提示清空；不播任何连击特效，也不重播上一步的回放（`MoveFx` 为空 → `UI.Playback.withMovePlayback` 直接清空弹字 / 总结） |
| 回放中点击左键 / 空格 / 回车 / `N` | 加速回放（`UI.Actions.speedUp`：阶段机每帧推进 `fastStep` = 3 帧，约 3 倍），连锁各轮和步末各段仍逐个可见；标题栏提示 `Fast-forward combo`。只作用于逐轮回放（含交换之后排队的回放），`S` 洗牌后的普通下落不加速 |

## 播放锁定（`animBusy`）

交换动画、逐轮回放（含步末阶段 `PhEnd`）和洗牌下落播放期间 `appAnim ≠ AnimNone`，前端按下表处理输入，保证不会在上一步没播完时误触下一步：

| 输入 | 播放中 |
|------|--------|
| 左键按下 / 拖拽（地图未打开、未暂停） | **锁定**：不选格、不交换；按下＝加速。地图打开时点击照常选关 |
| `1` / `2` / `3` 道具 | **锁定** |
| `U` 撤销 / `S` 洗牌 | **锁定** |
| `N` / 空格 / 回车 | 加速（不会推进到下一关 / 重试） |
| `H` 提示、`M` 地图、`D` 每日 | 可用（`D` 换盘会重置动画） |
| `R` 重开 | 可用；`freshLevelUi` 重置动画、弹字和总结 |
| `P` 暂停 | 可用；冻结动画，解除暂停后继续播 |
| `Esc` / `Q` | 退出 |

过关 / 胜利 / 失败叠层在播放结束（`animBusy` 为假）后才画出，最后几轮不会被面板挡住；叠层出现后点击或 `N` / 空格 / 回车才推进。回放的时间线、等级样式与步末阶段见 [`ui-art.md` 连击表现](ui-art.md#连击表现逐轮回放)。

## 道具模式（`ToolMode`）

| 键 | 模式 | 行为 |
|----|------|------|
| `1` | `ToolHammer` | 再点一格 → `useHammer`；再按 `1` 取消；无次数提示 |
| `2` | `ToolFreeSwap` | 点两格（可不相邻）→ `useFreeSwap`；再按 `2` 取消 |
| `3` | `ToolCross` | 再点一格 → `useCrossClear`；再按 `3` 取消 |
| 先选中格再 `1`/`3` | — | 对该格直接锤/十字（兼容旧快捷） |

锤子对 Maker / Snail / Bottle / MagicHat / Cookie 免疫：不扣次数（规则层 `hammerImmune`）。

## 其它键

| 键 | 行为 |
|----|------|
| `H` | `applyHint`（经 `gameStep`：`Act Hint`），高亮一手（终局后仍可用） |
| `U` | 撤销一步（经 `gameStep`：`Undo`，由 `Engine.History` 处理；终局后仍可撤销） |
| `S` | `shuffleGame`（经 `gameStep`：`Act Shuffle`） |
| `D` | `newDailyGame`（日期种子） |
| `M` | 开关选关地图；点击已解锁节点 `mapClickJump` |
| `N` / 空格 / 回车 | 过关叠层后下一关 / 失败后重试等；动画播放中＝加速（见上） |
| `R` | 重开本关（暂停中可用） |
| `P` | 暂停 + 完整键位说明；冻结动画；清除拖拽 |
| `Esc` / `Q` | 退出 |

## HUD 提示

- 开局 / 解除暂停后底部短键位条。
- 第一关短暂黄框提示可消一手。
- 暂停叠层列全键；过关/胜利/失败叠层提示继续键（回放播完才出现）。
- 连锁回放中右下角面板显示当前轮「连击 xN」（第 1 轮显示滚动上涨的分数），全部播完后若最高连击 ≥ 2 显示「N 连击！」约 1.6 s。

### HUD 的实现（第 10 刀）

- 几何降级版 HUD（无贴图时）按区块拆在 `app/UI/HudBlocks.hs`，`UI.HudPrim.drawHud` 只按固定顺序调用：`hudFrame`（底板）→ `hudLevel`（关卡号 + 各关进度点）→ `hudGoal`（目标条与数字）→ `hudGoalSwatch`（收集目标色块）→ `hudMoves`（步数条）→ `hudBoosters`（锤子 / 自由交换 / 十字次数与当前工具模式字样）→ `hudComboBadge`（连击徽章）→ `hudStatus`（右侧结局色条）。第 11 刀起各区块收视图模型（`GameView` / `GoalInfo` / `Boosters` / `PlayStatus`，见 [architecture.md § 视图模型](architecture.md#视图模型第-11-刀)），不再收 `GameState`。顺序即绘制层次；新增区块写一个 `hudXxx` 函数、在 `drawHud` 里加一行。
- 几何版格子上的覆盖层（草 / 藤 / 巧克力 / 迷雾 / 锁链 / 冰冻 / 窗帘 / 蒸汽）在 `app/UI/Cell/PrimOverlay.hs`，每种一个函数，`primOverlay` 只分派。
- 弹字 / 浮字 / 连击徽章的颜色与贴图名、各动画的帧数读表现表 `UI.Presentation`（见 [ui-art.md「表现表」](ui-art.md#表现表第-10-刀)）；操作触发的音效钩子（`effectSound`，内置全部无声）同在那张表里，前端不播放声音。
- 拆分与改为查表都不改变画面：与 `ac211d8` 的截图逐帧相同，窗口标题逐字相同。

更完整的功能与关卡表见根 [`README.md`](../README.md)。
