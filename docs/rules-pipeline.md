# 规则流水线（trySwap / 稳定化 / 连锁）

本文描述 **当前代码** 中一次成功玩家步的编排顺序，入口为 `Match3.Game.trySwap`（校验与起手选择在 `Match3.Game.Move.resolveSwap`，其后的步骤由交换与三种道具共用的 `Match3.Game.Resolve.resolveMove` 完成；`runMove` 为其别名）。不发明源码中不存在的机制。

第二刀 2b 起，下文每一步里「某个格子怎么反应」（能否交换、是否挡匹配、被命中 / 被邻格波及时削层还是打碎、是否下落、计数、洗牌是否保留、步末规则）都由元素注册表分派（`Match3.Element`，见 [architecture.md](architecture.md#元素框架与事件)）；流水线顺序本身不变，邻格规则按固定顺序表执行。

## 总览

```
校验（终局 / 边界 / 相邻 / 石头等挡交换）
    │
    ▼
交换两格 → 是否彩虹交换 / 特殊合成 / 普通三消？
    │ 否 → NoMatch（回滚，盘面不变）
    ▼
主连锁 cascadeMatches / cascadeSeeds（匹配清除 → 特殊扩展 → 邻障削层 → 重力/饼干底收/传送门 → 补子 → UFO）
    │
    ▼
倒计时 cascadeCountdowns（tick；归零则 3×3 种子再连锁）
    │
    ▼
传送带 beltMoves → applyBeltMoves → cascadeAfterBelt（有匹配则连锁；无匹配仍 settle 收底行饼干）
    │
    ▼
蔓延 spreadVines → spreadChoco → spreadSteam
    │
    ▼
蜗牛 stepSnailsAvoidingBlocked（跳过皮带格；传送门端点当墙）
    │
    ▼
若蜗牛后出现匹配 → 再跑一轮连锁（不再二次皮带/蜗牛/倒计时）
    │
    ▼
步末补结算 cascadeAfterEndWith（段 2c：挖 erHoles 空洞 → 边缘收集 + 补子 → 成消再连锁；内置元素下恒为空操作）
    │
    ▼
汇总分数/收集/地毯/精灵+2步/−1步 → decideOutcome → 非终局则 ensurePlayable（无可走步时自动洗牌）
```

前端按同样顺序播放：主连锁各轮 → 倒计时减一 → 倒计时爆炸轮 → 皮带滑动 → 皮带后连锁轮 → 蔓延 → 蜗牛 → 蜗牛成消轮 → 自动洗牌（见 §8）。

## 1. 交换门禁（`trySwap` 前段）

1. 已有 `gsOver` → 原样返回该结局。
2. 坐标越界或不相邻 → `InvalidSwap`。
3. `swapBlockedWith reg`（注册表的 `edBlocksSwap`：石头/宝箱/蜂蜜/气球/饼干/蛋糕/帽/机/蜗牛/保险箱/彩蛋/瓶/精灵/锁链/火箭冰冻等挡交换情形，见 Obstacles）→ `NoMatch`。
4. `swapCells` 后：若没有成对交换规则成立（段 4：注册表的 `edSwap`，按 `srOrder`：彩虹取色 10 → 特殊合成 20；经 `swapOpeningWith`）、且 `not hasAnyMatch` → `NoMatch`。成立时以该规则在交换后盘面给出的种子起手。

2–4 返回的状态盘面 / 分数 / 步数不变，但会经 `clearMoveFx` 把 `gsCombo`、`gsLastCleared` 清零（这两个字段只描述最近一次**真正结算**的一步）。道具被拒（锤免疫、次数用完、自由交换无匹配）、撤销（`Engine.History` 的 `Undo`，经 `match3History.hpRestore`）、`shuffleGame` 同样清零。

彩虹与合成在**交换前**的两端判定激活资格（`specialActivates`：多冰/锁链/窗帘软锁不点火）。

## 2. 主连锁（`Match3.Board.Cascade`）

### 2.1 普通匹配路径 — `cascadeMatches` / `cascadeMatchesFrom`

每轮依次执行下列步骤，直至无匹配（结果是 `CascadeRun`：终盘、`CascadeTally` 计数、每轮 `CascadeWave`、飞碟、生成器）：

1. `clearMatchesDetailed`：找 ≥3 连，扩展特殊（`expandSpecials`），`chipIceOnClear`，彩蛋通道，清本格草等 overlay（彩蛋通道 = 注册表开启规则 `edOpen`，段 4），邻格削石头/宝箱/蜂蜜/蛋糕/气球/雾/链/冻/帘/保险箱/精灵、打破气泡（段 5，邻格规则 170），触发帽与瓶，充能果汁机，清邻巧克力/蒸汽，挖空真清除格，可能在清除位生成新特殊。
2. `settleBoardPortals`：重力 → 底行饼干收集 → 传送门传送（段 4：`HookTeleport`，内置 = `portalTeleport`）→ 再重力/收集（循环至稳）。段 2c 起收集按元素的 `edDrains :: [Edge]` 进行（底 → 左 → 右 → 上，角格只收一次；内置只有饼干 = 底边），被收格按其 `edCounter` 计数；地面层（`gsGround`）在每轮的真清除格 + 收集格上各削一层（段 5 起第 39 关的双层果冻用到它）。
3. `refill` 补随机普通宝石。
4. 飞碟吸收（段 4：注册表关卡级元素 `HookAbsorb`，内置 = `stepUfos`；去掉 `ufo` 即不吸收）：吸正交同色可吸收目标；若有吸收，先 `maskUfoAbsorbSpecials`（特殊降级为 Normal）再 `clearUfoAbsorbed`，**吸走 ≠ 引爆**，再 settle/补子，计入 `GoalUfo`。

波次分：`scoreForWave wave n`。

### 2.2 种子路径 — `cascadeSeeds`

彩虹 / 特殊合成 / 倒计时爆炸 / 道具：先 `clearFromSeedsDetailed`（对种子做特殊扩展与邻障处理），再 UFO，再转入普通匹配连锁（波次编号衔接）。

## 3. 倒计时（`cascadeCountdowns`）

1. `tickCountdowns`：所有倒计时 −1。
2. 若有归零：`explodeSeedsFor` → 种子连锁（仍带 UFO + 传送门）。
3. 匹配或特殊清掉倒计时可在清除阶段解除（不走到爆炸）。

**注意**：`useFreeSwap` 不走 tick（不耗步、不推进倒计时）；普通 `trySwap` 成功后才 tick。

## 4. 传送带（`beltMoves` + `applyBeltMoves` + `cascadeAfterBelt`）

- 有皮带：沿每条 `Belt` 环向移位一格。`Conveyor.beltMoves`（段 4：经注册表关卡级元素 `HookShift` 取用；去掉 `belt` 即不移位、也没有皮带后的再连锁）给出「原格 → 新格」（同一格出现多次时以最后一次为准），`applyBeltMoves` 按它移位；结算、回放描述（`EndBeltShift`）与重放（`applyEndEffect`）共用这一份。
- 移位后有匹配 → 全连锁。
- **无匹配**仍 `settleBoardPortals`：皮带把饼干送到底行时也要收集；settle 后若出现匹配再连锁。

## 5. 蔓延与蜗牛

顺序写死在 `trySwap`：

```text
spreadSteam (spreadChoco (spreadVines boardBeltCas))
→ stepSnailsAvoidingBlocked beltCells portalEnds …
```

- 本步已清除的藤/巧/蒸汽不蔓延。
- 蜗牛避开皮带占用格；传送门端点视为墙（避免永生装饰卡死门对）。只推注册表里 `edPushable` 的格（段 4，内置 = 宝石 / 倒计时 / 双面块）；魔法帽 / 染色瓶同样只改 `edRecolorable` 的格。
- 蜗牛推动后若形成匹配：再 `cascadeMatches` 一次；**不再**重复倒计时/皮带/蜗牛/蔓延。

## 6. 结算

- 分数、色袋、石头/宝箱/蜂蜜/气球/饼干/蛋糕/保险箱计数、UFO 吸收、地毯（清除位 ∪ `carpetVacateSeeds`：饼干腾空或保险箱开启；段 4 起覆盖经 `HookCover`，内置 = `coverCarpets`）。
- 步数：`gsMoves - 1 + 2 * spiritHit`。
- `decideOutcome`：目标满足 → 每日则 `Won`，否则战役 `LevelClear` 或终章 `Won`；步数用尽 → `Lost`；否则 `MoveApplied`。
- `MoveApplied` 时 `ensurePlayable`：无合法手则洗牌并 `restoreDecor`（保留障碍/特殊/叠层等装饰）。

## 7. 道具旁路（摘要）

| API | 耗步 | 倒计时 tick | 皮带/蜗牛 |
|-----|------|-------------|-----------|
| `useHammer` | 否 | 否（种子连锁后仅藤 / 巧 / 蒸汽蔓延） | 不移位 / 不爬 |
| `useFreeSwap` | 否 | 否（交换后连锁，再仅蔓延） | 不移位 / 不爬 |
| `useCrossClear` | 否 | 否（同锤子：种子连锁后仅蔓延） | 不移位 / 不爬 |

三者成功（`MoveApplied`）后同样 `ensurePlayable`。回放脚本里道具的 `mtEnd` 只会出现 `EndSpread`（`trace_end_steps_boosters_replay` 锁定）。

校验与起手见 `src/Match3/Game/Boosters.hs` 各 `resolve*`，结算与交换共用 `Match3.Game.Resolve.resolveMove`（`MoveKind` 决定步末与扣次）；测试锁定「FreeSwap 不 tick」「锤免疫不扣次」等不变量。

## 8. 与前端的边界

前端（`app/UI/Actions.hs` / `UI.Input`）经通用接口 `gameStep`（`Match3.Engine.match3Shell`）执行交换 / 道具（一次结算，结果与 `trySwap` / 道具 API 逐位相同）**之后**经 `UI.Playback` 播放交换/下落动画与粒子；规则结果不依赖帧。暂停（`P`）冻结动画并清拖拽，不改 `GameState` 规则字段。

**特效是边沿触发**：前端用 `moveFx before after outcome` 取本次操作的 `MoveFx`（`fxCombo` 连击波数、`fxCleared` 清除格），只有这次调用真正结算了一步才非空；`NoMatch` / `InvalidSwap` / 操作前已终局一律为空。闪光、粒子、连击弹字和 HUD 总结都只看 `MoveFx` 和本次操作的 `MoveTrace`，不再直接读持久字段 `gsCombo`——旧实现里无匹配回滚后 `gsCombo` 仍是上一步的值，会把上一步的连击特效再播一遍。

**逐轮回放用的纯函数**：`traceSwap`（`Match3.Game.Move`）/ `traceFreeSwap` / `traceHammer` / `traceCrossClear`（`Match3.Game.Boosters`）与对应的结算 API 是同一次 `resolveMove` 计算的两个投影；连锁层同理，`Match3.Board.Cascade` 的记录版 `cascade*` 同时产出计数（`CascadeTally`）与每一轮（`CascadeWave`），调用方直接读 `CascadeRun` 的字段（第三刀删掉了旧的元组 API `runCascade*` / `traceCascade*`）。`MoveTrace` 含每一轮的消除前盘面、被消格、空洞、补子后盘面和得分（UFO 吸收单独算一轮）；`mtFinal` / `mtGen` 是 `ensurePlayable` 之前的稳定盘与生成器，没有自动洗牌时就等于结算结果的 `gsBoard` / `gsGen`，洗牌时 `mtShuffle` 记下洗牌后的盘面。`trace_*` 系列测试保留作回归，现在天然成立。

`mtEnd :: [EndStep]` 记录一步里的步末效果（非消除的盘面变化），按发生顺序：倒计时减一（`EndCountdownTick`）→ 皮带移位（`EndBeltShift`，多条皮带已合成为「原格 → 新格」）→ 藤 / 巧 / 蒸汽蔓延（`EndSpread`，带来源格）→ 蜗牛爬行（`EndSnail`，每只的起点、终点、朝向和被推的格子）。`esAfterWaves` 是它插在第几轮之后。道具路径只有蔓延。`applyEndEffect` 能把描述重放回盘面，测试按「轮 → 步末 → 轮」的时间线重放并与 `trySwap` 的终盘逐项比对。自动洗牌（`ensurePlayable`）不在 `mtEnd` 里，而是记在 `mtGen` / `mtShuffle`（`trace_shuffle_step_replays` 逐帧复现）；前端用 `mtFinal` 与结算后 `gsBoard` 的差异补一段洗牌动画。前端播放方式见 [`ui-art.md` 连击表现](ui-art.md#连击表现逐轮回放)，规则 / 回放两条路径需要同步的位置见 [`architecture.md`](architecture.md#逐轮回放与规则的同步)。
