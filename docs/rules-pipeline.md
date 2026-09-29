# 规则流水线（trySwap / 稳定化 / 连锁）

本文描述 **当前代码** 中一次成功玩家步的编排顺序，入口为 `Match3.Game.trySwap`（`runMove` 为其别名）。不发明源码中不存在的机制。

## 总览

```
校验（终局 / 边界 / 相邻 / 石头等挡交换）
    │
    ▼
交换两格 → 是否彩虹交换 / 特殊合成 / 普通三消？
    │ 否 → NoMatch（回滚，盘面不变）
    ▼
主连锁 runCascade*（匹配清除 → 特殊扩展 → 邻障削层 → 重力/饼干底收/传送门 → 补子 → UFO）
    │
    ▼
倒计时 resolveCountdowns（tick；归零则 3×3 种子再连锁）
    │
    ▼
传送带 shiftBelts → runPostBeltCascade（有匹配则连锁；无匹配仍 settle 收底行饼干）
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
汇总分数/收集/地毯/精灵+2步/−1步 → decideOutcome → 非终局则 ensurePlayable
```

## 1. 交换门禁（`trySwap` 前段）

1. 已有 `gsOver` → 原样返回该结局。
2. 坐标越界或不相邻 → `InvalidSwap`。
3. `swapBlockedByStone`（石头/宝箱/蜂蜜/气球/饼干/蛋糕/帽/机/蜗牛/保险箱/彩蛋/瓶/精灵/锁链/火箭冰冻等挡交换情形，见 Obstacles）→ `NoMatch`。
4. `swapCells` 后：若非 `isRainbowSwap`、非 `isSpecialCombo`、且 `not hasAnyMatch` → `NoMatch`。

2–4 返回的状态盘面 / 分数 / 步数不变，但会经 `clearMoveFx` 把 `gsCombo`、`gsLastCleared` 清零（这两个字段只描述最近一次**真正结算**的一步）。道具被拒（锤免疫、次数用完、自由交换无匹配）、`undoMove`、`shuffleGame` 同样清零。

彩虹与合成在**交换前**的两端判定激活资格（`specialActivates`：多冰/锁链/窗帘软锁不点火）。

## 2. 主连锁（`Board`）

### 2.1 普通匹配路径 — `runCascadeScoredWithUfos`

循环 `stepCascadeDetailed`，直至无匹配：

1. `clearMatchesDetailed`：找 ≥3 连，扩展特殊（`expandSpecials`），`chipIceOnClear`，彩蛋通道，清本格草等 overlay，邻格削石头/宝箱/蜂蜜/蛋糕/气球/雾/链/冻/帘/保险箱/精灵，触发帽与瓶，充能果汁机，清邻巧克力/蒸汽，挖空真清除格，可能在清除位生成新特殊。
2. `settleBoardPortals`：重力 → 底行饼干收集 → 传送门传送 → 再重力/收集（循环至稳）。
3. `refill` 补随机普通宝石。
4. `stepUfos`：吸正交同色可吸收目标；若有吸收，先 `maskUfoAbsorbSpecials`（特殊降级为 Normal）再 `clearUfoAbsorbed`，**吸走 ≠ 引爆**，再 settle/补子，计入 `GoalUfo`。

波次分：`scoreForWave wave n`。

### 2.2 种子路径 — `runCascadeScoredFromSeedsWithUfos`

彩虹 / 特殊合成 / 倒计时爆炸 / 道具：先 `clearFromSeedsDetailed`（对种子做特殊扩展与邻障处理），再 UFO，再转入普通匹配连锁（波次编号衔接）。

## 3. 倒计时（`resolveCountdowns`）

1. `tickCountdowns`：所有倒计时 −1。
2. 若有归零：`explodeSeedsFor` → 种子连锁（仍带 UFO + 传送门）。
3. 匹配或特殊清掉倒计时可在清除阶段解除（不走到爆炸）。

**注意**：`useFreeSwap` 不走 tick（不耗步、不推进倒计时）；普通 `trySwap` 成功后才 tick。

## 4. 传送带（`shiftBelts` + `runPostBeltCascade`）

- 有皮带：沿每条 `Belt` 环向移位一格。
- 移位后有匹配 → 全连锁。
- **无匹配**仍 `settleBoardPortals`：皮带把饼干送到底行时也要收集；settle 后若出现匹配再连锁。

## 5. 蔓延与蜗牛

顺序写死在 `trySwap`：

```text
spreadSteam (spreadChoco (spreadVines boardBeltCas))
→ stepSnailsAvoidingBlocked beltCells portalEnds …
```

- 本步已清除的藤/巧/蒸汽不蔓延。
- 蜗牛避开皮带占用格；传送门端点视为墙（避免永生装饰卡死门对）。
- 蜗牛推动后若形成匹配：再 `runCascadeScoredWithUfos` 一次；**不再**重复倒计时/皮带/蜗牛/蔓延。

## 6. 结算

- 分数、色袋、石头/宝箱/蜂蜜/气球/饼干/蛋糕/保险箱计数、UFO 吸收、地毯（清除位 ∪ `carpetVacateSeeds`：饼干腾空或保险箱开启）。
- 步数：`gsMoves - 1 + 2 * spiritHit`。
- `decideOutcome`：目标满足 → 每日则 `Won`，否则战役 `LevelClear` 或终章 `Won`；步数用尽 → `Lost`；否则 `MoveApplied`。
- `MoveApplied` 时 `ensurePlayable`：无合法手则洗牌并 `restoreDecor`（保留障碍/特殊/叠层等装饰）。

## 7. 道具旁路（摘要）

| API | 耗步 | 倒计时 tick | 皮带/蜗牛 |
|-----|------|-------------|-----------|
| `useHammer` | 否 | 否（实现为种子连锁后仅蔓延） | 不移位/不爬（见 Game 实现） |
| `useFreeSwap` | 否 | 否 | 有（与自由交换成功路径一致，见源码） |
| `useCrossClear` | 否 | 否 | 同锤子一类种子后处理 |

细节以 `Game.hs` 各 `use*` 为准；测试锁定「FreeSwap 不 tick」「锤免疫不扣次」等不变量。

## 8. 与前端的边界

`app/Main.hs` 在调用 `trySwap` / 道具 API **之后**播放交换/下落动画与粒子；规则结果不依赖帧。暂停（`P`）冻结动画并清拖拽，不改 `GameState` 规则字段。

**特效是边沿触发**：前端用 `moveFx before after outcome` 取本次操作的 `MoveFx`（`fxCombo` 连击波数、`fxCleared` 清除格），只有这次调用真正结算了一步才非空；`NoMatch` / `InvalidSwap` / 操作前已终局一律为空。闪光、粒子、连击弹字和 HUD 总结都只看 `MoveFx` 和本次操作的 `MoveTrace`，不再直接读持久字段 `gsCombo`——旧实现里无匹配回滚后 `gsCombo` 仍是上一步的值，会把上一步的连击特效再播一遍。

**逐轮回放用的纯函数**：`traceSwap` / `traceFreeSwap` / `traceHammer` / `traceCrossClear`（Game）以及 `traceCascade` / `traceCascadeFromWave` / `traceCascadeFromSeeds` / `tracePostBeltCascade` / `traceCountdowns`（Board）按和 `runCascade*` / `resolveCountdowns` / `runPostBeltCascade` 完全相同的顺序（包括随机数的消耗顺序）逐轮重算，返回 `MoveTrace`：每一轮的消除前盘面、被消格、空洞、补子后盘面和得分（UFO 吸收单独算一轮），`mtFinal` 就是 `trySwap` / 道具 API 的结果状态。它们只**新增**，不改已有函数的语义；`trace_*_final_equals_*` 系列测试保证两边完全一致。

`mtEnd :: [EndStep]` 记录一步里的步末效果（非消除的盘面变化），按发生顺序：倒计时减一（`EndCountdownTick`）→ 皮带移位（`EndBeltShift`，多条皮带已合成为「原格 → 新格」）→ 藤 / 巧 / 蒸汽蔓延（`EndSpread`，带来源格）→ 蜗牛爬行（`EndSnail`，每只的起点、终点、朝向和被推的格子）。`esAfterWaves` 是它插在第几轮之后。道具路径只有蔓延。`applyEndEffect` 能把描述重放回盘面，测试按「轮 → 步末 → 轮」的时间线重放并与 `trySwap` 的终盘逐项比对。自动洗牌（`ensurePlayable`）不在 `mtEnd` 里，前端用 `mtFinal` 与结算后 `gsBoard` 的差异自己补一段洗牌动画。前端播放方式见 [`ui-art.md` 连击表现](ui-art.md#连击表现逐轮回放)。
