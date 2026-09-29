# 测试

## 如何运行

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
cd /path/to/match3
stack test
```

- 库测 **不需要** 显示器或 SDL 运行库参与链接执行路径上的窗口。
- 期望：**228** 个命名用例通过（Tasty：`testCase` + `testProperty`）。
- 合并门禁：上述 `stack test` 全绿即可合入；不要在红测上合并。

可选完整链路：

```bash
stack build && stack test && stack exec match3-sdl
```

macOS Apple Silicon 构建 SDL 前端时：

```bash
export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig
```

若 Stack 镜像/签名异常，优先使用 `stack.yaml` 中 `system-ghc: true` 与本机已缓存的 GHC 9.4.8。

## 套件结构

- 入口：`test/Spec.hs` → `defaultMain tests`
- 框架：tasty + tasty-hunit + tasty-quickcheck
- 依赖库 API：主要通过 `Match3.Core`

覆盖类型（按主题，非穷尽）：

| 主题 | 示例用例名 |
|------|------------|
| 基本不变量 | `inv_no_match_rollback`、`inv_move_to_stable` |
| 匹配 / 重力 / 连锁 | `match_line_ge3`、`gravity_then_refill`、`cascade_until_stable` |
| 属性测试 | `qc_findMatches_ge3` |
| 特殊生成与合成 | `special_line_from_4`、`special_rainbow_from_5`、`special_combo_*` |
| 彩虹 | `rainbow_clears_color`、`rainbow_swap_without_match` |
| 目标与关卡 | `collect_goal_*`、`goal_*`、`level_table_mixes_collect` |
| 障碍与叠层 | `stone_*`、`ice_*`、`grass_*`、`vine_*`、`choco_*`、`fog_*`、`chain_*`、`freeze_*`、`curtain_*` |
| 收集物 | `chest_*`、`honey_*`、`balloon_*`、`cookie_*`、`cake_*`、`safe_*` |
| 机制 | `hat_*`、`maker_*`、`portal_*`、`ufo_*`、`snail_*`、`countdown_*`、`conveyor_*` |
| 道具 | `booster_hammer_*`、`booster_free_swap_*` |
| 每日 / 三星 | `daily_seed_stable`、`star_rating_tiers` |
| 提示 / 撤销 / 洗牌 | `hint_finds_move`、`undo_restores`、`shuffle_when_no_moves` |
| 逐轮回放 / 步末效果 | `trace_*`、`trace_end_*`（见下节） |
| 元素框架（第二刀 2b） | `element_registry_custom_crate_extensibility`、`element_registry_matches_legacy_predicates`、`trace_events_consistent_with_trace`（见「元素框架验收」） |
| 特效不重播（爆击/连击） | `failed_swap_resets_combo_feedback`、`invalid_swap_resets_combo_feedback`、`booster_noop_resets_combo_feedback`、`undo_shuffle_reset_combo_feedback`、`move_fx_ignores_already_over` |
| 稳定性巡航 | 软锁、地毯↔饼干/保险箱、精灵+2、步数携带上限、皮带→蒸汽→蜗牛顺序、吸走≠引爆、每日 Won 不解锁等 |

## 逐轮回放护栏

前端的连锁逐轮回放和步末动画只播 `trace*` 返回的 `MoveTrace`。第二刀起结算与回放是同一次计算的两个投影，这组测试天然成立，保留作回归（同步关系见 [architecture.md](architecture.md#逐轮回放与规则的同步)）：

| 用例 | 锁定的内容 |
|------|------------|
| `trace_cascade_final_equals_stabilized` | 普通匹配连锁：`cascadeMatches` 的逐轮回放 `crWaves` 与结算计数 `crTally` 一致——各轮得分之和、清除格并集、波数，终盘稳定、逐轮首尾相接（含飞碟、传送门；第三刀删掉元组 API 后两个投影直接取同一 `CascadeRun` 的字段） |
| `trace_seeds_final_equals_stabilized` | 种子起手（彩虹 / 特殊合成 / 道具 / 倒计时爆炸）的连锁同样一致 |
| `trace_swap_final_equals_trySwap` | 17 个关卡 × 种子 1–3 的全部相邻交换：`mtStart` = 交换后盘面，逐轮得分之和 = 本步得分，清除格并集 = `gsLastCleared`，有清除的轮数 = `gsCombo`；未自动洗牌的步再比对终盘 `mtFinal == gsBoard` |
| `trace_boosters_final_equal_result` | 锤子 / 十字 / 自由交换：终盘、得分、连击数一致（终局分支也比对得分和连击） |
| `trace_rejected_move_is_empty` | 无匹配 / 非相邻 / 已结束 / 道具无效：`mtWaves` 与 `mtEnd` 都为空，前端什么都不播 |
| `trace_multi_wave_each_round_visible` | 3 连锁及以上：每一轮都有自己的被消格，且这些格在该轮之前的盘面上确实存在 |
| `trace_end_steps_replay_to_trySwap_final` | 全部 38 关 × 种子 1–3 的每个成功交换按「轮 → 步末 → 轮」时间线重放：每段首尾相接、`applyEndEffect esEffect esBefore == esAfter`，终盘等于 `trySwap`（未洗牌时）；抽样必须覆盖 tick / belt / SpreadVine / SpreadChoco / SpreadSteam / snail 六种效果 |
| `trace_end_steps_boosters_replay` | 道具只有蔓延类步末效果，重放后同样到达终盘 |
| `trace_end_snail_push_and_turn` | 蜗牛碰壁原地掉头（`smFrom == smTo`、朝向反转）；前方是宝石则爬过去、宝石换到原格 |
| `trace_shuffle_step_replays` | 洗牌步逐帧比对（见下） |
| `trace_end_spread_from_adjacent_source` | 巧克力 / 藤蔓的新占格都能在之前的盘面找到正交相邻的同类来源（前端据此决定从哪边「长出」） |

**底线**：`trace_swap_final_equals_trySwap` 要求「未洗牌、实际比对终盘的步数 > 400」，失败信息会打印比对数和因自动洗牌跳过的步数（当前样本约 517 比对 / 9 跳过），防止抽样悄悄缩水、测试名存实亡。

**洗牌步（第二刀补上）**：`MoveTrace` 新增 `mtGen`（洗牌前的生成器）与 `mtShuffle`（洗牌后的盘面）。`trace_shuffle_step_replays` 对全部 38 关 × 种子 1–3 × 开局全部成交交换（每条再沿首个成交交换走 2 手）逐帧比对：时间线重放到 `mtFinal`，再从 `(mtFinal, mtGen)` 重放 `ensurePlayable` 必须到达结算后的 `gsBoard` / `gsGen`；未洗牌的步要求 `mtFinal == gsBoard`、`mtGen == gsGen`。当前 **3801** 个成交步逐帧比对，其中 **29** 个自动洗牌步；底线 > 3000 / > 20。

## 行为金标准（golden）

`test/golden/Golden.hs` 把固定种子下的规则结果投影成稳定文本，入库为 `test/golden/golden.txt`（2186 行），`golden_behaviour_snapshot` 在 `stack test` 里逐行比对，失败时报告第一处分叉的行号。

- **覆盖**：38 关 × 种子 {1, 2} × 最多 15 步（每步：成交的交换、结局、完整计数、`gsCombo` / `gsLastCleared`、随机数状态、`traceSwap` 回放；辅助列：撤销 / 提示 / 洗牌 / 自动洗牌 / 结局判定 / 被拒交换；每 3 步三种道具的有次数 / 无次数结果与回放）、2 个每日式开局、3 个手工局面（护栏里的蜗牛撞墙推格、巧克力关、首个 3 连锁，另含该局面全部成交交换）、连锁 API（`Match3.Board.Cascade` 的 `cascadeMatches` / `cascadeSeeds`）的普通 / 种子连锁 × 飞碟 / 传送门、各关开局 / 重开 / 下一关。
- **行首**：`L05 s2 #07` = 第 5 关、种子 2、第 7 步；`H1-snail`、`C03/1`、`S07/2`、`G12 s5` 等为其他用例。
- **投影**：格子短码（如 `2hi1+K2` = 颜色 2、横向直线、1 层冰、2 层锁链）、具名计数、`StdGen` 的 `show`；盘面与回放脚本（`mtWaves` / `mtEnd` 投影文本）用手写 FNV-1a 64 压缩。不对内部类型直接 `show`。
- **取数路径**：入库时（`4fbcefc`）取数只经过门面 `Match3.Board` / `Match3.Game`，同一份代码在 `3bd26d8` 与 `5eef3e3` 上原样编译、输出全等（md5 `b792e77a…`）。第三刀删掉了这两个门面和元组兼容层，`Golden.hs` 改为直接 import `Match3.Board.*` / `Match3.Game.*` 子模块、从 `CascadeRun` 记录取连锁字段，盘面经 `boardRows` 投影——**它今后不能在 `51b1cfa` 及更早的提交上编译**；要和旧提交比对，把 `4fbcefc` 版的 `Golden.hs` 拿到旧提交上跑（输出与 `golden.txt` 逐字相同）。
- **耗时**：第三刀把 `Board` 换成数组（`getCell` O(1)）后，`golden_behaviour_snapshot` 在 box 上约 6.05 s → 4.3–4.5 s（单独运行 `-p golden_behaviour_snapshot`）。
- **维护纪律**：内部表示变了只改投影函数，`golden.txt` 一个字都不动；确需重录（规则行为有意变化，机制刀停期间不应发生）用 `test/golden/regen.sh`，并在提交说明里写明原因。

## 元素框架验收

框架见 [architecture.md](architecture.md#元素框架与事件)。

| 用例 | 断言 |
|------|------|
| `element_registry_custom_crate_extensibility` | 测试专用元素「木箱」`Custom "crate" 耐久`（**只定义在 `test/Spec.hs`**：以 `baseDef` 为底，不下落、邻格真消除波及耐久 −1、耐久 1 再被波及就碎、计数 `CountNamed "crate"`）经 `register` 接入后：注册表多一项、内置一个不少；对它交换返回 `NoMatch` 且盘面不变；无匹配色；第 1 手（邻格 C5 三消）耐久 2→1、原地不动、不计数、不在清除格、事件里有 `EvHit "crate"`；第 2 手（耐久 1）碎掉、进入第一轮清除格、`gsElementCounts == [("crate",1)]`、事件里有 `EvClear "crate"`；锤子削到 1；洗牌保留；同一局面在 `defaultRegistry` 下它是惰性占格（不被波及、锤子免疫、不计数）；核心 16 个源文件里没有字面量 `"crate"` / 「木箱」——主流程没有为它改一行 |
| `element_registry_matches_legacy_predicates` | 全部内置本体 × 宝石种类 × 冰层 × 叠层：注册表的挡交换 / 锤子免疫 / 固定格 / 点火 / 匹配色 / 洗牌保留与第二刀之前按构造器写死的谓词逐格相等；直接命中的几条代表（冰、锁链、保险箱、翻转、石头）与旧口径一致 |
| `trace_events_consistent_with_trace` | 38 关 × 种子 1–2 × 前 3 个成交交换：`EvScore` 之和 = 本步得分；`EvClear` 的格 = 各轮清除格并集；步末事件数 = `mtEnd` 长度；`EvShuffle` ⇔ `mtShuffle`；`EvBlast` 只来自直线 / 炸弹；**逐轮严格相等**（第三刀，前端波次界面改读事件后加）：该轮 EvClear 的格按事件顺序拼接 = `cwCleared`（顺序也相同），该轮 EvScore 之和 = `cwScore` |

## 多游戏接口验收（第三刀）

接口见 [architecture.md](architecture.md#多游戏接口)。

| 用例 | 断言 |
|------|------|
| `engine_toy_counter_game` | 玩具实现 `test/Toy.hs`（一维计数器，**只 import `Engine.*`**）：种子决定目标；`runActions` 遇到胜局即停（其后动作不执行）；非法 `Inc 9` 被拒、状态不变、没有事件；超出目标 / 步数用完判负；结局后 `gameActions` 为空、`gameStep` 拒绝；`gameStatus`；效果按节拍排成两个提示（6 帧 / 20 帧），通用播放器总帧数 26、只在进入第二个提示时触发其事件，加速后 9 帧，3 帧后进度 0.5；自定义阶段机（倒数 3 → 0）事件与帧数 |
| `engine_layer_is_game_agnostic` | `src/Engine/*.hs`、`app/Shell/Loop.hs`、`test/Toy.hs` 没有任何 `import Match3…` 行（依赖方向单向） |
| `engine_match3_instance_matches_direct_api` | 4 关 × 2 种子：`gameNew` = `newGameAtLevel`；前 3 个候选交换经 `gameStep` 的状态 / 结局 / 事件与 `trySwap` / `traceEvents (traceSwap …)` 逐位相同，`toEffect` 不丢事件、score 效果之和 = 得分增量，`play` 的 `MoveFx` / `Outcome` 与 `moveFx` 相同，撤销 = `undoMove`；锤子 / 十字 = `useHammer` / `useCrossClear`；非相邻交换被拒且无事件；开局撤销被拒；洗牌 = `shuffleGame` 且只有一个 shuffle 效果；提示 = `applyHint`；`gameStatus` 的 combo = `gsCombo`；只剩 1 步时 `runActions` 在第一步之后停下、终局后没有候选动作且拒绝一切 |

## 编写约定

- 固定 `StdGen` / 手工构造 `Board`，避免 flaky。
- 改规则必须同步更新或新增用例；禁止「只改文档宣称」。
- 不修改测试来迁就错误实现；先修规则或先补回归再合。

## 与 CI 的关系

仓库可能另有工作流配置；**本地以 `stack test` 全绿（当前 228，含金标准比对）为合并门禁**。本文不依赖未跟踪的 `.github/` 内容。

门禁细则（第三刀起）：

- 每个提交都要 `stack test` 全绿、`golden_behaviour_snapshot` 逐行相等（`golden.txt` 不许改）。
- 通用层 `Engine.*` / `Shell.Loop` 不得 import `Match3`（`engine_layer_is_game_agnostic`）；三消实例与直接调用旧入口逐位相同（`engine_match3_instance_matches_direct_api`）。
- 动到前端绘制 / 回放时，另做截图比对：新旧二进制在 Xvfb 下的 6 个场景（展示盘、L16、L28 @2x、L5 暂停、L1 地图、L36 提示），贴图与几何降级两种模式都要能找到 `compare -metric AE` = 0 的帧配对（呼吸光随时钟变化，所以连拍找配对）。
- 元组兼容层（`runCascade*` / `resolveCountdowns` / `runPostBeltCascade` / `traceCascade*` / `toTuple*`）与门面 `Match3.Board` / `Match3.Game` 已删除；测试与 Golden 直接用 `CascadeRun` 记录和子模块。
