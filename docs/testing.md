# 测试

## 如何运行

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
cd /path/to/match3
stack test
```

- 库测 **不需要** 显示器或 SDL 运行库参与链接执行路径上的窗口。
- 期望：**222** 个命名用例通过（Tasty：`testCase` + `testProperty`）。
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
| 特效不重播（爆击/连击） | `failed_swap_resets_combo_feedback`、`invalid_swap_resets_combo_feedback`、`booster_noop_resets_combo_feedback`、`undo_shuffle_reset_combo_feedback`、`move_fx_ignores_already_over` |
| 稳定性巡航 | 软锁、地毯↔饼干/保险箱、精灵+2、步数携带上限、皮带→蒸汽→蜗牛顺序、吸走≠引爆、每日 Won 不解锁等 |

## 逐轮回放护栏

前端的连锁逐轮回放和步末动画只播 `trace*` 返回的 `MoveTrace`。第二刀起结算与回放是同一次计算的两个投影，这组测试天然成立，保留作回归（同步关系见 [architecture.md](architecture.md#逐轮回放与规则的同步)）：

| 用例 | 锁定的内容 |
|------|------------|
| `trace_cascade_final_equals_stabilized` | 普通匹配连锁：`traceCascade` 的终盘 / 生成器 / 得分 / 清除格 / 波数与 `runCascadeScoredWithUfos` 一致（含飞碟、传送门） |
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

- **覆盖**：38 关 × 种子 {1, 2} × 最多 15 步（每步：成交的交换、结局、完整计数、`gsCombo` / `gsLastCleared`、随机数状态、`traceSwap` 回放；辅助列：撤销 / 提示 / 洗牌 / 自动洗牌 / 结局判定 / 被拒交换；每 3 步三种道具的有次数 / 无次数结果与回放）、2 个每日式开局、3 个手工局面（护栏里的蜗牛撞墙推格、巧克力关、首个 3 连锁，另含该局面全部成交交换）、门面 `Match3.Board` 的普通 / 种子连锁 × 飞碟 / 传送门、各关开局 / 重开 / 下一关。
- **行首**：`L05 s2 #07` = 第 5 关、种子 2、第 7 步；`H1-snail`、`C03/1`、`S07/2`、`G12 s5` 等为其他用例。
- **投影**：格子短码（如 `2hi1+K2` = 颜色 2、横向直线、1 层冰、2 层锁链）、具名计数、`StdGen` 的 `show`；盘面与回放脚本（`mtWaves` / `mtEnd` 投影文本）用手写 FNV-1a 64 压缩。不对内部类型直接 `show`。
- **只经过门面**：取数只用 `Match3.Board` / `Match3.Game`（及数据类型 `Match3.Types` / `Match3.Ufo`），同一份代码在 `3bd26d8` 与 `5eef3e3` 上原样编译、输出全等（md5 `b792e77a…`）。
- **维护纪律**：内部表示变了只改投影函数，`golden.txt` 一个字都不动；确需重录（规则行为有意变化，机制刀停期间不应发生）用 `test/golden/regen.sh`，并在提交说明里写明原因。

## 编写约定

- 固定 `StdGen` / 手工构造 `Board`，避免 flaky。
- 改规则必须同步更新或新增用例；禁止「只改文档宣称」。
- 不修改测试来迁就错误实现；先修规则或先补回归再合。

## 与 CI 的关系

仓库可能另有工作流配置；**本地以 `stack test` 全绿（当前 222，含金标准比对）为合并门禁**。本文不依赖未跟踪的 `.github/` 内容。
