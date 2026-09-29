# 测试

## 如何运行

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
cd /path/to/match3
stack test
```

- 库测 **不需要** 显示器或 SDL 运行库参与链接执行路径上的窗口。
- 期望：**220** 个命名用例通过（Tasty：`testCase` + `testProperty`）。
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

前端的连锁逐轮回放和步末动画只播 `trace*` 返回的 `MoveTrace`，这组测试保证它和真实规则结果一致（同步关系见 [architecture.md](architecture.md#逐轮回放与规则的同步)）：

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
| `trace_end_spread_from_adjacent_source` | 巧克力 / 藤蔓的新占格都能在之前的盘面找到正交相邻的同类来源（前端据此决定从哪边「长出」） |

**底线**：`trace_swap_final_equals_trySwap` 要求「未洗牌、实际比对终盘的步数 > 400」，失败信息会打印比对数和因自动洗牌跳过的步数（当前样本约 517 比对 / 9 跳过），防止抽样悄悄缩水、测试名存实亡。

**已知缺口**：发生自动洗牌（`gsShuffled`）的步只比对得分、清除格和连击数，**暂不逐帧比对终盘**。`mtFinal` 是洗牌前的盘面，洗牌用的生成器状态没有进 `MoveTrace`；等给 `MoveTrace` 加上 `mtGen`、机制刀重开时再补上洗牌步的逐帧比对。前端洗牌动画（`StShuffle`）直接以结算后的 `gsBoard` 为终点，不受影响。

## 编写约定

- 固定 `StdGen` / 手工构造 `Board`，避免 flaky。
- 改规则必须同步更新或新增用例；禁止「只改文档宣称」。
- 不修改测试来迁就错误实现；先修规则或先补回归再合。

## 与 CI 的关系

仓库可能另有工作流配置；**本地以 `stack test` 220 绿为合并门禁**。本文不依赖未跟踪的 `.github/` 内容。
