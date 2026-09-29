# 架构

## 分层

```
┌─────────────────────────────────────┐
│  app/Main.hs（match3-sdl）          │  SDL2 输入 / 绘制 / 事件 / 粒子
│  依赖 Match3.Core、ComboFx、Art     │  不实现规则
└───────┬───────────────┬─────────────┘
        │               ▼
        │   ┌───────────────────────┐
        │   │ app/ComboFx.hs        │  纯阶段机：逐轮回放 + 步末阶段
        │   │ 只依赖 Match3.Core    │  只消费 MoveTrace，不算规则
        │   └───────────┬───────────┘
        │ 依赖          │ 依赖（类型 MoveTrace / Board）
┌───────▼───────────────▼─────────────┐
│  Match3.Core（library 门面）         │  再导出稳定公开 API
└─────────────────┬───────────────────┘
                  │
     ┌────────────┼────────────┐
     ▼            ▼            ▼
 Match3.Game   Match3.Board  Match3.Types
 （局面 / 步）  （棋盘 / 连锁） （类型 / 关卡表）
     │            │
     └──── 调用 ──┤
                  ▼
 Obstacles Rainbow Combos Ice Grass
 Carpet Snail Ufo Countdown Conveyor
 Boosters Daily
 （纯函数机制模块；无 IO）
```

**依赖方向（硬约束）**

- 纯核心 / library ← 应用：`match3-sdl` 依赖 `match3` 库；库**不**依赖 SDL。
- `Core` 是门面：把各子模块符号汇总导出，便于前端与测试只 import 一处。
- 机制子模块（障碍、彩虹、合成、冰、草系、地毯、蜗牛、飞碟、倒计时、传送带、道具种子、每日）尽量只依赖 `Types`（及彼此必要的窄依赖），由 `Board` / `Game` 编排调用顺序。
- 回放方向单向：`Match3.Board.traceCascade*` / `Match3.Game.trace*` 产出 `MoveTrace` → `app/ComboFx.hs`（纯阶段机，按时间线把它拆成帧）→ `app/Main.hs`（`AnimCascade` 绘制、事件锁定）。核心**不**知道帧、阶段或样式；`ComboFx` 不 import SDL，也不调用 `trySwap` 等规则入口，只读 `MoveTrace` 与 Main 传入的结算后盘面。

## 模块地图

| 模块 | 职责 | 不负责 |
|------|------|--------|
| `Match3.Types` | `Color` / `GemKind` / `CellOverlay` / `CellContents`、构造器与谓词、`LevelGoal` / `Outcome` / `allLevels` | 连锁、交换、IO |
| `Match3.Board` | 读写格、匹配查找、清除 / 特殊扩展、重力与补子、传送门沉降、UFO 波次连锁、`runCascade*`；逐轮快照 `traceCascade*`（`CascadeWave`） | 步数/目标结算、道具扣次 |
| `Match3.Game` | `GameState`、`trySwap` / 道具、逐轮回放脚本 `trace*`（`MoveTrace`，含步末效果 `EndStep`）、结局、洗牌保装饰、战役装饰与门户/皮带布局、步数携带 | 像素绘制 |
| `Match3.Core` | 再导出公共 API | 自身几乎无逻辑 |
| `Match3.Obstacles` | 石头/宝箱/蜂蜜/蛋糕/保险箱/气球/彩蛋/瓶子/精灵/魔法帽/果汁机的邻消削层与触发 | 连锁循环 |
| `Match3.Rainbow` | 彩虹判定与清色种子 | 合成几何（见 Combos） |
| `Match3.Combos` | 特殊×特殊合成种子 | 普通三消 |
| `Match3.Ice` | 匹配时削冰层 | overlay（Freeze/Chain…） |
| `Match3.Grass` | 草/藤/巧/雾/链/冻/帘/蒸汽的清除与蔓延 | 蜗牛爬行 |
| `Match3.Carpet` | 地毯覆盖计数、关卡地毯布局 | 饼干底行收集逻辑（在 Board/Game） |
| `Match3.Snail` | 蜗牛一步爬行 / 掉头 | 步末其它效果编排 |
| `Match3.Ufo` | 飞碟吸色目标与移格 | 棋盘清除（由 Board 掩码后清） |
| `Match3.Countdown` | 倒计时 tick / 归零爆炸种子 | 爆炸后连锁（Board） |
| `Match3.Conveyor` | 传送带循环移位 | 移位后再连锁（Game） |
| `Match3.Boosters` | 锤子/十字**种子位置**（纯几何） | 扣次数与连锁（Game） |
| `Match3.Daily` | 日期种子、每日配置、三星公式 | 每日盘面装饰（Game） |
| `app/Main.hs` | 窗口、事件、工具模式、地图 UI、动画；调 `trace*` 取回放脚本交给 `ComboFx`，绘制各阶段（`drawCascade`、弹字 / 浮字 / HUD 总结），播放期间锁定输入 | 改写规则结果 |
| `app/ComboFx.hs` | 连锁逐轮回放的纯逻辑：阶段机（高亮→消失→下落→落定，以及步末阶段：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌）、时间线常量、连击等级样式、下落映射、浮字曲线 | 绘制、规则计算（只消费 `MoveTrace`） |
| `app/Art.hs` | 贴图图集（BMP + 索引）加载、路径查找、九宫格面板、染色/加色绘制 | 游戏状态；缺资源时由 Main 退回几何绘制 |

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
- **回放脚本**：`MoveTrace { mtStart, mtWaves :: [CascadeWave], mtFinal, mtEnd :: [EndStep] }` 是纯数据，由 `trace*` 对同一 `GameState` 重算得到，不写回 `GameState`。`mtFinal` 是 `ensurePlayable` 之前的稳定盘：没有自动洗牌时等于结算后的 `gsBoard`；发生洗牌时二者不同，`ComboFx` 据此在最后追加 `StShuffle` 阶段（洗牌**不在** `mtEnd` 里）。前端调用顺序：先 `trySwap` / `use*` 拿结算结果，再用操作前的状态调 `trace*` 拿脚本，`moveFx` 为空时直接丢弃脚本。

## 逐轮回放与规则的同步

`trace*` 是**新增**的平行实现（不改原有函数语义），必须和规则路径按相同步骤、相同随机数消耗顺序重算，否则回放和真实结果会分叉。改规则时下面三处要一起改：

| # | 规则路径 | 回放路径 | 护栏测试 |
|---|----------|----------|----------|
| 1 | 连锁：`Board.runCascadeScoredWithUfos` / `runCascadeScoredFromSeedsWithUfos` / `resolveCountdowns` / `runPostBeltCascade` | `Board.traceCascade` / `traceCascadeFromWave` / `traceCascadeFromSeeds` / `traceCountdowns` / `tracePostBeltCascade`（每轮一个 `CascadeWave`，UFO 吸收单独一轮） | `trace_cascade_final_equals_stabilized`、`trace_seeds_final_equals_stabilized`、`trace_multi_wave_each_round_visible` |
| 2 | 步末：`Countdown.tickCountdowns`、`Conveyor.shiftBelts`、`Grass.spreadVines` / `spreadChoco` / `spreadSteam`、`Snail.stepSnailsAvoidingBlocked` | `Game.applyEndEffect`（把 `EndEffect` 重放回盘面）、`traceSpreads` / `spreadPairs`（蔓延及来源格）、`traceSnails`（逐只 `stepSnailAtBlocked`）、`beltMoves`（多条皮带合成「原格 → 新格」） | `trace_end_steps_replay_to_trySwap_final`、`trace_end_steps_boosters_replay`、`trace_end_snail_push_and_turn`、`trace_end_spread_from_adjacent_source` |
| 3 | 编排：`Game.trySwap` / `useFreeSwap` / `useHammer` / `useCrossClear` 内部顺序（主连锁 → 倒计时 → 皮带 → 藤 / 巧 / 蒸汽 → 蜗牛 →（成消）再连锁；道具只有蔓延） | `Game.traceSwap` / `traceFreeSwap` / `traceHammer` / `traceCrossClear`（`EndStep.esAfterWaves` 标出插在第几轮之后） | `trace_swap_final_equals_trySwap`、`trace_boosters_final_equal_result`、`trace_rejected_move_is_empty` |

对应源码处有中文「同步约束」注释（`runCascadeScoredWithUfos`、`trySwap`）。护栏细节与底线见 [testing.md](testing.md#逐轮回放护栏)；前端时间线见 [ui-art.md](ui-art.md#连击表现逐轮回放)。
