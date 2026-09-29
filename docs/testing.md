# 测试

## 如何运行

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
cd /path/to/match3
stack test
```

- 库测 **不需要** 显示器或 SDL 运行库参与链接执行路径上的窗口。
- 期望：**298** 个命名用例通过（Tasty：`testCase` + `testProperty`）：原有 262 个 + 第 1 刀新增 8 条 QuickCheck 性质与 1 个扫描工具自测 + 第 2 刀新增 2 个（`cell_accessors_total`、`ec_registry_checked_slots`） + 第 3 刀新增 1 条性质（`qc_find_hint_local_matches_reference`） + 第 4 刀新增 2 条性质（`qc_counts_algebra`、`qc_counts_monotone_legacy_view`） + 第 5 刀新增 3 条性质（`qc_goal_matches_legacy`、`qc_goal_progress_laws`、`qc_goal_progress_bounded`） + 第 6a 刀新增 9 个（`test/Spec/Levels.hs`：7 个单元测试 + 2 条性质） + 第 6b 刀新增 2 个（`ec_some_element_eq_by_type`、性质 `qc_name_newtypes_show_ord`） + 第 7a 刀新增 4 个（`ec_level_element_stateful_extension`、`br_board_takes_hooks_only`、性质 `qc_level_hooks_match_legacy` / `qc_level_elems_readers_roundtrip`） + 第 7b 刀新增 4 个（`br_end_phase_table_order`、`ext_end_effect_generic_hopper`、性质 `qc_end_table_matches_legacy` / `qc_ask_levels_folds_in_order`）。
- 合并门禁：上述 `stack test` 全绿即可合入；不要在红测上合并。

可选完整链路：

```bash
stack build && stack test && stack exec match3-sdl
```

macOS Apple Silicon 构建 SDL 前端时：

```bash
export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig
```

若 Stack 镜像/签名异常，优先使用 `stack.yaml` 中 `system-ghc: true` 与本机 ghcup 装好的 GHC 9.14.1；Hackage 镜像报 root.json 签名不足时改用官方 Hackage 源（去掉全局配置里的 `package-index` 镜像设置）。

## 套件结构

- 入口：`test/Spec.hs` 只汇总各功能模块：每个模块导出平铺的 `tests :: [TestTree]`，入口把它们拼进同一个顶层组 `"match3"`，所以 `stack test --ta '--list-tests'` 的完整路径（`match3.<测试名>`）与拆分前（`bf16a49`，单文件 8406 行）逐字相同。
- 框架：tasty + tasty-hunit + tasty-quickcheck
- 依赖库 API：主要通过 `Match3.Core`
- 模块由 hpack 按 `source-dirs: test` 自动发现（`match3.cabal` 头部仍写 hpack 0.38.1）；新测试放进对应功能模块，并加进该模块的 `tests` 列表。
- 目录（用例数合计 298）：

| 文件 | 用例数 | 内容 |
|------|-------:|------|
| `test/Spec/GridMatch.hs` | 6 | 盘面与匹配：交换回滚、稳定盘面、三连、提示、可玩开局；格子取值函数（`cellColor` / `cellKind` / `colorAt` 等）是总函数 |
| `test/Spec/Gravity.hs` | 2 | 重力与补子、固定格不下落 |
| `test/Spec/Cascade.hs` | 8 | 连锁 / 公共结算：连锁到稳定、连击计分、种子续波、步耗与步末顺序、`release_core_invariants_green`（复用 GridMatch / Gravity / GoalsLevels 的用例） |
| `test/Spec/Specials.hs` | 18 | 特殊块生成与引爆、特殊 × 特殊、彩虹取色、软锁 |
| `test/Spec/Builtin/Obstacle.hs` | 35 | 本体障碍（对应 `Element/Builtin/Obstacle`）：石头、宝箱、蜂蜜、气球、蛋糕、保险箱、双面、彩蛋 |
| `test/Spec/Builtin/Collectible.hs` | 7 | 收集物（对应 `Element/Builtin/Collectible`）：饼干、时间精灵（气泡在 `JellyBubble`） |
| `test/Spec/Builtin/Actor.hs` | 23 | 会动 / 会生成东西的元素（对应 `Element/Builtin/Actor`）：魔法帽、果汁机、染色瓶、倒计时、蜗牛 |
| `test/Spec/Builtin/Layer.hs` | 31 | 冰层与叠层（对应 `Element/Builtin/Layer`）：冰、草、藤、巧、迷雾、锁链、冰冻、窗帘、蒸汽、软命中 |
| `test/Spec/Builtin/Level.hs` | 24 | 关卡级元素（对应 `Element/Builtin/Level`）：皮带、传送门、飞碟、地毯 |
| `test/Spec/Boosters.hs` | 12 | 道具：锤子 / 自由交换 / 十字 |
| `test/Spec/GoalsLevels.hs` | 31 | 目标、结局、星级、关卡表、每日、地图与步数结转 |
| `test/Spec/Levels.hs` | 9 | 第 6a 刀：关卡记录与关卡表——全部内置关卡与每日挑战（两年每天，覆盖 10 种目标）的放置表都是 `Right`、`placeWith` 的 `UnknownElement` / `PlaceOutOfBounds`、坏放置表的报错带关卡名、`campaignGame` 与 `newGameAtLevel … (levelConfig …)` 相同、越界的重开 / 下一关夹到范围内、`allLevels !!` 源码扫描（src / app / web/hs / test），性质 `qc_lookup_level_in_range` / `qc_clamp_level_index_found` |
| `test/Spec/Element.hs` | 2 | 元素注册表（测试专用木箱 `Crate` / 条目 `crateDef` 在 Support 里） |
| `test/Spec/Extension.hs` | 7 | 段 2c 扩展钩子护栏：Board 层收注册表（源码扫描）、`GoalNamed`、地面层、边缘收集、步末补结算、经 Engine 的手动洗牌；第 7b 刀通用步末效果（跳跳虫）（样例元素苔藓 / 风筝 / 陷坑 / 浮尘 / 跳跳虫只定义在该模块里） |
| `test/Spec/Branches.hs` | 10 | 段 4 专门分支收编护栏：测试专用成对交换规则（拉杆）/ 开启规则（豆荚）/ 可推动（小车）只经注册表生效；内置改色 / 推动谓词与原写死谓词相同；关卡级元素（飞碟 / 皮带 / 传送门 / 地毯 / 地面层）的节拍回复与原实现相同、去掉后不生效（含 38 关实测；第 7a 刀起经钩子与 `Element.Level` 的节拍函数）；主流程源码扫描；第 7a 刀 `br_board_takes_hooks_only`（Board 核心只收钩子、流水线不读旧的五个字段、删掉的名字不再出现）；第 7b 刀 `br_end_phase_table_order`（步末表的内容与顺序、按表执行、删掉的步末构造器不再出现） |
| `test/Spec/ElementClass.hs` | 13 | 元素类（阶段 1 原型 → 阶段 2 迁移）：逐格查询 / 规则 / 逐手结果与阶段 1 的元素查询快照全等；`SomeElement` 的 Eq / Show（第 6b 刀：按类型比较，同名不同类型不等）、冰层修饰器组合、状态在元素值里、开放消息；扁平记录已删（源码扫描）、关卡级元素经消息、第 7a 刀带状态的扩展关卡级元素「虹吸」不改主流程接入（`levelStart` 开局、状态写回 `gsLevelElems`、去掉注册后原样不生效、`Show` 追加 `gsLevelExtra`；第 7b 刀起保留内置飞碟，同一 `Refilled` 节拍两者都吸收）、自定义可匹配宝石；注册表槽位由原型推导、`mkRegistryChecked` 报重名 / 槽位冲突 / 推不出槽位 |
| `test/Spec/JellyBubble.hs` | 8 | 段 5 双层果冻 / 气泡：按层计数与目标、洗牌 / 撤销、气泡邻消 / 直接命中即破、挡交换 / 无色 / 下落、第 39 / 40 关、主流程源码扫描 |
| `test/Spec/Engine.hs` | 5 | 多游戏通用接口（玩具 `test/Toy.hs`、依赖方向扫描、三消实例）；段 3：终局后撤销与 `13094d1` 比对、前端只经 `gameStep`（源码扫描） |
| `test/Spec/UIEvents.hs` | 8 | 前端反馈（MoveFx / 连击反馈 / 清除格）与效果事件 |
| `test/Spec/ReplayUndo.hs` | 17 | 回放脚本 `trace_*`、撤销、洗牌 |
| `test/Spec/Golden.hs` | 1 | `golden_behaviour_snapshot`（调 `test/golden/Golden.hs`） |
| `test/Spec/Properties.hs` | 20 | QuickCheck 性质（原有 1 条 + 第 1 刀 8 条 + 第 3 刀提示局部检查对照旧实现 1 条 + 第 4 刀计数 2 条 + 第 5 刀目标 3 条 + 第 6b 刀名字 newtype 1 条 + 第 7a 刀关卡级钩子 / 读数 2 条 + 第 7b 刀步末表 / 折叠回复 2 条，见「性质测试」） |
| `test/Spec/SourceScan.hs` | 1 | 源码扫描工具自测 `support_source_scanner`（注释剥离、import 解析、标识符匹配） |
| `test/Spec/Support.hs` | — | 多个模块共用的辅助：`allPos` / `setCells` / `customsOn` / `isCustomNamed`、`tripleBoard` / `tripleMove`（第 1 行 C5 四连局面）、`isWin`、`firstLevel`、`levelAt` / `levelGame`（第 6 刀：按下标取关 / 开局，没有这一关时报错，取代测试里的 `allLevels !! i`）、`firstWave`（没有连锁轮时断言失败，代替 `head . mtWaves`）、`stepThenUndo`（经 `match3ShellWith reg` 走一步再 `Undo`，段 3）、`findMatchPair` / `findNoMatchPair` / `stuckNoMoveBoard` / `stableBoard`、连击反馈局面、回放逐轮检查、事件细节检查、测试专用木箱 `Crate`（条目 `crateDef`）等；并重新导出 `Spec.Support.Source` |
| `test/Spec/Support/Source.hs` | — | 源码扫描工具（见「源码扫描约定」） |
| `test/Toy.hs` | — | 通用接口的玩具实现（只 import `Engine.*`） |
| `test/golden/` | — | 金标准投影 `Golden.hs` 与 `golden.txt`；元素查询快照 `ElementQueries.hs` 与 `element-queries.txt`（元素类迁移） |

- 拆分验收：拆分前后 `--list-tests` 排序后 diff 为空（228 = 228），每个测试函数逐字搬运（原文件每一行都能在新模块里找到），`golden.txt` 全等。
- 第 1 刀（测试基建）的搬家：原 `test/Spec/Obstacles/{Body,Features,Layers}.hs`（1960 / 1696 / 1017 行）按 `src/Match3/Element/Builtin/` 的分组拆成 `test/Spec/Builtin/*.hs`，每个测试函数逐字搬运（两份文件合起来排序后逐行 diff 只差模块头、import 与测试列表），原 262 个测试名排序后 diff 为空。新元素的测试放进与它的 Builtin 分组同名的模块。
- 测试套件不再用 `-Wno-x-partial`：原先的 16 处 `head` / `last` 改成 `firstWave` / `firstLevel`、模式匹配或 `take 1` 比较（空表时断言失败而不是崩溃）。

覆盖类型（按主题，非穷尽）：

| 主题 | 示例用例名 |
|------|------------|
| 基本不变量 | `inv_no_match_rollback`、`inv_move_to_stable` |
| 匹配 / 重力 / 连锁 | `match_line_ge3`、`gravity_then_refill`、`cascade_until_stable` |
| 属性测试 | `qc_findMatches_ge3`、`qc_*`（见「性质测试」） |
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

## 性质测试（第 1 刀）

`test/Spec/Properties.hs`。新增的 8 条统一固定种子（`localOption (QuickCheckReplayLegacy 20260930)`，命令行 `--quickcheck-replay` 对它们不生效），每条用 `withMaxSuccess` 控制次数，合计约 0.5 s。生成器只在测试里：任意格（全部内置本体、宝石种类、冰层 0–2、全部叠层、已注册 / 未注册的自定义名字）、约 1/4 空洞的可空盘面、带现成匹配的完整盘面、战役关卡 + 种子 + 动作选择（`Pick`：从某格起按行优先找第一对会被接受的相邻交换，约 1/10 换成锤子 / 十字 / 自由交换，没有次数时被拒也是合法输入）。

| 用例 | 次数 | 性质 |
|------|-----:|------|
| `qc_gravity_keeps_cells_and_column_order` | 500 | `applyGravityWith defaultRegistry`：每列非空格数不变；固定格（`falls = False`）原地不动；可下落格自上而下的相对顺序不变；相邻固定格之间的每一段空洞在上、实格在下 |
| `qc_refill_leaves_no_holes` | 500 | `refill` 对任意可空盘面都补满（不会走到 `error "refill: hole"`）；原有格不变；每个洞补成普通宝石 |
| `qc_cascade_terminates_stable` | 300 | 带现成匹配的盘面上 `cascadeMatches` 在 2 s 内结束，终盘没有现成匹配，逐轮首尾相接、最后一轮到终盘（约 93% 用例真的连锁，约 1/3 有 3 轮以上） |
| `qc_undo_redo_roundtrip` | 100 | 经 `match3Shell` 走 0–3 步到局中，再走一步被接受的走步：`Undo` 回到走步前状态经撤销规则整理后的快照（`hpRestore . hpSnapshot`，按 `Show` 全字段比较、含随机数生成器），历史深度复原；再把同一动作做一遍（历史层没有单独的重做，重做 = 重放同一动作），结果与第一次逐字段相同 |
| `qc_replay_same_seed_deterministic` | 60 | 同关卡、同种子、同一串动作，两次独立执行（逐步执行 vs `runActions`）每一步的状态、事件、是否接受逐字相同 |
| `qc_goal_progress_monotone` | 60 | 一局 1–8 步里分数、`gsCollected`、`gsProgress`、各类计数字段、各色清除数、按名字的计数都不减；目标一旦满足就一直满足（第 5 刀前读 `goalProgressEx` / `goalMetEx`） |
| `qc_goal_matches_legacy` | 2000 | 第 5 刀：测试里留一份第 5 刀前的 13 构造器目标 `OldGoal`（派生 `Show`）与逐字抄来的 `goalMetEx` / `goalProgressEx` / `goalTarget`；任意旧目标换成新目标数据后，`show`（含 `showsPrec 11` 加括号）、目标值、在任意分数 / 计数（内置各键、时间精灵、各色、名字）下的达成与进度都与旧实现相同（旧 `gsCollected` 按旧结算口径由计数给出） |
| `qc_goal_progress_laws` | 1000 | 第 5 刀：任意 1–3 项配额（分数与计数混合）：进度 ≥ 0；达成 ⟺ 每项度量 ≥ 目标值；单项 达成 ⟺ 进度 ≥ 目标值；多项 进度 ≤ 目标值且 达成 ⟺ 进度 = 目标值；分数 / 计数只增时进度不减、达成保持 |
| `qc_goal_progress_bounded` | 60 | 第 5 刀：整局 1–10 步（约 1/3 抽多色关 6 / 14）每个状态：进度 ≥ 0、达成 ⟺ 各项配额都达到、单项 达成 ⟺ 进度 ≥ 目标值、多色 进度 ≤ 目标值；`Won` / `LevelClear` 结局时目标达成、`Lost` 时未达成 |
| `qc_registry_decode_roundtrip` | 1000 | 任意格 `toCell (elementOf reg cell) == cell`；本体名落在槽位一致的条目上（内置本体 = `SlotCell (cellSlot cell)`，已注册自定义 = `SlotCustom`，未注册名字不在表里）；最上层的冰层 / 叠层落在 `SlotIce` / `SlotOverlay (overlaySlot o)` 的条目上 |
| `qc_registry_names_slots_unique` | 1 | 内置条目原始列表 `builtinDefs`（注册表去重之前）：名字互不相同；本体槽号恰好 0–19、叠层槽号恰好 0–7 各一个；冰层条目只有一个 |
| `qc_find_hint_local_matches_reference` | 400 | 第 3 刀：`findHintWith`（只对交换两格所在行 / 列做局部匹配检查）与留在测试里的旧实现 `findHintReference`（整盘 `hasAnyMatchWith (swapCells …)`）返回相同：带现成匹配的盘面、各关开局、默认开局、整盘随机格四类。去掉局部检查的任一分支时该性质在 20 例内即失败 |
| `qc_counts_algebra` | 1000 | 第 4 刀：`Counts` 的代数——`countOf` 等于按键求和；稀疏（不存 0）、键升序；`plusCounts`（`<>`）逐键相加、交换、结合、`noCounts` 为单位元；`bumpCount k n` = 加一个单键计数；`namedCounts` = `CountNamed` 项按名字升序 |
| `qc_counts_monotone_legacy_view` | 60 | 第 4 刀：一局 1–8 步里 `gsCounts` 每个存下的个数都 > 0、每个键不减；与旧字段对照——「按某个计数键」的目标（石块 … 地毯、名字；第 5 刀起用 `goalView` 取键）下 `gsCollected == gsCount 该键`（旧实现直接取对应字段），`show` 仍按旧字段名（`gsStonesCleared = …` … `gsElementCounts = …`）打印同一个数。另在仓库外做过一次对照：40 关 × 25 个种子 × 最多 25 手（提示交换 + 锤子 + 十字）共 17866 个局面，新旧（4b83dc9）`show` 全等；第 5 刀对 47485c3 重做（另加 30 天每日挑战，每行再打印目标值、`checkOutcome`、失败提示）共 18383 行全等 |
| `qc_lookup_level_in_range` | 500 | 第 6a 刀（在 `test/Spec/Levels.hs`，同样固定种子）：下标取 −20 … 关卡数 + 20，`lookupLevel i` 为 `Just` ⟺ 0 ≤ i < `levelCount`，取到的关 `lvlIndex = i` |
| `qc_clamp_level_index_found` | 500 | 第 6a 刀：任意 `Int`（含 ±1000 内与极端值），`clampLevelIndex` 的结果总能被 `lookupLevel` 找到、幂等、范围内不变 |
| `qc_level_hooks_match_legacy` | 500 | 第 7a 刀：随机盘 / 可空盘 × 0–3 个飞碟 × 0–3 对传送门：`builtinHooks` 的 `onAbsorb` = `stepUfos`（吸走格 + 移动后的飞碟），`onSettle` = `portalTeleport`（可穿门谓词取自注册表），吸收后交回的钩子传送不变 |
| `qc_level_elems_readers_roundtrip` | 100 | 第 7a 刀：战役关卡 × 种子开局的 `gsLevelElems` 名字依次为 ufo / belt / portal / carpet / ground；五个派生读数原样写回（`setUfos` 等）后 `==` 与 `show` 都不变；读数等于关卡记录（有放置的飞碟 / 地毯、皮带、传送门、地面层） |
| `qc_end_table_matches_legacy` | 300 | 第 7b 刀：战役任意关 + 种子走 0–4 手后的局面，交换取提示、道具取随机格为种子：`runEndTable` 按 `swapEndTable` / `boosterEndTable` 执行的结果（各段连锁的终盘 / 轮次 / 计数 / 生成器 / 关卡级元素、步末记录、终盘、地毯腾空盘面）与测试里逐字保留的第 7 刀前 `swapEnd` / `boosterEnd` 相同（约 59% 的交换用例有步末效果） |
| `qc_ask_levels_folds_in_order` | 300 | 第 7b 刀：0–5 个「追加编号」的测试关卡级元素随机注册：`askLevels`（原型）与 `askLevelsIn`（一局的状态）都按注册顺序折叠所有回复者（结果 = 编号依次追加），各回复者推进后的状态同名写回、顺序不变；没人注册时 Nothing |
| `qc_name_newtypes_show_ord` | 1000 | 第 6b 刀：任意名字（含引号 / 中文 / 空串）与整数：`ElementName` / `CustomState` 的 `show` 与 `showsPrec 11` 和底层 `String` / `Int` 相同，`compare` / `==` 相同；`Custom` 格的 `show` / `showsPrec 11` 逐字等于改前的派生输出、`Ord` 与按 (名字, 状态) 比较相同 |

条目的原型值（`Proto`）不导出，所以「解码往返」从格子一侧做：对每种格子验证解码再编码得到原格、并且解码落到槽位一致的条目上；再用 `qc_registry_names_slots_unique` 保证每个槽位恰好一个条目。第 1 刀跑这些性质时没有发现规则 bug。

## 源码扫描约定（第 1 刀）

依赖方向 / 「主流程不点名某元素」这类护栏都用 `test/Spec/Support/Source.hs`（经 `Spec.Support` 导出），不再各写一份：

- `sourcesUnder dir` / `sourcesUnderAll dirs`：递归列出目录下全部 `.hs`（按路径排序），**不写死文件清单**，新增或拆分模块时扫描范围自动跟上；每个扫描测试另外断言几份关键文件确实在清单里，防止扫描范围悄悄变空。
- 项目内的范围：`builtinSources`（`Element/Builtin.hs` + `Element/Builtin/`）、`mainFlowSources`（`Board/`、`Game/`、`Element/` 除内置定义、`Match3/Engine.hs`、`src/Engine/`；不含关卡数据 `Game/Level.hs`）、`pipelineSources`（`Board/` + `Game/`，不含 `Game/Level.hs` 与回放记录层 `Game/Trace.hs`）。
- `stripComments`：去掉 `--` 行注释（`-->` 这类运算符不算）与可嵌套的 `{- -}` 块注释（含 pragma），字符串 / 字符字面量原样保留、换行保留；`stripStrings` 再清空字面量内容。
- `importsOf`：只看 import 行，返回模块名（处理 `qualified`、包名导入）；依赖方向类的约束一律用它。
- `codeIdents` / `mentionsIdent`：去掉注释与字符串后的标识符，限定名按基本名比较（`M3E.play` 算 `play`）。
- 工具本身由 `support_source_scanner` 覆盖；各扫描测试改造后做过变异检查（往 `Game/Move.hs` 加 `import Match3.Combos`、往 `Board/Cascade.hs` 加代码 `stepUfos`、往 `app/` 加 `"crate"`、往 `Engine/Game.hs` 加 `import Match3.Types`、在 `Board/` / `Game/` 新建含 `defaultRegistry` / `"bubble"` 的模块，对应测试都会失败；只加进注释时不失败）。

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
| `trace_end_steps_replay_to_trySwap_final` | 全部关卡（段 5 起 40 关）× 种子 1–3 的每个成功交换按「轮 → 步末 → 轮」时间线重放：每段首尾相接、`applyEndEffect esEffect esBefore == esAfter`，终盘等于 `trySwap`（未洗牌时）；抽样必须覆盖 tick / belt / SpreadVine / SpreadChoco / SpreadSteam / snail 六种效果 |
| `trace_end_steps_boosters_replay` | 道具只有蔓延类步末效果，重放后同样到达终盘 |
| `trace_end_snail_push_and_turn` | 蜗牛碰壁原地掉头（`smFrom == smTo`、朝向反转）；前方是宝石则爬过去、宝石换到原格 |
| `trace_shuffle_step_replays` | 洗牌步逐帧比对（见下） |
| `trace_end_spread_from_adjacent_source` | 巧克力 / 藤蔓的新占格都能在之前的盘面找到正交相邻的同类来源（前端据此决定从哪边「长出」） |

**底线**：`trace_swap_final_equals_trySwap` 要求「未洗牌、实际比对终盘的步数 > 400」，失败信息会打印比对数和因自动洗牌跳过的步数（当前样本约 517 比对 / 9 跳过），防止抽样悄悄缩水、测试名存实亡。

**洗牌步（第二刀补上）**：`MoveTrace` 新增 `mtGen`（洗牌前的生成器）与 `mtShuffle`（洗牌后的盘面）。`trace_shuffle_step_replays` 对全部关卡（段 5 起 40 关）× 种子 1–3 × 开局全部成交交换（每条再沿首个成交交换走 2 手）逐帧比对：时间线重放到 `mtFinal`，再从 `(mtFinal, mtGen)` 重放 `ensurePlayable` 必须到达结算后的 `gsBoard` / `gsGen`；未洗牌的步要求 `mtFinal == gsBoard`、`mtGen == gsGen`。38 关时 **3801** 个成交步逐帧比对，其中 **29** 个自动洗牌步；底线 > 3000 / > 20。

## 行为金标准（golden）

`test/golden/Golden.hs` 把固定种子下的规则结果投影成稳定文本，入库为 `test/golden/golden.txt`（2534 行：前 2186 行自 `4fbcefc` 起逐字不变，第 2187–2344 行是段 1 追加的 H4–H6，第 2345–2534 行是段 5 追加的第 39 / 40 关），`golden_behaviour_snapshot` 在 `stack test` 里逐行比对，失败时报告第一处分叉的行号。

- **覆盖**：38 关 × 种子 {1, 2} × 最多 15 步（每步：成交的交换、结局、完整计数、`gsCombo` / `gsLastCleared`、随机数状态、`traceSwap` 回放；辅助列：撤销 / 提示 / 洗牌 / 自动洗牌 / 结局判定 / 被拒交换；每 3 步三种道具的有次数 / 无次数结果与回放）、2 个每日式开局、6 个手工局面（H1–H3：护栏里的蜗牛撞墙推格、巧克力关、首个 3 连锁；H4–H6 见下；另含该局面全部成交交换）、连锁 API（`Match3.Board.Cascade` 的 `cascadeMatches` / `cascadeSeeds`）的普通 / 种子连锁 × 飞碟 / 传送门、各关开局 / 重开 / 下一关。
- **H4–H6（段 1 追加，第 2187–2344 行）**：仓库与文档里没有 H4–H6 的现成定义，按下面三类代表性手工局面补齐：
  - `H4-spread`：第 28 关（皮带 + 传送门 + 地毯 + 飞碟）再放藤 / 巧 / 蒸汽、两只蜗牛、将归零的倒计时和两块饼干——步末全部阶段（倒计时 → 皮带 → 蔓延 → 蜗牛 → 再连锁）同盘出现，蔓延源紧贴本步消除留下的空洞；局面第 4 步后连自动洗牌也救不回（`shuffle` 行连续出现），一并锁住。选它是因为 2c 的「步末之后补结算」就改在这里。
  - `H5-layers`：第 37 关地毯上，冰（1–3 层）与锁链 / 草 / 迷雾 / 冰冻 / 窗帘 / 藤 / 巧 / 蒸汽同格，含直线 / 炸弹压叠层——「多种叠层同格」由注册表自上而下逐层询问，是第二刀元素框架最容易走偏的地方。
  - `H6-shuffle`：经 `match3GameWith h6Reg`（内置表 + 一个不在盘上的测试探针元素）的 `Shuffle` / `Swap` 动作逐步「手动洗牌 → 交换」，再记一个斜纹死局的自动洗牌 `ensurePlayableWith h6Reg` 与手动洗牌——锁住「自定义注册表下的洗牌」，2c 修 `playWith` 洗牌分支时它必须不变。
  - 生成与比对：`13094d1` 上用当前 `Golden.hs`、`31275da`（2b 之前）上用 `4fbcefc` 版 `Golden.hs` 加同一段 H4–H6（旧侧没有注册表，H6 直接调 `shuffleGame` / `trySwap` / `ensurePlayable`；文件存档在 `tools/golden/Golden-4fbcefc-h456.hs`，不参与编译），两边 2344 行逐字全等后才追加入库。
- **行首**：`L05 s2 #07` = 第 5 关、种子 2、第 7 步；`H1-snail`、`C03/1`、`S07/2`、`G12 s5` 等为其他用例。
- **投影**：格子短码（如 `2hi1+K2` = 颜色 2、横向直线、1 层冰、2 层锁链）、具名计数、`StdGen` 的 `show`；盘面与回放脚本（`mtWaves` / `mtEnd` 投影文本）用手写 FNV-1a 64 压缩。不对内部类型直接 `show`。
- **取数路径**：入库时（`4fbcefc`）取数只经过门面 `Match3.Board` / `Match3.Game`，同一份代码在 `3bd26d8` 与 `5eef3e3` 上原样编译、输出全等（md5 `b792e77a…`）。第三刀删掉了这两个门面和元组兼容层，`Golden.hs` 改为直接 import `Match3.Board.*` / `Match3.Game.*` 子模块、从 `CascadeRun` 记录取连锁字段，盘面经 `boardRows` 投影——**它今后不能在 `51b1cfa` 及更早的提交上编译**；要和旧提交比对，把 `4fbcefc` 版的 `Golden.hs` 拿到旧提交上跑（输出与 `golden.txt` 逐字相同）。
- **耗时**：第三刀把 `Board` 换成数组（`getCell` O(1)）后，`golden_behaviour_snapshot` 在 box 上约 6.05 s → 4.3–4.5 s（单独运行 `-p golden_behaviour_snapshot`）。
- **段 5 追加（第 2345–2534 行）**：关卡表在第 38 关之后追加了两关，原有各段（L / G 行）改为只跑前 `campaign38 = 38` 关，保证前 2344 行逐字不变（追加前后用 `head -2344` 与旧文件 `cmp` 相同）；新关卡的行在末尾：第 39 / 40 关 × 种子 {1,2} × 15 步（与 L 行同一走法），每步后多一行 `ext`（`gnd=` 地面层、`named=` 具名计数——旧的 `pState` 不含这两个字段，只在新行里补），再加 G39 / G40 开局行。
- **维护纪律**：内部表示变了只改投影函数，`golden.txt` 一个字都不动；确需重录（规则行为有意变化，机制刀停期间不应发生）用 `test/golden/regen.sh`，并在提交说明里写明原因。

## 元素框架验收

框架见 [architecture.md](architecture.md#元素框架与事件)。

| 用例 | 断言 |
|------|------|
| `element_registry_custom_crate_extensibility` | 测试专用元素「木箱」`Custom "crate" 耐久`（**只定义在测试里：`test/Spec/Support.hs` 的 `Crate` instance + `customEntry`**：原型 `Blocker`，不下落、邻格真消除波及耐久 −1、耐久 1 再被波及就碎、计数 `CountNamed "crate"`）经 `register` 接入后：注册表多一项、内置一个不少；对它交换返回 `NoMatch` 且盘面不变；无匹配色；第 1 手（邻格 C5 三消）耐久 2→1、原地不动、不计数、不在清除格、事件里有 `EvHit "crate"`；第 2 手（耐久 1）碎掉、进入第一轮清除格、`namedCounts (gsCounts gs) == [("crate",1)]`、事件里有 `EvClear "crate"`；锤子削到 1；洗牌保留；同一局面在 `defaultRegistry` 下它是惰性占格（不被波及、锤子免疫、不计数）；`src/` 与 `app/` 下全部源文件（含注释，按目录列出）里没有字面量 `"crate"` / 「木箱」——主流程没有为它改一行 |
| `element_registry_matches_legacy_predicates` | 全部内置本体 × 宝石种类 × 冰层 × 叠层：注册表的挡交换 / 锤子免疫 / 固定格 / 点火 / 匹配色 / 洗牌保留与第二刀之前按构造器写死的谓词逐格相等；直接命中的几条代表（冰、锁链、保险箱、翻转、石头）与旧口径一致 |
| `trace_events_consistent_with_trace` | 全部关卡（段 5 起 40 关）× 种子 1–2 × 前 3 个成交交换：`EvScore` 之和 = 本步得分；`EvClear` 的格 = 各轮清除格并集；步末事件数 = `mtEnd` 长度；`EvShuffle` ⇔ `mtShuffle`；`EvBlast` 只来自直线 / 炸弹；**逐轮严格相等**（第三刀，前端波次界面改读事件后加）：该轮 EvClear 的格按事件顺序拼接 = `cwCleared`（顺序也相同），该轮 EvScore 之和 = `cwScore` |

## 扩展钩子验收（段 2c）

钩子见 [architecture.md](architecture.md#扩展钩子段-2c)。样例元素全部只定义在 `test/Spec/Extension.hs`，核心源码里没有它们的名字。

| 用例 | 断言 |
|------|------|
| `ext_board_modules_take_registry` | 源码扫描：`src/Match3/Board/` 下除 `Board.Default` 以外的全部模块（按目录列出）代码里不用 `defaultRegistry`、不 import `Match3.Element.Builtin*`（不带 With 的旧名集中在 `Board.Default`） |
| `ext_goal_named_counts_crate` | 木箱经 `CountNamed "crate"` 计数，`GoalNamed "crate" N` 达成即判胜；`resolve` 与 `gameStep` 两条入口结果相同；内置表下木箱是惰性占格、不计数 |
| `ext_ground_layer_test_element` | 测试专用苔藓（`SlotGround`，2 层）：上方每消一次去一层并按层计数，不占格、不挡匹配；撤销恢复、洗牌不动；未注册时毫无反应 |
| `ext_edge_drain_side_collectible` | 测试专用风筝（`drains = [EdgeLeft]`）：到左边被收走并计数，内部 / 底边的留在原处；同盘饼干照常底收、计数不受影响；未注册时是惰性占格 |
| `ext_post_end_settle_hole_element` | 测试专用陷坑（`PhaseMove` 规则，只声明 `erHoles`）：步末之后被挖空、上方下落、补子，回放恰好多一个只含沉降的轮次且首尾相接；`Engine` 入口结果一致；内置表下原地不动 |
| `ext_end_effect_generic_hopper` | 第 7b 刀：测试专用跳跳虫（`PhaseMove` 规则，排在蜗牛之后）步末向右与宝石换位，产出通用 `EndEffect EvMove "hopper"`：不改 Event / Trace / 主流程，回放逐项重放到终盘、效果事件带 (起点, 终点)、`Show` 退回记录语法；内置表下原地不动、没有它的效果 |
| `ext_manual_shuffle_keeps_crate_via_engine` | 走 `match3GameWith reg` 的 `Shuffle` 动作：木箱原位保留；`keepOnShuffle = False` 的浮尘只在自定义表下被洗走（修的是 `playWith` 洗牌分支原先用内置表的问题） |

**补结算统一路径的扫描**：`cascadeAfterEndWith`（第 3 刀起为 `cascadeAfterWith (AfterEnd …)`）对内置元素恒为空操作的实测依据——38 关 × 种子 1..100 × 15 步，每步检查主交换、三种道具与全部可成交交换对的步末终盘（`esAfter`）有无待挖空洞 / 待收边缘 / 沉降变化，共 **610,751** 手，pending = 0；金标准 2344 行全等（扫描程序不入库，结论写在这里）。

## 专门分支收编验收（段 4）

`test/Spec/Branches.hs`；样例元素拉杆 / 豆荚 / 小车只定义在该模块里。

| 用例 | 断言 |
|------|------|
| `br_swap_rule_test_element` | 拉杆（`swapRule`，`srOrder` 5，种子 = 交换两端）：无普通匹配的交换被接受、两端在首轮清除；无路可走的盘上提示经同一条规则给出拉杆；内置表下拉杆挡交换、被拒 |
| `br_open_rule_test_element` | 豆荚（`openRule`）：邻格真消除时开成直线，本轮不清除（坐住）、落定后仍在；与内置彩蛋同盘时两条开启规则都跑；内置表下原样下落 |
| `br_builtin_predicates_match_legacy` | 21 种样例格上 `recolorableWith defaultRegistry` = `isGem`、`pushableWith defaultRegistry` = `Snail.pushable` |
| `br_recolorable_from_registry` | 魔法帽：内置表下给两个邻格换色；把 gem 定义改成 `recolorable = False` 后颜色不变 |
| `br_pushable_from_registry` | 蜗牛：`pushable` 的小车被推回、蜗牛前进；内置表下小车挡住、蜗牛掉头；gem 改成不可推后蜗牛不推宝石 |
| `br_level_hooks_builtin_and_removable` | 内置表的四个关卡级元素依次为 ufo / belt / portal / carpet，经消息回复的结果与原实现（`stepUfos` / `beltMoves` / `portalTeleport` / `coverCarpets`）结果相同；`removeLevel` 后各自退化为不吸收 / 不移位 / 不传送 / 不覆盖 |
| `br_level_hooks_removed_in_play` | 38 关、种子 1、按提示走 6 手：四个关卡级元素都去掉后，飞碟关飞碟不动不吸、皮带关没有皮带步末效果（`EvBelt`）、地毯关不覆盖；内置表下同样的对局里三者都发生过 |
| `br_main_flow_no_special_branches` | 源码扫描：结算流水线 `pipelineSources`（`Board/*`、`Game/*`，不含 `Game.Level` / `Game.Trace`）全部模块都不 import `Match3.Combos` / `Rainbow` / `Obstacles` / `Carpet`，代码里（去掉注释与字符串）不用 `isRainbowSwap` / `isSpecialCombo` / `rainbowClearSeeds` / `comboClearSeeds` / `openSurprises` / `stepUfos` / `coverCarpets` / `beltMoves` / `portalWith`（第 1 刀起从逐文件规则放宽为整条流水线） |

段 4 的等价性依据：金标准 2344 行全等；showcase / l28 / l1map 三场景截图与 `51b1cfa` 基线 AE=0。

## 双层果冻与气泡验收（段 5）

`test/Spec/JellyBubble.hs`；设定见 [domain.md](domain.md#双层果冻与气泡段-5)。

| 用例 | 断言 |
|------|------|
| `jb_jelly_two_layers_counted_per_layer` | 上方格子被消除一次去一层、每层计 1（`namedCounts (gsCounts gs)` / `gsCollected`）；没被消到的果冻不动；不占格（盘面与无果冻时相同）；第二次消除清掉该格；`gameStep` 入口结果相同 |
| `jb_jelly_goal_wins_on_last_layer` | `GoalNamed "jelly" 2`：去第一层未过关，去最后一层过关、地面层清空 |
| `jb_jelly_keeps_on_shuffle_and_undo` | 第 39 关开局 16 格双层；手动洗牌不动地面层；走一步再撤销恢复层数 |
| `jb_bubble_pops_on_adjacent_clear` | 与真消除格相邻的气泡在首轮被清、计数、达成 `GoalNamed "bubble" 1`；不相邻的不动 |
| `jb_bubble_pops_on_direct_hit` | 锤子直接命中即破并计数 |
| `jb_bubble_blocks_swap_falls_no_match` | 三个气泡连成一排不算匹配；与气泡交换被拒；列里没有消除时不动；下方格被清掉后下落一格 |
| `jb_levels_appended` | 共 40 关；第 39 / 40 关目标为 `GoalNamed "jelly" 32` / `GoalNamed "bubble" 12`，种子 1–3 开局层数 / 气泡数等于目标且有可走步；前 38 关开局没有地面层、没有气泡 |
| `jb_main_flow_untouched_scan` | `mainFlowSources`（`Board.*`、除 `Game.Level` 外的 `Game.*`、除内置定义外的 `Element.*`、`Match3.Engine`、`Engine.*`，按目录列出）源码里没有 `jelly` / `bubble`；两者定义在 `Element.Builtin` 及其分组文件（`builtinSources`）里并已注册 |

改动了的旧测试（名字不变）：7 个用例里的「关卡数 = 38」断言改为 40（`surprise_opens_to_special`、`chain_layer_decrement`、`freeze_layer_decrement`、`curtain_layer_decrement`、`steam_spreads_after_move`、`goal_carpet_counts`、`campaign_levels_batch_ok`）。

## 元素类验收（阶段 1 原型 → 阶段 2 迁移）

`test/Spec/ElementClass.hs`；框架见 [architecture.md](architecture.md#元素类阶段-1-原型--阶段-2-迁移)。样例元素鸟窝 / 磁铁 / 星星只定义在该模块里。

**元素查询快照**（`test/golden/element-queries.txt`，1648 行，生成器 `test/golden/ElementQueries.hs`）：阶段 2 删掉旧记录后，新旧两条路径不能再在同一进程里并排比对，所以在阶段 1（`9ae6a7b`）的代码上先生成快照（用阶段 1 的旧记录注册表生成的结果与之逐字相同，旧快照存档 md5 相同），阶段 2 一字不改地比对。行前缀：`Q` 逐格查询（全部格子组合 × 各 `*With` 查询）、`P` 放置、`R` 规则表 / 条目名 / 个数差计数 / 关卡级元素清单、`A` / `E` / `S` / `O` / `C` / `G` 邻格 / 步末 / 成对交换 / 开启规则、直接命中、地面层在样例盘上的输出、`M` 40 关 × 种子 1–2 × 12 手（提示、锤子、十字、交换）的逐手散列。维护纪律同金标准：内部表示变了只改投影，快照文件不许改。

| 用例 | 断言 |
|------|------|
| `ec_queries_match_stage1_snapshot` | 快照 Q / P / R 行逐行相等（取代阶段 1 的 `ec_bridge_fields_match_legacy`） |
| `ec_rules_match_stage1_snapshot` | 快照 A / E / S / O / C / G 行逐行相等（取代阶段 1 的 `ec_bridge_rules_match_legacy`） |
| `ec_play_matches_stage1_snapshot` | 快照 M 行逐行相等（取代阶段 1 的 `ec_registry_paths_agree_in_play`） |
| `ec_some_element_eq_show` | `SomeElement` 的 Eq（第 6b 刀起按具体类型 + 该类型的 Eq，不再比名字字符串）、Show 稳定；宝石全用默认方法（原型 `Piece`、颜色取自写回的格子）；彩虹 `hintable = False`；注册表把格子解码成（带冰的）元素值 |
| `ec_some_element_eq_by_type` | 第 6b 刀：两个同名（`"twin"`）、写回同一个格子的测试元素类型装箱后不相等；同类型同状态相等、不同状态不等；`SomeModifier` 同理；名字是 `ElementName`（Show 与 String 相同） |
| `ec_ice_modifier_composes` | 冰层修饰器：写回格子带冰层数、匹配色透过、名字取本体、多冰不点火 / 末层点火、命中削一层 / 末层随宝石碎、洗牌保留；与注册表 `directHitWith` 逐格一致 |
| `ec_state_lives_in_element_value` | 测试专用「鸟窝」：剩余命中数在元素值里，锤子每敲一次减一、最后一下才碎并计数；放置经构造器 |
| `ec_open_messages` | 开放消息：自定义消息被处理 / 不认识的原样返回 / 修饰器把消息转给里面 / `fromMessage` 按类型认领 |
| `ec_flat_record_removed` | 源码扫描（去掉注释与字符串）：`Element/`、`Board/`、`Game/` 全部模块里没有 `ElementDef` / `baseDef` / `LevelHook` / `Hook*`；`Board.Match` 不点名彩虹、不 import `Match3.Rainbow`；结算流水线（`pipelineSources`）不调关卡级元素的实现 `stepUfos` / `beltMoves` / `coverCarpets`；内置条目 31 个、关卡级元素 ufo / belt / portal / carpet |
| `ec_level_elements_by_message` | 测试专用「磁铁」只经 `registerLevel` 在补子节拍（`Refilled`）多吸走一颗宝石（第 7b 刀起保留内置飞碟，回复折叠）；新消息 `Ping` 经 `askLevels` 折叠所有回复者（磁铁 +1、倍增器 ×2，按注册顺序：16 / 15），内置表下没人回复 |
| `ec_custom_matchable_gem` | 测试专用「星星」（`Custom "star"`，原型 `Piece`、带颜色）：可交换、与同色宝石成三连被清除并按名字计数、进提示；未注册时是惰性占格 |

元素类迁移的等价性依据：上述快照全等；金标准 2534 行全等；showcase / l28 / l1map 三场景截图与 `25db84a` 基线 AE=0。

## 多游戏接口验收（第三刀）

接口见 [architecture.md](architecture.md#多游戏接口)。

| 用例 | 断言 |
|------|------|
| `engine_toy_counter_game` | 玩具实现 `test/Toy.hs`（一维计数器，**只 import `Engine.*`**）：种子决定目标；`runActions` 遇到胜局即停（其后动作不执行）；非法 `Inc 9` 被拒、状态不变、没有事件；超出目标 / 步数用完判负；结局后 `gameActions` 为空、`gameStep` 拒绝；`gameStatus`；效果按节拍排成两个提示（6 帧 / 20 帧），通用播放器总帧数 26、只在进入第二个提示时触发其事件，加速后 9 帧，3 帧后进度 0.5；自定义阶段机（倒数 3 → 0）事件与帧数 |
| `engine_layer_is_game_agnostic` | `src/Engine/` 与 `app/Shell/` 下全部模块（按目录列出）和 `test/Toy.hs` 都不 import `Match3` / `Match3.*`（`importsOf`，依赖方向单向） |
| `engine_match3_instance_matches_direct_api` | 4 关 × 2 种子：`gameNew` = `newGameAtLevel`；前 3 个候选交换经 `gameStep` 的状态 / 结局 / 事件与 `trySwap` / `traceEvents (traceSwap …)` 逐位相同，`toEffect` 不丢事件、score 效果之和 = 得分增量，`play` 的 `MoveFx` / `Outcome` 与 `moveFx` 相同；经 `match3Shell` 走同一步、历史深度 1，`Undo` 回到走步前快照（清本步特效 / 提示 / 洗牌标记）；锤子 / 十字 = `useHammer` / `useCrossClear`；非相邻交换被拒且无事件；开局撤销（`match3Shell` 无历史）被拒；洗牌 = `shuffleGame` 且只有一个 shuffle 效果；提示 = `applyHint`；`gameStatus` 的 combo = `gsCombo`；只剩 1 步时 `runActions` 在第一步之后停下、终局后没有候选动作且拒绝一切 |
| `engine_undo_after_terminal_matches_legacy_play` | 段 3：6 个场景（第 1 / 7 / 13 / 28 关判负，第 5 / 21 关把目标改成 1 分后过关），经 `match3Shell` 的 `gameStep` 按 `findHint` 走到终局，再连撤三次：终局后第一次撤销被接受、清掉终局标记；终局态与三次撤销后的状态投影（与 `13094d1` 共有的全部字段 + 历史深度，FNV-1a）及是否被接受，与 `13094d1` 上直接调 `Match3.Engine.play Undo`（当时的 `gsHistory`）逐位相同。期望值由 `13094d1` 上同一段投影程序生成后写进测试 |
| `engine_frontend_steps_only_via_gameStep` | 段 3：递归扫描 `app/` 下全部 `.hs`（去掉注释与字符串），没有标识符 `play` / `playWith` / `undoMove`（含限定名 `M3E.play`）；且前端确有 `gameStep M3E.match3Shell` 调用 |

## 编写约定

- 固定 `StdGen` / 手工构造 `Board`，避免 flaky。
- 改规则必须同步更新或新增用例；禁止「只改文档宣称」。
- 不修改测试来迁就错误实现；先修规则或先补回归再合。

## 与 CI 的关系

仓库可能另有工作流配置；**本地以 `stack test` 全绿（当前 298，含金标准与元素查询快照比对）为合并门禁**。本文不依赖未跟踪的 `.github/` 内容。

门禁细则（第三刀起）：

- 每个提交都要 `stack test` 全绿、`golden_behaviour_snapshot` 逐行相等（`golden.txt` 不许改）。
- 通用层 `Engine.*` / `Shell.Loop` 不得 import `Match3`（`engine_layer_is_game_agnostic`）；三消实例与直接调用旧入口逐位相同（`engine_match3_instance_matches_direct_api`）。
- 动到前端绘制 / 回放时，另做截图比对：新旧二进制在 Xvfb 下的 6 个场景（展示盘、L16、L28 @2x、L5 暂停、L1 地图、L36 提示），贴图与几何降级两种模式都要能找到 `compare -metric AE` = 0 的帧配对（呼吸光随时钟变化，所以连拍找配对）。
- 元组兼容层（`runCascade*` / `resolveCountdowns` / `runPostBeltCascade` / `traceCascade*` / `toTuple*`）与门面 `Match3.Board` / `Match3.Game` 已删除；测试与 Golden 直接用 `CascadeRun` 记录和子模块。
