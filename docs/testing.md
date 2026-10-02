# 测试

## 如何运行

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
cd /path/to/match3
stack test
```

- 库测 **不需要** 显示器或 SDL 运行库参与链接执行路径上的窗口。
- 期望：**389** 个命名用例通过（Tasty：`testCase` + `testProperty`）：原有 262 个 + 第 1 刀新增 8 条 QuickCheck 性质与 1 个扫描工具自测 + 第 2 刀新增 2 个（`cell_accessors_total`、`ec_registry_checked_slots`） + 第 3 刀新增 1 条性质（`qc_find_hint_local_matches_reference`） + 第 4 刀新增 2 条性质（`qc_counts_algebra`、`qc_counts_monotone_legacy_view`） + 第 5 刀新增 3 条性质（`qc_goal_matches_legacy`、`qc_goal_progress_laws`、`qc_goal_progress_bounded`） + 第 6a 刀新增 9 个（`test/Spec/Levels.hs`：7 个单元测试 + 2 条性质） + 第 6b 刀新增 2 个（`ec_some_element_eq_by_type`、性质 `qc_name_newtypes_show_ord`） + 第 7a 刀新增 4 个（`ec_level_element_stateful_extension`、`br_board_takes_hooks_only`、性质 `qc_level_hooks_match_legacy` / `qc_level_elems_readers_roundtrip`） + 第 7b 刀新增 4 个（`br_end_phase_table_order`、`ext_end_effect_generic_hopper`、性质 `qc_end_table_matches_legacy` / `qc_ask_levels_folds_in_order`） + 第 8 刀新增 9 个（`br_rule_tables_out_of_main_flow`、`ext_shape_rule_lt_bomb` / `ext_combo_rule_line_gem` / `ext_refill_policy_level_element` / `ext_refill_policy_level_colors`、性质 `qc_shape_table_matches_legacy` / `qc_combo_table_matches_legacy` / `qc_combo_table_symmetric` / `qc_refill_policy_default_matches_legacy`） + 第 9 刀新增 5 个（`test/Spec/Caps.hs`：`caps_element_class_is_thin`、`ext_caps_element_plugs_in`、性质 `qc_caps_match_legacy_elements` / `qc_caps_rules_match_legacy` / `qc_default_caps_match_legacy_defaults`） + 第 10 刀新增 10 个（`test/Spec/Presentation.hs`：前端表现表与音效钩子，见下文「前端表现表验收」） + 第 11 刀新增 9 个（`test/Spec/View.hs`：视图模型与通用网格组件，见下文「视图模型验收」） + 新玩法 1 新增 6 个（`test/Spec/BombShapes.hs`：L / T 形出炸弹，见下文「新玩法验收」） + 新玩法 2 新增 6 个（`test/Spec/MagicStone.hs`：魔法石） + 新玩法 3 新增 7 个（`test/Spec/Fuzzball.hs`：毛球） + 新玩法 4 新增 7 个（`test/Spec/RainbowCombos.hs`：魔力鸟组合增强） + 测试辅助修正新增 1 个（`find_match_pair_engine_accepts`，见下文「测试辅助 findMatchPair」） + 新玩法 5 新增 7 个（`test/Spec/SnowBoss.hs`：雪怪 Boss） + 新玩法 6 新增 7 个（`test/Spec/CookieDrop.hs`：饼干掉落口） + 新玩法 7 新增 8 个（`test/Spec/Chameleon.hs`：变色龙） + 合 main 9f5504e（目标中文标签）后新增 1 个（`outcome_lose_hint_no_internal_names`） + 新玩法 8 新增 8 个（`test/Spec/MagicGround.hs`：魔法地格）。
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
- 目录（用例数合计 389）：

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
| `test/Spec/GoalsLevels.hs` | 32 | 目标、结局、星级、关卡表、每日、地图与步数结转；`find_match_pair_engine_accepts`（测试辅助 `findMatchPair` 选出的对引擎必须接受） |
| `test/Spec/Levels.hs` | 9 | 第 6a 刀：关卡记录与关卡表——全部内置关卡与每日挑战（两年每天，覆盖 10 种目标）的放置表都是 `Right`、`placeWith` 的 `UnknownElement` / `PlaceOutOfBounds`、坏放置表的报错带关卡名、`campaignGame` 与 `newGameAtLevel … (levelConfig …)` 相同、越界的重开 / 下一关夹到范围内、`allLevels !!` 源码扫描（src / app / web/hs / test），性质 `qc_lookup_level_in_range` / `qc_clamp_level_index_found` |
| `test/Spec/Element.hs` | 2 | 元素注册表（测试专用木箱 `Crate` / 条目 `crateDef` 在 Support 里） |
| `test/Spec/Extension.hs` | 11 | 段 2c 扩展钩子护栏：Board 层收注册表（源码扫描）、`GoalNamed`、地面层、边缘收集、步末补结算、经 Engine 的手动洗牌；第 7b 刀通用步末效果（跳跳虫）；第 8 刀扩展一条形状规则（L / T → 炸弹）、一条组合规则（直线 × 普通宝石）、两种补子策略（关卡级元素「金币雨」、关卡颜色数 `colorsRefill 3`）（样例元素苔藓 / 风筝 / 陷坑 / 浮尘 / 跳跳虫 / 金币雨与样例规则只定义在该模块里） |
| `test/Spec/Branches.hs` | 11 | 段 4 专门分支收编护栏：测试专用成对交换规则（拉杆）/ 开启规则（豆荚）/ 可推动（小车）只经注册表生效；内置改色 / 推动谓词与原写死谓词相同；关卡级元素（飞碟 / 皮带 / 传送门 / 地毯 / 地面层）的节拍回复与原实现相同、去掉后不生效（含 38 关实测；第 7a 刀起经钩子与 `Element.Level` 的节拍函数）；主流程源码扫描；第 7a 刀 `br_board_takes_hooks_only`（Board 核心只收钩子、流水线不读旧的五个字段、删掉的名字不再出现）；第 7b 刀 `br_end_phase_table_order`（步末表的内容与顺序、按表执行、删掉的步末构造器不再出现）；第 8 刀 `br_rule_tables_out_of_main_flow`（流水线不点名特殊块种类、不直接随机选色、不用组合几何；特殊合成不再挂在元素上） |
| `test/Spec/ElementClass.hs` | 13 | 元素类（阶段 1 原型 → 阶段 2 迁移）：逐格查询 / 规则 / 逐手结果与阶段 1 的元素查询快照全等；`SomeElement` 的 Eq / Show（第 6b 刀：按类型比较，同名不同类型不等）、冰层修饰器组合、状态在元素值里、开放消息；扁平记录已删（源码扫描）、关卡级元素经消息、第 7a 刀带状态的扩展关卡级元素「虹吸」不改主流程接入（`levelStart` 开局、状态写回 `gsLevelElems`、去掉注册后原样不生效、`Show` 追加 `gsLevelExtra`；第 7b 刀起保留内置飞碟，同一 `Refilled` 节拍两者都吸收）、自定义可匹配宝石；注册表槽位由原型推导、`mkRegistryChecked` 报重名 / 槽位冲突 / 推不出槽位 |
| `test/Spec/JellyBubble.hs` | 8 | 段 5 双层果冻 / 气泡：按层计数与目标、洗牌 / 撤销、气泡邻消 / 直接命中即破、挡交换 / 无色 / 下落、第 39 / 40 关、主流程源码扫描 |
| `test/Spec/Engine.hs` | 5 | 多游戏通用接口（玩具 `test/Toy.hs`、依赖方向扫描、三消实例）；段 3：终局后撤销与 `13094d1` 比对、前端只经 `gameStep`（源码扫描） |
| `test/Spec/UIEvents.hs` | 8 | 前端反馈（MoveFx / 连击反馈 / 清除格）与效果事件 |
| `test/Spec/ReplayUndo.hs` | 17 | 回放脚本 `trace_*`、撤销、洗牌 |
| `test/Spec/Golden.hs` | 1 | `golden_behaviour_snapshot`（调 `test/golden/Golden.hs`；第 5 项起把 `Golden.goldenSections` 各段经 `Spec.Support.Parallel.parallelForce` 多核求值、按原顺序拼回再逐行比对） |
| `test/Spec/Properties.hs` | 24 | QuickCheck 性质（原有 1 条 + 第 1 刀 8 条 + 第 3 刀提示局部检查对照旧实现 1 条 + 第 4 刀计数 2 条 + 第 5 刀目标 3 条 + 第 6b 刀名字 newtype 1 条 + 第 7a 刀关卡级钩子 / 读数 2 条 + 第 7b 刀步末表 / 折叠回复 2 条 + 第 8 刀形状表 / 组合表 / 组合对称 / 补子策略 4 条，见「性质测试」） |
| `test/Spec/Caps.hs` | 5 | 第 9 刀：能力记录 Caps——内置元素（含冰 / 叠层组合）逐项查询与规则输出对照第 9 刀前的类（`Spec.Support.LegacyElement`）、缺省能力等价旧缺省方法、元素类只剩三个方法（源码扫描）、用 Caps 写的扩展元素「荆棘」不改主流程接入（见「能力记录验收」） |
| `test/Spec/Presentation.hs` | 10 | 第 10 刀：前端表现表 `UI.Presentation` 与音效钩子 `UI.Sound`（`app/pure`，测试直接编译）——每种事件恰一行、帧数 / 颜色 / 贴图 / 生长曲线 / 连击样式对照第 10 刀前各处 case 的字面副本、扩展元素缺省表现、音效全为 `Nothing`、真实连锁全程无声、源码扫描（散落的表与颜色已收掉、`drawHud` / `primOverlay` 只剩分派） |
| `test/Spec/BombShapes.hs` | 6 | 新玩法 1：L / T 形出炸弹（规则开关 `bomb_shapes`）——只有第 41 关打开、原有 40 关 / 每日挑战 / 自由开局的形状表等于内置表、插表顺序、L 形交点出炸弹（第 1 关与去掉开关时是空洞）、五连仍出彩虹、带四连的 L 出炸弹不出直线、第 41 关实战会生成炸弹 |
| `test/Spec/MagicStone.hs` | 6 | 新玩法 2：魔法石——能力（固定 / 挡交换 / 无色 / 洗牌保留 / 平时打不动、发射中命中归零）、邻格充能每轮 1 格且满 3 为止、满格在交换步末发射清整行整列并归零、不满不发射、道具不触发而下一次交换发射、第 42 关布局与实战（魔法石不动、发射过、石头有进度） |
| `test/Spec/Fuzzball.hs` | 7 | 新玩法 3：毛球——能力（挡交换 / 下落 / 无色 / 洗牌保留 / 命中即灭）、邻格真消除即灭（斜角不算、直接命中不重复）、跳到相邻普通宝石并换位且结果确定、墙 / 避让格 / 非普通宝石 / 已占格不跳、交换步末的 `EvBelt "fuzzball"` 可重放、去掉毛球条目后前 42 关逐步相同（不耗 `gsGen`）、第 43 关布局与实战（跳过格、有消灭计数、能过关） |
| `test/Spec/RainbowCombos.hs` | 7 | 新玩法 4：魔力鸟组合增强（规则开关 `rainbow_combos`）——只有第 44 关打开且 Show 不打印开关、成立条件（彩虹 × 直线 / 炸弹，两端能点火）、彩虹 × 直线先变身（第 0 轮之前的 `EvSpread rainbow_line`，横竖棋盘格交替）再在第一轮全部引爆、彩虹 × 炸弹同理、带冰 / 叠层的同色宝石不变身但仍是种子、原有 43 关同一局面与去掉开关时逐项相同、第 44 关布局与实战 |
| `test/Spec/SnowBoss.hs` | 7 | 新玩法 5：雪怪 Boss（2×2）——能力（固定 / 挡交换 / 不下落 / 无色 / 洗牌保留 / 直接命中原样吃掉、左上格按血量计权）、放置与第 45 关开局 / HUD 血条读数、身外一圈真消除与直接命中扣血且归零四格一起清除、锤子扣血与击败过关、交换步末的 `EvTick "snow_boss"` 与每 3 步确定地召唤雪块、去掉雪怪条目后前 44 关逐步相同、第 45 关实战（见下文「新玩法验收」） |
| `test/Spec/CookieDrop.hs` | 7 | 新玩法 6：饼干掉落口（关卡级元素 `CookieDrop`，关卡记录 `lvlDrops`）——只有第 46 / 47 关有掉落口、Show 不打印、视图读数；`dropRefill` 在掉落口格补饼干的条件与名额、随机数消耗与原策略相同；第 46 关开局不做目标补齐；实战掉落与收集；去掉条目后前 45 关与每日挑战逐步相同；难度（按提示 30 局赢 ≤ 25、贪心能过关）（见下文「新玩法验收」） |
| `test/Spec/Chameleon.hs` | 8 | 新玩法 7：变色龙（`Custom "chameleon" k`，k = 当前颜色）——能力与放置；按当前颜色匹配（`findMatchPair` / 提示 / 引擎一致）；换色的固定顺序与「不立刻连成三消」的顺延；玩家交换步末换色（`EvTick "chameleon"`，可重放）、道具不换色；彩虹 × 变色龙；掉落口按名字数同种；前 46 关与每日挑战不变；第 47 关布局与难度 |
| `test/Spec/MagicGround.hs` | 8 | 新玩法 8：魔法地格（地面层 `"magic"`）——不被消耗、不计数、带扩爆规则；`magicWiden` 几何（直线 → 三行、炸弹 3×3 → 5×5、盘边截断）；本步扩爆格只在第 48 关、只看引爆格；交换 / 锤子实战（`EvBlast` 覆盖格、底行碎石削层，与去掉条目对照）；成对规则的种子不扩、种子里的特效照常按引爆格扩；前 47 关与每日挑战没有扩爆格、逐步相同；第 48 关布局与难度 |
| `test/Spec/Phase.hs` | 4 | Haskell 特性第 1 项（类型层）：带阶段标签的一轮（消除 → 下落 → 补子）与无标签函数逐盘面 / 逐生成器相同（40 个随机盘）、`swapStage` = `swapCells`、类型化入口 `resolveMove SKindHammer …` = `resolveHammer`；7 个「编译不过」的写法（没下落就补子、下落两次、有空洞再消、有空洞当满盘、交换两次、锤子 + 普通匹配起手、锤子拿交换后的盘起手）在本模块里用 `-fdefer-type-errors` 推迟成运行时 `TypeError`，断言确实抛出且消息点名冲突的阶段（见 [haskell-features/01-类型层.md](haskell-features/01-类型层.md) §4.1） |
| `test/Spec/Classes.hs` | 8 | Haskell 特性第 2 项（类型类与抽象）：`deriving newtype` 的 `ElementName` / `CustomState` 的 Show / IsString 与底层逐字相同；六个 Int newtype 元素的缺省 `toCell`（DefaultSignatures）与改前手写编码相同、本体经注册表解码回同一值；表示不是 `Int` 的元素不写 `toCell` 是类型错误（`-fdefer-type-errors` 推迟成 `TypeError`，断言消息点名 representation 不匹配）；三个装箱类型按类型相等；`Grid` 的 Functor / Foldable / Traversable 定律、行主序、形状不变；`randomBoardSized`（`mapAccumL`）与改前递归逐种子 / 逐尺寸相同（含生成器状态）；盘面统计（`foldMap Sum`）与改前列表推导相同（全部战役关卡开局盘）；`foldMap` 合计数与改前 `foldl` + `bumpCount` 相同（见 [haskell-features/02-类型类与抽象.md](haskell-features/02-类型类与抽象.md)） |
| `test/Spec/Effects.hs` | 3 | Haskell 特性第 3 项（效果与架构）：全部战役关卡 × 2 种子 × 3 个盘面（能消的交换 / 同尺寸随机盘 / 开局静止盘）、关卡级元素按该关接上，七个连锁入口（匹配、带起始波次的匹配、种子、空种子、皮带后、步末后带空洞、倒计时）与单轮：对外入口（纯解释器）与改写前的逐字副本 `Spec.Support.LegacyCascade` 逐项相同（终盘、计数、回放、推进后的关卡级元素、生成器 `show`，倒计时另比步末记录）；追踪解释器 = 纯解释器 = 旧写法、日志里的轮次就是回放、补进的格子就是该轮终盘的格子，并确认用例走到了吸收、补子、只沉降三种节拍；手写 free monad 指令树 + 小解释器跑同一个程序也与旧写法相同（见 [haskell-features/03-效果与架构.md](haskell-features/03-效果与架构.md)） |
| `test/Spec/Lazy.hs` | 5 | Haskell 特性第 4 项（惰性与递归模式）：7 种行列 × 200 种子的无三连 / 可玩盘拒绝采样与旧手写递归逐项相同（盘面与生成器，内外两层重抽都走到）；49 关 × 2 种子 × 3 种盘面（开局 / 斜纹死盘 / 怎么洗都死的石头盘）+ 已终局的自动洗牌与旧计数循环整个 `GameState` 相等（不洗 / 洗到可走 / 用第 25 次三条路都走到）；`Engine.Stream` 真的惰性（`error` 占位）；`runPlayer` = 旧写法 = 手写 hylo = cata . ana（倒数阶段机、提示队列、每关真实连锁回放，正常与加速），hylo 能从无穷回放里惰性取事件；`countsFromList` 与 `foldl` 版相等、`runActions` 吃无穷动作表（见 [haskell-features/04-惰性与递归模式.md](haskell-features/04-惰性与递归模式.md)） |
| `test/Spec/Perf.hs` | 5 | Haskell 特性第 5 项（性能与并发）：一万多张真实盘面（全部关卡 × 种子 1–3 的开局盘、全部相邻交换、沿提示走 12 手）+ 1000 个随机盘上匹配码版 `findMatchRunsWith` / `hasAnyMatchWith` / `findHintWith` 与旧写法逐项相同、码与逐格 `matchColorWith` 一致；588 个挖空盘 + 1000 个随机盘上 ST 版重力与旧列表版相同（含固定格分段）；`parallelForce` 在 1 / 2 / 3 / 8 / 1000 个工人下与串行逐项相同、空表、按顺序报第一个出错的任务（见 [haskell-features/05-性能与并发.md](haskell-features/05-性能与并发.md)） |
| `test/Spec/View.hs` | 10 | 第 11 刀：视图模型 `Match3.View` 与通用网格组件 `Engine.GridUI`——整局 / 目标 / 棋盘读数、窗口标题、收集进度后缀、地毯标记、进度点、分数徽章、单格描述、关卡列表对照第 11 刀前各前端现算式的字面副本；网格几何对照旧 `pixelToCell` / `cellOrigin`、点选 / 拖动 / 高亮；源码扫描（前端不再从 `GameState` 现算） |
| `test/Spec/SourceScan.hs` | 1 | 源码扫描工具自测 `support_source_scanner`（注释剥离、import 解析、标识符匹配） |
| `test/Spec/Support.hs` | — | 多个模块共用的辅助：`allPos` / `setCells` / `customsOn` / `isCustomNamed`、`tripleBoard` / `tripleMove`（第 1 行 C5 四连局面）、`isWin`、`firstLevel`、`levelAt` / `levelGame`（第 6 刀：按下标取关 / 开局，没有这一关时报错，取代测试里的 `allLevels !! i`）、`firstWave`（没有连锁轮时断言失败，代替 `head . mtWaves`）、`stepThenUndo`（经 `match3ShellWith reg` 走一步再 `Undo`，段 3）、`findMatchPair` / `findNoMatchPair` / `stuckNoMoveBoard` / `stableBoard`、连击反馈局面、回放逐轮检查、事件细节检查、测试专用木箱 `Crate`（条目 `crateDef`）等；并重新导出 `Spec.Support.Source` |
| `test/Spec/Support/LegacyCascade.hs` | — | 第 3 项前 `Match3.Board.Cascade` 连锁核心的逐字副本（生成器 / 钩子手工传递），供 `Spec.Effects` 对照 |
| `test/Spec/Support/LegacyLazy.hs` | — | 第 4 项前四处写法的逐字副本（`Board.Random` 的两个拒绝采样、`Game.Shuffle.ensurePlayableWith` 的计数循环、`Engine.Playback.runPlayer`、`Counts.countsFromList` 的 `foldl`），供 `Spec.Lazy` 对照 |
| `test/Spec/Support/LegacyPerf.hs` | — | 第 5 项前匹配扫描（`findMatchRunsWith` / `hasAnyMatchWith` / `findHintWith`，含 `groupGemRunsWith` / `lineHasRunWith`）与 `applyGravityWith` 列表版的逐字副本，供 `Spec.Perf` 对照 |
| `test/Spec/Support/Parallel.hs` | — | 确定性并行批量求值 `parallelForce`（`forkIO` + STM：`TVar` 领任务、`TMVar` 结果槽、按原顺序取回、异常按顺序重抛）；`Spec.Golden` 用它并行求值金标准各段 |
| `test/Spec/Support/Source.hs` | — | 源码扫描工具（见「源码扫描约定」） |
| `test/Spec/Support/LegacyElement.hs` | — | 第 9 刀前的 `Element` 类（27 个方法）与 19 个内置本体 instance 的逐字副本（改名 `LElement` / `LSome` / `LHit`，孤儿 instance 挂在 src 的元素类型上），只给 `Spec.Caps` 对照用 |
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
| `qc_shape_table_matches_legacy` | 1000 | 第 8 刀：三色为主的随机盘（常有 4 / 5 连与横竖交叉）× 随机落点 × 连线格的随机子集当可清格：`spawnSpecialsWith defaultRegistry` 与 `spawnByShapes builtinShapeRules` 的产出（位置、种类、先后）与测试里逐字保留的第 8 刀前 `spawnSpecials` 相同；内置表名字依次为 line5→rainbow / line4h→line_h / line4v→line_v；空表不生成（`checkCoverage`：≥20% 用例生成特殊块，≥3% 生成两个以上） |
| `qc_combo_table_matches_legacy` | 3000 | 第 8 刀：随机盘上一对（多半相邻的）交换端，端点是任意种类 / 冰 / 叠层（含软锁）的宝石或障碍：组合表的成立判定（交换前盘）与清种子（交换前、交换后两个盘）与逐字保留的旧 `isSpecialCombo` / `comboClearSeeds` 相同；并进成对交换规则后次序 `[10, 20]`（元素声明的只剩彩虹 10），`swapOpeningWith` / `swapFiresWith` 与旧的两条规则相同（`checkCoverage`：≥5% 组合成立，≥5% 种类对得上但被软锁挡住） |
| `qc_combo_table_symmetric` | 3000 | 第 8 刀：同上的用例，交换两端（p1 ↔ p2）后每条组合规则是否对得上、整张表是否成立不变，清种子的格集合（交换前 / 后两个盘）不变 |
| `qc_refill_policy_default_matches_legacy` | 500 | 第 8 刀：任意可空盘 × 种子：`refillWith defaultRefill`、`refillWith (colorsRefill numColors)`、`Gravity.refill`、`activeRefill` 在 `noHooks` / `builtinHooks` 下的策略与逐字保留的旧 `refill` 相同——盘面相同、推进后的生成器（`show`）相同，即随机数消费顺序不变；内置关卡级元素不换策略（名字 random-gem） |
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
| `trace_end_steps_replay_to_trySwap_final` | 全部关卡（段 5 起 40 关，新玩法起逐关追加）× 种子 1–3 的每个成功交换按「轮 → 步末 → 轮」时间线重放：每段首尾相接、`applyEndEffect esEffect esBefore == esAfter`，终盘等于 `trySwap`（未洗牌时）；抽样必须覆盖 tick / belt / SpreadVine / SpreadChoco / SpreadSteam / snail 六种效果 |
| `trace_end_steps_boosters_replay` | 道具只有蔓延类步末效果，重放后同样到达终盘 |
| `trace_end_snail_push_and_turn` | 蜗牛碰壁原地掉头（`smFrom == smTo`、朝向反转）；前方是宝石则爬过去、宝石换到原格 |
| `trace_shuffle_step_replays` | 洗牌步逐帧比对（见下） |
| `trace_end_spread_from_adjacent_source` | 巧克力 / 藤蔓的新占格都能在之前的盘面找到正交相邻的同类来源（前端据此决定从哪边「长出」） |

**底线**：`trace_swap_final_equals_trySwap` 要求「未洗牌、实际比对终盘的步数 > 400」，失败信息会打印比对数和因自动洗牌跳过的步数（当前样本约 517 比对 / 9 跳过），防止抽样悄悄缩水、测试名存实亡。

**洗牌步（第二刀补上）**：`MoveTrace` 新增 `mtGen`（洗牌前的生成器）与 `mtShuffle`（洗牌后的盘面）。`trace_shuffle_step_replays` 对全部关卡（段 5 起 40 关，新玩法起逐关追加）× 种子 1–3 × 开局全部成交交换（每条再沿首个成交交换走 2 手）逐帧比对：时间线重放到 `mtFinal`，再从 `(mtFinal, mtGen)` 重放 `ensurePlayable` 必须到达结算后的 `gsBoard` / `gsGen`；未洗牌的步要求 `mtFinal == gsBoard`、`mtGen == gsGen`。38 关时 **3801** 个成交步逐帧比对，其中 **29** 个自动洗牌步；底线 > 3000 / > 20。

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
- **耗时**：第三刀把 `Board` 换成数组（`getCell` O(1)）后，`golden_behaviour_snapshot` 在 box 上约 6.05 s → 4.3–4.5 s（单独运行 `-p golden_behaviour_snapshot`）。Haskell 特性第 5 项（unboxed 匹配码 + 各段并行求值）前后，单独运行约 10.1 s → 1.35 s（8 核；整套墙钟 12.3 s → 9–10 s，见 [haskell-features/05-性能与并发.md](haskell-features/05-性能与并发.md) §4）。
- **段 5 追加（第 2345–2534 行）**：关卡表在第 38 关之后追加了两关，原有各段（L / G 行）改为只跑前 `campaign38 = 38` 关，保证前 2344 行逐字不变（追加前后用 `head -2344` 与旧文件 `cmp` 相同）；新关卡的行在末尾：第 39 / 40 关 × 种子 {1,2} × 15 步（与 L 行同一走法），每步后多一行 `ext`（`gnd=` 地面层、`named=` 具名计数——旧的 `pState` 不含这两个字段，只在新行里补），再加 G39 / G40 开局行。
- **新玩法追加（2026-09-30 起，第 2535 行以后）**：段 5 改为固定跑第 39 / 40 关（`campaign40 = 40`），新玩法关卡**每关一整块**（逐步投影 + 开局）依次追加在文件末尾（`seg6Lines`，第 41 关起），再加新关时前面的行不动。追加第 41 关时还有 4 行变化（第 2490 / 2521 / 2526 / 2527 行，第 40 关种子 1 / 2 过关的那一步）：第 40 关不再是终章，过关结局由 `W<分>`（`Won`）变为 `C<分>>40`（`LevelClear`，进入第 41 关），同一行 / 辅助行里整局状态的散列随之变化；盘面、计数、得分都不变。元素查询快照同理：`R level` 行多了 `bomb_shapes`，`M 40 1` / `M 40 2` 两行因第 40 关过关结局变化而散列不同，末尾追加 `M 41 1` / `M 41 2`。
- **维护纪律**：内部表示变了只改投影函数，`golden.txt` 一个字都不动；确需重录（规则行为有意变化，机制刀停期间不应发生）用 `test/golden/regen.sh`，并在提交说明里写明原因。

## 元素框架验收

框架见 [architecture.md](architecture.md#元素框架与事件)。

| 用例 | 断言 |
|------|------|
| `element_registry_custom_crate_extensibility` | 测试专用元素「木箱」`Custom "crate" 耐久`（**只定义在测试里：`test/Spec/Support.hs` 的 `Crate` instance + `customEntry`**：`fixed [hit …, onAdjacent 200 …, counts …]`：固定格，不下落、邻格真消除波及耐久 −1、耐久 1 再被波及就碎、计数 `CountNamed "crate"`）经 `register` 接入后：注册表多一项、内置一个不少；对它交换返回 `NoMatch` 且盘面不变；无匹配色；第 1 手（邻格 C5 三消）耐久 2→1、原地不动、不计数、不在清除格、事件里有 `EvHit "crate"`；第 2 手（耐久 1）碎掉、进入第一轮清除格、`namedCounts (gsCounts gs) == [("crate",1)]`、事件里有 `EvClear "crate"`；锤子削到 1；洗牌保留；同一局面在 `defaultRegistry` 下它是惰性占格（不被波及、锤子免疫、不计数）；`src/` 与 `app/` 下全部源文件（含注释，按目录列出）里没有字面量 `"crate"` / 「木箱」——主流程没有为它改一行 |
| `element_registry_matches_legacy_predicates` | 全部内置本体 × 宝石种类 × 冰层 × 叠层：注册表的挡交换 / 锤子免疫 / 固定格 / 点火 / 匹配色 / 洗牌保留与第二刀之前按构造器写死的谓词逐格相等；直接命中的几条代表（冰、锁链、保险箱、翻转、石头）与旧口径一致 |
| `trace_events_consistent_with_trace` | 全部关卡（段 5 起 40 关，新玩法起逐关追加）× 种子 1–2 × 前 3 个成交交换：`EvScore` 之和 = 本步得分；`EvClear` 的格 = 各轮清除格并集；步末事件数 = `mtEnd` 长度；`EvShuffle` ⇔ `mtShuffle`；`EvBlast` 只来自直线 / 炸弹；**逐轮严格相等**（第三刀，前端波次界面改读事件后加）：该轮 EvClear 的格按事件顺序拼接 = `cwCleared`（顺序也相同），该轮 EvScore 之和 = `cwScore` |

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
| `ext_shape_rule_lt_bomb` | 第 8 刀：测试专用 L / T 形状规则插到内置形状表最前面（`setShapeRules`）：同色横竖三连交叉时横线在交点放炸弹、竖线认领不生成；内置表下同一 L 形不生成特殊块（交点是空洞），扩展后 `clearMatchesDetailedWith` 与正式交换（`resolveSwapWith`）第一轮的交点都是炸弹 |
| `ext_combo_rule_line_gem` | 第 8 刀：往内置组合表末尾加「直线 × 普通宝石 → 直线端整行整列」（`setComboRules`）：内置表下这一步不成三消、`NoMatch`；扩展后两个方向都成立、交换被接受，第一轮清掉直线端所在行与交换列 |
| `ext_refill_policy_level_element` | 第 8 刀：测试专用关卡级元素「金币雨」回复 `Refilling`，把补子策略换成每洞一枚金币（惰性占格、不耗随机数）：C5 四连一步只有一轮，挖出的 3 个洞 (0,0) (0,1) (0,3) 补成金币（横消落在交换点）；缺省策略下同一步没有金币 |
| `ext_refill_policy_level_colors` | 第 8 刀：注册表换成关卡颜色数策略 `colorsRefill 3`（`setRefillPolicy`）：空盘沉降补子只出 C1..C3；缺省策略五色都出 |
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
| `br_rule_tables_out_of_main_flow` | 第 8 刀源码扫描：Board 核心（Match / Clear / Cascade / Gravity / Hooks）与 Game 的 Move / Resolve / Boosters 代码里不点名 `LineH` / `LineV` / `Bomb` / `Rainbow`、不用 `randomColor` / `numColors`、不用 `bigBomb` / `fullRowCol` / `lineBombCross`；元素声明的成对交换规则只剩彩虹 10，组合表并成 20 |
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
| `jb_levels_appended` | 共 48 关（新玩法 8 起）；第 39 / 40 关目标为 `GoalNamed "jelly" 32` / `GoalNamed "bubble" 12`，种子 1–3 开局层数 / 气泡数等于目标且有可走步；前 38 关开局没有地面层、没有气泡 |
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

## 视图模型验收（第 11 刀）

`test/Spec/View.hs`；结构见 [architecture.md](architecture.md#视图模型第-11-刀)。对照对象是本文件里第 11 刀前 `UI.Actions.updateTitle`、`UI.Input.collectMsg`、`UI.HudArt` / `UI.HudBlocks`、`UI.BoardPrim` / `UI.BoardArt`、`web/hs/Match3Web/Api.hs`、`UI.Layout` 现算式的字面副本。样本局面：全部关卡 × 种子 3 / 11 各按 `findHint` 连走 4 步、2026-09-30 每日挑战走 3 步，外加越界关卡号、改过道具 / 洗牌标记、四种结局（含 `Just MoveApplied`）、步数超过印制值的局面。

| 用例 | 断言 |
|------|------|
| `view_fields_match_legacy_reads` | 每个样本：关卡号 / 夹紧下标 / 原下标关名（无此关 `"?"`）、步数上限 `max 步数 印制步数`、分数 / 步数 / 每日、三种道具、连击（经 `gameStatus` = `gsCombo`）、洗牌 / 结局、结局色条分支、目标进度 / 目标值 / kind / text / 名字 / 失败提示、`gsHint` 与 `findHint`、盘面、清除格 / 地面层 / 皮带 / 传送门 / 地毯 / 已铺地毯 / 飞碟、逐格地面层，与旧读法相同 |
| `view_title_and_bracket_match_legacy` | `titleLine` 与旧 `updateTitle` 的标题串（不含 `"  \|  "` 与消息）逐字相同；`goalBracket` 与旧 `collectMsg` 相同（合 main 9f5504e 后两份旧副本的目标标签同步换成 `goalLabel` 的中文名，数字与其余字段不变） |
| `view_carpet_marks_match_legacy` | 有地毯的关卡（含已铺开的局面）逐格：`carpetAt` 与几何版（带 `not null` 的旧式子）、贴图版旧式子都一致 |
| `view_level_dots_match_legacy` | 当前关 −1..levelCount+1 × 已解锁 6 种：`levelDots` 共 levelCount 个，与贴图版四档、几何版三档（传 −1）旧分支相同 |
| `view_score_badge_matches_legacy` | 回放有无 / 连击 0,1,2,5 / 显示分 × 总结剩余帧 × 最高连击 × 洗牌：`scoreBadge` 与贴图版右下角、几何版 `hudComboBadge` 的旧分支相同 |
| `view_cell_face_and_level_list_match_legacy` | 全部样本盘面的格子与每种构造器 / 叠层组合：`cellFace` 与旧 `encodeCell` 的字段（`t` 在前、顺序相同）逐项相同；`levelViews` 与 `allLevels` 的序号 / 名字 / 步数 / 目标相同 |
| `grid_ui_geometry_matches_legacy_layout` | 棋盘几何（16, 124, 56, 8×8）下 `gridCellAt` 与旧 `pixelToCell` 在两组扫描线（含边界 ±1 像素）上相同；`gridCellOrigin` 与旧 `cellOrigin` 相同且往返；`gridCells` 行优先；非正方网格行列不混；`orthoAdjacent` = `adjacent` |
| `grid_ui_click_drag_highlight` | `gridClick` 三种结果；`gridDragRelease adjacent` 与旧 `Just p2 \| p1 /= p2 && adjacent p1 p2` 在全部落点（含棋盘外）上相同；`Highlight` 的三个查询、`noHighlight` 为空 |
| `frontends_read_view_model` | 源码扫描：`web/hs/Match3Web/Api.hs` 不再读 `gsLevel` / `gsGoal` / `gsBoard` / `findHint` / `levelCarpets` / `allLevels` 等；HUD 三个模块不读道具字段 / `gsProgress` / `goalTarget` / `lookupLevel` / `levelCount`；`BoardPrim` / `BoardArt` 不读地毯 / 地面层 / 提示 / 皮带 / 传送门字段；标题只调 `titleLine`；`pixelToCell` / `cellOrigin` 经 `boardGrid`；点选 / 拖动经 `gridClick` / `gridDragRelease`；`UI.GoalStyle` 不再有文字标签表；规则开关角标：`UI.HudArt` 与 `Api.hs` 都读 `ruleBadges`（HudArt 不点名 `"bomb_shapes"` / `"zh_rule_bomb"`）、关卡表里出现的每个规则开关都在 `ruleBadgeTable` 登记、第 41 关角标 = `bomb_shapes`「L/T 形出炸弹」、第 1 关无角标、没登记的规则退回规则名、角标文字与 `tools/gen_assets.py` 的 ZH 表字面相同、图标是 gen_assets.py 生成的贴图；目标中文显示名：`Api.hs` 读 `goalLabel`（网页 `goal.label`）、全部关卡与每日挑战（2026 年每月 1–28 日）的目标标签不含 `[a-z_]`、第 43 关 =「毛球」、第 45 关 =「雪怪」、第 46 关 =「饼干」、没登记的名字目标退回元素名 |
| `outcome_lose_hint_no_internal_names` | 合 main 9f5504e 后：全部关卡与每日挑战（2026 年 12 × 28 天）的失败提示 `loseHint`（及视图字段 `giLoseHint`）、桌面标题目标段 `goalLine`、提示后缀 `goalBracket` 都不含 `[a-z_]`（不露出元素内部名，也不再有 `score` / `stone` 等英文标签）；第 39 / 40 / 43 / 45 / 47 关（果冻 / 气泡 / 毛球 / 雪怪 Boss / 变色龙）的失败提示与第 47 关标题段逐字核对；碎石目标（测试跑手报告第 8 / 41 / 42 / 44 关写成「砸箱子」，2026-09-30 改为读 `countLabel CountStones`）：第 8 / 48 关逐字核对「用邻消或特效砸开碎石，目标 8 个」，全部关卡与每日挑战里的碎石目标失败提示都含 `goalLabel` 的「碎石」、不含「箱子」；`Outcome.hs` 读共用的 `countLabel` |

画面等价性依据：按 yu 的精简验收，第 11 刀只做编译 0 警告 + `stack test` 全过（含上表与金标准 / 元素查询快照）+ `make check`（改了 `web/hs`，网页 JSON 对照在里面），桌面版拍 1 张图肉眼确认。

## 前端表现表验收（第 10 刀）

`test/Spec/Presentation.hs`；表结构见 [architecture.md](architecture.md#前端表现表第-10-刀)。测试套件的 `source-dirs` 含 `app/pure`，直接编译 `ComboFx` / `UI.Presentation` / `UI.Sound`（不依赖 SDL）。对照对象是本文件里第 10 刀前各处 case 与常量的字面副本。

| 用例 | 断言 |
|------|------|
| `presentation_table_covers_every_event_kind` | 表的键 = `[minBound .. maxBound] :: [EventKind]`（每种恰一行、按定义顺序）；每个 `StageKind` 恰被一行使用；帧数非负 |
| `presentation_stage_rows_match_legacy_end_stage_table` | 全部事件种类的 `stageKindOf` / `ComboFx.stageKindFor`、全部段的 `stageFrames` / `endStageBase` 与旧 `endStageTable`（含缺省：非步末种类 → 蔓延段、缺省 18 帧）相同 |
| `presentation_frames_colors_match_legacy_constants` | 高亮 12 / 得分浮字 48 / 连击弹字 54 帧；`clearTint`（旧 `waveTint`）、`scorePopRGB`（旧 HudArt / HudPrim 的分支）在各等级与相位下相同；倒计时 / 洗牌主色与贴图、蔓延 / 蜗牛贴图、碎屑方式（旧 `endCrumbTable`）、`elementRGBTable`、缓动逐项相同 |
| `presentation_spread_curves_match_legacy` | vine / choco / steam / 未知名字 / 空名 × 106 个采样点（含区间外）：`curveAt (spreadCurveFor n) t` 与旧 `spreadProgress`（缺省 `t`）逐位相等 |
| `presentation_combo_style_matches_legacy` | 连击等级 −1..9 × 相位 0..80、399、4000：`comboStyle` 四个字段与 `styleRGB` 与旧定义（含 `hsv`）相同 |
| `presentation_extension_defaults` | `defaultPresentation` = 蔓延段 18 帧、无颜色 / 贴图 / 碎屑 / 音效；空表查询全部落到缺省；未知元素名匀速、白光、不迸碎屑；`EvMove`（扩展 `hopper`）→ 蜗牛段 |
| `effect_sound_defaults_to_nothing` | 每种事件 `effectSound` = `Nothing`、每行 `prSound` = `Nothing`；`playSounds` 是空操作 |
| `cascade_sounds_silent_on_real_moves` | 第 8 关 7 轮连锁与第 16 关倒计时一步完整回放：阶段事件覆盖 EvClear / EvScore / EvCombo / EvTick，`cascadeSounds` 全为空 |
| `presentation_scattered_cases_removed` | 源码扫描 `app/`：除 `UI.Presentation` 外不再定义 `endStageTable` / `spreadProgress` / `endCrumbTable` / `elementRGBTable` / `comboStyle` / `styleRGB` / `hsv` / `smoothT` / `easeOutT`，也不再出现搬走的颜色字面量与 `"zh_combo"`；`UI.Presentation` 不 import SDL；EndStage / Playback 确实读表 |
| `draw_hud_and_prim_overlay_are_thin` | `drawHud` ≤ 10 行、依次调用 8 个 `hud*` 区块、自身不画；`primOverlay` ≤ 10 行、分派到 8 个 `overlay*` 函数、自身不画；`UI.Cell.Prim` 不再定义 `primOverlay` |

画面等价性依据：22 个静态场景、6 个步末动画场景与连击逐轮高亮 / 特殊块爆炸 / 特殊块组合 / 锤子 / 十字动画场景截图与 `ac211d8` 逐帧相同（AE=0），窗口标题逐字相同。

## 能力记录验收（第 9 刀）

`test/Spec/Caps.hs`；框架见 [architecture.md](architecture.md#元素的能力caps与调用时机)。对照对象 `test/Spec/Support/LegacyElement.hs` 是第 9 刀前的类与内置 instance 的逐字副本；性质挂固定种子 20260930。

| 用例 | 断言 |
|------|------|
| `qc_caps_match_legacy_elements` | 3000 例：任意格（全部内置本体的各种状态、宝石 × 冰 0..3 × 叠层、已注册 / 未注册的 `Custom`）经 `elementOf` / `bodyOf defaultRegistry` 解码后，27 项查询与旧类逐项相同（受击后的新元素递归展开；规则比有无与次序；`handleMessage` 用探针消息） |
| `qc_caps_rules_match_legacy` | 600 例：邻格规则（`aoBoard` / `aoDead` / `aoSit`）、步末规则（效果、新盘、运行前后的种子与挖空）、成对交换规则（成立与种子）、开启规则在随机盘面 / 随机真消除格 / 直接命中格 / 保护格 / 交换对上的输出与旧实现相同 |
| `qc_default_caps_match_legacy_defaults` | 1000 例：只写 `caps = capsOf 原型` 的元素与旧写法里只覆盖 `archetype` 的元素（三种原型 × 任意写回格子）逐项相同；惰性占格 `Inert` 同样 |
| `caps_element_class_is_thin` | 源码扫描：`class Element` 只有 `name` / `toCell` / `caps`；`builtinSources` 里 19 个 `instance Element` 只定义这三项 |
| `ext_caps_element_plugs_in` | 四行 instance 的测试专用元素「荆棘」`caps (Thorn n) = blocker [hit …, counts (CountNamed "thorn")]`，`customEntry` 注册后：挡交换、无色；锤子第 1 下耐久 2→1、第 2 下碎掉并计 `("thorn",1)`；`defaultRegistry` 下是惰性占格 |

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

仓库可能另有工作流配置；**本地以 `stack test` 全绿（当前 389，含金标准与元素查询快照比对）为合并门禁**。本文不依赖未跟踪的 `.github/` 内容。

门禁细则（第三刀起）：

- 每个提交都要 `stack test` 全绿、`golden_behaviour_snapshot` 逐行相等（`golden.txt` 不许改）。
- 通用层 `Engine.*` / `Shell.Loop` 不得 import `Match3`（`engine_layer_is_game_agnostic`）；三消实例与直接调用旧入口逐位相同（`engine_match3_instance_matches_direct_api`）。
- 动到前端绘制 / 回放时，另做截图比对：新旧二进制在 Xvfb 下的 6 个场景（展示盘、L16、L28 @2x、L5 暂停、L1 地图、L36 提示），贴图与几何降级两种模式都要能找到 `compare -metric AE` = 0 的帧配对（呼吸光随时钟变化，所以连拍找配对）。
- 元组兼容层（`runCascade*` / `resolveCountdowns` / `runPostBeltCascade` / `traceCascade*` / `toTuple*`）与门面 `Match3.Board` / `Match3.Game` 已删除；测试与 Golden 直接用 `CascadeRun` 记录和子模块。

## 网页版测试（`make check`）

`make check` = `make build`（wasm + 页面 + 图集）→ `make test`（`stack test` + 状态一致性 + 动画一致性 + e2e）→ `make size`。
安卓壳另有 `make android-sync` + `make android-check`（见 [android.md](android.md)）。

- **0 警告**：`web/cabal.project` 对本包 `match3-web` 加了 `-Werror`（本包已开 `-Wall`），网页版自己的模块（`web/hs`）
  和它直接编译的 `../src`、`../app/pure` 模块有任何警告都会让 `make build` 失败；依赖包（random、splitmix 等）不受影响。
- **一致性用例**：`web/test/parity.sh` 默认状态 33 组、动画 31 组（「关卡:种子[:走法]」列表，含第 41 关「爆破」、第 42 关「魔石」、
  第 43 关「毛球」（每步都有毛球跳格 `EvBelt "fuzzball"`）、第 44 关「魔力鸟」、第 45 关「雪怪」种子 1–3 与第 46 关「掉落口」种子 1 / 28 / 30——后两者按提示走在第 5–6 步收走饼干、掉落口补下新饼干，种子 1 走满也不补；第 47 关「变色龙」种子 1 / 2（每步都有步末换色 `EvTick "chameleon"`）与种子 140 的 `cham-rainbow` 走法；第 48 关「魔法格」种子 2–5 的 `fix-…` 固定走法），`CASES=` 可改。走法：`hint`（缺省，按核心提示）、
  `combo`（盘上有「彩虹 × 直线 / 炸弹」相邻且都无冰无叠层时先换它，行优先、先右后下）、`combo-bomb`（同上但先换「彩虹 × 炸弹」）、`cham-rainbow`（先换「彩虹 × 变色龙」，同样行优先、先右后下；种子 140 在第 5 步（0 起）换 (1,2) 彩虹 × (1,3) 变色龙，第一轮清掉彩虹、变色龙与 15 颗同色宝石，即成对交换规则 15）、`fix-RCRC-RCRC-…`（按写死的交换走：第 k 步换第 k 对，每对 4 个数字 r1 c1 r2 c2，用完后按提示）；
  提示不会主动选彩虹组合，第 44 关的变身步（`rainbow_line` / `rainbow_bomb`）靠后两种走法覆盖，`parity.sh` 会检查这些用例真的走到了变身步
  （状态 JSON 里有 `"kind":"rainbow_…"`、动画帧里有蔓延段；`cham-rainbow` 查两侧 stderr 都有「走法 cham-rainbow：第 k 步换彩虹 × 变色龙」）。
  第 48 关 4 组 `fix` 用例是测试跑手原生穷举（≤ 3 步）找到的「特效在魔法地格上引爆」走法，第 3 步（stderr 记为第 2 步，0 起）扩爆：
  种子 2 `fix-3536-4445-4252` 炸弹@(5,3) 25 格 5 行 5 列；种子 3 `fix-0414-4445-5262` 横线@(6,2) 24 格 3 行 8 列；
  种子 4 `fix-1415-5455-5455` 横线@(5,4) 24 格 3 行 8 列；种子 5 `fix-1516-2434-4243` 横线@(5,3) 24 格 3 行 8 列。原生侧 `magicBlasts` 用
  `Match3.Engine.play` 的 `EvBlast`、node 侧用接口 JSON 的 `events`，按「来源在魔法地格上」各记一行「走法 fix：第 k 步魔法地格扩爆 元素@(r,c) N 格 R 行 C 列」；
  `parity.sh` 要求原生侧有这一行、两侧逐字相同，一致时把它打印在结果行下面。原生 `Parity.hs` / `AnimParity.hs` 与 node 两侧的 `pickMove` 逐条相同。
- **e2e 端口 `E2E_PORT`**：e2e 临时起 `web/serve.py`，只监听 `127.0.0.1`，端口取环境变量 `E2E_PORT`（默认 **8765**）。
  同一台机器上并行跑多份 e2e（多个工作树 / 多个任务）时各设一个端口，例如 `make check E2E_PORT=18765` 或 `E2E_PORT=18765 make e2e`，
  直接跑脚本时 `E2E_PORT=18765 node web/test/e2e.mjs`。端口已被占用时 e2e 立刻报错退出；服务器起来后还会核对它提供的
  `index.html` 就是本次的 `web/dist`，不会连到别人的服务器。其他测试不占固定端口：一致性测试不起服务器，
  `make android-check` 的服务器用端口 0（系统分配空闲端口）。
- **贴图护栏（每关）**：`cells.js` 按元素名统计走几何降级（`drawCellPrim` 与缩放画法的色块分支）的次数，`m3debug.fallbacks` 暴露。
  e2e 对全部关卡（当前 48 关；第 45 关「雪怪」按象限画，多格护栏见下；第 46 关掉落口标记 `cookie_drop` 缺图走几何版时计入 `fallbacks`；第 47 关变色龙见下；地面层表外名字或缺贴图时按 `<名字>#地面层` 计入，第 48 关魔法地格接入前就是表外名字的淡灰框、护栏查不出）逐关开局、按提示走 3 步（空格加速），贴图加载后 `fallbacks` 必须为空；失败信息列出关卡与元素名。
  **每个新元素合入 main 后都要跟进 `web/www/cells.js`**（`primarySprite` / `CELL_ART` / `ELEMENT_RGB`，贴图名要在网页图集里），
  漏了这条护栏会把 `make check` 拦下来（魔法石合入时网页画成「custom」灰块，就是它要防的情况）。另截第 42 关魔法石 0–3 格充能：
  `magic-stone-charges-0123.png`（四块同盘）与 `magic-stone-charge-<v>.png`。`report.json` 的 `fallbacksByLevel` 逐关记录计数，
  第 43–48 关（毛球、彩虹组合、雪怪、掉落口、变色龙、魔法格）另有单独的「fallbacks 为空」检查项。
  同一轮逐关开局用真实绘制钩子（见下）再查两项：每个地面层格（第 39 关 16 格果冻、第 48 关 4 格魔法地格）都画了表内贴图
  （`jelly` / `jelly_2` / `magic`，测试侧自己的一份 `GROUND_SPRITE`，同桌面 `UI.Ground.groundTable`），56 × 56 在该格、调用序在该格底格之后、
  棋子之前（`report.json` 的 `groundByLevel`）；HUD 关名真实画了且只画了一张 `name_<关卡下标>`（同桌面 `HudArt`），矩形 = 关名槽
  （`levelNames`；当前 48 关都有文字图，没有关卡退回浏览器字体）。截关名 `level-name-l01.png` / `level-name-l41.png` / `level-name-l48.png`。
- **HUD 目标中文标签（每关）**：同一轮逐关检查 `m3debug.hud.goal`（HUD 实际画出的目标标签）= 「目标 」+ `state.goal.label`，
  且 `goal.label` 不含 `[a-z_]`（不漏出 `fuzzball` / `GoalNamed` 这类内部名）；逐关结果在 `report.json` 的 `goalLabels`。
  `goal.label` 来自视图模型 `Match3.View.goalLabel`（名字目标查 `namedGoalLabelTable`），`stack test` 的 `frontends_read_view_model` 也核对全部关卡与每日挑战。
  同一轮还逐关核对失败提示 `state.loseHint`（失败面板副标题的前半）：全部关卡不含「箱子」与 `[a-z_]`，碎石目标关（第 8 / 41 / 42 / 44 / 48 关）
  =「用邻消或特效砸开碎石，目标 <target> 个」（核心 39dde8e 修正，原为「砸箱子」；逐关结果在 `report.json` 的 `loseHints`）。
- **第 43 关毛球**：静止时冻结在浮动偏移 −2 与 +2 设计像素的两帧（同桌面 `sprBob`），截 `fuzzball-float-a-up.png` / `fuzzball-float-b-down.png`，
  并在页面里对两帧格内像素做纵向平移搜索，最佳平移须在 4 设计像素 × `u` × dpr（1280×800 dpr2 下 12.8 物理像素）±2.5 以内；
  步末跳格冻结在皮带段中间（`trace.end` 的 belt 项从毛球格出发），截 `fuzzball-jump-mid-l43-*.png`；HUD「目标 毛球」竖屏 / 横屏截 `goal-label-l43-*.png`。
- **第 44 关彩虹组合**：`state.rules` = `[{name:"rainbow_combos", text:"彩虹组合变身", icons:["rainbow"]}]`，竖屏 / 横屏角标完整画在关卡面板里
  （`rules-badge-l44-*.png`）；开局的彩虹 × 直线、彩虹 × 炸弹各走一次，冻结在第一轮之前的蔓延段中间（`trace.end[0]` 为 `afterWaves = 0` 的
  `rainbow_line` / `rainbow_bomb`，此时轮次 0、连击 0），截 `rainbow-transform-{line,bomb}-mid-l44-*.png`，播完 `fallbacks` 仍为空。
- **雪怪 Boss（第 45 关）**：图集含 `snow_boss` / `snow_boss_0..3` / `snow_boss_hurt_0..3`；竖屏 390×844 与横屏 1280×800 下四格在
  (2,3)–(3,4)、象限 0–3、贴图正确，`state.boss` = HUD 血条读数 = 40/40（`m3debug.hud.boss`），第 42 关 `state.boss = null`；
  多格护栏：带 `q` 的 Custom 格走通用「元素名贴图 + 角标」画法时计入 `fallbacks`（键 `<元素名>#多格通用画法`），反证为页面里强制
  `forceGeneric.add("snow_boss")` 后护栏必须报出、撤掉后不再增加（前后对比 `snow-boss-crop-before-generic.png` / `snow-boss-crop-after.png`）；
  按提示走截「扣血那一轮的高亮」与「召唤雪块的步末」，种子 32 走到血量过半截受伤表情（四格 `hurt`、血条进入过半状态）；全程 `fallbacks` 为空。
  截图 `snow-boss-l45-*.png`、`snow-boss-hit-flash.png`、`snow-boss-summon-tick.png`、`snow-boss-hurt.png`。
- **饼干掉落口（第 46 关）**：图集含 `cookie_drop`；竖屏 390×844 与横屏 1280×800 下 `state.drops` = 顶行 (0,1)/(0,3)/(0,4)/(0,6)、开局饼干在口上，
  第 1 关 `drops = []`；种子 30 按提示走，截「掉落口补下饼干的下落段」与补完后的盘面（收 1 块、盘上仍 4 块饼干）；全程 `fallbacks` 为空。
  截图 `cookie-drop-l46-*.png`、`cookie-drop-fall.png`、`cookie-drop-after-refill.png`。
- **真实绘制钩子**：每个页面 `addInitScript` 注入，包住 `CanvasRenderingContext2D.prototype.drawImage`，对画到 `#board` 的每次调用按**调用序**
  记下序号 `seq`、是否图集、源矩形（按 `atlas.json` 反查贴图名）、变换后的屏幕矩形与旋转角；`__captureFrame()` 取完整一帧。
  格子网格从真实画出的 `tile_a` / `tile_b` 推出（不读 `m3debug.layout`），检查只看「真的画了什么、画在哪、先后顺序」：
  第 46 / 47 关竖屏 / 横屏的静止帧与交换补间帧（`swap` 段第 4 帧以后）里 `cookie_drop` = 桌面 `drawDropsArt` 的 `(16+56c, 16+56r−6)`；
  第 47 关每个变色龙格恰好一张 `gem_c<v+1>`（不旋转）且它的 `seq` 小于同格环 `chameleon` 的 `seq`（先宝石后环）。
- **变色龙（第 47 关）**：竖屏 390×844 与横屏 1280×800 下关名「变色龙」、`c = v + 1`、HUD「目标 变色龙」且目标图标 `chameleon_icon`、真实绘制核对，
  截 `chameleon-l47-*.png`；通用画法反证：页面里 `forceGeneric.add("chameleon")` 后真实绘制核对必须失败、`fallbacks` 出现
  `chameleon#通用画法缺底层宝石`，撤掉后恢复（`chameleon-crop-before-generic.png` / `chameleon-crop-after.png`）；
  步末换色冻结在倒计时段前半（真实绘制 = `trace.end` 的换色前颜色，`chameleon-shift-before.png`）与后半（换色后颜色，`chameleon-shift-mid.png`），
  播完 state 里的变色龙 = 换色结果、真实绘制随之更新。
- **魔法地格（第 48 关）**：竖屏 390×844 与横屏 1280×800 下 `state.ground` = 4 格 magic (6,2)(6,5)(5,3)(5,4)、layers 1，关名 `name_47`「魔法格」，
  HUD「目标 碎石」；真实绘制：4 格都画了 `magic`（位置 = 该格、棋盘格之上、棋子之下），像素上格边一圈（离边 4%–12%）的紫度
  (R + B) / 2 − G 比同行其他格平均高 ≥ 15（实测约 54–57 对 29；贴图被换成淡灰框时查得出来），截 `magic-l48-*.png`。
  扩圈爆炸：按 parity 的 4 组 `fix` 走法走，第 3 步冻结在扩爆那一轮的「消失」段（`pop`，约 1/3 处），`pending.events` 的 blast 与原生逐项相同
  （元素、来源、格数、行列数），每个目标格都有真实绘制——被消格在格中心画了光环（≥ 一格）与缩小的棋子，碎石等受击不消的格画了 `holes`
  里受击后的样子（层数变了）；两类合计 = EvBlast 格数，扩出来的一圈（原范围之外的 16 格）全在内；截 `magic-widen-seed<N>.png`。
  终章：第 47 关种子 2 / 第 48 关种子 3 按核心测试的一步贪心走法（`test/Spec/{Chameleon,MagicGround}.hs`，原生算出后写死在 e2e 里）走完——
  第 47 关 `LevelClear`「过关！… 进入第 48 关」，第 48 关（最后一关）`Won`「通关！」（`magic-l48-won.png`）。
- **玩到失败（第 8 / 39 / 40 / 41 / 42 / 43 / 44 / 45 / 47 / 48 关，种子 7，按提示过了关就换种子 8–11）**：按提示走满步数，状态 Lost、结算层标题「步数用完了」、副标题含 `state.loseHint`
  且不含 `[a-z_]` 与「箱子」（读 `m3debug.overlay`，即真正画出的文字），碎石目标关（8 / 41 / 42 / 44 / 48）副标题 =「用邻消或特效砸开碎石，目标 n 个。可「撤销」或「重开」」；截 `chameleon-l47-lost.png` / `magic-l48-lost.png`。
- **反证（测试跑手复核用，不进 `make check`）**：临时副本里改 `dist/cells.js` 跑完整 e2e，必须失败——掉落口画到 x+4（`dropMarks` 不变）
  → 8 项真实绘制掉落口检查失败；变色龙底层宝石画成 `gem_c<(c mod 5)+1>` → 6 项变色龙检查失败；先画环再画宝石（宝石名字对，只是顺序反了：环 seq 178 < 宝石 seq 179）→ 同样 6 项变色龙检查失败。三份副本都是整套 e2e 退出码 1、其余项照常通过。
  魔法地格 / 关名（web-magic-ground，149 项的版本，副本端口 8832–8835）：删掉 `cells.js` GROUND 表的 `magic` 行 → 13 项失败（逐关地面层真实绘制、
  逐关与第 48 关 `fallbacks`（`magic#地面层`）、第 48 关两个视口的贴图 / 像素 / 降级、4 组扩爆的降级）；`magic` 不画贴图、直接画淡灰框（不计 `fallbacks`，即适配前的样子）
  → 5 项失败（逐关地面层真实绘制、第 48 关两个视口的贴图与像素）；`atlas.webp` 里 `magic` 区域换成淡灰框（贴图名与位置都对）→ 2 项像素检查失败；
  `hud.js` 关名错位一关（画 `name_<i+1>`，第 48 关因此退回浏览器字体）→ 3 项失败（逐关关名真实绘制、第 48 关两个视口的开局检查）。四份副本 e2e 都退出码 1。
- **规则开关角标**：e2e 检查第 41 关 `state.rules` = `[{name:"bomb_shapes", text:"L/T 形出炸弹", icons:["bomb_glow","bomb_mark"]}]`，
  竖屏 390×844、横屏手机 844×390、桌面 1280×800 三种布局下 HUD 角标都完整画出、落在关卡面板里、不压「第 N 关」标签与关名、
  彼此不重叠（读 `m3debug.hud`），走一步后仍在；第 1 关与第 42 关（魔法石是元素不是规则开关）没有角标。截图
  `rules-badge-*.png` / `rules-badge-l42-*.png` 写到 `SHOTS`（默认 `/workspace/match3-web-shots/`，每次运行先清空）。

## 新玩法验收

2026-09-30 起解除机制冻结，按 [backlog.md](backlog.md) 逐项加新玩法。每个玩法：全量编译 0 警告 + `stack test` 全过；改了 `web/hs` 或 `app/pure` 才跑 `make check`；快照因新增内容变化时允许重录，但 diff 只能来自新增内容（原有关卡行为不变），并在提交说明里写明。

| 玩法 | 测试模块 | 用例 | 快照变化 |
|------|----------|-----:|----------|
| 1. L / T 形出炸弹（规则开关 `bomb_shapes`，第 41 关「爆破」） | `test/Spec/BombShapes.hs` | 6 | 金标准末尾追加第 41 关 105 行 + 第 40 关过关结局 4 行（`Won` → `LevelClear`）；元素查询快照 `R level` 1 行 + `M 40` 2 行 + 追加 `M 41` 2 行 |
| 2. 魔法石（`Custom "magic_stone"`，第 42 关「魔石」） | `test/Spec/MagicStone.hs` | 6 | 金标准末尾追加第 42 关 85 行 + 第 41 关过关结局 2 行（`Won` → `LevelClear`）；元素查询快照 `R adjacent` / `R end` / `R names` 3 行（新条目与规则）+ 5 行 `E … PhaseTick`（该阶段规则列表多了魔法石；去掉魔法石条目重跑生成器，除 `M 41` 外与旧快照逐字相同）+ `M 41` 2 行 + 追加 `M 42` 2 行 |
| 3. 毛球（`Custom "fuzzball"`，第 43 关「毛球」） | `test/Spec/Fuzzball.hs` | 7 | 金标准末尾追加第 43 关 76 行（2026-09-30 加难后为 85 行，见下文「第 43 关加难」） + 第 42 关种子 2 十字道具过关 3 行（2714 / 2719 / 2720：`Won` → `LevelClear>42`，同行 / 辅助行整局状态散列随之变）；元素查询快照 `R adjacent` / `R end` / `R names` 3 行（新条目与规则）+ 5 行 `E … PhaseMove`（该阶段规则列表多了毛球；去掉毛球条目重跑生成器，除追加的 `M 43` 外与旧快照逐字相同）+ 追加 `M 43` 2 行 |
| 4. 魔力鸟组合增强（规则开关 `rainbow_combos`，第 44 关「魔力鸟」） | `test/Spec/RainbowCombos.hs` | 7 | 金标准末尾追加第 44 关 105 行 + 第 43 关过关结局 8 行（2758 / 2768 / 2770 / 2771 / 2786 / 2794–2796：种子 1 / 2 的交换或道具过关，`Won` → `LevelClear>43`，同行 / 辅助行整局状态散列随之变）；元素查询快照 `R level` 1 行（多了 `rainbow_combos`）+ `M 43` 2 行（同上）+ 追加 `M 44` 2 行；原有关卡没有别的变化 |
| 5. 雪怪 Boss（`Custom "snow_boss"`，2×2，第 45 关「雪怪」） | `test/Spec/SnowBoss.hs` | 7 | 金标准末尾追加第 45 关 105 行（2915–3019），原有 2914 行逐字不变（第 44 关在金标准里没有过关的局，结局行不变）；元素查询快照 `R adjacent`（多了 200）/ `R end`（`PhaseMove` 多了 30）/ `R names`（多了 `snow_boss`）/ `R diff`（多了 `("snow_boss",Just (CountNamed "snow_boss"),0)`）4 行 + 5 行 `E {0,60,120,180,240} PhaseMove`（该阶段规则列表多了雪怪；去掉雪怪条目、保留权重钩子重跑生成器，前 1656 行与旧快照逐字相同，只是第 45 关装饰报 `UnknownElement`）+ 追加 `M 45 1` / `M 45 2` 2 行；`M 1`–`M 44` 不变 |
| 6. 饼干掉落口（关卡级元素 `CookieDrop`，第 46 关「掉落口」） | `test/Spec/CookieDrop.hs` | 7 | 金标准末尾追加第 46 关 105 行（3020–3124），原 3019 行逐字不变（第 45 关在金标准里没有过关的局，「不再是终章」不影响任何结局行）；元素查询快照 `R level` 1 行（多了 `cookie_drop`）+ 追加 `M 46 1` / `M 46 2` 2 行；`M 1`–`M 45` 不变 |
| 7. 变色龙（`Custom "chameleon"`，第 47 关「变色龙」） | `test/Spec/Chameleon.hs` | 8 | 金标准末尾追加第 47 关 105 行（3125–3229），原 3124 行逐字不变（第 46 关在金标准里没有过关的局，「不再是终章」不影响任何结局行）；元素查询快照 `R end`（`PhaseMove` 多了 40）/ `R swap`（`[10,20]` → `[10,15,20]`）/ `R names`（多了 `chameleon`）3 行 + 5 行 `E {0,60,120,180,240} PhaseMove` + 5 行 `S 0…240`（这两类散列的是规则列表，各多了一条变色龙规则；这些盘面上没有变色龙，规则什么都不做）+ 追加 `M 47 1` / `M 47 2` 2 行；去掉变色龙条目重跑生成器，前 1660 行与旧快照逐字相同 |
| 8. 魔法地格（地面层 `"magic"`，第 48 关「魔法格」） | `test/Spec/MagicGround.hs` | 8 | 金标准末尾追加第 48 关 105 行（3230–3334），原 3229 行逐字不变（第 47 关在金标准里没有过关的局，「不再是终章」不影响任何结局行）；元素查询快照只有 `R names` 1 行变化（末尾多了 `magic`）+ 追加 `M 48 1` / `M 48 2` 2 行；去掉魔法地格条目重跑生成器，前 1662 行与旧快照逐字相同，且 `M 48` 两行与带条目时相同（这两段脚本没有在魔法地格上引爆特效）。其余 R / E / S / O / C / G 行不变：魔法地格没有邻格 / 步末 / 成对 / 开启规则，扩爆格只在第 48 关的盘面上才有 |

`test/Spec/BombShapes.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `bs_switch_only_on_new_level` | 内置表不变；关卡表里只有第 41 关与第 48 关（新玩法 8 起，魔法格复用 `bomb_shapes`）写了 `bomb_shapes`；原有 40 关（种子 1 / 2）、每日挑战（种子 0–9）、自由开局每步结算用的形状表 = 内置表；第 41 关 = `withBombShapes` 内置表；`GameState` 的 `Show` 不打印开关 |
| `bs_rule_order` | `withBombShapes` 把 `l/t→bomb` 插在 `line5→rainbow` 之后、其余规则之前；表里没有五连规则时放最前面 |
| `bs_l_shape_bomb_on_level41` | 两条三连交成 L 的交换：第 41 关第一轮交点留下炸弹；同一局面在第 1 关、以及 `removeLevel "bomb_shapes"` 后，交点是空洞 |
| `bs_five_still_rainbow` | 横五连 + 竖三连（交点在端点）：开关打开时只出彩虹，和内置表一样 |
| `bs_four_in_l_gives_bomb_not_line` | 横四连 + 竖三连交成 L：开关打开时交点出炸弹、不出直线；内置表出一个横向直线 |
| `bs_level41_play_spawns_bombs` | 第 41 关种子 1–6 按提示各走 12 步：生成过炸弹；去掉开关后同样走法一颗炸弹也没有 |

`test/Spec/MagicStone.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `ms_caps_fixed_immune_colorless` | 充能 0–3：挡交换、不下落、无色、洗牌保留、直接命中 `HitImmune`；发射中（4）命中 `HitAbsorb` 成 0 格 |
| `ms_charges_once_per_round` | `runAdjacentWith`：两个邻格同一轮只 +1；满 3 不再涨；只有斜角邻格不动；三轮充满、第四轮仍是 3 |
| `ms_fires_row_and_col_at_step_end` | `tripleBoard` 的交换让 (2,1) 的魔法石从 2 格充满：步末恰一条 `EvTick "magic_stone"`，某一轮清掉第 2 行和第 1 列（除它自己），结算后 0 格 |
| `ms_not_full_does_not_fire` | 同一步魔法石 0 格：充到 1 格、没有发射 |
| `ms_boosters_wait_for_next_swap` | 满格的魔法石遇锤子不发射、仍满；下一次交换的步末发射并归零 |
| `ms_level42_layout_and_play` | 第 42 关种子 1–3 开局四块 0 格魔法石在固定位置；前 41 关开局没有魔法石；种子 1–6 按提示各走 20 步：魔法石始终原地、发射过、石头计数有进度 |

`test/Spec/Fuzzball.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `fz_caps_blocker_falls_breaks` | 挡交换、随重力下落、无色、洗牌保留、直接命中 `HitDestroy` |
| `fz_adjacent_clear_kills` | `runAdjacentWith`：与真消除格正交相邻的毛球进入死亡格且只算一次；只有斜角邻格的不动；本轮已被直接命中的不重复算 |
| `fz_jumps_to_plain_gem_neighbour` | `fuzzballJumps`：四周都是普通宝石时跳到其中一格、原格换成那颗宝石；同一盘面两次结果相同 |
| `fz_jumps_respect_walls_avoid_and_blockers` | 墙 + 避让格只剩一格时跳那一格；四周是石头 / 特殊块时不动、盘面不变；两个毛球相邻时只能跳普通宝石；两个毛球争同一格只有一个跳过去 |
| `fz_step_end_belt_effect_replays` | `tripleBoard` 的交换后，远处 (6,6) 的毛球步末恰一条 `EvBelt "fuzzball"`、两项，`applyEndEffect` 由前盘重放得后盘，毛球落在相邻格 |
| `fz_other_levels_unchanged` | 前 42 关开局没有毛球；去掉毛球条目的注册表与默认注册表按提示各走 6 步：盘面、得分、`gsGen` 逐关相同 |
| `fz_level43_layout_and_play` | 第 43 关种子 1–3 开局 14 个毛球；种子 1–6 按提示各走 22 步：跳过格、每局都有消灭计数、至少一局过关；难度护栏：种子 1–3 按提示走 8 步都还没过关 |

`test/Spec/RainbowCombos.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `rc_switch_only_on_new_level` | 关卡表里只有第 44 关写了 `rainbow_combos`；`GameState` 的 Show 不含开关；同一个彩虹 × 直线交换在第 1 关没有变身步 |
| `rc_morph_pairs_and_soft_lock` | `rainbowComboMorph`：彩虹 × 横 / 竖直线 → `rainbow_line`、彩虹 × 炸弹 → `rainbow_bomb`、直线在另一端同样成立；彩虹 × 普通宝石 / 彩虹 × 彩虹 / 直线 × 炸弹 / 冰 2 层的软锁彩虹都不成立 |
| `rc_rainbow_line_morphs_then_fires` | 第 44 关：`mtStart` = 交换后盘面；恰一条变身步、`esAfterWaves = 0`、`EvSpread`、从交换后盘面开始且可重放；目标 = 全部同色普通宝石、来源 = 彩虹所在格、横竖按 (行 + 列) 奇偶；第一轮从变身后盘面开始，每条横线的整行 / 竖线的整列都在第一轮清除格里；效果事件里有第 0 轮的 `EvSpread rainbow_line` |
| `rc_rainbow_bomb_morphs_then_fires` | 同色普通宝石全变炸弹，第一轮清掉每颗炸弹的 3×3 |
| `rc_morph_skips_iced_and_overlaid` | 带冰 / 带草的同色宝石不在变身格里、仍在起手种子里；普通的全部变身 |
| `rc_old_levels_unchanged` | 前 43 关 × {直线, 炸弹}：同一局面在默认注册表与 `removeLevel "rainbow_combos"` 下盘面、得分、`gsGen`、结局、步末数、各轮清除格逐项相同；第 44 关两者不同 |
| `rc_level44_layout_and_play` | 第 44 关种子 1–3 开局两组组合在固定位置、石头共 24 层；打出彩虹 × 直线：恰一条 `rainbow_line` 变身，石头剩余层数 + 已碎数 ≤ 16 |

改动了的旧测试：`test/Spec/Support.hs` 的 `checkEffectDetail`（逐轮回放护栏用）认识变身步：来源是彩虹、目标由同色普通宝石变成直线 / 炸弹且颜色不变（其余蔓延仍要求来源正交相邻）；`bs_switch_only_on_new_level` 的断言改为「只有第 41 关写了 `bomb_shapes`」；内置关卡级元素列表（`br_level_hooks_builtin_and_removable`、`ec_flat_record_removed`、`ec_level_elements_by_message`、`ec_level_element_stateful_extension`）多了 `rainbow_combos`（性质 `qc_level_elems_readers_roundtrip` 的开局元素列表同样）；关卡数 43 → 44。

`test/Spec/SnowBoss.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `sb_caps_fixed_blocker` | 四个象限：挡交换、不下落、无色、洗牌保留、直接命中 `HitAbsorb` 自身；差计权重左上 = 血量、其余 0；计数键 `CountNamed "snow_boss"`；编码往返 |
| `sb_placement_and_weight` | 放置 `[血量, 象限]` 与坏参数；第 45 关种子 1–3 四格在 (2,3)–(3,4)、满血 40 = 目标值、`weighElementWith` = 40；目标 = `goalCount (CountNamed "snow_boss") 40`；`gvBoss` 第 45 关 = `BossView 40 40`、前 44 关为 `Nothing`；`bossPart` 象限 / 过半受伤；其余元素 `weighElementWith` = `countElementWith` |
| `sb_adjacent_and_direct_damage` | `runAdjacentWith`：身外一圈 3 格真消除扣 3、1 格 + 直接命中 2 格扣 3，四格同血；血量不够时四格一起进入死亡格；远处 / 斜角的消除不变 |
| `sb_hammer_and_defeat_wins` | 锤子打 Boss 格被接受、血 40 → 39、计 1、Boss 原位、`gvBoss` = 39/40；血 1 时再锤：四格在第一轮清除、计数 40、过关 |
| `sb_step_end_summons_snow` | 连续 3 次交换：每步恰一条 `EvTick "snow_boss"` 且可重放，计数 1 → 2 → 0，第 3 步恰一块雪块（1 层石头）在身外一圈、前两步没有；选格确定；锤子不推进计数；一圈没有普通宝石 / 全在避让格与墙上时不召唤 |
| `sb_other_levels_unchanged` | 前 44 关开局没有 Boss；去掉雪怪条目的注册表与默认注册表按提示各走 6 步：盘面、得分、计数、步数、`gsGen` 逐关相同 |
| `sb_level45_layout_and_play` | 第 45 关种子 1–6 按提示走满 24 步：Boss 活着时始终在原位、每局都扣过血且计数 = 扣掉的血、召唤过雪块；「优先打 Boss」的一步贪心（每步选使 Boss 血量最低的交换）种子 1–3 都能过关 |

改动了的旧测试：关卡数 44 → 45（`jb_levels_appended`、`Builtin/Obstacle`、`Builtin/Layer` 4 处、`GoalsLevels` 2 处，标签多了「第 45 关雪怪」）；`ElementClass` 内置条目数 33 → 34；`Caps` 本体实例数 21 → 22；`Presentation` 的 `draw_hud_and_prim_overlay_are_thin` 期望的几何版 HUD 分派多了 `hudBoss`（在 `hudGoal` 之后）。

`test/Spec/CookieDrop.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `cd_drop_levels_are_46_and_47`（新玩法 7 前名为 `cd_only_level46_has_drops`） | 关卡表里只有第 46 关（四个顶行掉落口、饼干、保持 4 块）与第 47 关（(0,3)、`Custom "chameleon" 0`、保持 2 只）写了 `lvlDrops`；`levelDrops` / `bvDrops` 在第 46 关给出掉落口格，前 45 关与每日挑战为空；`GameState` 的 Show 不含 `CookieDrop` |
| `cd_refill_drops_cookie_at_drop_cells` | `dropRefill`：两个掉落口格都有空洞且盘上没有饼干时两格都补饼干，其余空洞与原策略相同；已有一块时只剩一个名额、行优先先补的拿到；已够两块、或掉落口格没有空洞时与原策略完全相同 |
| `cd_refill_same_rng_as_base` | 多列空洞、种子 1–5：补满后的生成器与原策略相同；盘面只在掉落口格上不同；策略名 `random-gem+drop` |
| `cd_level46_start_no_goal_decor` | 第 46 关种子 1–3 开局饼干恰在四个掉落口格（没有目标补齐）、目标收 8 块；第 22 / 23 关仍按目标补齐；去掉 `cookie_drop` 条目开局盘面相同 |
| `cd_drops_and_collects_in_play` | 第 46 关种子 1–6 一步贪心走满：盘上饼干始终 ≤ 4、每局都掉过饼干并有收集计数、每步新出现的饼干数在 0–4 之间；去掉条目后按提示走 26 步饼干总数不超过开局的 4 块 |
| `cd_other_levels_unchanged` | 去掉 `cookie_drop` 条目的注册表与默认注册表：前 45 关（种子 3）与 3 天的每日挑战按提示各走 6 步，盘面、得分、计数、步数、`gsGen` 逐项相同；第 46 关按提示走 26 步在种子 1–30 里至少一局不同 |
| `cd_level46_difficulty` | 按提示走种子 1–30 赢 ≤ 25 局（backlog 的「太容易」标准）；一步贪心种子 1 过关 |

改动了的旧测试：关卡数 45 → 46（`jb_levels_appended`、`Builtin/Obstacle`、`Builtin/Layer` 4 处、`GoalsLevels` 2 处）；内置关卡级元素列表多了 `cookie_drop`（`br_level_hooks_builtin_and_removable`、`ec_flat_record_removed`、`ec_level_elements_by_message`、`ec_level_element_stateful_extension`，性质 `qc_level_elems_readers_roundtrip` 同样）；`campaign_levels_batch_ok` 的「开局收集物数 ≥ 目标」对有掉落口的关卡改为「开局至少一块、掉落口掉的是饼干」。测试辅助 `findMatchPair` 已按引擎的挡交换条件过滤（饼干挡交换），`find_match_pair_engine_accepts` 自动覆盖第 46 关。

`test/Spec/Chameleon.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `ch_caps_piece_by_current_color` | `Custom "chameleon" k` 的颜色 = 第 k 种；可交换、会下落、进提示、命中即消、洗牌保留、不可改色、计 `CountNamed "chameleon"`；放置取原格宝石颜色（或 `AColor` 指定），原格不是宝石时不放 |
| `ch_matches_by_current_color` | 同色变色龙参与交换三消并计数，异色则 `NoMatch`；现成连线按当前颜色判断；无可走步的盘面放一只 C3 变色龙后唯一可走步 (1,0)↔(2,0)，`findMatchPair`、提示 `findHintWith` 都找到它且引擎接受，同一格换成石头则无步可走 |
| `ch_shift_fixed_order_skips_instant_runs` | `chameleonShift`：C2 → C3 → C4 → C5 → C1 → C2 循环；下一种会立刻连成三消时顺延（跳 C2 取 C3；再跳 C3 取 C4）；四种都连成时回到原色；两只相邻时后一只看的是前一只换过的颜色；没有变色龙时原样 |
| `ch_step_end_shift_swap_only` | 玩家交换一步：(6,6) C3 → C4、(7,7) C5 → C1，恰一条 `EvTick "chameleon"`（两项），`applyEndEffect` 重放一致；锤子不换色 |
| `ch_rainbow_swap_clears_current_color` | 彩虹 × C3 变色龙成对规则成立，种子含全部 C3 宝石、两只 C3 变色龙与彩虹，不含 C2 变色龙；实走计数 ≥ 2；没有彩虹时不成立 |
| `ch_drop_port_counts_any_color` | 第 47 关的掉落口按名字数同种：盘上一只时补一只 C1，已有两只（不同颜色）时与原策略相同 |
| `ch_other_levels_unchanged` | 前 46 关开局没有变色龙；去掉变色龙条目的注册表与默认注册表：前 46 关（种子 3）与 3 天的每日挑战按提示各走 6 步，盘面、得分、计数、步数、`gsGen` 逐项相同 |
| `ch_level47_layout_and_difficulty` | 第 47 关开局 2 只在 (3,1) / (5,6)、目标 30、18 步、掉落口 (0,3)；实战里换过色、掉过变色龙；按提示种子 1–30 赢 ≤ 25 局；一步贪心（先多消变色龙、再得分）种子 2 过关 |

改动了的旧测试：关卡数 46 → 47（`jb_levels_appended`、`Builtin/Obstacle`、`Builtin/Layer` 4 处、`GoalsLevels` 2 处，标签多了「第 47 关变色龙」）；`ElementClass` 内置条目数 34 → 35；`Caps` 本体实例数 22 → 23；`Branches` 的成对规则顺序 `[10,15]`（元素）/ `[10,15,20]`（含组合表）；`CookieDrop` 的 `cd_only_level46_has_drops` 改为 `cd_drop_levels_are_46_and_47`；`campaign_levels_batch_ok` 对有饼干掉落口的关卡收紧为「开局饼干数 ≥ 每条饼干 `DropSpec` 的 min(保持数, 掉落口格数)」（第 46 关 ≥ 4；原来只要求至少一块）。

`test/Spec/MagicGround.hs` 的断言：

| 用例 | 断言 |
|------|------|
| `mg_caps_ground_not_consumed` | 名字 `magic`、显示格 `Custom "magic" 1`、扩爆规则 = `magicWiden`（果冻没有）；`hitGroundWith` 命中 4 格魔法地格后原样、不计数；第 48 关按提示走 6 步后地面层不变、计数里没有 `magic` |
| `mg_widen_one_ring` | 第 6 行直线 → 第 5–7 行（原范围在前、新格按行优先在后）；中间 3×3 → 5×5（25 格）；角上一格 → 2×2；空范围不变 |
| `mg_blast_widened_only_at_magic_cell` | 第 48 关本步扩爆格 = [(6,2),(6,5),(5,3),(5,4)]；横直线在这 4 格引爆 = `magicWiden` 原范围，在 (6,3) / (5,2) / (4,3) / (7,2) 引爆 = 原范围；(6,2) 的炸弹 5×5 贴底边截成 20 格；普通宝石没有爆炸；缺省注册表不扩 |
| `mg_swap_and_hammer_reach_bottom_row` | 交换 (5,4)↔(6,4) 连成 (6,2..4) 三消，(6,2) 的横直线 `EvBlast` 覆盖 6 / 5 / 7 行共 24 格（去掉条目时只有第 6 行 8 格）；锤子敲 (5,3) 的横直线：8 块三层碎石全削成 2 层（去掉条目、或敲非魔法格 (5,2)：底行不动）；锤子敲 (6,2) 的炸弹削到第 0–4 列（去掉条目 1–3 列），竖直线削到 1–3 列（去掉条目只有第 2 列） |
| `mg_combo_seeds_not_widened` | 彩虹 × 宝石、直线 × 直线、炸弹 × 炸弹在 (6,2)↔(6,3) 交换：成对规则给的种子与缺省注册表相同；彩虹 × 宝石整步与去掉条目时逐项相同；直线 × 直线：交换到 (6,2) 的竖直线扩成 24 格、(6,3) 的横直线仍 8 格；炸弹 × 炸弹：(6,2) 的炸弹 20 格、(6,3) 的 9 格（去掉条目时都是原范围） |
| `mg_default_no_widening` | 缺省注册表、前 47 关（种子 1）与 3 天每日挑战的本步注册表都没有扩爆格 |
| `mg_other_levels_unchanged` | 前 47 关的地面层里没有 `magic`；去掉魔法地格条目的注册表与默认注册表：前 47 关（种子 3）与 3 天的每日挑战按提示各走 6 步，盘面、得分、计数、步数、`gsGen` 逐项相同 |
| `mg_level48_layout_and_difficulty` | 第 48 关名「魔法格」、目标碎石 8、18 步、规则 `bomb_shapes`、地面层 4 格；种子 1–3 底行 8 块三层碎石、开局无现成三消、地面层同关卡记录；按提示种子 1–30 赢 ≤ 25 局；一步贪心（先多碎石、再得分）种子 3 过关 |

改动了的旧测试：关卡数 47 → 48（`jb_levels_appended`、`Builtin/Obstacle`、`Builtin/Layer` 4 处、`GoalsLevels` 2 处，标签多了「第 48 关魔法格」）；`ElementClass` 内置条目数 35 → 36；`Caps` 本体实例数 23 → 24；`BombShapes` 的 `bs_switch_only_on_new_level` 期望 `bomb_shapes` 写在第 41 / 48 关（原来只有第 41 关）。

第 48 关模拟胜率（30 个种子，一次性脚本，不进测试）：按提示（`findHintWith`）0/30；一步贪心（先多碎石、再得分）14/30，过关用 9–18 步；与第 22 / 23 / 46 / 47 关的贪心 11 / 14 / 10 / 11 同档。对照：同一布局去掉魔法地格，贪心 9/30。调参记录：2 层碎石 / 22 步 / 6 格魔法地格 贪心 28/30（太容易）；3 层 / 18 步 / 不开 `bomb_shapes` 10/30；3 层 / 开 `bomb_shapes` / 6 格 15/30；4 层 4/30；魔法地格放在第 4 行（(4,3) / (4,4)）时扩出的一圈到不了底行，10/30。

### 第 43 关加难（2026-09-30）

网页端反映第 43 关种子 1 走 3 步就过关；桌面核心复现：原布局 10 个毛球挤在上三行，按提示走（`findHintWith`）种子 1 三步过关（第一步连锁灭 6 个），种子 1–30 里 26 局过关、种子 9 / 30 两步过关。改为 14 个毛球分散在全盘（隔行错开）、目标 14：种子 1 要 14 步，种子 1–30 里 19 局过关（其余 11 局 22 步用完）。快照变化只来自第 43 关：金标准第 43 关一段（原 2725–2800 行 76 行 → 新 2725–2809 行 85 行，`L43` / `G43` 行，目标 `namedfuzzball:10` → `:14`），前后各段逐字不变（第 42 关过关结局仍是 `LevelClear>42`）；元素查询快照只有 `M 43 1` / `M 43 2` 两行。`fz_level43_layout_and_play` 改为 14 个毛球并加难度护栏（种子 1–3 按提示 8 步内过不了关）。

### 测试辅助 findMatchPair（2026-09-30 修正）

`Spec.Support.findMatchPair`（`outcome_moves_or_score`、`release_core_invariants_green` 等 9 处用它挑一步能走的交换）原来只看「交换后有三连」，不查两格能否交换。第 43 关（`3fe0d9a` 时是最后一关）`newGameAtLevel 42 {5 步, 1 分} 42` 开局它选中 (1,4) 宝石 × (1,5) 毛球（毛球挡交换），引擎以 `NoMatch` 拒绝，`outcome_moves_or_score` 报「expected Won on last level, got NoMatch」（`release_core_invariants_green` 复用它，一起失败）。之后第 44 关成为最后一关、这一用例碰巧变绿，缺陷仍在（第 28 关同样会选中挡交换的对）。

修正：`findMatchPair` 先过滤掉 `swapBlockedWith defaultRegistry`（与 `resolveSwap` 的拒绝条件相同）的对，再看是否成消；顺序（先横后竖、行优先）不变，旧版保留为 `findMatchPairNaive` 只给回归对照。回归用例 `find_match_pair_engine_accepts`（`GoalsLevels`）：全部战役关卡 × {`newGameAtLevel` 5 步 / 1 分 / 种子 42、`campaignGame` 种子 1–3}，选出的对交给 `trySwap` 不是 `NoMatch` / `InvalidSwap`；并断言旧版在这些开局里至少一次选中被拒的对（第 28 关 (0,0)–(0,1)），保证用例确实覆盖该缺陷。用原第 43 关布局复核过：旧版选 (1,4)–(1,5) → `NoMatch`，新版选 (3,4)–(3,5) → `LevelClear`。

## shell 脚本检查（`make lint-sh`）

`make check` 第一步跑 `make lint-sh`（`python3 web/tools/lint-sh.py`），检查仓库里（`git ls-files`）全部 `*.sh`、`*.mk` 与 `Makefile`。

注意两处盲区：

- **只扫已跟踪的文件**：文件清单来自 `git ls-files`，新建但还没 `git add` 的脚本不在清单里，`make lint-sh` 不会扫它（照样显示 0 处问题）。
  提交前先 `git add`，或者直接把路径传给脚本：`python3 web/tools/lint-sh.py 新脚本.sh`（传了路径就只查这些文件，不管是否已跟踪）。
- **只按扩展名选文件**：没有扩展名的脚本（例如 Gradle 生成的 `web/android-app/android/gradlew`、自己写的无后缀可执行脚本）不在范围内，
  即使带 `#!/bin/bash` 也不查；需要时同样用路径参数单独查。

规则与细节：


- **规则**：`$` 后接命名变量 `[A-Za-z_][A-Za-z0-9_]*`，紧跟一个 ≥ 0x80 的字节（中文、全角标点等 UTF-8 字符的首字节）就报错，
  逐条打印 `文件:行号: $变量 …: 该行`，退出码 1。修法是加花括号：`"…$LABEL）"` → `"…${LABEL}）"`。
  Makefile 配方里的 `$$v，` 就是 shell 的 `$v，`，同样报（改成 `$${v}，`）。位置参数 / 特殊参数（`$1`、`$?`、`$@` 等）不查。
- **为什么**：macOS 自带的 bash 3.2 在 UTF-8 区域设置下，libc 把 0x80–0xFF 字节当成字母，全角字符的字节会被读进变量名
  （`$LABEL）` 被读成变量 `LABEL\xef…`），配合 `set -u` 直接报 `unbound variable` 退出；Linux 上的 bash 5 / glibc 不会这样，
  本机跑不出问题，所以靠这条静态检查。
- **严格、无白名单**：注释行也查（注释里写成 `${VAR}` 即可）。只按文件名选文件、不看 shebang——Gradle 生成的
  `web/android-app/android/gradlew` 不是手写脚本，不在范围内。
- **单独查几个文件**：`python3 web/tools/lint-sh.py 文件 …`。
- **在 Linux 上复现 macOS 行为**（可选）：用 bash 3.2.57 源码编译的 bash，加 `LD_PRELOAD` 一个把 Latin-1 0xC0–0xFF 等标成
  alpha / alnum 的 `__ctype_b_loc`（模拟 macOS libc），`LC_ALL=C.UTF-8`、`set -u` 下执行含 `$VAR）` 的行即可看到报错。
