# 设计文档索引

本目录为 **Match-3 消消乐** 的中文设计说明，依据仓库当前 `src/` / `app/` 实现撰写，供评审与上手阅读。竞品「开心消消乐」仅作背景对照，不伪造本仓库没有的机制。

| 文档 | 内容 |
|------|------|
| [architecture.md](architecture.md) | 分层架构、[模块地图](architecture.md#模块地图)（核心库 `Match3.Board.*` / `Match3.Game.*` 子模块、通用层 `Engine.*`、前端 `app/Shell` / `app/UI/*`）、依赖方向（含 `trace*` → `ComboFx` → `UI.Playback` → `UI.Cascade`）、[逐轮回放与规则的同步](architecture.md#逐轮回放与规则的同步)、[专门分支的收编（段 4）](architecture.md#专门分支的收编段-4)、Stack / GHC |
| [architecture.md § 多游戏接口](architecture.md#多游戏接口) | 通用层 / 三消实现 / SDL 外壳的分层图、`Game` / `Step` / `Effect` / `Stages` / `Player` / `Plugin` 字段说明、record-of-functions 的取舍与种子约定、三消动作映射、接入新游戏的步骤清单 |
| [domain.md](domain.md) | 领域词汇中英对照（与 `Types` / `GameState` 对齐），含[回放与表现](domain.md#回放与表现)词条 |
| [rules-pipeline.md](rules-pipeline.md) | `trySwap` / 稳定化 / 连锁波次流水线（据实描述代码）；[与前端的边界](rules-pipeline.md#8-与前端的边界)：`MoveFx`、`MoveTrace`、`mtEnd` |
| [testing.md](testing.md) | 如何跑测、[开发流程](testing.md#开发流程)（`make verify`、合 main、`web/dist` 只在部署前重建）、覆盖面、合并门禁（含截图 AE=0 比对）；[逐轮回放护栏](testing.md#逐轮回放护栏)（`trace_*` / `trace_end_*`、比对底线、洗牌步缺口）；[行为金标准](testing.md#行为金标准golden)；[多游戏接口验收](testing.md#多游戏接口验收第三刀) |
| [ui-controls.md](ui-controls.md) | SDL 键位与道具点选流（前端 `app/UI/Input.hs` / `Actions.hs`）；回放加速键与[播放锁定](ui-controls.md#播放锁定animbusy) |
| [web.md](web.md) | 网页版（GHC wasm 技术验证）：wasm 核心与导出接口、ComboFx 进 wasm、JS 渲染器、自适应布局、资源管线、构建 / 本地与局域网运行 / Mac 与 itch.io 部署、一致性测试与 e2e、已知限制与 TODO |
| [android.md](android.md) | 安卓版：Capacitor WebView 包装网页版、前置（JDK / SDK / Node）、`make apk` / `apk-release` / `aab`、装机、签名与 Google Play、验证、已知限制、iOS 说明 |
| [haskell-features/01-类型层.md](haskell-features/01-类型层.md) | Haskell 特性展示第 1 项：DataKinds / GADTs / 类型族，给盘面一步棋的阶段贴类型标签（`Stage p`），非法阶段转换编译不过；行为不变 |
| [haskell-features/02-类型类与抽象.md](haskell-features/02-类型类与抽象.md) | Haskell 特性展示第 2 项：DerivingStrategies / GND（名字 newtype、盘面容器的实例来源）、DefaultSignatures（Int newtype 元素的缺省 `toCell`）、存在类型与 xmonad 风格元素类的整理、盘面 `Grid` 的 Functor / Foldable / Traversable 加 Monoid 统计；行为不变 |
| [haskell-features/03-效果与架构.md](haskell-features/03-效果与架构.md) | Haskell 特性展示第 3 项：纯核心 + 可替换效果——连锁写成只依赖能力类（补子 / 关卡级钩子 / 发出回放）的程序，纯解释器、追踪解释器（事件日志）、测试里的手写 free monad 三种跑法；与 free / operational 风格对比；行为不变 |
| [haskell-features/04-惰性与递归模式.md](haskell-features/04-惰性与递归模式.md) | Haskell 特性展示第 4 项：惰性与递归模式——拒绝采样与自动洗牌的有限重试写成惰性无穷流（`Engine.Stream`），回放器与计数表的严格性修正（bang pattern / `foldl'`，附内存实验），测试里手写 `Fix` / `cata` / `ana` / `hylo` 对照回放器；行为不变 |
| [haskell-features/05-性能与并发.md](haskell-features/05-性能与并发.md) | Haskell 特性展示第 5 项：性能与并发——先剖析再改：匹配扫描先投影成 unboxed 的「匹配码」数组（每格只问一次注册表，提示搜索不再复制盘面），重力在 `runSTArray` 里就地压实，测试里的金标准用 `forkIO` + STM 并行求值、按原顺序拼回；附剖析表与实测数据（含没有收益的尝试）；行为不变 |
| [haskell-features/06-测试与光学.md](haskell-features/06-测试与光学.md) | Haskell 特性展示第 6 项：测试与光学——手写 van Laarhoven 透镜 / 遍历 / 棱镜（`Engine.Optics`，零依赖），把叠层代码里四份揭层、三份蔓延的复制合成一份（`Match3.Grass`），GameState 的字段与派生读数成为透镜；QuickCheck 检查透镜 / 遍历 / 棱镜定律、规则不变量（自定义 `Arbitrary` + shrink）与撤销历史的状态机模型；附变异检查；行为不变 |
| [haskell-features/07-网格几何.md](haskell-features/07-网格几何.md) | Haskell 特性展示第 7 项：网格几何——四个正交方向是和类型 `Dir`（`stepDir` / `dirBetween`），仓库原有的四种邻格顺序（上下左右 / 上左右下 / 右下左上 / 右下）各起名字；`Grid` 上带坐标的折叠 `ifoldMap` / `ifoldr` / `positionsWhere`（补上 `Foldable` 不给坐标的缺口）；UI 边界的像素 `PxX` / `PxY` 与每日挑战 `Year` / `Month` / `Day` newtype；与旧代码逐字副本的 QuickCheck 对照与变异检查；不含模式同义词；行为不变 |
| [haskell-features/08-数据边界.md](haskell-features/08-数据边界.md) | Haskell 特性展示第 8 项：数据边界——关卡数据的 Applicative 校验（手写 `Validation`，只有 Applicative、不能是合法的 Monad，皮带 / 传送门 / 飞碟 / 地毯 / 地面层 / 掉落口 / 放置表的 `LevelIssue` 一次报全，只收现有关卡都满足的不变量）；放置参数的 Applicative / Alternative 解析器 `ArgP`（精确匹配 `exactArgs` 与前缀匹配 `prefixArgs` 分开，12 个放置函数逐个保持原语义）；测试里用 `GHC.Generics` 的 `conName` + DeriveAnyClass 列出全部构造器，检查生成器 / 注册表 / `cellFace` / 桌面绘制表 / 网页结局编码的覆盖；`beats` 的分组改为 `NonEmpty`（`Belt` 因 `Show` 进金标准而不改）；与旧代码逐字副本的 QuickCheck 对照与变异检查；行为不变 |
| [haskell-features/09-规则去重.md](haskell-features/09-规则去重.md) | Haskell 特性展示第 9 项：规则去重——占格障碍的五份邻消揭层合成一个（棱镜 `_Stone` … 作参数，保险箱末层变饼干是另一个参数）、改色是遍历 `cellColorT`；步末规则的三份 `foldl` + `reverse` 与邻格波及的四元组折叠改为 `mapAccumL`（`runEndRules`）；能力声明 `Cap` 的拼接顺序由 `Dual (Endo Caps)`（DerivingVia）给出、字段写入器经透镜；步末规则按阶段的智能构造器；与旧代码逐字副本的 QuickCheck 对照与变异检查；行为不变 |
| [refactor-2026-09.md](refactor-2026-09.md) | 2026-09 的 11 刀重构总结：各刀 SHA 与要点、验收方式、行为差异汇总、现在怎样新增元素 / 关卡级元素 / 关卡 / 规则 / 表现 |
| [backlog.md](backlog.md) | 现状与待办（2026-10，玩法冻结）：现有关卡 / 元素盘点、2026-09-30 新玩法清单的结果、审计整改进度、其余未完成事项（Mac 同步、网页版 TODO） |
| [ui-art.md](ui-art.md) | 美术风格、颜色→形状对照、障碍图例、贴图生成与加载降级 |
| [ui-art.md § 连击表现](ui-art.md#连击表现逐轮回放) | 连锁逐轮回放时间线、[步末阶段（PhEnd）](ui-art.md#步末阶段phend)、[连击等级样式](ui-art.md#连击等级样式combostyle)、截图与复现 seed |

根目录 [`README.md`](../README.md) 是玩家向上手说明（含目录结构）；根目录 `Makefile` 收了常用任务（`make help`：桌面版 `desktop-build` / `run` / `test-native`，网页版构建、测试、部署，安卓 `apk` 等，详见 [web.md § make 目标](web.md#31-make-目标) 与 [android.md § 构建](android.md#3-构建)）；本目录偏实现与规则。源码按职责分在 `src/Engine/`（多游戏通用层）、`src/Match3/Board/`、`src/Match3/Game/`、`src/Match3/Engine.hs`（三消实例）与 `app/Shell/`（通用外壳）、`app/UI/`（三消插件），对应关系见 architecture.md。许可证见 [`LICENSE`](../LICENSE)（法律原文不译）。
