# 设计文档索引

本目录为 **Match-3 消消乐** 的中文设计说明，依据仓库当前 `src/` / `app/` 实现撰写，供评审与上手阅读。竞品「开心消消乐」仅作背景对照，不伪造本仓库没有的机制。

| 文档 | 内容 |
|------|------|
| [architecture.md](architecture.md) | 分层架构、[模块地图](architecture.md#模块地图)（核心库 `Match3.Board.*` / `Match3.Game.*` 子模块、通用层 `Engine.*`、前端 `app/Shell` / `app/UI/*`）、依赖方向（含 `trace*` → `ComboFx` → `UI.Playback` → `UI.Cascade`）、[逐轮回放与规则的同步](architecture.md#逐轮回放与规则的同步)、[专门分支的收编（段 4）](architecture.md#专门分支的收编段-4)、Stack / GHC |
| [architecture.md § 多游戏接口](architecture.md#多游戏接口) | 通用层 / 三消实现 / SDL 外壳的分层图、`Game` / `Step` / `Effect` / `Stages` / `Player` / `Plugin` 字段说明、record-of-functions 的取舍与种子约定、三消动作映射、接入新游戏的步骤清单 |
| [domain.md](domain.md) | 领域词汇中英对照（与 `Types` / `GameState` 对齐），含[回放与表现](domain.md#回放与表现)词条 |
| [rules-pipeline.md](rules-pipeline.md) | `trySwap` / 稳定化 / 连锁波次流水线（据实描述代码）；[与前端的边界](rules-pipeline.md#8-与前端的边界)：`MoveFx`、`MoveTrace`、`mtEnd` |
| [testing.md](testing.md) | 如何跑测、覆盖面、合并门禁（含截图 AE=0 比对）；[逐轮回放护栏](testing.md#逐轮回放护栏)（`trace_*` / `trace_end_*`、比对底线、洗牌步缺口）；[行为金标准](testing.md#行为金标准golden)；[多游戏接口验收](testing.md#多游戏接口验收第三刀) |
| [ui-controls.md](ui-controls.md) | SDL 键位与道具点选流（前端 `app/UI/Input.hs` / `Actions.hs`）；回放加速键与[播放锁定](ui-controls.md#播放锁定animbusy) |
| [web.md](web.md) | 网页版（GHC wasm 技术验证）：wasm 核心与导出接口、ComboFx 进 wasm、JS 渲染器、自适应布局、资源管线、构建 / 本地与局域网运行 / Mac 与 itch.io 部署、一致性测试与 e2e、已知限制与 TODO |
| [android.md](android.md) | 安卓版：Capacitor WebView 包装网页版、前置（JDK / SDK / Node）、`make apk` / `apk-release` / `aab`、装机、签名与 Google Play、验证、已知限制、iOS 说明 |
| [refactor-2026-09.md](refactor-2026-09.md) | 2026-09 的 11 刀重构总结：各刀 SHA 与要点、验收方式、行为差异汇总、现在怎样新增元素 / 关卡级元素 / 关卡 / 规则 / 表现 |
| [ui-art.md](ui-art.md) | 美术风格、颜色→形状对照、障碍图例、贴图生成与加载降级 |
| [ui-art.md § 连击表现](ui-art.md#连击表现逐轮回放) | 连锁逐轮回放时间线、[步末阶段（PhEnd）](ui-art.md#步末阶段phend)、[连击等级样式](ui-art.md#连击等级样式combostyle)、截图与复现 seed |

根目录 [`README.md`](../README.md) 是玩家向上手说明（含目录结构）；根目录 `Makefile` 收了常用任务（`make help`：桌面版 `desktop-build` / `run` / `test-native`，网页版构建、测试、部署，安卓 `apk` 等，详见 [web.md § make 目标](web.md#31-make-目标) 与 [android.md § 构建](android.md#3-构建)）；本目录偏实现与规则。源码按职责分在 `src/Engine/`（多游戏通用层）、`src/Match3/Board/`、`src/Match3/Game/`、`src/Match3/Engine.hs`（三消实例）与 `app/Shell/`（通用外壳）、`app/UI/`（三消插件），对应关系见 architecture.md。许可证见 [`LICENSE`](../LICENSE)（法律原文不译）。
