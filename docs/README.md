# 设计文档索引

本目录为 **Match-3 消消乐** 的中文设计说明，依据仓库当前 `src/` / `app/` 实现撰写，供评审与上手阅读。竞品「开心消消乐」仅作背景对照，不伪造本仓库没有的机制。

| 文档 | 内容 |
|------|------|
| [architecture.md](architecture.md) | 分层架构、[模块地图](architecture.md#模块地图)（核心库 `Match3.Board.*` / `Match3.Game.*` 子模块与外观、前端 `app/UI/*`）、依赖方向（含 `trace*` → `ComboFx` → `UI.Playback` → `UI.Cascade`）、[逐轮回放与规则的同步](architecture.md#逐轮回放与规则的同步)、Stack / GHC |
| [domain.md](domain.md) | 领域词汇中英对照（与 `Types` / `GameState` 对齐），含[回放与表现](domain.md#回放与表现)词条 |
| [rules-pipeline.md](rules-pipeline.md) | `trySwap` / 稳定化 / 连锁波次流水线（据实描述代码）；[与前端的边界](rules-pipeline.md#8-与前端的边界)：`MoveFx`、`MoveTrace`、`mtEnd` |
| [testing.md](testing.md) | 如何跑测、覆盖面、合并门禁；[逐轮回放护栏](testing.md#逐轮回放护栏)（`trace_*` / `trace_end_*`、比对底线、洗牌步缺口） |
| [ui-controls.md](ui-controls.md) | SDL 键位与道具点选流（前端 `app/UI/Input.hs` / `Actions.hs`）；回放加速键与[播放锁定](ui-controls.md#播放锁定animbusy) |
| [ui-art.md](ui-art.md) | 美术风格、颜色→形状对照、障碍图例、贴图生成与加载降级 |
| [ui-art.md § 连击表现](ui-art.md#连击表现逐轮回放) | 连锁逐轮回放时间线、[步末阶段（PhEnd）](ui-art.md#步末阶段phend)、[连击等级样式](ui-art.md#连击等级样式combostyle)、截图与复现 seed |

根目录 [`README.md`](../README.md) 是玩家向上手说明（含目录结构）；本目录偏实现与规则。源码按职责分在 `src/Match3/Board/`、`src/Match3/Game/`（`Board.hs` / `Game.hs` 为再导出外观）与 `app/UI/`，对应关系见 architecture.md。许可证见 [`LICENSE`](../LICENSE)（法律原文不译）。
