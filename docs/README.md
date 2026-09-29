# 设计文档索引

本目录为 **Match-3 消消乐** 的中文设计说明，依据仓库当前 `src/` / `app/` 实现撰写，供评审与上手阅读。竞品「开心消消乐」仅作背景对照，不伪造本仓库没有的机制。

| 文档 | 内容 |
|------|------|
| [architecture.md](architecture.md) | 分层架构、模块地图、依赖方向、Stack / GHC |
| [domain.md](domain.md) | 领域词汇中英对照（与 `Types` / `GameState` 对齐） |
| [rules-pipeline.md](rules-pipeline.md) | `trySwap` / 稳定化 / 连锁波次流水线（据实描述代码） |
| [testing.md](testing.md) | 如何跑测、覆盖面、合并门禁 |
| [ui-controls.md](ui-controls.md) | SDL 键位与道具点选流（前端） |
| [ui-art.md](ui-art.md) | 美术风格、颜色→形状对照、障碍图例、贴图生成与加载降级 |

根目录 [`README.md`](../README.md) 是玩家向上手说明；本目录偏实现与规则。许可证见 [`LICENSE`](../LICENSE)（法律原文不译）。
