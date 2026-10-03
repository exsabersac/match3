# 现状与待办（2026-10）

> **状态（2026-10-03）**：**玩法冻结**，不再加新玩法、新元素、新关卡。本文件原是 2026-09-30 解冻时的新玩法清单；清单里第 1–7、9 项已经做完（第 41–48 关），其余几项随冻结不做。现在在做的是代码审计后的重构整改（只改结构与文档，行为不变）。

## 1. 现有内容

- **关卡**：49 关（下标 0–48，`Match3.Levels.Campaign.levelCount`）。第 1–40 关是原有关卡（第 39 关果冻、第 40 关气泡）；第 41–48 关各展示一个新玩法（见下表）；第 49 关「宽域」是唯一的 6×9 矩形盘面，其余都是 8×8。
- **宝石与特效**：5 色宝石；直线（横 / 竖）、炸弹、彩虹（魔力鸟）。形状表：五连 → 彩虹、四连 → 直线；打开规则开关 `bomb_shapes` 的关（第 41 关）另有 L / T 形 → 炸弹。组合表：直线 × 直线十字、直线 × 炸弹三行三列、炸弹 × 炸弹 5×5、彩虹 × 彩虹清全盘；打开 `rainbow_combos` 的关（第 44 关）彩虹 × 直线 / 炸弹先把同色宝石变成该特效再引爆，其余关只清同色。
- **元素**：36 个内置条目（`Element.Builtin.builtinDefs`）：宝石与 4 种特效（`gem` / `line_h` / `line_v` / `bomb` / `rainbow`）、冰层、8 种叠层（草、藤蔓、巧克力、迷雾、锁链、冰冻、窗帘、蒸汽）、地面层（果冻、魔法地格），以及石头、宝箱、蜂蜜、气球、饼干、蛋糕、魔法帽、果汁机、蜗牛、保险箱、双面块、彩蛋、染色瓶、时间精灵、倒计时炸弹、气泡、魔法石、毛球、雪怪 Boss、变色龙等占格本体。
- **关卡级元素**：飞碟、皮带、传送门、地毯、L / T 炸弹开关、魔力鸟组合开关、饼干掉落口（`builtinLevelDefs`），另有核心地面层。
- **其他**：锤子 / 自由交换 / 十字三种道具；每日挑战 10 种目标轮换；桌面音效与 BGM（K / B 开关）。
- **前端**：桌面 SDL 版；网页版（GHC wasm，只接了交换、撤销、提示、切关、重开，见 [web.md §8–9](web.md#8-已知限制)）；安卓 APK（Capacitor 套网页版，见 [android.md](android.md)）。

## 2. 2026-09-30 新玩法清单的结果

| 原清单 | 结果 |
|--------|------|
| 1. L / T 形 → 炸弹 | 已完成：新玩法 1，规则开关 `bomb_shapes`，第 41 关「爆破」 |
| 2. 魔法石 | 已完成：新玩法 2，`Custom "magic_stone"`，第 42 关「魔石」 |
| 3. 毛球 | 已完成：新玩法 3，`Custom "fuzzball"`，第 43 关「毛球」 |
| 4. 雪怪 Boss | 已完成：新玩法 5，`Custom "snow_boss"`（2×2，含召唤雪块），第 45 关「雪怪」 |
| 5. 魔力鸟组合增强 | 已完成：新玩法 4，规则开关 `rainbow_combos`，第 44 关「魔力鸟」 |
| 6. 掉落口 | 已完成：新玩法 6，关卡级元素 `CookieDrop`（`lvlDrops`），第 46 关「掉落口」 |
| 7. 变色龙 | 已完成：新玩法 7，`Custom "chameleon"`，第 47 关「变色龙」 |
| 8. 过关剩余步数奖励 | 不做（玩法冻结） |
| 9. 魔法地格 | 已完成：新玩法 8，地面层 `magic`，第 48 关「魔法格」 |
| 10. 流沙 | 不做（玩法冻结） |
| 11. 海洋生物 | 不做（玩法冻结） |
| 12. 栏杆 / 银币 / 限时关 | 不做（玩法冻结） |

各玩法的规则与验收见 [domain.md](domain.md) 与 [testing.md「新玩法验收」](testing.md#新玩法验收)。

## 3. Haskell 特性展示

第 1–9 项已完成并合入（[haskell-features/](haskell-features/) 01–09）；第 10 项已取消。

## 4. 正在做：审计整改（2026-10）

按代码审计报告的顺序逐项做，每项单独一条分支、单独验收（0 警告构建含 `match3-sdl`、`stack test` 全过、`test/golden` 金标准不变）：

| 项 | 内容 | 状态 |
|----|------|------|
| 1 | 选格散列 `boardSeed`（`show board` 的 FNV-1a）抽成一份并用测试钉住 | 已完成（`fix/board-seed`），测试中 |
| 2 | 删掉没人用的兼容包装与导出；删除 `Spec.Support.Legacy*` 旧副本，有价值的对照改成固定例子 | 已完成（`refactor/dead-code`），测试中 |
| 3 | 收掉被 pragma 关掉的警告（`Anim` 部分字段、`Spec.Phase` 推迟名字错误）与多余的 `array` 依赖 | 已完成（`refactor/warnings-deps`），测试中 |
| 4 | 文档脱节（本文件、测试数、`cells.js` 说法、testing.md 长句、模块地图） | 已完成（`docs/refresh`），测试中 |
| 5 | 桌面道具动作去重，文案表移进 `app/pure` 并加测试 | 已完成（`refactor/desktop-boosters`），测试中 |
| 8 | JS 颜色 / 蔓延表与 Haskell 表现表的一致性护栏 | 进行中（`refactor/js-color-parity`）；比对时发现桌面 `elementRGBTable` 缺 `magic_stone` / `fuzzball`（网页有），已按网页补上，两边现在逐项相同 |
| 6 | 拆分 `Match3.Core`：前端 API 与测试入口分开 | 待做 |
| 7 | 终局类型 `Terminal`（`gsOver :: Maybe Terminal`） | 待做 |
| 9 | 清理「第 N 刀 / 第 N 项前」历史注释 | 随各项顺手做（只清碰到的模块） |
| 10–12 | `Legacy*` 拆分、`Modifier` 改能力记录、合并 `*Prim` / `*Art` | 10 已并入第 2 项；11、12 按审计建议不做 |

## 5. 其他未完成

- **Mac 同步**：把 Mac 上的仓库与网页部署（`web/deploy-mac.sh`）同步到最新 main（拉取后重新 `make build`，或装新的 dist 包）；需要在 Mac 上操作。
- **APK 体积**：[android.md](android.md) 里的产物大小是 2026-10-03 `fix/web-audio-toggle` 时实测的，`web/dist` 之后重建过（`feat/dist-rebuild`），需要重新 `make build apk` 后更新数字。
- **APK 真机 / 模拟器验证**：APK 还没有在模拟器或真机上跑过（[android.md](android.md)）；iOS 壳需要装了 Xcode 的 Mac。
- **网页版 TODO**：道具与洗牌按钮、每日挑战与选关地图、真机测试、资源文件名带哈希、itch.io 上线、CI 等，见 [web.md §9](web.md#9-todo)。这些是前端接入，不加新玩法。
- **调色板的第三份副本未受护栏**：`tools/gen_assets.py` 的五色调色板与 `UI.Palette.colorRGB` / `cells.js` 的 `COLOR_RGB` 应一致，但不在 `test/Spec/WebColors.hs` 的比对范围内（改主色时三处要一起改）。
