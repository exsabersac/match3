# Match-3 消消乐（Haskell + SDL2）

8×8、5 色可玩 Match-3。纯规则在 library（`Match3.Core`），SDL 前端为 `match3-sdl`。

## 功能

- 相邻四邻交换；横/竖同色 ≥3 消除（无 L/T）
- 无匹配回滚；成功则 Clear → Fall → Refill 至稳定
- **特殊块**：四连 → 直线（横/竖清行/列）；五连 → 炸弹（3×3）
- **多关卡**：5 关逐步提高目标分 / 压缩步数；过关按 `N`
- **提示** `H`、**撤销** `U`、**重开本关** `R`
- 选中脉冲高亮、提示黄框、消除闪白轻动画
- 窗口标题显示关卡 / 分数 / 步数 / 提示

## 环境

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
sudo apt-get install -y libsdl2-dev   # 需要 SDL2 头文件
```

- Stack + snapshot `lts-21.25`（GHC 9.4.8）
- 若 hsc2hs 报 `cannot find ld`（`-fuse-ld=gold`）：`sudo ln -sf /usr/bin/ld.bfd /usr/bin/ld.gold`

## 构建与运行

```bash
stack build
stack test
stack exec match3-sdl
```

无显示器时可用：

```bash
xvfb-run -a stack exec match3-sdl
```

## 操作

| 键/操作 | 作用 |
|---------|------|
| 鼠标左键 | 点两格相邻交换 |
| `H` | 提示一步可消除的交换 |
| `U` | 撤销上一步 |
| `N` | 过关后进入下一关 / 通关后重开战役 |
| `R` | 重开当前关 |
| `Esc` / `Q` | 退出 |

HUD：绿条≈分数进度，蓝条≈步数；右侧色条绿=通关胜 / 黄=过关 / 红=失败。

特殊块外观：白横条=横清；白竖条=竖清；黑心黄芯=炸弹。

## 规则摘要

| 项目 | 说明 |
|------|------|
| 棋盘 | 8×8 |
| 颜色 | 5 种 |
| 交换 | 仅上下左右 |
| 匹配 | 横或竖连续 ≥3 |
| 计分 | 每清除 1 格 +10（含特殊扩散） |
| 胜负 | 达目标分过关；步数耗尽失败 |

## 工程结构

```
src/Match3/   Types Board Game Core（纯规则）
app/Main.hs   SDL2 前端
test/Spec.hs  tasty 不变量 + 特殊块/提示/撤销
```

依赖方向：`App → Core`（单向）。
