# Match-3 消消乐（Haskell + SDL2）

8×8、5 色可玩 Match-3。纯规则在 library（`Match3.Core`），SDL 前端为 `match3-sdl`。

Playable 8×8 / 5-color match-3. Pure rules live in the library (`Match3.Core`); the SDL2 frontend is `match3-sdl`.

## Features / 功能

- Adjacent 4-neighbor swaps; horizontal/vertical same-color runs ≥3 clear (no L/T shapes)
- 相邻四邻交换；横/竖同色 ≥3 消除（无 L/T）
- No-match rollback; success runs Clear → Fall → Refill until stable
- 无匹配回滚；成功则 Clear → Fall → Refill 至稳定
- **Specials**: 4-match → line clearer (row/col); 5-match → bomb (3×3)
- **特殊块**：四连 → 直线（横/竖清行/列）；五连 → 炸弹（3×3）
- **Combo scoring**: cascade wave *n* scores `cells × 10 × n`
- **连击计分**：第 *n* 波连锁按 `格数 × 10 × n`
- **No-move detect + shuffle**: auto-reshuffle to a stable playable board (no initial three-in-a-row); manual `S`
- **无解检测 + 洗牌**：自动洗成无初始三连且有解的棋盘；也可按 `S`
- **Levels**: 5 stages, rising target / tighter move budget; press `N` after clear
- **多关卡**：5 关逐步提高目标分 / 压缩步数；过关按 `N`
- Hint `H`, undo `U`, restart level `R`
- Selection pulse, hint outline, clear flash, swap/fall tweens
- 选中脉冲、提示黄框、消除闪白、交换/下落补间
- In-window HUD: bitmap digits for level/score/moves, meters, combo badge, status strip
- 局内 HUD：点阵数字显示关卡/分数/步数、进度条、连击角标、状态色条

## Levels / 关卡

| # | Name 名称 | Moves 步数 | Target 目标分 |
|---|-----------|------------|---------------|
| 1 | 入门 Beginner | 30 | 300 |
| 2 | 热身 Warm-up | 28 | 450 |
| 3 | 进阶 Intermediate | 25 | 650 |
| 4 | 高手 Expert | 22 | 850 |
| 5 | 大师 Master | 20 | 1100 |

Reach the target to clear the level (`LevelClear` → press `N`). Finish level 5 to win the campaign. Running out of moves loses the level (`R` to retry).

达目标分过关（按 `N` 进下一关）。打完第 5 关通关。步数耗尽失败（`R` 重开本关）。

## Controls / 操作

| Key / 键 | Action / 作用 |
|----------|----------------|
| Left click / 鼠标左键 | Select two adjacent cells to swap / 点两格相邻交换 |
| `H` | Hint one matching swap / 提示一步可消除的交换 |
| `U` | Undo last move / 撤销上一步 |
| `S` | Shuffle board (stable + playable) / 洗牌（无初始三连且有解） |
| `N` | Next level after clear / campaign restart after win / 过关后下一关；通关后重开战役 |
| `R` | Restart current level / 重开当前关 |
| `Esc` / `Q` | Quit / 退出 |

HUD: green bar ≈ score progress, blue bar ≈ moves left; right strip green=win / yellow=level clear / red=lose / purple=just auto-shuffled. Combo badge shows `xN` after multi-wave cascades.

HUD：绿条≈分数进度，蓝条≈剩余步数；右侧色条绿=通关 / 黄=过关 / 红=失败 / 紫=刚自动洗牌。多波连锁后显示连击 `xN`。

Special look: white horizontal bar = row clear; white vertical bar = column clear; black square + yellow core = bomb.

特殊块外观：白横条=横清；白竖条=竖清；黑心黄芯=炸弹。

## Rules summary / 规则摘要

| Item | Rule |
|------|------|
| Board 棋盘 | 8×8 |
| Colors 颜色 | 5 |
| Swap 交换 | Up/down/left/right only 仅上下左右 |
| Match 匹配 | Horizontal or vertical run ≥3（对角与 2 连不算） |
| Score 计分 | `cells × 10 × wave` per cascade wave |
| Win/lose 胜负 | Reach target → clear; 0 moves → lose |

## Environment / 环境

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
sudo apt-get install -y libsdl2-dev   # SDL2 headers
```

- Stack + snapshot `lts-21.25`（GHC 9.4.8）
- If hsc2hs reports `cannot find ld` (`-fuse-ld=gold`):  
  `sudo ln -sf /usr/bin/ld.bfd /usr/bin/ld.gold`

## Build & run / 构建与运行

```bash
stack build
stack test
stack exec match3-sdl
```

Headless / 无显示器：

```bash
xvfb-run -a stack exec match3-sdl
```

## CI

GitHub Actions runs `stack test` on push/PR (`.github/workflows/ci.yml`).

## Layout / 工程结构

```
src/Match3/   Types Board Game Core（纯规则）
app/Main.hs   SDL2 frontend（HUD / tweens）
test/Spec.hs  tasty invariants + specials / hint / undo / shuffle / combo
```

Dependency direction: `App → Core` (one way).
