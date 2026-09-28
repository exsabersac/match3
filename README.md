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
- **Color-collect goals**: some levels ask you to clear N gems of a color
- **收集色目标**：部分关卡要求消除 N 个指定颜色
- **No-move detect + shuffle**: auto-reshuffle to a stable playable board; manual `S`
- **无解检测 + 洗牌**：自动洗成无初始三连且有解的棋盘；也可按 `S`
- **Levels**: 7 stages mixing score + collect; press `N` / Space / Enter / click after clear
- **多关卡**：7 关混入分数 / 收集目标；过关按 `N`、空格、回车或点击
- Hint `H`, undo `U`, restart / retry `R` (or click on lose overlay)
- Selection pulse, hint outline, clear flash + particle bursts, swap/fall tweens
- 选中脉冲、提示黄框、消除闪白 + 粒子爆散、交换/下落补间
- Fullscreen outcome banners: CLEAR! / WIN! / LOSE + NEXT / RETRY
- 过关 / 通关 / 失败全屏大条提示
- In-window HUD: bitmap digits, meters (score or collect color), combo badge, status strip
- 局内 HUD：点阵数字、进度条（分数或收集色）、连击角标、状态色条
- Sound: skipped (no mixer / raw-audio dependency); easy to add later with `sdl2-mixer`
- 音效：已跳过（无合适混音依赖）；后续可用 `sdl2-mixer` 接入

## Levels / 关卡

| # | Name 名称 | Moves 步数 | Goal 目标 |
|---|-----------|------------|-----------|
| 1 | 入门 Beginner | 30 | Score 300 |
| 2 | 采红 Collect Red | 28 | Clear 20× RED (C1) |
| 3 | 热身 Warm-up | 26 | Score 500 |
| 4 | 采蓝 Collect Blue | 24 | Clear 25× BLUE (C3) |
| 5 | 进阶 Intermediate | 22 | Score 700 |
| 6 | 采绿 Collect Green | 20 | Clear 30× GREEN (C2) |
| 7 | 大师 Master | 18 | Score 1100 |

Reach the goal to clear the level (`LevelClear` → `N` / Space / Enter / click). Finish level 7 to win the campaign. Running out of moves loses (`R` / click to retry).

达目标过关（按 `N` / 空格 / 回车 / 点击）。打完第 7 关通关。步数耗尽失败（`R` / 点击重试）。

## Controls / 操作

| Key / 键 | Action / 作用 |
|----------|----------------|
| Left click / 鼠标左键 | Select two adjacent cells to swap / 点两格相邻交换；过关/失败时点屏幕继续 |
| `H` | Hint one matching swap / 提示一步可消除的交换 |
| `U` | Undo last move / 撤销上一步 |
| `S` | Shuffle board (stable + playable) / 洗牌（无初始三连且有解） |
| `N` / Space / Enter | Next level after clear / campaign restart after win / retry after lose |
| `R` | Restart current level / 重开当前关 |
| `Esc` / `Q` | Quit / 退出 |

HUD: green (or collect-color) bar ≈ goal progress, blue bar ≈ moves left; right strip green=win / yellow=level clear / red=lose / purple=just auto-shuffled. Combo badge shows `xN` after multi-wave cascades. Collect levels show a color swatch next to the meter.

HUD：绿条（或收集色条）≈目标进度，蓝条≈剩余步数；右侧色条绿=通关 / 黄=过关 / 红=失败 / 紫=刚自动洗牌。多波连锁后显示连击 `xN`。收集关在进度旁显示目标色块。

Special look: white horizontal bar = row clear; white vertical bar = column clear; black square + yellow core = bomb.

特殊块外观：白横条=横清；白竖条=竖清；黑心黄芯=炸弹。

## Rules summary / 规则摘要

| Item | Rule |
|------|------|
| Board 棋盘 | 8×8 |
| Colors 颜色 | 5 (RED/GRN/BLU/YEL/PRP) |
| Swap 交换 | Up/down/left/right only 仅上下左右 |
| Match 匹配 | Horizontal or vertical run ≥3（对角与 2 连不算） |
| Score 计分 | `cells × 10 × wave` per cascade wave |
| Goals 目标 | `GoalScore N` or `GoalCollect color N` |
| Win/lose 胜负 | Meet goal → clear; 0 moves → lose |

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

## itch.io release checklist

Ship a playable Linux (and optionally Windows) build without asking players to install Stack:

1. **Build release binary**
   ```bash
   stack build --ghc-options="-O2"
   stack exec -- which match3-sdl   # note path under .stack-work/...
   ```
2. **Bundle**
   - Copy `match3-sdl` into a folder (e.g. `match3-linux/`)
   - Include this README (or a short `HOW_TO_PLAY.txt`)
   - Document: needs `libSDL2-2.0.so.0` (`sudo apt install libsdl2-2.0-0`)
   - Optional: wrap with a `run.sh` that `cd`s to the script dir and execs the binary
3. **Smoke-test** on a clean machine / VM: open window, complete one collect + one score level, confirm overlay + `N`/`R`
4. **itch page**
   - Title: Match-3 / 消消乐
   - Genre: Puzzle
   - Screenshots: board + HUD, CLEAR banner, collect-color meter
   - Price: free / PWYW
   - Upload `.zip` of the bundle; set Linux (and Win if cross-built) as platforms
5. **Optional Windows**: cross-compile or build on Windows with Stack + SDL2; ship `SDL2.dll` next to the `.exe`
6. **Out of scope for v0.1**: audio, mobile, save slots, online leaderboard

## CI

GitHub Actions runs `stack test` on push/PR (`.github/workflows/ci.yml`) when the workflow file is present on the default branch.

## Layout / 工程结构

```
src/Match3/   Types Board Game Core（纯规则 + LevelGoal）
app/Main.hs   SDL2 frontend（HUD / tweens / particles / overlays）
test/Spec.hs  tasty invariants + specials / hint / undo / shuffle / combo / collect
```

Dependency direction: `App → Core` (one way).
