# Match-3 消消乐（Haskell + SDL2）

8×8、5 色可玩 Match-3，对标开心消消乐常见机制：特殊块、多层障碍、多样目标、每日挑战。纯规则在 library（`Match3.Core`），SDL 前端为 `match3-sdl`。

Playable 8×8 / 5-color match-3 inspired by Happy Match (开心消消乐). Pure rules in `Match3.Core`; SDL2 frontend is `match3-sdl`.

## 30 秒上手 / 30-second start

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
sudo apt-get install -y libsdl2-dev   # once
stack build && stack exec match3-sdl
```

1. **左键**点两格相邻交换，或**拖拽**到相邻格；无三连会回滚  
2. 开局底部有键位条；**P** 暂停看完整键位（H 提示 / U 撤销 / S 洗牌 / D 每日 / R 重开 / N 过关）  
3. 第一关会短暂黄框提示可消一手；达目标后按 **N** / 空格 / 点击继续  

Need `libSDL2` at runtime (`libsdl2-2.0-0`). Headless: `xvfb-run -a stack exec match3-sdl`.

## Features / 功能

- Adjacent swaps (click or drag); horizontal/vertical ≥3 clear
- **Specials**: 4-match → Line; 5-match → Rainbow (clear all of a color); Bomb exists for combos
- **Line×Bomb (3×3 cross), Rainbow×Line, Bomb×Bomb (5×5), Line×Line (row+col)
- **Stone crates**: layered blockers (`Stone n`); adjacent clears chip; last layer removes
- **Ice**: layers on gems; match chips ice; last layer clears the gem; crack lines in UI
- **Conveyor belts** (传送带): cyclic `Belt` paths; shift after move; may trigger cascades
- **Countdown bombs** (倒计时炸弹): colored timers; tick −1 after each move; at 0 explode 3×3; match/special disarms
- **Goals**: score / single collect / multi-color collect / clear stones
- **Daily challenge** (`D`): date-seeded board + rotating goal; **star rating** on clear
- Combo scoring, hint, undo, auto-shuffle, 9 campaign levels
- HUD meters, particles, swap/fall tweens, pause help, CLEAR/WIN/LOSE overlays

## Levels / 关卡

| # | Name | Moves | Goal |
|---|------|-------|------|
| 1 | 入门 | 30 | Score 300 |
| 2 | 采红 | 30 | Collect 20× RED |
| 3 | 热身 | 26 | Score 500 |
| 4 | 采蓝 | 26 | Collect 22× BLUE |
| 5 | 进阶 | 22 | Score 700 |
| 6 | 采绿 | 24 | Collect 26× GREEN |
| 7 | 双采 | 28 | Collect RED 12 + BLUE 12 |
| 8 | 碎石 | 26 | Destroy 8 stones |
| 9 | 大师 | 18 | Score 1100 |

Press `D` for a **每日** daily run (seed from calendar date).

## Controls / 操作

| Key | Action |
|-----|--------|
| Left click / drag | Adjacent swap |
| `H` | Hint |
| `U` | Undo |
| `S` | Shuffle |
| `D` | Daily challenge |
| `N` / Space / Enter | Next / retry after overlay |
| `R` | Restart level |
| `P` | Pause + key help |
| `Esc` / `Q` | Quit |

Special look: white+gold bar = line; multi-color ring = rainbow; black/yellow+red ring = bomb; gray rock = stone (layer pips); cyan frame + cracks = ice; dark fuse + turn pips = countdown bomb.

## 对标开心消消乐 / Feature map

| 开心消消乐 | 本项目 |
|-----------|--------|
| 三消 / 四消横竖线 / 五消彩色精灵 | ✅ Normal / LineH·V / Rainbow |
| 炸弹与特殊合成 | ✅ Bomb；Line×Bomb / Rainbow×Line / Bomb×Bomb / Line×Line |
| 箱子 / 多层障碍 | ✅ `Stone n`（邻消削层） |
| 冰层 | ✅ gem 上 ice；裂纹绘制 |
| 倒计时炸弹 | ✅ `Countdown`：步末−1、归零 3×3、匹配解除 |
| 收集动物 / 多目标 | ✅ GoalCollect / GoalCollectMulti / GoalClearStone |
| 每日挑战 / 三星 | ✅ Daily + starRating |
| 传送带 | ✅ `Belt` 步末循环移位，可触发新消 |
| 道具（锤子等） | ⏳ 计划中 |

## Build & test

```bash
stack build && stack test && stack exec match3-sdl
```

## Layout

```
src/Match3/  Types Board Game Core Obstacles Rainbow Combos Ice Daily Countdown Conveyor
app/Main.hs  SDL2 frontend
test/Spec.hs tasty (43 named cases)
```

Frozen rule API shapes: `trySwap` / `runMove` / `ensurePlayable` / `shuffleGame` / `Outcome` / `GoalCollect`.
