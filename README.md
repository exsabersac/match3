# Match-3 消消乐（Haskell + SDL2）

8×8、5 色可玩 Match-3，对标开心消消乐常见机制：特殊块、多层障碍、草/藤蔓/巧克力、宝箱、蜂蜜罐、气球、饼干掉落收集、传送带、倒计时炸弹、飞碟、道具点选、多样目标、每日挑战。纯规则在 library（`Match3.Core`），SDL 前端为 `match3-sdl`。

Playable 8×8 / 5-color match-3 inspired by Happy Match (开心消消乐). Pure rules in `Match3.Core`; SDL2 frontend is `match3-sdl`.

## 30 秒上手 / 30-second start

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
sudo apt-get install -y libsdl2-dev   # once
stack build && stack exec match3-sdl
```

1. **左键**点两格相邻交换，或**拖拽**到相邻格；无三连会回滚  
2. 开局底部有键位条；**P** 暂停看完整键位（H 提示 / **1** 锤子 / **2** 任意交换 / **M** 选关地图 / U 撤销 / S 洗牌 / D 每日 / R 重开 / N 过关）  
3. 第一关会短暂黄框提示可消一手；达目标后按 **N** / 空格 / 点击继续  

Need `libSDL2` at runtime (`libsdl2-2.0-0`). Headless: `xvfb-run -a stack exec match3-sdl`.

## Features / 功能

- Adjacent swaps (click or drag); horizontal/vertical ≥3 clear
- **Specials**: 4-match → Line; 5-match → Rainbow (clear all of a color); Bomb exists for combos
- **Line×Bomb (3×3 cross), Rainbow×Line, Bomb×Bomb (5×5), Line×Line (row+col)
- **Stone crates**: layered blockers (`Stone n`); adjacent clears chip; last layer removes
- **Treasure chests** (`Chest n` / 宝箱): layered gold chests; adjacent clears chip; `GoalChest`
- **Honey jars** (`Honey n` / 蜂蜜罐): amber jars; adjacent clears chip; `GoalHoney`
- **Balloons** (`Balloon c` / 气球): colored; adjacent **same-color** clear pops; `GoalBalloon`
- **Cookies** (`Cookie` / 饼干): fall with gravity; collected on the **bottom row**; `GoalCookie`
- **Ice**: layers on gems; match chips ice; last layer clears the gem; crack lines in UI
- **Grass / Vine / Chocolate** (`CellOverlay`): Grass clears on match; Vine spreads at end of move; **Choco** clears when adjacent to a match and surviving chocolate spreads (cleared choco does not)
- **Boosters**: `1` → hammer mode → click cell; `2` → free-swap mode → click two cells (any distance); limited charges; select-then-1 still works
- **Conveyor belts** (传送带): cyclic `Belt` paths; shift after move; may trigger cascades
- **Countdown bombs** (倒计时炸弹): colored timers; tick −1 after each move; at 0 explode 3×3; match/special disarms
- **Goals**: score / single collect / multi-color collect / clear stones / open chests / smash honey jars / pop balloons / collect cookies / UFO absorb
- **UFO / 飞碟**: overlay `Ufo{cell,color}`; each cascade wave `stepUfo` absorbs ortho same-color gems then relocates
- **Daily challenge** (`D`): date-seeded board + rotating goal; **star rating** on clear (3★ ≥40% moves left)
- Combo scoring, hint, undo, auto-shuffle, 24 campaign levels
- HUD meters, booster charges, particles, swap/fall tweens, vine-spread pulse hints, UFO overlays, pause help, CLEAR/WIN/LOSE overlays

## Levels / 关卡

| # | Name | Moves | Goal | Décor |
|---|------|-------|------|-------|
| 1 | 入门 | 30 | Score 300 | — |
| 2 | 采红 | 30 | Collect 20× RED | — |
| 3 | 热身 | 26 | Score 500 | — |
| 4 | 采蓝 | 26 | Collect 22× BLUE | — |
| 5 | 进阶 | 24 | Score 700 | choco |
| 6 | 冰绿 | 24 | Collect 26× GREEN | ice |
| 7 | 双采 | 28 | Collect RED 12 + BLUE 12 | — |
| 8 | 碎石 | 26 | Destroy 8 stones | stones + belt |
| 9 | 草场 | 24 | Score 600 | grass |
| 10 | 藤袭 | 22 | Collect 18× RED | vines |
| 11 | 传送 | 22 | Score 800 | grass + belt |
| 12 | 轰炸 | 18 | Score 750 | countdown bombs |
| 13 | 飞碟 | 24 | UFO absorb 10 | UFO C1 |
| 14 | 碟猎 | 20 | UFO absorb 14 | 2 UFOs + belt |
| 15 | 压力 | 18 | Multi RED/GRN/BLU | grass + choco |
| 16 | 大师 | 18 | Score 1100 | stone+grass+vine+choco+bomb+belts+UFO |
| 17 | 宝箱 | 24 | Open 6 chests | chests |
| 18 | 巧箱 | 22 | Open 5 chests | chests + choco |
| 19 | 蜂蜜 | 24 | Smash 6 honey jars | honey jars |
| 20 | 蜜压 | 22 | Smash 5 honey jars | honey + choco |
| 21 | 气球 | 24 | Pop 6 balloons | colored balloons |
| 22 | 饼干 | 24 | Collect 6 cookies | cookies high on board |
| 23 | 巧饼 | 22 | Collect 5 cookies | cookies + choco |
| 24 | 终章 | 16 | Score 1400 | stone+chest+honey+balloon+cookie+choco+vine+bomb+belt+UFO |

Press `D` for a **每日** daily run (seed from calendar date).

## Controls / 操作

| Key / Input | Action |
|-------------|--------|
| Left click / drag | Adjacent swap |
| `1` then click | Hammer mode (clear one cell); `1` again cancels |
| `2` then two clicks | Free-swap any two cells; `2` again cancels |
| Select cell then `1` | Hammer that cell (legacy shortcut) |
| `H` | Hint |
| `U` | Undo |
| `S` | Shuffle |
| `D` | Daily challenge |
| `M` | Level map (选关); click unlocked node |
| `N` / Space / Enter | Next / retry after overlay |
| `R` | Restart level |
| `P` | Pause + key help |
| `Esc` / `Q` | Quit |

Special look: white+gold bar = line; multi-color ring = rainbow; black/yellow+red ring = bomb; gray rock = stone (layer pips); gold chest = 宝箱 (layer pips); amber jar = 蜂蜜罐; colored balloon = 气球; tan biscuit + chips = 饼干; cyan frame + cracks = ice; green tufts = grass; green frame + vines = vine (pulse = next spread); brown slab = chocolate (pulse = next spread); dark fuse + turn pips = countdown bomb; silver dome + color rim = UFO.

## 对标开心消消乐 / Feature map

| 开心消消乐 | 本项目 |
|-----------|--------|
| 三消 / 四消横竖线 / 五消彩色精灵 | ✅ Normal / LineH·V / Rainbow |
| 炸弹与特殊合成 | ✅ Bomb；Line×Bomb / Rainbow×Line / Bomb×Bomb / Line×Line |
| 箱子 / 多层障碍 | ✅ `Stone n`（邻消削层） |
| 宝箱 | ✅ `Chest n` + `GoalChest`（邻消削层打开） |
| 饼干 / 掉落收集 | ✅ `Cookie` + `GoalCookie`（重力掉落，底行收集） |
| 冰层 | ✅ gem 上 ice；裂纹绘制 |
| 草 / 藤蔓 | ✅ `CellOverlay` Grass（匹配清除）/ Vine（步末蔓延，清则不蔓） |
| 巧克力 | ✅ `CellOverlay` Choco（邻消清除 + 步末蔓延，清则不蔓） |
| 倒计时炸弹 | ✅ `Countdown`：步末−1、归零 3×3、匹配解除 |
| 收集动物 / 多目标 | ✅ GoalCollect / GoalCollectMulti / GoalClearStone |
| 飞碟吸色 | ✅ `Ufo{cell,color}` + `stepUfo` 波末吸同色邻格并移格；`GoalUfo` |
| 每日挑战 / 三星 | ✅ Daily + starRating |
| 传送带 | ✅ `Belt` 步末循环移位，可触发新消 |
| 道具（锤子等） | ✅ 锤子 / 任意交换：按键进模式 + 点选完整流 |

## itch.io

See [`ITCH.md`](ITCH.md) for packaging / page checklist.

## Build & test

```bash
stack build && stack test && stack exec match3-sdl
```

## Layout

```
src/Match3/  Types Board Game Core Obstacles Rainbow Combos Ice Daily Countdown Conveyor Boosters Grass Ufo
app/Main.hs  SDL2 frontend
test/Spec.hs tasty (69+ named cases)
```

Frozen rule API shapes: `trySwap` / `runMove` / `ensurePlayable` / `shuffleGame` / `Outcome` / `GoalCollect`.
