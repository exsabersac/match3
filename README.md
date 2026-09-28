# Match-3 消消乐（Haskell + SDL2）

8×8、5 色可玩 Match-3，对标开心消消乐常见机制：特殊块、多层障碍、草/藤蔓/巧克力/迷雾/锁链/火箭冰冻/窗帘、蒸汽、蜗牛、宝箱、保险箱、蜂蜜罐、蛋糕、魔法帽、果汁机、气球、饼干掉落收集、双面块、彩蛋惊喜盒、染色瓶、时间精灵、地毯、传送带、双向传送门、倒计时炸弹、飞碟、道具点选、多样目标、每日挑战、步数携带。纯规则在 library（`Match3.Core`），SDL 前端为 `match3-sdl`。

Playable 8×8 / 5-color match-3 inspired by Happy Match (开心消消乐). Pure rules in `Match3.Core`; SDL2 frontend is `match3-sdl`.

## 30 秒上手 / 30-second start

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
sudo apt-get install -y libsdl2-dev   # once (headers); runtime: libsdl2-2.0-0
stack test                            # 173 green — optional but recommended
stack build && stack exec match3-sdl
```

1. **左键**点两格相邻交换，或**拖拽**到相邻格；无三连会回滚  
2. 开局底部有键位条；**P** 暂停看完整键位（H 提示 / **1** 锤子 / **2** 任意交换 / **3** 十字清除 / **M** 选关地图 / U 撤销 / S 洗牌 / D 每日 / R 重开 / N 过关）  
3. 第一关会短暂黄框提示可消一手；达目标后按 **N** / 空格 / 点击继续  

Headless smoke: `xvfb-run -a stack exec match3-sdl`. No display needed for `stack test`.

## Features / 功能

- Adjacent swaps (click or drag); horizontal/vertical ≥3 clear
- **Specials**: 4-match → Line; 5-match → Rainbow (swap clears partner color only — Gem/Countdown/Flip front; own-color expand is a no-op); Bomb exists for combos
- **Line×Bomb (3×3 cross), Rainbow×Line / Rainbow×Bomb (partner color + Line/Bomb expand), Bomb×Bomb (5×5), Line×Line (row+col)
- **Stone crates**: layered blockers (`Stone n`); adjacent clears chip; last layer removes
- **Treasure chests** (`Chest n` / 宝箱): layered gold chests; adjacent / Line·Bomb·Hammer chip one layer; `GoalChest`
- **Honey jars** (`Honey n` / 蜂蜜罐): amber jars; adjacent / Line·Bomb·Hammer chip one layer; `GoalHoney`
- **Balloons** (`Balloon c` / 气球): colored; adjacent **same-color** clear pops; `GoalBalloon`
- **Cookies** (`Cookie` / 饼干): fall with gravity; collected on the **bottom row** only (immune to Line/Bomb/Hammer mid-board wipe); `GoalCookie`
- **Cakes** (`Cake n` / 蛋糕): layered obstacles (≠ Cookie); adjacent / Line·Bomb·Hammer chip one layer; `GoalCake`
- **Magic hats** (`MagicHat` / 魔法帽): adjacent clear swaps/recolors neighbor gem colors
- **Chains** (`Chain n` / 锁链): lock gems; adjacent clears peel; chained gems cannot swap or match
- **Rocket freeze** (`Freeze n` / 火箭冰冻): blocks **swap only** (gems still match); adjacent clears peel; ≠ Ice (Ice chips when the gem itself matches)
- **Snails** (`Snail dr dc` / 蜗牛): block swap; after each successful move crawl one step (push gems; reverse at edges/blockers)
- **Curtains** (`Curtain n` / 窗帘): column/region shade overlay; adjacent clears peel; curtained gems do not match
- **Safes** (`Safe n` / 保险箱): layered vault; adjacent clears chip; last layer opens into a Cookie; `GoalSafe`
- **Dual-face gems** (`Flip front back` / 双面块): matches as front; a clear hit flips to Normal gem of back color
- **Surprise boxes** (`Surprise` / 彩蛋): adjacent *or* direct-seed (hammer/cross/line) opens → Line/Bomb special or 3×3 pop
- **Dye bottles** (`Bottle c` / 染色瓶): adjacent clear dyes ortho gems to bottle color
- **Time spirits** (`TimeSpirit` / 时间精灵): adjacent clear awards **+2 moves** this level
- **Steam** (`Steam` overlay / 蒸汽): blocks match; adjacent clear extinguishes; surviving steam spreads each move
- **Carpet** (地毯 / 目标地砖): floor tiles under gems; clearing a gem on the tile covers it; `GoalCarpet`
- **Move bank**: clearing a campaign level carries up to **3 leftover moves** into the next
- **Juice makers** (`Maker c n` / 果汁机): same-color adjacent clears charge; at 0 produce a Bomb of color c
- **Portals** (传送门 pairs): after gravity, gem on A with hole at B teleports A→B (bidirectional)
- **Ice**: layers on gems; match chips ice; last layer clears the gem; crack lines in UI (distinct from Freeze overlay)
- **Grass / Vine / Chocolate / Fog** (`CellOverlay`): Grass clears on match; Vine spreads at end of move; **Choco** clears when adjacent to a match and surviving chocolate spreads; **Fog n** peels by adjacent clear (fogged gems do not match until clear)
- **Boosters**: `1` → hammer; `2` → free-swap any two cells; `3` → cross clear (row+col); limited charges; select-then-1/3 still works; hammer/cross **peel** Chain/Curtain one layer and **chip** Stone one layer (≠ nuke)
- **Conveyor belts** (传送带): cyclic `Belt` paths; shift after move; may trigger cascades
- **Countdown bombs** (倒计时炸弹): colored timers; tick −1 after each move; at 0 explode 3×3; match/special disarms
- **Goals**: score / single collect / multi-color collect / clear stones / open chests / smash honey jars / pop balloons / collect cookies / clear cakes / open safes / UFO absorb / cover carpet
- **UFO / 飞碟**: overlay `Ufo{cell,color}`; each cascade wave `stepUfo` absorbs ortho same-color gems then relocates
- **Daily challenge** (`D`): date-seeded board + **10 rotating goals** (score/collect/multi/stone/honey/UFO/chest/cake/safe/balloon); obstacle goals auto-seed décor when level index has none; **star rating** on clear (3★ ≥40% of **printed** level moves left; carry does not inflate the denominator)
- Combo scoring (seed clears continue wave multipliers), hint, undo, auto-shuffle, **38 campaign levels** with map **chapter separators** (CH1–CH7)
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
| 12 | 轰炸 | 20 | Score 750 | countdown bombs |
| 13 | 飞碟 | 24 | UFO absorb 10 | UFO C1 |
| 14 | 碟猎 | 20 | UFO absorb 14 | 2 UFOs + belt |
| 15 | 压力 | 20 | Multi RED/GRN/BLU | grass + choco |
| 16 | 大师 | 22 | Score 1000 | stone+grass+vine+choco+bomb+belts+UFO |
| 17 | 宝箱 | 24 | Open 6 chests | chests |
| 18 | 巧箱 | 22 | Open 5 chests | chests + choco |
| 19 | 蜂蜜 | 24 | Smash 6 honey jars | honey jars |
| 20 | 蜜压 | 22 | Smash 5 honey jars | honey + choco |
| 21 | 气球 | 24 | Pop 6 balloons | colored balloons |
| 22 | 饼干 | 24 | Collect 6 cookies | cookies high on board |
| 23 | 巧饼 | 22 | Collect 5 cookies | cookies + choco + fog |
| 24 | 蛋糕 | 24 | Clear 6 cakes | layered cakes |
| 25 | 帽宴 | 22 | Clear 5 cakes | cakes + magic hats + choco |
| 26 | 锁链 | 22 | Score 900 | iron chains + stone + choco |
| 27 | 果汁 | 24 | Collect 18× RED | juice makers + fog + portals |
| 28 | 终章 | 24 | Score 1400 | stone+chest+honey+balloon+cookie+cake+hat+maker+chain+freeze+curtain+safe+flip+surprise+bottle+snail+choco+fog+vine+bomb+belt+portal+UFO+carpet |
| 29 | 蜗牛 | 20 | Score 850 | crawling snails |
| 30 | 冰冻 | 20 | Collect 16× GREEN | rocket freeze + choco |
| 31 | 窗帘 | 22 | Collect 16× RED | curtain columns + choco |
| 32 | 金库 | 22 | Open 5 safes | safes→cookie + dual-face + choco |
| 33 | 惊喜 | 22 | Score 900 | surprise boxes + choco |
| 34 | 染色 | 22 | Collect 16× BLUE | dye bottles + fog |
| 35 | 时灵 | 22 | Score 850 | time spirits (+2 moves) |
| 36 | 蒸汽 | 22 | Collect 16× GREEN | steam clouds + choco |
| 37 | 地毯 | 24 | Cover 8 carpet tiles | carpet floor + choco |
| 38 | 织毯 | 24 | Cover 12 carpet tiles | carpet + choco + fog |

Press `D` for a **每日** daily run (seed from calendar date).

## Controls / 操作

| Key / Input | Action |
|-------------|--------|
| Left click / drag | Adjacent swap |
| `1` then click | Hammer mode (clear one cell); `1` again cancels |
| `2` then two clicks | Free-swap any two cells; `2` again cancels |
| `3` then click | Cross clear (row+col); `3` again cancels |
| Select cell then `1`/`3` | Hammer / cross that cell (legacy shortcut) |
| `H` | Hint |
| `U` | Undo |
| `S` | Shuffle |
| `D` | Daily challenge |
| `M` | Level map (选关); click unlocked node |
| `N` / Space / Enter | Next / retry after overlay |
| `R` | Restart level (also works while paused) |
| `P` | Pause + key help (freezes anim; clears in-flight drag) |
| `Esc` / `Q` | Quit |

Special look: white+gold bar = line; multi-color ring = rainbow; black/yellow+red ring = bomb; gray rock = stone (layer pips); gold chest = 宝箱 (layer pips); amber jar = 蜂蜜罐; pink frosted cake = 蛋糕 (layer pips, ≠ cookie); purple brim hat = 魔法帽; metal spout = 果汁机 (color + charge pips); gray cross links = 锁链 (layer pips); deep-blue snowflake glaze = 火箭冰冻 (≠ cyan ice cracks); wine vertical stripes + rod = 窗帘; steel vault + gold dial = 保险箱; split two-tone gem = 双面块; pink gift + gold bow = 彩蛋; tinted bottle + neck = 染色瓶; cyan orb + hourglass = 时间精灵; gray steam wisps = 蒸汽; magenta weave floor = 地毯 (target / covered); olive shell + dir tick = 蜗牛; violet rings = 传送门 pair; colored balloon = 气球; tan biscuit + chips = 饼干; soft white cloud = 迷雾 (layer pips); cyan frame + cracks = ice; green tufts = grass; green frame + vines = vine (pulse = next spread); brown slab = chocolate (pulse = next spread); dark fuse + turn pips = countdown bomb; silver dome + color rim = UFO.

## 对标开心消消乐 / Feature map

| 开心消消乐 | 本项目 |
|-----------|--------|
| 三消 / 四消横竖线 / 五消彩色精灵 | ✅ Normal / LineH·V / Rainbow |
| 炸弹与特殊合成 | ✅ Bomb；Line×Bomb / Rainbow×Line / Bomb×Bomb / Line×Line |
| 箱子 / 多层障碍 | ✅ `Stone n`（邻消削层） |
| 宝箱 | ✅ `Chest n` + `GoalChest`（邻消/直线·炸弹·锤子削层打开） |
| 饼干 / 掉落收集 | ✅ `Cookie` + `GoalCookie`（重力掉落，底行收集） |
| 蛋糕（分层障碍） | ✅ `Cake n` + `GoalCake`（邻消/直线·炸弹·锤子削层） |
| 魔法帽 | ✅ `MagicHat`（邻消触发，交换/重染邻格颜色） |
| 锁链 / 铁链 | ✅ `Chain n` overlay（邻消揭层；锁住不可交换/匹配） |
| 火箭冰冻 | ✅ `Freeze n` overlay（只挡交换不挡匹配；邻消揭层；≠ Ice 自消削层） |
| 窗帘 / 卷帘 | ✅ `Curtain n` overlay（邻消揭层；帘下宝石不可匹配；≠ 迷雾） |
| 保险箱 / 金库 | ✅ `Safe n` + `GoalSafe`（邻消削层，开出 Cookie） |
| 双面块 | ✅ `Flip front back`（正面参与匹配；命中翻成背面 Normal 宝石） |
| 彩蛋 / 惊喜盒 | ✅ `Surprise`（邻消/直接命中打开 → 特殊块或 3×3 小爆炸） |
| 染色瓶 | ✅ `Bottle c`（邻消把邻格宝石染成瓶色） |
| 时间精灵 | ✅ `TimeSpirit`（邻消清除，本关 +2 步）；过关剩余步最多携带 3 步入下一关 |
| 蒸汽 | ✅ `Steam` overlay（挡匹配；邻消扑灭；步末蔓延） |
| 地毯 / 目标地砖 | ✅ `gsCarpetOpen` + `GoalCarpet`（该格宝石消除则铺地毯） |
| 蜗牛 | ✅ `Snail dr dc`（挡交换；步末爬一格推宝石，碰壁掉头） |
| 果汁机 / 制造机 | ✅ `Maker c n`（同色邻消充能，满则产出 Bomb） |
| 传送门 | ✅ 双向 `Portal` 对（重力后 A 有子且 B 为空则传送） |
| 云朵 / 迷雾 | ✅ `Fog n` overlay（邻消揭层；雾下宝石不可匹配） |
| 冰层 | ✅ gem 上 ice；匹配削层；裂纹绘制（≠ 火箭冰冻 overlay） |
| 草 / 藤蔓 | ✅ `CellOverlay` Grass（匹配清除）/ Vine（步末蔓延，清则不蔓） |
| 巧克力 | ✅ `CellOverlay` Choco（邻消清除 + 步末蔓延，清则不蔓） |
| 倒计时炸弹 | ✅ `Countdown`：步末−1、归零 3×3、匹配解除 |
| 收集动物 / 多目标 | ✅ GoalCollect / GoalCollectMulti / GoalClearStone |
| 飞碟吸色 | ✅ `Ufo{cell,color}` + `stepUfo` 波末吸同色邻格并移格；`GoalUfo` |
| 每日挑战 / 三星 | ✅ Daily（10 目标轮换）+ starRating |
| 选关章节 | ✅ 地图 CH1–CH7 分隔 |
| 传送带 | ✅ `Belt` 步末循环移位，可触发新消 |
| 道具（锤子等） | ✅ 锤子 / 任意交换 / 十字清除：按键进模式 + 点选完整流 |

## itch.io

See [`ITCH.md`](ITCH.md) for packaging / page checklist.

## Build & test

```bash
stack build && stack test && stack exec match3-sdl
```

## Layout

```
src/Match3/  Types Board Game Core Obstacles Rainbow Combos Ice Daily Countdown Conveyor Boosters Grass Ufo Snail Carpet (+ Steam / TimeSpirit)
app/Main.hs  SDL2 frontend
test/Spec.hs tasty (190 named cases)
```

Frozen rule API shapes: `trySwap` / `runMove` / `ensurePlayable` / `shuffleGame` / `Outcome` / `GoalCollect`.

## Release status / 发布状态

- Campaign: **38** levels (CH1–CH7 on map), batch-tested constructible / playable / décor-vs-goal
- Tests: `stack test` **198** (Tasty + QuickCheck); core move invariants include spirit +2, carry cap 3, belt→steam→snail end-of-move order; booster peel locks + daily décor; snail×belt / maker charge / map unlock+resume / clear-only particles; UFO skip peel-locks/Flip; hammer immune no-spend; Rainbow×Flip partner; Surprise direct-seed opens; soft-hit preserves Choco/Steam; Surprise blast peels adj obstacles; shuffle preserves Line/Bomb/Rainbow; soft-lock blocks Line/Bomb expand; Line blast no double-peel Chain/Curtain/Stone/Safe; Line/Bomb/Hammer single-chip Chest/Honey/Cake; MagicHat immune to Line/Bomb/Hammer direct clear; soft-hit preserves on-cell Grass/Vine/Choco; soft-hit no adj Fog/Chain/Freeze/Curtain/Maker/Bottle/Balloon; soft-hit keeps on-cell Fog/Steam; Surprise explode opens nested Surprises (Bomb parity); nested Surprise special sits (no fire-and-survive); soft-lock blocks Rainbow swap + special combos (ice>1/Chain/Curtain); soft-lock FreeSwap activation + asymmetric double-Rainbow gates; belt→bottom Cookie drains without follow-up match; bare GoalCookie/GoalCarpet décor seed (ensureGoalDecor / UFO-parity carpets); portal Flip+Countdown teleport; Carpet covers Cookie vacate / Safe→Cookie open; snail crawl follow-up cascade (move ends stable); finale portal endpoints not immortal-blocked; snail reverses at portal endpoints; UFO absorb no special expand (吸走≠引爆) (终章 snail→(2,0), stepSnailsAvoidingBlocked walls); Portal/Surprise→Safe bottom Cookie drain covers Carpet (boundary); Surprise-opened special sits through same-wave Hat/Bottle
- Stackage: **lts-21.25** / GHC **9.4.8**; binary: `stack build && stack exec match3-sdl`
- itch checklist: see `ITCH.md`

