# itch.io upload checklist

## Build (Linux)

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
sudo apt-get install -y libsdl2-dev
stack build
stack exec match3-sdl   # smoke-test
```

Runtime needs `libsdl2-2.0-0`. Headless: `xvfb-run -a stack exec match3-sdl`.

## Package

- [ ] Ship `match3-sdl` binary + note SDL2 system dependency
- [x] Include `README.md` and `LICENSE` (BSD-3-Clause) — in repo root
- [ ] Short GIF / screenshots: swap, cascade combo, chocolate, chest, safe, honey jar, cake, magic hat, chain, freeze, curtain, flip, surprise, bottle, time spirit, steam, carpet, snail, juice maker, portal, balloon, cookie, UFO, map (`M`)
- [ ] Cover image 630×500 (itch) with gem board + title
- [x] Description draft ready (see **Page copy** below): 8×8 / 5-color Match-3 inspired by 开心消消乐; Haskell + SDL2

## Page copy (short)

**Match-3 消消乐** — adjacent swaps, lines / rainbow / bombs, stone crates, ice, rocket freeze (火箭冰冻), curtains (窗帘), snails (蜗牛), grass / vine / chocolate, conveyor belts, portals (传送门), countdown bombs, treasure chests, safes (保险箱→cookie), dual-face gems (双面块), surprise boxes (彩蛋), dye bottles (染色瓶), honey jars (蜂蜜罐), cakes (蛋糕 layered), magic hats (魔法帽), chains (锁链), juice makers (果汁机), balloons (气球), cookies (饼干 drop-collect), fog/clouds (迷雾), UFO absorb, daily challenge, boosters (hammer / free-swap / cross), campaign map (38 levels, chapter separators), time spirits (+2 moves), steam clouds, carpet floor tiles (地毯), leftover-move bank.

Controls: click/drag swap · `1` hammer · `2` free-swap · `3` cross · `H` hint · `M` map · `D` daily · `P` pause

## Tags

`puzzle` `match-3` `casual` `sdl2` `haskell` `indie`

## After upload

- [ ] Test download on clean machine / container
- [ ] Set price / donation as preferred
- [x] GitHub link ready: https://github.com/exsabersac/match3

## Status (repo)

Stability cruise: Surprise special sits through same-wave Hat/Bottle; 38 levels / 198 tests green.

- 38 campaign levels + daily challenge; `stack test` **198** green on lts-21.25 / GHC 9.4.8
- Finale (终章) 24 moves / Score 1400; Master (大师) 22 moves / Score 1000; Steam 22 / Carpet weave 24; map shows CH1–CH7 for all 38 nodes
- Controls match in-game help strip and pause overlay (H / 1 / 2 / 3 / U / S / D / M / R / N / P)
- Fragile locks: Carpet↔Ice cover; TimeSpirit last-move rescue (−1+2); Portal after Belt match teleport; Steam→Snail / Belt→Steam; Flip 4-match spawn; Surprise blast expands Bomb; Maker→Bomb same-wave sit; Chain+Freeze co-peel; Honey+Balloon same clear; Safe bottom→Cookie collect; Cookie bottom before Portal teleport; Countdown explode keeps UFO+portals; Hammer/Cross peel Chain·Curtain + chip Stone; Daily obstacle-goal décor seed; Rainbow expand noop (partner-only); Rainbow×Bomb 3×3; star vs printed moves; map/restart no carry; curtain allows swap / blocks match; Freeze blocks trySwap+free-swap; Hammer clears Grass/Vine; snail×belt no double-step; maker multi-adj once; gsLastCleared particles; map unlock+resume; UFO skip peel-locks/Flip; hammer immune no-spend; Rainbow×Flip partner; Surprise direct-seed opens; soft-hit preserves Choco/Steam; Surprise blast peels adj obstacles; shuffle preserves Line/Bomb/Rainbow; soft-lock (ice>1/Chain/Curtain) blocks Line/Bomb expand; Line/Bomb blast no double-peel Chain/Curtain/Stone/Safe; Line/Bomb/Hammer single-chip Chest/Honey/Cake; MagicHat immune to Line/Bomb/Hammer direct clear; soft-hit preserves on-cell Grass/Vine/Choco; soft-hit no adj Fog/Chain/Freeze/Curtain/Maker/Bottle/Balloon; soft-hit keeps on-cell Fog/Steam; nested Surprise special sits (no fire-and-survive); soft-lock (ice>1/Chain/Curtain) blocks Rainbow swap + Line/Bomb/Rainbow combos (isRainbowSwap/isSpecialCombo via specialActivates); FreeSwap soft-lock parity; asymmetric double-Rainbow conjunction gate; Cookie immune to Line/Bomb/Hammer (触底收集 only); belt→bottom Cookie drains without match; bare GoalCookie/GoalCarpet décor seed; portal Flip (+Countdown) teleport; Carpet covers Cookie vacate / Safe open; snail crawl follow-up cascade; finale portal endpoints not immortal-blocked (Bottle off (0,3)); snail reverses at portal endpoints; UFO absorb Line/Bomb/Rainbow no expandSpecials detonation (吸走≠引爆) (终章 snail off row 0 → (2,0)); Cookie mid-settle bottom-drain covers Carpet (drain sites→clear lists); Portal-delivered / Surprise→Safe bottom Cookie drain covers Carpet (boundary); L27 Bottle off belt (stuck immortal on conveyor, portal-class layout); Surprise-opened special sits through same-wave Hat/Bottle mutate

