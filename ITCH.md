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
- [ ] Include `README.md` and `LICENSE` (BSD-3-Clause)
- [ ] Short GIF / screenshots: swap, cascade combo, chocolate, chest, UFO, map (`M`)
- [ ] Cover image 630×500 (itch) with gem board + title
- [ ] Description: 8×8 / 5-color Match-3 inspired by 开心消消乐; Haskell + SDL2

## Page copy (short)

**Match-3 消消乐** — adjacent swaps, lines / rainbow / bombs, stone crates, ice, grass / vine / chocolate, conveyor belts, countdown bombs, treasure chests, UFO absorb, daily challenge, boosters (hammer / free-swap), campaign map.

Controls: click/drag swap · `1` hammer · `2` free-swap · `H` hint · `M` map · `D` daily · `P` pause

## Tags

`puzzle` `match-3` `casual` `sdl2` `haskell` `indie`

## After upload

- [ ] Test download on clean machine / container
- [ ] Set price / donation as preferred
- [ ] Link GitHub: https://github.com/exsabersac/match3
