# itch.io 上传清单

## 构建（Linux）

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
sudo apt-get install -y libsdl2-dev
stack build
stack exec match3-sdl   # 冒烟
```

运行期需要 `libsdl2-2.0-0`。无显示器：`xvfb-run -a stack exec match3-sdl`。

macOS Apple Silicon 额外：

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig
stack build && stack exec match3-sdl
```

## 打包

- [ ] 附带 `match3-sdl` 二进制，并注明依赖系统 SDL2
- [x] 包含仓库根目录的 `README.md` 与 `LICENSE`（BSD-3-Clause）
- [ ] 短 GIF / 截图：交换、连锁连击、巧克力、宝箱、保险箱、蜂蜜罐、蛋糕、魔法帽、锁链、火箭冰冻、窗帘、双面块、彩蛋、染色瓶、时间精灵、蒸汽、地毯、蜗牛、果汁机、传送门、气球、饼干、飞碟、选关（`M`）
- [ ] 封面图 630×500（itch）：棋盘 + 标题
- [x] 页面文案草稿就绪（见下方 **页面文案**）：8×8 / 五色三消，灵感来自开心消消乐；Haskell + SDL2

## 页面文案（短）

**Match-3 消消乐** — 相邻交换，直线 / 彩虹 / 炸弹，石头箱，冰层，火箭冰冻，窗帘，蜗牛，草 / 藤蔓 / 巧克力，传送带，传送门，倒计时炸弹，宝箱，保险箱（开出饼干），双面块，彩蛋惊喜盒，染色瓶，蜂蜜罐，分层蛋糕，魔法帽，锁链，果汁机，气球，饼干掉落收集，迷雾，飞碟吸色，每日挑战，道具（锤子 / 任意交换 / 十字），战役地图（38 关，章节分隔），时间精灵（+2 步），蒸汽，地毯，过关剩余步携带。

操作：点击/拖拽交换 · `1` 锤子 · `2` 任意交换 · `3` 十字 · `H` 提示 · `M` 地图 · `D` 每日 · `P` 暂停

## 标签

`puzzle` `match-3` `casual` `sdl2` `haskell` `indie`

## 上传后

- [ ] 在干净机器 / 容器上下载试玩
- [ ] 按需设置价格 / 捐赠
- [x] GitHub 链接就绪：https://github.com/exsabersac/match3

## 仓库状态

稳定性巡航：每日通关为 Won（不是战役 LevelClear/解锁）；38 关 / 210 测试通过。

- 38 关战役 + 每日挑战；`stack test` **210** 绿于 lts-21.25 / GHC 9.4.8
- 终章 24 步 / 分数 1400；大师 22 步 / 分数 1000；蒸汽 22 / 织毯 24；地图 CH1–CH7 覆盖全部 38 节点
- 键位与游戏内帮助条、暂停叠层一致（H / 1 / 2 / 3 / U / S / D / M / R / N / P）
