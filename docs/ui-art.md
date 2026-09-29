# UI 美术与贴图

本文说明 `match3-sdl` 的美术风格、贴图清单、生成方式，以及运行时如何加载和降级。规则层（`src/Match3/*`）完全不受影响；贴图只在 `app/` 里使用。

![关卡 16「大师」](images/screenshot-l16.png)

## 风格

- **糖果宝石风**：深紫夜空背景，棋盘格是圆角深蓝半透明方砖（棋盘格交替两种深浅）。宝石有高光、渐变和柔和投影。
- **统一规格**：棋盘贴图按 **112×112**（格子 56 px 的 2 倍）绘制，运行时用线性过滤缩到格子大小，Retina / 高 DPI 下也清晰。
- **颜色 + 形状双编码**：每种颜色同时对应一种轮廓，色弱玩家或灰度截图也能一眼区分（见下表）。
- **层数可读**：多层障碍的外观会随层数变化（裂纹、层数、厚度），层数 ≥ 2 时右下角还有**数字角标**。
- **HUD**：九宫格圆角面板（`panel_*`），金色描边表示强调/选中；数字用描边字形，中文标签预渲染成贴图，所以运行时不需要字体库（无 SDL_ttf）。

## 颜色 → 形状

| 颜色 | 内部 | 形状 | RGB |
|------|------|------|-----|
| 红 | `C1` | 圆形宝珠 | (236, 62, 78) |
| 绿 | `C2` | 切角方形翡翠 | (52, 196, 96) |
| 蓝 | `C3` | 菱形 | (56, 128, 246) |
| 黄 | `C4` | 五角星 | (255, 194, 36) |
| 紫 | `C5` | 三角形 | (172, 88, 236) |

带颜色的障碍（气球、果汁机、染色瓶、飞碟）也会在自身上印同样的**形状徽记**，所以不必只靠颜色判断。

## 图例

完整图例（中英文标签）：[`images/legend.png`](images/legend.png)，每次运行生成脚本都会重新生成。

![图例](images/legend.png)

### 特殊块

| 贴图 | 含义 | 外观 |
|------|------|------|
| `line_h` / `line_v` | 直线 LineH / LineV | 宝石上一条白金光带，两端带箭头，方向即消除方向（横 / 竖） |
| `bomb_glow` + `bomb_mark` | 炸弹 Bomb | 宝石后面有橙色呼吸光晕，宝石上有黑色小炸弹标记 |
| `rainbow` | 彩虹 Rainbow | 七彩旋涡球，游戏中缓慢旋转 |

### 覆盖物（宝石之上）

| 贴图 | 构造子 | 外观 / 层数表示 |
|------|--------|-----------------|
| `ice_1..3` | 冰层 `ice` | 青色透明冰壳，层数越多越厚、裂纹越少；≥2 显示角标 |
| `grass` | `Grass` | 格子底部一丛绿草 |
| `vine` | `Vine` | 绿藤缠绕宝石；将要蔓延的格子会闪绿色提示 |
| `choco` | `Choco` | 巧克力方块（盖住宝石）；将要蔓延的格子会闪棕色提示 |
| `fog_1..2` | `Fog n` | 白色云团，2 层更浓；≥2 显示角标 |
| `chain_1..2` | `Chain n` | 铁链 + 金锁：1 层是一条斜链，2 层是交叉双链 |
| `freeze_1..2` | `Freeze n` | 深蓝雪花釉壳，2 层颜色更深（和青色冰层 Ice 区分开） |
| `curtain_1..2` | `Curtain n` | 酒红幕布，2 层全闭，1 层半开 |
| `steam` | `Steam` | 灰白蒸汽团 |

### 障碍 / 特殊格

| 贴图 | 构造子 | 外观 / 层数表示 |
|------|--------|-----------------|
| `stone_1..3` | `Stone n` | 灰色岩块：3 层完整，2 层有裂纹，1 层碎裂；更多层时用 `stone_3` 加角标 |
| `chest` | `Chest n` | 金边木宝箱 + 层数角标 |
| `honey` | `Honey n` | 琥珀蜂蜜罐（带蜂巢六边形）+ 层数角标 |
| `balloon_c1..c5` | `Balloon c` | 对应颜色的气球，上面印有形状徽记，游戏中会上下浮动 |
| `cookie` | `Cookie` | 巧克力豆饼干（收集物） |
| `cake_1..3` | `Cake n` | 奶油蛋糕，层数就是蛋糕的层数（1/2/3 层），≥2 显示角标 |
| `magic_hat` | `MagicHat` | 紫色星星魔法帽 |
| `maker_c1..c5` | `Maker c n` | 对应颜色的果汁机；剩余次数 n **始终**用角标显示 |
| `snail` | `Snail dr dc` | 橙壳蜗牛，按爬行方向旋转 / 镜像 |
| `safe` | `Safe n` | 钢制保险箱 + 金色转盘 + 层数角标 |
| `flip_mark` | `Flip f b` | 正面宝石 + 右上角小宝石（翻面后的颜色）+ 左下角循环箭头 |
| `surprise` | `Surprise` | 粉色礼盒 + 问号 |
| `bottle_c1..c5` | `Bottle c` | 对应颜色的染色瓶 + 形状徽记 |
| `time_spirit` | `TimeSpirit` | 带光环的小精灵，标着「+2」，会上下浮动 |
| `countdown_1..9` | `Countdown c n` | 宝石上叠一个红色倒计时炸弹，中间写剩余步数 |
| `ufo_c1..c5` | 飞碟 `Ufo` | 对应颜色的飞碟，悬停在格子上方 |

### 地砖（宝石之下）

| 贴图 | 含义 |
|------|------|
| `tile_a` / `tile_b` | 普通棋盘格（交替两种深浅） |
| `carpet_open` / `carpet_covered` | 地毯目标格：虚线品红框表示未铺，编织纹品红地毯表示已铺 |
| `belt` | 传送带：青色边轨 + 箭头，箭头朝向就是移动方向 |
| `portal` | 紫色旋转传送门环 |

### 交互 / HUD

`sel_ring`（选中框，颜色随道具模式变化）、`hint_glow`（提示呼吸光）、`spark`（消除闪光）、`star_on/off`、`medal`、`node_cur/done/lock`（地图节点）、`icon_*`（锤子 / 交换 / 十字 / 步数 / 分数 / 多色）、`badge_1..9`、`g_*`（字形）、`zh_*`（中文标签）、`name_*`（关卡名）。

## 重新生成贴图

```bash
pip install pillow numpy        # 或 apt install python3-pil python3-numpy
python3 tools/gen_assets.py     # 约 40 秒；加 --preview 另存 /tmp/atlas_preview.png
```

脚本会生成：

- `assets/atlas.bmp`：全部贴图打包成一张图集（32 位 BGRA，带透明通道）
- `assets/atlas.txt`：索引文件，每行 `name x y w h`
- `assets/background.bmp`：窗口背景（480×588）
- `docs/images/legend.png`：图例

绘制采用 4 倍超采样再缩小，随机种子固定，所以同一台机器上多次运行结果一致。关卡名从 `src/Match3/Types.hs` 解析，新增关卡后重新跑一次即可。字体按顺序查找：拉丁字形用 Barlow Condensed / DejaVu / Arial，中文用 Noto Sans CJK / 文泉驿 / 苹方 / 华文黑体。

## 加载与降级

实现见 `app/Art.hs`。

1. **格式**：BMP V4 头（`BI_BITFIELDS` + alpha 掩码），用 SDL2 核心的 `SDL.loadBMP` 就能保留透明度，**不需要 SDL_image / SDL_ttf**。macOS 只要已有的 `brew install sdl2`，**不用装任何新的 brew 包**。新增的 Haskell 依赖只有 GHC 自带的 `containers`、`directory`、`filepath`。
2. **路径查找**（找到第一个同时包含 `atlas.bmp` 和 `atlas.txt` 的目录就用它）：
   1. 环境变量 `MATCH3_ASSETS`
   2. 当前目录下的 `./assets`（在仓库根目录运行 `stack exec match3-sdl` 时就是这个）
   3. 可执行文件所在目录，以及它向上最多 12 层父目录里的 `assets/`（从 `.stack-work/.../bin` 也能找到仓库里的资源）
3. **降级**：找不到资源或加载失败时，只会在 stderr 打印一行警告，然后使用原来的纯色方块 / 几何图形渲染，游戏照常运行。单个贴图缺失时，只有对应的格子退回原来的几何绘制。`background.bmp` 可选，缺了就用纯色背景。

## 开发用环境变量

| 变量 | 作用 |
|------|------|
| `MATCH3_ASSETS=/path/to/assets` | 指定资源目录 |
| `MATCH3_LEVEL=16` | 从第 N 关开始（从 1 开始计数，方便截图） |
| `MATCH3_SHOWCASE=1` | 展示盘面：一屏摆出所有宝石、特殊块、覆盖物、障碍、地砖（仅用于预览美术，不影响规则） |

截图示例（无显示器）：

```bash
MATCH3_SHOWCASE=1 xvfb-run -a stack exec match3-sdl
```

![展示盘面](images/showcase.png)
