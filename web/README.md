# 网页版技术验证（GHC WebAssembly 后端）

目标：不改一行核心代码，把纯规则核心 `src/Engine/*` + `src/Match3/*` 用 GHC 的 wasm 后端编成 `.wasm`，
在浏览器里用桌面版同一套美术（2x 精灵图集）把 49 关跑通。**所有规则判定和动画时间轴都来自 Haskell 核心**
（动画状态机 `app/pure/ComboFx.hs` 与表现表 `app/pure/UI/Presentation.hs` 也一起编进 wasm），JS 只负责加载、Canvas 2D 绘制、收指针事件。

结构、取舍、部署与 TODO 的总览见 [`docs/web.md`](../docs/web.md)；本文是操作手册。
日常任务用**仓库根目录**的 `Makefile`（`make help`），见 §0。

```
web/
├── build.sh              构建脚本（→ web/dist/），--serve 构建后用 serve.py 起服务器
├── dist/                 可部署构建产物（已提交；只在部署前全量重建；html/js/wasm/图集），拷贝即可部署
├── serve.py              本地静态服务器（Python 3 标准库；正确 MIME、开发期 no-cache、打印局域网地址）
├── serve.sh              serve.py 的薄包装
├── deploy-mac.sh         打包 dist 并在 macOS 上安装 / 前台运行 / launchd 常驻 / 状态检查
├── cabal.project         独立的 cabal 工程（只给 wasm32-wasi-cabal 用）
├── match3-web.cabal      可执行 match3-web：hs/ + 直接引用 ../src（不复制核心源码）
├── hs/
│   ├── Match3Web/Api.hs  纯接口层：经 Match3.Engine.match3Shell 的 gameStep 执行，Step / Played → JSON（无 JSFFI，原生 GHC 也能编）
│   ├── Match3Web/Anim.hs 动画接口：Played → ComboFx 播放器（与桌面 withMovePlayback 同条件），逐帧 JSON
│   ├── Match3Web/Json.hs 极简 JSON 拼接（只输出整数，保证原生与 wasm 逐字节一致）
│   └── WebMain.hs        JSFFI 导出 m3New / m3Swap / m3Undo / m3State / m3Levels / m3Meta / m3AnimStart / m3AnimTick，
│                         以及迁自桌面的 m3Hammer / m3Cross / m3FreeSwap / m3Shuffle / m3Daily / m3Restart / m3Advance /
│                         m3Showcase / m3Progress / m3MapJump / m3Badge
├── tools/
│   ├── gen_web_atlas.py  由 assets/ 打网页图集（只读 assets/，不改 tools/gen_assets.py）
│   ├── doctor.sh         环境自检（make doctor）
│   └── size.sh           体积报告（make size）
├── www/
│   ├── index.html        只有一张全屏画布（viewport-fit=cover、禁缩放、touch-action:none）
│   ├── main.js           加载 wasm + 图集、rAF 固定步长、输入、交换 / 连锁流程、m3debug 调试钩子
│   ├── layout.js         自适应布局：竖排 / 横排取格子更大者，安全区，布局变换与命中检测
│   ├── art.js            图集绘制：普通 / 着色（离屏缓存）/ 叠加 / 旋转翻转 / 九宫格面板
│   ├── cells.js          桌面 CellTable 的 JS 移植（每种格子怎么画、地面层、传送带、传送门、飞碟、蔓延预告）
│   ├── render.js         盘面与动画：交换、逐轮 flash/pop/fall/rest、步末 tick/belt/spread/snail/shuffle、粒子、浮字、震屏
│   ├── hud.js            HUD 面板、目标进度条、消息折行、按钮、结局遮罩（字号随格子缩放）
│   ├── audio.js          音效与 BGM（各自开关，localStorage 键 m3-sfx / m3-bgm；WAV 由 build.sh 从 assets/sfx/ 复制到 dist/sfx/）
│   └── guide.js          本关特殊格子说明（问号面板，只列这一关出现过的格子）
└── test/
    ├── parity.sh         批量跑两组一致性对比（make parity / make anim-parity）
    ├── e2e.mjs           无头 Chrome：真实指针交换、7 种视口、动画中途改尺寸、截图 + report.json
    ├── node-parity.mjs   node 里跑 wasm，按提示连走 N 步打印 JSON
    ├── Parity.hs         原生 GHC 跑同样的步骤（最后撤销一步）；两边输出应逐字节相同
    ├── node-anim-parity.mjs  wasm 侧：每步 m3AnimStart + 逐帧 m3AnimTick，打印全部帧 JSON
    └── AnimParity.hs     原生侧同一流程（每第 3 步从第 5 帧起加速），并与 ComboFx runPlayer 核对帧数
```

## 0. 常用命令（make）

仓库根目录的 `Makefile` 把下面各节的命令收成目标，**一律在仓库根目录运行 `make <目标>`**；
`make`（即 `make help`）按分组列出：通用 / 桌面版 / 网页版构建与运行 / 网页版测试 / 打包与部署 / 清理 / 环境。

```sh
make doctor          # 先看缺什么
make build           # 构建到 web/dist
make serve PORT=9000 # 本地 / 局域网试玩
make test            # stack test + 状态一致性 + 动画一致性 + e2e
make check           # CI：lint-sh + 构建 + 全部测试 + 体积
make verify          # 合 main 前的验收：只跑 stack test（见 docs/testing.md「开发流程」）
```

| 目标 | 作用 |
| --- | --- |
| `make` / `make help` | 按分组列出全部目标与当前变量（默认目标） |
| `make desktop-build` | 桌面版：`stack build`（需系统 SDL2） |
| `make run` | 桌面版：`stack run match3-sdl`（需显示器；环境变量原样传给游戏） |
| `make doctor` | 检查 ghc-wasm、wasm-opt、node、playwright、Chrome、python3 + Pillow(WebP)、cwebp（可选）、stack + GHC 9.14.1、curl/gzip/tar、lsof（可选），缺什么给安装提示；必需项缺失退出码 1 |
| `make toolchain` | 已安装则校验 ghc-wasm-meta（FLAVOUR=9.14）各组件；没装则检查依赖后跑官方 bootstrap 安装；`FORCE=1` 重跑安装 |
| `make build` | `web/build.sh`：wasm + 页面 + 图集 → `web/dist` |
| `make atlas` | 强制重新生成网页图集（有 dist 时同步进去） |
| `make serve [PORT=8080] [BIND=0.0.0.0]` | 用 `serve.py` 起服务器（不自动构建） |
| `make test-native` | `stack test`（核心 464 个，桌面版与网页版共用） |
| `make parity` / `make anim-parity` | 状态 / 动画一致性（`web/test/parity.sh`；`STEPS=`、`CASES="关卡:种子[:走法] …"` 可改，走法 `hint` / `combo` / `combo-bomb` 见 §4） |
| `make e2e [SHOTS=目录] [E2E_PORT=8765]` | 无头 Chrome 端到端测试（`CHROME=` 可改浏览器；`E2E_PORT` = 临时 serve.py 的端口，默认 8765，见 §4） |
| `make test` | 以上四组测试依次跑 |
| `make verify` | 合 main 前的验收：只跑 `stack test` |
| `make check` | CI 用：`lint-sh` → `build` → `test` → `size` |
| `make lint-sh` | shell 脚本 / Makefile 检查：`$变量名` 后紧跟中文等非 ASCII 字符即报错（macOS bash 3.2 会读错变量名，须写 `${VAR}`；见 `docs/testing.md`） |
| `make size` | wasm 原始 / `-Oz` 后、dist 各文件与合计，原始与 gzip -9 |
| `make pack [TGZ=…]` | 打包 dist + serve.py + 部署脚本 |
| `make deploy-install [TGZ=…] [DEST=…]` | 在目标机上解包安装到 DEST（默认 `/Users/yubin/Documents/dev/haskell/match3-web`） |
| `make deploy-start` / `deploy-stop` | 仅 macOS：launchd 常驻 / 停止（`DEST`、`PORT`、`BIND` 可改） |
| `make deploy-status` | launchd 状态 + `lsof` 端口监听 + curl 自检 |
| `make clean` | 只清网页版：`web/dist`、`web/dist-newstyle`、`web/.cache`、`web/*.tgz`；不碰 `~/.ghc-wasm` 和 `.stack-work`（桌面版用 `stack clean`） |

## 1. 安装工具链（一次性，约 6.4 GB，装在 ~/.ghc-wasm）

用 [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) 的安装脚本，
选 `9.14` 分支（GHC 9.14 是 LTS 系列，也是 ghc-wasm-meta 当前默认 flavour，JSFFI 已稳定）：

```sh
make toolchain       # 已安装则只校验；没装则检查依赖后执行下面的官方安装脚本

# 等价的手动步骤：
# 安装脚本依赖（Debian/Ubuntu）；缺 xz 或 make 会在解包 / make install 阶段失败
sudo apt-get install -y curl jq unzip zstd xz-utils make

curl -fL https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh \
  | FLAVOUR=9.14 PREFIX=$HOME/.ghc-wasm sh
```

装完得到 `wasm32-wasi-ghc 9.14.1.x`、`wasm32-wasi-cabal`（内置 cabal-install 3.14.2）、
wasi-sdk、binaryen（`wasm-opt`）、wasmtime、node（自带 playwright-core）。

注意：
- `~/.ghc-wasm/env` 会改写 `CC`/`AR`/`LD` 等变量。**不要把它 source 进日常 shell**，
  否则桌面版 `stack build` 会拿 wasm 的 clang 去编 C 代码。`build.sh` 只在自己的子进程里 source。
- 不影响桌面版的原生 GHC 9.14.1 / Stack，两者完全独立。
- 首次构建前 `build.sh` 会自动走 `wasm32-wasi-cabal build`；如提示没有 Hackage 索引，先跑一次
  `bash -c 'source ~/.ghc-wasm/env && wasm32-wasi-cabal update'`。

## 2. 构建

```sh
make build           # 仓库根目录；等价于 web/build.sh
make size            # 事后单独看体积
```

`web/build.sh` 会：
1. 检查 `match3-web.cabal` 里的核心模块清单（`Engine.*` + `Match3.*`）与 `package.yaml` 的 `library.exposed-modules` 是否一致
   （核心新增模块时要同步到 cabal 文件，否则会打印警告；例如 main 2121bf8 新增的 `Match3.Element.Class` / `Message` /
   `Builtin.{Common,Gem,Layer,Obstacle,Collectible,Actor,Ground,Level}`，以及元素类重构第 2 刀换下 `Match3.Element.Caps` 的
   `Match3.Element.{Ability,Kind,Layer,World}` 已同步）；
2. `wasm32-wasi-cabal build exe:match3-web`，链接为 WASI **reactor** 模块；
3. `wasm-opt -Oz` 压体积；用 GHC 自带的 `post-link.mjs` 生成 JSFFI 胶水 `ghc_wasm_jsffi.js`；
4. 下载并缓存浏览器 WASI 垫片 `@bjorn3/browser_wasi_shim@0.4.2`（MIT/Apache-2.0，约 96 KB）；
3b. 用 `tools/gen_web_atlas.py`（需 python3 + Pillow）从 `assets/` 生成 `atlas.webp` + `atlas.json` + `background.webp`，
   缓存在 `web/.cache/art`，只有 `assets/` 或生成器变动时才重新生成；
5. 输出到 `web/dist/`，并打印 wasm 原始 / 优化后 / gzip 后的体积，以及 dist 总大小。

图集：174 张 2x 精灵（每格 112 px；不含 `g_`/`zh_` 文字图和 `@` 变体，保留 `badge_*`；收 49 张关名文字图 `name_<i>`，HUD 关名同桌面画这张图），
1024×1730，WebP 约 488 KB（488,226 B）；`atlas.json` 约 5.0 KB；背景 WebP 约 17 KB。

当前体积（2026-10-03，chore/audit-wrapup（审计整改第 1–8 项之后，基于 fa719fa），`make clean` 后全量重建的发布产物）：wasm 原始 5,442,572 B → `-Oz` 2,212,226 B ≈ 2.21 MB（gzip 817,932 B）；
dist 合计 3,074,199 B ≈ 3.07 MB，逐文件 gzip 合计 1,467,127 B（约 1.47 MB）（WebP / WAV 已压缩或体积小，gzip 收益主要在 wasm 与 JS；`sfx/` 7 个 WAV 约 196 KB，其中 `bgm.wav` 127,052 B）。
审计整改后 `-Oz` 后的 wasm 比上一版（96bd2d8）增加 5,242 B（gzip +1,807 B）；`cells.js` / `main.js` 换成 `web/www` 的现行版本（只差注释），图集与其余文件逐字节不变。

随机数：`cabal.project` 把 `random` / `splitmix` 钉在与桌面版 `stack.yaml` 相同的版本
（`extra-deps` 的 random-1.2.1.1 / splitmix-0.1.0.5，桌面版为 GHC 9.14.1：lts-24.60 + `compiler: ghc-9.14.1`；两边都只放宽 splitmix 的 base 上界），因此**同关卡同种子，网页版与桌面版开局和每一步结果完全一致**
（`test/Parity.hs` 与 `test/node-parity.mjs` 已验证）。

## 3. 本地试玩

```sh
make serve                          # 仓库根目录；已构建过就直接起（默认 0.0.0.0:8080）
make serve PORT=9000 BIND=127.0.0.1 # 改端口 / 只给本机
# 浏览器打开 http://127.0.0.1:8080/?level=0&seed=42

# 底层等价命令：
web/build.sh --serve                # 构建后起 serve.py，其余参数原样传给 serve.py
python3 web/serve.py --port 9000 --bind 127.0.0.1 --dir /path/to/dist --quiet
```

`serve.py`（Python 3.7+ 标准库，macOS 自带 python3 即可）：
- 默认目录 `web/dist`，`--dir` 改目录；`--port` 默认 8080；`--bind` 默认 `0.0.0.0`（只给本机用填 `127.0.0.1`）；`--quiet` 不打印请求；
- Content-Type：`.wasm` → `application/wasm`、`.webp` → `image/webp`、`.json` → `application/json`、`.js` → `text/javascript`；
- 对 HTML / wasm / JS / JSON 发 `Cache-Control: no-cache`（改完刷新即生效），图片 `max-age=300`；
- 启动时打印本机与局域网地址（手机连同一 Wi-Fi 打开即可；macOS 首次会弹防火墙提示，选「允许」）；
- 端口被占用时提示用 `lsof -i :端口` 查；Ctrl-C / SIGTERM 干净退出。

部署到 Mac（目标机只需 python3，不需要工具链）：

```sh
make build pack                                    # 仓库根目录 → web/match3-web-dist.tgz
# 拷到 Mac 后（Mac 上有仓库时同样在根目录用 make）：
make deploy-install TGZ=~/Downloads/match3-web-dist.tgz   # 默认 DEST=/Users/yubin/Documents/dev/haskell/match3-web
make deploy-start                                  # launchd 常驻；deploy-stop 停止，deploy-status 查看
# Mac 上没有仓库时，直接用包里的脚本：
bash deploy-mac.sh install match3-web-dist.tgz && bash deploy-mac.sh run   # 前台运行
```

注意：不要在一次性会话里 `nohup … &`（会话结束进程就被杀）；Mac 睡眠时不响应、重启后前台进程不会自己回来；
用 `lsof -i :8080` 或 `deploy-mac.sh status` 检查。详见 [`docs/web.md` §5](../docs/web.md#5-部署)（含 itch.io 静态托管）。

- 点一格再点相邻格交换，或按住拖向相邻格（拖过半格即交换）；
- 交换补间 → （第 44 关彩虹组合：第一轮之前的变身段）→ 逐轮高亮 / 消失 + 粒子 / 下落补子 → 连锁、连击浮字、震屏 → 步末效果（倒计时、传送带 / 毛球跳格、蔓延、蜗牛、洗牌）；
  播放期间锁输入，点击或空格加速（对应 `m3AnimTick(1)`）；
- 换了不能消：换过去再换回，不扣步；过关 / 通关 / 步数用完时出现结局遮罩；
- 按钮：第一行 ? / ‹ / › 切关、重开、提示（高亮核心 `findHint`）、撤销（核心 `Engine.History`，最多 20 步）；
  第二行（迁自桌面）锤 / 换 / 十 三种道具（图标 + 剩余次数）、洗牌、地图（选关，进度存 localStorage）、每日（每日挑战）、暂停（全部按键说明）；
- 快捷键同桌面：`H` 提示、`1/2/3` 道具、`U`/`Z` 撤销、`S` 洗牌、`D` 每日、`M` 地图、`K`/`B` 音效 / BGM、`R` 重开、
  `N`/回车/空格 加速或结局后前进、`P` 暂停、`?` 本关说明、`Esc` 关浮层；
- URL 参数 `?level=0..48&seed=N`（关卡下标 0 起，共 49 关）、`?daily=YYYY-MM-DD`、`?showcase=1`；
- HUD「目标 …」显示核心给的中文名（`state.goal.label`，如第 43 关「目标 毛球」），不显示内部名。

### 自适应布局（layout.js）
- 画布铺满视口，监听 `visualViewport` resize、`resize`、`orientationchange` 和 `ResizeObserver`；
  backing store = CSS 尺寸 × devicePixelRatio（上限 3）；改尺寸只重算布局，不重置对局与动画（e2e 已验证中途改尺寸）；
- 设计单位：格子 56、棋盘边距 16，按关卡行列数算棋盘大小；竖排（HUD 在上、按钮条在下）和横排（HUD 在右侧栏）
  各算一次缩放，取格子更大的那种；格子上限 112 CSS px；
- 安全区：用探针元素读 `env(safe-area-inset-*)`，布局避开；页面禁滚动 / 缩放 / 双击放大；
- 输入：指针事件经同一布局变换反算到格子，点选、拖划都支持；按钮条在纵向有富余时加高到 ≥ 46 CSS px；
- 精灵是 2x（112 px/格）：格子物理像素超过 112 时（dpr3 手机约 134、平板约 167）轻微放大，`imageSmoothingQuality = "high"`；
- 文字用浏览器字体（HUD 关名除外：同桌面画预渲染文字图 `name_<i>`，缺图时才退回浏览器字体），字号随格子缩放。

实测格子尺寸（e2e）：

| 视口 | 模式 | 格子 CSS px | 最小按钮 CSS px | 格子物理 px |
| --- | --- | --- | --- | --- |
| 375×667 dpr2（iPhone SE） | 竖 | 43.0 | 36.9 | 86 |
| 390×844 dpr3 | 竖 | 44.8 | 38.4 | 134 |
| 844×390 dpr3（横屏手机） | 横 | 40.4 | 38.3 | 121 |
| 768×1024 dpr2（平板） | 竖 | 76.7 | 65.7 | 153 |
| 1280×800 | 横 | 83.0 | 78.5 | 83 |
| 1920×1080 / 2560×1440 | 横 | 112（上限） | 106 | 112 |

（`feat/web-sdl-parity` 起竖排按钮条两行、横排侧栏四行，格子与按钮比之前小一些，见 docs/web.md §2.6。）

必须通过 HTTP 访问（`file://` 下 `fetch` wasm 会被浏览器拒绝）。服务器需给 `.wasm` 返回
`application/wasm`（python http.server 默认如此），否则 `instantiateStreaming` 会失败。

## 4. 测试

一般在仓库根目录直接 `make test`（或分别 `make test-native` / `make parity` / `make anim-parity` / `make e2e`）；
下面是各自的底层命令。当前（2026-10-03，chore/audit-wrapup，基于 fa719fa）：49 关，`stack test` 460 个用例全过，
状态一致性 40 组、动画一致性 34 组（都含第 43–48 关与迁自桌面的道具 / 每日 / 前进走法），e2e 258 项全过（第 5 节 90 项为迁自桌面的功能）。

```sh
# 无头浏览器：真实鼠标点选/拖拽，截图到 /workspace/match3-web-shots/，并输出 report.json
NODE_PATH=~/.ghc-wasm/nodejs/lib/node_modules ~/.ghc-wasm/nodejs/bin/node web/test/e2e.mjs
#   默认用 /usr/bin/google-chrome，可用 CHROME=/path/to/chromium 覆盖
#   服务器端口：E2E_PORT=8765（默认）；同一台机器上并行跑多份 e2e（多个工作树 / 多个任务）时各设一个，
#   例如 make check E2E_PORT=18765。端口已被占用时 e2e 直接报错退出（不会连到别人的服务器）

# 原生 vs wasm 一致性（仓库根目录）
~/.ghc-wasm/nodejs/bin/node web/test/node-parity.mjs 0 20260929 12 > /tmp/wasm.txt
stack exec -- runghc -isrc -iweb/hs web/test/Parity.hs 0 20260929 12 > /tmp/native.txt   # 需 LANG=C.UTF-8
cmp /tmp/wasm.txt /tmp/native.txt && echo 一致

# 动画帧一致性：原生 ComboFx 与 wasm 逐帧 JSON 相同（AnimParity 需 -iapp/pure，因为 ComboFx 与 UI.Presentation 在 app/pure）
stack exec -- ghc -O1 -isrc -iapp/pure -iweb/hs -outputdir /tmp/par/o -o /tmp/par/animparity web/test/AnimParity.hs
/tmp/par/animparity 27 1 20 > /tmp/native-anim.txt
~/.ghc-wasm/nodejs/bin/node web/test/node-anim-parity.mjs 27 1 20 > /tmp/wasm-anim.txt
cmp /tmp/native-anim.txt /tmp/wasm-anim.txt && echo 一致

# 第 4 个参数是走法：hint（缺省，按核心提示）/ combo（盘上有「彩虹 × 直线 / 炸弹」相邻就先换它，覆盖第 44 关的变身步；
# 提示不会主动选彩虹组合）/ combo-bomb（同 combo，但先换「彩虹 × 炸弹」）/ cham-rainbow（先换「彩虹 × 变色龙」，覆盖第 47 关的成对交换规则 15）
# / fix-RCRC-RCRC-…（按写死的交换走，每对 4 个数字 r1 c1 r2 c2，用完后按提示；第 48 关 4 组在魔法地格上引爆的用例）。parity.sh 的 CASES 写成「关卡:种子:走法」
/tmp/par/animparity 43 1 20 combo > /tmp/native-anim.txt
~/.ghc-wasm/nodejs/bin/node web/test/node-anim-parity.mjs 43 1 20 combo > /tmp/wasm-anim.txt
```

e2e 截图（每次运行先清空输出目录）：`01–06` 主流程（开局、无效交换退回、连锁三帧、撤销、结局），
`07–12` 特殊块爆炸、步末蜗牛 / 传送带 / 蔓延、第 39 关果冻、第 40 关气泡，`20-分辨率-*` 七种视口，
`30–32` 下落中改尺寸前后与播完，`rules-badge-*` 第 41 关规则开关角标（竖屏 390×844、横屏手机 844×390、桌面 1280×800）
与 `rules-badge-l42-*` 第 42 关「魔石」（魔法石是元素不是规则开关，没有角标），`magic-stone-charge*` 魔法石 0–3 格充能，
`snow-boss-*` 第 45 关「雪怪」Boss（竖屏 / 横屏开局与血条、扣血那一轮的高亮、召唤雪块的步末、血量过半的受伤表情）。
`fuzzball-float-a-up` / `fuzzball-float-b-down` 第 43 关毛球浮动的两帧（偏移 −2 / +2 设计像素，e2e 按像素测出两帧纵向平移 ≈ 4 设计像素 × 缩放）、
`fuzzball-jump-mid-l43-*` 毛球步末跳格（皮带段）中间帧、`goal-label-l43-*` 第 43 关 HUD「目标 毛球」（竖屏 390×844 / 横屏 1280×800）、
`rules-badge-l44-*` 第 44 关规则角标「彩虹组合变身」、`rainbow-transform-{line,bomb}-mid-l44-*` 彩虹 × 直线 / 炸弹变身段中间帧、
`cookie-drop-*` 第 46 关「掉落口」（竖屏 / 横屏开局的掉落口标记、掉落口补下饼干的下落段与补完后的盘面）、
`chameleon-l47-*` 第 47 关「变色龙」（竖屏 / 横屏开局、HUD 目标图标）、`chameleon-crop-{before-generic,after}` 通用画法反证前后、
`chameleon-shift-{before,mid}` 步末换色段前半 / 后半、`chameleon-l47-lost` 玩到失败的结算层、
`magic-l48-*` 第 48 关「魔法格」（竖屏 / 横屏开局的魔法地格、`magic-l48-lost` 玩到失败）、`wide-l49-won` 第 49 关「宽域」终章通关、
`magic-widen-seed{2,3,4,5}` 扩圈爆炸那一轮的消失段、`sound-chips-l{01,48,49}-*` HUD 音效 / BGM 开关芯片、`level-name-l{01,41,48}` HUD 关名（预渲染文字图 `name_<i>`）。
另有逐关贴图护栏：每关开局 + 走 3 步后 `m3debug.fallbacks`（走几何降级的格子，按元素名计）必须为空——新元素合入 main 后要在 `www/cells.js` 补画法，漏了 e2e 会失败
（`report.json` 的 `fallbacksByLevel` 逐关记录，第 43–48 关另有单独的检查项；地面层表外名字 / 缺贴图记为 `<名字>#地面层`；多格 Custom 元素走了通用「元素名贴图 + 角标」画法时记为 `<元素名>#多格通用画法`，带颜色 `c` 的 Custom 格（第 47 关变色龙）走通用画法时记为 `<元素名>#通用画法缺底层宝石`，第 45 关雪怪接入前就是这样画成每格一只整图 + 角标 9、原护栏查不出，见 docs/web.md §2.3）；同一轮逐关检查 HUD 目标标签 = 「目标 」+ `state.goal.label` 且不含 `[a-z_]` 内部名（`goalLabels`）。
报告里有每个视口的格子尺寸、耗时（tick / 绘制均值）和控制台错误。

## 5. 网页端与核心的接口

wasm 导出 19 个 **同步** JSFFI 函数（`foreign export javascript "... sync"`），都返回 JSON 字符串：

| 导出 | 参数 | 返回 |
| --- | --- | --- |
| `m3New(level, seed)` | 关卡序号（0 起）、种子 | `{ok, state}` |
| `m3Swap(r1, c1, r2, c2)` | 两个格子 | `{ok, accepted, outcome, trace, events, state}` |
| `m3Undo()` | – | 同 `m3Swap`（`trace` 为空脚本、`events` 为空；没有历史时 `accepted:false`） |
| `m3State()` | – | `{ok, state}` |
| `m3Levels()` | – | 关卡列表 `[{index,name,moves,goal}]` |
| `m3Meta()` | – | 表现表（`UI.WebMeta`，`main.js` 启动时读一次，JS 不手抄）：`{colorRGB,elementRGB,colorTags,tagRGB,fallbackRGB,spreadCrumbRGB,tickCrumbRGB,spreadGlow,spreadCurves,frames,sounds,chapters}`（`chapters = [{start,label,title}]`，`UI.Chapters`，选关地图用） |
| `m3AnimStart()` | – | 为上一次被接受的 `m3Swap` 建动画播放器：`{ok,anim:true,boards,base,fall}`；不需要播放时 `{ok,anim:false}` |
| `m3AnimTick(fast)` | 0 / 1（1 = 加速） | 推进一帧（60 fps 固定步长）：播放中 `{p,fr,n,w,k,g,b[,s][,ev]}`，播完 `{done:true,b,best,g,fall}` |
| `m3Hammer(r,c)` / `m3Cross(r,c)` | 一格 | 道具锤子 / 十字消：同 `m3Swap`，另带 `keepTool` |
| `m3FreeSwap(r1,c1,r2,c2)` | 两个格子 | 道具自由交换：同 `m3Swap`，另带 `keepTool`（换不掉时 `true`，前端留在点选模式） |
| `m3Shuffle()` | – | 手动洗牌：同 `m3Swap` |
| `m3Daily(y,m,d)` | 日期 | 开每日挑战：`{ok, state}`（`state.daily = true`） |
| `m3Restart(sm,seed)` | 开局步数、种子 | 重开（每日挑战按开局步数重开同一配置）：`{ok, state}` |
| `m3Advance(sm,seed)` | 开局步数、种子 | 结局后前进：`{ok, accepted, startMoves, state}`（未结束时 `accepted:false`） |
| `m3Showcase()` | – | 元素展示盘：`{ok, state}` |
| `m3Progress(reached,sm)` | 已解锁关、开局步数 | 只读 `{reached, stars, dots}`（`dots` 每关一字：C 当前 / D 已过 / U 已解锁 / L 未解锁） |
| `m3MapJump(reached,li)` | 已解锁关、点的关 | 只读 `{jump}`（`null` = 不跳） |
| `m3Badge(replaying,combo,shown,summaryLeft,best)` | 回放状态 | 只读分数徽章 `{kind:"combo|rolling|summary|score", n, shuffled}` |

- `state`：`level/name/daily/title/boosters/rules/score/moves/goal/progress/target/boss/over/loseHint/combo/shuffled/undo/hint/lastCleared/ground/board`
  （`daily` = 每日挑战局；`title` = 窗口标题行（`titleLine`）；`boosters = {hammer,swap,cross}` 剩余道具次数；`undo` = 可撤销步数；`ground` = 地面层 `[{p,name,layers}]`，如第 39 关果冻；`goal = {kind,text,target[,name],label}`：`goal.name` 为 `GoalNamed` 的元素名，
  `goal.label` 为中文显示名（视图模型 `Match3.View.goalLabel`：分数 / 收集红色宝石 / 多色收集（红 / 蓝）/ 碎石 / 毛球 …，名字目标查 `namedGoalLabelTable`（由元素 caps 的 `labelled` 推出），
  没登记的名字退回元素名），HUD「目标 …」直接画它，`m3Levels` 的 `goal` 同样带 `label`；
  `boss` = 雪怪 Boss 血条 `{hp,max}`（视图模型 `gvBoss`，目标不是「击败 Boss」时为 `null`），`hud.js` 用它把目标条换成血条）；
- `outcome.tag`：`MoveApplied | NoMatch | InvalidSwap | LevelClear | Won | Lost`；
- 执行路径：`Api.hs` 只调通用接口 `gameStep match3Shell`（与桌面外壳 app/UI/Plugin.hs 相同），
  表现数据全部取自 `stepReport`（`Played`），规则每步只算一次；
- 状态 JSON（第 11 刀起）：`encodeState` / `apiLevels` / 盘面编码读视图模型 `Match3.View`（与桌面 HUD / 标题同一份读数），不再从 `GameState` 现算；
- `trace`：`pdTrace` 的逐轮快照 `start → waves[{before,cleared,drained,holes,after,score}] → end[] → final → shuffle`，
  前端按它逐轮播放；`end[i] = {afterWaves,before,after,effect}`，`effect` 为结构化步末效果：
  `{type:"tick",cells}` / `{type:"belt",pairs}` / `{type:"spread",kind:"vine|choco|steam",pairs}` / `{type:"snail",moves:[{from,to,dir,pushed}]}`；
  第 43 关毛球的步末跳格也是 `belt`（毛球与相邻宝石互换，两项）；第 44 关彩虹组合的变身是 `afterWaves = 0` 的
  `{type:"spread",kind:"rainbow_line|rainbow_bomb",pairs:[[彩虹格],[同色宝石]]…}`，在第一轮之前播放；
  `shuffle` 为本步触发自动洗牌后的盘面（否则 `null`）；
- `events`：规则层效果事件 `pdEvents`（按时间顺序）`{kind,beat,subject,pairs:[[来源],[目标]],amount}`，
  `kind` ∈ `clear/hit/blast/drain/score/combo/tick/belt/spread/move/shuffle`（同 `Match3.Engine.eventKindTag`），
  `beat` 与 `end[].afterWaves` 同一时间轴、同 beat 同时播放；比通用 `Engine.Effect` 多保留来源格（爆炸方向、移动轨迹）；
- `state.rules`：本关打开的规则开关角标 `[{name,text,icons}]`（视图模型 `gvRules` 查 `Match3.View.ruleBadge`，与桌面 HUD 同一张表；
  如第 41 关 `[{"name":"bomb_shapes","text":"L/T 形出炸弹","icons":["bomb_glow","bomb_mark"]}]`，其余关卡与每日挑战为 `[]`）。
  前端 `hud.js` 在关卡面板「第 N 关」右侧逐个画「叠放图标 + 文字」小胶囊（文字用画布字体，图集里没有文字贴图），
  竖屏 / 横屏同一套；放不下时先截断文字、再只留图标。新规则只要在 `ruleBadgeTable` 登记一行，两个前端自动显示；
- `state` 另含关卡级元素：`belts`（皮带路径）、`portals`（传送门对）、`ufos[{p,c}]`、`carpets`、`carpetOpen`、`drops`（饼干掉落口格，视图模型 `bvDrops`；`cells.js` 的 `drawDrops` 在格子上沿画 `cookie_drop`）；
- `board`：行 × 列（每关不同），结构化编码，每格都带 `"s"`（核心 show 文本）：
  宝石 `{"t":"G","c":1..5,"k":"N|H|V|B|R","i":冰层,"o":覆盖物名|null,"n":覆盖层数}`；
  其他 `t` ∈ `stone/chest/honey/balloon/cookie/cake/hat/maker/snail(dr,dc)/safe/flip(c,b)/surprise/bottle/spirit/countdown/custom(name,v)`，
  各带自己的字段；`custom` 格后面按顺序追加元素自带的显示字段（`Match3.View.cellExtras`，元素 caps 的 `displays`）：雪怪 `q/hurt/turn/every`（象限、血量是否过半、召唤计数、周期）、变色龙 `c`（当前颜色）；
  前端 `cells.js` 按 `t` 取精灵（`custom` 先按名字查 `CUSTOM_ART`）。

### 动画接口
- 播放器就是桌面 `UI.ComboFx` 的状态机（同一份源码），建立条件与桌面 `withMovePlayback` 相同；前端每帧调一次 `m3AnimTick`，
  只负责按相位插值画图，帧数和事件序列由核心决定；
- `boards`：本步用到的全部盘面，编号顺序为 `[trace.start] ++ 每轮 [before, after] ++ 每个步末 [before, after] ++ [trace.final, state.board]`；
  逐帧 JSON 里的 `b` / `s.b0` / `s.b1` 都是这个表的下标，所以单帧很小（≤ 约 140 B）；`base` = 静止时显示的盘面下标；
- `fall[轮][行][列] = 下落格数 × 2 + (是否新补子)`；
- 逐帧字段：`p` 相位（flash/pop/fall/rest/end…）、`fr` 相位内帧、`n` 相位总帧、`w` 轮下标、`k` 连击数、`g` 已得分、`b` 当前盘面；
  `s` 为步末效果 `{kind,e:[end 下标],b0,b1,n}`；`ev` 为本帧事件 `{e:"hl"|"van",k,w}` / `{e:"end",kind}`；
- 播完的 `fall:true` 表示显示盘面与最终盘面不同（如自动洗牌），前端再补一段轻落；
- 实测 `m3AnimTick` 约 0.12–0.15 ms/帧（node）；浏览器里 tick 均值约 0.3 ms，绘制均值约 0.8–1.2 ms/帧（dpr3）。

当前局面保存在 Haskell 侧的全局 `IORef`（单线程 RTS，一个页面一局）；异常会被兜底成 `{ok:false,error}`。

## 6. 已知限制

- 网页是唯一前端：桌面版功能已全部迁来（道具、洗牌、每日挑战、选关地图、暂停、结局后前进等，对照表与「可删桌面代码」见 docs/web.md §2.6）；PC 壳尚未做；
- 小屏触控：iPhone SE 竖屏格子 43 CSS px、按钮最小约 37；横屏手机格子约 40.4 / 按钮约 38 CSS px，低于 44 的建议值
  （8 列棋盘宽度受限；横屏是高度受限，侧栏有四行按钮），拖划交换可弥补；
- 3x 图集：面积是 2x 的 2.25 倍，WebP 估计多约 400 KB，只对 dpr3 手机和平板（格子物理 130–170 px）有收益，
  目前放大后观感可接受，先不做，可作为可选项（按 dpr 选图集）；
- 播放期间 HUD 的分数 / 装饰层显示的是结算后的状态（与桌面版一致），不逐轮递增；
- `m3Swap` 仍返回完整 JSON（中位数约 30 KB/步，长连锁可达约 120 KB），没做增量；
- 真机（iOS Safari / Android Chrome）与 itch.io 上线尚未实测；TODO 列表见 [`docs/web.md` §9](../docs/web.md#9-todo)。
