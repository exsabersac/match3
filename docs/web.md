# 网页版（GHC WebAssembly）

> 技术验证，最初在分支 `web-wasm-spike` 上开发，已合入 main（`59f1e53`）。操作细节（命令、参数）以 [`web/README.md`](../web/README.md) 为准，
> 本文讲结构与取舍，供评审阅读。

## 1. 一句话

用 GHC 9.14 的 wasm 后端把**纯规则核心**（`src/Engine/*` + `src/Match3/*`）和**动画状态机**（`app/pure/ComboFx.hs`，帧数读同目录的表现表 `UI/Presentation.hs`）
编成一个 `.wasm`，浏览器里的 JS 只做三件事：**加载、画、收输入**。规则判定、连锁时间轴、帧数都在 Haskell 里算，
所以同关卡同种子，网页版与桌面版的每一步结果、每一帧动画相位都逐字节一致（有测试守着，见 §7）。

核心源码一行未改：`web/match3-web.cabal` 直接用 `hs-source-dirs: hs ../src ../app/pure` 引用仓库里的模块。

## 2. 结构

```
浏览器
┌───────────────────────────────────────────────────────────────────────┐
│ index.html（一张全屏 <canvas>）                                        │
│ main.js ── 加载 wasm + 图集，rAF 固定步长（60 fps），输入，交换 / 连锁流程 │
│   ├─ layout.js  自适应布局：竖排 / 横排，安全区，CSS↔设计单位变换，命中检测 │
│   ├─ render.js  盘面与动画（交换、逐轮消除下落、步末效果、粒子、浮字、震屏） │
│   │    └─ cells.js  桌面 UI.CellTable 的 JS 移植：每种格子怎么画          │
│   │         └─ art.js  图集绘制：普通 / 着色 / 叠加 / 旋转 / 九宫格        │
│   └─ hud.js     HUD 面板、目标进度、按钮、结局遮罩                        │
│        │  JSON 字符串（同步 JSFFI 调用）                                   │
│        ▼                                                               │
│ match3-web.wasm（WASI reactor + ghc_wasm_jsffi.js 胶水 + WASI 垫片）      │
│   WebMain.hs    JSFFI 导出；当前局面、动画播放器放在 IORef（一页一局）      │
│   Match3Web/Api.hs   gameStep match3Shell → JSON（与桌面外壳同一条路径）   │
│   Match3Web/Anim.hs  Played → ComboFx 播放器 → 逐帧 JSON                  │
│   Match3Web/Json.hs  极简 JSON（只输出整数，保证原生 / wasm 输出一致）      │
│   ── 核心：Engine.* / Match3.*（纯）  ComboFx（纯阶段机）                  │
└───────────────────────────────────────────────────────────────────────┘
```

依赖方向与桌面版相同：接口层只依赖核心和 `ComboFx`，不依赖 SDL。`Api.hs` / `Anim.hs` 没有 JSFFI，
原生 GHC 也能编，这是原生 / wasm 对比测试的基础。

### 2.0 元素框架在 wasm 里

main 在 2121bf8 把元素改成类型类：`Match3.Element.Class` 定义 `class Element`（本体）/ `Modifier`（冰层、叠层）/
`LevelElement`（飞碟、皮带、传送门、地毯、地面层等关卡级机制，第 7 刀起状态在元素值里）及对应的存在类型 `SomeElement` / `SomeModifier` / `SomeLevelElement`，
`Match3.Element.Message` 是主流程发给元素的消息（xmonad 风格：`Refilled` / `EndTicked` / `Settling` / `Covering` …）；`Match3.Element.Level` 管一局的关卡级元素（`gsLevelElems`），`Match3.Board.Hooks` 是 Board 层收的钩子记录；
内置元素按功能分到 `Match3.Element.Builtin.{Gem,Layer,Obstacle,Collectible,Actor,Ground,Level,Common}`，
`Match3.Element.Builtin` 仍导出 `builtinDefs` / `builtinLevelDefs` / `defaultRegistry`；注册表是「元素名 → 构造器」。
第 9 刀起 `class Element` 只剩 `name` / `toCell` / `caps`，能力是带默认值的记录 `Caps`（声明简写在新模块 `Match3.Element.Caps`，已同步进 `web/match3-web.cabal`）；
网页接口层（`web/hs`）不调元素类，签名与 JSON 都不变。

对网页版的影响：

- 这些模块全部是纯 Haskell（只多了 `ExistentialQuantification`），原样编进 wasm；`web/match3-web.cabal` 的模块清单与
  `package.yaml` 同步（`build.sh` 第 1 步会核对）；
- 网页接口层**不直接用元素框架**：`Api.hs` 只走 `gameStep match3Shell`，盘面编码按 `Match3.Types` 的 `Cell` 构造器
  （宝石 / 各障碍 / `Custom 名字 值`）输出，而 `Cell` 类型没有变（第 6b 刀起名字 / 状态是 newtype，编码处用 `unElementName` / `unCustomState` 取出，JSON 不变）；`Anim.hs` 只用 `ComboFx` 和效果事件；
- 第 7 刀起 `GameState` 的关卡级字段收进 `gsLevelElems`，`Api.hs` 读的 `gsGround` / `gsBelts` / `gsPortals` / `gsUfos` / `gsCarpetOpen`
  变成 `Match3.Core` 导出的同名派生读数，源码与 JSON 都不用改；
- 第 7b 刀起步末效果 `EndEffect` 是通用形状（事件类型 + 元素名 + 逐项 `EndItem`），`Api.hs` 的 `encodeEndEffect` 改为按
  事件类型编码（`tick` / `belt` / `spread` / `snail` 四种输出与之前逐字节相同；其余事件类型编码为 `{type, kind, pairs}`）；
- 第 11 刀起 `encodeState` / `encodeGoal` / `apiLevels` / `encodeCell` 全部读视图模型 `Match3.View`（`gameView` / `GoalInfo` / `BoardView` / `levelViews` / `cellFace`，与桌面 HUD、标题同一份读数），`Api.hs` 不再从 `GameState` 现算（测试 `frontends_read_view_model` 扫描）；字段与顺序逐字搬迁，JSON 逐字节不变（`make check` 22 组一致）；
- 因此 JSON 形状、`cells.js`（CellTable 移植）的映射都不用改。合入前后 22 组一致性输出（原生与 wasm 各一份）逐字节相同，
  说明规则行为与编码都没变。新增元素时：元素框架里注册即可生效，网页端只在它引入新的 `Cell` 构造器或新贴图时才要改
  `Match3.View.cellFace` 与 `cells.js`。

### 2.1 wasm 导出（`WebMain.hs`）

全部是同步 JSFFI（`foreign export javascript "… sync"`），参数是整数，返回 JSON 字符串；异常兜底成 `{ok:false,error}`。

| 导出 | 用途 |
| --- | --- |
| `m3New(level, seed)` | 开一局（清空历史与动画） |
| `m3Swap(r1,c1,r2,c2)` | 交换一步：`{accepted, outcome, trace, events, state}` |
| `m3Undo()` | 撤销（核心 `Engine.History`，最多 20 步） |
| `m3State()` / `m3Levels()` | 当前状态 / 44 关列表 |
| `m3AnimStart()` | 为上一步建 ComboFx 播放器，返回本步用到的盘面表与下落表 |
| `m3AnimTick(fast)` | 推进一帧，返回相位、帧号、连击、得分、当前盘面编号和本帧事件 |

`state` 包含分数、步数、目标、结局、提示、规则开关角标 `rules`（视图模型 `gvRules` → `Match3.View.ruleBadge`，HUD 在关卡面板里通用地画，
与桌面同一张表）、地面层（果冻）、关卡级元素（皮带、传送门、飞碟、地毯）和结构化盘面（每格 `{"t":种类,…,"s":show 文本}`）。字段细节见 `web/README.md` §5。

### 2.2 ComboFx 在 wasm 里

桌面版的连锁回放由 `ComboFx.cascadeStages`（高亮 → 消失 → 下落 → 落定，再加步末：倒计时 / 皮带 / 蔓延 / 蜗牛 / 洗牌）
和 `Engine.Playback.Player`（帧号、加速）驱动。网页版把这两块原样编进 wasm：

- `m3AnimStart` 按与桌面 `withMovePlayback` 相同的条件建播放器，一次性返回本步的**盘面编号表**
  （`[start] ++ 每轮 [before, after] ++ 每个步末 [before, after] ++ [final, state.board]`）和下落表；
- 之后每帧 `m3AnimTick` 只返回编号和几个整数（≤ 约 140 B），JS 按相位插值绘制；
- 加速（点击 / 空格）就是 `m3AnimTick(1)`，与桌面同一套加速规则。

好处：动画节奏不需要在 JS 里再写一遍，也不会与桌面版漂移。代价：每帧一次 JSFFI 调用，实测约 0.14 ms（node）/ 0.3 ms（Chrome），可以忽略。

### 2.3 JS 渲染器

- Canvas 2D，`requestAnimationFrame` + 固定步长累加器（60 fps 逻辑帧，渲染帧率随显示器）；
- 流程：`doSwap` → 交换补间（不能消时换过去再换回）→ `m3AnimStart` → 每帧 `m3AnimTick` → 播完（必要时补一段轻落，如自动洗牌）→ 刷新 HUD；
- 播放期间锁输入（与桌面 `animBusy` 一致），按钮 / 撤销在播完后可用；
- 特效（粒子、连击浮字、得分浮字、震屏）只在 JS 里，由 `m3AnimTick` 的事件（`hl` / `van` / `end`）触发，不影响规则；
- `window.m3debug` 暴露状态、布局、耗时和断点钩子，给 e2e 用；
- **新元素要跟进 `cells.js`**：格子 JSON 由核心 `Match3.View.cellFace` 生成，新元素（包括 `Custom` 自定义元素）合入 main 后网页端
  不会自动有画法——`cells.js` 的 `primarySprite`（主贴图名）/ `CELL_ART`（画法）/ `ELEMENT_RGB`（降级色）要补，贴图名要在网页图集里
  （`web/tools/gen_web_atlas.py` 只跳过文字图 `g_*` / `zh_*` / `name_*` 与 `@` 变体）。漏了的格子会走几何降级（色块 + 类型名，
  如魔法石合入时的「custom」灰块）。回归护栏：`cells.js` 按元素名统计走降级的次数，`m3debug.fallbacks` 暴露；e2e 对**每一关**
  开局并按提示走 3 步，图集加载后它必须为空，否则列出元素名和关卡（见 [testing.md](testing.md#网页版测试make-check)）。
  例：第 42 关魔法石 `{t:"custom", name:"magic_stone", v:0..4}`（4 = 发射中）画 `magic_stone_${min(3,v)}`，满 3 格时像桌面 `sprBob` 一样浮动。

### 2.4 自适应布局（`layout.js`）

- 画布铺满视口；监听 `visualViewport` resize、`resize`、`orientationchange`、`ResizeObserver`；
  backing store = CSS 尺寸 × `devicePixelRatio`（上限 3）；改尺寸只重算布局，对局与动画不受影响；
- 所有绘制用**设计单位**（格子 56、边距 16），一个缩放系数 `u` 把设计单位映射到 CSS 像素；
  竖排（HUD 在上、按钮条在下，手机）与横排（HUD 在右侧栏，桌面 / 平板 / 横屏手机）各算一次 `u`，取格子更大的那种；
  棋盘按每关的行 × 列算，格子上限 112 CSS px；
- 避开 `env(safe-area-inset-*)`（探针元素读取），`viewport-fit=cover`，页面禁滚动 / 缩放 / 双击放大（`touch-action: none`）；
- 指针事件用同一个变换反算到格子 / 按钮，点选与拖划都支持；竖排纵向有富余时按钮条加高到 ≥ 46 CSS px；
- 文字用浏览器字体，字号随 `u` 缩放。

实测：iPhone SE（375×667）格子 43 CSS px，390×844 为 44.8，横屏手机 43.7，平板 83，1280×800 为 90，1920 以上封顶 112。

### 2.5 资源管线

```
tools/gen_assets.py（桌面版，已有）→ assets/*.bmp（2x，112 px/格）
                                          │  只读
web/tools/gen_web_atlas.py（Pillow）─────┘→ atlas.webp（111 张，1024×1350，约 327 KB）
                                            atlas.json（约 3 KB，名字 → 矩形）
                                            background.webp（约 17 KB）
```

- 由 `web/build.sh` 第 3b 步调用，结果缓存在 `web/.cache/art`，`assets/` 或生成器变动才重新生成；
- 不收文字图（`g_` / `zh_` / `name_`，网页用浏览器字体）和 `@` 变体，保留角标 `badge_*`；
- 着色 / 加色在 JS 里用离屏画布缓存（对应桌面 `Art` 的染色 / 加色绘制）；
- 格子物理像素超过 112（dpr3 手机约 134、平板约 167）时轻微放大，`imageSmoothingQuality = "high"`，观感可接受。

体积（2026-09-30，对齐桌面版 GHC 9.14.1 之后）：wasm `-Oz` 后 1,737,478 B ≈ 1.74 MB（gzip 674,329 B）；dist 合计 2,194,833 B ≈ 2.19 MB，逐文件 gzip 约 1.05 MB。

## 3. 工具链与构建

- 工具链：[ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) `FLAVOUR=9.14`，装在 `~/.ghc-wasm`（约 6.4 GB），
  与桌面版的 Stack / GHC 9.14.1（原生 x86_64 / arm64）完全独立；`~/.ghc-wasm/env` 会改 `CC` 等变量，**不要 source 进日常 shell**；
- 随机数：`web/cabal.project` 把 `random` / `splitmix` 钉在与桌面版 `stack.yaml` 相同的版本（`extra-deps` 的 random-1.2.1.1 / splitmix-0.1.0.5；桌面版现为 GHC 9.14.1，lts-24.60 + `compiler: ghc-9.14.1`），保证同种子同结果；
- 构建：`web/build.sh` → 核对模块清单与 `package.yaml` 一致 → `wasm32-wasi-cabal build` → `wasm-opt -Oz` → JSFFI 胶水
  → WASI 垫片（`@bjorn3/browser_wasi_shim`，缓存）→ 图集 → `web/dist/`，最后打印体积；
- 构建只在 Linux 盒子上做过；macOS 上理论可行（ghc-wasm-meta 支持），未验证。部署到 Mac 不需要工具链，只拷 `dist/`。

### 3.1 make 目标

仓库根目录的 `Makefile` 是日常入口，**在仓库根目录运行 `make <目标>`**；`make help` 按分组列出
（通用 / 桌面版 / 网页版构建与运行 / 网页版测试 / 打包与部署 / 清理 / 环境）。桌面版目标只是 Stack 命令的薄包装。
第一次先 `make doctor` 看缺什么，再 `make toolchain`（已装则只校验）→ `make build` → `make test`；CI 用 `make check`。
在 box 上从 `make clean` 开始跑 `make check`（完整重编 wasm + 四组测试 + 体积）约 2 分 45 秒。

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
| `make test-native` | `stack test`（核心 357 个，桌面版与网页版共用） |
| `make parity` / `make anim-parity` | 状态 / 动画一致性（`web/test/parity.sh`；`STEPS=`、`CASES="关卡:种子 …"` 可改） |
| `make e2e [SHOTS=目录]` | 无头 Chrome 端到端测试（`CHROME=` 可改浏览器） |
| `make test` | 以上四组测试依次跑 |
| `make check` | CI 用：`lint-sh` → `build` → `test` → `size` |
| `make lint-sh` | shell 脚本 / Makefile 检查：`$变量名` 后紧跟中文等非 ASCII 字符即报错（macOS bash 3.2 会读错变量名，须写 `${VAR}`；见 `docs/testing.md`） |
| `make size` | wasm 原始 / `-Oz` 后、dist 各文件与合计，原始与 gzip -9 |
| `make pack [TGZ=…]` | 打包 dist + serve.py + 部署脚本 |
| `make deploy-install [TGZ=…] [DEST=…]` | 在目标机上解包安装到 DEST（默认 `/Users/yubin/Documents/dev/haskell/match3-web`） |
| `make deploy-start` / `deploy-stop` | 仅 macOS：launchd 常驻 / 停止（`DEST`、`PORT`、`BIND` 可改） |
| `make deploy-status` | launchd 状态 + `lsof` 端口监听 + curl 自检 |
| `make clean` | 只清网页版：`web/dist`、`web/dist-newstyle`、`web/.cache`、`web/*.tgz`；不碰 `~/.ghc-wasm` 和 `.stack-work`（桌面版用 `stack clean`） |
| `make android-sync` / `apk` / `apk-release` / `aab` / `android-check` | 安卓：同步 dist 进 Capacitor 工程 / 调试版 APK / 正式版 APK / Play 用 AAB / 桌面 Chrome 手机视口替代验证（`web/android-app/build-apk.sh`，见 [`android.md`](android.md)） |

## 4. 本地运行

```sh
make build serve                    # 仓库根目录：构建后用 web/serve.py 起服务器（默认 0.0.0.0:8080）
make serve PORT=9000 BIND=127.0.0.1 # 已构建过：直接起；改端口 / 只给本机
# 底层等价：web/build.sh --serve，或 python3 web/serve.py [--port --bind --dir --quiet]
```

`web/serve.py` 只用 Python 3 标准库（3.7+，macOS 自带的 python3 即可），它：

- 给 `.wasm` 发 `application/wasm`（`instantiateStreaming` 的硬性要求），`.webp` / `.json` / `.js` 也按正确类型；
- 对 HTML / wasm / JS / JSON 发 `Cache-Control: no-cache`，改完刷新即生效；图片短缓存 5 分钟；
- 启动时打印本机和局域网地址；端口被占用时提示 `lsof -i :端口`；Ctrl-C / SIGTERM 干净退出。

必须走 HTTP，`file://` 打开会被浏览器拒绝加载 wasm。

### 手机 / 局域网访问

1. 电脑和手机连同一个 Wi-Fi（访客网络常开「客户端隔离」，会不通）；
2. 服务器监听 `0.0.0.0`（默认），手机浏览器打开启动时打印的 `http://<局域网 IP>:8080/?level=0`；
3. macOS 首次运行会弹「是否允许 python3 接受传入网络连接」，选**允许**；点错了到
   「系统设置 → 网络 → 防火墙 → 选项」里把 Python 改成允许；
4. 局域网 HTTP 不是安全上下文，但本项目只用 WebAssembly 与 Canvas，不受影响（真机尚未实测，见 §8）。

## 5. 部署

### 5.1 Mac 上常驻（`web/deploy-mac.sh`）

```sh
# 构建机上（仓库根目录）
make build pack                                   # → web/match3-web-dist.tgz（dist + serve.py + 脚本）
# 把 tgz 拷到 Mac 后。Mac 上有仓库时在仓库根目录：
make deploy-install TGZ=~/Downloads/match3-web-dist.tgz   # 默认 DEST=/Users/yubin/Documents/dev/haskell/match3-web
make deploy-start                                 # 交给 launchd 后台常驻（DEST、PORT、BIND 可改）
make deploy-status                                # launchd 状态 + lsof -i :8080 + curl 自检
make deploy-stop                                  # 停止
# Mac 上没有仓库时，用包里自带的脚本（子命令一一对应）：
bash deploy-mac.sh install match3-web-dist.tgz    # 安装
bash deploy-mac.sh run                            # 前台运行（exec），Ctrl-C 停
bash deploy-mac.sh start | status | stop [--remove]   # launchd 常驻 / 状态 / 停止（--remove 同时删 plist）
```

注意事项：

- **不要用 `nohup … &` 在一次性会话里起服务**：远程命令 / 自动化工具的会话一结束，整组进程会被杀掉，
  表现为「启动成功，过一会儿就没了」。要么在自己开的终端（或 tmux）里前台 `run`，要么 `start` 交给 launchd
  （用户级 LaunchAgent `com.match3.web`，`KeepAlive` 崩溃自动拉起，登录后自动启动，日志在目标目录 `serve.log`）；
- **睡眠 / 重启**：Mac 睡眠时服务器不响应；重启后前台 `run` 不会自己回来，`start` 的 launchd 代理要等用户登录后才起。
  演示期间可用 `caffeinate -s bash deploy-mac.sh run` 防止睡眠；
- **检查是否在跑**：`lsof -i :8080`（或 `lsof -nP -iTCP:8080 -sTCP:LISTEN`），`bash deploy-mac.sh status`；
- **防火墙**：见 §4，launchd 起的 python3 同样会触发一次授权提示。

### 5.2 itch.io（静态托管，可选）

网页版是纯静态文件、全部相对路径，可以直接当 itch.io 的 HTML5 项目上传：

1. `cd web/dist && zip -r ../match3-web-itch.zip .`（`index.html` 必须在 zip 根目录）；
2. itch.io 新建项目，类型选 HTML，上传 zip 并勾选「This file will be played in the browser」；
3. 嵌入尺寸建议 960×640 并打开全屏按钮，勾选移动端友好（布局本身会适配任意尺寸）；
4. 上传后在桌面和手机浏览器各试一次，确认 wasm 正常加载（itch 的 CDN 应返回 `application/wasm`，未实测）。

桌面版的上传清单见 [`ITCH.md`](../ITCH.md)；网页版可以作为同一项目的在线试玩，或单独一个页面。
其他静态托管（GitHub Pages、Netlify、任意 nginx）同理，只要 `.wasm` 的 MIME 类型正确。

### 5.3 安卓应用（Capacitor）

同一份 `web/dist` 可以用 Capacitor 包成 Android 应用（WebView 离线加载，`.wasm` 由 Capacitor 本地服务器按
`application/wasm` 提供）：`make build apk` 产出调试版 APK。前置、签名、AAB、装机方法与已知限制见 [`android.md`](android.md)。

## 6. 调试要点

- 控制台 `m3debug.state` / `m3debug.layout` / `m3debug.hud`（上一帧关卡面板与规则角标的矩形）/ `m3debug.perf`；URL `?level=0..41&seed=N` 复现一局；
- 快捷键：`u` / `z` 撤销，`h` 提示，空格加速；
- 页面白屏先看网络面板里 `.wasm` 的 Content-Type（必须是 `application/wasm`）。

## 7. 测试

| 测试 | 守什么 | 怎么跑 |
| --- | --- | --- |
| `stack test` | 核心规则（357 个） | `make test-native` |
| 状态一致性 `Parity.hs` ↔ `node-parity.mjs` | 同关卡同种子，原生与 wasm 每步 `m3Swap` / `m3Undo` 输出逐字节相同 | `make parity`（14 组，含第 41 / 42 关） |
| 动画一致性 `AnimParity.hs` ↔ `node-anim-parity.mjs` | 每步全部帧 JSON 逐字节相同（含加速），并与 ComboFx `runPlayer` 核对帧数 | `make anim-parity`（12 组，含第 41 / 42 关） |
| e2e `web/test/e2e.mjs` | 无头 Chrome：真实指针交换、无效交换退回、连锁、撤销、特殊块、步末、果冻 / 气泡、7 种视口、动画中途改尺寸、第 41 关规则角标（三种布局不出框不重叠）与第 42 关无角标、serve.py 的 Content-Type、无控制台错误 | `make e2e`（端口 `E2E_PORT`，默认 8765） |

`make test` 依次跑这四组；底层命令见 `web/README.md` §4。当前结果（2026-09-30，web-rules-badge 合入 main 64c0351 后）：`stack test` 343 通过；状态一致性 14 组、动画一致性 12 组全部一致；e2e 54 项全过（含逐关贴图护栏）；`make android-check` 6 项全过。
e2e 截图输出到 `/workspace/match3-web-shots/`（编号 01–32 与 `rules-badge-*`，外加 `report.json`）。网页版自家模块编译 0 警告（`web/cabal.project` 对本包开 `-Werror`），e2e 端口用 `E2E_PORT` 改（默认 8765）。

## 8. 已知限制

- 只接了交换、撤销、提示、切关、重开；道具（锤子 / 任意交换 / 十字）、洗牌按钮、每日挑战、选关地图未接；
- 小屏触控目标略低于 44 CSS px（iPhone SE 格子 43，横屏手机按钮约 42）；
- 播放期间 HUD 显示的是结算后的分数与装饰层（与桌面一致），不逐轮递增；
- 没有音效；没有离线缓存；
- `m3Swap` 每步返回完整 JSON（中位数约 30 KB，长连锁可达约 120 KB），未做增量；
- 真机（iOS Safari / Android Chrome）和 itch.io 上线都还没实测，只在无头 Chrome 里验证过。

## 9. TODO

- [ ] 道具与洗牌按钮：导出 `m3Hammer` / `m3FreeSwap` / `m3Cross` / `m3Shuffle`（`match3Shell` 已支持），HUD 加按钮与点选流
- [ ] 每日挑战与选关地图（CH1–CH7）
- [ ] 真机测试：iPhone（dpr3）、Android、iPad；确认安全区与手势（安卓应用壳见 [`android.md`](android.md)）
- [ ] 小屏触控：iPhone SE 竖排考虑缩小棋盘边距，让格子到 44 px
- [ ] 按 dpr 选 3x 图集（约 +400 KB，只给 dpr3 / 平板）
- [ ] 音效（Web Audio）
- [ ] 资源文件名带哈希，正式部署时换成长缓存
- [ ] itch.io 打包脚本（`deploy-mac.sh pack` 的 zip 版）与上线实测
- [ ] CI：构建 wasm 并跑两组一致性测试 + e2e
- [ ] 合入 main 前评审：`web/` 目录结构、`match3-web.cabal` 与 `package.yaml` 的模块清单同步方式
