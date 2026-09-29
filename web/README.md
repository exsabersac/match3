# 网页版技术验证（GHC WebAssembly 后端）

目标：不改一行核心代码，把纯规则核心 `src/Engine/*` + `src/Match3/*` 用 GHC 的 wasm 后端编成 `.wasm`，
在浏览器里用最简单的彩色方块把一关跑通。**所有规则判定都调用 Haskell 核心**，
JS 只负责加载、画格子、收鼠标事件、按核心给的逐轮快照播放动画。

```
web/
├── build.sh              构建脚本（→ web/dist/），--serve 顺便起静态服务器
├── cabal.project         独立的 cabal 工程（只给 wasm32-wasi-cabal 用）
├── match3-web.cabal      可执行 match3-web：hs/ + 直接引用 ../src（不复制核心源码）
├── hs/
│   ├── Match3Web/Api.hs  纯接口层：经 Match3.Engine.match3Shell 的 gameStep 执行，Step / Played → JSON（无 JSFFI，原生 GHC 也能编）
│   └── WebMain.hs        JSFFI 导出 m3New / m3Swap / m3Undo / m3State / m3Levels
├── www/
│   ├── index.html        页面（HUD + 画布 + 结局提示）
│   └── main.js           加载 wasm、Canvas 2D 渲染、点选/拖拽、回放动画
└── test/
    ├── e2e.mjs           无头 Chrome 冒烟测试（真实鼠标交换 + 截图 + 耗时）
    ├── node-parity.mjs   node 里跑 wasm，按提示连走 N 步打印 JSON
    └── Parity.hs         原生 GHC 跑同样的步骤（最后撤销一步）；两边输出应逐字节相同
```

## 1. 安装工具链（一次性，约 6.4 GB，装在 ~/.ghc-wasm）

用 [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) 的安装脚本，
选 `9.14` 分支（GHC 9.14 是 LTS 系列，也是 ghc-wasm-meta 当前默认 flavour，JSFFI 已稳定）：

```sh
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
- 不影响已有的 GHC 9.4.8 / Stack，两者完全独立。
- 首次构建前 `build.sh` 会自动走 `wasm32-wasi-cabal build`；如提示没有 Hackage 索引，先跑一次
  `bash -c 'source ~/.ghc-wasm/env && wasm32-wasi-cabal update'`。

## 2. 构建

```sh
cd web
./build.sh
```

它会：
1. 检查 `match3-web.cabal` 里的核心模块清单（`Engine.*` + `Match3.*`）与 `package.yaml` 的 `library.exposed-modules` 是否一致
   （核心新增模块时要同步到 cabal 文件，否则会打印警告）；
2. `wasm32-wasi-cabal build exe:match3-web`，链接为 WASI **reactor** 模块；
3. `wasm-opt -Oz` 压体积；用 GHC 自带的 `post-link.mjs` 生成 JSFFI 胶水 `ghc_wasm_jsffi.js`；
4. 下载并缓存浏览器 WASI 垫片 `@bjorn3/browser_wasi_shim@0.4.2`（MIT/Apache-2.0，约 96 KB）；
5. 输出到 `web/dist/`，并打印 wasm 原始 / 优化后 / gzip 后的体积。

随机数：`cabal.project` 把 `random` / `splitmix` 钉在与桌面版 lts-21.25 相同的版本
（1.2.1.1 / 0.1.0.5，后者放宽了 base 上界），因此**同关卡同种子，网页版与桌面版开局和每一步结果完全一致**
（`test/Parity.hs` 与 `test/node-parity.mjs` 已验证）。

## 3. 本地试玩

```sh
cd web
./build.sh --serve          # 等价于构建后在 dist/ 里 python3 -m http.server 8080
# 浏览器打开 http://127.0.0.1:8080/?level=0&seed=42
```

- 点一格再点相邻格交换，或按住拖到相邻格松开；
- 匹配 3 个以上消除 → 下落 → 补子 → 连锁，逐轮播放；HUD 显示分数、剩余步数、目标进度；
- 换了不能消会提示「已退回」且不扣步；过关 / 通关 / 步数用完时盘面上出现结局提示；
- 下拉框可选 40 关中的任一关（非宝石障碍目前只画成带文字的灰块）；「提示」按钮高亮核心 `findHint` 的建议；
  「撤销」回到上一步（历史在核心 `Engine.History`，最多 20 步，终局后也可撤销）。

必须通过 HTTP 访问（`file://` 下 `fetch` wasm 会被浏览器拒绝）。服务器需给 `.wasm` 返回
`application/wasm`（python http.server 默认如此），否则 `instantiateStreaming` 会失败。

## 4. 测试

```sh
# 无头浏览器：真实鼠标点选/拖拽，截图到 /workspace/match3-web-shots/，并输出 report.json
NODE_PATH=~/.ghc-wasm/nodejs/lib/node_modules ~/.ghc-wasm/nodejs/bin/node web/test/e2e.mjs
#   默认用 /usr/bin/google-chrome，可用 CHROME=/path/to/chromium 覆盖

# 原生 vs wasm 一致性（仓库根目录）
~/.ghc-wasm/nodejs/bin/node web/test/node-parity.mjs 0 20260929 12 > /tmp/wasm.txt
stack exec -- runghc -isrc -iweb/hs web/test/Parity.hs 0 20260929 12 > /tmp/native.txt   # 需 LANG=C.UTF-8
cmp /tmp/wasm.txt /tmp/native.txt && echo 一致
```

## 5. 网页端与核心的接口

wasm 导出 5 个 **同步** JSFFI 函数（`foreign export javascript "... sync"`），都返回 JSON 字符串：

| 导出 | 参数 | 返回 |
| --- | --- | --- |
| `m3New(level, seed)` | 关卡序号（0 起）、种子 | `{ok, state}` |
| `m3Swap(r1, c1, r2, c2)` | 两个格子 | `{ok, accepted, outcome, trace, events, state}` |
| `m3Undo()` | – | 同 `m3Swap`（`trace` 为空脚本、`events` 为空；没有历史时 `accepted:false`） |
| `m3State()` | – | `{ok, state}` |
| `m3Levels()` | – | 关卡列表 `[{index,name,moves,goal}]` |

- `state`：`level/name/score/moves/goal/progress/target/over/loseHint/combo/shuffled/undo/hint/lastCleared/ground/board`
  （`undo` = 可撤销步数；`ground` = 地面层 `[{p,name,layers}]`，如第 39 关果冻；`goal.name` 为 `GoalNamed` 的元素名）；
- `outcome.tag`：`MoveApplied | NoMatch | InvalidSwap | LevelClear | Won | Lost`；
- 执行路径：`Api.hs` 只调通用接口 `gameStep match3Shell`（与桌面外壳 app/UI/Plugin.hs 相同），
  表现数据全部取自 `stepReport`（`Played`），规则每步只算一次；
- `trace`：`pdTrace` 的逐轮快照 `start → waves[{before,cleared,drained,holes,after,score}] → end[] → final → shuffle`，
  前端按它逐轮播放；`end[i] = {afterWaves,before,after,effect}`，`effect` 为结构化步末效果：
  `{type:"tick",cells}` / `{type:"belt",pairs}` / `{type:"spread",kind:"vine|choco|steam",pairs}` / `{type:"snail",moves:[{from,to,dir,pushed}]}`；
  `shuffle` 为本步触发自动洗牌后的盘面（否则 `null`）；
- `events`：规则层效果事件 `pdEvents`（按时间顺序）`{kind,beat,subject,pairs:[[来源],[目标]],amount}`，
  `kind` ∈ `clear/hit/blast/drain/score/combo/tick/belt/spread/move/shuffle`（同 `Match3.Engine.eventKindTag`），
  `beat` 与 `end[].afterWaves` 同一时间轴、同 beat 同时播放；比通用 `Engine.Effect` 多保留来源格（爆炸方向、移动轨迹）；
- `board`：8×8，宝石 `{"t":"G","c":1..5,"k":"N|H|V|B|R","i":冰层,"o":覆盖物}`，其他格 `{"t":"X","c":颜色,"s":"核心 show 文本"}`
  （元素框架自定义格如气泡另带 `name` / `v`）。

当前局面保存在 Haskell 侧的全局 `IORef`（单线程 RTS，一个页面一局）；异常会被兜底成 `{ok:false,error}`。

## 6. 已知限制

- 只接了交换与撤销；道具（锤子/自由交换/十字消）、洗牌按钮、每日挑战未接（`match3Shell` 已支持，只差 JSFFI 导出与 UI）；
- 关卡级元素（传送门 / 皮带路径 / 飞碟 / 地毯）还没进 `state`，只能从盘面格看出；
- 非宝石格（石头、宝箱、蜗牛等）只画灰块 + 文字；步末效果（皮带、蔓延、蜗牛）只按快照瞬切，`events` 还没被前端使用；
- 每次调用返回完整 JSON（约 15–23 KB/步），没做增量；详见仓库外的 spike 报告。
