// 无头浏览器端到端测试：起静态服务器 → 打开页面 → 用真实指针（点选 / 拖划 / 按钮）操作 → 关键时刻逐帧截图 + 报告。
// 用法（先 ./build.sh）：
//   NODE_PATH=~/.ghc-wasm/nodejs/lib/node_modules node web/test/e2e.mjs [截图目录]
//   [E2E_PORT=8765]：serve.py 监听 127.0.0.1 的端口（默认 8765）；端口被占用时直接报错退出，换一个再跑
// 需要 playwright-core（ghc-wasm-meta 自带）和本机 Chrome/Chromium（CHROME=路径 可覆盖）。
//
// 关键时刻截图用页面的调试断点：m3debug.breakWhen(info) 为真时帧循环冻结（画面停在那一帧），截图后解冻继续。
// 覆盖：
//   1. 主流程（390×844 dpr3）：无效交换退回不扣步、按提示走到结局（点选 / 拖划交替）、连锁中「高亮 / 消失 / 下落」三帧、撤销按钮；
//   2. 特殊块爆炸（直线消除）的「消失」帧；步末效果（皮带 / 蜗牛 / 蔓延 / 倒计时）中间帧；
//   3. 第 39 关果冻、第 40 关气泡：静止 + 连锁中；
//   4. 分辨率矩阵：7 种视口截图，并检查布局完整落在视口 / 安全区内、格子与按钮的 CSS 尺寸；
//   4b. 规则开关角标：第 41 关 state.rules 与 HUD 角标（竖屏 / 横屏手机 / 桌面，截图 rules-badge-*.png），第 1 关没有角标；
//   4c. 贴图护栏：每一关开局 + 走 3 步后 m3debug.fallbacks（走几何降级的格子）为空；第 42 关魔法石 0–3 格充能截图；
//       同时逐关检查 HUD 目标标签是中文显示名（state.goal.label，不含 [a-z_] 内部名）；
//   4d. 第 43 关毛球：浮动两帧（按像素测上下偏移）、步末跳格（皮带段）中间帧、HUD「目标 毛球」竖屏 / 横屏；
//   4e. 第 44 关彩虹组合：规则角标「彩虹组合变身」、彩虹 × 直线 / 炸弹的变身段（第一轮之前的蔓延段）中间帧；
//   4f. 真实绘制钩子（drawImage 按调用序记录）：第 46 / 47 关掉落口、第 47 关变色龙；逐关地面层贴图与 HUD 关名 name_<i>；
//   4g. 第 48 关魔法地格：地面层 magic 贴图与像素、4 组扩圈爆炸（真实绘制格数 = EvBlast 格数）、终章（第 47→48 LevelClear，第 48→49 LevelClear，第 49 关 Won）；
//   4h. 第 8 / 39–45 / 47 / 48 关玩到失败的结局面板文字（碎石关「用邻消或特效砸开碎石，目标 n 个」）、逐关失败提示无「箱子」/ [a-z_]；
//   4i. 音效 / BGM 开关芯片：3 种视口 × 第 1 / 45 / 48 / 49 关，真实绘制的字形在芯片内、不大于按钮、不压提示行；
//   5. 动画进行中改变视口大小：不重置对局与动画，播完后状态正确。
import { spawn } from "node:child_process";
import { createRequire } from "node:module";
import path from "node:path";
import fs from "node:fs";
import net from "node:net";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const { chromium } = require("playwright-core");
const here = path.dirname(fileURLToPath(import.meta.url));
const dist = path.resolve(here, "../dist");
const shots = path.resolve(process.argv[2] || "/workspace/match3-web-shots");
fs.rmSync(shots, { recursive: true, force: true });
fs.mkdirSync(shots, { recursive: true });
// 服务器端口：环境变量 E2E_PORT（默认 8765；make e2e / make check 传入）。同机并行跑多份 e2e 时各用各的端口
const PORT = Number(process.env.E2E_PORT || 8765);
if (!Number.isInteger(PORT) || PORT < 1 || PORT > 65535) { console.error(`E2E_PORT 无效：${process.env.E2E_PORT}（要 1–65535 的整数）`); process.exit(2); }
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
// 端口已被别的进程占用时直接失败（否则会连到别人的服务器、测到别的 dist）
const portFree = await new Promise((r) => { const t = net.createServer(); t.once("error", () => r(false)); t.listen(PORT, "127.0.0.1", () => t.close(() => r(true))); });
if (!portFree) { console.error(`端口 ${PORT} 已被占用：换一个再跑，例如 E2E_PORT=8799 make e2e`); process.exit(2); }
// 用仓库自带的 web/serve.py（与本地试玩 / 部署同一个服务器，顺带验证它的 Content-Type）
const server = spawn("python3", [path.resolve(here, "../serve.py"), "--dir", dist, "--port", String(PORT), "--bind", "127.0.0.1", "--quiet"], { stdio: "ignore" });
let serverExit = null;
server.on("exit", (code) => { serverExit = code; });
// 等服务器就绪（最多 10 秒），并确认它给的是本次的 web/dist
{
  const want = fs.readFileSync(path.join(dist, "index.html"), "utf8");
  let ok = false;
  for (let i = 0; i < 100 && !ok && serverExit === null; i++) {
    await sleep(100);
    try { const r = await fetch(`http://127.0.0.1:${PORT}/index.html`); ok = r.status === 200 && (await r.text()) === want; } catch { /* 还没起来 */ }
  }
  if (!ok) { console.error(`serve.py 没有在端口 ${PORT} 上提供 ${dist}（退出码 ${serverExit}）`); server.kill(); process.exit(2); }
}
const serverTypes = {};
for (const [f, want] of [["index.html", "text/html"], ["match3-web.wasm", "application/wasm"], ["atlas.webp", "image/webp"], ["atlas.json", "application/json"], ["main.js", "text/javascript"]]) {
  const r = await fetch(`http://127.0.0.1:${PORT}/${f}`);
  serverTypes[f] = { status: r.status, type: r.headers.get("content-type"), cache: r.headers.get("cache-control"), want };
  await r.arrayBuffer();
}
const browser = await chromium.launch({ executablePath: process.env.CHROME || "/usr/bin/google-chrome", headless: true });
const report = { shots: [], checks: [] };
const logs = [];
const check = (name, ok, detail) => { report.checks.push({ name, ok: !!ok, ...(detail !== undefined ? { detail } : {}) }); if (!ok) console.error("FAIL", name, detail ?? ""); };
report.serverTypes = serverTypes;
check("serve.py 的 Content-Type 正确", Object.values(serverTypes).every((t) => t.status === 200 && (t.type || "").startsWith(t.want)), serverTypes);

// 真实绘制钩子（每个页面注入）：包住 CanvasRenderingContext2D.prototype.drawImage，按调用序（seq）记录画到 #board 上的每次绘制——
// 源矩形（在图集里反查贴图名）+ 经当前变换后的目标矩形（左上角 x0/y0、中心 cx/cy、宽高、是否旋转，后备缓冲像素）。
// 只在 window.__captureFrame() 之后的下一个有绘制的 requestAnimationFrame 回调里记录（= 一整帧）。检查用它，不用页面自报的记录：
// 页面代码画偏了、画错贴图或画反了顺序，这里都看得到。
const DRAW_HOOK = `(() => {
  const P = CanvasRenderingContext2D.prototype, orig = P.drawImage;
  let cur = null, done = null, seq = 0;
  P.drawImage = function (...a) {
    if (cur && this.canvas && this.canvas.id === "board") {
      const img = a[0], m = this.getTransform();
      let sx = 0, sy = 0, sw = img.width, sh = img.height, dx, dy, dw = img.width, dh = img.height;
      if (a.length >= 9) [, sx, sy, sw, sh, dx, dy, dw, dh] = a;
      else if (a.length >= 5) [, dx, dy, dw, dh] = a;
      else [, dx, dy] = a;
      const pt = (x, y) => [m.a * x + m.c * y + m.e, m.b * x + m.d * y + m.f];
      const [x0, y0] = pt(dx, dy), [cx, cy] = pt(dx + dw / 2, dy + dh / 2);
      cur.push({ seq: seq++, atlas: img instanceof HTMLImageElement && /atlas/.test(img.src), src: [sx, sy, sw, sh], x0, y0, cx, cy,
        w: Math.hypot(m.a, m.b) * dw, h: Math.hypot(m.c, m.d) * dh, rot: Math.abs(m.b) > 1e-9 || Math.abs(m.c) > 1e-9 });
    }
    return orig.apply(this, a);
  };
  // 文字：同一帧里画到 #board 的每次 fillText（文字、字体、经当前变换后的实际字形外框，后备缓冲像素），帧末放进 window.__frameTexts
  const origText = P.fillText;
  let curT = null;
  P.fillText = function (s, x, y, ...rest) {
    if (cur && this.canvas && this.canvas.id === "board") {
      const m = this.getTransform(), t = this.measureText(String(s));
      const l = x - (t.actualBoundingBoxLeft || 0), r = x + (t.actualBoundingBoxRight || 0);
      const tp = y - (t.actualBoundingBoxAscent || 0), bt = y + (t.actualBoundingBoxDescent || 0);
      const X = (u) => m.a * u + m.e, Y = (v) => m.d * v + m.f;
      curT.push({ seq: seq++, text: String(s), font: this.font, x0: X(l), y0: Y(tp), w: (r - l) * m.a, h: (bt - tp) * m.d });
    }
    return origText.call(this, s, x, y, ...rest);
  };
  const raf = window.requestAnimationFrame.bind(window);
  window.requestAnimationFrame = (cb) => raf((t) => {
    try { cb(t); } finally { if (cur && cur.length) { const d = done, log = cur; window.__frameTexts = curT; cur = null; curT = null; done = null; d(log); } }
  });
  window.__captureFrame = () => new Promise((res) => { cur = []; curT = []; done = res; });
})();`;
const atlasSprites = JSON.parse(fs.readFileSync(path.join(dist, "atlas.json"), "utf8")).sprites;
const spriteAt = new Map(Object.entries(atlasSprites).map(([n, r]) => [r.join(","), n]));
// 抓一整帧的真实绘制，按源矩形反查贴图名
async function captureDraws(P) {
  const log = await P.page.evaluate(() => window.__captureFrame());
  return log.map((d) => ({ ...d, name: d.atlas ? spriteAt.get(d.src.map((v) => Math.round(v)).join(",")) ?? null : null }));
}
const near = (a, b, tol = 0.5) => Math.abs(a - b) <= tol;
// 音效 / BGM 开关（HUD 的 sfx / bgm 芯片）：用真实绘制的 fillText 外框（__frameTexts，换算成设计单位）核对——
// 芯片里只画了一个字（效 / 乐 / 静）且字形整个落在芯片矩形里；芯片（矩形 ∪ 字形）宽高都不超过 HUD 最小的按钮；
// 与提示行（真实画出的 600 16px 文字）不相交；两枚芯片互不重叠、都在 HUD 区域里
async function soundChipCheck(P) {
  await captureDraws(P);
  const [texts, hud, L, dpr] = await P.page.evaluate(() => [window.__frameTexts || [], window.m3debug.hud, window.m3debug.layout, window.m3debug.dpr]);
  const k = dpr * L.u, U = (t) => ({ text: t.text, font: t.font, x: (t.x0 / dpr - L.ox) / L.u, y: (t.y0 / dpr - L.oy) / L.u, w: t.w / k, h: t.h / k });
  const ts = texts.map(U), H = L.hud;
  const inter = (a, b) => Math.min(a.x + a.w, b.x + b.w) - Math.max(a.x, b.x) > 0.01 && Math.min(a.y + a.h, b.y + b.h) - Math.max(a.y, b.y) > 0.01;
  const inside = (a, b, tol = 0.5) => a.x >= b.x - tol && a.y >= b.y - tol && a.x + a.w <= b.x + b.w + tol && a.y + a.h <= b.y + b.h + tol;
  const union = (a, b) => { const x = Math.min(a.x, b.x), y = Math.min(a.y, b.y); return { x, y, w: Math.max(a.x + a.w, b.x + b.w) - x, h: Math.max(a.y + a.h, b.y + b.h) - y }; };
  const btnW = Math.min(...L.buttons.map((b) => b.w)), btnH = Math.min(...L.buttons.map((b) => b.h));
  const msgLines = ts.filter((t) => /^600 16px/.test(t.font) && t.text.trim() && inside(t, H, 1));
  const chips = ["sfx", "bgm"].map((id) => {
    const r = hud && hud[id];
    if (!r) return { id, ok: false, why: "hud 没有记录芯片矩形" };
    const cx = r.x + r.w / 2, cy = r.y + r.h / 2;
    const glyphs = ts.filter((t) => /^[效乐静]$/.test(t.text) && Math.abs(t.x + t.w / 2 - cx) <= r.w / 2 && Math.abs(t.y + t.h / 2 - cy) <= r.h / 2 + 4);
    const g = glyphs[0], drawn = g ? union(r, g) : r;
    const hits = msgLines.filter((m) => inter(drawn, m)).map((m) => m.text);
    const ok = glyphs.length === 1 && inside(g, r) && drawn.w <= btnW + 0.01 && drawn.h <= btnH + 0.01 && hits.length === 0 && inside(drawn, H, 1);
    return { id, ok, rect: r, glyph: g ?? null, nGlyphs: glyphs.length, drawn, hitsMsg: hits };
  });
  const apart = chips.every((c) => c.drawn) && !inter(chips[0].drawn, chips[1].drawn);
  return { ok: chips.every((c) => c.ok) && apart && msgLines.length > 0, mode: L.mode, btn: { w: btnW, h: btnH }, chips, apart, msgLines: msgLines.map((m) => ({ text: m.text, x: +m.x.toFixed(1), y: +m.y.toFixed(1), w: +m.w.toFixed(1), h: +m.h.toFixed(1) })) };
}
// 棋盘网格：从真实画出的底格 tile_a / tile_b 求 (0,0) 格左上角（设计坐标 (16,16)）与每设计单位的后备缓冲像素数 k；
// 所有底格都要落在同一张 56k 网格上
function boardGrid(draws, rows, cols) {
  const tiles = draws.filter((d) => (d.name === "tile_a" || d.name === "tile_b") && !d.rot);
  const uniq = new Map(tiles.map((t) => [`${t.x0.toFixed(2)},${t.y0.toFixed(2)}`, t]));
  if (uniq.size !== rows * cols) return null;
  const ts = [...uniq.values()], k = ts[0].w / 56, x = Math.min(...ts.map((t) => t.x0)), y = Math.min(...ts.map((t) => t.y0));
  const ok = ts.every((t) => { const c = (t.x0 - x) / (56 * k), r = (t.y0 - y) / (56 * k); return near(c, Math.round(c), 0.01) && near(r, Math.round(r), 0.01) && near(t.w, 56 * k, 0.01) && near(t.h, 56 * k, 0.01); });
  return ok ? { x, y, k } : null;
}
// 掉落口：桌面 UI.BoardArt.drawDropsArt 画在 (cellOrigin 的 x, y − 6)、56 × 56；设计坐标 (16 + 56c, 16 + 56r − 6) → 屏幕 = 网格原点 + (X − 16, Y − 16) × k
function dropMarkCheck(draws, grid, drops) {
  const got = draws.filter((d) => d.name === "cookie_drop").map((d) => ({ seq: d.seq, x: d.x0, y: d.y0, w: d.w, h: d.h, rot: d.rot }));
  const want = drops.map(([r, c]) => ({ p: [r, c], x: grid.x + (16 + 56 * c - 16) * grid.k, y: grid.y + (16 + 56 * r - 6 - 16) * grid.k, w: 56 * grid.k }));
  const ok = got.length === want.length && want.every((w) => got.some((g) => !g.rot && near(g.x, w.x) && near(g.y, w.y) && near(g.w, w.w) && near(g.h, w.w)));
  return { ok, got, want };
}
// 变色龙：每只变色龙格（按给定盘面）在该格画了且只画了一次 gem_c<v+1>（v = 核心格子的原始值，不用 Api 的 c），调用序在环 chameleon 之前
function chamCheck(draws, grid, board) {
  const cells = [];
  board.forEach((row, r) => row.forEach((cell, c) => {
    if (!(cell.t === "custom" && cell.name === "chameleon")) return;
    const s = 56 * grid.k, cx = grid.x + 56 * c * grid.k + s / 2, cy = grid.y + 56 * r * grid.k + s / 2;
    const at = draws.filter((d) => near(d.cx, cx) && near(d.cy, cy) && near(d.w, s) && near(d.h, s));
    const gems = at.filter((d) => /^gem_c\d$/.test(d.name || "")), ring = at.find((d) => d.name === "chameleon"), want = `gem_c${cell.v + 1}`;
    const ok = gems.length === 1 && gems[0].name === want && !gems[0].rot && !!ring && gems[0].seq < ring.seq;
    cells.push({ p: [r, c], v: cell.v, want, gems: gems.map((g) => [g.name, g.seq]), ring: ring ? ring.seq : null, ok });
  }));
  return { ok: cells.length > 0 && cells.every((x) => x.ok), cells };
}
// 地面层：桌面 UI.Ground.groundTable 的贴图名（测试侧自己的一份，不 import 页面的 cells.js）；表外的名字没有贴图（页面画淡灰框）
const GROUND_SPRITE = { jelly: (n) => (n >= 2 ? "jelly_2" : "jelly"), magic: () => "magic" };
// 每个地面层格：真实画了表内贴图、56 × 56 画在该格左上角、不旋转，且调用序在该格底格 tile 之后、在该格任何棋子之前（棋盘格之上、棋子之下）
const UNDER = new Set(["tile_a", "tile_b", "carpet_open", "carpet_covered", "belt", "portal"]);
function groundCheck(draws, grid, ground) {
  const s = 56 * grid.k, cells = [];
  for (const g of ground) {
    const [r, c] = g.p, x = grid.x + 56 * c * grid.k, y = grid.y + 56 * r * grid.k, f = GROUND_SPRITE[g.name], want = f ? f(g.layers) : null;
    const at = draws.filter((d) => !d.rot && near(d.x0, x) && near(d.y0, y) && near(d.w, s) && near(d.h, s));
    const tile = at.find((d) => d.name === "tile_a" || d.name === "tile_b"), mine = at.filter((d) => d.name === want);
    // 该格的棋子 = 该格上除底格 / 地毯 / 传送带 / 传送门 / 本地面层贴图以外的整格贴图（这些底层都在棋子之前画）
    const piece = at.find((d) => d.name && d.name !== want && !UNDER.has(d.name));
    const ok = !!want && mine.length === 1 && !!tile && mine[0].seq > tile.seq && (!piece || mine[0].seq < piece.seq);
    cells.push({ p: [r, c], name: g.name, layers: g.layers, want, got: mine.map((d) => d.seq), tile: tile?.seq ?? null, piece: piece ? [piece.name, piece.seq] : null, ok });
  }
  return { ok: cells.every((x) => x.ok), cells };
}
// HUD 关名：真实画了且只画了一张 name_*（预渲染文字图），就是 name_<关卡下标>（同桌面 HudArt 的 "name_" ++ show li），
// 目标矩形 = 页面报告的关名槽（m3debug.hud.name，设计单位）经 HUD 变换（dpr × u、偏移 ox / oy）
function nameCheck(draws, li, hud, L, dpr) {
  const names = draws.filter((d) => /^name_\d+$/.test(d.name || "")), want = `name_${li}`, n = hud?.name;
  const k = dpr * L.u, rect = n ? { x: dpr * L.ox + k * n.x, y: dpr * L.oy + k * n.y, w: k * n.w, h: k * n.h } : null;
  const d = names[0];
  const ok = names.length === 1 && d.name === want && !d.rot && !!rect && n.sprite === want && near(d.x0, rect.x, 1) && near(d.y0, rect.y, 1) && near(d.w, rect.w, 1) && near(d.h, rect.h, 1);
  return { ok, want, drawn: names.map((x) => x.name), rect, got: d ? { x: d.x0, y: d.y0, w: d.w, h: d.h } : null, hudName: n };
}
// 格子边缘一圈（离边 4%–12% 格宽）的平均「紫度」(R + B) / 2 − G（后备缓冲像素）：魔法地格的贴图是紫色地砖，淡灰框几乎为 0
async function purpleBand(P, grid, cells) {
  return P.page.evaluate(({ grid, cells }) => {
    const g = document.getElementById("board").getContext("2d"), S = Math.round(56 * grid.k);
    return cells.map(([r, c]) => {
      const d = g.getImageData(Math.round(grid.x + 56 * c * grid.k), Math.round(grid.y + 56 * r * grid.k), S, S).data;
      let sum = 0, n = 0;
      for (let j = 0; j < S; j++) for (let i = 0; i < S; i++) {
        const e = Math.min(i, j, S - 1 - i, S - 1 - j);
        if (e < S * 0.04 || e > S * 0.12) continue;
        const o = (j * S + i) * 4; sum += (d[o] + d[o + 2]) / 2 - d[o + 1]; n++;
      }
      return +(sum / n).toFixed(1);
    });
  }, { grid, cells });
}

async function openPage(vp, level, seed) {
  const ctx = await browser.newContext({ viewport: { width: vp.w, height: vp.h }, deviceScaleFactor: vp.dpr, hasTouch: !!vp.touch, isMobile: !!vp.mobile });
  await ctx.addInitScript(DRAW_HOOK);
  const page = await ctx.newPage();
  page.on("console", (m) => logs.push(`${m.type()}: ${m.text()}`));
  page.on("pageerror", (e) => logs.push(`pageerror: ${e.message}`));
  const t0 = Date.now();
  await page.goto(`http://127.0.0.1:${PORT}/?level=${level}&seed=${seed}`);
  await page.waitForFunction(() => window.m3debug && window.m3debug.state, null, { timeout: 30000 });
  const loadMs = Date.now() - t0;
  const P = makeOps(page);
  return { ctx, page, loadMs, ...P };
}

function makeOps(page) {
  const st = () => page.evaluate(() => window.m3debug.state);
  const center = (p) => page.evaluate(([r, c]) => window.m3debug.cellCenter(r, c), p);
  const shot = async (name) => { const f = `${shots}/${name}.png`; await page.screenshot({ path: f }); report.shots.push(f); return f; };
  const idle = () => page.waitForFunction(() => !window.m3debug.busy, null, { timeout: 30000 });
  // 设断点：cond 是 info => bool 的函数源码
  const breakAt = (cond) => page.evaluate((src) => { window.m3debug.breakWhen = new Function("i", `return (${src})(i)`); }, cond.toString());
  const clearBreak = () => page.evaluate(() => { window.m3debug.breakWhen = null; });
  const frozenOrIdle = () => page.waitForFunction(() => window.m3debug.frozen || !window.m3debug.busy, null, { timeout: 30000 });
  const isFrozen = () => page.evaluate(() => window.m3debug.frozen);
  const resume = () => page.evaluate(() => { window.m3debug.frozen = false; });
  const info = () => page.evaluate(() => window.m3debug.anim);
  async function swap(a, b, drag) {
    const pa = await center(a), pb = await center(b);
    if (drag) {
      await page.mouse.move(pa[0], pa[1]); await page.mouse.down();
      await page.mouse.move(pb[0], pb[1], { steps: 6 }); await page.mouse.up();
    } else { await page.mouse.click(pa[0], pa[1]); await page.mouse.click(pb[0], pb[1]); }
  }
  // 点按钮：常驻按钮（撤销 / 提示 / 菜单）直接点；其余先点「菜单」打开菜单面板再点对应项
  async function button(id) {
    const has = await page.evaluate((i) => window.m3debug.layout.buttons.some((b) => b.id === i), id);
    if (!has && !(await page.evaluate(() => window.m3debug.ui.menuOpen))) {
      const [mx, my] = await page.evaluate(() => window.m3debug.buttonCenter("menu")); await page.mouse.click(mx, my); await sleep(40);
    }
    const [x, y] = await page.evaluate((i) => (window.m3debug.layout.buttons.some((b) => b.id === i) ? window.m3debug.buttonCenter(i) : window.m3debug.menuItemCenter(i)), id);
    await page.mouse.click(x, y);
  }
  return { st, center, shot, idle, breakAt, clearBreak, frozenOrIdle, isFrozen, resume, info, swap, button };
}

try {
  // -------------------------------------------------------------------------
  // 1. 主流程
  {
    const P = await openPage({ w: 390, h: 844, dpr: 3 }, 0, 20260929);
    report.coldLoadToReadyMs = P.loadMs;
    await P.shot("01-开局-390x844");
    // 无效交换：依次试几对相邻格，直到核心返回 NoMatch
    const probes = [[[0, 1], [1, 1]], [[0, 0], [1, 0]], [[7, 7], [7, 6]], [[3, 3], [3, 4]], [[5, 2], [6, 2]]];
    for (const [a, b] of probes) {
      const before = await P.st();
      await P.swap(a, b, false); await sleep(30); await P.idle();
      const last = await P.page.evaluate(() => window.m3debug.perf.steps.at(-1));
      const after = await P.st();
      if (last.outcome === "NoMatch") {
        check("无效交换不扣步且盘面不变", after.moves === before.moves && JSON.stringify(after.board) === JSON.stringify(before.board));
        await P.shot("01b-无效交换已退回");
        break;
      }
    }
    // 按提示走到结局；连锁时抓三帧；第 3 步后测撤销
    // 这一局没出现 ≥2 连击就换下一个种子再开（最多 8 局）
    let n = 0, chain = false, undoDone = false, games = 1, seed = 20260929;
    report.games = [];
    for (let guard = 0; guard < 400; guard++) {
      const s = await P.st();
      if (s.over || !s.hint) {
        report.games.push({ seed, over: s.over, score: s.score, movesLeft: s.moves });
        if (games === 1) await P.shot("06-结局");
        if (chain || games >= 8) break;
        seed++; games++;
        await P.page.goto(`http://127.0.0.1:${PORT}/?level=0&seed=${seed}`);
        await P.page.waitForFunction(() => window.m3debug && window.m3debug.state, null, { timeout: 30000 });
        continue;
      }
      if (!chain) await P.breakAt((i) => i.p === "flash" && i.k >= 2 && i.fr >= 6);
      await P.swap(s.hint[0], s.hint[1], n % 2 === 1);
      n++;
      await P.frozenOrIdle();
      if (!chain && (await P.isFrozen())) {
        chain = true;
        await P.shot("02-连锁中-第2轮高亮");
        await P.breakAt((i) => i.p === "pop" && i.fr >= 2); await P.resume(); await P.frozenOrIdle();
        if (await P.isFrozen()) await P.shot("03-连锁中-消失与粒子");
        await P.breakAt((i) => i.p === "fall" && i.fr >= Math.floor(i.n / 2)); await P.resume(); await P.frozenOrIdle();
        if (await P.isFrozen()) await P.shot("04-连锁中-下落补子");
        await P.clearBreak(); await P.resume();
      }
      await P.clearBreak(); await P.idle();
      if (n === 3 && !undoDone) {
        undoDone = true;
        const before = await P.st();
        await P.button("undo"); await sleep(60);
        const after = await P.st();
        check("撤销按钮：步数 +1、可撤销数 -1", after.moves === before.moves + 1 && after.undo === before.undo - 1, { before: [before.moves, before.undo], after: [after.moves, after.undo] });
        await P.shot("05-撤销之后");
      }
    }
    check("主流程出现连锁并截到三帧", chain);
    const perf = await P.page.evaluate(() => { const p = window.m3debug.perf; return { instantiateMs: p.instantiateMs, initMs: p.initMs, firstNewMs: p.firstNewMs,
      tickMsAvg: p.tickMs / Math.max(1, p.ticks), drawMsAvg: p.drawMs / Math.max(1, p.draws), ticks: p.ticks, swaps: p.steps.length,
      swapWasmMsMedian: p.steps.map((s) => s.wasmMs).sort((a, b) => a - b)[Math.floor(p.steps.length / 2)] }; });
    report.perf390x844dpr3 = perf;
    await P.ctx.close();
  }

  // -------------------------------------------------------------------------
  // 2. 特殊块爆炸 / 步末效果 / 果冻 / 气泡：按提示走，命中断点即截图
  async function hunt(vp, level, seed, cond, name, maxSteps = 30) {
    const P = await openPage(vp, level, seed);
    if (name.static) await P.shot(name.static);
    let hit = false;
    for (let k = 0; k < maxSteps && !hit; k++) {
      const s = await P.st();
      if (s.over || !s.hint) break;
      await P.breakAt(cond);
      await P.swap(s.hint[0], s.hint[1], k % 2 === 0);
      await P.frozenOrIdle();
      if (await P.isFrozen()) { hit = true; await P.shot(name.hit); }
      await P.clearBreak(); await P.resume(); await P.idle();
    }
    check(`截到：${name.hit}`, hit);
    await P.ctx.close();
    return hit;
  }
  const desk = { w: 1280, h: 800, dpr: 2 };
  await hunt(desk, 12, 42, (i) => i.blast && i.p === "pop" && i.fr >= 1, { hit: "07-特殊块爆炸-直线消除" });
  await hunt(desk, 12, 42, (i) => i.blast && i.p === "flash" && i.fr >= 6, { hit: "07b-特殊块爆炸-高亮" });
  await hunt(desk, 27, 1, (i) => i.p === "end" && i.stage === "snail" && i.fr >= 8, { static: "08a-第28关-静止", hit: "08-步末-蜗牛爬行" });
  await hunt(desk, 13, 1, (i) => i.p === "end" && i.stage === "belt" && i.fr >= 7, { hit: "09-步末-传送带移位" });
  await hunt(desk, 15, 1, (i) => i.p === "end" && i.stage === "spread" && i.fr >= 9, { hit: "10-步末-蔓延生长" });
  await hunt({ w: 390, h: 844, dpr: 3 }, 38, 2026, (i) => i.p === "flash" && i.fr >= 6, { static: "11a-第39关果冻-静止", hit: "11-第39关果冻-消除高亮" });
  await hunt({ w: 390, h: 844, dpr: 3 }, 39, 31337, (i) => i.p === "pop" && i.fr >= 2, { static: "12a-第40关气泡-静止", hit: "12-第40关气泡-消失" });

  // -------------------------------------------------------------------------
  // 3. 分辨率矩阵
  const matrix = [
    { name: "iPhoneSE-375x667-dpr2", w: 375, h: 667, dpr: 2, touch: true, mobile: true },
    { name: "390x844-dpr3", w: 390, h: 844, dpr: 3, touch: true, mobile: true },
    { name: "横屏手机-844x390-dpr3", w: 844, h: 390, dpr: 3, touch: true, mobile: true },
    { name: "平板-768x1024-dpr2", w: 768, h: 1024, dpr: 2, touch: true, mobile: true },
    { name: "1280x800", w: 1280, h: 800, dpr: 1 },
    { name: "1920x1080", w: 1920, h: 1080, dpr: 1 },
    { name: "2560x1440", w: 2560, h: 1440, dpr: 1 },
  ];
  report.layouts = [];
  for (const vp of matrix) {
    const P = await openPage(vp, 38, 7);
    await sleep(200);
    const L = await P.page.evaluate(() => { const L = window.m3debug.layout; return { mode: L.mode, u: L.u, ox: L.ox, oy: L.oy, w: L.w, h: L.h, W: L.W, H: L.H, cellCss: L.cellCss,
      minButtonCss: Math.min(...L.buttons.map((b) => Math.min(b.w, b.h) * L.u)), dpr: window.m3debug.dpr, backing: [document.getElementById("board").width, document.getElementById("board").height] }; });
    const inside = L.ox >= -0.5 && L.oy >= -0.5 && L.ox + L.w * L.u <= L.W + 0.5 && L.oy + L.h * L.u <= L.H + 0.5;
    check(`布局完整落在视口内：${vp.name}`, inside, L);
    check(`后备缓冲 = CSS × dpr：${vp.name}`, L.backing[0] === Math.round(L.W * L.dpr) && L.backing[1] === Math.round(L.H * L.dpr), L.backing);
    // 触屏：用拖划走一步（验证指针映射）
    if (vp.touch) {
      const s = await P.st();
      await P.swap(s.hint[0], s.hint[1], true); await sleep(30); await P.idle();
      const s2 = await P.st();
      check(`拖划交换生效：${vp.name}`, s2.moves === s.moves - 1);
    }
    await P.shot(`20-分辨率-${vp.name}`);
    report.layouts.push({ name: vp.name, mode: L.mode, cellCss: +L.cellCss.toFixed(1), minButtonCss: +L.minButtonCss.toFixed(1), cellPxPhysical: +(L.cellCss * L.dpr).toFixed(1) });
    await P.ctx.close();
  }

  // -------------------------------------------------------------------------
  // 3a. 第 45 关「雪怪」（下标 44，新玩法 5）：Boss 占 (2,3)–(3,4) 的 2×2，四格按象限画 snow_boss_<q>（过半受伤换 snow_boss_hurt_<q>），
  //     HUD 目标条换成血条（state.boss = 视图模型 gvBoss）。竖屏 390×844 / 横屏 1280×800 截图 snow-boss-l45-*.png；
  //     按提示走，截「扣血那一轮的高亮」snow-boss-hit-flash.png 与「召唤雪块的步末」snow-boss-summon-tick.png；全程不走几何降级
  {
    const atlas = JSON.parse(fs.readFileSync(path.join(dist, "atlas.json"), "utf8")).sprites;
    const bossSprites = ["snow_boss", ...[0, 1, 2, 3].flatMap((q) => [`snow_boss_${q}`, `snow_boss_hurt_${q}`])];
    check("图集含雪怪贴图 snow_boss / snow_boss_0..3 / snow_boss_hurt_0..3", bossSprites.every((n) => atlas[n]), bossSprites.filter((n) => !atlas[n]));
    const bossCells = (b) => { const o = []; b.forEach((row, r) => row.forEach((c, col) => { if (c.t === "custom" && c.name === "snow_boss") o.push({ p: [r, col], q: c.q, hurt: c.hurt, turn: c.turn }); })); return o; };
    report.snowBoss = [];
    for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3 }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
      const P = await openPage(vp, 44, 1);
      await sleep(150);
      const s = await P.st(), hud = await P.page.evaluate(() => window.m3debug.hud);
      const cells = bossCells(s.board);
      const layoutOk = cells.length === 4 && JSON.stringify(cells.map((c) => [c.p, c.q])) === JSON.stringify([[[2, 3], 0], [[2, 4], 1], [[3, 3], 2], [[3, 4], 3]]);
      check(`第 45 关：Boss 四格在 (2,3)–(3,4)、象限 0–3：${vp.name}`, s.level === 44 && layoutOk, cells);
      check(`第 45 关 state.boss = 40/40、HUD 画出血条：${vp.name}`, s.boss && s.boss.hp === 40 && s.boss.max === 40 && hud.boss && hud.boss.hp === 40 && hud.boss.max === 40, { boss: s.boss, hud: hud.boss });
      const sprites = await P.page.evaluate(async (b) => { const m = await import("/cells.js"); return b.flat().filter((c) => c.t === "custom").map((c) => m.primarySprite(c)); }, s.board);
      check(`第 45 关 Boss 四格有贴图：${vp.name}`, JSON.stringify(sprites) === JSON.stringify(["snow_boss_0", "snow_boss_1", "snow_boss_2", "snow_boss_3"]), sprites);
      await P.shot(`snow-boss-l45-${vp.name}`);
      report.snowBoss.push({ name: vp.name, boss: s.boss, hudBoss: hud.boss });
      await P.ctx.close();
    }
    // 多格护栏的反证 + 前后对比：Boss 区域裁图 snow-boss-crop-after.png（专门画法）；在页面里临时强制雪怪走旧的通用画法
    // （整只缩小贴图 + 层数角标 9，即接入前 main 上的画面）截 snow-boss-crop-before-generic.png，此时 fallbacks 必须报出
    // 「snow_boss#多格通用画法」（护栏确实能拦住）；撤掉强制后不再增加
    {
      const P = await openPage({ w: 390, h: 844, dpr: 3 }, 44, 1);
      await sleep(200);
      const [x0, y0] = await P.center([2, 3]), [x1, y1] = await P.center([3, 4]), m = await P.page.evaluate(() => 40 * window.m3debug.layout.u);
      const clip = { x: x0 - m, y: y0 - m, width: x1 - x0 + 2 * m, height: y1 - y0 + 2 * m };
      const crop = async (name) => { const f = `${shots}/${name}.png`; await P.page.screenshot({ path: f, clip }); report.shots.push(f); };
      await crop("snow-boss-crop-after");
      const fb0 = await P.page.evaluate(() => window.m3debug.fallbacks);
      await P.page.evaluate(async () => { const c = await import("/cells.js"); c.forceGeneric.add("snow_boss"); });
      await sleep(200);
      await crop("snow-boss-crop-before-generic");
      const fb1 = await P.page.evaluate(() => window.m3debug.fallbacks);
      await P.page.evaluate(async () => { const c = await import("/cells.js"); c.forceGeneric.delete("snow_boss"); });
      await sleep(100);
      const fb2 = await P.page.evaluate(() => window.m3debug.fallbacks);
      await sleep(200);
      const fb3 = await P.page.evaluate(() => window.m3debug.fallbacks);
      const k = "snow_boss#多格通用画法";
      check("多格护栏：专门画法下 fallbacks 为空", Object.keys(fb0).length === 0, fb0);
      check("多格护栏反证：强制雪怪走通用画法时 fallbacks 报出 snow_boss#多格通用画法", (fb1[k] || 0) > 0, fb1);
      check("多格护栏：撤掉强制后不再增加", fb3[k] === fb2[k], { fb2, fb3 });
      report.multiCellGuard = { before: fb0, forced: fb1, after: fb3 };
      await P.ctx.close();
    }
    // 其它关卡没有血条
    {
      const P = await openPage({ w: 390, h: 844, dpr: 1 }, 41, 1);
      await sleep(100);
      const s = await P.st(), hud = await P.page.evaluate(() => window.m3debug.hud);
      check("第 42 关没有 Boss 血条（state.boss = null）", s.boss === null && hud.boss === null, { boss: s.boss, hud: hud.boss });
      await P.ctx.close();
    }
    // 扣血 / 召唤动画：竖屏按提示走，命中断点即截图（扣血轮 = 本轮前后 Boss 左上格的 v 不同；召唤 = 步末 tick 段里有格变成石头）
    const P = await openPage({ w: 390, h: 844, dpr: 3 }, 44, 1);
    const hitCond = (i) => {
      if (i.p !== "flash" || i.fr < 6) return false;
      const w = window.m3debug.pending?.trace.waves[i.w];
      const v = (b) => b.flat().find((c) => c.t === "custom" && c.name === "snow_boss" && c.q === 0)?.v ?? -1;
      return !!w && v(w.before) !== v(w.after);
    };
    const tickCond = (i) => {
      if (i.p !== "end" || i.stage !== "tick" || i.fr < Math.floor(i.n / 2) - 2) return false;
      const ends = window.m3debug.pending?.trace.end || [];
      return ends.some((e) => e.effect.type === "tick" && e.effect.cells.some(([r, c]) => e.after[r][c].t === "stone" && e.before[r][c].t === "G"));
    };
    let gotHit = false, gotTick = false, hpSeen = [];
    for (let k = 0; k < 24 && !(gotHit && gotTick); k++) {
      const s = await P.st();
      if (s.over || !s.hint) break;
      hpSeen.push(s.boss.hp);
      await P.breakAt(gotHit ? tickCond : hitCond);
      await P.swap(s.hint[0], s.hint[1], k % 2 === 0);
      await P.frozenOrIdle();
      if (await P.isFrozen()) {
        if (!gotHit) {
          gotHit = true; await P.shot("snow-boss-hit-flash");
          // 同一步里也可能召唤：换成召唤断点继续
          await P.breakAt(tickCond); await P.resume(); await P.frozenOrIdle();
          if (await P.isFrozen()) { gotTick = true; await P.shot("snow-boss-summon-tick"); }
        } else { gotTick = true; await P.shot("snow-boss-summon-tick"); }
      }
      await P.clearBreak(); await P.resume(); await P.idle();
    }
    const s2 = await P.st();
    hpSeen.push(s2.boss.hp);
    report.snowBossPlay = { hpSeen, stones: s2.board.flat().filter((c) => c.t === "stone").length };
    check("第 45 关截到扣血那一轮的高亮", gotHit, hpSeen);
    check("第 45 关截到召唤雪块的步末", gotTick);
    check("第 45 关走完后 Boss 血量下降、血条与 state 一致", s2.boss.hp < 40 && (await P.page.evaluate(() => window.m3debug.hud.boss.hp)) === s2.boss.hp, hpSeen);
    check("第 45 关画面没有走几何降级", Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0, await P.page.evaluate(() => window.m3debug.fallbacks));
    await P.ctx.close();
    // 受伤表情：种子 32 按提示走约 22 步后血量 ≤ 满血一半，四格 hurt = true、画 snow_boss_hurt_<q>，血条变深红闪烁。截图 snow-boss-hurt.png
    {
      const Q = await openPage({ w: 390, h: 844, dpr: 3 }, 44, 32);
      let hurt = null;
      for (let k = 0; k < 24; k++) {
        const s = await Q.st();
        if (s.boss.hp * 2 <= s.boss.max) { hurt = s; break; }
        if (s.over || !s.hint) break;
        await Q.swap(s.hint[0], s.hint[1], false); await sleep(30);
        await Q.page.keyboard.press(" "); await Q.idle();
      }
      if (hurt) {
        await sleep(1600);   // 等浮字散掉
        const cells = bossCells(hurt.board);
        const sprites = await Q.page.evaluate(() => window.m3debug.hud.boss);
        check("第 45 关血量过半：四格 hurt、HUD 血条进入过半状态", cells.length === 4 && cells.every((c) => c.hurt) && sprites && sprites.half, { boss: hurt.boss, cells, hud: sprites });
        await Q.shot("snow-boss-hurt");
      } else check("第 45 关种子 32 按提示走到血量过半", false);
      check("受伤画面没有走几何降级", Object.keys(await Q.page.evaluate(() => window.m3debug.fallbacks)).length === 0);
      await Q.ctx.close();
    }
  }

  // -------------------------------------------------------------------------
  // 3b. 规则开关角标：第 41 关（下标 40，打开 bomb_shapes）的 state.rules 与 HUD 角标；竖屏 / 横屏手机 / 桌面三种布局
  //     角标都画出来、文字完整、落在关卡面板里，不压「第 N 关」标签、不压关名、彼此不重叠；第 1 关没有角标
  {
    const wantRules = [{ name: "bomb_shapes", text: "L/T 形出炸弹", icons: ["bomb_glow", "bomb_mark"] }];
    const inside = (a, b) => a.x >= b.x - 0.5 && a.y >= b.y - 0.5 && a.x + a.w <= b.x + b.w + 0.5 && a.y + a.h <= b.y + b.h + 0.5;
    const overlap = (a, b) => a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h;
    report.ruleBadges = [];
    for (const vp of [
      { name: "portrait-390x844", w: 390, h: 844, dpr: 3, touch: true, mobile: true },
      { name: "landscape-phone-844x390", w: 844, h: 390, dpr: 3, touch: true, mobile: true },
      { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 },
    ]) {
      const P = await openPage(vp, 40, 20260929);
      await sleep(150);
      const s = await P.st();
      check(`第 41 关 state.rules：${vp.name}`, JSON.stringify(s.rules) === JSON.stringify(wantRules), s.rules);
      const hud = await P.page.evaluate(() => window.m3debug.hud);
      const b = hud?.badges || [];
      const labelsOk = b.length === wantRules.length && b.every((x, i) => x.name === wantRules[i].name && x.label === wantRules[i].text);
      check(`第 41 关 HUD 画出完整角标：${vp.name}`, labelsOk, b);
      const layoutOk = b.length > 0 && b.every((x) => inside(x, hud.chip) && !overlap(x, hud.label) && x.y + x.h <= hud.name.y + 0.5)
        && b.every((x, i) => b.every((y, j) => i === j || !overlap(x, y)));
      check(`角标在关卡面板内、不压标签 / 关名 / 彼此：${vp.name}`, layoutOk, { chip: hud?.chip, label: hud?.label, name: hud?.name, badges: b });
      const mode = await P.page.evaluate(() => window.m3debug.layout.mode);
      report.ruleBadges.push({ name: vp.name, mode, badges: b.map((x) => ({ x: +x.x.toFixed(1), y: +x.y.toFixed(1), w: +x.w.toFixed(1), h: x.h, label: x.label })) });
      await P.shot(`rules-badge-${vp.name}`);
      // 走一步：新状态里规则仍在（每步的 state 都带 rules）
      await P.swap(s.hint[0], s.hint[1], !!vp.touch); await sleep(30); await P.idle();
      const s2 = await P.st();
      check(`走一步后 state.rules 不变：${vp.name}`, s2.moves === s.moves - 1 && JSON.stringify(s2.rules) === JSON.stringify(wantRules));
      await P.ctx.close();
    }
    const P = await openPage({ w: 390, h: 844, dpr: 3 }, 0, 20260929);
    await sleep(100);
    const s = await P.st(), hud = await P.page.evaluate(() => window.m3debug.hud);
    check("第 1 关没有规则角标", Array.isArray(s.rules) && s.rules.length === 0 && hud && hud.badges.length === 0, { rules: s.rules, badges: hud?.badges });
    await P.ctx.close();
    // 第 42 关「魔石」（下标 41，新玩法 2）：魔法石是元素（Custom "magic_stone"）不是规则开关，lvlRules 为空 → 没有角标
    // （与桌面 gvRules 相同）；盘面上有 4 块魔法石，图集里有 magic_stone_0..3 贴图。截图 rules-badge-l42-*.png
    const atlas = JSON.parse(fs.readFileSync(path.join(dist, "atlas.json"), "utf8")).sprites;
    check("图集含魔法石贴图 magic_stone_0..3", [0, 1, 2, 3].every((k) => atlas[`magic_stone_${k}`]));
    for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3 }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
      const Q = await openPage(vp, 41, 20260929);
      await sleep(150);
      const s42 = await Q.st(), hud42 = await Q.page.evaluate(() => window.m3debug.hud);
      const stones = s42.board.flat().filter((c) => c.t === "custom" && c.name === "magic_stone").length;
      // 魔法石走贴图（不是几何降级）：cells.js 为它选的贴图在图集里
      const sprites = await Q.page.evaluate(async (b) => { const m = await import("/cells.js"); return b.flat().filter((c) => c.t === "custom").map((c) => m.primarySprite(c)); }, s42.board);
      check(`第 42 关魔法石有贴图：${vp.name}`, sprites.length === 4 && sprites.every((n) => atlas[n]), sprites);
      check(`第 42 关：没有规则角标、盘面 4 块魔法石：${vp.name}`, s42.level === 41 && s42.rules.length === 0 && hud42.badges.length === 0 && stones === 4, { rules: s42.rules, stones });
      await Q.shot(`rules-badge-l42-${vp.name}`);
      await Q.ctx.close();
    }
  }

  // -------------------------------------------------------------------------
  // 3c. 贴图护栏：每一关开局 + 按提示走 3 步（空格加速），图集加载后 m3debug.fallbacks（走几何降级的格子，按元素名计；
  //     多格 Custom 元素走了通用画法时记为「<元素名>#多格通用画法」）必须为空。
  //     新元素合入 main 却没在 cells.js 补画法 / 网页图集里没有贴图时，这里会列出元素名和关卡。
  {
    const P = await openPage({ w: 390, h: 844, dpr: 1 }, 0, 7);
    const nLevels = await P.page.evaluate(() => window.m3debug.levels);
    const bad = [], badGoal = [], badGround = [], badName = [], badLose = [];
    report.fallbacksByLevel = [];
    report.loseHints = [];
    report.goalLabels = [];
    report.groundByLevel = [];
    report.levelNames = [];
    for (let li = 0; li < nLevels; li++) {
      await P.page.goto(`http://127.0.0.1:${PORT}/?level=${li}&seed=7`);
      await P.page.waitForFunction(() => window.m3debug && window.m3debug.state, null, { timeout: 30000 });
      await sleep(60);
      // 真实绘制（开局静止帧）：地面层格画了表内贴图（不走表外淡灰框）、HUD 关名画了 name_<li>
      {
        const s = await P.st(), d = await captureDraws(P), g = boardGrid(d, s.board.length, s.board[0].length);
        const [hud, L, dpr] = await P.page.evaluate(() => [window.m3debug.hud, window.m3debug.layout, window.m3debug.dpr]);
        if (s.ground.length) {
          const gc = g ? groundCheck(d, g, s.ground) : { ok: false, grid: null };
          report.groundByLevel.push({ level: li + 1, cells: gc.cells ?? null, ok: gc.ok });
          if (!gc.ok) badGround.push({ level: li + 1, ...gc });
        }
        const nc = nameCheck(d, li, hud, L, dpr);
        report.levelNames.push({ level: li + 1, name: s.name, sprite: hud?.name?.sprite ?? null, text: hud?.name?.text ?? null, ok: nc.ok });
        if (!nc.ok) badName.push({ level: li + 1, ...nc });
        if ([0, 40, 47].includes(li) && hud?.chip) {
          const c = hud.chip, f = `${shots}/level-name-l${String(li + 1).padStart(2, "0")}.png`;
          await P.page.screenshot({ path: f, clip: { x: L.ox + L.u * c.x - 4, y: L.oy + L.u * c.y - 4, width: L.u * c.w + 8, height: L.u * c.h + 8 } });
          report.shots.push(f);
        }
      }
      // HUD 目标标签：画出来的文字 = 「目标 」+ 核心给的中文名 goal.label，且不含内部名（[a-z_] 标识符，如 fuzzball / GoalNamed）
      const s0 = await P.st(), hudGoal = await P.page.evaluate(() => window.m3debug.hud?.goal ?? null);
      report.goalLabels.push({ level: li + 1, kind: s0.goal.kind, label: s0.goal.label, hud: hudGoal });
      if (typeof s0.goal.label !== "string" || /[a-z_]/.test(s0.goal.label) || hudGoal !== `目标 ${s0.goal.label}`) {
        badGoal.push({ level: li + 1, goal: s0.goal, hud: hudGoal });
      }
      // 失败提示（失败面板副标题 = loseHint +「。可「撤销」或「重开」」）：全关不含「箱子」与 [a-z_]；
      // 碎石目标关（第 8 / 41 / 42 / 44 / 48 关）= 「用邻消或特效砸开碎石，目标 <target> 个」（核心 39dde8e 修正）
      {
        const stoneLv = [8, 41, 42, 44, 48].includes(li + 1);
        report.loseHints.push({ level: li + 1, loseHint: s0.loseHint, target: s0.target });
        const okText = typeof s0.loseHint === "string" && s0.loseHint.length > 0 && !/箱子|[a-z_]/.test(s0.loseHint);
        const okStone = !stoneLv || s0.loseHint === `用邻消或特效砸开碎石，目标 ${s0.target} 个`;
        if (!okText || !okStone) badLose.push({ level: li + 1, loseHint: s0.loseHint, target: s0.target });
      }
      for (let k = 0; k < 3; k++) {
        const s = await P.st();
        if (s.over || !s.hint) break;
        await P.swap(s.hint[0], s.hint[1], k % 2 === 1); await sleep(30);
        await P.page.keyboard.press(" "); await P.idle();
      }
      await sleep(60);
      const fb = await P.page.evaluate(() => window.m3debug.fallbacks);
      report.fallbacksByLevel.push({ level: li + 1, fallbacks: fb });
      if (Object.keys(fb).length) bad.push({ level: li + 1, fallbacks: fb });
    }
    report.fallbackLevels = nLevels;
    check(`全部 ${nLevels} 关开局 + 走 3 步：没有格子走几何降级（m3debug.fallbacks 为空）`, nLevels >= 48 && bad.length === 0, bad);
    const groundLevels = report.groundByLevel.map((x) => x.level), groundCells = report.groundByLevel.reduce((n, x) => n + (x.cells?.length ?? 0), 0);
    check(`全部 ${nLevels} 关的地面层格（${groundLevels.length} 关 ${groundCells} 格）都真实画了表内贴图（jelly / jelly_2 / magic），棋盘格之上、棋子之下`,
      nLevels >= 48 && groundLevels.includes(39) && groundLevels.includes(48) && badGround.length === 0, { levels: groundLevels, bad: badGround });
    check(`全部 ${nLevels} 关 HUD 关名真实画了预渲染文字图 name_<关卡下标>（同桌面 HudArt），位置 = 关名槽`, nLevels >= 48 && badName.length === 0, badName);
    for (const lv of [43, 44, 45, 46, 47, 48]) {
      const row = report.fallbacksByLevel.find((x) => x.level === lv);
      check(`第 ${lv} 关 m3debug.fallbacks 为空`, !!row && Object.keys(row.fallbacks).length === 0, row);
    }
    check(`全部 ${nLevels} 关 HUD 目标标签是中文显示名（无 [a-z_] 内部名，= 「目标 」+ state.goal.label）`, nLevels >= 48 && badGoal.length === 0, badGoal);
    check(`全部 ${nLevels} 关失败提示不含「箱子」与 [a-z_]，第 8 / 41 / 42 / 44 / 48 关 =「用邻消或特效砸开碎石，目标 n 个」`,
      nLevels >= 48 && report.loseHints.length === nLevels && badLose.length === 0, badLose);
    await P.ctx.close();
  }

  // -------------------------------------------------------------------------
  // 3d. 魔法石 0–3 格充能的贴图：第 42 关种子 2 按提示走，直到盘面上同时出现 0 / 1 / 2 / 3 格（按提示走法第 11 步后为 1,3,2,0），
  //     截四块魔法石所在区域 magic-stone-charges-0123.png 与每种充能的单格放大 magic-stone-charge-<v>.png
  {
    const P = await openPage({ w: 390, h: 844, dpr: 3 }, 41, 2);
    const want = [0, 1, 2, 3], got = new Set();
    let all = false;
    for (let k = 0; k < 30 && !all; k++) {
      const s = await P.st();
      const stones = [];
      s.board.forEach((row, r) => row.forEach((c, col) => { if (c.t === "custom" && c.name === "magic_stone") stones.push({ p: [r, col], v: c.v }); }));
      // 有新充能要截图时先等连击 / 得分浮字散掉（否则会压在魔法石上）
      if (stones.some((st) => st.v <= 3 && !got.has(st.v))) await sleep(1600);
      for (const st of stones) {
        if (st.v > 3 || got.has(st.v)) continue;
        got.add(st.v);
        const [x, y] = await P.center(st.p), half = await P.page.evaluate(() => 28 * window.m3debug.layout.u + 4);
        await P.page.screenshot({ path: `${shots}/magic-stone-charge-${st.v}.png`, clip: { x: x - half, y: y - half - 4, width: 2 * half, height: 2 * half + 4 } });
        report.shots.push(`${shots}/magic-stone-charge-${st.v}.png`);
      }
      if (want.every((v) => stones.some((st) => st.v === v))) {
        all = true;
        const [x0, y0] = await P.center([2, 2]), [x1, y1] = await P.center([5, 5]), m = await P.page.evaluate(() => 34 * window.m3debug.layout.u);
        await P.page.screenshot({ path: `${shots}/magic-stone-charges-0123.png`, clip: { x: x0 - m, y: y0 - m, width: x1 - x0 + 2 * m, height: y1 - y0 + 2 * m } });
        report.shots.push(`${shots}/magic-stone-charges-0123.png`);
        report.magicStoneCharges = stones;
        await P.shot("magic-stone-charges-全盘");
        break;
      }
      if (s.over || !s.hint) break;
      await P.swap(s.hint[0], s.hint[1], false); await sleep(30);
      await P.page.keyboard.press(" "); await P.idle();
    }
    check("第 42 关魔法石 0 / 1 / 2 / 3 格充能都截到（同盘出现四种）", all && want.every((v) => got.has(v)), [...got]);
    check("魔法石画面没有走几何降级", Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0);
    await P.ctx.close();
  }

  // -------------------------------------------------------------------------
  // 3e. 第 43 关「毛球」（下标 42，新玩法 3）
  //     (1) 浮动：同桌面 sprBob（round(2·sin(pulse/9)) 设计像素）。静止时冻结在偏移 -2 与 +2 的两帧，截毛球所在格，
  //         并在页面里对两帧的格内像素做纵向平移搜索：最佳平移应 ≈ 4 设计像素 × u × dpr；
  //     (2) 步末跳格：核心记为 EvBelt "fuzzball"，按皮带段平移播放，冻结在段中间截图；
  //     (3) HUD 目标标签「目标 毛球」：竖屏 390×844 / 横屏 1280×800
  {
    const fuzzAt = (board) => { for (let r = 0; r < board.length; r++) for (let c = 0; c < board[r].length; c++) if (board[r][c].t === "custom" && board[r][c].name === "fuzzball") return [r, c]; return null; };
    const P = await openPage(desk, 42, 1);
    await sleep(150);
    const s = await P.st(), fz = fuzzAt(s.board);
    const grab = (slot) => P.page.evaluate(([r, c, slot]) => {
      // 取格子内部（±24 设计单位，避开棋盘格边缘）的后备缓冲像素
      const cv = document.getElementById("board"), d = window.m3debug.dpr, u = window.m3debug.layout.u;
      const [x, y] = window.m3debug.cellCenter(r, c), half = Math.round(24 * u * d);
      window[slot] = cv.getContext("2d").getImageData(Math.round(x * d) - half, Math.round(y * d) - half, 2 * half, 2 * half);
      return window.m3debug.anim.pulse;
    }, [fz[0], fz[1], slot]);
    // 截若干格（取外接框，四周各留 12 设计单位）
    const cropShot = async (name, ...cells) => {
      const cs = await Promise.all(cells.map((p) => P.center(p))), m = await P.page.evaluate(() => 40 * window.m3debug.layout.u);
      const xs = cs.map((c) => c[0]), ys = cs.map((c) => c[1]), x0 = Math.min(...xs) - m, y0 = Math.min(...ys) - m;
      const f = `${shots}/${name}.png`;
      await P.page.screenshot({ path: f, clip: { x: x0, y: y0, width: Math.max(...xs) + m - x0, height: Math.max(...ys) + m - y0 } });
      report.shots.push(f);
    };
    const floats = [];
    for (const [bob, slot, name] of [[-2, "__fzA", "fuzzball-float-a-up"], [2, "__fzB", "fuzzball-float-b-down"]]) {
      await P.page.evaluate((src) => { window.m3debug.breakWhen = new Function("i", src); }, `return i.kind === null && Math.round(2 * Math.sin(i.pulse / 9)) === ${bob};`);
      await P.page.waitForFunction(() => window.m3debug.frozen, null, { timeout: 10000 });
      await sleep(80);   // 等冻结帧画出来
      const pulse = await grab(slot);
      await cropShot(name, fz);
      floats.push({ bob, pulse });
      await P.resume();
    }
    const est = await P.page.evaluate(() => {
      const A = window.__fzA, B = window.__fzB, w = A.width, h = A.height;
      let best = 0, bestErr = Infinity, err0 = 0;
      for (let dy = -40; dy <= 40; dy++) {
        let err = 0, n = 0;
        for (let y = Math.max(0, -dy); y < Math.min(h, h - dy); y++) for (let x = 0; x < w; x++) {
          const i = (y * w + x) * 4, j = ((y + dy) * w + x) * 4;
          err += Math.abs(A.data[i] - B.data[j]) + Math.abs(A.data[i + 1] - B.data[j + 1]) + Math.abs(A.data[i + 2] - B.data[j + 2]); n++;
        }
        if (dy === 0) err0 = err / n;
        if (err / n < bestErr) { bestErr = err / n; best = dy; }
      }
      return { best, bestErr: +bestErr.toFixed(2), err0: +err0.toFixed(2), expected: 4 * window.m3debug.layout.u * window.m3debug.dpr };
    });
    report.fuzzballFloat = { cell: fz, frames: floats, shiftPx: est };
    check("第 43 关毛球浮动：两帧（偏移 -2 / +2 设计像素）的毛球纵向平移 ≈ 4 设计像素 × u × dpr", !!fz && Math.abs(est.best - est.expected) <= 2.5 && est.err0 > est.bestErr, report.fuzzballFloat);
    // 步末跳格：冻结在皮带段中间（本关没有真正的传送带，皮带段就是毛球跳格）
    let jumped = false;
    for (let k = 0; k < 8 && !jumped; k++) {
      const s1 = await P.st();
      if (s1.over || !s1.hint) break;
      await P.breakAt((i) => i.p === "end" && i.stage === "belt" && i.fr >= Math.floor(i.n / 2));
      await P.swap(s1.hint[0], s1.hint[1], false);
      await P.frozenOrIdle();
      if (await P.isFrozen()) {
        const jump = await P.page.evaluate(() => {
          const tr = window.m3debug.pending.trace;
          for (const e of tr.end) if (e.effect.type === "belt") for (const [o, d] of e.effect.pairs) { const c = e.before[o[0]][o[1]]; if (c.t === "custom" && c.name === "fuzzball") return { from: o, to: d }; }
          return null;
        });
        const inf = await P.info();
        if (jump) {
          jumped = true;
          await P.shot("fuzzball-jump-mid-l43-1280x800");
          await cropShot("fuzzball-jump-mid-l43-crop", jump.from, jump.to);
          report.fuzzballJump = { step: k, jump, frame: inf };
        }
      }
      await P.clearBreak(); await P.resume(); await P.idle();
    }
    check("第 43 关毛球步末跳格：冻结在皮带段中间帧（trace.end 的 belt 项从毛球格出发）", jumped, report.fuzzballJump);
    check("第 43 关（浮动 / 跳格画面）没有走几何降级", Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0);
    await P.ctx.close();
    // HUD 目标标签（竖屏 / 横屏）
    report.goalLabelL43 = [];
    for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3, touch: true, mobile: true }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
      const Q = await openPage(vp, 42, 20260929);
      await sleep(150);
      const sq = await Q.st(), hud = await Q.page.evaluate(() => window.m3debug.hud);
      report.goalLabelL43.push({ name: vp.name, label: sq.goal.label, hud: hud?.goal });
      check(`第 43 关 HUD 目标标签「目标 毛球」：${vp.name}`, sq.goal.label === "毛球" && hud?.goal === "目标 毛球", { label: sq.goal.label, hud: hud?.goal });
      await Q.shot(`goal-label-l43-${vp.name}`);
      await Q.ctx.close();
    }
  }

  // -------------------------------------------------------------------------
  // 3f. 第 44 关「魔力鸟」（下标 43，规则开关 rainbow_combos）
  //     (1) state.rules 与 HUD 角标「彩虹组合变身」（通用 state.rules 角标，竖屏 / 横屏）；
  //     (2) 彩虹 × 直线 / 彩虹 × 炸弹：核心在第一轮之前发一条蔓延步末（rainbow_line / rainbow_bomb），网页按蔓延段播放
  //         （来源不相邻 → 从格子中心长出，同桌面 drawEndSpread），冻结在变身段中间截图
  {
    const want44 = [{ name: "rainbow_combos", text: "彩虹组合变身", icons: ["rainbow"] }];
    const inside = (a, b) => a.x >= b.x - 0.5 && a.y >= b.y - 0.5 && a.x + a.w <= b.x + b.w + 0.5 && a.y + a.h <= b.y + b.h + 0.5;
    const overlap = (a, b) => a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h;
    for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3, touch: true, mobile: true }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
      const P = await openPage(vp, 43, 1);
      await sleep(150);
      const s = await P.st(), hud = await P.page.evaluate(() => window.m3debug.hud);
      const b = hud?.badges || [];
      check(`第 44 关 state.rules：${vp.name}`, JSON.stringify(s.rules) === JSON.stringify(want44), s.rules);
      check(`第 44 关 HUD 角标「彩虹组合变身」完整、在关卡面板内、不压标签 / 关名：${vp.name}`,
        b.length === 1 && b[0].name === "rainbow_combos" && b[0].label === "彩虹组合变身" && inside(b[0], hud.chip) && !overlap(b[0], hud.label) && b[0].y + b[0].h <= hud.name.y + 0.5, { badges: b, chip: hud?.chip });
      await P.shot(`rules-badge-l44-${vp.name}`);
      await P.ctx.close();
    }
    report.rainbowTransform = [];
    for (const [kind, marks] of [["line", "HV"], ["bomb", "B"]]) {
      const P = await openPage(desk, 43, 1);
      await sleep(150);
      const s = await P.st();
      let pair = null;
      for (let r = 0; r < s.board.length && !pair; r++) for (let c = 0; c < s.board[r].length && !pair; c++) for (const [qr, qc] of [[r, c + 1], [r + 1, c], [r, c - 1], [r - 1, c]]) {
        const x = s.board[r][c], y = s.board[qr]?.[qc];
        if (!pair && y && x.t === "G" && x.k === "R" && y.t === "G" && marks.includes(y.k)) pair = [[r, c], [qr, qc]];
      }
      let frozen = false, detail = { pair };
      if (pair) {
        await P.breakAt((i) => i.p === "end" && i.stage === "spread" && i.fr >= Math.floor(i.n / 2));
        await P.swap(pair[0], pair[1], false);
        await P.frozenOrIdle();
        if (await P.isFrozen()) {
          frozen = true;
          const inf = await P.info();
          const ends = await P.page.evaluate(() => window.m3debug.pending.trace.end.map((e) => ({ afterWaves: e.afterWaves, type: e.effect.type, kind: e.effect.kind, n: e.effect.pairs ? e.effect.pairs.length : 0 })));
          detail = { pair, frame: inf, ends };
          await P.shot(`rainbow-transform-${kind}-mid-l44-1280x800`);
        }
        await P.clearBreak(); await P.resume(); await P.idle();
      }
      const e0 = detail.ends?.[0];
      report.rainbowTransform.push(detail);
      check(`第 44 关彩虹 × ${kind === "line" ? "直线" : "炸弹"}：第一轮之前的变身段（rainbow_${kind}）冻结在中间帧`,
        frozen && e0 && e0.type === "spread" && e0.kind === `rainbow_${kind}` && e0.afterWaves === 0 && detail.frame.w === 0 && detail.frame.k === 0, detail);
      check(`第 44 关彩虹 × ${kind === "line" ? "直线" : "炸弹"}：播完没有走几何降级`, Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0);
      await P.ctx.close();
    }
  }

  // -------------------------------------------------------------------------
  // 3g. 第 46 关「掉落口」（下标 45，新玩法 6）：state.drops = 视图模型 bvDrops（顶行 4 个掉落口），掉落口格上沿画 cookie_drop；
  //     竖屏 390×844 / 横屏 1280×800 截图 cookie-drop-l46-*.png；按提示走，截「掉落口补下饼干的下落段」cookie-drop-fall.png
  //     与补完后的静止盘 cookie-drop-after-refill.png；
  //     交换补间中与补间结束后标记格 = state.drops（bvDrops）、坐标同桌面；其它关卡 drops 为空；全程不走几何降级
  {
    const atlas = JSON.parse(fs.readFileSync(path.join(dist, "atlas.json"), "utf8")).sprites;
    check("图集含掉落口贴图 cookie_drop", !!atlas.cookie_drop);
    const wantDrops = [[0, 1], [0, 3], [0, 4], [0, 6]];
    for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3 }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
      const P = await openPage(vp, 45, 1);
      await sleep(150);
      const s = await P.st();
      const cookiesOnDrops = wantDrops.every(([r, c]) => s.board[r][c].t === "cookie");
      check(`第 46 关 state.drops = 顶行 4 个掉落口、开局饼干在口上：${vp.name}`, s.level === 45 && JSON.stringify(s.drops) === JSON.stringify(wantDrops) && cookiesOnDrops, s.drops);
      await P.shot(`cookie-drop-l46-${vp.name}`);
      await P.ctx.close();
    }
    {
      const P = await openPage({ w: 390, h: 844, dpr: 1 }, 0, 1);
      await sleep(100);
      const s = await P.st();
      check("第 1 关没有掉落口（state.drops = []）", Array.isArray(s.drops) && s.drops.length === 0, s.drops);
      await P.ctx.close();
    }
    // 掉落口标记是不动的装饰：交换补间中（网页也画，桌面 drawSwap 不画）与补间结束后的静止盘上，标记格 = state.drops
    // （核心 bvDrops），坐标 = 桌面 drawDropsArt 的 (cellOrigin, y − 6)，换成棋盘设计坐标即 (16 + 56c, 16 + 56r − 6)
    {
      const P = await openPage({ w: 390, h: 844, dpr: 1 }, 45, 1);
      await sleep(150);
      const want = (drops) => drops.map(([r, c]) => ({ p: [r, c], x: 16 + 56 * c, y: 16 + 56 * r - 6 }));
      const fresh = async () => { const a = await P.page.evaluate(() => window.m3debug.dropMarks.seq); await sleep(80); const m = await P.page.evaluate(() => window.m3debug.dropMarks); return m.seq > a ? m : null; };
      const s0 = await P.st(), idle0 = await fresh();
      await P.breakAt((i) => i.kind === "swap" && i.fr >= 3);
      await P.swap(s0.hint[0], s0.hint[1], false);
      await P.frozenOrIdle();
      const inSwap = (await P.isFrozen()) ? await P.page.evaluate(() => window.m3debug.dropMarks) : null;
      await P.clearBreak(); await P.resume(); await P.idle();
      const s1 = await P.st(), idle1 = await fresh();
      report.dropMarks = { drops: s1.drops, idle0: idle0?.marks, inSwap: inSwap?.marks, idle1: idle1?.marks };
      const ok = (m, drops) => !!m && JSON.stringify(m) === JSON.stringify(want(drops));
      check("第 46 关交换补间中：掉落口标记格 = state.drops（bvDrops）、坐标同桌面 drawDropsArt", ok(inSwap?.marks, s0.drops), report.dropMarks);
      check("第 46 关补间结束后：掉落口标记格 = state.drops（bvDrops）、坐标同桌面 drawDropsArt，与走之前相同",
        s1.moves === s0.moves - 1 && ok(idle0?.marks, s0.drops) && ok(idle1?.marks, s1.drops) && JSON.stringify(s1.drops) === JSON.stringify(wantDrops), report.dropMarks);
      await P.ctx.close();
    }
    // 种子 30 按提示走第 5 步收走一块饼干，同一步补子时掉落口 (0,1) 补下新饼干（种子 1 走满 26 步也收不到饼干，不会补）
    const P = await openPage({ w: 390, h: 844, dpr: 3 }, 45, 30);
    const fallCond = (i) => {
      if (i.p !== "fall" || i.fr < Math.floor(i.n * 0.75)) return false;
      const st = window.m3debug.pending, w = st?.trace.waves[i.w];
      return !!w && (st.state.drops || []).some(([r, c]) => w.after[r][c].t === "cookie" && !(w.holes[r][c] && w.holes[r][c].t === "cookie"));
    };
    let got = false, collected0 = (await P.st()).progress;
    for (let k = 0; k < 26 && !got; k++) {
      const s = await P.st();
      if (s.over || !s.hint) break;
      await P.breakAt(fallCond);
      await P.swap(s.hint[0], s.hint[1], k % 2 === 0);
      await P.frozenOrIdle();
      if (await P.isFrozen()) { got = true; await P.shot("cookie-drop-fall"); }
      await P.clearBreak(); await P.resume(); await P.idle();
    }
    const s2 = await P.st();
    await sleep(1600);   // 等浮字散掉
    if (got) await P.shot("cookie-drop-after-refill");
    report.cookieDrop = { progress: [collected0, s2.progress], movesLeft: s2.moves, cookies: s2.board.flat().filter((c) => c.t === "cookie").length };
    check("第 46 关截到掉落口补下饼干的下落段、之后盘上仍是 4 块饼干", got && report.cookieDrop.cookies === 4 && report.cookieDrop.progress[1] >= 1, report.cookieDrop);
    check("第 46 关画面没有走几何降级", Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0, await P.page.evaluate(() => window.m3debug.fallbacks));
    await P.ctx.close();
  }

  // -------------------------------------------------------------------------
  // 3h. 真实绘制核对（ctx.drawImage 钩子，见 DRAW_HOOK；不用页面自报的 m3debug.dropMarks）：
  //     (a) 第 46 / 47 关掉落口标记 cookie_drop 的真实目标矩形 = 桌面 drawDropsArt 的 (16 + 56c, 16 + 56r − 6)（换算到屏幕：
  //         以真实画出的底格 tile_a / tile_b 求 (0,0) 格位置与缩放），静止帧与交换补间帧都查；
  //     (b) 第 47 关「变色龙」（下标 46，新玩法 7）：每只变色龙格真实画了 gem_c<v+1>（v 取核心格子的原始值）、位置在该格，
  //         且调用序在环 chameleon 之前；HUD「目标 变色龙」+ 图标 chameleon_icon；换色（步末 tick 段）前半段旧色、后半段新色，
  //         播完后 state 的颜色 = trace.end 的换色结果、真实绘制随之更新；截图 chameleon-l47-*.png / chameleon-shift-{before,mid}.png；
  //     反证：页面里 forceGeneric.add("chameleon") 强制旧的通用画法（只有环），(b) 必须不成立、fallbacks 报出
  //     「chameleon#通用画法缺底层宝石」；撤掉后恢复。裁图 chameleon-crop-{before-generic,after}.png
  {
    report.drawHook = { drops: [], chameleon: [] };
    const chamCells = (b) => b.flatMap((row, r) => row.flatMap((x, c) => (x.t === "custom" && x.name === "chameleon" ? [{ p: [r, c], v: x.v, c: x.c }] : [])));
    // (a) 掉落口：第 46 关（顶行 4 个）与第 47 关 (0,3)，静止帧 + 交换补间中间帧
    for (const [li, seed, wantDrops] of [[45, 1, [[0, 1], [0, 3], [0, 4], [0, 6]]], [46, 1, [[0, 3]]]]) {
      for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3 }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
        const P = await openPage(vp, li, seed);
        await sleep(150);
        const s = await P.st();
        const d0 = await captureDraws(P), g0 = boardGrid(d0, s.board.length, s.board[0].length), r0 = g0 && dropMarkCheck(d0, g0, s.drops);
        check(`第 ${li + 1} 关真实绘制（静止帧）：掉落口 cookie_drop 画在桌面 drawDropsArt 的 (16+56c, 16+56r−6)：${vp.name}`,
          JSON.stringify(s.drops) === JSON.stringify(wantDrops) && !!r0?.ok, { drops: s.drops, grid: g0, ...r0 });
        await P.breakAt((i) => i.kind === "swap" && i.fr >= 4);
        await P.swap(s.hint[0], s.hint[1], false);
        await P.frozenOrIdle();
        let r1 = null, g1 = null, frozen = await P.isFrozen();
        if (frozen) {
          const d1 = await captureDraws(P);
          g1 = boardGrid(d1, s.board.length, s.board[0].length); r1 = g1 ? dropMarkCheck(d1, g1, s.drops) : null;
          if (r1) r1.swapFrame = (await P.info()).kind;
        }
        check(`第 ${li + 1} 关真实绘制（交换补间帧）：掉落口 cookie_drop 位置同上：${vp.name}`, frozen && !!r1?.ok && r1.swapFrame === "swap", { grid: g1, ...r1 });
        report.drawHook.drops.push({ level: li + 1, vp: vp.name, static: r0, swap: r1 });
        await P.clearBreak(); await P.resume(); await P.idle();
        await P.ctx.close();
      }
    }
    // (b) 变色龙：竖屏 / 横屏开局
    for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3 }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
      const P = await openPage(vp, 46, 1);
      await sleep(150);
      const s = await P.st(), hud = await P.page.evaluate(() => window.m3debug.hud), cs = chamCells(s.board);
      check(`第 47 关开局：关名「变色龙」、两只变色龙、格子 c = v + 1（元素自带的显示字段）：${vp.name}`,
        s.level === 46 && s.name === "变色龙" && cs.length === 2 && cs.every((x) => x.c === x.v + 1), cs);
      check(`第 47 关 HUD「目标 变色龙」、目标图标 chameleon_icon：${vp.name}`,
        s.goal.label === "变色龙" && hud?.goal === "目标 变色龙" && s.goal.icon === "chameleon_icon" && hud?.goalIcon === "chameleon_icon",
        { label: s.goal.label, icon: s.goal.icon, hud: { goal: hud?.goal, goalIcon: hud?.goalIcon } });
      const d = await captureDraws(P), g = boardGrid(d, s.board.length, s.board[0].length), cc = g && chamCheck(d, g, s.board);
      check(`第 47 关真实绘制：变色龙格先画 gem_c<v+1>（在该格）再叠环 chameleon：${vp.name}`, !!cc?.ok, cc);
      report.drawHook.chameleon.push({ vp: vp.name, cells: cc?.cells });
      await P.shot(`chameleon-l47-${vp.name}`);
      await P.ctx.close();
    }
    // 反证：强制通用画法（接入前的样子：只画环 + 角标，没有底层宝石）
    {
      const P = await openPage({ w: 390, h: 844, dpr: 3 }, 46, 1);
      await sleep(150);
      const s = await P.st(), cs = chamCells(s.board);
      const crop = async (name) => {
        const [x, y] = await P.center(cs[0].p), m = await P.page.evaluate(() => 40 * window.m3debug.layout.u), f = `${shots}/${name}.png`;
        await P.page.screenshot({ path: f, clip: { x: x - m, y: y - m, width: 2 * m, height: 2 * m } }); report.shots.push(f);
      };
      await crop("chameleon-crop-after");
      await P.page.evaluate(async () => { const c = await import("/cells.js"); c.forceGeneric.add("chameleon"); });
      await sleep(80);
      const dF = await captureDraws(P), gF = boardGrid(dF, s.board.length, s.board[0].length), ccF = gF && chamCheck(dF, gF, s.board);
      const fb1 = await P.page.evaluate(() => window.m3debug.fallbacks);
      await crop("chameleon-crop-before-generic");
      await P.page.evaluate(async () => { const c = await import("/cells.js"); c.forceGeneric.delete("chameleon"); });
      await sleep(80);
      const dR = await captureDraws(P), gR = boardGrid(dR, s.board.length, s.board[0].length), ccR = gR && chamCheck(dR, gR, s.board);
      report.drawHook.forcedGeneric = { forced: ccF, fallbacks: fb1, restored: ccR?.ok };
      check("反证：强制变色龙走通用画法时，真实绘制核对不成立（没有 gem_c<v+1>）且 fallbacks 报出 chameleon#通用画法缺底层宝石",
        !!ccF && !ccF.ok && (fb1["chameleon#通用画法缺底层宝石"] || 0) > 0, { forced: ccF, fb1 });
      check("反证撤掉后：真实绘制核对恢复成立", !!ccR?.ok, ccR);
      await P.ctx.close();
    }
    // 换色：步末 tick 段前半段（旧色）/ 后半段（新色）各冻结一次，核对真实绘制；播完后 state 颜色 = 换色结果
    {
      const P = await openPage({ w: 390, h: 844, dpr: 3 }, 46, 1);
      await sleep(150);
      const s0 = await P.st(), rows = s0.board.length, cols = s0.board[0].length;
      const tickEnd = () => P.page.evaluate(() => {
        const e = window.m3debug.pending.trace.end.find((x) => x.effect.type === "tick");
        return e ? { cells: e.effect.cells, before: e.before, after: e.after } : null;
      });
      await P.breakAt((i) => i.p === "end" && i.stage === "tick" && i.fr >= 1 && i.fr <= Math.floor(i.n * 0.3));
      await P.swap(s0.hint[0], s0.hint[1], false);
      await P.frozenOrIdle();
      let early = null, late = null, te = null;
      if (await P.isFrozen()) {
        te = await tickEnd();
        const d = await captureDraws(P), g = boardGrid(d, rows, cols);
        early = g && chamCheck(d, g, te.before);
        await P.shot("chameleon-shift-before");
        await P.breakAt((i) => i.p === "end" && i.stage === "tick" && i.fr >= Math.ceil(i.n * 0.6));
        await P.resume();
        await P.frozenOrIdle();
        if (await P.isFrozen()) {
          const d2 = await captureDraws(P), g2 = boardGrid(d2, rows, cols);
          late = g2 && chamCheck(d2, g2, te.after);
          await P.shot("chameleon-shift-mid");
        }
      }
      await P.clearBreak(); await P.resume(); await P.idle();
      await sleep(100);
      const s1 = await P.st(), after = te ? te.cells.map(([r, c]) => te.after[r][c]) : [], before = te ? te.cells.map(([r, c]) => te.before[r][c]) : [];
      const d3 = await captureDraws(P), g3 = boardGrid(d3, rows, cols), idle = g3 && chamCheck(d3, g3, s1.board);
      const changed = before.some((b, i) => b.v !== after[i].v);
      report.chameleonShift = { cells: te?.cells, before: before.map((x) => x.v), after: after.map((x) => x.v), early: early?.cells, late: late?.cells, idle: idle?.cells };
      check("第 47 关换色段前半段：真实绘制是换色前的颜色（gem_c<旧 v+1> 在环之前）", !!early?.ok, report.chameleonShift);
      check("第 47 关换色段后半段：真实绘制是换色后的颜色（gem_c<新 v+1> 在环之前）", !!late?.ok && changed, report.chameleonShift);
      check("第 47 关换色播完：state 里变色龙 = trace.end 的换色结果，真实绘制随之更新",
        !!te && te.cells.every(([r, c]) => JSON.stringify(s1.board[r][c]) === JSON.stringify(te.after[r][c])) && !!idle?.ok, report.chameleonShift);
      check("第 47 关画面没有走几何降级", Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0, await P.page.evaluate(() => window.m3debug.fallbacks));
      await P.ctx.close();
    }
  }

  // -------------------------------------------------------------------------
  // 3k. 音效 / BGM 开关芯片（效 / 乐）：竖屏 390×844、横屏 1280×800、横持手机 844×390 下第 1 / 45 / 48 / 49 关开局，
  //     真实绘制（fillText 外框）核对：字形在芯片里、芯片不大于 HUD 最小按钮、不压提示行（soundChipCheck）；
  //     第 1 关竖屏点芯片不切换（芯片只显示状态），经菜单「音效」关掉 → 画成「静」、仍满足同样条件
  {
    report.soundChips = [];
    for (const vp of [{ w: 390, h: 844, dpr: 2, tag: "portrait-390x844" }, { w: 1280, h: 800, dpr: 1, tag: "landscape-1280x800" }, { w: 844, h: 390, dpr: 2, tag: "landscape-844x390" }]) {
      for (const li of [0, 44, 47, 48]) {
        const P = await openPage(vp, li, 1);
        await sleep(300);
        const sc = await soundChipCheck(P);
        report.soundChips.push({ level: li + 1, vp: vp.tag, ...sc });
        check(`第 ${li + 1} 关音效 / BGM 开关：字形在芯片内、不大于 HUD 按钮、不压提示行：${vp.tag}`, sc.ok, sc);
        if (li === 0 && vp.tag === "portrait-390x844") {
          // 芯片只显示状态（太小，不当触控目标）：点芯片不切换；经菜单「音效」关掉后芯片画成「静」
          const r = sc.chips[0].rect, L = await P.page.evaluate(() => window.m3debug.layout);
          await P.page.mouse.click(L.ox + (r.x + r.w / 2) * L.u, L.oy + (r.y + r.h / 2) * L.u); await sleep(100);
          const still = await P.page.evaluate(() => localStorage.getItem("m3-sfx"));
          await P.button("sfx"); await sleep(200);
          const s2 = await soundChipCheck(P);
          report.soundChips.push({ level: 1, vp: vp.tag, toggled: true, ...s2 });
          check("第 1 关点芯片不切换；菜单「音效」关掉后芯片画成「静」，仍在芯片内、不压提示行：portrait-390x844", still !== "off" && s2.ok && s2.chips[0].glyph?.text === "静" && s2.chips[1].glyph?.text === "乐", s2);
        }
        if ([0, 47, 48].includes(li) && vp.tag !== "landscape-844x390") await P.shot(`sound-chips-l${String(li + 1).padStart(2, "0")}-${vp.tag}`);
        await P.ctx.close();
      }
    }
  }

  // -------------------------------------------------------------------------
  // 3j. 第 48 关「魔法格」（下标 47，新玩法 8：地面层 magic，特效在上面引爆时范围扩一圈）：
  //     (a) 竖屏 / 横屏开局：state.ground = 4 格 magic (6,2)(6,5)(5,3)(5,4)、layers 1，关名 name_47「魔法格」，HUD「目标 碎石」；
  //         真实绘制：4 格都画了 magic 贴图（位置 = 该格、棋盘格之上、棋子之下），像素上格边一圈是紫色（不是淡灰框）；截图 magic-l48-*.png
  //     (b) 测试跑手找到的 4 组走法（同 parity.sh 的 fix 用例）第 3 步在魔法地格上引爆：冻结在该轮「消失」段中间，
  //         EvBlast（pending.events 的 blast）的来源 / 格数 / 行列数 = 原生（parity 的「魔法地格扩爆」行），扩出来的一圈与原范围的每个目标格
  //         都有真实绘制——被消格画了消失段（格中心的光环 + 缩小的棋子），碎石等受击不消的格画了受击后的样子（holes，层数变了）；
  //         两类合计 = EvBlast 格数；截图 magic-widen-seed<N>.png
  //     (c) 终章：第 47 关种子 2 / 第 48 关种子 3 / 第 49 关种子 1（宽域 golden 走法）走完：
  //         第 47/48 关 LevelClear 进入下一关，第 49 关（宽域，最后一关）Won「通关！」
  {
    const MAGIC = [[6, 2], [6, 5], [5, 3], [5, 4]];
    report.magic = { layout: [], widen: [], ending: [] };
    for (const vp of [{ name: "portrait-390x844", w: 390, h: 844, dpr: 3 }, { name: "landscape-1280x800", w: 1280, h: 800, dpr: 2 }]) {
      const P = await openPage(vp, 47, 1);
      await sleep(150);
      const s = await P.st(), hud = await P.page.evaluate(() => window.m3debug.hud);
      const magic = s.ground.filter((g) => g.name === "magic");
      check(`第 48 关开局：state.ground = 4 格 magic（layers 1）、关名「魔法格」、HUD「目标 碎石」：${vp.name}`,
        s.level === 47 && s.name === "魔法格" && s.ground.length === 4 && magic.length === 4 && magic.every((g) => g.layers === 1) &&
        MAGIC.every(([r, c]) => magic.some((g) => g.p[0] === r && g.p[1] === c)) && s.goal.label === "碎石" && hud?.goal === "目标 碎石" && hud?.name?.sprite === "name_47",
        { ground: s.ground, name: s.name, goal: hud?.goal, nameSprite: hud?.name?.sprite });
      const d = await captureDraws(P), g = boardGrid(d, s.board.length, s.board[0].length), gc = g && groundCheck(d, g, s.ground);
      check(`第 48 关真实绘制：4 格魔法地格画了 magic 贴图（在该格、棋盘格之上、棋子之下）：${vp.name}`, !!gc?.ok && gc.cells.length === 4, gc);
      const others = [];
      for (const [r] of MAGIC) for (let c = 0; c < 8; c++) if (!MAGIC.some((m) => m[0] === r && m[1] === c)) others.push([r, c]);
      const pm = g ? await purpleBand(P, g, MAGIC) : [], po = g ? await purpleBand(P, g, others) : [];
      const base = po.length ? po.reduce((a, b) => a + b, 0) / po.length : 0;
      report.magic.layout.push({ vp: vp.name, cells: gc?.cells, purpleMagic: pm, purpleOthersMean: +base.toFixed(1) });
      check(`第 48 关像素：魔法地格格边一圈是紫色地砖（紫度比同行其他格平均高 ≥ 15；淡灰框不带紫色）：${vp.name}`, pm.length === 4 && pm.every((x) => x >= base + 15), { purpleMagic: pm, othersMean: base });
      check(`第 48 关开局没有走几何降级：${vp.name}`, Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0, await P.page.evaluate(() => window.m3debug.fallbacks));
      await P.shot(`magic-l48-${vp.name}`);
      await P.ctx.close();
    }
    // (b) 扩圈爆炸
    const PLAN = { 2: [[[3, 5], [3, 6]], [[4, 4], [4, 5]], [[4, 2], [5, 2]]], 3: [[[0, 4], [1, 4]], [[4, 4], [4, 5]], [[5, 2], [6, 2]]],
      4: [[[1, 4], [1, 5]], [[5, 4], [5, 5]], [[5, 4], [5, 5]]], 5: [[[1, 5], [1, 6]], [[2, 4], [3, 4]], [[4, 2], [4, 3]]] };
    // 原生侧（parity.sh 的 fix 用例、Parity.hs 的 magicBlasts）给出的扩爆：元素、来源、格数、行数、列数
    const NATIVE = { 2: ["bomb", [5, 3], 25, 5, 5], 3: ["line_h", [6, 2], 24, 3, 8], 4: ["line_h", [5, 4], 24, 3, 8], 5: ["line_h", [5, 3], 24, 3, 8] };
    for (const seed of [2, 3, 4, 5]) {
      const P = await openPage({ w: 390, h: 844, dpr: 3 }, 47, seed);
      await sleep(150);
      const mk = new Set(MAGIC.map((p) => p.join(",")));
      let ok12 = true;
      for (let k = 0; k < 2; k++) {
        const m0 = (await P.st()).moves;
        await P.swap(PLAN[seed][k][0], PLAN[seed][k][1], false); await sleep(30);
        await P.page.keyboard.press(" "); await P.idle();
        ok12 = ok12 && (await P.st()).moves === m0 - 1;
      }
      const sb = await P.st();
      await P.breakAt((i) => i.kind === "cascade" && i.p === "pop" && i.blast && i.n > 0 && i.fr >= Math.floor(i.n / 3));
      await P.swap(PLAN[seed][2][0], PLAN[seed][2][1], false);
      await P.frozenOrIdle();
      const row = { seed, ok12 };
      if (await P.isFrozen()) {
        const inf = await P.info();
        const pend = await P.page.evaluate(() => ({ events: window.m3debug.pending.events, waves: window.m3debug.pending.trace.waves }));
        const blasts = pend.events.filter((e) => e.kind === "blast").flatMap((e) => [...new Set(e.pairs.map((p) => p[0].join(",")))].filter((q) => mk.has(q)).map((src) => {
          const ts = [...new Set(e.pairs.filter((p) => p[0].join(",") === src).map((p) => p[1].join(",")))].map((q) => q.split(",").map(Number));
          return { subject: e.subject, beat: e.beat, src: src.split(",").map(Number), targets: ts, n: ts.length, rows: new Set(ts.map((q) => q[0])).size, cols: new Set(ts.map((q) => q[1])).size };
        }));
        const b = blasts[0];
        row.blast = b ? { subject: b.subject, beat: b.beat, src: b.src, n: b.n, rows: b.rows, cols: b.cols } : null;
        row.frozenAt = { w: inf.w, p: inf.p, fr: inf.fr, n: inf.n };
        if (b && inf.w === b.beat) {
          const w = pend.waves[b.beat], cleared = new Set(pend.events.filter((e) => e.beat === b.beat && e.kind === "clear").flatMap((e) => e.pairs.map((p) => p[0].join(","))));
          const d = await captureDraws(P), g = boardGrid(d, sb.board.length, sb.board[0].length);
          // 原范围：炸弹 = 来源 3×3，横线 = 来源所在行，竖线 = 来源所在列；其余目标格是扩出来的一圈
          const base = ([r, c]) => (b.subject === "bomb" ? Math.abs(r - b.src[0]) <= 1 && Math.abs(c - b.src[1]) <= 1 : b.subject === "line_h" ? r === b.src[0] : c === b.src[1]);
          const per = b.targets.map(([r, c]) => {
            const S = 56 * g.k, cx = g.x + (56 * c + 28) * g.k, cy = g.y + (56 * r + 28) * g.k, x0 = g.x + 56 * c * g.k, y0 = g.y + 56 * r * g.k;
            const isCl = cleared.has(`${r},${c}`);
            // 被消格：消失段在格中心画光环（≥ 一格大）与缩小的棋子（< 一格）；受击不消：在该格画 holes 里受击后的格子，且与消除前不同
            const ringDrawn = d.some((x) => near(x.cx, cx, 1) && near(x.cy, cy, 1) && x.w >= S - 0.5 && (!x.name || x.name === "spark"));
            const shrunk = d.some((x) => near(x.cx, cx, 1) && near(x.cy, cy, 1) && x.w < S - 0.5 && !!x.name);
            const hitDrawn = !isCl && JSON.stringify(w.holes[r][c]) !== JSON.stringify(w.before[r][c]) && d.some((x) => !!x.name && !/^tile_/.test(x.name) && x.name !== "magic" && near(x.x0, x0, 1) && near(x.y0, y0, 1) && near(x.w, S, 1));
            return { p: [r, c], ring: !base([r, c]), kind: isCl ? "clear" : "hit", ok: isCl ? ringDrawn && shrunk : hitDrawn };
          });
          row.drawn = per.filter((x) => x.ok).length;
          row.drawnClear = per.filter((x) => x.ok && x.kind === "clear").length;
          row.drawnHit = per.filter((x) => x.ok && x.kind === "hit").length;
          row.ringCells = per.filter((x) => x.ring).length;
          row.ringDrawn = per.filter((x) => x.ring && x.ok).length;
          row.missing = per.filter((x) => !x.ok);
        }
        await P.shot(`magic-widen-seed${seed}`);
      }
      await P.clearBreak(); await P.resume(); await P.idle();
      row.fallbacks = await P.page.evaluate(() => window.m3debug.fallbacks);
      report.magic.widen.push(row);
      const nat = NATIVE[seed], bb = row.blast;
      check(`第 48 关种子 ${seed}：前两步照走、第 3 步在魔法地格上引爆，EvBlast = 原生（${nat[0]}@(${nat[1]}) ${nat[2]} 格 ${nat[3]} 行 ${nat[4]} 列）`,
        ok12 && !!bb && bb.subject === nat[0] && bb.src.join(",") === nat[1].join(",") && bb.n === nat[2] && bb.rows === nat[3] && bb.cols === nat[4], row);
      check(`第 48 关种子 ${seed}：扩圈爆炸的每个目标格都有消失 / 受击的真实绘制，合计 = EvBlast 格数（扩出来的一圈全在内）`,
        !!bb && row.drawn === bb.n && row.ringCells > 0 && row.ringDrawn === row.ringCells, row);
      check(`第 48 关种子 ${seed}：没有走几何降级`, Object.keys(row.fallbacks).length === 0, row.fallbacks);
      await P.ctx.close();
    }
    // (c) 终章：第 47 关过关进入第 48 关；第 48 关过关进入第 49 关；第 49 关（宽域 6×9）是最后一关，过关为 Won「通关！」
    const LINES = { 46: [2, "3132-7677-4353-4142-1213-3242-3637-6263-6171-3444-4555-4445-0212-1727-0414-3132"],
      47: [3, "4445-5051-6364-4344-6667-6263-5060-0405-2526-4445-2636-5666-5556-4454-5565"],
      48: [1, "1222-3444-0616-1626"] };
    for (const li of [46, 47, 48]) {
      const [seed, line] = LINES[li];
      const P = await openPage({ w: 390, h: 844, dpr: 1 }, li, seed);
      for (const mv of line.split("-")) {
        const [a, b2, c, d] = [...mv].map(Number);
        if ((await P.st()).over) break;
        await P.swap([a, b2], [c, d], false); await sleep(20);
        await P.page.keyboard.press(" "); await P.idle();
      }
      await sleep(120);
      const s = await P.st(), ov = await P.page.evaluate(() => window.m3debug.overlay), n = await P.page.evaluate(() => window.m3debug.levels);
      report.magic.ending.push({ level: li + 1, seed, over: s.over, overlay: ov, levels: n });
      if (li === 46) check("第 47 关过关（LevelClear）：结局面板「过关！」、进入第 48 关",
        s.over?.tag === "LevelClear" && s.over.next === 47 && ov?.title === "过关！" && ov.sub.includes("进入第 48 关"), { over: s.over, overlay: ov });
      else if (li === 47) check("第 48 关过关（LevelClear）：结局面板「过关！」、进入第 49 关（不再是终章）",
        s.over?.tag === "LevelClear" && s.over.next === 48 && ov?.title === "过关！" && ov.sub.includes("进入第 49 关"), { over: s.over, overlay: ov, levels: n });
      else {
        check("第 49 关是终章：最后一关、过关为 Won，结局面板「通关！」", n === 49 && s.over?.tag === "Won" && ov?.title === "通关！", { over: s.over, overlay: ov, levels: n });
        await P.shot("wide-l49-won");
      }
      await P.ctx.close();
    }
  }

  // -------------------------------------------------------------------------
  // 3i. 失败提示不漏内部名：第 8 / 39 / 40 / 41 / 42 / 43 / 44 / 45 / 47 / 48 关按提示走到步数用完（空格加速），结局面板实际画出的文字（m3debug.overlay）
  //     标题「步数用完了」、副标题含 state.loseHint（核心中文失败提示）且不含 [a-z_] /「箱子」；
  //     碎石目标关（8 / 41 / 42 / 44 / 48）副标题 =「用邻消或特效砸开碎石，目标 n 个。可「撤销」或「重开」」。种子 7 按提示过了关就换下一个种子
  {
    report.loseOverlays = [];
    for (const li of [7, 38, 39, 40, 41, 42, 43, 44, 46, 47]) {
      let P = null, s = null, ov = null, seed = 7;
      for (; seed < 12; seed++) {
        if (P) await P.ctx.close();
        P = await openPage({ w: 390, h: 844, dpr: 1 }, li, seed);
        for (let k = 0; k < 80; k++) {
          const t = await P.st();
          if (t.over || !t.hint) break;
          await P.swap(t.hint[0], t.hint[1], false); await sleep(20);
          await P.page.keyboard.press(" "); await P.idle();
        }
        await sleep(120);
        s = await P.st(); ov = await P.page.evaluate(() => window.m3debug.overlay);
        if (s.over?.tag === "Lost") break;
      }
      const stoneLv = [8, 41, 42, 44, 48].includes(li + 1);
      report.loseOverlays.push({ level: li + 1, seed, over: s.over, loseHint: s.loseHint, overlay: ov });
      check(`第 ${li + 1} 关失败面板（种子 ${seed}）：副标题含核心失败提示、不含 [a-z_] 内部名与「箱子」` + (stoneLv ? "，=「用邻消或特效砸开碎石，目标 n 个。…」" : ""),
        s.over?.tag === "Lost" && ov && ov.title === "步数用完了" && ov.sub.includes(s.loseHint) && !/[a-z_]|箱子/.test(ov.title + ov.sub) &&
        (!stoneLv || ov.sub === `用邻消或特效砸开碎石，目标 ${s.target} 个。可「撤销」或「重开」`), { over: s.over, loseHint: s.loseHint, target: s.target, overlay: ov });
      if (li === 46 && s.over?.tag === "Lost") await P.shot("chameleon-l47-lost");
      if (li === 47 && s.over?.tag === "Lost") await P.shot("magic-l48-lost");
      await P.ctx.close();
    }
  }

  // -------------------------------------------------------------------------
  // 4. 动画中改变视口：横屏桌面 → 竖屏手机尺寸，冻结在连锁中截图，再解冻播完
  {
    const P = await openPage({ w: 1280, h: 800, dpr: 2 }, 0, 20260930);
    let done = false;
    for (let k = 0; k < 20 && !done; k++) {
      const s = await P.st();
      if (s.over || !s.hint) break;
      await P.breakAt((i) => i.p === "fall" && i.fr >= 3);
      await P.swap(s.hint[0], s.hint[1], false);
      await P.frozenOrIdle();
      if (await P.isFrozen()) {
        done = true;
        const before = await P.info();
        await P.shot("30-改尺寸前-1280x800-下落中");
        await P.page.setViewportSize({ width: 430, height: 900 });
        await sleep(300);
        const mid = await P.info();
        await P.shot("31-改尺寸后-430x900-同一帧");
        check("改尺寸不打断动画（同一阶段同一帧）", mid.kind === before.kind && mid.p === before.p && mid.fr === before.fr && mid.w === before.w, { before, mid });
        await P.clearBreak(); await P.resume();
        await sleep(120);
        await P.page.setViewportSize({ width: 1024, height: 700 });
        await P.idle();
        const s2 = await P.st();
        check("改尺寸后本步正常结算（扣 1 步）", s2.moves === s.moves - 1);
        await P.shot("32-播完-1024x700");
      }
      await P.clearBreak(); await P.resume(); await P.idle();
    }
    check("截到改尺寸中的动画", done);
    await P.ctx.close();
  }

  // -------------------------------------------------------------------------
  // 5. 桌面版功能迁移（web-sdl-parity，网页将是唯一前端）：HUD 常驻按钮 + 菜单（格子不小于 33e65bf、触控 ≥ 44 px、菜单逐项可用）、道具（锤子 / 自由交换 / 十字消，含 keepTool 与次数用完）、
  //    手动洗牌、每日挑战、选关地图（跳关 / 未解锁 / localStorage 进度）、暂停（冻结动画、R 可重开、点任意处继续）、按键表与 Esc、
  //    结局后前进（过关带入剩余步数 / 失败重开）与星级、分数徽章（连击 / 总结）、首关提示与按键条、窗口标题、元素展示盘。
  //    每个功能竖屏 390×844 与横屏 1280×800 各截一张 sdl-<功能>-<竖屏|横屏>.png。
  {
    const VPS = [{ tag: "竖屏", w: 390, h: 844, dpr: 2 }, { tag: "横屏", w: 1280, h: 800, dpr: 1 }];
    const ui = (P) => P.page.evaluate(() => window.m3debug.ui);
    const hudOf = (P) => P.page.evaluate(() => window.m3debug.hud);
    const press = async (P, k) => { await P.page.keyboard.press(k); await sleep(40); };
    const ws = (x) => String(x).replace(/\s+/g, " ").trim();   // document.title 会把连续空白折成一个
    const lsGet = (P, k) => P.page.evaluate((key) => localStorage.getItem(key), k);
    const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
    const overlap = (a, b) => a.x < b.x + b.w - 0.01 && b.x < a.x + a.w - 0.01 && a.y < b.y + b.h - 0.01 && b.y < a.y + a.h - 0.01;
    // 按提示走到结局（空格加速）；cb(P) 在每步交换之后、加速之前调用
    async function playToEnd(P, maxMoves = 80) {
      for (let k = 0; k < maxMoves; k++) {
        const s = await P.st();
        if (s.over || !s.hint) return s;
        await P.swap(s.hint[0], s.hint[1], false); await sleep(30);
        await P.page.keyboard.press(" "); await P.idle();
      }
      return P.st();
    }
    // 自由交换「换不掉」的一对：两格都是普通宝石、颜色不同、不相邻，换完后经过两格都没有 3 连（测试侧只看普通宝石）
    function noMatchPair(b) {
      const R = b.length, C = b[0].length, plain = (x) => x && x.t === "G" && x.k === "N" && x.i === 0 && x.o === null;
      const runs = (g, r, c) => {
        const col = g[r][c].c, at = (rr, cc) => rr >= 0 && rr < R && cc >= 0 && cc < C && g[rr][cc].t === "G" && g[rr][cc].c === col;
        let h = 1, v = 1;
        for (let d = 1; at(r, c - d); d++) h++; for (let d = 1; at(r, c + d); d++) h++;
        for (let d = 1; at(r - d, c); d++) v++; for (let d = 1; at(r + d, c); d++) v++;
        return h >= 3 || v >= 3;
      };
      for (let r1 = 0; r1 < R; r1++) for (let c1 = 0; c1 < C; c1++) for (let r2 = r1; r2 < R; r2++) for (let c2 = 0; c2 < C; c2++) {
        if ((r2 === r1 && c2 <= c1) || Math.abs(r1 - r2) + Math.abs(c1 - c2) <= 1) continue;
        const a = b[r1][c1], q = b[r2][c2];
        if (!plain(a) || !plain(q) || a.c === q.c) continue;
        const g = b.map((row) => row.slice()); g[r1][c1] = q; g[r2][c2] = a;
        if (!runs(g, r1, c1) && !runs(g, r2, c2)) return [[r1, c1], [r2, c2]];
      }
      return null;
    }
    report.sdlParity = { layout: [] };

    // 5a. HUD 常驻按钮 + 菜单（yu 2026-10-05：新增按钮收进菜单，保住格子大小与 44 px 触控）：
    //     5 种视口 × 第 39 关（8×8，分辨率矩阵同一关）/ 第 49 关（6×9）：格子不小于加按钮之前（33e65bf）——8×8 对照当时 e2e 实测值，
    //     两种盘面都对照测试侧照抄的 33e65bf 布局公式；常驻按钮恰好 撤销 / 提示 / 菜单，每个 ≥ 44×44 CSS px、互不重叠、
    //     不压棋盘、不压提示行与音效芯片、在布局内；点「菜单」打开菜单：14 项齐全、每项 ≥ 44×44 CSS px、互不重叠、在面板内、
    //     道具项显示的次数 = state.boosters；点空白处关闭、再打开按 Esc 关闭
    const BASE_CELL_8x8 = { "竖屏390x844": 44.8, "小屏375x667": 43.0, "横屏1280x800": 89.6, "横屏手机844x390": 43.7, "平板768x1024": 83.3 };
    const baseCell = (W, H, rows, cols) => {   // 33e65bf 的 computeLayout（竖排 TOP 124 + 盘 + 按钮条 64；横排 盘 + 10 + 侧栏 236、与盘等高）
      const VW = cols * 56 + 32, VH = rows * 56 + 32;
      const up = Math.min(W / (VW + 8), H / (124 + VH + 64 + 20)), ul = Math.min(W / (VW + 10 + 236 + 20), H / (VH + 20));
      return Math.min(ul > up * 1.02 ? ul : up, 2) * 56;
    };
    const MENU_IDS = ["hammer", "swap", "cross", "shuffle", "restart", "daily", "map", "prev", "next", "help", "pause", "sfx", "bgm", "close"];
    const VP5A = [{ tag: "竖屏390x844", w: 390, h: 844, dpr: 2 }, { tag: "小屏375x667", w: 375, h: 667, dpr: 2, touch: true, mobile: true },
      { tag: "横屏1280x800", w: 1280, h: 800, dpr: 1 }, { tag: "横屏手机844x390", w: 844, h: 390, dpr: 3, touch: true, mobile: true },
      { tag: "平板768x1024", w: 768, h: 1024, dpr: 2, touch: true, mobile: true }];
    report.menu = { layouts: [] };
    for (const vp of VP5A) for (const li of [38, 48]) {
      const tag = `${vp.tag} 第 ${li + 1} 关`;
      const P = await openPage(vp, li, 7);
      await sleep(200);
      const L = await P.page.evaluate(() => window.m3debug.layout), hud = await P.page.evaluate(() => window.m3debug.hud);
      const base = baseCell(vp.w, vp.h, L.rows, L.cols), fixed = L.rows === 8 && L.cols === 8 ? BASE_CELL_8x8[vp.tag] : null;
      check(`格子不小于加按钮之前（33e65bf）：${tag}`, L.cellCss >= base - 0.01 && (fixed === null || L.cellCss >= fixed - 0.05), { cellCss: L.cellCss, base, fixed });
      const css = (b) => ({ w: b.w * L.u, h: b.h * L.u });
      const ids = L.buttons.map((b) => b.id), pairs = [];
      L.buttons.forEach((a, i) => L.buttons.slice(i + 1).forEach((b) => { if (overlap(a, b)) pairs.push([a.id, b.id]); }));
      const board = { x: L.board.x, y: L.board.y, w: L.VW, h: L.VH };
      const avoid = [...(hud.msg || []).filter((m) => m.text), hud.sfx, hud.bgm].filter(Boolean);
      const hits = L.buttons.flatMap((b) => [...(overlap(b, board) ? [[b.id, "棋盘"]] : []), ...avoid.filter((m) => overlap(b, m)).map((m) => [b.id, m.text ?? "音效芯片"])]);
      const inside = L.buttons.every((b) => b.x >= -0.5 && b.y >= -0.5 && b.x + b.w <= L.w + 0.5 && b.y + b.h <= L.h + 0.5);
      const minTouch = Math.min(...L.buttons.map((b) => Math.min(css(b).w, css(b).h)));
      check(`常驻按钮 = 撤销 / 提示 / 菜单，≥ 44×44 CSS px、互不重叠、不压棋盘 / 提示行 / 芯片、在布局内：${tag}`,
        same(ids, ["undo", "hint", "menu"]) && minTouch >= 44 && !pairs.length && !hits.length && inside, { ids, minTouch, pairs, hits, inside });
      if (li === 38 && (vp.tag === "竖屏390x844" || vp.tag === "横屏1280x800")) await P.shot(`menu-hud-${vp.tag.startsWith("竖") ? "竖屏" : "横屏"}`);
      await P.button("menu"); await sleep(150);
      const st = await P.st(), m = await P.page.evaluate(() => ({ open: window.m3debug.ui.menuOpen, d: window.m3debug.ui.drawn.menu }));
      const items = m.d ? m.d.items : [], ipairs = [];
      items.forEach((a, i) => items.slice(i + 1).forEach((b) => { if (overlap(a, b)) ipairs.push([a.id, b.id]); }));
      const minItem = items.length ? Math.min(...items.map((b) => Math.min(css(b).w, css(b).h))) : 0;
      const inPanel = items.every((b) => b.x >= m.d.panel.x - 0.5 && b.y >= m.d.panel.y - 0.5 && b.x + b.w <= m.d.panel.x + m.d.panel.w + 0.5 && b.y + b.h <= m.d.panel.y + m.d.panel.h + 0.5);
      const counts = Object.fromEntries(items.filter((b) => ["hammer", "swap", "cross"].includes(b.id)).map((b) => [b.id, b.value]));
      const countsOk = same(counts, { hammer: `×${st.boosters.hammer}`, swap: `×${st.boosters.swap}`, cross: `×${st.boosters.cross}` });
      check(`点「菜单」打开菜单：14 项齐全、每项 ≥ 44×44 CSS px、互不重叠、在面板内、道具次数 = state.boosters：${tag}`,
        m.open && same(items.map((b) => b.id), MENU_IDS) && minItem >= 44 && !ipairs.length && inPanel && countsOk, { open: m.open, ids: items.map((b) => b.id), minItem, ipairs, inPanel, counts, boosters: st.boosters });
      if (li === 38 && (vp.tag === "竖屏390x844" || vp.tag === "横屏1280x800")) await P.shot(`menu-open-${vp.tag.startsWith("竖") ? "竖屏" : "横屏"}`);
      // 点面板标题处（不是任何一项）关闭；再打开后 Esc 关闭；关闭后局面没变
      const [hx, hy] = [L.ox + (m.d.panel.x + m.d.panel.w / 2) * L.u, L.oy + (m.d.panel.y + 8) * L.u];
      await P.page.mouse.click(hx, hy); await sleep(80);
      const c1 = await P.page.evaluate(() => window.m3debug.ui.menuOpen);
      await P.button("menu"); await sleep(60);
      const o2 = await P.page.evaluate(() => window.m3debug.ui.menuOpen);
      await press(P, "Escape");
      const c2 = await P.page.evaluate(() => window.m3debug.ui.menuOpen), st2 = await P.st();
      check(`菜单：点空白处关闭、再打开按 Esc 关闭、局面不变：${tag}`, !c1 && o2 && !c2 && same(st2.board, st.board) && st2.moves === st.moves, { c1, o2, c2 });
      report.menu.layouts.push({ vp: vp.tag, level: li + 1, mode: L.mode, cellCss: +L.cellCss.toFixed(1), base: +base.toFixed(1), minButtonCss: +minTouch.toFixed(1), minMenuItemCss: +minItem.toFixed(1) });
      await P.ctx.close();
    }
    // 菜单里每一项都能用（竖屏 / 横屏）：逐项打开菜单 → 点该项 → 核对效果；另核对菜单打开时冻结回放
    for (const vp of VPS) {
      const t = vp.tag;
      const P = await openPage(vp, 3, 7);
      await sleep(150);
      const ui = () => P.page.evaluate(() => window.m3debug.ui);
      const lsGet = (k) => P.page.evaluate((key) => localStorage.getItem(key), k);
      const closeAll = async () => { await press(P, "Escape"); await press(P, "Escape"); await press(P, "Escape"); };
      const res = {};
      let s0 = await P.st();
      await P.button("hammer"); await sleep(40); res.hammer = (await ui()).tool === "hammer"; await closeAll();
      await P.button("swap"); await sleep(40); res.swap = (await ui()).tool === "swap"; await closeAll();
      await P.button("cross"); await sleep(40); res.cross = (await ui()).tool === "cross"; await closeAll();
      await P.button("shuffle"); await P.idle(); let s1 = await P.st(); res.shuffle = !same(s1.board, s0.board) && s1.moves === s0.moves && (await ui()).msg === "已洗牌";
      await P.swap(s1.hint[0], s1.hint[1]); await P.idle();
      await P.button("restart"); await sleep(60); s1 = await P.st(); res.restart = s1.level === 3 && s1.moves === s0.moves && s1.undo === 0;
      await P.button("next"); await sleep(60); res.next = (await P.st()).level === 4;
      await P.button("prev"); await sleep(60); res.prev = (await P.st()).level === 3;
      await P.button("help"); await sleep(60); res.help = (await ui()).showGuide === true; await closeAll();
      await P.button("pause"); await sleep(60); res.pause = (await ui()).paused === true; await closeAll();
      await P.button("map"); await sleep(60); res.map = (await ui()).mapOpen === true; await closeAll();
      const sfx0 = await lsGet("m3-sfx"); await P.button("sfx"); await sleep(40); const sfx1 = await lsGet("m3-sfx");
      res.sfx = sfx0 !== "off" && sfx1 === "off" && (await P.page.evaluate(() => window.m3debug.hud.sfx && true)); await P.button("sfx"); await sleep(40); res.sfx = res.sfx && (await lsGet("m3-sfx")) === "on";
      const bgm0 = await lsGet("m3-bgm"); await P.button("bgm"); await sleep(40); const bgm1 = await lsGet("m3-bgm");
      res.bgm = bgm0 !== "off" && bgm1 === "off"; await P.button("bgm"); await sleep(40); res.bgm = res.bgm && (await lsGet("m3-bgm")) === "on";
      await P.button("close"); await sleep(40); res.close = (await ui()).menuOpen === false;
      await P.button("daily"); await sleep(80); res.daily = (await P.st()).daily === true;
      const label = { hammer: "进入锤子模式", swap: "进入自由交换模式", cross: "进入十字消模式", shuffle: "洗牌（步数不变）", restart: "重开本关", next: "下一关", prev: "上一关",
        help: "本关说明", pause: "暂停与按键说明", map: "选关地图", sfx: "音效开关", bgm: "音乐开关", close: "继续游戏（关菜单）", daily: "每日挑战" };
      for (const id of MENU_IDS) check(`菜单「${id}」可用：${label[id]}：${t}`, res[id] === true, { id, got: res[id] });
      // 冻结：走一步、回放中打开菜单，400 ms 内帧号不变；关菜单后继续播完
      await P.page.evaluate(() => window.m3debug.state); await P.idle();
      const s2 = await P.st();
      await P.swap(s2.hint[0], s2.hint[1]); await sleep(60);
      const [mx, my] = await P.page.evaluate(() => window.m3debug.buttonCenter("menu")); await P.page.mouse.click(mx, my);
      const a1 = await P.info(); await sleep(400); const a2 = await P.info(), busy = await P.page.evaluate(() => window.m3debug.busy);
      await press(P, "Escape"); await P.idle();
      check(`菜单打开时冻结回放（400 ms 内帧号不变）、关闭后播完：${t}`, busy && a1.p === a2.p && a1.fr === a2.fr && (await P.st()).moves === s2.moves - 1, { a1, a2, busy });
      await P.ctx.close();
    }

    for (const vp of VPS) {
      const t = vp.tag;
      // 5b. 道具：锤子（按钮 → 点格）、十字消（已选中一格时按 3 立即使用）、自由交换（换不掉留在模式且不扣次数；换掉扣一次退出）、次数用完
      {
        const P = await openPage(vp, 3, 7);
        await sleep(120);
        const s0 = await P.st();
        check(`道具初始次数 锤子 2 / 自由交换 1 / 十字消 1：${t}`, same(s0.boosters, { hammer: 2, swap: 1, cross: 1 }), s0.boosters);
        await P.button("hammer"); await sleep(60);
        let u = await ui(P);
        check(`菜单 → 锤子：进入锤子模式、棋盘上沿横幅「锤子：点一格」：${t}`, u.tool === "hammer" && u.drawn.banner?.text === "锤子：点一格", { tool: u.tool, banner: u.drawn.banner });
        await P.shot(`sdl-hammer-${t}`);
        const [hx, hy] = await P.center([4, 4]); await P.page.mouse.click(hx, hy); await sleep(30); await P.idle();
        let s1 = await P.st(); u = await ui(P);
        check(`锤子打一格：次数 2 → 1、退出模式、盘面变化：${t}`, s1.boosters.hammer === 1 && u.tool === null && !same(s1.board, s0.board), { boosters: s1.boosters, tool: u.tool, msg: u.msg });
        // 十字消：先点选一格再按 3，立即使用（同桌面 keyCross 的「已选格且有次数」分支）
        const [cx, cy] = await P.center([3, 3]); await P.page.mouse.click(cx, cy); await sleep(40);
        await press(P, "3"); await sleep(30); await P.idle();
        let s2 = await P.st();
        check(`已选中一格时按 3：十字消立即使用（次数 1 → 0）：${t}`, s2.boosters.cross === 0 && !same(s2.board, s1.board), s2.boosters);
        // 自由交换换不掉：留在模式、不扣次数、盘面不变（keepTool）
        const pr = noMatchPair(s2.board) || [[0, 0], [0, 2]];   // 找不到时随便给一对（下面的检查会失败并报出盘面）
        await P.button("swap"); await sleep(40);
        const [ax, ay] = await P.center(pr[0]), [bx, by] = await P.center(pr[1]);
        await P.page.mouse.click(ax, ay); await sleep(30);
        u = await ui(P);
        const firstOk = same(u.swapFirst, pr[0]);
        await P.page.mouse.click(bx, by); await sleep(30); await P.idle();
        let s3 = await P.st(); u = await ui(P);
        check(`自由交换换不掉（不相邻两格）：留在自由交换模式、不扣次数、盘面不变：${t}`, !!pr && firstOk && u.tool === "swap" && s3.boosters.swap === 1 && same(s3.board, s2.board), { pr, tool: u.tool, boosters: s3.boosters, msg: u.msg });
        if (t === "竖屏") await P.shot(`sdl-freeswap-keep-${t}`);
        // 再按提示的两格换：扣一次、退出模式
        await P.page.mouse.click(...(await P.center(s3.hint[0]))); await sleep(30);
        await P.page.mouse.click(...(await P.center(s3.hint[1]))); await sleep(30); await P.idle();
        const s4 = await P.st(); u = await ui(P);
        check(`自由交换换掉：次数 1 → 0、退出模式、盘面变化：${t}`, s4.boosters.swap === 0 && u.tool === null && !same(s4.board, s3.board), { boosters: s4.boosters, tool: u.tool });
        // 次数用完：自由交换拒绝进入；十字消进入模式但提示用完、点格即退出且盘面不变
        await press(P, "2"); u = await ui(P);
        check(`自由交换用完：按 2 不进入模式、提示用完：${t}`, u.tool === null && u.msg === "自由交换用完了", { tool: u.tool, msg: u.msg });
        await press(P, "3"); u = await ui(P);
        const enter = u.tool === "cross" && u.msg === "十字消用完了";
        await P.page.mouse.click(cx, cy); await sleep(40);
        const s5 = await P.st(); u = await ui(P);
        check(`十字消用完：按 3 进入模式并提示用完、点格退出且盘面不变：${t}`, enter && u.tool === null && same(s5.board, s4.board) && !(await P.page.evaluate(() => window.m3debug.busy)), { tool: u.tool, msg: u.msg });
        // 再按一次道具键 = 取消（锤子还剩 1 次）
        await press(P, "1"); const on = (await ui(P)).tool; await press(P, "1"); u = await ui(P);
        check(`再按 1 取消锤子模式：${t}`, on === "hammer" && u.tool === null && u.msg === "已取消锤子", { on, tool: u.tool, msg: u.msg });
        await P.ctx.close();
      }
      // 5c. 手动洗牌（S 键 / 「洗牌」按钮）：轻落动画、步数不变、盘面换了、提示「已洗牌」；分数芯片标签「已洗牌」由核心 m3Badge 决定
      {
        const P = await openPage(vp, 5, 1);
        await sleep(120);
        const s0 = await P.st();
        if (t === "竖屏") await P.button("shuffle"); else await press(P, "s");
        const busy = await P.page.evaluate(() => window.m3debug.anim.kind);
        await P.idle();
        const s1 = await P.st(), u = await ui(P);
        check(`洗牌：播轻落、步数不变、盘面换了、提示「已洗牌」、分数芯片标签「已洗牌」：${t}`,
          busy === "fall" && s1.moves === s0.moves && !same(s1.board, s0.board) && u.msg === "已洗牌" && u.badge.kind === "score" && u.badge.shuffled === true,
          { anim: busy, moves: [s0.moves, s1.moves], msg: u.msg, badge: u.badge });
        await P.shot(`sdl-shuffle-${t}`);
        await P.ctx.close();
      }
      // 5d. 每日挑战：?daily=2026-09-29 开局（同一天盘面相同）、HUD 关名「每日挑战」+ 日期、D 键开今天的挑战
      {
        const P = await openPage(vp, 0, 1);
        await P.page.goto(`http://127.0.0.1:${PORT}/?daily=2026-09-29`);
        await P.page.waitForFunction(() => window.m3debug && window.m3debug.state && window.m3debug.state.daily, null, { timeout: 30000 });
        await sleep(120);
        const s0 = await P.st(), u = await ui(P), hud = await hudOf(P);
        check(`每日挑战 2026-09-29：state.daily、HUD 关名「每日挑战」、关卡标签为日期、窗口标题 = 每日局的 titleLine：${t}`,
          s0.daily === true && hud.name?.text === "每日挑战" && u.daily === "2026-09-29" && hud.label && ws(u.title).startsWith(ws(`${s0.title}  |  `)), { name: hud.name, daily: u.daily, title: u.title });
        await P.shot(`sdl-daily-${t}`);
        await press(P, "r"); await sleep(60);
        const s1 = await P.st();
        check(`每日挑战按 R 重开：仍是每日挑战、步数回到开局：${t}`, s1.daily === true && s1.moves === s0.moves && (await ui(P)).daily === "2026-09-29", { daily: s1.daily, moves: s1.moves });
        const Q = await openPage(vp, 7, 3);
        await Q.page.goto(`http://127.0.0.1:${PORT}/?daily=2026-09-29&seed=99`);
        await Q.page.waitForFunction(() => window.m3debug && window.m3debug.state && window.m3debug.state.daily, null, { timeout: 30000 });
        check(`每日挑战：同一天盘面相同（与种子参数无关）：${t}`, same((await Q.st()).board, s0.board));
        await Q.ctx.close();
        const R = await openPage(vp, 4, 1);
        if (t === "竖屏") await R.button("daily"); else await press(R, "d");
        await sleep(60);
        const ud = await ui(R), now = new Date(), today = `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, "0")}-${String(now.getDate()).padStart(2, "0")}`;
        check(`「每日」按钮 / D 键：开本地今天的每日挑战：${t}`, (await R.st()).daily === true && ud.daily === today, { daily: ud.daily, today });
        await R.ctx.close();
        await P.ctx.close();
      }
      // 5e. 选关地图：M / 「地图」按钮打开；49 个节点、7 个中文章节标签；未解锁节点 → 关地图不换关；已解锁节点 → 跳关；
      //     进度存 localStorage「m3-reached」，刷新后保留
      {
        const P = await openPage(vp, 3, 7);
        await sleep(120);
        if (t === "竖屏") await P.button("map"); else await press(P, "m");
        await sleep(60);
        let u = await ui(P);
        const m = u.drawn.map;
        check(`地图打开：49 个节点（当前第 4 关金色、1–3 已过、5 起未解锁）、7 个章节「第一章…第七章」：${t}`,
          u.mapOpen && m && m.nodes.length === 49 && m.nodes[3].kind === "node_cur" && m.nodes[2].kind === "node_done" && m.nodes[4].kind === "node_lock" &&
          same(m.labels, ["第一章", "第二章", "第三章", "第四章", "第五章", "第六章", "第七章"]), { labels: m?.labels, n: m?.nodes.length, kinds: m?.nodes.slice(0, 6) });
        await P.shot(`sdl-map-${t}`);
        await P.page.mouse.click(...(await P.page.evaluate(() => window.m3debug.mapNodeCenter(9)))); await sleep(60);
        u = await ui(P);
        check(`地图点未解锁的第 10 关：关地图、不换关：${t}`, !u.mapOpen && (await P.st()).level === 3 && /还没解锁/.test(u.msg), { msg: u.msg });
        await press(P, "m");
        await P.page.mouse.click(...(await P.page.evaluate(() => window.m3debug.mapNodeCenter(1)))); await sleep(60);
        u = await ui(P);
        check(`地图点已解锁的第 2 关：跳过去、地图关闭：${t}`, !u.mapOpen && (await P.st()).level === 1, { msg: u.msg, level: (await P.st()).level });
        await press(P, "m"); await press(P, "Escape"); u = await ui(P);
        check(`Esc 关闭地图：${t}`, !u.mapOpen);
        check(`进度写进 localStorage「m3-reached」= 3：${t}`, (await lsGet(P, "m3-reached")) === "3", await lsGet(P, "m3-reached"));
        await P.page.goto(`http://127.0.0.1:${PORT}/?level=0&seed=5`);
        await P.page.waitForFunction(() => window.m3debug && window.m3debug.state, null, { timeout: 30000 });
        await press(P, "m"); u = await ui(P);
        check(`刷新后进度保留：第 4 关仍可进（节点不锁）、第 5 关仍锁：${t}`, u.reached === 3 && u.drawn.map?.nodes[3].kind === "node_done" && u.drawn.map?.nodes[4].kind === "node_lock", { reached: u.reached, kinds: u.drawn.map?.nodes.slice(0, 6) });
        await P.page.mouse.click(5, 5); await sleep(40);
        check(`点地图空白处关闭、不换关：${t}`, !(await ui(P)).mapOpen && (await P.st()).level === 0);
        // HUD 进度点：每关一个点，当前关 C
        const hud = await hudOf(P);
        check(`HUD 关卡进度点 49 个：${t}`, hud.dots && hud.dots.n === 49, hud.dots);
        await P.ctx.close();
      }
      // 5f. 暂停：P / 「暂停」按钮；13 行按键 + 触屏对应、5 颗宝石图例；动画中暂停 = 冻结；R 在暂停中可重开；点任意处继续
      {
        const P = await openPage(vp, 0, 20260929);
        await sleep(120);
        const s0 = await P.st();
        await P.swap(s0.hint[0], s0.hint[1], false);
        await sleep(50);
        if (t === "竖屏") await P.button("pause"); else await press(P, "p");
        const a1 = await P.info(); await sleep(400); const a2 = await P.info();
        let u = await ui(P);
        check(`暂停：13 行按键（含触屏入口）、5 颗宝石图例：${t}`, u.paused && u.drawn.pause?.rows.length === 13 && u.drawn.pause.rows.every((r) => r.touch) && u.drawn.pause.gems.length === 5, u.drawn.pause && { rows: u.drawn.pause.rows.map((r) => r.key).join(""), gems: u.drawn.pause.gems });
        check(`暂停冻结动画（400 ms 内帧号不变）：${t}`, a1.kind !== null && a1.kind === a2.kind && a1.fr === a2.fr && a1.p === a2.p, { a1, a2 });
        await P.shot(`sdl-pause-${t}`);
        await P.page.mouse.click(10, 10); await sleep(40);
        u = await ui(P);
        check(`暂停中点任意处继续：${t}`, !u.paused);
        await P.idle();
        const s1 = await P.st();
        await press(P, "p"); await press(P, "r"); await sleep(60);
        const s2 = await P.st(); u = await ui(P);
        check(`暂停中按 R 重开本关（步数回到开局、取消暂停）：${t}`, !u.paused && s2.moves === s0.moves && s1.moves === s0.moves - 1 && s2.level === 0, { moves: [s0.moves, s1.moves, s2.moves], paused: u.paused });
        await press(P, "p"); await press(P, "Escape");
        check(`Esc 关闭暂停：${t}`, !(await ui(P)).paused);
        await P.ctx.close();
      }
      // 5g. 按键表：H 提示、U 撤销、K / B 音效开关（localStorage）、? 本关说明、Esc 关说明；窗口标题 = titleLine + 提示
      {
        const P = await openPage(vp, 2, 4);
        await sleep(120);
        const s0 = await P.st();
        await press(P, "h"); let u = await ui(P);
        check(`H：亮提示（与核心 hint 相同）：${t}`, same(u.hint, s0.hint) && u.msg === "提示：交换高亮的两格", { hint: u.hint, msg: u.msg });
        await P.swap(s0.hint[0], s0.hint[1], false); await sleep(30); await P.idle();
        await press(P, "u"); const s1 = await P.st();
        check(`U：撤销一步（步数回到开局）：${t}`, s1.moves === s0.moves && same(s1.board, s0.board));
        const sfx0 = await lsGet(P, "m3-sfx"); await press(P, "k"); const sfx1 = await lsGet(P, "m3-sfx"); await press(P, "k");
        const bgm0 = await lsGet(P, "m3-bgm"); await press(P, "b"); const bgm1 = await lsGet(P, "m3-bgm"); await press(P, "b");
        check(`K / B：音效 / BGM 开关（localStorage m3-sfx / m3-bgm）：${t}`, sfx1 === "off" && bgm1 === "off" && (await lsGet(P, "m3-sfx")) === "on" && (await lsGet(P, "m3-bgm")) === "on", { sfx0, sfx1, bgm0, bgm1 });
        await press(P, "?"); const g1 = (await ui(P)).showGuide; await press(P, "Escape"); const g2 = (await ui(P)).showGuide;
        check(`? 打开本关说明、Esc 关闭：${t}`, g1 === true && g2 === false);
        u = await ui(P);
        check(`窗口标题 = Match3.View.titleLine +「  |  」+ 提示：${t}`, ws(u.title) === ws(`${s1.title}  |  ${u.msg}`) && u.title.includes("Hm="), { title: u.title, want: `${s1.title}  |  ${u.msg}` });
        await P.ctx.close();
      }
      // 5h. 首关提示与按键条（桌面 freshLevelUi / drawHelpStripArt）：第 1 关开局自动亮提示 + 横幅「按 H 查看提示」，
      //     有键盘鼠标时棋盘底部按键条 300 帧后消失；触屏设备横幅改成「点「提示」查看提示」、不画按键条
      {
        const P = await openPage(vp, 0, 20260929);
        await sleep(150);
        const s0 = await P.st(); let u = await ui(P);
        check(`第 1 关开局：自动亮提示、横幅「按 H 查看提示」、按键条 H123USDMRKBNP：${t}`, same(u.hint, s0.hint) && u.drawn.banner?.text === "按 H 查看提示" && !!u.drawn.help && u.fine, { hint: u.hint, banner: u.drawn.banner, help: u.drawn.help });
        await P.shot(`sdl-tip-help-${t}`);
        await P.page.waitForFunction(() => window.m3debug.ui.helpFrames === 0 && window.m3debug.ui.tipFrames === 0, null, { timeout: 15000 });
        await sleep(60); u = await ui(P);
        check(`300 帧后按键条与提示横幅消失：${t}`, !u.drawn.help && !u.drawn.banner, u.drawn);
        await P.ctx.close();
        const Q = await openPage({ ...vp, touch: true, mobile: true }, 0, 20260929);
        await sleep(150); const q = await ui(Q);
        check(`触屏设备：横幅「点「提示」查看提示」、不画按键条：${t}`, !q.fine && q.drawn.banner?.text === "点「提示」查看提示" && !q.drawn.help, q.drawn);
        await Q.ctx.close();
      }
    }

    // 5i. 结局后前进与星级（竖屏用点棋盘，横屏用 N）：第 1 关种子 20260930 按提示约 6 步过关（第 3 步有 2 连击）→ 结算面板星级 + 操作提示；
    //     过关后洗牌无效；前进 = 第 2 关（开局步数 = 印制步数，剩余步数最多带入 3 步）；途中抓一次连击，查分数徽章「连击 xN」与本步总结
    for (const vp of VPS) {
      const t = vp.tag;
      const P = await openPage(vp, 0, 20260930);
      await sleep(120);
      let combo = null, summary = null;
      for (let k = 0; k < 40; k++) {
        const s = await P.st();
        if (s.over || !s.hint) break;
        if (!combo) await P.breakAt((i) => i.p === "flash" && i.k >= 2 && i.fr >= 4);
        await P.swap(s.hint[0], s.hint[1], false);
        await P.frozenOrIdle();
        if (!combo && (await P.isFrozen())) {
          combo = { hud: (await hudOf(P)).badge, badge: (await ui(P)).badge, k: (await P.info()).k };
          await P.shot(`sdl-badge-combo-${t}`);
          await P.clearBreak(); await P.resume(); await P.idle();
          await sleep(30);
          summary = { hud: (await hudOf(P)).badge, u: await ui(P) };
        }
        await P.clearBreak(); await P.resume(); await P.idle();
      }
      const end = await P.st(), ov = await P.page.evaluate(() => window.m3debug.overlay), u = await ui(P);
      check(`分数徽章：连击中「连击 xN」、播完后本步总结（m3Badge）：${t}`, !!combo && combo.hud === "combo" && combo.badge.kind === "combo" && combo.badge.n === combo.k &&
        summary.hud === "summary" && summary.u.comboLeft > 0 && summary.u.badge.kind === "summary", { combo, summary: summary && { hud: summary.hud, left: summary.u.comboLeft, badge: summary.u.badge } });
      check(`第 1 关过关：结算面板「过关！」、三颗星贴图、操作提示、说明文字不变：${t}`,
        end.over?.tag === "LevelClear" && ov.title === "过关！" && ov.sub.includes("进入第 2 关") && ov.stars === u.progress.stars && ov.stars >= 1 && ov.starSprites.length === 3 &&
        ov.starSprites.filter((x) => x === "star_on").length === ov.stars && /下一关/.test(ov.action), { over: end.over, ov });
      await P.shot(`sdl-result-stars-${t}`);
      await press(P, "s");
      check(`过关后洗牌无效（盘面不变）：${t}`, same((await P.st()).board, end.board) && !(await P.page.evaluate(() => window.m3debug.busy)));
      check(`过关后进度解锁到第 2 关（m3-reached = 1）：${t}`, (await lsGet(P, "m3-reached")) === "1" && u.reached === 1, { ls: await lsGet(P, "m3-reached"), reached: u.reached });
      if (t === "竖屏") await P.page.mouse.click(...(await P.center([4, 4]))); else await press(P, "n");
      await sleep(80);
      const nx = await P.st(), u2 = await ui(P), carry = Math.min(3, end.moves);
      check(`前进：第 2 关、开局步数 = 印制步数、带入剩余步数 min(3, ${end.moves})：${t}`, nx.level === 1 && !nx.over && nx.moves === u2.startMoves + carry && u2.startMoves > 0, { level: nx.level, moves: nx.moves, startMoves: u2.startMoves, carry });
      await P.ctx.close();
    }
    // 5j. 失败后前进 = 重开本关（第 8 关种子 2 按提示会用完步数）：竖屏点棋盘、横屏按 R
    for (const vp of VPS) {
      const t = vp.tag;
      const P = await openPage(vp, 7, 2);
      const end = await playToEnd(P, 60);
      await sleep(100);
      const ov = await P.page.evaluate(() => window.m3debug.overlay), sm = (await ui(P)).startMoves;
      check(`失败面板：不画星星、操作提示「重试」：${t}`, end.over?.tag === "Lost" && ov.stars === null && /重试/.test(ov.action), { over: end.over, ov });
      await P.shot(`sdl-lost-${t}`);
      if (t === "竖屏") await P.page.mouse.click(...(await P.center([4, 4]))); else await press(P, "n");
      await sleep(80);
      const s = await P.st();
      check(`失败后点棋盘 / N：重开本关（同一关、步数回到开局）：${t}`, s.level === 7 && !s.over && s.moves === sm, { level: s.level, moves: s.moves, sm });
      await P.ctx.close();
    }
    // 5k. 元素展示盘（桌面 MATCH3_SHOWCASE → ?showcase=1）
    for (const vp of VPS) {
      const P = await openPage(vp, 0, 1);
      await P.page.goto(`http://127.0.0.1:${PORT}/?showcase=1`);
      await P.page.waitForFunction(() => window.m3debug && window.m3debug.state && /展示盘/.test(window.m3debug.ui.msg), null, { timeout: 30000 });
      await sleep(150);
      const s = await P.st(), kinds = new Set(s.board.flat().map((c) => (c.t === "custom" ? c.name : c.t + (c.k || ""))));
      check(`元素展示盘：盘上至少 8 种元素、没有几何降级：${vp.tag}`, kinds.size >= 8 && Object.keys(await P.page.evaluate(() => window.m3debug.fallbacks)).length === 0, [...kinds]);
      await P.shot(`sdl-showcase-${vp.tag}`);
      await P.ctx.close();
    }
  }
} finally {
  await browser.close();
  server.kill();
}
report.consoleErrors = logs.filter((l) => l.startsWith("error") || l.startsWith("pageerror"));
check("无控制台错误", report.consoleErrors.length === 0, report.consoleErrors);
report.passed = report.checks.filter((c) => c.ok).length;
report.failed = report.checks.filter((c) => !c.ok).length;
fs.writeFileSync(`${shots}/report.json`, JSON.stringify(report, null, 2));
console.log(JSON.stringify({ passed: report.passed, failed: report.failed, perf: report.perf390x844dpr3, layouts: report.layouts, coldLoadToReadyMs: report.coldLoadToReadyMs,
  failedChecks: report.checks.filter((c) => !c.ok), consoleErrors: report.consoleErrors }, null, 2));
process.exit(report.failed ? 1 : 0);
