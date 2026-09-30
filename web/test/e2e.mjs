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

async function openPage(vp, level, seed) {
  const ctx = await browser.newContext({ viewport: { width: vp.w, height: vp.h }, deviceScaleFactor: vp.dpr, hasTouch: !!vp.touch, isMobile: !!vp.mobile });
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
  async function button(id) { const [x, y] = await page.evaluate((i) => window.m3debug.buttonCenter(i), id); await page.mouse.click(x, y); }
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
    const bad = [], badGoal = [];
    report.fallbacksByLevel = [];
    report.goalLabels = [];
    for (let li = 0; li < nLevels; li++) {
      await P.page.goto(`http://127.0.0.1:${PORT}/?level=${li}&seed=7`);
      await P.page.waitForFunction(() => window.m3debug && window.m3debug.state, null, { timeout: 30000 });
      await sleep(60);
      // HUD 目标标签：画出来的文字 = 「目标 」+ 核心给的中文名 goal.label，且不含内部名（[a-z_] 标识符，如 fuzzball / GoalNamed）
      const s0 = await P.st(), hudGoal = await P.page.evaluate(() => window.m3debug.hud?.goal ?? null);
      report.goalLabels.push({ level: li + 1, kind: s0.goal.kind, label: s0.goal.label, hud: hudGoal });
      if (typeof s0.goal.label !== "string" || /[a-z_]/.test(s0.goal.label) || hudGoal !== `目标 ${s0.goal.label}`) {
        badGoal.push({ level: li + 1, goal: s0.goal, hud: hudGoal });
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
    check(`全部 ${nLevels} 关开局 + 走 3 步：没有格子走几何降级（m3debug.fallbacks 为空）`, nLevels >= 46 && bad.length === 0, bad);
    for (const lv of [43, 44, 45, 46]) {
      const row = report.fallbacksByLevel.find((x) => x.level === lv);
      check(`第 ${lv} 关 m3debug.fallbacks 为空`, !!row && Object.keys(row.fallbacks).length === 0, row);
    }
    check(`全部 ${nLevels} 关 HUD 目标标签是中文显示名（无 [a-z_] 内部名，= 「目标 」+ state.goal.label）`, nLevels >= 46 && badGoal.length === 0, badGoal);
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
  //     其它关卡 drops 为空；全程不走几何降级
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
