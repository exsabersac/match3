// 无头浏览器端到端测试：起静态服务器 → 打开页面 → 用真实指针（点选 / 拖划 / 按钮）操作 → 关键时刻逐帧截图 + 报告。
// 用法（先 ./build.sh）：
//   NODE_PATH=~/.ghc-wasm/nodejs/lib/node_modules node web/test/e2e.mjs [截图目录]
// 需要 playwright-core（ghc-wasm-meta 自带）和本机 Chrome/Chromium（CHROME=路径 可覆盖）。
//
// 关键时刻截图用页面的调试断点：m3debug.breakWhen(info) 为真时帧循环冻结（画面停在那一帧），截图后解冻继续。
// 覆盖：
//   1. 主流程（390×844 dpr3）：无效交换退回不扣步、按提示走到结局（点选 / 拖划交替）、连锁中「高亮 / 消失 / 下落」三帧、撤销按钮；
//   2. 特殊块爆炸（直线消除）的「消失」帧；步末效果（皮带 / 蜗牛 / 蔓延 / 倒计时）中间帧；
//   3. 第 39 关果冻、第 40 关气泡：静止 + 连锁中；
//   4. 分辨率矩阵：7 种视口截图，并检查布局完整落在视口 / 安全区内、格子与按钮的 CSS 尺寸；
//   5. 动画进行中改变视口大小：不重置对局与动画，播完后状态正确。
import { spawn } from "node:child_process";
import { createRequire } from "node:module";
import path from "node:path";
import fs from "node:fs";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const { chromium } = require("playwright-core");
const here = path.dirname(fileURLToPath(import.meta.url));
const dist = path.resolve(here, "../dist");
const shots = path.resolve(process.argv[2] || "/workspace/match3-web-shots");
fs.rmSync(shots, { recursive: true, force: true });
fs.mkdirSync(shots, { recursive: true });
const PORT = 8765;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const server = spawn("python3", ["-m", "http.server", String(PORT), "--bind", "127.0.0.1"], { cwd: dist, stdio: "ignore" });
await sleep(800);
const browser = await chromium.launch({ executablePath: process.env.CHROME || "/usr/bin/google-chrome", headless: true });
const report = { shots: [], checks: [] };
const logs = [];
const check = (name, ok, detail) => { report.checks.push({ name, ok: !!ok, ...(detail !== undefined ? { detail } : {}) }); if (!ok) console.error("FAIL", name, detail ?? ""); };

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
