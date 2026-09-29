// 无头浏览器冒烟测试：起静态服务器 → 打开页面 → 用真实鼠标点击/拖拽完成若干次交换 → 截图 + 性能数据。
// 用法（先 ./build.sh）：
//   NODE_PATH=~/.ghc-wasm/nodejs/lib/node_modules node web/test/e2e.mjs [截图目录]
// 需要 playwright-core（ghc-wasm-meta 自带一份）和本机 Chrome/Chromium（CHROME=路径 可覆盖）。
// 交换对象取自核心 findHint 给出的提示（state.hint），操作本身走真实鼠标事件。
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
fs.mkdirSync(shots, { recursive: true });
const PORT = 8765;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const server = spawn("python3", ["-m", "http.server", String(PORT), "--bind", "127.0.0.1"], { cwd: dist, stdio: "ignore" });
await sleep(800);
const browser = await chromium.launch({ executablePath: process.env.CHROME || "/usr/bin/google-chrome", headless: true });
const report = {};
const allSteps = [];   // 跨局汇总每步耗时（页面重载会清空页面里的 perf）
try {
  const ctx = await browser.newContext({ viewport: { width: 520, height: 760 }, deviceScaleFactor: 2 });
  const page = await ctx.newPage();
  const logs = [];
  page.on("console", (m) => logs.push(`${m.type()}: ${m.text()}`));
  page.on("pageerror", (e) => logs.push(`pageerror: ${e.message}`));
  const open = async (seed) => {
    const t0 = Date.now();
    await page.goto(`http://127.0.0.1:${PORT}/?level=0&seed=${seed}`);
    await page.waitForFunction(() => window.m3debug && window.m3debug.state, null, { timeout: 30000 });
    return Date.now() - t0;
  };

  // 首次加载（全新浏览器上下文，无缓存）
  let seed = 20260929;
  report.coldLoadToReadyMs = await open(seed);
  report.coldPagePerf = await page.evaluate(() => ({ ...window.m3debug.perf, steps: undefined }));
  await page.screenshot({ path: `${shots}/01-开局.png` });

  const box = await page.locator("#board").boundingBox();
  const CELL = await page.evaluate(() => window.m3debug.CELL);
  const pt = ([r, c]) => ({ x: box.x + (c + 0.5) * CELL, y: box.y + (r + 0.5) * CELL });
  const st = () => page.evaluate(() => window.m3debug.state);
  const lastStep = () => page.evaluate(() => window.m3debug.perf.steps.at(-1));

  // 交换方式：点选两格，或按住拖到相邻格松开
  async function swap(a, b, drag) {
    const pa = pt(a), pb = pt(b);
    if (drag) { await page.mouse.move(pa.x, pa.y); await page.mouse.down(); await page.mouse.move(pb.x, pb.y, { steps: 5 }); await page.mouse.up(); }
    else { await page.mouse.click(pa.x, pa.y); await page.mouse.click(pb.x, pb.y); }
    await sleep(30);
    await page.waitForFunction(() => !window.m3debug.busy, null, { timeout: 30000 });
    const s = await lastStep();
    allSteps.push({ seed, ...s });
    return s;
  }

  // 1) 「换了不能消」：依次试几对相邻格，直到核心返回 NoMatch，验证不扣步、盘面不变
  const probes = [[[0, 0], [1, 0]], [[0, 1], [1, 1]], [[7, 7], [7, 6]], [[3, 3], [3, 4]], [[5, 2], [6, 2]], [[2, 6], [2, 7]]];
  report.probeSwaps = [];
  for (const [a, b] of probes) {
    const before = await st();
    const s = await swap(a, b, false);
    const after = await st();
    report.probeSwaps.push({ a, b, outcome: s.outcome, movesBefore: before.moves, movesAfter: after.moves,
      boardUnchanged: JSON.stringify(before.board) === JSON.stringify(after.board) });
    if (s.outcome === "NoMatch") { await page.screenshot({ path: `${shots}/01b-无效交换被退回.png` }); break; }
  }

  // 2) 按核心提示走有效交换直到关卡结束；这局没出现连锁就换种子再开，最多 12 局
  let n = 0, games = 1, chainShot = false;
  report.games = [];
  for (;;) {
    const s = await st();
    if (s.over || !s.hint) {
      report.games.push({ seed, over: s.over, score: s.score, movesLeft: s.moves });
      if (games === 1) await page.screenshot({ path: `${shots}/05-结局.png` });
      if (chainShot || games >= 12) break;
      seed++; games++;
      await open(seed);
      continue;
    }
    const step = await swap(s.hint[0], s.hint[1], n % 2 === 1);
    n++;
    if (n === 1) await page.screenshot({ path: `${shots}/02-第一次交换后.png` });
    if (n === 5) await page.screenshot({ path: `${shots}/04-五步后.png` });
    if (step.waves > 1 && !chainShot) {
      chainShot = true;
      report.chain = { seed, step };
      await page.screenshot({ path: `${shots}/03-连锁后.png` });
    }
  }

  const applied = allSteps.filter((x) => x.outcome !== "NoMatch" && x.outcome !== "InvalidSwap");
  const stat = (k) => { const v = applied.map((x) => x[k]).sort((a, b) => a - b); return { min: v[0], median: v[Math.floor(v.length / 2)], max: v.at(-1) }; };
  report.swaps = {
    total: allSteps.length, applied: applied.length, chains: applied.filter((x) => x.waves > 1).length,
    maxWaves: Math.max(...applied.map((x) => x.waves)),
    totalMs: stat("ms"), wasmMs: stat("wasmMs"), parseMs: stat("parseMs"), jsonBytes: stat("jsonBytes"),
  };

  // 3) 二次加载（HTTP 缓存 / 编译缓存可能生效）
  report.warmReloadToReadyMs = await open(seed);
  report.warmPagePerf = await page.evaluate(() => ({ ...window.m3debug.perf, steps: undefined }));
  report.consoleErrors = logs.filter((l) => l.startsWith("error") || l.startsWith("pageerror"));
} finally {
  await browser.close();
  server.kill();
}
console.log(JSON.stringify(report, null, 2));
fs.writeFileSync(`${shots}/report.json`, JSON.stringify(report, null, 2));
