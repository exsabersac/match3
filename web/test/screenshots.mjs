// 文档截图（网页版）：起 serve.py → 无头 Chrome 打开 web/dist → 用真实指针按提示走棋 → 用 m3debug 断点冻结关键帧截图。
// 产物（原始帧 + manifest.json）写到 OUT 目录，再由 web/tools/compose_screenshots.py 拼成 docs/images 下的 WebP。
// 用法（先 make build）：make screenshots [E2E_PORT=8831]
//   或：E2E_PORT=8831 NODE_PATH=~/.ghc-wasm/nodejs/lib/node_modules node web/test/screenshots.mjs [OUT]
//       python3 web/tools/compose_screenshots.py [OUT]
// 截图内容：
//   screenshot-l16：第 16 关开局，竖屏 390×844（dpr 2）整页；
//   combo-strip：一次 ≥4 轮的连锁，每轮一行，四列 = 高亮 / 消失 / 下落 / 落定（棋盘区域）；
//   end-of-step-strip：步末各段（巧克力 / 藤蔓蔓延、蜗牛、传送带、倒计时、自动洗牌），每段一行，四列 = 起始 / 1/3 / 2/3 / 末帧。
// 每个场景按「关卡 + 种子」开局、按提示走，命中条件的那一步才截；种子与关卡写在下面的表里，结果可复现。
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
const out = path.resolve(process.argv[2] || "/tmp/match3-doc-shots");
fs.rmSync(out, { recursive: true, force: true });
fs.mkdirSync(out, { recursive: true });
const PORT = Number(process.env.E2E_PORT || 8765);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const portFree = await new Promise((r) => { const t = net.createServer(); t.once("error", () => r(false)); t.listen(PORT, "127.0.0.1", () => t.close(() => r(true))); });
if (!portFree) { console.error(`端口 ${PORT} 已被占用：换一个，例如 E2E_PORT=8799`); process.exit(2); }
const server = spawn("python3", [path.resolve(here, "../serve.py"), "--dir", dist, "--port", String(PORT), "--bind", "127.0.0.1", "--quiet"], { stdio: "ignore" });
for (let i = 0; i < 100; i++) { await sleep(100); try { if ((await fetch(`http://127.0.0.1:${PORT}/index.html`)).ok) break; } catch { /* 未就绪 */ } }
const browser = await chromium.launch({ executablePath: process.env.CHROME || "/usr/bin/google-chrome", headless: true });
const manifest = { portrait: null, combo: null, endStages: [] };

async function open(level, seed, vp = { w: 390, h: 844, dpr: 2 }) {
  const ctx = await browser.newContext({ viewport: { width: vp.w, height: vp.h }, deviceScaleFactor: vp.dpr });
  const page = await ctx.newPage();
  await page.goto(`http://127.0.0.1:${PORT}/?level=${level}&seed=${seed}`);
  await page.waitForFunction(() => window.m3debug && window.m3debug.state && window.m3debug.layout, null, { timeout: 30000 });
  await sleep(300);
  const ev = (f, a) => page.evaluate(f, a);
  const P = {
    ctx, page,
    st: () => ev(() => window.m3debug.state),
    idle: () => page.waitForFunction(() => !window.m3debug.busy, null, { timeout: 30000 }),
    breakAt: (cond) => ev((src) => { window.m3debug.breakWhen = new Function("i", `return (${src})(i)`); }, cond.toString()),
    clearBreak: () => ev(() => { window.m3debug.breakWhen = null; }),
    frozenOrIdle: () => page.waitForFunction(() => window.m3debug.frozen || !window.m3debug.busy, null, { timeout: 30000 }),
    isFrozen: () => ev(() => window.m3debug.frozen),
    resume: () => ev(() => { window.m3debug.frozen = false; }),
    trace: () => ev(() => window.m3debug.pending && window.m3debug.pending.trace),
    async swap([a, b]) {
      const pa = await ev(([r, c]) => window.m3debug.cellCenter(r, c), a), pb = await ev(([r, c]) => window.m3debug.cellCenter(r, c), b);
      await page.mouse.click(pa[0], pa[1]); await page.mouse.click(pb[0], pb[1]);
    },
    // 棋盘区域（CSS 像素）：左上格与右下格中心各向外半格，再留 6 px 边
    async boardClip() {
      const s = await P.st();
      const R = s.board.length, C = s.board[0].length;
      const a = await ev(() => window.m3debug.cellCenter(0, 0)), b = await ev(([r, c]) => window.m3debug.cellCenter(r, c), [R - 1, C - 1]);
      const half = (b[0] - a[0]) / (C - 1) / 2, m = 6;
      return { x: a[0] - half - m, y: a[1] - half - m, width: b[0] - a[0] + 2 * half + 2 * m, height: b[1] - a[1] + 2 * half + 2 * m };
    },
  };
  return P;
}

// 按提示走，直到某一步的回放脚本满足 want(trace)；命中时帧循环冻结在回放第一帧，返回 trace（未命中返回 null）
async function huntMove(P, want, maxSteps) {
  for (let k = 0; k < maxSteps; k++) {
    const s = await P.st();
    if (s.over || !s.hint) return null;
    await P.breakAt((i) => i.kind !== null);
    await P.swap(s.hint);
    await P.frozenOrIdle();
    if (await P.isFrozen()) {
      const tr = await P.trace();
      if (tr && want(tr)) return { tr, step: k + 1 };
    }
    await P.clearBreak(); await P.resume(); await P.idle();
  }
  return null;
}

// 依次冻结在每个断点并截棋盘区域
async function grab(P, conds, prefix) {
  const clip = await P.boardClip(), files = [];
  for (let j = 0; j < conds.length; j++) {
    await P.breakAt(conds[j]); await P.resume(); await P.frozenOrIdle();
    if (!(await P.isFrozen())) throw new Error(`${prefix}：第 ${j} 个断点没有命中`);
    const f = path.join(out, `${prefix}-${j}.png`);
    await P.page.screenshot({ path: f, clip }); files.push(path.basename(f));
  }
  await P.clearBreak(); await P.resume(); await P.idle();
  return files;
}

try {
  // 1. 第 16 关开局（竖屏整页）
  {
    const P = await open(15, 20260929);
    await P.page.screenshot({ path: path.join(out, "l16.png") });
    manifest.portrait = { file: "l16.png", level: 16, seed: 20260929, viewport: "390x844@2" };
    await P.ctx.close();
  }

  // 2. 连锁：找一步 ≥4 轮的连锁，每轮四帧
  {
    let done = false;
    for (const [level, seed] of [[4, 10], [4, 11], [4, 12], [0, 20260929], [0, 7], [1, 3], [2, 5], [4, 13], [3, 1], [5, 2], [0, 11], [0, 12], [0, 13]]) {
      const P = await open(level, seed);
      const hit = await huntMove(P, (tr) => tr.waves.length >= 4, 25);
      if (hit) {
        const W = Math.min(5, hit.tr.waves.length), conds = [];
        for (let k = 1; k <= W; k++) {
          conds.push(new Function("i", `return i.p === "flash" && i.k === ${k} && i.fr >= 8`));
          conds.push(new Function("i", `return i.p === "pop" && i.k === ${k} && i.fr >= 3`));
          conds.push(new Function("i", `return i.p === "fall" && i.k === ${k} && i.fr >= Math.floor(i.n / 2)`));
          conds.push(new Function("i", `return i.p === "rest" && i.k === ${k} && i.fr >= 1`));
        }
        const files = await grab(P, conds.map((f) => f.toString().replace(/^function anonymous\(i\s*\)\s*\{\s*/, "(i) => {")), "combo");
        manifest.combo = { level: level + 1, seed, step: hit.step, waves: hit.tr.waves.length, rows: W, files };
        done = true;
      }
      await P.ctx.close();
      if (done) break;
    }
    if (!done) throw new Error("没有找到 ≥4 轮的连锁");
  }

  // 3. 步末各段：每段一行（起始 / 1/3 / 2/3 / 末帧）
  const scenes = [
    { label: "巧克力蔓延", stage: "spread", want: (tr) => tr.end.some((e) => e.effect.type === "spread" && e.effect.kind === "choco"), tries: [[4, 1], [4, 2], [4, 3], [6, 1]] },
    { label: "藤蔓蔓延", stage: "spread", want: (tr) => tr.end.some((e) => e.effect.type === "spread" && e.effect.kind === "vine"), tries: [[9, 1], [9, 2], [15, 1], [9, 3]] },
    { label: "蜗牛爬行", stage: "snail", want: (tr) => tr.end.some((e) => e.effect.type === "snail"), tries: [[27, 1], [28, 1], [27, 2]] },
    { label: "传送带移位", stage: "belt", want: (tr) => tr.end.some((e) => e.effect.type === "belt"), tries: [[13, 1], [7, 1], [13, 2]] },
    { label: "倒计时减一", stage: "tick", want: (tr) => tr.end.some((e) => e.effect.type === "tick"), tries: [[11, 1], [11, 2], [11, 3]] },
    { label: "自动洗牌", stage: "shuffle", want: (tr) => !!tr.shuffle, tries: [[35, 1], [35, 2], [35, 3], [35, 4], [35, 5], [35, 6]] },
  ];
  for (const sc of scenes) {
    let got = null;
    for (const [level, seed] of sc.tries) {
      const P = await open(level, seed);
      const hit = await huntMove(P, sc.want, 30);
      if (hit) {
        const st = JSON.stringify(sc.stage);
        const conds = [
          `(i) => i.p === "end" && i.stage === ${st} && i.fr >= 1`,
          `(i) => i.p === "end" && i.stage === ${st} && i.fr >= Math.round(i.n / 3)`,
          `(i) => i.p === "end" && i.stage === ${st} && i.fr >= Math.round(2 * i.n / 3)`,
          `(i) => i.p === "end" && i.stage === ${st} && i.fr >= i.n - 1`,
        ];
        try { got = { label: sc.label, level: level + 1, seed, step: hit.step, files: await grab(P, conds, `end-${sc.stage}-${manifest.endStages.length}`) }; } catch (e) { console.error(String(e)); }
      }
      await P.ctx.close();
      if (got) break;
    }
    if (got) manifest.endStages.push(got); else console.error(`未截到：${sc.label}`);
  }
} finally {
  fs.writeFileSync(path.join(out, "manifest.json"), JSON.stringify(manifest, null, 2));
  await browser.close();
  server.kill();
}
console.log(JSON.stringify(manifest, null, 2));
