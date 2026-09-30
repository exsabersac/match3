// 安卓壳网页层检查（没有模拟器时的替代验证）：用桌面 Chrome 以手机视口打开 web/android-app/www
// （即打进 APK 的 assets/public：web/dist + android-shim.js），模拟 Capacitor 的 App 插件，检查：
//   1. wasm 以 application/wasm 提供时走 instantiateStreaming（不触发兜底），游戏能开局；
//   2. 触摸点按 / 拖划交换若干步，步数减少，截图；
//   3. 返回键：触发 backButton → 弹「退出消消乐？」确认框 → 确定后调用 App.exitApp；
//   4. wasm 故意以 application/octet-stream 提供时，android-shim.js 的 arrayBuffer 兜底生效，游戏照常开局；
//   5. viewport 禁止缩放。
// 用法（先 build-apk.sh sync 或 node prepare-www.mjs 生成 www/）：
//   NODE_PATH=~/.ghc-wasm/nodejs/lib/node_modules node web/android-app/test/check-www.mjs [截图目录]
// 需要 playwright-core（ghc-wasm-meta 自带）与本机 Chrome（CHROME=路径 可覆盖）。
import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const { chromium } = require("playwright-core");
const here = path.dirname(fileURLToPath(import.meta.url));
const www = path.resolve(here, "../www");
const shots = path.resolve(process.argv[2] || "/workspace/match3-android-shots");
fs.mkdirSync(shots, { recursive: true });
if (!fs.existsSync(path.join(www, "android-shim.js"))) { console.error("没有 www/android-shim.js：先运行 node prepare-www.mjs"); process.exit(1); }

const TYPES = { ".html": "text/html; charset=utf-8", ".js": "text/javascript", ".json": "application/json", ".webp": "image/webp", ".wasm": "application/wasm" };
function serve(port, wasmType) {
  const srv = http.createServer((req, res) => {
    const p = path.join(www, decodeURIComponent(new URL(req.url, "http://x").pathname).replace(/\/$/, "/index.html"));
    if (!p.startsWith(www) || !fs.existsSync(p)) { res.writeHead(404); res.end(); return; }
    const ext = path.extname(p);
    res.writeHead(200, { "content-type": ext === ".wasm" ? wasmType : (TYPES[ext] || "application/octet-stream") });
    fs.createReadStream(p).pipe(res);
  });
  // 端口 0：由系统分配空闲端口
  return new Promise((r) => srv.listen(port, "127.0.0.1", () => r(srv)));
}

// Pixel 6 近似：412×915 CSS 像素，dpr 2.625，Android Chrome WebView UA
const PHONE = { viewport: { width: 412, height: 915 }, deviceScaleFactor: 2.625, isMobile: true, hasTouch: true,
  userAgent: "Mozilla/5.0 (Linux; Android 14; Pixel 6 Build/UQ1A; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/129.0.0.0 Mobile Safari/537.36" };
// 在页面脚本之前注入 Capacitor 的 App 插件桩（真机上由 Capacitor 原生桥提供）
const CAP_STUB = () => {
  window.__back = null; window.__exited = false;
  window.Capacitor = { isNativePlatform: () => true, getPlatform: () => "android",
    Plugins: { App: { addListener(ev, cb) { if (ev === "backButton") window.__back = cb; return Promise.resolve({ remove() {} }); },
                      exitApp() { window.__exited = true; } } } };
};

const checks = [];
const check = (name, ok, detail) => { checks.push({ name, ok: !!ok, detail }); console.log(`${ok ? "通过" : "失败"}  ${name}${detail !== undefined ? "  " + JSON.stringify(detail) : ""}`); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const browser = await chromium.launch({ executablePath: process.env.CHROME || "/usr/bin/google-chrome", headless: true });
async function open(port, name) {
  const ctx = await browser.newContext(PHONE);
  const page = await ctx.newPage();
  const logs = [];
  page.on("console", (m) => logs.push(`${m.type()}: ${m.text()}`));
  page.on("pageerror", (e) => logs.push(`pageerror: ${e.message}`));
  await page.addInitScript(CAP_STUB);
  const t0 = Date.now();
  await page.goto(`http://127.0.0.1:${port}/`);
  await page.waitForFunction(() => window.m3debug && window.m3debug.state, null, { timeout: 30000 });
  return { ctx, page, logs, loadMs: Date.now() - t0, name };
}
const idle = (page) => page.waitForFunction(() => !window.m3debug.busy, null, { timeout: 30000 });

const s1 = await serve(0, "application/wasm");
const s2 = await serve(0, "application/octet-stream");
const port1 = s1.address().port, port2 = s2.address().port;
try {
  // ---- 1 + 2 + 3 + 5：正常 MIME ----
  {
    const { ctx, page, logs, loadMs } = await open(port1);
    const fb = await page.evaluate(() => window.__m3WasmFallback ?? null);
    check("application/wasm：流式实例化成功（未走兜底）", fb === null, { loadMs });
    const vp = await page.evaluate(() => document.querySelector('meta[name=viewport]').content);
    check("viewport 禁止缩放", /user-scalable=no/.test(vp) && /maximum-scale=1/.test(vp), vp);
    await page.screenshot({ path: `${shots}/01-开局-412x915.png` });
    const m0 = (await page.evaluate(() => window.m3debug.state)).moves;
    for (let k = 0; k < 3; k++) {
      const s = await page.evaluate(() => window.m3debug.state);
      if (s.over || !s.hint) break;
      const [a, b] = s.hint;
      const pa = await page.evaluate(([r, c]) => window.m3debug.cellCenter(r, c), a);
      const pb = await page.evaluate(([r, c]) => window.m3debug.cellCenter(r, c), b);
      if (k % 2 === 0) { await page.touchscreen.tap(pa[0], pa[1]); await sleep(60); await page.touchscreen.tap(pb[0], pb[1]); }
      else {   // 拖划：用 CDP 发触摸序列（pointer 事件由触摸合成）
        const cdp = await ctx.newCDPSession(page);
        const pt = (p) => [{ x: p[0], y: p[1] }];
        await cdp.send("Input.dispatchTouchEvent", { type: "touchStart", touchPoints: pt(pa) });
        for (let i = 1; i <= 6; i++) await cdp.send("Input.dispatchTouchEvent", { type: "touchMove", touchPoints: pt([pa[0] + (pb[0] - pa[0]) * i / 6, pa[1] + (pb[1] - pa[1]) * i / 6]) });
        await cdp.send("Input.dispatchTouchEvent", { type: "touchEnd", touchPoints: [] });
      }
      await sleep(250);
      await page.screenshot({ path: `${shots}/0${2 + k}-第${k + 1}步-动画中.png` });
      await idle(page);
    }
    const st = await page.evaluate(() => window.m3debug.state);
    check("触摸点按 / 拖划 3 步后步数减少 3", m0 - st.moves === 3, { before: m0, after: st.moves, score: st.score });
    await page.screenshot({ path: `${shots}/05-三步后.png` });
    // 返回键
    let dialogMsg = null;
    page.once("dialog", (d) => { dialogMsg = d.message(); d.accept(); });
    const hasBack = await page.evaluate(() => typeof window.__back === "function");
    await page.evaluate(() => window.__back && window.__back({ canGoBack: false }));
    await sleep(100);
    const exited = await page.evaluate(() => window.__exited);
    check("返回键：弹确认框，确定后 exitApp", hasBack && dialogMsg === "退出消消乐？" && exited, { hasBack, dialogMsg, exited });
    const errs = logs.filter((l) => /^(error|pageerror)/.test(l));
    check("控制台无错误", errs.length === 0, errs);
    await ctx.close();
  }
  // ---- 4：错误 MIME → 兜底 ----
  {
    const { ctx, page, logs, loadMs } = await open(port2);
    const fb = await page.evaluate(() => window.__m3WasmFallback ?? null);
    const st = await page.evaluate(() => window.m3debug.state);
    check("application/octet-stream：android-shim 兜底为 arrayBuffer 实例化，游戏正常开局", fb !== null && st && st.board.length > 0, { fallbackReason: fb, loadMs });
    await page.screenshot({ path: `${shots}/06-错误MIME兜底后开局.png` });
    await ctx.close();
  }
} finally {
  await browser.close(); s1.close(); s2.close();
}
fs.writeFileSync(`${shots}/check-www-report.json`, JSON.stringify({ checks }, null, 2));
const bad = checks.filter((c) => !c.ok);
console.log(bad.length ? `${bad.length} 项失败` : `全部 ${checks.length} 项通过；截图在 ${shots}`);
process.exit(bad.length ? 1 : 0);
