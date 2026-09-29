// 逐轮回放的一致性脚本（wasm 一侧，node 直接加载 dist/ 的 wasm）：走法与 AnimParity.hs 相同——
// 按 state.hint 连走 N 步，每步 m3AnimStart 后逐帧 m3AnimTick 到播完（每第 3 步从第 5 帧起加速），
// 打印开播 JSON、每帧 JSON 与 "frames=帧数 events=事件数"。另把每帧 m3AnimTick 耗时与 JSON 字节数写到 stderr。
// 用法：node web/test/node-anim-parity.mjs 12 42 20   （先 ./build.sh）
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
const dist = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../dist");
const { WASI, OpenFile, File, ConsoleStdout } = await import(path.join(dist, "vendor/browser_wasi_shim/index.js"));
const makeJsffi = (await import(path.join(dist, "ghc_wasm_jsffi.js"))).default;

const [li, seed, n] = process.argv.slice(2).map(Number);
const wasi = new WASI([], [], [new OpenFile(new File([])), ConsoleStdout.lineBuffered(console.error), ConsoleStdout.lineBuffered(console.error)], { debug: false });
const ex = {};
const { instance } = await WebAssembly.instantiate(fs.readFileSync(path.join(dist, "match3-web.wasm")), {
  ghc_wasm_jsffi: makeJsffi(ex), wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(ex, instance.exports);
wasi.initialize(instance);
const X = instance.exports;
let s = JSON.parse(X.m3New(li, seed)).state;
const out = [];
let tickMs = 0, ticks = 0, maxBytes = 0, startBytes = 0;
for (let k = 0; k < n; k++) {
  if (s.over || !s.hint) break;
  const [[r1, c1], [r2, c2]] = s.hint;
  const res = JSON.parse(X.m3Swap(r1, c1, r2, c2));
  s = res.state;
  out.push(`step ${k}`);
  const j0 = X.m3AnimStart();
  if (!JSON.parse(j0).anim) { out.push("noanim"); continue; }
  out.push(j0);
  startBytes = Math.max(startBytes, j0.length);
  const fastStep = k % 3 === 2;
  let fr = 0, evs = 0;
  for (;;) {
    const a = performance.now();
    const j = X.m3AnimTick(fastStep && fr >= 5 ? 1 : 0);
    tickMs += performance.now() - a; ticks++;
    maxBytes = Math.max(maxBytes, j.length);
    out.push(j);
    fr++;
    const t = JSON.parse(j);
    if (t.ev) evs += t.ev.length;
    if (t.done) break;
  }
  out.push(`frames=${fr} events=${evs}`);
}
console.log(out.join("\n"));
console.error(`node: m3AnimTick ${ticks} 帧，平均 ${(tickMs / Math.max(1, ticks)).toFixed(3)} ms/帧，单帧 JSON 最大 ${maxBytes} 字节；m3AnimStart 最大 ${startBytes} 字节`);
