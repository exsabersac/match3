// wasm 一侧的一致性脚本（node 直接加载 dist/ 里的 wasm，不经浏览器）：
// 与 Parity.hs 相同的走法：开局后按 state.hint 连走 N 步，逐步打印 JSON，最后撤销一步；另把每步耗时写到 stderr。
// 用法：node web/test/node-parity.mjs 0 20260929 12   （先 ./build.sh）
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
const dist = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../dist");
const { WASI, OpenFile, File, ConsoleStdout } = await import(path.join(dist, "vendor/browser_wasi_shim/index.js"));
const makeJsffi = (await import(path.join(dist, "ghc_wasm_jsffi.js"))).default;

const [li, seed, n] = process.argv.slice(2).map(Number);
const t0 = performance.now();
const wasi = new WASI([], [], [new OpenFile(new File([])), ConsoleStdout.lineBuffered(console.error), ConsoleStdout.lineBuffered(console.error)], { debug: false });
const ex = {};
const { instance } = await WebAssembly.instantiate(fs.readFileSync(path.join(dist, "match3-web.wasm")), {
  ghc_wasm_jsffi: makeJsffi(ex), wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(ex, instance.exports);
wasi.initialize(instance);
const t1 = performance.now();
let j = instance.exports.m3New(li, seed);
console.log(j);
const times = [];
for (let k = 0; k < n; k++) {
  const s = JSON.parse(j).state;
  if (s.over || !s.hint) break;   // 与 Parity.hs 一致：走完（或提前结束）后再撤销一步
  const [[r1, c1], [r2, c2]] = s.hint;
  const a = performance.now();
  j = instance.exports.m3Swap(r1, c1, r2, c2);
  times.push(performance.now() - a);
  console.log(j);
}
console.log(instance.exports.m3Undo());
console.error(`node: 实例化+初始化 ${(t1 - t0).toFixed(1)} ms；m3Swap 每步 ms：${times.map((t) => t.toFixed(1)).join(" ")}`);
