// wasm 一侧的一致性脚本（node 直接加载 dist/ 里的 wasm，不经浏览器）：
// 与 Parity.hs 相同的走法：开局后按 state.hint 连走 N 步，逐步打印 JSON，最后撤销一步；另把每步耗时写到 stderr。
// 用法：node web/test/node-parity.mjs 0 20260929 12 [hint|combo|combo-bomb|cham-rainbow|fix-RCRC-…|boost|daily|advance]   （先 ./build.sh；第 4 个参数是走法，见 pickMove）
// boost / daily / advance（桌面迁来的接口）同 Parity.hs：boost 前四步锤子 / 十字消 / 自由交换 / 洗牌；daily 开 2026-09-29 每日挑战；
// advance 按提示走到结局（最后不撤销）；三者走完后再打印 extras。
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
const dist = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../dist");
const { WASI, OpenFile, File, ConsoleStdout } = await import(path.join(dist, "vendor/browser_wasi_shim/index.js"));
const makeJsffi = (await import(path.join(dist, "ghc_wasm_jsffi.js"))).default;

const [li, seed, n] = process.argv.slice(2, 5).map(Number);
const mode = process.argv[5] || "hint";
// 走法（第 4 个参数，缺省 hint）：与原生侧 pickMove 逐条相同——combo 时盘上有「彩虹 × 直线 / 炸弹」相邻（两格都无冰、无叠层）
// 就先换这一对（行优先，先右后下），否则按 state.hint。提示不会主动选彩虹组合，第 44 关的变身步靠它覆盖；
// combo-bomb 同 combo，但先换「彩虹 × 炸弹」（覆盖 rainbow_bomb 变身）。
// 变色龙 × 彩虹（第 47 关，成对交换规则 15）：盘上相邻的「彩虹（无冰无叠层）× 变色龙」，行优先、先右后下（同原生侧 chamRainbowPairs）
function chamRainbowPairs(b) {
  const rainbow = (x) => x.t === "G" && x.i === 0 && x.o === null && x.k === "R", cham = (x) => x.t === "custom" && x.name === "chameleon";
  const pairs = [];
  for (let r = 0; r < b.length; r++) for (let c = 0; c < b[r].length; c++) {
    for (const [qr, qc] of [[r, c + 1], [r + 1, c]]) {
      if (qr >= b.length || qc >= b[r].length) continue;
      const x = b[r][c], y = b[qr][qc];
      if ((rainbow(x) && cham(y)) || (cham(x) && rainbow(y))) pairs.push([[r, c], [qr, qc]]);
    }
  }
  return pairs;
}
// 走法 fix-RCRC-RCRC-…（第 48 关魔法地格的固定用例）：第 k 步（0 起）换第 k 对（每对 4 个数字 r1 c1 r2 c2），列表用完后按提示（同原生侧 fixedMove）
function fixedMove(mode, k) {
  const w = mode.split("-");
  if (w[0] !== "fix" || k + 1 >= w.length) return null;
  const d = [...w[k + 1]].map(Number);
  return [[d[0], d[1]], [d[2], d[3]]];
}
// 魔法地格扩爆（第 48 关）：本步 events 里 blast 的来源格在魔法地格（换之前 state.ground 的 magic）上的，每个来源一行
// 「元素@(r,c) N 格 R 行 C 列」（同原生侧 magicBlasts）
function magicBlasts(s, events) {
  const magic = new Set((s.ground || []).filter((g) => g.name === "magic").map((g) => g.p.join(",")));
  const out = [];
  for (const e of events || []) {
    if (e.kind !== "blast") continue;
    for (const src of [...new Set(e.pairs.map((p) => p[0].join(",")))]) {
      if (!magic.has(src)) continue;
      const ts = [...new Set(e.pairs.filter((p) => p[0].join(",") === src).map((p) => p[1].join(",")))].map((q) => q.split(","));
      out.push(`${e.subject}@(${src}) ${ts.length} 格 ${new Set(ts.map((q) => q[0])).size} 行 ${new Set(ts.map((q) => q[1])).size} 列`);
    }
  }
  return out;
}
// cham-rainbow：先换第一对「彩虹 × 变色龙」（提示不会主动选它），否则按 state.hint
function pickMove(mode, s) {
  if (mode === "cham-rainbow") { const pr = chamRainbowPairs(s.board); return pr.length ? pr[0] : s.hint; }
  if (mode !== "combo" && mode !== "combo-bomb") return s.hint;
  const b = s.board, plain = (x) => x.t === "G" && x.i === 0 && x.o === null;
  const rainbow = (x) => plain(x) && x.k === "R", special = (x) => plain(x) && (x.k === "H" || x.k === "V" || x.k === "B");
  const pairs = [];
  for (let r = 0; r < b.length; r++) for (let c = 0; c < b[r].length; c++) {
    for (const [qr, qc] of [[r, c + 1], [r + 1, c]]) {
      if (qr >= b.length || qc >= b[r].length) continue;
      const x = b[r][c], y = b[qr][qc];
      if ((rainbow(x) && special(y)) || (special(x) && rainbow(y))) pairs.push([[r, c], [qr, qc]]);
    }
  }
  const hasBomb = ([p, q]) => b[p[0]][p[1]].k === "B" || b[q[0]][q[1]].k === "B";
  const ordered = mode === "combo" ? pairs : [...pairs.filter(hasBomb), ...pairs.filter((pr) => !hasBomb(pr))];
  return ordered.length ? ordered[0] : s.hint;
}

const t0 = performance.now();
const wasi = new WASI([], [], [new OpenFile(new File([])), ConsoleStdout.lineBuffered(console.error), ConsoleStdout.lineBuffered(console.error)], { debug: false });
const ex = {};
const { instance } = await WebAssembly.instantiate(fs.readFileSync(path.join(dist, "match3-web.wasm")), {
  ghc_wasm_jsffi: makeJsffi(ex), wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(ex, instance.exports);
wasi.initialize(instance);
const t1 = performance.now();
const X = instance.exports;
let j = mode === "daily" ? X.m3Daily(2026, 9, 29) : X.m3New(li, seed);
console.log(j);
const m0 = JSON.parse(j).state.moves;
// boost 走法：前四步依次用锤子（提示第一格）/ 十字消（提示第二格）/ 自由交换（提示两格）/ 洗牌（同原生侧 boostStep）
function boostStep(mode, k, [a, b]) {
  if (mode !== "boost") return null;
  return [() => X.m3Hammer(a[0], a[1]), () => X.m3Cross(b[0], b[1]), () => X.m3FreeSwap(a[0], a[1], b[0], b[1]), () => X.m3Shuffle()][k] || null;
}
const times = [];
for (let k = 0; k < (mode === "advance" ? 400 : n); k++) {
  const s = JSON.parse(j).state;
  if (s.over || !s.hint) break;   // 与 Parity.hs 一致：走完（或提前结束）后再撤销一步
  const bs = boostStep(mode, k, s.hint);
  if (bs) { j = bs(); console.log(j); continue; }
  const [[r1, c1], [r2, c2]] = fixedMove(mode, k) || pickMove(mode, s);
  if (mode === "cham-rainbow" && chamRainbowPairs(s.board).some(([p, q]) => p[0] === r1 && p[1] === c1 && q[0] === r2 && q[1] === c2)) {
    console.error(`走法 cham-rainbow：第 ${k} 步换彩虹 × 变色龙 ((${r1},${c1}),(${r2},${c2}))`);
  }
  const a = performance.now();
  j = instance.exports.m3Swap(r1, c1, r2, c2);
  times.push(performance.now() - a);
  if (mode.startsWith("fix-")) for (const l of magicBlasts(s, JSON.parse(j).events)) console.error(`走法 fix：第 ${k} 步魔法地格扩爆 ${l}`);
  console.log(j);
}
if (mode !== "advance") console.log(instance.exports.m3Undo());   // advance 走法停在结局上（同原生侧）
// 走完后的只读查询与换局接口（同原生侧 extras，同序同参数）
if (["boost", "daily", "advance"].includes(mode)) {
  const lv = JSON.parse(X.m3State()).state.level;
  for (const f of [() => X.m3Progress(0, m0), () => X.m3Progress(40, m0),
    () => X.m3Badge(0, 0, 0, 0, 0), () => X.m3Badge(1, 3, 123, 0, 0), () => X.m3Badge(1, 1, 77, 0, 0), () => X.m3Badge(0, 0, 0, 50, 4),
    () => X.m3MapJump(5, 3), () => X.m3MapJump(0, 7), () => X.m3MapJump(48, lv),
    () => X.m3Advance(m0, 7), () => X.m3Restart(m0, 7), () => X.m3Showcase()]) console.log(f());
}
console.error(`node: 实例化+初始化 ${(t1 - t0).toFixed(1)} ms；m3Swap 每步 ms：${times.map((t) => t.toFixed(1)).join(" ")}`);
