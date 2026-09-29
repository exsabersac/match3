// 网页外壳（spike）：只负责加载 wasm、画彩色方块、收集点击/拖拽、按回放脚本播放动画。
// 所有规则（能否交换、消除、下落、补子、连锁、计分、胜负）都在 Haskell 核心里，
// 这里通过 m3New / m3Swap / m3Undo / m3State / m3Levels 五个 JSFFI 导出拿到 JSON 结果
// （核心侧走 Engine.Game 的三消外壳实例 match3Shell，与桌面版同一条路径；m3Swap 另带结构化效果事件 events）。
import { WASI, OpenFile, File, ConsoleStdout } from "./vendor/browser_wasi_shim/index.js";
import makeJsffi from "./ghc_wasm_jsffi.js";

// ---------------------------------------------------------------------------
// 1. 加载与初始化 wasm（记录耗时，供报告与调试）
const perf = { steps: [] };
const tStart = performance.now();

const wasi = new WASI([], [], [
  new OpenFile(new File([])),                                   // stdin
  ConsoleStdout.lineBuffered((m) => console.log("[hs]", m)),    // stdout
  ConsoleStdout.lineBuffered((m) => console.warn("[hs]", m)),   // stderr
], { debug: false });   // 关掉垫片自带的 "wasi:" 调试输出
const hsExports = {};   // knot-tying：jsffi 导入需要访问实例导出，实例化后再填
const { instance } = await WebAssembly.instantiateStreaming(fetch("match3-web.wasm"), {
  ghc_wasm_jsffi: makeJsffi(hsExports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(hsExports, instance.exports);
perf.instantiateMs = performance.now() - tStart;
wasi.initialize(instance);   // 调用 _initialize（其中的构造器会初始化 GHC RTS）
perf.initMs = performance.now() - tStart;

// 包一层：调用导出 → 解析 JSON → 出错时抛异常
function call(name, ...args) {
  const r = JSON.parse(instance.exports[name](...args));
  if (r && r.ok === false) throw new Error(`${name}: ${r.error}`);
  return r;
}

// ---------------------------------------------------------------------------
// 2. 画布与渲染（Canvas 2D，按 devicePixelRatio 放大后备缓冲，Retina 下不糊）
const N = 8, CELL = 56, PAD = 4;
const canvas = document.getElementById("board");
const ctx = canvas.getContext("2d");
const dpr = window.devicePixelRatio || 1;
canvas.style.width = canvas.style.height = `${N * CELL}px`;
canvas.width = canvas.height = Math.round(N * CELL * dpr);
ctx.scale(dpr, dpr);

const COLORS = { 1: "#e53935", 2: "#43a047", 3: "#1e88e5", 4: "#fdd835", 5: "#8e24aa" };
const $ = (id) => document.getElementById(id);

let state = null;        // 最近一次核心返回的状态
let sel = null;          // 当前选中格 [r,c]
let busy = false;        // 播放动画期间锁输入
let showHint = null;     // 提示高亮的两格

function drawCell(r, c, cell, opts = {}) {
  const x = c * CELL, y = r * CELL;
  if (!cell) return;                                  // 空洞（消除后、下落前）
  ctx.globalAlpha = opts.alpha ?? 1;
  if (cell.t === "G") {
    ctx.fillStyle = COLORS[cell.c] || "#999";
    ctx.fillRect(x + PAD, y + PAD, CELL - 2 * PAD, CELL - 2 * PAD);
    ctx.fillStyle = "rgba(0,0,0,.55)";
    ctx.strokeStyle = "rgba(0,0,0,.55)";
    ctx.lineWidth = 4;
    const cx = x + CELL / 2, cy = y + CELL / 2;
    // 特殊块的最简标记：H 横线、V 竖线、B 圆环、R 星
    if (cell.k === "H") { ctx.beginPath(); ctx.moveTo(x + 10, cy); ctx.lineTo(x + CELL - 10, cy); ctx.stroke(); }
    if (cell.k === "V") { ctx.beginPath(); ctx.moveTo(cx, y + 10); ctx.lineTo(cx, y + CELL - 10); ctx.stroke(); }
    if (cell.k === "B") { ctx.beginPath(); ctx.arc(cx, cy, 13, 0, Math.PI * 2); ctx.stroke(); }
    if (cell.k === "R") { ctx.font = "bold 28px sans-serif"; ctx.textAlign = "center"; ctx.textBaseline = "middle"; ctx.fillStyle = "#fff"; ctx.fillText("★", cx, cy + 1); }
    if (cell.i > 0) { ctx.strokeStyle = "rgba(255,255,255,.9)"; ctx.lineWidth = 3; ctx.strokeRect(x + PAD + 2, y + PAD + 2, CELL - 2 * PAD - 4, CELL - 2 * PAD - 4); }
    if (cell.o) { ctx.font = "10px sans-serif"; ctx.fillStyle = "#fff"; ctx.textAlign = "left"; ctx.textBaseline = "top"; ctx.fillText(cell.o, x + PAD + 2, y + PAD + 2); }
  } else {
    // 非宝石格（石头、宝箱……）：灰块 + 核心 show 文本占位
    ctx.fillStyle = cell.c ? COLORS[cell.c] : "#78808c";
    ctx.fillRect(x + PAD, y + PAD, CELL - 2 * PAD, CELL - 2 * PAD);
    ctx.fillStyle = "#fff"; ctx.font = "10px sans-serif"; ctx.textAlign = "center"; ctx.textBaseline = "middle";
    ctx.fillText(cell.s.slice(0, 9), x + CELL / 2, y + CELL / 2);
  }
  ctx.globalAlpha = 1;
}

// board：8×8 单元格数组；flash：要高亮（即将消除）的格
function drawBoard(board, { flash = [] } = {}) {
  ctx.clearRect(0, 0, N * CELL, N * CELL);
  for (let r = 0; r < N; r++) for (let c = 0; c < N; c++) drawCell(r, c, board[r][c]);
  ctx.fillStyle = "rgba(255,255,255,.75)";
  for (const [r, c] of flash) ctx.fillRect(c * CELL + PAD, r * CELL + PAD, CELL - 2 * PAD, CELL - 2 * PAD);
  const ring = (p, color) => { ctx.strokeStyle = color; ctx.lineWidth = 3; ctx.strokeRect(p[1] * CELL + 1.5, p[0] * CELL + 1.5, CELL - 3, CELL - 3); };
  if (sel) ring(sel, "#fff");
  if (showHint) showHint.forEach((p) => ring(p, "#00e5ff"));
}

const GOAL_NAMES = { GoalScore: "分数", GoalCollect: "收集", GoalCollectMulti: "多色收集", GoalClearStone: "碎石",
  GoalChest: "宝箱", GoalHoney: "蜂蜜罐", GoalBalloon: "气球", GoalCookie: "饼干", GoalCake: "蛋糕",
  GoalSafe: "保险箱", GoalUfo: "飞碟", GoalCarpet: "地毯" };

function renderHud(s) {
  $("lv").textContent = `${s.level + 1}「${s.name}」`;
  $("score").textContent = s.score;
  $("moves").textContent = s.moves;
  $("goal").textContent = `${GOAL_NAMES[s.goal.kind] || s.goal.text} ${Math.min(s.progress, s.target)}/${s.target}`;
}

function renderOverlay(s) {
  const ov = $("overlay");
  if (!s.over) { ov.style.display = "none"; return; }
  const o = s.over;
  const title = { LevelClear: "过关！", Won: "通关！", Lost: "步数用完了" }[o.tag] || o.tag;
  const sub = o.tag === "Lost" ? s.loseHint : `得分 ${o.score}` + (o.tag === "LevelClear" ? `，可进入第 ${o.next + 1} 关` : "");
  ov.innerHTML = `<div>${title}</div><small>${sub}</small><small>点「重新开始」或在下拉框选关再玩</small>`;
  Object.assign(ov.style, { display: "flex", left: "0", top: "0", width: `${N * CELL}px`, height: `${N * CELL}px` });
}

function renderAll() { drawBoard(state.board); renderHud(state); renderOverlay(state); }

// ---------------------------------------------------------------------------
// 3. 回放一步：按核心给的逐轮快照 start → (闪光 → 空洞 → 下落补子)×N → 步末效果 → 最终
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
async function playTrace(tr) {
  drawBoard(tr.start); await sleep(150);                       // 交换后的盘面
  let wi = 0;
  const playEnds = async (k) => { for (const e of tr.end.filter((e) => e.afterWaves === k)) { drawBoard(e.after); await sleep(200); } };
  for (const w of tr.waves) {
    await playEnds(wi);
    if (w.cleared.length) { drawBoard(w.before, { flash: w.cleared }); await sleep(180); }
    drawBoard(w.holes); await sleep(120);
    drawBoard(w.after); await sleep(180);
    wi++;
    if (tr.waves.length > 1) $("msg").textContent = `连击 ×${wi}！+${w.score}`;
  }
  await playEnds(wi);
}

// ---------------------------------------------------------------------------
// 4. 交换：调用核心，播放，更新 HUD
async function doSwap(a, b) {
  if (busy || state.over) return;
  busy = true; sel = null; showHint = null;
  const t0 = performance.now();
  const raw = instance.exports.m3Swap(a[0], a[1], b[0], b[1]);  // ← 规则全在这一步（Haskell）
  const t1 = performance.now();
  const res = JSON.parse(raw);
  const t2 = performance.now();
  if (res.ok === false) { busy = false; throw new Error(res.error); }
  const waves = res.trace.waves.length;
  // 记录：wasm 调用耗时（含 Haskell 结算 + JSON 序列化 + JSString 转换）、JS 解析耗时、JSON 字节数
  perf.steps.push({ ms: +(t2 - t0).toFixed(2), wasmMs: +(t1 - t0).toFixed(2), parseMs: +(t2 - t1).toFixed(2),
                    jsonBytes: raw.length, outcome: res.outcome?.tag ?? null, waves });
  const o = res.outcome;
  if (!res.accepted) {
    $("msg").textContent = o && o.tag === "NoMatch" ? "这样换不能消除，已退回" : "只能交换相邻两格";
  } else {
    await playTrace(res.trace);
    const gained = res.trace.waves.reduce((s, w) => s + w.score, 0);
    $("msg").textContent = waves > 1 ? `连锁 ${waves} 轮！本步 +${gained}` : `+${gained}`;
    if (res.state.shuffled) $("msg").textContent += "（无可走步，已自动洗牌）";
  }
  state = res.state;
  renderAll();
  renderPerf();
  busy = false;
}

function renderPerf() {
  const last = perf.steps.slice(-1)[0];
  $("perf").textContent =
    `wasm 实例化 ${perf.instantiateMs.toFixed(1)} ms，初始化 ${perf.initMs.toFixed(1)} ms，首局 m3New ${perf.firstNewMs.toFixed(1)} ms` +
    (last ? `\n上一步 m3Swap（含 JSON 解析）${last.ms} ms，${last.waves} 轮，结果 ${last.outcome}` : "");
}

// ---------------------------------------------------------------------------
// 5. 输入：点选两格，或按下后拖到相邻格松开
function cellAt(ev) {
  const rect = canvas.getBoundingClientRect();
  const c = Math.floor((ev.clientX - rect.left) / CELL), r = Math.floor((ev.clientY - rect.top) / CELL);
  return r >= 0 && r < N && c >= 0 && c < N ? [r, c] : null;
}
const isAdj = (a, b) => Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) === 1;
let dragFrom = null;
canvas.addEventListener("pointerdown", (ev) => {
  if (busy) return;
  const p = cellAt(ev); if (!p) return;
  if (sel && isAdj(sel, p)) { const a = sel; dragFrom = null; doSwap(a, p); return; }
  sel = p; dragFrom = p; drawBoard(state.board);
});
canvas.addEventListener("pointerup", (ev) => {
  const p = cellAt(ev);
  if (dragFrom && p && isAdj(dragFrom, p)) { const a = dragFrom; dragFrom = null; doSwap(a, p); return; }
  dragFrom = null;
});

// ---------------------------------------------------------------------------
// 6. 控件与开局
function newGame(level, seed) {
  const t0 = performance.now();
  state = call("m3New", level, seed).state;
  const dt = performance.now() - t0;
  if (perf.firstNewMs === undefined) perf.firstNewMs = dt;
  sel = null; showHint = null; busy = false;
  $("msg").textContent = `第 ${state.level + 1} 关开始：交换相邻两格，凑 3 个以上同色消除`;
  renderAll(); renderPerf();
}
const levels = call("m3Levels");
$("level").innerHTML = levels.map((l) => `<option value="${l.index}">第 ${l.index + 1} 关 ${l.name}</option>`).join("");
$("level").addEventListener("change", () => newGame(+$("level").value, Date.now() & 0x7fffffff));
$("restart").addEventListener("click", () => newGame(+$("level").value, Date.now() & 0x7fffffff));
$("hintBtn").addEventListener("click", () => { showHint = state.hint; drawBoard(state.board); });
// 撤销：历史在核心的 Engine.History 里（最多 20 步，终局后也能撤销）
$("undoBtn").addEventListener("click", () => {
  if (busy) return;
  const res = call("m3Undo");
  if (!res.accepted) { $("msg").textContent = "没有可撤销的步"; return; }
  state = res.state; sel = null; showHint = null;
  $("msg").textContent = `已撤销（还可撤销 ${state.undo} 步）`;
  renderAll();
});

// URL 参数：?level=0&seed=42 便于复现（无头浏览器测试也用它固定盘面）
const q = new URLSearchParams(location.search);
newGame(+(q.get("level") ?? 0), +(q.get("seed") ?? 20260929));
$("level").value = String(state.level);

// 调试/自动化钩子：只读地暴露状态与耗时，自动化脚本仍通过真实点击来操作
window.m3debug = { get state() { return state; }, get busy() { return busy; }, perf, CELL };
