// 网页外壳：加载 wasm 与贴图集、自适应布局、固定步长的帧循环、输入（点选 / 拖划）与 HUD。
// 规则全在 Haskell 核心（m3New / m3Swap / m3Undo / m3State / m3Levels，经 Engine.Game 的 match3Shell）；
// 逐轮回放的时间线也在 wasm 里（m3AnimStart / m3AnimTick，桌面 app/ComboFx.hs 的阶段机），
// JS 每帧推进一次、按返回的阶段 / 帧号插值绘制（render.js），不做任何规则或时间线判断。
import { WASI, OpenFile, File, ConsoleStdout } from "./vendor/browser_wasi_shim/index.js";
import makeJsffi from "./ghc_wasm_jsffi.js";
import { loadArt } from "./art.js";
import { CELL, PAD, fallbacks, setDims } from "./cells.js";
import { Fx, SWAP_FRAMES, FALL_FRAMES, drawCascade, drawLightFall, drawStatic, drawSwap } from "./render.js";
import { buttonAtUnits, cellAtUnits, cellCenterCss, computeLayout, safeInsets, toUnits } from "./layout.js";
import { FONT, drawHud, drawOverlay } from "./hud.js";

// ---------------------------------------------------------------------------
// 1. 加载 wasm 与贴图（并行），记录耗时
const perf = { steps: [], tickMs: 0, ticks: 0, drawMs: 0, draws: 0 };
const tStart = performance.now();
const wasi = new WASI([], [], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((m) => console.log("[hs]", m)),
  ConsoleStdout.lineBuffered((m) => console.warn("[hs]", m)),
], { debug: false });
const hsExports = {};
const [{ instance }, art] = await Promise.all([
  WebAssembly.instantiateStreaming(fetch("match3-web.wasm"), { ghc_wasm_jsffi: makeJsffi(hsExports), wasi_snapshot_preview1: wasi.wasiImport }),
  loadArt(),
]);
Object.assign(hsExports, instance.exports);
perf.instantiateMs = performance.now() - tStart;
wasi.initialize(instance);
perf.initMs = performance.now() - tStart;
const X = instance.exports;
function call(name, ...args) {
  const r = JSON.parse(X[name](...args));
  if (r && r.ok === false) throw new Error(`${name}: ${r.error}`);
  return r;
}

// ---------------------------------------------------------------------------
// 2. 画布与自适应布局：画布铺满视口，后备缓冲 = CSS 尺寸 × dpr（dpr 上限 3）；尺寸变化只重算布局，不动对局与动画
const canvas = document.getElementById("board");
const ctx = canvas.getContext("2d");
let L = null, dpr = 1;
function relayout() {
  const vv = window.visualViewport;
  const W = Math.round(vv ? vv.width : window.innerWidth), H = Math.round(vv ? vv.height : window.innerHeight);
  dpr = Math.min(3, window.devicePixelRatio || 1);
  canvas.style.width = `${W}px`; canvas.style.height = `${H}px`;
  const bw = Math.round(W * dpr), bh = Math.round(H * dpr);
  if (canvas.width !== bw || canvas.height !== bh) { canvas.width = bw; canvas.height = bh; }
  const rows = state ? state.board.length : 8, cols = state ? state.board[0].length : 8;
  L = computeLayout(W, H, rows, cols, safeInsets());
}
let relayoutPending = false;
const scheduleRelayout = () => { if (!relayoutPending) { relayoutPending = true; requestAnimationFrame(() => { relayoutPending = false; relayout(); }); } };
window.addEventListener("resize", scheduleRelayout);
window.addEventListener("orientationchange", scheduleRelayout);
window.visualViewport?.addEventListener("resize", scheduleRelayout);
new ResizeObserver(scheduleRelayout).observe(document.documentElement);

// ---------------------------------------------------------------------------
// 3. 对局状态
const levels = call("m3Levels");

let state = null;        // 最近一次核心返回的（已生效的）状态
let pending = null;      // 正在播放的一步的 m3Swap 结果（播完后才把 state 换成它）
let anim = null;         // null | {kind:"swap"|"cascade"|"fall", ...}
let busy = false, fastReq = false;
let sel = null, showHint = null, msg = "", pulse = 0, shownScore = 0;
let hudDrawn = null;   // 上一帧 HUD 关卡面板各部件的矩形（drawHud 返回；调试钩子 / e2e 检查规则角标布局）
let pressed = null, frozen = false, frames = 0;
const fx = new Fx();

// HUD「目标 …」的中文显示名：直接用核心给的 goal.label（视图模型 Match3.View.goalLabel，唯一来源；
// 名字目标在 namedGoalLabelTable 登记）。JS 不再自带「目标种类 → 中文」映射表，新目标 / 新元素不用改这里。
function goalText(s) { return s.goal.label ?? s.goal.text; }

function newGame(level, seed) {
  const t0 = performance.now();
  state = call("m3New", level, seed).state;
  if (perf.firstNewMs === undefined) perf.firstNewMs = performance.now() - t0;
  setDims(state.board.length, state.board[0].length);
  pending = null; anim = null; busy = false; fastReq = false; sel = null; showHint = null;
  shownScore = state.score; fx.clear();
  msg = `第 ${state.level + 1} 关：交换相邻两格（点选或拖划），凑 3 个以上同色消除`;
  relayout();
}

// 交换：调用核心；被拒则播「换过去再换回来」，接受则 交换补间 → 逐轮回放 → (轻落) → 生效
function doSwap(a, b) {
  if (busy || state.over) return;
  sel = null; showHint = null;
  const t0 = performance.now();
  const raw = X.m3Swap(a[0], a[1], b[0], b[1]);
  const t1 = performance.now();
  const res = JSON.parse(raw);
  if (res.ok === false) throw new Error(res.error);
  const waves = res.trace.waves.length;
  perf.steps.push({ ms: +(performance.now() - t0).toFixed(2), wasmMs: +(t1 - t0).toFixed(2), jsonBytes: raw.length,
    outcome: res.outcome?.tag ?? null, waves });
  if (!res.accepted) {
    msg = res.outcome?.tag === "NoMatch" ? "这样换不能消除，已退回" : "只能交换相邻两格";
    if (res.outcome?.tag === "NoMatch") { busy = true; anim = { kind: "swap", board: state.board, a, b, frame: 0, back: true }; }
    return;
  }
  busy = true; fastReq = false; pending = res;
  anim = { kind: "swap", board: state.board, a, b, frame: 0, back: false };
}

// 盘面编号表：顺序与 Match3Web.Anim.animBoards 相同
function cascadeData(res) {
  const tr = res.trace;
  const boards = [tr.start, ...tr.waves.flatMap((w) => [w.before, w.after]), ...tr.end.flatMap((e) => [e.before, e.after]), tr.final, res.state.board];
  const cleared = tr.waves.map((_, k) => res.events.filter((e) => e.beat === k && e.kind === "clear").flatMap((e) => e.pairs.map((p) => p[0])));
  const score = tr.waves.map((_, k) => res.events.filter((e) => e.beat === k && e.kind === "score").reduce((s, e) => s + e.amount, 0));
  const blast = tr.waves.map((_, k) => res.events.some((e) => e.beat === k && e.kind === "blast"));
  return { boards, waves: tr.waves, ends: tr.end, cleared, score, blast };
}

function startCascade() {
  const a0 = call("m3AnimStart");
  if (!a0.anim) { anim = { kind: "fall", board: pending.state.board, frame: 0 }; fx.burst(state.board, pending.state.lastCleared); return; }
  const cas = cascadeData(pending);
  cas.fall = a0.fall; cas.base = a0.base;
  anim = { kind: "cascade", cas, tk: { p: "start", fr: 0, n: 0, w: 0, k: 0, g: 0, b: 0 }, best: 0 };
}

// 步末碎屑色（桌面 UI.Presentation.elementRGBTable 里会出现在蔓延步末的几行）
const SPREAD_CRUMB_RGB = { vine: [110, 220, 90], choco: [150, 90, 45], steam: [225, 225, 235] };

function stepCascade() {
  const t0 = performance.now();
  const tk = JSON.parse(X.m3AnimTick(fastReq ? 1 : 0));
  perf.tickMs += performance.now() - t0; perf.ticks++;
  const cas = anim.cas;
  shownScore = cas.base + tk.g;
  if (tk.done) {
    anim.best = tk.best;
    if (tk.fall) anim = { kind: "fall", board: pending.state.board, frame: 0, best: tk.best };
    else finishMove(tk.best);
    return;
  }
  for (const e of tk.ev || []) {
    const cl = cas.cleared[e.w] || [];
    if (e.e === "hl" && e.k >= 2) fx.comboPop(e.k, cl);
    if (e.e === "van") {
      fx.burst(cas.waves[e.w].before, cl);
      if (cas.score[e.w] > 0) fx.scorePop(cas.score[e.w], e.k, cl);
      if (e.k >= 2) fx.startShake(e.k);
    }
    if (e.e === "end" && tk.s) {
      for (const i of tk.s.e) {
        const ef = cas.ends[i].effect;
        // 蔓延碎屑按元素名取色（桌面 UI.Playback.endCrumbs 的 CrumbsByElement：表里没有的元素不迸，
        // 例如第 44 关彩虹组合的变身步 rainbow_line / rainbow_bomb）
        const crumbRGB = ef.type === "spread" && SPREAD_CRUMB_RGB[ef.kind];
        if (crumbRGB) fx.crumbs(crumbRGB, ef.pairs.map((p) => p[1]));
        if (ef.type === "tick") fx.crumbs([255, 110, 70], ef.cells);
      }
    }
  }
  anim.tk = tk;
}

function finishMove(best = 0) {
  const res = pending;
  state = res.state; pending = null; anim = null; busy = false; fastReq = false;
  shownScore = state.score;
  const gained = res.trace.waves.reduce((s, w) => s + w.score, 0);
  msg = best >= 2 ? `${best} 连击！本步 +${gained}` : `+${gained}`;
  if (state.shuffled) msg += "（无可走步，已自动洗牌）";
}

// 固定步长的一帧（60 fps）：呼吸计数、粒子 / 浮字 / 震屏、当前动画
function stepFrame() {
  pulse++; frames++;
  fx.tick();
  if (!anim) return;
  if (anim.kind === "swap") {
    if (++anim.frame >= SWAP_FRAMES) {
      if (anim.back) {
        if (anim.returning) { anim = null; busy = false; }
        else anim = { ...anim, frame: 0, returning: true, a: anim.b, b: anim.a, board: swapped(anim.board, anim.a, anim.b) };
      } else startCascade();
    }
  } else if (anim.kind === "cascade") stepCascade();
  else if (anim.kind === "fall" && ++anim.frame >= FALL_FRAMES) finishMove(anim.best || 0);
}
function swapped(board, a, b) {
  const nb = board.map((row) => row.slice());
  nb[a[0]][a[1]] = board[b[0]][b[1]]; nb[b[0]][b[1]] = board[a[0]][a[1]];
  return nb;
}

// ---------------------------------------------------------------------------
// 4. 绘制
function hudInfo() {
  const s = pending ? pending.state : state;
  return { level: s.level, name: s.name, rules: s.rules || [], score: shownScore, moves: s.moves, goalText: goalText(s),
    progress: anim ? state.progress : s.progress, target: s.target, msg, undo: s.undo, busy,
    boss: anim ? state.boss : s.boss, pulse };   // Boss 血条与目标条一样：播放期间显示本步之前的读数
}
function drawBackground(W, H) {
  ctx.fillStyle = "#1c1630"; ctx.fillRect(0, 0, W, H);
  const iw = art.bg.width, ih = art.bg.height, k = Math.max(W / iw, H / ih);
  ctx.drawImage(art.bg, (W - iw * k) / 2, (H - ih * k) / 2, iw * k, ih * k);
}
function render() {
  const t0 = performance.now();
  ctx.imageSmoothingEnabled = true; ctx.imageSmoothingQuality = "high";
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  drawBackground(L.W, L.H);
  // 整体：设计单位 → CSS 像素 → 后备缓冲像素
  ctx.setTransform(dpr * L.u, 0, 0, dpr * L.u, dpr * L.ox, dpr * L.oy);
  hudDrawn = drawHud(ctx, art, L, hudInfo(), pressed);
  const [sx, sy] = fx.shakeOffset(pulse);
  ctx.save();
  ctx.translate(L.board.x + sx, L.board.y + sy);
  const v = { st: pending ? pending.state : state, pulse, sel, hint: showHint, busy };
  if (!anim) drawStatic(ctx, art, v, state.board);
  else if (anim.kind === "swap") drawSwap(ctx, art, v, anim.board, anim.a, anim.b, anim.frame / SWAP_FRAMES);
  else if (anim.kind === "cascade") drawCascade(ctx, art, v, anim.cas, anim.tk);
  else drawLightFall(ctx, art, v, anim.board, anim.frame / FALL_FRAMES);
  fx.draw(ctx, art, pulse, FONT);
  if (!anim && state.over) {
    const o = state.over;
    const title = { LevelClear: "过关！", Won: "通关！", Lost: "步数用完了" }[o.tag] || o.tag;
    const sub = o.tag === "Lost" ? `${state.loseHint}。可「撤销」或「重开」` : `得分 ${o.score}` + (o.tag === "LevelClear" ? `，点「›」进入第 ${o.next + 1} 关` : "");
    drawOverlay(ctx, art, { x: PAD, y: PAD, w: L.cols * CELL, h: L.rows * CELL }, title, sub);
  }
  ctx.restore();
  perf.drawMs += performance.now() - t0; perf.draws++;
}

// 帧循环：requestAnimationFrame 驱动、固定 1/60 s 步长累加（高刷 / 掉帧时时间线不变），每次最多追 15 帧
const STEP = 1000 / 60;
let acc = 0, last = performance.now();
function loop(ts) {
  acc += Math.min(250, Math.max(0, ts - last)); last = ts;
  if (frozen) acc = 0;
  while (acc >= STEP) {
    stepFrame(); acc -= STEP;
    const bw = window.m3debug.breakWhen;
    if (bw && bw(debugInfo())) { window.m3debug.breakWhen = null; frozen = true; acc = 0; break; }
  }
  render();
  requestAnimationFrame(loop);
}

// ---------------------------------------------------------------------------
// 5. 输入：指针事件经同一布局变换映射；点选两格或按住向相邻方向拖划；播放中点击棋盘 = 加速
const isAdj = (a, b) => Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) === 1;
let drag = null;   // {cell, x, y}（设计单位）
function unitsOf(ev) { const r = canvas.getBoundingClientRect(); return toUnits(L, ev.clientX - r.left, ev.clientY - r.top); }
canvas.addEventListener("pointerdown", (ev) => {
  ev.preventDefault();
  const [x, y] = unitsOf(ev);
  const b = buttonAtUnits(L, x, y);
  if (b) { pressed = b.id; return; }
  const p = cellAtUnits(L, x, y);
  if (!p) return;
  if (busy) { if (anim && anim.kind === "cascade") fastReq = true; return; }
  if (state.over) return;
  if (sel && isAdj(sel, p)) { const a = sel; drag = null; doSwap(a, p); return; }
  sel = p; drag = { cell: p, x, y };
  canvas.setPointerCapture?.(ev.pointerId);
});
canvas.addEventListener("pointermove", (ev) => {
  if (!drag || busy) return;
  const [x, y] = unitsOf(ev), dx = x - drag.x, dy = y - drag.y;
  if (Math.max(Math.abs(dx), Math.abs(dy)) < CELL * 0.4) return;   // 拖划超过 0.4 格才算
  const [r, c] = drag.cell, to = Math.abs(dx) > Math.abs(dy) ? [r, c + Math.sign(dx)] : [r + Math.sign(dy), c];
  drag = null;
  if (to[0] >= 0 && to[0] < L.rows && to[1] >= 0 && to[1] < L.cols) doSwap([r, c], to);
});
canvas.addEventListener("pointerup", (ev) => {
  drag = null;
  if (!pressed) return;
  const [x, y] = unitsOf(ev), b = buttonAtUnits(L, x, y), id = pressed;
  pressed = null;
  if (b && b.id === id) onButton(id);
});
canvas.addEventListener("pointercancel", () => { drag = null; pressed = null; });
// 禁止双击缩放 / 捏合缩放 / 长按菜单
for (const t of ["gesturestart", "dblclick", "contextmenu"]) document.addEventListener(t, (e) => e.preventDefault(), { passive: false });
document.addEventListener("touchmove", (e) => { if (e.touches.length > 1) e.preventDefault(); }, { passive: false });
window.addEventListener("keydown", (e) => {
  if (e.key === " " && busy) fastReq = true;
  if (e.key === "z" || e.key === "u") onButton("undo");
  if (e.key === "h") onButton("hint");
});

function onButton(id) {
  if (busy) return;
  const seed = Date.now() & 0x7fffffff;
  if (id === "prev") newGame((state.level + levels.length - 1) % levels.length, seed);
  else if (id === "next") newGame((state.level + 1) % levels.length, seed);
  else if (id === "restart") newGame(state.level, seed);
  else if (id === "hint") { showHint = state.hint; msg = showHint ? "提示：交换高亮的两格" : "没有可走的步"; }
  else if (id === "undo") {
    // 撤销：历史在核心的 Engine.History（最多 20 步，终局后也能撤销）
    const res = call("m3Undo");
    if (!res.accepted) { msg = "没有可撤销的步"; return; }
    state = res.state; sel = null; showHint = null; shownScore = state.score; fx.clear();
    msg = `已撤销（还可撤销 ${state.undo} 步）`;
  }
}

// ---------------------------------------------------------------------------
// 6. 开局与调试钩子
const q = new URLSearchParams(location.search);
newGame(+(q.get("level") ?? 0), +(q.get("seed") ?? 20260929));
function debugInfo() {
  const tk = anim && anim.kind === "cascade" ? anim.tk : null;
  return { kind: anim ? anim.kind : null, p: tk?.p ?? null, fr: tk?.fr ?? anim?.frame ?? 0, n: tk?.n ?? 0, w: tk?.w ?? 0, k: tk?.k ?? 0,
    stage: tk?.s?.kind ?? null, blast: !!(tk && anim.cas.blast[tk.w]), pulse, frames, events: tk?.ev ?? null };
}
// 自动化钩子：只读状态 + 断点（breakWhen(info) 为真时冻结帧循环，截图后置 frozen=false 继续）；操作仍走真实指针事件
window.m3debug = {
  get state() { return state; }, get pending() { return pending; }, get busy() { return busy; }, get anim() { return debugInfo(); },
  get layout() { return L; }, get hud() { return hudDrawn; }, get levels() { return levels.length; },
  // 走几何降级的次数（按元素名，见 cells.js 的 fallbacks）；图集加载后应一直为空，e2e 每关检查
  get fallbacks() { return { ...fallbacks }; }, get dpr() { return dpr; }, perf, breakWhen: null,
  get frozen() { return frozen; }, set frozen(v) { frozen = v; },
  cellCenter: (r, c) => cellCenterCss(L, [r, c]),
  buttonCenter: (id) => { const b = L.buttons.find((x) => x.id === id); return [L.ox + (b.x + b.w / 2) * L.u, L.oy + (b.y + b.h / 2) * L.u]; },
};
requestAnimationFrame((ts) => { last = ts; loop(ts); });
