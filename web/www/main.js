// 网页外壳：加载 wasm 与贴图集、自适应布局、固定步长的帧循环、输入（点选 / 拖划 / 键盘）与 HUD。
// 网页是唯一前端（PC 端将像 Android 一样套壳），桌面 SDL 版的交互都迁到这里：道具、洗牌、每日挑战、选关地图、暂停、
// 结局后前进、星级、进度点、分数徽章、首关提示、按键条、窗口标题、元素展示盘（见 docs/web.md §8.1）。
// 规则全在 Haskell 核心（m3New / m3Swap / m3Hammer / m3Cross / m3FreeSwap / m3Shuffle / m3Undo / m3Daily / m3Restart /
// m3Advance / m3Progress / m3MapJump / m3Badge / m3State / m3Levels，经 Engine.Game 的 match3Shell）；
// 逐轮回放的时间线也在 wasm 里（m3AnimStart / m3AnimTick，桌面 app/pure/ComboFx.hs 的阶段机），
// JS 每帧推进一次、按返回的阶段 / 帧号插值绘制（render.js），不做任何规则或时间线判断。
import { WASI, OpenFile, File, ConsoleStdout } from "./vendor/browser_wasi_shim/index.js";
import makeJsffi from "./ghc_wasm_jsffi.js";
import { loadArt } from "./art.js";
import { CELL, PAD, dropMarks, fallbacks, setDims, setPalette } from "./cells.js";
import { Fx, SWAP_FRAMES, FALL_FRAMES, comboStyle, drawCascade, drawLightFall, drawStatic, drawSwap, setAnimMeta, styleRGB } from "./render.js";
import { buttonAtUnits, cellAtUnits, cellCenterCss, computeLayout, safeInsets, toUnits } from "./layout.js";
import { FONT, drawBanner, drawHelpStrip, drawHud, drawOverlay } from "./hud.js";
import { drawMap, drawMenu, drawPause, mapHit, mapLayout, menuHit, menuLayout } from "./panels.js";
import { drawGuide, guideEntries, noteSpecials } from "./guide.js";
import { unlock, play, toggleSfx, toggleBgm, sfxEnabled, bgmEnabled, startBgm, setSoundNames } from "./audio.js";

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

// 表现表（颜色、格子取色规则、碎屑色、生长曲线、帧数、音效名）由 wasm 下发一次（m3Meta = UI.WebMeta），不在 JS 里手抄
const meta = call("m3Meta");
setPalette(meta); setAnimMeta(meta); setSoundNames(meta.sounds);

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
const CHAPTERS = meta.chapters;

let state = null;        // 最近一次核心返回的（已生效的）状态
let pending = null;      // 正在播放的一步的 m3Swap / 道具 / 洗牌结果（播完后才把 state 换成它；pending.ui = 来自哪条界面路径）
let anim = null;         // null | {kind:"swap"|"cascade"|"fall", ...}
let busy = false, fastReq = false;
let sel = null, showHint = null, showGuide = false, seenSpecials = new Set(), msg = "", pulse = 0, shownScore = 0;
let hudDrawn = null;   // 上一帧 HUD 关卡面板各部件的矩形（drawHud 返回；调试钩子 / e2e 检查规则角标布局）
let pressed = null, frozen = false, frames = 0;
let overlayDrawn = null;   // 上一帧画出的结局面板文字 {title, sub, stars, action}（e2e 查失败提示不漏内部名）
const fx = new Fx();
// 从桌面版（app/UI/Types.hs 的 App）迁来的界面状态：
let tool = null, swapFirst = null;          // 道具点选模式 "hammer" | "swap" | "cross"（appTool）；自由交换已点的第一格
let paused = false, mapOpen = false;        // 暂停说明（appPaused）、选关地图（appMapOpen）
let menuOpen = false;                       // 菜单面板（网页自有：常驻按钮之外的全部操作；打开时冻结回放）
let startMoves = 0;                         // 开局步数（appStartMoves：三星分母、每日挑战重开用）
let tipFrames = 0, helpFrames = 0;          // 首关提示横幅（appTipFrames，240 帧）、按键条（appHelpFrames，300 帧）
let comboLeft = 0, comboBest = 0;           // 本步连击总结剩余帧（appComboShow）与本步最高连击（appComboBest）
let dailyDate = null;                       // 当前每日挑战的日期 [年, 月, 日]（战役关为 null）
let drawnExtra = { banner: null, help: null, pause: null, map: null, menu: null };   // 上一帧各浮层画出的内容（e2e 用）
// 选关进度（appMaxReached）：桌面只在内存里，网页存 localStorage「m3-reached」，刷新 / 重开浏览器后保留
const REACHED_KEY = "m3-reached";
let reached = (() => { try { return Math.max(0, parseInt(localStorage.getItem(REACHED_KEY) ?? "0", 10) || 0); } catch { return 0; } })();
let prog = { reached, stars: 0, dots: "" };  // m3Progress：{reached, stars, dots}
const finePointer = () => window.matchMedia?.("(hover: hover) and (pointer: fine)").matches ?? false;
const newSeed = () => Date.now() & 0x7fffffff;

// HUD「目标 …」的中文显示名：直接用核心给的 goal.label（视图模型 Match3.View.goalLabel，唯一来源；
// 名字目标在 namedGoalLabelTable 登记）。JS 不再自带「目标种类 → 中文」映射表，新目标 / 新元素不用改这里。
function goalText(s) { return s.goal.label ?? s.goal.text; }

// 进度点 / 星级 / 解锁：核心 m3Progress（Match3.Core.unlockAfterOutcome、starRating、Match3.View.levelDots）
function refreshProgress() {
  prog = call("m3Progress", reached, startMoves);
  if (prog.reached !== reached) {
    reached = prog.reached;
    try { localStorage.setItem(REACHED_KEY, String(reached)); } catch { /* 隐私模式等：只在内存里 */ }
  }
}

// 换一局（新关 / 重开 / 前进 / 每日 / 展示盘）后的界面重置：同桌面 UI.Actions.freshLevelUi——
// 清选中 / 动画 / 道具模式 / 暂停 / 地图，记开局步数，第 1 关（下标 0，含每日挑战）自动亮提示并显示 240 帧「按 H 查看提示」，
// 按键条重新显示 300 帧，进度记到当前关。
function begin(st, text, sm = st.moves) {
  state = st;
  setDims(state.board.length, state.board[0].length);
  pending = null; anim = null; busy = false; fastReq = false; sel = null; showHint = null; drag = null;
  showGuide = false; seenSpecials = new Set(); noteSpecials(state, seenSpecials);
  shownScore = state.score; fx.clear();
  tool = null; swapFirst = null; paused = false; mapOpen = false; menuOpen = false; comboLeft = 0; comboBest = 0;
  if (!state.daily) dailyDate = null;
  startMoves = sm;
  tipFrames = state.level === 0 ? 240 : 0;
  if (state.level === 0) showHint = state.hint;
  helpFrames = 300;
  msg = text;
  refreshProgress();
  startBgm();
  relayout();
}

function newGame(level, seed) {
  const t0 = performance.now();
  const st = call("m3New", level, seed).state;
  if (perf.firstNewMs === undefined) perf.firstNewMs = performance.now() - t0;
  begin(st, `第 ${st.level + 1} 关：交换相邻两格（点选或拖划），凑 3 个以上同色消除`);
}

// 每日挑战（桌面 D 键 keyDaily；桌面用固定演示日期 2026-09-29，网页用本地今天，?daily=YYYY-MM-DD 可指定）
const dailyLabel = (d) => d && `${d[0]}-${String(d[1]).padStart(2, "0")}-${String(d[2]).padStart(2, "0")}`;
function today() { const t = new Date(); return [t.getFullYear(), t.getMonth() + 1, t.getDate()]; }
function startDaily(d) {
  dailyDate = d;
  begin(call("m3Daily", d[0], d[1], d[2]).state, `每日挑战 ${dailyLabel(d)}：同一天的盘面人人相同`);
}

// 重开本关（桌面 R 键 restartSame：每日挑战按开局步数与原目标换种子重开，战役关同一关换种子）
function restart() {
  const res = call("m3Restart", startMoves, newSeed());
  begin(res.state, state.daily ? `每日挑战 ${dailyLabel(dailyDate) ?? ""}：已重开` : `第 ${res.state.level + 1} 关：已重开`);
}

// 结局后前进（桌面 N / 回车 / 空格 / 点结算面板，UI.Actions.advanceOrMsg）：过关 → 下一关（带入剩余步数，最多 3 步）、
// 通关 → 从第 1 关重开战役、失败 → 重开本关。没结束时只提示。
function advance() {
  if (busy) return;
  if (!state.over) { msg = "还没结束：先过关（或用完步数）再前进"; return; }
  const tag = state.over.tag, res = call("m3Advance", startMoves, newSeed());
  if (!res.accepted) return;
  const st = res.state, carry = st.moves - res.startMoves;
  const text = tag === "LevelClear" ? `第 ${st.level + 1} 关` + (carry > 0 ? `：带入上一关剩余 ${carry} 步` : "：开始")
    : tag === "Won" ? "全部通关！从第 1 关重新开始" : st.daily ? "再试一次每日挑战！" : `第 ${st.level + 1} 关：再试一次！`;
  begin(st, text, res.startMoves);
}

// 交换：调用核心；被拒则播「换过去再换回来」，接受则 交换补间 → 逐轮回放 → (轻落) → 生效
function doSwap(a, b) {
  if (busy || state.over) return;
  sel = null; showHint = null; tipFrames = 0;
  const t0 = performance.now();
  const raw = X.m3Swap(a[0], a[1], b[0], b[1]);
  const t1 = performance.now();
  const res = JSON.parse(raw);
  if (res.ok === false) throw new Error(res.error);
  const waves = res.trace.waves.length;
  perf.steps.push({ ms: +(performance.now() - t0).toFixed(2), wasmMs: +(t1 - t0).toFixed(2), jsonBytes: raw.length,
    outcome: res.outcome?.tag ?? null, waves });
  if (!res.accepted) {
    play("illegal");
    msg = res.outcome?.tag === "NoMatch" ? "这样换不能消除，已退回" : "只能交换相邻两格";
    if (res.outcome?.tag === "NoMatch") { busy = true; anim = { kind: "swap", board: state.board, a, b, frame: 0, back: true }; }
    return;
  }
  play("swap");
  busy = true; fastReq = false; pending = res; comboLeft = 0;
  anim = { kind: "swap", board: state.board, a, b, frame: 0, back: false };
}

// 道具（桌面 1 / 2 / 3 键与 HUD 道具芯片，UI.Actions.applyBooster）：锤子 / 十字消打一格，自由交换换任意两格。
// 核心经 gameStep 结算（m3Hammer / m3Cross / m3FreeSwap），keepTool 决定之后是否留在点选模式（UI.MoveText.keepsTool）。
// 锤子 / 十字直接进逐轮回放；自由交换先播交换补间（换不掉时换过去再换回来、不扣次数）。
const TOOL = {
  hammer: { key: "1", icon: "icon_hammer", name: "锤子", banner: "锤子：点一格", empty: "锤子用完了", bad: ["锤子打不了这一格", "锤子没打掉东西"] },
  swap: { key: "2", icon: "icon_swap", name: "自由交换", banner: "交换：点两格", empty: "自由交换用完了", bad: ["自由交换无效", "这样换不能消除，未扣次数：重新选第一格"] },
  cross: { key: "3", icon: "icon_cross", name: "十字消", banner: "十字：点一格", empty: "十字消用完了", bad: ["十字消打不了这一格", "十字消没打掉东西"] },
};
const boostersLeft = (k) => (state.boosters ? state.boosters[k] : 0);
function applyBooster(k, a, b = null) {
  const t0 = performance.now();
  const raw = k === "hammer" ? X.m3Hammer(a[0], a[1]) : k === "cross" ? X.m3Cross(a[0], a[1]) : X.m3FreeSwap(a[0], a[1], b[0], b[1]);
  const res = JSON.parse(raw);
  if (res.ok === false) throw new Error(res.error);
  const tag = res.outcome?.tag ?? null;
  perf.steps.push({ ms: +(performance.now() - t0).toFixed(2), jsonBytes: raw.length, outcome: tag, waves: res.trace.waves.length, booster: k });
  sel = null; showHint = null; tipFrames = 0; drag = null; swapFirst = null;
  tool = res.keepTool ? "swap" : null;
  if (!res.accepted || tag === "NoMatch" || tag === "InvalidSwap") {
    play("illegal");
    msg = TOOL[k].bad[tag === "NoMatch" ? 1 : 0];
    if (k === "swap" && tag === "NoMatch") { busy = true; anim = { kind: "swap", board: state.board, a, b, frame: 0, back: true }; }
    return;
  }
  busy = true; fastReq = false; pending = res; pending.ui = k; comboLeft = 0;
  if (k === "swap") { play("swap"); anim = { kind: "swap", board: state.board, a, b, frame: 0, back: false }; }
  else startCascade();
}
// 按道具键 / 道具按钮（同桌面 keyHammer / keyFreeSwap / keyCross）：再按一次取消；锤子 / 十字在已选中一格且有次数时立即使用；
// 次数为 0 时锤子 / 十字仍进入模式（提示用完，点格即退出），自由交换不进入。
function toggleTool(k) {
  if (busy || state.over) return;
  const T = TOOL[k];
  if (tool === k) { tool = null; swapFirst = null; sel = null; msg = `已取消${T.name}`; return; }
  if (k !== "swap" && sel && boostersLeft(k) > 0) { applyBooster(k, sel); return; }
  if (k === "swap" && boostersLeft(k) <= 0) { tool = null; swapFirst = null; msg = T.empty; return; }
  tool = k; swapFirst = null; sel = null; drag = null;
  msg = boostersLeft(k) <= 0 ? T.empty : `${T.banner}（菜单里再点「${T.name}」或按 ${T.key} 取消）`;
}
// 点选模式下点中一格（同桌面 cellClick 的道具分支；自由交换两步点选同 Engine.GridUI.gridClick）
function toolClick(p) {
  if (tool === "hammer" || tool === "cross") {
    if (boostersLeft(tool) <= 0) { msg = TOOL[tool].empty; tool = null; return; }
    applyBooster(tool, p);
  } else if (!swapFirst) { swapFirst = p; sel = p; msg = "交换：再点第二格"; }
  else if (swapFirst[0] === p[0] && swapFirst[1] === p[1]) { swapFirst = null; sel = null; msg = "交换：重新选第一格"; }
  else applyBooster("swap", swapFirst, p);
}

// 手动洗牌（桌面 S 键 keyShuffle：播放中 / 已结束时无效）：核心 m3Shuffle，新盘面播一段轻落；收掉连击角标 / 浮字 / 粒子
function doShuffle() {
  if (busy || state.over) return;
  const res = call("m3Shuffle");
  if (!res.accepted) { msg = "现在不能洗牌"; return; }
  sel = null; showHint = null; tool = null; swapFirst = null; fx.clear(); comboLeft = 0; comboBest = 0;
  busy = true; fastReq = false; pending = res; pending.ui = "shuffle";
  anim = { kind: "fall", board: res.state.board, frame: 0 };
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

// 步末碎屑色（m3Meta：蔓延元素在 elementRGBTable 的颜色、倒计时火星 = 表现表 EvTick 的 CrumbsAtSources）
const SPREAD_CRUMB_RGB = meta.spreadCrumbRGB, TICK_CRUMB_RGB = meta.tickCrumbRGB;

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
      play(cas.blast[e.w] ? "special" : "clear");
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
        if (ef.type === "tick" && TICK_CRUMB_RGB) fx.crumbs(TICK_CRUMB_RGB, ef.cells);
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
  const pre = res.ui && TOOL[res.ui] ? `${TOOL[res.ui].name} ` : "";
  msg = res.ui === "shuffle" ? "已洗牌" : best >= 2 ? `${pre}${best} 连击！本步 +${gained}` : `${pre}+${gained}`;
  if (state.shuffled && res.ui !== "shuffle") msg += "（无可走步，已自动洗牌）";
  // 本步连击总结（桌面 appComboShow = comboSummaryFrames）
  if (best >= 2) { comboBest = best; comboLeft = meta.frames.comboSummary; }
  const tag = state.over && state.over.tag;
  if (tag === "Won" || tag === "LevelClear") play("win");
  else if (tag === "Lost") play("lose");
  refreshProgress();
}

// 固定步长的一帧（60 fps）：呼吸计数、粒子 / 浮字 / 震屏、当前动画。暂停 / 地图打开时冻结（同桌面）
function stepFrame() {
  pulse++; frames++;
  if (paused || mapOpen || menuOpen) return;
  if (tipFrames > 0) tipFrames--;
  if (helpFrames > 0) helpFrames--;
  if (comboLeft > 0 && !busy) comboLeft--;
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
// 分数徽章（核心 m3Badge = Match3.View.scoreBadge）：输入不变时复用上一次结果
let badgeKey = "", badge = null;
function scoreBadge() {
  const rp = busy && pending ? 1 : 0, k = anim && anim.kind === "cascade" ? anim.tk.k || 0 : 0;
  const key = `${rp},${k},${shownScore},${comboLeft > 0 ? 1 : 0},${comboBest},${state.score},${state.shuffled}`;
  if (key !== badgeKey) { badgeKey = key; badge = call("m3Badge", rp, k, shownScore, comboLeft, comboBest); }
  return badge;
}
function hudInfo() {
  const s = pending ? pending.state : state, b = scoreBadge();
  const rgb = b.kind === "combo" || b.kind === "summary" ? styleRGB(comboStyle(b.n), pulse) : null;
  return { level: s.level, name: s.name, rules: s.rules || [], score: shownScore, moves: s.moves, goalText: goalText(s), goalIcon: s.goal.icon, sfx: sfxEnabled(), bgm: bgmEnabled(),
    progress: anim ? state.progress : s.progress, target: s.target, msg, undo: s.undo, busy,
    boss: anim ? state.boss : s.boss, pulse,   // Boss 血条与目标条一样：播放期间显示本步之前的读数
    daily: !!s.daily, dailyLabel: dailyLabel(dailyDate), dots: prog.dots, badge: b, comboColor: rgb && `rgb(${rgb.join(",")})`,
    boosters: s.boosters, tool, over: !!s.over };
}
function drawBackground(W, H) {
  ctx.fillStyle = "#1c1630"; ctx.fillRect(0, 0, W, H);
  const iw = art.bg.width, ih = art.bg.height, k = Math.max(W / iw, H / ih);
  ctx.drawImage(art.bg, (W - iw * k) / 2, (H - ih * k) / 2, iw * k, ih * k);
}
// 结局面板的操作提示（点棋盘 = 前进；有键盘时同时写出按键）
function overlayAction(tag) {
  const k = finePointer();
  if (tag === "LevelClear") return k ? "点棋盘或按 N / 空格 进入下一关" : "点棋盘进入下一关";
  if (tag === "Won") return k ? "点棋盘或按 N 从第 1 关重新开始" : "点棋盘从第 1 关重新开始";
  return k ? "点棋盘或按 R 重试 · U 撤销一步" : "点棋盘重试 · 「撤销」退一步";
}
let menuCache = null;
function curMenuLayout() {
  if (!menuCache || menuCache.L !== L) menuCache = { L, ml: menuLayout({ x: 0, y: 0, w: L.w, h: L.h }) };
  return menuCache.ml;
}
let mapCache = null;
function curMapLayout() {
  const a = { x: 0, y: 0, w: L.w, h: L.h };
  if (!mapCache || mapCache.L !== L) mapCache = { L, ml: mapLayout(a, CHAPTERS, levels.length), a };
  return mapCache;
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
  noteSpecials(v.st, seenSpecials);
  const boardRect = { x: PAD, y: PAD, w: L.cols * CELL, h: L.rows * CELL };
  drawnExtra = { banner: null, help: null, pause: null, map: null, menu: null };
  if (!anim && state.over) {
    const o = state.over;
    const title = { LevelClear: "过关！", Won: "通关！", Lost: "步数用完了" }[o.tag] || o.tag;
    const sub = o.tag === "Lost" ? `${state.loseHint}。可「撤销」或「重开」` : `得分 ${o.score}` + (o.tag === "LevelClear" ? `，进入第 ${o.next + 1} 关` : "");
    const stars = o.tag === "Lost" ? null : prog.stars, action = overlayAction(o.tag);
    const d = drawOverlay(ctx, art, boardRect, title, sub, stars, action);
    overlayDrawn = { title, sub, stars, action, panel: d.panel, starSprites: d.stars.map((s) => (s.on ? "star_on" : "star_off")) };
  } else {
    overlayDrawn = null;
    // 棋盘上沿横幅：道具点选模式（桌面 drawToolBannerArt）优先，其次第 1 关的提示（drawTipBannerArt）
    if (!paused && !mapOpen && !menuOpen) {
      if (tool) drawnExtra.banner = drawBanner(ctx, art, boardRect, TOOL[tool].banner, { icon: TOOL[tool].icon });
      else if (tipFrames > 0 && state.level === 0)
        drawnExtra.banner = drawBanner(ctx, art, boardRect, finePointer() ? "按 H 查看提示" : "点「提示」查看提示", { key: finePointer() ? "H" : null });
      // 按键条（桌面 drawHelpStripArt）：只在有键盘鼠标的设备上
      if (helpFrames > 0 && finePointer()) drawnExtra.help = drawHelpStrip(ctx, art, boardRect);
    }
  }
  ctx.restore();
  if (showGuide) {
    drawGuide(ctx, art, { x: L.board.x + PAD, y: L.board.y + PAD, w: L.cols * CELL, h: L.rows * CELL }, guideEntries(seenSpecials));
  }
  if (mapOpen) {
    const { ml, a } = curMapLayout();
    drawnExtra.map = drawMap(ctx, art, a, ml, levels, prog.dots, pulse, finePointer() ? "点击关卡进入 · M 关闭" : "点击关卡进入 · 点空白处关闭");
  }
  if (paused) drawnExtra.pause = drawPause(ctx, art, { x: 0, y: 0, w: L.w, h: L.h }, pulse);
  if (menuOpen) {
    const s = pending ? pending.state : state;
    drawnExtra.menu = drawMenu(ctx, art, { x: 0, y: 0, w: L.w, h: L.h }, curMenuLayout(),
      { boosters: s.boosters, tool, over: !!state.over, busy, sfx: sfxEnabled(), bgm: bgmEnabled(), keys: finePointer() });
  }
  // 窗口标题（桌面 updateTitle：Match3.View.titleLine + 「  |  」+ 最近提示）；PC 壳一般把它显示在标题栏
  const title = `${(pending ? pending.state : state).title}  |  ${msg}`;
  if (document.title !== title) document.title = title;
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
// 5. 输入：指针事件经同一布局变换映射；点选两格或按住向相邻方向拖划；播放中点击棋盘 = 加速；
//    结局后点棋盘 = 前进（同桌面）；道具点选模式下点格 = 用道具；暂停时点任意处继续；地图上点节点跳关、点别处关闭。
//    键盘（桌面 UI.Input.handleKey 的按键表）见 keydown；每个按键在触屏上都有对应按钮（暂停页列出）。
const isAdj = (a, b) => Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) === 1;
let drag = null;   // {cell, x, y}（设计单位）
function unitsOf(ev) { const r = canvas.getBoundingClientRect(); return toUnits(L, ev.clientX - r.left, ev.clientY - r.top); }
canvas.addEventListener("pointerdown", (ev) => {
  ev.preventDefault();
  unlock();
  const [x, y] = unitsOf(ev);
  if (menuOpen) { menuClick(x, y); return; }
  if (paused) { togglePause(); return; }
  if (mapOpen) { mapClick(x, y); return; }
  const b = buttonAtUnits(L, x, y);
  if (b) { pressed = b.id; return; }
  if (showGuide) { showGuide = false; return; }
  const p = cellAtUnits(L, x, y);
  if (!p) return;
  if (busy) { if (anim && anim.kind === "cascade") fastReq = true; return; }
  if (state.over) { advance(); return; }
  if (tool) { toolClick(p); return; }
  if (sel && isAdj(sel, p)) { const a = sel; drag = null; doSwap(a, p); return; }
  sel = p; drag = { cell: p, x, y };
  canvas.setPointerCapture?.(ev.pointerId);
});
canvas.addEventListener("pointermove", (ev) => {
  if (!drag || busy || tool) return;
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

// 菜单（常驻「菜单」按钮）：打开时冻结回放、清拖划 / 按下；与暂停 / 地图互斥
function toggleMenu() {
  menuOpen = !menuOpen; paused = false; mapOpen = false; drag = null; pressed = null;
  msg = menuOpen ? "菜单：点一项执行，点空白处继续" : "继续游戏";
}
// 菜单里点一项：先关菜单再执行（同按对应的键）；点空白处只关菜单
function menuClick(x, y) {
  const it = menuHit(curMenuLayout(), x, y);
  menuOpen = false;
  if (!it || it.id === "close") { msg = "继续游戏"; return; }
  if (it.id === "sfx") toggleSfx();
  else if (it.id === "bgm") toggleBgm();
  else onButton(it.id);
}
// 暂停（桌面 P：全屏按键说明，冻结动画，清掉拖划 / 选中；取消暂停后按键条再显示 240 帧）
function togglePause() {
  paused = !paused; mapOpen = false; menuOpen = false; drag = null; sel = null; swapFirst = null; pressed = null;
  msg = paused ? "已暂停：R 重开 · P / Esc / 点任意处继续" : "继续游戏";
  if (!paused) helpFrames = 240;
}
// 选关地图（桌面 M）
function toggleMap() {
  mapOpen = !mapOpen; paused = false; menuOpen = false; drag = null; pressed = null;
  msg = mapOpen ? "选关地图：点已解锁的关卡进入" : "已关闭选关地图";
}
// 地图上的点击（桌面 mapClick）：已解锁的别的关 → 跳过去；当前关 / 未解锁 / 节点外 → 关地图、保留当前进度
function mapClick(x, y) {
  const { ml } = curMapLayout(), i = mapHit(ml, x, y);
  mapOpen = false;
  if (i === null) { msg = "已关闭选关地图"; return; }
  const r = call("m3MapJump", reached, i);
  if (r.jump === null) { msg = i === state.level && !state.daily ? "已经在这一关" : `第 ${i + 1} 关还没解锁`; return; }
  newGame(r.jump, newSeed());
  msg = `选关：第 ${r.jump + 1} 关 ${state.name}`;
}
// Esc：关掉最上层的浮层（菜单 → 暂停 → 地图 → 本关说明 → 道具模式 → 选中）；退出程序是 PC 壳的事，网页不处理
function closeTop() {
  if (menuOpen) toggleMenu();
  else if (paused) togglePause();
  else if (mapOpen) toggleMap();
  else if (showGuide) showGuide = false;
  else if (tool) { msg = `已取消${TOOL[tool].name}`; tool = null; swapFirst = null; sel = null; }
  else sel = null;
}
// N / 回车 / 空格（桌面 keySpeedOrAdvance）：播放中加速，否则前进 / 重试
function speedOrAdvance() {
  if (busy) { if (!fastReq && anim && anim.kind === "cascade") msg = "加速回放"; fastReq = true; return; }
  advance();
}
window.addEventListener("keydown", (e) => {
  if (e.ctrlKey || e.metaKey || e.altKey || e.repeat) return;
  const k = e.key.length === 1 ? e.key.toLowerCase() : e.key;
  if (k === " " || k === "Enter" || k === "Escape") e.preventDefault();
  unlock();
  // 菜单开着时按快捷键：先关菜单再照常处理（Esc 只关菜单）
  if (menuOpen && k !== "Escape" && k.length === 1) menuOpen = false;
  // 任何时候都响应：Esc / P / K / B / R（暂停中也能重开，同桌面 handleKey）
  if (k === "Escape") return closeTop();
  if (k === "p") return togglePause();
  if (k === "k") return toggleSfx();
  if (k === "b") return toggleBgm();
  if (k === "r") return onButton("restart");
  if (paused) return;
  if (k === "m") return toggleMap();
  if (mapOpen) return;
  const act = {
    n: speedOrAdvance, Enter: speedOrAdvance, " ": speedOrAdvance,
    u: () => onButton("undo"), z: () => onButton("undo"), h: () => onButton("hint"),
    1: () => onButton("hammer"), 2: () => onButton("swap"), 3: () => onButton("cross"),
    s: () => onButton("shuffle"), d: () => onButton("daily"), "?": () => onButton("help"),
  }[k];
  if (act) act();
});

function onButton(id) {
  if (id === "menu") return toggleMenu();
  if (id === "pause") return togglePause();
  if (id === "map") return toggleMap();
  if (id === "restart" && paused) return restart();
  if (paused || mapOpen || busy) return;
  if (id === "prev") newGame((state.level + levels.length - 1) % levels.length, newSeed());
  else if (id === "next") {
    // 过关后「›」= 前进（带入剩余步数，同桌面 N）；其余时候自由选关
    if (state.over && state.over.tag === "LevelClear") advance();
    else newGame((state.level + 1) % levels.length, newSeed());
  } else if (id === "restart") restart();
  else if (id === "help") showGuide = !showGuide;
  else if (id === "hint") { showHint = state.hint; msg = showHint ? "提示：交换高亮的两格" : "没有可走的步：菜单 → 洗牌（S）"; }
  else if (id === "undo") {
    // 撤销：历史在核心的 Engine.History（最多 20 步，终局后也能撤销）
    const res = call("m3Undo");
    if (!res.accepted) { msg = "没有可撤销的步"; return; }
    state = res.state; sel = null; showHint = null; shownScore = state.score; fx.clear();
    tool = null; swapFirst = null; comboLeft = 0;
    msg = `已撤销（还可撤销 ${state.undo} 步）`;
    refreshProgress();
  } else if (TOOL[id]) toggleTool(id);
  else if (id === "shuffle") doShuffle();
  else if (id === "daily") startDaily(dailyParam ?? today());
}

// ---------------------------------------------------------------------------
// 6. 开局与调试钩子：?level=（0 起）&seed=；?daily=YYYY-MM-DD 开当天的每日挑战；?showcase=1 元素展示盘（桌面 MATCH3_SHOWCASE）
const q = new URLSearchParams(location.search);
const dm = /^(\d{4})-(\d{1,2})-(\d{1,2})$/.exec(q.get("daily") ?? "");
const dailyParam = dm ? [+dm[1], +dm[2], +dm[3]] : null;
newGame(+(q.get("level") ?? 0), +(q.get("seed") ?? 20260929));
if (dailyParam) startDaily(dailyParam);
if (q.get("showcase") === "1") begin(call("m3Showcase").state, "元素展示盘：各种元素各摆一格（桌面 MATCH3_SHOWCASE）");
function debugInfo() {
  const tk = anim && anim.kind === "cascade" ? anim.tk : null;
  return { kind: anim ? anim.kind : null, p: tk?.p ?? null, fr: tk?.fr ?? anim?.frame ?? 0, n: tk?.n ?? 0, w: tk?.w ?? 0, k: tk?.k ?? 0,
    stage: tk?.s?.kind ?? null, blast: !!(tk && anim.cas.blast[tk.w]), pulse, frames, events: tk?.ev ?? null };
}
// 自动化钩子：只读状态 + 断点（breakWhen(info) 为真时冻结帧循环，截图后置 frozen=false 继续）；操作仍走真实指针 / 键盘事件
window.m3debug = {
  get state() { return state; }, get pending() { return pending; }, get busy() { return busy; }, get anim() { return debugInfo(); },
  get layout() { return L; }, get hud() { return hudDrawn; }, get overlay() { return overlayDrawn; }, get levels() { return levels.length; },
  // 走几何降级的次数（按元素名，见 cells.js 的 fallbacks）；图集加载后应一直为空，e2e 每关检查
  get fallbacks() { return { ...fallbacks }; },
  // 最近一帧画的掉落口标记（cells.js 的 dropMarks：{seq, marks:[{p,x,y}]}，棋盘设计坐标）
  get dropMarks() { return { seq: dropMarks.seq, marks: dropMarks.marks.map((m) => ({ ...m, p: [...m.p] })) }; }, get dpr() { return dpr; }, perf, breakWhen: null,
  get frozen() { return frozen; }, set frozen(v) { frozen = v; },
  // 迁自桌面版的界面状态（e2e 用）
  get ui() {
    return { tool, swapFirst, sel, hint: showHint, paused, mapOpen, menuOpen, showGuide, startMoves, reached, progress: { ...prog }, tipFrames, helpFrames, comboLeft, comboBest,
      daily: dailyLabel(dailyDate), msg, title: document.title, badge, drawn: drawnExtra, fine: finePointer() };
  },
  cellCenter: (r, c) => cellCenterCss(L, [r, c]),
  buttonCenter: (id) => { const b = L.buttons.find((x) => x.id === id); return [L.ox + (b.x + b.w / 2) * L.u, L.oy + (b.y + b.h / 2) * L.u]; },
  // 菜单项中心（菜单要先打开；返回 CSS 坐标）
  menuItemCenter: (id) => { const it = curMenuLayout().items.find((x) => x.id === id); return [L.ox + (it.x + it.w / 2) * L.u, L.oy + (it.y + it.h / 2) * L.u]; },
  mapNodeCenter: (i) => { const nd = curMapLayout().ml.nodes.find((d) => d.i === i); return [L.ox + nd.x * L.u, L.oy + nd.y * L.u]; },
};
requestAnimationFrame((ts) => { last = ts; loop(ts); });
