// 棋盘动画绘制（网页版）：对应桌面 app/UI/Cascade.hs（交换补间 / 逐轮 高亮→消失→下落→落定 / 轻落）、
// app/UI/EndStage.hs（步末：倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌）与 UI/Playback.hs 的粒子、浮字、震屏。
// 只画插值，不算规则：阶段、帧号、轮次由 wasm 里的 ComboFx 阶段机（m3AnimTick）给出，盘面快照来自 m3Swap。
// 全部坐标为棋盘设计单位（格 56，见 cells.js）。
import {
  CELL, PAD, ROWS, COLS, ELEMENT_RGB, boardW, boardH, breathe, cellRGB, clamp, drawBoardBase, drawCell,
  drawCellScaled, drawUfos, origin, snailPose, spreadTargets,
} from "./cells.js";

// 时间线常量（帧，60 fps；UI.Types / ComboFx）
export const SWAP_FRAMES = 10, FALL_FRAMES = 12, SHAKE_FRAMES = 10, COMBO_POP_LIFE = 54, SCORE_POP_LIFE = 48;

const smoothT = (x) => { const y = clamp(0, 1, x); return y * y * (3 - 2 * y); };
const easeOutT = (x) => { const y = clamp(0, 1, x); return 1 - (1 - y) * (1 - y); };
const lerp = (a, b, e) => a + (b - a) * e;
const cellsOf = () => { const o = []; for (let r = 0; r < ROWS; r++) for (let c = 0; c < COLS; c++) o.push([r, c]); return o; };
const same = (a, b) => a[0] === b[0] && a[1] === b[1];
const inList = (p, list) => list.some((q) => same(p, q));

// 连击等级样式（ComboFx.comboStyle）；x5+ 色相流转（量化到 10° 以便着色缓存命中）
export function comboStyle(k) {
  if (k <= 2) return { rgb: [255, 238, 150], h: 30, shake: 2, rainbow: false };
  if (k === 3) return { rgb: [255, 164, 52], h: 36, shake: 3, rainbow: false };
  if (k === 4) return { rgb: [255, 76, 64], h: 42, shake: 4, rainbow: false };
  return { rgb: [214, 120, 255], h: 48, shake: 5, rainbow: true };
}
function hsv(h, s, v) {
  const c = v * s, hp = h / 60, x = c * (1 - Math.abs((hp % 2) - 1)), m = v - c;
  const [r, g, b] = hp < 1 ? [c, x, 0] : hp < 2 ? [x, c, 0] : hp < 3 ? [0, c, x] : hp < 4 ? [0, x, c] : hp < 5 ? [x, 0, c] : [c, 0, x];
  return [r, g, b].map((u) => clamp(0, 255, Math.round((u + m) * 255)));
}
export function styleRGB(st, pulse) { return st.rainbow ? hsv(Math.floor(((pulse * 9) % 360) / 10) * 10, 0.55, 1) : st.rgb; }
const waveTint = (k, pulse) => (k <= 1 ? [255, 250, 220] : styleRGB(comboStyle(k), pulse));

// ---------------------------------------------------------------------------
// 静止盘面：底层 → 提示光 → 棋子 → 选中框 → 蔓延预告（仅静止时）→ 飞碟（UI.BoardArt.drawStaticArt）
export function drawStatic(ctx, art, v, board, yOff = 0) {
  drawBoardBase(ctx, art, v.st, v.pulse);
  const hintA = Math.round(120 + 135 * breathe(v.pulse, 60));
  for (const p of v.hint || []) { const [x, y] = origin(p); art.mod(ctx, "hint_glow", x - 3, y - 3, CELL + 6, CELL + 6, null, hintA); }
  for (const p of cellsOf()) { const [x, y] = origin(p); drawCell(ctx, art, v.pulse, x, y + yOff, board[p[0]][p[1]]); }
  for (const p of v.hint || []) { const [x, y] = origin(p); art.add(ctx, "hint_glow", x, y, CELL, CELL, [255, 230, 150], Math.floor(hintA / 3)); }
  if (v.sel) {
    const [x, y] = origin(v.sel), g = Math.round(2 * breathe(v.pulse, 30));
    art.mod(ctx, "sel_ring", x - 2 - g, y - 2 - g, CELL + 4 + 2 * g, CELL + 4 + 2 * g, null, 255);
  }
  if (!v.busy) {
    const a = Math.round(70 + 110 * breathe(v.pulse, 60));
    for (const p of spreadTargets(board, "vine")) { const [x, y] = origin(p); art.mod(ctx, "hint_glow", x, y + yOff, CELL, CELL, [90, 255, 120], a); }
    for (const p of spreadTargets(board, "choco")) { const [x, y] = origin(p); art.mod(ctx, "hint_glow", x, y + yOff, CELL, CELL, [210, 120, 60], a); }
  }
  drawUfos(ctx, art, v.st, v.pulse, yOff);
}

// 底层 + 除 hidden 以外的格
function drawCellsExcept(ctx, art, v, board, hidden) {
  drawBoardBase(ctx, art, v.st, v.pulse);
  for (const p of cellsOf()) if (!inList(p, hidden)) { const [x, y] = origin(p); drawCell(ctx, art, v.pulse, x, y, board[p[0]][p[1]]); }
  drawUfos(ctx, art, v.st, v.pulse);
}

function veil(ctx, alpha) {
  ctx.fillStyle = `rgba(8,6,24,${alpha / 255})`;
  ctx.fillRect(PAD, PAD, boardW(), boardH());
}
function clipBoard(ctx) { ctx.save(); ctx.beginPath(); ctx.rect(PAD, PAD, boardW(), boardH()); ctx.clip(); }

// ---------------------------------------------------------------------------
// 交换补间：两格沿直线互换位置
export function drawSwap(ctx, art, v, board, a, b, t) {
  drawCellsExcept(ctx, art, v, board, [a, b]);
  const [x1, y1] = origin(a), [x2, y2] = origin(b);
  drawCell(ctx, art, v.pulse, lerp(x1, x2, t), lerp(y1, y2, t), board[a[0]][a[1]]);
  drawCell(ctx, art, v.pulse, lerp(x2, x1, t), lerp(y2, y1, t), board[b[0]][b[1]]);
}

// 轻落：整盘从上方约 0.35 格处落下（洗牌与回放兜底；UI.Cascade.drawFall）
export function drawLightFall(ctx, art, v, board, t) {
  const ease = 1 - (1 - t) * (1 - t);
  drawStatic(ctx, art, v, board, Math.round(CELL * (1 - ease) * -0.35));
}

// ---------------------------------------------------------------------------
// 逐轮回放：cas = { boards, waves(trace.waves), ends(trace.end), fall, cleared[w] }，tk = 最近一帧 m3AnimTick
export function drawCascade(ctx, art, v, cas, tk) {
  const t = tk.n > 0 ? Math.min(1, tk.fr / tk.n) : 1;
  if (tk.p === "end" && tk.s) return drawEndStage(ctx, art, v, cas, tk.s, t);
  const w = cas.waves[tk.w];
  if (!w) return drawStatic(ctx, art, v, cas.boards[tk.b]);
  const cleared = cas.cleared[tk.w] || [];
  switch (tk.p) {
    case "start": return drawStatic(ctx, art, v, w.before);
    case "flash": return drawWaveFlash(ctx, art, v, w, cleared, tk.k, t);
    case "pop": return drawWavePop(ctx, art, v, w, cleared, tk.k, t);
    case "fall": return drawWaveFall(ctx, art, v, w, cas.fall[tk.w], t);
    default: return drawStatic(ctx, art, v, w.after);
  }
}

// 高亮：整盘压暗，被消格提到暗幕之上，闪两下 + 轻微弹跳 + 等级色光圈
function drawWaveFlash(ctx, art, v, w, cleared, k, t) {
  const tint = waveTint(k, v.pulse), blink = 0.5 + 0.5 * Math.cos(t * 2 * Math.PI * 2), bounce = Math.round(3 * Math.sin(t * Math.PI));
  drawStatic(ctx, art, { ...v, hint: null, sel: null, busy: true }, w.before);
  veil(ctx, Math.round(Math.min(1, t * 4) * 120));
  for (const p of cleared) {
    const [x, y0] = origin(p), y = y0 - bounce;
    art.add(ctx, "spark", x - 12, y - 12, CELL + 24, CELL + 24, tint, Math.round(90 + 120 * blink));
    drawCell(ctx, art, v.pulse, x, y, w.before[p[0]][p[1]]);
    art.add(ctx, "spark", x, y, CELL, CELL, [255, 255, 255], Math.round(80 * blink));
    art.mod(ctx, "sel_ring", x - 2, y - 2, CELL + 4, CELL + 4, tint, 235);
  }
}

// 消失：被消格缩小淡出 + 光环外扩；本轮生成的特殊块从中心放大出现
function drawWavePop(ctx, art, v, w, cleared, k, t) {
  const tint = waveTint(k, v.pulse);
  drawBoardBase(ctx, art, v.st, v.pulse);
  for (const p of cellsOf()) {
    const h = w.holes[p[0]][p[1]];
    if (h && !inList(p, cleared)) { const [x, y] = origin(p); drawCell(ctx, art, v.pulse, x, y, h); }
  }
  veil(ctx, Math.round((1 - t) * 120));
  for (const p of cleared) {
    const [x, y] = origin(p), cx = x + CELL / 2, cy = y + CELL / 2, ring = Math.round(CELL * (1 + 0.9 * t));
    art.add(ctx, "spark", cx - ring / 2, cy - ring / 2, ring, ring, tint, Math.round(255 * (1 - t)));
    drawCellScaled(ctx, art, cx, cy, 1.1 * (1 - t) * (1 - t) + 0.02, Math.round(255 * (1 - t)), w.before[p[0]][p[1]]);
    const h = w.holes[p[0]][p[1]];
    if (h) drawCellScaled(ctx, art, cx, cy, Math.max(0.05, t), 255, h);
  }
}

// 下落 + 补子：按列复现重力（ComboFx.fallTable：每格 = 下落行数×2 + 是否新补），加速度下落；新格从上沿外落入（裁剪）
function drawWaveFall(ctx, art, v, w, table, t) {
  const e = t * t;
  drawBoardBase(ctx, art, v.st, v.pulse);
  clipBoard(ctx);
  for (const p of cellsOf()) {
    const d = table ? table[p[0]][p[1]] >> 1 : 0, [x, y] = origin(p);
    drawCell(ctx, art, v.pulse, x, y - Math.round(d * CELL * (1 - e)), w.after[p[0]][p[1]]);
  }
  ctx.restore();
  drawUfos(ctx, art, v.st, v.pulse);
}

// ---------------------------------------------------------------------------
// 步末阶段：s = {kind, e:[trace.end 下标], b0, b1, n}
function stageMoves(cas, s) {
  const out = [];
  for (const i of s.e) {
    const ef = cas.ends[i].effect;
    if (ef.type === "tick") for (const p of ef.cells) out.push([p, p]);
    else if (ef.type === "snail") for (const m of ef.moves) out.push([m.from, m.to]);
    else for (const pr of ef.pairs) out.push(pr);
  }
  return out;
}
const SPREAD_PROGRESS = {
  vine: (t) => { const u = t * 4, seg = Math.floor(u); return Math.min(1, (seg + smoothT(u - seg)) / 4); },
  choco: easeOutT,
  steam: (t) => t,
};

function drawEndStage(ctx, art, v, cas, s, t) {
  const before = cas.boards[s.b0], after = cas.boards[s.b1];
  const moves = stageMoves(cas, s);
  if (s.kind === "tick") {
    // 倒计时减一：前半段旧数字、后半段新数字，红光脉冲 + 数字放大回弹
    const board = t < 0.5 ? before : after, k = Math.sin(Math.PI * t);
    drawStatic(ctx, art, { ...v, busy: true }, board);
    for (const [p] of moves) {
      const [x, y] = origin(p), g = Math.round(10 * k), cell = board[p[0]][p[1]];
      art.add(ctx, "spark", x - 8, y - 8, CELL + 16, CELL + 16, [255, 90, 60], Math.round(200 * k));
      if (cell.t === "countdown") art.draw(ctx, `countdown_${clamp(1, 9, cell.n)}`, x - g, y - g, CELL + 2 * g, CELL + 2 * g);
    }
  } else if (s.kind === "belt") {
    // 皮带移位：相邻格平滑滑过去；首尾相接的那一格在终点缩放淡入
    const e = smoothT(t);
    drawCellsExcept(ctx, art, v, before, moves.map((m) => m[1]));
    clipBoard(ctx);
    for (const [o, d] of moves) {
      const cell = before[o[0]][o[1]], [x0, y0] = origin(o), [x1, y1] = origin(d);
      if (Math.abs(o[0] - d[0]) + Math.abs(o[1] - d[1]) === 1) drawCell(ctx, art, v.pulse, lerp(x0, x1, e), lerp(y0, y1, e), cell);
      else drawCellScaled(ctx, art, x1 + CELL / 2, y1 + CELL / 2, Math.max(0.05, e), Math.round(255 * e), cell);
    }
    ctx.restore();
  } else if (s.kind === "spread") {
    // 蔓延：新格的覆盖层从来源格那一侧「长」过来；生长前沿带同色柔光
    drawStatic(ctx, art, { ...v, busy: true }, before);
    for (const i of s.e) {
      const ef = cas.ends[i].effect, name = ef.kind;
      for (const [src, q] of ef.pairs) {
        const [x, y] = origin(q), prog = (SPREAD_PROGRESS[name] || ((u) => u))(t);
        const w = Math.max(1, Math.round(CELL * prog)), dr = q[0] - src[0], dc = q[1] - src[1];
        let clip, front;
        if (dc === 1) { clip = [x, y, w, CELL]; front = [x + w - 10, y - 4, 20, CELL + 8]; }
        else if (dc === -1) { clip = [x + CELL - w, y, w, CELL]; front = [x + CELL - w - 10, y - 4, 20, CELL + 8]; }
        else if (dr === 1) { clip = [x, y, CELL, w]; front = [x - 4, y + w - 10, CELL + 8, 20]; }
        else if (dr === -1) { clip = [x, y + CELL - w, CELL, w]; front = [x - 4, y + CELL - w - 10, CELL + 8, 20]; }
        else { const h = w / 2, cx = x + CELL / 2, cy = y + CELL / 2; clip = front = [cx - h, cy - h, 2 * h, 2 * h]; }
        ctx.save(); ctx.beginPath(); ctx.rect(...clip); ctx.clip();
        drawCell(ctx, art, v.pulse, x, y, after[q[0]][q[1]]);
        ctx.restore();
        art.add(ctx, "spark", ...front, ELEMENT_RGB[name] || [255, 255, 255], Math.round(220 * (1 - t) + 30));
      }
    }
  } else if (s.kind === "snail") {
    // 蜗牛：沿爬行方向平滑挪一格（轻微一拱），被推的宝石退到蜗牛原格；碰壁原地翻身掉头
    const ms = []; for (const i of s.e) { const ef = cas.ends[i].effect; if (ef.type === "snail") ms.push(...ef.moves); }
    const e = smoothT(t);
    drawCellsExcept(ctx, art, v, before, ms.flatMap((m) => [m.from, m.to]));
    for (const m of ms) {
      const [x0, y0] = origin(m.from), [x1, y1] = origin(m.to);
      if (same(m.from, m.to)) {
        const old = before[m.from[0]][m.from[1]], sq = Math.abs(Math.cos(Math.PI * t));
        const w = Math.max(2, Math.round(CELL * sq)), hop = Math.round(4 * Math.sin(Math.PI * t));
        const [dr, dc] = t < 0.5 && old.t === "snail" ? [old.dr, old.dc] : m.dir;
        const [ang, flip] = snailPose(dr, dc);
        art.ex(ctx, "snail", x0 + (CELL - w) / 2, y0 - hop, w, CELL, ang, flip);
      } else {
        const side = Math.round(9 * Math.sin(Math.PI * t)), [sx, sy] = y0 === y1 ? [0, side] : [side, 0];
        if (m.pushed) drawCell(ctx, art, v.pulse, lerp(x1, x0, e) + sx, lerp(y1, y0, e) + sy, m.pushed);
        const hop = Math.round(5 * Math.sin(Math.PI * t)), [ang, flip] = snailPose(m.dir[0], m.dir[1]);
        art.ex(ctx, "snail", lerp(x0, x1, e), lerp(y0, y1, e) - hop, CELL, CELL, ang, flip);
      }
    }
  } else if (s.kind === "shuffle") {
    // 自动洗牌：旧盘向中心收拢并被暗幕盖住 → 新盘从中心散开、暗幕褪去
    const [board, k] = t < 0.5 ? [before, smoothT(t * 2)] : [after, 1 - smoothT(t * 2 - 1)];
    const cx = PAD + boardW() / 2, cy = PAD + boardH() / 2;
    drawBoardBase(ctx, art, v.st, v.pulse);
    for (const p of cellsOf()) {
      const [x, y] = origin(p);
      drawCell(ctx, art, v.pulse, lerp(x, cx - CELL / 2, 0.3 * k), lerp(y, cy - CELL / 2, 0.3 * k), board[p[0]][p[1]]);
    }
    drawUfos(ctx, art, v.st, v.pulse);
    ctx.fillStyle = `rgba(20,12,40,${(230 * k) / 255})`;
    ctx.fillRect(PAD, PAD, boardW(), boardH());
    const sz = Math.max(boardW(), boardH()) * (0.2 + 0.5 * k);
    art.add(ctx, "spark", cx - sz / 2, cy - sz / 2, sz, sz, [200, 150, 255], Math.round(160 * k));
  } else drawStatic(ctx, art, v, after);
}

// ---------------------------------------------------------------------------
// 粒子 / 浮字 / 震屏（UI.Playback；随机数用 JS 自己的，只影响观感）
export class Fx {
  constructor() { this.parts = []; this.pops = []; this.shake = 0; this.amp = 0; }
  clear() { this.parts = []; this.pops = []; this.shake = 0; }
  tick() {
    for (const p of this.parts) { p.x += p.vx; p.y += p.vy; p.vy += 0.18; p.life--; }
    this.parts = this.parts.filter((p) => p.life > 0);
    for (const p of this.pops) p.age++;
    this.pops = this.pops.filter((p) => p.age < p.life);
    this.shake = Math.max(0, this.shake - 1);
  }
  // 被消格中心迸出的粒子（颜色取消除前的盘面）
  burst(board, cells) {
    for (const [r, c] of cells) {
      const [ox, oy] = origin([r, c]), rgb = cellRGB(board[r][c]);
      for (let i = 0; i < 5; i++) this.spawn(ox + CELL / 2, oy + CELL / 2, rgb, 1.2, 4.5, 18, 36, 3, 7, -1.5);
    }
  }
  // 小颗碎屑（步末：蔓延同色 / 倒计时火星）
  crumbs(rgb, cells) {
    for (const [r, c] of cells) {
      const [ox, oy] = origin([r, c]);
      for (let i = 0; i < 3; i++) this.spawn(ox + CELL / 2, oy + CELL / 2, rgb, 0.6, 2.0, 14, 24, 2, 4, -1.0);
    }
  }
  spawn(x, y, rgb, s0, s1, l0, l1, z0, z1, up) {
    const ang = Math.random() * 2 * Math.PI, spd = s0 + Math.random() * (s1 - s0);
    const life = l0 + Math.floor(Math.random() * (l1 - l0 + 1)), size = z0 + Math.floor(Math.random() * (z1 - z0 + 1));
    this.parts.push({ x, y, vx: Math.cos(ang) * spd, vy: Math.sin(ang) * spd + up, life, max: life, rgb, size });
  }
  // 「连击 xN」：放在本轮消除区域上方（放不下放下方），水平夹在棋盘内；旧连击弹字加速淡出
  comboPop(k, cleared) {
    const [, cc, rTop, rBot] = anchor(cleared), h = comboStyle(k).h, halfW = h * 1.7;
    const x = clamp(PAD + halfW, PAD + boardW() - halfW, PAD + (cc + 0.5) * CELL);
    const above = PAD + rTop * CELL - h * 0.75, below = PAD + (rBot + 1) * CELL + h * 0.75;
    const y = above - h * 0.65 >= PAD - 4 ? above : below + h * 0.65 <= PAD + boardH() + 4 ? below : PAD + h * 0.7;
    for (const p of this.pops) if (p.kind === "combo") p.age = Math.max(p.age, p.life - 10);
    this.pops.push({ kind: "combo", k, x, y, age: 0, life: COMBO_POP_LIFE });
  }
  scorePop(n, k, cleared) {
    const [cr, cc] = anchor(cleared);
    this.pops.push({ kind: "score", n, k, x: PAD + (cc + 0.5) * CELL, y: PAD + (cr + 0.5) * CELL, age: 0, life: SCORE_POP_LIFE });
  }
  startShake(k) { this.shake = SHAKE_FRAMES; this.amp = comboStyle(k).shake; }
  shakeOffset(pulse) {
    if (this.shake <= 0 || this.amp <= 0) return [0, 0];
    const a = this.amp * (this.shake / SHAKE_FRAMES);
    return [Math.round(a * Math.sin(pulse * 2.3)), Math.round(a * Math.cos(pulse * 3.1))];
  }
  draw(ctx, art, pulse, font) {
    for (const p of this.parts) {
      const s = p.size * 3;
      art.mod(ctx, "spark", p.x - s / 2, p.y - s / 2, s, s, p.rgb, Math.floor((255 * p.life) / p.max));
    }
    for (const p of this.pops) {
      const left = p.life - p.age, alpha = left >= 16 ? 1 : Math.max(0, left / 16);
      let text, size, rgb, rise;
      if (p.kind === "combo") {
        const st = comboStyle(p.k);
        size = st.h * comboPopScale(p.age); rgb = styleRGB(st, pulse); text = `连击 x${p.k}`;
        rise = p.age < 12 ? 0 : (p.age - 12) * 0.35;
      } else {
        size = 22 * (p.age < 5 ? 0.6 + (0.4 * p.age) / 5 : 1); rgb = p.k >= 2 ? styleRGB(comboStyle(p.k), pulse) : [255, 255, 255];
        text = `+${p.n}`; rise = p.age * 0.9;
      }
      ctx.save();
      ctx.globalAlpha = alpha;
      ctx.font = `900 ${size.toFixed(1)}px ${font}`;
      ctx.textAlign = "center"; ctx.textBaseline = "middle";
      ctx.lineJoin = "round"; ctx.lineWidth = Math.max(2, size / 7); ctx.strokeStyle = "rgba(30,14,40,.9)";
      ctx.strokeText(text, p.x, p.y - rise);
      ctx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
      ctx.fillText(text, p.x, p.y - rise);
      ctx.restore();
    }
  }
}
function comboPopScale(age) {
  if (age < 7) { const t = age / 7, e = 1 - (1 - t) * (1 - t); return 0.35 + (1.3 - 0.35) * e; }
  if (age < 12) return 1.3 - (0.3 * (age - 7)) / 5;
  return 1;
}
function anchor(ps) {
  if (!ps.length) return [3.5, 3.5, 3, 4];
  const n = ps.length;
  return [ps.reduce((s, p) => s + p[0], 0) / n, ps.reduce((s, p) => s + p[1], 0) / n, Math.min(...ps.map((p) => p[0])), Math.max(...ps.map((p) => p[0]))];
}
