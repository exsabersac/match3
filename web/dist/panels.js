// 全屏浮层：菜单（网页自有：常驻按钮之外的全部操作）、暂停说明（从桌面 app/UI/HudArt.drawPauseHelpArt 迁来）
// 与选关地图（app/UI/LevelMap.drawLevelMapArt）。
// 盖住整个布局（area = {x, y, w, h}，设计单位），随布局缩放；竖排一列、横排两列。
// 只画与命中，不算规则：能否跳关由核心 m3MapJump（Match3.Core.mapClickJump）判，进度点由 m3Progress 给。
import { FONT, keyCap } from "./hud.js";

function text(ctx, s, x, y, size, color, align = "left", weight = 700) {
  ctx.font = `${weight} ${size}px ${FONT}`;
  ctx.textAlign = align; ctx.textBaseline = "middle";
  ctx.fillStyle = color;
  ctx.fillText(s, x, y);
}
function veil(ctx, a, rgba) { ctx.fillStyle = rgba; ctx.fillRect(a.x, a.y, a.w, a.h); }

// ---------------------------------------------------------------------------
// 菜单：常驻按钮只有 撤销 / 提示 / 菜单，其余操作都在这里（键盘快捷键照旧，键名写在每项右上角）。
// 盖住整个布局；竖排 3 列、横排 5 列，最后一项「继续游戏」占满最后一行剩下的格。每项都远大于 44 CSS px（e2e 5a 核对）。
// booster：道具项（画图标 + 剩余次数，0 次 / 结局后变灰，当前点选模式金框）；toggle：开关项（画「开 / 关」）。
export const MENU_ITEMS = [
  { id: "hammer", label: "锤子", key: "1", booster: "hammer", icon: "icon_hammer" },
  { id: "swap", label: "自由交换", key: "2", booster: "swap", icon: "icon_swap" },
  { id: "cross", label: "十字消", key: "3", booster: "cross", icon: "icon_cross" },
  { id: "shuffle", label: "洗牌", key: "S" }, { id: "restart", label: "重开本关", key: "R" }, { id: "daily", label: "每日挑战", key: "D" },
  { id: "map", label: "选关地图", key: "M" }, { id: "prev", label: "上一关", key: null }, { id: "next", label: "下一关", key: null },
  { id: "help", label: "本关说明", key: "?" }, { id: "pause", label: "按键说明", key: "P" },
  { id: "sfx", label: "音效", key: "K", toggle: true }, { id: "bgm", label: "音乐", key: "B", toggle: true },
  { id: "close", label: "继续游戏", key: "Esc" },
];
const MENU_HEAD = 64, MENU_M = 16, MENU_PAD = 14, MENU_GAP = 10;
// 菜单各项的矩形（设计单位）；a = 整个布局矩形
export function menuLayout(a) {
  const cols = a.w / a.h > 1.2 ? 5 : 3, n = MENU_ITEMS.length, rows = Math.ceil(n / cols);
  const px = a.x + MENU_M, py = a.y + MENU_M, pw = a.w - 2 * MENU_M, ph = a.h - 2 * MENU_M;
  const gx = px + MENU_PAD, gy = py + MENU_HEAD, gw = pw - 2 * MENU_PAD, gh = ph - MENU_HEAD - MENU_PAD;
  const iw = (gw - (cols - 1) * MENU_GAP) / cols, ih = Math.min(120, (gh - (rows - 1) * MENU_GAP) / rows);
  const items = MENU_ITEMS.map((it, i) => {
    const r = Math.floor(i / cols), c = i % cols, last = i === n - 1, span = last ? cols - c : 1;
    return { ...it, x: gx + c * (iw + MENU_GAP), y: gy + r * (ih + MENU_GAP), w: iw * span + MENU_GAP * (span - 1), h: ih };
  });
  return { panel: { x: px, y: py, w: pw, h: ph }, items, cols, rows };
}
export function menuHit(mlay, x, y) {
  return mlay.items.find((b) => x >= b.x && x < b.x + b.w && y >= b.y && y < b.y + b.h) || null;
}
// st：{boosters, tool, over, busy, sfx, bgm, keys}（keys = 有键盘时画键名）。返回画出的各项（e2e 用）。
export function drawMenu(ctx, art, a, mlay, st) {
  veil(ctx, a, "rgba(8,6,20,.80)");
  const p = mlay.panel;
  if (!art.panel(ctx, "panel_gold", p.x, p.y, p.w, p.h, 18)) { ctx.fillStyle = "#2a2148"; ctx.fillRect(p.x, p.y, p.w, p.h); }
  text(ctx, "菜单", p.x + p.w / 2, p.y + 26, 26, "#ffe082", "center", 900);
  text(ctx, st.keys ? "点一项执行 · Esc / 点空白处 继续" : "点一项执行 · 点空白处继续", p.x + p.w / 2, p.y + 50, 13, "rgba(255,248,225,.8)", "center", 600);
  const drawn = [];
  for (const it of mlay.items) {
    const left = it.booster ? st.boosters?.[it.booster] ?? 0 : null;
    const active = !!it.booster && st.tool === it.id;
    const on = it.toggle ? st[it.id] !== false : null;
    const enabled = it.booster ? left > 0 && !st.over && !st.busy
      : it.id === "shuffle" ? !st.over && !st.busy
      : ["restart", "daily", "prev", "next", "help"].includes(it.id) ? !st.busy : true;
    ctx.save();
    ctx.globalAlpha = enabled || active ? 1 : 0.45;
    const name = active || it.id === "close" ? "panel_gold" : "panel_chip";
    if (!art.panel(ctx, name, it.x, it.y, it.w, it.h, 14)) { ctx.fillStyle = "#3a3060"; ctx.fillRect(it.x, it.y, it.w, it.h); }
    if (active) { ctx.lineWidth = 3; ctx.strokeStyle = "#ffd65a"; ctx.strokeRect(it.x + 2, it.y + 2, it.w - 4, it.h - 4); }
    const cx = it.x + it.w / 2, fs = Math.max(14, Math.min(20, it.w / 6));
    let value = null;
    if (it.booster) {
      const s = Math.min(36, it.h * 0.42);
      const drewIcon = art.draw(ctx, it.icon, cx - s - 2, it.y + it.h * 0.42 - s / 2, s, s);
      value = `×${left}`;
      text(ctx, value, drewIcon ? cx + 4 : cx, it.y + it.h * 0.42, 20, left > 0 ? "#fff" : "#9a94c0", drewIcon ? "left" : "center", 800);
      text(ctx, it.label, cx, it.y + it.h * 0.78, fs, "#fff8e1", "center", 700);
    } else if (it.toggle) {
      value = on ? "开" : "关";
      text(ctx, it.label, cx, it.y + it.h * 0.38, fs, "#fff8e1", "center", 700);
      text(ctx, value, cx, it.y + it.h * 0.7, 18, on ? "#9be89b" : "#ff9a9a", "center", 800);
    } else text(ctx, it.label, cx, it.y + it.h / 2, fs, it.id === "close" ? "#ffe9a8" : "#fff8e1", "center", 800);
    if (st.keys && it.key) {
      ctx.font = `800 12px ${FONT}`;
      const kw = Math.max(18, ctx.measureText(it.key).width + 8);
      keyCap(ctx, art, it.x + it.w - kw - 4, it.y + 4, it.key, 18);
    }
    ctx.restore();
    drawn.push({ id: it.id, label: it.label, value, enabled, active, x: it.x, y: it.y, w: it.w, h: it.h });
  }
  return { panel: p, items: drawn, cols: mlay.cols };
}

// ---------------------------------------------------------------------------
// 暂停：每个按键一行（键帽 + 作用 + 触屏上对应的按钮），底部宝石图例（gem_c1..5，同桌面「图例」行）
// 与桌面 rows 同序；第三列是网页 / 触屏入口（PC 壳和手机都没有键盘时用）。
export const PAUSE_ROWS = [
  ["H", "提示", "「提示」"], ["1", "锤子", "菜单 → 锤子"], ["2", "自由交换", "菜单 → 自由交换"], ["3", "十字消", "菜单 → 十字消"],
  ["U", "撤销", "「撤销」"], ["S", "洗牌", "菜单 → 洗牌"], ["D", "每日挑战", "菜单 → 每日挑战"], ["M", "选关地图", "菜单 → 选关地图"],
  ["K", "音效", "菜单 → 音效"], ["B", "BGM", "菜单 → 音乐"], ["R", "重开本关", "菜单 → 重开本关"],
  ["N", "下一关", "点结算面板"], ["P", "继续游戏", "点任意处"],
];
export function drawPause(ctx, art, a, pulse) {
  veil(ctx, a, "rgba(8,6,20,.84)");
  const m = 16, px = a.x + m, py = a.y + m, pw = a.w - 2 * m, ph = a.h - 2 * m;
  if (!art.panel(ctx, "panel_gold", px, py, pw, ph, 18)) { ctx.fillStyle = "#2a2148"; ctx.fillRect(px, py, pw, ph); }
  text(ctx, "暂停", a.x + a.w / 2, py + 32, 32, "#ffe082", "center", 900);
  text(ctx, "按键 · 触屏操作（Esc / 点任意处 继续）", a.x + a.w / 2, py + 62, 14, "rgba(255,248,225,.8)", "center", 600);
  const cols = pw >= 600 ? 2 : 1, perCol = Math.ceil(PAUSE_ROWS.length / cols);
  const top = py + 86, legendH = 64, rowH = Math.min(34, (ph - (top - py) - legendH - 8) / perCol);
  const colW = (pw - 32 - (cols - 1) * 24) / cols;
  const rows = [];
  PAUSE_ROWS.forEach(([k, desc, touch], i) => {
    const c = Math.floor(i / perCol), r = i % perCol;
    const x = px + 16 + c * (colW + 24), y = top + r * rowH, cy = y + rowH / 2;
    keyCap(ctx, art, x, cy - 12, k, 24);
    text(ctx, desc, x + 34, cy, 18, "#fff", "left", 700);
    text(ctx, touch, x + colW, cy, 15, "rgba(205,200,250,.92)", "right", 600);
    rows.push({ key: k, desc, touch, x, y, w: colW, h: rowH });
  });
  // 图例：五种颜色的宝石（形状 + 颜色双编码）
  const ly = py + ph - legendH + 8, s = 40;
  text(ctx, "图例", px + 24, ly + s / 2, 20, "#ffe082", "left", 800);
  const gems = [];
  for (let c = 1; c <= 5; c++) {
    const gx = px + 84 + (c - 1) * (s + 10);
    if (gx + s > px + pw - 8) break;
    if (art.draw(ctx, `gem_c${c}`, gx, ly, s, s)) gems.push(`gem_c${c}`);
  }
  return { panel: { x: px, y: py, w: pw, h: ph }, rows, gems, cols };
}

// ---------------------------------------------------------------------------
// 选关地图：按章节（m3Meta.chapters = UI.Chapters 的 chapterStarts / chapterTitle）分块，每块一个章节标签 + 若干行节点
// （每行最多 PER_ROW 个，块内蛇形排列，同桌面 mapNodePos 的 zig-zag）；竖排一列、横排两列，整体缩放到放得下。
const PER_ROW = 7, LABEL_U = 0.5, COL_GAP_U = 0.8, HEAD_H = 76;
export function mapLayout(a, chapters, n) {
  const blocks = chapters.map((ch, k) => {
    const end = k + 1 < chapters.length ? chapters[k + 1].start : n;
    const ids = []; for (let i = ch.start; i < end; i++) ids.push(i);
    return { label: ch.title || ch.label, ids, rows: Math.max(1, Math.ceil(ids.length / PER_ROW)) };
  }).filter((b) => b.ids.length);
  const ncol = a.w > a.h * 1.1 ? 2 : 1;
  const totalU = blocks.reduce((s, b) => s + LABEL_U + b.rows, 0);
  // 按顺序把章节分到各列，每列高度尽量接近 total / ncol
  const columns = [[]];
  let acc = 0;
  for (const b of blocks) {
    const hU = LABEL_U + b.rows;
    if (columns.length < ncol && columns.at(-1).length && acc + hU / 2 > (totalU / ncol) * columns.length) columns.push([]);
    columns.at(-1).push(b); acc += hU;
  }
  const colH = Math.max(...columns.map((c) => c.reduce((s, b) => s + LABEL_U + b.rows, 0)));
  const availW = a.w - 32, availH = a.h - HEAD_H - 16;
  const u = Math.min(76, availW / (columns.length * PER_ROW + (columns.length - 1) * COL_GAP_U), availH / colH);
  const totalW = u * (columns.length * PER_ROW + (columns.length - 1) * COL_GAP_U);
  const x0 = a.x + (a.w - totalW) / 2, y0 = a.y + HEAD_H + (availH - u * colH) / 2;
  const nodes = [], labels = [], paths = [];
  columns.forEach((col, ci) => {
    let y = y0;
    const cx0 = x0 + ci * u * (PER_ROW + COL_GAP_U);
    for (const b of col) {
      labels.push({ text: b.label, x: cx0, y: y + (LABEL_U * u) / 2 });
      y += LABEL_U * u;
      const pts = [];
      b.ids.forEach((i, j) => {
        const row = Math.floor(j / PER_ROW), c = j % PER_ROW, cc = row % 2 === 0 ? c : PER_ROW - 1 - c;
        const nd = { i, x: cx0 + (cc + 0.5) * u, y: y + (row + 0.5) * u, r: u * 0.3 };
        nodes.push(nd); pts.push(nd);
      });
      paths.push(pts);
      y += b.rows * u;
    }
  });
  return { nodes, labels, paths, u, ncol: columns.length };
}
// 点中哪个节点：命中区 = 节点所在的整格（边长 u，比节点图大，手指好点；桌面 mapHitTest 是节点外 ±2）；没点中为 null
export function mapHit(ml, x, y) {
  const nd = ml.nodes.find((d) => Math.abs(x - d.x) < ml.u / 2 && Math.abs(y - d.y) < ml.u / 2);
  return nd ? nd.i : null;
}
// levels：m3Levels（goal.icon 画在节点右下角）；dots：m3Progress 的进度点字符串（C 当前 / D 已过 / U 已解锁 / L 未解锁）。
export function drawMap(ctx, art, a, ml, levels, dots, pulse, hint) {
  veil(ctx, a, "rgba(12,10,32,.93)");
  text(ctx, "选关地图", a.x + a.w / 2, a.y + 28, 30, "#ffe082", "center", 900);
  text(ctx, hint, a.x + a.w / 2, a.y + 58, 15, "rgba(255,248,225,.85)", "center", 600);
  ctx.lineWidth = Math.max(2, ml.u * 0.05); ctx.strokeStyle = "rgba(140,130,220,.75)";
  for (const pts of ml.paths) {
    if (pts.length < 2) continue;
    ctx.beginPath(); ctx.moveTo(pts[0].x, pts[0].y);
    for (const p of pts.slice(1)) ctx.lineTo(p.x, p.y);
    ctx.stroke();
  }
  const drawn = [];
  for (const nd of ml.nodes) {
    const d = dots ? dots[nd.i] : "L", s = nd.r * 2;
    const kind = d === "C" ? "node_cur" : d === "L" ? "node_lock" : "node_done";
    if (d === "C") {
      const al = Math.round(120 + 120 * (0.5 + 0.5 * Math.sin((pulse * 2 * Math.PI) / 50)));
      art.add(ctx, "spark", nd.x - s * 0.85, nd.y - s * 0.85, s * 1.7, s * 1.7, [255, 210, 90], al);
    }
    if (!art.draw(ctx, kind, nd.x - nd.r, nd.y - nd.r, s, s)) {
      ctx.fillStyle = d === "C" ? "rgb(255,200,60)" : d === "L" ? "rgb(50,50,70)" : "rgb(80,180,120)";
      ctx.fillRect(nd.x - nd.r, nd.y - nd.r, s, s);
    }
    text(ctx, String(nd.i + 1), nd.x, nd.y - nd.r * 0.12, Math.round(nd.r * 0.85), d === "L" ? "rgb(170,170,200)" : "#fff", "center", 800);
    const icon = levels[nd.i]?.goal?.icon;
    if (icon) art.draw(ctx, icon, nd.x + nd.r * 0.45, nd.y + nd.r * 0.25, nd.r * 0.9, nd.r * 0.9);
    drawn.push({ i: nd.i, kind });
  }
  for (const lb of ml.labels) {
    ctx.font = `700 14px ${FONT}`;
    const w = ctx.measureText(lb.text).width + 14;
    if (!art.panel(ctx, "panel_gold", lb.x, lb.y - 10, w, 20, 7)) { ctx.fillStyle = "rgba(90,60,10,.9)"; ctx.fillRect(lb.x, lb.y - 10, w, 20); }
    text(ctx, lb.text, lb.x + 7, lb.y, 14, "#fff8e1", "left", 700);
  }
  return { nodes: drawn, labels: ml.labels.map((l) => l.text), ncol: ml.ncol };
}
