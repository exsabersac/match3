// 全屏浮层（从桌面版迁来）：暂停说明（app/UI/HudArt.drawPauseHelpArt）与选关地图（app/UI/LevelMap.drawLevelMapArt）。
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
// 暂停：每个按键一行（键帽 + 作用 + 触屏上对应的按钮），底部宝石图例（gem_c1..5，同桌面「图例」行）
// 与桌面 rows 同序；第三列是网页 / 触屏入口（PC 壳和手机都没有键盘时用）。
export const PAUSE_ROWS = [
  ["H", "提示", "「提示」"], ["1", "锤子", "「锤」按钮"], ["2", "自由交换", "「换」按钮"], ["3", "十字消", "「十」按钮"],
  ["U", "撤销", "「撤销」"], ["S", "洗牌", "「洗牌」"], ["D", "每日挑战", "「每日」"], ["M", "选关地图", "「地图」"],
  ["K", "音效", "「效」芯片"], ["B", "BGM", "「乐」芯片"], ["R", "重开本关", "「重开」"],
  ["N", "下一关", "点结算面板 /「›」"], ["P", "继续游戏", "点任意处"],
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
