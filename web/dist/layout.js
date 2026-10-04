// 自适应布局：按可用空间（视口减去安全区）为「棋盘 + HUD」算出唯一的缩放 u（设计单位 → CSS 像素；格 = 56 单位），
// 竖屏（手机）HUD 在棋盘上方、两行按钮条在下方；横屏（桌面 / 平板 / 横持手机）HUD 在棋盘右侧（侧栏至少 SIDE_MIN_H 高，
// 棋盘在其中竖直居中）。两种都算一遍，取格子更大的。
// 棋盘尺寸按关卡行列数（rows×cols）计算，不假设正方形。所有绘制与指针命中都走同一个变换（toUnits / fromUnits）。
import { CELL, PAD } from "./cells.js";

export const TOP_H = 124, BAR_H = 64, SIDE_W = 236, GAP = 10;   // HUD 各块的设计单位尺寸（BAR_H = 竖排按钮条每行）
export const SIDE_MIN_H = 520;                                   // 横排侧栏最小高度：四行按钮 + 至少两行提示文字
export const MIN_TOUCH_CSS = 46;                                  // 按钮目标最小触控尺寸（CSS 像素，≥ 44 的建议值）
export const MAX_CELL_CSS = 112;                                 // 格子最大 CSS 像素（大屏上不再放大，居中留白）

// 读 CSS env(safe-area-inset-*)：用一个固定定位的探针元素的 padding 取值
let probe = null;
export function safeInsets() {
  if (!probe) {
    probe = document.createElement("div");
    probe.style.cssText = "position:fixed;left:0;top:0;visibility:hidden;pointer-events:none;" +
      "padding:env(safe-area-inset-top) env(safe-area-inset-right) env(safe-area-inset-bottom) env(safe-area-inset-left)";
    document.body.appendChild(probe);
  }
  const cs = getComputedStyle(probe);
  return { t: parseFloat(cs.paddingTop) || 0, r: parseFloat(cs.paddingRight) || 0, b: parseFloat(cs.paddingBottom) || 0, l: parseFloat(cs.paddingLeft) || 0 };
}

// 计算布局。W/H：CSS 像素视口；rows/cols：盘面行列数。
export function computeLayout(W, H, rows, cols, ins = { t: 0, r: 0, b: 0, l: 0 }) {
  const aw = Math.max(1, W - ins.l - ins.r), ah = Math.max(1, H - ins.t - ins.b);
  const VW = cols * CELL + 2 * PAD, VH = rows * CELL + 2 * PAD;
  const portrait = { mode: "portrait", w: VW, h: TOP_H + VH + 2 * BAR_H - 4 };
  const landscape = { mode: "landscape", w: VW + GAP + SIDE_W, h: Math.max(VH, SIDE_MIN_H) };
  // 四周留白：竖排左右只留 4 单位（手机上宽度最紧），其余 GAP
  const fit = (m) => Math.min(aw / (m.w + (m.mode === "portrait" ? 8 : 2 * GAP)), ah / (m.h + 2 * GAP));
  const up = fit(portrait), ul = fit(landscape);
  const m = ul > up * 1.02 ? landscape : portrait;          // 相近时优先竖排（手机常见）
  const u = Math.min(ul > up * 1.02 ? ul : up, MAX_CELL_CSS / CELL);
  const ox = ins.l + (aw - m.w * u) / 2, oy = ins.t + (ah - m.h * u) / 2;   // 整体居中（CSS 像素）
  const L = { mode: m.mode, u, ox, oy, W, H, w: m.w, h: m.h, VW, VH, rows, cols, cellCss: CELL * u };
  if (m.mode === "portrait") {
    // 竖排通常是宽度受限、纵向有富余：把按钮条加高到至少 MIN_TOUCH_CSS（不挤占棋盘）
    const spare = ah / u - 2 * GAP - m.h;
    // 两行按钮各自加高到至少 MIN_TOUCH_CSS（富余按两行平分）
    const rowH = Math.round(Math.max(BAR_H, Math.min(MIN_TOUCH_CSS / u + 10, BAR_H + Math.max(0, spare) / 2)));
    const barH = 2 * rowH - 4;
    L.h = TOP_H + VH + barH;
    L.oy = ins.t + (ah - L.h * u) / 2;
    L.board = { x: 0, y: TOP_H };
    L.hud = { x: 0, y: 0, w: VW, h: TOP_H };
    L.bar = { x: 0, y: TOP_H + VH, w: VW, h: barH, rowH };
  } else {
    L.board = { x: 0, y: Math.round((m.h - VH) / 2) };
    L.hud = { x: VW + GAP, y: 0, w: SIDE_W, h: m.h };
    L.bar = null;   // 按钮放在侧栏底部
  }
  L.buttons = layoutButtons(L);
  return L;
}

// 按钮（设计单位矩形）。第一组：本关说明 ?、选关 ‹ ›、重开、提示、撤销；第二组是从桌面版迁来的按键（键盘之外的触屏入口）：
// 三种道具（锤子 1 / 自由交换 2 / 十字消 3，按钮上画图标 + 剩余次数，当前点选模式金框）、洗牌 S、选关地图 M、每日挑战 D、暂停 P。
// 问号跟按钮条走，不压在棋盘格子上。booster：道具按钮对应的 state.boosters 字段 / 图标。
export const BOOSTERS = { hammer: { key: "hammer", icon: "icon_hammer" }, swap: { key: "swap", icon: "icon_swap" }, cross: { key: "cross", icon: "icon_cross" } };
const ROW_A = [["help", "?"], ["prev", "‹"], ["next", "›"], ["restart", "重开"], ["hint", "提示"], ["undo", "撤销"]];
const ROW_B = [["hammer", "锤"], ["swap", "换"], ["cross", "十"], ["shuffle", "洗牌"], ["map", "地图"], ["daily", "每日"], ["pause", "暂停"]];
const SMALL_BTN = new Set(["help", "prev", "next"]);
const btn = (id, label, x, y, w, h) => ({ id, label, x, y, w, h, ...(BOOSTERS[id] ? { booster: BOOSTERS[id] } : {}) });
// 一行等宽排开（small 里的按钮固定宽 small，其余平分）
function row(out, items, x, y, w, h, gap, small = 0) {
  const nSmall = small ? items.filter(([id]) => SMALL_BTN.has(id)).length : 0;
  const big = (w - nSmall * small - gap * (items.length - 1)) / (items.length - nSmall);
  let cx = x;
  for (const [id, label] of items) {
    const bw = small && SMALL_BTN.has(id) ? small : big;
    out.push(btn(id, label, cx, y, bw, h));
    cx += bw + gap;
  }
}
function layoutButtons(L) {
  const out = [];
  if (L.bar) {
    // 竖排：棋盘下方两行
    const { x, y, w, rowH } = L.bar;
    row(out, ROW_A, x, y + 6, w, rowH - 10, 8, 48);
    row(out, ROW_B, x, y + rowH - 4 + 6, w, rowH - 10, 6);
  } else {
    // 横排：侧栏底部四行按钮，高度至少 MIN_TOUCH_CSS（消息区相应变矮）
    const { x, y, w, h } = L.hud, gap = 8, bh = Math.round(Math.max(54, Math.min(72, MIN_TOUCH_CSS / L.u)));
    const r4 = y + h - bh, r3 = r4 - gap - bh, r2 = r3 - gap - bh, r1 = r2 - gap - bh;
    const third = (w - 2 * gap) / 3, nav = 54;
    row(out, ROW_B.slice(0, 3), x, r1, w, bh, gap);
    row(out, ROW_B.slice(3), x, r2, w, bh, gap);
    out.push(btn("prev", "‹", x, r3, nav, bh));
    out.push(btn("next", "›", x + nav + gap, r3, nav, bh));
    out.push(btn("restart", "重开", x + 2 * (nav + gap), r3, w - 2 * (nav + gap), bh));
    out.push(btn("hint", "提示", x, r4, third, bh));
    out.push(btn("undo", "撤销", x + third + gap, r4, third, bh));
    out.push(btn("help", "?", x + 2 * (third + gap), r4, third, bh));
  }
  return out;
}

// CSS 像素 ↔ 设计单位（整体坐标）
export const toUnits = (L, cx, cy) => [(cx - L.ox) / L.u, (cy - L.oy) / L.u];
export const fromUnits = (L, x, y) => [L.ox + x * L.u, L.oy + y * L.u];

// 设计单位坐标 → 棋盘格（棋盘外 null）
export function cellAtUnits(L, x, y) {
  const bx = x - L.board.x - PAD, by = y - L.board.y - PAD;
  const c = Math.floor(bx / CELL), r = Math.floor(by / CELL);
  return r >= 0 && r < L.rows && c >= 0 && c < L.cols ? [r, c] : null;
}

// 格子中心的 CSS 坐标（自动化测试 / 调试用）
export function cellCenterCss(L, [r, c]) {
  return fromUnits(L, L.board.x + PAD + (c + 0.5) * CELL, L.board.y + PAD + (r + 0.5) * CELL);
}

export function buttonAtUnits(L, x, y) {
  return L.buttons.find((b) => x >= b.x && x < b.x + b.w && y >= b.y && y < b.y + b.h) || null;
}
