// 自适应布局：按可用空间（视口减去安全区）为「棋盘 + HUD」算出唯一的缩放 u（设计单位 → CSS 像素；格 = 56 单位），
// 竖屏（手机）HUD 在棋盘上方、一行按钮条在下方；横屏（桌面 / 平板 / 横持手机）HUD 在棋盘右侧（侧栏与棋盘等高）。
// 两种都算一遍，取格子更大的。几何与加道具按钮之前（33e65bf）相同，所以格子大小不变；常驻按钮只有 撤销 / 提示 / 菜单，
// 其余（道具、洗牌、重开、每日、地图、选关、说明、音效 / 音乐）收进菜单面板（panels.js 的 menuLayout）。
// 棋盘尺寸按关卡行列数（rows×cols）计算，不假设正方形。所有绘制与指针命中都走同一个变换（toUnits / fromUnits）。
import { CELL, PAD } from "./cells.js";

export const TOP_H = 124, BAR_H = 64, SIDE_W = 236, GAP = 10;   // HUD 各块的设计单位尺寸
export const MIN_TOUCH_CSS = 46;                                  // 按钮目标最小触控尺寸（CSS 像素，≥ 44 的建议值，留 2 px 余量）
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
  const portrait = { mode: "portrait", w: VW, h: TOP_H + VH + BAR_H };
  const landscape = { mode: "landscape", w: VW + GAP + SIDE_W, h: VH };
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
    const barH = Math.round(Math.max(BAR_H, Math.min(MIN_TOUCH_CSS / u + 10, BAR_H + Math.max(0, spare))));
    L.h = TOP_H + VH + barH;
    L.oy = ins.t + (ah - L.h * u) / 2;
    L.board = { x: 0, y: TOP_H };
    L.hud = { x: 0, y: 0, w: VW, h: TOP_H };
    L.bar = { x: 0, y: TOP_H + VH, w: VW, h: barH };
  } else {
    L.board = { x: 0, y: 0 };
    L.hud = { x: VW + GAP, y: 0, w: SIDE_W, h: m.h };
    L.bar = null;   // 按钮放在侧栏底部
  }
  L.buttons = layoutButtons(L);
  return L;
}

// 常驻按钮（设计单位矩形）：撤销、提示、菜单。其余操作在菜单面板里（panels.js MENU_ITEMS），键盘快捷键不变。
// BOOSTERS：道具菜单项对应的 state.boosters 字段 / 图标。
export const BOOSTERS = { hammer: { key: "hammer", icon: "icon_hammer" }, swap: { key: "swap", icon: "icon_swap" }, cross: { key: "cross", icon: "icon_cross" } };
const BAR = [["undo", "撤销"], ["hint", "提示"], ["menu", "菜单"]];
function layoutButtons(L) {
  const out = [], gap = 8, n = BAR.length;
  let x, y, w, h;
  if (L.bar) ({ x, w } = L.bar, y = L.bar.y + 6, h = L.bar.h - 10);
  else {
    // 横排：侧栏底部一行，高度至少 MIN_TOUCH_CSS（消息区在它上面）
    h = Math.round(Math.max(54, MIN_TOUCH_CSS / L.u));
    ({ x, w } = L.hud); y = L.hud.y + L.hud.h - h;
  }
  const bw = (w - gap * (n - 1)) / n;
  BAR.forEach(([id, label], i) => out.push({ id, label, x: x + i * (bw + gap), y, w: bw, h }));
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
