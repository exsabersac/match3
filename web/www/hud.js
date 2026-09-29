// HUD（画在同一张画布上，随布局缩放）：关卡 / 分数 / 步数 小面板、目标进度条、提示消息、按钮。
// 所有尺寸（含字号）都是设计单位（格 = 56），因此文字大小随格子大小一起缩放。
export const FONT = 'system-ui, -apple-system, "PingFang SC", "Hiragino Sans GB", "Noto Sans CJK SC", "Microsoft YaHei", sans-serif';

function text(ctx, s, x, y, size, color, align = "left", weight = 700) {
  ctx.font = `${weight} ${size}px ${FONT}`;
  ctx.textAlign = align; ctx.textBaseline = "middle";
  ctx.fillStyle = color;
  ctx.fillText(s, x, y);
}

// 截断到宽度 w（末尾省略号）
function fit(ctx, s, w) {
  if (ctx.measureText(s).width <= w) return s;
  let t = s;
  while (t.length > 1 && ctx.measureText(t + "…").width > w) t = t.slice(0, -1);
  return t + "…";
}

// 按宽度折行（逐字，中文友好），最多 maxLines 行
function wrap(ctx, s, w, maxLines) {
  const lines = [];
  let cur = "";
  for (const ch of s) {
    if (ch === "\n" || ctx.measureText(cur + ch).width > w) { lines.push(cur); cur = ch === "\n" ? "" : ch; }
    else cur += ch;
  }
  if (cur) lines.push(cur);
  if (lines.length > maxLines) { lines.length = maxLines; lines[maxLines - 1] = fit(ctx, lines[maxLines - 1] + "……", w); }
  return lines;
}

function chip(ctx, art, x, y, w, h, label, value, valueColor = "#ffe082") {
  if (!art.panel(ctx, "panel_chip", x, y, w, h, 12)) { ctx.fillStyle = "rgba(30,24,60,.8)"; ctx.fillRect(x, y, w, h); }
  text(ctx, label, x + 12, y + h * 0.3, 13, "rgba(235,230,255,.8)", "left", 600);
  ctx.font = `800 21px ${FONT}`;
  text(ctx, fit(ctx, String(value), w - 24), x + 12, y + h * 0.68, 21, valueColor, "left", 800);
}

function goalBar(ctx, art, x, y, w, h, info) {
  const frac = info.target > 0 ? Math.min(1, info.progress / info.target) : 0;
  ctx.font = `700 15px ${FONT}`;
  text(ctx, fit(ctx, `目标 ${info.goalText}`, w - 80), x + 2, y + 9, 15, "#f3eefc", "left", 700);
  text(ctx, `${Math.min(info.progress, info.target)}/${info.target}`, x + w - 2, y + 9, 15, "#ffe082", "right", 800);
  const by = y + 20, bh = h - 20;
  if (!art.panel(ctx, "panel_bar", x, by, w, bh, bh / 2)) { ctx.fillStyle = "#2a2340"; ctx.fillRect(x, by, w, bh); }
  if (frac > 0) {
    const fw = Math.max(bh, w * frac);
    if (!art.panel(ctx, "panel_fill", x, by, fw, bh, bh / 2)) { ctx.fillStyle = "#7cd67c"; ctx.fillRect(x, by, fw, bh); }
  }
}

function button(ctx, art, b, enabled, pressed) {
  ctx.save();
  ctx.globalAlpha = enabled ? 1 : 0.45;
  const name = b.id === "hint" || b.id === "undo" || b.id === "restart" ? "panel_gold" : "panel_chip";
  if (!art.panel(ctx, name, b.x, b.y + (pressed ? 2 : 0), b.w, b.h, 14)) { ctx.fillStyle = "#554"; ctx.fillRect(b.x, b.y, b.w, b.h); }
  const big = b.label.length === 1;
  const gold = name === "panel_gold";
  text(ctx, b.label, b.x + b.w / 2, b.y + b.h / 2 + (pressed ? 2 : 0) + (big ? -2 : 0), big ? 32 : 20, gold ? "#ffe9a8" : "#fff", "center", 800);
  ctx.restore();
}

// info：{level, name, score, moves, goalText, progress, target, msg, undo, busy}；pressed：当前按下的按钮 id
export function drawHud(ctx, art, L, info, pressed) {
  const h = L.hud;
  if (L.mode === "portrait") {
    const g = 10, wl = h.w - 2 * (130 + g);
    chip(ctx, art, h.x, h.y, wl, 48, `第 ${info.level + 1} 关`, info.name, "#fff");
    chip(ctx, art, h.x + wl + g, h.y, 130, 48, "分数", info.score);
    chip(ctx, art, h.x + wl + 130 + 2 * g, h.y, 130, 48, "步数", info.moves, info.moves <= 5 ? "#ff8a80" : "#ffe082");
    goalBar(ctx, art, h.x, h.y + 56, h.w, 36, info);
    ctx.font = `600 16px ${FONT}`;
    text(ctx, fit(ctx, info.msg, h.w), h.x + 2, h.y + 110, 16, "#fff8e1", "left", 600);
  } else {
    chip(ctx, art, h.x, h.y, h.w, 56, `第 ${info.level + 1} 关`, info.name, "#fff");
    const hw = (h.w - 8) / 2;
    chip(ctx, art, h.x, h.y + 64, hw, 56, "分数", info.score);
    chip(ctx, art, h.x + hw + 8, h.y + 64, hw, 56, "步数", info.moves, info.moves <= 5 ? "#ff8a80" : "#ffe082");
    goalBar(ctx, art, h.x, h.y + 130, h.w, 40, info);
    const top = h.y + 186, bottom = L.buttons[0].y - 8;
    ctx.font = `600 16px ${FONT}`;
    const lines = wrap(ctx, info.msg, h.w - 4, Math.max(1, Math.floor((bottom - top) / 22)));
    lines.forEach((ln, i) => text(ctx, ln, h.x + 2, top + 11 + i * 22, 16, "#fff8e1", "left", 600));
  }
  for (const b of L.buttons) {
    const enabled = !info.busy && (b.id !== "undo" || info.undo > 0);
    button(ctx, art, b, enabled, pressed === b.id);
  }
}

// 结局面板（盖在棋盘上）
export function drawOverlay(ctx, art, rect, title, sub) {
  ctx.fillStyle = "rgba(10,6,24,.72)";
  ctx.fillRect(rect.x, rect.y, rect.w, rect.h);
  const pw = rect.w * 0.8, ph = 150, px = rect.x + (rect.w - pw) / 2, py = rect.y + (rect.h - ph) / 2;
  art.panel(ctx, "panel_dark", px, py, pw, ph, 18);
  text(ctx, title, rect.x + rect.w / 2, py + 50, 38, "#ffe082", "center", 900);
  ctx.font = `600 17px ${FONT}`;
  wrap(ctx, sub, pw - 30, 3).forEach((ln, i) => text(ctx, ln, rect.x + rect.w / 2, py + 96 + i * 24, 17, "#fff", "center", 600));
}
