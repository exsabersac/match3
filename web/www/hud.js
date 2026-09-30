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

// value 为 null 时只画底板和标签（关名由 levelName 画）
function chip(ctx, art, x, y, w, h, label, value, valueColor = "#ffe082") {
  if (!art.panel(ctx, "panel_chip", x, y, w, h, 12)) { ctx.fillStyle = "rgba(30,24,60,.8)"; ctx.fillRect(x, y, w, h); }
  text(ctx, label, x + 12, y + h * 0.3, 13, "rgba(235,230,255,.8)", "left", 600);
  if (value === null) return;
  ctx.font = `800 21px ${FONT}`;
  text(ctx, fit(ctx, String(value), w - 24), x + 12, y + h * 0.68, 21, valueColor, "left", 800);
}

// 关名（同桌面 UI.HudArt：zhA ren art ("name_" ++ show li) 66 11 24）：画预渲染文字图 name_<关卡下标 0 起>（tools/gen_assets.py
// 按关卡表烘焙，与桌面同一张图；第 N 关 = name_<N−1>），高 21 设计单位、按原图宽高比（与原来浏览器字体 21 px 的关名槽同高，规则角标布局不变），超出槽宽时等比缩小；
// 图集里没有这张图时退回浏览器字体画 state.name。返回实际画出的矩形与贴图名 / 文字（e2e 检查每关画的是对应的 name_N）。
const NAME_H = 21;
function levelName(ctx, art, x, cy, maxW, info) {
  const sprite = `name_${info.level}`, sz = art.size(sprite);
  if (sz) {
    let h = NAME_H, w = (sz[0] * NAME_H) / sz[1];
    if (w > maxW) { h = (h * maxW) / w; w = maxW; }
    art.draw(ctx, sprite, x, cy - h / 2, w, h);
    return { x, y: cy - h / 2, w, h, sprite, text: null };
  }
  ctx.font = `800 21px ${FONT}`;
  const t = fit(ctx, String(info.name), maxW);
  text(ctx, t, x, cy, 21, "#fff", "left", 800);
  return { x, y: cy - 10.5, w: Math.min(maxW, ctx.measureText(t).width), h: 21, sprite: null, text: t };
}

// 圆角矩形路径（不依赖 ctx.roundRect，老 WebView 也能画）
function roundPath(ctx, x, y, w, h, r) {
  r = Math.min(r, w / 2, h / 2);
  ctx.beginPath();
  ctx.moveTo(x + r, y);
  ctx.arcTo(x + w, y, x + w, y + h, r); ctx.arcTo(x + w, y + h, x, y + h, r);
  ctx.arcTo(x, y + h, x, y, r); ctx.arcTo(x, y, x + w, y, r);
  ctx.closePath();
}

// 规则开关角标（state.rules = [{name, text, icons}]：视图模型 gvRules 查 Match3.View.ruleBadge，与桌面 HUD 同一张表）。
// 通用画法：每个规则一枚「叠放图标 + 文字」小胶囊，从 (x, cy) 起向右排，总宽不超过 maxW；
// 放不下时先把各枚的文字截断（省略号），再不够就只留图标，仍放不下的不画。文字用画布字体（网页图集没有文字贴图）。
// 返回画出的各枚矩形（设计单位，e2e 检查不重叠 / 不出框）。
const BADGE_PAD = 5, BADGE_GAP = 6;
function drawRuleBadges(ctx, art, rules, x, cy, maxW, h, size) {
  if (!rules || !rules.length) return [];
  ctx.font = `700 ${size}px ${FONT}`;
  const iconW = (r) => (r.icons && r.icons.length ? h - 2 + 3 : 0);
  const natural = rules.map((r) => BADGE_PAD * 2 + iconW(r) + ctx.measureText(r.text).width);
  const total = natural.reduce((a, b) => a + b, 0) + BADGE_GAP * (rules.length - 1);
  // 超宽：平均分给每枚，文字按剩余宽度截断
  const share = total <= maxW ? null : (maxW - BADGE_GAP * (rules.length - 1)) / rules.length;
  const out = [];
  let bx = x;
  rules.forEach((r, i) => {
    const iw = iconW(r);
    let w = natural[i], label = r.text;
    if (share !== null && w > share) {
      const room = share - BADGE_PAD * 2 - iw;
      // 太窄：有图标就只留图标；没图标至少留文字的第一个字
      label = room >= size * 1.5 ? fit(ctx, r.text, room) : (iw ? "" : [...r.text][0] || "");
      w = label ? BADGE_PAD * 2 + iw + ctx.measureText(label).width : Math.max(h, BADGE_PAD + iw);
    }
    if (bx + w > x + maxW + 0.5) return;
    const by = cy - h / 2;
    ctx.save();
    roundPath(ctx, bx, by, w, h, h / 2);
    ctx.fillStyle = "rgba(40,20,70,.72)"; ctx.fill();
    ctx.lineWidth = 1; ctx.strokeStyle = "rgba(255,214,90,.85)"; ctx.stroke();
    ctx.restore();
    let tx = bx + BADGE_PAD;
    if (iw) {
      // 图标从下往上叠画（如 bomb_glow + bomb_mark，同桌面 HudArt）；缺图时退回一个金色圆点
      const s = h - 2, ix = bx + (label ? BADGE_PAD - 2 : (w - s) / 2), iy = cy - s / 2;
      let drawn = false;
      for (const ic of r.icons) drawn = art.draw(ctx, ic, ix, iy, s, s) || drawn;
      if (!drawn) { ctx.fillStyle = "#ffd65a"; ctx.beginPath(); ctx.arc(ix + s / 2, cy, s / 3, 0, Math.PI * 2); ctx.fill(); }
      tx = ix + s + 3;
    }
    if (label) text(ctx, label, tx, cy + 0.5, size, "#ffe9a8", "left", 700);
    out.push({ name: r.name, x: bx, y: by, w, h, label });
    bx += w + BADGE_GAP;
  });
  return out;
}

// 关卡小面板：chip + 标签行右侧的规则角标（标签「第 N 关」之后到面板右边距之间）。返回各部件矩形（e2e 用）。
function levelChip(ctx, art, x, y, w, h, info, badgeH, badgeSize) {
  chip(ctx, art, x, y, w, h, `第 ${info.level + 1} 关`, null);
  const name = levelName(ctx, art, x + 12, y + h * 0.68, w - 24, info);
  ctx.font = `600 13px ${FONT}`;
  const labelW = ctx.measureText(`第 ${info.level + 1} 关`).width;
  const bx = x + 12 + labelW + 8;
  const badges = drawRuleBadges(ctx, art, info.rules, bx, y + h * 0.3, x + w - 8 - bx, badgeH, badgeSize);
  return {
    chip: { x, y, w, h },
    label: { x: x + 12, y: y + h * 0.3 - 6.5, w: labelW, h: 13 },
    name,
    badges,
  };
}

// 目标进度条：左边目标图标（info.goalIcon = state.goal.icon，核心侧 UI.GoalIcon.goalIcon，与桌面 HUD 同一张表；缺图不画、
// 条不右移）+ 标签「目标 <中文名>」（info.goalText = state.goal.label）+ 进度数字 + 进度条。
// 返回实际画出的标签文字与图标名（e2e 检查）。
function goalBar(ctx, art, x0, y, w0, h, info) {
  const icon = info.goalIcon && art.draw(ctx, info.goalIcon, x0, y, h, h) ? info.goalIcon : null;
  const x = icon ? x0 + h + 6 : x0, w = icon ? w0 - h - 6 : w0;
  const frac = info.target > 0 ? Math.min(1, info.progress / info.target) : 0;
  ctx.font = `700 15px ${FONT}`;
  const label = fit(ctx, `目标 ${info.goalText}`, w - 80);
  text(ctx, label, x + 2, y + 9, 15, "#f3eefc", "left", 700);
  text(ctx, `${Math.min(info.progress, info.target)}/${info.target}`, x + w - 2, y + 9, 15, "#ffe082", "right", 800);
  const by = y + 20, bh = h - 20;
  if (!art.panel(ctx, "panel_bar", x, by, w, bh, bh / 2)) { ctx.fillStyle = "#2a2340"; ctx.fillRect(x, by, w, bh); }
  if (frac > 0) {
    const fw = Math.max(bh, w * frac);
    if (!art.panel(ctx, "panel_fill", x, by, fw, bh, bh / 2)) { ctx.fillStyle = "#7cd67c"; ctx.fillRect(x, by, fw, bh); }
  }
  return { label, icon };
}

// 雪怪 Boss 血条（新玩法 5；state.boss = 视图模型 gvBoss {hp, max}）：目标是「击败 Boss」时替换目标条，同桌面 UI.HudArt——
// 左边 snow_boss 头像，红色进度条长度 = 剩余 / 满血，文字「HP 剩余/满血」；剩余 ≤ 一半后变深红并随呼吸计数闪烁
// （桌面 150 + 80 × breathe(pulse, 30)）。左上仍画目标标签「目标 <中文名>」（state.goal.label，与 goalBar 同一条路径）。
// 返回血条矩形、读数与标签文字（e2e 检查用）。
function bossBar(ctx, art, x, y, w, h, info, pulse) {
  const boss = info.boss, s = h, bx = x + s + 6, bw = w - s - 6;
  if (!art.draw(ctx, "snow_boss", x, y, s, s)) { ctx.fillStyle = "rgb(214,232,250)"; ctx.fillRect(x + 4, y + 4, s - 8, s - 8); }
  const mx = Math.max(1, boss.max), hp = clampHp(boss.hp, mx);
  ctx.font = `700 15px ${FONT}`;
  const label = fit(ctx, `目标 ${info.goalText}`, bw - 100);
  text(ctx, label, bx + 2, y + 9, 15, "#f3eefc", "left", 700);
  text(ctx, `HP ${hp}/${boss.max}`, bx + bw - 2, y + 9, 15, "#ffe082", "right", 800);
  const by = y + 20, bh = h - 20;
  if (!art.panel(ctx, "panel_bar", bx, by, bw, bh, bh / 2)) { ctx.fillStyle = "#2a2340"; ctx.fillRect(bx, by, bw, bh); }
  const half = hp * 2 <= mx;
  const tint = half ? [Math.round(150 + 80 * (0.5 + 0.5 * Math.sin((pulse * 2 * Math.PI) / 30))), 40, 60] : [235, 70, 80];
  if (hp > 0) {
    const fw = Math.max(bh, (bw * hp) / mx);
    if (!art.panel(ctx, "panel_fill", bx, by, fw, bh, bh / 2, tint)) { ctx.fillStyle = `rgb(${tint.join(",")})`; ctx.fillRect(bx, by, fw, bh); }
  }
  return { x, y, w, h, hp, max: boss.max, half, label };
}
const clampHp = (hp, mx) => Math.max(0, Math.min(mx, hp));

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

// info：{level, name, rules, score, moves, goalText, goalIcon, progress, target, boss, pulse, msg, undo, busy}（boss = state.boss，非 null 时画血条）；
// pressed：当前按下的按钮 id。返回关卡面板各部件（面板 / 标签 / 关名 / 规则角标）的矩形、目标标签文字 goal 与 Boss 血条 boss，
// 供调试钩子与 e2e 检查（goalIcon = 目标条实际画出的图标名，血条关卡为 undefined）。
export function drawHud(ctx, art, L, info, pressed) {
  const h = L.hud;
  let lv, boss = null;
  if (L.mode === "portrait") {
    const g = 10, wl = h.w - 2 * (130 + g);
    lv = levelChip(ctx, art, h.x, h.y, wl, 48, info, 15, 11);
    chip(ctx, art, h.x + wl + g, h.y, 130, 48, "分数", info.score);
    chip(ctx, art, h.x + wl + 130 + 2 * g, h.y, 130, 48, "步数", info.moves, info.moves <= 5 ? "#ff8a80" : "#ffe082");
    if (info.boss) { boss = bossBar(ctx, art, h.x, h.y + 56, h.w, 36, info, info.pulse); lv.goal = boss.label; }
    else { const g = goalBar(ctx, art, h.x, h.y + 56, h.w, 36, info); lv.goal = g.label; lv.goalIcon = g.icon; }
    ctx.font = `600 16px ${FONT}`;
    text(ctx, fit(ctx, info.msg, h.w), h.x + 2, h.y + 110, 16, "#fff8e1", "left", 600);
  } else {
    lv = levelChip(ctx, art, h.x, h.y, h.w, 56, info, 17, 12);
    const hw = (h.w - 8) / 2;
    chip(ctx, art, h.x, h.y + 64, hw, 56, "分数", info.score);
    chip(ctx, art, h.x + hw + 8, h.y + 64, hw, 56, "步数", info.moves, info.moves <= 5 ? "#ff8a80" : "#ffe082");
    if (info.boss) { boss = bossBar(ctx, art, h.x, h.y + 130, h.w, 40, info, info.pulse); lv.goal = boss.label; }
    else { const g = goalBar(ctx, art, h.x, h.y + 130, h.w, 40, info); lv.goal = g.label; lv.goalIcon = g.icon; }
    const top = h.y + 186, bottom = L.buttons[0].y - 8;
    ctx.font = `600 16px ${FONT}`;
    const lines = wrap(ctx, info.msg, h.w - 4, Math.max(1, Math.floor((bottom - top) / 22)));
    lines.forEach((ln, i) => text(ctx, ln, h.x + 2, top + 11 + i * 22, 16, "#fff8e1", "left", 600));
  }
  for (const b of L.buttons) {
    const enabled = !info.busy && (b.id !== "undo" || info.undo > 0);
    button(ctx, art, b, enabled, pressed === b.id);
  }
  return { ...lv, boss };
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
