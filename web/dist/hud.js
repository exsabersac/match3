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

// 取能放进宽度 w 的第一行（逐字，遇换行符断开），返回 [这一行, 剩下的文字]
function takeLine(ctx, s, w) {
  let cur = "", i = 0;
  const chars = [...s];
  for (; i < chars.length; i++) {
    const ch = chars[i];
    if (ch === "\n") return [cur, chars.slice(i + 1).join("")];
    if (cur && ctx.measureText(cur + ch).width > w) break;
    cur += ch;
  }
  return [cur, chars.slice(i).join("")];
}

// value 为 null 时只画底板和标签（关名由 levelName 画）
function chip(ctx, art, x, y, w, h, label, value, valueColor = "#ffe082") {
  if (!art.panel(ctx, "panel_chip", x, y, w, h, 12)) { ctx.fillStyle = "rgba(30,24,60,.8)"; ctx.fillRect(x, y, w, h); }
  text(ctx, label, x + 12, y + h * 0.3, 13, "rgba(235,230,255,.8)", "left", 600);
  if (value === null) return;
  ctx.font = `800 21px ${FONT}`;
  text(ctx, fit(ctx, String(value), w - 24), x + 12, y + h * 0.68, 21, valueColor, "left", 800);
}

// 关名（同原桌面版 UI.HudArt：zhA ren art ("name_" ++ show li) 66 11 24）：画预渲染文字图 name_<关卡下标 0 起>（tools/gen_assets.py
// 按关卡表烘焙，与原桌面版同一张图；第 N 关 = name_<N−1>），高 21 设计单位、按原图宽高比（与原来浏览器字体 21 px 的关名槽同高，规则角标布局不变），超出槽宽时等比缩小；
// 图集里没有这张图时退回浏览器字体画 state.name。返回实际画出的矩形与贴图名 / 文字（e2e 检查每关画的是对应的 name_N）。
const NAME_H = 21;
function levelName(ctx, art, x, cy, maxW, info) {
  // 每日挑战：同原桌面版 HudArt 的 zh_daily，画「每日挑战」（图集里没有中文标签贴图，用画布字体）
  if (info.daily) {
    ctx.font = `800 21px ${FONT}`;
    const t = fit(ctx, "每日挑战", maxW);
    text(ctx, t, x, cy, 21, "#ffe082", "left", 800);
    return { x, y: cy - 10.5, w: Math.min(maxW, ctx.measureText(t).width), h: 21, sprite: null, text: t };
  }
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

// 规则开关角标（state.rules = [{name, text, icons}]：视图模型 gvRules 查 Match3.View.ruleBadge，与原桌面版 HUD 同一张表）。
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
      // 图标从下往上叠画（如 bomb_glow + bomb_mark，同原桌面版 HudArt）；缺图时退回一个金色圆点
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
// 标签：战役关「第 N 关」，每日挑战是挑战日期（info.dailyLabel）。底边一排关卡进度点（levelDots）。
function levelChip(ctx, art, x, y, w, h, info, badgeH, badgeSize) {
  const lab = info.daily ? info.dailyLabel || "每日" : `第 ${info.level + 1} 关`;
  chip(ctx, art, x, y, w, h, lab, null);
  const name = levelName(ctx, art, x + 12, y + h * 0.68, w - 24, info);
  ctx.font = `600 13px ${FONT}`;
  const labelW = ctx.measureText(lab).width;
  const bx = x + 12 + labelW + 8;
  const badges = drawRuleBadges(ctx, art, info.rules, bx, y + h * 0.3, x + w - 8 - bx, badgeH, badgeSize);
  const dots = levelDots(ctx, info.dots, x + 12, y + h - 4, w - 24);
  return {
    chip: { x, y, w, h },
    label: { x: x + 12, y: y + h * 0.3 - 6.5, w: labelW, h: 13 },
    name,
    badges,
    dots,
  };
}

// 关卡进度点（同原桌面版 drawHudArt：m3Progress 的 dots，每关一个字符 C 当前 / D 已过 / U 已解锁 / L 未解锁；
// 已过绿、当前金且高一些、未解锁暗）。点距最多 6，关卡多时收窄到放得下。cy = 点的底边。返回 {x, y, w, h, n}。
const DOT_RGB = { C: "rgb(255,214,90)", D: "rgb(90,210,130)", U: "rgb(120,180,140)", L: "rgb(80,72,130)" };
function levelDots(ctx, dots, x, bottom, maxW) {
  if (!dots) return null;
  const n = dots.length, step = Math.min(6, maxW / Math.max(1, n)), dw = Math.max(1.5, step * 0.66);
  [...dots].forEach((d, i) => {
    const hh = d === "C" ? 6 : 3;
    ctx.fillStyle = DOT_RGB[d] || DOT_RGB.L;
    ctx.fillRect(x + i * step, bottom - hh, dw, hh);
  });
  return { x, y: bottom - 6, w: n * step, h: 6, n };
}

// 分数芯片（同原桌面版 drawHudArt 右下角 Match3.View.scoreBadge，经 m3Badge）：回放中连击 ≥ 2 显示「连击 xN」，否则滚动的分数；
// 播完后 comboSummary 帧内显示本步「N 连击！」；其余显示得分（洗过牌时标签换成「已洗牌」）。
function scoreChip(ctx, art, x, y, w, h, info) {
  const b = info.badge || { kind: "score", n: info.score, shuffled: false };
  if (b.kind === "combo") {
    if (!art.panel(ctx, "panel_gold", x, y, w, h, 12)) { ctx.fillStyle = "rgba(90,60,10,.85)"; ctx.fillRect(x, y, w, h); }
    text(ctx, "连击", x + 12, y + h * 0.3, 13, "rgba(255,240,200,.9)", "left", 600);
    text(ctx, `x${b.n}`, x + 12, y + h * 0.68, 21, info.comboColor || "#ffe082", "left", 800);
  } else if (b.kind === "summary") {
    if (!art.panel(ctx, "panel_gold", x, y, w, h, 12)) { ctx.fillStyle = "rgba(90,60,10,.85)"; ctx.fillRect(x, y, w, h); }
    text(ctx, "本步", x + 12, y + h * 0.3, 13, "rgba(255,240,200,.9)", "left", 600);
    ctx.font = `800 21px ${FONT}`;
    text(ctx, fit(ctx, `${b.n} 连击！`, w - 24), x + 12, y + h * 0.68, 21, info.comboColor || "#ffe082", "left", 800);
  } else chip(ctx, art, x, y, w, h, b.kind === "score" && b.shuffled ? "已洗牌" : "分数", b.kind === "rolling" ? b.n : info.score);
  return b.kind;
}
// 步数 ≤ 5 时变红并闪烁（同原桌面版 160 + 95 × breathe(pulse, 40)）
function movesColor(info) {
  if (info.moves > 5) return "#ffe082";
  const k = Math.round(160 + 95 * (0.5 + 0.5 * Math.sin(((info.pulse || 0) * 2 * Math.PI) / 40)));
  return `rgb(255,${Math.round(k / 2)},${Math.round(k / 2)})`;
}

// 目标进度条：左边目标图标（info.goalIcon = state.goal.icon，核心侧 UI.GoalIcon.goalIcon，与原桌面版 HUD 同一张表；缺图不画、
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

// 雪怪 Boss 血条（新玩法 5；state.boss = 视图模型 gvBoss {hp, max}）：目标是「击败 Boss」时替换目标条，同原桌面版 UI.HudArt——
// 左边 snow_boss 头像，红色进度条长度 = 剩余 / 满血，文字「HP 剩余/满血」；剩余 ≤ 一半后变深红并随呼吸计数闪烁
// （原桌面版 150 + 80 × breathe(pulse, 30)）。左上仍画目标标签「目标 <中文名>」（state.goal.label，与 goalBar 同一条路径）。
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

// left：道具按钮的剩余次数（其余 null）；active：当前道具点选模式（同原桌面版道具芯片的金框）。
// 道具按钮画「图标 + 次数」（同原桌面版 HudArt 的道具芯片：icon_hammer / icon_swap / icon_cross），缺图时退回单字标签。
function button(ctx, art, b, enabled, pressed, left = null, active = false) {
  ctx.save();
  ctx.globalAlpha = enabled || active ? 1 : 0.45;
  const name = active || b.id === "hint" || b.id === "undo" ? "panel_gold" : "panel_chip";
  const dy = pressed ? 2 : 0;
  if (!art.panel(ctx, name, b.x, b.y + dy, b.w, b.h, 14)) { ctx.fillStyle = "#554"; ctx.fillRect(b.x, b.y, b.w, b.h); }
  if (active) { ctx.lineWidth = 3; ctx.strokeStyle = "#ffd65a"; roundPath(ctx, b.x + 1.5, b.y + dy + 1.5, b.w - 3, b.h - 3, 12); ctx.stroke(); }
  if (b.booster) {
    const s = Math.min(b.h - 16, 30), num = String(left ?? 0);
    ctx.font = `800 20px ${FONT}`;
    const tw = ctx.measureText(num).width, total = s + 4 + tw, x0 = b.x + (b.w - total) / 2, cy = b.y + b.h / 2 + dy;
    if (art.draw(ctx, b.booster.icon, x0, cy - s / 2, s, s)) text(ctx, num, x0 + s + 4, cy, 20, left > 0 ? "#fff" : "#9a94c0", "left", 800);
    else text(ctx, `${b.label}${num}`, b.x + b.w / 2, cy, 20, "#fff", "center", 800);
    ctx.restore();
    return;
  }
  const big = b.label.length === 1;
  const gold = name === "panel_gold";
  text(ctx, b.label, b.x + b.w / 2, b.y + b.h / 2 + (pressed ? 2 : 0) + (big ? -2 : 0), big ? 32 : 20, gold ? "#ffe9a8" : "#fff", "center", 800);
  ctx.restore();
}

// 音效 / BGM 状态芯片：同原桌面版 UI.HudArt.drawSoundChipsArt——46 × 28 的 panel_chip（圆角 10）+ 18 高的单字（效 / 乐，关掉为 静），
// 只显示状态、不接点击（太小，够不到 44 CSS px 触控尺寸；开关在菜单里与 K / B 键），
// 字号固定 18 设计单位（不走 button 的单字大号 32：那是给 ‹ › 用的，放进 28 高的芯片会溢出、压到提示行）。
const SOUND_W = 46, SOUND_H = 28, SOUND_GAP = 4, SOUND_FONT = 18;
function soundChip(ctx, art, r, label) {
  if (!art.panel(ctx, "panel_chip", r.x, r.y, r.w, r.h, 10)) { ctx.fillStyle = "#554"; ctx.fillRect(r.x, r.y, r.w, r.h); }
  text(ctx, label, r.x + r.w / 2, r.y + r.h / 2, SOUND_FONT, "#fff", "center", 800);
}
// 两枚芯片占的宽度（含与左侧内容的间隔 8）
const SOUND_SPAN = 2 * SOUND_W + SOUND_GAP + 8;
// 一行提示文字的实际外框（设计单位；e2e 用它找真实绘制的文字）
function msgBox(ctx, s, x, cy) {
  ctx.textAlign = "left"; ctx.textBaseline = "middle";
  const m = ctx.measureText(s);
  return { text: s, x: x - (m.actualBoundingBoxLeft || 0), y: cy - (m.actualBoundingBoxAscent || 8), w: (m.actualBoundingBoxLeft || 0) + (m.actualBoundingBoxRight || m.width), h: (m.actualBoundingBoxAscent || 8) + (m.actualBoundingBoxDescent || 8) };
}

// info：{level, name, rules, score, moves, goalText, goalIcon, progress, target, boss, pulse, msg, undo, busy}（boss = state.boss，非 null 时画血条）；
// pressed：当前按下的按钮 id。返回关卡面板各部件（面板 / 标签 / 关名 / 规则角标）的矩形、目标标签文字 goal 与 Boss 血条 boss，
// 供调试钩子与 e2e 检查（goalIcon = 目标条实际画出的图标名，血条关卡为 undefined）。
export function drawHud(ctx, art, L, info, pressed) {
  const h = L.hud;
  let lv, boss = null, sfx = null;
  const msg = [];
  if (L.mode === "portrait") {
    const g = 10, wl = h.w - 2 * (130 + g);
    lv = levelChip(ctx, art, h.x, h.y, wl, 48, info, 15, 11);
    lv.badge = scoreChip(ctx, art, h.x + wl + g, h.y, 130, 48, info);
    chip(ctx, art, h.x + wl + 130 + 2 * g, h.y, 130, 48, "步数", info.moves, movesColor(info));
    const gw = h.w - SOUND_SPAN;
    if (info.boss) { boss = bossBar(ctx, art, h.x, h.y + 56, gw, 36, info, info.pulse); lv.goal = boss.label; }
    else { const g = goalBar(ctx, art, h.x, h.y + 56, gw, 36, info); lv.goal = g.label; lv.goalIcon = g.icon; }
    sfx = { x: h.x + h.w - 2 * SOUND_W - SOUND_GAP, y: h.y + 56 + (36 - SOUND_H) / 2, w: SOUND_W, h: SOUND_H };
    ctx.font = `600 16px ${FONT}`;
    const ln = fit(ctx, info.msg, h.w);
    msg.push(msgBox(ctx, ln, h.x + 2, h.y + 110));
    text(ctx, ln, h.x + 2, h.y + 110, 16, "#fff8e1", "left", 600);
  } else {
    lv = levelChip(ctx, art, h.x, h.y, h.w, 56, info, 17, 12);
    const hw = (h.w - 8) / 2;
    lv.badge = scoreChip(ctx, art, h.x, h.y + 64, hw, 56, info);
    chip(ctx, art, h.x + hw + 8, h.y + 64, hw, 56, "步数", info.moves, movesColor(info));
    if (info.boss) { boss = bossBar(ctx, art, h.x, h.y + 130, h.w, 40, info, info.pulse); lv.goal = boss.label; }
    else { const g = goalBar(ctx, art, h.x, h.y + 130, h.w, 40, info); lv.goal = g.label; lv.goalIcon = g.icon; }
    const top = h.y + 186, bottom = Math.min(...L.buttons.map((b) => b.y)) - 8;
    sfx = { x: h.x + h.w - 2 * SOUND_W - SOUND_GAP, y: top - 8, w: SOUND_W, h: SOUND_H };
    ctx.font = `600 16px ${FONT}`;
    const maxLines = Math.max(1, Math.floor((bottom - top) / 22));
    // 第一行与芯片同一行，宽度让出 SOUND_SPAN；剩下的文字按整宽折行（只有一行可用时第一行截断加省略号）
    const [first, rest] = takeLine(ctx, info.msg, h.w - 4 - SOUND_SPAN);
    const lines = !rest ? [first] : maxLines > 1 ? [first, ...wrap(ctx, rest, h.w - 4, maxLines - 1)] : [fit(ctx, info.msg.split("\n")[0], h.w - 4 - SOUND_SPAN)];
    lines.forEach((ln, i) => { msg.push(msgBox(ctx, ln, h.x + 2, top + 11 + i * 22)); text(ctx, ln, h.x + 2, top + 11 + i * 22, 16, "#fff8e1", "left", 600); });
  }
  // 常驻按钮：撤销 / 提示 / 菜单（菜单播放中也能开；道具点选模式中菜单按钮加金框，提示「再点菜单里的同一项取消」）
  for (const b of L.buttons) {
    const enabled = (b.id === "menu" || !info.busy) && (b.id !== "undo" || info.undo > 0);
    button(ctx, art, b, enabled, pressed === b.id, null, b.id === "menu" && !!info.tool);
  }
  const bgm = { ...sfx, x: sfx.x + SOUND_W + SOUND_GAP };
  soundChip(ctx, art, sfx, info.sfx === false ? "静" : "效");
  soundChip(ctx, art, bgm, info.bgm === false ? "静" : "乐");
  return { ...lv, boss, sfx, bgm, msg };
}

// 结局面板（盖在棋盘上）：标题、星级（stars = null 时不画；同原桌面版 drawOverlayArtNow：过关 / 通关画 star_on / star_off 三颗，
// 失败不画）、说明 sub（最多 3 行）、底部一行操作提示 action（点棋盘 / 按键做什么）。返回星星与各行的矩形（e2e 用）。
export function drawOverlay(ctx, art, rect, title, sub, stars = null, action = "") {
  ctx.fillStyle = "rgba(10,6,24,.72)";
  ctx.fillRect(rect.x, rect.y, rect.w, rect.h);
  const starH = stars === null ? 0 : 46;
  const pw = rect.w * 0.8, ph = 150 + starH + (action ? 28 : 0), px = rect.x + (rect.w - pw) / 2, py = rect.y + (rect.h - ph) / 2, cx = rect.x + rect.w / 2;
  art.panel(ctx, "panel_dark", px, py, pw, ph, 18);
  text(ctx, title, cx, py + 50, 38, "#ffe082", "center", 900);
  const starRects = [];
  if (stars !== null) {
    const s = 40;
    for (let i = 0; i < 3; i++) {
      const r = { x: cx - 1.5 * s - 6 + i * (s + 6), y: py + 78, w: s, h: s, on: i < stars };
      if (!art.draw(ctx, r.on ? "star_on" : "star_off", r.x, r.y, s, s)) text(ctx, r.on ? "★" : "☆", r.x + s / 2, r.y + s / 2, 32, "#ffd65a", "center", 800);
      starRects.push(r);
    }
  }
  ctx.font = `600 17px ${FONT}`;
  wrap(ctx, sub, pw - 30, 3).forEach((ln, i) => text(ctx, ln, cx, py + 96 + starH + i * 24, 17, "#fff", "center", 600));
  if (action) {
    ctx.font = `600 14px ${FONT}`;
    text(ctx, fit(ctx, action, pw - 30), cx, py + ph - 20, 14, "rgba(255,236,170,.9)", "center", 600);
  }
  return { panel: { x: px, y: py, w: pw, h: ph }, stars: starRects };
}

// 棋盘上沿的横幅（同原桌面版 drawToolBannerArt / drawTipBannerArt：panel_gold 底 + 图标或按键芯片 + 文字）。
// icon：贴图名；key：按键字母（画成小键帽）；返回横幅矩形。
export function drawBanner(ctx, art, board, s, { icon = null, key = null } = {}) {
  ctx.font = `700 18px ${FONT}`;
  const tw = ctx.measureText(s).width, lead = icon || key ? 30 : 0, w = Math.min(board.w - 16, 24 + lead + tw), h = 32;
  const x = board.x + (board.w - w) / 2, y = board.y + 6;
  if (!art.panel(ctx, "panel_gold", x, y, w, h, 12)) { ctx.fillStyle = "rgba(90,60,10,.9)"; ctx.fillRect(x, y, w, h); }
  let tx = x + 12;
  if (icon && art.draw(ctx, icon, tx, y + 5, 22, 22)) tx += lead;
  else if (key) { keyCap(ctx, art, tx, y + 5, key); tx += lead; }
  text(ctx, fit(ctx, s, x + w - 12 - tx), tx, y + h / 2, 18, "#fff8e1", "left", 700);
  return { x, y, w, h, text: s };
}

// 小键帽：22 × 22 的 panel_chip + 金色字母（同原桌面版 keyChipA）
export function keyCap(ctx, art, x, y, ch, size = 22) {
  if (!art.panel(ctx, "panel_chip", x, y, size, size, 6)) { ctx.fillStyle = "#3a3060"; ctx.fillRect(x, y, size, size); }
  text(ctx, ch, x + size / 2, y + size / 2 + 0.5, Math.round(size * 0.62), "#ffdc78", "center", 800);
}

// 按键条（同原桌面版 drawHelpStripArt：开局 / 取消暂停后 300 帧，棋盘底部浮层）：各键帽 + 「P：暂停并查看全部按键」。
// 只在有键盘鼠标的设备上画（调用方判断 (hover: hover) and (pointer: fine)）。返回矩形。
export const HELP_KEYS = "H123USDMRKBNP";
export function drawHelpStrip(ctx, art, board) {
  const h = 30, x = board.x + 8, w = board.w - 16, y = board.y + board.h - h - 6;
  ctx.save(); ctx.globalAlpha = 0.94;
  if (!art.panel(ctx, "panel_chip", x, y, w, h, 10)) { ctx.fillStyle = "rgba(30,24,60,.9)"; ctx.fillRect(x, y, w, h); }
  ctx.restore();
  const step = Math.min(24, (w - 190) / HELP_KEYS.length), cap = Math.min(20, step - 2);
  [...HELP_KEYS].forEach((ch, i) => keyCap(ctx, art, x + 6 + i * step, y + (h - cap) / 2, ch, cap));
  const tx = x + 6 + HELP_KEYS.length * step + 4;
  ctx.font = `700 14px ${FONT}`;
  text(ctx, fit(ctx, "P：暂停并查看全部按键", x + w - 6 - tx), tx, y + h / 2, 14, "#fff8e1", "left", 700);
  return { x, y, w, h };
}
