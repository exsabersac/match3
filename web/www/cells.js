// 单格与棋盘底层绘制（网页版）：对应桌面 app/UI/CellTable.hs（元素 → 渲染器查表）、UI/Cell/Art.hs（各元素贴图画法）、
// UI/Ground.hs（地面层）与 UI/BoardArt.hs 的棋盘底层。格子 JSON 见 Match3Web.Api.encodeCell（t = 元素种类）。
// 主贴图缺失时逐格退回几何画法（纯色块），不会因为缺图而崩。

// 坐标约定：棋盘层一律用「设计单位」——格 = 56、棋盘外框留白 PAD = 16（与桌面逻辑像素相同），
// 由 layout.js 的变换把设计单位映射到屏幕（按可用空间算出的格子大小 × devicePixelRatio）。
// 行列数按关卡盘面设置（setDims）；当前 49 关里前 48 关是 8×8，第 49 关「宽域」是 6 行 × 9 列，绘制不假设正方形。
export const CELL = 56, PAD = 16;
export let ROWS = 8, COLS = 8;
export function setDims(rows, cols) { ROWS = rows; COLS = cols; }
export const boardW = () => COLS * CELL, boardH = () => ROWS * CELL;

// 颜色表与桌面逐项比对（test/Spec/WebColors.hs，改这里或桌面任一边都要两边一起改）。
// 五色主色（与 tools/gen_assets.py 调色板、UI.Palette.colorRGB 一致）
export const COLOR_RGB = { 1: [236, 62, 78], 2: [52, 196, 96], 3: [56, 128, 246], 4: [255, 194, 36], 5: [172, 88, 236] };
// 按元素名取色（UI.Presentation.elementRGBTable）：生长前沿光 / 自定义格
export const ELEMENT_RGB = { vine: [110, 220, 90], choco: [150, 90, 45], steam: [225, 225, 235], jelly: [240, 110, 180], bubble: [150, 215, 250], magic_stone: [92, 60, 160],
  fuzzball: [196, 150, 170] };   // 毛球：同桌面几何版 UI.Cell.Prim.primFuzzball 的灰粉色（降级色 / 消灭时的粒子色）

export const clamp = (lo, hi, v) => Math.max(lo, Math.min(hi, v));
export const breathe = (pulse, period) => 0.5 + 0.5 * Math.sin((pulse * 2 * Math.PI) / period);
export const origin = ([r, c]) => [PAD + c * CELL, PAD + r * CELL];
const gemSprite = (c) => `gem_c${c}`;

// 粒子 / 退回画法颜色（UI.Palette.cellRGB）
export function cellRGB(cell) {
  if (!cell) return [160, 160, 170];
  switch (cell.t) {
    case "G": case "balloon": case "maker": case "flip": case "bottle": case "countdown": return COLOR_RGB[cell.c] || [200, 200, 200];
    case "stone": return [120, 120, 130];
    case "chest": return [220, 170, 60];
    case "honey": return [240, 180, 40];
    case "cookie": return [210, 160, 90];
    case "cake": return [255, 140, 180];
    case "hat": return [140, 90, 200];
    case "snail": return [90, 160, 70];
    case "safe": return [180, 150, 40];
    case "surprise": return [255, 100, 160];
    case "spirit": return [80, 220, 255];
    case "custom": return cell.name === "chameleon" && cell.c ? COLOR_RGB[cell.c] : ELEMENT_RGB[cell.name] || [160, 160, 170];   // 变色龙：当前颜色（同桌面 cellRGB）
    default: return [160, 160, 170];
  }
}

// 主贴图名（缺图检测与缩放绘制用；UI.CellTable.primarySprite）
export function primarySprite(cell) {
  if (cell.t === "custom" && cell.name === "snow_boss" && !forceGeneric.has("snow_boss")) return `snow_boss_${cell.q}`;   // 雪怪 Boss：本格象限（同桌面 customTable）
  switch (cell.t) {
    case "G": return cell.k === "R" ? "rainbow" : gemSprite(cell.c);
    case "stone": return "stone_3";
    case "chest": return "chest";
    case "honey": return "honey";
    case "balloon": return `balloon_c${cell.c}`;
    case "cookie": return "cookie";
    case "cake": return "cake_1";
    case "hat": return "magic_hat";
    case "maker": return `maker_c${cell.c}`;
    case "snail": return "snail";
    case "safe": return "safe";
    case "flip": return gemSprite(cell.c);
    case "surprise": return "surprise";
    case "bottle": return `bottle_c${cell.c}`;
    case "spirit": return "time_spirit";
    case "countdown": return gemSprite(cell.c);
    case "custom":
      if (cell.name === "magic_stone") return `magic_stone_${clamp(0, 3, cell.v)}`;   // 魔法石按充能取贴图
      // 变色龙：当前颜色的宝石（c 由 Api 按核心 chameleonColor 解码）。桌面 customTable 的主贴图是环 "chameleon"，
      // 缩放画法（消失 / 缩放段）因此只画环；网页缩放画法画当前颜色的宝石（见 docs/web.md §8.1）
      if (cell.name === "chameleon" && !forceGeneric.has("chameleon")) return gemSprite(cell.c);
      return cell.name;
    default: return "";
  }
}

// 蜗牛朝向 → (角度, 水平翻转)；贴图朝右
export function snailPose(dr, dc) {
  if (Math.abs(dc) >= Math.abs(dr) && dc >= 0) return [0, false];
  if (Math.abs(dc) >= Math.abs(dr)) return [0, true];
  return dr > 0 ? [90, false] : [-90, false];
}

function badgeAt(ctx, art, x, y, n) { art.draw(ctx, `badge_${clamp(1, 9, n)}`, x + CELL - 23, y + CELL - 23, 23, 23); }
function layerBadge(ctx, art, x, y, n) { if (n >= 2) badgeAt(ctx, art, x, y, n); }

// 元素 → 贴图画法（UI.Cell.Art 的 artGem / artStone / …）。参数：画布、贴图集、呼吸计数、格左上角、格子。
const OVERLAY_SPRITE = {
  grass: () => "grass", vine: () => "vine", choco: () => "choco", steam: () => "steam",
  fog: (n) => `fog_${clamp(1, 2, n)}`, chain: (n) => `chain_${clamp(1, 2, n)}`,
  freeze: (n) => `freeze_${clamp(1, 2, n)}`, curtain: (n) => `curtain_${clamp(1, 2, n)}`,
};
// 轻微上下浮动（桌面 UI.Cell.Art.cellKit 的 sprBob：bob = round (2 * sin (pulse / 9))，振幅 2 设计像素、周期 2π×9 ≈ 56.5 个呼吸计数）。
// 气球 / 时间精灵 / 气泡 / 满格魔法石 / 毛球共用。呼吸计数每个逻辑帧 +1：桌面固定 16 ms 一帧（≈ 0.90 s 一个周期），
// 网页固定 1/60 s 一帧（≈ 0.94 s），同一个公式、慢约 4%，与其它浮动元素保持一致，不单独折算。
const bobY = (pulse) => Math.round(2 * Math.sin(pulse / 9));
const CELL_ART = {
  G(ctx, art, pulse, x, y, cell) {
    const spr = (n) => art.draw(ctx, n, x, y, CELL, CELL);
    if (cell.k === "B") art.mod(ctx, "bomb_glow", x, y, CELL, CELL, null, Math.round(140 + 110 * breathe(pulse, 50)));
    if (cell.k === "R") art.ex(ctx, "rainbow", x, y, CELL, CELL, (pulse * 2) % 360);
    else spr(gemSprite(cell.c));
    if (cell.k === "H") spr("line_h");
    else if (cell.k === "V") spr("line_v");
    else if (cell.k === "B") spr("bomb_mark");
    if (cell.i > 0) spr(`ice_${clamp(1, 3, cell.i)}`);
    let layers = 0;
    if (cell.o && OVERLAY_SPRITE[cell.o]) { spr(OVERLAY_SPRITE[cell.o](cell.n)); layers = cell.n || 0; }
    layerBadge(ctx, art, x, y, layers > 0 ? layers : cell.i);
  },
  stone(ctx, art, p, x, y, c) { art.draw(ctx, `stone_${clamp(1, 3, c.n)}`, x, y, CELL, CELL); layerBadge(ctx, art, x, y, c.n); },
  chest(ctx, art, p, x, y, c) { art.draw(ctx, "chest", x, y, CELL, CELL); layerBadge(ctx, art, x, y, c.n); },
  honey(ctx, art, p, x, y, c) { art.draw(ctx, "honey", x, y, CELL, CELL); layerBadge(ctx, art, x, y, c.n); },
  balloon(ctx, art, p, x, y, c) { art.draw(ctx, `balloon_c${c.c}`, x, y + bobY(p), CELL, CELL); },
  cookie(ctx, art, p, x, y) { art.draw(ctx, "cookie", x, y, CELL, CELL); },
  cake(ctx, art, p, x, y, c) { art.draw(ctx, `cake_${clamp(1, 3, c.n)}`, x, y, CELL, CELL); layerBadge(ctx, art, x, y, c.n); },
  hat(ctx, art, p, x, y) { art.draw(ctx, "magic_hat", x, y, CELL, CELL); },
  maker(ctx, art, p, x, y, c) { art.draw(ctx, `maker_c${c.c}`, x, y, CELL, CELL); badgeAt(ctx, art, x, y, Math.max(1, c.n)); },
  snail(ctx, art, p, x, y, c) { const [a, f] = snailPose(c.dr, c.dc); art.ex(ctx, "snail", x, y, CELL, CELL, a, f); },
  safe(ctx, art, p, x, y, c) { art.draw(ctx, "safe", x, y, CELL, CELL); layerBadge(ctx, art, x, y, c.n); },
  flip(ctx, art, p, x, y, c) {
    art.draw(ctx, gemSprite(c.c), x, y, CELL, CELL);
    art.draw(ctx, gemSprite(c.b), x + CELL - 25, y + 1, 24, 24);   // 右上角 = 翻面后的颜色
    art.draw(ctx, "flip_mark", x, y, CELL, CELL);
  },
  surprise(ctx, art, p, x, y) { art.draw(ctx, "surprise", x, y, CELL, CELL); },
  bottle(ctx, art, p, x, y, c) { art.draw(ctx, `bottle_c${c.c}`, x, y, CELL, CELL); },
  spirit(ctx, art, p, x, y) { art.draw(ctx, "time_spirit", x, y + bobY(p), CELL, CELL); },
  countdown(ctx, art, p, x, y, c) { art.draw(ctx, gemSprite(c.c), x, y, CELL, CELL); art.draw(ctx, `countdown_${clamp(1, 9, c.n)}`, x, y, CELL, CELL); },
  custom(ctx, art, p, x, y, c) {
    // 气泡有专门画法（轻微浮动、无角标）；其它自定义元素：贴图名 = 元素名 + 层数角标
    if (c.name === "bubble") art.draw(ctx, "bubble", x, y + bobY(p), CELL, CELL);
    // 新玩法 2 魔法石：按充能 v 画 magic_stone_0..3（满 3 格时浮动），同桌面 UI.Cell.Art.artMagicStone；不画层数角标
    else if (c.name === "magic_stone") {
      art.draw(ctx, primarySprite(c), x, y + (c.v >= 3 ? bobY(p) : 0), CELL, CELL);
    }
    // 新玩法 3 毛球：贴图 fuzzball，一直轻微浮动（它每步会跳），同桌面 UI.Cell.Art.artFuzzball（sprBob "fuzzball"）；不画状态角标
    else if (c.name === "fuzzball") art.draw(ctx, "fuzzball", x, y + bobY(p), CELL, CELL);
    else { noteGenericCustom(c); art.draw(ctx, c.name, x, y, CELL, CELL); layerBadge(ctx, art, x, y, c.v); }
  },
};

// 按 Custom 名字分派的专门画法（同桌面 UI.CellTable.customTable；查不到的名字走 CELL_ART.custom）。
// 新玩法 5 雪怪 Boss（Custom "snow_boss"，占 2×2）：同桌面 UI.Cell.Art.artSnowBoss——每格画整只雪怪的四分之一
// snow_boss_<象限>（血量 ≤ 满血一半换 snow_boss_hurt_<象限> 受伤表情）；右下格底部画召唤进度小点（每 3 次交换召唤一块雪块，
// 点亮已走的次数）。象限 q / 受伤 hurt / 计数 turn / 周期 every 由 Api 按 Match3.View.bossPart 解码给出，这里不拆 v。
// 四块拼成一只：画布缩放时双线性采样会从图集里贴图外的透明缝取色，格子边又落在小数像素上，四块之间会露出一条细缝（十字线）。
// 这里在朝向另外三块的两条内边上把源矩形各收 1 个源像素，并把目标矩形对齐到后备缓冲的整像素（相邻格算出的边界相同）；
// 桌面按 1:1 画，没有这个问题。
function drawSnowBoss(ctx, art, pulse, x, y, c) {
  const s = art.S[`${c.hurt ? "snow_boss_hurt_" : "snow_boss_"}${c.q}`];
  if (!s) return;
  const left = c.q % 2 === 0, top = c.q < 2;
  const src = [s[0] + (left ? 0 : 1), s[1] + (top ? 0 : 1), s[2] - 1, s[3] - 1];
  const m = ctx.getTransform ? ctx.getTransform() : null;
  if (m && m.b === 0 && m.c === 0) {
    const X0 = Math.round(m.a * x + m.e), Y0 = Math.round(m.d * y + m.f);
    const X1 = Math.round(m.a * (x + CELL) + m.e), Y1 = Math.round(m.d * (y + CELL) + m.f);
    ctx.save(); ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.drawImage(art.img, ...src, X0, Y0, X1 - X0, Y1 - Y0);
    ctx.restore();
  } else ctx.drawImage(art.img, ...src, x, y, CELL, CELL);
  if (c.q !== 3) return;
  for (let i = 0; i < c.every; i++) {
    ctx.fillStyle = i < c.turn ? "rgb(120,210,255)" : "rgba(40,60,110,0.784)";
    ctx.fillRect(x + CELL - 12 - (c.every - 1 - i) * 9, y + CELL - 11, 6, 6);
  }
}
// 新玩法 7 变色龙（Custom "chameleon"，v = 颜色下标 0..4）：同桌面 UI.Cell.Art.artChameleon——先画当前颜色的宝石
// gem_c<c>（c = 1..5 由 Api 按核心 Match3.Element.Builtin.chameleonColor 解码，前端不拆 v），再叠一张缓慢旋转的五色描边环
// chameleon（角度 = 呼吸计数 mod 360 度，每个逻辑帧 1 度：桌面 16 ms 一帧约 5.8 s 一圈，网页 1/60 s 一帧 6 s 一圈）。
// 每步换色是步末 EvTick "chameleon"（原格改写），render.js 的倒计时段照常播：前半段旧色、后半段新色，全程红光脉冲。
function drawChameleon(ctx, art, pulse, x, y, c) {
  art.draw(ctx, gemSprite(c.c), x, y, CELL, CELL);
  if (!art.ex(ctx, "chameleon", x, y, CELL, CELL, pulse % 360)) fallbacks["chameleon#环"] = (fallbacks["chameleon#环"] || 0) + 1;
}
const CUSTOM_ART = { snow_boss: drawSnowBoss, chameleon: drawChameleon };

// 多格元素护栏：占多格的 Custom 元素（格子 JSON 带 q = 本格在整体里的编号，如雪怪 Boss 的象限）不能走通用的「贴图名 = 元素名
// + 层数角标」画法——那样每格都画一只缩小的整只贴图，v 是打包值时角标还会夹成 9（第 45 关网页版接入前就是这样）；
// 因为元素名贴图本身在图集里，普通降级护栏查不出来。走到通用画法时按「元素名#多格通用画法」计进 fallbacks，e2e 逐关要求为空。
// forceGeneric：e2e 的反证用（在页面里 import 本模块后临时加入元素名，强制走旧的通用画法，护栏必须报错）。
export const forceGeneric = new Set();
// 同理，带颜色 c 的 Custom 格（第 47 关变色龙：本体是一颗当前颜色的宝石）走通用画法时只画元素名贴图（变色龙的环）、
// 底下没有宝石，也看不出颜色（第 47 关网页版接入前就是这样），记为「元素名#通用画法缺底层宝石」。
const isMultiCell = (c) => c.t === "custom" && c.q !== undefined;
const isColoredCustom = (c) => c.t === "custom" && c.c !== undefined;
function noteGenericCustom(c) {
  const bump = (k) => { fallbacks[k] = (fallbacks[k] || 0) + 1; };
  if (isMultiCell(c)) bump(`${c.name}#多格通用画法`);
  if (isColoredCustom(c)) bump(`${c.name}#通用画法缺底层宝石`);
}

// 回归护栏：走几何降级（drawCellPrim，以及缩放画法 drawCellScaled 的色块分支）的次数，按元素名计
// （custom 取 name，如 "magic_stone"；其余取 t）。贴图在开局前就加载好，正常游戏里它应当一直为空；
// 非空 = 有元素在网页图集里没有贴图 / cells.js 没有画法（新元素合入 main 后要在本文件补）。main.js 以 m3debug.fallbacks 暴露给 e2e。
export const fallbacks = {};
function noteFallback(cell) {
  const k = cell.t === "custom" ? cell.name : cell.t;
  fallbacks[k] = (fallbacks[k] || 0) + 1;
}

// 几何降级：纯色圆角块 + 元素缩写
function drawCellPrim(ctx, x, y, cell) {
  noteFallback(cell);
  const [r, g, b] = cellRGB(cell);
  ctx.fillStyle = `rgb(${r},${g},${b})`;
  ctx.fillRect(x + 4, y + 4, CELL - 8, CELL - 8);
  ctx.fillStyle = "#fff"; ctx.font = "10px sans-serif"; ctx.textAlign = "center"; ctx.textBaseline = "middle";
  ctx.fillText(cell.t === "G" ? cell.k : cell.t.slice(0, 6), x + CELL / 2, y + CELL / 2);
}

// 单格：按元素查表画贴图；闪白统一叠一层柔光（UI.BoardArt.drawCellArt）
export function drawCell(ctx, art, pulse, x, y, cell, flashing = false) {
  if (!cell) return;
  const f = (cell.t === "custom" && !forceGeneric.has(cell.name) && CUSTOM_ART[cell.name]) || CELL_ART[cell.t];
  if (!f || !art.has(primarySprite(cell))) drawCellPrim(ctx, x, y, cell);
  else f(ctx, art, pulse, x, y, cell);
  if (flashing) art.add(ctx, "spark", x - 10, y - 10, CELL + 20, CELL + 20, [255, 255, 230], 210);
}

// 以中心 (cx,cy) 按比例 s、透明度 a 画一格（缩放用简化贴图：主贴图 + 特殊标记；UI.BoardArt.drawCellScaled）
export function drawCellScaled(ctx, art, cx, cy, s, a, cell) {
  if (!cell || s <= 0.03 || a <= 0) return;
  const sz = Math.max(1, Math.round(CELL * s)), x = cx - sz / 2, y = cy - sz / 2, name = primarySprite(cell);
  if (art.has(name)) {
    if (cell.t === "custom" && name === cell.name) noteGenericCustom(cell);   // 缩放画法也按元素名取了整只贴图
    art.mod(ctx, name, x, y, sz, sz, null, a);
    const mark = cell.t === "G" && { H: "line_h", V: "line_v", B: "bomb_mark" }[cell.k];
    if (mark) art.mod(ctx, mark, x, y, sz, sz, null, a);
  } else {
    noteFallback(cell);
    const [r, g, b] = cellRGB(cell);
    ctx.fillStyle = `rgba(${r},${g},${b},${a / 255})`;
    ctx.fillRect(x, y, sz, sz);
  }
}

// 传送带每格的朝向角度（右 0 / 下 90 / 左 180 / 上 270；UI.BoardArt.beltAngles）
export function beltAngles(belt) {
  const out = [];
  let prev = null;
  for (let i = 0; i < belt.length; i++) {
    const [r1, c1] = belt[i], [r2, c2] = belt[(i + 1) % belt.length];
    let ang = null;
    if (r1 === r2 && c2 === c1 + 1) ang = 0;
    else if (r1 === r2 && c2 === c1 - 1) ang = 180;
    else if (c1 === c2 && r2 === r1 + 1) ang = 90;
    else if (c1 === c2 && r2 === r1 - 1) ang = 270;
    if (ang === null) ang = prev ?? 0;
    out.push([belt[i], ang]);
    prev = ang;
  }
  return out;
}

const key = ([r, c]) => r * 64 + c;

// 地面层（UI.Ground.groundTable）：名字 → 贴图名(层数)；画在棋盘格之上、棋子之下（同桌面 drawGroundArtAt）。
// 表里没有的名字或贴图缺失时画淡灰框，并按「<名字>#地面层」计进 fallbacks（e2e 逐关护栏；第 48 关魔法地格接入前就是淡灰框、护栏查不出）。
// magic：第 48 关魔法地格（新玩法 8，layers 恒为 1、只用于显示），贴图同桌面 const "magic"。
const GROUND = { jelly: (n) => (n >= 2 ? "jelly_2" : "jelly"), magic: () => "magic" };
function drawGround(ctx, art, x, y, g) {
  const f = GROUND[g.name];
  if (f && art.draw(ctx, f(g.layers), x, y, CELL, CELL)) return;
  fallbacks[`${g.name}#地面层`] = (fallbacks[`${g.name}#地面层`] || 0) + 1;
  ctx.strokeStyle = "rgba(170,170,180,.8)"; ctx.lineWidth = 1; ctx.strokeRect(x + 2.5, y + 2.5, CELL - 5, CELL - 5);
}

// 棋盘底层：圆角框 → 棋盘格 → 地毯 → 地面层（果冻）→ 传送带 → 传送门（都在棋子下面；UI.BoardArt.drawBoardBgArt）
// 整屏背景图由 main.js 在屏幕坐标里按 cover 铺满，不在这里画。
export function drawBoardBase(ctx, art, st, pulse) {
  art.panel(ctx, "panel_dark", PAD - 8, PAD - 8, boardW() + 16, boardH() + 16, 16);
  const open = new Set((st.carpetOpen || []).map(key));
  const carpets = new Set((st.carpets || []).map(key));
  const ground = new Map((st.ground || []).map((g) => [key(g.p), g]));
  for (let r = 0; r < ROWS; r++) for (let c = 0; c < COLS; c++) {
    const [x, y] = origin([r, c]), k = r * 64 + c;
    art.draw(ctx, (r + c) % 2 === 0 ? "tile_a" : "tile_b", x, y, CELL, CELL);
    if (open.has(k)) art.draw(ctx, "carpet_open", x, y, CELL, CELL);
    else if (carpets.has(k)) art.draw(ctx, "carpet_covered", x, y, CELL, CELL);
    if (ground.has(k)) drawGround(ctx, art, x, y, ground.get(k));
  }
  for (const belt of st.belts || []) for (const [p, ang] of beltAngles(belt)) {
    const [x, y] = origin(p);
    art.ex(ctx, "belt", x, y, CELL, CELL, ang);
  }
  for (const pair of st.portals || []) for (const p of pair) {
    const [x, y] = origin(p);
    art.ex(ctx, "portal", x, y, CELL, CELL, (pulse * 3) % 360);
  }
}

// 飞碟（轻微上下浮动）
export function drawUfos(ctx, art, st, pulse, yOff = 0) {
  const bob = Math.round(3 * Math.sin(pulse / 10));
  for (const u of st.ufos || []) {
    const [x, y] = origin(u.p);
    art.draw(ctx, `ufo_c${u.c}`, x, y + yOff - 12 + bob, CELL, CELL);
  }
}

// 饼干掉落口标记（新玩法 6，state.drops = 视图模型 bvDrops）：同桌面 UI.BoardArt.drawDropsArt——画在棋子之上、掉落口格上沿
// （上移 6 压在棋盘框上），固定不随下落偏移；缺图时退回几何画法（同 UI.BoardPrim.drawDropMark：三级金色台阶 + 白色箭头），
// 并按 "cookie_drop" 计进 fallbacks（e2e 逐关护栏）。
// dropMarks 记下最近一次画的标记（格子与棋盘设计坐标，seq 每画一次加 1），main.js 以 m3debug.dropMarks 暴露，
// e2e 用它核对交换补间中 / 补间结束后的标记格 = state.drops（bvDrops）、坐标 = 桌面 drawDropsArt 的 (cellOrigin, y − 6)。
export const dropMarks = { seq: 0, marks: [] };
export function drawDrops(ctx, art, st) {
  dropMarks.seq++;
  dropMarks.marks = (st.drops || []).map((p) => { const [x, y] = origin(p); return { p: [p[0], p[1]], x, y: y - 6 }; });
  for (const p of st.drops || []) {
    const [x, y] = origin(p);
    if (art.draw(ctx, "cookie_drop", x, y - 6, CELL, CELL)) continue;
    fallbacks.cookie_drop = (fallbacks.cookie_drop || 0) + 1;
    const box = (rgb, bx, by, bw, bh) => { ctx.fillStyle = `rgb(${rgb})`; ctx.fillRect(bx, by, bw, bh); }, yy = y - 3, m = x + CELL / 2;
    box("120,70,20", x + 4, yy, CELL - 8, 3);
    box("240,190,90", x + 6, yy + 3, CELL - 12, 4); box("240,190,90", x + 11, yy + 7, CELL - 22, 4);
    box("200,130,50", x + 16, yy + 11, CELL - 32, 3);
    box("255,250,230", m - 2, yy + 3, 4, 5); box("255,250,230", m - 5, yy + 8, 10, 2); box("255,250,230", m - 2, yy + 10, 4, 2);
  }
}

// 藤蔓 / 巧克力下一步可能蔓延到的格（与 UI.BoardArt.spreadTargets 相同的判定）
export function spreadTargets(board, overlay) {
  const out = [];
  for (let r = 0; r < ROWS; r++) for (let c = 0; c < COLS; c++) {
    const cell = board[r][c];
    if (!(cell.t === "G" && cell.o === overlay)) continue;
    for (const [qr, qc] of [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]]) {
      if (qr < 0 || qr >= ROWS || qc < 0 || qc >= COLS) continue;
      const q = board[qr][qc];
      if (q.t === "G" && !q.o) out.push([qr, qc]);
    }
  }
  return out;
}
