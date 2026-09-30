// 本关特殊格子说明：只列出这一关已经出现过的障碍、叠层、特殊块和关卡机制。
// 文案跟 docs/domain.md 对齐，给玩家看「是什么、怎么消」，不暴露内部名。
import { FONT } from "./hud.js";

const COPY = {
  ice: ["冰", "盖在宝石上。这颗宝石自己被消除时削一层，削完才能把它消掉。"],
  grass: ["草", "盖在宝石上，宝石可以正常交换和消除。这颗宝石被消掉时，草一起消失。"],
  vine: ["藤蔓", "盖在宝石上。宝石被消掉时藤蔓消失；没消掉的话，这一步结束它会蔓到旁边。"],
  choco: ["巧克力", "旁边有消除就能清掉这一格。没清掉的话，这一步结束它会蔓到旁边。"],
  fog: ["迷雾", "雾下面的宝石不能消除。在旁边消除，揭掉一层。"],
  chain: ["锁链", "锁住的宝石不能交换，也不能消除。在旁边消除，揭掉一层。锁着的时候，直线、炸弹、彩虹不会引爆。"],
  freeze: ["冰冻", "冻住的宝石不能交换，但可以参与消除。在旁边消除，揭掉一层。"],
  curtain: ["窗帘", "窗帘下的宝石不能消除。在旁边消除，揭一层。盖着的时候，直线、炸弹、彩虹不会引爆。"],
  steam: ["蒸汽", "挡住下面的宝石，不能消除。旁边一消就扑灭；没扑灭的话，这一步结束会蔓延。"],
  line: ["直线", "四连消会生成。和旁边的宝石交换就会引爆，清掉整行或整列。"],
  bomb: ["炸弹", "交换引爆后清掉周围一小片。L 形或 T 形五连、果汁机、彩蛋也可能变出炸弹。"],
  rainbow: ["彩虹", "五连消会生成。和一颗宝石交换，清掉那种颜色。"],
  stone: ["碎石", "不能交换。在旁边消除削一层，削完就碎掉。"],
  chest: ["宝箱", "不能交换。在旁边消除，或用直线、炸弹打到它，削一层。"],
  honey: ["蜂蜜罐", "不能交换。在旁边消除削一层，削完就破。"],
  balloon: ["气球", "只有旁边消掉和它同色的宝石才会爆。"],
  cookie: ["饼干", "会往下掉。掉到最底下一行就被收走。在半空打它没用。"],
  cake: ["蛋糕", "不能交换。在旁边消除削一层。"],
  hat: ["魔法帽", "旁边一消，它会把周围宝石换颜色。直线、炸弹打不掉它。"],
  maker: ["果汁机", "旁边反复消掉和它同色的宝石给它充能，充满后变成一颗炸弹。"],
  snail: ["蜗牛", "挡住交换。每走一步它爬一格，把宝石推开。"],
  safe: ["保险箱", "在旁边消除削一层。打开后变成饼干。"],
  flip: ["双面块", "按正面的颜色参与消除。被打中后翻成背面那种颜色的普通宝石。"],
  surprise: ["彩蛋", "在旁边消除就打开，可能变成特殊块，或炸开周围一小片。"],
  bottle: ["染色瓶", "在旁边消除后，把上下左右的宝石染成瓶子的颜色。"],
  spirit: ["时间精灵", "在旁边消除，这一关多 2 步。"],
  countdown: ["倒计时", "可以按它的颜色正常消除。每走一步数字减 1，减到 0 会炸开周围一小片。"],
  bubble: ["气泡", "不能交换，自己连成三排也不会消。旁边任意颜色一消，或者被特殊块打中，它就破。它会往下掉。"],
  magic_stone: ["魔法石", "固定在格子上，打不掉。旁边每消除一轮充一格，满 3 格后，这一步结束清掉它所在的整行和整列，然后重新充能。"],
  fuzzball: ["毛球", "不能交换，会往下掉。旁边一消，或被特殊块打中，它就没了。每走一步它还会跳到旁边一颗普通宝石上。"],
  snow_boss: ["雪怪", "占四格，不能交换，也不会自己消失。按目标打掉它的血；它还会隔几步召唤障碍。"],
  chameleon: ["变色龙", "按当前颜色参与消除。每走一步，它会按固定顺序换一种颜色。"],
  jelly: ["果冻", "铺在格子下面，不占地方，上面的宝石照常消。上面的宝石每被消掉或收走一次，果冻薄一层，两层都没了这一格才清完。"],
  belt: ["传送带", "这一步结束后，带子上的东西会顺着转一格。"],
  portal: ["传送门", "宝石往下掉时，一头有东西、另一头是空的，就会穿过去。"],
  ufo: ["飞碟", "一轮消除结束后，吸走旁边和它同色的宝石，然后挪到别的格子。吸走不算引爆。"],
  carpet: ["地毯", "目标是把还空着的格子铺上。格子被消除、饼干掉走或保险箱打开时，可以铺上去。"],
  drops: ["掉落口", "上面标了标记的格子，补新宝石时可能掉进饼干。"],
  bomb_shapes: ["L/T 形", "这一关同色凑成 L 形或 T 形时，交叉点会生成炸弹，而不是直线。"],
};

function push(keys, id) { if (COPY[id]) keys.add(id); }

// 扫当前局面，把出现过的特殊格子记进 set（清掉之后仍留在说明里）。
export function noteSpecials(state, keys) {
  if (!state) return keys;
  for (const row of state.board || []) {
    for (const cell of row) {
      if (!cell) continue;
      if (cell.t === "G") {
        if (cell.i > 0) push(keys, "ice");
        if (cell.o) push(keys, cell.o);
        if (cell.k === "H" || cell.k === "V") push(keys, "line");
        else if (cell.k === "B") push(keys, "bomb");
        else if (cell.k === "R") push(keys, "rainbow");
      } else if (cell.t === "custom") push(keys, cell.name);
      else push(keys, cell.t);
    }
  }
  for (const g of state.ground || []) push(keys, g.name);
  if ((state.belts || []).length) push(keys, "belt");
  if ((state.portals || []).length) push(keys, "portal");
  if ((state.ufos || []).length) push(keys, "ufo");
  if ((state.carpets || []).length || (state.carpetOpen || []).length) push(keys, "carpet");
  if ((state.drops || []).length) push(keys, "drops");
  for (const r of state.rules || []) push(keys, r.name);
  return keys;
}

export function guideEntries(keys) {
  const out = [];
  for (const id of keys) {
    const row = COPY[id];
    if (row) out.push({ title: row[0], body: row[1] });
  }
  return out;
}

function wrap(ctx, s, w) {
  const lines = [];
  let cur = "";
  for (const ch of s) {
    if (ctx.measureText(cur + ch).width > w) { if (cur) lines.push(cur); cur = ch; }
    else cur += ch;
  }
  if (cur) lines.push(cur);
  return lines;
}

// 问号按钮（设计单位），贴在棋盘右上角，不挤原来的按钮条。
export function helpButtonRect(L) {
  const size = 36;
  return { id: "help", label: "?", x: L.board.x + L.VW - 8 - size, y: L.board.y + 8, w: size, h: size };
}

export function drawHelpButton(ctx, art, rect, pressed) {
  const y = rect.y + (pressed ? 2 : 0);
  if (!art.panel(ctx, "panel_gold", rect.x, y, rect.w, rect.h, 12)) {
    ctx.fillStyle = "#554"; ctx.fillRect(rect.x, y, rect.w, rect.h);
  }
  ctx.font = `800 22px ${FONT}`;
  ctx.textAlign = "center"; ctx.textBaseline = "middle";
  ctx.fillStyle = "#ffe9a8";
  ctx.fillText("?", rect.x + rect.w / 2, y + rect.h / 2);
}

// 盖在棋盘上的说明。entries 为空时告诉玩家这一关没有特殊格子。
export function drawGuide(ctx, art, board, entries) {
  const pad = 16;
  ctx.fillStyle = "rgba(10,6,24,.78)";
  ctx.fillRect(board.x, board.y, board.w, board.h);
  const px = board.x + pad, py = board.y + pad, pw = board.w - pad * 2, ph = board.h - pad * 2;
  if (!art.panel(ctx, "panel_dark", px, py, pw, ph, 16)) {
    ctx.fillStyle = "#241c3a"; ctx.fillRect(px, py, pw, ph);
  }
  ctx.textAlign = "left"; ctx.textBaseline = "middle";
  ctx.font = `800 22px ${FONT}`;
  ctx.fillStyle = "#ffe082";
  ctx.fillText("本关特殊格子", px + 16, py + 28);
  ctx.font = `600 14px ${FONT}`;
  ctx.fillStyle = "rgba(255,248,225,.75)";
  ctx.fillText("再点问号关闭", px + pw - 16 - ctx.measureText("再点问号关闭").width, py + 28);
  let y = py + 56;
  const maxY = py + ph - 16;
  const list = entries.length ? entries : [{ title: "没有特殊格子", body: "这一关就是普通三消，三个同色连成一线就能消。" }];
  for (const e of list) {
    if (y > maxY - 20) break;
    ctx.font = `800 18px ${FONT}`;
    ctx.fillStyle = "#fff";
    ctx.fillText(e.title, px + 16, y);
    y += 22;
    ctx.font = `600 15px ${FONT}`;
    ctx.fillStyle = "#fff8e1";
    for (const ln of wrap(ctx, e.body, pw - 32)) {
      if (y > maxY - 8) return;
      ctx.fillText(ln, px + 16, y);
      y += 20;
    }
    y += 10;
  }
}

