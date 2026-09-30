// 贴图集（网页版）：加载 atlas.webp + atlas.json + background.webp（由 web/tools/gen_web_atlas.py 从桌面资源生成），
// 提供与桌面 app/Art.hs 对应的绘制原语：
//   draw  = drawSprite（整张贴图画进目标矩形）
//   mod   = drawSpriteMod（着色 + 透明度，SDL textureColorMod / AlphaMod）
//   add   = drawSpriteAdd（叠加混合发光，SDL BlendAdditive → Canvas "lighter"）
//   ex    = drawSpriteEx（绕中心旋转 / 水平翻转）
//   panel = drawPanel（九宫格面板）
// 贴图都是 2x 烘焙（格 56 逻辑像素 → 112 像素），画布按 devicePixelRatio 放大后备缓冲，Retina 上约 1:1。
// 着色版本按 (贴图, 颜色) 缓存到离屏画布：先画贴图，再 multiply 填色，最后 destination-in 恢复原 alpha。

function loadImage(src) {
  const im = new Image();
  im.src = src;
  return im.decode().then(() => im);
}

export async function loadArt() {
  const [meta, img, bg] = await Promise.all([
    fetch("atlas.json").then((r) => r.json()),
    loadImage("atlas.webp"),
    loadImage("background.webp"),
  ]);
  return new Art(meta, img, bg);
}

export class Art {
  constructor(meta, img, bg) {
    this.S = meta.sprites;
    this.img = img;
    this.bg = bg;
    this.tints = new Map();
  }

  has(name) { return Object.prototype.hasOwnProperty.call(this.S, name); }

  // 整张贴图画进 (x,y,w,h)；缺图返回 false，调用方退回几何画法
  draw(ctx, name, x, y, w, h) {
    const s = this.S[name];
    if (!s) return false;
    ctx.drawImage(this.img, s[0], s[1], s[2], s[3], x, y, w, h);
    return true;
  }

  // 着色后的离屏画布（白色直接用原图）
  source(name, rgb) {
    const s = this.S[name];
    if (!rgb || (rgb[0] === 255 && rgb[1] === 255 && rgb[2] === 255)) return [this.img, s[0], s[1], s[2], s[3]];
    const key = `${name}|${rgb[0]}|${rgb[1]}|${rgb[2]}`;
    let c = this.tints.get(key);
    if (!c) {
      c = document.createElement("canvas");
      c.width = s[2]; c.height = s[3];
      const g = c.getContext("2d");
      g.drawImage(this.img, s[0], s[1], s[2], s[3], 0, 0, s[2], s[3]);
      g.globalCompositeOperation = "multiply";
      g.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
      g.fillRect(0, 0, s[2], s[3]);
      g.globalCompositeOperation = "destination-in";
      g.drawImage(this.img, s[0], s[1], s[2], s[3], 0, 0, s[2], s[3]);
      if (this.tints.size > 600) this.tints.clear();   // 防止彩色流转等无限增长
      this.tints.set(key, c);
    }
    return [c, 0, 0, s[2], s[3]];
  }

  // 着色 + 透明度（alpha 0..255）
  mod(ctx, name, x, y, w, h, rgb, alpha = 255, op = "source-over") {
    if (!this.has(name) || alpha <= 0) return this.has(name);
    const [src, sx, sy, sw, sh] = this.source(name, rgb);
    const a0 = ctx.globalAlpha, o0 = ctx.globalCompositeOperation;
    ctx.globalAlpha = a0 * Math.min(1, alpha / 255);
    ctx.globalCompositeOperation = op;
    ctx.drawImage(src, sx, sy, sw, sh, x, y, w, h);
    ctx.globalAlpha = a0; ctx.globalCompositeOperation = o0;
    return true;
  }

  // 叠加混合（发光 / 闪白）
  add(ctx, name, x, y, w, h, rgb, alpha = 255) { return this.mod(ctx, name, x, y, w, h, rgb, alpha, "lighter"); }

  // 绕目标矩形中心旋转（角度，顺时针）/ 水平翻转
  ex(ctx, name, x, y, w, h, deg, flipH = false) {
    const s = this.S[name];
    if (!s) return false;
    ctx.save();
    ctx.translate(x + w / 2, y + h / 2);
    if (deg) ctx.rotate((deg * Math.PI) / 180);
    if (flipH) ctx.scale(-1, 1);
    ctx.drawImage(this.img, s[0], s[1], s[2], s[3], -w / 2, -h / 2, w, h);
    ctx.restore();
    return true;
  }

  // 九宫格：源图四角各取 1/4 边长，目标角半径 c，中间拉伸（同 Art.drawPanelMod）；rgb 给出时先着色（同 drawPanelTint）
  panel(ctx, name, x, y, w, h, c, rgb = null) {
    const s = this.S[name];
    if (!s) return false;
    const [img, sx, sy, sw, sh] = this.source(name, rgb), k = Math.floor(sw / 4);
    const cc = Math.max(1, Math.min(c, Math.floor(w / 2), Math.floor(h / 2)));
    const SX = [sx, sx + k, sx + sw - k], SW = [k, sw - 2 * k, k];
    const SY = [sy, sy + k, sy + sh - k], SH = [k, sh - 2 * k, k];
    const DX = [x, x + cc, x + w - cc], DW = [cc, w - 2 * cc, cc];
    const DY = [y, y + cc, y + h - cc], DH = [cc, h - 2 * cc, cc];
    for (let j = 0; j < 3; j++) for (let i = 0; i < 3; i++)
      if (DW[i] > 0 && DH[j] > 0) ctx.drawImage(img, SX[i], SY[j], SW[i], SH[j], DX[i], DY[j], DW[i], DH[j]);
    return true;
  }
}
