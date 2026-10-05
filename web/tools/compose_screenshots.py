#!/usr/bin/env python3
"""把 web/test/screenshots.mjs 截下的原始帧拼成 docs/images 下的 WebP（需要 Pillow 与一款中文字体）。

用法：python3 web/tools/compose_screenshots.py [原始帧目录，默认 /tmp/match3-doc-shots] [输出目录，默认 docs/images]
"""
import json, os, sys
from PIL import Image, ImageDraw, ImageFont

src = sys.argv[1] if len(sys.argv) > 1 else "/tmp/match3-doc-shots"
dst = sys.argv[2] if len(sys.argv) > 2 else os.path.join(os.path.dirname(__file__), "../../docs/images")
m = json.load(open(os.path.join(src, "manifest.json"), encoding="utf-8"))
FONTS = ["/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc", "/usr/share/fonts/noto-cjk/NotoSansCJK-Regular.ttc",
         "/usr/share/fonts/truetype/wqy/wqy-microhei.ttc"]
fpath = next((f for f in FONTS if os.path.exists(f)), None)
font = (lambda s: ImageFont.truetype(fpath, s)) if fpath else (lambda s: ImageFont.load_default())
CELL_W = 300          # 每帧缩放后的宽度（px）
BG, FG = (24, 26, 36), (230, 230, 240)


def strip(rows, cols, out):
    """rows = [(行标题, [帧文件…])]；cols = 列标题。"""
    frames = [[Image.open(os.path.join(src, f)).convert("RGB") for f in fs] for _, fs in rows]
    h = max(round(im.height * CELL_W / im.width) for r in frames for im in r)
    lw, th, gap = 150, 44, 6
    W = lw + len(cols) * (CELL_W + gap) + gap
    H = th + len(rows) * (h + gap) + gap
    canvas = Image.new("RGB", (W, H), BG)
    d = ImageDraw.Draw(canvas)
    for j, c in enumerate(cols):
        d.text((lw + gap + j * (CELL_W + gap) + CELL_W // 2, th // 2), c, fill=FG, font=font(22), anchor="mm")
    for i, ((label, _), r) in enumerate(zip(rows, frames)):
        y = th + gap + i * (h + gap)
        d.text((lw // 2, y + h // 2), label, fill=FG, font=font(22), anchor="mm")
        for j, im in enumerate(r):
            canvas.paste(im.resize((CELL_W, round(im.height * CELL_W / im.width)), Image.LANCZOS), (lw + gap + j * (CELL_W + gap), y))
    canvas.save(out, "WEBP", quality=82, method=6)
    print(out, os.path.getsize(out))


os.makedirs(dst, exist_ok=True)
p = m["portrait"]
im = Image.open(os.path.join(src, p["file"])).convert("RGB")
o = os.path.join(dst, "screenshot-l16.webp"); im.save(o, "WEBP", quality=85, method=6); print(o, os.path.getsize(o))
c = m["combo"]
strip([(f"第 {k + 1} 轮", c["files"][4 * k:4 * k + 4]) for k in range(c["rows"])],
      ["高亮", "消失", "下落", "落定"], os.path.join(dst, "combo-strip.webp"))
strip([(s["label"], s["files"]) for s in m["endStages"]], ["起始", "1/3", "2/3", "末帧"], os.path.join(dst, "end-of-step-strip.webp"))
