#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
网页版贴图集生成器：把 assets/ 里的资源（assets/atlas*.bmp + atlas.txt + background.bmp，由 tools/gen_assets.py 生成）
重新打包成浏览器用的 2x WebP 图集。只读 assets/，不改 assets/ 与生成器 tools/gen_assets.py。

输出（到指定目录，默认 web/.cache/art/）：
  atlas.webp      2x 贴图集（一页，1024 宽；RGBA，有损 WebP，alpha 无损）
  atlas.json      {"scale":2,"cell":112,"size":[w,h],"sprites":{"名字":[x,y,w,h],...}}
  background.webp 窗口背景（960x1176 = 480x588 的 2x，有损 WebP）

取舍：
  - 收 assets/ 的全部贴图（2x 烘焙：格 56 逻辑像素 → 贴图 112 像素）。原桌面版的 g_* 字形、zh_* 中文标签与
    `名字@高度` 尺寸变体已从 gen_assets.py 删除（refactor/web-only-2），这里原先跳过它们的过滤随之去掉；
    唯一的文字贴图是关卡名 name_<i>（第 N 关 = name_<N−1>，见 www/hud.js 的 levelName）；
  - 贴图之间留 2 像素透明缝，避免缩放采样串色。
用法：python3 web/tools/gen_web_atlas.py [--assets assets] [--out web/.cache/art] [--quality 90]
依赖：Pillow（需带 WebP 支持）。结果是确定的：同样的输入得到同样的布局。
"""
import argparse
import json
import os
import sys

from PIL import Image, features

GAP = 2
WIDTH = 1024


def load_index(assets):
    entries = []
    with open(os.path.join(assets, "atlas.txt"), encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            name, x, y, w, h, page = line.split()
            entries.append((name, int(x), int(y), int(w), int(h), int(page)))
    return entries


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--assets", default="assets")
    ap.add_argument("--out", default="web/.cache/art")
    ap.add_argument("--quality", type=int, default=90)
    a = ap.parse_args()
    if not features.check("webp"):
        sys.exit("Pillow 没有 WebP 支持")
    os.makedirs(a.out, exist_ok=True)

    pages = {}

    def page(n):
        if n not in pages:
            fn = "atlas.bmp" if n == 0 else f"atlas{n}.bmp"
            pages[n] = Image.open(os.path.join(a.assets, fn)).convert("RGBA")
        return pages[n]

    sprites = [(n, page(p).crop((x, y, x + w, y + h))) for (n, x, y, w, h, p) in load_index(a.assets)]
    # 货架式装箱：按高度降序、再按名字，行满换行
    sprites.sort(key=lambda s: (-s[1].size[1], s[0]))
    x = y = row = 0
    placed = []
    for name, im in sprites:
        w, h = im.size
        if x + w > WIDTH:
            x, y, row = 0, y + row + GAP, 0
        placed.append((name, im, x, y))
        x += w + GAP
        row = max(row, h)
    height = y + row
    sheet = Image.new("RGBA", (WIDTH, height), (0, 0, 0, 0))
    index = {}
    for name, im, px, py in placed:
        sheet.paste(im, (px, py))
        index[name] = [px, py, im.size[0], im.size[1]]

    sheet.save(os.path.join(a.out, "atlas.webp"), "WEBP", quality=a.quality, alpha_quality=100, method=6)
    meta = {"scale": 2, "cell": 112, "size": [WIDTH, height], "sprites": dict(sorted(index.items()))}
    with open(os.path.join(a.out, "atlas.json"), "w", encoding="utf-8") as f:
        json.dump(meta, f, separators=(",", ":"), ensure_ascii=False)
    bg = Image.open(os.path.join(a.assets, "background.bmp")).convert("RGB")
    bg.save(os.path.join(a.out, "background.webp"), "WEBP", quality=a.quality, method=6)

    sz = lambda fn: os.path.getsize(os.path.join(a.out, fn))
    print(f"网页图集：{len(index)} 张贴图，{WIDTH}x{height}；atlas.webp {sz('atlas.webp')} 字节，"
          f"atlas.json {sz('atlas.json')} 字节，background.webp {sz('background.webp')} 字节")


if __name__ == "__main__":
    main()
