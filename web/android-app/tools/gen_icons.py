#!/usr/bin/env python3
"""由网页图集里的宝石精灵生成 Android 启动图标与启动画面图（只需运行一次，结果提交进仓库）。

用法：python3 tools/gen_icons.py [--atlas-dir ../dist]
读取 atlas.webp + atlas.json（web/build.sh 的产物），写入 android/app/src/main/res/：
  mipmap-*/ic_launcher.png、ic_launcher_round.png   旧版（API < 26）图标：深色圆角方块 / 圆形底 + 2×2 宝石
  mipmap-*/ic_launcher_foreground.png                自适应图标前景（108dp 画布，内容在 66dp 安全区内）
  drawable-*/splash_gems.png                         启动画面中央的宝石图（配 drawable/splash.xml 使用）
自适应图标背景色在 values/ic_launcher_background.xml（与游戏背景同色系）。需要 Pillow（带 WebP）。
"""
import argparse
import json
from pathlib import Path

from PIL import Image, ImageDraw

HERE = Path(__file__).resolve().parent.parent
RES = HERE / "android" / "app" / "src" / "main" / "res"
DENSITIES = {"mdpi": 1.0, "hdpi": 1.5, "xhdpi": 2.0, "xxhdpi": 3.0, "xxxhdpi": 4.0}
BG = (36, 27, 63, 255)          # #241b3f，比游戏背景 #1c1630 稍亮，图标在深色桌面上也能看清
GEMS = ["gem_c1", "gem_c2", "gem_c3", "gem_c4"]   # 红、绿、蓝、星


def load_gems(atlas_dir: Path):
    meta = json.loads((atlas_dir / "atlas.json").read_text())
    sheet = Image.open(atlas_dir / "atlas.webp").convert("RGBA")
    out = []
    for name in GEMS:
        x, y, w, h = meta["sprites"][name]
        out.append(sheet.crop((x, y, x + w, y + h)))
    return out


def grid(gems, size: int) -> Image.Image:
    """2×2 宝石方阵，边长 size 像素（格间留 4% 空隙）。"""
    img = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    gap = round(size * 0.04)
    cell = (size - gap) // 2
    for i, g in enumerate(gems):
        gi = g.resize((cell, cell), Image.LANCZOS)
        img.alpha_composite(gi, ((i % 2) * (cell + gap), (i // 2) * (cell + gap)))
    return img


def centered(canvas: Image.Image, art: Image.Image) -> Image.Image:
    canvas.alpha_composite(art, ((canvas.width - art.width) // 2, (canvas.height - art.height) // 2))
    return canvas


def legacy(gems, px: int, round_: bool) -> Image.Image:
    # 先画 4 倍再缩小，边缘抗锯齿
    big = px * 4
    base = Image.new("RGBA", (big, big), (0, 0, 0, 0))
    d = ImageDraw.Draw(base)
    m = round(big * 0.04)
    if round_:
        d.ellipse((m, m, big - m, big - m), fill=BG)
        art = grid(gems, round(big * 0.60))
    else:
        d.rounded_rectangle((m, m, big - m, big - m), radius=round(big * 0.18), fill=BG)
        art = grid(gems, round(big * 0.72))
    return centered(base, art).resize((px, px), Image.LANCZOS)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--atlas-dir", default=str(HERE.parent / "dist"), help="含 atlas.webp / atlas.json 的目录（默认 web/dist）")
    a = ap.parse_args()
    gems = load_gems(Path(a.atlas_dir))
    for dens, k in DENSITIES.items():
        mip = RES / f"mipmap-{dens}"
        mip.mkdir(parents=True, exist_ok=True)
        legacy(gems, round(48 * k), False).save(mip / "ic_launcher.png", optimize=True)
        legacy(gems, round(48 * k), True).save(mip / "ic_launcher_round.png", optimize=True)
        fg = Image.new("RGBA", (round(108 * k), round(108 * k)), (0, 0, 0, 0))
        centered(fg, grid(gems, round(60 * k))).save(mip / "ic_launcher_foreground.png", optimize=True)
        dr = RES / f"drawable-{dens}"
        dr.mkdir(parents=True, exist_ok=True)
        grid(gems, round(120 * k)).save(dr / "splash_gems.png", optimize=True)
    print(f"已写入 {RES}")


if __name__ == "__main__":
    main()
