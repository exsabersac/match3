#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
match3 美术资源生成器（程序化、可复现）。

输出（均提交到仓库）：
  assets/atlas.bmp      32 位 BGRA 贴图集第 0 页（BITMAPV4 头 + alpha 掩码，SDL2 核心 SDL_LoadBMP 可直接读取）
  assets/atlas1.bmp ... 第 1 页起（单页超过 1024x2048 时自动分页）
  assets/atlas.txt      贴图索引：每行 `名字 x y w h 页号`
  assets/background.bmp 窗口背景（960x1176 = 480x588 的 2x，24 位不透明）
  docs/images/legend.png 图例总表（中英文标注）

用法：python3 tools/gen_assets.py   （依赖 Pillow + numpy）
风格：2x 超采样绘制（格子 56px → 贴图 112px），光泽宝石 + 「颜色 × 形状」双编码。

高分屏（Retina）约定：
  - 所有 UI 贴图都按「逻辑尺寸 × 2」烘焙，Retina 上 1 个贴图像素 = 1 个物理像素。
  - 文字（中文标签 / HUD 字形）按游戏内实际使用的逻辑高度 × TS 直接用 FreeType 渲染（带 hinting），
    不再「超大字号 + 缩小」，笔画落在像素格上更锐利。
  - 同一贴图可有多个尺寸变体，命名为 `基名@像素高`（如 `zh_combo@68`、`g_48@60`、`gem_c1@56`）；
    运行时 app/Art.hs 按「目标物理高度」挑最小的够用变体，基名本身始终存在，旧名字全部可用。
"""
import math
import os
import random
import re
import struct
import sys
from pathlib import Path

import numpy as np
from PIL import Image, ImageChops, ImageDraw, ImageFilter, ImageFont

ROOT = Path(__file__).resolve().parents[1]
ASSETS = ROOT / "assets"
DOCIMG = ROOT / "docs" / "images"

S = 112          # 棋子贴图边长（游戏内按 56px 绘制，即 2x）
SS = 4           # 超采样倍数
N = S * SS       # 工作画布边长
WIN_W, WIN_H = 480, 588   # 与 app/Main.hs 的 winW / winH 一致
TS = 2           # 文字 / UI 烘焙倍率：贴图像素 = 逻辑像素 × TS（Retina 2x 下 1:1）
PAGE_W, PAGE_H = 1024, 2048   # 单页图集上限（兼顾老 GPU 的 2048 纹理限制）

# ---------------------------------------------------------------- 调色板
# 颜色 × 形状双编码：色弱玩家也能靠轮廓区分
# rgb 以 UI.Palette.colorRGB 为准（stack test 的 gen_assets_gem_palette_matches_palette 逐项比对，保持这种写法）
GEMS = {
    "c1": dict(rgb=(236, 62, 78), shape="circle", zh="红·圆", en="Red / Circle"),
    "c2": dict(rgb=(52, 196, 96), shape="square", zh="绿·方", en="Green / Square"),
    "c3": dict(rgb=(56, 128, 246), shape="diamond", zh="蓝·菱", en="Blue / Diamond"),
    "c4": dict(rgb=(255, 194, 36), shape="star", zh="黄·星", en="Yellow / Star"),
    "c5": dict(rgb=(172, 88, 236), shape="triangle", zh="紫·三角", en="Purple / Triangle"),
}
LIGHT = (-0.55, -0.83)   # 左上方光源


# ---------------------------------------------------------------- 基础工具
def clamp8(v):
    return int(max(0, min(255, round(v))))


def mix(a, b, t):
    return tuple(clamp8(a[i] + (b[i] - a[i]) * t) for i in range(3))


def lighten(c, t):
    return mix(c, (255, 255, 255), t)


def darken(c, t):
    return mix(c, (0, 0, 0), t)


def rgba(c, a=255):
    return (c[0], c[1], c[2], a)


def new(w=N, h=None):
    return Image.new("RGBA", (w, h or w), (0, 0, 0, 0))


def odd(v):
    v = max(1, int(round(v)))
    return v if v % 2 == 1 else v + 1


def U(v):
    """单位坐标（0..1）→ 工作画布像素。"""
    return v * N


def pts_px(pts):
    return [(U(x), U(y)) for x, y in pts]


def mask_new():
    return Image.new("L", (N, N), 0)


def poly_mask(pts):
    m = mask_new()
    ImageDraw.Draw(m).polygon(pts, fill=255)
    return m


def ellipse_mask(box):
    m = mask_new()
    ImageDraw.Draw(m).ellipse(box, fill=255)
    return m


def rrect_mask(box, r):
    m = mask_new()
    ImageDraw.Draw(m).rounded_rectangle(box, radius=r, fill=255)
    return m


def mul_mask(a, b):
    return ImageChops.multiply(a, b)


def sub_mask(a, b):
    return ImageChops.subtract(a, b)


def add_mask(a, b):
    return ImageChops.lighter(a, b)


def dilate(m, px):
    return m.filter(ImageFilter.MaxFilter(odd(px)))


def erode(m, px):
    return m.filter(ImageFilter.MinFilter(odd(px)))


def blur(m, r):
    return m.filter(ImageFilter.GaussianBlur(r))


def fill_layer(mask, fill, alpha=1.0):
    """按 fill 规格生成一层 RGBA，并以 mask 作为 alpha。
    fill: (r,g,b) 纯色 | ('v', top, bottom) 竖直渐变 | ('r', inner, outer, (fx, fy)) 径向渐变
          | ('h', left, right) 水平渐变 | Image 直接使用。"""
    w, h = mask.size
    if isinstance(fill, Image.Image):
        layer = fill.copy().convert("RGBA")
    elif isinstance(fill, tuple) and len(fill) in (3, 4) and isinstance(fill[0], int):
        layer = Image.new("RGBA", (w, h), rgba(fill[:3]))
    else:
        kind = fill[0]
        bbox = mask.getbbox() or (0, 0, w, h)
        x0, y0, x1, y1 = bbox
        bw, bh = max(1, x1 - x0), max(1, y1 - y0)
        yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
        if kind == "v":
            t = np.clip((yy - y0) / bh, 0, 1)
            a, b = fill[1], fill[2]
        elif kind == "h":
            t = np.clip((xx - x0) / bw, 0, 1)
            a, b = fill[1], fill[2]
        else:
            fx, fy = fill[3] if len(fill) > 3 else (0.35, 0.3)
            cx, cy = x0 + fx * bw, y0 + fy * bh
            rad = max(bw, bh) * 0.95
            t = np.clip(np.hypot(xx - cx, yy - cy) / rad, 0, 1)
            a, b = fill[1], fill[2]
        arr = np.zeros((h, w, 4), np.float32)
        for i in range(3):
            arr[..., i] = a[i] + (b[i] - a[i]) * t
        arr[..., 3] = 255
        layer = Image.fromarray(arr.astype(np.uint8), "RGBA")
    al = layer.getchannel("A")
    al = ImageChops.multiply(al, mask)
    if alpha < 1.0:
        al = al.point(lambda v: int(v * alpha))
    layer.putalpha(al)
    return layer


def comp(base, layer):
    return Image.alpha_composite(base, layer)


def paint(img, mask, fill, outline=None, ow=0.0, shadow=0.0, alpha=1.0):
    """通用上色：阴影 → 描边（膨胀）→ 填充。ow 为单位坐标下的描边宽度。"""
    if shadow > 0:
        sh = blur(mask, U(0.018)).transform(mask.size, Image.AFFINE, (1, 0, 0, 0, 1, -U(0.03)))
        img = comp(img, fill_layer(sh, (10, 8, 30), shadow))
    if outline is not None and ow > 0:
        img = comp(img, fill_layer(dilate(mask, U(ow) * 2), outline, alpha))
    img = comp(img, fill_layer(mask, fill, alpha))
    return img


def gloss(img, mask, box, strength=0.55, r=0.02):
    """高光：椭圆柔光裁剪到 mask 内。"""
    hl = blur(ellipse_mask(box), U(r))
    img = comp(img, fill_layer(mul_mask(hl, mask), (255, 255, 255), strength))
    return img


def sparkle(img, cx, cy, r, alpha=1.0, color=(255, 255, 255)):
    """四角星闪光。"""
    d = r * 0.22
    pts = [(cx, cy - r), (cx + d, cy - d), (cx + r, cy), (cx + d, cy + d),
           (cx, cy + r), (cx - d, cy + d), (cx - r, cy), (cx - d, cy - d)]
    m = poly_mask(pts)
    img = comp(img, fill_layer(blur(m, r * 0.25), color, 0.6 * alpha))
    img = comp(img, fill_layer(m, color, alpha))
    return img


def line(img, pts, color, width, alpha=1.0, joint="curve"):
    m = mask_new()
    ImageDraw.Draw(m).line(pts, fill=255, width=int(width), joint=joint)
    return comp(img, fill_layer(m, color, alpha))


def down(img, size=S):
    if isinstance(size, int):
        size = (size, size)
    return img.resize(size, Image.LANCZOS)


# ---------------------------------------------------------------- 形状
def shape_points(shape, cx=0.5, cy=0.5, r=0.40):
    """返回单位坐标多边形（屏幕坐标）。"""
    P = []
    if shape == "circle":
        for i in range(24):
            a = -math.pi / 2 + i * 2 * math.pi / 24
            P.append((math.cos(a), math.sin(a)))
        k = 0.98
    elif shape == "square":
        s, c = 0.84, 0.24
        P = [(-s + c, -s), (s - c, -s), (s, -s + c), (s, s - c), (s - c, s), (-s + c, s), (-s, s - c), (-s, -s + c)]
        k = 1.0
    elif shape == "diamond":
        w, h, c = 0.88, 1.06, 0.10
        P = [(-c * w, -h + c * h), (c * w, -h + c * h), (w - c * w, -c * h), (w - c * w, c * h),
             (c * w, h - c * h), (-c * w, h - c * h), (-w + c * w, c * h), (-w + c * w, -c * h)]
        k = 1.0
    elif shape == "star":
        for i in range(10):
            a = -math.pi / 2 + i * math.pi / 5
            rr = 1.12 if i % 2 == 0 else 0.56
            P.append((rr * math.cos(a), rr * math.sin(a) + 0.06))
        k = 1.0
    elif shape == "triangle":
        base = []
        for i in range(3):
            a = -math.pi / 2 + i * 2 * math.pi / 3
            base.append((1.16 * math.cos(a), 1.16 * math.sin(a) + 0.16))
        # 截角，避免尖角过锐
        c = 0.16
        for i in range(3):
            p0, p1, p2 = base[i - 1], base[i], base[(i + 1) % 3]
            P.append((p1[0] + (p0[0] - p1[0]) * c, p1[1] + (p0[1] - p1[1]) * c))
            P.append((p1[0] + (p2[0] - p1[0]) * c, p1[1] + (p2[1] - p1[1]) * c))
        k = 1.0
    else:
        raise ValueError(shape)
    return [(cx + r * k * x, cy + r * k * y) for x, y in P]


def gem_image(rgb, shape):
    """刻面光泽宝石：外轮廓刻面按法线明暗 + 台面渐变 + 高光。"""
    img = new()
    upts = shape_points(shape)
    cx, cy = 0.5, 0.5 + (0.03 if shape == "triangle" else 0.0)
    pts = pts_px(upts)
    body = poly_mask(pts)
    # 阴影 + 深色描边
    sh = blur(dilate(body, U(0.02)), U(0.02)).transform(body.size, Image.AFFINE, (1, 0, 0, 0, 1, -U(0.035)))
    img = comp(img, fill_layer(sh, (8, 6, 24), 0.55))
    img = comp(img, fill_layer(dilate(body, U(0.028)), darken(rgb, 0.62)))
    # 刻面
    k = 0.50 if shape != "star" else 0.46
    inner_u = [(cx + (x - cx) * k - 0.02, cy + (y - cy) * k - 0.03) for x, y in upts]
    inner = pts_px(inner_u)
    fac = new()
    d = ImageDraw.Draw(fac)
    n = len(pts)
    lx, ly = LIGHT
    for i in range(n):
        p0, p1 = pts[i], pts[(i + 1) % n]
        q0, q1 = inner[i], inner[(i + 1) % n]
        ex, ey = p1[0] - p0[0], p1[1] - p0[1]
        nx, ny = ey, -ex
        ln = math.hypot(nx, ny) or 1
        nx, ny = nx / ln, ny / ln
        mx, my = (p0[0] + p1[0]) / 2 - U(cx), (p0[1] + p1[1]) / 2 - U(cy)
        if nx * mx + ny * my < 0:
            nx, ny = -nx, -ny
        kk = nx * lx + ny * ly
        col = lighten(rgb, 0.50 * kk) if kk > 0 else darken(rgb, 0.42 * -kk)
        d.polygon([p0, p1, q1, q0], fill=rgba(col))
    fac.putalpha(ImageChops.multiply(fac.getchannel("A"), body))
    img = comp(img, fac)
    # 台面
    tm = poly_mask(inner)
    img = comp(img, fill_layer(tm, ("v", lighten(rgb, 0.42), mix(rgb, lighten(rgb, 0.1), 0.5))))
    # 刻面线
    ln_img = new()
    d = ImageDraw.Draw(ln_img)
    step = 1 if n <= 10 else 3
    for i in range(0, n, step):
        d.line([pts[i], inner[i]], fill=(255, 255, 255, 60), width=int(U(0.007)))
    d.line(inner + [inner[0]], fill=(255, 255, 255, 110), width=int(U(0.008)), joint="curve")
    ln_img.putalpha(ImageChops.multiply(ln_img.getchannel("A"), body))
    img = comp(img, ln_img)
    # 高光
    img = gloss(img, body, (U(0.24), U(0.16), U(0.56), U(0.40)), 0.55, 0.03)
    img = sparkle(img, U(0.36), U(0.30), U(0.07), 0.95)
    return img


def emblem(img, shape, cx, cy, r, fill=(255, 255, 255), outline=(40, 30, 60), alpha=1.0):
    """小号形状徽记（气球 / 瓶 / 果汁机 / 飞碟上用于颜色双编码）。"""
    m = poly_mask(pts_px(shape_points(shape, cx, cy, r)))
    img = comp(img, fill_layer(dilate(m, U(0.022)), outline, 0.85 * alpha))
    img = comp(img, fill_layer(m, fill, alpha))
    return img


# ---------------------------------------------------------------- 特殊块
def line_special(vertical):
    """直线消除：发光光带 + 两端箭头。"""
    img = new()
    band = mask_new()
    d = ImageDraw.Draw(band)
    d.rounded_rectangle((U(0.06), U(0.43), U(0.94), U(0.57)), radius=U(0.07), fill=255)
    arrows = mask_new()
    d = ImageDraw.Draw(arrows)
    for sgn in (-1, 1):
        tip = 0.5 + sgn * 0.47
        base = 0.5 + sgn * 0.30
        d.polygon([(U(tip), U(0.5)), (U(base), U(0.36)), (U(base), U(0.64))], fill=255)
    m = add_mask(band, arrows)
    if vertical:
        m = m.transpose(Image.TRANSPOSE)
    img = comp(img, fill_layer(blur(dilate(m, U(0.04)), U(0.03)), (255, 230, 120), 0.9))
    img = comp(img, fill_layer(dilate(m, U(0.018)), (120, 60, 10), 0.9))
    img = comp(img, fill_layer(m, ("v", (255, 255, 255), (255, 236, 160))))
    core = mask_new()
    ImageDraw.Draw(core).rounded_rectangle((U(0.12), U(0.48), U(0.88), U(0.52)), radius=U(0.02), fill=255)
    if vertical:
        core = core.transpose(Image.TRANSPOSE)
    img = comp(img, fill_layer(core, (255, 170, 40)))
    return img


def bomb_glow():
    img = new()
    ring = sub_mask(ellipse_mask((U(0.02), U(0.02), U(0.98), U(0.98))), ellipse_mask((U(0.14), U(0.14), U(0.86), U(0.86))))
    img = comp(img, fill_layer(blur(ring, U(0.05)), (255, 150, 40), 1.0))
    img = comp(img, fill_layer(blur(ellipse_mask((U(0.1), U(0.1), U(0.9), U(0.9))), U(0.08)), (255, 200, 80), 0.45))
    return img


def bomb_mark(img=None, scale=1.0, cx=0.5, cy=0.54):
    """黑色炸弹徽记 + 引信火花。"""
    img = img or new()
    r = 0.17 * scale
    body = ellipse_mask((U(cx - r), U(cy - r), U(cx + r), U(cy + r)))
    img = paint(img, body, ("r", (110, 110, 130), (18, 18, 28), (0.3, 0.25)), outline=(255, 240, 200), ow=0.012 * scale)
    cap = rrect_mask((U(cx + r * 0.35), U(cy - r * 1.25), U(cx + r * 0.95), U(cy - r * 0.7)), U(0.02 * scale))
    img = paint(img, cap, (90, 90, 105), outline=(255, 240, 200), ow=0.008 * scale)
    img = line(img, [(U(cx + r * 0.7), U(cy - r * 1.2)), (U(cx + r * 1.0), U(cy - r * 1.7)), (U(cx + r * 1.5), U(cy - r * 1.8))],
               (240, 220, 180), U(0.018 * scale))
    img = sparkle(img, U(cx + r * 1.55), U(cy - r * 1.85), U(0.085 * scale), 1.0, (255, 220, 90))
    img = gloss(img, body, (U(cx - r * 0.7), U(cy - r * 0.8), U(cx + r * 0.1), U(cy - r * 0.1)), 0.6, 0.01)
    return img


def rainbow_orb():
    img = new()
    m = ellipse_mask((U(0.1), U(0.1), U(0.9), U(0.9)))
    yy, xx = np.mgrid[0:N, 0:N].astype(np.float32)
    ang = (np.arctan2(yy - N / 2, xx - N / 2) + np.hypot(yy - N / 2, xx - N / 2) / (N * 0.12)) % (2 * np.pi)
    hue = ang / (2 * np.pi)
    hsv = np.stack([hue * 255, np.full_like(hue, 200), np.full_like(hue, 255)], -1).astype(np.uint8)
    rainbow = Image.fromarray(hsv, "HSV").convert("RGBA")
    img = comp(img, fill_layer(blur(dilate(m, U(0.03)), U(0.03)), (255, 255, 255), 0.8))
    img = comp(img, fill_layer(dilate(m, U(0.024)), (50, 30, 80)))
    img = comp(img, fill_layer(m, rainbow))
    # 球体明暗
    img = comp(img, fill_layer(m, ("r", (255, 255, 255), (20, 10, 40), (0.35, 0.3)), 0.35))
    img = gloss(img, m, (U(0.22), U(0.16), U(0.58), U(0.42)), 0.7, 0.02)
    for (x, y, r) in [(0.33, 0.3, 0.07), (0.68, 0.64, 0.05), (0.62, 0.3, 0.035), (0.35, 0.7, 0.03)]:
        img = sparkle(img, U(x), U(y), U(r))
    return img


# ---------------------------------------------------------------- 覆盖层
def ice(n):
    img = new()
    box = (U(0.03), U(0.03), U(0.97), U(0.97))
    m = rrect_mask(box, U(0.16))
    a = {1: 0.34, 2: 0.50, 3: 0.64}[n]
    img = comp(img, fill_layer(m, ("v", (225, 248, 255), (140, 210, 245)), a))
    rim = sub_mask(m, erode(m, U(0.05 + 0.02 * n)))
    img = comp(img, fill_layer(rim, ("v", (255, 255, 255), (120, 200, 240)), 0.9))
    # 斜向光泽条
    st = mask_new()
    d = ImageDraw.Draw(st)
    d.polygon([(U(0.18), U(0.06)), (U(0.36), U(0.06)), (U(0.06), U(0.36)), (U(0.06), U(0.18))], fill=255)
    d.polygon([(U(0.46), U(0.06)), (U(0.52), U(0.06)), (U(0.06), U(0.52)), (U(0.06), U(0.46))], fill=255)
    img = comp(img, fill_layer(mul_mask(st, m), (255, 255, 255), 0.55))
    if n == 1:
        cracks = [[(0.62, 0.08), (0.56, 0.3), (0.66, 0.44), (0.58, 0.62)], [(0.56, 0.3), (0.8, 0.36), (0.94, 0.3)],
                  [(0.66, 0.44), (0.9, 0.62)], [(0.58, 0.62), (0.42, 0.78), (0.36, 0.95)]]
    elif n == 2:
        cracks = [[(0.8, 0.06), (0.74, 0.2), (0.86, 0.3)]]
    else:
        cracks = []
    for c in cracks:
        img = line(img, pts_px(c), (255, 255, 255), U(0.016), 0.95)
    return img


def grass():
    img = new()
    rnd = random.Random(7)
    turf = mask_new()
    ImageDraw.Draw(turf).rounded_rectangle((U(0.04), U(0.78), U(0.96), U(0.97)), radius=U(0.08), fill=255)
    blades = mask_new()
    d = ImageDraw.Draw(blades)
    for i in range(22):
        x = 0.06 + i * 0.041 + rnd.uniform(-0.01, 0.01)
        h = rnd.uniform(0.14, 0.28) * (1.25 if i % 5 == 0 else 1.0)
        lean = rnd.uniform(-0.05, 0.05)
        d.polygon([(U(x - 0.025), U(0.86)), (U(x + 0.025), U(0.86)), (U(x + lean), U(0.86 - h))], fill=255)
    # 左右边角小草
    for (x0, s) in [(0.05, 1), (0.95, -1)]:
        for j in range(3):
            y = 0.62 + j * 0.06
            d.polygon([(U(x0), U(y)), (U(x0), U(y + 0.06)), (U(x0 + s * 0.12), U(y - 0.05))], fill=255)
    m = add_mask(turf, blades)
    img = comp(img, fill_layer(dilate(m, U(0.02)), (18, 70, 24), 0.95))
    img = comp(img, fill_layer(m, ("v", (150, 230, 90), (40, 140, 50))))
    for (x, col) in [(0.2, (255, 255, 255)), (0.52, (255, 225, 80)), (0.8, (255, 255, 255))]:
        f = ellipse_mask((U(x - 0.035), U(0.84), U(x + 0.035), U(0.91)))
        img = paint(img, f, col, outline=(30, 90, 30), ow=0.006)
    return img


def vine():
    img = new()
    m = mask_new()
    d = ImageDraw.Draw(m)
    for phase, sgn in [(0.0, 1), (math.pi, -1)]:
        pts = []
        for i in range(41):
            t = i / 40
            x = 0.06 + 0.88 * t
            y = 0.5 + sgn * 0.36 * math.sin(t * math.pi * 1.5 + phase) * (0.9 - 0.3 * t)
            pts.append((U(x), U(y)))
        d.line(pts, fill=255, width=int(U(0.05)), joint="curve")
    img = comp(img, fill_layer(dilate(m, U(0.025)), (16, 50, 18)))
    img = comp(img, fill_layer(m, ("v", (70, 150, 60), (30, 90, 30))))
    for (x, y, a) in [(0.18, 0.2, 30), (0.42, 0.78, -40), (0.66, 0.2, 20), (0.84, 0.72, -20), (0.3, 0.52, 60), (0.72, 0.48, -60)]:
        leaf = Image.new("L", (int(U(0.22)), int(U(0.12))), 0)
        ImageDraw.Draw(leaf).ellipse((0, 0, leaf.size[0] - 1, leaf.size[1] - 1), fill=255)
        leaf = leaf.rotate(a, expand=True, resample=Image.BICUBIC)
        lm = mask_new()
        lm.paste(leaf, (int(U(x) - leaf.size[0] / 2), int(U(y) - leaf.size[1] / 2)))
        img = paint(img, lm, ("r", (170, 240, 110), (40, 130, 40), (0.3, 0.3)), outline=(16, 50, 18), ow=0.01)
    return img


def choco():
    img = new()
    m = rrect_mask((U(0.04), U(0.04), U(0.96), U(0.96)), U(0.12))
    bite = ellipse_mask((U(0.74), U(-0.08), U(1.08), U(0.26)))
    m = sub_mask(m, bite)
    img = paint(img, m, ("v", (132, 76, 44), (82, 42, 22)), outline=(45, 22, 10), ow=0.014, alpha=0.96)
    for (x0, y0) in [(0.1, 0.1), (0.52, 0.1), (0.1, 0.52), (0.52, 0.52)]:
        sq = mul_mask(rrect_mask((U(x0), U(y0), U(x0 + 0.38), U(y0 + 0.38)), U(0.05)), m)
        img = comp(img, fill_layer(sq, ("r", (160, 98, 58), (96, 52, 28), (0.3, 0.3)), 0.96))
        hi = sub_mask(sq, sq.transform(sq.size, Image.AFFINE, (1, 0, -U(0.02), 0, 1, -U(0.02))))
        img = comp(img, fill_layer(hi, (200, 140, 95), 0.8))
    return img


def cloud_mask(cx, cy, s):
    m = mask_new()
    d = ImageDraw.Draw(m)
    for (dx, dy, r) in [(-0.22, 0.06, 0.16), (-0.06, -0.08, 0.2), (0.14, -0.02, 0.17), (0.26, 0.1, 0.12), (0.0, 0.12, 0.17)]:
        d.ellipse((U(cx + (dx - r) * s), U(cy + (dy - r) * s), U(cx + (dx + r) * s), U(cy + (dy + r) * s)), fill=255)
    return m


def fog(n):
    img = new()
    base = rrect_mask((U(0.03), U(0.03), U(0.97), U(0.97)), U(0.16))
    img = comp(img, fill_layer(blur(base, U(0.02)), (215, 210, 240), 0.55 if n == 1 else 0.78))
    clouds = [(0.5, 0.52, 1.35)] if n == 1 else [(0.42, 0.38, 1.05), (0.6, 0.66, 1.15)]
    for (x, y, s) in clouds:
        cm = cloud_mask(x, y, s)
        img = paint(img, cm, ("v", (255, 255, 255), (200, 196, 232)), outline=(140, 130, 190), ow=0.012, alpha=0.95)
    return img


def chain(n):
    img = new()
    diags = [((0.04, 0.04), (0.96, 0.96))] if n == 1 else [((0.04, 0.04), (0.96, 0.96)), ((0.96, 0.04), (0.04, 0.96))]
    for (a, b) in diags:
        steps = 7
        ang = math.degrees(math.atan2(b[1] - a[1], b[0] - a[0]))
        for i in range(steps):
            t = (i + 0.5) / steps
            x, y = a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t
            w, h = (0.2, 0.11) if i % 2 == 0 else (0.2, 0.05)
            link = Image.new("L", (int(U(w)), int(U(h + 0.001))), 0)
            ld = ImageDraw.Draw(link)
            ld.rounded_rectangle((0, 0, link.size[0] - 1, link.size[1] - 1), radius=link.size[1] // 2, fill=255)
            if i % 2 == 0:
                hole = int(U(0.03))
                ld.rounded_rectangle((hole, hole, link.size[0] - 1 - hole, link.size[1] - 1 - hole), radius=max(1, link.size[1] // 2 - hole), fill=0)
            link = link.rotate(-ang, expand=True, resample=Image.BICUBIC)
            lm = mask_new()
            lm.paste(link, (int(U(x) - link.size[0] / 2), int(U(y) - link.size[1] / 2)))
            img = paint(img, lm, ("v", (225, 230, 240), (110, 118, 135)), outline=(35, 38, 50), ow=0.01)
    # 挂锁
    sh = sub_mask(ellipse_mask((U(0.38), U(0.30), U(0.62), U(0.54))), ellipse_mask((U(0.42), U(0.34), U(0.58), U(0.5))))
    img = paint(img, sh, ("v", (230, 230, 240), (130, 135, 150)), outline=(35, 38, 50), ow=0.01)
    body = rrect_mask((U(0.34), U(0.44), U(0.66), U(0.68)), U(0.04))
    img = paint(img, body, ("v", (255, 214, 90), (200, 140, 30)), outline=(90, 55, 10), ow=0.012)
    img = comp(img, fill_layer(ellipse_mask((U(0.47), U(0.5), U(0.53), U(0.56))), (70, 40, 10)))
    img = comp(img, fill_layer(rrect_mask((U(0.487), U(0.54), U(0.513), U(0.62)), U(0.01)), (70, 40, 10)))
    return img


def snowflake(img, cx, cy, r, color=(255, 255, 255), w=0.018):
    for k in range(3):
        a = k * math.pi / 3
        dx, dy = math.cos(a) * r, math.sin(a) * r
        img = line(img, [(U(cx - dx), U(cy - dy)), (U(cx + dx), U(cy + dy))], color, U(w))
        for sgn in (-1, 1):
            bx, by = cx + sgn * dx * 0.6, cy + sgn * dy * 0.6
            for t in (-1, 1):
                aa = a + t * math.pi / 4
                img = line(img, [(U(bx), U(by)), (U(bx + sgn * math.cos(aa) * r * 0.35), U(by + sgn * math.sin(aa) * r * 0.35))], color, U(w * 0.8))
    return img


def freeze(n):
    img = new()
    m = rrect_mask((U(0.03), U(0.03), U(0.97), U(0.97)), U(0.14))
    img = comp(img, fill_layer(m, ("v", (70, 120, 235), (25, 50, 160)), 0.42 if n == 1 else 0.62))
    # 晶体刻面
    fac = mask_new()
    d = ImageDraw.Draw(fac)
    for p in [[(0.03, 0.03), (0.4, 0.03), (0.03, 0.35)], [(0.97, 0.6), (0.97, 0.97), (0.6, 0.97)], [(0.6, 0.03), (0.97, 0.03), (0.97, 0.25)]]:
        d.polygon(pts_px(p), fill=255)
    img = comp(img, fill_layer(mul_mask(fac, m), (190, 225, 255), 0.5))
    rim = sub_mask(m, erode(m, U(0.05)))
    img = comp(img, fill_layer(rim, ("v", (220, 240, 255), (80, 130, 230))))
    img = snowflake(img, 0.8, 0.2, 0.13, (255, 255, 255), 0.022)
    if n >= 2:
        img = snowflake(img, 0.2, 0.8, 0.11, (230, 245, 255), 0.02)
    return img


def curtain(n):
    img = new()
    yy, xx = np.mgrid[0:N, 0:N].astype(np.float32)
    folds = (np.sin(xx / N * math.pi * 10) * 0.5 + 0.5)
    arr = np.zeros((N, N, 4), np.uint8)
    lo, hi = np.array([120, 20, 45]), np.array([225, 60, 90])
    for i in range(3):
        arr[..., i] = (lo[i] + (hi[i] - lo[i]) * folds).astype(np.uint8)
    arr[..., 3] = 255
    fabric = Image.fromarray(arr, "RGBA")
    if n >= 2:
        m = rrect_mask((U(0.04), U(0.1), U(0.96), U(0.96)), U(0.06))
    else:
        m = mask_new()
        d = ImageDraw.Draw(m)
        d.polygon(pts_px([(0.04, 0.1), (0.36, 0.1), (0.22, 0.55), (0.28, 0.96), (0.04, 0.96)]), fill=255)
        d.polygon(pts_px([(0.64, 0.1), (0.96, 0.1), (0.96, 0.96), (0.72, 0.96), (0.78, 0.55)]), fill=255)
    img = comp(img, fill_layer(dilate(m, U(0.02)), (60, 8, 20)))
    img = comp(img, fill_layer(m, fabric))
    img = comp(img, fill_layer(m, ("v", (255, 255, 255), (0, 0, 0)), 0.18))
    # 帷幔
    val = mask_new()
    d = ImageDraw.Draw(val)
    d.rectangle((U(0.04), U(0.1), U(0.96), U(0.2)), fill=255)
    for i in range(5):
        x = 0.04 + i * 0.184
        d.ellipse((U(x), U(0.14), U(x + 0.184), U(0.28)), fill=255)
    img = paint(img, val, ("v", (240, 90, 120), (160, 30, 60)), outline=(60, 8, 20), ow=0.01)
    rod = rrect_mask((U(0.02), U(0.05), U(0.98), U(0.11)), U(0.03))
    img = paint(img, rod, ("v", (255, 230, 130), (190, 130, 30)), outline=(90, 55, 10), ow=0.01)
    if n == 1:
        for x in (0.26, 0.74):
            t = ellipse_mask((U(x - 0.05), U(0.5), U(x + 0.05), U(0.6)))
            img = paint(img, t, (255, 210, 80), outline=(90, 55, 10), ow=0.008)
    return img


def steam():
    img = new()
    rnd = random.Random(3)
    for i in range(7):
        x, y, r = rnd.uniform(0.25, 0.75), rnd.uniform(0.3, 0.8), rnd.uniform(0.14, 0.22)
        img = comp(img, fill_layer(blur(ellipse_mask((U(x - r), U(y - r), U(x + r), U(y + r))), U(0.04)), (210, 215, 225), 0.55))
    for (x0, s) in [(0.3, 1), (0.52, -1), (0.72, 1)]:
        pts = []
        for i in range(30):
            t = i / 29
            pts.append((U(x0 + s * 0.06 * math.sin(t * math.pi * 2.2)), U(0.9 - 0.75 * t)))
        img = line(img, pts, (90, 100, 120), U(0.045), 0.55)
        img = line(img, pts, (250, 252, 255), U(0.028), 0.95)
    return img


# ---------------------------------------------------------------- 障碍物
def boulder_mask(seed=5):
    rnd = random.Random(seed)
    pts = []
    for i in range(11):
        a = -math.pi / 2 + i * 2 * math.pi / 11
        r = 0.40 * rnd.uniform(0.9, 1.05)
        pts.append((0.5 + r * math.cos(a), 0.53 + r * 0.92 * math.sin(a)))
    return poly_mask(pts_px(pts)), pts


def stone(n):
    img = new()
    m, _ = boulder_mask()
    m = blur(m, U(0.012)).point(lambda v: 255 if v > 127 else 0)
    img = paint(img, m, ("r", (190, 192, 206), (84, 86, 102), (0.35, 0.28)), outline=(40, 40, 52), ow=0.022, shadow=0.5)
    rnd = random.Random(11)
    for i in range(9):
        x, y, r = rnd.uniform(0.25, 0.72), rnd.uniform(0.3, 0.78), rnd.uniform(0.02, 0.045)
        img = comp(img, fill_layer(mul_mask(ellipse_mask((U(x - r), U(y - r), U(x + r), U(y + r))), m), (80, 82, 96), 0.7))
    img = gloss(img, m, (U(0.24), U(0.18), U(0.54), U(0.4)), 0.35, 0.03)
    cracks = {3: [], 2: [[(0.5, 0.16), (0.46, 0.34), (0.56, 0.46)]],
              1: [[(0.5, 0.16), (0.46, 0.34), (0.56, 0.46), (0.5, 0.64), (0.58, 0.88)], [(0.46, 0.34), (0.26, 0.42), (0.16, 0.38)],
                  [(0.56, 0.46), (0.78, 0.5), (0.86, 0.62)], [(0.5, 0.64), (0.32, 0.74)]]}[min(3, n)]
    for c in cracks:
        img = line(img, pts_px([(x + 0.008, y + 0.008) for x, y in c]), (235, 235, 245), U(0.016), 0.7)
        img = line(img, pts_px(c), (30, 30, 40), U(0.02))
    return img


def magic_stone(k):
    """魔法石（新玩法 2）：紫色八角石板 + 中央十字符文 + 底部 3 个充能槽（点亮 k 个）；满 3 格时外发光、符文变亮。"""
    img = new()
    oct_pts = [(0.3, 0.1), (0.7, 0.1), (0.9, 0.3), (0.9, 0.7), (0.7, 0.9), (0.3, 0.9), (0.1, 0.7), (0.1, 0.3)]
    m = poly_mask(pts_px(oct_pts))
    if k >= 3:
        img = comp(img, fill_layer(blur(dilate(m, U(0.05)), U(0.04)), (255, 120, 255), 0.75))
    img = paint(img, m, ("r", (150, 110, 220), (52, 30, 104), (0.4, 0.32)), outline=(26, 12, 58), ow=0.022, shadow=0.5)
    inner = erode(m, U(0.07))
    img = comp(img, fill_layer(sub_mask(inner, erode(inner, U(0.012))), (200, 170, 255), 0.55))
    rune = add_mask(rrect_mask((U(0.44), U(0.2), U(0.56), U(0.62)), U(0.03)), rrect_mask((U(0.24), U(0.35), U(0.76), U(0.47)), U(0.03)))
    img = paint(img, rune, (255, 236, 150) if k >= 3 else (170, 140, 230), outline=(60, 30, 110), ow=0.008)
    for i, cx in enumerate((0.32, 0.5, 0.68)):
        r = 0.055
        sock = ellipse_mask((U(cx - r), U(0.7 - r), U(cx + r), U(0.7 + r)))
        lit = i < k
        img = paint(img, sock, ("r", (255, 250, 200), (255, 170, 30), (0.4, 0.35)) if lit else (40, 26, 70), outline=(30, 14, 50), ow=0.008)
    img = gloss(img, m, (U(0.22), U(0.14), U(0.56), U(0.34)), 0.35, 0.03)
    if k >= 3:
        img = sparkle(img, U(0.74), U(0.22), U(0.07), 0.9)
    return img


def fuzzball():
    """毛球（新玩法 3）：灰粉色毛团——一圈小绒球叠成毛茸茸的轮廓，两只大眼睛 + 一撮呆毛；会跳，所以画得圆滚滚。"""
    img = new()
    m = ellipse_mask((U(0.2), U(0.22), U(0.8), U(0.82)))
    for i in range(14):
        a = i / 14 * math.pi * 2
        cx, cy, r = 0.5 + 0.31 * math.cos(a), 0.52 + 0.31 * math.sin(a), 0.085
        m = add_mask(m, ellipse_mask((U(cx - r), U(cy - r), U(cx + r), U(cy + r))))
    tuft = ellipse_mask((U(0.44), U(0.06), U(0.56), U(0.22)))
    m = add_mask(m, tuft)
    img = paint(img, m, ("r", (246, 214, 226), (168, 116, 140), (0.42, 0.36)), outline=(84, 48, 70), ow=0.02, shadow=0.5)
    for i in range(10):
        a = i / 10 * math.pi * 2 + 0.3
        cx, cy = 0.5 + 0.2 * math.cos(a), 0.54 + 0.2 * math.sin(a)
        img = line(img, [(U(cx - 0.03), U(cy)), (U(cx + 0.03), U(cy - 0.02))], (255, 236, 244), U(0.012))
    for ex in (0.39, 0.61):
        eye = ellipse_mask((U(ex - 0.075), U(0.4), U(ex + 0.075), U(0.58)))
        img = paint(img, eye, (255, 255, 255), outline=(60, 30, 50), ow=0.012)
        img = comp(img, fill_layer(ellipse_mask((U(ex - 0.03), U(0.46), U(ex + 0.04), U(0.56))), (30, 18, 32)))
        img = comp(img, fill_layer(ellipse_mask((U(ex - 0.01), U(0.465), U(ex + 0.02), U(0.495))), (255, 255, 255)))
    img = line(img, [(U(0.45), U(0.66)), (U(0.5), U(0.69)), (U(0.55), U(0.66))], (84, 48, 70), U(0.016))
    img = gloss(img, m, (U(0.24), U(0.2), U(0.56), U(0.36)), 0.3, 0.03)
    return img


def chameleon():
    """变色龙（新玩法 7）：叠在当前颜色宝石之上的五色描边环（按 C1..C5 顺时针五段，段间留白缝）+ 右下角一小截卷尾。
    游戏里先画 gem_<颜色> 再画它，并让环缓慢旋转（提示「会换色」）。"""
    img = new()
    ring = sub_mask(ellipse_mask((U(0.03), U(0.03), U(0.97), U(0.97))), ellipse_mask((U(0.11), U(0.11), U(0.89), U(0.89))))
    img = comp(img, fill_layer(dilate(ring, U(0.012) * 2), (40, 30, 60), 0.85))
    for i, k in enumerate(GEMS):
        a0, a1 = -90 + i * 72 + 5, -90 + (i + 1) * 72 - 5
        wedge = mask_new()
        ImageDraw.Draw(wedge).pieslice((U(-0.1), U(-0.1), U(1.1), U(1.1)), a0, a1, fill=255)
        img = comp(img, fill_layer(mul_mask(ring, wedge), lighten(GEMS[k]["rgb"], 0.15)))
    tail = mask_new()
    ImageDraw.Draw(tail).arc((U(0.74), U(0.74), U(0.94), U(0.94)), 180, 450, fill=255, width=int(U(0.045)))
    img = comp(img, fill_layer(dilate(tail, U(0.01) * 2), (40, 30, 60), 0.85))
    img = comp(img, fill_layer(tail, (120, 220, 120)))
    img = sparkle(img, U(0.2), U(0.16), U(0.07))
    return img


def snow_boss(hurt=False):
    """雪怪 Boss（新玩法 5）：整只画在一张图上（占 2×2 格），游戏里切成四块 snow_boss_<象限>（hurt = 血量过半后的受伤表情）。
    冰蓝雪怪：毛茸茸的白色身体 + 两只冰角 + 浅蓝脸 + 怒眉獠牙，两侧短胳膊；受伤版一只眼打叉、头上贴创可贴、身上裂纹、汗滴。"""
    img = new()
    # 身体：大椭圆 + 一圈绒球
    m = ellipse_mask((U(0.1), U(0.16), U(0.9), U(0.97)))
    for i in range(22):
        a = i / 22 * math.pi * 2
        cx, cy, r = 0.5 + 0.39 * math.cos(a), 0.57 + 0.39 * math.sin(a), 0.07
        m = add_mask(m, ellipse_mask((U(cx - r), U(cy - r), U(cx + r), U(cy + r))))
    # 胳膊
    for sx in (-1, 1):
        cx = 0.5 + sx * 0.4
        arm = ellipse_mask((U(cx - 0.1), U(0.56), U(cx + 0.1), U(0.84)))
        img = paint(img, arm, ("r", (250, 252, 255), (150, 180, 220), (0.45, 0.35)), outline=(40, 60, 110), ow=0.012, shadow=0.4)
    # 冰角
    for sx in (-1, 1):
        pts = [(0.5 + sx * 0.18, 0.2), (0.5 + sx * 0.3, 0.02), (0.5 + sx * 0.33, 0.1), (0.5 + sx * 0.28, 0.24)]
        horn = poly_mask(pts_px(pts))
        img = paint(img, horn, ("v", (235, 250, 255), (120, 190, 240)), outline=(30, 70, 130), ow=0.01)
    img = paint(img, m, ("r", (255, 255, 255), (170, 196, 232), (0.42, 0.36)), outline=(34, 54, 104), ow=0.014, shadow=0.55)
    for i in range(16):
        a = i / 16 * math.pi * 2 + 0.2
        cx, cy = 0.5 + 0.3 * math.cos(a), 0.6 + 0.3 * math.sin(a)
        img = line(img, [(U(cx - 0.02), U(cy)), (U(cx + 0.02), U(cy - 0.015))], (205, 222, 245), U(0.008))
    # 脸
    face = ellipse_mask((U(0.27), U(0.32), U(0.73), U(0.74)))
    img = paint(img, face, ("r", (214, 236, 255), (120, 170, 226), (0.5, 0.4)), outline=(40, 70, 130), ow=0.008)
    # 眼睛
    for sx in (-1, 1):
        ex = 0.5 + sx * 0.1
        if hurt and sx < 0:
            img = line(img, [(U(ex - 0.045), U(0.42)), (U(ex + 0.045), U(0.5))], (30, 30, 60), U(0.016))
            img = line(img, [(U(ex - 0.045), U(0.5)), (U(ex + 0.045), U(0.42))], (30, 30, 60), U(0.016))
        else:
            eye = ellipse_mask((U(ex - 0.055), U(0.4), U(ex + 0.055), U(0.52)))
            img = paint(img, eye, (255, 255, 255), outline=(30, 40, 80), ow=0.006)
            img = comp(img, fill_layer(ellipse_mask((U(ex - 0.025), U(0.44), U(ex + 0.025), U(0.51))), (200, 30, 40) if not hurt else (30, 30, 60)))
            img = comp(img, fill_layer(ellipse_mask((U(ex - 0.008), U(0.45), U(ex + 0.012), U(0.47))), (255, 255, 255)))
        # 眉毛：怒 = 内低外高；受伤 = 内高外低（皱眉）
        inner, outer = (0.37, 0.33) if not hurt else (0.33, 0.37)
        img = line(img, [(U(0.5 + sx * 0.035), U(inner)), (U(0.5 + sx * 0.16), U(outer))], (26, 40, 90), U(0.02))
    # 嘴 + 獠牙
    mouth = ellipse_mask((U(0.38), U(0.56), U(0.62), U(0.68))) if not hurt else ellipse_mask((U(0.41), U(0.58), U(0.59), U(0.66)))
    img = paint(img, mouth, (60, 20, 50), outline=(30, 20, 50), ow=0.006)
    for fx in (0.44, 0.56):
        fang = poly_mask(pts_px([(fx - 0.025, 0.565), (fx + 0.025, 0.565), (fx, 0.62)]))
        img = paint(img, fang, (255, 255, 255), outline=(60, 60, 90), ow=0.004)
    img = gloss(img, m, (U(0.2), U(0.2), U(0.55), U(0.4)), 0.3, 0.03)
    if hurt:
        # 创可贴
        band = poly_mask(pts_px([(0.56, 0.2), (0.7, 0.14), (0.73, 0.2), (0.59, 0.26)]))
        img = paint(img, band, (250, 210, 170), outline=(150, 100, 70), ow=0.005)
        for d in (0.0, 0.03):
            img = comp(img, fill_layer(ellipse_mask((U(0.62 + d), U(0.19 - d / 3), U(0.635 + d), U(0.205 - d / 3))), (190, 140, 110)))
        # 裂纹
        for c in ([(0.2, 0.5), (0.25, 0.58), (0.21, 0.66), (0.27, 0.72)], [(0.8, 0.62), (0.74, 0.7), (0.79, 0.8)]):
            img = line(img, pts_px(c), (60, 90, 150), U(0.01))
        # 汗滴
        drop = add_mask(ellipse_mask((U(0.74), U(0.36), U(0.8), U(0.43))), poly_mask(pts_px([(0.745, 0.385), (0.77, 0.32), (0.795, 0.385)])))
        img = paint(img, drop, ("v", (220, 245, 255), (110, 190, 250)), outline=(40, 90, 160), ow=0.004)
    else:
        img = sparkle(img, U(0.82), U(0.2), U(0.06), 0.9)
    return img


def chest():
    img = new()
    body = rrect_mask((U(0.12), U(0.42), U(0.88), U(0.86)), U(0.05))
    img = paint(img, body, ("v", (196, 124, 62), (120, 66, 30)), outline=(64, 34, 14), ow=0.02, shadow=0.5)
    for y in (0.56, 0.7):
        img = line(img, [(U(0.14), U(y)), (U(0.86), U(y))], (90, 48, 20), U(0.012), 0.8)
    lid = rrect_mask((U(0.1), U(0.16), U(0.9), U(0.44)), U(0.13))
    img = paint(img, lid, ("v", (220, 150, 80), (150, 86, 40)), outline=(64, 34, 14), ow=0.02)
    gold = ("v", (255, 228, 120), (206, 146, 36))
    for x in (0.22, 0.7):
        band = rrect_mask((U(x), U(0.17), U(x + 0.08), U(0.86)), U(0.015))
        img = paint(img, mul_mask(band, add_mask(body, lid)), gold, outline=(110, 70, 10), ow=0.006)
    trim = rrect_mask((U(0.1), U(0.4), U(0.9), U(0.46)), U(0.02))
    img = paint(img, trim, gold, outline=(110, 70, 10), ow=0.008)
    lock = rrect_mask((U(0.42), U(0.38), U(0.58), U(0.58)), U(0.03))
    img = paint(img, lock, gold, outline=(90, 55, 10), ow=0.012)
    img = comp(img, fill_layer(ellipse_mask((U(0.475), U(0.43), U(0.525), U(0.48))), (60, 30, 10)))
    img = comp(img, fill_layer(rrect_mask((U(0.49), U(0.46), U(0.51), U(0.53)), U(0.005)), (60, 30, 10)))
    img = gloss(img, lid, (U(0.16), U(0.17), U(0.6), U(0.28)), 0.35, 0.02)
    img = sparkle(img, U(0.8), U(0.22), U(0.06))
    return img


def honey():
    img = new()
    body = rrect_mask((U(0.16), U(0.3), U(0.84), U(0.9)), U(0.16))
    img = paint(img, body, ("r", (255, 214, 92), (206, 118, 14), (0.35, 0.35)), outline=(110, 58, 6), ow=0.02, shadow=0.5)
    hexm = poly_mask(pts_px([(0.5 + 0.13 * math.cos(math.pi / 6 + k * math.pi / 3), 0.64 + 0.13 * math.sin(math.pi / 6 + k * math.pi / 3)) for k in range(6)]))
    img = paint(img, hexm, ("v", (255, 248, 220), (240, 214, 150)), outline=(150, 90, 20), ow=0.01)
    hexs = poly_mask(pts_px([(0.5 + 0.06 * math.cos(math.pi / 6 + k * math.pi / 3), 0.64 + 0.06 * math.sin(math.pi / 6 + k * math.pi / 3)) for k in range(6)]))
    img = comp(img, fill_layer(hexs, (240, 170, 30)))
    lid = rrect_mask((U(0.18), U(0.16), U(0.82), U(0.34)), U(0.05))
    img = paint(img, lid, ("v", (250, 230, 190), (200, 170, 120)), outline=(110, 70, 30), ow=0.014)
    for x in range(4):
        for y in range(2):
            if (x + y) % 2 == 0:
                sq = mul_mask(rrect_mask((U(0.18 + x * 0.16), U(0.16 + y * 0.09), U(0.34 + x * 0.16), U(0.25 + y * 0.09)), 1), lid)
                img = comp(img, fill_layer(sq, (220, 70, 60), 0.85))
    string = rrect_mask((U(0.16), U(0.3), U(0.84), U(0.345)), U(0.02))
    img = paint(img, string, (170, 110, 50), outline=(90, 50, 20), ow=0.006)
    drip = mask_new()
    d = ImageDraw.Draw(drip)
    d.rectangle((U(0.3), U(0.34), U(0.4), U(0.46)), fill=255)
    d.ellipse((U(0.3), U(0.42), U(0.4), U(0.52)), fill=255)
    d.rectangle((U(0.62), U(0.34), U(0.7), U(0.41)), fill=255)
    d.ellipse((U(0.62), U(0.37), U(0.7), U(0.45)), fill=255)
    img = paint(img, drip, ("v", (255, 200, 60), (220, 130, 10)), outline=(110, 58, 6), ow=0.008)
    img = gloss(img, body, (U(0.2), U(0.36), U(0.34), U(0.8)), 0.55, 0.012)
    return img


def balloon(key):
    g = GEMS[key]
    rgb = g["rgb"]
    img = new()
    body = ellipse_mask((U(0.2), U(0.06), U(0.8), U(0.72)))
    knot = poly_mask(pts_px([(0.5, 0.7), (0.44, 0.78), (0.56, 0.78)]))
    pts = [(U(0.5 + 0.05 * math.sin(t / 10 * math.pi * 2)), U(0.78 + 0.18 * t / 10)) for t in range(11)]
    img = line(img, pts, (70, 60, 80), U(0.02))
    img = paint(img, add_mask(body, knot), ("r", lighten(rgb, 0.45), darken(rgb, 0.35), (0.35, 0.28)), outline=darken(rgb, 0.65), ow=0.02, shadow=0.45)
    img = emblem(img, g["shape"], 0.5, 0.42, 0.13, (255, 255, 255), darken(rgb, 0.6), 0.95)
    img = gloss(img, body, (U(0.28), U(0.12), U(0.46), U(0.3)), 0.75, 0.012)
    return img


def cookie():
    img = new()
    rnd = random.Random(4)
    pts = []
    for i in range(28):
        a = i * 2 * math.pi / 28
        r = 0.38 + rnd.uniform(-0.012, 0.012)
        pts.append((0.5 + r * math.cos(a), 0.52 + r * math.sin(a)))
    m = poly_mask(pts_px(pts))
    img = paint(img, m, ("r", (246, 200, 128), (190, 126, 58), (0.4, 0.35)), outline=(110, 62, 24), ow=0.02, shadow=0.5)
    rim = sub_mask(m, erode(m, U(0.05)))
    img = comp(img, fill_layer(rim, (170, 104, 44), 0.45))
    for (x, y, r) in [(0.36, 0.36, 0.055), (0.6, 0.32, 0.045), (0.66, 0.56, 0.06), (0.42, 0.62, 0.05), (0.52, 0.47, 0.04), (0.3, 0.52, 0.035), (0.54, 0.74, 0.04)]:
        cp = poly_mask(pts_px([(x + r * math.cos(k * 1.1) * rnd.uniform(0.8, 1.1), y + r * math.sin(k * 1.1) * rnd.uniform(0.8, 1.1)) for k in range(6)]))
        img = paint(img, cp, ("r", (110, 64, 40), (54, 28, 16), (0.3, 0.3)))
    img = gloss(img, m, (U(0.24), U(0.2), U(0.5), U(0.36)), 0.3, 0.03)
    return img


def cake(n):
    img = new()
    n = max(1, min(3, n))
    plate = ellipse_mask((U(0.06), U(0.82), U(0.94), U(0.96)))
    img = paint(img, plate, ("v", (250, 250, 255), (190, 196, 214)), outline=(110, 110, 140), ow=0.012, shadow=0.4)
    tiers = {1: [(0.14, 0.86, 0.46, 0.88)], 2: [(0.12, 0.88, 0.6, 0.88), (0.24, 0.76, 0.36, 0.6)],
             3: [(0.1, 0.9, 0.68, 0.88), (0.2, 0.8, 0.48, 0.68), (0.3, 0.7, 0.3, 0.48)]}[n]
    for (x0, x1, y0, y1) in tiers:
        side = rrect_mask((U(x0), U(y0), U(x1), U(y1)), U(0.04))
        img = paint(img, side, ("v", (252, 222, 170), (214, 160, 104)), outline=(120, 70, 50), ow=0.014)
        icing = mask_new()
        d = ImageDraw.Draw(icing)
        d.rounded_rectangle((U(x0), U(y0), U(x1), U(y0 + 0.07)), radius=U(0.035), fill=255)
        k = max(3, int((x1 - x0) / 0.1))
        for i in range(k):
            xx = x0 + 0.02 + (x1 - x0 - 0.04) * (i + 0.5) / k
            dl = 0.05 + 0.04 * ((i * 7) % 3) / 2
            d.rounded_rectangle((U(xx - 0.025), U(y0 + 0.03), U(xx + 0.025), U(y0 + 0.05 + dl)), radius=U(0.025), fill=255)
        img = paint(img, icing, ("v", (255, 190, 220), (240, 104, 158)), outline=(150, 40, 90), ow=0.01)
    top = tiers[-1][2]
    cherry = ellipse_mask((U(0.44), U(top - 0.13), U(0.56), U(top - 0.01)))
    img = line(img, [(U(0.5), U(top - 0.1)), (U(0.56), U(top - 0.2))], (60, 120, 40), U(0.015))
    img = paint(img, cherry, ("r", (255, 120, 120), (180, 10, 40), (0.35, 0.3)), outline=(90, 0, 20), ow=0.01)
    img = gloss(img, cherry, (U(0.46), U(top - 0.12), U(0.5), U(top - 0.08)), 0.8, 0.005)
    return img


def magic_hat():
    img = new()
    brim = ellipse_mask((U(0.08), U(0.68), U(0.92), U(0.9)))
    img = paint(img, brim, ("v", (120, 70, 190), (60, 26, 110)), outline=(30, 10, 60), ow=0.02, shadow=0.5)
    cone = mask_new()
    pts = [(0.24, 0.8), (0.4, 0.4), (0.52, 0.16), (0.64, 0.06), (0.62, 0.2), (0.62, 0.45), (0.76, 0.8)]
    ImageDraw.Draw(cone).polygon(pts_px(pts), fill=255)
    cone = blur(cone, U(0.006)).point(lambda v: 255 if v > 127 else 0)
    img = paint(img, cone, ("h", (170, 110, 240), (80, 36, 150)), outline=(30, 10, 60), ow=0.02)
    band = mul_mask(rrect_mask((U(0.2), U(0.64), U(0.8), U(0.74)), U(0.02)), dilate(cone, U(0.01)))
    img = paint(img, band, ("v", (255, 228, 110), (210, 150, 30)), outline=(100, 60, 10), ow=0.008)
    for (x, y, r) in [(0.46, 0.44, 0.06), (0.58, 0.28, 0.045), (0.42, 0.58, 0.035)]:
        img = sparkle(img, U(x), U(y), U(r), 1.0, (255, 236, 120))
    img = sparkle(img, U(0.66), U(0.07), U(0.07), 1.0, (255, 250, 200))
    return img


def maker(key):
    g = GEMS[key]
    rgb = g["rgb"]
    img = new()
    body = rrect_mask((U(0.14), U(0.44), U(0.86), U(0.92)), U(0.06))
    img = paint(img, body, ("h", (200, 208, 224), (100, 108, 128)), outline=(40, 44, 60), ow=0.02, shadow=0.5)
    tank = rrect_mask((U(0.24), U(0.08), U(0.76), U(0.5)), U(0.08))
    img = paint(img, tank, ("v", (235, 245, 255), (190, 210, 230)), outline=(40, 44, 60), ow=0.018, alpha=0.95)
    liquid = mul_mask(tank, rrect_mask((U(0.2), U(0.22), U(0.8), U(0.52)), 0))
    img = comp(img, fill_layer(erode(liquid, U(0.02)), ("v", lighten(rgb, 0.25), darken(rgb, 0.2))))
    for (x, y, r) in [(0.36, 0.32, 0.025), (0.56, 0.4, 0.02), (0.62, 0.3, 0.015)]:
        img = comp(img, fill_layer(ellipse_mask((U(x - r), U(y - r), U(x + r), U(y + r))), (255, 255, 255), 0.7))
    img = gloss(img, tank, (U(0.28), U(0.1), U(0.38), U(0.46)), 0.6, 0.008)
    panel_m = rrect_mask((U(0.28), U(0.56), U(0.72), U(0.84)), U(0.04))
    img = paint(img, panel_m, ("v", (60, 64, 84), (30, 32, 44)), outline=(20, 20, 30), ow=0.008)
    img = emblem(img, g["shape"], 0.5, 0.7, 0.1, rgb, (255, 255, 255))
    return img


def snail():
    img = new()
    bodym = mask_new()
    d = ImageDraw.Draw(bodym)
    d.rounded_rectangle((U(0.08), U(0.66), U(0.84), U(0.86)), radius=U(0.1), fill=255)
    d.ellipse((U(0.66), U(0.46), U(0.92), U(0.76)), fill=255)
    img = paint(img, bodym, ("v", (190, 236, 120), (96, 170, 60)), outline=(34, 70, 20), ow=0.018, shadow=0.5)
    for (x0, x1) in [(0.74, 0.7), (0.84, 0.88)]:
        img = line(img, [(U(x0), U(0.52)), (U(x1), U(0.28))], (34, 70, 20), U(0.036))
        img = line(img, [(U(x0), U(0.52)), (U(x1), U(0.28))], (140, 200, 90), U(0.018))
        e = ellipse_mask((U(x1 - 0.05), U(0.22), U(x1 + 0.05), U(0.32)))
        img = paint(img, e, (255, 255, 255), outline=(34, 70, 20), ow=0.01)
        img = comp(img, fill_layer(ellipse_mask((U(x1 - 0.015), U(0.255), U(x1 + 0.03), U(0.3))), (20, 20, 30)))
    img = line(img, [(U(0.76), U(0.66)), (U(0.8), U(0.68)), (U(0.85), U(0.66))], (34, 70, 20), U(0.014))
    shell = ellipse_mask((U(0.14), U(0.24), U(0.66), U(0.76)))
    img = paint(img, shell, ("r", (255, 196, 110), (184, 90, 30), (0.35, 0.3)), outline=(90, 40, 10), ow=0.02)
    pts = []
    for i in range(80):
        t = i / 79
        a = t * math.pi * 4.2
        r = 0.22 * (1 - t) + 0.02
        pts.append((U(0.41 + r * math.cos(a)), U(0.5 + r * math.sin(a))))
    img = line(img, pts, (120, 54, 16), U(0.024))
    img = gloss(img, shell, (U(0.2), U(0.28), U(0.42), U(0.44)), 0.4, 0.02)
    return img


def safe():
    img = new()
    m = rrect_mask((U(0.1), U(0.1), U(0.9), U(0.9)), U(0.08))
    img = paint(img, m, ("v", (168, 178, 196), (78, 86, 104)), outline=(30, 34, 46), ow=0.022, shadow=0.5)
    door = rrect_mask((U(0.18), U(0.18), U(0.82), U(0.82)), U(0.05))
    img = paint(img, door, ("r", (150, 160, 180), (96, 104, 124), (0.3, 0.3)), outline=(50, 56, 70), ow=0.012)
    for (x, y) in [(0.14, 0.14), (0.86, 0.14), (0.14, 0.86), (0.86, 0.86)]:
        b = ellipse_mask((U(x - 0.025), U(y - 0.025), U(x + 0.025), U(y + 0.025)))
        img = paint(img, b, (220, 226, 236), outline=(40, 44, 56), ow=0.006)
    dial = ellipse_mask((U(0.32), U(0.32), U(0.68), U(0.68)))
    img = paint(img, dial, ("r", (255, 230, 130), (190, 130, 30), (0.35, 0.3)), outline=(90, 55, 10), ow=0.014)
    for k in range(12):
        a = k * math.pi / 6
        img = line(img, [(U(0.5 + 0.14 * math.cos(a)), U(0.5 + 0.14 * math.sin(a))), (U(0.5 + 0.17 * math.cos(a)), U(0.5 + 0.17 * math.sin(a)))], (110, 70, 10), U(0.01))
    for k in range(3):
        a = -math.pi / 2 + k * 2 * math.pi / 3
        img = line(img, [(U(0.5), U(0.5)), (U(0.5 + 0.1 * math.cos(a)), U(0.5 + 0.1 * math.sin(a)))], (70, 44, 10), U(0.022))
    img = comp(img, fill_layer(ellipse_mask((U(0.46), U(0.46), U(0.54), U(0.54))), (70, 44, 10)))
    img = gloss(img, m, (U(0.14), U(0.12), U(0.6), U(0.3)), 0.3, 0.02)
    return img


def surprise():
    img = new()
    box = rrect_mask((U(0.16), U(0.42), U(0.84), U(0.9)), U(0.05))
    img = paint(img, box, ("v", (255, 120, 180), (206, 40, 120)), outline=(90, 10, 50), ow=0.02, shadow=0.5)
    lid = rrect_mask((U(0.12), U(0.32), U(0.88), U(0.46)), U(0.04))
    img = paint(img, lid, ("v", (255, 150, 200), (220, 70, 140)), outline=(90, 10, 50), ow=0.018)
    gold = ("v", (255, 236, 130), (220, 150, 30))
    rib = mul_mask(rrect_mask((U(0.45), U(0.32), U(0.55), U(0.9)), 0), add_mask(box, lid))
    img = paint(img, rib, gold, outline=(120, 70, 10), ow=0.006)
    for s in (-1, 1):
        loop = Image.new("L", (int(U(0.26)), int(U(0.16))), 0)
        ImageDraw.Draw(loop).ellipse((0, 0, loop.size[0] - 1, loop.size[1] - 1), fill=255)
        loop = loop.rotate(s * 25, expand=True, resample=Image.BICUBIC)
        lm = mask_new()
        lm.paste(loop, (int(U(0.5 + s * 0.13) - loop.size[0] / 2), int(U(0.24) - loop.size[1] / 2)))
        img = paint(img, lm, gold, outline=(120, 70, 10), ow=0.012)
    img = paint(img, ellipse_mask((U(0.44), U(0.24), U(0.56), U(0.36))), gold, outline=(120, 70, 10), ow=0.01)
    img = text_on(img, "?", 0.31, 0.66, 0.26, (255, 255, 255), (120, 20, 70))
    img = text_on(img, "?", 0.69, 0.66, 0.26, (255, 255, 255), (120, 20, 70))
    img = sparkle(img, U(0.86), U(0.2), U(0.06))
    return img


def bottle(key):
    g = GEMS[key]
    rgb = g["rgb"]
    img = new()
    flask = mask_new()
    d = ImageDraw.Draw(flask)
    d.ellipse((U(0.2), U(0.36), U(0.8), U(0.94)), fill=255)
    d.rounded_rectangle((U(0.4), U(0.16), U(0.6), U(0.44)), radius=U(0.03), fill=255)
    img = paint(img, flask, (230, 240, 255), outline=(40, 40, 70), ow=0.02, shadow=0.5, alpha=0.9)
    liq = mul_mask(erode(flask, U(0.04)), rrect_mask((0, U(0.52), N, N), 0))
    img = comp(img, fill_layer(liq, ("v", lighten(rgb, 0.2), darken(rgb, 0.3))))
    img = emblem(img, g["shape"], 0.5, 0.72, 0.11, (255, 255, 255), darken(rgb, 0.6))
    cork = rrect_mask((U(0.38), U(0.08), U(0.62), U(0.2)), U(0.03))
    img = paint(img, cork, ("v", (220, 160, 100), (150, 96, 50)), outline=(70, 40, 20), ow=0.012)
    img = gloss(img, flask, (U(0.26), U(0.42), U(0.42), U(0.62)), 0.7, 0.012)
    return img


def time_spirit():
    img = new()
    img = comp(img, fill_layer(blur(ellipse_mask((U(0.08), U(0.1), U(0.92), U(0.94))), U(0.06)), (90, 230, 255), 0.7))
    body = mask_new()
    d = ImageDraw.Draw(body)
    d.ellipse((U(0.2), U(0.18), U(0.8), U(0.78)), fill=255)
    d.polygon(pts_px([(0.24, 0.6), (0.5, 0.92), (0.44, 0.74), (0.62, 0.9), (0.76, 0.6)]), fill=255)
    img = paint(img, body, ("r", (230, 255, 255), (30, 160, 220), (0.4, 0.3)), outline=(10, 70, 110), ow=0.018)
    for x in (0.4, 0.6):
        e = ellipse_mask((U(x - 0.04), U(0.4), U(x + 0.04), U(0.52)))
        img = comp(img, fill_layer(e, (20, 40, 70)))
        img = comp(img, fill_layer(ellipse_mask((U(x - 0.015), U(0.415), U(x + 0.012), U(0.445))), (255, 255, 255)))
    for x in (0.32, 0.68):
        img = comp(img, fill_layer(blur(ellipse_mask((U(x - 0.05), U(0.53), U(x + 0.05), U(0.58))), U(0.01)), (255, 130, 170), 0.7))
    img = line(img, [(U(0.46), U(0.58)), (U(0.5), U(0.61)), (U(0.54), U(0.58))], (20, 40, 70), U(0.014))
    ring = sub_mask(ellipse_mask((U(0.3), U(0.06), U(0.7), U(0.2))), ellipse_mask((U(0.35), U(0.09), U(0.65), U(0.17))))
    img = paint(img, ring, (255, 220, 90), outline=(130, 90, 10), ow=0.008)
    tag = rrect_mask((U(0.6), U(0.68), U(0.96), U(0.9)), U(0.08))
    img = paint(img, tag, ("v", (255, 236, 120), (230, 160, 30)), outline=(120, 70, 10), ow=0.012)
    img = text_on(img, "+2", 0.78, 0.79, 0.17, (255, 255, 255), (130, 70, 0))
    img = gloss(img, body, (U(0.28), U(0.22), U(0.5), U(0.36)), 0.6, 0.015)
    return img


def countdown(n):
    """倒计时炸弹覆盖层（叠在对应颜色宝石上）：黑盘 + 橙环 + 数字。"""
    img = new()
    ringm = ellipse_mask((U(0.26), U(0.28), U(0.74), U(0.76)))
    img = comp(img, fill_layer(blur(dilate(ringm, U(0.05)), U(0.03)), (255, 120, 20), 0.85))
    img = paint(img, ringm, ("r", (80, 80, 96), (10, 10, 18), (0.35, 0.3)), outline=(255, 170, 40), ow=0.024)
    img = line(img, [(U(0.64), U(0.32)), (U(0.74), U(0.2)), (U(0.84), U(0.18))], (240, 220, 180), U(0.02))
    img = sparkle(img, U(0.86), U(0.17), U(0.08), 1.0, (255, 220, 90))
    img = text_on(img, str(n), 0.5, 0.52, 0.3, (255, 255, 255), (0, 0, 0))
    return img


def flip_mark():
    """双面块标记：环形双箭头（左下角）。"""
    img = new()
    cx, cy, r = 0.2, 0.8, 0.12
    disc = ellipse_mask((U(cx - 0.19), U(cy - 0.19), U(cx + 0.19), U(cy + 0.19)))
    img = paint(img, disc, ("v", (255, 255, 255), (214, 220, 238)), outline=(40, 40, 70), ow=0.016)
    for a0 in (0.3, math.pi + 0.3):
        pts = [(U(cx + r * math.cos(a0 + t / 20 * 2.2)), U(cy + r * math.sin(a0 + t / 20 * 2.2))) for t in range(21)]
        img = line(img, pts, (60, 60, 110), U(0.028))
        ae = a0 + 2.2
        tx, ty = cx + r * math.cos(ae), cy + r * math.sin(ae)
        dx, dy = -math.sin(ae), math.cos(ae)
        ox, oy = math.cos(ae), math.sin(ae)
        tri = [(tx + dx * 0.06, ty + dy * 0.06), (tx + ox * 0.05, ty + oy * 0.05), (tx - ox * 0.05, ty - oy * 0.05)]
        img = comp(img, fill_layer(poly_mask(pts_px(tri)), (60, 60, 110)))
    return img


# ---------------------------------------------------------------- 地面 / 标记
def tile(light):
    img = new()
    m = rrect_mask((U(0.035), U(0.035), U(0.965), U(0.965)), U(0.14))
    top, bot = ((74, 70, 132), (58, 54, 112)) if light else ((60, 56, 114), (46, 42, 94))
    img = comp(img, fill_layer(m, ("v", top, bot), 0.92))
    rim = sub_mask(m, m.transform(m.size, Image.AFFINE, (1, 0, 0, 0, 1, -U(0.02))))
    img = comp(img, fill_layer(rim, (255, 255, 255), 0.10))
    inner = sub_mask(m, erode(m, U(0.02)))
    img = comp(img, fill_layer(inner, (20, 16, 50), 0.35))
    return img


def carpet(covered):
    img = new()
    m = rrect_mask((U(0.03), U(0.03), U(0.97), U(0.97)), U(0.1))
    if covered:
        img = comp(img, fill_layer(m, ("v", (206, 70, 120), (150, 40, 90))))
        pat = mask_new()
        d = ImageDraw.Draw(pat)
        for (x, y) in [(0.5, 0.5), (0.18, 0.18), (0.82, 0.18), (0.18, 0.82), (0.82, 0.82)]:
            s = 0.16 if (x, y) == (0.5, 0.5) else 0.09
            d.polygon(pts_px([(x, y - s), (x + s, y), (x, y + s), (x - s, y)]), fill=255)
        img = comp(img, fill_layer(mul_mask(pat, m), (255, 210, 110), 0.9))
        inner = sub_mask(erode(m, U(0.06)), erode(m, U(0.09)))
        img = comp(img, fill_layer(inner, (255, 220, 140), 0.8))
        rim = sub_mask(m, erode(m, U(0.03)))
        img = comp(img, fill_layer(rim, (90, 20, 50)))
    else:
        img = comp(img, fill_layer(m, (160, 60, 130), 0.28))
        dash = mask_new()
        d = ImageDraw.Draw(dash)
        for i in range(8):
            t0 = 0.08 + i * 0.11
            for seg in [((t0, 0.06), (t0 + 0.06, 0.06)), ((t0, 0.94), (t0 + 0.06, 0.94)), ((0.06, t0), (0.06, t0 + 0.06)), ((0.94, t0), (0.94, t0 + 0.06))]:
                d.line(pts_px(list(seg)), fill=255, width=int(U(0.03)))
        img = comp(img, fill_layer(dash, (255, 150, 220), 0.95))
        img = comp(img, fill_layer(poly_mask(pts_px([(0.5, 0.36), (0.64, 0.5), (0.5, 0.64), (0.36, 0.5)])), (255, 150, 220), 0.35))
    return img


def cookie_drop():
    """饼干掉落口（新玩法 6）：顶行格子上沿的金色漏斗 + 白色向下箭头，画在棋子之上（只占格子上部四分之一）。"""
    img = new()
    fun = poly_mask(pts_px([(0.06, 0.02), (0.94, 0.02), (0.74, 0.24), (0.26, 0.24)]))
    img = comp(img, fill_layer(blur(fun, U(0.03)), (40, 20, 0), 0.45))
    img = comp(img, fill_layer(fun, ("v", (255, 222, 130), (190, 120, 45)), 0.97))
    rim = poly_mask(pts_px([(0.06, 0.02), (0.94, 0.02), (0.91, 0.06), (0.09, 0.06)]))
    img = comp(img, fill_layer(rim, (110, 62, 18), 0.95))
    lip = poly_mask(pts_px([(0.27, 0.20), (0.73, 0.20), (0.74, 0.24), (0.26, 0.24)]))
    img = comp(img, fill_layer(lip, (92, 50, 14), 0.95))
    for x0 in (0.16, 0.8):
        img = comp(img, fill_layer(ellipse_mask((U(x0 - 0.022), U(0.058), U(x0 + 0.022), U(0.102))), (255, 246, 210), 0.9))
    arr = poly_mask(pts_px([(0.44, 0.07), (0.56, 0.07), (0.56, 0.12), (0.63, 0.12), (0.5, 0.21), (0.37, 0.12), (0.44, 0.12)]))
    img = comp(img, fill_layer(arr, (255, 252, 236), 0.97))
    return img


def jelly(n):
    """双层果冻（段 5，地面层）：半透明粉色果冻块，画在棋子下面。n=2 为双层（更厚、带一道层线），n=1 为单层。"""
    img = new()
    m = rrect_mask((U(0.05), U(0.05), U(0.95), U(0.95)), U(0.16))
    if n >= 2:
        img = comp(img, fill_layer(m, ("v", (255, 120, 190), (200, 50, 130)), 0.78))
        inner = rrect_mask((U(0.16), U(0.16), U(0.84), U(0.84)), U(0.12))
        seam = sub_mask(inner, erode(inner, U(0.025)))
        img = comp(img, fill_layer(seam, (255, 225, 240), 0.85))
        rim = sub_mask(m, erode(m, U(0.035)))
        img = comp(img, fill_layer(rim, (140, 20, 80), 0.9))
    else:
        img = comp(img, fill_layer(m, ("v", (255, 170, 215), (235, 110, 175)), 0.5))
        rim = sub_mask(m, erode(m, U(0.03)))
        img = comp(img, fill_layer(rim, (200, 60, 130), 0.75))
    img = gloss(img, m, (U(0.1), U(0.06), U(0.6), U(0.3)), 0.35, 0.03)
    return img


def magic_ground():
    """魔法地格（新玩法 8，地面层 "magic"）：紫色符文地砖，画在棋子下面——径向紫光底 + 发光描边 +
    四角菱形符点 + 中央淡淡的八角星（棋子挡住中间，露在外面的是边框与四角）。不随消除变化（永久）。"""
    img = new()
    m = rrect_mask((U(0.04), U(0.04), U(0.96), U(0.96)), U(0.14))
    img = comp(img, fill_layer(m, ("r", (190, 120, 255), (70, 30, 150), (0.5, 0.5)), 0.62))
    glow = sub_mask(blur(m, U(0.03)), erode(m, U(0.09)))
    img = comp(img, fill_layer(glow, (215, 170, 255), 0.7))
    rim = sub_mask(m, erode(m, U(0.035)))
    img = comp(img, fill_layer(rim, (245, 215, 255), 0.95))
    # 八角星（两个方块叠成），只作底纹
    sq1 = poly_mask(pts_px([(0.5, 0.2), (0.8, 0.5), (0.5, 0.8), (0.2, 0.5)]))
    sq2 = rrect_mask((U(0.29), U(0.29), U(0.71), U(0.71)), U(0.01))
    star8 = add_mask(sq1, sq2)
    img = comp(img, fill_layer(sub_mask(star8, erode(star8, U(0.03))), (240, 200, 255), 0.55))
    # 四角菱形符点
    for cx, cy in ((0.15, 0.15), (0.85, 0.15), (0.15, 0.85), (0.85, 0.85)):
        d = poly_mask(pts_px([(cx, cy - 0.06), (cx + 0.06, cy), (cx, cy + 0.06), (cx - 0.06, cy)]))
        img = comp(img, fill_layer(blur(dilate(d, U(0.02)), U(0.012)), (200, 140, 255), 0.8))
        img = comp(img, fill_layer(d, (255, 245, 255), 0.95))
    return img


def bubble():
    """气泡（段 5）：透明水泡，蓝青色边缘 + 虹彩 + 高光；无颜色徽记（不参与匹配）。"""
    img = new()
    disc = ellipse_mask((U(0.1), U(0.1), U(0.9), U(0.9)))
    img = comp(img, fill_layer(disc, ("r", (200, 240, 255), (90, 170, 235), (0.4, 0.35)), 0.38))
    ring = sub_mask(disc, erode(disc, U(0.06)))
    img = comp(img, fill_layer(blur(ring, U(0.01)), ("h", (150, 230, 255), (230, 170, 255)), 0.95))
    rim = sub_mask(disc, erode(disc, U(0.018)))
    img = comp(img, fill_layer(rim, (40, 110, 180), 0.9))
    img = gloss(img, disc, (U(0.24), U(0.18), U(0.52), U(0.4)), 0.8, 0.012)
    img = sparkle(img, U(0.68), U(0.68), U(0.07), 0.8)
    return img


def belt():
    """传送带：整格履带 + 上下两条轨道（露在宝石外圈），箭头指向运动方向（朝右，运行时旋转）。"""
    img = new()
    m = rrect_mask((U(0.01), U(0.01), U(0.99), U(0.99)), U(0.1))
    img = comp(img, fill_layer(m, ("v", (64, 78, 92), (34, 42, 54)), 0.95))
    for x in np.linspace(0.08, 0.92, 8):
        img = line(img, [(U(x), U(0.14)), (U(x), U(0.86))], (24, 30, 38), U(0.012), 0.8)
    for x0 in (0.26, 0.56):
        ch = poly_mask(pts_px([(x0, 0.3), (x0 + 0.12, 0.3), (x0 + 0.26, 0.5), (x0 + 0.12, 0.7), (x0, 0.7), (x0 + 0.14, 0.5)]))
        img = comp(img, fill_layer(ch, (60, 230, 200), 0.9))
    for (y0, y1) in ((0.01, 0.13), (0.87, 0.99)):
        rail = mul_mask(rrect_mask((U(0.01), U(y0), U(0.99), U(y1)), U(0.04)), m)
        img = comp(img, fill_layer(rail, ("v", (40, 150, 140), (20, 90, 90))))
        yc = (y0 + y1) / 2
        for x0 in (0.12, 0.42, 0.72):
            ch = poly_mask(pts_px([(x0, yc - 0.045), (x0 + 0.06, yc - 0.045), (x0 + 0.13, yc), (x0 + 0.06, yc + 0.045), (x0, yc + 0.045), (x0 + 0.07, yc)]))
            img = comp(img, fill_layer(ch, (190, 255, 240)))
    return img


def portal():
    img = new()
    yy, xx = np.mgrid[0:N, 0:N].astype(np.float32)
    ang = np.arctan2(yy - N / 2, xx - N / 2)
    rr = np.hypot(yy - N / 2, xx - N / 2) / (N / 2)
    sw = (np.sin(ang * 3 + rr * 9) * 0.5 + 0.5)
    arr = np.zeros((N, N, 4), np.uint8)
    arr[..., 0] = (120 + 110 * sw).astype(np.uint8)
    arr[..., 1] = (50 + 80 * sw).astype(np.uint8)
    arr[..., 2] = (200 + 55 * sw).astype(np.uint8)
    arr[..., 3] = 255
    swirl = Image.fromarray(arr, "RGBA")
    disc = ellipse_mask((U(0.04), U(0.04), U(0.96), U(0.96)))
    img = comp(img, fill_layer(blur(disc, U(0.03)), swirl, 0.75))
    ring = sub_mask(disc, ellipse_mask((U(0.12), U(0.12), U(0.88), U(0.88))))
    img = comp(img, fill_layer(ring, ("v", (240, 200, 255), (140, 60, 220))))
    return img


def ufo(key):
    g = GEMS[key]
    rgb = g["rgb"]
    img = new()
    beam = poly_mask(pts_px([(0.36, 0.56), (0.64, 0.56), (0.84, 0.98), (0.16, 0.98)]))
    img = comp(img, fill_layer(blur(beam, U(0.02)), ("v", lighten(rgb, 0.4), rgb), 0.45))
    dome = ellipse_mask((U(0.3), U(0.12), U(0.7), U(0.5)))
    img = paint(img, dome, ("r", (230, 250, 255), (120, 180, 220), (0.35, 0.3)), outline=(30, 40, 70), ow=0.016, alpha=0.95)
    img = emblem(img, g["shape"], 0.5, 0.33, 0.085, rgb, (30, 30, 50))
    disc = ellipse_mask((U(0.06), U(0.38), U(0.94), U(0.64)))
    img = paint(img, disc, ("v", (224, 228, 240), (110, 116, 140)), outline=(30, 34, 56), ow=0.018, shadow=0.4)
    band = mul_mask(disc, rrect_mask((0, U(0.49), N, U(0.56)), 0))
    img = comp(img, fill_layer(band, darken(rgb, 0.2)))
    for x in (0.2, 0.35, 0.5, 0.65, 0.8):
        lm = ellipse_mask((U(x - 0.03), U(0.495), U(x + 0.03), U(0.555)))
        img = comp(img, fill_layer(blur(dilate(lm, U(0.02)), U(0.01)), lighten(rgb, 0.5), 0.8))
        img = comp(img, fill_layer(lm, (255, 255, 230)))
    img = gloss(img, dome, (U(0.36), U(0.15), U(0.5), U(0.26)), 0.8, 0.01)
    return img


# ---------------------------------------------------------------- UI
def sel_ring():
    img = new()
    m = rrect_mask((U(0.02), U(0.02), U(0.98), U(0.98)), U(0.16))
    ring = sub_mask(m, erode(m, U(0.07)))
    img = comp(img, fill_layer(blur(ring, U(0.025)), (255, 255, 255), 0.9))
    img = comp(img, fill_layer(sub_mask(m, erode(m, U(0.035))), (255, 255, 255)))
    for (x, y) in [(0.1, 0.1), (0.9, 0.9)]:
        img = sparkle(img, U(x), U(y), U(0.09))
    return img


def hint_glow():
    img = new()
    m = rrect_mask((U(0.03), U(0.03), U(0.97), U(0.97)), U(0.18))
    ring = sub_mask(m, erode(m, U(0.12)))
    img = comp(img, fill_layer(blur(ring, U(0.04)), (255, 220, 90), 1.0))
    img = comp(img, fill_layer(blur(m, U(0.06)), (255, 230, 140), 0.25))
    return img


def spark(size=64):
    img = new()
    img = comp(img, fill_layer(blur(ellipse_mask((U(0.2), U(0.2), U(0.8), U(0.8))), U(0.1)), (255, 255, 255)))
    img = comp(img, fill_layer(ellipse_mask((U(0.38), U(0.38), U(0.62), U(0.62))), (255, 255, 255)))
    return down(img, size)


def star(on, size=96):     # 结算面板按 48 逻辑像素绘制
    img = new()
    m = poly_mask(pts_px(shape_points("star", 0.5, 0.47, 0.39)))
    if on:
        img = comp(img, fill_layer(blur(dilate(m, U(0.05)), U(0.04)), (255, 210, 80), 0.8))
        img = paint(img, m, ("r", (255, 250, 190), (240, 150, 20), (0.4, 0.3)), outline=(120, 60, 0), ow=0.03)
        img = gloss(img, m, (U(0.3), U(0.2), U(0.6), U(0.42)), 0.5, 0.02)
    else:
        img = paint(img, m, ("v", (90, 90, 120), (54, 54, 80)), outline=(24, 24, 40), ow=0.03)
    return down(img, size)


def medal(size=96):
    img = new()
    m = ellipse_mask((U(0.04), U(0.04), U(0.96), U(0.96)))
    img = paint(img, m, ("v", (255, 230, 120), (200, 130, 20)), outline=(90, 50, 0), ow=0.02)
    inner = ellipse_mask((U(0.14), U(0.14), U(0.86), U(0.86)))
    img = paint(img, inner, ("v", (90, 110, 230), (40, 50, 150)), outline=(40, 30, 10), ow=0.01)
    img = gloss(img, m, (U(0.18), U(0.08), U(0.8), U(0.4)), 0.25, 0.03)
    return down(img, size)


def map_node(kind, size=80):
    img = new()
    m = ellipse_mask((U(0.06), U(0.06), U(0.94), U(0.94)))
    if kind == "cur":
        img = comp(img, fill_layer(blur(dilate(m, U(0.05)), U(0.04)), (255, 210, 80), 0.9))
        fill, ol = ("r", (255, 244, 170), (230, 150, 20), (0.35, 0.3)), (110, 60, 0)
    elif kind == "done":
        fill, ol = ("r", (150, 240, 170), (30, 150, 80), (0.35, 0.3)), (10, 60, 30)
    else:
        fill, ol = ("r", (110, 110, 140), (56, 56, 80), (0.35, 0.3)), (24, 24, 40)
    img = paint(img, m, fill, outline=ol, ow=0.03, shadow=0.4)
    img = gloss(img, m, (U(0.2), U(0.12), U(0.7), U(0.4)), 0.4, 0.02)
    return down(img, size)


def panel(kind, size=144):
    """九宫格面板（角 = size/4）。144 → 角 36px，覆盖逻辑角半径 18 的 2x；
    另生成 @80 小变体（角 20px）给 7..10 的小角半径，避免大比例缩小时描边发虚 / 锯齿。"""
    img = new()
    m = rrect_mask((U(0.03), U(0.03), U(0.97), U(0.97)), U(0.22))
    if kind == "dark":
        img = comp(img, fill_layer(m, ("v", (48, 42, 96), (30, 26, 66)), 0.94))
        rim = sub_mask(m, erode(m, U(0.04)))
        img = comp(img, fill_layer(rim, ("v", (150, 140, 230), (70, 60, 140))))
    elif kind == "gold":
        img = comp(img, fill_layer(m, ("v", (70, 46, 110), (40, 24, 70)), 0.97))
        rim = sub_mask(m, erode(m, U(0.06)))
        img = comp(img, fill_layer(rim, ("v", (255, 236, 140), (210, 140, 30))))
    elif kind == "chip":
        img = comp(img, fill_layer(m, ("v", (36, 32, 74), (24, 20, 52)), 0.95))
        rim = sub_mask(m, erode(m, U(0.05)))
        img = comp(img, fill_layer(rim, (110, 100, 190)))
    elif kind == "bar":
        img = comp(img, fill_layer(m, ("v", (18, 16, 40), (30, 26, 60)), 0.95))
        rim = sub_mask(m, erode(m, U(0.05)))
        img = comp(img, fill_layer(rim, (100, 92, 170)))
    elif kind == "fill":
        img = comp(img, fill_layer(m, ("v", (255, 255, 255), (170, 170, 170))))
        img = comp(img, fill_layer(mul_mask(m, rrect_mask((0, 0, N, U(0.45)), 0)), (255, 255, 255), 0.35))
    return down(img, size)


def icon(kind, size=64):
    img = new()
    if kind == "hammer":
        handle = Image.new("L", (int(U(0.12)), int(U(0.62))), 0)
        ImageDraw.Draw(handle).rounded_rectangle((0, 0, handle.size[0] - 1, handle.size[1] - 1), radius=int(U(0.04)), fill=255)
        hm = mask_new()
        hm.paste(handle, (int(U(0.44)), int(U(0.3))))
        hm = hm.rotate(35, center=(U(0.5), U(0.5)), resample=Image.BICUBIC)
        img = paint(img, hm, ("h", (230, 170, 100), (150, 90, 40)), outline=(60, 30, 10), ow=0.03)
        head = Image.new("L", (int(U(0.6)), int(U(0.26))), 0)
        ImageDraw.Draw(head).rounded_rectangle((0, 0, head.size[0] - 1, head.size[1] - 1), radius=int(U(0.05)), fill=255)
        km = mask_new()
        km.paste(head, (int(U(0.2)), int(U(0.12))))
        km = km.rotate(35, center=(U(0.5), U(0.5)), resample=Image.BICUBIC)
        img = paint(img, km, ("v", (230, 236, 248), (120, 128, 150)), outline=(30, 34, 50), ow=0.03)
    elif kind == "swap":
        for (y, s, col) in [(0.34, 1, (120, 200, 255)), (0.66, -1, (255, 190, 90))]:
            x0, x1 = (0.14, 0.86) if s == 1 else (0.86, 0.14)
            m = mask_new()
            d = ImageDraw.Draw(m)
            d.line([(U(x0), U(y)), (U(x1 - s * 0.16), U(y))], fill=255, width=int(U(0.12)))
            d.polygon(pts_px([(x1, y), (x1 - s * 0.24, y - 0.16), (x1 - s * 0.24, y + 0.16)]), fill=255)
            img = paint(img, m, col, outline=(20, 30, 60), ow=0.03)
    elif kind == "cross":
        m = mask_new()
        d = ImageDraw.Draw(m)
        d.rounded_rectangle((U(0.08), U(0.41), U(0.92), U(0.59)), radius=U(0.08), fill=255)
        d.rounded_rectangle((U(0.41), U(0.08), U(0.59), U(0.92)), radius=U(0.08), fill=255)
        img = comp(img, fill_layer(blur(dilate(m, U(0.05)), U(0.04)), (255, 120, 255), 0.8))
        img = paint(img, m, ("v", (255, 220, 255), (220, 90, 230)), outline=(70, 10, 80), ow=0.03)
    elif kind == "moves":
        m = mask_new()
        d = ImageDraw.Draw(m)
        for (x, y) in [(0.34, 0.5), (0.66, 0.34)]:
            d.ellipse((U(x - 0.13), U(y - 0.2), U(x + 0.13), U(y + 0.14)), fill=255)
            d.ellipse((U(x - 0.1), U(y + 0.17), U(x + 0.1), U(y + 0.33)), fill=255)
        img = paint(img, m, ("v", (160, 220, 255), (70, 140, 240)), outline=(20, 40, 90), ow=0.03)
    elif kind == "score":
        cup = mask_new()
        d = ImageDraw.Draw(cup)
        d.pieslice((U(0.2), U(-0.1), U(0.8), U(0.64)), 0, 180, fill=255)
        d.rectangle((U(0.2), U(0.1), U(0.8), U(0.27)), fill=255)
        d.rectangle((U(0.44), U(0.6), U(0.56), U(0.76)), fill=255)
        d.rounded_rectangle((U(0.28), U(0.74), U(0.72), U(0.9)), radius=U(0.03), fill=255)
        hands = sub_mask(ellipse_mask((U(0.06), U(0.14), U(0.94), U(0.5))), ellipse_mask((U(0.14), U(0.2), U(0.86), U(0.44))))
        hands = mul_mask(hands, rrect_mask((0, U(0.14), N, U(0.5)), 0))
        img = paint(img, add_mask(cup, hands), ("v", (255, 240, 150), (220, 140, 20)), outline=(100, 50, 0), ow=0.03)
        img = sparkle(img, U(0.4), U(0.3), U(0.08))
    elif kind == "multi":
        for (key, x, y) in [("c1", 0.3, 0.34), ("c3", 0.7, 0.34), ("c2", 0.5, 0.7)]:
            gi = gem_image(GEMS[key]["rgb"], GEMS[key]["shape"]).resize((int(U(0.56)), int(U(0.56))), Image.LANCZOS)
            img.alpha_composite(gi, (int(U(x - 0.28)), int(U(y - 0.28))))
    return down(img, size)


def badge(n, size=46):     # 角标按 23 逻辑像素绘制 → 2x 正好 46
    img = new()
    m = ellipse_mask((U(0.06), U(0.06), U(0.94), U(0.94)))
    img = paint(img, m, ("v", (70, 60, 140), (30, 24, 80)), outline=(255, 220, 100), ow=0.06)
    img = text_on(img, str(n), 0.5, 0.5, 0.58, (255, 255, 255), (0, 0, 0))
    return down(img, size)


# ---------------------------------------------------------------- 字体与文字
FONT_LATIN = [
    "/usr/share/fonts/truetype/sand-box/google/Barlow Condensed/BarlowCondensed-Bold.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansCondensed-Bold.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
    "/System/Library/Fonts/Supplemental/Arial Narrow Bold.ttf",
    "/System/Library/Fonts/Supplemental/Arial Bold.ttf",
    "/Library/Fonts/Arial Bold.ttf",
]
FONT_CJK = [
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Bold.ttc", 2),
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Black.ttc", 2),
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc", 2),
    ("/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc", 0),
    ("/System/Library/Fonts/PingFang.ttc", 0),
    ("/System/Library/Fonts/STHeiti Medium.ttc", 0),
]


def font_latin(px):
    for p in FONT_LATIN:
        if os.path.exists(p):
            return ImageFont.truetype(p, int(px))
    return ImageFont.load_default(int(px))


def font_cjk(px):
    for p, idx in FONT_CJK:
        if os.path.exists(p):
            try:
                return ImageFont.truetype(p, int(px), index=idx)
            except OSError:
                return ImageFont.truetype(p, int(px))
    print("warning: no CJK font found, Chinese labels will be blank", file=sys.stderr)
    return font_latin(px)


def text_on(img, s, cx, cy, h, fill, outline):
    """在工作画布上居中写字（h 为单位字高，按大写字母高度对齐）。"""
    f = font_latin(U(h) * 1.4)
    layer = new()
    d = ImageDraw.Draw(layer)
    sw = max(1, int(U(h) * 0.12))
    bb = d.textbbox((0, 0), s, font=f, stroke_width=sw)
    ref = d.textbbox((0, 0), "8", font=f, stroke_width=sw)
    w = bb[2] - bb[0]
    x = U(cx) - w / 2 - bb[0]
    y = U(cy) - (ref[3] - ref[1]) / 2 - ref[1]
    d.text((x, y), s, font=f, fill=rgba(fill), stroke_width=sw, stroke_fill=rgba(outline))
    return comp(img, layer)


def _probe():
    return ImageDraw.Draw(Image.new("RGBA", (8, 8)))


def glyph(ch, px=3, ts=TS):
    """HUD 字形：白色字 + 深色描边，运行时用 colorMod 着色。
    游戏内每字占 4px × 6px 逻辑像素（px = 3/4/5），这里直接按物理像素 (4px·ts) × (6px·ts)
    用 FreeType 渲染（带 hinting），不做超采样缩小，保证 Retina 下笔画锐利。
    过宽的字（如 W、M）只做水平压缩，竖直方向保持 1:1，横笔依旧清晰。"""
    W, H = 4 * px * ts, 6 * px * ts
    probe = _probe()
    size = H * 0.98
    sw = max(1, round(H * 0.07))
    f = font_latin(size)
    while True:
        ref = probe.textbbox((0, 0), "H", font=f, stroke_width=sw)
        if ref[3] - ref[1] <= H * 0.92 or size < 6:
            break
        size *= 0.97
        f = font_latin(size)
    bb = probe.textbbox((0, 0), ch, font=f, stroke_width=sw)
    gw = bb[2] - bb[0]
    cw = max(W, gw + 2)
    img = Image.new("RGBA", (cw, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    x = (cw - gw) // 2 - bb[0]
    y = round((H - (ref[3] - ref[1])) / 2 - ref[1])
    d.text((x, y), ch, font=f, fill=(255, 255, 255, 255), stroke_width=sw, stroke_fill=(16, 12, 36, 255))
    if cw != W:
        img = img.resize((W, H), Image.LANCZOS)
    return img


def zh_label(s, h=20, color=(255, 255, 255), outline=(16, 12, 36), ts=TS):
    """中文标签：h 为游戏内逻辑高度，贴图高 h·ts，按目标字号直接渲染（FreeType hinting）。
    宽度补齐到 ts 的整数倍，保证逻辑宽度为整数、Retina 下贴图像素与物理像素一一对应。"""
    H = h * ts
    f = font_cjk(round(H * 0.8))
    sw = max(1, round(H * 0.07))
    bb = _probe().textbbox((0, 0), s, font=f, stroke_width=sw)
    W = bb[2] - bb[0] + 2 * ts
    W = -(-W // ts) * ts
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    x = (W - (bb[2] - bb[0])) // 2 - bb[0]
    y = round((H - (bb[3] - bb[1])) / 2 - bb[1])
    d.text((x, y), s, font=f, fill=rgba(color), stroke_width=sw, stroke_fill=rgba(outline))
    return img


GLYPH_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ!+-/?:x"
GLYPH_PX = (3, 4, 5)   # textA 用到的字号；3 为基名 g_<码点>，其余为 g_<码点>@<像素高>
# 连击弹字「x2」「x5」、得分浮字「+120」、HUD 总结「4 连击！」要放大弹出（最高约 75 逻辑像素高），
# 数字 / x / + 额外烘焙大号变体（同样 2x），运行时按目标高度自动挑选，放大时也不糊。
BIG_GLYPH_CHARS = "0123456789x+"
BIG_GLYPH_PX = (7, 9, 12)

# 中文标签在游戏内的逻辑高度（与 app/Main.hs 的 zhA / zhAC 调用一致）；第一个是基名，其余生成 @变体。
# 未列出的默认 20。改了 Main.hs 里的高度，记得同步这里，否则会被非整数倍缩放（略糊但仍可用）。
ZH_SIZES = {
    "daily": [22], "combo": [18, 24, 34, 44, 64], "combo_end": [20, 28], "shuffle": [18], "score": [18], "help_more": [16],
    "pause": [32], "clear": [38], "win": [38], "lose": [38], "next": [22], "retry": [26],
    "map": [32], "map_hint": [18],
    "ch1": [14], "ch2": [14], "ch3": [14], "ch4": [14], "ch5": [14], "ch6": [14], "ch7": [14],
    "rule_bomb": [18], "rule_rainbow": [18],
    "sfx": [18], "bgm": [18], "mute": [18],
}
NAME_SIZES = [24]      # 关卡名 name_<i>

# 中文 UI 标签（键 → 文本）
ZH = {
    "moves": "步数", "goal": "目标", "score": "得分", "pause": "暂停", "map": "选关地图",
    "clear": "过关！", "win": "胜利！", "lose": "失败", "next": "下一关", "retry": "按 R 重试",
    "combo": "连击", "combo_end": "连击！", "tip": "按 H 查看提示", "daily": "每日挑战", "shuffle": "已洗牌",
    "tool_hammer": "锤子：点一格", "tool_swap": "交换：点两格", "tool_cross": "十字：点一格",
    "k_hint": "提示", "k_hammer": "锤子", "k_swap": "自由交换", "k_cross": "十字消", "k_undo": "撤销",
    "k_shuffle": "洗牌", "k_daily": "每日挑战", "k_map": "选关地图", "k_retry": "重开本关", "k_next": "下一关",
    "k_play": "继续游戏", "k_quit": "退出", "legend": "图例", "map_hint": "点击关卡进入 · M 关闭",
    "help_more": "P：暂停并查看全部按键",
    "ch1": "第一章", "ch2": "第二章", "ch3": "第三章", "ch4": "第四章", "ch5": "第五章", "ch6": "第六章", "ch7": "第七章",
    # 关卡规则开关的 HUD 角标（关名右侧）
    "rule_bomb": "L/T 形出炸弹", "rule_rainbow": "彩虹组合变身",
    "sfx": "效", "bgm": "乐", "mute": "静",
}


def level_names():
    # 第 6a 刀起关卡表在 src/Match3/Levels/Campaign.hs，写法是 `level <0 基下标> "名字" ...`
    src = (ROOT / "src" / "Match3" / "Levels" / "Campaign.hs").read_text(encoding="utf-8")
    names = re.findall(r'\blevel\s+(\d+)\s+"([^"]+)"', src)
    return [(int(i), n) for i, n in names]


# ---------------------------------------------------------------- 背景
def background(w=WIN_W, h=WIN_H, k=1):
    """窗口背景；k 为像素倍率（游戏用 k=2 生成 960x1176，图案几何按 k 等比放大，观感与 1x 相同）。"""
    w, h = w * k, h * k
    rnd = random.Random(21)
    yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
    t = yy / h
    top, bot = np.array([44, 30, 92]), np.array([18, 14, 44])
    arr = np.zeros((h, w, 3), np.float32)
    for i in range(3):
        arr[..., i] = top[i] + (bot[i] - top[i]) * t
    g = np.exp(-(((xx - w * 0.5) / (w * 0.6)) ** 2 + ((yy - h * 0.55) / (h * 0.5)) ** 2))
    arr += g[..., None] * np.array([30, 20, 50])
    img = Image.fromarray(np.clip(arr, 0, 255).astype(np.uint8), "RGB").convert("RGBA")
    pat = Image.new("L", (w, h), 0)
    d = ImageDraw.Draw(pat)
    step, half, dia = 40 * k, 20 * k, 8 * k
    for y in range(-step, h + step, step):
        for x in range(-step, w + step, step):
            ox = half if (y // step) % 2 else 0
            d.polygon([(x + ox, y - dia), (x + ox + dia, y), (x + ox, y + dia), (x + ox - dia, y)], fill=255)
    img = Image.alpha_composite(img, fill_layer(pat, (255, 255, 255), 0.035))
    bok = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    area = w * h / (WIN_W * WIN_H * k * k)
    for i in range(int(26 * area)):
        x, y, r = rnd.uniform(0, w), rnd.uniform(0, h), rnd.uniform(6, 30) * k
        col = rnd.choice([(255, 120, 200), (120, 180, 255), (255, 220, 120), (180, 120, 255)])
        m = Image.new("L", (w, h), 0)
        ImageDraw.Draw(m).ellipse((x - r, y - r, x + r, y + r), fill=255)
        bok = Image.alpha_composite(bok, fill_layer(m.filter(ImageFilter.GaussianBlur(r * 0.4)), col, rnd.uniform(0.05, 0.14)))
    img = Image.alpha_composite(img, bok)
    stars = Image.new("L", (w, h), 0)
    d = ImageDraw.Draw(stars)
    for i in range(int(40 * area)):
        x, y = rnd.uniform(0, w), rnd.uniform(0, h)
        r = rnd.uniform(0.6, 1.6) * k
        d.ellipse((x - r, y - r, x + r, y + r), fill=int(rnd.uniform(80, 220)))
    img = Image.alpha_composite(img, fill_layer(stars, (255, 255, 255)))
    return img


# ---------------------------------------------------------------- 打包与输出
def bleed(img):
    """把全透明像素的 RGB 填成邻近颜色，避免线性缩放时出现黑边。"""
    a = np.asarray(img).astype(np.float32)
    rgb, al = a[..., :3], a[..., 3:4] / 255.0
    pm = Image.fromarray(np.clip(np.concatenate([rgb * al, al * 255], -1), 0, 255).astype(np.uint8), "RGBA")
    out = a.copy()
    for radius in (10, 3):
        src = np.asarray(pm.filter(ImageFilter.GaussianBlur(radius))).astype(np.float32)
        w = src[..., 3:4] / 255.0
        col = src[..., :3] / np.maximum(w, 1e-3)
        sel = (a[..., 3] == 0) & (w[..., 0] > 1e-3)
        out[..., :3][sel] = col[sel]
    return Image.fromarray(np.clip(out, 0, 255).astype(np.uint8), "RGBA")


def pack(sprites, width=PAGE_W, max_h=PAGE_H):
    """货架式打包，精灵之间留 2px 间隙；一页放不下就开新页。
    返回 (pages, rects)，rects[name] = (x, y, w, h, page)。"""
    items = sorted(sprites.items(), key=lambda kv: (-kv[1].size[1], kv[0]))
    gap = 2
    rects = {}
    heights = []
    page = x = y = shelf = 0
    for name, im in items:
        w, h = im.size
        if x + w > width:
            x = 0
            y += shelf + gap
            shelf = 0
        if y + h > max_h:
            heights.append(y - gap)
            page += 1
            x = y = shelf = 0
        rects[name] = (x, y, w, h, page)
        x += w + gap
        shelf = max(shelf, h)
    heights.append(y + shelf)
    pages = [Image.new("RGBA", (width, ph), (0, 0, 0, 0)) for ph in heights]
    for name, im in sprites.items():
        rx, ry, _, _, pg = rects[name]
        pages[pg].paste(im, (rx, ry))
    return pages, rects


def page_file(i):
    return "atlas.bmp" if i == 0 else "atlas%d.bmp" % i


def save_bmp32(img, path):
    """写 32 位 BGRA BMP（BITMAPV4HEADER，BI_BITFIELDS + alpha 掩码），SDL_LoadBMP 可保留透明度。"""
    img = img.convert("RGBA")
    w, h = img.size
    a = np.asarray(img)
    bgra = a[::-1, :, [2, 1, 0, 3]].tobytes()
    hdr_size = 108
    off = 14 + hdr_size
    fh = struct.pack("<2sIHHI", b"BM", off + len(bgra), 0, 0, off)
    ih = struct.pack("<IiiHHIIiiII", hdr_size, w, h, 1, 32, 3, len(bgra), 2835, 2835, 0, 0)
    ih += struct.pack("<IIII", 0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000)
    ih += struct.pack("<I", 0x73524742)  # 'sRGB'
    ih += b"\0" * 36 + b"\0" * 12
    assert len(ih) == hdr_size
    with open(path, "wb") as fp:
        fp.write(fh + ih + bgra)


def build_sprites():
    sp = {}
    for k, g in GEMS.items():
        sp["gem_" + k] = down(gem_image(g["rgb"], g["shape"]))
        sp["balloon_" + k] = down(balloon(k))
        sp["maker_" + k] = down(maker(k))
        sp["bottle_" + k] = down(bottle(k))
        sp["ufo_" + k] = down(ufo(k))
    sp["line_h"] = down(line_special(False))
    sp["line_v"] = down(line_special(True))
    sp["bomb_glow"] = down(bomb_glow())
    sp["bomb_mark"] = down(bomb_mark())
    sp["rainbow"] = down(rainbow_orb())
    for n in (1, 2, 3):
        sp["ice_%d" % n] = down(ice(n))
        sp["stone_%d" % n] = down(stone(n))
        sp["cake_%d" % n] = down(cake(n))
    for n in (1, 2):
        sp["fog_%d" % n] = down(fog(n))
        sp["chain_%d" % n] = down(chain(n))
        sp["freeze_%d" % n] = down(freeze(n))
        sp["curtain_%d" % n] = down(curtain(n))
    for n in range(1, 10):
        sp["countdown_%d" % n] = down(countdown(n))
        sp["badge_%d" % n] = badge(n)
    sp["grass"] = down(grass())
    sp["vine"] = down(vine())
    sp["choco"] = down(choco())
    sp["steam"] = down(steam())
    sp["chest"] = down(chest())
    sp["honey"] = down(honey())
    sp["cookie"] = down(cookie())
    sp["magic_hat"] = down(magic_hat())
    sp["snail"] = down(snail())
    sp["safe"] = down(safe())
    sp["surprise"] = down(surprise())
    sp["time_spirit"] = down(time_spirit())
    sp["flip_mark"] = down(flip_mark())
    sp["tile_a"] = down(tile(True))
    sp["tile_b"] = down(tile(False))
    sp["carpet_open"] = down(carpet(False))
    sp["carpet_covered"] = down(carpet(True))
    sp["jelly"] = down(jelly(1))
    sp["jelly_2"] = down(jelly(2))
    sp["magic"] = down(magic_ground())
    sp["bubble"] = down(bubble())
    sp["cookie_drop"] = down(cookie_drop())
    for k in range(4):
        sp["magic_stone_%d" % k] = down(magic_stone(k))
    sp["fuzzball"] = down(fuzzball())
    sp["chameleon"] = down(chameleon())
    # 变色龙目标图标（HUD / 地图节点）：黄宝石 + 五色环合成一张
    cham_icon = sp["gem_c4"].copy()
    cham_icon.alpha_composite(sp["chameleon"])
    sp["chameleon_icon"] = cham_icon
    # 雪怪 Boss：整只一张（HUD 目标 / 血条图标）+ 切成 2×2 四块（象限 0 左上 / 1 右上 / 2 左下 / 3 右下），正常与受伤两套
    for tag, hurt in (("", False), ("_hurt", True)):
        whole = snow_boss(hurt)
        if not hurt:
            sp["snow_boss"] = down(whole)
        big = down(whole, 2 * S)
        for q, (ox, oy) in enumerate([(0, 0), (S, 0), (0, S), (S, S)]):
            sp["snow_boss%s_%d" % (tag, q)] = big.crop((ox, oy, ox + S, oy + S))
    sp["belt"] = down(belt())
    sp["portal"] = down(portal())
    sp["sel_ring"] = down(sel_ring())
    sp["hint_glow"] = down(hint_glow())
    sp["spark"] = spark()
    sp["star_on"] = star(True)
    sp["star_off"] = star(False)
    sp["medal"] = medal()
    for k in ("cur", "done", "lock"):
        sp["node_" + k] = map_node(k)
    for k in ("dark", "gold", "chip", "bar", "fill"):
        sp["panel_" + k] = panel(k)
        sp["panel_%s@80" % k] = panel(k, 80)
    for k in ("hammer", "swap", "cross", "moves", "score", "multi"):
        sp["icon_" + k] = icon(k)
    for ch in GLYPH_CHARS:
        for j, px in enumerate(GLYPH_PX):
            im = glyph(ch, px)
            sp["g_%d" % ord(ch) + ("" if j == 0 else "@%d" % im.size[1])] = im
    for ch in BIG_GLYPH_CHARS:
        for px in BIG_GLYPH_PX:
            im = glyph(ch, px)
            sp["g_%d@%d" % (ord(ch), im.size[1])] = im
    for k, s in ZH.items():
        for j, h in enumerate(ZH_SIZES.get(k, [20])):
            sp["zh_" + k + ("" if j == 0 else "@%d" % (h * TS))] = zh_label(s, h)
    for i, n in level_names():
        for j, h in enumerate(NAME_SIZES):
            sp["name_%d" % i + ("" if j == 0 else "@%d" % (h * TS))] = zh_label(n, h, (255, 240, 200), (40, 20, 10))
    # 112px 棋盘贴图再各出一个 @56 半尺寸变体：HUD 目标图标（26 / 18 逻辑像素）、
    # 双面块小角标、以及 1x 屏上的棋盘格都用它，避免 SDL 双线性一次缩小 2 倍以上产生锯齿。
    for name in [n for n, im in sp.items() if im.size == (S, S)]:
        sp[name + "@56"] = sp[name].resize((S // 2, S // 2), Image.LANCZOS)
    return sp


# 图例：名字 → (中文, English)；'@' 开头表示叠在宝石上预览
LEGEND = [
    ("宝石 Gems（颜色 × 形状）", [("gem_" + k, g["zh"], g["en"]) for k, g in GEMS.items()]),
    ("特殊块 Specials", [("@line_h", "横向直线", "Line H"), ("@line_v", "纵向直线", "Line V"), ("@bomb", "炸弹", "Bomb"),
                        ("rainbow", "彩虹球", "Rainbow"), ("@flip", "双面块", "Flip front/back"), ("@countdown", "倒计时炸弹", "Countdown n"),
                        ("@chameleon", "变色龙", "Chameleon")]),
    ("宝石覆盖 Overlays", [("@ice_1", "冰 1 层", "Ice 1"), ("@ice_2", "冰 2 层", "Ice 2"), ("@ice_3", "冰 3 层+", "Ice 3+"),
                          ("@grass", "草坪", "Grass"), ("@vine", "藤蔓", "Vine"), ("@choco", "巧克力", "Chocolate"),
                          ("@fog_1", "迷雾 1", "Fog 1"), ("@fog_2", "迷雾 2+", "Fog 2+"), ("@chain_1", "锁链 1", "Chain 1"),
                          ("@chain_2", "锁链 2+", "Chain 2+"), ("@freeze_1", "火箭冰冻 1", "Freeze 1"), ("@freeze_2", "火箭冰冻 2+", "Freeze 2+"),
                          ("@curtain_1", "窗帘 1", "Curtain 1"), ("@curtain_2", "窗帘 2+", "Curtain 2+"), ("@steam", "蒸汽", "Steam")]),
    ("障碍物 Blockers", [("stone_3", "石头 3+", "Stone 3+"), ("stone_2", "石头 2", "Stone 2"), ("stone_1", "石头 1", "Stone 1"),
                        ("chest", "宝箱", "Chest n"), ("honey", "蜂蜜罐", "Honey n"), ("cake_1", "蛋糕 1", "Cake 1"),
                        ("cake_2", "蛋糕 2", "Cake 2"), ("cake_3", "蛋糕 3+", "Cake 3+"), ("safe", "保险箱", "Safe n"),
                        ("cookie", "饼干", "Cookie"), ("magic_hat", "魔法帽", "Magic hat"), ("snail", "蜗牛", "Snail"),
                        ("surprise", "彩蛋", "Surprise"), ("time_spirit", "时间精灵", "Time spirit"), ("bubble", "气泡", "Bubble"),
                        ("magic_stone_1", "魔法石", "Magic stone"), ("magic_stone_3", "魔法石 满", "Magic stone full"),
                        ("fuzzball", "毛球", "Fuzzball"), ("snow_boss", "雪怪 Boss（2×2）", "Snow boss 2x2")]),
    ("带颜色的障碍 Colored（同样用形状徽记）", [("balloon_" + k, "气球", "Balloon " + k.upper()) for k in GEMS]
     + [("bottle_" + k, "染色瓶", "Bottle " + k.upper()) for k in GEMS]
     + [("maker_" + k, "果汁机", "Maker " + k.upper()) for k in GEMS] + [("ufo_" + k, "飞碟", "UFO " + k.upper()) for k in GEMS]),
    ("地面与标记 Floor & UI", [("@tiles", "棋盘格", "Cells"), ("carpet_open", "地毯目标", "Carpet target"), ("carpet_covered", "已铺地毯", "Carpet"),
                             ("jelly_2", "双层果冻", "Jelly x2"), ("jelly", "果冻 1 层", "Jelly x1"), ("magic", "魔法地格", "Magic ground"), ("belt", "传送带", "Belt"), ("portal", "传送门", "Portal"), ("cookie_drop", "饼干掉落口", "Cookie drop"), ("sel_ring", "选中框", "Selection"),
                             ("hint_glow", "提示光", "Hint"), ("@badge", "层数角标", "Layer badge")]),
]


def compose(sp, key):
    """图例用：把覆盖层叠到宝石上预览（与游戏内叠放顺序一致）。"""
    base = Image.new("RGBA", (S, S), (0, 0, 0, 0))
    base.alpha_composite(sp["tile_a"])
    if not key.startswith("@"):
        base.alpha_composite(sp[key])
        return base
    k = key[1:]
    if k in ("line_h", "line_v"):
        base.alpha_composite(sp["gem_c3" if k == "line_h" else "gem_c1"])
        base.alpha_composite(sp[k])
    elif k == "bomb":
        base.alpha_composite(sp["bomb_glow"])
        base.alpha_composite(sp["gem_c5"])
        base.alpha_composite(sp["bomb_mark"])
    elif k == "flip":
        base.alpha_composite(sp["gem_c1"])
        base.alpha_composite(sp["gem_c3"].resize((S * 11 // 25, S * 11 // 25), Image.LANCZOS), (S - S * 11 // 25, 0))
        base.alpha_composite(sp["flip_mark"])
    elif k == "countdown":
        base.alpha_composite(sp["gem_c2"])
        base.alpha_composite(sp["countdown_3"])
    elif k == "chameleon":
        base.alpha_composite(sp["gem_c4"])
        base.alpha_composite(sp["chameleon"])
    elif k == "badge":
        base.alpha_composite(sp["chest"])
        base.alpha_composite(sp["badge_2"].resize((44, 44), Image.LANCZOS), (S - 46, S - 46))
    elif k == "tiles":
        half = S // 2
        for i, (x, y) in enumerate([(0, 0), (half, 0), (0, half), (half, half)]):
            base.alpha_composite(sp["tile_a" if i in (0, 3) else "tile_b"].resize((half, half), Image.LANCZOS), (x, y))
    else:
        gem = {"ice": "gem_c3", "grass": "gem_c4", "vine": "gem_c2", "choco": "gem_c1", "fog": "gem_c5", "chain": "gem_c1",
               "freeze": "gem_c4", "curtain": "gem_c2", "steam": "gem_c3"}[k.split("_")[0]]
        base.alpha_composite(sp[gem])
        base.alpha_composite(sp[k])
    return base


def legend(sp, path):
    cols = 8
    cell_w, cell_h = 150, 176
    title_h = 50
    fz = font_cjk(20)
    fe = font_latin(19)
    ft = font_cjk(26)
    rows_total = sum((len(items) + cols - 1) // cols for _, items in LEGEND)
    W = cols * cell_w + 40
    H = 90 + len(LEGEND) * title_h + rows_total * cell_h + 20
    bg = background(W, H)
    d = ImageDraw.Draw(bg)
    d.text((20, 22), "match3 贴图图例 / Sprite legend（形状 + 颜色双编码）", font=font_cjk(32), fill=(255, 236, 170, 255))
    y = 90
    for title, items in LEGEND:
        d.text((20, y + 8), title, font=ft, fill=(200, 210, 255, 255))
        y += title_h
        for i, (key, zh, en) in enumerate(items):
            cx = 20 + (i % cols) * cell_w
            cy = y + (i // cols) * cell_h
            card = Image.new("RGBA", (cell_w - 12, cell_h - 12), (0, 0, 0, 0))
            ImageDraw.Draw(card).rounded_rectangle((0, 0, card.size[0] - 1, card.size[1] - 1), radius=14, fill=(30, 26, 66, 230), outline=(110, 100, 190, 255), width=2)
            bg.alpha_composite(card, (cx, cy))
            bg.alpha_composite(compose(sp, key), (cx + (cell_w - 12 - S) // 2, cy + 6))
            tw = d.textlength(zh, font=fz)
            d.text((cx + (cell_w - 12 - tw) / 2, cy + S + 8), zh, font=fz, fill=(255, 255, 255, 255))
            tw = d.textlength(en, font=fe)
            d.text((cx + (cell_w - 12 - tw) / 2, cy + S + 34), en, font=fe, fill=(190, 190, 225, 255))
        y += ((len(items) + cols - 1) // cols) * cell_h
    bg.convert("RGB").save(path)


def main():
    ASSETS.mkdir(exist_ok=True)
    DOCIMG.mkdir(parents=True, exist_ok=True)
    sp = build_sprites()
    pages, rects = pack({k: bleed(v) for k, v in sp.items()})
    for old in ASSETS.glob("atlas*.bmp"):
        old.unlink()
    for i, pg in enumerate(pages):
        save_bmp32(pg, ASSETS / page_file(i))
    with open(ASSETS / "atlas.txt", "w", encoding="utf-8") as fp:
        fp.write("# match3 sprite atlas index: name x y w h page  (generated by tools/gen_assets.py)\n")
        fp.write("# page 0 = atlas.bmp, page n = atlas<n>.bmp; name@H = size variant of name (H px tall)\n")
        for name in sorted(rects):
            x, y, w, h, pg = rects[name]
            fp.write("%s %d %d %d %d %d\n" % (name, x, y, w, h, pg))
    background(k=2).convert("RGB").save(ASSETS / "background.bmp")
    legend({k: v for k, v in sp.items() if "@" not in k}, DOCIMG / "legend.png")
    if "--preview" in sys.argv:
        for i, pg in enumerate(pages):
            pg.save("/tmp/atlas_preview%s.png" % ("" if i == 0 else i))
    print("atlas: %d page(s) %s, %d sprites" % (len(pages), ", ".join("%dx%d" % pg.size for pg in pages), len(rects)))


if __name__ == "__main__":
    main()
