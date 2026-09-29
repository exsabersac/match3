#!/usr/bin/env bash
# 体积报告（make size）：wasm 原始 / -Oz 后，dist 各文件与合计，均给 gzip -9 后大小。
set -euo pipefail
HERE="$(cd "$(dirname "$0")/.." && pwd)"
DIST="$HERE/dist"
[ -f "$DIST/match3-web.wasm" ] || { echo "没有 web/dist，先 make build" >&2; exit 1; }
sz() { wc -c < "$1" | tr -d ' '; }
gz() { gzip -9 -c "$1" | wc -c | tr -d ' '; }
row() { printf '%11s %11s  %s\n' "$2" "$3" "$1"; }
echo "   原始字节      gzip-9  文件"
raw="$(find "$HERE/dist-newstyle" -type f -name match3-web.wasm -path '*/build/*' 2>/dev/null | head -1 || true)"
[ -n "$raw" ] && row "wasm（链接后，未优化）" "$(sz "$raw")" "$(gz "$raw")"
total=0; totalgz=0
while IFS= read -r f; do
  s=$(sz "$f"); g=$(gz "$f"); total=$((total + s)); totalgz=$((totalgz + g))
  row "${f#$DIST/}" "$s" "$g"
done < <(find "$DIST" -type f | LC_ALL=C sort)
row "dist 合计" "$total" "$totalgz"
echo "（WebP 本身已压缩，gzip 几乎无收益；服务器开 gzip/br 时主要省在 wasm 与 JS）"
