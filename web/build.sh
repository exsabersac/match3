#!/usr/bin/env bash
# 构建网页版 spike：Haskell 核心 → wasm（GHC wasm 后端）→ 静态站点 web/dist/
#
# 用法：
#   ./build.sh            # 构建到 web/dist/
#   ./build.sh --serve    # 构建后在 http://127.0.0.1:8080/ 起静态服务器
#
# 前置：已用 ghc-wasm-meta 安装工具链到 ~/.ghc-wasm（见 README.md），可用 GHC_WASM_PREFIX 覆盖。
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
PREFIX="${GHC_WASM_PREFIX:-$HOME/.ghc-wasm}"
DIST="$HERE/dist"
WASI_SHIM_VERSION="0.4.2"   # @bjorn3/browser_wasi_shim，GHC wasm 文档推荐的浏览器 WASI 实现

if [ ! -f "$PREFIX/env" ]; then
  echo "找不到 $PREFIX/env，请先按 README 安装 ghc-wasm-meta 工具链" >&2
  exit 1
fi
# 只在本脚本的子进程里加载 wasm 工具链环境（它会改 CC/AR 等变量，别污染桌面版 stack 构建）
# shellcheck disable=SC1091
source "$PREFIX/env"

# 1) 检查核心模块列表：match3-web.cabal 直接编译 ../src，模块清单（Engine.* + Match3.*）要与 package.yaml 一致
want=$(sed -n '/^library:/,/^[a-z]/p' "$ROOT/package.yaml" | sed -n 's/^  - \(\(Match3\|Engine\)\..*\)$/\1/p' | sort)
have=$(sed -n 's/^    \(\(Match3\|Engine\)\.[A-Za-z.]*\)$/\1/p' "$HERE/match3-web.cabal" | sort)
if [ "$want" != "$have" ]; then
  echo "警告：match3-web.cabal 的核心模块清单与 package.yaml 不一致：" >&2
  diff <(echo "$want") <(echo "$have") >&2 || true
fi

# 2) 编译并链接 wasm reactor 模块
cd "$HERE"
wasm32-wasi-cabal build exe:match3-web
WASM_RAW="$(wasm32-wasi-cabal list-bin exe:match3-web)"

# 3) 产物：优化后的 wasm + JSFFI 胶水 JS + 页面 + WASI 垫片
rm -rf "$DIST"
mkdir -p "$DIST/vendor"
if command -v wasm-opt >/dev/null; then
  # binaryen 优化体积（ghc-wasm-meta 自带）；失败则退回原始产物
  wasm-opt --enable-bulk-memory --enable-reference-types --enable-sign-ext --enable-nontrapping-float-to-int \
           --enable-mutable-globals --enable-simd --enable-multivalue \
           -Oz "$WASM_RAW" -o "$DIST/match3-web.wasm" || cp "$WASM_RAW" "$DIST/match3-web.wasm"
else
  cp "$WASM_RAW" "$DIST/match3-web.wasm"
fi
# post-link：从 wasm 的 ghc_wasm_jsffi 自定义段生成 JS 胶水（提供 ghc_wasm_jsffi 导入）
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$WASM_RAW" -o "$DIST/ghc_wasm_jsffi.js"
cp "$HERE/www/"* "$DIST/"

# 4) 浏览器 WASI 垫片（首次联网下载，之后用缓存）
CACHE="$HERE/.cache/browser_wasi_shim-$WASI_SHIM_VERSION"
if [ ! -f "$CACHE/index.js" ]; then
  tmp="$(mktemp -d)"
  (cd "$tmp" && npm pack --silent "@bjorn3/browser_wasi_shim@$WASI_SHIM_VERSION" >/dev/null 2>&1 && tar xzf ./*.tgz)
  mkdir -p "$CACHE"
  cp "$tmp"/package/dist/*.js "$tmp"/package/LICENSE-* "$CACHE/"
  rm -rf "$tmp"
fi
mkdir -p "$DIST/vendor/browser_wasi_shim"
cp "$CACHE"/* "$DIST/vendor/browser_wasi_shim/"

# 5) 体积报告
raw=$(stat -c %s "$WASM_RAW"); rawgz=$(gzip -9 -c "$WASM_RAW" | wc -c)
opt=$(stat -c %s "$DIST/match3-web.wasm"); gz=$(gzip -9 -c "$DIST/match3-web.wasm" | wc -c)
echo "wasm 原始 $raw 字节（gzip -9 后 $rawgz）；wasm-opt -Oz 后 $opt 字节（gzip -9 后 $gz）"
echo "产物在 $DIST"

if [ "${1:-}" = "--serve" ]; then
  echo "打开 http://127.0.0.1:8080/?level=0 试玩（Ctrl+C 退出）"
  cd "$DIST" && exec python3 -m http.server 8080 --bind 127.0.0.1
fi
