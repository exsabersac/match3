#!/usr/bin/env bash
# 环境自检（make doctor）：逐项检查网页版构建 / 测试 / 部署需要的工具，缺什么就给出安装提示。
# 必需项缺失时退出码 1；可选项只提示。不修改任何东西。
set -uo pipefail

HERE="$(cd "$(dirname "$0")/.." && pwd)"
PREFIX="${GHC_WASM_PREFIX:-$HOME/.ghc-wasm}"
NODE="${NODE:-$PREFIX/nodejs/bin/node}"
CHROME="${CHROME:-/usr/bin/google-chrome}"
WANT_FLAVOUR="${FLAVOUR:-9.14}"
miss=0

ok()   { printf '  ✓ %-22s %s\n' "$1" "$2"; }
bad()  { printf '  ✗ %-22s %s\n      → %s\n' "$1" "$2" "$3"; miss=$((miss + 1)); }
opt()  { printf '  - %-22s %s\n      → %s\n' "$1" "$2" "$3"; }
ver()  { "$@" 2>&1 | head -1; }
# 在 wasm 工具链环境里执行（子 shell，不污染当前环境）
inwasm() { ( . "$PREFIX/env" >/dev/null 2>&1 && "$@" ); }

echo "== wasm 工具链（${PREFIX}）"
if [ -f "$PREFIX/env" ]; then
  ok "ghc-wasm-meta env" "$PREFIX/env"
  if v="$(inwasm wasm32-wasi-ghc --numeric-version 2>/dev/null)"; then
    case "$v" in "$WANT_FLAVOUR".*) ok "wasm32-wasi-ghc" "$v" ;;
      *) bad "wasm32-wasi-ghc" "${v}（期望 $WANT_FLAVOUR.x）" "重装：make toolchain FORCE=1" ;; esac
  else bad "wasm32-wasi-ghc" "找不到" "make toolchain"; fi
  if v="$(inwasm wasm32-wasi-cabal --numeric-version 2>/dev/null)"; then ok "wasm32-wasi-cabal" "$v"
  else bad "wasm32-wasi-cabal" "找不到" "make toolchain"; fi
  if v="$(inwasm wasm-opt --version 2>/dev/null)"; then ok "wasm-opt（binaryen）" "$v"
  else opt "wasm-opt（binaryen）" "找不到（build 会跳过 -Oz，wasm 大一倍）" "ghc-wasm-meta 自带；make toolchain FORCE=1"; fi
else
  bad "ghc-wasm-meta" "没有 $PREFIX/env" "make toolchain（约 6.4 GB；依赖 curl jq unzip zstd xz-utils make）"
fi

echo "== Node / 浏览器（一致性测试与 e2e）"
if [ -x "$NODE" ] || command -v "$NODE" >/dev/null 2>&1; then ok "node" "$(ver "$NODE" --version)（${NODE}）"
else bad "node" "找不到 $NODE" "ghc-wasm-meta 自带 node；或设 NODE=/path/to/node"; fi
pw="$(dirname "$(dirname "$NODE")")/lib/node_modules/playwright-core"
if [ -d "$pw" ]; then ok "playwright-core" "$pw"
else opt "playwright-core" "找不到（只影响 e2e）" "ghc-wasm-meta 的 node 自带；或 npm i -g playwright-core"; fi
if [ -x "$CHROME" ]; then ok "Chrome（e2e）" "$(ver "$CHROME" --version)"
else opt "Chrome（e2e）" "找不到 ${CHROME}（只影响 e2e）" "装 google-chrome，或设 CHROME=/path/to/chromium"; fi
if command -v npm >/dev/null 2>&1 || [ -x "$PREFIX/nodejs/bin/npm" ] || [ -d "$HERE/.cache/browser_wasi_shim-0.4.2" ]; then
  ok "WASI 垫片来源" "$( [ -d "$HERE/.cache/browser_wasi_shim-0.4.2" ] && echo 已缓存 || echo 'npm 可用（首次构建联网下载）')"
else bad "npm" "找不到，且没有 WASI 垫片缓存" "首次构建需要 npm 下载 @bjorn3/browser_wasi_shim"; fi

echo "== Python / 图集"
if command -v python3 >/dev/null 2>&1; then ok "python3" "$(ver python3 --version)"
  if python3 -c 'from PIL import features, Image; import sys; sys.exit(0 if features.check("webp") else 1)' 2>/dev/null; then
    ok "Pillow（WebP）" "$(python3 -c 'import PIL; print(PIL.__version__)')"
  else bad "Pillow（WebP）" "缺少或不带 WebP" "pip install --user pillow（或 apt install python3-pil）；已有缓存 web/.cache/art 时 build 仍可用"; fi
else bad "python3" "找不到" "apt install python3 / macOS: xcode-select --install"; fi
if command -v cwebp >/dev/null 2>&1; then ok "cwebp（可选）" "$(ver cwebp -version)"
else opt "cwebp（可选）" "未安装（图集用 Pillow 编码，不需要它）" "apt install webp / brew install webp"; fi

echo "== 原生 Haskell（stack test 与一致性测试原生侧）"
if command -v stack >/dev/null 2>&1; then ok "stack" "$(ver stack --numeric-version)"
else bad "stack" "找不到" "https://docs.haskellstack.org/ 或 ghcup install stack"; fi
# stack.yaml 用 system-ghc：以 stack 实际拿到的 ghc 为准（可能来自 ~/.ghcup/bin）
if command -v stack >/dev/null 2>&1 && v="$(cd "$HERE/.." && stack exec -- ghc --numeric-version 2>/dev/null)"; then
  case "$v" in 9.14.*) ok "ghc（stack 用的）" "$v" ;; *) opt "ghc（stack 用的）" "${v}（stack.yaml 期望 9.14.1）" "ghcup install ghc 9.14.1" ;; esac
else bad "ghc（stack 用的）" "stack 找不到 ghc（stack.yaml 用 system-ghc）" "ghcup install ghc 9.14.1；并 export PATH=\$HOME/.ghcup/bin:\$PATH"; fi
if [ -n "${CC:-}" ] && printf '%s' "${CC}" | grep -q wasm; then
  bad "环境变量 CC" "$CC" "当前 shell source 过 ~/.ghc-wasm/env，会让 stack 用错编译器；开个新终端"
fi

echo "== 其他"
for t in curl gzip tar; do
  if command -v "$t" >/dev/null 2>&1; then ok "$t" "$(command -v "$t")"; else bad "$t" "找不到" "用系统包管理器安装 $t"; fi
done
if command -v lsof >/dev/null 2>&1; then ok "lsof（可选）" "$(command -v lsof)"
else opt "lsof（可选）" "未安装（只用于查端口占用 / deploy-status）" "apt install lsof（macOS 自带）"; fi

echo
if [ "$miss" -eq 0 ]; then echo "doctor：必需项齐全 ✓"; else echo "doctor：缺 $miss 项必需工具，按上面的 → 提示处理"; fi
[ "$miss" -eq 0 ]
