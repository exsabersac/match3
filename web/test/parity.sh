#!/usr/bin/env bash
# 原生 GHC 与 wasm 的一致性批量对比（make parity / make anim-parity 调用）。
#
# 用法：web/test/parity.sh state|anim [步数]
#   state：Parity.hs ↔ node-parity.mjs，每步 m3Swap（最后 m3Undo）的 JSON 逐字节相同
#   anim ：AnimParity.hs ↔ node-anim-parity.mjs，每步全部动画帧 JSON 逐字节相同（原生侧另核对 runPlayer）
# 环境变量：
#   CASES   "关卡:种子 …"（关卡 0 起），默认见下；NODE 默认 ~/.ghc-wasm/nodejs/bin/node
#   OUT     输出与原生二进制目录，默认 web/.cache/parity
# 前置：已 make build（需要 web/dist）；原生侧用 stack 的 GHC 9.14.1 编译（首次约 1 分钟，之后增量）。
set -euo pipefail

HERE="$(cd "$(dirname "$0")/.." && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
MODE="${1:-}"; STEPS="${2:-}"
PREFIX="${GHC_WASM_PREFIX:-$HOME/.ghc-wasm}"
NODE="${NODE:-$PREFIX/nodejs/bin/node}"
OUT="${OUT:-$HERE/.cache/parity}"

case "$MODE" in
  state) SRC=Parity;     JS=node-parity.mjs;      STEPS="${STEPS:-12}"
         DEF="0:20260929 5:1 12:42 15:1 20:1 25:1 27:1 30:1 35:1 37:1 38:2026 39:31337" ;;
  anim)  SRC=AnimParity; JS=node-anim-parity.mjs; STEPS="${STEPS:-20}"
         DEF="0:20260929 11:1 12:42 13:1 15:1 27:1 28:1 35:1 38:2026 39:31337" ;;
  *) echo "用法：$0 state|anim [步数]" >&2; exit 2 ;;
esac
CASES="${CASES:-$DEF}"

[ -f "$HERE/dist/match3-web.wasm" ] || { echo "没有 web/dist/match3-web.wasm，先 make build" >&2; exit 1; }
command -v "$NODE" >/dev/null 2>&1 || { echo "找不到 node（$NODE）；设 NODE=... 或先 make toolchain" >&2; exit 1; }
command -v stack >/dev/null 2>&1 || { echo "找不到 stack（原生侧需要）" >&2; exit 1; }

mkdir -p "$OUT/obj-$SRC"
BIN="$OUT/$SRC"
echo "== 编译原生 $SRC（stack exec -- ghc -O1，增量）"
# 不要带着 ~/.ghc-wasm/env 的 CC/AR 等变量编原生代码
(cd "$ROOT" && env -u CC -u CXX -u AR -u LD -u RANLIB -u NM -u STRIP \
   stack exec -- ghc -O1 -v0 -isrc -iapp/pure -iweb/hs -outputdir "$OUT/obj-$SRC" -o "$BIN" "web/test/$SRC.hs")

export LANG="${LANG:-C.UTF-8}" LC_ALL="${LC_ALL:-C.UTF-8}"
fail=0; n=0
for c in $CASES; do
  li="${c%%:*}"; seed="${c##*:}"; n=$((n + 1))
  nat="$OUT/$MODE-$li-$seed.native"; was="$OUT/$MODE-$li-$seed.wasm"
  if ! "$BIN" "$li" "$seed" "$STEPS" > "$nat" 2> "$nat.err"; then
    echo "✗ 第 $((li + 1)) 关 种子 $seed：原生侧失败（$(tail -1 "$nat.err")）"; fail=$((fail + 1)); continue
  fi
  if ! "$NODE" "$HERE/test/$JS" "$li" "$seed" "$STEPS" > "$was" 2> "$was.err"; then
    echo "✗ 第 $((li + 1)) 关 种子 $seed：wasm 侧失败（$(tail -1 "$was.err")）"; fail=$((fail + 1)); continue
  fi
  if cmp -s "$nat" "$was"; then
    echo "✓ 第 $((li + 1)) 关 种子 $seed：一致（$(wc -c < "$nat" | tr -d ' ') 字节）$( [ "$MODE" = anim ] && echo "  $(tail -1 "$was.err")")"
  else
    echo "✗ 第 $((li + 1)) 关 种子 $seed：不一致 → diff $nat $was"; fail=$((fail + 1))
  fi
done
echo "== $MODE 一致性：$((n - fail))/$n 组一致"
[ "$fail" -eq 0 ]
