#!/usr/bin/env bash
# 原生 GHC 与 wasm 的一致性批量对比（make parity / make anim-parity 调用）。
#
# 用法：web/test/parity.sh state|anim [步数]
#   state：Parity.hs ↔ node-parity.mjs，每步 m3Swap（最后 m3Undo）的 JSON 逐字节相同
#   anim ：AnimParity.hs ↔ node-anim-parity.mjs，每步全部动画帧 JSON 逐字节相同（原生侧另核对 runPlayer）
# 环境变量：
#   CASES   "关卡:种子[:走法] …"（关卡 0 起），默认见下；走法 hint（缺省，按核心提示）或 combo
#           （先换盘上的「彩虹 × 直线 / 炸弹」再按提示，覆盖第 44 关 rainbow_combos 的变身步；见 Parity.hs 的 pickMove）
#           或 combo-bomb（同 combo，但先换「彩虹 × 炸弹」）或 cham-rainbow（先换「彩虹 × 变色龙」，第 47 关成对交换规则 15）
#   NODE    默认 ~/.ghc-wasm/nodejs/bin/node
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
         DEF="0:20260929 5:1 12:42 15:1 20:1 25:1 27:1 30:1 35:1 37:1 38:2026 39:31337 40:1 41:1 42:1 42:3 43:1 43:1:combo 43:2:combo 43:3:combo-bomb" ;;
  anim)  SRC=AnimParity; JS=node-anim-parity.mjs; STEPS="${STEPS:-20}"
         DEF="0:20260929 11:1 12:42 13:1 15:1 27:1 28:1 35:1 38:2026 39:31337 40:1 41:1 42:1 42:2 42:5 43:1:combo 43:2:combo 43:3:combo-bomb" ;;
  *) echo "用法：$0 state|anim [步数]" >&2; exit 2 ;;
esac
# 第 45 关「雪怪」Boss（下标 44，新玩法 5）：两组都跑 3 个种子
DEF="$DEF 44:1 44:2 44:3"
# 第 47 关「变色龙」（下标 46，新玩法 7）：两组都跑种子 1、2（每步都有步末换色 EvTick "chameleon"）与种子 140 的
# cham-rainbow 走法（第 6 步换彩虹 × 变色龙，覆盖成对交换规则 15）
DEF="$DEF 46:1 46:2 46:140:cham-rainbow"
CASES="${CASES:-$DEF}"

[ -f "$HERE/dist/match3-web.wasm" ] || { echo "没有 web/dist/match3-web.wasm，先 make build" >&2; exit 1; }
command -v "$NODE" >/dev/null 2>&1 || { echo "找不到 node（${NODE}）；设 NODE=... 或先 make toolchain" >&2; exit 1; }
command -v stack >/dev/null 2>&1 || { echo "找不到 stack（原生侧需要）" >&2; exit 1; }

mkdir -p "$OUT/obj-$SRC"
BIN="$OUT/$SRC"
echo "== 编译原生 ${SRC}（stack exec -- ghc -O1，增量）"
# 不要带着 ~/.ghc-wasm/env 的 CC/AR 等变量编原生代码
(cd "$ROOT" && env -u CC -u CXX -u AR -u LD -u RANLIB -u NM -u STRIP \
   stack exec -- ghc -O1 -v0 -isrc -iapp/pure -iweb/hs -outputdir "$OUT/obj-$SRC" -o "$BIN" "web/test/$SRC.hs")

export LANG="${LANG:-C.UTF-8}" LC_ALL="${LC_ALL:-C.UTF-8}"
fail=0; n=0
for c in $CASES; do
  IFS=: read -r li seed how <<< "${c}"; how="${how:-hint}"; n=$((n + 1))
  tag="$( [ "${how}" = hint ] && echo "" || echo "（${how} 走法）")"
  nat="$OUT/$MODE-$li-$seed-${how}.native"; was="$OUT/$MODE-$li-$seed-${how}.wasm"
  if ! "$BIN" "$li" "$seed" "$STEPS" "${how}" > "$nat" 2> "$nat.err"; then
    echo "✗ 第 $((li + 1)) 关 种子 ${seed}${tag}：原生侧失败（$(tail -1 "$nat.err")）"; fail=$((fail + 1)); continue
  fi
  if ! "$NODE" "$HERE/test/$JS" "$li" "$seed" "$STEPS" "${how}" > "$was" 2> "$was.err"; then
    echo "✗ 第 $((li + 1)) 关 种子 ${seed}${tag}：wasm 侧失败（$(tail -1 "$was.err")）"; fail=$((fail + 1)); continue
  fi
  # combo 走法必须真的走到变身步（状态 JSON 的 trace.end 里有 rainbow_line / rainbow_bomb；动画帧里有蔓延段 spread）
  # cham-rainbow 走法必须真的换到了「彩虹 × 变色龙」（两侧 stderr 都记了这一步）
  if [ "${how}" = cham-rainbow ]; then
    if ! grep -q "走法 cham-rainbow：" "$nat.err" || ! grep -q "走法 cham-rainbow：" "$was.err"; then
      echo "✗ 第 $((li + 1)) 关 种子 ${seed}${tag}：没有走到彩虹 × 变色龙"; fail=$((fail + 1)); continue
    fi
  elif [ "${how}" != hint ] && ! grep -q "$( [ "$MODE" = state ] && echo '"kind":"rainbow_' || echo '"kind":"spread"')" "$nat"; then
    echo "✗ 第 $((li + 1)) 关 种子 ${seed}${tag}：没有走到彩虹组合变身步"; fail=$((fail + 1)); continue
  fi
  if cmp -s "$nat" "$was"; then
    echo "✓ 第 $((li + 1)) 关 种子 ${seed}${tag}：一致（$(wc -c < "$nat" | tr -d ' ') 字节）$( [ "$MODE" = anim ] && echo "  $(tail -1 "$was.err")")"
  else
    echo "✗ 第 $((li + 1)) 关 种子 ${seed}${tag}：不一致 → diff $nat $was"; fail=$((fail + 1))
  fi
done
echo "== $MODE 一致性：$((n - fail))/$n 组一致"
[ "$fail" -eq 0 ]
