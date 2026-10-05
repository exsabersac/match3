#!/usr/bin/env bash
# make verify：合 main 前的验收 = stack test（见 docs/testing.md「开发流程」）。
# 跑完在最后一行写结论、测试数与用时。make check / make test 留作手动使用，这里不跑。
set -u
set -o pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT" || exit 1
START=$(date +%s)
LOG="$(mktemp "${TMPDIR:-/tmp}/match3-verify.XXXXXX")"

# 原生 stack 不能带着 ~/.ghc-wasm/env 的编译器变量
unset CC CXX AR LD RANLIB NM STRIP

# stdin 接到未关闭的管道（代理 / IDE / 某些 CI）时，tasty/RTS 可能一直等 EOF 而挂死；
# 合 main 验收不需要交互，一律关掉 stdin。
stack test </dev/null 2>&1 | tee "$LOG"
rc=$?
t=$(( $(date +%s) - START ))
took=$(printf '%dm%02ds' $((t / 60)) $((t % 60)))
passed=$(sed -n 's/.*All \([0-9][0-9]*\) tests passed.*/\1/p' "$LOG" | tail -n 1)
if [ "$rc" -eq 0 ] && [ -n "$passed" ]; then
  rm -f "$LOG"
  echo "== verify 通过：stack test ${passed} 个全过；用时 ${took}"
  exit 0
fi
failed=$(sed -n 's/.*\([0-9][0-9]* out of [0-9][0-9]* tests failed\).*/\1/p' "$LOG" | tail -n 1)
echo "== verify 失败：stack test 未通过（${failed:-构建失败或见日志}）；用时 ${took}；日志 ${LOG}"
exit 1
