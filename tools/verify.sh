#!/usr/bin/env bash
# make verify：日常提交前的一条命令验收（见 docs/testing.md「开发流程」）。
#
#   1. 0 警告构建：stack build --test --no-run-tests --ghc-options=-Werror（含 match3-sdl 与测试）
#   2. stack test（同一份 -Werror 构建，不再重编）
#   3. 仅当相对 BASE（默认 origin/main）的改动触及网页版依赖的路径时跑 make check；FULL=1 强制跑。
#      跑的是 make check 去掉 test-native 的那几步（lint-sh、build、parity、anim-parity、e2e、size），
#      stack test 第 2 步已经跑过，不重复。
#
# -Werror 构建放在单独的工作目录 .stack-work-verify（STACK_VERIFY_WORK_DIR 可改）：
# 换编译选项不会让日常的 .stack-work 重编，自己的增量缓存也一直是 -Werror 编出来的
# （有警告的模块编译失败、不进缓存，下次照样报）。第一次运行要全量编译一遍。
#
# 写法只用 bash 3.2 也有的特性（不用 mapfile、关联数组、${v,,} 等）。
set -u
set -o pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT" || exit 1

BASE="${BASE:-origin/main}"
FULL="${FULL:-0}"
KEEP_DIST="${KEEP_DIST:-0}"
WORK_DIR="${STACK_VERIFY_WORK_DIR:-.stack-work-verify}"
MAKE_CMD="${MAKE:-make}"
LOG_DIR="$(mktemp -d "${TMPDIR:-/tmp}/match3-verify.XXXXXX")"
START=$(date +%s)

# 网页版 make check 依赖的路径（git diff --name-only 的前缀 / 文件名）。web/dist 是构建产物、
# web/android-app 只进 APK，不触发；src/ 下只看增删改名（模块清单要与 web/match3-web.cabal 一致），
# 内容改动由 stack test 覆盖（网页版对拍比的是同一份核心源码的原生与 wasm 输出）。
needs_check() {
  case "$1" in
    web/dist/*|web/android-app/*) return 1 ;;
    web/*|app/pure/*|assets/*|Makefile|package.yaml|match3.cabal|stack.yaml|tools/verify.sh) return 0 ;;
  esac
  return 1
}

elapsed() {
  t=$(( $(date +%s) - START ))
  printf '%dm%02ds' $((t / 60)) $((t % 60))
}

finish() {
  # $1 = 0 通过 / 其他 失败；$2 = 说明
  if [ "$1" -eq 0 ]; then
    echo "== verify 通过：$2；用时 $(elapsed)"
    rm -rf "$LOG_DIR"
  else
    echo "== verify 失败：$2；用时 $(elapsed)；日志在 ${LOG_DIR}"
  fi
  exit "$1"
}

if ! command -v stack >/dev/null 2>&1; then
  echo "找不到 stack（make doctor）" >&2
  finish 1 "缺 stack"
fi
# 原生 stack 不能带着 ~/.ghc-wasm/env 的编译器变量
unset CC CXX AR LD RANLIB NM STRIP

STACK_OPTS="--work-dir ${WORK_DIR}"
GHC_OPTS="--ghc-options=-Werror"

echo "== verify 1/3：0 警告构建（-Werror，工作目录 ${WORK_DIR}）"
# shellcheck disable=SC2086
if ! stack $STACK_OPTS build --test --no-run-tests $GHC_OPTS 2>&1 | tee "$LOG_DIR/build.log"; then
  finish 1 "构建失败或有警告（见上面的 error / warning）"
fi

echo "== verify 2/3：stack test"
# shellcheck disable=SC2086
stack $STACK_OPTS test $GHC_OPTS 2>&1 | tee "$LOG_DIR/test.log"
test_rc=$?
passed=$(sed -n 's/.*All \([0-9][0-9]*\) tests passed.*/\1/p' "$LOG_DIR/test.log" | tail -n 1)
if [ "$test_rc" -ne 0 ] || [ -z "$passed" ]; then
  failed=$(sed -n 's/.*\([0-9][0-9]* out of [0-9][0-9]* tests failed\).*/\1/p' "$LOG_DIR/test.log" | tail -n 1)
  finish 1 "stack test 未通过（${failed:-见日志}）"
fi
summary="stack test ${passed} 个全过"

echo "== verify 3/3：是否需要 make check"
run_check=0
reason=""
if [ "$FULL" = "1" ]; then
  run_check=1
  reason="FULL=1"
elif ! mb=$(git merge-base "$BASE" HEAD 2>/dev/null); then
  run_check=1
  reason="找不到 BASE=${BASE} 的合并基点，保守起见跑"
else
  changed=$( { git diff --name-only "$mb"; git ls-files --others --exclude-standard; } 2>/dev/null | sort -u)
  hit=""
  while IFS= read -r f; do
    if [ -n "$f" ] && needs_check "$f"; then hit="$f"; break; fi
  done <<LIST
$changed
LIST
  if [ -z "$hit" ]; then
    hit=$( { git diff --name-only --diff-filter=ADR "$mb" -- src; git ls-files --others --exclude-standard -- src; } 2>/dev/null | head -n 1)
  fi
  if [ -n "$hit" ]; then
    run_check=1
    reason="相对 ${BASE} 改了 ${hit} 等"
  else
    reason="相对 ${BASE} 没有改网页版依赖的路径"
  fi
fi

if [ "$run_check" -eq 0 ]; then
  echo "   不跑（${reason}；FULL=1 可强制）"
  finish 0 "${summary}；make check 未跑（${reason}）"
fi

echo "   跑 make check（${reason}）"
dist_clean=0
if git diff --quiet HEAD -- web/dist 2>/dev/null; then dist_clean=1; fi
# make check = lint-sh build test size，test = test-native parity anim-parity e2e；test-native 即第 2 步，这里不重复
"$MAKE_CMD" -f "$ROOT/Makefile" lint-sh build parity anim-parity e2e size 2>&1 | tee "$LOG_DIR/check.log"
check_rc=$?
# 平时不提交 web/dist：make check 重建出的产物还原成提交里的版本（KEEP_DIST=1 保留）
if [ "$dist_clean" -eq 1 ] && [ "$KEEP_DIST" != "1" ]; then
  git checkout -q HEAD -- web/dist 2>/dev/null
  echo "   已把 web/dist 还原成 HEAD 的版本（KEEP_DIST=1 可保留重建产物）"
fi
if [ "$check_rc" -ne 0 ]; then
  finish 1 "${summary}；make check 失败（${reason}）"
fi
finish 0 "${summary}；make check 全绿（${reason}）"
