#!/usr/bin/env bash
# 重新生成 test/golden/golden.txt（只在确认规则行为「应该」变化时运行；机制刀停期间不应需要）。
# 用法：在仓库根目录 test/golden/regen.sh [输出文件]，默认写到 test/golden/golden.txt。
# Golden.hs 只依赖门面 Match3.Board / Match3.Game（及 Types / Ufo），可在任意历史提交上原样编译比对。
set -euo pipefail
out="${1:-test/golden/golden.txt}"
build="$(mktemp -d)"
stack build ${STACK_FLAGS:-}
stack exec ${STACK_FLAGS:-} -- ghc -O1 -package match3 -package random -itest/golden \
  -outputdir "$build" -main-is Golden -o "$build/golden-gen" test/golden/Golden.hs >/dev/null
"$build/golden-gen" > "$out"
wc -l "$out"
