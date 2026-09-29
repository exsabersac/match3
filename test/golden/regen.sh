#!/usr/bin/env bash
# 重新生成 test/golden/golden.txt（只在确认规则行为「应该」变化时运行；机制刀停期间不应需要）。
# 用法：在仓库根目录 test/golden/regen.sh [输出文件]，默认写到 test/golden/golden.txt。
# 第三刀起 Golden.hs 直接 import Board.* / Game.* 子模块，只能在 4fbcefc 之后（门面删除后）的提交上编译；
# 要在更早的提交上比对，取 4fbcefc 版的 Golden.hs（只经过门面 Match3.Board / Match3.Game）。
set -euo pipefail
out="${1:-test/golden/golden.txt}"
build="$(mktemp -d)"
stack build ${STACK_FLAGS:-}
stack exec ${STACK_FLAGS:-} -- ghc -O1 -package match3 -package random -itest/golden \
  -outputdir "$build" -main-is Golden -o "$build/golden-gen" test/golden/Golden.hs >/dev/null
"$build/golden-gen" > "$out"
wc -l "$out"
