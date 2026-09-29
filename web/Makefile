# 网页版（GHC wasm）自动化任务。在 web/ 下运行 `make <目标>`，或在仓库根目录 `make -C web <目标>`。
# 不带目标时显示帮助。兼容 GNU make 3.81+（macOS 自带版本即可）；配方只用 POSIX sh。
#
# 常用变量（命令行覆盖，例如 `make serve PORT=9000 BIND=127.0.0.1`）：
#   PORT / BIND        serve 与 deploy-* 的端口 / 监听地址
#   DEST               deploy-* 的目标目录（Mac 上）
#   TGZ                pack / deploy-install 的包路径
#   GHC_WASM_PREFIX    ghc-wasm-meta 安装目录；FLAVOUR 工具链版本系列
#   NODE / CHROME      一致性测试与 e2e 用的 node、Chrome
#   SHOTS              e2e 截图目录；STEPS / CASES 一致性测试的步数 / 「关卡:种子」列表

SHELL := /bin/sh
.DEFAULT_GOAL := help
.NOTPARALLEL:

WEB             := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
ROOT            := $(patsubst %/,%,$(dir $(WEB)))
GHC_WASM_PREFIX ?= $(HOME)/.ghc-wasm
FLAVOUR         ?= 9.14
PORT            ?= 8080
BIND            ?= 0.0.0.0
DEST            ?= /Users/yubin/Documents/dev/haskell/match3-web
TGZ             ?= $(WEB)/match3-web-dist.tgz
NODE            ?= $(GHC_WASM_PREFIX)/nodejs/bin/node
CHROME          ?= /usr/bin/google-chrome
SHOTS           ?= /workspace/match3-web-shots
STEPS           ?=
CASES           ?=
BOOTSTRAP_URL   := https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh

export GHC_WASM_PREFIX NODE CHROME PORT BIND CASES

# 没有 dist 时给出提示并失败（用于不自动构建的目标）
NEED_DIST = @[ -f "$(WEB)/dist/match3-web.wasm" ] || { echo "没有 web/dist，先运行 make build" >&2; exit 1; }
# 原生 stack 不能带着 ~/.ghc-wasm/env 的编译器变量
NATIVE_ENV = env -u CC -u CXX -u AR -u LD -u RANLIB -u NM -u STRIP

.PHONY: help build atlas serve test-native parity anim-parity e2e test check size pack \
        deploy-install deploy-start deploy-stop deploy-status clean toolchain doctor

help: ## 显示本帮助（默认目标）
	@echo "用法：make <目标> [变量=值]（仓库根目录用 make -C web <目标>）"
	@echo
	@awk 'BEGIN { FS = ":[^#]*## " } /^[a-z][a-z0-9-]*:.*## / { printf "  %-16s %s\n", $$1, $$2 }' "$(WEB)/Makefile"
	@echo
	@echo "变量：PORT=$(PORT) BIND=$(BIND) DEST=$(DEST)"
	@echo "      GHC_WASM_PREFIX=$(GHC_WASM_PREFIX) FLAVOUR=$(FLAVOUR) SHOTS=$(SHOTS)"

build: ## 构建 wasm + 页面 + 图集到 web/dist（web/build.sh）
	@[ -f "$(GHC_WASM_PREFIX)/env" ] || { echo "找不到 $(GHC_WASM_PREFIX)/env：先 make toolchain（或 make doctor 看缺什么）" >&2; exit 1; }
	GHC_WASM_PREFIX="$(GHC_WASM_PREFIX)" "$(WEB)/build.sh"

atlas: ## 强制重新生成网页图集（atlas.webp/json + background.webp），有 dist 时同步进去
	@python3 -c 'from PIL import features; import sys; sys.exit(0 if features.check("webp") else 1)' 2>/dev/null \
	  || { echo "需要带 WebP 的 Pillow：pip install --user pillow（或 apt install python3-pil）" >&2; exit 1; }
	python3 "$(WEB)/tools/gen_web_atlas.py" --assets "$(ROOT)/assets" --out "$(WEB)/.cache/art"
	@if [ -d "$(WEB)/dist" ]; then cp "$(WEB)/.cache/art/atlas.webp" "$(WEB)/.cache/art/atlas.json" "$(WEB)/.cache/art/background.webp" "$(WEB)/dist/" && echo "已同步到 web/dist"; fi

serve: ## 用 serve.py 起本地 / 局域网服务器（PORT、BIND 可改；不自动构建）
	$(NEED_DIST)
	exec python3 "$(WEB)/serve.py" --dir "$(WEB)/dist" --port "$(PORT)" --bind "$(BIND)"

test-native: ## 核心规则测试（仓库根目录 stack test）
	@command -v stack >/dev/null 2>&1 || { echo "找不到 stack：见 https://docs.haskellstack.org/（make doctor）" >&2; exit 1; }
	cd "$(ROOT)" && $(NATIVE_ENV) stack test

parity: ## 状态一致性：原生 Parity.hs 与 wasm 每步 JSON 逐字节相同（STEPS、CASES 可改）
	$(NEED_DIST)
	"$(WEB)/test/parity.sh" state $(STEPS)

anim-parity: ## 动画一致性：原生 ComboFx 与 wasm 逐帧 JSON 逐字节相同（STEPS、CASES 可改）
	$(NEED_DIST)
	"$(WEB)/test/parity.sh" anim $(STEPS)

e2e: ## 无头 Chrome 端到端测试，截图与 report.json 写到 SHOTS
	$(NEED_DIST)
	@[ -x "$(CHROME)" ] || { echo "找不到 Chrome：$(CHROME)；设 CHROME=/path/to/chromium" >&2; exit 1; }
	NODE_PATH="$(dir $(NODE))../lib/node_modules" "$(NODE)" "$(WEB)/test/e2e.mjs" "$(SHOTS)"

test: test-native parity anim-parity e2e ## 全部测试（stack test + 两组一致性 + e2e）
	@echo "== 全部测试通过"

check: ## CI 用：构建后跑全部测试
	$(MAKE) -f "$(WEB)/Makefile" build
	$(MAKE) -f "$(WEB)/Makefile" test
	$(MAKE) -f "$(WEB)/Makefile" size

size: ## 体积报告：wasm 原始 / 优化后、dist 各文件与合计（含 gzip -9）
	$(NEED_DIST)
	@"$(WEB)/tools/size.sh"

pack: ## 打包 dist + serve.py + 部署脚本为 TGZ（默认 web/match3-web-dist.tgz）
	$(NEED_DIST)
	"$(WEB)/deploy-mac.sh" pack "$(TGZ)"

deploy-install: ## 把 TGZ 解包安装到 DEST（在目标机上运行）
	@[ -f "$(TGZ)" ] || { echo "没有 $(TGZ)：先 make pack，或 TGZ=/path/to/match3-web-dist.tgz" >&2; exit 1; }
	"$(WEB)/deploy-mac.sh" install "$(TGZ)" "$(DEST)"

deploy-start: ## macOS：交给 launchd 在 DEST 后台常驻（PORT、BIND 可改）
	"$(WEB)/deploy-mac.sh" start "$(DEST)"

deploy-stop: ## macOS：停止 launchd 代理
	"$(WEB)/deploy-mac.sh" stop

deploy-status: ## 查看部署状态（launchd、lsof 端口监听、curl 自检）
	"$(WEB)/deploy-mac.sh" status

clean: ## 删除 web/dist、网页构建缓存（dist-newstyle、.cache）和 tgz；不碰 ~/.ghc-wasm
	rm -rf "$(WEB)/dist" "$(WEB)/dist-newstyle" "$(WEB)/.cache"
	rm -f "$(WEB)"/*.tgz

toolchain: ## 安装或校验 ghc-wasm-meta（FLAVOUR=9.14）；已安装则只校验，FORCE=1 重跑安装
	@if [ -f "$(GHC_WASM_PREFIX)/env" ] && [ -z "$(FORCE)" ]; then \
	  echo "== 校验 $(GHC_WASM_PREFIX)"; \
	  v=$$(. "$(GHC_WASM_PREFIX)/env" >/dev/null 2>&1 && wasm32-wasi-ghc --numeric-version) || { echo "wasm32-wasi-ghc 不能运行：make toolchain FORCE=1" >&2; exit 1; }; \
	  case "$$v" in $(FLAVOUR).*) echo "  wasm32-wasi-ghc $$v ✓（FLAVOUR $(FLAVOUR)）";; \
	    *) echo "  wasm32-wasi-ghc $$v，与 FLAVOUR=$(FLAVOUR) 不符：make toolchain FORCE=1" >&2; exit 1;; esac; \
	  c=$$(. "$(GHC_WASM_PREFIX)/env" >/dev/null 2>&1 && wasm32-wasi-cabal --numeric-version) || { echo "  缺 wasm32-wasi-cabal" >&2; exit 1; }; \
	  echo "  wasm32-wasi-cabal $$c ✓"; \
	  o=$$(. "$(GHC_WASM_PREFIX)/env" >/dev/null 2>&1 && wasm-opt --version) && echo "  $$o ✓" || echo "  缺 wasm-opt（build 会跳过 -Oz）"; \
	  [ -x "$(GHC_WASM_PREFIX)/nodejs/bin/node" ] && echo "  node $$("$(GHC_WASM_PREFIX)/nodejs/bin/node" --version) ✓" || echo "  缺自带 node"; \
	  echo "工具链可用；首次构建若提示没有 Hackage 索引：sh -c '. $(GHC_WASM_PREFIX)/env && wasm32-wasi-cabal update'"; \
	else \
	  for t in curl jq unzip zstd xz make; do command -v $$t >/dev/null 2>&1 || { echo "安装脚本需要 $$t：sudo apt-get install -y curl jq unzip zstd xz-utils make" >&2; exit 1; }; done; \
	  echo "== 安装 ghc-wasm-meta FLAVOUR=$(FLAVOUR) 到 $(GHC_WASM_PREFIX)（约 6.4 GB）"; \
	  curl -fL "$(BOOTSTRAP_URL)" | FLAVOUR="$(FLAVOUR)" PREFIX="$(GHC_WASM_PREFIX)" sh; \
	fi

doctor: ## 检查前置工具（ghc-wasm、node、python3/Pillow、stack、wasm-opt、Chrome…）并给出安装提示
	@"$(WEB)/tools/doctor.sh"
