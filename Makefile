# 仓库自动化任务入口（仓库根目录运行 `make <目标>`）：核心规则测试（Stack）+ 网页版（GHC wasm，web/，唯一前端）。
# 不带目标时显示帮助（按分组列出）。兼容 GNU make 3.81+（macOS 自带版本即可）；配方只用 POSIX sh。
# verify / test-native 只是 stack test 的薄包装，日常直接用 stack 也完全一样。
#
# 常用变量（命令行覆盖，例如 `make serve PORT=9000 BIND=127.0.0.1`）：
#   PORT / BIND        serve 与 deploy-* 的端口 / 监听地址
#   DEST               deploy-* 的目标目录（Mac 上）
#   TGZ                pack / deploy-install 的包路径
#   GHC_WASM_PREFIX    ghc-wasm-meta 安装目录；FLAVOUR 工具链版本系列
#   NODE / CHROME      一致性测试与 e2e 用的 node、Chrome
#   SHOTS              e2e 截图目录；STEPS / CASES 一致性测试的步数 / 「关卡:种子」列表
#   E2E_PORT           e2e 临时起的 serve.py 端口（默认 8765，只监听 127.0.0.1；与别的任务同机并行时改开，
#                      例如 `make check E2E_PORT=8799`；也可直接 export E2E_PORT）
#   APK_OUT            apk / apk-release / aab 产物复制到的路径（默认 web/android-app/out/）
#   ANDROID_HOME       Android SDK 目录（默认 ~/android-sdk）；ANDROID_SHOTS 安卓网页层检查的截图目录

SHELL := /bin/sh
.DEFAULT_GOAL := help
.NOTPARALLEL:

ROOT            := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
WEB             := $(ROOT)/web
GHC_WASM_PREFIX ?= $(HOME)/.ghc-wasm
FLAVOUR         ?= 9.14
PORT            ?= 8080
BIND            ?= 0.0.0.0
DEST            ?= /Users/yubin/Documents/dev/haskell/match3-web
TGZ             ?= $(WEB)/match3-web-dist.tgz
NODE            ?= $(GHC_WASM_PREFIX)/nodejs/bin/node
CHROME          ?= /usr/bin/google-chrome
SHOTS           ?= /workspace/match3-web-shots
E2E_PORT        ?= 8765
STEPS           ?=
CASES           ?=
ANDROID_APP     := $(WEB)/android-app
ANDROID_SHOTS   ?= /workspace/match3-android-shots
BOOTSTRAP_URL   := https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh

export GHC_WASM_PREFIX NODE CHROME PORT BIND CASES E2E_PORT

# 没有 dist 时给出提示并失败（用于不自动构建的目标）
NEED_DIST = @[ -f "$(WEB)/dist/match3-web.wasm" ] || { echo "没有 web/dist，先运行 make build" >&2; exit 1; }
# 原生 stack 不能带着 ~/.ghc-wasm/env 的编译器变量
NATIVE_ENV = env -u CC -u CXX -u AR -u LD -u RANLIB -u NM -u STRIP

.PHONY: android-sync apk apk-release aab android-check
.PHONY: help verify test-native build atlas serve parity anim-parity e2e test check lint-sh size pack \
        deploy-install deploy-start deploy-stop deploy-status clean toolchain doctor

##@ 通用

help: ## 显示本帮助（默认目标）
	@echo "用法：make <目标> [变量=值]（在仓库根目录运行）"
	@awk 'BEGIN { FS = ":[^#]*## " } /^##@ / { printf "\n%s\n", substr($$0, 5); next } /^[a-z][a-z0-9-]*:.*## / { printf "  %-16s %s\n", $$1, $$2 }' "$(ROOT)/Makefile"
	@echo
	@echo "变量：PORT=$(PORT) BIND=$(BIND) DEST=$(DEST)"
	@echo "      GHC_WASM_PREFIX=$(GHC_WASM_PREFIX) FLAVOUR=$(FLAVOUR) SHOTS=$(SHOTS) E2E_PORT=$(E2E_PORT)"

verify: ## 合 main 前的验收：stack test（最后一行写通过 / 失败、测试数与用时）
	@command -v stack >/dev/null 2>&1 || { echo "找不到 stack：见 https://docs.haskellstack.org/（make doctor）" >&2; exit 1; }
	"$(ROOT)/tools/verify.sh"

##@ 原生核心（Stack）

test-native: ## 核心规则测试（stack test；原生 GHC 编译核心 src/ 与 app/pure）
	@command -v stack >/dev/null 2>&1 || { echo "找不到 stack：见 https://docs.haskellstack.org/（make doctor）" >&2; exit 1; }
	cd "$(ROOT)" && $(NATIVE_ENV) stack test

##@ 网页版：构建与运行（web/，GHC wasm）

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

##@ 网页版：测试

parity: ## 状态一致性：原生 Parity.hs 与 wasm 每步 JSON 逐字节相同（STEPS、CASES 可改）
	$(NEED_DIST)
	"$(WEB)/test/parity.sh" state $(STEPS)

anim-parity: ## 动画一致性：原生 ComboFx 与 wasm 逐帧 JSON 逐字节相同（STEPS、CASES 可改）
	$(NEED_DIST)
	"$(WEB)/test/parity.sh" anim $(STEPS)

e2e: ## 无头 Chrome 端到端测试，截图与 report.json 写到 SHOTS（服务器端口 E2E_PORT，默认 8765）
	$(NEED_DIST)
	@[ -x "$(CHROME)" ] || { echo "找不到 Chrome：$(CHROME)；设 CHROME=/path/to/chromium" >&2; exit 1; }
	E2E_PORT="$(E2E_PORT)" NODE_PATH="$(dir $(NODE))../lib/node_modules" "$(NODE)" "$(WEB)/test/e2e.mjs" "$(SHOTS)"

test: test-native parity anim-parity e2e ## 全部测试（stack test + 网页两组一致性 + e2e）
	@echo "== 全部测试通过"

lint-sh: ## shell 脚本 / Makefile 检查：变量名后紧跟中文等非 ASCII 字符（macOS bash 3.2 会读错变量名）须写 ${VAR}
	python3 "$(WEB)/tools/lint-sh.py"

check: ## CI 用：lint-sh + 构建网页版后跑全部测试并报告体积
	$(MAKE) -f "$(ROOT)/Makefile" lint-sh
	$(MAKE) -f "$(ROOT)/Makefile" build
	$(MAKE) -f "$(ROOT)/Makefile" test
	$(MAKE) -f "$(ROOT)/Makefile" size

size: ## 体积报告：wasm 原始 / 优化后、dist 各文件与合计（含 gzip -9）
	$(NEED_DIST)
	@"$(WEB)/tools/size.sh"

##@ 网页版：打包与部署（deploy-start/stop 仅 macOS）

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

##@ 网页版：清理

clean: ## 只清网页版：web/dist、web/dist-newstyle、web/.cache、web/*.tgz；不碰 ~/.ghc-wasm 与 .stack-work
	rm -rf "$(WEB)/dist" "$(WEB)/dist-newstyle" "$(WEB)/.cache"
	rm -f "$(WEB)"/*.tgz

##@ 安卓（Capacitor WebView 包装网页版，web/android-app/，见 docs/android.md）

android-sync: ## 把 web/dist 复制进安卓工程并 cap sync（不打包；需 Node ≥ 22）
	$(NEED_DIST)
	"$(ANDROID_APP)/build-apk.sh" sync

apk: ## 构建调试版 APK（build-apk.sh；需 JDK 21 + Android SDK）；APK_OUT= 可改输出路径
	$(NEED_DIST)
	"$(ANDROID_APP)/build-apk.sh" debug

apk-release: ## 构建正式版 APK（有 android/keystore.properties 才签名，否则产出未签名包）
	$(NEED_DIST)
	"$(ANDROID_APP)/build-apk.sh" release

aab: ## 构建正式版 AAB（Google Play 上传用，需签名配置）
	$(NEED_DIST)
	"$(ANDROID_APP)/build-apk.sh" aab

android-check: ## 无模拟器时的替代验证：桌面 Chrome 手机视口跑打进 APK 的页面（wasm MIME 兜底、触摸、返回键）
	@[ -f "$(ANDROID_APP)/www/android-shim.js" ] || { echo "没有 web/android-app/www：先 make android-sync" >&2; exit 1; }
	NODE_PATH="$(dir $(NODE))../lib/node_modules" "$(NODE)" "$(ANDROID_APP)/test/check-www.mjs" "$(ANDROID_SHOTS)"

##@ 环境（前置检查与网页版工具链）

toolchain: ## 安装或校验 ghc-wasm-meta（FLAVOUR=9.14）；已安装则只校验，FORCE=1 重跑安装
	@if [ -f "$(GHC_WASM_PREFIX)/env" ] && [ -z "$(FORCE)" ]; then \
	  echo "== 校验 $(GHC_WASM_PREFIX)"; \
	  v=$$(. "$(GHC_WASM_PREFIX)/env" >/dev/null 2>&1 && wasm32-wasi-ghc --numeric-version) || { echo "wasm32-wasi-ghc 不能运行：make toolchain FORCE=1" >&2; exit 1; }; \
	  case "$$v" in $(FLAVOUR).*) echo "  wasm32-wasi-ghc $$v ✓（FLAVOUR $(FLAVOUR)）";; \
	    *) echo "  wasm32-wasi-ghc $${v}，与 FLAVOUR=$(FLAVOUR) 不符：make toolchain FORCE=1" >&2; exit 1;; esac; \
	  c=$$(. "$(GHC_WASM_PREFIX)/env" >/dev/null 2>&1 && wasm32-wasi-cabal --numeric-version) || { echo "  缺 wasm32-wasi-cabal" >&2; exit 1; }; \
	  echo "  wasm32-wasi-cabal $$c ✓"; \
	  o=$$(. "$(GHC_WASM_PREFIX)/env" >/dev/null 2>&1 && wasm-opt --version) && echo "  $$o ✓" || echo "  缺 wasm-opt（build 会跳过 -Oz）"; \
	  [ -x "$(GHC_WASM_PREFIX)/nodejs/bin/node" ] && echo "  node $$("$(GHC_WASM_PREFIX)/nodejs/bin/node" --version) ✓" || echo "  缺自带 node"; \
	  echo "工具链可用；首次构建若提示没有 Hackage 索引：sh -c '. $(GHC_WASM_PREFIX)/env && wasm32-wasi-cabal update'"; \
	else \
	  for t in curl jq unzip zstd xz make; do command -v $$t >/dev/null 2>&1 || { echo "安装脚本需要 $${t}：sudo apt-get install -y curl jq unzip zstd xz-utils make" >&2; exit 1; }; done; \
	  echo "== 安装 ghc-wasm-meta FLAVOUR=$(FLAVOUR) 到 $(GHC_WASM_PREFIX)（约 6.4 GB）"; \
	  curl -fL "$(BOOTSTRAP_URL)" | FLAVOUR="$(FLAVOUR)" PREFIX="$(GHC_WASM_PREFIX)" sh; \
	fi

doctor: ## 检查前置工具（ghc-wasm、node、python3/Pillow、stack、wasm-opt、Chrome…）并给出安装提示
	@"$(WEB)/tools/doctor.sh"
