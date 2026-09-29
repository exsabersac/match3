#!/usr/bin/env bash
# 消消乐 Android 构建：web/dist（GHC wasm 网页版）→ Capacitor 同步 → Gradle 打包。
#
# 用法（在任意目录运行均可）：
#   web/android-app/build-apk.sh              # 调试版 APK（默认；需要已有 web/dist）
#   web/android-app/build-apk.sh --web        # 先跑 web/build.sh 重新构建网页版，再打调试版
#   web/android-app/build-apk.sh sync         # 只准备 www/ 并 cap sync（改了网页后同步进 android/，不打包）
#   web/android-app/build-apk.sh release      # 正式版 APK（有 android/keystore.properties 才会签名）
#   web/android-app/build-apk.sh aab          # 正式版 AAB（Google Play 上传用，同样需要签名配置）
#
# 环境变量：
#   ANDROID_HOME   Android SDK 目录（默认 ~/android-sdk；也认 ANDROID_SDK_ROOT）
#   JAVA_HOME      JDK 21（或 17）；不设则用 PATH 里的 java
#   NODE_DIR       Node ≥ 22 的 bin 目录（PATH 里的 node 太旧时用；默认会试 ~/opt/node24/bin）
#   APK_OUT        产物复制到的路径（默认 web/android-app/out/ 下）
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
WEB="$(cd "$HERE/.." && pwd)"
MODE="debug"; BUILD_WEB=0
for a in "$@"; do
  case "$a" in
    debug|release|aab|sync) MODE="$a" ;;
    --web) BUILD_WEB=1 ;;
    -h|--help) sed -n '2,17p' "$0"; exit 0 ;;
    *) echo "未知参数：$a（--help 看用法）" >&2; exit 2 ;;
  esac
done

# ---- 前置检查 ----
export ANDROID_HOME="${ANDROID_HOME:-${ANDROID_SDK_ROOT:-$HOME/android-sdk}}"
[ -d "$ANDROID_HOME/platforms" ] || { echo "找不到 Android SDK：$ANDROID_HOME（设 ANDROID_HOME，安装见 docs/android.md）" >&2; exit 1; }

node_major() { node -p 'process.versions.node.split(".")[0]' 2>/dev/null || echo 0; }
if [ -n "${NODE_DIR:-}" ]; then export PATH="$NODE_DIR:$PATH"; fi
if [ "$(node_major)" -lt 22 ] && [ -x "$HOME/opt/node24/bin/node" ]; then export PATH="$HOME/opt/node24/bin:$PATH"; fi
[ "$(node_major)" -ge 22 ] || { echo "需要 Node ≥ 22（Capacitor 8 的要求），当前 $(node -v 2>/dev/null || echo 无)；设 NODE_DIR" >&2; exit 1; }

if [ "$MODE" != "sync" ]; then
  if [ -n "${JAVA_HOME:-}" ]; then export PATH="$JAVA_HOME/bin:$PATH"; fi
  jv=$(java -version 2>&1 | sed -n 's/.*version "\([0-9]*\).*/\1/p' | head -1)
  [ "${jv:-0}" -ge 17 ] || { echo "需要 JDK 21（或 17），当前：${jv:-无}；设 JAVA_HOME" >&2; exit 1; }
fi

# ---- 1. 网页版产物 ----
if [ "$BUILD_WEB" = 1 ]; then "$WEB/build.sh"; fi
[ -f "$WEB/dist/match3-web.wasm" ] || { echo "没有 web/dist：先 make build（或加 --web）" >&2; exit 1; }

# ---- 2. npm 依赖（版本锁在 package-lock.json）→ www/ → cap sync ----
cd "$HERE"
if [ ! -d node_modules/@capacitor/cli ]; then npm ci --no-fund --no-audit; fi
node prepare-www.mjs "$WEB/dist"
npx cap sync android

[ "$MODE" = "sync" ] && { echo "已同步到 android/（未打包）"; exit 0; }

# ---- 3. Gradle ----
cd "$HERE/android"
# local.properties 不提交；没有时按 ANDROID_HOME 生成
[ -f local.properties ] || echo "sdk.dir=$ANDROID_HOME" > local.properties
case "$MODE" in
  debug)   task=assembleDebug;   src=app/build/outputs/apk/debug/app-debug.apk ;;
  release) task=assembleRelease; src=app/build/outputs/apk/release/app-release.apk ;;
  aab)     task=bundleRelease;   src=app/build/outputs/bundle/release/app-release.aab ;;
esac
./gradlew --no-daemon "$task"
if [ "$MODE" = release ] && [ ! -f "$src" ]; then src=app/build/outputs/apk/release/app-release-unsigned.apk; fi
[ -f "$src" ] || { echo "Gradle 成功但没找到产物 $src" >&2; exit 1; }

ext="${src##*.}"
out="${APK_OUT:-$HERE/out/match3-$MODE.$ext}"
mkdir -p "$(dirname "$out")"
cp "$src" "$out"
echo "产物：$out（$(stat -c %s "$out" 2>/dev/null || stat -f %z "$out") 字节）"
case "$src" in *unsigned*) echo "注意：没有 android/keystore.properties，release APK 未签名，不能直接安装（见 docs/android.md）" ;; esac
