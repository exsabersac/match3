#!/usr/bin/env bash
# 网页版打包与 macOS 部署（静态文件 + serve.py，目标机只需系统自带的 python3）。
#
# 在构建机（有 ghc-wasm 工具链，先跑过 ./build.sh）上：
#   web/deploy-mac.sh pack                 # → web/match3-web-dist.tgz（dist/ + serve.py + serve.sh + 本脚本）
# 把 tgz 拷到 Mac（scp / AirDrop / 网盘均可），在 Mac 上：
#   bash deploy-mac.sh install match3-web-dist.tgz [目标目录]   # 解包到目标目录（默认见 TARGET）
#   bash deploy-mac.sh run    [目标目录]    # 前台运行（exec，Ctrl-C 停止）——最稳妥
#   bash deploy-mac.sh start  [目标目录]    # 后台常驻：用户级 launchd 代理（登录后自动起、崩溃自动拉起）
#   bash deploy-mac.sh stop | status        # 停止后台代理 / 查看端口占用（lsof -i :$PORT）
#
# 环境变量：PORT（默认 8080）、BIND（默认 0.0.0.0）、TARGET（默认 /Users/yubin/Documents/dev/haskell/match3-web）
#
# 注意：不要在一次性的远程会话里用 `nohup ... &` 起服务——会话结束时整组进程会被一起杀掉，
# 服务器看起来「启动成功」随即消失。要么前台 run（放在自己开的终端 / tmux 里），要么 start 交给 launchd。
# 注意：变量后面紧跟中文全角字符时一律写成 ${VAR}——macOS 自带 bash 3.2 在 UTF-8 区域设置下会把全角字符的字节
# 当成变量名的一部分（`$LABEL）` 会被读成变量 `LABEL\xef…`），配合 set -u 直接报 unbound variable 退出。
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
PORT="${PORT:-8080}"
BIND="${BIND:-0.0.0.0}"
DEFAULT_TARGET="${TARGET:-/Users/yubin/Documents/dev/haskell/match3-web}"
LABEL="com.match3.web"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"

die() { echo "错误：$*" >&2; exit 1; }
need_mac() { [ "$(uname -s)" = "Darwin" ] || die "$1 只能在 macOS 上用（launchd）；其他系统请用 run"; }

cmd="${1:-}"; [ -n "$cmd" ] || { sed -n '2,16p' "$0"; exit 1; }
shift

case "$cmd" in
  pack)
    [ -f "$HERE/dist/index.html" ] || die "没有 $HERE/dist，先运行 web/build.sh"
    out="${1:-$HERE/match3-web-dist.tgz}"
    stage="$(mktemp -d)"; trap 'rm -rf "$stage"' EXIT
    mkdir -p "$stage/match3-web"
    cp -R "$HERE/dist" "$stage/match3-web/dist"
    cp "$HERE/serve.py" "$HERE/serve.sh" "$HERE/deploy-mac.sh" "$stage/match3-web/"
    # COPYFILE_DISABLE：在 Mac 上打包时不带 ._ 资源叉文件
    COPYFILE_DISABLE=1 tar -czf "$out" -C "$stage" match3-web
    echo "已打包：${out}（$(wc -c < "$out" | tr -d ' ') 字节）"
    ;;
  install)
    tgz="${1:-}"; [ -f "$tgz" ] || die "用法：deploy-mac.sh install match3-web-dist.tgz [目标目录]"
    target="${2:-$DEFAULT_TARGET}"
    mkdir -p "$target"
    tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
    tar -xzf "$tgz" -C "$tmp"
    [ -f "$tmp/match3-web/dist/index.html" ] || die "tgz 里没有 match3-web/dist/index.html"
    rm -rf "$target/dist"                       # 只替换 dist/，目标目录里的其他文件不动
    cp -R "$tmp/match3-web/dist" "$target/dist"
    cp "$tmp/match3-web/serve.py" "$tmp/match3-web/serve.sh" "$tmp/match3-web/deploy-mac.sh" "$target/"
    chmod +x "$target/serve.py" "$target/serve.sh" "$target/deploy-mac.sh"
    echo "已安装到 $target"
    echo "前台运行：bash \"$target/deploy-mac.sh\" run \"$target\""
    ;;
  run)
    target="${1:-$DEFAULT_TARGET}"
    [ -f "$target/dist/index.html" ] || die "$target/dist 不存在，先 install"
    exec python3 "$target/serve.py" --dir "$target/dist" --port "$PORT" --bind "$BIND"
    ;;
  start)
    need_mac start
    target="${1:-$DEFAULT_TARGET}"
    [ -f "$target/dist/index.html" ] || die "$target/dist 不存在，先 install"
    py="$(command -v python3)" || die "找不到 python3（装 Xcode 命令行工具：xcode-select --install）"
    mkdir -p "$HOME/Library/LaunchAgents"
    cat > "$PLIST" <<PL
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key><array>
    <string>$py</string><string>$target/serve.py</string>
    <string>--dir</string><string>$target/dist</string>
    <string>--port</string><string>$PORT</string>
    <string>--bind</string><string>$BIND</string>
    <string>--quiet</string>
  </array>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>StandardOutPath</key><string>$target/serve.log</string>
  <key>StandardErrorPath</key><string>$target/serve.log</string>
</dict></plist>
PL
    launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null || true
    launchctl bootstrap "gui/$(id -u)" "$PLIST"
    sleep 1
    echo "已交给 launchd（${LABEL}），日志 ${target}/serve.log："
    tail -n 5 "$target/serve.log" 2>/dev/null || true
    lsof -nP -iTCP:"$PORT" -sTCP:LISTEN || echo "（端口 $PORT 暂未监听，看日志）"
    ;;
  stop)
    need_mac stop
    launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null && echo "已停止 $LABEL" || echo "$LABEL 没在运行"
    if [ "${1:-}" = "--remove" ]; then rm -f "$PLIST"; echo "已删除 $PLIST"; fi
    ;;
  status)
    if [ "$(uname -s)" = "Darwin" ]; then
      launchctl print "gui/$(id -u)/$LABEL" 2>/dev/null | grep -E '^\s*(state|pid|last exit code)' || echo "launchd 代理 $LABEL 未加载"
    fi
    if command -v lsof >/dev/null; then
      lsof -nP -iTCP:"$PORT" -sTCP:LISTEN || echo "端口 $PORT 没有进程在监听"
    else
      echo "（没有 lsof，跳过端口监听检查）"
    fi
    if code="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$PORT/")"; then
      echo "本机 http://127.0.0.1:$PORT/ → HTTP $code"
    else
      echo "本机 http://127.0.0.1:$PORT/ 无响应（服务器没在跑？用 run 或 start 启动）"
    fi
    ;;
  *) die "未知子命令 ${cmd}（pack / install / run / start / stop / status）" ;;
esac
