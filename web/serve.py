#!/usr/bin/env python3
"""网页版本地静态服务器（只用 Python 3 标准库，3.7+）。

用法：
  python3 web/serve.py                    # 默认目录 web/dist，0.0.0.0:8080
  python3 web/serve.py --port 9000 --bind 127.0.0.1
  python3 web/serve.py --dir /path/to/dist --quiet

- 正确的 Content-Type：.wasm → application/wasm（instantiateStreaming 必需）、.webp、.json、.js/.mjs；
- 开发期对 HTML / wasm / JS / JSON 发 Cache-Control: no-cache，改完刷新即生效；图片允许短缓存；
- 启动时列出本机和局域网地址（手机连同一 Wi-Fi 打开即可）；Ctrl-C / SIGTERM 干净退出。
"""
import argparse
import http.server
import os
import re
import signal
import socket
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

TYPES = {
    ".wasm": "application/wasm",
    ".webp": "image/webp",
    ".json": "application/json",
    ".js": "text/javascript",
    ".mjs": "text/javascript",
    ".html": "text/html; charset=utf-8",
    ".css": "text/css",
    ".png": "image/png",
    ".svg": "image/svg+xml",
    ".txt": "text/plain; charset=utf-8",
}
NO_CACHE = {".html", ".wasm", ".js", ".mjs", ".json"}


class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = dict(http.server.SimpleHTTPRequestHandler.extensions_map, **TYPES)
    quiet = False

    def guess_type(self, path):
        ext = os.path.splitext(path)[1].lower()
        return TYPES.get(ext) or super().guess_type(path)

    def end_headers(self):
        path = self.path.split("?", 1)[0].split("#", 1)[0]
        ext = os.path.splitext(path)[1].lower()
        if path.endswith("/") or ext in NO_CACHE:
            self.send_header("Cache-Control", "no-cache")
        else:
            self.send_header("Cache-Control", "max-age=300")
        super().end_headers()

    def log_message(self, fmt, *args):
        if not self.quiet:
            sys.stderr.write("%s - %s\n" % (self.address_string(), fmt % args))


def local_ipv4s():
    """尽量找出本机的局域网 IPv4 地址（多种办法取并集，全部失败就返回空表）。"""
    ips = []

    def add(ip):
        if ip and not ip.startswith("127.") and not ip.startswith("169.254.") and ip not in ips:
            ips.append(ip)

    # 1) 默认路由出口地址（UDP connect 不发包）
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.connect(("192.0.2.1", 80))
        add(s.getsockname()[0])
        s.close()
    except OSError:
        pass
    # 2) 网卡列表：Linux 用 ip，macOS 用 ifconfig
    for cmd in (["ip", "-4", "-o", "addr", "show"], ["ifconfig"]):
        try:
            out = subprocess.run(cmd, capture_output=True, text=True, timeout=2).stdout
        except (OSError, subprocess.SubprocessError):
            continue
        for m in re.finditer(r"\binet (?:addr:)?(\d+\.\d+\.\d+\.\d+)", out):
            add(m.group(1))
        if ips:
            break
    # 3) 主机名解析
    try:
        for info in socket.getaddrinfo(socket.gethostname(), None, socket.AF_INET):
            add(info[4][0])
    except OSError:
        pass
    return ips


def main():
    ap = argparse.ArgumentParser(description="match3 网页版本地静态服务器")
    ap.add_argument("--dir", default=os.path.join(HERE, "dist"), help="站点目录（默认 web/dist）")
    ap.add_argument("--port", type=int, default=8080, help="端口（默认 8080）")
    ap.add_argument("--bind", default="0.0.0.0", help="监听地址（默认 0.0.0.0；只给本机用填 127.0.0.1）")
    ap.add_argument("--quiet", action="store_true", help="不打印每个请求")
    a = ap.parse_args()

    root = os.path.abspath(a.dir)
    if not os.path.isfile(os.path.join(root, "index.html")):
        sys.exit("找不到 %s/index.html：先运行 web/build.sh，或用 --dir 指定站点目录" % root)
    if not os.path.isfile(os.path.join(root, "match3-web.wasm")):
        print("警告：%s 里没有 match3-web.wasm，页面会加载失败" % root, file=sys.stderr)

    Handler.quiet = a.quiet

    def handler(*args, **kw):
        return Handler(*args, directory=root, **kw)

    http.server.ThreadingHTTPServer.daemon_threads = True
    try:
        httpd = http.server.ThreadingHTTPServer((a.bind, a.port), handler)
    except OSError as e:
        sys.exit("无法监听 %s:%d（%s）。端口被占用时可用 `lsof -i :%d` 查看占用进程，或换 --port"
                 % (a.bind, a.port, e.strerror or e, a.port))

    port = httpd.server_address[1]
    print("站点目录：%s" % root)
    print("本机访问：http://127.0.0.1:%d/?level=0" % port)
    if a.bind in ("0.0.0.0", ""):
        ips = local_ipv4s()
        for ip in ips:
            print("局域网：  http://%s:%d/?level=0" % (ip, port))
        if not ips:
            print("（没找到局域网地址；手机访问请用本机 IP）")
    elif a.bind not in ("127.0.0.1", "localhost"):
        print("监听地址：http://%s:%d/" % (a.bind, port))
    print("Ctrl-C 退出", flush=True)

    # SIGTERM（kill / launchctl stop）与 Ctrl-C 走同一条退出路径
    def on_term(signum, frame):
        raise KeyboardInterrupt
    signal.signal(signal.SIGTERM, on_term)
    signal.signal(signal.SIGINT, on_term)   # 显式装上：后台 / 非交互启动时 SIGINT 可能被继承为忽略
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\n已停止", flush=True)
    finally:
        httpd.server_close()


if __name__ == "__main__":
    main()
