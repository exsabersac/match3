#!/usr/bin/env python3
"""shell 脚本 / Makefile 的「变量名后紧跟非 ASCII 字节」检查（make lint-sh，make check 第一步）。

背景：macOS 自带的 bash 3.2 在 UTF-8 区域设置下，libc 把 0x80–0xFF 字节当成字母，于是
`echo "…$LABEL）"` 里全角括号的首字节会被读进变量名（变量 `LABEL\\xef…`），配合 set -u 直接
报 unbound variable 退出（Linux 的 bash 5 / glibc 不会，所以本机测不出来）。修法：写成 `${LABEL}）`。

规则（简单且严格，注释行也查，不设白名单）：`$` 后接命名变量 `[A-Za-z_][A-Za-z0-9_]*`，紧跟一个 >= 0x80
的字节即报错；位置参数 / 特殊参数（$1、$?、$@ 等）不查。Makefile 里的 `$$v，` 是配方 shell 的 `$v，`，同样报。

用法：
  python3 web/tools/lint-sh.py              # 查仓库里（git ls-files）全部 *.sh、*.mk 与 Makefile
  python3 web/tools/lint-sh.py 文件 ...      # 只查给定文件
按文件名选文件，不看 shebang：Gradle 生成的 web/android-app/android/gradlew 不是手写脚本（每次升级 Gradle
包装器会重新生成），不在检查范围内。
发现问题时逐条打印「文件:行号: 变量名: 该行」并以 1 退出；全部干净时打印一行汇总并以 0 退出。
"""
import os
import re
import subprocess
import sys

BAD = re.compile(rb"\$([A-Za-z_][A-Za-z0-9_]*)(?=[\x80-\xff])")
SKIP_DIRS = {".git", "node_modules", ".stack-work", "dist-newstyle", "dist", ".cache", "build", ".gradle"}


def is_script(path):
    name = os.path.basename(path)
    return name == "Makefile" or name.endswith((".sh", ".mk"))


def repo_files(root):
    """优先用 git ls-files（只查受版本控制的文件）；不在 git 仓库里时退回遍历目录。"""
    try:
        out = subprocess.run(["git", "-C", root, "ls-files", "-z"], capture_output=True, check=True).stdout
        return [os.path.join(root, p) for p in out.decode("utf-8", "surrogateescape").split("\0") if p]
    except (OSError, subprocess.CalledProcessError):
        files = []
        for d, subdirs, names in os.walk(root):
            subdirs[:] = [s for s in subdirs if s not in SKIP_DIRS]
            files += [os.path.join(d, n) for n in names]
        return files


def main(argv):
    root = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
    if argv:
        files = argv
    else:
        files = sorted(p for p in repo_files(root) if os.path.isfile(p) and not os.path.islink(p) and is_script(p))
    bad = 0
    for path in files:
        with open(path, "rb") as f:
            for no, line in enumerate(f, 1):
                for m in BAD.finditer(line):
                    shown = os.path.relpath(path, root) if not argv else path
                    text = line.rstrip(b"\r\n").decode("utf-8", "replace").strip()
                    name = m.group(1).decode()
                    fix = f"${{{name}}}"
                    if m.start() > 0 and line[m.start() - 1:m.start()] == b"$":
                        fix = "$" + fix  # Makefile 配方里的 $$v → $${v}
                    print(f"{shown}:{no}: ${name} 后紧跟非 ASCII 字符（改成 {fix}）: {text}")
                    bad += 1
    if bad:
        print(f"lint-sh：{bad} 处变量名后紧跟非 ASCII 字节（macOS bash 3.2 + UTF-8 下会读错变量名），请加花括号", file=sys.stderr)
        return 1
    print(f"lint-sh：检查 {len(files)} 个 shell 脚本 / Makefile，0 处问题")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
