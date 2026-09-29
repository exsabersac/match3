#!/usr/bin/env bash
# serve.py 的薄包装：./serve.sh [--port 8080] [--bind 0.0.0.0] [--dir DIR] [--quiet]
exec python3 "$(cd "$(dirname "$0")" && pwd)/serve.py" "$@"
