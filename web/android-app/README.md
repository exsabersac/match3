# web/android-app：消消乐安卓壳（Capacitor）

把 `web/dist`（GHC wasm 网页版）装进 Android WebView。完整说明见 [`docs/android.md`](../../docs/android.md)。

```sh
make build apk        # 仓库根目录：构建网页版 + 调试版 APK → web/android-app/out/match3-debug.apk
./build-apk.sh --help # 或直接用脚本：debug / release / aab / sync，--web 先重建网页版
```
