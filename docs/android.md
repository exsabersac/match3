# 安卓版（Capacitor 包装网页版）

> 最初在分支 `android-capacitor` 上开发，已合入 main。网页版本身见 [`web.md`](web.md)；本文讲怎么把它装进 Android 应用。

## 1. 思路

不写原生游戏代码：把网页版产物 `web/dist`（Haskell 核心编成的 `match3-web.wasm` + Canvas 2D 渲染的 JS + 图集）
原样放进 [Capacitor](https://capacitorjs.com/) 工程，由系统 WebView（Android System WebView / Chrome）加载。

```
web/build.sh ──► web/dist ──prepare-www.mjs──► web/android-app/www ──cap sync──► android/app/src/main/assets/public
                              （复制 + 注入 android-shim.js）                         │
                                                                                    ▼
                           MainActivity（BridgeActivity）→ WebView 打开 https://localhost/ → Gradle 打成 APK / AAB
```

- **全部离线**：规则、动画时间线都在 wasm 里，页面不发任何网络请求；资源由 Capacitor 的本地服务器从 APK 的 assets 里读出；
- **wasm 的 MIME**：Capacitor 8 的 `WebViewLocalServer` 对 `.wasm` 返回 `application/wasm`，所以 `main.js` 里的
  `WebAssembly.instantiateStreaming` 直接可用；`android-shim.js` 另外包了一层兜底——流式编译失败（MIME 不对、旧 WebView）
  时自动改用 `fetch → arrayBuffer → WebAssembly.instantiate`，`main.js` 不用改；
- **网页代码零改动**：安卓专用的逻辑全在 `web/android-app/shim/android-shim.js`，只在打包时插进 `index.html`
  （浏览器里跑网页版时不加载它）。

### 1.1 目录

```
web/android-app/
├── package.json / package-lock.json   Capacitor 版本锁定（@capacitor/core、cli、android 8.5.2，@capacitor/app 8.1.1）
├── capacitor.config.json              appId io.github.exsabersac.match3、appName 消消乐、webDir www、https scheme、系统栏配置
├── prepare-www.mjs                    web/dist → www/（复制 + 注入 android-shim.js + 改标题）
├── shim/android-shim.js               wasm 流式实例化兜底、返回键（确认退出）、禁止多指缩放
├── build-apk.sh                       一键构建：debug（默认）/ release / aab / sync，--web 先重建网页版
├── tools/gen_icons.py                 从网页图集的宝石精灵生成启动图标与启动画面图（结果已提交）
├── test/check-www.mjs                 没有模拟器时的替代验证（桌面 Chrome 手机视口，见 §6）
├── www/                               （生成，不提交）
├── out/                               （build-apk.sh 产物，不提交）
└── android/                           `npx cap add android` 生成的原生工程（按 Capacitor 惯例提交）
    ├── app/src/main/AndroidManifest.xml    竖屏锁定、单实例
    ├── app/src/main/java/.../MainActivity.java   沉浸式全屏、关闭 WebView 缩放、固定文字缩放
    ├── app/src/main/res/                  图标（mipmap-*）、启动画面（drawable/splash.xml）、深色主题（values/）
    ├── app/build.gradle                   版本号、release 签名（读 keystore.properties）
    └── keystore.properties.example        签名配置模板（真正的 keystore.properties 不提交）
```

`.gitignore`：`node_modules/`、`www/`、`out/`，以及 Capacitor 模板自带的 `build/`、`.gradle/`、`local.properties`、
`app/src/main/assets/public`、生成的 `capacitor.config.json` 副本；另加 `keystore.properties`、`*.jks`、`*.keystore`。

## 2. 前置

| 工具 | 版本 | 说明 |
| --- | --- | --- |
| Node.js | ≥ 22（用 24 LTS） | Capacitor 8 CLI 的要求。PATH 里的 node 太旧时，`build-apk.sh` 会试 `~/opt/node24/bin`，或设 `NODE_DIR` |
| JDK | 21（17 也可） | Gradle 8.14 / AGP 8.13；`JAVA_HOME` 可指定 |
| Android SDK | platform-tools、`platforms;android-36`、`build-tools;36.0.0`（AGP 默认还会用 35.0.0） | 默认 `~/android-sdk`，`ANDROID_HOME` 可改 |
| 网页版工具链 | ghc-wasm-meta 9.14 | 只在要重建 `web/dist` 时需要（见 `web.md` §3） |

Linux（Debian / Ubuntu）上的安装步骤（box 上就是这样装的）：

```sh
sudo apt-get install -y openjdk-21-jdk-headless unzip

# Node 24 LTS（不动系统自带的 node）
curl -fLO https://nodejs.org/dist/v24.21.0/node-v24.21.0-linux-x64.tar.xz
mkdir -p ~/opt && tar xJf node-v24.21.0-linux-x64.tar.xz -C ~/opt && ln -sfn ~/opt/node-v24.21.0-linux-x64 ~/opt/node24

# Android 命令行工具（版本号见 https://developer.android.com/studio#command-line-tools-only）
mkdir -p ~/android-sdk/cmdline-tools
curl -fLo /tmp/clt.zip https://dl.google.com/android/repository/commandlinetools-linux-15859902_latest.zip
unzip -q /tmp/clt.zip -d ~/android-sdk/cmdline-tools && mv ~/android-sdk/cmdline-tools/cmdline-tools ~/android-sdk/cmdline-tools/latest
export ANDROID_HOME=~/android-sdk
yes | ~/android-sdk/cmdline-tools/latest/bin/sdkmanager --licenses          # 非交互接受许可
~/android-sdk/cmdline-tools/latest/bin/sdkmanager "platform-tools" "platforms;android-36" "build-tools;36.0.0" "build-tools;35.0.0"
# 可选：模拟器 + 系统镜像（约 4.5 GB）
~/android-sdk/cmdline-tools/latest/bin/sdkmanager "emulator" "system-images;android-34;google_apis;x86_64"
```

macOS 上直接装 Android Studio（自带 JDK 与 SDK 管理器）最省事；命令行构建时把 `ANDROID_HOME` 指到
`~/Library/Android/sdk`，`JAVA_HOME` 指到 Android Studio 自带的 JBR（`/Applications/Android Studio.app/Contents/jbr/Contents/Home`）。

磁盘占用（box 实测）：Android SDK 5.6 GB（其中 API 34 x86_64 系统镜像 4.2 GB，不跑模拟器可以不装），
Gradle 缓存约 1.1 GB，JDK 21 约 290 MB，Node 24 约 210 MB，`web/android-app/node_modules` 约 70 MB。
不装模拟器时总共约 3 GB。

> **box 上的网络特例**：box 到 `dl.google.com` 的 TLS 握手会卡住，但内容相同的 `dl-ssl.google.com` 正常。
> 所以 box 上 `sdkmanager` 要带 `SDK_TEST_BASE_URL=https://dl-ssl.google.com/android/repository/`，
> 并在 `~/.gradle/init.d/google-maven-dl-ssl.gradle` 放了一个 init 脚本，把 Gradle 的 `google()` 仓库改走 dl-ssl。
> 这两样都只在 box 本机，不在仓库里；网络正常的机器不需要。偶尔 Gradle 缓存了一次失败的下载（报 `Could not find …`），
> 在 `web/android-app/android` 下跑一次 `./gradlew assembleDebug --refresh-dependencies` 即可。

## 3. 构建

仓库根目录：

```sh
make build                       # 先构建网页版 web/dist（已有可跳过）
make apk                         # 调试版 APK → web/android-app/out/match3-debug.apk
make apk APK_OUT=/workspace/match3-android-debug.apk   # 指定输出路径
make android-sync                # 只把 dist 同步进 android/（在 Android Studio 里调试前用）
make apk-release                 # 正式版 APK（需要签名配置，见 §5）
make aab                         # 正式版 AAB（Google Play）
make android-check               # 替代验证：桌面 Chrome 手机视口跑打进 APK 的页面（§6）
```

这些目标都只是 `web/android-app/build-apk.sh` 的薄包装，也可以直接跑：

```sh
web/android-app/build-apk.sh               # = make apk
web/android-app/build-apk.sh --web         # 先 web/build.sh 重建网页版再打包
web/android-app/build-apk.sh sync | release | aab
```

脚本做的事：检查 SDK / JDK / Node → （`npm ci`，只在没有 `node_modules` 时）→ `node prepare-www.mjs` → `npx cap sync android`
→ 没有 `android/local.properties` 时按 `ANDROID_HOME` 生成 → `./gradlew assembleDebug`（或 `assembleRelease` / `bundleRelease`）
→ 把产物复制到 `out/`（或 `APK_OUT`）。首次构建要下载 Gradle 与依赖（几分钟），之后增量约 15 秒。

产物大小（box 实测，2026-10-03 fix/web-audio-toggle）：调试版 APK 约 6.2 MB（6,207,832 B），内含未压缩 wasm 2.18 MB（2,183,488 B，打包时的版本；当前 `web/dist` 的 wasm 是 2,212,226 B，APK 下次构建时一起重测）；正式版 APK 约 4.6 MB（不含调试信息；上次实测，本次未重测）。

用 Android Studio：`make android-sync` 后打开 `web/android-app/android` 目录即可（或 `cd web/android-app && npx cap open android`）。

## 4. 装到手机上

**方式一：直接装 APK（最简单）**

1. 把 `match3-debug.apk` 传到手机（微信 / 网盘 / USB 都行）；
2. 在手机上点开它，系统会提示「禁止安装未知来源应用」→ 按提示给当前的文件管理器 / 浏览器打开
   「允许安装未知应用」（设置 → 应用 → 特殊应用权限 → 安装未知应用，各厂商叫法略有不同）；
3. 安装后桌面出现「消消乐」图标。

**方式二：adb**

1. 手机开「开发者选项」（设置 → 关于手机 → 连点「版本号」7 次），打开「USB 调试」；
2. USB 连电脑，手机上允许调试；
3. `adb install -r web/android-app/out/match3-debug.apk`（`-r` 覆盖安装，保留数据）。

调试：手机连着电脑时，桌面 Chrome 打开 `chrome://inspect` 可以检查调试版里的 WebView（控制台里 `m3debug.*` 照常可用）；
日志用 `adb logcat -s Capacitor:* chromium:*`。

调试版用 SDK 自动生成的调试密钥签名，**不能**和正式版互相覆盖安装（签名不同，要先卸载）。

## 5. 正式版、签名与 Google Play

**生成密钥库（只做一次，妥善保管，丢了就无法再更新同一个应用）：**

```sh
keytool -genkeypair -v -keystore ~/keys/match3-release.jks -alias match3 -keyalg RSA -keysize 4096 -validity 10000
```

**配置签名**：复制 `web/android-app/android/keystore.properties.example` 为 `keystore.properties`（同目录，已被 gitignore），填入：

```properties
storeFile=/home/你/keys/match3-release.jks     # 绝对路径，或相对 web/android-app/android/ 的路径
storePassword=…
keyAlias=match3
keyPassword=…
```

密钥库和 `keystore.properties` **绝不提交**（`.gitignore` 已忽略 `keystore.properties`、`*.jks`、`*.keystore`）；
CI 上可以在构建前从机密变量写出这个文件。没有这个文件时 `make apk-release` 仍能构建，但产出的是
`app-release-unsigned.apk`，不能直接安装。

**发版前**：在 `android/app/build.gradle` 里递增 `versionCode`（每次上传商店必须变大）并改 `versionName`。

**Google Play**：Play 只收 AAB。`make aab` → `web/android-app/out/match3-aab.aab`，在 Play Console 上传即可；
首次上传时建议开启「Play 应用签名」（Google 保管应用签名密钥，你本地的密钥库作为上传密钥）。
上架还要准备隐私政策（本应用不收集任何数据、不联网）、商店截图与图标（512×512，可用 `tools/gen_icons.py` 的思路放大生成）。
国内应用商店一般直接收签名好的 APK（`make apk-release`）。

## 6. 验证

**Android 模拟器（box 上未跑通）**：box 有 `/dev/kvm`（`emulator-check accel` 显示 KVM 可用），API 34 google_apis x86_64
镜像和 AVD `match3-api34` 都已装好，但无头启动（`-no-window -gpu swiftshader_indirect`）时 qemu 停在
「Activated packet streamer for bluetooth emulation」之后，vCPU 线程始终没起来、控制台端口不响应，`adb` 一直 `offline`。
试过关蓝牙 / Wi-Fi 包转发 / UWB、设 `XDG_RUNTIME_DIR`、换 `-gpu guest`、断掉模拟器访问 dl.google.com 的连接，都不行。
所以 APK 还没有在模拟器或真机上实际跑过。在有桌面环境的机器上（或装了 Android Studio 的 Mac）可以这样验证：

```sh
emulator -avd match3-api34 -no-snapshot &      # 或 Android Studio 的 Device Manager
adb wait-for-device && adb install -r web/android-app/out/match3-debug.apk
adb shell am start -n io.github.exsabersac.match3/.MainActivity
adb exec-out screencap -p > shot.png
adb logcat -d | grep -iE "capacitor|chromium|wasm"
```

**替代验证（已做）**：

- `aapt dump badging`：包名 `io.github.exsabersac.match3`、versionName 0.1.0、minSdk 24 / targetSdk 36、应用名「消消乐」、
  `screenOrientation=portrait`；`apksigner verify` 通过（调试证书）；APK 里 `assets/public/` 含 wasm、图集和注入了
  `android-shim.js` 的 `index.html`；
- `make android-check`（`web/android-app/test/check-www.mjs`）：桌面 Chrome 以 Pixel 6 视口（412×915、dpr 2.625、触摸、
  Android WebView UA）打开打进 APK 的同一份 `www/`，并模拟 Capacitor 的 App 插件，6 项全过：
  wasm 以 `application/wasm` 提供时走流式实例化；viewport 禁止缩放；触摸点按 + 拖划 3 步步数正确减少；
  返回键弹「退出消消乐？」、确定后调用 `exitApp`；控制台无错误；**故意把 wasm 以 `application/octet-stream` 提供时，
  shim 兜底为 arrayBuffer 实例化，游戏照常开局**。截图在 `/workspace/match3-android-shots/`。

## 7. 应用内行为

- **屏幕**：竖屏锁定；沉浸式全屏（状态栏、导航栏隐藏，从屏幕边缘滑动临时呼出，几秒后自动收起；
  从对话框或其他应用回来时重新隐藏）。窗口底色、启动画面都是游戏背景色 `#1c1630`，冷启动不闪白；
- **安全区**：Android 15+ 强制 edge-to-edge。WebView ≥ 140 时 Capacitor 把系统栏 / 刘海 inset 原样交给网页，
  页面的 `env(safe-area-inset-*)`（`layout.js` 已在用）生效；更旧的 WebView 上 Capacitor 改为给 WebView 加 padding 让开这些区域，
  留白处露出深色窗口底色，两种情况游戏都不会被遮挡；
- **缩放**：页面 `user-scalable=no` + WebView `setSupportZoom(false)` + shim 拦截多指 `touchstart`；
  系统字体大小不影响画面（`setTextZoom(100)`，画布上的字按格子缩放）；
- **返回键**：游戏没有关卡列表页，按返回键弹原生确认框「退出消消乐？」，确定退出，取消继续玩；
- **图标**：2×2 宝石（红 / 绿 / 蓝 / 星，取自网页图集），深紫底；Android 8+ 为自适应图标。

## 8. 已知限制

- **未上真机 / 模拟器**（见 §6）：性能（低端机上 Canvas 2D + 60 fps 固定步长）、WebView 版本兼容、刘海屏安全区都还只是推断；
- **WebView 版本**：GHC wasm 产物用到 reference types（JSFFI 的 externref）、bulk memory 等特性，`main.js` 用了模块顶层 await，
  估计需要 Chrome / WebView 96 以上（2021 年底；未在旧 WebView 上实测）。minSdk 24（Android 7）的设备只要更新过
  「Android System WebView」即可；没有 Google Play 的老设备 WebView 可能过旧，会停在「加载失败」；
- **大屏**：Android 16 起 sw600dp 以上的设备会忽略竖屏锁定；网页布局本身支持横排，横过来也能玩；
- **INTERNET 权限**：游戏不联网，但保留了 Capacitor 模板默认的 INTERNET 权限（调试时 live reload 需要）；
- 进度（当前关卡）不持久化：退出后重开从第 1 关开始（网页版同样没有存档）；
- 没有振动反馈（音效 / BGM 随网页版一起打进 APK，HUD 有开关）；
- 调试版 APK 里 WebView 可被 `chrome://inspect` 调试（Capacitor 默认，正式版关闭）。

## 9. iOS

同一套 Capacitor 工程可以加 iOS，但必须在 **装了 Xcode 的 Mac** 上做：

```sh
cd web/android-app          # 目录名叫 android-app，但 Capacitor 工程是跨平台的
npm install @capacitor/ios@8.5.2
npx cap add ios             # 生成 ios/（Xcode 工程，用 Swift Package Manager）
npx cap sync ios && npx cap open ios
```

要注意：iOS 的 WKWebView 通过 `capacitor://localhost` 提供页面，同样支持 wasm；返回键逻辑在 iOS 上不会触发；
状态栏 / 刘海需在 Xcode 里设 `UIViewControllerBasedStatusBarAppearance` 并配合 SystemBars 插件；
上架需要 Apple 开发者账号（年费）与签名证书。box（Linux）上无法构建 iOS。

## 10. 后续

- [ ] 在真机或能跑模拟器的机器上装 APK 实测：加载时间、帧率、安全区、返回键、息屏恢复；
- [ ] 存档：用 `@capacitor/preferences`（或 localStorage）记住当前关卡与最高分；
- [ ] 振动（`@capacitor/haptics`）；音效已随网页版提供；
- [ ] 正式签名、Play Console 内部测试轨道；
- [ ] CI：`make build apk android-check`，产物作为构建附件；
- [ ] 考虑把 `web/android-app/` 改名为 `mobile/`（加 iOS 之后）。
