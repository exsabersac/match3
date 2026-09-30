// Android（Capacitor）壳专用的补丁脚本：prepare-www.mjs 把它复制进 www/ 并插在 index.html 的模块脚本之前。
// 网页版 web/www/ 本身不依赖它；浏览器里直接打开 index.html 时不会加载本文件。
//
// 1) WebAssembly.instantiateStreaming 兜底：Capacitor 本地服务器（https://localhost）已按 application/wasm
//    返回 .wasm，正常走流式编译；万一 MIME 不对（旧 WebView、第三方改包）或浏览器没有该 API，
//    退回 fetch → arrayBuffer → WebAssembly.instantiate，main.js 不用改。
// 2) 返回键：没有关卡列表页，所以按返回键弹「退出游戏？」确认框，确定则退出应用。
// 3) 禁止捏合 / 双击缩放（viewport 已 user-scalable=no，这里再兜一层 touch 手势）。
(function () {
  "use strict";

  // ---- 1. wasm 流式实例化兜底 ----
  var WA = window.WebAssembly;
  if (WA) {
    var orig = typeof WA.instantiateStreaming === "function" ? WA.instantiateStreaming.bind(WA) : null;
    WA.instantiateStreaming = function (source, imports) {
      return Promise.resolve(source).then(function (resp) {
        if (!orig) return resp.arrayBuffer().then(function (buf) { return WA.instantiate(buf, imports); });
        // 先克隆一份响应体：流式编译失败（通常是 MIME 不是 application/wasm）时还能再读一次
        var backup = resp.clone();
        return orig(resp, imports).catch(function (e) {
          console.warn("[android-shim] instantiateStreaming 失败，改用 arrayBuffer：", e && e.message);
          window.__m3WasmFallback = String(e && e.message);
          return backup.arrayBuffer().then(function (buf) { return WA.instantiate(buf, imports); });
        });
      });
    };
  }

  // ---- 2. 返回键 ----
  function setupBackButton() {
    var C = window.Capacitor;
    var App = C && C.Plugins && C.Plugins.App;
    if (!App || typeof App.addListener !== "function") return false;
    var asking = false;
    App.addListener("backButton", function () {
      if (asking) return;
      asking = true;
      // window.confirm 在 Capacitor 里由 BridgeWebChromeClient 变成原生对话框
      var ok = window.confirm("退出消消乐？");
      asking = false;
      if (ok) App.exitApp();
    });
    return true;
  }
  if (!setupBackButton()) document.addEventListener("DOMContentLoaded", setupBackButton, { once: true });

  // ---- 3. 缩放手势 ----
  document.addEventListener("touchstart", function (e) { if (e.touches.length > 1) e.preventDefault(); }, { passive: false });
  document.addEventListener("gesturestart", function (e) { e.preventDefault(); }, { passive: false });
})();
