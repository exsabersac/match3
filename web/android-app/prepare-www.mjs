// 把网页版产物 web/dist 复制到 web/android-app/www（Capacitor 的 webDir），并注入 android-shim.js。
// 用法：node prepare-www.mjs [dist 目录]（默认 ../dist，即 web/build.sh 的输出）
import { cpSync, existsSync, readFileSync, rmSync, writeFileSync, statSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const dist = resolve(process.argv[2] ?? join(here, "..", "dist"));
const www = join(here, "www");

for (const f of ["index.html", "main.js", "match3-web.wasm", "ghc_wasm_jsffi.js", "atlas.webp", "atlas.json", "background.webp"]) {
  if (!existsSync(join(dist, f))) {
    console.error(`找不到 ${join(dist, f)}：先运行 web/build.sh（或 make build）构建网页版`);
    process.exit(1);
  }
}

rmSync(www, { recursive: true, force: true });
cpSync(dist, www, { recursive: true });
cpSync(join(here, "shim", "android-shim.js"), join(www, "android-shim.js"));

// 在第一个 <script> 之前插入补丁脚本（普通脚本同步执行，先于 type=module 的 main.js）
const indexPath = join(www, "index.html");
let html = readFileSync(indexPath, "utf8");
const tag = '<script src="android-shim.js"></script>\n';
const at = html.indexOf("<script");
if (at < 0) { console.error("index.html 里没有 <script>，无法注入 android-shim.js"); process.exit(1); }
html = html.slice(0, at) + tag + html.slice(at);
// 标题换成应用名（任务切换器里看不到，但 WebView 调试时好认）
html = html.replace(/<title>[^<]*<\/title>/, "<title>消消乐</title>");
writeFileSync(indexPath, html);

const wasm = statSync(join(www, "match3-web.wasm")).size;
console.log(`已准备 ${www}（来自 ${dist}；match3-web.wasm ${wasm} 字节，已注入 android-shim.js）`);
