// 网页音效。浏览器要在用户点过之后才能出声。总开关默认开，记在 localStorage m3-sound。
const NAMES = ["swap", "clear", "special", "illegal", "win", "lose", "bgm"];
const KEY = "m3-sound";
let ctx = null, on = localStorage.getItem(KEY) !== "off", buffers = {}, bgm = null, unlocked = false;

function ac() {
  if (!ctx) ctx = new (window.AudioContext || window.webkitAudioContext)();
  return ctx;
}

export function enabled() { return on; }

export async function unlock() {
  const c = ac();
  if (c.state === "suspended") await c.resume();
  if (!unlocked) {
    unlocked = true;
    await Promise.all(NAMES.map(async (n) => {
      if (buffers[n]) return;
      try {
        const res = await fetch("sfx/" + n + ".wav");
        buffers[n] = await c.decodeAudioData(await res.arrayBuffer());
      } catch (_) { /* 缺文件就跳过这一声 */ }
    }));
  }
  if (on) startBgm();
}

export function startBgm() {
  if (!on || !ctx || !buffers.bgm) return;
  if (bgm) return;
  const src = ctx.createBufferSource();
  src.buffer = buffers.bgm;
  src.loop = true;
  src.connect(ctx.destination);
  src.start();
  bgm = src;
}

export function stopBgm() {
  if (bgm) { try { bgm.stop(); } catch (_) {} bgm = null; }
}

export function play(name) {
  if (name === "win" || name === "lose") stopBgm();
  if (!on || !ctx || !buffers[name]) return;
  const src = ctx.createBufferSource();
  src.buffer = buffers[name];
  src.connect(ctx.destination);
  src.start();
}

export function toggle() {
  on = !on;
  localStorage.setItem(KEY, on ? "on" : "off");
  if (!on) stopBgm();
  if (on) { unlock(); }
  return on;
}
