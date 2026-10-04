// 网页音效。浏览器要在用户点过之后才能出声。
// 音效与 BGM 各自开关，默认开；localStorage 键 m3-sfx / m3-bgm。
// 音效名由 wasm 下发（m3Meta 的 sounds = UI.Presentation.soundNames），启动时 setSoundNames 填入
let NAMES = [];
export function setSoundNames(names) { NAMES = names; }
const KEY_SFX = "m3-sfx";
const KEY_BGM = "m3-bgm";

function readPref(key) {
  const v = localStorage.getItem(key);
  if (v === "on" || v === "off") return v !== "off";
  // 旧总开关 m3-sound：新键缺失时借用一次
  const legacy = localStorage.getItem("m3-sound");
  if (legacy === "on" || legacy === "off") return legacy !== "off";
  return true;
}

let ctx = null;
let sfxOn = readPref(KEY_SFX);
let bgmOn = readPref(KEY_BGM);
let buffers = {}, bgm = null, unlocked = false, bgmEnded = false;

function ac() {
  if (!ctx) ctx = new (window.AudioContext || window.webkitAudioContext)();
  return ctx;
}

export function sfxEnabled() { return sfxOn; }
export function bgmEnabled() { return bgmOn; }

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
  startBgm();
}

export function startBgm() {
  bgmEnded = false;
  actuallyStartBgm();
}

function actuallyStartBgm() {
  if (!bgmOn || bgmEnded || !ctx || !buffers.bgm) return;
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
  if (name === "win" || name === "lose") {
    bgmEnded = true;
    stopBgm();
  }
  if (!sfxOn || !ctx || !buffers[name]) return;
  const src = ctx.createBufferSource();
  src.buffer = buffers[name];
  src.connect(ctx.destination);
  src.start();
}

export function toggleSfx() {
  sfxOn = !sfxOn;
  localStorage.setItem(KEY_SFX, sfxOn ? "on" : "off");
  return sfxOn;
}

export function toggleBgm() {
  bgmOn = !bgmOn;
  localStorage.setItem(KEY_BGM, bgmOn ? "on" : "off");
  if (!bgmOn) stopBgm();
  else actuallyStartBgm();
  return bgmOn;
}
