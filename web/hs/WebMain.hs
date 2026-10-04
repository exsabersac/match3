-- | wasm 入口：用 GHC wasm 后端的 JavaScript FFI 把 Match3Web.Api 导出给浏览器。
--
-- * 编译为 WASI reactor 模块（-no-hs-main -mexec-model=reactor），没有 main 循环；
--   JS 侧在 wasi.initialize(instance) 之后即可调用下面的导出函数（RTS 由构造器自动初始化）。
-- * 全部是 "sync" 导出：JS 调用立即返回字符串（JSON），无需 await。
-- * 当前局面存在一个全局 IORef 里（单线程 RTS，一个页面一局），JS 只持有 JSON 快照。
-- * 规则全在核心（经 Match3.Engine.match3Shell 的 gameStep）；本文件只负责 IORef 读写、异常兜底和 String ↔ JSString。
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Wasm.Prim (JSString (..), toJSString)
import System.IO.Unsafe (unsafePerformIO)

import ComboFx (Cascade)
import Engine.Playback (Player)
import Match3Web.Anim (AnimSeed, animStart, animTick)
import Match3Web.Api
  ( WebGame, apiAdvance, apiBadge, apiCross, apiDaily, apiFreeSwap, apiHammer, apiLevels, apiMapJump, apiMeta, apiNew
  , apiProgress, apiRestart, apiShowcase, apiShuffle, apiState, apiSwapAnim, apiUndo, jsonString )

-- | 当前这一局（含撤销历史；Nothing = 还没调用过 m3New）。
{-# NOINLINE stateRef #-}
stateRef :: IORef (Maybe WebGame)
stateRef = unsafePerformIO (newIORef Nothing)

-- | 最近一步的回放输入（m3Swap 写入；m3New / m3Undo 清空）与正在播放的回放器（m3AnimStart 建立，播完清空）。
{-# NOINLINE seedRef #-}
seedRef :: IORef (Maybe AnimSeed)
seedRef = unsafePerformIO (newIORef Nothing)

{-# NOINLINE playerRef #-}
playerRef :: IORef (Maybe (Player Cascade))
playerRef = unsafePerformIO (newIORef Nothing)

foreign export javascript "m3New sync" jsNew :: Int -> Int -> IO JSString
foreign export javascript "m3Swap sync" jsSwap :: Int -> Int -> Int -> Int -> IO JSString
foreign export javascript "m3Undo sync" jsUndo :: IO JSString
foreign export javascript "m3State sync" jsState :: IO JSString
foreign export javascript "m3AnimStart sync" jsAnimStart :: IO JSString
foreign export javascript "m3AnimTick sync" jsAnimTick :: Int -> IO JSString
foreign export javascript "m3Levels sync" jsLevels :: IO JSString
foreign export javascript "m3Meta sync" jsMeta :: IO JSString
-- 桌面版功能迁移（web-sdl-parity）：道具 / 洗牌（JSON 形状同 m3Swap，道具另带 keepTool）、每日挑战、重开、结局后前进、
-- 元素展示盘、选关进度 / 星级、地图点选、分数徽章
foreign export javascript "m3Hammer sync" jsHammer :: Int -> Int -> IO JSString
foreign export javascript "m3Cross sync" jsCross :: Int -> Int -> IO JSString
foreign export javascript "m3FreeSwap sync" jsFreeSwap :: Int -> Int -> Int -> Int -> IO JSString
foreign export javascript "m3Shuffle sync" jsShuffle :: IO JSString
foreign export javascript "m3Daily sync" jsDaily :: Int -> Int -> Int -> IO JSString
foreign export javascript "m3Restart sync" jsRestart :: Int -> Int -> IO JSString
foreign export javascript "m3Advance sync" jsAdvance :: Int -> Int -> IO JSString
foreign export javascript "m3Showcase sync" jsShowcase :: IO JSString
foreign export javascript "m3Progress sync" jsProgress :: Int -> Int -> IO JSString
foreign export javascript "m3MapJump sync" jsMapJump :: Int -> Int -> IO JSString
foreign export javascript "m3Badge sync" jsBadge :: Int -> Int -> Int -> Int -> Int -> IO JSString

-- | m3New(level, seed)：开新局并返回 {ok,state}。
jsNew :: Int -> Int -> IO JSString
jsNew li seed = guarded $ do
  let (gs, out) = apiNew li seed
  _ <- evaluate (length out)       -- 先把 JSON 完整算出来，失败时不污染全局状态
  writeIORef stateRef (Just gs)
  clearAnim
  pure out

-- | m3Swap(r1,c1,r2,c2)：交换两格；返回 {ok,accepted,outcome,trace,events,state}。
jsSwap :: Int -> Int -> Int -> Int -> IO JSString
jsSwap r1 c1 r2 c2 = withGame (apiSwapAnim (r1, c1) (r2, c2))

-- | m3Hammer(r,c) / m3Cross(r,c) / m3FreeSwap(r1,c1,r2,c2)：三种道具；m3Shuffle()：手动洗牌。JSON 形状同 m3Swap。
jsHammer :: Int -> Int -> IO JSString
jsHammer r c = withGame (apiHammer (r, c))

jsCross :: Int -> Int -> IO JSString
jsCross r c = withGame (apiCross (r, c))

jsFreeSwap :: Int -> Int -> Int -> Int -> IO JSString
jsFreeSwap r1 c1 r2 c2 = withGame (apiFreeSwap (r1, c1) (r2, c2))

jsShuffle :: IO JSString
jsShuffle = withGame apiShuffle

-- | m3Daily(年, 月, 日)：开每日挑战（种子由日期决定）；返回 {ok,state}。
jsDaily :: Int -> Int -> Int -> IO JSString
jsDaily y m d = guarded $ do
  let (h, out) = apiDaily y m d
  _ <- evaluate (length out)
  writeIORef stateRef (Just h)
  clearAnim
  pure out

-- | m3Restart(开局步数, 种子)：重开本关；m3Advance(开局步数, 种子)：结局后前进；m3Showcase()：换成元素展示盘。
jsRestart :: Int -> Int -> IO JSString
jsRestart sm seed = withFresh (apiRestart sm seed)

jsAdvance :: Int -> Int -> IO JSString
jsAdvance sm seed = withFresh (apiAdvance sm seed)

jsShowcase :: IO JSString
jsShowcase = withFresh apiShowcase

-- | 换一局（重开 / 前进 / 展示盘）：同 withGame，但清空回放。
withFresh :: (WebGame -> (WebGame, String)) -> IO JSString
withFresh f = withGame (\h -> let (h', j) = f h in (h', Nothing, j))

-- | m3Progress(已解锁, 开局步数) / m3MapJump(已解锁, 点中的关卡) / m3Badge(回放中, 连击, 显示分数, 总结剩余帧, 最高连击)：只读查询。
jsProgress :: Int -> Int -> IO JSString
jsProgress reached sm = readGame (apiProgress reached sm)

jsMapJump :: Int -> Int -> IO JSString
jsMapJump reached li = readGame (apiMapJump reached li)

jsBadge :: Int -> Int -> Int -> Int -> Int -> IO JSString
jsBadge rp k shown left best = readGame (apiBadge rp k shown left best)

readGame :: (WebGame -> String) -> IO JSString
readGame f = guarded $ maybe (errJson "no game") f <$> readIORef stateRef

-- | m3Undo()：撤销一步；JSON 形状同 m3Swap。
jsUndo :: IO JSString
jsUndo = withGame (\h -> let (h', j) = apiUndo h in (h', Nothing, j))

-- | 对当前一局执行一个纯动作：JSON 完整算出后才写回全局状态；同时记下本步的回放输入。
withGame :: (WebGame -> (WebGame, Maybe AnimSeed, String)) -> IO JSString
withGame f = guarded $ do
  mh <- readIORef stateRef
  case mh of
    Nothing -> pure (errJson "no game; call m3New first")
    Just h -> do
      let (h', seed, out) = f h
      _ <- evaluate (length out)
      writeIORef stateRef (Just h')
      writeIORef seedRef seed
      writeIORef playerRef Nothing
      pure out

clearAnim :: IO ()
clearAnim = writeIORef seedRef Nothing >> writeIORef playerRef Nothing

-- | m3AnimStart()：为上一步 m3Swap 建立逐轮回放（ComboFx 阶段机）。
-- 返回 {ok, anim:true, boards, base, fall} 或 {ok, anim:false}（本步无需回放）。
jsAnimStart :: IO JSString
jsAnimStart = guarded $ do
  ms <- readIORef seedRef
  case ms of
    Nothing -> pure "{\"ok\":true,\"anim\":false}"
    Just s -> do
      let (p, out) = animStart s
      _ <- evaluate (length out)
      writeIORef playerRef (Just p)
      pure out

-- | m3AnimTick(fast)：推进一帧（fast ≠ 0 表示点击加速）。见 Match3Web.Anim.animTick 的 JSON 说明。
jsAnimTick :: Int -> IO JSString
jsAnimTick fast = guarded $ do
  ms <- readIORef seedRef
  mp <- readIORef playerRef
  case (ms, mp) of
    (Just s, Just p) -> do
      let (mp', _, out) = animTick (fast /= 0) s p
      _ <- evaluate (length out)
      writeIORef playerRef mp'
      pure out
    _ -> pure "{\"done\":true,\"none\":true}"

-- | m3State()：当前局面快照。
jsState :: IO JSString
jsState = guarded $ maybe (errJson "no game") apiState <$> readIORef stateRef

-- | m3Levels()：关卡列表。
jsLevels :: IO JSString
jsLevels = guarded (pure apiLevels)

-- | m3Meta()：表现表（颜色、格子取色规则、碎屑色、生长曲线、帧数、音效名；见 Match3Web.Api.apiMeta）。
jsMeta :: IO JSString
jsMeta = guarded (pure apiMeta)

-- | sync 导出目前不会把 Haskell 异常传给 JS，这里统一兜底成 {"ok":false,"error":...}。
guarded :: IO String -> IO JSString
guarded act = do
  r <- try (act >>= \s -> evaluate (length s) >> pure s)
  pure . toJSString $ case r of
    Right s -> s
    Left e -> errJson (show (e :: SomeException))

errJson :: String -> String
errJson msg = "{\"ok\":false,\"error\":" ++ jsonString msg ++ "}"

-- | reactor 模块用不到 main，但 cabal 的 executable 需要 Main 模块有它。
main :: IO ()
main = pure ()
