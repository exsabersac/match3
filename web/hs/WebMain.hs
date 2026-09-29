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

import Match3Web.Api (WebGame, apiLevels, apiNew, apiState, apiSwap, apiUndo, jsonString)

-- | 当前这一局（含撤销历史；Nothing = 还没调用过 m3New）。
{-# NOINLINE stateRef #-}
stateRef :: IORef (Maybe WebGame)
stateRef = unsafePerformIO (newIORef Nothing)

foreign export javascript "m3New sync" jsNew :: Int -> Int -> IO JSString
foreign export javascript "m3Swap sync" jsSwap :: Int -> Int -> Int -> Int -> IO JSString
foreign export javascript "m3Undo sync" jsUndo :: IO JSString
foreign export javascript "m3State sync" jsState :: IO JSString
foreign export javascript "m3Levels sync" jsLevels :: IO JSString

-- | m3New(level, seed)：开新局并返回 {ok,state}。
jsNew :: Int -> Int -> IO JSString
jsNew li seed = guarded $ do
  let (gs, out) = apiNew li seed
  _ <- evaluate (length out)       -- 先把 JSON 完整算出来，失败时不污染全局状态
  writeIORef stateRef (Just gs)
  pure out

-- | m3Swap(r1,c1,r2,c2)：交换两格；返回 {ok,accepted,outcome,trace,events,state}。
jsSwap :: Int -> Int -> Int -> Int -> IO JSString
jsSwap r1 c1 r2 c2 = withGame (apiSwap (r1, c1) (r2, c2))

-- | m3Undo()：撤销一步；JSON 形状同 m3Swap。
jsUndo :: IO JSString
jsUndo = withGame apiUndo

-- | 对当前一局执行一个纯动作：JSON 完整算出后才写回全局状态。
withGame :: (WebGame -> (WebGame, String)) -> IO JSString
withGame f = guarded $ do
  mh <- readIORef stateRef
  case mh of
    Nothing -> pure (errJson "no game; call m3New first")
    Just h -> do
      let (h', out) = f h
      _ <- evaluate (length out)
      writeIORef stateRef (Just h')
      pure out

-- | m3State()：当前局面快照。
jsState :: IO JSString
jsState = guarded $ maybe (errJson "no game") apiState <$> readIORef stateRef

-- | m3Levels()：关卡列表。
jsLevels :: IO JSString
jsLevels = guarded (pure apiLevels)

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
