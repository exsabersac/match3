-- 逐轮回放的一致性脚本（原生一侧）：同一关卡 + 种子，按核心提示连走 N 步；每步之后建立回放
-- （Match3Web.Anim，即桌面 ComboFx 阶段机）并逐帧推进到播完，打印开播 JSON、每帧 JSON 与
-- 「frames=帧数 events=事件数」。每第 3 步从第 5 帧起加速（覆盖 fast 路径）。
-- 与 node-anim-parity.mjs（wasm 一侧）的输出逐字节比较；另外在原生一侧核对：不加速的步，
-- 逐帧循环的帧数 / 事件数与 Engine.Playback.runPlayer 一口气播完的结果相同（不一致则退出码 1）。
-- 用法（仓库根目录）：stack exec -- runghc -isrc -iapp -iweb/hs web/test/AnimParity.hs 12 42 20
module Main (main) where

import Control.Monad (unless, when)
import Data.IORef
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Match3.Core
import Match3Web.Anim (animRunPlayer, animStart, animTick)
import Match3Web.Api (apiNew, apiSwapAnim, webState)

main :: IO ()
main = do
  [li, seed, n] <- map read <$> getArgs
  bad <- newIORef (0 :: Int)
  checked <- newIORef (0 :: Int)
  let (h0, _) = apiNew li seed
      go k h
        | k >= (n :: Int) = pure ()
        | otherwise = do
            let gs = webState h
            case (gsOver gs, findHint (gsBoard gs)) of
              (Nothing, Just (a, b)) -> do
                let (h', ms, _) = apiSwapAnim a b h
                putStrLn ("step " ++ show k)
                case ms of
                  Nothing -> putStrLn "noanim"
                  Just s -> do
                    let (p0, j0) = animStart s
                        fastStep = k `mod` 3 == 2
                        loop fr evs p = do
                          let (mp, ne, j) = animTick (fastStep && fr >= 5) s p
                          putStrLn j
                          case mp of
                            Just p' -> loop (fr + 1) (evs + ne) p'
                            Nothing -> pure (fr + 1, evs + ne)
                    putStrLn j0
                    (frames, evs) <- loop (0 :: Int) (0 :: Int) p0
                    putStrLn ("frames=" ++ show frames ++ " events=" ++ show evs)
                    unless fastStep $ do
                      modifyIORef' checked (+ 1)
                      let (rf, re) = animRunPlayer s
                      when ((rf, re) /= (frames, evs)) $ do
                        modifyIORef' bad (+ 1)
                        hPutStrLn stderr ("MISMATCH step " ++ show k ++ ": loop " ++ show (frames, evs) ++ " runPlayer " ++ show (rf, re))
                go (k + 1) h'
              _ -> pure ()
  go 0 h0
  b <- readIORef bad
  c <- readIORef checked
  hPutStrLn stderr ("native: runPlayer 核对 " ++ show c ++ " 步，不一致 " ++ show b)
  when (b > 0) exitFailure
