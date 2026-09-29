-- 原生（桌面 GHC 9.4.8 / lts-21.25）一侧的一致性脚本：
-- 同一关卡 + 种子开局，按核心提示连走 N 步，逐步打印接口 JSON。
-- 与 node-parity.mjs（wasm 一侧）的输出逐字节比较，验证两端规则与随机数完全一致。
-- 用法（仓库根目录）：stack runghc -- -isrc -iweb/hs web/test/Parity.hs 0 20260929 12
module Main (main) where

import System.Environment (getArgs)
import Match3.Core
import Match3Web.Api (apiNew, apiSwap)

main :: IO ()
main = do
  [li, seed, n] <- map read <$> getArgs
  let (gs0, j0) = apiNew li seed
  putStrLn j0
  go (n :: Int) gs0
  where
    go 0 _ = pure ()
    go k gs = case (gsOver gs, findHint (gsBoard gs)) of
      (Nothing, Just (a, b)) -> do
        let (gs', j) = apiSwap a b gs
        putStrLn j
        go (k - 1) gs'
      _ -> pure ()
