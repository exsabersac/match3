-- | 第 5 项文档 §4 的并行实验：金标准串行求值 vs. Spec.Support.Parallel 并行求值（不在任何构建目标里）。
--
-- 编译运行（仓库根目录；必须 -threaded，核数由 +RTS -N 决定）：
--
-- > b=$(mktemp -d); stack exec -- ghc -O1 -threaded -rtsopts -Wall -package-id "$(stack exec -- ghc-pkg --simple-output field match3 id)" -package stm -itest -itest/golden -outputdir "$b" -o "$b/p" docs/haskell-features/demo/ParGolden.hs
-- > time "$b/p" serial +RTS -N1
-- > time "$b/p" par +RTS -N8
--
-- 两种模式都打印行数和全部行的 FNV-1a 散列；与 test/golden/golden.txt 的散列（模式 file）相同即逐行一致。
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Word (Word64)
import qualified Golden
import Spec.Support.Parallel (forceLines, parallelForce)
import System.Environment (getArgs)

fnv :: [String] -> Word64
fnv = foldl' (\h ch -> (h `xor` fromIntegral (ord ch)) * 1099511628211) 14695981039346656037 . concatMap (++ "\n")

main :: IO ()
main = do
  args <- getArgs
  ls <- case args of
    ["serial"] -> pure Golden.goldenLines
    ["par"] -> do
      n <- getNumCapabilities
      concat <$> parallelForce n forceLines Golden.goldenSections
    ["file"] -> lines <$> readFile "test/golden/golden.txt"
    _ -> fail "用法：ParGolden serial|par|file"
  print (length ls, fnv ls)
