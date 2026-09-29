-- | 极简 JSON 构造器（手写，不引入 aeson，减小 wasm 体积与依赖面）。
-- 所有输出都是拼好的 JSON 文本片段；数值只输出整数（不输出 Double，避免原生 / wasm 两端 show 细节差异）。
module Match3Web.Json
  ( obj
  , arr
  , int
  , bool
  , str
  , jsonString
  ) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)

-- | 对象：键按给定顺序输出。
obj :: [(String, String)] -> String
obj kvs = "{" ++ intercalate "," [str k ++ ":" ++ v | (k, v) <- kvs] ++ "}"

-- | 数组。
arr :: [String] -> String
arr xs = "[" ++ intercalate "," xs ++ "]"

int :: Int -> String
int = show

bool :: Bool -> String
bool True = "true"
bool False = "false"

-- | 对外暴露的 JSON 字符串编码（WebMain 的错误信息用）。
jsonString :: String -> String
jsonString = str

-- | JSON 字符串转义；非 ASCII（中文关卡名等）原样输出，由 toJSString 负责 UTF-8。
str :: String -> String
str s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc ch
      | ord ch < 0x20 = "\\u" ++ pad4 (showHex (ord ch) "")
      | otherwise = [ch]
    pad4 h = replicate (4 - length h) '0' ++ h
