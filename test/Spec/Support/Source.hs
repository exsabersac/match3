-- | 源码扫描测试的共用工具：按目录列源文件（不写死清单）、统一剥离注释、取 import 的模块名与代码里的标识符。
--
-- 约定：
--   * 'sourcesUnder' 递归列出目录下全部 @.hs@ 文件（按路径排序），新增 / 拆分模块时扫描范围自动跟上；
--   * 'stripComments' 去掉行注释 @--@（不误伤 @-->@ 这类运算符）与可嵌套的块注释 @{- -}@（含 pragma），
--     字符串与字符字面量原样保留，换行全部保留（行号、按行判断 import 都不受影响）；
--   * 'importsOf' 只看 import 行，返回被导入的模块名——依赖方向类的约束优先用它，而不是在全文里找子串；
--   * 'codeIdents' 去掉注释与字符串之后切出标识符（含限定名，如 @M3E.play@）。
module Spec.Support.Source
  ( sourcesUnder
  , sourcesUnderAll
  , stripComments
  , stripStrings
  , readCode
  , importsOf
  , codeIdents
  , mentionsIdent
    -- * 本项目的扫描范围
  , builtinSources
  , mainFlowSources
  , pipelineSources
  ) where

import Data.Char (isAlphaNum, isUpper)
import Data.List (isSuffixOf, sort, (\\))
import System.Directory (doesDirectoryExist, listDirectory)

-- | 目录下（递归）全部 .hs 文件，路径形如 @src/Match3/Board/Cascade.hs@，按路径排序。
sourcesUnder :: FilePath -> IO [FilePath]
sourcesUnder dir = do
  names <- sort <$> listDirectory dir
  fmap concat $
    mapM
      ( \n -> do
          let p = dir ++ "/" ++ n
          isDir <- doesDirectoryExist p
          if isDir then sourcesUnder p else pure [p | ".hs" `isSuffixOf` n]
      )
      names

-- | 多个目录的 'sourcesUnder' 拼接。
sourcesUnderAll :: [FilePath] -> IO [FilePath]
sourcesUnderAll dirs = concat <$> mapM sourcesUnder dirs

-- | 读文件并去掉注释。
readCode :: FilePath -> IO String
readCode f = stripComments <$> readFile f

symbolChar :: Char -> Bool
symbolChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_' || c == '\''

-- | 去掉行注释与（可嵌套的）块注释；字符串 / 字符字面量保留；换行保留。
stripComments :: String -> String
stripComments = scrub False

-- | 同 'stripComments'，并把字符串字面量的内容清空（@"abc"@ → @""@）、字符字面量换成空格。
stripStrings :: String -> String
stripStrings = scrub True

-- | 词法扫描：跳过注释；blankLits 为 True 时同时清空字符串 / 字符字面量。
scrub :: Bool -> String -> String
scrub blankLits = code Nothing
  where
    -- prev：上一个输出的代码字符（判断 @--@ 是否是运算符的一部分、@'@ 是否是标识符里的撇号）
    code :: Maybe Char -> String -> String
    code _ [] = []
    code _ ('{' : '-' : rest) = block (1 :: Int) rest
    code prev s@('-' : '-' : _)
      | not (maybe False symbolChar prev)
      , not (startsWithSymbol (dropWhile (== '-') s)) =
          lineComment (dropWhile (== '-') s)
    code _ ('"' : rest) = '"' : str rest
    code prev ('\'' : rest)
      | not (maybe False identChar prev)
      , Just (lit, rest') <- charLit rest =
          (if blankLits then " " else '\'' : lit) ++ code (Just '\'') rest'
    code _ (c : rest) = c : code (Just c) rest

    startsWithSymbol r = case r of
      (c : _) -> symbolChar c
      [] -> False

    lineComment r = code Nothing (dropWhile (/= '\n') r)

    block :: Int -> String -> String
    block _ [] = []
    block n ('{' : '-' : rest) = block (n + 1) rest
    block n ('-' : '}' : rest)
      | n <= 1 = code Nothing rest
      | otherwise = block (n - 1) rest
    block n ('\n' : rest) = '\n' : block n rest
    block n (_ : rest) = block n rest

    -- 字符串体：处理转义，遇到未转义的引号结束
    str [] = []
    str ('\\' : c : rest) = (if blankLits then id else (['\\', c] ++)) (str rest)
    str ('"' : rest) = '"' : code (Just '"') rest
    str (c : rest) = (if blankLits then id else (c :)) (str rest)

    -- 字符字面量（开头的撇号之后）：'x'、'\n'、'\''、'\\'、'\DEL' 等；不像字面量时 Nothing（按普通字符处理）
    charLit r = case r of
      ('\\' : c : rest) ->
        let (esc, rest') = span (/= '\'') rest
        in case rest' of
             ('\'' : rest'') | length esc <= 5 -> Just ('\\' : c : esc ++ "'", rest'')
             _ -> Nothing
      (c : '\'' : rest) | c /= '\n' -> Just ([c, '\''], rest)
      _ -> Nothing

-- | 源码里 import 的模块名（按出现顺序；先去注释，只看以 @import@ 开头的行）。
importsOf :: String -> [String]
importsOf src =
  [ m
  | l <- lines (stripComments src)
  , ("import" : rest) <- [words l]
  , m : _ <- [dropWhile (`elem` ["qualified", "safe", "{-#", "SOURCE", "#-}"]) (dropPkg rest)]
  , startsUpper m
  ]
  where
    dropPkg ws = case ws of
      (w : r) | take 1 w == "\"" -> r
      _ -> ws
    startsUpper w = case w of
      (c : _) -> isUpper c
      [] -> False

-- | 去掉注释与字符串之后的标识符（含限定名 @M3E.play@；不含运算符与数字）。
codeIdents :: String -> [String]
codeIdents = go . stripStrings
  where
    go s = case dropWhile (not . start) s of
      [] -> []
      s' ->
        let (w, rest) = span (\c -> identChar c || c == '.') s'
        in trimDots w : go rest
    start c = isAlphaNum c || c == '_'
    trimDots = reverse . dropWhile (== '.') . reverse

-- | 代码里（去注释、去字符串）是否出现某个标识符：按 @.@ 后的基本名比较，因此 @M.foo@ 也算 @foo@。
mentionsIdent :: String -> String -> Bool
mentionsIdent name src = any ((== name) . baseName) (codeIdents src)
  where
    baseName = reverse . takeWhile (/= '.') . reverse

-- | 内置元素定义：@src/Match3/Element/Builtin.hs@ 与 @Builtin/@ 下的全部分组文件。
builtinSources :: IO [FilePath]
builtinSources = ("src/Match3/Element/Builtin.hs" :) <$> sourcesUnder "src/Match3/Element/Builtin"

-- | 主流程（规则流水线与通用层）：@Board/@、@Game/@、@Element/@、@ECS/@ 下除内置定义以外的模块、
-- @Match3/Engine.hs@ 与 @src/Engine/@；不含关卡数据 @Game/Level.hs@（那里按元素名写放置表）。
mainFlowSources :: IO [FilePath]
mainFlowSources = do
  flow <- sourcesUnderAll ["src/Match3/Board", "src/Match3/Game"]
  element <- sourcesUnderAll ["src/Match3/Element", "src/Match3/ECS"]
  builtin <- builtinSources
  engine <- sourcesUnder "src/Engine"
  pure ((flow \\ ["src/Match3/Game/Level.hs"]) ++ (element \\ builtin) ++ ["src/Match3/Engine.hs"] ++ engine)

-- | 结算流水线：@Board/@ 与 @Game/@ 下的模块，去掉关卡数据（@Game/Level.hs@）与
-- 回放记录层（@Game/Trace.hs@：它为逐轮回放重算步末记录，按设计引用皮带 / 蜗牛的实现）。
pipelineSources :: IO [FilePath]
pipelineSources = do
  fs <- sourcesUnderAll ["src/Match3/Board", "src/Match3/Game"]
  pure (fs \\ ["src/Match3/Game/Level.hs", "src/Match3/Game/Trace.hs"])
