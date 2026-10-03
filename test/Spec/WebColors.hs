-- | 网页版 JS 颜色表、贴图生成器调色板与 Haskell 调色板 / 表现表的一致性。
--
-- 读 web/www/cells.js 与 web/www/main.js 的源码，解析出颜色表，与桌面的唯一来源逐项比对：
--   * cells.js 的 COLOR_RGB（五色主色）         ↔ UI.Palette.colorRGB
--   * cells.js 的 ELEMENT_RGB（按元素名取色）     ↔ UI.Presentation.elementRGBTable
--   * cells.js 的 cellRGB（粒子 / 退回画法颜色）  ↔ UI.Palette.cellRGB（每种格子逐个比，格子的 JS 标签取自 Match3.View.cellFace）
--   * main.js 的 SPREAD_CRUMB_RGB（蔓延碎屑色）   ↔ elementRGBTable 里会蔓延的元素（spreadCurves 的名字）
--   * main.js 的倒计时火星色、render.js 的生长前沿缺省光 ↔ 表现表 EvTick 的 CrumbsAtSources / defaultSpreadGlow
--   * tools/gen_assets.py 的 GEMS 调色板（宝石贴图的主色）↔ UI.Palette.colorRGB
-- 解析不到预期的写法时直接失败并说明是哪个文件的哪张表（改了 JS / Python 的写法就同步改这里的解析）。
module Spec.WebColors
  ( tests
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (isInfixOf, isPrefixOf, nub)
import Match3.Core
import Match3.Element.Event (EventKind (..))
import Match3.View (CellField (..), cellFace, colorNum)
import Test.Tasty
import Test.Tasty.HUnit
import UI.Palette (cellRGB, colorRGB)
import UI.Presentation

tests :: [TestTree]
tests =
  [ testCase "web_color_rgb_matches_palette" web_color_rgb_matches_palette
  , testCase "web_element_rgb_matches_presentation" web_element_rgb_matches_presentation
  , testCase "web_cell_rgb_matches_palette" web_cell_rgb_matches_palette
  , testCase "web_spread_crumbs_match_presentation" web_spread_crumbs_match_presentation
  , testCase "web_end_stage_colors_match_presentation" web_end_stage_colors_match_presentation
  , testCase "gen_assets_gem_palette_matches_palette" gen_assets_gem_palette_matches_palette
  ]

--------------------------------------------------------------------------------
-- 读 JS

cellsJs, mainJs, renderJs :: FilePath
cellsJs = "web/www/cells.js"
mainJs = "web/www/main.js"
renderJs = "web/www/render.js"

-- | 去掉 // 行注释（这几张表所在的行里没有含 // 的字符串）。
stripLineComments :: String -> String
stripLineComments = unlines . map cut . lines
  where
    cut l = go l
      where
        go ('/' : '/' : _) = ""
        go (c : cs) = c : go cs
        go [] = []

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | 在文件里找 @const 名字 = { … }@，解析成 [(键, (r, g, b))]（键是标识符或数字，值是三元数组）。
jsRgbObject :: FilePath -> String -> IO [(String, RGB)]
jsRgbObject file name = do
  src <- stripLineComments <$> readFile file
  let marker = "const " ++ name ++ " = {"
  case breakOn marker src of
    Nothing -> failWith ("找不到 " ++ marker)
    Just rest -> case parseEntries (takeWhile (/= '}') rest) of
      Right es | not (null es) -> pure es
      Right _ -> failWith (name ++ " 是空表")
      Left err -> failWith (name ++ "：" ++ err)
  where
    failWith msg = assertFailure (file ++ "：" ++ msg) >> pure []
    parseEntries s = case dropWhile (\c -> isSpace c || c == ',') s of
      "" -> Right []
      s1 ->
        let (key, s2) = span (\c -> isAlphaNum c || c == '_') s1
        in case dropWhile isSpace s2 of
             ':' : s3 -> case parseTriple s3 of
               Just (rgb, s4) | not (null key) -> ((key, rgb) :) <$> parseEntries s4
               _ -> Left ("键 " ++ show key ++ " 的值不是 [r, g, b]")
             _ -> Left ("解析不到 键: 值（在 " ++ show (take 30 s1) ++ "）")

-- | 解析 @[r, g, b]@（前面可有空白），返回剩余部分。
parseTriple :: String -> Maybe (RGB, String)
parseTriple s = case dropWhile isSpace s of
  '[' : body ->
    let (inside, rest) = break (== ']') body
        parts = map trim (splitOn ',' inside)
    in case parts of
         [a, b, c] | all (\p -> not (null p) && all isDigit p) parts ->
           Just ((fromInteger (read a), fromInteger (read b), fromInteger (read c)), drop 1 rest)
         _ -> Nothing
  _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn d s = case break (== d) s of
  (a, []) -> [a]
  (a, _ : b) -> a : splitOn d b

-- | @marker@ 第一次出现之后的内容。
breakOn :: String -> String -> Maybe String
breakOn marker = go
  where
    go s
      | marker `isPrefixOf` s = Just (drop (length marker) s)
      | otherwise = case s of
          [] -> Nothing
          _ : rest -> go rest

-- | cells.js 的 cellRGB 每个 case 返回什么。
data JsColor
  = JsFixed RGB      -- ^ 固定颜色 @[r, g, b]@
  | JsByColor        -- ^ @COLOR_RGB[cell.c] || [200, 200, 200]@：按格子的颜色字段
  | JsCustom RGB     -- ^ 自定义格：变色龙按当前颜色，其余查 ELEMENT_RGB，查不到用这个缺省
  deriving (Eq, Show)

-- | 解析 cellRGB 的 switch：[(标签, 返回)]，外加 default 的颜色。
jsCellRGB :: IO ([(String, JsColor)], RGB)
jsCellRGB = do
  src <- readFile cellsJs
  let body = takeWhile (/= "}") (drop 1 (dropWhile (not . ("export function cellRGB(cell) {" `isPrefixOf`)) (lines src)))
      caseLines = [trim (stripLineComments l) | l <- body, any (`isPrefixOf` trim l) ["case ", "default:"]]
  assertBool (cellsJs ++ "：找不到 cellRGB 的 switch") (not (null caseLines))
  parsed <- mapM parseCase caseLines
  let cases = concat [[(t, r) | t <- ts] | (Just ts, r) <- parsed]
      defaults = [r | (Nothing, JsFixed r) <- parsed]
  case defaults of
    [d] -> pure (cases, d)
    _ -> assertFailure (cellsJs ++ "：cellRGB 应恰有一个 default: return [r, g, b]") >> pure (cases, (0, 0, 0))
  where
    parseCase l = do
      let (tags, ret) = caseTags l
      case classify (trim ret) of
        Just c -> pure (if l `startsWith` "default:" then Nothing else Just tags, c)
        Nothing -> assertFailure (cellsJs ++ "：cellRGB 里认不出的返回：" ++ l) >> pure (Nothing, JsFixed (0, 0, 0))
    startsWith l p = p `isPrefixOf` l
    -- 依次吃掉 case "x": 前缀，剩下 return …;
    caseTags l = case l of
      _ | "default:" `isPrefixOf` l -> ([], drop (length "default:") l)
      _ | "case \"" `isPrefixOf` l ->
            let (t, rest) = break (== '"') (drop (length "case \"") l)
                (ts, ret) = caseTags (trim (drop 1 (dropWhile (/= ':') rest)))
            in (t : ts, ret)
      _ -> ([], l)
    classify r0 = do
      r <- stripSuffix ";" =<< stripPrefix' "return " r0
      case parseTriple r of
        Just (rgb, rest) | all isSpace rest -> Just (JsFixed rgb)
        _
          | r == "COLOR_RGB[cell.c] || [200, 200, 200]" -> Just JsByColor
          | "cell.name === \"chameleon\" && cell.c ? COLOR_RGB[cell.c] : ELEMENT_RGB[cell.name] ||" `isPrefixOf` r ->
              case breakOn "ELEMENT_RGB[cell.name] ||" r >>= parseTriple of
                Just (d, rest) | all isSpace rest -> Just (JsCustom d)
                _ -> Nothing
          | otherwise -> Nothing
    stripPrefix' p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing
    stripSuffix p s = let n = length s - length p in if n >= 0 && drop n s == p then Just (take n s) else Nothing

--------------------------------------------------------------------------------
-- 比对

showRGB :: RGB -> String
showRGB (r, g, b) = "[" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ "]"

-- | 两张按键的表逐项比：键集合与值都要相同；返回每处差异一行。
diffTables :: String -> String -> [(String, RGB)] -> [(String, RGB)] -> [String]
diffTables jsName hsName js hs =
  [ jsName ++ "." ++ k ++ " = " ++ showRGB v ++ "，" ++ hsName ++ " 没有这一项" | (k, v) <- js, k `notElem` map fst hs ]
    ++ [ jsName ++ " 缺 " ++ k ++ "（" ++ hsName ++ " = " ++ showRGB v ++ "）" | (k, v) <- hs, k `notElem` map fst js ]
    ++ [ jsName ++ "." ++ k ++ " = " ++ showRGB a ++ "，" ++ hsName ++ " = " ++ showRGB b
       | (k, a) <- js, Just b <- [lookup k hs], a /= b ]

assertNoDiff :: String -> [String] -> Assertion
assertNoDiff what ds = assertBool (what ++ " 与 Haskell 不一致：\n  " ++ concatMap (++ "\n  ") ds) (null ds)

web_color_rgb_matches_palette :: Assertion
web_color_rgb_matches_palette = do
  js <- jsRgbObject cellsJs "COLOR_RGB"
  assertNoDiff "cells.js COLOR_RGB"
    (diffTables "COLOR_RGB" "UI.Palette.colorRGB" js [(show (colorNum c), colorRGB c) | c <- allColors])

elementTable :: [(String, RGB)]
elementTable = [(unElementName n, rgb) | (n, rgb) <- elementRGBTable]

web_element_rgb_matches_presentation :: Assertion
web_element_rgb_matches_presentation = do
  js <- jsRgbObject cellsJs "ELEMENT_RGB"
  assertNoDiff "cells.js ELEMENT_RGB" (diffTables "ELEMENT_RGB" "UI.Presentation.elementRGBTable" js elementTable)

-- | 每种格子各取几个样本（带颜色的取全部五色；自定义格取两边表里出现的每个名字、变色龙的五种颜色、一个未知名字）。
sampleCells :: [String] -> [Cell]
sampleCells extraNames =
  [Gem c k 0 Nothing | c <- allColors, k <- [Normal, LineH, Bomb]]
    ++ [Stone 1, Chest 2, Honey 1, Cookie, Cake 3, MagicHat, Snail 0 1, Safe 1, Surprise, TimeSpirit]
    ++ concat [[Balloon c, Maker c 3, Flip c (succColor c), Bottle c, Countdown c 5] | c <- allColors]
    ++ [Custom (ElementName n) (CustomState 0) | n <- nub (map fst elementTable ++ extraNames ++ ["no_such_element"])]
    ++ [Custom (ElementName "chameleon") (CustomState i) | i <- [0 .. 4]]
  where
    succColor c = if c == maxBound then minBound else succ c

-- | 按 JS 的写法算一格的颜色（标签与颜色字段取自 cellFace；变色龙的 "c" 由 chameleonColor 给，同 Match3Web.Api）。
jsColorOf :: ([(String, JsColor)], RGB) -> [(String, RGB)] -> [(String, RGB)] -> Cell -> RGB
jsColorOf (cases, dflt) colorTable elemTable cell =
  case lookup tag cases of
    Just (JsFixed rgb) -> rgb
    Just JsByColor -> maybe (200, 200, 200) id (cField >>= \c -> lookup (show c) colorTable)
    Just (JsCustom d) -> case (name, chamC) of
      (Just "chameleon", Just c) -> maybe d id (lookup (show c) colorTable)
      (Just n, _) -> maybe d id (lookup n elemTable)
      _ -> d
    Nothing -> dflt
  where
    (tag, fields) = cellFace cell
    cField = case lookup "c" fields of
      Just (FieldInt c) -> Just c
      _ -> Nothing
    chamC = colorNum <$> chameleonColor cell
    name = case lookup "name" fields of
      Just (FieldText n) -> Just n
      _ -> Nothing

web_cell_rgb_matches_palette :: Assertion
web_cell_rgb_matches_palette = do
  fn@(cases, _) <- jsCellRGB
  colors <- jsRgbObject cellsJs "COLOR_RGB"
  elems <- jsRgbObject cellsJs "ELEMENT_RGB"
  let cells = sampleCells (map fst elems)
      knownTags = nub (map (fst . cellFace) cells)
  -- JS 的每个 case 标签都要是真实格子的标签（拼错的标签永远走不到）
  assertEqual "cellRGB 里有核心不会产生的标签" [] [t | (t, _) <- cases, t `notElem` knownTags]
  let mismatches =
        [ (show cell, js, hs)
        | cell <- cells
        , let js = jsColorOf fn colors elems cell
              hs = cellRGB cell
        , js /= hs
        ]
      render (c, js, hs) = c ++ "：cells.js cellRGB = " ++ showRGB js ++ "，UI.Palette.cellRGB = " ++ showRGB hs
  assertNoDiff "cells.js cellRGB" (map render mismatches)

web_spread_crumbs_match_presentation :: Assertion
web_spread_crumbs_match_presentation = do
  js <- jsRgbObject mainJs "SPREAD_CRUMB_RGB"
  let spreading = [unElementName n | (n, _) <- spreadCurves]
      hs = [(n, rgb) | n <- spreading, Just rgb <- [lookup n elementTable]]
  assertEqual "会蔓延的元素都有碎屑色" (length spreading) (length hs)
  assertNoDiff "main.js SPREAD_CRUMB_RGB" (diffTables "SPREAD_CRUMB_RGB" "elementRGBTable（蔓延元素）" js hs)

web_end_stage_colors_match_presentation :: Assertion
web_end_stage_colors_match_presentation = do
  main' <- stripLineComments <$> readFile mainJs
  render' <- stripLineComments <$> readFile renderJs
  -- 倒计时火星：if (ef.type === "tick") fx.crumbs([r, g, b], ef.cells);
  let tickLine = [l | l <- lines main', "ef.type === \"tick\"" `isInfixOf` l, "fx.crumbs(" `isInfixOf` l]
  case (tickLine, prCrumbs (presentationFor EvTick)) of
    ([l], CrumbsAtSources hs) -> case breakOn "fx.crumbs(" l >>= parseTriple of
      Just (js, _) -> assertEqual "main.js 倒计时火星色 = 表现表 EvTick 的 CrumbsAtSources" hs js
      Nothing -> assertFailure (mainJs ++ "：倒计时火星色不是 [r, g, b]：" ++ trim l)
    (ls, c) -> assertFailure (mainJs ++ "：应恰有一行倒计时火星（找到 " ++ show (length ls) ++ " 行）；表现表 EvTick 碎屑 = " ++ show c)
  -- 生长前沿光：ELEMENT_RGB[name] || [r, g, b]（缺省 = defaultSpreadGlow）
  case [l | l <- lines render', "ELEMENT_RGB[name] ||" `isInfixOf` l] of
    [l] -> case breakOn "ELEMENT_RGB[name] ||" l >>= parseTriple of
      Just (js, _) -> assertEqual "render.js 生长前沿缺省光 = defaultSpreadGlow" defaultSpreadGlow js
      Nothing -> assertFailure (renderJs ++ "：生长前沿缺省光不是 [r, g, b]：" ++ trim l)
    ls -> assertFailure (renderJs ++ "：应恰有一处 ELEMENT_RGB[name] || [r, g, b]（找到 " ++ show (length ls) ++ " 处）")

--------------------------------------------------------------------------------
-- 贴图生成器的调色板

genAssetsPy :: FilePath
genAssetsPy = "tools/gen_assets.py"

-- | 解析 gen_assets.py 的 @GEMS = { "c1": dict(rgb=(r, g, b), …), … }@：[(颜色编号, rgb)]。
pyGemPalette :: IO [(String, RGB)]
pyGemPalette = do
  src <- readFile genAssetsPy
  let block = takeWhile ((/= "}") . trim) (drop 1 (dropWhile ((/= "GEMS = {") . trim) (lines src)))
      entry l = case trim l of
        '"' : 'c' : rest ->
          let (num, tl) = span isDigit rest
          in case breakOn "rgb=(" tl of
               Just r | not (null num) ->
                 case map trim (splitOn ',' (takeWhile (/= ')') r)) of
                   ps@[a, b, c] | all (\p -> not (null p) && all isDigit p) ps ->
                     Just ('c' : num, (fromInteger (read a), fromInteger (read b), fromInteger (read c)))
                   _ -> Nothing
               _ -> Nothing
        _ -> Nothing
      parsed = [(l, entry l) | l <- block, not (all isSpace l)]
  assertBool (genAssetsPy ++ "：找不到 GEMS = { … }") (not (null parsed))
  case [l | (l, Nothing) <- parsed] of
    [] -> pure ()
    bad -> assertFailure (genAssetsPy ++ "：GEMS 里认不出的行：" ++ unlines bad)
  pure [e | (_, Just e) <- parsed]

-- | 宝石贴图的主色（gen_assets.py 的 GEMS）与 UI.Palette.colorRGB 逐项相同（cells.js 的 COLOR_RGB 由上面的用例比对）。
gen_assets_gem_palette_matches_palette :: Assertion
gen_assets_gem_palette_matches_palette = do
  py <- pyGemPalette
  assertNoDiff "tools/gen_assets.py GEMS"
    (diffTables "GEMS" "UI.Palette.colorRGB" py [('c' : show (colorNum c), colorRGB c) | c <- allColors])
