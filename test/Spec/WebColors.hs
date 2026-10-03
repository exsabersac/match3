-- | 网页表现表（m3Meta = UI.WebMeta）与贴图生成器调色板的一致性。
--
-- 网页不再手抄颜色 / 生长曲线 / 帧数 / 音效名：启动时从 wasm 的 m3Meta 读（UI.WebMeta 由 UI.Palette / UI.Presentation /
-- ComboFx 推出）。这里核对：
--   * 按网页 cells.js 的取色规则、只用 m3Meta 的表算每种格子的颜色（UI.WebMeta.metaCellRGB）= UI.Palette.cellRGB
--   * 蔓延碎屑色 / 倒计时火星色 / 生长曲线与表现表一致，每个会蔓延的元素都有碎屑色
--   * 帧数常量、音效名齐全（表现表里出现的音效名都在 soundNames 里）
--   * tools/gen_assets.py 的 GEMS 调色板（宝石贴图的主色）↔ UI.Palette.colorRGB（唯一还要解析源码的一张表）
module Spec.WebColors
  ( tests
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, nub)
import Data.Maybe (mapMaybe)
import Match3.Core
import Match3.Element.Event (EventKind (..))
import Match3.View (cellFace, colorNum)
import Test.Tasty
import Test.Tasty.HUnit
import UI.Palette (cellRGB, colorRGB)
import UI.Presentation
import UI.WebMeta (WebMeta (..), metaCellRGB, webMeta)

tests :: [TestTree]
tests =
  [ testCase "web_meta_cell_rgb_matches_palette" web_meta_cell_rgb_matches_palette
  , testCase "web_meta_spread_matches_presentation" web_meta_spread_matches_presentation
  , testCase "web_meta_frames_and_sounds" web_meta_frames_and_sounds
  , testCase "gen_assets_gem_palette_matches_palette" gen_assets_gem_palette_matches_palette
  ]

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

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

--------------------------------------------------------------------------------
-- m3Meta

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

elementTable :: [(String, RGB)]
elementTable = [(unElementName n, rgb) | (n, rgb) <- elementRGBTable]

-- | 每种格子各取几个样本（带颜色的取全部五色、带层数的取几档；自定义格取颜色表里的每个名字、变色龙的五种颜色、
-- 雪怪 Boss、一个未知名字）。
sampleCells :: [Cell]
sampleCells =
  [Gem c k 0 Nothing | c <- allColors, k <- [Normal, LineH, Bomb]]
    ++ [Gem C2 Normal 1 (Just Grass)]
    ++ [Stone n | n <- [1 .. 3]] ++ [Chest n | n <- [1, 2]] ++ [Honey 1, Cookie] ++ [Cake n | n <- [1 .. 4]]
    ++ [MagicHat, Snail 0 1, Snail 1 0] ++ [Safe n | n <- [1, 2]] ++ [Surprise, TimeSpirit]
    ++ concat [[Balloon c, Maker c 3, Flip c (succColor c), Bottle c, Countdown c 5] | c <- allColors]
    ++ [Custom (ElementName n) (CustomState 0) | n <- nub (map fst elementTable ++ ["snow_boss", "no_such_element"])]
    ++ [Custom (ElementName "chameleon") (CustomState i) | i <- [0 .. 4]]
  where
    succColor c = if c == maxBound then minBound else succ c

web_meta_cell_rgb_matches_palette :: Assertion
web_meta_cell_rgb_matches_palette = do
  let m = webMeta
      knownTags = nub (map (fst . cellFace) sampleCells)
      metaTags = wmColorTags m ++ map fst (wmTagRGB m)
  assertEqual "五色主色" [(colorNum c, colorRGB c) | c <- allColors] (wmColorRGB m)
  assertEqual "元素颜色表" elementTable (wmElementRGB m)
  -- 表里的每个标签都是真实格子的标签（拼错的标签永远走不到），且一个标签只归一类
  assertEqual "m3Meta 里有核心不会产生的标签" [] [t | t <- metaTags, t `notElem` knownTags]
  assertEqual "标签重复" metaTags (nub metaTags)
  -- 每个内置本体标签都归了类（custom 走自定义规则）
  assertEqual "m3Meta 没覆盖的格子标签" [] [t | t <- knownTags, t `notElem` ("custom" : metaTags)]
  let mismatches = [(show cell, js, hs) | cell <- sampleCells, let js = metaCellRGB m cell; hs = cellRGB cell, js /= hs]
      render (c, js, hs) = c ++ "：m3Meta 规则 = " ++ showRGB js ++ "，UI.Palette.cellRGB = " ++ showRGB hs
  assertNoDiff "m3Meta 的格子取色" (map render mismatches)

web_meta_spread_matches_presentation :: Assertion
web_meta_spread_matches_presentation = do
  let m = webMeta
      spreading = [unElementName n | (n, _) <- spreadCurves]
      hs = [(n, rgb) | n <- spreading, Just rgb <- [lookup n elementTable]]
  assertEqual "会蔓延的元素都有碎屑色" (length spreading) (length hs)
  assertNoDiff "m3Meta spreadCrumbRGB" (diffTables "spreadCrumbRGB" "elementRGBTable（蔓延元素）" (wmSpreadCrumbRGB m) hs)
  assertEqual "生长曲线" [(unElementName n, c) | (n, c) <- spreadCurves] (wmSpreadCurves m)
  assertEqual "生长前沿缺省光 = defaultSpreadGlow" defaultSpreadGlow (wmSpreadGlow m)
  case prCrumbs (presentationFor EvTick) of
    CrumbsAtSources rgb -> assertEqual "倒计时火星色 = 表现表 EvTick 的 CrumbsAtSources" (Just rgb) (wmTickCrumbRGB m)
    c -> assertFailure ("表现表 EvTick 的碎屑不再是 CrumbsAtSources：" ++ show c ++ "（网页 main.js 按 tickCrumbRGB 迸火星，要一起改）")

web_meta_frames_and_sounds :: Assertion
web_meta_frames_and_sounds = do
  let m = webMeta
      frames = wmFrames m
  assertEqual "帧数常量（网页 render.js / main.js 读这几个键）" ["swap", "fall", "shake", "comboPop", "scorePop"] (map fst frames)
  assertBool ("帧数都 > 0：" ++ show frames) (all ((> 0) . snd) frames)
  assertEqual "音效名不重复" (wmSounds m) (nub (wmSounds m))
  let used = nub (mapMaybe effectSound [minBound .. maxBound])
  assertEqual "表现表的音效名都在 soundNames 里" [] [s | s <- used, s `notElem` wmSounds m]

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
