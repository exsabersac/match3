-- | 网页启动时从 wasm 取的表现表（m3Meta，见 web/hs/Match3Web/Api.hs 的 encodeMeta）：网页不再手抄颜色 / 生长曲线 /
-- 帧数 / 音效名，全由这里从桌面的唯一来源（UI.Palette / UI.Presentation / ComboFx）推出。
--
-- 网页 cells.js 的 cellRGB 规则（按格子 JSON 的 t 查）：wmColorTags 里的标签取 COLOR_RGB[c]；wmTagRGB 里的标签取固定色；
-- custom 有 "c"（元素给出的当前颜色，见 Match3.View.cellExtras）取 COLOR_RGB[c]，否则 ELEMENT_RGB[name]；其余 wmFallbackRGB。
-- stack test 的 Spec.WebColors 按同一规则逐格核对它与 UI.Palette.cellRGB 相同。
-- 依赖：Match3.Core、Match3.View、ComboFx、UI.Palette、UI.Presentation。
module UI.WebMeta
  ( WebMeta (..)
  , webMeta
  , metaCellRGB
  ) where

import ComboFx (comboPopLife, fallFrames, scorePopLife, shakeFrames, swapFrames)
import Match3.Core
import Match3.Element.Event (EventKind (..))
import Match3.View (CellField (..), FaceValue (..), cellExtras, cellFace, colorNum)
import UI.Palette (cellRGB, colorRGB)
import UI.Presentation

-- | m3Meta 的内容。
data WebMeta = WebMeta
  { wmColorRGB :: [(Int, RGB)]           -- ^ 五色主色（键 = 颜色编号 1..5）
  , wmElementRGB :: [(String, RGB)]      -- ^ 按元素名取色（elementRGBTable）：生长前沿光 / 自定义格
  , wmColorTags :: [String]              -- ^ 按格子的 "c" 取主色的格子标签
  , wmTagRGB :: [(String, RGB)]          -- ^ 固定颜色的格子标签
  , wmFallbackRGB :: RGB                 -- ^ 其余（未知元素 / 空格）
  , wmSpreadCrumbRGB :: [(String, RGB)]  -- ^ 蔓延碎屑色（spreadCurves 里的元素在 elementRGBTable 的颜色）
  , wmTickCrumbRGB :: Maybe RGB          -- ^ 倒计时火星色（表现表 EvTick 的 CrumbsAtSources；没有 = 不迸）
  , wmSpreadGlow :: RGB                  -- ^ 生长前沿光的缺省色（表里没有的元素）
  , wmSpreadCurves :: [(String, Curve)]  -- ^ 蔓延的生长曲线（表里没有的元素匀速）
  , wmFrames :: [(String, Int)]          -- ^ 帧数常量（swap / fall / shake / comboPop / scorePop）
  , wmSounds :: [SoundName]              -- ^ 音效名
  }

-- | 固定颜色的格子（每个标签一个样本；颜色与层数无关）与按 "c" 取色的格子。
fixedSamples, colorSamples :: [Cell]
fixedSamples = [Stone 1, Chest 1, Honey 1, Cookie, Cake 1, MagicHat, Snail 0 1, Safe 1, Surprise, TimeSpirit]
colorSamples = [Gem C1 Normal 0 Nothing, Balloon C1, Maker C1 1, Flip C1 C2, Bottle C1, Countdown C1 1]

webMeta :: WebMeta
webMeta =
  WebMeta
    { wmColorRGB = [(colorNum c, colorRGB c) | c <- allColors]
    , wmElementRGB = elementTable
    , wmColorTags = map (fst . cellFace) colorSamples
    , wmTagRGB = [(fst (cellFace c), cellRGB c) | c <- fixedSamples]
    , wmFallbackRGB = cellRGB (Custom (ElementName "") (CustomState 0))
    , wmSpreadCrumbRGB = [(n, rgb) | (n, _) <- curves, Just rgb <- [lookup n elementTable]]
    , wmTickCrumbRGB = case prCrumbs (presentationFor EvTick) of
        CrumbsAtSources rgb -> Just rgb
        _ -> Nothing
    , wmSpreadGlow = defaultSpreadGlow
    , wmSpreadCurves = curves
    , wmFrames = [("swap", swapFrames), ("fall", fallFrames), ("shake", shakeFrames), ("comboPop", comboPopLife), ("scorePop", scorePopLife)]
    , wmSounds = soundNames
    }
  where
    elementTable = [(unElementName n, rgb) | (n, rgb) <- elementRGBTable]
    curves = [(unElementName n, c) | (n, c) <- spreadCurves]

-- | 按网页 cells.js 的规则、只用 m3Meta 的表算一格的颜色（Spec.WebColors 用它核对 = UI.Palette.cellRGB）。
metaCellRGB :: WebMeta -> Cell -> RGB
metaCellRGB m cell
  | tag `elem` wmColorTags m = maybe grey id (faceC >>= \c -> lookup c (wmColorRGB m))
  | Just rgb <- lookup tag (wmTagRGB m) = rgb
  | tag == "custom" = case extraC of
      Just c -> maybe grey id (lookup c (wmColorRGB m))
      Nothing -> maybe (wmFallbackRGB m) id (name >>= \n -> lookup n (wmElementRGB m))
  | otherwise = wmFallbackRGB m
  where
    grey = (200, 200, 200)
    (tag, fields) = cellFace cell
    faceC = case lookup "c" fields of
      Just (FieldInt c) -> Just c
      _ -> Nothing
    extraC = case lookup "c" (cellExtras cell) of
      Just (FaceColor c) -> Just (colorNum c)
      Just (FaceInt c) -> Just c
      _ -> Nothing
    name = case lookup "name" fields of
      Just (FieldText n) -> Just n
      _ -> Nothing
