{-# LANGUAGE OverloadedStrings #-}
-- | 第 10 刀：前端表现表（app/pure/UI/Presentation.hs）。音效钩子 UI.Sound 与只测桌面绘制的用例随 SDL2 前端移除（refactor/web-only）。
--
-- 对照的旧实现是本文件里的字面副本：第 10 刀前散在 ComboFx（endStageTable / 帧数常量 / comboStyle）、
-- UI.EndStage（spreadProgress、倒计时 / 洗牌颜色）、UI.Playback（endCrumbTable）、UI.BoardArt（waveTint）、
-- UI.HudArt / UI.HudPrim（得分浮字颜色）、UI.Layout（elementRGBTable / 缓动）里的 case 与常量（这些桌面模块都已移除）。
-- 只给原桌面版用的贴图名列（prSprite）与轮内取色函数（clearTint / scorePopRGB）随桌面遗留清理删除（refactor/web-only-2）。
-- 另有源码扫描：这些散落的表与颜色字面量已收进表现表。
module Spec.Presentation
  ( tests
  ) where

import ComboFx
import Data.List (isInfixOf, isPrefixOf)
import Data.Word (Word8)
import Match3.Core
import Match3.Element.Event (EventKind (..))
import Spec.Support.Source (readCode, sourcesUnder)
import Test.Tasty
import Test.Tasty.HUnit
import UI.Presentation

tests :: [TestTree]
tests =
  [ testCase "presentation_table_covers_every_event_kind" presentation_table_covers_every_event_kind
  , testCase "presentation_stage_rows_match_legacy_end_stage_table" presentation_stage_rows_match_legacy_end_stage_table
  , testCase "presentation_frames_colors_match_legacy_constants" presentation_frames_colors_match_legacy_constants
  , testCase "presentation_spread_curves_match_legacy" presentation_spread_curves_match_legacy
  , testCase "presentation_combo_style_matches_legacy" presentation_combo_style_matches_legacy
  , testCase "presentation_extension_defaults" presentation_extension_defaults
  , testCase "effect_sound_names_clear_and_special" effect_sound_names_clear_and_special
  , testCase "presentation_scattered_cases_removed" presentation_scattered_cases_removed
  ]

allKinds :: [EventKind]
allKinds = [minBound .. maxBound]

-- | 每种内置事件恰好一行，且按 EventKind 的定义顺序排列。
presentation_table_covers_every_event_kind :: Assertion
presentation_table_covers_every_event_kind = do
  map fst presentationTable @?= allKinds
  -- 每个步末表现段都恰好被一种事件使用（网页 render.js 的 drawEndStage 按段分派）
  [s | (_, p) <- presentationTable, LookStage s <- [prLook p]] @?= [minBound .. maxBound]
  mapM_ (\k -> assertBool ("frames >= 0: " ++ show k) (prFrames (presentationFor k) >= 0)) allKinds

-- 第 10 刀前 ComboFx 的 endStageTable / endStageBase / stageKindFor（逐字副本）。
legacyEndStageTable :: [(EventKind, (StageKind, Int))]
legacyEndStageTable =
  [ (EvTick, (StTick, 10))
  , (EvBelt, (StBelt, 14))
  , (EvSpread, (StSpread, 18))
  , (EvMove, (StSnail, 18))
  , (EvShuffle, (StShuffle, 22))
  ]

legacyEndStageBase :: StageKind -> Int
legacyEndStageBase k = case [n | (_, (k', n)) <- legacyEndStageTable, k' == k] of
  n : _ -> n
  [] -> 18

legacyStageKindFor :: EventKind -> StageKind
legacyStageKindFor ev = maybe StSpread fst (lookup ev legacyEndStageTable)

presentation_stage_rows_match_legacy_end_stage_table :: Assertion
presentation_stage_rows_match_legacy_end_stage_table = do
  mapM_ (\k -> (show k, stageKindOf k, stageKindFor k) @?= (show k, legacyStageKindFor k, legacyStageKindFor k)) allKinds
  mapM_ (\s -> (show s, stageFrames s, endStageBase s) @?= (show s, legacyEndStageBase s, legacyEndStageBase s)) [minBound .. maxBound]

-- | 帧数、颜色与第 10 刀前的常量 / case 分支相同。
presentation_frames_colors_match_legacy_constants :: Assertion
presentation_frames_colors_match_legacy_constants = do
  -- ComboFx：waveFlashFrames = 12、scorePopLife = 48、comboPopLife = 54
  (waveFlashFrames, scorePopLife, comboPopLife) @?= (12, 48, 54)
  -- 第 1 轮高亮柔白 (255, 250, 220)、得分浮字 (255, 244, 200)（原 UI.BoardArt.waveTint / UI.HudArt 的字面量）
  (presentationRGB (presentationFor EvClear), presentationRGB (presentationFor EvScore)) @?= ((255, 250, 220), (255, 244, 200))
  -- 原 UI.EndStage：倒计时红光 (255, 90, 60)；洗牌 (200, 150, 255)
  let st = stagePresentation
  presentationRGB (st StTick) @?= (255, 90, 60)
  presentationRGB (st StShuffle) @?= (200, 150, 255)
  -- 原 UI.Playback.endCrumbTable：蔓延按元素名取色、倒计时 (255, 110, 70)，其余不迸
  map (prCrumbs . st) [minBound .. maxBound]
    @?= [CrumbsAtSources (255, 110, 70), NoCrumbs, CrumbsByElement, NoCrumbs, NoCrumbs]
  -- elementRGBTable：前五行是搬来时的逐字副本；magic_stone / fuzzball 是审计第 8 项按网页 ELEMENT_RGB 补上的
  elementRGBTable
    @?= [ ("vine", (110, 220, 90))
        , ("choco", (150, 90, 45))
        , ("steam", (225, 225, 235))
        , ("jelly", (240, 110, 180))
        , ("bubble", (150, 215, 250))
        , ("magic_stone", (92, 60, 160))
        , ("fuzzball", (196, 150, 170))
        ]
  -- 缓动（UI.Layout 的旧定义）
  mapM_ (\t -> (smoothT t, easeOutT t) @?= (legacySmoothT t, legacyEaseOutT t)) samples

samples :: [Double]
samples = [-0.25, 0, 1e-9] ++ [fromIntegral i / 97 | i <- [1 .. 96 :: Int]] ++ [0.25, 0.5, 0.75, 1 - 1e-9, 1, 1.3]

legacySmoothT :: Double -> Double
legacySmoothT x = let y = max 0 (min 1 x) in y * y * (3 - 2 * y)

legacyEaseOutT :: Double -> Double
legacyEaseOutT x = let y = max 0 (min 1 x) in 1 - (1 - y) * (1 - y)

-- 第 10 刀前 UI.EndStage 的 spreadProgress（逐字副本）与缺省 `maybe t ($ t) (lookup name spreadProgress)`。
legacySpreadProgress :: [(ElementName, Double -> Double)]
legacySpreadProgress =
  [ ( "vine"
    , \t ->
        let u = t * 4
            seg = fromIntegral (floor u :: Int)
        in min 1 ((seg + legacySmoothT (u - seg)) / 4)
    )
  , ("choco", legacyEaseOutT)
  , ("steam", id)
  ]

presentation_spread_curves_match_legacy :: Assertion
presentation_spread_curves_match_legacy =
  mapM_
    ( \(name, t) ->
        (name, t, curveAt (spreadCurveFor name) t) @?= (name, t, maybe t ($ t) (lookup name legacySpreadProgress))
    )
    [(name, t) | name <- ["vine", "choco", "steam", "moss", ""], t <- samples]

-- 第 10 刀前 ComboFx 的 comboStyle / styleRGB / hsv（逐字副本，只留字段元组）。
legacyComboStyle :: Int -> ((Word8, Word8, Word8), Int, Int, Bool)
legacyComboStyle k
  | k <= 2 = ((255, 238, 150), 30, 2, False)
  | k == 3 = ((255, 164, 52), 36, 3, False)
  | k == 4 = ((255, 76, 64), 42, 4, False)
  | otherwise = ((214, 120, 255), 48, 5, True)

legacyStyleRGB :: ((Word8, Word8, Word8), Int, Int, Bool) -> Int -> (Word8, Word8, Word8)
legacyStyleRGB (rgb, _, _, rainbow) pulse
  | not rainbow = rgb
  | otherwise =
      let h = fromIntegral ((pulse * 9) `mod` 360) :: Double
      in legacyHsv h 0.55 1.0

legacyHsv :: Double -> Double -> Double -> (Word8, Word8, Word8)
legacyHsv h s v =
  let c = v * s
      hp = h / 60
      x = c * (1 - abs ((hp - 2 * fromIntegral (floor (hp / 2) :: Int)) - 1))
      (r1, g1, b1)
        | hp < 1 = (c, x, 0)
        | hp < 2 = (x, c, 0)
        | hp < 3 = (0, c, x)
        | hp < 4 = (0, x, c)
        | hp < 5 = (x, 0, c)
        | otherwise = (c, 0, x)
      m = v - c
      to8 u = fromIntegral (max 0 (min 255 (round ((u + m) * 255) :: Int)))
  in (to8 r1, to8 g1, to8 b1)

presentation_combo_style_matches_legacy :: Assertion
presentation_combo_style_matches_legacy =
  mapM_
    ( \(k, pulse) -> do
        let cs = comboStyle k
        ((csRGB cs, csHeight cs, csShake cs, csRainbow cs), styleRGB cs pulse)
          @?= (legacyComboStyle k, legacyStyleRGB (legacyComboStyle k) pulse)
    )
    [(k, pulse) | k <- [-1 .. 9], pulse <- [0 .. 80] ++ [399, 4000]]

-- | 扩展元素：事件种类封闭，扩展元素的步末效果落在已有种类的那一行；按元素名细分的表里没有的名字用明确的缺省。
presentation_extension_defaults :: Assertion
presentation_extension_defaults = do
  -- 整张表查不到的种类（未来新增的 EventKind 忘了加行）：蔓延段、18 帧、无颜色 / 碎屑 / 音效
  defaultPresentation @?= Presentation (LookStage StSpread) 18 Nothing NoCrumbs Nothing
  mapM_ (\k -> presentationIn [] k @?= defaultPresentation) allKinds
  mapM_ (\k -> presentationIn presentationTable k @?= presentationFor k) allKinds
  presentationRGB defaultPresentation @?= (255, 255, 255)
  -- 扩展元素的蔓延（例如 "moss"）：匀速生长、白色前沿柔光、不迸碎屑（elementRGBTable 查不到）
  spreadCurveFor "moss" @?= defaultSpreadCurve
  defaultSpreadCurve @?= CurveLinear
  spreadGlowFor "moss" @?= defaultSpreadGlow
  defaultSpreadGlow @?= (255, 255, 255)
  lookup "moss" elementRGBTable @?= Nothing
  -- 扩展元素的会走效果（Spec.Extension 的 "hopper" 产出 EvMove）：按蜗牛段播放
  stageKindOf EvMove @?= StSnail
  -- 内置的藤 / 巧 / 蒸汽各有曲线与颜色
  mapM_ (\n -> assertBool (show n) (lookup n spreadCurves /= Nothing && lookup n elementRGBTable /= Nothing)) ["vine", "choco", "steam"]

-- | 消除与爆炸有音效名，其余事件无声。
effect_sound_names_clear_and_special :: Assertion
effect_sound_names_clear_and_special = do
  effectSound EvClear @?= Just "clear"
  effectSound EvBlast @?= Just "special"
  map effectSound (filter (`notElem` [EvClear, EvBlast]) allKinds) @?= map (const Nothing) (filter (`notElem` [EvClear, EvBlast]) allKinds)
  prSound defaultPresentation @?= Nothing

-- | 源码扫描：散落的表 / case 已收进表现表；搬走的颜色字面量只在 UI.Presentation 里出现。
presentation_scattered_cases_removed :: Assertion
presentation_scattered_cases_removed = do
  files <- sourcesUnder "app"
  let others = filter (/= "app/pure/UI/Presentation.hs") files
  codes <- mapM (\f -> (,) f <$> readCode f) others
  let tops c = [takeWhile (/= ' ') l | l <- lines c, not (null l), not (" " `isPrefixOf` l)]
  mapM_
    ( \(f, c) ->
        mapM_
          ( \nm -> assertBool (f ++ " still defines " ++ nm) (nm `notElem` tops c)
          )
          ["endStageTable", "spreadProgress", "endCrumbTable", "elementRGBTable", "comboStyle", "styleRGB", "hsv", "smoothT", "easeOutT"]
    )
    codes
  let literals =
        [ "255, 90, 60", "255 90 60", "255, 110, 70", "200, 150, 255", "200 150 255"
        , "255, 244, 200", "255, 250, 220", "255 250 220", "\"snail\" (rect"
        ]
  mapM_
    (\(f, c) -> mapM_ (\lit -> assertBool (f ++ " still has " ++ lit) (not (lit `isInfixOf` c))) literals)
    codes
  -- 表现表所在模块是纯模块：不 import SDL
  pres <- readFile "app/pure/UI/Presentation.hs"
  assertBool "UI.Presentation is pure" (not ("import SDL" `isInfixOf` pres))
