{-# LANGUAGE OverloadedStrings #-}

-- | 效果事件 → 前端表现的一张表（纯数据，不含 SDL）。
--
-- 规则层的每种效果事件（Match3.Element.Event.EventKind）在这里查到一条 'Presentation'：表现方式（轮内的
-- 高亮消失 / 得分浮字 / 连击弹字，或步末的某个表现段 'StageKind'）、基础帧数、主色、步末碎屑、音效名。
-- 帧数被 ComboFx 的时间线读取；碎屑、生长曲线、元素颜色与音效名经 UI.WebMeta（m3Meta）给网页；
-- 各表现段怎么画在网页 web/www/render.js（按 'StageKind' 分派）。
--
-- 扩展元素：事件种类是封闭的（EventKind），扩展元素的步末效果也落在某个种类上；按元素名细分的表
-- （'spreadCurves' 生长曲线、'elementRGBTable' 颜色）里没有的名字用明确的缺省（'defaultSpreadCurve' /
-- 'defaultSpreadGlow'，不迸碎屑）。整张表里查不到的种类用 'defaultPresentation'（蔓延段、18 帧）。
--
-- 音效：'effectSound' 查表项的音效名（内置：消除 clear、爆炸 special，其余无声）；本模块不播放，网页按事件选音效并播放（web/www/main.js）。
module UI.Presentation
  ( -- * 表
    RGB
  , SoundName
  , StageKind (..)
  , Look (..)
  , Crumbs (..)
  , Presentation (..)
  , presentationTable
  , presentationFor
  , presentationIn
  , defaultPresentation
    -- * 步末表现段
  , stageKindOf
  , stageFrames
  , stagePresentation
  , presentationRGB
    -- * 音效钩子
  , effectSound
  , soundNames
    -- * 按元素名细分
  , Curve (..)
  , curveAt
  , spreadCurves
  , defaultSpreadCurve
  , spreadCurveFor
  , elementRGBTable
  , defaultSpreadGlow
  , spreadGlowFor
    -- * 连击等级样式
  , ComboStyle (..)
  , comboStyle
  , styleRGB
    -- * 缓动
  , smoothT
  , easeOutT
  ) where

import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Match3.Element.Event (EventKind (..))
import Match3.Core (ElementName)

-- | 颜色（红, 绿, 蓝）。
type RGB = (Word8, Word8, Word8)

-- | 音效名（网页按名字播放 sfx/<名字>.wav）。
type SoundName = String

-- | 步末阶段的种类（同一时刻连续的藤 / 巧 / 蒸汽合并为一个 StSpread 同时播放）。
data StageKind = StTick | StBelt | StSpread | StSnail | StShuffle
  deriving (Eq, Show, Enum, Bounded)

-- | 表现方式。
data Look
  = LookClear           -- ^ 轮内：高亮（压暗 + 光圈闪两下）→ 消失（缩小淡出 + 光环 + 粒子）
  | LookScore           -- ^ 轮内：本轮得分浮字
  | LookCombo           -- ^ 轮内：「连击 xN」弹字 + 震屏，播完后 HUD「N 连击！」总结
  | LookWithClear       -- ^ 轮内：随消除一起表现，没有单独的动画（爆炸 / 波及 / 底收）
  | LookStage StageKind -- ^ 步末：一个表现段（绘制见网页 render.js 的 drawEndStage）
  deriving (Eq, Show)

-- | 进入步末表现段时迸出的碎屑。
data Crumbs
  = NoCrumbs
  | CrumbsAtSources RGB -- ^ 本段每个来源格一撮固定颜色的碎屑（倒计时的火星）
  | CrumbsByElement     -- ^ 每个生长目标格一撮、颜色按步末效果的元素名查 'elementRGBTable'（表里没有的名字不迸）
  deriving (Eq, Show)

-- | 一种效果事件的前端表现。
data Presentation = Presentation
  { prLook   :: Look
  , prFrames :: Int              -- ^ 基础帧数（1 帧 ≈ 16.7 ms；0 = 不单独占时长）
  , prColor  :: Maybe RGB        -- ^ 固定主色（Nothing = 按格子 / 连击等级 / 元素名取色）
  , prCrumbs :: Crumbs
  , prSound  :: Maybe SoundName  -- ^ 音效名；消除 clear、爆炸 special，其余无声
  } deriving (Eq, Show)

-- | 只给表现方式与帧数，其余为空。
look :: Look -> Int -> Presentation
look l n = Presentation l n Nothing NoCrumbs Nothing

-- | 效果事件 → 前端表现（按 EventKind 的定义顺序，每种一行）。1 帧 ≈ 16.7 ms：
-- 高亮 12（≈ 200 ms）、得分浮字 48（≈ 0.8 s）、连击弹字 54（≈ 0.9 s）；步末倒计时减一 10（≈ 170 ms）、
-- 皮带移位 14（≈ 230 ms）、蔓延 18（≈ 300 ms，藤 / 巧 / 蒸汽同时长出）、会走的元素（蜗牛）18、自动洗牌 22（≈ 370 ms）。
presentationTable :: [(EventKind, Presentation)]
presentationTable =
  [ (EvClear, (look LookClear 12) {prColor = Just (255, 250, 220), prSound = Just "clear"}) -- 第 1 轮柔白光圈，连击轮用等级色
  , (EvHit, look LookWithClear 0)
  , (EvBlast, (look LookWithClear 0) {prSound = Just "special"})
  , (EvDrain, look LookWithClear 0)
  , (EvScore, (look LookScore 48) {prColor = Just (255, 244, 200)}) -- 第 1 轮的得分色，连击轮用等级色
  , (EvCombo, look LookCombo 54)
  , (EvTick, (look (LookStage StTick) 10) {prColor = Just (255, 90, 60), prCrumbs = CrumbsAtSources (255, 110, 70)})
  , (EvBelt, look (LookStage StBelt) 14)
  , (EvSpread, (look (LookStage StSpread) 18) {prCrumbs = CrumbsByElement})
  , (EvMove, look (LookStage StSnail) 18)
  , (EvShuffle, (look (LookStage StShuffle) 22) {prColor = Just (200, 150, 255)})
  ]

-- | 表里没有的事件种类：按蔓延段、18 帧播放。
defaultPresentation :: Presentation
defaultPresentation = look (LookStage StSpread) 18

-- | 查内置表。
presentationFor :: EventKind -> Presentation
presentationFor = presentationIn presentationTable

-- | 在给定的表里查；查不到用 'defaultPresentation'（测试用它锁定缺省表现）。
presentationIn :: [(EventKind, Presentation)] -> EventKind -> Presentation
presentationIn table k = fromMaybe defaultPresentation (lookup k table)

-- | 步末效果的事件种类 → 表现段（不是步末表现的种类也按蔓延段播放，同 'defaultPresentation'）。
stageKindOf :: EventKind -> StageKind
stageKindOf k = case prLook (presentationFor k) of
  LookStage s -> s
  _ -> StSpread

-- | 表现段的那一行（没有任何事件用这个段时为 'defaultPresentation'）。
stagePresentation :: StageKind -> Presentation
stagePresentation s = case [p | (_, p) <- presentationTable, prLook p == LookStage s] of
  p : _ -> p
  [] -> defaultPresentation

-- | 表现段的基础帧数。
stageFrames :: StageKind -> Int
stageFrames = prFrames . stagePresentation

-- | 表项的固定主色；没填主色的表项（扩展元素的缺省表现也是）用白色。
presentationRGB :: Presentation -> RGB
presentationRGB = fromMaybe defaultSpreadGlow . prColor

-- | 音效钩子：效果事件 → 音效名（消除 clear、爆炸 special，其余 Nothing）。
effectSound :: EventKind -> Maybe SoundName
effectSound = prSound . presentationFor

-- | 全部音效名（素材 sfx/<名字>.wav；bgm 是循环背景音乐，其余是一次性音效）：网页经 m3Meta 取。
soundNames :: [SoundName]
soundNames = ["swap", "clear", "special", "illegal", "win", "lose", "bgm"]

--------------------------------------------------------------------------------
-- 按元素名细分

-- | 生长曲线（t ∈ [0,1] → 露出比例）。
data Curve
  = CurveLinear       -- ^ 匀速
  | CurveEaseOut      -- ^ 先快后慢
  | CurveSegments Int -- ^ 分 n 段一节一节伸长（每段内 smoothstep）
  deriving (Eq, Show)

curveAt :: Curve -> Double -> Double
curveAt c t = case c of
  CurveLinear -> t
  CurveEaseOut -> easeOutT t
  CurveSegments n ->
    let k = fromIntegral n
        u = t * k
        seg = fromIntegral (floor u :: Int)
    in min 1 ((seg + smoothT (u - seg)) / k)

-- | 蔓延的生长曲线（按元素名）：藤蔓分 4 段一节一节伸长；巧克力先快后慢；蒸汽匀速。
spreadCurves :: [(ElementName, Curve)]
spreadCurves =
  [ ("vine", CurveSegments 4)
  , ("choco", CurveEaseOut)
  , ("steam", CurveLinear)
  ]

-- | 表里没有的元素（扩展元素的蔓延）匀速生长。
defaultSpreadCurve :: Curve
defaultSpreadCurve = CurveLinear

spreadCurveFor :: ElementName -> Curve
spreadCurveFor n = fromMaybe defaultSpreadCurve (lookup n spreadCurves)

-- | 按元素名取色：步末效果（事件 evElement / endEffectElement 的键）藤 / 巧 / 蒸汽的蔓延色与碎屑色；
-- 也给名字目标与自定义格取色（果冻 / 气泡 / 魔法石 / 毛球，见 UI.Palette.namedRGB / cellRGB，各自有缺省色）。
-- 魔法石 / 毛球取自原桌面版几何画法（UI.Cell.Prim，已随 SDL2 前端移除）的主体色。网页 web/www/cells.js 的 ELEMENT_RGB 是同一张表（test/Spec/WebColors.hs 比对）。
elementRGBTable :: [(ElementName, RGB)]
elementRGBTable =
  [ ("vine", (110, 220, 90))
  , ("choco", (150, 90, 45))
  , ("steam", (225, 225, 235))
  , ("jelly", (240, 110, 180))
  , ("bubble", (150, 215, 250))
  , ("magic_stone", (92, 60, 160))
  , ("fuzzball", (196, 150, 170))
  ]

-- | 表里没有的元素：生长前沿的柔光为白色（碎屑不迸，见 'CrumbsByElement'）。
defaultSpreadGlow :: RGB
defaultSpreadGlow = (255, 255, 255)

spreadGlowFor :: ElementName -> RGB
spreadGlowFor n = fromMaybe defaultSpreadGlow (lookup n elementRGBTable)

--------------------------------------------------------------------------------
-- 连击等级样式（ComboFx 再导出）

-- | 等级越高：字越大、颜色越暖越亮、震屏略大（克制：最多 5 px）。
data ComboStyle = ComboStyle
  { csRGB     :: RGB  -- ^ 主色
  , csHeight  :: Int  -- ^ 「连击」字高（逻辑像素，弹出放大前）
  , csShake   :: Int  -- ^ 震屏振幅（逻辑像素）
  , csRainbow :: Bool -- ^ x5+：彩色流转
  }

comboStyle :: Int -> ComboStyle
comboStyle k
  | k <= 2 = ComboStyle (255, 238, 150) 30 2 False -- x2 白黄
  | k == 3 = ComboStyle (255, 164, 52) 36 3 False  -- x3 橙
  | k == 4 = ComboStyle (255, 76, 64) 42 4 False   -- x4 红
  | otherwise = ComboStyle (214, 120, 255) 48 5 True -- x5+ 紫 / 彩

-- | 当前帧颜色：x5+ 在紫色基础上做色相流转（彩虹感），其它等级固定。
styleRGB :: ComboStyle -> Int -> RGB
styleRGB st pulse
  | not (csRainbow st) = csRGB st
  | otherwise =
      let h = fromIntegral ((pulse * 9) `mod` 360) :: Double
          (r, g, b) = hsv h 0.55 1.0
      in (r, g, b)

hsv :: Double -> Double -> Double -> RGB
hsv h s v =
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

--------------------------------------------------------------------------------
-- 缓动（生长曲线 'curveAt' 用）

-- | smoothstep 缓动（两端慢）。
smoothT :: Double -> Double
smoothT x = let y = max 0 (min 1 x) in y * y * (3 - 2 * y)

-- | 先快后慢的缓动。
easeOutT :: Double -> Double
easeOutT x = let y = max 0 (min 1 x) in 1 - (1 - y) * (1 - y)
