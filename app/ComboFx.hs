-- | 连击（连锁）表现层的纯逻辑：逐轮回放的阶段机与时间线、下落映射、连击等级样式、浮字曲线。
-- 只描述「怎么播」，不含任何 SDL 绘制（绘制见 Main.hs 的 drawCascade* / drawPops*），
-- 也不改规则：回放脚本来自 Match3.Game 的 trace*，结算结果仍以 trySwap 等为准。
module ComboFx
  ( -- * 时间线（帧；主循环固定 60 fps 步长，1 帧 ≈ 16.7 ms）
    waveFlashFrames
  , wavePopFrames
  , waveRestFrames
  , fallFramesFor
  , fastStep
  , comboPopLife
  , scorePopLife
  , comboSummaryFrames
  , shakeFrames
    -- * 逐轮回放阶段机
  , WavePhase (..)
  , Cascade (..)
  , CascadeEvent (..)
  , StepResult (..)
  , newCascade
  , stepPlayback
  , phaseLen
  , phaseT
    -- * 下落映射
  , fallTable
    -- * 连击等级样式
  , ComboStyle (..)
  , comboStyle
  , styleRGB
    -- * 浮字（连击提示 / 本轮得分）
  , PopKind (..)
  , TextPop (..)
  , tickPops
  , comboPopScale
  , popAlpha
  , popRise
  , scorePopScale
  , clearedAnchor
  ) where

import Data.List (transpose)
import Data.Word (Word8)
import Match3.Core

--------------------------------------------------------------------------------
-- 时间线
--------------------------------------------------------------------------------

-- | 被消格高亮闪烁停留（≈ 200 ms）：玩家在这一段看清「这一轮消的是哪些格」。
waveFlashFrames :: Int
waveFlashFrames = 12

-- | 被消格缩小消失（≈ 100 ms），同时迸出粒子、弹出本轮得分。
wavePopFrames :: Int
wavePopFrames = 6

-- | 落定后的短停顿（≈ 67 ms），让下一轮的高亮与上一轮的下落分得开。
waveRestFrames :: Int
waveRestFrames = 4

-- | 下落 + 补子时长：8..14 帧（≈ 133..233 ms），落差越大越长。
fallFramesFor :: Int -> Int
fallFramesFor maxDrop = max 8 (min 14 (6 + maxDrop))

-- | 点击加速：每帧推进的回放帧数。
fastStep :: Int
fastStep = 3

-- | 「连击 xN」弹字寿命（≈ 0.9 s，放大弹出 → 停留 → 淡出）。
comboPopLife :: Int
comboPopLife = 54

-- | 本轮得分浮字寿命（≈ 0.8 s）。
scorePopLife :: Int
scorePopLife = 48

-- | 连锁结束后 HUD「N 连击！」总结的显示时长（≈ 1.6 s）。
comboSummaryFrames :: Int
comboSummaryFrames = 96

-- | 震屏持续帧数（振幅随时间线性衰减）。
shakeFrames :: Int
shakeFrames = 10

--------------------------------------------------------------------------------
-- 阶段机
--------------------------------------------------------------------------------

-- | 一轮的四个阶段；PhStart 只是「尚未进入第一轮」的占位（长度 0）。
data WavePhase = PhStart | PhFlash | PhPop | PhFall | PhRest
  deriving (Eq, Show)

-- | 一步操作的回放进度。
data Cascade = Cascade
  { cWaves :: [CascadeWave] -- ^ 尚未播完的轮次，head 为当前轮
  , cPhase :: WavePhase
  , cFrame :: Int
  , cCombo :: Int           -- ^ 当前轮的连击序号（从 1 起，只计有消除的轮）
  , cBest  :: Int           -- ^ 已播到的最高连击
  , cGain  :: Int           -- ^ 已消失轮次的累计得分（HUD 分数滚动）
  , cBase  :: Int           -- ^ 本步之前的总分
  , cFinal :: Board         -- ^ 结算后的真实盘面（含蔓延 / 蜗牛 / 自动洗牌）
  , cShown :: Board         -- ^ 最近一次落定的盘面
  , cFast  :: Bool          -- ^ 玩家点击加速
  }

-- | 阶段切换时前端要做的一次性动作。
data CascadeEvent
  = EvHighlight Int CascadeWave -- ^ 进入高亮：k ≥ 2 时弹「连击 xk」
  | EvVanish Int CascadeWave    -- ^ 进入消失：粒子 + 本轮得分浮字 + 震屏（k ≥ 2）

data StepResult = Continue Cascade | Finished Cascade

newCascade :: MoveTrace -> Board -> Int -> Cascade
newCascade mt final base =
  Cascade
    { cWaves = mtWaves mt
    , cPhase = PhStart
    , cFrame = 0
    , cCombo = 0
    , cBest = 0
    , cGain = 0
    , cBase = base
    , cFinal = final
    , cShown = mtStart mt
    , cFast = False
    }

phaseLen :: WavePhase -> CascadeWave -> Int
phaseLen ph w = case ph of
  PhStart -> 0
  PhFlash -> waveFlashFrames
  PhPop -> wavePopFrames
  PhFall -> fallFramesFor (maximum (0 : [d | row <- fallTable w, (d, _) <- row]))
  PhRest -> waveRestFrames

-- | 当前阶段进度 0..1。
phaseT :: Cascade -> Double
phaseT c = case cWaves c of
  (w : _) ->
    let n = phaseLen (cPhase c) w
    in if n <= 0 then 1 else min 1 (fromIntegral (cFrame c) / fromIntegral n)
  [] -> 1

-- | 推进一帧（加速时一次推进 fastStep 帧）。
stepPlayback :: Cascade -> (StepResult, [CascadeEvent])
stepPlayback c = case cWaves c of
  [] -> (Finished c, [])
  (w : _) ->
    let f = cFrame c + (if cFast c then fastStep else 1)
    in if f < phaseLen (cPhase c) w
         then (Continue c {cFrame = f}, [])
         else advance c

advance :: Cascade -> (StepResult, [CascadeEvent])
advance c = case (cPhase c, cWaves c) of
  (_, []) -> (Finished c, [])
  (PhStart, _) -> enterWave c
  (PhFlash, w : _) ->
    (Continue c {cPhase = PhPop, cFrame = 0, cGain = cGain c + cwScore w}, [EvVanish (cCombo c) w])
  (PhPop, _) -> (Continue c {cPhase = PhFall, cFrame = 0}, [])
  (PhFall, w : _) -> (Continue c {cPhase = PhRest, cFrame = 0, cShown = cwAfter w}, [])
  (PhRest, _ : rest) -> enterWave c {cWaves = rest}

-- | 进入 head 轮：没有被消格的轮（皮带沉降收饼干等）直接下落，不计连击。
enterWave :: Cascade -> (StepResult, [CascadeEvent])
enterWave c = case cWaves c of
  [] -> (Finished c, [])
  (w : _)
    | null (cwCleared w) ->
        (Continue c {cPhase = PhFall, cFrame = 0, cGain = cGain c + cwScore w}, [])
    | otherwise ->
        let k = cCombo c + 1
        in ( Continue c {cPhase = PhFlash, cFrame = 0, cCombo = k, cBest = max (cBest c) k}
           , [EvHighlight k w]
           )

--------------------------------------------------------------------------------
-- 下落映射
--------------------------------------------------------------------------------

-- | 每格 (落定前向上偏移的行数, 是否新补的格)，按 [行][列] 索引。
-- 按列复现重力：固定不动的格（染色瓶 / 果汁机 / 魔法帽 / 蜗牛）把一列切成若干段，段内
-- 幸存格依次落到底部、顶上补新格。若预测与 cwAfter 不符（饼干底收 / 传送门换位），该列
-- 退回「就地淡入」：只有变了的格从上一行落下。
fallTable :: CascadeWave -> [[(Int, Bool)]]
fallTable w = transpose [colInfo c | c <- [0 .. boardSize - 1]]
  where
    rows = [0 .. boardSize - 1]
    colInfo c =
      let holes = [(cwHoles w !! r) !! c | r <- rows]
          after = [getCell (cwAfter w) (r, c) | r <- rows]
          segs = segments (zip rows holes)
          predicted = concatMap segPredict segs -- [(targetRow, drop, isNew, expected)]
          ok =
            length predicted == boardSize
              && and [maybe True (== (after !! rt)) ex | (rt, _, _, ex) <- predicted]
          byRow = [(d, n) | (_, d, n, _) <- sortRows predicted]
          fallback =
            [ if holes !! r == Just (after !! r) then (0, False) else (1, True)
            | r <- rows
            ]
      in if ok then byRow else fallback
    sortRows ps = [p | r <- rows, p@(rt, _, _, _) <- ps, rt == r]
    isFixed (Just cell) = case cell of
      Bottle _ -> True
      Maker _ _ -> True
      MagicHat -> True
      Snail _ _ -> True
      _ -> False
    isFixed Nothing = False
    segments [] = []
    segments xs =
      let (seg, rest) = break (isFixed . snd) xs
      in case rest of
           (fx : ys) -> seg : [fx] : segments ys
           [] -> [seg]
    segPredict [] = []
    segPredict [(r, Just cell)] | isFixed (Just cell) = [(r, 0, False, Just cell)]
    segPredict seg =
      let segRows = map fst seg
          survivors = [(r, cell) | (r, Just cell) <- seg]
          h = length seg - length survivors
          news = [(rt, h, True, Nothing) | rt <- take h segRows]
          olds =
            [ (rt, rt - rs, False, Just cell)
            | ((rs, cell), rt) <- zip survivors (drop h segRows)
            ]
      in news ++ olds

--------------------------------------------------------------------------------
-- 连击等级样式
--------------------------------------------------------------------------------

-- | 等级越高：字越大、颜色越暖越亮、震屏略大（克制：最多 5 px）。
data ComboStyle = ComboStyle
  { csRGB     :: (Word8, Word8, Word8) -- ^ 主色
  , csHeight  :: Int                   -- ^ 「连击」字高（逻辑像素，弹出放大前）
  , csShake   :: Int                   -- ^ 震屏振幅（逻辑像素）
  , csRainbow :: Bool                  -- ^ x5+：彩色流转
  }

comboStyle :: Int -> ComboStyle
comboStyle k
  | k <= 2 = ComboStyle (255, 238, 150) 30 2 False -- x2 白黄
  | k == 3 = ComboStyle (255, 164, 52) 36 3 False  -- x3 橙
  | k == 4 = ComboStyle (255, 76, 64) 42 4 False   -- x4 红
  | otherwise = ComboStyle (214, 120, 255) 48 5 True -- x5+ 紫 / 彩

-- | 当前帧颜色：x5+ 在紫色基础上做色相流转（彩虹感），其它等级固定。
styleRGB :: ComboStyle -> Int -> (Word8, Word8, Word8)
styleRGB st pulse
  | not (csRainbow st) = csRGB st
  | otherwise =
      let h = fromIntegral ((pulse * 9) `mod` 360) :: Double
          (r, g, b) = hsv h 0.55 1.0
      in (r, g, b)

hsv :: Double -> Double -> Double -> (Word8, Word8, Word8)
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
-- 浮字
--------------------------------------------------------------------------------

data PopKind
  = PopCombo Int     -- ^ 「连击 xN」
  | PopScore Int Int -- ^ 本轮得分（分数, 连击序号）
  deriving (Eq, Show)

-- | 浮字：位置为逻辑像素（中心点）。
data TextPop = TextPop
  { tpKind :: PopKind
  , tpX    :: Float
  , tpY    :: Float
  , tpAge  :: Int
  , tpLife :: Int
  }

tickPops :: [TextPop] -> [TextPop]
tickPops = filter (\p -> tpAge p < tpLife p) . map (\p -> p {tpAge = tpAge p + 1})

-- | 「连击」弹出缩放：0.35 → 1.3（7 帧回弹式放大）→ 1.0（5 帧回落）→ 保持。
comboPopScale :: Int -> Double
comboPopScale age
  | age < 7 =
      let t = fromIntegral age / 7
          e = 1 - (1 - t) * (1 - t)
      in 0.35 + (1.3 - 0.35) * e
  | age < 12 = 1.3 - 0.3 * fromIntegral (age - 7) / 5
  | otherwise = 1.0

-- | 得分浮字：0.6 → 1.0 轻微弹出。
scorePopScale :: Int -> Double
scorePopScale age
  | age < 5 = 0.6 + 0.4 * fromIntegral age / 5
  | otherwise = 1.0

-- | 最后 16 帧线性淡出。
popAlpha :: TextPop -> Word8
popAlpha p =
  let left = tpLife p - tpAge p
  in if left >= 16 then 255 else fromIntegral (max 0 (left * 255 `div` 16))

-- | 上浮距离（逻辑像素）：连击提示停稳后缓慢上浮，得分从一开始就飘起。
popRise :: TextPop -> Float
popRise p = case tpKind p of
  PopCombo _ -> if tpAge p < 12 then 0 else fromIntegral (tpAge p - 12) * 0.35
  PopScore _ _ -> fromIntegral (tpAge p) * 0.9

-- | 被消格的锚点：(平均行, 平均列, 最上行, 最下行)。
clearedAnchor :: [Pos] -> (Float, Float, Int, Int)
clearedAnchor [] = (3.5, 3.5, 3, 4)
clearedAnchor ps =
  let n = fromIntegral (length ps)
  in ( sum [fromIntegral r | (r, _) <- ps] / n
     , sum [fromIntegral c | (_, c) <- ps] / n
     , minimum (map fst ps)
     , maximum (map fst ps)
     )
