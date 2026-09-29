-- | 连击（连锁）表现层的纯逻辑：逐轮回放的阶段机与时间线、步末效果阶段、下落映射、连击等级样式、浮字曲线。
-- 只描述「怎么播」，不含任何 SDL 绘制（绘制见 UI.Cascade / UI.HudArt / UI.HudPrim），
-- 也不改规则：回放脚本与效果事件来自 Match3.Engine.play，结算结果仍以规则层为准。
--
-- 第三刀：时钟（帧号、加速、进度）交给通用播放层 Engine.Playback——本模块只给出阶段机
-- cascadeStages（每个阶段多长、播完去哪、进入时触发什么）；回放器是 Player Cascade。
-- 波次级界面（高亮 / 消失 / 粒子 / 得分浮字）读 WaveView 里的效果事件（EvClear 格、EvScore 分），
-- 不再读 CascadeWave 的 cwCleared / cwScore；底图快照（cwBefore / cwHoles / cwAfter）仍取自波次。
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
  , endStageBase
  , endStageTable
  , stageKindFor
  , endBudgetFrames
    -- * 逐轮回放阶段机
  , WavePhase (..)
  , Cascade (..)
  , CascadePlayer
  , CascadeEvent (..)
  , newCascade
  , cascadeStages
  , stepPlayback
  , phaseLen
  , phaseT
    -- * 波次视图（快照 + 效果事件）
  , WaveView (..)
  , waveViews
  , wvCleared
  , wvScore
    -- * 步末效果阶段
  , StageKind (..)
  , EndStage (..)
  , stageMoves
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
import Match3.Board.Gravity (gravityFixedCell)
import Match3.Element.Event (Event (..), EventKind (..), endEffectKind, endEffectPairs)
import Engine.Playback (Player (..), Stages (..), Tick (..), playerProgress, stepPlayer)

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

-- | 步末播放表：规则层的事件类型（Match3.Element.Event.EventKind）→ (表现段种类, 基础帧数)。
-- 1 帧 ≈ 16.7 ms：倒计时减一 10（≈ 170 ms）、皮带移位 14（≈ 230 ms）、蔓延 18（≈ 300 ms，藤 / 巧 / 蒸汽
-- 同时长出）、会走的元素（蜗牛）18（≈ 300 ms）、自动洗牌 22（≈ 370 ms）。新增步末事件只需在这里加一行。
endStageTable :: [(EventKind, (StageKind, Int))]
endStageTable =
  [ (EvTick, (StTick, 10))
  , (EvBelt, (StBelt, 14))
  , (EvSpread, (StSpread, 18))
  , (EvMove, (StSnail, 18))
  , (EvShuffle, (StShuffle, 22))
  ]

-- | 步末阶段的基础帧数（查 endStageTable）。
endStageBase :: StageKind -> Int
endStageBase k = head ([n | (_, (k', n)) <- endStageTable, k' == k] ++ [18])

-- | 事件类型对应的表现段种类（查 endStageTable）。
stageKindFor :: EventKind -> StageKind
stageKindFor ev = maybe StSpread fst (lookup ev endStageTable)

-- | 同一时刻连续发生的步末阶段（不含自动洗牌）合计不超过 36 帧（≈ 0.6 s），超出时按比例压缩，
-- 每段至少 8 帧，保证仍能看清。
endBudgetFrames :: Int
endBudgetFrames = 36

--------------------------------------------------------------------------------
-- 阶段机
--------------------------------------------------------------------------------

-- | 一轮的四个阶段；PhStart 只是「尚未进入第一轮」的占位（长度 0）。
-- PhEnd：步末效果阶段（倒计时 / 皮带 / 蔓延 / 蜗牛 / 自动洗牌），当前段是 cStages 的 head。
data WavePhase = PhStart | PhFlash | PhPop | PhFall | PhRest | PhEnd
  deriving (Eq, Show)

-- | 步末阶段的种类（同一时刻连续的藤 / 巧 / 蒸汽合并为一个 StSpread 同时播放）。
data StageKind = StTick | StBelt | StSpread | StSnail | StShuffle
  deriving (Eq, Show)

-- | 一段步末动画：从 stBefore 播到 stAfter。
data EndStage = EndStage
  { stKind   :: StageKind
  , stSteps  :: [EndStep] -- ^ 规则层记录的效果（StShuffle 为空）
  , stBefore :: Board
  , stAfter  :: Board
  , stFrames :: Int
  }

-- | 一轮的界面视图：底图快照（消除前 / 挖洞 / 落定）来自规则层的波次，
-- 被消格与得分来自本轮的效果事件（EvClear / EvScore 等，evWave = 轮下标）。
data WaveView = WaveView
  { wvWave   :: CascadeWave -- ^ 快照：cwBefore / cwHoles / cwAfter（下落映射也按它算）
  , wvEvents :: [Event]     -- ^ 本轮的轮内效果事件（爆炸 / 消除 / 波及 / 底收 / 得分 / 连击）
  }

-- | 本轮被消格：各 EvClear 事件的格按事件顺序拼接（规则层按连续同名分组，拼起来即原顺序）。
wvCleared :: WaveView -> [Pos]
wvCleared v = [p | e <- wvEvents v, evKind e == EvClear, (p, _) <- evCells e]

-- | 本轮得分：EvScore 事件之和（无得分的轮没有 EvScore，即 0）。
wvScore :: WaveView -> Int
wvScore v = sum [evAmount e | e <- wvEvents v, evKind e == EvScore]

-- | 按轮下标把效果事件分给各轮（只取轮内事件；步末事件由 EndStage 播放）。
waveViews :: MoveTrace -> [Event] -> [WaveView]
waveViews mt evs =
  [ WaveView w [e | e <- evs, evWave e == k, evKind e `elem` inWave]
  | (k, w) <- zip [0 ..] (mtWaves mt)
  ]
  where
    inWave = [EvBlast, EvClear, EvHit, EvDrain, EvScore, EvCombo]

-- | 一步操作的回放进度（阶段状态；帧号与加速在 Player 里）。
data Cascade = Cascade
  { cWaves :: [WaveView]    -- ^ 尚未播完的轮次，head 为当前轮
  , cPhase :: WavePhase
  , cCombo :: Int           -- ^ 当前轮的连击序号（从 1 起，只计有消除的轮）
  , cBest  :: Int           -- ^ 已播到的最高连击
  , cGain  :: Int           -- ^ 已消失轮次的累计得分（HUD 分数滚动）
  , cBase  :: Int           -- ^ 本步之前的总分
  , cFinal :: Board         -- ^ 结算后的真实盘面（含蔓延 / 蜗牛 / 自动洗牌）
  , cShown :: Board         -- ^ 最近一次落定的盘面
  , cEnds  :: [EndStep]     -- ^ 尚未播放的步末效果（按 esAfterWaves 排序）
  , cDone  :: Int           -- ^ 已播完的轮数（= 下一个步末效果插入点）
  , cStages :: [EndStage]   -- ^ PhEnd 中：当前及后续的步末段
  }

-- | 阶段切换时前端要做的一次性动作。
data CascadeEvent
  = EvHighlight Int WaveView -- ^ 进入高亮：k ≥ 2 时弹「连击 xk」
  | EvVanish Int WaveView    -- ^ 进入消失：粒子 + 本轮得分浮字 + 震屏（k ≥ 2）
  | EvEndStage EndStage      -- ^ 进入一段步末动画：蔓延碎屑 / 倒计时火花等

-- | 逐轮回放器 = 通用播放器 + 本模块的阶段机。
type CascadePlayer = Player Cascade

-- | 由回放脚本、本步效果事件、结算后盘面与本步之前的总分建立回放（阶段 PhStart）。
newCascade :: MoveTrace -> [Event] -> Board -> Int -> Cascade
newCascade mt evs final base =
  Cascade
    { cWaves = waveViews mt evs
    , cPhase = PhStart
    , cCombo = 0
    , cBest = 0
    , cGain = 0
    , cBase = base
    , cFinal = final
    , cShown = mtStart mt
    , cEnds = mtEnd mt
    , cDone = 0
    , cStages = []
    }

phaseLen :: WavePhase -> CascadeWave -> Int
phaseLen ph w = case ph of
  PhStart -> 0
  PhFlash -> waveFlashFrames
  PhPop -> wavePopFrames
  PhFall -> fallFramesFor (maximum (0 : [d | row <- fallTable w, (d, _) <- row]))
  PhRest -> waveRestFrames
  PhEnd -> 0 -- 步末段长度见 EndStage.stFrames

-- | 当前阶段的总帧数。
curLen :: Cascade -> Int
curLen c = case cPhase c of
  PhStart -> 0
  PhEnd -> case cStages c of
    (s : _) -> stFrames s
    [] -> 0
  ph -> case cWaves c of
    (w : _) -> phaseLen ph (wvWave w)
    [] -> 0

-- | 逐轮回放的阶段机：长度 = curLen，播完 = advance（Left = 全部播完）。
cascadeStages :: Stages Cascade CascadeEvent
cascadeStages = Stages curLen next
  where
    next c = case advance c of
      (Continue c', evs) -> Right (c', evs)
      (Finished c', _) -> Left c'

-- | 当前阶段进度 0..1。
phaseT :: CascadePlayer -> Double
phaseT = playerProgress cascadeStages

-- | 推进一帧（加速时一次推进 fastStep 帧，步末阶段同样加速）。
stepPlayback :: CascadePlayer -> Tick Cascade CascadeEvent
stepPlayback = stepPlayer fastStep cascadeStages

-- | advance 的内部结果：还有下一阶段 / 播完。
data StepResult = Continue Cascade | Finished Cascade

advance :: Cascade -> (StepResult, [CascadeEvent])
advance c = case (cPhase c, cWaves c) of
  (PhStart, _) -> arrive c
  (PhEnd, _) -> case cStages c of
    (s : rest@(s2 : _)) -> (Continue c {cStages = rest, cShown = stAfter s}, [EvEndStage s2])
    (s : []) -> enterWave c {cStages = [], cShown = stAfter s}
    [] -> enterWave c
  (_, []) -> (Finished c, [])
  (PhFlash, w : _) ->
    (Continue c {cPhase = PhPop, cGain = cGain c + wvScore w}, [EvVanish (cCombo c) w])
  (PhPop, _) -> (Continue c {cPhase = PhFall}, [])
  (PhFall, w : _) -> (Continue c {cPhase = PhRest, cShown = cwAfter (wvWave w)}, [])
  (PhRest, _ : rest) -> arrive c {cWaves = rest, cDone = cDone c + 1}

-- | 到达插入点 cDone：先播这里的步末效果（全部轮次之后还要检查自动洗牌），再进入下一轮。
arrive :: Cascade -> (StepResult, [CascadeEvent])
arrive c =
  let (now, later) = span ((<= cDone c) . esAfterWaves) (cEnds c)
      stages0 = budget (groupStages now)
      lastBoard = case reverse stages0 of
        (s : _) -> stAfter s
        [] -> cShown c
      shuffle =
        [ EndStage StShuffle [] lastBoard (cFinal c) (endStageBase StShuffle)
        | null (cWaves c)
        , lastBoard /= cFinal c
        ]
      stages = stages0 ++ shuffle
      c' = c {cEnds = later}
  in case stages of
       (s : _) -> (Continue c' {cPhase = PhEnd, cStages = stages}, [EvEndStage s])
       [] -> enterWave c'

-- | 规则层的步末效果 → 表现段：连续的蔓延合并为一段同时播放。
groupStages :: [EndStep] -> [EndStage]
groupStages [] = []
groupStages steps@(e : _) =
  let k = kindOf e
      (same, rest)
        | k == StSpread = span ((== StSpread) . kindOf) steps
        | otherwise = ([e], drop 1 steps)
  in EndStage k same (esBefore (head same)) (esAfter (last same)) (endStageBase k) : groupStages rest
  where
    kindOf = stageKindFor . endEffectKind . esEffect

-- | 按 endBudgetFrames 压缩同一时刻的多段步末动画。
budget :: [EndStage] -> [EndStage]
budget ss =
  let total = sum (map stFrames ss)
  in if total <= endBudgetFrames
       then ss
       else [s {stFrames = max 8 (stFrames s * endBudgetFrames `div` total)} | s <- ss]

-- | 本段的「(来源, 目标)」格对：皮带 / 蔓延 / 蜗牛各自的移动或生长方向（倒计时 / 洗牌为空）。
stageMoves :: EndStage -> [(Pos, Pos)]
stageMoves s = concatMap (endEffectPairs . esEffect) (stSteps s)

-- | 进入 head 轮：没有被消格的轮（皮带沉降收饼干等）直接下落，不计连击。
enterWave :: Cascade -> (StepResult, [CascadeEvent])
enterWave c = case cWaves c of
  [] -> (Finished c {cPhase = PhRest, cStages = []}, [])
  (w : _)
    | null (wvCleared w) ->
        (Continue c {cPhase = PhFall, cGain = cGain c + wvScore w}, [])
    | otherwise ->
        let k = cCombo c + 1
        in ( Continue c {cPhase = PhFlash, cCombo = k, cBest = max (cBest c) k}
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
    -- 固定格 = 规则层的重力定义（元素 edFalls = False），不在表现层另列
    isFixed (Just cell) = gravityFixedCell cell
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
