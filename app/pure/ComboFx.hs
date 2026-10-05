-- | 连击（连锁）表现层的纯逻辑：逐轮回放的阶段机与时间线、步末效果阶段、下落映射、连击等级样式。
-- 只描述「怎么播」，不含任何绘制（绘制在网页 web/www/render.js / hud.js，经 Match3Web.Anim 取阶段与帧号），
-- 也不改规则：回放脚本与效果事件来自通用接口 gameStep 的整步报告（Match3.Engine.match3Shell），结算结果仍以规则层为准。
--
-- 时钟（帧号、加速、进度）在通用播放层 Engine.Playback：本模块只给出阶段机
-- cascadeStages（每个阶段多长、播完去哪、进入时触发什么）；回放器是 Player Cascade。
-- 波次级界面（高亮 / 消失 / 粒子 / 得分浮字）读 WaveView 里的效果事件（EvClear 格、EvScore 分），
-- 不读 CascadeWave 的 cwCleared / cwScore；底图快照（cwBefore / cwHoles / cwAfter）取自波次。
--
-- 纯前端模块（app/pure，网页 wasm 编译、原生测试也编译）。帧数（高亮、得分浮字、连击弹字、各步末段）、
-- 步末段种类与连击等级样式都来自表现表 UI.Presentation（StageKind / ComboStyle 在那里定义，这里再导出）。
module ComboFx
  ( -- * 时间线（帧；主循环固定 60 fps 步长，1 帧 ≈ 16.7 ms）
    swapFrames
  , fallFrames
  , waveFlashFrames
  , wavePopFrames
  , waveRestFrames
  , fallFramesFor
  , fastStep
  , comboPopLife
  , scorePopLife
  , comboSummaryFrames
  , shakeFrames
  , endStageBase
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
    -- * 波次视图（快照 + 效果事件）
  , WaveView (..)
  , waveViews
  , wvCleared
  , wvScore
    -- * 步末效果阶段（StageKind 定义在 UI.Presentation，这里再导出）
  , StageKind (..)
  , EndStage (..)
    -- * 下落映射
  , fallTable
    -- * 连击等级样式（定义在 UI.Presentation，这里再导出）
  , ComboStyle (..)
  , comboStyle
  , styleRGB
  ) where

import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Match3.Core
import Match3.Element.Event (Event (..), EventKind (..))
import Engine.Playback (Player (..), Stages (..), Tick (..), stepPlayer)
import UI.Presentation (ComboStyle (..), Presentation (..), StageKind (..), comboStyle, presentationFor, stageFrames, stageKindOf, styleRGB)

--------------------------------------------------------------------------------
-- 时间线
--------------------------------------------------------------------------------

-- | 被消格高亮闪烁停留（≈ 200 ms）：玩家在这一段看清「这一轮消的是哪些格」。帧数取表现表 EvClear 行。
waveFlashFrames :: Int
waveFlashFrames = prFrames (presentationFor EvClear)

-- | 被消格缩小消失（≈ 100 ms），同时迸出粒子、弹出本轮得分。
wavePopFrames :: Int
wavePopFrames = 6

-- | 交换补间与无连锁时的轻落（帧；网页经 m3Meta 取）。
swapFrames, fallFrames :: Int
swapFrames = 10
fallFrames = 12

-- | 落定后的短停顿（≈ 67 ms），让下一轮的高亮与上一轮的下落分得开。
waveRestFrames :: Int
waveRestFrames = 4

-- | 下落 + 补子时长：8..14 帧（≈ 133..233 ms），落差越大越长。
fallFramesFor :: Int -> Int
fallFramesFor maxDrop = max 8 (min 14 (6 + maxDrop))

-- | 点击加速：每帧推进的回放帧数。
fastStep :: Int
fastStep = 3

-- | 「连击 xN」弹字寿命（≈ 0.9 s，放大弹出 → 停留 → 淡出）：表现表 EvCombo 行。
comboPopLife :: Int
comboPopLife = prFrames (presentationFor EvCombo)

-- | 本轮得分浮字寿命（≈ 0.8 s）：表现表 EvScore 行。
scorePopLife :: Int
scorePopLife = prFrames (presentationFor EvScore)

-- | 连锁结束后 HUD「N 连击！」总结的显示时长（≈ 1.6 s）。
comboSummaryFrames :: Int
comboSummaryFrames = 96

-- | 震屏持续帧数（振幅随时间线性衰减）。
shakeFrames :: Int
shakeFrames = 10

-- | 步末阶段的基础帧数：查表现表（UI.Presentation.presentationTable）里这个段的那一行。
endStageBase :: StageKind -> Int
endStageBase = stageFrames

-- | 事件类型对应的表现段种类（查表现表；不是步末表现的种类按蔓延段播放）。新增步末事件只需在表现表里加一行。
stageKindFor :: EventKind -> StageKind
stageKindFor = stageKindOf

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
groupStages (e : more) =
  let k = kindOf e
      (sameMore, rest)
        | k == StSpread = span ((== StSpread) . kindOf) more
        | otherwise = ([], more)
      same = e :| sameMore
  in EndStage k (NE.toList same) (esBefore e) (esAfter (NE.last same)) (endStageBase k) : groupStages rest
  where
    kindOf = stageKindFor . endEffectKind . esEffect

-- | 按 endBudgetFrames 压缩同一时刻的多段步末动画。
budget :: [EndStage] -> [EndStage]
budget ss =
  let total = sum (map stFrames ss)
  in if total <= endBudgetFrames
       then ss
       else [s {stFrames = max 8 (stFrames s * endBudgetFrames `div` total)} | s <- ss]

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
fallTable w = transpose [colInfo c | c <- cols]
  where
    rows = boardRowIndices (cwAfter w)
    cols = boardColIndices (cwAfter w)
    nRows = length rows
    colInfo c =
      let holes = [holeAt w (r, c) | r <- rows]
          afterAt r = getCell (cwAfter w) (r, c)
          segs = segments (zip rows holes)
          predicted = concatMap segPredict segs -- [(targetRow, drop, isNew, expected)]
          ok =
            length predicted == nRows
              && and [maybe True (== afterAt rt) ex | (rt, _, _, ex) <- predicted]
          byRow = [(d, n) | (_, d, n, _) <- sortRows predicted]
          fallback =
            [ if hole == Just (afterAt r) then (0, False) else (1, True)
            | (r, hole) <- zip rows holes
            ]
      in if ok then byRow else fallback
    sortRows ps = [p | r <- rows, p@(rt, _, _, _) <- ps, rt == r]
    -- 固定格 = 规则层的重力定义（元素 falls = False），不在表现层另列
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

-- | 本轮消除并放下新特殊块之后、下落之前某格的内容（Nothing = 空洞；越界也按空洞）。
holeAt :: CascadeWave -> Pos -> Maybe Cell
holeAt w = atM (cwHoles w)
