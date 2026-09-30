-- | 网页端的逐轮回放：直接复用桌面版的纯阶段机 app/pure/ComboFx.hs（cascadeStages + Engine.Playback 的播放器），
-- 因此网页与桌面的时间线（每阶段帧数、加速、步末段压缩、触发的一次性事件）逐帧相同。
--
-- 数据流（每步一次 + 每帧一次）：
--   * m3Swap 已把本步所有盘面快照随 trace 发给 JS；本模块用「盘面编号」引用它们（见 animBoards），
--     m3AnimStart 只额外发每轮的下落映射 fallTable；
--   * m3AnimTick(fast) 每帧推进播放器一帧，只回传几十字节：阶段、帧号 / 阶段长度、轮次、连击、
--     当前落定盘面编号、步末段描述与进入阶段时触发的事件。
--
-- 何时有回放：与桌面 UI.Playback.withMovePlayback 相同——本步 MoveFx 为空（被拒 / 终局后）不播；
-- 有轮次或步末效果才建立回放（结算过却两者皆无的兜底情形这里不播，JS 直接显示结算后盘面）。
module Match3Web.Anim
  ( AnimSeed(..)
  , seedOf
  , animBoards
  , animCascade
  , animStart
  , animTick
  , animRunPlayer
  ) where

import Data.List (elemIndex)

import ComboFx
import Engine.Playback (Player(..), Stages(..), Tick(..), acceleratePlayer, newPlayer, runPlayer)
import Match3.Core
import Match3.Element.Event (Event)
import Match3.Engine (Played(..))
import Match3Web.Json

-- | 建立一段回放所需的全部输入（与桌面 newCascade 的参数相同）。
data AnimSeed = AnimSeed
  { asTrace  :: MoveTrace  -- ^ 本步回放脚本
  , asEvents :: [Event]    -- ^ 本步效果事件
  , asFinal  :: Board      -- ^ 结算后的真实盘面（含自动洗牌）
  , asBase   :: Int        -- ^ 本步之前的总分
  }

-- | 由走步前状态与本步报告决定是否播放（见模块说明）。
seedOf :: GameState -> Played -> Maybe AnimSeed
seedOf before p
  | not (pdAccepted p) || pdFx p == MoveFx 0 [] = Nothing
  | null (mtWaves mt) && null (mtEnd mt) = Nothing
  | otherwise = Just (AnimSeed mt (pdEvents p) (gsBoard (pdState p)) (gsScore before))
  where
    mt = pdTrace p

-- | 盘面编号表：JS 用 m3Swap 的 JSON 按同样顺序建表 ——
--   [trace.start] ++ 每轮 [before, after] ++ 每个步末 [before, after] ++ [trace.final, state.board]。
-- 回放中出现的盘面（落定盘面 cShown、步末段的前后盘面、结算后盘面）都在表里；取第一个相等的编号。
animBoards :: AnimSeed -> [Board]
animBoards s =
  [mtStart mt]
    ++ concat [[cwBefore w, cwAfter w] | w <- mtWaves mt]
    ++ concat [[esBefore e, esAfter e] | e <- mtEnd mt]
    ++ [mtFinal mt, asFinal s]
  where
    mt = asTrace s

boardId :: [Board] -> Board -> Int
boardId bs b = maybe (-1) id (elemIndex b bs)

-- | 与桌面相同的初始回放状态。
animCascade :: AnimSeed -> Cascade
animCascade s = newCascade (asTrace s) (asEvents s) (asFinal s) (asBase s)

-- | 开播：返回新播放器与 {ok, anim:true, boards:盘面数, base, fall:[每轮 8×8]}。
-- fall 每格编码为 下落行数×2 + (新补的格 ? 1 : 0)（ComboFx.fallTable）。
animStart :: AnimSeed -> (Player Cascade, String)
animStart s =
  ( newPlayer (animCascade s)
  , obj
      [ ("ok", "true")
      , ("anim", "true")
      , ("boards", int (length (animBoards s)))
      , ("base", int (asBase s))
      , ("fall", arr [arr [arr [int (d * 2 + fromEnum n) | (d, n) <- row] | row <- fallTable w] | w <- mtWaves (asTrace s)])
      ]
  )

-- | 推进一帧（fast = 点击加速，此后每帧推进 ComboFx.fastStep 帧）。播完返回 Nothing。
-- 播放中：{p, fr, n, w, k, g, b[, s][, ev]}
--   p 阶段 start|flash|pop|fall|rest|end；fr / n 阶段内帧号 / 阶段长度（进度 = fr / n）；
--   w 当前轮下标（= 已播完的轮数）；k 连击序号；g 已消失轮次的累计得分；b 最近落定盘面编号；
--   s 步末段 {kind, e:[trace.end 下标], b0, b1, n}（仅 p = end）；ev 进入阶段时的一次性事件：
--   {e:"hl"|"van", k, w}（高亮 / 消失）或 {e:"end", kind}。
-- 播完：{done:true, b, best, g, fall}（fall = 最后落定盘面 ≠ 结算后盘面，需补一段轻落，同桌面 AnimFall）。
-- 第二个分量是本帧触发的事件数（测试用）。
animTick :: Bool -> AnimSeed -> Player Cascade -> (Maybe (Player Cascade), Int, String)
animTick fast s p0 =
  let p = if fast then acceleratePlayer p0 else p0
  in case stepPlayback p of
       Playing p' evs -> (Just p', length evs, tickJson p' evs)
       Done c ->
         ( Nothing
         , 0
         , obj
             [ ("done", "true")
             , ("b", int (bid (cShown c)))
             , ("best", int (cBest c))
             , ("g", int (cGain c))
             , ("fall", bool (cShown c /= cFinal c))
             ]
         )
  where
    mt = asTrace s
    bs = animBoards s
    bid = boardId bs
    waveIdx v = maybe (-1) id (elemIndex (wvWave v) (mtWaves mt))
    tickJson p evs =
      let c = plStage p
      in obj $
           [ ("p", str (phaseName (cPhase c)))
           , ("fr", int (plFrame p))
           , ("n", int (stageLen cascadeStages c))
           , ("w", int (cDone c))
           , ("k", int (cCombo c))
           , ("g", int (cGain c))
           , ("b", int (bid (cShown c)))
           ]
             ++ [("s", stageJson st) | PhEnd <- [cPhase c], st : _ <- [cStages c]]
             ++ [("ev", arr (map evJson evs)) | not (null evs)]
    stageJson st =
      obj
        [ ("kind", str (stageName (stKind st)))
        , ("e", arr [int i | e <- stSteps st, Just i <- [elemIndex e (mtEnd mt)]])
        , ("b0", int (bid (stBefore st)))
        , ("b1", int (bid (stAfter st)))
        , ("n", int (stFrames st))
        ]
    evJson ev = case ev of
      EvHighlight k v -> obj [("e", str "hl"), ("k", int k), ("w", int (waveIdx v))]
      EvVanish k v -> obj [("e", str "van"), ("k", int k), ("w", int (waveIdx v))]
      EvEndStage st -> obj [("e", str "end"), ("kind", str (stageName (stKind st)))]

-- | 离线一口气播完（不加速）：(总帧数, 事件数)。原生一侧用来核对逐帧循环与 Engine.Playback.runPlayer 一致。
animRunPlayer :: AnimSeed -> (Int, Int)
animRunPlayer s =
  let (n, evs, _) = runPlayer fastStep cascadeStages (newPlayer (animCascade s))
  in (n, length evs)

phaseName :: WavePhase -> String
phaseName ph = case ph of
  PhStart -> "start"
  PhFlash -> "flash"
  PhPop -> "pop"
  PhFall -> "fall"
  PhRest -> "rest"
  PhEnd -> "end"

stageName :: StageKind -> String
stageName k = case k of
  StTick -> "tick"
  StBelt -> "belt"
  StSpread -> "spread"
  StSnail -> "snail"
  StShuffle -> "shuffle"
