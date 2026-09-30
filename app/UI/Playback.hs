{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 动画推进与一步操作后的表现编排（纯函数）：每帧推进交换 / 下落 / 逐轮回放，
-- 回放阶段切换时产生弹字、得分浮字、震屏和粒子；按本次操作的 MoveFx / MoveTrace 决定播什么。
--
-- 依赖：ComboFx（阶段机）、UI.Presentation（表现表：碎屑颜色 / 帧数）、UI.Sound（音效钩子）、UI.Types、UI.Layout、Match3.Core。
-- 不变量：只由本次调用返回的 MoveFx / MoveTrace 驱动；MoveFx 为空时清空弹字与总结、不播放，
-- 绝不重播上一步（护栏 failed_swap_resets_combo_feedback 等在规则层锁定 MoveFx）。
module UI.Playback
  ( tickAnim
  , stepAnim
  , applyCascadeEvent
  , crumbParticles
  , boardTopF
  , boardLeftF
  , cellF
  , spawnComboPop
  , scorePopAt
  , tickParticles
  , burstParticles
  , comboFxFrames
  , noMoveFx
  , withMovePlayback
  , accelerate
  ) where

import ComboFx
import Data.Word (Word8)
import Engine.Playback (Tick (..), acceleratePlayer, newPlayer)
import Match3.Core
import qualified Match3.Element.Event as Ev
import System.Random (StdGen, mkStdGen, randomR)
import UI.Layout
import UI.Presentation (Crumbs (..), Presentation (..), stagePresentation)
import UI.Sound (cascadeSounds)
import UI.Types

-- | 规则层效果事件（与 SDL 的 Event 区分）。
type Match3Event = Ev.Event

-- | 每帧推进：暂停时只走呼吸光；否则推进动画、闪光、粒子、弹字、震屏与各类倒计时。
tickAnim :: App -> App
tickAnim app
  -- 暂停时一切冻结（只让呼吸光的 pulse 继续走）
  | appPaused app = app {appPulse = appPulse app + 1}
  | otherwise =
      stepAnim
        app
          { appFlash = [(p, n - 1) | (p, n) <- appFlash app, n > 1]
          , appPulse = appPulse app + 1
          , appComboShow = max 0 (appComboShow app - 1)
          , appPops = tickPops (appPops app)
          , appShake = max 0 (appShake app - 1)
          , appParticles = tickParticles (appParticles app)
          , appTipFrames = max 0 (appTipFrames app - 1)
          , appHelpFrames = max 0 (appHelpFrames app - 1)
          }

-- | 推进当前动画一帧；逐轮回放在阶段切换时触发弹字 / 粒子 / 震屏。
stepAnim :: App -> App
stepAnim app = case appAnim app of
  AnimNone -> app
  a@AnimSwap {asFrame, asNext}
    | asFrame + 1 >= swapFrames -> app {appAnim = asNext}
    | otherwise -> app {appAnim = a {asFrame = asFrame + 1}}
  AnimFall {afBoard, afFrame}
    | afFrame + 1 >= fallFrames -> app {appAnim = AnimNone}
    | otherwise -> app {appAnim = AnimFall afBoard (afFrame + 1)}
  AnimCascade p -> case stepPlayback p of
    Playing p' evs -> foldl applyCascadeEvent app {appAnim = AnimCascade p'} evs
    Done c' ->
      app
        { appAnim = if cShown c' == cFinal c' then AnimNone else AnimFall (cFinal c') 0
          -- 连锁播完才亮 HUD 总结（最高连击 ≥ 2）
        , appComboShow = if cBest c' >= 2 then comboSummaryFrames else 0
        , appComboBest = cBest c'
        }

-- | 回放阶段切换时的一次性表现：高亮时弹「连击 xN」，消失时出粒子 / 得分浮字 / 震屏，步末段出碎屑火花；
-- 同时把表现表里配置的音效名排进 appSounds（内置表全为空）。
applyCascadeEvent :: App -> CascadeEvent -> App
applyCascadeEvent app0 ev = case ev of
  EvHighlight k v
    | k >= 2 -> app {appPops = spawnComboPop k (wvCleared v) (appPops app)}
    | otherwise -> app
  EvVanish k v ->
    -- 被消格与得分读本轮效果事件（EvClear / EvScore）；粒子颜色取消除前的快照
    let cleared = wvCleared v
        gain = wvScore v
        parts = burstParticles (appPulse app) (cwBefore (wvWave v)) cleared
        scorePop = [scorePopAt gain k cleared | gain > 0]
        st = comboStyle k
    in app
         { appParticles = parts ++ appParticles app
         , appPops = scorePop ++ appPops app
         , appShake = if k >= 2 then shakeFrames else appShake app
         , appShakeAmp = if k >= 2 then csShake st else appShakeAmp app
         }
  EvEndStage st ->
    -- 步末：按表现表里这个段的碎屑方式出粒子（藤 / 巧 / 蒸汽迸同色碎屑，倒计时冒红色火星；皮带 / 蜗牛 / 洗牌只靠位移动画）
    app {appParticles = endCrumbs (appPulse app) st ++ appParticles app}
  where
    app = case cascadeSounds ev of
      [] -> app0
      ss -> app0 {appSounds = appSounds app0 ++ ss}

-- | 步末碎屑：解释表现表的 prCrumbs。CrumbsByElement 的颜色按步末效果的元素名查 elementRGBTable（表里没有的元素不迸）。
endCrumbs :: Int -> EndStage -> [Particle]
endCrumbs pulse st = case prCrumbs (stagePresentation (stKind st)) of
  NoCrumbs -> []
  CrumbsAtSources rgb -> crumbParticles pulse rgb [p | (p, _) <- stageMoves st]
  CrumbsByElement ->
    concat
      [ crumbParticles (pulse + i) rgb [q]
      | (i, e) <- zip [0 :: Int ..] (stSteps st)
      , let eff = esEffect e
      , Just rgb <- [lookup (endEffectElement eff) elementRGBTable]
      , (_, q) <- endEffectPairs eff
      ]

-- | 小颗碎屑（比消除粒子少、慢、小），用于步末效果。
crumbParticles :: Int -> (Word8, Word8, Word8) -> [Pos] -> [Particle]
crumbParticles seed (cr, cg, cb) positions = concat (zipWith one [0 :: Int ..] positions)
  where
    one i pos =
      let (ox, oy) = cellOrigin pos
      in go (3 :: Int) (fromIntegral ox + fromIntegral cellPx / 2) (fromIntegral oy + fromIntegral cellPx / 2) (mkStdGen (seed * 6151 + i * 7727 + 3))
    go 0 _ _ _ = []
    go n cx cy g0 =
      let (ang, g1) = randomR (0, 2 * pi :: Float) g0
          (spd, g2) = randomR (0.6, 2.0 :: Float) g1
          (life, g3) = randomR (14, 24 :: Int) g2
          (sz, g4) = randomR (2, 4 :: Int) (g3 :: StdGen)
          p =
            Particle
              { pX = cx
              , pY = cy
              , pVX = cos ang * spd
              , pVY = sin ang * spd - 1.0
              , pLife = life
              , pMax = life
              , pR = cr
              , pG = cg
              , pB = cb
              , pSize = fromIntegral sz
              }
      in p : go (n - 1) cx cy g4

-- | 棋盘左上角（逻辑像素，Float）。
boardTopF, boardLeftF, cellF :: Float
boardTopF = fromIntegral (padPx + hudH)
boardLeftF = fromIntegral padPx
cellF = fromIntegral cellPx

-- | 「连击 xN」弹字：放在本轮消除区域的上方（放不下就放下方），不挡住正在高亮的格子；
-- 水平方向限制在棋盘内。新弹字出现时，旧的连击弹字加速淡出，屏幕上只保留一个主提示。
spawnComboPop :: Int -> [Pos] -> [TextPop] -> [TextPop]
spawnComboPop k cleared pops =
  let (_, cc, rTop, rBot) = clearedAnchor cleared
      h = fromIntegral (csHeight (comboStyle k)) :: Float
      halfW = h * 1.7 -- 「连击」≈ 2h 宽 + 「xN」，放大到 1.3 倍时的一半
      xRaw = boardLeftF + (cc + 0.5) * cellF
      x = max (boardLeftF + halfW) (min (boardLeftF + fromIntegral boardPx - halfW) xRaw)
      above = boardTopF + fromIntegral rTop * cellF - h * 0.75
      below = boardTopF + fromIntegral (rBot + 1) * cellF + h * 0.75
      boardBot = boardTopF + fromIntegral boardPx
      y
        | above - h * 0.65 >= boardTopF - 4 = above
        | below + h * 0.65 <= boardBot + 4 = below
        | otherwise = boardTopF + h * 0.7
      fadeOld p = case tpKind p of
        PopCombo _ -> p {tpAge = max (tpAge p) (tpLife p - 10)}
        PopScore _ _ -> p
  in TextPop (PopCombo k) x y 0 comboPopLife : map fadeOld pops

-- | 本轮得分浮字：从消除区域中心飘起。
scorePopAt :: Int -> Int -> [Pos] -> TextPop
scorePopAt n k cleared =
  let (cr, cc, _, _) = clearedAnchor cleared
  in TextPop (PopScore n k) (boardLeftF + (cc + 0.5) * cellF) (boardTopF + (cr + 0.5) * cellF) 0 scorePopLife

-- | 粒子前进一帧（位置 + 重力），寿命到了移除。
tickParticles :: [Particle] -> [Particle]
tickParticles =
  filter ((> 0) . pLife)
    . map
      ( \p ->
          p
            { pX = pX p + pVX p
            , pY = pY p + pVY p
            , pVY = pVY p + 0.18  -- gravity
            , pLife = pLife p - 1
            }
      )

-- | 被消格中心迸出的粒子（颜色取自消除前的盘面）。纯函数：随机数由 seed 派生，
-- 这样可以在 tickAnim（纯）里按回放阶段生成。
burstParticles :: Int -> Board -> [Pos] -> [Particle]
burstParticles seed board positions = concat (zipWith one [0 :: Int ..] positions)
  where
    one i pos =
      let (ox, oy) = cellOrigin pos
          cx = fromIntegral ox + fromIntegral cellPx / 2
          cy = fromIntegral oy + fromIntegral cellPx / 2
          rgb = cellRGB (getCell board pos)
      in go (5 :: Int) cx cy rgb (mkStdGen (seed * 7919 + i * 104729 + 17))
    go 0 _ _ _ _ = []
    go n cx cy rgb@(cr, cg, cb) g0 =
      let (ang, g1) = randomR (0, 2 * pi :: Float) g0
          (spd, g2) = randomR (1.2, 4.5 :: Float) g1
          (life, g3) = randomR (18, 36 :: Int) g2
          (sz, g4) = randomR (3, 7 :: Int) (g3 :: StdGen)
          p =
            Particle
              { pX = cx
              , pY = cy
              , pVX = cos ang * spd
              , pVY = sin ang * spd - 1.5
              , pLife = life
              , pMax = life
              , pR = cr
              , pG = cg
              , pB = cb
              , pSize = fromIntegral sz
              }
      in p : go (n - 1) cx cy rgb g4

-- | 连击（爆击）总结帧数。只由本次操作的 MoveFx 决定（边沿触发）：
-- 旧实现直接读 gsCombo，无匹配回滚后 gsCombo 仍是上一步的值，于是又置 120 帧重播。
-- 清除格 fxCleared 已排除传送带 / 蜗牛挪位噪声（见 gsLastCleared）。
comboFxFrames :: MoveFx -> Int
comboFxFrames fx = if fxCombo fx > 1 then comboSummaryFrames else 0

-- | 「本次操作没有结算」的空特效。
noMoveFx :: MoveFx
noMoveFx = MoveFx 0 []

-- | 一步操作之后的表现编排。只看本次调用的 MoveFx（边沿触发）：
--
-- * fx 为空（NoMatch / InvalidSwap / 操作前已结束）：不回放、不闪光，并清掉残留的连击弹字
--   与 HUD 总结 —— 绝不重播上一步的连击特效（回归：failed_swap_resets_combo_feedback）。
-- * 有回放脚本（MoveTrace）：交换动画 → 逐轮回放（高亮 → 消失 → 下落补子 → 下一轮），
--   连击弹字 / 得分浮字 / 震屏 / 粒子都在回放阶段切换时产生；轮次之间与全部落定之后按
--   mtEnd 播放步末效果（倒计时减一 / 皮带 / 蔓延 / 蜗牛，必要时自动洗牌），HUD 总结在全部播完后才亮。
-- * 兜底（理论上不会发生：结算过却没有轮次）：沿用旧的「闪光 + 粒子 + 轻落」。
withMovePlayback :: GameState -> GameState -> MoveFx -> MoveTrace -> [Match3Event] -> Maybe (Pos, Pos) -> App -> App
withMovePlayback before after fx mt evs swapPair app
  | fx == noMoveFx =
      app {appFlash = [], appAnim = AnimNone, appComboShow = 0, appComboBest = 0, appPops = []}
  | null (mtWaves mt) && null (mtEnd mt) =
      let changed = fxCleared fx
          fall = AnimFall (gsBoard after) 0
      in app
           { appFlash = [(p, 18) | p <- changed]
           , appAnim = viaSwap fall
           , appParticles = burstParticles (appPulse app) (gsBoard before) changed ++ appParticles app
           , appComboShow = comboFxFrames fx
           , appComboBest = fxCombo fx
           , appPops = []
           }
  | otherwise =
      app
        { appFlash = []
        , appAnim = viaSwap (AnimCascade (newPlayer (newCascade mt evs (gsBoard after) (gsScore before))))
        , appComboShow = 0
        , appComboBest = 0
        , appPops = []
        }
  where
    viaSwap next = case swapPair of
      Just (p1, p2) -> AnimSwap p1 p2 (gsBoard before) 0 next
      Nothing -> next

-- | 点击 / 空格加速正在播放的连锁回放（输入本身仍被锁定，不会误触下一步）。
accelerate :: Anim -> Anim
accelerate a = case a of
  AnimCascade p -> AnimCascade (acceleratePlayer p)
  AnimSwap {asNext} -> a {asNext = accelerate asNext}
  _ -> a
