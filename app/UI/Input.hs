{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 输入处理（三消插件的输入映射）：SDL 事件（键盘 / 鼠标）→ 界面动作。含播放锁定：animBusy 期间交换、道具、撤销、
-- 洗牌被锁，点击 / 空格 / 回车 / N 变为加速（键位表见 docs/ui-controls.md）。
--
-- 第三刀：原来约 512 行的 handleEvent 拆成 handleKey / handleMouseUp / handleMouseDown，
-- 每个键、每条鼠标路径各一个函数；规则调用一律经通用接口 gameStep（UI.Actions.stepShell / playMove，实例 Match3.Engine.match3Shell），
-- 结果与原来直接调用 trySwap / use* / applyHint / shuffleGame 逐位相同；撤销由 Engine.History 处理（终局后同样可撤销）。
--
-- 依赖：UI.Actions、UI.Playback、UI.LevelMap（地图点选）、UI.Env（鼠标坐标换算）、UI.Types、UI.Layout、Match3.Engine、Match3.View（收集进度后缀）、Engine.GridUI（点选 / 拖动判定）。
module UI.Input
  ( foldEvents
  , handleEvent
  , handleKey
  , handleMouseUp
  , handleMouseDown
  ) where

import Control.Monad (unless)
import Data.IORef
import Data.Int (Int32)
import Data.Maybe (isJust)
import Engine.Game (Step (..))
import Engine.History (Undoable (..), histNow)
import qualified Data.Text as T
import Data.Text (Text)
import Match3.Core
import qualified Match3.Engine as M3E
import SDL hiding (Normal)
import System.Random (randomIO)
import UI.Actions
import UI.Audio (beginLevel, toggleBgm, toggleSfx)
import UI.Env
import Engine.GridUI (Click (..), gridClick, gridDragRelease)
import Match3.View (GameView (..), gameView, goalBracket)
import UI.Layout
import UI.LevelMap
import UI.Types

-- | 处理本帧所有事件；先把鼠标坐标换算为逻辑坐标。返回是否退出。
foldEvents :: IORef App -> Window -> [Event] -> IO Bool
foldEvents ref window = go False
  where
    go q [] = pure q
    go q (e : es) = do
      ms <- appMouseScale <$> readIORef ref
      -- 先把鼠标坐标换算成逻辑坐标，后面的点选 / 拖拽 / 地图 / 按钮判定全部沿用逻辑坐标
      q' <- handleEvent ref window (mouseToLogical ms e)
      go (q || q') es

-- | 单个事件 → 动作（分派到键盘 / 鼠标抬起 / 鼠标按下）。返回是否退出。
handleEvent :: IORef App -> Window -> Event -> IO Bool
handleEvent ref window ev = case eventPayload ev of
  QuitEvent -> pure True
  KeyboardEvent ke
    | keyboardEventKeyMotion ke == Pressed -> handleKey ref window (keysymKeycode (keyboardEventKeysym ke))
    | otherwise -> pure False
  MouseButtonEvent me
    | mouseButtonEventMotion me == Released
        && mouseButtonEventButton me == ButtonLeft -> handleMouseUp ref window me
    | mouseButtonEventMotion me == Pressed
        && mouseButtonEventButton me == ButtonLeft -> handleMouseDown ref window me
    | otherwise -> pure False
  _ -> pure False

-- | 写回 App 并刷新标题。
commit :: IORef App -> Window -> App -> IO ()
commit ref window app' = do
  writeIORef ref app'
  updateTitle window app'

--------------------------------------------------------------------------------
-- 键盘

-- | 按键：Esc / Q / P / R 在任何状态下都响应；暂停时其余键忽略。返回是否退出。
handleKey :: IORef App -> Window -> Keycode -> IO Bool
handleKey ref window code = do
  appGate <- readIORef ref
  case code of
    KeycodeEscape -> pure True
    KeycodeQ -> pure True
    KeycodeP -> False <$ keyPause ref window appGate
    KeycodeK -> False <$ (toggleSfx >> pure ())
    KeycodeB -> False <$ (toggleBgm >> pure ())
    -- Restart works while paused (暂停重开); freshLevelUi clears pause.
    KeycodeR -> False <$ keyRestart ref window
    _
      | appPaused appGate -> pure False
      | otherwise -> False <$ playKey ref window code

-- | 非暂停时的按键表。
playKey :: IORef App -> Window -> Keycode -> IO ()
playKey ref window code = case code of
  KeycodeM -> keyMap ref window
  KeycodeS -> keyShuffle ref window
  KeycodeN -> keySpeedOrAdvance ref window
  KeycodeReturn -> keySpeedOrAdvance ref window
  KeycodeSpace -> keySpeedOrAdvance ref window
  KeycodeU -> keyUndo ref window
  Keycode1 -> keyHammer ref window
  Keycode2 -> keyFreeSwap ref window
  Keycode3 -> keyCross ref window
  KeycodeH -> keyHint ref window
  KeycodeD -> keyDaily ref window
  _ -> pure ()

-- | P：暂停 / 继续。
keyPause :: IORef App -> Window -> App -> IO ()
keyPause ref window appGate = do
  let paused' = not (appPaused appGate)
      app' =
        appGate
          { appPaused = paused'
            -- Drop in-flight drag/selection so resume cannot double-swap.
          , appDragFrom = Nothing
          , appSel = Nothing
          , appMsg =
              if paused'
                then "Paused — R restart / P resume / Esc quit"
                else helpKeysMsg
          , appHelpFrames =
              if paused' then appHelpFrames appGate else 240
          }
  commit ref window app'

-- | 同一关（或同一份每日配置）换种子重开。
restartSame :: App -> Int -> GameState
restartSame app seed =
  let gs0 = appGame app
  in if gsDaily gs0
       then newDailyGame (GameConfig (appStartMoves app) (gsGoal gs0)) seed
       else restartLevel gs0 seed

-- | R：重开本关。
keyRestart :: IORef App -> Window -> IO ()
keyRestart ref window = do
  seed <- randomIO
  app <- readIORef ref
  let app' = (freshLevelUi (restartSame app seed) app) { appMsg = "Restarted level" }
  commit ref window app'
  beginLevel

-- | M：开关关卡地图。
keyMap :: IORef App -> Window -> IO ()
keyMap ref window = do
  app <- readIORef ref
  let app' =
        app
          { appMapOpen = not (appMapOpen app)
          , appPaused = False
          , appMsg =
              if appMapOpen app
                then helpKeysMsg
                else "Level map — click a node (unlocked) / M closes"
          }
  commit ref window app'

-- | S：手动洗牌（播放中 / 已结束时无效）。
keyShuffle :: IORef App -> Window -> IO ()
keyShuffle ref window = do
  app <- readIORef ref
  unless (animBusy app || isJust (gsOver (appGame app))) $ do
    let st = stepShell (Act M3E.Shuffle) app
        gs = histNow (stepState st)
        app' =
          app
            { appHist = stepState st
            , appSel = Nothing
            , appMsg = "Shuffled"
            , appFlash = []
              -- 洗牌不是消除：收掉仍在播的连击角标 / 弹字
            , appComboShow = 0
            , appComboBest = 0
            , appPops = []
            , appParticles = []
            , appShake = 0
            , appAnim = AnimFall { afBoard = gsBoard gs, afFrame = 0 }
            }
    commit ref window app'

-- | N / 回车 / 空格：播放中加速，否则前进 / 重试。
keySpeedOrAdvance :: IORef App -> Window -> IO ()
keySpeedOrAdvance ref window = do
  busy <- animBusy <$> readIORef ref
  if busy then speedUp ref window else advanceOrMsg ref window

-- | U：撤销一步（播放中无效）。
keyUndo :: IORef App -> Window -> IO ()
keyUndo ref window = do
  app <- readIORef ref
  unless (animBusy app) $ do
    let st = stepShell Undo app
    if not (stepAccepted st)
      then commit ref window app { appMsg = "Nothing to undo" }
      else
        commit ref window
          app
            { appHist = stepState st
            , appSel = Nothing
            , appMsg = "Undone"
            , appFlash = []
            , appAnim = AnimNone
            , appComboShow = 0
            , appComboBest = 0
            , appPops = []
            , appParticles = []
            , appShake = 0
            }

-- | 1：锤子（已选格且有次数则立即使用，否则进入 / 退出点选模式）。
keyHammer :: IORef App -> Window -> IO ()
keyHammer ref window = do
  app <- readIORef ref
  unless (animBusy app || isJust (gsOver (appGame app))) $
    case appTool app of
      ToolHammer -> commit ref window app { appTool = ToolNone, appMsg = "Hammer cancelled" }
      _ ->
        case appSel app of
          Just pos | gsHammers (appGame app) > 0 -> () <$ applyHammer ref window app pos
          _ ->
            commit ref window
              app
                { appTool = ToolHammer
                , appSel = Nothing
                , appDragFrom = Nothing
                , appMsg =
                    if gsHammers (appGame app) <= 0
                      then "No hammers left"
                      else "Hammer: click a cell (1 again cancels)"
                }

-- | 2：自由交换点选模式。
keyFreeSwap :: IORef App -> Window -> IO ()
keyFreeSwap ref window = do
  app <- readIORef ref
  unless (animBusy app || isJust (gsOver (appGame app))) $
    case appTool app of
      ToolFreeSwap _ -> commit ref window app { appTool = ToolNone, appSel = Nothing, appMsg = "Free-swap cancelled" }
      _ -> do
        let app' =
              app
                { appTool = ToolFreeSwap Nothing
                , appSel = Nothing
                , appDragFrom = Nothing
                , appMsg =
                    if gsFreeSwaps (appGame app) <= 0
                      then "No free-swaps left"
                      else "Free-swap: click two cells (2 cancels)"
                }
        writeIORef ref
          ( if gsFreeSwaps (appGame app) <= 0
              then app { appMsg = "No free-swaps left", appTool = ToolNone }
              else app'
          )
        updateTitle window =<< readIORef ref

-- | 3：十字清除（已选格且有次数则立即使用，否则进入 / 退出点选模式）。
keyCross :: IORef App -> Window -> IO ()
keyCross ref window = do
  app <- readIORef ref
  unless (animBusy app || isJust (gsOver (appGame app))) $
    case appTool app of
      ToolCross -> commit ref window app { appTool = ToolNone, appMsg = "Cross cancelled" }
      _ ->
        case appSel app of
          Just pos | gsCrossClears (appGame app) > 0 -> () <$ applyCrossClear ref window app pos
          _ ->
            commit ref window
              app
                { appTool = ToolCross
                , appSel = Nothing
                , appDragFrom = Nothing
                , appMsg =
                    if gsCrossClears (appGame app) <= 0
                      then "No cross-clears left"
                      else "Cross: click a cell (3 again cancels)"
                }

-- | H：提示。
keyHint :: IORef App -> Window -> IO ()
keyHint ref window = do
  app <- readIORef ref
  let st = stepShell (Act M3E.Hint) app
      msg = case M3E.pdHint =<< stepReport st of
        Just (p1, p2) ->
          "Hint: " <> T.pack (show p1) <> " <-> " <> T.pack (show p2)
        Nothing -> "No moves — press S to shuffle"
  commit ref window app { appHist = stepState st, appMsg = msg }

-- | D：固定演示日期的每日挑战。
keyDaily :: IORef App -> Window -> IO ()
keyDaily ref window = do
  -- Daily challenge for a fixed demo date (box clock may vary)
  app <- readIORef ref
  let y = 2026; m = 9; d = 29
      lvl = dailyLevel y m d
      gs = newDailyGame (levelConfig lvl) (dailySeed y m d)
  commit ref window (freshLevelUi gs app) { appMsg = "Daily challenge!" }
  beginLevel

--------------------------------------------------------------------------------
-- 鼠标

-- | 左键抬起：拖拽交换（暂停 / 播放中 / 已结束时只清掉拖拽起点）。
handleMouseUp :: IORef App -> Window -> MouseButtonEventData -> IO Bool
handleMouseUp ref window me = do
  app0 <- readIORef ref
  if appPaused app0 || animBusy app0 || isJust (gsOver (appGame app0))
    then do
      -- Drop sticky drag if pause/anim/overlay ate the release.
      case appDragFrom app0 of
        Nothing -> pure False
        Just _ -> do
          writeIORef ref app0 { appDragFrom = Nothing }
          pure False
    else case appDragFrom app0 of
      Nothing -> pure False
      Just p1 -> do
        let P (V2 mx my) = mouseButtonEventPos me
            (nr, nc) = boardDims (gsBoard (appGame app0))
        case gridDragRelease adjacent p1 (pixelToCellOn nr nc mx my) of
          Just (_, p2) -> do
            app <- readIORef ref
            commit ref window (swapTo dragMsg app p1 p2)
            pure False
          Nothing -> do
            writeIORef ref app0 { appDragFrom = Nothing }
            pure False

-- | 左键按下：地图点选优先；暂停时忽略；播放中加速；结束浮层上前进 / 重试；否则点格。
handleMouseDown :: IORef App -> Window -> MouseButtonEventData -> IO Bool
handleMouseDown ref window me = do
  app0 <- readIORef ref
  let P (V2 mx my) = mouseButtonEventPos me
  -- 音效 / BGM 芯片优先（与网页一致；暂停 / 回放中也可点）
  if hitChip sfxChipRect mx my then False <$ toggleSfx
  else if hitChip bgmChipRect mx my then False <$ toggleBgm
  else if appMapOpen app0
    then False <$ mapClick ref window app0 mx my
    else if appPaused app0
    then pure False
    else if animBusy app0
    then do
      -- 回放中点击：加速（不接受新操作）
      speedUp ref window
      pure False
    else do
      -- Click anywhere on overlay advances / retries
      case gsOver (appGame app0) of
        Just (LevelClear _ _) -> False <$ advanceOrMsg ref window
        Just (Won _) -> False <$ advanceOrMsg ref window
        Just (Lost _) -> do
          seed <- randomIO
          app <- readIORef ref
          commit ref window (freshLevelUi (restartSame app seed) app) { appMsg = "Retry!" }
          beginLevel
          pure False
        _ ->
          let (nr, nc) = boardDims (gsBoard (appGame app0))
          in case pixelToCellOn nr nc mx my of
            Nothing -> pure False
            Just pos -> False <$ cellClick ref window pos

-- | 地图打开时的点击：跳关 / 关闭地图。
mapClick :: IORef App -> Window -> App -> Int32 -> Int32 -> IO ()
mapClick ref window app0 mx0 my0 =
  case mapHitTest mx0 my0 of
    Just li ->
      case mapClickJump (gsLevel (appGame app0)) (appMaxReached app0) li of
        Just jump | Just lvl <- lookupLevel jump -> do
          seed <- randomIO
          let gs = newGameAtLevel jump (levelConfig lvl) seed
          commit ref window
            (freshLevelUi gs app0)
              { appMapOpen = False
              , appMsg = "Map -> L" <> T.pack (show (jump + 1)) <> " " <> T.pack (lvlName lvl)
              , appMaxReached = max (appMaxReached app0) jump
              }
          beginLevel
        _ ->
          -- Same level / locked: close map and resume (keep mid-level progress)
          commit ref window app0 { appMapOpen = False, appMsg = helpKeysMsg }
    Nothing ->
      -- click outside nodes closes map
      commit ref window app0 { appMapOpen = False, appMsg = helpKeysMsg }

-- | 点中棋盘格：按当前道具模式分派（锤子 / 十字 / 自由交换 / 普通选中与点击交换）。
cellClick :: IORef App -> Window -> Pos -> IO ()
cellClick ref window pos = do
  app <- readIORef ref
  case appTool app of
    ToolHammer
      | gsHammers (appGame app) <= 0 -> commit ref window app { appTool = ToolNone, appMsg = "No hammers left" }
      | otherwise -> () <$ applyHammer ref window app pos
    ToolCross
      | gsCrossClears (appGame app) <= 0 -> commit ref window app { appTool = ToolNone, appMsg = "No cross-clears left" }
      | otherwise -> () <$ applyCrossClear ref window app pos
    -- 两步点选（Engine.GridUI.gridClick）：自由交换的第一格记在工具模式里，普通模式记在 appSel
    ToolFreeSwap first -> case gridClick first pos of
      ClickSelect p ->
        commit ref window
          app
            { appTool = ToolFreeSwap (Just p)
            , appSel = Just p
            , appMsg = "Free-swap: click second cell"
            }
      ClickDeselect ->
        commit ref window
          app
            { appTool = ToolFreeSwap Nothing
            , appSel = Nothing
            , appMsg = "Free-swap: pick first cell again"
            }
      ClickPair p1 p2 -> applyFreeSwap ref window app p1 p2
    ToolNone ->
      case gridClick (appSel app) pos of
        ClickSelect p -> commit ref window app { appSel = Just p, appDragFrom = Just p, appMsg = "Selected; click/drag adjacent" }
        ClickDeselect -> commit ref window app { appSel = Nothing, appMsg = "Deselected" }
        ClickPair p1 p2 -> commit ref window (swapTo clickMsg app p1 p2)

--------------------------------------------------------------------------------
-- 交换

-- | 交换 p1 ↔ p2：经 Match3.Engine 结算一次，按提示文案函数写消息，编排回放并记录解锁。
-- 无匹配回滚 / 非相邻：fx 为空，不闪光、不播连击（修复重播上一步爆击特效）。
swapTo :: (GameState -> GameState -> MoveFx -> Outcome -> Text) -> App -> Pos -> Pos -> App
swapTo msgOf app p1 p2 =
  let before = appGame app
      (pd, out, h') = playMove (M3E.Swap p1 p2) app
      gs' = M3E.pdState pd
      -- Combo SFX placeholder: when audio lands, play a rising
      -- pitched blip on each EvHighlight k >= 2 (cascade wave cheer).
  in withUnlock
       ( playbackOf before pd (Just (p1, p2))
           app
             { appHist = h'
             , appSel = Nothing
             , appDragFrom = Nothing
             , appMsg = msgOf before gs' (M3E.pdFx pd) out
             , appTipFrames = 0
             , appSounds = hear out ++ appSounds app
             }
       )
       out

-- | 拖拽交换的提示文案。
dragMsg :: GameState -> GameState -> MoveFx -> Outcome -> Text
dragMsg before _ _ out = case out of
  NoMatch -> "No match; rolled back"
  InvalidSwap -> "Need adjacent"
  MoveApplied s -> "Drag +" <> T.pack (show s)
  Won s -> "YOU WIN score=" <> T.pack (show s)
  LevelClear _ n -> "Level clear -> L" <> T.pack (show (n + 1))
  Lost s -> "Out of moves score=" <> T.pack (show s) <> " — " <> T.pack (loseHint (gsGoal before))

-- | 点击交换的提示文案（含连击 / 收集进度 / 自动洗牌）。
clickMsg :: GameState -> GameState -> MoveFx -> Outcome -> Text
clickMsg before gs' fx out =
  let shuffledMsg =
        if gsShuffled gs' then " (auto-shuffled)" else ""
      comboMsg =
        if fxCombo fx > 1
          then " combo x" <> T.pack (show (fxCombo fx))
          else ""
  in case out of
       InvalidSwap -> "Need 4-neighbor adjacent"
       NoMatch -> "No match; rolled back"
       MoveApplied s ->
         "Cleared +"
           <> T.pack (show s)
           <> comboMsg
           <> collectMsg gs'
           <> T.pack shuffledMsg
       Won s -> "YOU WIN score=" <> T.pack (show s) <> " — N/click"
       LevelClear s n ->
         "Level clear +"
           <> T.pack (show s)
           <> comboMsg
           <> " -> L"
           <> T.pack (show (n + 1))
           <> " (N/Space/click)"
       Lost s -> "Out of moves score=" <> T.pack (show s) <> " — " <> T.pack (loseHint (gsGoal before)) <> " — R/click"

-- | 收集类目标的进度后缀（第 11 刀起读 Match3.View.goalBracket）。
collectMsg :: GameState -> Text
collectMsg gs' = T.pack (goalBracket (gvGoal (gameView gs')))

-- | 交换结果要播的音效（表现层；规则不发声）。非法与换不掉都用 illegal。
hear :: Outcome -> [String]
hear InvalidSwap = ["illegal"]
hear NoMatch = ["illegal"]
hear (MoveApplied _) = ["swap"]
hear (Won _) = ["swap", "win"]
hear (Lost _) = ["lose"]
hear (LevelClear _ _) = ["swap", "win"]
