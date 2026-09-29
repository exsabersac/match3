{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 输入处理：SDL 事件（键盘 / 鼠标）→ 界面动作。含播放锁定：animBusy 期间交换、道具、撤销、
-- 洗牌被锁，点击 / 空格 / 回车 / N 变为加速（键位表见 docs/ui-controls.md）。
--
-- 依赖：UI.Actions、UI.Playback、UI.LevelMap（地图点选）、UI.Env（鼠标坐标换算）、UI.Types、UI.Layout。
module UI.Input
  ( foldEvents
  , handleEvent
  ) where

import Control.Monad (unless)
import Data.IORef
import Data.Maybe (isJust)
import qualified Data.Text as T
import Match3.Core
import SDL hiding (Normal)
import System.Random (randomIO)
import UI.Actions
import UI.Env
import UI.Layout
import UI.LevelMap
import UI.Playback
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

-- | 单个事件 → 动作。Esc / Q / P / R 在任何状态下都响应；暂停时其余键忽略；
-- 播放中（animBusy）交换、道具、撤销、洗牌被锁，点击与 N / 空格 / 回车只做加速。返回是否退出。
handleEvent :: IORef App -> Window -> Event -> IO Bool
handleEvent ref window ev = case eventPayload ev of
  QuitEvent -> pure True
  KeyboardEvent ke
    | keyboardEventKeyMotion ke == Pressed -> do
        appGate <- readIORef ref
        let code = keysymKeycode (keyboardEventKeysym ke)
        case code of
          KeycodeEscape -> pure True
          KeycodeQ -> pure True
          KeycodeP -> do
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
            writeIORef ref app'
            updateTitle window app'
            pure False
          -- Restart works while paused (暂停重开); freshLevelUi clears pause.
          KeycodeR -> do
            seed <- randomIO
            app <- readIORef ref
            let gs0 = appGame app
                gs =
                  if gsDaily gs0
                    then newDailyGame (GameConfig (appStartMoves app) (gsGoal gs0)) seed
                    else restartLevel gs0 seed
                app' = (freshLevelUi gs app) { appMsg = "Restarted level" }
            writeIORef ref app'
            updateTitle window app'
            pure False
          _
            | appPaused appGate -> pure False
            | otherwise -> case code of
                KeycodeM -> do
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
                  writeIORef ref app'
                  updateTitle window app'
                  pure False
                KeycodeS -> do
                  app <- readIORef ref
                  unless (animBusy app || isJust (gsOver (appGame app))) $ do
                    let gs = shuffleGame (appGame app)
                        app' =
                          app
                            { appGame = gs
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
                    writeIORef ref app'
                    updateTitle window app'
                  pure False
                KeycodeN -> do
                  busy <- animBusy <$> readIORef ref
                  if busy then speedUp ref window else advanceOrMsg ref window
                  pure False
                KeycodeReturn -> do
                  busy <- animBusy <$> readIORef ref
                  if busy then speedUp ref window else advanceOrMsg ref window
                  pure False
                KeycodeSpace -> do
                  busy <- animBusy <$> readIORef ref
                  if busy then speedUp ref window else advanceOrMsg ref window
                  pure False
                KeycodeU -> do
                  app <- readIORef ref
                  unless (animBusy app) $
                    case undoMove (appGame app) of
                      Nothing -> do
                        let app' = app { appMsg = "Nothing to undo" }
                        writeIORef ref app'
                        updateTitle window app'
                      Just gs -> do
                        let app' =
                              app
                                { appGame = gs
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
                        writeIORef ref app'
                        updateTitle window app'
                  pure False
                Keycode1 -> do
                  app <- readIORef ref
                  unless (animBusy app || isJust (gsOver (appGame app))) $ do
                    case appTool app of
                      ToolHammer -> do
                        let app' = app { appTool = ToolNone, appMsg = "Hammer cancelled" }
                        writeIORef ref app'
                        updateTitle window app'
                      _ ->
                        case appSel app of
                          Just pos | gsHammers (appGame app) > 0 -> do
                            _ <- applyHammer ref window app pos
                            pure ()
                          _ -> do
                            let app' =
                                  app
                                    { appTool = ToolHammer
                                    , appSel = Nothing
                                    , appDragFrom = Nothing
                                    , appMsg =
                                        if gsHammers (appGame app) <= 0
                                          then "No hammers left"
                                          else "Hammer: click a cell (1 again cancels)"
                                    }
                            writeIORef ref app'
                            updateTitle window app'
                  pure False
                Keycode2 -> do
                  app <- readIORef ref
                  unless (animBusy app || isJust (gsOver (appGame app))) $ do
                    case appTool app of
                      ToolFreeSwap _ -> do
                        let app' = app { appTool = ToolNone, appSel = Nothing, appMsg = "Free-swap cancelled" }
                        writeIORef ref app'
                        updateTitle window app'
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
                  pure False
                Keycode3 -> do
                  app <- readIORef ref
                  unless (animBusy app || isJust (gsOver (appGame app))) $ do
                    case appTool app of
                      ToolCross -> do
                        let app' = app { appTool = ToolNone, appMsg = "Cross cancelled" }
                        writeIORef ref app'
                        updateTitle window app'
                      _ ->
                        case appSel app of
                          Just pos | gsCrossClears (appGame app) > 0 -> do
                            _ <- applyCrossClear ref window app pos
                            pure ()
                          _ -> do
                            let app' =
                                  app
                                    { appTool = ToolCross
                                    , appSel = Nothing
                                    , appDragFrom = Nothing
                                    , appMsg =
                                        if gsCrossClears (appGame app) <= 0
                                          then "No cross-clears left"
                                          else "Cross: click a cell (3 again cancels)"
                                    }
                            writeIORef ref app'
                            updateTitle window app'
                  pure False
                KeycodeH -> do
                  app <- readIORef ref
                  let (gs, h) = applyHint (appGame app)
                      msg = case h of
                        Just (p1, p2) ->
                          "Hint: " <> T.pack (show p1) <> " <-> " <> T.pack (show p2)
                        Nothing -> "No moves — press S to shuffle"
                      app' = app { appGame = gs, appMsg = msg }
                  writeIORef ref app'
                  updateTitle window app'
                  pure False
                KeycodeD -> do
                  -- Daily challenge for a fixed demo date (box clock may vary)
                  app <- readIORef ref
                  let y = 2026; m = 9; d = 29
                      lvl = dailyLevel y m d
                      gs = newDailyGame (levelConfig lvl) (dailySeed y m d)
                      app' = (freshLevelUi gs app) { appMsg = "Daily challenge!" }
                  writeIORef ref app'
                  updateTitle window app'
                  pure False
                _ -> pure False
    | otherwise -> pure False
  MouseButtonEvent me
    | mouseButtonEventMotion me == Released
        && mouseButtonEventButton me == ButtonLeft -> do
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
              case pixelToCell mx my of
                Just p2 | p1 /= p2 && adjacent p1 p2 -> do
                  app <- readIORef ref
                  let (gs', out) = trySwap p1 p2 (appGame app)
                      -- 无匹配回滚：fx 为空，不闪光、不播连击（修复重播上一步爆击特效）
                      fx = moveFx (appGame app) gs' out
                      mt = traceSwap p1 p2 (appGame app)
                  let msg = case out of
                        NoMatch -> "No match; rolled back"
                        InvalidSwap -> "Need adjacent"
                        MoveApplied s -> "Drag +" <> T.pack (show s)
                        Won s -> "YOU WIN score=" <> T.pack (show s)
                        LevelClear _ n -> "Level clear -> L" <> T.pack (show (n + 1))
                        Lost s -> "Out of moves score=" <> T.pack (show s) <> " — " <> T.pack (loseHint (gsGoal (appGame app)))
                      app' =
                        withUnlock
                          ( withMovePlayback (appGame app) gs' fx mt (Just (p1, p2))
                              app
                                { appGame = gs'
                                , appSel = Nothing
                                , appDragFrom = Nothing
                                , appMsg = msg
                                , appTipFrames = 0
                                }
                          )
                          out
                  writeIORef ref app'
                  updateTitle window app'
                  pure False
                _ -> do
                  writeIORef ref app0 { appDragFrom = Nothing }
                  pure False
    | mouseButtonEventMotion me == Pressed
        && mouseButtonEventButton me == ButtonLeft -> do
        app0 <- readIORef ref
        let P (V2 mx0 my0) = mouseButtonEventPos me
        -- Level map click takes priority
        if appMapOpen app0
          then case mapHitTest mx0 my0 of
            Just li ->
              case mapClickJump (gsLevel (appGame app0)) (appMaxReached app0) li of
                Just jump -> do
                  seed <- randomIO
                  let lvl = allLevels !! jump
                      gs = newGameAtLevel jump (levelConfig lvl) seed
                      app' =
                        (freshLevelUi gs app0)
                          { appMapOpen = False
                          , appMsg = "Map -> L" <> T.pack (show (jump + 1)) <> " " <> T.pack (lvlName lvl)
                          , appMaxReached = max (appMaxReached app0) jump
                          }
                  writeIORef ref app'
                  updateTitle window app'
                  pure False
                Nothing -> do
                  -- Same level / locked: close map and resume (keep mid-level progress)
                  let app' = app0 { appMapOpen = False, appMsg = helpKeysMsg }
                  writeIORef ref app'
                  updateTitle window app'
                  pure False
            Nothing -> do
              -- click outside nodes closes map
              let app' = app0 { appMapOpen = False, appMsg = helpKeysMsg }
              writeIORef ref app'
              updateTitle window app'
              pure False
          else if appPaused app0
          then pure False
          else if animBusy app0
          then do
            -- 回放中点击：加速（不接受新操作）
            speedUp ref window
            pure False
          else do
            let P (V2 mx my) = mouseButtonEventPos me
            -- Click anywhere on overlay advances / retries
            case gsOver (appGame app0) of
              Just (LevelClear _ _) -> do
                advanceOrMsg ref window
                pure False
              Just (Won _) -> do
                advanceOrMsg ref window
                pure False
              Just (Lost _) -> do
                seed <- randomIO
                app <- readIORef ref
                let gs0 = appGame app
                    gs =
                      if gsDaily gs0
                        then newDailyGame (GameConfig (appStartMoves app) (gsGoal gs0)) seed
                        else restartLevel gs0 seed
                    app' = (freshLevelUi gs app) { appMsg = "Retry!" }
                writeIORef ref app'
                updateTitle window app'
                pure False
              _ ->
                case pixelToCell mx my of
                  Nothing -> pure False
                  Just pos -> do
                    app <- readIORef ref
                    case appTool app of
                      ToolHammer -> do
                        if gsHammers (appGame app) <= 0
                          then do
                            let app' = app { appTool = ToolNone, appMsg = "No hammers left" }
                            writeIORef ref app'
                            updateTitle window app'
                          else do
                            _ <- applyHammer ref window app pos
                            pure ()
                        pure False
                      ToolCross -> do
                        if gsCrossClears (appGame app) <= 0
                          then do
                            let app' = app { appTool = ToolNone, appMsg = "No cross-clears left" }
                            writeIORef ref app'
                            updateTitle window app'
                          else do
                            _ <- applyCrossClear ref window app pos
                            pure ()
                        pure False
                      ToolFreeSwap Nothing -> do
                        let app' =
                              app
                                { appTool = ToolFreeSwap (Just pos)
                                , appSel = Just pos
                                , appMsg = "Free-swap: click second cell"
                                }
                        writeIORef ref app'
                        updateTitle window app'
                        pure False
                      ToolFreeSwap (Just p1)
                        | p1 == pos -> do
                            let app' =
                                  app
                                    { appTool = ToolFreeSwap Nothing
                                    , appSel = Nothing
                                    , appMsg = "Free-swap: pick first cell again"
                                    }
                            writeIORef ref app'
                            updateTitle window app'
                            pure False
                        | otherwise -> do
                            applyFreeSwap ref window app p1 pos
                            pure False
                      ToolNone ->
                        case appSel app of
                          Nothing -> do
                            let app' = app { appSel = Just pos, appDragFrom = Just pos, appMsg = "Selected; click/drag adjacent" }
                            writeIORef ref app'
                            updateTitle window app'
                            pure False
                          Just p1
                            | p1 == pos -> do
                                let app' = app { appSel = Nothing, appMsg = "Deselected" }
                                writeIORef ref app'
                                updateTitle window app'
                                pure False
                            | otherwise -> do
                            let (gs', out) = trySwap p1 pos (appGame app)
                                -- 无匹配回滚 / 非相邻：fx 为空，不闪光、不播连击（修复重播上一步爆击特效）
                                fx = moveFx (appGame app) gs' out
                                mt = traceSwap p1 pos (appGame app)
                                shuffledMsg =
                                  if gsShuffled gs' then " (auto-shuffled)" else ""
                                comboMsg =
                                  if fxCombo fx > 1
                                    then " combo x" <> T.pack (show (fxCombo fx))
                                    else ""
                                collectMsg = case gsGoal gs' of
                                  GoalCollect col n ->
                                    " ["
                                      <> T.pack (colorTag col)
                                      <> " "
                                      <> T.pack (show (gsCollected gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalCollectMulti reqs ->
                                    " [multi "
                                      <> T.pack (show (gsCollected gs'))
                                      <> "/"
                                      <> T.pack (show (sum [n | (_, n) <- reqs]))
                                      <> "]"
                                  GoalClearStone n ->
                                    " [stones "
                                      <> T.pack (show (gsStonesCleared gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalChest n ->
                                    " [chest "
                                      <> T.pack (show (gsChestsCleared gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalHoney n ->
                                    " [honey "
                                      <> T.pack (show (gsHoneyCleared gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalBalloon n ->
                                    " [balloon "
                                      <> T.pack (show (gsBalloonsPopped gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalCookie n ->
                                    " [cookie "
                                      <> T.pack (show (gsCookiesCollected gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalCake n ->
                                    " [cake "
                                      <> T.pack (show (gsCakesCleared gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalSafe n ->
                                    " [safe "
                                      <> T.pack (show (gsSafesOpened gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalUfo n ->
                                    " [ufo "
                                      <> T.pack (show (gsUfoCollected gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  GoalCarpet n ->
                                    " [carpet "
                                      <> T.pack (show (gsCarpetsCovered gs'))
                                      <> "/"
                                      <> T.pack (show n)
                                      <> "]"
                                  _ -> ""
                                msg = case out of
                                  InvalidSwap -> "Need 4-neighbor adjacent"
                                  NoMatch -> "No match; rolled back"
                                  MoveApplied s ->
                                    "Cleared +"
                                      <> T.pack (show s)
                                      <> comboMsg
                                      <> collectMsg
                                      <> T.pack shuffledMsg
                                  Won s -> "YOU WIN score=" <> T.pack (show s) <> " — N/click"
                                  LevelClear s n ->
                                    "Level clear +"
                                      <> T.pack (show s)
                                      <> comboMsg
                                      <> " -> L"
                                      <> T.pack (show (n + 1))
                                      <> " (N/Space/click)"
                                  Lost s -> "Out of moves score=" <> T.pack (show s) <> " — " <> T.pack (loseHint (gsGoal (appGame app))) <> " — R/click"
                                -- Combo SFX placeholder: when audio lands, play a rising
                                -- pitched blip on each EvHighlight k >= 2 (cascade wave cheer).
                            let app' =
                                  withUnlock
                                    ( withMovePlayback (appGame app) gs' fx mt (Just (p1, pos))
                                        app
                                          { appGame = gs'
                                          , appSel = Nothing
                                          , appDragFrom = Nothing
                                          , appMsg = msg
                                          , appTipFrames = 0
                                          }
                                    )
                                    out
                            writeIORef ref app'
                            updateTitle window app'
                            pure False
    | otherwise -> pure False
  _ -> pure False
