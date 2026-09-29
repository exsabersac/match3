{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 界面动作（IO）：刷新窗口标题、关卡重置、三种道具的执行、回放加速、过关后前进 / 重试、解锁记录。
--
-- 依赖：UI.Types、UI.Playback、Match3.Core、SDL（窗口标题）。
-- 规则结果一律来自 Match3.Core（trySwap / use* / trace*），这里只更新 App。
module UI.Actions
  ( updateTitle
  , colorTag
  , freshLevelUi
  , applyHammer
  , applyCrossClear
  , applyFreeSwap
  , speedUp
  , withUnlock
  , advanceOrMsg
  ) where

import ComboFx
import Data.IORef
import qualified Data.Text as T
import Match3.Core
import SDL hiding (Normal)
import System.Random (randomIO)
import UI.Playback
import UI.Types

-- | 把关卡、步数、分数、道具次数、状态与最近提示写到窗口标题（调试 / 无贴图时也能看到）。
updateTitle :: Window -> App -> IO ()
updateTitle window app = do
  let gs = appGame app
      lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
      status = case gsOver gs of
        Just (Won s) -> " CLEAR! score=" <> show s
        Just (LevelClear s n) -> " LEVEL UP ->" <> show (n + 1) <> " score=" <> show s
        Just (Lost s) -> " LOSE score=" <> show s
        _ -> ""
      comboBits =
        if gsCombo gs > 1
          then "  combo x" ++ show (gsCombo gs)
          else ""
      goalBits = case gsGoal gs of
        GoalScore t ->
          "score=" ++ show (gsScore gs) ++ "/" ++ show t
        GoalCollect col n ->
          "collect " ++ colorTag col ++ "=" ++ show (gsCollected gs) ++ "/" ++ show n
        GoalCollectMulti reqs ->
          "multi " ++ show (gsCollected gs) ++ "/" ++ show (sum [n | (_, n) <- reqs])
        GoalClearStone n ->
          "stones=" ++ show (gsStonesCleared gs) ++ "/" ++ show n
        GoalChest n ->
          "chest=" ++ show (gsChestsCleared gs) ++ "/" ++ show n
        GoalHoney n ->
          "honey=" ++ show (gsHoneyCleared gs) ++ "/" ++ show n
        GoalBalloon n ->
          "balloon=" ++ show (gsBalloonsPopped gs) ++ "/" ++ show n
        GoalCookie n ->
          "cookie=" ++ show (gsCookiesCollected gs) ++ "/" ++ show n
        GoalCake n ->
          "cake=" ++ show (gsCakesCleared gs) ++ "/" ++ show n
        GoalSafe n ->
          "safe=" ++ show (gsSafesOpened gs) ++ "/" ++ show n
        GoalUfo n ->
          "ufo=" ++ show (gsUfoCollected gs) ++ "/" ++ show n
        GoalCarpet n ->
          "carpet=" ++ show (gsCarpetsCovered gs) ++ "/" ++ show n
      title =
        T.pack $
          "L"
            ++ show (gsLevel gs + 1)
            ++ " "
            ++ lvlName lvl
            ++ "  "
            ++ goalBits
            ++ "  moves="
            ++ show (gsMoves gs)
            ++ comboBits
            ++ "  Hm="
            ++ show (gsHammers gs)
            ++ " Sw="
            ++ show (gsFreeSwaps gs)
            ++ " Cr="
            ++ show (gsCrossClears gs)
            ++ status
            ++ "  |  "
            ++ T.unpack (appMsg app)
  windowTitle window $= title

-- | 颜色的三字母标签（标题栏用）。
colorTag :: Color -> String
colorTag C1 = "RED"
colorTag C2 = "GRN"
colorTag C3 = "BLU"
colorTag C4 = "YEL"
colorTag C5 = "PRP"

-- | Reset tip/help for a (re)started level; auto-hint on level 1 (index 0).
freshLevelUi :: GameState -> App -> App
freshLevelUi gs app =
  let (gs', _) =
        if gsLevel gs == 0
          then applyHint gs
          else (gs { gsHint = Nothing }, Nothing)
      tip = if gsLevel gs' == 0 then 240 else 0
  in app
       { appGame = gs'
       , appSel = Nothing
       , appMsg = helpKeysMsg
       , appFlash = []
       , appAnim = AnimNone
       , appComboShow = 0
       , appComboBest = 0
       , appPops = []
       , appShake = 0
       , appParticles = []
       , appTipFrames = tip
       , appStartMoves = gsMoves gs
       , appHelpFrames = 300
       , appPaused = False
       , appTool = ToolNone
       , appDragFrom = Nothing
       , appMapOpen = False
       , appMaxReached = max (appMaxReached app) (gsLevel gs')
       }


-- | Apply hammer booster at pos with flash / particles / msg.
applyHammer :: IORef App -> Window -> App -> Pos -> IO App
applyHammer ref window app pos = do
  let (gs', out) = useHammer pos (appGame app)
      -- 特效只看本次调用的 MoveFx（边沿触发），道具无效时不重播上一步连击
      fx = moveFx (appGame app) gs' out
      mt = traceHammer pos (appGame app)
      msg = case out of
        InvalidSwap -> "No hammers left"
        NoMatch -> "Hammer failed"
        MoveApplied g -> "Hammer +" <> T.pack (show g)
        LevelClear _ _ -> "Hammer cleared level!"
        Won _ -> "Hammer won!"
        Lost _ -> T.pack (loseHint (gsGoal gs'))
  let app' =
        withUnlock
          ( withMovePlayback (appGame app) gs' fx mt Nothing
              app
                { appGame = gs'
                , appSel = Nothing
                , appTool = ToolNone
                , appDragFrom = Nothing
                , appMsg = msg
                }
          )
          out
  writeIORef ref app'
  updateTitle window app'
  pure app'


-- | Apply cross-clear booster at pos with flash / particles / msg.
applyCrossClear :: IORef App -> Window -> App -> Pos -> IO App
applyCrossClear ref window app pos = do
  let (gs', out) = useCrossClear pos (appGame app)
      -- 特效只看本次调用的 MoveFx（边沿触发），道具无效时不重播上一步连击
      fx = moveFx (appGame app) gs' out
      mt = traceCrossClear pos (appGame app)
      msg = case out of
        InvalidSwap -> "No cross-clears left"
        NoMatch -> "Cross failed"
        MoveApplied g -> "Cross +" <> T.pack (show g)
        LevelClear _ _ -> "Cross cleared level!"
        Won _ -> "Cross won!"
        Lost _ -> T.pack (loseHint (gsGoal gs'))
  let app' =
        withUnlock
          ( withMovePlayback (appGame app) gs' fx mt Nothing
              app
                { appGame = gs'
                , appSel = Nothing
                , appTool = ToolNone
                , appDragFrom = Nothing
                , appMsg = msg
                }
          )
          out
  writeIORef ref app'
  updateTitle window app'
  pure app'

-- | 执行自由交换道具：规则结果来自 useFreeSwap，回放脚本来自 traceFreeSwap。
applyFreeSwap :: IORef App -> Window -> App -> Pos -> Pos -> IO ()
applyFreeSwap ref window app p1 p2 = do
  let (gs', out) = useFreeSwap p1 p2 (appGame app)
      -- 特效只看本次调用的 MoveFx（边沿触发），道具无效时不重播上一步连击
      fx = moveFx (appGame app) gs' out
      mt = traceFreeSwap p1 p2 (appGame app)
      msg = case out of
        InvalidSwap -> "Free-swap invalid / empty"
        NoMatch -> "Free-swap: no match; not spent"
        MoveApplied g -> "Free-swap +" <> T.pack (show g)
        LevelClear _ _ -> "Free-swap cleared level!"
        Won _ -> "Free-swap won!"
        Lost _ -> T.pack (loseHint (gsGoal gs'))
      -- Only clear tool if charge spent or terminal
      tool' = case out of
        NoMatch -> ToolFreeSwap Nothing
        InvalidSwap -> ToolNone
        _ -> ToolNone
  let app' =
        withUnlock
          ( withMovePlayback (appGame app) gs' fx mt (Just (p1, p2))
              app
                { appGame = gs'
                , appSel = Nothing
                , appTool = tool'
                , appDragFrom = Nothing
                , appMsg = msg
                }
          )
          out
  writeIORef ref app'
  updateTitle window app'

-- | 回放中按下鼠标 / 空格：加速并提示。
speedUp :: IORef App -> Window -> IO ()
speedUp ref window = do
  app <- readIORef ref
  case playingCascade app of
    Just c | not (cFast c) -> do
      let app' = app {appAnim = accelerate (appAnim app), appMsg = "Fast-forward combo"}
      writeIORef ref app'
      updateTitle window app'
    _ -> pure ()

-- | Bump map unlock after LevelClear / Won (daily Won must not unlock campaign).
withUnlock :: App -> Outcome -> App
withUnlock app out =
  app { appMaxReached = unlockAfterOutcome (appGame app) (appMaxReached app) out }

-- | N / Enter / Space / click-on-clear: next level or new campaign.
advanceOrMsg :: IORef App -> Window -> IO ()
advanceOrMsg ref window = do
  seed <- randomIO
  app <- readIORef ref
  case gsOver (appGame app) of
    Just (LevelClear _ n) -> do
      let gs = nextLevel (appGame app) seed
          -- Stars rate vs printed level moves; carry must not inflate the denominator.
          baseMoves = lvlMoves (allLevels !! gsLevel gs)
          app' =
            (freshLevelUi gs app)
              { appMsg = "Next level!"
              , appMaxReached = max (appMaxReached app) n
              , appStartMoves = baseMoves
              }
      writeIORef ref app'
      updateTitle window app'
    Just (Won _) -> do
      let gs = newGameAtLevel 0 (levelConfig (head allLevels)) seed
          app' = (freshLevelUi gs app) { appMsg = "New campaign" }
      writeIORef ref app'
      updateTitle window app'
    Just (Lost _) -> do
      let gs0 = appGame app
          gs =
            if gsDaily gs0
              then newDailyGame (GameConfig (appStartMoves app) (gsGoal gs0)) seed
              else restartLevel gs0 seed
          app' = (freshLevelUi gs app) { appMsg = "Retry!" }
      writeIORef ref app'
      updateTitle window app'
    _ -> do
      let app' = app { appMsg = "Clear the level first (or finish)" }
      writeIORef ref app'
      updateTitle window app'
