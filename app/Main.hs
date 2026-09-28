{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import System.Random (randomIO, randomRIO)

cellPx, padPx, hudH, boardPx, winW, winH :: CInt
cellPx = 56
padPx = 16
hudH = 108
boardPx = cellPx * fromIntegral boardSize
winW = padPx * 2 + boardPx
winH = padPx * 2 + boardPx + hudH

swapFrames, fallFrames :: Int
swapFrames = 10
fallFrames = 12

-- | Visual-only animation; rules already applied.
data Anim
  = AnimNone
  | AnimSwap
      { asP1 :: Pos
      , asP2 :: Pos
      , asBefore :: Board
      , asAfter :: Board
      , asFrame :: Int
      }
  | AnimFall
      { afBoard :: Board
      , afFrame :: Int
      }

-- | Simple rectangle particle (no textures).
data Particle = Particle
  { pX     :: Float
  , pY     :: Float
  , pVX    :: Float
  , pVY    :: Float
  , pLife  :: Int
  , pMax   :: Int
  , pR     :: Word8
  , pG     :: Word8
  , pB     :: Word8
  , pSize  :: CInt
  }

-- | Booster click-flow modes (开心消消乐道具点选).
data ToolMode
  = ToolNone
  | ToolHammer          -- next cell click hammers
  | ToolFreeSwap (Maybe Pos)  -- first click stores, second free-swaps
  deriving (Eq, Show)

data App = App
  { appGame      :: GameState
  , appSel       :: Maybe Pos
  , appMsg       :: Text
  , appFlash     :: [(Pos, Int)]
  , appPulse     :: Int
  , appAnim      :: Anim
  , appComboShow :: Int  -- frames left to highlight combo
  , appParticles :: [Particle]
  , appTipFrames :: Int  -- first-level tip/highlight countdown
  , appHelpFrames :: Int -- brief help strip after start / unpause
  , appPaused    :: Bool -- pause + full key help overlay
  , appStartMoves :: MovesLeft
  , appDragFrom  :: Maybe Pos
  , appTool      :: ToolMode
  }

colorRGB :: Color -> (Word8, Word8, Word8)
colorRGB C1 = (220, 70, 70)
colorRGB C2 = (70, 180, 90)
colorRGB C3 = (70, 120, 220)
colorRGB C4 = (240, 200, 60)
colorRGB C5 = (180, 80, 200)

helpKeysMsg :: Text
helpKeysMsg = "H hint | 1 hammer | 2 free-swap | U undo | S shuffle | D daily | R restart | N next | P pause | Esc"

main :: IO ()
main = do
  initializeAll
  seed <- randomIO
  let lvl = head allLevels
      gs0 = newGameAtLevel 0 (levelConfig lvl) seed
  window <-
    createWindow
      "Match-3"
      defaultWindow { windowInitialSize = V2 winW winH }
  renderer <- createRenderer window (-1) defaultRenderer
  let (gsHinted, _) = applyHint gs0
  ref <-
    newIORef
      App
        { appGame = gsHinted
        , appSel = Nothing
        , appMsg = helpKeysMsg
        , appFlash = []
        , appPulse = 0
        , appAnim = AnimNone
        , appComboShow = 0
        , appParticles = []
        , appTipFrames = 240
        , appHelpFrames = 300
        , appPaused = False
        , appStartMoves = lvlMoves lvl
        , appDragFrom = Nothing
        , appTool = ToolNone
        }
  updateTitle window =<< readIORef ref
  let loop = do
        events <- pollEvents
        shouldQuit <- foldEvents ref window events
        modifyIORef' ref tickAnim
        app <- readIORef ref
        draw renderer app
        present renderer
        threadDelay 16000
        unless shouldQuit loop
  loop
  destroyRenderer renderer
  destroyWindow window
  quit

tickAnim :: App -> App
tickAnim app =
  let flash' = [ (p, n - 1) | (p, n) <- appFlash app, n > 1 ]
      combo' = max 0 (appComboShow app - 1)
      tip' = if appPaused app then appTipFrames app else max 0 (appTipFrames app - 1)
      help' = if appPaused app then appHelpFrames app else max 0 (appHelpFrames app - 1)
      anim' = case appAnim app of
        AnimNone -> AnimNone
        AnimSwap { asP1, asP2, asBefore, asAfter, asFrame }
          | asFrame + 1 >= swapFrames ->
              AnimFall { afBoard = asAfter, afFrame = 0 }
          | otherwise ->
              AnimSwap asP1 asP2 asBefore asAfter (asFrame + 1)
        AnimFall { afBoard, afFrame }
          | afFrame + 1 >= fallFrames -> AnimNone
          | otherwise -> AnimFall afBoard (afFrame + 1)
      parts' = tickParticles (appParticles app)
  in app
       { appFlash = flash'
       , appPulse = appPulse app + 1
       , appAnim = anim'
       , appComboShow = combo'
       , appParticles = parts'
       , appTipFrames = tip'
       , appHelpFrames = help'
       }

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

-- | Spawn burst particles at cleared cell centers (colors from before-board).
spawnBurst :: Board -> [Pos] -> IO [Particle]
spawnBurst board positions =
  fmap concat $
    mapM
      ( \pos -> do
          let (ox, oy) = cellOrigin pos
              cx = fromIntegral ox + fromIntegral cellPx / 2
              cy = fromIntegral oy + fromIntegral cellPx / 2
              (cr, cg, cb) = case getCell board pos of
                    Stone _ -> (120, 120, 130)
                    Countdown _ _ -> colorRGB (cellColor (getCell board pos))
                    Gem _ _ _ _ -> colorRGB (cellColor (getCell board pos))
          mapM
            ( \_ -> do
                ang <- randomRIO (0, 2 * pi :: Float)
                spd <- randomRIO (1.2, 4.5 :: Float)
                life <- randomRIO (18, 36 :: Int)
                sz <- randomRIO (3, 7 :: Int)
                pure
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
            )
            [1 .. 5 :: Int]
      )
      positions

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
            ++ status
            ++ "  |  "
            ++ T.unpack (appMsg app)
  windowTitle window $= title

colorTag :: Color -> String
colorTag C1 = "RED"
colorTag C2 = "GRN"
colorTag C3 = "BLU"
colorTag C4 = "YEL"
colorTag C5 = "PRP"

foldEvents :: IORef App -> Window -> [Event] -> IO Bool
foldEvents ref window = go False
  where
    go q [] = pure q
    go q (e : es) = do
      q' <- handleEvent ref window e
      go (q || q') es

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
       , appParticles = []
       , appTipFrames = tip
       , appStartMoves = gsMoves gs
       , appHelpFrames = 300
       , appPaused = False
       , appTool = ToolNone
       , appDragFrom = Nothing
       }


-- | Apply hammer booster at pos with flash / particles / msg.
applyHammer :: IORef App -> Window -> App -> Pos -> IO App
applyHammer ref window app pos = do
  let before = gsBoard (appGame app)
      (gs', out) = useHammer pos (appGame app)
      after = gsBoard gs'
      changed =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , getCell before p /= getCell after p
        ]
      flash = case out of
        NoMatch -> []
        InvalidSwap -> []
        _ -> [(p, 18) | p <- changed]
      msg = case out of
        InvalidSwap -> "No hammers left"
        NoMatch -> "Hammer failed"
        MoveApplied g -> "Hammer +" <> T.pack (show g)
        LevelClear _ _ -> "Hammer cleared level!"
        Won _ -> "Hammer won!"
        Lost _ -> T.pack (loseHint (gsGoal gs'))
  parts <-
    if null flash
      then pure (appParticles app)
      else do
        burst <- spawnBurst before changed
        pure (burst ++ appParticles app)
  let app' =
        app
          { appGame = gs'
          , appSel = Nothing
          , appTool = ToolNone
          , appDragFrom = Nothing
          , appMsg = msg
          , appFlash = flash
          , appAnim = if null flash then AnimNone else AnimFall { afBoard = after, afFrame = 0 }
          , appComboShow = if gsCombo gs' > 1 then 90 else 0
          , appParticles = parts
          }
  writeIORef ref app'
  updateTitle window app'
  pure app'

applyFreeSwap :: IORef App -> Window -> App -> Pos -> Pos -> IO ()
applyFreeSwap ref window app p1 p2 = do
  let before = gsBoard (appGame app)
      (gs', out) = useFreeSwap p1 p2 (appGame app)
      after = gsBoard gs'
      changed =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , getCell before p /= getCell after p
        ]
      flash = case out of
        NoMatch -> []
        InvalidSwap -> []
        _ -> [(p, 18) | p <- changed]
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
  parts <-
    if null flash
      then pure (appParticles app)
      else do
        burst <- spawnBurst before changed
        pure (burst ++ appParticles app)
  let app' =
        app
          { appGame = gs'
          , appSel = Nothing
          , appTool = tool'
          , appDragFrom = Nothing
          , appMsg = msg
          , appFlash = flash
          , appAnim = if null flash then AnimNone else AnimFall { afBoard = after, afFrame = 0 }
          , appComboShow = if gsCombo gs' > 1 then 90 else 0
          , appParticles = parts
          }
  writeIORef ref app'
  updateTitle window app'

-- | Ignore input while a tween is playing (rules already committed).
animBusy :: App -> Bool
animBusy app = case appAnim app of
  AnimNone -> False
  _ -> True

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
                    , appMsg =
                        if paused'
                          then "Paused — keys: H U S R N P Esc"
                          else helpKeysMsg
                    , appHelpFrames =
                        if paused' then appHelpFrames appGate else 240
                    }
            writeIORef ref app'
            updateTitle window app'
            pure False
          _
            | appPaused appGate -> pure False
            | otherwise -> case code of
                KeycodeR -> do
                  seed <- randomIO
                  app <- readIORef ref
                  let gs = restartLevel (appGame app) seed
                      app' = (freshLevelUi gs app) { appMsg = "Restarted level" }
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
                            , appAnim = AnimFall { afBoard = gsBoard gs, afFrame = 0 }
                            }
                    writeIORef ref app'
                    updateTitle window app'
                  pure False
                KeycodeN -> do
                  advanceOrMsg ref window
                  pure False
                KeycodeReturn -> do
                  advanceOrMsg ref window
                  pure False
                KeycodeSpace -> do
                  advanceOrMsg ref window
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
                                , appParticles = []
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
                      gs = newGameAtLevel 0 (levelConfig lvl) (dailySeed y m d)
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
          then pure False
          else case appDragFrom app0 of
            Nothing -> pure False
            Just p1 -> do
              let P (V2 mx my) = mouseButtonEventPos me
              case pixelToCell mx my of
                Just p2 | p1 /= p2 && adjacent p1 p2 -> do
                  app <- readIORef ref
                  let before = gsBoard (appGame app)
                      (gs', out) = trySwap p1 p2 (appGame app)
                      after = gsBoard gs'
                      changed =
                        [ p
                        | r <- [0 .. boardSize - 1]
                        , c <- [0 .. boardSize - 1]
                        , let p = (r, c)
                        , getCell before p /= getCell after p
                        ]
                      flash = case out of
                        NoMatch -> []
                        InvalidSwap -> []
                        _ -> [(p, 18) | p <- changed]
                      anim = case out of
                        MoveApplied _ -> AnimSwap p1 p2 before after 0
                        Won _ -> AnimSwap p1 p2 before after 0
                        Lost _ -> AnimSwap p1 p2 before after 0
                        LevelClear _ _ -> AnimSwap p1 p2 before after 0
                        _ -> AnimNone
                  parts <-
                    if null flash
                      then pure (appParticles app)
                      else do
                        burst <- spawnBurst before changed
                        pure (burst ++ appParticles app)
                  let msg = case out of
                        NoMatch -> "No match; rolled back"
                        InvalidSwap -> "Need adjacent"
                        MoveApplied s -> "Drag +" <> T.pack (show s)
                        Won s -> "YOU WIN score=" <> T.pack (show s)
                        LevelClear _ n -> "Level clear -> L" <> T.pack (show (n + 1))
                        Lost s -> "Out of moves score=" <> T.pack (show s) <> " — " <> T.pack (loseHint (gsGoal (appGame app)))
                      app' =
                        app
                          { appGame = gs'
                          , appSel = Nothing
                          , appDragFrom = Nothing
                          , appMsg = msg
                          , appFlash = flash
                          , appAnim = anim
                          , appComboShow = if gsCombo gs' > 1 then 90 else 0
                          , appParticles = parts
                          , appTipFrames = 0
                          }
                  writeIORef ref app'
                  updateTitle window app'
                  pure False
                _ -> do
                  writeIORef ref app0 { appDragFrom = Nothing }
                  pure False
    | mouseButtonEventMotion me == Pressed
        && mouseButtonEventButton me == ButtonLeft -> do
        app0 <- readIORef ref
        if appPaused app0 || animBusy app0
          then pure False
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
                let gs = restartLevel (appGame app) seed
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
                            let before = gsBoard (appGame app)
                                (gs', out) = trySwap p1 pos (appGame app)
                                after = gsBoard gs'
                                changed =
                                  [ p
                                  | r <- [0 .. boardSize - 1]
                                  , c <- [0 .. boardSize - 1]
                                  , let p = (r, c)
                                  , getCell before p /= getCell after p
                                  ]
                                flash =
                                  case out of
                                    NoMatch -> []
                                    InvalidSwap -> []
                                    _ -> [(p, 18) | p <- changed]
                                shuffledMsg =
                                  if gsShuffled gs' then " (auto-shuffled)" else ""
                                comboMsg =
                                  if gsCombo gs' > 1
                                    then " combo x" <> T.pack (show (gsCombo gs'))
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
                                anim = case out of
                                  MoveApplied _ ->
                                    AnimSwap p1 pos before after 0
                                  Won _ -> AnimSwap p1 pos before after 0
                                  Lost _ -> AnimSwap p1 pos before after 0
                                  LevelClear _ _ -> AnimSwap p1 pos before after 0
                                  _ -> AnimNone
                                comboShow =
                                  if gsCombo gs' > 1 then 90 else 0
                            parts <-
                              if null flash
                                then pure (appParticles app)
                                else do
                                  burst <- spawnBurst before changed
                                  pure (burst ++ appParticles app)
                            let app' =
                                  app
                                    { appGame = gs'
                                    , appSel = Nothing
                                    , appMsg = msg
                                    , appFlash = flash
                                    , appAnim = anim
                                    , appComboShow = comboShow
                                    , appParticles = parts
                                    , appTipFrames = 0
                                    }
                            writeIORef ref app'
                            updateTitle window app'
                            pure False
    | otherwise -> pure False
  _ -> pure False

-- | N / Enter / Space / click-on-clear: next level or new campaign.
advanceOrMsg :: IORef App -> Window -> IO ()
advanceOrMsg ref window = do
  seed <- randomIO
  app <- readIORef ref
  case gsOver (appGame app) of
    Just (LevelClear _ _) -> do
      let gs = nextLevel (appGame app) seed
          app' = (freshLevelUi gs app) { appMsg = "Next level!" }
      writeIORef ref app'
      updateTitle window app'
    Just (Won _) -> do
      let gs = newGameAtLevel 0 (levelConfig (head allLevels)) seed
          app' = (freshLevelUi gs app) { appMsg = "New campaign" }
      writeIORef ref app'
      updateTitle window app'
    Just (Lost _) -> do
      let gs = restartLevel (appGame app) seed
          app' = (freshLevelUi gs app) { appMsg = "Retry!" }
      writeIORef ref app'
      updateTitle window app'
    _ -> do
      let app' = app { appMsg = "Clear the level first (or finish)" }
      writeIORef ref app'
      updateTitle window app'

pixelToCell :: Int32 -> Int32 -> Maybe Pos
pixelToCell mx my =
  let x = fromIntegral mx - padPx
      y = fromIntegral my - padPx - hudH
  in if x < 0 || y < 0 || x >= boardPx || y >= boardPx
       then Nothing
       else
         let c = fromIntegral (x `div` cellPx)
             r = fromIntegral (y `div` cellPx)
         in if inBounds (r, c) then Just (r, c) else Nothing

draw :: Renderer -> App -> IO ()
draw ren app = do
  rendererDrawColor ren $= V4 28 28 38 255
  clear ren
  drawHud ren app
  drawBoard ren app
  drawParticles ren (appParticles app)
  drawTipBanner ren app
  drawHelpStrip ren app
  drawOverlay ren app
  drawPauseHelp ren app

--------------------------------------------------------------------------------
-- Bitmap 3x5 digits (no TTF)
--------------------------------------------------------------------------------

digitGlyph :: Int -> [[Word8]]
digitGlyph d = case d of
  0 -> [[1,1,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]
  1 -> [[0,1,0],[1,1,0],[0,1,0],[0,1,0],[1,1,1]]
  2 -> [[1,1,1],[0,0,1],[1,1,1],[1,0,0],[1,1,1]]
  3 -> [[1,1,1],[0,0,1],[1,1,1],[0,0,1],[1,1,1]]
  4 -> [[1,0,1],[1,0,1],[1,1,1],[0,0,1],[0,0,1]]
  5 -> [[1,1,1],[1,0,0],[1,1,1],[0,0,1],[1,1,1]]
  6 -> [[1,1,1],[1,0,0],[1,1,1],[1,0,1],[1,1,1]]
  7 -> [[1,1,1],[0,0,1],[0,0,1],[0,0,1],[0,0,1]]
  8 -> [[1,1,1],[1,0,1],[1,1,1],[1,0,1],[1,1,1]]
  9 -> [[1,1,1],[1,0,1],[1,1,1],[0,0,1],[1,1,1]]
  _ -> digitGlyph 0

drawDigit :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> Int -> IO ()
drawDigit ren x0 y0 px col d = do
  rendererDrawColor ren $= col
  let g = digitGlyph (abs d `mod` 10)
  forM_ (zip [0 :: CInt ..] g) $ \(ry, row) ->
    forM_ (zip [0 :: CInt ..] row) $ \(cx, bit) ->
      when (bit == 1) $
        fillRect
          ren
          (Just (Rectangle (P (V2 (x0 + cx * px) (y0 + ry * px))) (V2 px px)))

drawNumber :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> Int -> IO ()
drawNumber ren x0 y0 px col n =
  let s = show (max 0 n)
      step = 3 * px + px
  in forM_ (zip [0 :: CInt ..] s) $ \(i, ch) ->
       drawDigit ren (x0 + i * step) y0 px col (fromEnum ch - fromEnum '0')

-- | Tiny letter blocks for CLEAR / WIN / LOSE / NEXT / RETRY banners.
-- Only a few needed glyphs (pure rect approximations).
drawBannerWord :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> String -> IO ()
drawBannerWord ren x0 y0 px col word = do
  rendererDrawColor ren $= col
  let gap = 4 * px + px
  forM_ (zip [0 :: CInt ..] word) $ \(i, ch) ->
    drawGlyph ren (x0 + i * gap) y0 px col ch

drawGlyph :: Renderer -> CInt -> CInt -> CInt -> V4 Word8 -> Char -> IO ()
drawGlyph ren x y px col ch = do
  rendererDrawColor ren $= col
  let spit :: [[Int]] -> IO ()
      spit bits =
        forM_ (zip [0 :: CInt ..] bits) $ \(ry, row) ->
          forM_ (zip [0 :: CInt ..] row) $ \(cx, bit) ->
            when (bit == 1) $
              fillRect
                ren
                (Just (Rectangle (P (V2 (x + cx * px) (y + ry * px))) (V2 px px)))
  spit $ case ch of
    'C' -> [[1,1,1],[1,0,0],[1,0,0],[1,0,0],[1,1,1]]
    'L' -> [[1,0,0],[1,0,0],[1,0,0],[1,0,0],[1,1,1]]
    'E' -> [[1,1,1],[1,0,0],[1,1,0],[1,0,0],[1,1,1]]
    'A' -> [[0,1,0],[1,0,1],[1,1,1],[1,0,1],[1,0,1]]
    'R' -> [[1,1,0],[1,0,1],[1,1,0],[1,0,1],[1,0,1]]
    'W' -> [[1,0,1],[1,0,1],[1,0,1],[1,1,1],[1,0,1]]
    'I' -> [[1,1,1],[0,1,0],[0,1,0],[0,1,0],[1,1,1]]
    'N' -> [[1,0,1],[1,1,1],[1,1,1],[1,0,1],[1,0,1]]
    'O' -> [[1,1,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]
    'S' -> [[1,1,1],[1,0,0],[1,1,1],[0,0,1],[1,1,1]]
    'X' -> [[1,0,1],[1,0,1],[0,1,0],[1,0,1],[1,0,1]]
    'T' -> [[1,1,1],[0,1,0],[0,1,0],[0,1,0],[0,1,0]]
    'Y' -> [[1,0,1],[1,0,1],[0,1,0],[0,1,0],[0,1,0]]
    'P' -> [[1,1,0],[1,0,1],[1,1,0],[1,0,0],[1,0,0]]
    'H' -> [[1,0,1],[1,0,1],[1,1,1],[1,0,1],[1,0,1]]
    'U' -> [[1,0,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]
    'G' -> [[1,1,1],[1,0,0],[1,0,1],[1,0,1],[1,1,1]]
    'M' -> [[1,0,1],[1,1,1],[1,1,1],[1,0,1],[1,0,1]]
    'K' -> [[1,0,1],[1,0,1],[1,1,0],[1,0,1],[1,0,1]]
    'B' -> [[1,1,0],[1,0,1],[1,1,0],[1,0,1],[1,1,0]]
    'D' -> [[1,1,0],[1,0,1],[1,0,1],[1,0,1],[1,1,0]]
    'F' -> [[1,1,1],[1,0,0],[1,1,0],[1,0,0],[1,0,0]]
    'Q' -> [[1,1,1],[1,0,1],[1,0,1],[1,1,1],[0,0,1]]
    '!' -> [[0,1,0],[0,1,0],[0,1,0],[0,0,0],[0,1,0]]
    '1' -> [[0,1,0],[1,1,0],[0,1,0],[0,1,0],[1,1,1]]
    '2' -> [[1,1,1],[0,0,1],[1,1,1],[1,0,0],[1,1,1]]
    ' ' -> [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0]]
    _   -> [[1,1,1],[1,0,1],[1,0,1],[1,0,1],[1,1,1]]


--------------------------------------------------------------------------------
-- Help / tip / pause overlays (bitmap, no TTF)
--------------------------------------------------------------------------------

drawKeyChip :: Renderer -> CInt -> CInt -> Char -> V4 Word8 -> IO ()
drawKeyChip ren x y ch col = do
  rendererDrawColor ren $= V4 30 30 45 255
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 18 18)))
  rendererDrawColor ren $= col
  drawRect ren (Just (Rectangle (P (V2 x y)) (V2 18 18)))
  drawGlyph ren (x + 4) (y + 3) 2 col ch

-- | Brief key strip along bottom of HUD (start / after unpause).
drawHelpStrip :: Renderer -> App -> IO ()
drawHelpStrip ren app
  | appPaused app = pure ()
  | appHelpFrames app <= 0 = pure ()
  | otherwise = do
      let y = hudH - 22
          keys =
            [ ('H', V4 255 220 100 255)
            , ('1', V4 255 160 100 255)
            , ('2', V4 160 220 255 255)
            , ('U', V4 180 200 255 255)
            , ('S', V4 200 160 255 255)
            , ('R', V4 255 160 140 255)
            , ('N', V4 140 220 160 255)
            , ('P', V4 255 200 120 255)
            ]
      rendererDrawColor ren $= V4 20 20 32 220
      fillRect ren (Just (Rectangle (P (V2 0 y)) (V2 winW 22)))
      forM_ (zip [0 :: CInt ..] keys) $ \(i, (ch, col)) ->
        drawKeyChip ren (8 + i * 26) (y + 2) ch col

-- | First-level tip banner over the board top edge.
drawTipBanner :: Renderer -> App -> IO ()
drawTipBanner ren app
  | appPaused app = pure ()
  | appTipFrames app <= 0 = pure ()
  | gsLevel (appGame app) /= 0 = pure ()
  | otherwise = do
      let y = hudH + 6
          pulse = appPulse app
          bright = fromIntegral (200 + (pulse `mod` 30)) :: Word8
      rendererDrawColor ren $= V4 40 35 15 230
      fillRect ren (Just (Rectangle (P (V2 24 y)) (V2 (winW - 48) 28)))
      rendererDrawColor ren $= V4 255 bright 80 255
      drawRect ren (Just (Rectangle (P (V2 24 y)) (V2 (winW - 48) 28)))
      drawBannerWord ren 40 (y + 6) 2 (V4 255 230 120 255) "TIP"
      drawKeyChip ren 120 (y + 5) 'H' (V4 255 220 100 255)
      drawBannerWord ren 150 (y + 8) 2 (V4 220 220 200 255) "HINT"

-- | Full pause overlay with key legend.
drawPauseHelp :: Renderer -> App -> IO ()
drawPauseHelp ren app
  | not (appPaused app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 8 8 16 200
      fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW winH)))
      let panelY = hudH + 40
          panelH = 340 :: CInt
      rendererDrawColor ren $= V4 32 32 48 245
      fillRect ren (Just (Rectangle (P (V2 32 panelY)) (V2 (winW - 64) panelH)))
      rendererDrawColor ren $= V4 255 200 80 255
      drawRect ren (Just (Rectangle (P (V2 32 panelY)) (V2 (winW - 64) panelH)))
      drawBannerWord ren 100 (panelY + 16) 4 (V4 255 220 100 255) "PAUSE"
      let rows :: [(Int, Char, String)]
          rows =
            [ (0, 'H', "HINT")
            , (1, '1', "HAMMER")
            , (2, '2', "SWAP")
            , (3, 'U', "UNDO")
            , (4, 'S', "SHUFFLE")
            , (5, 'R', "RETRY")
            , (6, 'N', "NEXT")
            , (7, 'P', "PLAY")
            ]
      forM_ rows $ \(i, ch, label) -> do
        let yy = panelY + 70 + fromIntegral i * 32
        drawKeyChip ren 80 yy ch (V4 255 220 120 255)
        drawBannerWord ren 120 yy 2 (V4 210 210 230 255) label

drawHud :: Renderer -> App -> IO ()
drawHud ren app = do
  let gs = appGame app
      lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
      white = V4 230 230 245 255 :: V4 Word8
      dim = V4 140 140 170 255
      accent = V4 255 200 80 255
  rendererDrawColor ren $= V4 42 42 58 255
  fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW hudH)))

  -- Level label
  drawNumber ren 10 8 3 white (gsLevel gs + 1)
  forM_ (zip [0 :: Int ..] allLevels) $ \(i, _) -> do
    let col =
          if i == gsLevel gs
            then V4 255 200 80 255
            else if i < gsLevel gs then V4 80 180 120 255 else V4 60 60 80 255
    rendererDrawColor ren $= col
    fillRect ren (Just (Rectangle (P (V2 (70 + fromIntegral i * 12) 10)) (V2 10 14)))

  -- Goal meter (score or collect)
  let prog = goalProgress (gsGoal gs) (gsScore gs) (gsCollected gs)
      targ = goalTarget (gsGoal gs)
      meterCol = case gsGoal gs of
        GoalScore _ -> V4 100 220 140 255
        GoalCollectMulti _ -> V4 220 180 100 255
        GoalClearStone _ -> V4 160 160 170 255
        GoalCollect col _ ->
          let (r, g, b) = colorRGB col in V4 r g b 255
  drawMeter ren 10 36 prog targ meterCol
  drawNumber ren 10 40 2 white prog
  rendererDrawColor ren $= dim
  fillRect ren (Just (Rectangle (P (V2 78 48)) (V2 8 2)))
  drawNumber ren 90 40 2 dim targ

  -- Collect color swatch
  case gsGoal gs of
    GoalCollectMulti _ -> pure ()
    GoalClearStone _ -> pure ()
    GoalCollect col _ -> do
      let (r, g, b) = colorRGB col
      rendererDrawColor ren $= V4 r g b 255
      fillRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
      rendererDrawColor ren $= white
      drawRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
    GoalScore _ -> pure ()

  -- Moves meter
  let moveCap = max (gsMoves gs) (lvlMoves lvl)
  drawMeter ren 10 68 (gsMoves gs) (max 1 moveCap) (V4 100 160 240 255)
  drawNumber ren 10 72 2 white (gsMoves gs)


  -- Booster charges + tool mode
  do
    let hx = winW - 200
    rendererDrawColor ren $= V4 255 140 80 255
    fillRect ren (Just (Rectangle (P (V2 hx 8)) (V2 14 14)))
    drawNumber ren (hx + 18) 8 2 white (gsHammers gs)
    rendererDrawColor ren $= V4 100 180 255 255
    fillRect ren (Just (Rectangle (P (V2 (hx + 50) 8)) (V2 14 14)))
    drawNumber ren (hx + 68) 8 2 white (gsFreeSwaps gs)
    case appTool app of
      ToolHammer -> do
        rendererDrawColor ren $= V4 255 180 80 255
        drawBannerWord ren (hx) 72 2 (V4 255 200 100 255) "HAMMER"
      ToolFreeSwap _ -> do
        drawBannerWord ren (hx) 72 2 (V4 140 200 255 255) "SWAP"
      ToolNone -> pure ()

  -- Combo badge
  when (gsCombo gs > 1 && appComboShow app > 0) $ do
    rendererDrawColor ren $= V4 60 40 20 255
    fillRect ren (Just (Rectangle (P (V2 (winW - 110) 36)) (V2 80 40)))
    rendererDrawColor ren $= accent
    drawRect ren (Just (Rectangle (P (V2 (winW - 110) 36)) (V2 80 40)))
    fillRect ren (Just (Rectangle (P (V2 (winW - 100) 48)) (V2 10 3)))
    fillRect ren (Just (Rectangle (P (V2 (winW - 100) 58)) (V2 10 3)))
    drawNumber ren (winW - 82) 44 3 accent (gsCombo gs)

  -- Status strip
  case gsOver gs of
    Just (Won _) -> rendererDrawColor ren $= V4 60 180 90 255
    Just (LevelClear _ _) -> rendererDrawColor ren $= V4 220 180 60 255
    Just (Lost _) -> rendererDrawColor ren $= V4 200 70 70 255
    _ ->
      rendererDrawColor ren $=
        if gsShuffled gs then V4 180 140 220 255 else V4 70 70 90 255
  fillRect ren (Just (Rectangle (P (V2 (winW - 24) 8)) (V2 16 (hudH - 16))))

drawMeter :: Renderer -> CInt -> CInt -> Int -> Int -> V4 Word8 -> IO ()
drawMeter ren x y value cap col = do
  let maxW = winW - 48
  rendererDrawColor ren $= V4 20 20 30 255
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 maxW 24)))
  rendererDrawColor ren $= col
  let w =
        if cap <= 0
          then 0
          else min maxW (max 0 (fromIntegral value * maxW `div` fromIntegral (max 1 cap)))
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 w 24)))
  rendererDrawColor ren $= V4 200 200 220 255
  drawRect ren (Just (Rectangle (P (V2 x y)) (V2 maxW 24)))

--------------------------------------------------------------------------------
-- Fullscreen outcome overlay
--------------------------------------------------------------------------------

drawOverlay :: Renderer -> App -> IO ()
drawOverlay ren app = case gsOver (appGame app) of
  Nothing -> pure ()
  Just outcome -> do
    -- Dim board
    rendererDrawColor ren $= V4 10 10 18 180
    fillRect ren (Just (Rectangle (P (V2 0 hudH)) (V2 winW (winH - hudH))))
    -- Banner panel
    let panelH = 120 :: CInt
        panelY = hudH + (boardPx - panelH) `div` 2
    case outcome of
      LevelClear _ _ -> do
        rendererDrawColor ren $= V4 40 50 20 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 255 220 80 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawRect ren (Just (Rectangle (P (V2 26 (panelY + 2))) (V2 (winW - 52) (panelH - 4))))
        drawBannerWord ren 80 (panelY + 18) 5 (V4 255 230 100 255) "CLEAR!"
        let stars = starRating (appStartMoves app) (gsMoves (appGame app))
        drawNumber ren 200 (panelY + 22) 3 (V4 255 220 80 255) stars
        drawBannerWord ren 100 (panelY + 70) 3 (V4 200 220 180 255) "NEXT"
        drawNumber ren (winW - 120) (panelY + 68) 3 (V4 200 220 180 255) (gsLevel (appGame app) + 2)
      Won s -> do
        rendererDrawColor ren $= V4 20 50 30 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 80 220 120 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawBannerWord ren 110 (panelY + 18) 5 (V4 120 255 160 255) "WIN!"
        drawNumber ren 160 (panelY + 70) 3 (V4 200 255 210 255) s
      Lost s -> do
        rendererDrawColor ren $= V4 50 20 20 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 220 80 80 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawBannerWord ren 100 (panelY + 18) 5 (V4 255 120 120 255) "LOSE"
        drawBannerWord ren 90 (panelY + 70) 3 (V4 255 180 180 255) "RETRY"
        drawNumber ren (winW - 140) (panelY + 68) 3 (V4 255 180 180 255) s
      _ -> pure ()

--------------------------------------------------------------------------------
-- Particles
--------------------------------------------------------------------------------

drawParticles :: Renderer -> [Particle] -> IO ()
drawParticles ren = mapM_ drawOne
  where
    drawOne p = do
      let fade =
            if pMax p <= 0
              then 255
              else fromIntegral (255 * pLife p `div` pMax p) :: Word8
      rendererDrawColor ren $= V4 (pR p) (pG p) (pB p) fade
      let s = pSize p
          x = round (pX p) - s `div` 2
          y = round (pY p) - s `div` 2
      fillRect ren (Just (Rectangle (P (V2 x y)) (V2 s s)))

--------------------------------------------------------------------------------
-- Board + tweens
--------------------------------------------------------------------------------

cellOrigin :: Pos -> (CInt, CInt)
cellOrigin (r, c) =
  ( padPx + fromIntegral c * cellPx
  , padPx + hudH + fromIntegral r * cellPx
  )

lerpI :: CInt -> CInt -> Int -> Int -> CInt
lerpI a b frame maxF
  | maxF <= 0 = b
  | otherwise =
      let t = fromIntegral frame :: Double
          m = fromIntegral maxF :: Double
          u = t / m
      in a + round (fromIntegral (b - a) * u)

drawGemAt :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()
drawGemAt ren x y cell flashing = case cell of
  Stone layers -> do
    let gap = 3 :: CInt
        (cr, cg, cb) = if flashing then (200, 200, 200) else (90, 90, 100)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Speckle to look rocky
    rendererDrawColor ren $= V4 60 60 70 255
    fillRect ren (Just (Rectangle (P (V2 (x + 12) (y + 14))) (V2 8 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + 28) (y + 30))) (V2 10 7)))
    -- Layer pips (开心消消乐箱子层数感)
    rendererDrawColor ren $= V4 220 200 120 255
    forM_ [0 .. min 3 layers - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Countdown col turns -> do
    let (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
        gap = 3 :: CInt
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Dark fuse / bomb body
    rendererDrawColor ren $= V4 20 20 20 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 10) (y + cellPx `div` 2 - 10))) (V2 20 20)))
    rendererDrawColor ren $= V4 255 180 40 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + cellPx `div` 2 - 5))) (V2 10 10)))
    -- Turn pips (up to 9)
    rendererDrawColor ren $= V4 255 255 255 255
    forM_ [0 .. min 8 turns - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 6 + fromIntegral (i `mod` 3) * 10) (y + 6 + fromIntegral (i `div` 3) * 8)))
              (V2 6 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Gem _ _ ice ov -> do
    let (cr0, cg0, cb0) = colorRGB (cellColor cell)
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
        gap = 3 :: CInt
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
    -- Ice overlay: cyan frames + crack scratches (开心消消乐冰裂纹)
    when (ice > 0) $ do
      rendererDrawColor ren $= V4 140 220 255 220
      drawRect ren (Just (Rectangle (P (V2 (x + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
      rendererDrawColor ren $= V4 200 240 255 200
      -- Diagonal crack lines
      drawLine ren (P (V2 (x + 8) (y + 12))) (P (V2 (x + cellPx - 10) (y + cellPx - 14)))
      drawLine ren (P (V2 (x + cellPx - 12) (y + 10))) (P (V2 (x + 14) (y + cellPx - 12)))
      when (ice > 1) $ do
        drawRect ren (Just (Rectangle (P (V2 (x + 5) (y + 5))) (V2 (cellPx - 10) (cellPx - 10))))
        drawLine ren (P (V2 (x + 10) (y + cellPx `div` 2))) (P (V2 (x + cellPx - 10) (y + cellPx `div` 2 + 4)))
    -- Grass / vine overlays (开心消消乐草·藤蔓)
    case ov of
      Just Grass -> do
        rendererDrawColor ren $= V4 40 140 50 200
        fillRect ren (Just (Rectangle (P (V2 (x + 6) (y + cellPx - 14))) (V2 (cellPx - 12) 8)))
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + cellPx - 20))) (V2 8 8)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx - 18) (y + cellPx - 18))) (V2 8 6)))
      Just Vine -> do
        rendererDrawColor ren $= V4 20 100 40 230
        drawRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
        drawLine ren (P (V2 (x + 8) (y + 8))) (P (V2 (x + cellPx - 10) (y + cellPx - 12)))
        drawLine ren (P (V2 (x + cellPx - 12) (y + 10))) (P (V2 (x + 12) (y + cellPx - 10)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 6))) (V2 8 8)))
      Nothing -> pure ()
    case cellKind cell of
      Normal -> pure ()
      LineH -> do
        rendererDrawColor ren $= V4 255 255 255 230
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx `div` 2 - 4))) (V2 (cellPx - 16) 8)))
        rendererDrawColor ren $= V4 255 200 80 255
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx `div` 2 - 1))) (V2 (cellPx - 16) 2)))
      LineV -> do
        rendererDrawColor ren $= V4 255 255 255 230
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 8))) (V2 8 (cellPx - 16))))
        rendererDrawColor ren $= V4 255 200 80 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 1) (y + 8))) (V2 2 (cellPx - 16))))
      Bomb -> do
        rendererDrawColor ren $= V4 20 20 20 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 10) (y + cellPx `div` 2 - 10))) (V2 20 20)))
        rendererDrawColor ren $= V4 255 220 80 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + cellPx `div` 2 - 5))) (V2 10 10)))
        rendererDrawColor ren $= V4 255 80 40 255
        drawRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 12) (y + cellPx `div` 2 - 12))) (V2 24 24)))
      Rainbow -> do
        let cx = x + cellPx `div` 2
            cy = y + cellPx `div` 2
        rendererDrawColor ren $= V4 255 80 80 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 14) (cy - 6))) (V2 10 12)))
        rendererDrawColor ren $= V4 80 220 100 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 4) (cy - 14))) (V2 10 12)))
        rendererDrawColor ren $= V4 80 140 255 255
        fillRect ren (Just (Rectangle (P (V2 (cx + 4) (cy - 6))) (V2 10 12)))
        rendererDrawColor ren $= V4 255 220 60 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 4) (cy + 2))) (V2 10 12)))
        rendererDrawColor ren $= V4 255 255 255 255
        fillRect ren (Just (Rectangle (P (V2 (cx - 4) (cy - 4))) (V2 8 8)))

drawBoard :: Renderer -> App -> IO ()
drawBoard ren app = case appAnim app of
  AnimSwap { asP1, asP2, asBefore, asFrame } ->
    drawSwap ren app asBefore asP1 asP2 asFrame
  AnimFall { afBoard, afFrame } ->
    drawFall ren app afBoard afFrame
  AnimNone ->
    drawStatic ren app (gsBoard (appGame app)) 0

drawStatic :: Renderer -> App -> Board -> CInt -> IO ()
drawStatic ren app board yOff = do
  let sel = appSel app
      hint = gsHint (appGame app)
      flashSet = map fst (appFlash app)
      pulse = appPulse app
  mapM_
    ( \(r, c) -> do
        let pos = (r, c)
            cell = getCell board pos
            (x0, y0) = cellOrigin pos
            y = y0 + yOff
            flashing = pos `elem` flashSet
        drawGemAt ren x0 y cell flashing
        when (sel == Just pos) $ do
          let bright = fromIntegral (180 + (pulse `mod` 40) * 2) :: Word8
              (sr, sg, sb) = case appTool app of
                ToolHammer -> (255, 160, 80)
                ToolFreeSwap _ -> (100, 180, 255)
                ToolNone -> (255, bright, bright)
          rendererDrawColor ren $= V4 sr sg sb 255
          drawRect ren (Just (Rectangle (P (V2 (x0 + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
          drawRect ren (Just (Rectangle (P (V2 (x0 + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
        case hint of
          Just (h1, h2)
            | pos == h1 || pos == h2 -> do
                rendererDrawColor ren $= V4 255 255 100 255
                drawRect ren (Just (Rectangle (P (V2 x0 y)) (V2 cellPx cellPx)))
          _ -> pure ()
    )
    [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
  -- Conveyor belt path markers (teal chevrons)
  mapM_ (drawBelt ren yOff) (gsBelts (appGame app))

drawBelt :: Renderer -> CInt -> [Pos] -> IO ()
drawBelt _ _ [] = pure ()
drawBelt ren yOff belt = do
  rendererDrawColor ren $= V4 40 200 180 220
  let pairs = zip belt (tail belt ++ [head belt])
  mapM_
    ( \(a, b) -> do
        let (x0, y0) = cellOrigin a
            (x1, y1) = cellOrigin b
            y0' = y0 + yOff
            y1' = y1 + yOff
            cx0 = x0 + cellPx `div` 2
            cy0 = y0' + cellPx `div` 2
            cx1 = x1 + cellPx `div` 2
            cy1 = y1' + cellPx `div` 2
        drawLine ren (P (V2 cx0 cy0)) (P (V2 cx1 cy1))
        -- small chevron near destination
        fillRect ren (Just (Rectangle (P (V2 (cx1 - 3) (cy1 - 3))) (V2 6 6)))
    )
    pairs

drawSwap :: Renderer -> App -> Board -> Pos -> Pos -> Int -> IO ()
drawSwap ren app board p1 p2 frame = do
  let (x1, y1) = cellOrigin p1
      (x2, y2) = cellOrigin p2
      xa = lerpI x1 x2 frame swapFrames
      ya = lerpI y1 y2 frame swapFrames
      xb = lerpI x2 x1 frame swapFrames
      yb = lerpI y2 y1 frame swapFrames
      c1 = getCell board p1
      c2 = getCell board p2
      flashSet = map fst (appFlash app)
  mapM_
    ( \(r, c) -> do
        let pos = (r, c)
        unless (pos == p1 || pos == p2) $ do
          let (x0, y0) = cellOrigin pos
          drawGemAt ren x0 y0 (getCell board pos) (pos `elem` flashSet)
    )
    [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
  drawGemAt ren xa ya c1 (p1 `elem` flashSet)
  drawGemAt ren xb yb c2 (p2 `elem` flashSet)

drawFall :: Renderer -> App -> Board -> Int -> IO ()
drawFall ren app board frame = do
  let t = fromIntegral frame / fromIntegral fallFrames :: Double
      ease = 1 - (1 - t) * (1 - t)
      offset = round (fromIntegral cellPx * (1 - ease) * (-0.35)) :: CInt
  drawStatic ren app board offset
