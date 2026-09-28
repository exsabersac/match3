{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.IORef
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Match3.Core
import SDL hiding (Normal)
import System.Random (randomIO)

cellPx, padPx, hudH, boardPx, winW, winH :: CInt
cellPx = 56
padPx = 16
hudH = 88
boardPx = cellPx * fromIntegral boardSize
winW = padPx * 2 + boardPx
winH = padPx * 2 + boardPx + hudH

data App = App
  { appGame   :: GameState
  , appSel    :: Maybe Pos
  , appMsg    :: Text
  , appFlash  :: [(Pos, Int)]  -- positions still flashing (frames left)
  , appPulse  :: Int           -- selection pulse phase
  }

colorRGB :: Color -> (Word8, Word8, Word8)
colorRGB C1 = (220, 70, 70)
colorRGB C2 = (70, 180, 90)
colorRGB C3 = (70, 120, 220)
colorRGB C4 = (240, 200, 60)
colorRGB C5 = (180, 80, 200)

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
  ref <-
    newIORef
      App
        { appGame = gs0
        , appSel = Nothing
        , appMsg = "Click swap | H hint | U undo | N next | R restart | Esc quit"
        , appFlash = []
        , appPulse = 0
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
  app
    { appFlash = [ (p, n - 1) | (p, n) <- appFlash app, n > 1 ]
    , appPulse = appPulse app + 1
    }

updateTitle :: Window -> App -> IO ()
updateTitle window app = do
  let gs = appGame app
      lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
      status = case gsOver gs of
        Just (Won s) -> " CLEAR! score=" <> show s
        Just (LevelClear s n) -> " LEVEL UP ->" <> show (n + 1) <> " score=" <> show s
        Just (Lost s) -> " LOSE score=" <> show s
        _ -> ""
      title =
        T.pack $
          "L"
            ++ show (gsLevel gs + 1)
            ++ " "
            ++ lvlName lvl
            ++ "  score="
            ++ show (gsScore gs)
            ++ "/"
            ++ show (gsTarget gs)
            ++ "  moves="
            ++ show (gsMoves gs)
            ++ status
            ++ "  |  "
            ++ T.unpack (appMsg app)
  windowTitle window $= title

foldEvents :: IORef App -> Window -> [Event] -> IO Bool
foldEvents ref window = go False
  where
    go q [] = pure q
    go q (e : es) = do
      q' <- handleEvent ref window e
      go (q || q') es

handleEvent :: IORef App -> Window -> Event -> IO Bool
handleEvent ref window ev = case eventPayload ev of
  QuitEvent -> pure True
  KeyboardEvent ke
    | keyboardEventKeyMotion ke == Pressed ->
        case keysymKeycode (keyboardEventKeysym ke) of
          KeycodeEscape -> pure True
          KeycodeQ -> pure True
          KeycodeR -> do
            seed <- randomIO
            app <- readIORef ref
            let gs = restartLevel (appGame app) seed
                app' = app { appGame = gs, appSel = Nothing, appMsg = "Restarted level", appFlash = [] }
            writeIORef ref app'
            updateTitle window app'
            pure False
          KeycodeN -> do
            seed <- randomIO
            app <- readIORef ref
            case gsOver (appGame app) of
              Just (LevelClear _ _) -> do
                let gs = nextLevel (appGame app) seed
                    app' = app { appGame = gs, appSel = Nothing, appMsg = "Next level!", appFlash = [] }
                writeIORef ref app'
                updateTitle window app'
              Just (Won _) -> do
                let gs = newGameAtLevel 0 (levelConfig (head allLevels)) seed
                    app' = app { appGame = gs, appSel = Nothing, appMsg = "New campaign", appFlash = [] }
                writeIORef ref app'
                updateTitle window app'
              _ -> do
                let app' = app { appMsg = "Clear the level first (or finish)" }
                writeIORef ref app'
                updateTitle window app'
            pure False
          KeycodeU -> do
            app <- readIORef ref
            case undoMove (appGame app) of
              Nothing -> do
                let app' = app { appMsg = "Nothing to undo" }
                writeIORef ref app'
                updateTitle window app'
              Just gs -> do
                let app' = app { appGame = gs, appSel = Nothing, appMsg = "Undone", appFlash = [] }
                writeIORef ref app'
                updateTitle window app'
            pure False
          KeycodeH -> do
            app <- readIORef ref
            let (gs, h) = applyHint (appGame app)
                msg = case h of
                  Just (p1, p2) -> "Hint: " <> T.pack (show p1) <> " <-> " <> T.pack (show p2)
                  Nothing -> "No moves (shuffle via R)"
                app' = app { appGame = gs, appMsg = msg }
            writeIORef ref app'
            updateTitle window app'
            pure False
          _ -> pure False
    | otherwise -> pure False
  MouseButtonEvent me
    | mouseButtonEventMotion me == Pressed
        && mouseButtonEventButton me == ButtonLeft -> do
        let P (V2 mx my) = mouseButtonEventPos me
        case pixelToCell mx my of
          Nothing -> pure False
          Just pos -> do
            app <- readIORef ref
            case gsOver (appGame app) of
              Just (LevelClear _ _) -> do
                let app' = app { appMsg = "Press N for next level" }
                writeIORef ref app'
                updateTitle window app'
                pure False
              Just _ -> pure False
              Nothing -> case appSel app of
                Nothing -> do
                  let app' = app { appSel = Just pos, appMsg = "Selected; click adjacent" }
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
                          flash =
                            case out of
                              NoMatch -> []
                              InvalidSwap -> []
                              _ ->
                                [ (p, 18)
                                | r <- [0 .. boardSize - 1]
                                , c <- [0 .. boardSize - 1]
                                , let p = (r, c)
                                , getCell before p /= getCell (gsBoard gs') p
                                ]
                          msg = case out of
                            InvalidSwap -> "Need 4-neighbor adjacent"
                            NoMatch -> "No match; rolled back"
                            MoveApplied s -> "Cleared +" <> T.pack (show s)
                            Won s -> "YOU WIN score=" <> T.pack (show s) <> " — N/R"
                            LevelClear s n ->
                              "Level clear +"
                                <> T.pack (show s)
                                <> " -> L"
                                <> T.pack (show (n + 1))
                                <> " (press N)"
                            Lost s -> "Out of moves score=" <> T.pack (show s) <> " — R"
                          app' =
                            app
                              { appGame = gs'
                              , appSel = Nothing
                              , appMsg = msg
                              , appFlash = flash
                              }
                      writeIORef ref app'
                      updateTitle window app'
                      pure False
    | otherwise -> pure False
  _ -> pure False

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

drawHud :: Renderer -> App -> IO ()
drawHud ren app = do
  let gs = appGame app
  rendererDrawColor ren $= V4 42 42 58 255
  fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW hudH)))
  drawMeter ren 12 10 (gsScore gs) (gsTarget gs) (V4 100 220 140 255)
  drawMeter ren 12 48 (gsMoves gs) (max 1 (gsMoves gs + gsScore gs `div` 50 + 5)) (V4 100 160 240 255)
  case gsOver gs of
    Just (Won _) -> rendererDrawColor ren $= V4 60 180 90 255
    Just (LevelClear _ _) -> rendererDrawColor ren $= V4 220 180 60 255
    Just (Lost _) -> rendererDrawColor ren $= V4 200 70 70 255
    _ -> rendererDrawColor ren $= V4 70 70 90 255
  fillRect ren (Just (Rectangle (P (V2 (winW - 24) 8)) (V2 16 (hudH - 16))))

drawMeter :: Renderer -> CInt -> CInt -> Int -> Int -> V4 Word8 -> IO ()
drawMeter ren x y value cap col = do
  let maxW = winW - 48
  rendererDrawColor ren $= V4 20 20 30 255
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 maxW 28)))
  rendererDrawColor ren $= col
  let w =
        if cap <= 0
          then 0
          else min maxW (max 0 (fromIntegral value * maxW `div` fromIntegral (max 1 cap)))
  fillRect ren (Just (Rectangle (P (V2 x y)) (V2 w 28)))
  rendererDrawColor ren $= V4 200 200 220 255
  drawRect ren (Just (Rectangle (P (V2 x y)) (V2 maxW 28)))

drawBoard :: Renderer -> App -> IO ()
drawBoard ren app = do
  let board = gsBoard (appGame app)
      sel = appSel app
      hint = gsHint (appGame app)
      flashSet = map fst (appFlash app)
      pulse = appPulse app
  mapM_
    ( \(r, c) -> do
        let pos = (r, c)
            cell = getCell board pos
            (cr0, cg0, cb0) = colorRGB (cellColor cell)
            flashing = pos `elem` flashSet
            (cr, cg, cb) =
              if flashing
                then (255, 255, 255)
                else (cr0, cg0, cb0)
            x = padPx + fromIntegral c * cellPx
            y = padPx + hudH + fromIntegral r * cellPx
            gap = 3 :: CInt
        rendererDrawColor ren $= V4 cr cg cb 255
        fillRect
          ren
          (Just
             (Rectangle
                (P (V2 (x + gap) (y + gap)))
                (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
        -- Special markers
        case cellKind cell of
          Normal -> pure ()
          LineH -> do
            rendererDrawColor ren $= V4 255 255 255 220
            fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + cellPx `div` 2 - 3))) (V2 (cellPx - 20) 6)))
          LineV -> do
            rendererDrawColor ren $= V4 255 255 255 220
            fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + 10))) (V2 6 (cellPx - 20))))
          Bomb -> do
            rendererDrawColor ren $= V4 20 20 20 255
            fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 8) (y + cellPx `div` 2 - 8))) (V2 16 16)))
            rendererDrawColor ren $= V4 255 220 80 255
            fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + cellPx `div` 2 - 4))) (V2 8 8)))
        -- Selection pulse
        when (sel == Just pos) $ do
          let bright = fromIntegral (180 + (pulse `mod` 40) * 2) :: Word8
          rendererDrawColor ren $= V4 255 bright bright 255
          drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
          drawRect ren (Just (Rectangle (P (V2 (x + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
        -- Hint outline
        case hint of
          Just (h1, h2)
            | pos == h1 || pos == h2 -> do
                rendererDrawColor ren $= V4 255 255 100 255
                drawRect ren (Just (Rectangle (P (V2 x y)) (V2 cellPx cellPx)))
          _ -> pure ()
    )
    [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
