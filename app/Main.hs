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
import System.Random (randomIO)

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

data App = App
  { appGame   :: GameState
  , appSel    :: Maybe Pos
  , appMsg    :: Text
  , appFlash  :: [(Pos, Int)]
  , appPulse  :: Int
  , appAnim   :: Anim
  , appComboShow :: Int  -- frames left to highlight combo
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
        , appMsg = "Click swap | H hint | U undo | S shuffle | N next | R restart | Esc"
        , appFlash = []
        , appPulse = 0
        , appAnim = AnimNone
        , appComboShow = 0
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
  in app
       { appFlash = flash'
       , appPulse = appPulse app + 1
       , appAnim = anim'
       , appComboShow = combo'
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
      comboBits =
        if gsCombo gs > 1
          then "  combo x" ++ show (gsCombo gs)
          else ""
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
            ++ comboBits
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

-- | Ignore input while a tween is playing (rules already committed).
animBusy :: App -> Bool
animBusy app = case appAnim app of
  AnimNone -> False
  _ -> True

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
                app' =
                  app
                    { appGame = gs
                    , appSel = Nothing
                    , appMsg = "Restarted level"
                    , appFlash = []
                    , appAnim = AnimNone
                    , appComboShow = 0
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
                      , appAnim = AnimFall { afBoard = gsBoard gs, afFrame = 0 }
                      }
              writeIORef ref app'
              updateTitle window app'
            pure False
          KeycodeN -> do
            seed <- randomIO
            app <- readIORef ref
            case gsOver (appGame app) of
              Just (LevelClear _ _) -> do
                let gs = nextLevel (appGame app) seed
                    app' =
                      app
                        { appGame = gs
                        , appSel = Nothing
                        , appMsg = "Next level!"
                        , appFlash = []
                        , appAnim = AnimNone
                        , appComboShow = 0
                        }
                writeIORef ref app'
                updateTitle window app'
              Just (Won _) -> do
                let gs = newGameAtLevel 0 (levelConfig (head allLevels)) seed
                    app' =
                      app
                        { appGame = gs
                        , appSel = Nothing
                        , appMsg = "New campaign"
                        , appFlash = []
                        , appAnim = AnimNone
                        , appComboShow = 0
                        }
                writeIORef ref app'
                updateTitle window app'
              _ -> do
                let app' = app { appMsg = "Clear the level first (or finish)" }
                writeIORef ref app'
                updateTitle window app'
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
                          }
                  writeIORef ref app'
                  updateTitle window app'
            pure False
          KeycodeH -> do
            app <- readIORef ref
            let (gs, h) = applyHint (appGame app)
                msg = case h of
                  Just (p1, p2) -> "Hint: " <> T.pack (show p1) <> " <-> " <> T.pack (show p2)
                  Nothing -> "No moves — press S to shuffle"
                app' = app { appGame = gs, appMsg = msg }
            writeIORef ref app'
            updateTitle window app'
            pure False
          _ -> pure False
    | otherwise -> pure False
  MouseButtonEvent me
    | mouseButtonEventMotion me == Pressed
        && mouseButtonEventButton me == ButtonLeft -> do
        app0 <- readIORef ref
        if animBusy app0
          then pure False
          else do
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
                              after = gsBoard gs'
                              flash =
                                case out of
                                  NoMatch -> []
                                  InvalidSwap -> []
                                  _ ->
                                    [ (p, 18)
                                    | r <- [0 .. boardSize - 1]
                                    , c <- [0 .. boardSize - 1]
                                    , let p = (r, c)
                                    , getCell before p /= getCell after p
                                    ]
                              shuffledMsg =
                                if gsShuffled gs' then " (auto-shuffled)" else ""
                              comboMsg =
                                if gsCombo gs' > 1
                                  then " combo x" <> T.pack (show (gsCombo gs'))
                                  else ""
                              msg = case out of
                                InvalidSwap -> "Need 4-neighbor adjacent"
                                NoMatch -> "No match; rolled back"
                                MoveApplied s ->
                                  "Cleared +"
                                    <> T.pack (show s)
                                    <> comboMsg
                                    <> T.pack shuffledMsg
                                Won s -> "YOU WIN score=" <> T.pack (show s) <> " — N/R"
                                LevelClear s n ->
                                  "Level clear +"
                                    <> T.pack (show s)
                                    <> comboMsg
                                    <> " -> L"
                                    <> T.pack (show (n + 1))
                                    <> " (press N)"
                                Lost s -> "Out of moves score=" <> T.pack (show s) <> " — R"
                              anim = case out of
                                MoveApplied _ ->
                                  AnimSwap p1 pos before after 0
                                Won _ -> AnimSwap p1 pos before after 0
                                Lost _ -> AnimSwap p1 pos before after 0
                                LevelClear _ _ -> AnimSwap p1 pos before after 0
                                _ -> AnimNone
                              comboShow =
                                if gsCombo gs' > 1 then 90 else 0
                              app' =
                                app
                                  { appGame = gs'
                                  , appSel = Nothing
                                  , appMsg = msg
                                  , appFlash = flash
                                  , appAnim = anim
                                  , appComboShow = comboShow
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

drawHud :: Renderer -> App -> IO ()
drawHud ren app = do
  let gs = appGame app
      lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
      white = V4 230 230 245 255 :: V4 Word8
      dim = V4 140 140 170 255
      accent = V4 255 200 80 255
  rendererDrawColor ren $= V4 42 42 58 255
  fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW hudH)))

  -- Level label (L + number) via color bar segments
  drawNumber ren 10 8 3 white (gsLevel gs + 1)
  -- Level color ticks
  forM_ (zip [0 :: Int ..] allLevels) $ \(i, _) -> do
    let col =
          if i == gsLevel gs
            then V4 255 200 80 255
            else if i < gsLevel gs then V4 80 180 120 255 else V4 60 60 80 255
    rendererDrawColor ren $= col
    fillRect ren (Just (Rectangle (P (V2 (70 + fromIntegral i * 14) 10)) (V2 12 14)))

  -- Score meter + digits
  drawMeter ren 10 36 (gsScore gs) (gsTarget gs) (V4 100 220 140 255)
  drawNumber ren 10 40 2 white (gsScore gs)
  rendererDrawColor ren $= dim
  -- slash as small rects
  fillRect ren (Just (Rectangle (P (V2 78 48)) (V2 8 2)))
  drawNumber ren 90 40 2 dim (gsTarget gs)

  -- Moves meter + digits
  let moveCap = max (gsMoves gs) (lvlMoves lvl)
  drawMeter ren 10 68 (gsMoves gs) (max 1 moveCap) (V4 100 160 240 255)
  drawNumber ren 10 72 2 white (gsMoves gs)

  -- Combo badge
  when (gsCombo gs > 1 && appComboShow app > 0) $ do
    rendererDrawColor ren $= V4 60 40 20 255
    fillRect ren (Just (Rectangle (P (V2 (winW - 110) 36)) (V2 80 40)))
    rendererDrawColor ren $= accent
    drawRect ren (Just (Rectangle (P (V2 (winW - 110) 36)) (V2 80 40)))
    -- draw "x" as two lines approximated by rects, then number
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
drawGemAt ren x y cell flashing = do
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
          rendererDrawColor ren $= V4 255 bright bright 255
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
  -- Draw all except the two swapping cells
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
  -- Ease-out drop offset: gems settle from above
  let t = fromIntegral frame / fromIntegral fallFrames :: Double
      ease = 1 - (1 - t) * (1 - t)
      offset = round (fromIntegral cellPx * (1 - ease) * (-0.35)) :: CInt
  drawStatic ren app board offset
