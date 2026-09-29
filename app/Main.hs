{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SDL2 前端：窗口、输入、道具点选模式、选关地图、交换/下落动画与粒子。
-- 规则一律经 Match3.Core；本模块不改写 trySwap 结果，只展示。
module Main (main) where

import Control.Concurrent (threadDelay)
import Art
import Control.Monad (forM_, unless, void, when)
import Data.Char (ord, toUpper)
import Data.IORef
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Match3.Core
import SDL hiding (Normal)
import qualified SDL.Internal.Types as SI
import qualified SDL.Raw as Raw
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Random (randomIO, randomRIO)
import Text.Read (readMaybe)

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
  | ToolCross           -- next cell click cross-clears row+col
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
  , appMapOpen   :: Bool  -- level map overlay (选关)
  , appMaxReached :: Int  -- highest unlocked campaign index
  , appArt       :: Maybe Art  -- 贴图集；Nothing 时回退到矩形绘制
  , appScale     :: Float  -- 渲染倍率：物理像素 / 逻辑像素（Retina = 2）；0 表示尚未同步
  , appMouseScale :: Float -- 鼠标倍率：窗口坐标 / 逻辑像素（macOS Retina = 1；MATCH3_SCALE=N 时 = N）
  }

colorRGB :: Color -> (Word8, Word8, Word8)
colorRGB C1 = (236, 62, 78)   -- 红·圆（与 tools/gen_assets.py 调色板一致）
colorRGB C2 = (52, 196, 96)   -- 绿·方
colorRGB C3 = (56, 128, 246)  -- 蓝·菱
colorRGB C4 = (255, 194, 36)  -- 黄·星
colorRGB C5 = (172, 88, 236)  -- 紫·三角

helpKeysMsg :: Text
helpKeysMsg = "H hint | 1 hammer | 2 free-swap | 3 cross | U undo | S shuffle | D daily | M map | R restart | N next | P pause | Esc"

main :: IO ()
main = do
  initializeAll
  -- 线性过滤：2x 贴图缩到 56px 格子时更平滑（须在创建纹理之前设置）
  HintRenderScaleQuality $= ScaleLinear
  seed <- envSeed
  startIdx <- envStartLevel
  showcase <- isJust <$> lookupEnv "MATCH3_SHOWCASE"
  winScale <- envWindowScale
  let lvl = allLevels !! startIdx
      gs0 = newGameAtLevel startIdx (levelConfig lvl) seed
  -- 高分屏：windowHighDPI 让 macOS Retina 给出 2x 物理像素的绘制表面（窗口坐标仍是逻辑点）。
  -- MATCH3_SCALE=N（测试用）把窗口本身放大 N 倍，在 Xvfb 等没有 HiDPI 的环境里模拟 Retina。
  window <-
    createWindow
      "Match-3"
      defaultWindow
        { windowInitialSize = V2 (winW * winScale) (winH * winScale)
        , windowHighDPI = True
        }
  renderer <- createRenderer window (-1) defaultRenderer
  -- 开启 alpha 混合：面板、遮罩、粒子的半透明才生效
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  art <- loadArt renderer
  let (gsHinted0, _) = applyHint gs0
      gsHinted = if showcase then showcaseState gsHinted0 else gsHinted0
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
        , appTipFrames = if startIdx == 0 && not showcase then 240 else 0
        , appHelpFrames = if showcase then 0 else 300
        , appPaused = False
        , appStartMoves = lvlMoves lvl
        , appDragFrom = Nothing
        , appTool = ToolNone
        , appMapOpen = False
        , appMaxReached = startIdx
        , appArt = art
        , appScale = 0
        , appMouseScale = 1
        }
  updateTitle window =<< readIORef ref
  let loop = do
        -- 每帧同步倍率（两次查询很便宜）：窗口拖到不同 DPI 的显示器上也能立刻跟上
        syncScale window renderer ref
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
  let paused = appPaused app
      flash' =
        if paused then appFlash app else [ (p, n - 1) | (p, n) <- appFlash app, n > 1 ]
      combo' =
        if paused then appComboShow app else max 0 (appComboShow app - 1)
      tip' = if paused then appTipFrames app else max 0 (appTipFrames app - 1)
      help' = if paused then appHelpFrames app else max 0 (appHelpFrames app - 1)
      anim' =
        if paused
          then appAnim app
          else case appAnim app of
            AnimNone -> AnimNone
            AnimSwap { asP1, asP2, asBefore, asAfter, asFrame }
              | asFrame + 1 >= swapFrames ->
                  AnimFall { afBoard = asAfter, afFrame = 0 }
              | otherwise ->
                  AnimSwap asP1 asP2 asBefore asAfter (asFrame + 1)
            AnimFall { afBoard, afFrame }
              | afFrame + 1 >= fallFrames -> AnimNone
              | otherwise -> AnimFall afBoard (afFrame + 1)
      parts' =
        if paused then appParticles app else tickParticles (appParticles app)
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
                    Chest _ -> (220, 170, 60)
                    Honey _ -> (240, 180, 40)
                    Balloon col -> colorRGB col
                    Cookie -> (210, 160, 90)
                    Cake _ -> (255, 140, 180)
                    MagicHat -> (140, 90, 200)
                    Maker col _ -> colorRGB col
                    Snail _ _ -> (90, 160, 70)
                    Safe _ -> (180, 150, 40)
                    Flip f _ -> colorRGB f
                    Surprise -> (255, 100, 160)
                    Bottle col -> colorRGB col
                    TimeSpirit -> (80, 220, 255)
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
      ms <- appMouseScale <$> readIORef ref
      -- 先把鼠标坐标换算成逻辑坐标，后面的点选 / 拖拽 / 地图 / 按钮判定全部沿用逻辑坐标
      q' <- handleEvent ref window (mouseToLogical ms e)
      go (q || q') es

--------------------------------------------------------------------------------
-- 高分屏（Retina）倍率
--------------------------------------------------------------------------------

-- | 渲染器输出尺寸（物理像素）。Retina + windowHighDPI 下是窗口点数的 2 倍。
rendererOutputPixels :: Renderer -> IO (V2 CInt)
rendererOutputPixels (SI.Renderer raw) =
  alloca $ \pw -> alloca $ \ph -> do
    rc <- Raw.getRendererOutputSize raw pw ph
    if rc /= 0 then pure (V2 winW winH) else V2 <$> peek pw <*> peek ph

-- | 由「尺寸 / 逻辑尺寸」求倍率；宽高取较小者，保证整个逻辑画面都放得下。
ratioOf :: V2 CInt -> Float
ratioOf (V2 w h) =
  max 0.25 (min (fromIntegral w / fromIntegral winW) (fromIntegral h / fromIntegral winH))

-- | 查询当前倍率，变化时更新 SDL 渲染缩放和贴图变体选择用的 artScale。
-- 游戏逻辑与所有绘制坐标始终是 480x588 逻辑单位；SDL_RenderSetScale 负责乘上物理倍率。
syncScale :: Window -> Renderer -> IORef App -> IO ()
syncScale window ren ref = do
  out <- rendererOutputPixels ren
  win <- get (windowSize window)
  let ps = ratioOf out
      ms = ratioOf win
  app <- readIORef ref
  when (ps /= appScale app || ms /= appMouseScale app) $ do
    rendererScale ren $= V2 (realToFrac ps) (realToFrac ps)
    hPutStrLn stderr $
      "match3-sdl: render scale " ++ show ps ++ " (output " ++ showV2 out
        ++ ", window " ++ showV2 win ++ ", logical " ++ showV2 (V2 winW winH) ++ ")"
    writeIORef ref app
      { appScale = ps
      , appMouseScale = ms
      , appArt = fmap (\a -> a {artScale = ps}) (appArt app)
      }
  where
    showV2 (V2 a b) = show a ++ "x" ++ show b

-- | 鼠标事件：窗口坐标 → 逻辑坐标。macOS Retina 上 SDL 给的已是逻辑点（倍率 1，不变）；
-- MATCH3_SCALE=N 放大窗口时窗口坐标 = 物理像素，需要除以 N。
mouseToLogical :: Float -> Event -> Event
mouseToLogical s ev
  | s == 1 = ev
  | otherwise = case eventPayload ev of
      MouseButtonEvent me ->
        ev {eventPayload = MouseButtonEvent me {mouseButtonEventPos = conv (mouseButtonEventPos me)}}
      MouseMotionEvent mm ->
        ev {eventPayload = MouseMotionEvent mm {mouseMotionEventPos = conv (mouseMotionEventPos mm)}}
      _ -> ev
  where
    conv (P (V2 x y)) = P (V2 (d x) (d y))
    d :: Int32 -> Int32
    d v = floor (fromIntegral v / s :: Float)

-- | MATCH3_SCALE=N（1..4，测试用）：窗口按 N 倍逻辑尺寸创建。默认 1。
envWindowScale :: IO CInt
envWindowScale = do
  v <- lookupEnv "MATCH3_SCALE"
  pure $ case v >>= readMaybe of
    Just n | n >= 1 && n <= (4 :: Int) -> fromIntegral n
    _ -> 1

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
       , appMapOpen = False
       , appMaxReached = max (appMaxReached app) (gsLevel gs')
       }


-- | Apply hammer booster at pos with flash / particles / msg.
applyHammer :: IORef App -> Window -> App -> Pos -> IO App
applyHammer ref window app pos = do
  let before = gsBoard (appGame app)
      (gs', out) = useHammer pos (appGame app)
      after = gsBoard gs'
      -- 特效只看本次调用的 MoveFx（边沿触发），道具无效时不重播上一步连击
      fx = moveFx (appGame app) gs' out
      changed = fxCleared fx
      flash = [(p, 18) | p <- changed]
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
        withUnlock
          app
            { appGame = gs'
            , appSel = Nothing
            , appTool = ToolNone
            , appDragFrom = Nothing
            , appMsg = msg
            , appFlash = flash
            , appAnim = if null flash then AnimNone else AnimFall { afBoard = after, afFrame = 0 }
            , appComboShow = comboFxFrames fx
            , appParticles = parts
            }
          out
  writeIORef ref app'
  updateTitle window app'
  pure app'


-- | Apply cross-clear booster at pos with flash / particles / msg.
applyCrossClear :: IORef App -> Window -> App -> Pos -> IO App
applyCrossClear ref window app pos = do
  let before = gsBoard (appGame app)
      (gs', out) = useCrossClear pos (appGame app)
      after = gsBoard gs'
      -- 特效只看本次调用的 MoveFx（边沿触发），道具无效时不重播上一步连击
      fx = moveFx (appGame app) gs' out
      changed = fxCleared fx
      flash = [(p, 18) | p <- changed]
      msg = case out of
        InvalidSwap -> "No cross-clears left"
        NoMatch -> "Cross failed"
        MoveApplied g -> "Cross +" <> T.pack (show g)
        LevelClear _ _ -> "Cross cleared level!"
        Won _ -> "Cross won!"
        Lost _ -> T.pack (loseHint (gsGoal gs'))
  parts <-
    if null flash
      then pure (appParticles app)
      else do
        burst <- spawnBurst before changed
        pure (burst ++ appParticles app)
  let app' =
        withUnlock
          app
            { appGame = gs'
            , appSel = Nothing
            , appTool = ToolNone
            , appDragFrom = Nothing
            , appMsg = msg
            , appFlash = flash
            , appAnim = if null flash then AnimNone else AnimFall { afBoard = after, afFrame = 0 }
            , appComboShow = comboFxFrames fx
            , appParticles = parts
            }
          out
  writeIORef ref app'
  updateTitle window app'
  pure app'

applyFreeSwap :: IORef App -> Window -> App -> Pos -> Pos -> IO ()
applyFreeSwap ref window app p1 p2 = do
  let before = gsBoard (appGame app)
      (gs', out) = useFreeSwap p1 p2 (appGame app)
      after = gsBoard gs'
      -- 特效只看本次调用的 MoveFx（边沿触发），道具无效时不重播上一步连击
      fx = moveFx (appGame app) gs' out
      changed = fxCleared fx
      flash = [(p, 18) | p <- changed]
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
        withUnlock
          app
            { appGame = gs'
            , appSel = Nothing
            , appTool = tool'
            , appDragFrom = Nothing
            , appMsg = msg
            , appFlash = flash
            , appAnim = if null flash then AnimNone else AnimFall { afBoard = after, afFrame = 0 }
            , appComboShow = comboFxFrames fx
            , appParticles = parts
            }
          out
  writeIORef ref app'
  updateTitle window app'

-- | Ignore input while a tween is playing (rules already committed).
animBusy :: App -> Bool
animBusy app = case appAnim app of
  AnimNone -> False
  _ -> True

-- | 连击（爆击）特效持续帧数。只由本次操作的 MoveFx 决定（边沿触发）：
-- 旧实现直接读 gsCombo，无匹配回滚后 gsCombo 仍是上一步的值，于是又置 120 帧重播。
-- 清除格 fxCleared 已排除传送带 / 蜗牛挪位噪声（见 gsLastCleared）。
comboFxFrames :: MoveFx -> Int
comboFxFrames fx = if fxCombo fx > 1 then 120 else 0

-- | Bump map unlock after LevelClear / Won (daily Won must not unlock campaign).
withUnlock :: App -> Outcome -> App
withUnlock app out =
  app { appMaxReached = unlockAfterOutcome (appGame app) (appMaxReached app) out }

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
                  let before = gsBoard (appGame app)
                      (gs', out) = trySwap p1 p2 (appGame app)
                      after = gsBoard gs'
                      -- 无匹配回滚：fx 为空，不闪光、不播连击（修复重播上一步爆击特效）
                      fx = moveFx (appGame app) gs' out
                      changed = fxCleared fx
                      flash = [(p, 18) | p <- changed]
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
                        withUnlock
                          app
                            { appGame = gs'
                            , appSel = Nothing
                            , appDragFrom = Nothing
                            , appMsg = msg
                            , appFlash = flash
                            , appAnim = anim
                            , appComboShow = comboFxFrames fx
                            , appParticles = parts
                            , appTipFrames = 0
                            }
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
          else if appPaused app0 || animBusy app0
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
                            let before = gsBoard (appGame app)
                                (gs', out) = trySwap p1 pos (appGame app)
                                after = gsBoard gs'
                                -- 无匹配回滚 / 非相邻：fx 为空，不闪光、不播连击（修复重播上一步爆击特效）
                                fx = moveFx (appGame app) gs' out
                                changed = fxCleared fx
                                flash = [(p, 18) | p <- changed]
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
                                anim = case out of
                                  MoveApplied _ ->
                                    AnimSwap p1 pos before after 0
                                  Won _ -> AnimSwap p1 pos before after 0
                                  Lost _ -> AnimSwap p1 pos before after 0
                                  LevelClear _ _ -> AnimSwap p1 pos before after 0
                                  _ -> AnimNone
                                -- Combo SFX placeholder: when audio lands, play a rising
                                -- pitched blip for fxCombo fx >= 2 (cascade wave cheer).
                                comboShow = comboFxFrames fx
                            parts <-
                              if null flash
                                then pure (appParticles app)
                                else do
                                  burst <- spawnBurst before changed
                                  pure (burst ++ appParticles app)
                            let app' =
                                  withUnlock
                                    app
                                      { appGame = gs'
                                      , appSel = Nothing
                                      , appDragFrom = Nothing
                                      , appMsg = msg
                                      , appFlash = flash
                                      , appAnim = anim
                                      , appComboShow = comboShow
                                      , appParticles = parts
                                      , appTipFrames = 0
                                      }
                                    out
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
  case appArt app of
    -- 贴图模式：背景图 + 圆角面板 HUD + 精灵棋子
    Just art -> do
      forM_ (artBg art) $ \bg -> copy ren bg Nothing Nothing
      drawHudArt ren art app
      drawBoard ren app
      drawParticlesAny ren app
      drawComboPopArt ren art app
      drawTipBannerArt ren art app
      drawToolBannerArt ren art app
      drawHelpStripArt ren art app
      drawOverlayArt ren art app
      drawPauseHelpArt ren art app
      drawLevelMapArt ren art app
    -- 回退：无资源时沿用原有矩形 / 位图字绘制
    Nothing -> do
      drawHud ren app
      drawBoard ren app
      drawParticlesAny ren app
      drawComboPop ren app
      drawTipBanner ren app
      drawHelpStrip ren app
      drawOverlay ren app
      drawPauseHelp ren app
      drawLevelMap ren app

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
    ' ' -> [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0]]
    d | d >= '0' && d <= '9' ->
      map (map fromIntegral) (digitGlyph (fromEnum d - fromEnum '0'))
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
            , ('3', V4 240 140 240 255)
            , ('U', V4 180 200 255 255)
            , ('S', V4 200 160 255 255)
            , ('D', V4 140 220 200 255)
            , ('M', V4 180 200 255 255)
            , ('R', V4 255 160 140 255)
            , ('N', V4 140 220 160 255)
            , ('P', V4 255 200 120 255)
            ]
      rendererDrawColor ren $= V4 20 20 32 220
      fillRect ren (Just (Rectangle (P (V2 0 y)) (V2 winW 22)))
      forM_ (zip [0 :: CInt ..] keys) $ \(i, (ch, col)) ->
        drawKeyChip ren (8 + i * 22) (y + 2) ch col

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
          panelH = 430 :: CInt
      rendererDrawColor ren $= V4 32 32 48 245
      fillRect ren (Just (Rectangle (P (V2 32 panelY)) (V2 (winW - 64) panelH)))
      rendererDrawColor ren $= V4 255 200 80 255
      drawRect ren (Just (Rectangle (P (V2 32 panelY)) (V2 (winW - 64) panelH)))
      drawBannerWord ren 100 (panelY + 16) 4 (V4 255 220 100 255) "PAUSE"
      -- Mechanism reminder strip (carpet / steam)
      drawBannerWord ren 60 (panelY + 52) 2 (V4 200 140 180 255) "CARPET"
      drawBannerWord ren 220 (panelY + 52) 2 (V4 180 190 200 255) "STEAM"
      let rows :: [(Int, Char, String)]
          rows =
            [ (0, 'H', "HINT")
            , (1, '1', "HAMMER")
            , (2, '2', "SWAP")
            , (3, '3', "CROSS")
            , (4, 'U', "UNDO")
            , (5, 'S', "SHUFFLE")
            , (6, 'D', "DAILY")
            , (7, 'M', "MAP")
            , (8, 'R', "RETRY")
            , (9, 'N', "NEXT")
            , (10, 'P', "PLAY")
            ]
      forM_ rows $ \(i, ch, label) -> do
        let yy = panelY + 72 + fromIntegral i * 28
        drawKeyChip ren 80 yy ch (V4 255 220 120 255)
        drawBannerWord ren 120 yy 2 (V4 210 210 230 255) label

drawHud :: Renderer -> App -> IO ()
drawHud ren app = do
  let gs = appGame app
      lvl = allLevels !! min (gsLevel gs) (length allLevels - 1)
      white = V4 230 230 245 255 :: V4 Word8
      dim = V4 140 140 170 255
      _accent = V4 255 200 80 255 :: V4 Word8
  rendererDrawColor ren $= V4 42 42 58 255
  fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW hudH)))

  -- Level label
  drawNumber ren 10 8 3 white (gsLevel gs + 1)
  forM_ (zip [0 :: Int ..] allLevels) $ \(i, _) -> do
    let col =
          if i == gsLevel gs
            then V4 255 200 80 255
            else if i < gsLevel gs then V4 80 180 120 255 else V4 60 60 80 255
        -- 38 levels fit in HUD: 7px stride
        xDot = 60 + fromIntegral i * 7
    rendererDrawColor ren $= col
    fillRect ren (Just (Rectangle (P (V2 xDot 10)) (V2 7 14)))

  -- Goal meter (score or collect)
  let prog = goalProgress (gsGoal gs) (gsScore gs) (gsCollected gs)
      targ = goalTarget (gsGoal gs)
      meterCol = case gsGoal gs of
        GoalScore _ -> V4 100 220 140 255
        GoalCollectMulti _ -> V4 220 180 100 255
        GoalClearStone _ -> V4 160 160 170 255
        GoalChest _ -> V4 220 170 60 255
        GoalHoney _ -> V4 240 180 40 255
        GoalBalloon _ -> V4 255 120 160 255
        GoalCookie _ -> V4 210 160 90 255
        GoalCake _ -> V4 255 140 180 255
        GoalSafe _ -> V4 200 170 50 255
        GoalUfo _ -> V4 180 120 255 255
        GoalCarpet _ -> V4 180 100 160 255
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
    GoalChest _ -> do
      rendererDrawColor ren $= V4 220 170 60 255
      fillRect ren (Just (Rectangle (P (V2 200 42)) (V2 20 16)))
      rendererDrawColor ren $= V4 180 120 40 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 12 6)))
    GoalHoney _ -> do
      rendererDrawColor ren $= V4 240 180 40 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 18)))
      rendererDrawColor ren $= V4 200 140 20 255
      fillRect ren (Just (Rectangle (P (V2 206 36)) (V2 8 6)))
    GoalBalloon _ -> do
      rendererDrawColor ren $= V4 255 120 160 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 14 16)))
      rendererDrawColor ren $= V4 200 80 120 255
      fillRect ren (Just (Rectangle (P (V2 209 54)) (V2 4 6)))
    GoalCookie _ -> do
      rendererDrawColor ren $= V4 210 160 90 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 16)))
      rendererDrawColor ren $= V4 90 50 30 255
      fillRect ren (Just (Rectangle (P (V2 206 44)) (V2 3 3)))
      fillRect ren (Just (Rectangle (P (V2 212 48)) (V2 3 3)))
    GoalCake _ -> do
      -- Pink frosted cake swatch (distinct from tan cookie)
      rendererDrawColor ren $= V4 255 140 180 255
      fillRect ren (Just (Rectangle (P (V2 202 44)) (V2 16 14)))
      rendererDrawColor ren $= V4 255 220 230 255
      fillRect ren (Just (Rectangle (P (V2 204 38)) (V2 12 8)))
      rendererDrawColor ren $= V4 255 80 120 255
      fillRect ren (Just (Rectangle (P (V2 208 36)) (V2 4 4)))
    GoalSafe _ -> do
      -- Brass vault door swatch
      rendererDrawColor ren $= V4 200 170 50 255
      fillRect ren (Just (Rectangle (P (V2 202 40)) (V2 16 18)))
      rendererDrawColor ren $= V4 80 80 90 255
      fillRect ren (Just (Rectangle (P (V2 208 46)) (V2 6 6)))
    GoalUfo _ -> do
      rendererDrawColor ren $= V4 180 120 255 255
      fillRect ren (Just (Rectangle (P (V2 200 42)) (V2 20 16)))
      rendererDrawColor ren $= V4 220 200 255 255
      fillRect ren (Just (Rectangle (P (V2 204 40)) (V2 12 6)))
    GoalCarpet _ -> do
      -- Magenta weave swatch (地毯)
      rendererDrawColor ren $= V4 180 100 160 255
      fillRect ren (Just (Rectangle (P (V2 200 40)) (V2 20 20)))
      rendererDrawColor ren $= V4 220 140 200 255
      fillRect ren (Just (Rectangle (P (V2 204 44)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 212 44)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 208 50)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 204 52)) (V2 4 4)))
      fillRect ren (Just (Rectangle (P (V2 212 52)) (V2 4 4)))
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
    rendererDrawColor ren $= V4 220 80 220 255
    fillRect ren (Just (Rectangle (P (V2 (hx + 100) 8)) (V2 14 14)))
    drawNumber ren (hx + 118) 8 2 white (gsCrossClears gs)
    case appTool app of
      ToolHammer -> do
        rendererDrawColor ren $= V4 255 180 80 255
        drawBannerWord ren (hx) 72 2 (V4 255 200 100 255) "HAMMER"
      ToolFreeSwap _ -> do
        drawBannerWord ren (hx) 72 2 (V4 140 200 255 255) "SWAP"
      ToolCross -> do
        drawBannerWord ren (hx) 72 2 (V4 240 140 240 255) "CROSS"
      ToolNone -> pure ()

  -- Combo badge (连击反馈)
  when (gsCombo gs > 1 && appComboShow app > 0) $ do
    let intensity = min 255 (140 + gsCombo gs * 25)
        pulseBright = fromIntegral (200 + (appPulse app `mod` 40)) :: Word8
        badgeCol = V4 255 (fromIntegral intensity) 40 255
    rendererDrawColor ren $= V4 50 20 10 255
    fillRect ren (Just (Rectangle (P (V2 (winW - 130) 30)) (V2 110 50)))
    rendererDrawColor ren $= badgeCol
    drawRect ren (Just (Rectangle (P (V2 (winW - 130) 30)) (V2 110 50)))
    drawBannerWord ren (winW - 124) 34 2 (V4 255 pulseBright 80 255) "COMBO"
    drawNumber ren (winW - 70) 52 3 badgeCol (gsCombo gs)

  -- Status strip
  case gsOver gs of
    Just (Won _) -> rendererDrawColor ren $= V4 60 180 90 255
    Just (LevelClear _ _) -> rendererDrawColor ren $= V4 220 180 60 255
    Just (Lost _) -> rendererDrawColor ren $= V4 200 70 70 255
    _ ->
      rendererDrawColor ren $=
        if gsShuffled gs then V4 180 140 220 255 else V4 70 70 90 255
  fillRect ren (Just (Rectangle (P (V2 (winW - 24) 8)) (V2 16 (hudH - 16))))


-- | Floating 连击 pop over the board center (voice-style visual shout).
drawComboPop :: Renderer -> App -> IO ()
drawComboPop ren app
  | appMapOpen app = pure ()
  | appPaused app = pure ()
  | gsCombo (appGame app) <= 1 = pure ()
  | appComboShow app <= 0 = pure ()
  | otherwise = do
      let combo = gsCombo (appGame app)
          life = appComboShow app
          -- Rise and fade over the show window
          yOff = fromIntegral ((120 - min 120 life) `div` 2) :: CInt
          alphaPulse = fromIntegral (180 + (appPulse app `mod` 50)) :: Word8
          cx = padPx + boardPx `div` 2 - 70
          cy = hudH + padPx + boardPx `div` 2 - 40 - yOff
          col =
            if combo >= 5
              then V4 255 80 200 alphaPulse
              else if combo >= 3
                then V4 255 160 40 alphaPulse
                else V4 255 220 80 alphaPulse
      rendererDrawColor ren $= V4 20 10 5 180
      fillRect ren (Just (Rectangle (P (V2 (cx - 8) (cy - 8))) (V2 160 56)))
      rendererDrawColor ren $= col
      drawRect ren (Just (Rectangle (P (V2 (cx - 8) (cy - 8))) (V2 160 56)))
      drawBannerWord ren cx cy 3 col "COMBO"
      drawNumber ren (cx + 100) (cy + 8) 4 col combo

-- | Chapter boundaries (0-based level index starts). Light map separators.
chapterStarts :: [Int]
chapterStarts = [0, 7, 14, 21, 28, 34, 36]

chapterLabel :: Int -> String
chapterLabel 0 = "CH1"
chapterLabel 7 = "CH2"
chapterLabel 14 = "CH3"
chapterLabel 21 = "CH4"
chapterLabel 28 = "CH5"
chapterLabel 34 = "CH6"
chapterLabel 36 = "CH7"
chapterLabel _ = ""

-- | Extra vertical gap before chapter-start nodes.
chapterGapBefore :: Int -> CInt
chapterGapBefore i
  | i `elem` drop 1 chapterStarts = 14
  | otherwise = 0

-- | Simplified campaign map (选关): zig-zag nodes + chapter gaps.
mapNodePos :: Int -> (CInt, CInt)
mapNodePos i =
  let cols = 6 :: Int
      row = i `div` cols
      col = i `mod` cols
      col' = if even row then col else (cols - 1 - col)
      -- Accumulate chapter gaps for rows that contain a chapter start
      -- Compact spacing so all 38 nodes + CH1–CH7 labels fit in winH.
      gapY =
        sum
          [ chapterGapBefore j
          | j <- [0 .. i]
          ]
      x = 40 + fromIntegral col' * 72
      y = hudH + 36 + fromIntegral row * 52 + gapY
  in (x, y)

mapHitTest :: Int32 -> Int32 -> Maybe Int
mapHitTest mx my =
  let hits =
        [ i
        | i <- [0 .. length allLevels - 1]
        , let (nx, ny) = mapNodePos i
              r = 22 :: CInt
        , fromIntegral mx >= nx - r
        , fromIntegral mx <= nx + r
        , fromIntegral my >= ny - r
        , fromIntegral my <= ny + r
        ]
  in case hits of
       (i : _) -> Just i
       [] -> Nothing

drawLevelMap :: Renderer -> App -> IO ()
drawLevelMap ren app
  | not (appMapOpen app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 12 18 28 230
      fillRect ren (Just (Rectangle (P (V2 0 0)) (V2 winW winH)))
      drawBannerWord ren 80 20 4 (V4 255 220 100 255) "MAP"
      drawBannerWord ren 250 28 2 (V4 180 200 220 255) "M"
      -- Chapter separators / labels
      forM_ chapterStarts $ \ci -> do
        let lab = chapterLabel ci
        when (not (null lab)) $ do
          let (_nx, ny) = mapNodePos ci
          rendererDrawColor ren $= V4 90 110 140 255
          fillRect ren (Just (Rectangle (P (V2 16 (ny - 36))) (V2 (winW - 32) 2)))
          drawBannerWord ren 20 (ny - 32) 2 (V4 160 190 220 255) lab
      -- Path lines between consecutive nodes (skip visual break at chapter edges)
      rendererDrawColor ren $= V4 60 80 100 255
      forM_ [0 .. length allLevels - 2] $ \i -> do
        let (x0, y0) = mapNodePos i
            (x1, y1) = mapNodePos (i + 1)
        drawLine ren (P (V2 x0 y0)) (P (V2 x1 y1))
      let reached = appMaxReached app
          cur = gsLevel (appGame app)
      forM_ (zip [0 :: Int ..] allLevels) $ \(i, lvl) -> do
        let (nx, ny) = mapNodePos i
            unlocked = i <= reached
            isCur = i == cur
            body
              | isCur = V4 255 200 60 255
              | unlocked = V4 80 180 120 255
              | otherwise = V4 50 50 70 255
        rendererDrawColor ren $= body
        fillRect ren (Just (Rectangle (P (V2 (nx - 18) (ny - 18))) (V2 36 36)))
        rendererDrawColor ren $= V4 230 230 245 255
        drawRect ren (Just (Rectangle (P (V2 (nx - 18) (ny - 18))) (V2 36 36)))
        -- Pulse ring on current level so map focus is obvious
        when isCur $ do
          let bright = fromIntegral (200 + (appPulse app `mod` 50)) :: Word8
          rendererDrawColor ren $= V4 255 bright 60 255
          drawRect ren (Just (Rectangle (P (V2 (nx - 22) (ny - 22))) (V2 44 44)))
          drawRect ren (Just (Rectangle (P (V2 (nx - 20) (ny - 20))) (V2 40 40)))
        drawNumber ren (nx - 10) (ny - 8) 2 (V4 240 240 255 255) (i + 1)
        -- Tiny goal color pip
        let pip = case lvlGoal lvl of
              GoalScore _ -> V4 100 220 140 255
              GoalCollect c _ -> let (r,g,b) = colorRGB c in V4 r g b 255
              GoalCollectMulti _ -> V4 220 180 100 255
              GoalClearStone _ -> V4 160 160 170 255
              GoalChest _ -> V4 220 170 60 255
              GoalHoney _ -> V4 240 180 40 255
              GoalBalloon _ -> V4 255 120 160 255
              GoalCookie _ -> V4 210 160 90 255
              GoalCake _ -> V4 255 140 180 255
              GoalSafe _ -> V4 200 170 50 255
              GoalUfo _ -> V4 180 120 255 255
              GoalCarpet _ -> V4 180 100 160 255
        rendererDrawColor ren $= pip
        fillRect ren (Just (Rectangle (P (V2 (nx - 6) (ny + 22))) (V2 12 6)))

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
      LevelClear _ nextIdx -> do
        rendererDrawColor ren $= V4 40 50 20 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 255 220 80 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawRect ren (Just (Rectangle (P (V2 26 (panelY + 2))) (V2 (winW - 52) (panelH - 4))))
        drawBannerWord ren 80 (panelY + 18) 5 (V4 255 230 100 255) "CLEAR!"
        let stars = starRating (appStartMoves app) (gsMoves (appGame app))
        drawNumber ren 200 (panelY + 22) 3 (V4 255 220 80 255) stars
        -- Clearer next-level prompt: NEXT L# + N key chip (click / N / Space)
        drawBannerWord ren 60 (panelY + 70) 3 (V4 200 220 180 255) "NEXT"
        drawNumber ren 180 (panelY + 68) 3 (V4 200 220 180 255) (nextIdx + 1)
        drawKeyChip ren (winW - 100) (panelY + 70) 'N' (V4 140 220 160 255)
      Won s -> do
        rendererDrawColor ren $= V4 20 50 30 240
        fillRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        rendererDrawColor ren $= V4 80 220 120 255
        drawRect ren (Just (Rectangle (P (V2 24 panelY)) (V2 (winW - 48) panelH)))
        drawBannerWord ren 110 (panelY + 18) 5 (V4 120 255 160 255) "WIN!"
        let stars = starRating (appStartMoves app) (gsMoves (appGame app))
        drawNumber ren 200 (panelY + 22) 3 (V4 255 220 80 255) stars
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
  Chest layers -> do
    let gap = 3 :: CInt
        (cr, cg, cb) = if flashing then (255, 230, 140) else (200, 150, 50)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 6)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 6))))
    rendererDrawColor ren $= V4 230 180 70 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) 14)))
    rendererDrawColor ren $= V4 80 160 220 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + cellPx `div` 2 - 2))) (V2 10 10)))
    rendererDrawColor ren $= V4 255 240 180 255
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
  Honey layers -> do
    -- Amber honey jar (蜂蜜罐): round body + lid + drip, distinct from chest
    let gap = 4 :: CInt
        (cr, cg, cb) = if flashing then (255, 230, 120) else (230, 170, 35)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap + 4) (y + gap + 10)))
            (V2 (cellPx - 2 * gap - 8) (cellPx - 2 * gap - 12))))
    -- Lid
    rendererDrawColor ren $= V4 180 120 30 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 8) (y + gap + 2))) (V2 (cellPx - 2 * gap - 16) 10)))
    -- Highlight drip
    rendererDrawColor ren $= V4 255 220 100 220
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + cellPx `div` 2))) (V2 8 14)))
    -- Layer pips
    rendererDrawColor ren $= V4 255 240 160 255
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
  Balloon col -> do
    let (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
        gap = 6 :: CInt
    -- Roundish body
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 8))))
    -- Highlight
    rendererDrawColor ren $= V4 255 255 255 160
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 4) (y + gap + 4))) (V2 8 8)))
    -- String
    rendererDrawColor ren $= V4 220 220 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 1) (y + cellPx - 14))) (V2 2 10)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Cookie -> do
    -- Tan biscuit with chocolate chips (饼干)
    let gap = 5 :: CInt
        (cr, cg, cb) = if flashing then (255, 220, 160) else (210, 160, 90)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    rendererDrawColor ren $= V4 90 50 30 255
    fillRect ren (Just (Rectangle (P (V2 (x + 14) (y + 14))) (V2 6 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + 28) (y + 22))) (V2 5 5)))
    fillRect ren (Just (Rectangle (P (V2 (x + 18) (y + 32))) (V2 5 5)))
    fillRect ren (Just (Rectangle (P (V2 (x + 32) (y + 12))) (V2 4 4)))
    rendererDrawColor ren $= V4 180 120 60 255
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Cake layers -> do
    -- Pink layered cake (蛋糕) — frosted tiers, distinct from tan Cookie
    let gap = 4 :: CInt
        (cr, cg, cb) = if flashing then (255, 200, 220) else (255, 140, 180)
    -- Bottom tier
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 18)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 18))))
    -- Mid frosting
    rendererDrawColor ren $= V4 255 220 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 4) (y + gap + 10))) (V2 (cellPx - 2 * gap - 8) 10)))
    -- Top cherry
    rendererDrawColor ren $= V4 220 40 80 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + gap + 2))) (V2 8 8)))
    -- Layer pips
    rendererDrawColor ren $= V4 255 240 250 255
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
  MagicHat -> do
    -- Purple magic hat (魔法帽): brim + cone
    let gap = 5 :: CInt
        (cr, cg, cb) = if flashing then (200, 160, 255) else (120, 60, 180)
    rendererDrawColor ren $= V4 cr cg cb 255
    -- Brim
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + cellPx - 18))) (V2 (cellPx - 2 * gap) 10)))
    -- Cone
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap + 8) (y + gap + 4)))
            (V2 (cellPx - 2 * gap - 16) (cellPx - 2 * gap - 14))))
    -- Band
    rendererDrawColor ren $= V4 255 220 80 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 6) (y + cellPx - 24))) (V2 (cellPx - 2 * gap - 12) 5)))
    -- Star tip
    rendererDrawColor ren $= V4 255 255 200 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + gap))) (V2 6 6)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Maker col charges -> do
    -- Juice maker / factory (果汁机): metallic body + color spout + charge pips
    let gap = 4 :: CInt
        (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
    rendererDrawColor ren $= V4 90 100 120 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 8)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap - 8))))
    -- Spout / hopper tinted with target color
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 8) (y + gap))) (V2 (cellPx - 2 * gap - 16) 12)))
    rendererDrawColor ren $= V4 200 210 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 6) (y + gap + 14))) (V2 12 8)))
    -- Charge pips
    rendererDrawColor ren $= V4 255 220 80 255
    forM_ [0 .. min 3 charges - 1] $ \i ->
      fillRect
        ren
        (Just
           (Rectangle
              (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
              (V2 7 5)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Snail dr dc -> do
    -- Snail (蜗牛): olive body + shell spiral; tip shows crawl direction
    let gap = 5 :: CInt
        (cr, cg, cb) = if flashing then (180, 230, 140) else (90, 150, 60)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap + 4) (y + gap + 12)))
            (V2 (cellPx - 2 * gap - 8) (cellPx - 2 * gap - 16))))
    rendererDrawColor ren $= V4 60 110 40 255
    fillRect ren (Just (Rectangle (P (V2 (x + gap + 10) (y + gap + 4))) (V2 (cellPx - 2 * gap - 20) 14)))
    rendererDrawColor ren $= V4 200 230 120 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + gap + 8))) (V2 8 8)))
    -- Direction tick
    rendererDrawColor ren $= V4 255 255 200 255
    let (tx, ty) =
          if abs dr >= abs dc
            then if dr >= 0
                   then (x + cellPx `div` 2 - 3, y + cellPx - 14)
                   else (x + cellPx `div` 2 - 3, y + 8)
            else if dc >= 0
                   then (x + cellPx - 14, y + cellPx `div` 2 - 3)
                   else (x + 8, y + cellPx `div` 2 - 3)
    fillRect ren (Just (Rectangle (P (V2 tx ty)) (V2 6 6)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Safe layers -> do
    -- Vault / safe (保险箱): dark steel door + gold dial; distinct from Chest
    let gap = 3 :: CInt
        (cr, cg, cb) = if flashing then (220, 200, 120) else (70, 75, 85)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    rendererDrawColor ren $= V4 200 170 50 255
    drawRect ren (Just (Rectangle (P (V2 (x + gap + 2) (y + gap + 2))) (V2 (cellPx - 2 * gap - 4) (cellPx - 2 * gap - 4))))
    -- Dial
    rendererDrawColor ren $= V4 220 190 60 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 8) (y + cellPx `div` 2 - 8))) (V2 16 16)))
    rendererDrawColor ren $= V4 40 40 50 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + cellPx `div` 2 - 3))) (V2 6 6)))
    rendererDrawColor ren $= V4 255 230 120 255
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
  Flip front back -> do
    -- Dual-face gem (双面块): front color body + back color corner triangle
    let gap = 4 :: CInt
        (fr, fg, fb) = colorRGB front
        (br, bg, bb) = colorRGB back
        (cr, cg, cb) = if flashing then (255, 255, 255) else (fr, fg, fb)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Back-face wedge (top-right)
    rendererDrawColor ren $= V4 br bg bb 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2) (y + gap))) (V2 (cellPx `div` 2 - gap) (cellPx `div` 2 - gap))))
    rendererDrawColor ren $= V4 255 255 255 200
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    -- Split line
    rendererDrawColor ren $= V4 30 30 40 220
    drawLine ren (P (V2 (x + gap) (y + cellPx - gap))) (P (V2 (x + cellPx - gap) (y + gap)))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Surprise -> do
    -- Surprise egg / gift box (彩蛋): pink package + gold bow
    let gap = 4 :: CInt
        (cr, cg, cb) = if flashing then (255, 200, 220) else (255, 90, 150)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap)))
            (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    rendererDrawColor ren $= V4 255 210 80 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + gap))) (V2 6 (cellPx - 2 * gap))))
    fillRect ren (Just (Rectangle (P (V2 (x + gap) (y + cellPx `div` 2 - 3))) (V2 (cellPx - 2 * gap) 6)))
    rendererDrawColor ren $= V4 255 255 255 220
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap))) (V2 (cellPx - 2 * gap) (cellPx - 2 * gap))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  Bottle col -> do
    -- Dye bottle (染色瓶): body tinted with bottle color + neck
    let gap = 6 :: CInt
        (cr0, cg0, cb0) = colorRGB col
        (cr, cg, cb) = if flashing then (255, 255, 255) else (cr0, cg0, cb0)
    rendererDrawColor ren $= V4 cr cg cb 255
    fillRect
      ren
      (Just
         (Rectangle
            (P (V2 (x + gap) (y + gap + 10)))
            (V2 (cellPx - 2 * gap) (cellPx - gap - 14))))
    -- Neck
    rendererDrawColor ren $= V4 220 220 230 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 5) (y + gap))) (V2 10 12)))
    rendererDrawColor ren $= V4 40 40 50 255
    drawRect ren (Just (Rectangle (P (V2 (x + gap) (y + gap + 10))) (V2 (cellPx - 2 * gap) (cellPx - gap - 14))))
    when flashing $ do
      rendererDrawColor ren $= V4 255 255 200 200
      drawRect ren (Just (Rectangle (P (V2 (x + 1) (y + 1))) (V2 (cellPx - 2) (cellPx - 2))))
  TimeSpirit -> do
    -- Time spirit (时间精灵): cyan orb + hourglass ticks; adjacent clear → +2 moves
    rendererDrawColor ren $= V4 40 180 220 255
    fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 10))) (V2 (cellPx - 20) (cellPx - 20))))
    rendererDrawColor ren $= V4 200 250 255 255
    fillRect ren (Just (Rectangle (P (V2 (x + 16) (y + 14))) (V2 (cellPx - 32) 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + 16) (y + cellPx - 20))) (V2 (cellPx - 32) 6)))
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 3) (y + 18))) (V2 6 (cellPx - 36))))
    rendererDrawColor ren $= V4 255 240 100 255
    fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 8) (y + cellPx `div` 2 - 4))) (V2 16 8)))
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
    -- Grass / vine / chocolate overlays (开心消消乐草·藤蔓·巧克力)
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
      Just Choco -> do
        -- Brown chocolate slab with bite notches (不改六色宝石本体)
        rendererDrawColor ren $= V4 110 60 30 220
        fillRect ren (Just (Rectangle (P (V2 (x + 5) (y + 5))) (V2 (cellPx - 10) (cellPx - 10))))
        rendererDrawColor ren $= V4 80 40 20 255
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + 8))) (V2 (cellPx - 16) 4)))
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx `div` 2 - 2))) (V2 (cellPx - 16) 4)))
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + cellPx - 16))) (V2 (cellPx - 16) 4)))
        rendererDrawColor ren $= V4 150 90 50 200
        fillRect ren (Just (Rectangle (P (V2 (x + 12) (y + 14))) (V2 6 6)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx - 20) (y + cellPx - 22))) (V2 6 6)))
      Just (Fog layers) -> do
        -- Soft white/gray cloud veil (迷雾); layer pips
        rendererDrawColor ren $= V4 200 210 230 200
        fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
        rendererDrawColor ren $= V4 240 245 255 180
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 10))) (V2 14 10)))
        fillRect ren (Just (Rectangle (P (V2 (x + 22) (y + 18))) (V2 16 12)))
        fillRect ren (Just (Rectangle (P (V2 (x + 12) (y + 26))) (V2 18 10)))
        rendererDrawColor ren $= V4 120 140 180 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just (Chain layers) -> do
        -- Iron chain lock (锁链): gray links over gem
        rendererDrawColor ren $= V4 70 75 90 220
        drawRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
        rendererDrawColor ren $= V4 140 150 170 255
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + cellPx `div` 2 - 4))) (V2 (cellPx - 20) 8)))
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 4) (y + 10))) (V2 8 (cellPx - 20))))
        rendererDrawColor ren $= V4 200 210 230 255
        fillRect ren (Just (Rectangle (P (V2 (x + cellPx `div` 2 - 6) (y + cellPx `div` 2 - 6))) (V2 12 12)))
        rendererDrawColor ren $= V4 180 190 210 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just (Freeze layers) -> do
        -- Rocket freeze (火箭冰冻): deep-blue glaze + snowflake ticks; ≠ cyan ice cracks
        rendererDrawColor ren $= V4 40 90 200 180
        fillRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
        rendererDrawColor ren $= V4 180 220 255 230
        drawRect ren (Just (Rectangle (P (V2 (x + 5) (y + 5))) (V2 (cellPx - 10) (cellPx - 10))))
        -- Snowflake cross
        drawLine ren (P (V2 (x + cellPx `div` 2) (y + 10))) (P (V2 (x + cellPx `div` 2) (y + cellPx - 10)))
        drawLine ren (P (V2 (x + 10) (y + cellPx `div` 2))) (P (V2 (x + cellPx - 10) (y + cellPx `div` 2)))
        drawLine ren (P (V2 (x + 14) (y + 14))) (P (V2 (x + cellPx - 14) (y + cellPx - 14)))
        drawLine ren (P (V2 (x + cellPx - 14) (y + 14))) (P (V2 (x + 14) (y + cellPx - 14)))
        rendererDrawColor ren $= V4 220 240 255 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just (Curtain layers) -> do
        -- Curtain / roller shade (窗帘): vertical fabric stripes; ≠ soft Fog clouds
        rendererDrawColor ren $= V4 160 50 90 200
        fillRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
        rendererDrawColor ren $= V4 200 80 120 220
        forM_ [0 .. 3 :: Int] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + 6)))
                  (V2 5 (cellPx - 14))))
        -- Rod
        rendererDrawColor ren $= V4 220 180 100 255
        fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) 5)))
        rendererDrawColor ren $= V4 255 220 180 255
        forM_ [0 .. min 3 layers - 1] $ \i ->
          fillRect
            ren
            (Just
               (Rectangle
                  (P (V2 (x + 8 + fromIntegral i * 10) (y + cellPx - 12)))
                  (V2 7 5)))
      Just Steam -> do
        -- Steam cloud (蒸汽): soft gray wisps; blocks match until adjacent clear
        rendererDrawColor ren $= V4 170 180 190 190
        fillRect ren (Just (Rectangle (P (V2 (x + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
        rendererDrawColor ren $= V4 220 230 240 200
        fillRect ren (Just (Rectangle (P (V2 (x + 8) (y + 8))) (V2 16 10)))
        fillRect ren (Just (Rectangle (P (V2 (x + 20) (y + 16))) (V2 18 12)))
        fillRect ren (Just (Rectangle (P (V2 (x + 10) (y + 28))) (V2 20 10)))
        rendererDrawColor ren $= V4 140 160 180 255
        drawRect ren (Just (Rectangle (P (V2 (x + 3) (y + 3))) (V2 (cellPx - 6) (cellPx - 6))))
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
drawStatic ren app board yOff = case appArt app of
  Just art -> drawStaticArt ren art app board yOff
  Nothing -> drawStaticPrim ren app board yOff

-- | 原有矩形版棋盘绘制（无贴图时的回退）。
drawStaticPrim :: Renderer -> App -> Board -> CInt -> IO ()
drawStaticPrim ren app board yOff = do
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
            -- Soft checkerboard under gems; carpet weave if open / covered target
            carpetOpen = pos `elem` gsCarpetOpen (appGame app)
            carpetCovered =
              pos `elem` levelCarpets (gsLevel (appGame app))
                && not carpetOpen
                && not (null (levelCarpets (gsLevel (appGame app))))
            (br, bg, bb) =
              if carpetOpen
                then (90, 40, 80)  -- uncovered target (magenta base)
                else if carpetCovered
                  then (140, 70, 120)  -- covered weave
                  else if even (r + c) then (36, 36, 48) else (28, 28, 40)
        rendererDrawColor ren $= V4 br bg bb 255
        fillRect ren (Just (Rectangle (P (V2 x0 y)) (V2 cellPx cellPx)))
        -- Carpet weave ticks (目标地砖底纹)
        when (carpetOpen || carpetCovered) $ do
          let tick = if carpetCovered then V4 200 120 180 220 else V4 160 80 140 200
          rendererDrawColor ren $= tick
          fillRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 10))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 22) (y + 10))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 36) (y + 10))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 15) (y + 24))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 29) (y + 24))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 38))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 22) (y + 38))) (V2 6 6)))
          fillRect ren (Just (Rectangle (P (V2 (x0 + 36) (y + 38))) (V2 6 6)))
          when carpetCovered $ do
            rendererDrawColor ren $= V4 255 200 230 180
            drawRect ren (Just (Rectangle (P (V2 (x0 + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
        drawGemAt ren x0 y cell flashing
        when (sel == Just pos) $ do
          let bright = fromIntegral (180 + (pulse `mod` 40) * 2) :: Word8
              (sr, sg, sb) = case appTool app of
                ToolHammer -> (255, 160, 80)
                ToolFreeSwap _ -> (100, 180, 255)
                ToolCross -> (220, 80, 220)
                ToolNone -> (255, bright, bright)
          -- Outer glow ring
          rendererDrawColor ren $= V4 sr sg sb 120
          drawRect ren (Just (Rectangle (P (V2 (x0 - 1) (y - 1))) (V2 (cellPx + 2) (cellPx + 2))))
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
  -- Portal pair markers (violet rings)
  mapM_ (drawPortal ren yOff) (gsPortals (appGame app))
  -- Vine / chocolate spread preview pulses
  drawVineSpreadHints ren yOff pulse (gsBoard (appGame app))
  drawChocoSpreadHints ren yOff pulse (gsBoard (appGame app))
  -- UFO overlays
  mapM_ (drawUfo ren yOff pulse) (gsUfos (appGame app))

-- | Pulse outline on cells a vine would spread onto next move.
drawVineSpreadHints :: Renderer -> CInt -> Int -> Board -> IO ()
drawVineSpreadHints ren yOff pulse board = do
  let sources =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , hasVine (getCell board (r, c))
        ]
      neigh (r, c) =
        filter
          (\(rr, cc) -> rr >= 0 && rr < boardSize && cc >= 0 && cc < boardSize)
          [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
      targets =
        [ q
        | p <- sources
        , q <- neigh p
        , case getCell board q of
            Gem _ _ _ Nothing -> True
            _ -> False
        ]
      alpha = fromIntegral (100 + (pulse `mod` 30) * 4) :: Word8
  rendererDrawColor ren $= V4 40 200 80 alpha
  mapM_
    ( \pos -> do
        let (x0, y0) = cellOrigin pos
            y = y0 + yOff
        drawRect ren (Just (Rectangle (P (V2 (x0 + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
    )
    targets

-- | Draw flying saucer overlay at its cell (飞碟).
drawChocoSpreadHints :: Renderer -> CInt -> Int -> Board -> IO ()
drawChocoSpreadHints ren yOff pulse board = do
  let sources =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , hasChoco (getCell board (r, c))
        ]
      neigh (r, c) =
        filter
          (\(rr, cc) -> rr >= 0 && rr < boardSize && cc >= 0 && cc < boardSize)
          [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
      targets =
        [ q
        | p <- sources
        , q <- neigh p
        , case getCell board q of
            Gem _ _ _ Nothing -> True
            _ -> False
        ]
      alpha = fromIntegral (100 + (pulse `mod` 30) * 4) :: Word8
  rendererDrawColor ren $= V4 160 90 40 alpha
  mapM_
    ( \pos -> do
        let (x0, y0) = cellOrigin pos
            y = y0 + yOff
        drawRect ren (Just (Rectangle (P (V2 (x0 + 4) (y + 4))) (V2 (cellPx - 8) (cellPx - 8))))
    )
    targets


drawPortal :: Renderer -> CInt -> (Pos, Pos) -> IO ()
drawPortal ren yOff (a, b) = do
  let mark pos = do
        let (x0, y0) = cellOrigin pos
            y = y0 + yOff
        rendererDrawColor ren $= V4 160 80 220 220
        drawRect ren (Just (Rectangle (P (V2 (x0 + 2) (y + 2))) (V2 (cellPx - 4) (cellPx - 4))))
        rendererDrawColor ren $= V4 220 160 255 180
        drawRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 8))) (V2 (cellPx - 16) (cellPx - 16))))
  mark a
  mark b

drawUfo :: Renderer -> CInt -> Int -> Ufo -> IO ()
drawUfo ren yOff pulse (Ufo cell col) = do
  let (x0, y0) = cellOrigin cell
      y = y0 + yOff
      (cr, cg, cb) = colorRGB col
      bob = fromIntegral ((pulse `mod` 20) - 10) :: CInt
  -- dome
  rendererDrawColor ren $= V4 220 220 240 230
  fillRect ren (Just (Rectangle (P (V2 (x0 + 14) (y + 10 + bob))) (V2 (cellPx - 28) 12)))
  -- saucer body tinted by target color
  rendererDrawColor ren $= V4 cr cg cb 240
  fillRect ren (Just (Rectangle (P (V2 (x0 + 8) (y + 20 + bob))) (V2 (cellPx - 16) 10)))
  rendererDrawColor ren $= V4 255 255 255 200
  fillRect ren (Just (Rectangle (P (V2 (x0 + 18) (y + 22 + bob))) (V2 (cellPx - 36) 4)))
  -- beam hint downward
  rendererDrawColor ren $= V4 cr cg cb 100
  drawLine ren (P (V2 (x0 + cellPx `div` 2) (y + 30 + bob))) (P (V2 (x0 + cellPx `div` 2) (y + cellPx - 6)))

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
  -- 贴图模式先画棋盘底（格子 / 地毯 / 传送带 / 传送门），交换中也不露底色
  forM_ (appArt app) $ \art -> drawBoardBgArt ren art app
  mapM_
    ( \(r, c) -> do
        let pos = (r, c)
        unless (pos == p1 || pos == p2) $ do
          let (x0, y0) = cellOrigin pos
          drawCellAny ren app x0 y0 (getCell board pos) (pos `elem` flashSet)
    )
    [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
  drawCellAny ren app xa ya c1 (p1 `elem` flashSet)
  drawCellAny ren app xb yb c2 (p2 `elem` flashSet)

drawFall :: Renderer -> App -> Board -> Int -> IO ()
drawFall ren app board frame = do
  let t = fromIntegral frame / fromIntegral fallFrames :: Double
      ease = 1 - (1 - t) * (1 - t)
      offset = round (fromIntegral cellPx * (1 - ease) * (-0.35)) :: CInt
  drawStatic ren app board offset

--------------------------------------------------------------------------------
-- 贴图渲染（assets/atlas.bmp）：棋子精灵、圆角面板 HUD、中文标签
--------------------------------------------------------------------------------

rect :: CInt -> CInt -> CInt -> CInt -> Rectangle CInt
rect x y w h = Rectangle (P (V2 x y)) (V2 w h)

cellRect :: CInt -> CInt -> Rectangle CInt
cellRect x y = rect x y cellPx cellPx

-- | 开发 / 截图用：MATCH3_LEVEL=N（1 起）直接从第 N 关开始；仅前端，不改规则。
envStartLevel :: IO Int
envStartLevel = do
  v <- lookupEnv "MATCH3_LEVEL"
  pure $ case v >>= readMaybe of
    Just n | n >= 1 && n <= length allLevels -> n - 1
    _ -> 0

-- | 开发 / 复现用：MATCH3_SEED=N 固定开局随机种子（便于 Xvfb 下按固定坐标复现问题）；
-- 未设置时仍随机。只影响首局，重开 / 下一关照旧随机。
envSeed :: IO Int
envSeed = do
  v <- lookupEnv "MATCH3_SEED"
  case v >>= readMaybe of
    Just n -> pure n
    Nothing -> randomIO

-- | MATCH3_SHOWCASE=1：把棋盘换成「全部棋子一览」，用于检查贴图（仅展示，不影响规则模块）。
showcaseState :: GameState -> GameState
showcaseState gs =
  gs
    { gsBoard = showcaseBoard
    , gsHint = Nothing
    , gsUfos = [Ufo (7, 7) C3]
    , gsPortals = [((7, 5), (7, 6))]
    , gsBelts = [[(7, 0), (7, 1), (7, 2), (7, 3)]]
    , gsCarpetOpen = [(7, 4)]
    }

showcaseBoard :: Board
showcaseBoard =
  [ [mkGem C1, mkGem C2, mkGem C3, mkGem C4, mkGem C5, Gem C1 LineH 0 Nothing, Gem C2 LineV 0 Nothing, Gem C3 Bomb 0 Nothing]
  , [Gem C4 Rainbow 0 Nothing, mkFlip C1 C3, mkCountdown C2 3, mkIceGem C5 1, mkIceGem C1 2, mkIceGem C2 3, mkGrassGem C3, mkVineGem C4]
  , [mkChocoGem C5, mkFogGem C1 1, mkFogGem C2 3, mkChainGem C3 1, mkChainGem C4 2, mkFreezeGem C5 1, mkFreezeGem C1 2, mkCurtainGem C2 1]
  , [mkCurtainGem C3 2, mkSteamGem C4, mkStoneLayers 1, mkStoneLayers 2, mkStoneLayers 3, mkChestLayers 1, mkChestLayers 2, mkHoneyLayers 2]
  , [mkBalloon C1, mkBalloon C2, mkBalloon C3, mkBalloon C4, mkBalloon C5, mkCookie, mkCakeLayers 1, mkCakeLayers 3]
  , [mkMagicHat, mkMakerCharges C1 3, mkMakerCharges C3 2, mkSnail 0 1, mkSnail 1 0, mkSafeLayers 2, mkSurprise, mkTimeSpirit]
  , [mkBottle C1, mkBottle C2, mkBottle C3, mkBottle C4, mkBottle C5, mkSnail 0 (-1), mkSnail (-1) 0, mkGem C1]
  , [mkGem C2, mkGem C3, mkGem C4, mkGem C5, mkGem C1, mkGem C2, mkGem C3, mkGem C4]
  ]

-- | 颜色 → 贴图后缀（c1..c5）。
colorKey :: Color -> String
colorKey C1 = "c1"
colorKey C2 = "c2"
colorKey C3 = "c3"
colorKey C4 = "c4"
colorKey C5 = "c5"

gemSprite :: Color -> String
gemSprite c = "gem_" ++ colorKey c

clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

-- | 0..1 正弦呼吸，周期约 period 帧。
breathe :: Int -> Double -> Double
breathe pulse period = 0.5 + 0.5 * sin (fromIntegral pulse * 2 * pi / period)

-- | 单格绘制入口：有贴图走精灵，否则走原矩形版。
drawCellAny :: Renderer -> App -> CInt -> CInt -> Cell -> Bool -> IO ()
drawCellAny ren app x y cell flashing = case appArt app of
  Just art -> drawCellArt ren art (appPulse app) x y cell flashing
  Nothing -> drawGemAt ren x y cell flashing

-- | 该格的主贴图名（用于检测资源缺失时逐格回退）。
primarySprite :: Cell -> String
primarySprite cell = case cell of
  Gem c k _ _ -> if k == Rainbow then "rainbow" else gemSprite c
  Stone _ -> "stone_3"
  Chest _ -> "chest"
  Honey _ -> "honey"
  Balloon c -> "balloon_" ++ colorKey c
  Cookie -> "cookie"
  Cake _ -> "cake_1"
  MagicHat -> "magic_hat"
  Maker c _ -> "maker_" ++ colorKey c
  Snail _ _ -> "snail"
  Safe _ -> "safe"
  Flip f _ -> gemSprite f
  Surprise -> "surprise"
  Bottle c -> "bottle_" ++ colorKey c
  TimeSpirit -> "time_spirit"
  Countdown c _ -> gemSprite c

-- | 精灵版单格：底层宝石 / 障碍 → 特殊标记 → 冰 → 覆盖层 → 层数角标 → 闪白。
drawCellArt :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> Bool -> IO ()
drawCellArt ren art pulse x y cell flashing
  | not (hasSprite art (primarySprite cell)) = drawGemAt ren x y cell flashing
  | otherwise = do
      let dst = cellRect x y
          spr n = void (drawSprite ren art n dst)
          -- 气球 / 精灵轻微上下浮动
          bob = round (2 * sin (fromIntegral pulse / 9 :: Double)) :: CInt
          sprBob n = void (drawSprite ren art n (cellRect x (y + bob)))
          badge = drawLayerBadge ren art x y
      case cell of
        Gem col kind ice ov -> do
          -- 炸弹：身后橙色呼吸光晕
          when (kind == Bomb) $ do
            let a = round (140 + 110 * breathe pulse 50) :: Int
            void (drawSpriteMod ren art "bomb_glow" dst (V3 255 255 255) (fromIntegral a))
          if kind == Rainbow
            then void (drawSpriteEx ren art "rainbow" dst (fromIntegral (pulse * 2 `mod` 360) :: CDouble) False)
            else spr (gemSprite col)
          case kind of
            LineH -> spr "line_h"
            LineV -> spr "line_v"
            Bomb -> spr "bomb_mark"
            _ -> pure ()
          when (ice > 0) $ spr ("ice_" ++ show (clampI 1 3 ice))
          layers <- case ov of
            Just Grass -> spr "grass" >> pure 0
            Just Vine -> spr "vine" >> pure 0
            Just Choco -> spr "choco" >> pure 0
            Just (Fog n) -> spr ("fog_" ++ show (clampI 1 2 n)) >> pure n
            Just (Chain n) -> spr ("chain_" ++ show (clampI 1 2 n)) >> pure n
            Just (Freeze n) -> spr ("freeze_" ++ show (clampI 1 2 n)) >> pure n
            Just (Curtain n) -> spr ("curtain_" ++ show (clampI 1 2 n)) >> pure n
            Just Steam -> spr "steam" >> pure 0
            Nothing -> pure 0
          badge (if layers > 0 then layers else ice)
        Stone n -> spr ("stone_" ++ show (clampI 1 3 n)) >> badge n
        Chest n -> spr "chest" >> badge n
        Honey n -> spr "honey" >> badge n
        Balloon c -> sprBob ("balloon_" ++ colorKey c)
        Cookie -> spr "cookie"
        Cake n -> spr ("cake_" ++ show (clampI 1 3 n)) >> badge n
        MagicHat -> spr "magic_hat"
        Maker c n -> do
          spr ("maker_" ++ colorKey c)
          -- 果汁机是计数器：剩余次数始终显示
          drawBadgeAt ren art x y (max 1 n)
        Snail dr dc -> do
          -- 贴图朝右；按爬行方向旋转 / 翻转
          let (ang, flipH)
                | abs dc >= abs dr && dc >= 0 = (0, False)
                | abs dc >= abs dr = (0, True)
                | dr > 0 = (90, False)
                | otherwise = (-90, False)
          void (drawSpriteEx ren art "snail" dst ang flipH)
        Safe n -> spr "safe" >> badge n
        Flip f b -> do
          spr (gemSprite f)
          -- 右上角小图 = 翻面后的颜色；左下角双箭头标记
          void (drawSprite ren art (gemSprite b) (rect (x + cellPx - 25) (y + 1) 24 24))
          spr "flip_mark"
        Surprise -> spr "surprise"
        Bottle c -> spr ("bottle_" ++ colorKey c)
        TimeSpirit -> sprBob "time_spirit"
        Countdown c n -> do
          spr (gemSprite c)
          spr ("countdown_" ++ show (clampI 1 9 n))
      when flashing $
        void (drawSpriteAdd ren art "spark" (rect (x - 10) (y - 10) (cellPx + 20) (cellPx + 20)) (V3 255 255 230) 210)

-- | 层数 ≥ 2 时右下角数字角标。
drawLayerBadge :: Renderer -> Art -> CInt -> CInt -> Int -> IO ()
drawLayerBadge ren art x y n = when (n >= 2) $ drawBadgeAt ren art x y n

drawBadgeAt :: Renderer -> Art -> CInt -> CInt -> Int -> IO ()
drawBadgeAt ren art x y n =
  void (drawSprite ren art ("badge_" ++ show (clampI 1 9 n)) (rect (x + cellPx - 23) (y + cellPx - 23) 23 23))

-- | 传送带每格的朝向角度（右 0 / 下 90 / 左 180 / 上 270）。
beltAngles :: [Pos] -> [(Pos, CDouble)]
beltAngles belt = go Nothing (zip belt (drop 1 belt ++ take 1 belt))
  where
    go _ [] = []
    go prev ((a, b) : rest) =
      let ang = case dirAngle a b of
            Just d -> d
            Nothing -> maybe 0 id prev
      in (a, ang) : go (Just ang) rest
    dirAngle (r1, c1) (r2, c2)
      | r1 == r2 && c2 == c1 + 1 = Just 0
      | r1 == r2 && c2 == c1 - 1 = Just 180
      | c1 == c2 && r2 == r1 + 1 = Just 90
      | c1 == c2 && r2 == r1 - 1 = Just 270
      | otherwise = Nothing

-- | 棋盘底层：圆角框 + 棋盘格 + 地毯 + 传送带 + 传送门（都在棋子下面）。
drawBoardBgArt :: Renderer -> Art -> App -> IO ()
drawBoardBgArt ren art app = do
  let gs = appGame app
      carpets = levelCarpets (gsLevel gs)
  _ <- drawPanel ren art "panel_dark" (rect (padPx - 8) (hudH + padPx - 8) (boardPx + 16) (boardPx + 16)) 16
  forM_ [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]] $ \pos@(r, c) -> do
    let (x, y) = cellOrigin pos
        carpetOpen = pos `elem` gsCarpetOpen gs
        carpetCovered = pos `elem` carpets && not carpetOpen
    void (drawSprite ren art (if even (r + c) then "tile_a" else "tile_b") (cellRect x y))
    when carpetCovered $ void (drawSprite ren art "carpet_covered" (cellRect x y))
    when carpetOpen $ void (drawSprite ren art "carpet_open" (cellRect x y))
  forM_ (gsBelts gs) $ \belt ->
    forM_ (beltAngles belt) $ \(pos, ang) -> do
      let (x, y) = cellOrigin pos
      void (drawSpriteEx ren art "belt" (cellRect x y) ang False)
  forM_ (gsPortals gs) $ \(a, b) ->
    forM_ [a, b] $ \pos -> do
      let (x, y) = cellOrigin pos
          spin = fromIntegral (appPulse app * 3 `mod` 360) :: CDouble
      void (drawSpriteEx ren art "portal" (cellRect x y) spin False)

-- | 精灵版棋盘：底层 → 提示光 → 棋子（下落时带 yOff）→ 选中框 → 蔓延预告 → 飞碟。
drawStaticArt :: Renderer -> Art -> App -> Board -> CInt -> IO ()
drawStaticArt ren art app board yOff = do
  let gs = appGame app
      pulse = appPulse app
      flashSet = map fst (appFlash app)
      hintCells = maybe [] (\(a, b) -> [a, b]) (gsHint gs)
      hintA = round (120 + 135 * breathe pulse 60) :: Int
  drawBoardBgArt ren art app
  forM_ hintCells $ \pos -> do
    let (x, y) = cellOrigin pos
    void (drawSpriteMod ren art "hint_glow" (rect (x - 3) (y - 3) (cellPx + 6) (cellPx + 6)) (V3 255 255 255) (fromIntegral hintA))
  forM_ [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]] $ \pos -> do
    let (x, y) = cellOrigin pos
    drawCellArt ren art pulse x (y + yOff) (getCell board pos) (pos `elem` flashSet)
  -- 提示格再叠一层淡淡的加色光，便于一眼看到
  forM_ hintCells $ \pos -> do
    let (x, y) = cellOrigin pos
    void (drawSpriteAdd ren art "hint_glow" (cellRect x y) (V3 255 230 150) (fromIntegral (hintA `div` 3)))
  forM_ (appSel app) $ \pos -> do
    let (x, y) = cellOrigin pos
        tint = case appTool app of
          ToolHammer -> V3 255 170 80
          ToolFreeSwap _ -> V3 110 190 255
          ToolCross -> V3 235 110 235
          ToolNone -> V3 255 255 255
        grow = round (2 * breathe pulse 30) :: CInt
    void (drawSpriteMod ren art "sel_ring" (rect (x - 2 - grow) (y - 2 - grow) (cellPx + 4 + 2 * grow) (cellPx + 4 + 2 * grow)) tint 255)
  -- 自由交换第一格：保持高亮
  case appTool app of
    ToolFreeSwap (Just p) -> do
      let (x, y) = cellOrigin p
      void (drawSpriteMod ren art "sel_ring" (cellRect x y) (V3 110 190 255) 220)
    _ -> pure ()
  -- 藤蔓 / 巧克力下一步可能蔓延到的格子：绿 / 棕色柔光呼吸
  let spreadA = fromIntegral (round (70 + 110 * breathe pulse 60) :: Int) :: Word8
  forM_ (spreadTargets hasVine (gsBoard gs)) $ \pos -> do
    let (x, y) = cellOrigin pos
    void (drawSpriteMod ren art "hint_glow" (cellRect x (y + yOff)) (V3 90 255 120) spreadA)
  forM_ (spreadTargets hasChoco (gsBoard gs)) $ \pos -> do
    let (x, y) = cellOrigin pos
    void (drawSpriteMod ren art "hint_glow" (cellRect x (y + yOff)) (V3 210 120 60) spreadA)
  forM_ (gsUfos gs) $ \(Ufo pos col) -> do
    let (x, y) = cellOrigin pos
        bob = round (3 * sin (fromIntegral pulse / 10 :: Double)) :: CInt
    ok <- drawSprite ren art ("ufo_" ++ colorKey col) (rect x (y + yOff - 12 + bob) cellPx cellPx)
    unless ok $ drawUfo ren yOff pulse (Ufo pos col)

-- | 与 drawVineSpreadHints 相同的判定：源格正交相邻、且无覆盖层的普通宝石格。
spreadTargets :: (Cell -> Bool) -> Board -> [Pos]
spreadTargets isSource board =
  [ q
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , isSource (getCell board (r, c))
  , q <- [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
  , inBounds q
  , case getCell board q of
      Gem _ _ _ Nothing -> True
      _ -> False
  ]

-- | 粒子：有贴图时用柔光圆点（按颜色着色、随寿命淡出）。
drawParticlesAny :: Renderer -> App -> IO ()
drawParticlesAny ren app = case appArt app of
  Nothing -> drawParticles ren (appParticles app)
  Just art ->
    forM_ (appParticles app) $ \p -> do
      let fade = if pMax p <= 0 then 255 else fromIntegral (255 * pLife p `div` pMax p) :: Word8
          s = pSize p * 3
          x = round (pX p) - s `div` 2
          y = round (pY p) - s `div` 2
      void (drawSpriteMod ren art "spark" (rect x y s s) (V3 (pR p) (pG p) (pB p)) fade)

--------------------------------------------------------------------------------
-- 贴图字形 / 中文标签 / 面板工具
--------------------------------------------------------------------------------

-- | 贴图字形文字：每字 4*px 宽、6*px 高（与位图字体步幅一致），颜色经 colorMod 着色。
textA :: Renderer -> Art -> CInt -> CInt -> CInt -> V4 Word8 -> String -> IO ()
textA ren art x0 y0 px col@(V4 r g b a) s =
  forM_ (zip [0 :: CInt ..] s) $ \(i, ch0) -> do
    let ch = if ch0 == 'x' then 'x' else toUpper ch0
        x = x0 + i * 4 * px
    unless (ch == ' ') $ do
      ok <- drawSpriteMod ren art ("g_" ++ show (ord ch)) (rect x y0 (4 * px) (6 * px)) (V3 r g b) a
      unless ok $ drawGlyph ren x (y0 + px) px col ch

textW :: CInt -> String -> CInt
textW px s = 4 * px * fromIntegral (length s)

-- | 水平居中文字。
textAC :: Renderer -> Art -> CInt -> CInt -> CInt -> V4 Word8 -> String -> IO ()
textAC ren art cx y px col s = textA ren art (cx - textW px s `div` 2) y px col s

-- | 中文标签贴图宽度（按目标高度 h 等比；用实际选中的尺寸变体计算，保证与绘制一致）。
zhW :: Art -> String -> CInt -> CInt
zhW art key h = maybe 0 (\(w0, h0) -> w0 * h `div` max 1 h0) (spriteSizeAt art key h)

-- | 画中文标签：按游戏内实际高度 h 烘焙了 2x 变体（见 tools/gen_assets.py 的 ZH_SIZES），
-- Retina 上贴图像素与物理像素 1:1；返回宽度。
zhA :: Renderer -> Art -> String -> CInt -> CInt -> CInt -> IO CInt
zhA ren art key x y h = do
  let w = zhW art key h
  when (w > 0) $ void (drawSprite ren art key (rect x y w h))
  pure w

zhAC :: Renderer -> Art -> String -> CInt -> CInt -> CInt -> IO ()
zhAC ren art key cx y h = void (zhA ren art key (cx - zhW art key h `div` 2) y h)

-- | 着色九宫格（进度条填充）。
drawPanelTint :: Renderer -> Art -> String -> Rectangle CInt -> CInt -> V3 Word8 -> IO ()
drawPanelTint ren art name r c tint = void (drawPanelMod ren art name r c tint)

-- | 按键小方块。
keyChipA :: Renderer -> Art -> CInt -> CInt -> Char -> V4 Word8 -> IO ()
keyChipA ren art x y ch col = do
  _ <- drawPanel ren art "panel_chip" (rect x y 22 22) 7
  textA ren art (x + 5) (y + 2) 3 col [ch]

-- | 进度条：底槽 + 着色填充 + 右对齐数字。
meterA :: Renderer -> Art -> CInt -> CInt -> CInt -> Int -> Int -> V3 Word8 -> String -> IO ()
meterA ren art x y w value cap tint label = do
  _ <- drawPanel ren art "panel_bar" (rect x y w 20) 9
  let inner = w - 4
      fw
        | cap <= 0 = 0
        | otherwise = min inner (max 0 (fromIntegral value * inner `div` fromIntegral (max 1 cap)))
  when (fw > 0) $ drawPanelTint ren art "panel_fill" (rect (x + 2) (y + 2) (max 16 fw) 16) 8 tint
  textA ren art (x + w - 8 - textW 3 label) (y + 1) 3 (V4 255 255 255 255) label

--------------------------------------------------------------------------------
-- 贴图版 HUD / 横幅 / 暂停 / 结算 / 地图
--------------------------------------------------------------------------------

-- | 目标图标（复用棋子贴图）。
goalIcon :: LevelGoal -> String
goalIcon g = case g of
  GoalScore _ -> "icon_score"
  GoalCollect c _ -> gemSprite c
  GoalCollectMulti _ -> "icon_multi"
  GoalClearStone _ -> "stone_3"
  GoalChest _ -> "chest"
  GoalHoney _ -> "honey"
  GoalBalloon _ -> "balloon_c1"
  GoalCookie _ -> "cookie"
  GoalCake _ -> "cake_1"
  GoalSafe _ -> "safe"
  GoalUfo _ -> "ufo_c3"
  GoalCarpet _ -> "carpet_covered"

-- | HUD 目标进度（与窗口标题使用同一组计数器）。
hudProgress :: GameState -> Int
hudProgress gs = case gsGoal gs of
  GoalScore _ -> gsScore gs
  GoalCollect _ _ -> gsCollected gs
  GoalCollectMulti reqs -> sum [min n (lookupCount (gsColorBag gs) c) | (c, n) <- reqs]
  GoalClearStone _ -> gsStonesCleared gs
  GoalChest _ -> gsChestsCleared gs
  GoalHoney _ -> gsHoneyCleared gs
  GoalBalloon _ -> gsBalloonsPopped gs
  GoalCookie _ -> gsCookiesCollected gs
  GoalCake _ -> gsCakesCleared gs
  GoalSafe _ -> gsSafesOpened gs
  GoalUfo _ -> gsUfoCollected gs
  GoalCarpet _ -> gsCarpetsCovered gs

goalTint :: LevelGoal -> V3 Word8
goalTint g = case g of
  GoalScore _ -> V3 110 230 150
  GoalCollect c _ -> let (r, gg, b) = colorRGB c in V3 r gg b
  GoalCollectMulti _ -> V3 240 190 100
  GoalClearStone _ -> V3 180 184 200
  GoalChest _ -> V3 230 170 70
  GoalHoney _ -> V3 250 190 50
  GoalBalloon _ -> V3 255 120 160
  GoalCookie _ -> V3 220 160 90
  GoalCake _ -> V3 255 140 190
  GoalSafe _ -> V3 200 180 90
  GoalUfo _ -> V3 170 130 255
  GoalCarpet _ -> V3 220 90 150

drawHudArt :: Renderer -> Art -> App -> IO ()
drawHudArt ren art app = do
  let gs = appGame app
      li = min (gsLevel gs) (length allLevels - 1)
      lvl = allLevels !! li
      white = V4 245 245 255 255
      dim = V4 150 145 190 255
      gold = V4 255 214 90 255
  _ <- drawPanel ren art "panel_dark" (rect 8 6 464 96) 14
  -- 关卡徽章 + 名称
  _ <- drawSprite ren art "medal" (rect 14 11 44 44)
  textAC ren art 36 24 3 white (show (li + 1))
  _ <- if gsDaily gs
    then zhA ren art "zh_daily" 66 12 22
    else zhA ren art ("name_" ++ show li) 66 11 24
  -- 关卡进度点：已过绿、当前金、未解锁暗
  forM_ [0 .. length allLevels - 1] $ \i -> do
    let xD = 66 + fromIntegral i * 6
        (col, yy, hh)
          | i == li = (V4 255 214 90 255, 38, 10)
          | i < li = (V4 90 210 130 255, 40, 6)
          | i <= appMaxReached app = (V4 120 180 140 255, 40, 6)
          | otherwise = (V4 80 72 130 255, 40, 6)
    rendererDrawColor ren $= col
    fillRect ren (Just (rect xD yy 4 hh))
  -- 道具：锤子 / 自由交换 / 十字消（当前模式金框）
  let chips =
        [ ("icon_hammer", gsHammers gs, appTool app == ToolHammer)
        , ("icon_swap", gsFreeSwaps gs, case appTool app of ToolFreeSwap _ -> True; _ -> False)
        , ("icon_cross", gsCrossClears gs, appTool app == ToolCross)
        ]
  forM_ (zip [0 :: CInt ..] chips) $ \(i, (ic, n, active)) -> do
    let cx = 298 + i * 57
    _ <- drawPanel ren art (if active then "panel_gold" else "panel_chip") (rect cx 11 53 32) 10
    _ <- drawSprite ren art ic (rect (cx + 4) 15 24 24)
    textA ren art (cx + 30) 18 3 (if n > 0 then white else dim) (show n)
  -- 目标条
  let goal = gsGoal gs
      prog = hudProgress gs
      targ = goalTarget goal
  _ <- drawSprite ren art (goalIcon goal) (rect 12 50 26 26)
  meterA ren art 42 53 332 prog targ (goalTint goal) (show prog ++ "/" ++ show targ)
  -- 步数条（≤5 步时变红并闪烁）
  let mv = gsMoves gs
      moveCap = max mv (lvlMoves lvl)
      low = mv <= 5
      tintMv
        | low = let k = round (160 + 95 * breathe (appPulse app) 40) :: Int in V3 255 (fromIntegral (k `div` 2)) 80
        | otherwise = V3 90 165 255
  _ <- drawSprite ren art "icon_moves" (rect 13 77 24 24)
  meterA ren art 42 79 332 mv (max 1 moveCap) tintMv (show mv)
  -- 右下：连击优先，否则得分
  if gsCombo gs > 1 && appComboShow app > 0
    then do
      _ <- drawPanel ren art "panel_gold" (rect 382 52 84 48) 12
      zhAC ren art "zh_combo" 424 56 18
      textAC ren art 424 76 3 gold ("x" ++ show (gsCombo gs))
    else do
      _ <- drawPanel ren art "panel_chip" (rect 382 52 84 48) 12
      zhAC ren art (if gsShuffled gs then "zh_shuffle" else "zh_score") 424 56 18
      textAC ren art 424 76 3 gold (show (gsScore gs))

-- | 首关提示横幅（棋盘上沿）。
drawTipBannerArt :: Renderer -> Art -> App -> IO ()
drawTipBannerArt ren art app
  | appPaused app || appMapOpen app = pure ()
  | appTipFrames app <= 0 = pure ()
  | gsLevel (appGame app) /= 0 = pure ()
  | appTool app /= ToolNone = pure ()
  | otherwise = do
      let y = hudH + padPx + 4
          w = 40 + zhW art "zh_tip" 20
          x = (winW - w) `div` 2
      _ <- drawPanel ren art "panel_gold" (rect x y w 30) 12
      keyChipA ren art (x + 10) (y + 5) 'H' (V4 255 220 100 255)
      void (zhA ren art "zh_tip" (x + 34) (y + 5) 20)

-- | 道具点选模式横幅：告诉玩家下一步要点哪里。
drawToolBannerArt :: Renderer -> Art -> App -> IO ()
drawToolBannerArt ren art app
  | appPaused app || appMapOpen app = pure ()
  | otherwise = case appTool app of
      ToolNone -> pure ()
      tool -> do
        let (ic, key) = case tool of
              ToolHammer -> ("icon_hammer", "zh_tool_hammer")
              ToolFreeSwap _ -> ("icon_swap", "zh_tool_swap")
              _ -> ("icon_cross", "zh_tool_cross")
            y = hudH + padPx + 4
            w = 44 + zhW art key 20
            x = (winW - w) `div` 2
        _ <- drawPanel ren art "panel_gold" (rect x y w 30) 12
        _ <- drawSprite ren art ic (rect (x + 10) (y + 4) 22 22)
        void (zhA ren art key (x + 36) (y + 5) 20)

-- | 开局 / 取消暂停后的按键条。
drawHelpStripArt :: Renderer -> Art -> App -> IO ()
drawHelpStripArt ren art app
  | appPaused app || appMapOpen app = pure ()
  | appHelpFrames app <= 0 = pure ()
  | otherwise = do
      -- 放在棋盘底部浮层，避免遮住 HUD 的步数行
      let y = winH - padPx - 34
          keys = "H123USDMRNP"
      _ <- drawPanel ren art "panel_chip" (rect 8 y 464 28) 10
      forM_ (zip [0 :: CInt ..] keys) $ \(i, ch) ->
        keyChipA ren art (13 + i * 24) (y + 3) ch (V4 255 220 120 255)
      void (zhA ren art "zh_help_more" (13 + 11 * 24 + 4) (y + 6) 16)

-- | 全屏暂停：按键说明（中文）+ 形状图例。
drawPauseHelpArt :: Renderer -> Art -> App -> IO ()
drawPauseHelpArt ren art app
  | not (appPaused app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 8 6 20 200
      fillRect ren (Just (rect 0 0 winW winH))
      let px0 = 40
          py0 = 40
          pw = winW - 80
          ph = winH - 80
      _ <- drawPanel ren art "panel_gold" (rect px0 py0 pw ph) 18
      zhAC ren art "zh_pause" (winW `div` 2) (py0 + 14) 32
      let rows :: [(Char, String, String)]
          rows =
            [ ('H', "zh_k_hint", "HINT"), ('1', "zh_k_hammer", "HAMMER"), ('2', "zh_k_swap", "SWAP")
            , ('3', "zh_k_cross", "CROSS"), ('U', "zh_k_undo", "UNDO"), ('S', "zh_k_shuffle", "SHUFFLE")
            , ('D', "zh_k_daily", "DAILY"), ('M', "zh_k_map", "MAP"), ('R', "zh_k_retry", "RETRY")
            , ('N', "zh_k_next", "NEXT"), ('P', "zh_k_play", "PLAY")
            ]
      forM_ (zip [0 :: CInt ..] rows) $ \(i, (ch, key, en)) -> do
        let yy = py0 + 60 + i * 30
        keyChipA ren art (px0 + 40) yy ch (V4 255 220 120 255)
        _ <- zhA ren art key (px0 + 72) yy 20
        textA ren art (px0 + pw - 40 - textW 3 en) (yy + 2) 3 (V4 190 185 240 255) en
      -- 图例：颜色 × 形状
      let ly = py0 + ph - 62
      _ <- zhA ren art "zh_legend" (px0 + 40) (ly + 8) 20
      forM_ (zip [0 :: CInt ..] allColors) $ \(i, c) ->
        void (drawSprite ren art (gemSprite c) (rect (px0 + 100 + i * 50) ly 40 40))

-- | 结算面板：过关 / 胜利 / 失败 + 星级 + 分数 + 下一步提示。
drawOverlayArt :: Renderer -> Art -> App -> IO ()
drawOverlayArt ren art app = case gsOver (appGame app) of
  Nothing -> pure ()
  Just outcome -> do
    rendererDrawColor ren $= V4 10 8 24 170
    fillRect ren (Just (rect 0 hudH winW (winH - hudH)))
    let pw = 380
        ph = 180
        px0 = (winW - pw) `div` 2
        py0 = hudH + padPx + (boardPx - ph) `div` 2
        cx = winW `div` 2
        stars = starRating (appStartMoves app) (gsMoves (appGame app))
        drawStars = forM_ [0 .. 2 :: Int] $ \i ->
          void (drawSprite ren art (if i < stars then "star_on" else "star_off")
                  (rect (cx - 72 + fromIntegral i * 48) (py0 + 58) 48 48))
        white = V4 255 255 255 255
    _ <- drawPanel ren art "panel_gold" (rect px0 py0 pw ph) 18
    case outcome of
      LevelClear s nextIdx -> do
        zhAC ren art "zh_clear" cx (py0 + 12) 38
        drawStars
        -- 得分：图标 + 金色数字
        let sw = textW 3 (show s)
            sx = cx - (sw + 30) `div` 2
        _ <- drawSprite ren art "icon_score" (rect sx (py0 + 108) 24 24)
        textA ren art (sx + 30) (py0 + 111) 3 (V4 255 220 120 255) (show s)
        let w = zhW art "zh_next" 22
            rowX = cx - (w + 60) `div` 2
        _ <- zhA ren art "zh_next" rowX (py0 + 142) 22
        textA ren art (rowX + w + 6) (py0 + 144) 3 white (show (nextIdx + 1))
        keyChipA ren art (px0 + pw - 40) (py0 + 144) 'N' (V4 140 230 160 255)
      Won s -> do
        zhAC ren art "zh_win" cx (py0 + 12) 38
        drawStars
        _ <- drawSprite ren art "icon_score" (rect (cx - 60) (py0 + 120) 32 32)
        textA ren art (cx - 20) (py0 + 124) 4 white (show s)
      Lost s -> do
        zhAC ren art "zh_lose" cx (py0 + 12) 38
        _ <- drawSprite ren art "icon_score" (rect (cx - 60) (py0 + 64) 32 32)
        textA ren art (cx - 20) (py0 + 68) 4 white (show s)
        zhAC ren art "zh_retry" cx (py0 + 130) 26
      _ -> pure ()

-- | 棋盘中央浮起的「连击 xN」。
drawComboPopArt :: Renderer -> Art -> App -> IO ()
drawComboPopArt ren art app
  | appMapOpen app || appPaused app = pure ()
  | gsCombo (appGame app) <= 1 || appComboShow app <= 0 = pure ()
  | otherwise = do
      let combo = gsCombo (appGame app)
          life = appComboShow app
          yOff = fromIntegral ((120 - min 120 life) `div` 2) :: CInt
          cx = padPx + boardPx `div` 2
          cy = hudH + padPx + boardPx `div` 2 - 40 - yOff
          col
            | combo >= 5 = V4 255 120 220 255
            | combo >= 3 = V4 255 180 60 255
            | otherwise = V4 255 230 110 255
          s = "x" ++ show combo
          w = zhW art "zh_combo" 34 + 12 + textW 5 s
      _ <- drawPanel ren art "panel_gold" (rect (cx - w `div` 2 - 16) (cy - 8) (w + 32) 52) 16
      wz <- zhA ren art "zh_combo" (cx - w `div` 2) (cy + 1) 34
      textA ren art (cx - w `div` 2 + wz + 12) (cy + 3) 5 col s

-- | 贴图版选关地图（节点坐标 / 点击判定与原版一致）。
drawLevelMapArt :: Renderer -> Art -> App -> IO ()
drawLevelMapArt ren art app
  | not (appMapOpen app) = pure ()
  | otherwise = do
      rendererDrawColor ren $= V4 12 10 32 235
      fillRect ren (Just (rect 0 0 winW winH))
      zhAC ren art "zh_map" (winW `div` 2) 16 32
      zhAC ren art "zh_map_hint" (winW `div` 2) 56 18
      forM_ chapterStarts $ \ci -> do
        let (_nx, ny) = mapNodePos ci
        rendererDrawColor ren $= V4 110 100 190 160
        fillRect ren (Just (rect 16 (ny - 36) (winW - 32) 1))
      rendererDrawColor ren $= V4 140 130 220 200
      forM_ [0 .. length allLevels - 2] $ \i -> do
        let (x0, y0) = mapNodePos i
            (x1, y1) = mapNodePos (i + 1)
        forM_ [-1, 0, 1] $ \d -> drawLine ren (P (V2 x0 (y0 + d))) (P (V2 x1 (y1 + d)))
      let reached = appMaxReached app
          cur = gsLevel (appGame app)
      forM_ (zip [0 :: Int ..] allLevels) $ \(i, lvl) -> do
        let (nx, ny) = mapNodePos i
            kind
              | i == cur = "node_cur"
              | i <= reached = "node_done"
              | otherwise = "node_lock"
        when (i == cur) $ do
          let a = round (120 + 120 * breathe (appPulse app) 50) :: Int
          void (drawSpriteAdd ren art "spark" (rect (nx - 34) (ny - 34) 68 68) (V3 255 210 90) (fromIntegral a))
        _ <- drawSprite ren art kind (rect (nx - 20) (ny - 20) 40 40)
        textAC ren art nx (ny - 9) 3 (if i <= reached then V4 255 255 255 255 else V4 170 170 200 255) (show (i + 1))
        void (drawSprite ren art (goalIcon (lvlGoal lvl)) (rect (nx + 10) (ny + 6) 18 18))
      -- 章节标签最后画（小底板），避免被节点遮住
      forM_ (zip [0 :: Int ..] chapterStarts) $ \(k, ci) -> do
        let (nx, ny) = mapNodePos ci
            key = "zh_ch" ++ show (k + 1)
            w = zhW art key 14
            lx = max 8 (nx - (w + 12) `div` 2)
        _ <- drawPanel ren art "panel_gold" (rect lx (ny - 45) (w + 12) 20) 7
        void (zhA ren art key (lx + 6) (ny - 42) 14)
