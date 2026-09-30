-- | 桌面音效（只在 UI 层播）。引擎不发声。
-- 素材在 assets/sfx/*.wav（单声道 16-bit）。
-- 音效与 BGM 各自开关，默认开；偏好分别写在 ~/.config/match3/sfx 与 ~/.config/match3/bgm。
module UI.Audio
  ( start
  , cue
  , beginLevel
  , toggleSfx
  , toggleBgm
  , sfxEnabled
  , bgmEnabled
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Int (Int16)
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import SDL.Audio
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory (..))
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

data Clip = Clip !(V.Vector Int16) !Int

data Mixer = Mixer
  { mxClips :: !(Map String (V.Vector Int16))
  , mxBgm :: !(V.Vector Int16)
  , mxBgmAt :: !(IORef Int)
  , mxVoices :: !(IORef [Clip])
  , mxSfxOn :: !(IORef Bool)
  , mxBgmOn :: !(IORef Bool)
  , mxBgmGo :: !(IORef Bool)
  , mxDev :: !(Maybe AudioDevice)
  }

mixerRef :: IORef (Maybe Mixer)
mixerRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE mixerRef #-}

start :: IO ()
start = do
  clips <- loadClips
  sfxOn <- readPref "sfx"
  bgmOn <- readPref "bgm"
  bgmAt <- newIORef 0
  voices <- newIORef []
  sfxRef <- newIORef sfxOn
  bgmRef <- newIORef bgmOn
  bgmGo <- newIORef True
  let bgm = M.findWithDefault V.empty "bgm" clips
      oneshots = M.delete "bgm" clips
  dev <- openDev bgmRef bgmGo bgmAt voices bgm
  writeIORef mixerRef $ Just Mixer
    { mxClips = oneshots
    , mxBgm = bgm
    , mxBgmAt = bgmAt
    , mxVoices = voices
    , mxSfxOn = sfxRef
    , mxBgmOn = bgmRef
    , mxBgmGo = bgmGo
    , mxDev = dev
    }

cue :: [String] -> IO ()
cue names = do
  m <- readIORef mixerRef
  case m of
    Nothing -> pure ()
    Just mx -> do
      when (any (`elem` ["win", "lose"]) names) $ writeIORef (mxBgmGo mx) False
      on <- readIORef (mxSfxOn mx)
      when on $ do
        let vs = [Clip c 0 | n <- names, Just c <- [M.lookup n (mxClips mx)]]
        unless (null vs) $ modifyIORef' (mxVoices mx) (take 8 . (++ vs))

-- | 进关：重新循环 BGM（缺文件时无声，不抛）。是否真正出声看 BGM 开关。
beginLevel :: IO ()
beginLevel = do
  m <- readIORef mixerRef
  case m of
    Nothing -> pure ()
    Just mx -> do
      writeIORef (mxBgmGo mx) True
      writeIORef (mxBgmAt mx) 0

toggleSfx :: IO Bool
toggleSfx = do
  m <- readIORef mixerRef
  case m of
    Nothing -> pure True
    Just mx -> do
      on <- atomicModifyIORef' (mxSfxOn mx) (\b -> let b' = not b in (b', b'))
      unless on $ writeIORef (mxVoices mx) []
      writePref "sfx" on
      pure on

toggleBgm :: IO Bool
toggleBgm = do
  m <- readIORef mixerRef
  case m of
    Nothing -> pure True
    Just mx -> do
      on <- atomicModifyIORef' (mxBgmOn mx) (\b -> let b' = not b in (b', b'))
      writePref "bgm" on
      -- 开回 BGM：本关尚未胜负则从头再循环；已胜负（mxBgmGo 已关）保持停。
      when on $ do
        go <- readIORef (mxBgmGo mx)
        when go $ writeIORef (mxBgmAt mx) 0
      pure on

sfxEnabled :: IO Bool
sfxEnabled = do
  m <- readIORef mixerRef
  case m of
    Nothing -> pure True
    Just mx -> readIORef (mxSfxOn mx)

bgmEnabled :: IO Bool
bgmEnabled = do
  m <- readIORef mixerRef
  case m of
    Nothing -> pure True
    Just mx -> readIORef (mxBgmOn mx)

openDev :: IORef Bool -> IORef Bool -> IORef Int -> IORef [Clip] -> V.Vector Int16 -> IO (Maybe AudioDevice)
openDev bgmOn bgmGo bgmAt voices bgm = do
  r <- try (openAudioDevice OpenDeviceSpec
    { openDeviceFreq = Desire 22050
    , openDeviceFormat = Mandate Signed16BitNativeAudio
    , openDeviceChannels = Desire Mono
    , openDeviceSamples = 1024
    -- 格式已 Mandate 为 16-bit，回调里的样本向量就是 Int16。
    , openDeviceCallback = \_ vec -> fill bgmOn bgmGo bgmAt voices bgm (unsafeCoerce vec)
    , openDeviceUsage = ForPlayback
    , openDeviceName = Nothing
    }) :: IO (Either SomeException (AudioDevice, AudioSpec))
  case r of
    Left e -> do
      hPutStrLn stderr ("audio disabled: " ++ show e)
      pure Nothing
    Right (dev, _) -> do
      setAudioDevicePlaybackState dev Play
      pure (Just dev)

fill :: IORef Bool -> IORef Bool -> IORef Int -> IORef [Clip] -> V.Vector Int16 -> VM.IOVector Int16 -> IO ()
fill bgmOn bgmGo bgmAt voices bgm vec = do
  let n = VM.length vec
  pos0 <- readIORef bgmAt
  vs0 <- readIORef voices
  go <- readIORef bgmGo
  onB <- readIORef bgmOn
  let blen = V.length bgm
      playB = onB && go && blen > 0
      step i pos vs
        | i >= n = pure (pos, vs)
        | otherwise = do
            let sampleB = if not playB then 0 else fromIntegral (V.unsafeIndex bgm (pos `mod` blen))
                (sampleS, vs') = foldVoices vs
            VM.unsafeWrite vec i (clamp (sampleB + sampleS))
            step (i + 1) (pos + 1) vs'
  (pos1, vs1) <- step 0 pos0 vs0
  writeIORef bgmAt pos1
  writeIORef voices vs1

foldVoices :: [Clip] -> (Int, [Clip])
foldVoices [] = (0, [])
foldVoices (Clip v p : rest) =
  let (s, rest') = foldVoices rest
      (here, clip') =
        if p >= V.length v then (0, Nothing)
        else (fromIntegral (V.unsafeIndex v p) :: Int, Just (Clip v (p + 1)))
  in (s + here, maybe rest' (: rest') clip')

clamp :: Int -> Int16
clamp x = fromIntegral (max (-32767) (min 32767 x))

loadClips :: IO (Map String (V.Vector Int16))
loadClips = do
  dirs <- assetDirs
  let names = ["swap", "clear", "special", "illegal", "win", "lose", "bgm"]
  ms <- mapM (\n -> findWav dirs n) names
  pure $ M.fromList [(n, v) | (n, Just v) <- zip names ms]

findWav :: [FilePath] -> String -> IO (Maybe (V.Vector Int16))
findWav dirs name = go dirs
  where
    go [] = pure Nothing
    go (d : ds) = do
      let p = d </> "sfx" </> name ++ ".wav"
      ok <- doesFileExist p
      if ok then Just <$> readWav p else go ds

readWav :: FilePath -> IO (V.Vector Int16)
readWav path = do
  bs <- BS.readFile path
  let rest = dropData bs
      n = BS.length rest `div` 2
      ix i = let b0 = BS.index rest (2 * i); b1 = BS.index rest (2 * i + 1) in word16 b0 b1
  pure $ V.generate n ix

word16 :: Word8 -> Word8 -> Int16
word16 lo hi = fromIntegral (fromIntegral lo + fromIntegral hi * 256 :: Int)

-- 跳到 data 块的样本（标准小端 PCM）。
dropData :: BS.ByteString -> BS.ByteString
dropData bs = case BS.breakSubstring (BS.pack [100, 97, 116, 97]) bs of
  (_, rest) | BS.length rest >= 8 -> BS.drop 8 rest
  _ -> BS.drop 44 bs

assetDirs :: IO [FilePath]
assetDirs = do
  env <- lookupEnv "MATCH3_ASSETS"
  exe <- try getExecutablePath :: IO (Either SomeException FilePath)
  let exeDirs = case exe of
        Left _ -> []
        Right p -> take 4 $ iterate takeDirectory (takeDirectory p)
  pure $ maybe [] pure env ++ ["assets"] ++ map (</> "assets") exeDirs

prefPath :: String -> IO FilePath
prefPath name = do
  dir <- getXdgDirectory XdgConfig "match3"
  createDirectoryIfMissing True dir
  pure (dir </> name)

-- | 读偏好：缺文件 = 开。旧版总开关 ~/.config/match3/sound 在新文件缺失时借用一次。
readPref :: String -> IO Bool
readPref name = do
  p <- prefPath name
  ok <- doesFileExist p
  if ok then readOnOff p
  else do
    legacy <- prefPath "sound"
    legOk <- doesFileExist legacy
    if legOk then readOnOff legacy else pure True

readOnOff :: FilePath -> IO Bool
readOnOff p = do
  t <- readFile p
  pure (t /= "off\n" && t /= "off")

writePref :: String -> Bool -> IO ()
writePref name on = do
  p <- prefPath name
  writeFile p (if on then "on\n" else "off\n")
