-- | 贴图资源层（仅前端）：加载 assets/atlas.bmp + atlas.txt 与 background.bmp。
-- 只用 SDL2 核心的 SDL_LoadBMP（32 位 BMP 带 alpha），Mac 无需 sdl2_image。
-- 目录查找顺序：$MATCH3_ASSETS → ./assets → 可执行文件所在目录逐级向上的 assets/。
-- 任一步失败都返回 Nothing（打印一行警告），前端据此回退到原有矩形绘制，不会崩溃。
module Art
  ( Art (..)
  , loadArt
  , hasSprite
  , spriteSize
  , drawSprite
  , drawSpriteMod
  , drawSpriteAdd
  , drawSpriteEx
  , drawPanel
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import SDL
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | 已加载的贴图集：一张大纹理 + 名字到子矩形的索引；背景可缺省。
data Art = Art
  { artTex   :: Texture
  , artRects :: M.Map String (Rectangle CInt)
  , artBg    :: Maybe Texture
  , artDir   :: FilePath
  }

-- | 候选资源目录（去重，保持优先级）。
candidateDirs :: IO [FilePath]
candidateDirs = do
  env <- lookupEnv "MATCH3_ASSETS"
  exeE <- try getExecutablePath :: IO (Either SomeException FilePath)
  let exeDirs = case exeE of
        Right p -> take 12 (iterate takeDirectory (takeDirectory p))
        Left _ -> []
  pure $ nub $ maybe [] pure env ++ ["assets"] ++ map (</> "assets") exeDirs

-- | 找到第一个同时含 atlas.bmp 与 atlas.txt 的目录并加载。
loadArt :: Renderer -> IO (Maybe Art)
loadArt ren = do
  dirs <- candidateDirs
  found <- firstM hasAtlas dirs
  case found of
    Nothing -> do
      warn "assets/atlas.bmp not found (set MATCH3_ASSETS); using primitive rendering"
      pure Nothing
    Just dir -> do
      r <- try (loadFrom ren dir) :: IO (Either SomeException Art)
      case r of
        Left e -> do
          warn ("failed to load assets from " ++ dir ++ ": " ++ show e ++ "; using primitive rendering")
          pure Nothing
        Right a -> pure (Just a)
  where
    hasAtlas d = (&&) <$> doesFileExist (d </> "atlas.bmp") <*> doesFileExist (d </> "atlas.txt")

firstM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
firstM _ [] = pure Nothing
firstM p (x : xs) = do
  ok <- p x
  if ok then pure (Just x) else firstM p xs

warn :: String -> IO ()
warn msg = hPutStrLn stderr ("match3-sdl: " ++ msg)

loadFrom :: Renderer -> FilePath -> IO Art
loadFrom ren dir = do
  idx <- readFile (dir </> "atlas.txt")
  rects <- evaluate (M.fromList (parseIndex idx))
  unless (M.size rects > 0) $ ioError (userError "empty atlas.txt")
  tex <- loadTexture ren (dir </> "atlas.bmp")
  bgE <- try (loadTexture ren (dir </> "background.bmp")) :: IO (Either SomeException Texture)
  pure Art {artTex = tex, artRects = rects, artBg = either (const Nothing) Just bgE, artDir = dir}

-- | BMP → 纹理（开启 alpha 混合）。
loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture ren path = do
  surf <- loadBMP path
  tex <- createTextureFromSurface ren surf
  freeSurface surf
  textureBlendMode tex $= BlendAlphaBlend
  pure tex

-- | 解析索引行 `name x y w h`；# 开头为注释，坏行忽略。
parseIndex :: String -> [(String, Rectangle CInt)]
parseIndex txt =
  [ (name, Rectangle (P (V2 x y)) (V2 w h))
  | ln <- lines txt
  , take 1 ln /= "#"
  , (name : rest) <- [words ln]
  , Just [x, y, w, h] <- [mapM readMaybe rest]
  ]

hasSprite :: Art -> String -> Bool
hasSprite art name = M.member name (artRects art)

-- | 贴图原始尺寸（像素）。
spriteSize :: Art -> String -> Maybe (CInt, CInt)
spriteSize art name = (\(Rectangle _ (V2 w h)) -> (w, h)) <$> M.lookup name (artRects art)

-- | 绘制子图到目标矩形；缺图返回 False 以便调用方回退。
drawSprite :: Renderer -> Art -> String -> Rectangle CInt -> IO Bool
drawSprite ren art name dst = case M.lookup name (artRects art) of
  Nothing -> pure False
  Just src -> do
    copy ren (artTex art) (Just src) (Just dst)
    pure True

-- | 带着色 / 透明度的绘制（用后恢复默认调制）。
drawSpriteMod :: Renderer -> Art -> String -> Rectangle CInt -> V3 Word8 -> Word8 -> IO Bool
drawSpriteMod ren art name dst tint alpha = case M.lookup name (artRects art) of
  Nothing -> pure False
  Just src -> do
    let tex = artTex art
    textureColorMod tex $= tint
    textureAlphaMod tex $= alpha
    copy ren tex (Just src) (Just dst)
    textureColorMod tex $= V3 255 255 255
    textureAlphaMod tex $= 255
    pure True

-- | 叠加混合（发光 / 闪白）。
drawSpriteAdd :: Renderer -> Art -> String -> Rectangle CInt -> V3 Word8 -> Word8 -> IO Bool
drawSpriteAdd ren art name dst tint alpha = do
  let tex = artTex art
  textureBlendMode tex $= BlendAdditive
  ok <- drawSpriteMod ren art name dst tint alpha
  textureBlendMode tex $= BlendAlphaBlend
  pure ok

-- | 旋转 / 水平翻转绘制（蜗牛朝向、传送带方向）。
drawSpriteEx :: Renderer -> Art -> String -> Rectangle CInt -> CDouble -> Bool -> IO Bool
drawSpriteEx ren art name dst ang flipH = case M.lookup name (artRects art) of
  Nothing -> pure False
  Just src -> do
    copyEx ren (artTex art) (Just src) (Just dst) ang Nothing (V2 flipH False)
    pure True

-- | 九宫格面板：源图四角各取 1/4 边长，目标角半径 c；中间拉伸。
drawPanel :: Renderer -> Art -> String -> Rectangle CInt -> CInt -> IO Bool
drawPanel ren art name (Rectangle (P (V2 x y)) (V2 w h)) c = case M.lookup name (artRects art) of
  Nothing -> pure False
  Just (Rectangle (P (V2 sx sy)) (V2 sw sh)) -> do
    let k = sw `div` 4
        c' = max 1 (min c (min (w `div` 2) (h `div` 2)))
        srcX = [sx, sx + k, sx + sw - k]
        srcW = [k, sw - 2 * k, k]
        srcY = [sy, sy + k, sy + sh - k]
        srcH = [k, sh - 2 * k, k]
        dstX = [x, x + c', x + w - c']
        dstW = [c', w - 2 * c', c']
        dstY = [y, y + c', y + h - c']
        dstH = [c', h - 2 * c', c']
    forM_ [0 .. 2 :: Int] $ \j ->
      forM_ [0 .. 2 :: Int] $ \i ->
        unless ((dstW !! i) <= 0 || (dstH !! j) <= 0) $
          copy ren (artTex art)
            (Just (Rectangle (P (V2 (srcX !! i) (srcY !! j))) (V2 (srcW !! i) (srcH !! j))))
            (Just (Rectangle (P (V2 (dstX !! i) (dstY !! j))) (V2 (dstW !! i) (dstH !! j))))
    pure True
