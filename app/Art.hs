-- | 贴图资源层（仅前端）：加载 assets/atlas*.bmp + atlas.txt 与 background.bmp。
-- 只用 SDL2 核心的 SDL_LoadBMP（32 位 BMP 带 alpha），Mac 无需 sdl2_image。
-- 目录查找顺序：$MATCH3_ASSETS → ./assets → 可执行文件所在目录逐级向上的 assets/。
-- 任一步失败都返回 Nothing（打印一行警告），前端据此回退到原有矩形绘制，不会崩溃。
--
-- 高分屏：图集可分多页（atlas.bmp = 第 0 页，atlasN.bmp = 第 N 页）；同一贴图可有多个
-- 尺寸变体 `基名@像素高`。绘制时按「目标逻辑高度 × artScale（物理像素 / 逻辑像素）」
-- 挑最小的够用变体，这样 Retina 上文字 / 图标尽量 1:1 映射到物理像素。
module Art
  ( Art (..)
  , Sprite (..)
  , loadArt
  , hasSprite
  , spriteSize
  , spriteSizeAt
  , drawSprite
  , drawSpriteMod
  , drawSpriteAdd
  , drawSpriteEx
  , drawPanel
  , drawPanelMod
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, unless, when)
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import SDL
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | 图集中的一张子图：所在页的纹理 + 子矩形。
data Sprite = Sprite
  { sprTex  :: Texture
  , sprRect :: Rectangle CInt
  }

-- | 已加载的贴图集：若干页纹理 + 名字索引 + 尺寸变体；背景可缺省。
data Art = Art
  { artPages :: [Texture]
  , artRects :: M.Map String Sprite    -- 全名（含 `@变体`）→ 子图
  , artMips  :: M.Map String [Sprite]  -- 基名 → 全部尺寸变体（含基名本身），按高度升序
  , artBg    :: Maybe Texture
  , artDir   :: FilePath
  , artScale :: Float                  -- 物理像素 / 逻辑像素（1 普通屏，2 Retina）；由 Main 每帧同步
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

-- | 第 n 页图集的文件名。
pageFile :: Int -> FilePath
pageFile 0 = "atlas.bmp"
pageFile n = "atlas" ++ show n ++ ".bmp"

loadFrom :: Renderer -> FilePath -> IO Art
loadFrom ren dir = do
  idx <- readFile (dir </> "atlas.txt")
  entries <- evaluate (parseIndex idx)
  when (null entries) $ ioError (userError "empty atlas.txt")
  let nPages = 1 + foldr max 0 [pg | (_, _, pg) <- entries]
  -- 任何一页缺失都算整体失败（交给调用方回退），避免半套贴图；先检查文件再建纹理
  forM_ [0 .. nPages - 1] $ \i -> do
    ok <- doesFileExist (dir </> pageFile i)
    unless ok $ ioError (userError ("missing atlas page " ++ pageFile i))
  pages <- forM [0 .. nPages - 1] $ \i -> loadTexture ren (dir </> pageFile i)
  let pageAt = M.fromList (zip [0 ..] pages)
      rects = M.fromList [(name, Sprite tex r) | (name, r, pg) <- entries, Just tex <- [M.lookup pg pageAt]]
      mips =
        M.map (sortOn sprH) $
          M.fromListWith (++) [(baseName name, [s]) | (name, s) <- M.toList rects]
  bgE <- try (loadTexture ren (dir </> "background.bmp")) :: IO (Either SomeException Texture)
  pure
    Art
      { artPages = pages
      , artRects = rects
      , artMips = mips
      , artBg = either (const Nothing) Just bgE
      , artDir = dir
      , artScale = 1
      }

-- | `zh_combo@68` → `zh_combo`。
baseName :: String -> String
baseName = takeWhile (/= '@')

sprH :: Sprite -> CInt
sprH (Sprite _ (Rectangle _ (V2 _ h))) = h

-- | BMP → 纹理（开启 alpha 混合）。线性过滤由 Main 在建纹理前设置的 HintRenderScaleQuality 决定。
loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture ren path = do
  surf <- loadBMP path
  tex <- createTextureFromSurface ren surf
  freeSurface surf
  textureBlendMode tex $= BlendAlphaBlend
  pure tex

-- | 解析索引行 `name x y w h [page]`（旧格式无页号 = 第 0 页）；# 开头为注释，坏行忽略。
parseIndex :: String -> [(String, Rectangle CInt, Int)]
parseIndex txt =
  [ (name, Rectangle (P (V2 x y)) (V2 w h), pg)
  | ln <- lines txt
  , take 1 ln /= "#"
  , (name : rest) <- [words ln]
  , Just (x, y, w, h, pg) <- [fields rest]
  , pg >= 0
  ]
  where
    fields [a, b, c, d] = (\x y w h -> (x, y, w, h, 0)) <$> readMaybe a <*> readMaybe b <*> readMaybe c <*> readMaybe d
    fields [a, b, c, d, e] = (,,,,) <$> readMaybe a <*> readMaybe b <*> readMaybe c <*> readMaybe d <*> readMaybe e
    fields _ = Nothing

hasSprite :: Art -> String -> Bool
hasSprite art name = M.member name (artRects art)

-- | 按「逻辑高度 want」挑变体：目标物理高度 = want × artScale，取最小的 ≥ 目标的变体，
-- 都不够就取最大的。没有变体表时按全名精确查找。
pickSprite :: Art -> String -> CInt -> Maybe Sprite
pickSprite art name want = case M.lookup name (artMips art) of
  Just (v0 : vs') ->
    let vs = v0 :| vs'
        target = ceiling (fromIntegral want * artScale art - 0.01 :: Float)
    in case NE.filter ((>= target) . sprH) vs of
         (v : _) -> Just v
         [] -> Just (NE.last vs)
  _ -> M.lookup name (artRects art)

-- | 基名贴图的原始尺寸（像素）。
spriteSize :: Art -> String -> Maybe (CInt, CInt)
spriteSize art name = (\(Sprite _ (Rectangle _ (V2 w h))) -> (w, h)) <$> M.lookup name (artRects art)

-- | 以逻辑高度 h 绘制时实际选用的变体尺寸（像素），用于按比例算宽度。
spriteSizeAt :: Art -> String -> CInt -> Maybe (CInt, CInt)
spriteSizeAt art name h = (\(Sprite _ (Rectangle _ (V2 w0 h0))) -> (w0, h0)) <$> pickSprite art name h

dstH :: Rectangle CInt -> CInt
dstH (Rectangle _ (V2 _ h)) = h

-- | 绘制子图到目标矩形（逻辑坐标）；缺图返回 False 以便调用方回退。
drawSprite :: Renderer -> Art -> String -> Rectangle CInt -> IO Bool
drawSprite ren art name dst = case pickSprite art name (dstH dst) of
  Nothing -> pure False
  Just (Sprite tex src) -> do
    copy ren tex (Just src) (Just dst)
    pure True

-- | 带着色 / 透明度的绘制（用后恢复默认调制）。
drawSpriteMod :: Renderer -> Art -> String -> Rectangle CInt -> V3 Word8 -> Word8 -> IO Bool
drawSpriteMod ren art name dst tint alpha = case pickSprite art name (dstH dst) of
  Nothing -> pure False
  Just (Sprite tex src) -> do
    withMod tex tint alpha (copy ren tex (Just src) (Just dst))
    pure True

withMod :: Texture -> V3 Word8 -> Word8 -> IO () -> IO ()
withMod tex tint alpha act = do
  textureColorMod tex $= tint
  textureAlphaMod tex $= alpha
  act
  textureColorMod tex $= V3 255 255 255
  textureAlphaMod tex $= 255

-- | 叠加混合（发光 / 闪白）。
drawSpriteAdd :: Renderer -> Art -> String -> Rectangle CInt -> V3 Word8 -> Word8 -> IO Bool
drawSpriteAdd ren art name dst tint alpha = case pickSprite art name (dstH dst) of
  Nothing -> pure False
  Just (Sprite tex src) -> do
    textureBlendMode tex $= BlendAdditive
    withMod tex tint alpha (copy ren tex (Just src) (Just dst))
    textureBlendMode tex $= BlendAlphaBlend
    pure True

-- | 旋转 / 水平翻转绘制（蜗牛朝向、传送带方向）。
drawSpriteEx :: Renderer -> Art -> String -> Rectangle CInt -> CDouble -> Bool -> IO Bool
drawSpriteEx ren art name dst ang flipH = case pickSprite art name (dstH dst) of
  Nothing -> pure False
  Just (Sprite tex src) -> do
    copyEx ren tex (Just src) (Just dst) ang Nothing (V2 flipH False)
    pure True

-- | 九宫格面板：源图四角各取 1/4 边长，目标角半径 c；中间拉伸。
-- 按角半径挑变体：源角 (高/4) ≥ c × artScale 的最小变体（小角用 @80，大角用 144）。
drawPanel :: Renderer -> Art -> String -> Rectangle CInt -> CInt -> IO Bool
drawPanel ren art name dst c = drawPanelMod ren art name dst c (V3 255 255 255)

-- | 着色九宫格（进度条填充）；tint = 白色即不着色。
drawPanelMod :: Renderer -> Art -> String -> Rectangle CInt -> CInt -> V3 Word8 -> IO Bool
drawPanelMod ren art name (Rectangle (P (V2 x y)) (V2 w h)) c tint = case pickSprite art name (4 * c) of
  Nothing -> pure False
  Just (Sprite tex (Rectangle (P (V2 sx sy)) (V2 sw sh))) -> do
    let k = sw `div` 4
        c' = max 1 (min c (min (w `div` 2) (h `div` 2)))
        srcX = [sx, sx + k, sx + sw - k]
        srcW = [k, sw - 2 * k, k]
        srcY = [sy, sy + k, sy + sh - k]
        srcH = [k, sh - 2 * k, k]
        dstX = [x, x + c', x + w - c']
        dstW = [c', w - 2 * c', c']
        dstY = [y, y + c', y + h - c']
        dstHs = [c', h - 2 * c', c']
    textureColorMod tex $= tint
    forM_ (zip3 srcY srcH (zip dstY dstHs)) $ \(syj, shj, (dyj, dhj)) ->
      forM_ (zip3 srcX srcW (zip dstX dstW)) $ \(sxi, swi, (dxi, dwi)) ->
        unless (dwi <= 0 || dhj <= 0) $
          copy ren tex
            (Just (Rectangle (P (V2 sxi syj)) (V2 swi shj)))
            (Just (Rectangle (P (V2 dxi dyj)) (V2 dwi dhj)))
    textureColorMod tex $= V3 255 255 255
    pure True
