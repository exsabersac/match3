-- 逐轮回放的一致性脚本（原生一侧）：同一关卡 + 种子，按核心提示连走 N 步；每步之后建立回放
-- （Match3Web.Anim，即桌面 ComboFx 阶段机）并逐帧推进到播完，打印开播 JSON、每帧 JSON 与
-- 「frames=帧数 events=事件数」。每第 3 步从第 5 帧起加速（覆盖 fast 路径）。
-- 与 node-anim-parity.mjs（wasm 一侧）的输出逐字节比较；另外在原生一侧核对：不加速的步，
-- 逐帧循环的帧数 / 事件数与 Engine.Playback.runPlayer 一口气播完的结果相同（不一致则退出码 1）。
-- 用法（仓库根目录）：stack exec -- runghc -isrc -iapp/pure -iweb/hs web/test/AnimParity.hs 12 42 20 [hint|combo|combo-bomb|cham-rainbow]
-- 第 4 个参数是走法（见 pickMove）：combo 先换盘上的「彩虹 × 直线 / 炸弹」，覆盖第 44 关的变身步。
module Main (main) where

import Control.Monad (unless, when)
import Data.IORef
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Match3.Core
import Match3.Element.Builtin (chameleonColor)
import Match3Web.Anim (animRunPlayer, animStart, animTick)
import Match3Web.Api (apiNew, apiSwapAnim, webState)

main :: IO ()
main = do
  args <- getArgs
  let (li, seed, n) = case map read (take 3 args) of
        [a, b, c] -> (a, b, c)
        _ -> error "用法：AnimParity 关卡 种子 步数 [hint|combo|combo-bomb|cham-rainbow]"
      mode = case drop 3 args of
        (m : _) -> m
        [] -> "hint"
  bad <- newIORef (0 :: Int)
  checked <- newIORef (0 :: Int)
  let (h0, _) = apiNew li seed
      go k h
        | k >= (n :: Int) = pure ()
        | otherwise = do
            let gs = webState h
            case (gsOver gs, findHint (gsBoard gs)) of
              (Nothing, Just hint) -> do
                let (a, b) = pickMove mode (gsBoard gs) hint
                    (h', ms, _) = apiSwapAnim a b h
                when (mode == "cham-rainbow" && (a, b) `elem` chamRainbowPairs (gsBoard gs)) $
                  hPutStrLn stderr ("走法 cham-rainbow：第 " ++ show k ++ " 步换彩虹 × 变色龙 " ++ show (a, b))
                putStrLn ("step " ++ show k)
                case ms of
                  Nothing -> putStrLn "noanim"
                  Just s -> do
                    let (p0, j0) = animStart s
                        fastStep = k `mod` 3 == 2
                        loop fr evs p = do
                          let (mp, ne, j) = animTick (fastStep && fr >= 5) s p
                          putStrLn j
                          case mp of
                            Just p' -> loop (fr + 1) (evs + ne) p'
                            Nothing -> pure (fr + 1, evs + ne)
                    putStrLn j0
                    (frames, evs) <- loop (0 :: Int) (0 :: Int) p0
                    putStrLn ("frames=" ++ show frames ++ " events=" ++ show evs)
                    unless fastStep $ do
                      modifyIORef' checked (+ 1)
                      let (rf, re) = animRunPlayer s
                      when ((rf, re) /= (frames, evs)) $ do
                        modifyIORef' bad (+ 1)
                        hPutStrLn stderr ("MISMATCH step " ++ show k ++ ": loop " ++ show (frames, evs) ++ " runPlayer " ++ show (rf, re))
                go (k + 1) h'
              _ -> pure ()
  go 0 h0
  b <- readIORef bad
  c <- readIORef checked
  hPutStrLn stderr ("native: runPlayer 核对 " ++ show c ++ " 步，不一致 " ++ show b)
  when (b > 0) exitFailure

-- | 走法（第 4 个参数，缺省 hint）：hint = 按核心提示；combo = 盘上有「彩虹 × 直线 / 炸弹」相邻（两格都无冰、无叠层）时
-- 先换这一对（行优先，先右后下），否则按提示——提示不会主动选彩虹组合，第 44 关（rainbow_combos）要靠它覆盖变身步；
-- combo-bomb 同 combo，但先换「彩虹 × 炸弹」（覆盖 rainbow_bomb 变身）；cham-rainbow 先换「彩虹 × 变色龙」（第 47 关）。与 node-parity.mjs / node-anim-parity.mjs 的 pickMove 逐条相同。
pickMove :: String -> Board -> (Pos, Pos) -> (Pos, Pos)
pickMove mode b hint = case ordered of
  (pr : _) -> pr
  [] -> hint
  where
    ordered
      | mode == "combo" = comboPairs
      | mode == "combo-bomb" = filter hasBomb comboPairs ++ filter (not . hasBomb) comboPairs
      | mode == "cham-rainbow" = chamRainbowPairs b
      | otherwise = []
    hasBomb (p, q) = any isBomb [getCell b p, getCell b q]
    isBomb cell = case cell of
      Gem _ Bomb _ _ -> True
      _ -> False
    comboPairs =
      [ (p, q)
      | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]
      , let p = (r, c)
      , q <- [(r, c + 1), (r + 1, c)]
      , inBounds q
      , let (x, y) = (getCell b p, getCell b q)
      , (rainbow x && special y) || (special x && rainbow y)
      ]
    rainbow cell = case cell of
      Gem _ Rainbow 0 Nothing -> True
      _ -> False
    special cell = case cell of
      Gem _ k 0 Nothing -> k `elem` [LineH, LineV, Bomb]
      _ -> False

-- | 变色龙 × 彩虹（第 47 关，成对交换规则 15）：盘上相邻的「彩虹（无冰无叠层）× 变色龙」，行优先、先右后下。
-- 走法 cham-rainbow 时先换第一对（提示不会主动选它），与 node 两侧的 chamRainbowPairs 逐条相同。
chamRainbowPairs :: Board -> [(Pos, Pos)]
chamRainbowPairs b =
  [ (p, q)
  | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]
  , let p = (r, c)
  , q <- [(r, c + 1), (r + 1, c)]
  , inBounds q
  , let (x, y) = (getCell b p, getCell b q)
  , (rainbow x && cham y) || (cham x && rainbow y)
  ]
  where
    rainbow cell = case cell of
      Gem _ Rainbow 0 Nothing -> True
      _ -> False
    cham cell = chameleonColor cell /= Nothing
