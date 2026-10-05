-- 原生（GHC 9.14.1 / lts-24.60 + compiler 覆盖）一侧的一致性脚本：
-- 同一关卡 + 种子开局，按核心提示连走 N 步，逐步打印接口 JSON。
-- 与 node-parity.mjs（wasm 一侧）的输出逐字节比较，验证两端规则与随机数完全一致。
-- 走完后再撤销一步（覆盖 m3Undo / Engine.History），最后一行是撤销结果。
-- 用法（仓库根目录）：stack runghc -- -isrc -iweb/hs web/test/Parity.hs 0 20260929 12 [hint|combo|combo-bomb|cham-rainbow|fix-RCRC-…|boost|daily|advance]
-- 桌面迁来的接口（web-sdl-parity）：boost = 第 0 步锤子打提示的第一格、第 1 步十字消打提示的第二格、第 2 步自由交换提示的两格、
-- 第 3 步手动洗牌，之后按提示；daily = 开 2026-09-29 的每日挑战（apiDaily，不用关卡 / 种子参数）后按提示；
-- advance = 按提示一直走到结局（不受步数限制、最后不撤销）。这三种走完（撤销）后再打印 extras（进度 / 徽章 / 地图点选 / 前进 / 重开 / 展示盘）。
module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Char (digitToInt)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Match3.Board.Default (findHint)
import Match3.Board.Grid (inBounds)
import Match3.Core
import Match3.Element.Event (Event(..), EventKind(..))
import Match3.Engine (Action(..), Played(..), play)
import Match3.Game.State (gsGround)
import Match3Web.Anim (AnimSeed)
import Match3Web.Api
  ( WebGame, apiAdvance, apiBadge, apiCross, apiDaily, apiFreeSwap, apiHammer, apiMapJump, apiNew, apiProgress
  , apiRestart, apiShowcase, apiShuffle, apiSwap, apiUndo, webState )

main :: IO ()
main = do
  args <- getArgs
  let (li, seed, n) = case map read (take 3 args) of
        [a, b, c] -> (a, b, c)
        _ -> error "用法：Parity 关卡 种子 步数 [hint|combo|combo-bomb|cham-rainbow|fix-RCRC-…]"
      mode = case drop 3 args of
        (m : _) -> m
        [] -> "hint"
      (gs0, j0) = if mode == "daily" then apiDaily 2026 9 29 else apiNew li seed
      m0 = gsMoves (webState gs0)
      -- advance 走法停在结局上（不撤销），好让 extras 里的前进真的生效
      undoLast h
        | mode == "advance" = pure h
        | otherwise = let (h', j) = apiUndo h in h' <$ putStrLn j
      go :: Int -> WebGame -> IO WebGame
      go 0 h = undoLast h
      go k h = let gs = webState h in case (gsOver gs, findHint (gsBoard gs)) of
        (Nothing, Just hint) | Just f <- boostStep mode (n - k) hint -> do
          let (h', _, j) = f h
          putStrLn j
          go (k - 1) h'
        (Nothing, Just hint) -> do
          let (a, b) = fromMaybe (pickMove mode (gsBoard gs) hint) (fixedMove mode (n - k))
              (h', j) = apiSwap a b h
          when (mode == "cham-rainbow" && (a, b) `elem` chamRainbowPairs (gsBoard gs)) $
            hPutStrLn stderr ("走法 cham-rainbow：第 " ++ show (n - k) ++ " 步换彩虹 × 变色龙 " ++ show (a, b))
          when (take 4 mode == "fix-") $
            mapM_ (\l -> hPutStrLn stderr ("走法 fix：第 " ++ show (n - k) ++ " 步魔法地格扩爆 " ++ l)) (magicBlasts gs a b)
          putStrLn j
          go (k - 1) h'
        _ -> undoLast h
  putStrLn j0
  hEnd <- go (if mode == "advance" then 400 else n) gs0
  when (mode `elem` ["boost", "daily", "advance"]) $ mapM_ putStrLn (extras m0 hEnd)

-- | boost 走法：前四步依次用锤子 / 十字消 / 自由交换 / 洗牌（位置取当步的核心提示），之后 Nothing（按提示交换）。
-- 与 node-parity.mjs 的 boostStep 逐条相同。
boostStep :: String -> Int -> (Pos, Pos) -> Maybe (WebGame -> (WebGame, Maybe AnimSeed, String))
boostStep "boost" k (a, b) = case k of
  0 -> Just (apiHammer a)
  1 -> Just (apiCross b)
  2 -> Just (apiFreeSwap a b)
  3 -> Just apiShuffle
  _ -> Nothing
boostStep _ _ _ = Nothing

-- | 走完后的只读查询与换局接口（boost / daily / advance）：与 node-parity.mjs 的 extras 同序同参数；
-- 前进 → 重开 → 展示盘依次作用在上一步的结果上。
extras :: Int -> WebGame -> [String]
extras m0 h =
  [ apiProgress 0 m0 h, apiProgress 40 m0 h
  , apiBadge 0 0 0 0 0 h, apiBadge 1 3 123 0 0 h, apiBadge 1 1 77 0 0 h, apiBadge 0 0 0 50 4 h
  , apiMapJump 5 3 h, apiMapJump 0 7 h, apiMapJump 48 (gsLevel (webState h)) h
  , j1, j2, j3
  ]
  where
    -- 换局接口依次作用（wasm 侧 m3Advance / m3Restart / m3Showcase 会改写当前局，原生侧同样串起来）
    (h1, j1) = apiAdvance m0 7 h
    (h2, j2) = apiRestart m0 7 h1
    (_, j3) = apiShowcase h2

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
      , inBounds b q
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
  , inBounds b q
  , let (x, y) = (getCell b p, getCell b q)
  , (rainbow x && cham y) || (cham x && rainbow y)
  ]
  where
    rainbow cell = case cell of
      Gem _ Rainbow 0 Nothing -> True
      _ -> False
    cham cell = chameleonColor cell /= Nothing

-- | 走法 fix-RCRC-RCRC-…（第 48 关魔法地格的固定用例）：第 k 步（0 起）换第 k 对（每对 4 个数字 r1 c1 r2 c2），
-- 列表用完后按提示。与 node 两侧的 fixedMove 逐条相同。
fixedMove :: String -> Int -> Maybe (Pos, Pos)
fixedMove mode k = case splitDash mode of
  ("fix" : mvs) | k < length mvs, [a, b, c, d] <- map digitToInt (mvs !! k) -> Just ((a, b), (c, d))
  _ -> Nothing
  where
    splitDash s = case break (== '-') s of
      (w, []) -> [w]
      (w, _ : rest) -> w : splitDash rest

-- | 魔法地格扩爆（第 48 关）：本步的 EvBlast 里来源格在魔法地格上的，每个来源一行
-- 「元素@(r,c) N 格 R 行 C 列」（N = 该来源的目标格数，含扩出来的一圈）。node 两侧按接口 JSON 的 events 算出同样的行。
magicBlasts :: GameState -> Pos -> Pos -> [String]
magicBlasts gs a b =
  [ unElementName (evElement e) ++ "@" ++ show s ++ " " ++ show (length ts) ++ " 格 "
      ++ show (length (nub (map fst ts))) ++ " 行 " ++ show (length (nub (map snd ts))) ++ " 列"
  | e <- pdEvents (play (Swap a b) gs), evKind e == EvBlast
  , s <- nub (map fst (evCells e)), s `elem` magic
  , let ts = nub [t | (s', t) <- evCells e, s' == s]
  ]
  where
    magic = [p | (p, (nm, _)) <- gsGround gs, unElementName nm == "magic"]
