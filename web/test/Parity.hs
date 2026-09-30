-- 原生（桌面 GHC 9.14.1 / lts-24.60 + compiler 覆盖）一侧的一致性脚本：
-- 同一关卡 + 种子开局，按核心提示连走 N 步，逐步打印接口 JSON。
-- 与 node-parity.mjs（wasm 一侧）的输出逐字节比较，验证两端规则与随机数完全一致。
-- 走完后再撤销一步（覆盖 m3Undo / Engine.History），最后一行是撤销结果。
-- 用法（仓库根目录）：stack runghc -- -isrc -iweb/hs web/test/Parity.hs 0 20260929 12 [hint|combo|combo-bomb]
module Main (main) where

import System.Environment (getArgs)
import Match3.Core
import Match3Web.Api (apiNew, apiSwap, apiUndo, webState)

main :: IO ()
main = do
  args <- getArgs
  let (li, seed, n) = case map read (take 3 args) of
        [a, b, c] -> (a, b, c)
        _ -> error "用法：Parity 关卡 种子 步数 [hint|combo|combo-bomb]"
      mode = case drop 3 args of
        (m : _) -> m
        [] -> "hint"
      (gs0, j0) = apiNew li seed
      go 0 h = putStrLn (snd (apiUndo h))
      go k h = let gs = webState h in case (gsOver gs, findHint (gsBoard gs)) of
        (Nothing, Just hint) -> do
          let (a, b) = pickMove mode (gsBoard gs) hint
              (h', j) = apiSwap a b h
          putStrLn j
          go (k - 1) h'
        _ -> putStrLn (snd (apiUndo h))
  putStrLn j0
  go (n :: Int) gs0

-- | 走法（第 4 个参数，缺省 hint）：hint = 按核心提示；combo = 盘上有「彩虹 × 直线 / 炸弹」相邻（两格都无冰、无叠层）时
-- 先换这一对（行优先，先右后下），否则按提示——提示不会主动选彩虹组合，第 44 关（rainbow_combos）要靠它覆盖变身步；
-- combo-bomb 同 combo，但先换「彩虹 × 炸弹」（覆盖 rainbow_bomb 变身）。与 node-parity.mjs / node-anim-parity.mjs 的 pickMove 逐条相同。
pickMove :: String -> Board -> (Pos, Pos) -> (Pos, Pos)
pickMove mode b hint = case ordered of
  (pr : _) -> pr
  [] -> hint
  where
    ordered
      | mode == "combo" = comboPairs
      | mode == "combo-bomb" = filter hasBomb comboPairs ++ filter (not . hasBomb) comboPairs
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

