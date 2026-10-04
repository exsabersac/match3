-- | 「重开本关」规则（桌面 R 键 / 失败后 N 与网页 m3Restart / m3Advance 共用）：
-- 每日挑战按开局步数与原目标换种子重开，战役关 restartLevel。原在桌面 UI.Input.restartSame，web-sdl-parity 时移进 app/pure，
-- 桌面 UI.Input.restartSame 改为调这里（行为不变）；网页 Api 不直接读 GameState 字段（测试 frontends_read_view_model）。
--
-- 依赖：Match3.Core。纯函数。
module UI.Restart
  ( restartSame
  ) where

import Match3.Core

-- | 重开同一局：开局步数（每日挑战用）、新种子、当前局面。
restartSame :: Int -> Int -> GameState -> GameState
restartSame startMoves seed gs
  | gsDaily gs = newDailyGame (GameConfig startMoves (gsGoal gs)) seed
  | otherwise = restartLevel gs seed
