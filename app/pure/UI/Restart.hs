-- | 「重开本关」规则（网页 m3Restart / m3Advance 用；R 键 / 失败后 N）：
-- 每日挑战按开局步数与原目标换种子重开，战役关 restartLevel。原在已移除的 SDL 桌面版 UI.Input.restartSame，web-sdl-parity 时移进
-- app/pure（行为不变）；网页 Api 不直接读 GameState 字段（测试 frontends_read_view_model）。
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
