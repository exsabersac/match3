-- | 桌面走步的提示文案：交换（拖拽 / 点击）与三种道具（锤子 / 十字 / 自由交换）各一张
-- 「结算结果 'Outcome' → 一句话」的表，写进 App 的 appMsg，显示在窗口标题「  |  」之后与 HUD 提示行。
-- 另有 'keepsTool'：哪种结果之后道具点选模式保持不变。
--
-- 纯函数、返回 String（调用方 T.pack 进 appMsg）；文案逐字由 test/Spec/MoveText.hs 钉住。
-- 依赖：Match3.Core（Outcome / GameState / MoveFx / loseHint）、Match3.View（收集进度后缀 goalBracket）。
module UI.MoveText
  ( MoveUi(..)
  , moveMsg
  , keepsTool
  ) where

import Match3.Core
import Match3.View (GameView (..), gameView, goalBracket)

-- | 走步从哪条界面路径来：拖拽交换、点击交换、三种道具。
data MoveUi = UiDrag | UiClick | UiHammer | UiCross | UiFreeSwap
  deriving (Eq, Show, Enum, Bounded)

-- | 走步提示：界面路径、走步前状态、走步后状态、本步特效（连击数）、结算结果。
moveMsg :: MoveUi -> GameState -> GameState -> MoveFx -> Outcome -> String
moveMsg ui before after fx out = case ui of
  UiDrag -> dragMsg before out
  UiClick -> clickMsg before after fx out
  UiHammer -> boosterMsg "Hammer" "No hammers left" "Hammer failed" after out
  UiCross -> boosterMsg "Cross" "No cross-clears left" "Cross failed" after out
  UiFreeSwap -> boosterMsg "Free-swap" "Free-swap invalid / empty" "Free-swap: no match; not spent" after out

-- | 道具结果之后是否保持点选模式：只有自由交换换不掉（不扣次数）时留在自由交换、重新选第一格；
-- 其余一律退出点选模式。
keepsTool :: MoveUi -> Outcome -> Bool
keepsTool UiFreeSwap NoMatch = True
keepsTool _ _ = False

-- | 三种道具共用的表：道具名、没有次数 / 无效时、没打掉东西时的两句不同，其余按道具名拼。
boosterMsg :: String -> String -> String -> GameState -> Outcome -> String
boosterMsg name invalid noMatch after out = case out of
  InvalidSwap -> invalid
  NoMatch -> noMatch
  MoveApplied g -> name ++ " +" ++ show g
  LevelClear _ _ -> name ++ " cleared level!"
  Won _ -> name ++ " won!"
  Lost _ -> loseHint (gsGoal after)

-- | 拖拽交换。
dragMsg :: GameState -> Outcome -> String
dragMsg before out = case out of
  NoMatch -> "No match; rolled back"
  InvalidSwap -> "Need adjacent"
  MoveApplied s -> "Drag +" ++ show s
  Won s -> "YOU WIN score=" ++ show s
  LevelClear _ n -> "Level clear -> L" ++ show (n + 1)
  Lost s -> "Out of moves score=" ++ show s ++ " — " ++ loseHint (gsGoal before)

-- | 点击交换（含连击 / 收集进度 / 自动洗牌）。
clickMsg :: GameState -> GameState -> MoveFx -> Outcome -> String
clickMsg before after fx out = case out of
  InvalidSwap -> "Need 4-neighbor adjacent"
  NoMatch -> "No match; rolled back"
  MoveApplied s -> "Cleared +" ++ show s ++ comboMsg ++ collectMsg ++ shuffledMsg
  Won s -> "YOU WIN score=" ++ show s ++ " — N/click"
  LevelClear s n -> "Level clear +" ++ show s ++ comboMsg ++ " -> L" ++ show (n + 1) ++ " (N/Space/click)"
  Lost s -> "Out of moves score=" ++ show s ++ " — " ++ loseHint (gsGoal before) ++ " — R/click"
  where
    comboMsg = if fxCombo fx > 1 then " combo x" ++ show (fxCombo fx) else ""
    -- 收集类目标的进度后缀（Match3.View.goalBracket）
    collectMsg = goalBracket (gvGoal (gameView after))
    shuffledMsg = if gsShuffled after then " (auto-shuffled)" else ""
