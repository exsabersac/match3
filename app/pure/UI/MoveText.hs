-- | 道具点选模式的规则：'keepsTool' 决定哪种结算结果之后道具点选模式保持不变（网页 m3Hammer / m3Cross / m3FreeSwap
-- 的 keepTool 字段）。原桌面走步提示文案 moveMsg（英文，写进 SDL 窗口标题）随 SDL2 前端移除（refactor/web-only）；
-- 网页的提示文字在 web/www/main.js。
--
-- 依赖：Match3.Core（Outcome）。纯函数。
module UI.MoveText
  ( MoveUi(..)
  , keepsTool
  ) where

import Match3.Core

-- | 走步从哪条界面路径来：拖拽交换、点击交换、三种道具。
data MoveUi = UiDrag | UiClick | UiHammer | UiCross | UiFreeSwap
  deriving (Eq, Show, Enum, Bounded)

-- | 道具结果之后是否保持点选模式：只有自由交换换不掉（不扣次数）时留在自由交换、重新选第一格；
-- 其余一律退出点选模式。
keepsTool :: MoveUi -> Outcome -> Bool
keepsTool UiFreeSwap NoMatch = True
keepsTool _ _ = False
