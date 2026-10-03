{-# LANGUAGE OverloadedStrings #-}
-- | 单格绘制的元素查表（第三刀）：键 = 规则层注册表的元素名（elementName defaultRegistry），
-- 值 = 同一元素的两个渲染后端 —— 几何降级版（UI.Cell.Prim）与贴图版（UI.Cell.Art）——
-- 以及它的主贴图名（缺图检测用）。UI.BoardPrim.drawGemAt 与 UI.BoardArt.drawCellArt 都经这张表分派。
--
-- 宝石的 5 个名字（gem / line_h / line_v / bomb / rainbow）共用一个渲染器（特殊块标记在渲染器内部按
-- GemKind 画）。自定义元素（Custom，名字由关卡定义、可能与任何键同名）不查 cellTable：先查按 Custom 名字的
-- customTable（段 5：气泡），查不到走自定义渲染器；cellTable 里查不到的名字也退回自定义渲染器（它只画 Custom 格，其它格为空）。
-- 新增棋盘元素：在 UI.Cell.Prim / UI.Cell.Art 各写一个函数，在这里加一行。
--
-- 依赖：UI.Cell.Prim、UI.Cell.Art、Art、Match3.Core（含元素名 elementName / defaultRegistry）。
module UI.CellTable
  ( CellRenderer(..)
  , cellTable
  , cellRenderer
  , customRenderer
  , customTable
  , primarySprite
  ) where

import Art (Art)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CInt)
import Match3.Core
import Match3.View (BossPart (..), bossPart)
import SDL (Renderer)
import UI.Cell.Art
import UI.Cell.Prim

-- | 一种元素的绘制后端。
data CellRenderer = CellRenderer
  { crPrim   :: Renderer -> CInt -> CInt -> Cell -> Bool -> IO ()      -- ^ 几何版（含闪白）
  , crArt    :: Renderer -> Art -> Int -> CInt -> CInt -> Cell -> IO () -- ^ 贴图版（闪白由分派方统一画）
  , crSprite :: Cell -> String                                          -- ^ 主贴图名
  }

-- | 元素名 → 渲染器。
cellTable :: [(ElementName, CellRenderer)]
cellTable =
  [ ("gem", gemRenderer)
  , ("line_h", gemRenderer)
  , ("line_v", gemRenderer)
  , ("bomb", gemRenderer)
  , ("rainbow", gemRenderer)
  , ("stone", CellRenderer primStone artStone (const "stone_3"))
  , ("chest", CellRenderer primChest artChest (const "chest"))
  , ("honey", CellRenderer primHoney artHoney (const "honey"))
  , ("balloon", CellRenderer primBalloon artBalloon (\c -> case c of Balloon k -> "balloon_" ++ colorKey k; _ -> ""))
  , ("cookie", CellRenderer primCookie artCookie (const "cookie"))
  , ("cake", CellRenderer primCake artCake (const "cake_1"))
  , ("magic_hat", CellRenderer primMagicHat artMagicHat (const "magic_hat"))
  , ("maker", CellRenderer primMaker artMaker (\c -> case c of Maker k _ -> "maker_" ++ colorKey k; _ -> ""))
  , ("snail", CellRenderer primSnail artSnail (const "snail"))
  , ("safe", CellRenderer primSafe artSafe (const "safe"))
  , ("flip", CellRenderer primFlip artFlip (\c -> case c of Flip f _ -> gemSprite f; _ -> ""))
  , ("surprise", CellRenderer primSurprise artSurprise (const "surprise"))
  , ("bottle", CellRenderer primBottle artBottle (\c -> case c of Bottle k -> "bottle_" ++ colorKey k; _ -> ""))
  , ("time_spirit", CellRenderer primTimeSpirit artTimeSpirit (const "time_spirit"))
  , ("countdown", CellRenderer primCountdown artCountdown (\c -> case c of Countdown k _ -> gemSprite k; _ -> ""))
  ]

-- | 宝石（普通 / 直线 / 炸弹 / 彩虹）：彩虹的主贴图是 rainbow，其余按颜色。
gemRenderer :: CellRenderer
gemRenderer =
  CellRenderer primGem artGem $ \c -> case c of
    Gem k kind _ _ -> if kind == Rainbow then "rainbow" else gemSprite k
    _ -> ""

-- | 自定义元素：贴图名 = 元素名（缺图时逐格回退到几何画法：灰色方块）。
customRenderer :: CellRenderer
customRenderer =
  CellRenderer primCustom artCustom $ \c -> case c of
    Custom n _ -> unElementName n
    _ -> ""

-- | 有专门画法的 Custom 元素（段 5：气泡；新玩法 2：魔法石；新玩法 3：毛球；新玩法 5：雪怪 Boss；新玩法 7：变色龙）：键 = Custom 的名字，与 cellTable 分开，名字不会和内置键冲突。
-- 查不到的 Custom 名字走 customRenderer（贴图名 = 元素名 + 层数角标，缺图画灰块）。
customTable :: [(ElementName, CellRenderer)]
customTable =
  [ ("bubble", CellRenderer primBubble artBubble (const "bubble"))
  , ("magic_stone", CellRenderer primMagicStone artMagicStone (const "magic_stone_0"))
  , ("fuzzball", CellRenderer primFuzzball artFuzzball (const "fuzzball"))
  , ("chameleon", CellRenderer primChameleon artChameleon (const "chameleon"))
  , ("snow_boss", CellRenderer primSnowBoss artSnowBoss (\c -> maybe "snow_boss" (\bp -> "snow_boss_" ++ show (bpQuad bp)) (bossPart c)))
  ]

-- | 某格的渲染器。
cellRenderer :: Cell -> CellRenderer
cellRenderer cell = case cell of
  Custom n _ -> fromMaybe customRenderer (lookup n customTable)
  _ -> fromMaybe customRenderer (lookup (elementName defaultRegistry cell) cellTable)

-- | 该格的主贴图名（用于检测资源缺失时逐格回退）。
primarySprite :: Cell -> String
primarySprite cell = crSprite (cellRenderer cell) cell
