-- | 调色板（纯数据，不含 SDL）：五色主色、名字目标 / 自定义元素按名字取色、格子的粒子 / 退回画法颜色。
-- 元素名的颜色表本身是 UI.Presentation.elementRGBTable；这里只按格子 / 名字查它。
--
-- 网页版有对应的 JS 副本（web/www/cells.js 的 COLOR_RGB / ELEMENT_RGB / cellRGB、web/www/main.js 的步末碎屑色），
-- 两边由 test/Spec/WebColors.hs 逐项比对；colorRGB 与 tools/gen_assets.py 的 GEMS 调色板也在那里比对。
-- 依赖：Match3.Core、UI.CellFace、UI.Presentation。网页经 UI.WebMeta（m3Meta）读这些表（原桌面版 UI.Layout 曾再导出本模块，已随 SDL2 前端移除）。
module UI.Palette
  ( colorRGB
  , namedRGB
  , cellRGB
  ) where

import Match3.Core
import UI.CellFace (faceColor)
import UI.Presentation (RGB, elementRGBTable)

-- | 五色的主色（与 tools/gen_assets.py 调色板一致）。
colorRGB :: Color -> RGB
colorRGB C1 = (236, 62, 78)   -- 红·圆
colorRGB C2 = (52, 196, 96)   -- 绿·方
colorRGB C3 = (56, 128, 246)  -- 蓝·菱
colorRGB C4 = (255, 194, 36)  -- 黄·星
colorRGB C5 = (172, 88, 236)  -- 紫·三角

-- | 名字目标（goalCount (CountNamed …)） / 自定义元素按名字取色；表里没有的名字为灰蓝。
namedRGB :: ElementName -> RGB
namedRGB n = maybe (200, 200, 220) id (lookup n elementRGBTable)

-- | 格子对应的粒子 / 退回画法颜色。
cellRGB :: Cell -> RGB
cellRGB cell = case cell of
  Stone _ -> (120, 120, 130)
  Chest _ -> (220, 170, 60)
  Honey _ -> (240, 180, 40)
  Balloon col -> colorRGB col
  Cookie -> (210, 160, 90)
  Cake _ -> (255, 140, 180)
  MagicHat -> (140, 90, 200)
  Maker col _ -> colorRGB col
  Snail _ _ -> (90, 160, 70)
  Safe _ -> (180, 150, 40)
  Flip f _ -> colorRGB f
  Surprise -> (255, 100, 160)
  Bottle col -> colorRGB col
  TimeSpirit -> (80, 220, 255)
  Countdown col _ -> colorRGB col
  Gem col _ _ _ -> colorRGB col
  Custom n _
    | Just col <- faceColor cell -> colorRGB col -- 元素给出的当前颜色（变色龙）
    | otherwise -> maybe (160, 160, 170) id (lookup n elementRGBTable)
