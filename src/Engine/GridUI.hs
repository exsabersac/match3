-- | 通用网格 UI 组件（第 11 刀）：与具体游戏无关的网格几何（像素 ↔ 格子坐标）、点选 / 拖动的判定、
-- 高亮集合。三消桌面版的 UI.Layout.pixelToCell / cellOrigin / allCells 与 UI.Input 的点选 / 拖动交换
-- 都经这里算；以后的其他网格类游戏直接复用。
--
-- 依赖：只有 base（不 import 任何 Match3 模块，测试 engine_layer_is_game_agnostic 扫描）。
-- 坐标约定：格子坐标是 (行, 列)，行优先；像素坐标是 (x, y)，x 对应列、y 对应行。
module Engine.GridUI
  ( -- * 网格几何
    GridGeom (..)
  , gridWidth
  , gridHeight
  , gridInside
  , gridCellAt
  , gridCellOrigin
  , gridCells
  , orthoAdjacent
    -- * 点选 / 拖动
  , Click (..)
  , gridClick
  , gridDragRelease
    -- * 高亮
  , Highlight (..)
  , noHighlight
  , isSelected
  , isHinted
  , isFlashing
  ) where

-- | 一块矩形网格在屏幕上的摆放：左上角像素、单格边长、行列数。
data GridGeom a = GridGeom
  { ggLeft :: a
  , ggTop :: a
  , ggCell :: a
  , ggRows :: Int
  , ggCols :: Int
  }
  deriving (Eq, Show)

-- | 网格总宽 / 总高（像素）。
gridWidth, gridHeight :: Num a => GridGeom a -> a
gridWidth g = ggCell g * fromIntegral (ggCols g)
gridHeight g = ggCell g * fromIntegral (ggRows g)

-- | 格子坐标是否在网格内。
gridInside :: GridGeom a -> (Int, Int) -> Bool
gridInside g (r, c) = r >= 0 && c >= 0 && r < ggRows g && c < ggCols g

-- | 像素 → 格子（网格外返回 Nothing）。先减去左上角，越出 [0, 宽/高) 即在外，再按单格边长整除。
gridCellAt :: Integral a => GridGeom a -> a -> a -> Maybe (Int, Int)
gridCellAt g px py =
  let x = px - ggLeft g
      y = py - ggTop g
  in if x < 0 || y < 0 || x >= gridWidth g || y >= gridHeight g
       then Nothing
       else
         let c = fromIntegral (x `div` ggCell g)
             r = fromIntegral (y `div` ggCell g)
         in if gridInside g (r, c) then Just (r, c) else Nothing

-- | 格子 → 左上角像素。
gridCellOrigin :: Num a => GridGeom a -> (Int, Int) -> (a, a)
gridCellOrigin g (r, c) = (ggLeft g + fromIntegral c * ggCell g, ggTop g + fromIntegral r * ggCell g)

-- | 全部格子（行优先）。
gridCells :: GridGeom a -> [(Int, Int)]
gridCells g = [(r, c) | r <- [0 .. ggRows g - 1], c <- [0 .. ggCols g - 1]]

-- | 正交（4 邻接）相邻。
orthoAdjacent :: (Int, Int) -> (Int, Int) -> Bool
orthoAdjacent (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) == 1

-- | 点一格的结果（两步点选：先选中一格，再点另一格成对；点同一格取消）。
data Click c
  = ClickSelect c     -- ^ 之前没有选中：选中这一格
  | ClickDeselect     -- ^ 点了已选中的格：取消
  | ClickPair c c     -- ^ 已选中 a、又点了 b：成对（a, b）
  deriving (Eq, Show)

-- | 由当前选中格与点中的格决定点选结果。
gridClick :: Eq c => Maybe c -> c -> Click c
gridClick sel pos = case sel of
  Nothing -> ClickSelect pos
  Just p1
    | p1 == pos -> ClickDeselect
    | otherwise -> ClickPair p1 pos

-- | 拖动松手：起点 from、松手处（网格外为 Nothing）；落在另一格且满足相邻判定时给出 (from, to)。
gridDragRelease :: Eq c => (c -> c -> Bool) -> c -> Maybe c -> Maybe (c, c)
gridDragRelease adj from mTo = case mTo of
  Just to | from /= to && adj from to -> Just (from, to)
  _ -> Nothing

-- | 一帧要高亮的格子：选中格、提示格（按顺序）、闪光格、固定高亮格（如两步道具的第一格）。
data Highlight c = Highlight
  { hlSelected :: Maybe c
  , hlHint :: [c]
  , hlFlash :: [c]
  , hlPinned :: Maybe c
  }
  deriving (Eq, Show)

noHighlight :: Highlight c
noHighlight = Highlight Nothing [] [] Nothing

isSelected, isHinted, isFlashing :: Eq c => Highlight c -> c -> Bool
isSelected h p = hlSelected h == Just p
isHinted h p = p `elem` hlHint h
isFlashing h p = p `elem` hlFlash h
