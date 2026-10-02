{-# LANGUAGE RankNTypes #-}

-- | 盘面与单元格的光学（Haskell 特性第 6 项，见 docs/haskell-features/06-测试与光学.md）。
--
-- 单元格是一个大的和类型，宝石 @Gem 颜色 种类 冰层 叠层@ 只是其中一种；叠层又是一个和类型，
-- 其中迷雾 / 锁链 / 火箭冰冻 / 窗帘带层数。第 6 项前「改宝石上的叠层」到处写成
-- @case cell of Gem col kind ice (Just (Fog n)) -> Gem col kind ice (Just (Fog (n - 1))); _ -> cell@，
-- 四种揭层叠层各抄一遍（Match3.Grass）。这里把「格 → 宝石 → 叠层 → 某种叠层的层数」拆成可以复合的几段：
--
-- > cellAt p . overlay . _Fog   :: Traversal' Board Int     -- 第 p 格（若是带迷雾的宝石）的迷雾层数
--
-- 'cellAt' 是透镜（坐标在界内时恰好一个焦点），'gemOverlay' / 'overlay' 是遍历（非宝石格没有焦点），
-- '_Fog' 等是棱镜（叠层是不是这一种）。定律见 test/Spec/Optics.hs。
--
-- 依赖：Engine.Optics、Match3.Types.Cell、Match3.Types.Board。
module Match3.Types.Optics
  ( -- * 盘面
    cellAt
  , cells
    -- * 单元格
  , _Gem
  , gemOverlay
  , overlay
    -- * 叠层
  , _Fog
  , _Chain
  , _Freeze
  , _Curtain
  ) where

import Engine.Optics
import Match3.Color (Color)
import Match3.Types.Board (Board, Pos, boardAt, boardSet)
import Match3.Types.Cell

-- | 盘面上的一格。坐标必须在界内（与 boardAt / boardSet 相同；越界读报错）。
cellAt :: Pos -> Lens' Board Cell
cellAt p = lens (`boardAt` p) (\b v -> boardSet b p v)

-- | 全部格子，行主序（= Grid 的 traverse，第 2 项派生的 Traversable）。
cells :: Traversal' Board Cell
cells = traverse

-- | 宝石格的四个字段（颜色, 种类, 冰层, 叠层）。
_Gem :: Prism' Cell (Color, GemKind, Int, Maybe CellOverlay)
_Gem = prism' (\(c, k, i, o) -> Gem c k i o) match
  where
    match cell = case cell of
      Gem c k i o -> Just (c, k, i, o)
      _ -> Nothing

-- | 宝石的叠层槽（有没有叠层都算一个焦点）；非宝石格没有焦点。
gemOverlay :: Traversal' Cell (Maybe CellOverlay)
gemOverlay f cell = case cell of
  Gem c k i o -> Gem c k i <$> f o
  _ -> pure cell

-- | 宝石上现有的叠层（= gemOverlay . _Just）；没有叠层或不是宝石时没有焦点。
overlay :: Traversal' Cell CellOverlay
overlay = gemOverlay . _Just

-- | 带层数的叠层：迷雾 / 锁链 / 火箭冰冻 / 窗帘。
_Fog, _Chain, _Freeze, _Curtain :: Prism' CellOverlay Int
_Fog = prism' Fog (\o -> case o of Fog n -> Just n; _ -> Nothing)
_Chain = prism' Chain (\o -> case o of Chain n -> Just n; _ -> Nothing)
_Freeze = prism' Freeze (\o -> case o of Freeze n -> Just n; _ -> Nothing)
_Curtain = prism' Curtain (\o -> case o of Curtain n -> Just n; _ -> Nothing)
