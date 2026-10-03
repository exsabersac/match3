-- | 内置元素的共用小件：被两个以上分组文件用到的规则 / 放置辅助函数。
-- 只在一个分组里用到的辅助函数留在该分组文件里（如 Obstacle 的 chip / layersPlace、Layer 的 peel / layerChip）。
module Match3.Element.Builtin.Common
  ( deadRule
  , colorPlace
  ) where

import Match3.Element.Registry (Placer)
import Match3.Element.Types
import Match3.Types

-- | 邻消规则：只削层 / 打碎，打碎的格并入清除格（石头 / 宝箱 / 蜂蜜 / 蛋糕 / 气球 / 时间精灵）。
deadRule :: (Board -> [Pos] -> [Pos] -> (Board, [Pos])) -> AdjCtx -> Board -> AdjOut
deadRule f ctx b = let (b', dead) = f b (acTrue ctx) (acDirect ctx) in AdjOut b' dead []

-- | 放置：一个颜色参数（气球 / 染色瓶；精确匹配）。
colorPlace :: (Color -> Cell) -> Placer
colorPlace con args _ = con <$> exactArgs argColor args
