-- | 通用效果事件（第三刀）：各游戏把自己的事件映射成 Effect，通用播放层只认它。
--
-- 约定：
--   * efBeat 是节拍号，同一节拍的效果同时播放；一步里的节拍单调不减；
--   * efKind / efSubject 是字符串标签（种类 / 主体），外壳按它们查表决定怎么播，通用层不解释；
--   * efSpots 是涉及的格（没有网格的游戏为空），efAmount 是附带的数值（得分、连击数……）。
--
-- 不依赖任何具体游戏。
module Engine.Effect
  ( Effect(..)
  , beats
  , beatsOf
  ) where

-- | 一条通用效果。
data Effect = Effect
  { efBeat    :: Int
  , efKind    :: String
  , efSubject :: String
  , efSpots   :: [(Int, Int)]
  , efAmount  :: Int
  } deriving (Eq, Show)

-- | 按节拍把相邻的效果分组（保持原顺序）。
beats :: [Effect] -> [(Int, [Effect])]
beats [] = []
beats (e : es) =
  let (same, rest) = span ((== efBeat e) . efBeat) es
  in (efBeat e, e : same) : beats rest

-- | 某个节拍的全部效果。
beatsOf :: Int -> [Effect] -> [Effect]
beatsOf b = filter ((== b) . efBeat)
