-- | 按名字读单格的显示附加字段（Match3.View.cellExtras，由元素自己提供；网页格子 JSON 是同一组字段）。
-- 这里只把字段拼回顺手的形状（UI.WebMeta 等用），不认元素名：哪个元素给出了这些字段，就按它画。
-- 依赖：Match3.Core、Match3.View。
module UI.CellFace
  ( BossPart (..)
  , bossPart
  , faceColor
  ) where

import Match3.Core
import Match3.View (FaceValue (..), cellExtras)

-- | Boss 的一格怎么画（新玩法 5 雪怪 Boss 给出 q / hurt / turn / every）：象限（0 左上 / 1 右上 / 2 左下 / 3 右下，
-- 贴图 snow_boss[_hurt]_<象限>）、是否受伤（换受伤表情）、召唤计数与周期（右下格画进度小点）。
data BossPart = BossPart
  { bpQuad :: Int
  , bpHurt :: Bool
  , bpTurn :: Int
  , bpEvery :: Int
  }
  deriving (Eq, Show)

-- | 格子带齐 q / hurt / turn / every 四个字段时的 Boss 画法；否则 Nothing。
bossPart :: Cell -> Maybe BossPart
bossPart cell = BossPart <$> int "q" <*> flag "hurt" <*> int "turn" <*> int "every"
  where
    fs = cellExtras cell
    int k = case lookup k fs of
      Just (FaceInt i) -> Just i
      _ -> Nothing
    flag k = case lookup k fs of
      Just (FaceBool b) -> Just b
      _ -> Nothing

-- | 元素给出的当前颜色（"c"；内置 = 变色龙）。
faceColor :: Cell -> Maybe Color
faceColor cell = case lookup "c" (cellExtras cell) of
  Just (FaceColor c) -> Just c
  _ -> Nothing
