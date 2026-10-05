-- | 选关地图的章节划分（CH1–CH7）：章节起点的关卡下标与章节标签。
-- 网页经 Match3Web.Api 的 m3Meta.chapters 取；原在已移除的 SDL 桌面版 UI.LevelMap，web-sdl-parity 时逐字移进 app/pure。
-- 同步：章节起点要与 allLevels 的章节划分一致。
module UI.Chapters
  ( chapterStarts
  , chapterLabel
  , chapterTitle
  ) where

-- | Chapter boundaries (0-based level index starts). Light map separators.
chapterStarts :: [Int]
chapterStarts = [0, 7, 14, 21, 28, 34, 36]

chapterLabel :: Int -> String
chapterLabel 0 = "CH1"
chapterLabel 7 = "CH2"
chapterLabel 14 = "CH3"
chapterLabel 21 = "CH4"
chapterLabel 28 = "CH5"
chapterLabel 34 = "CH6"
chapterLabel 36 = "CH7"
chapterLabel _ = ""

-- | 第 k 章（0 起）的中文章名，与贴图 zh_ch1 … zh_ch7（tools/gen_assets.py 的 ZH 表）同一组文字；
-- 网页图集没有 zh_* 文字图，选关地图用它画章节标签。超出七章时退回「第 N 章」。
chapterTitle :: Int -> String
chapterTitle k
  | k >= 0 && k < length names = names !! k
  | otherwise = "第 " ++ show (k + 1) ++ " 章"
  where
    names = ["第一章", "第二章", "第三章", "第四章", "第五章", "第六章", "第七章"]
