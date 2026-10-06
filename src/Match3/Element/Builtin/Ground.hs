{-# LANGUAGE OverloadedStrings #-}
-- | 地面层元素：格子下面的层（在 GameState.gsGround 里按名字记层数），一条 'GroundKind' 记录。
--
-- 共同特征：不占格、不挡交换 / 匹配、不随重力 / 洗牌 / 皮带移动；上方格子每被消除（或被边缘收走）一次
-- 受一次命中（'groundHit'：层数 → 新层数），每去一层按 'groundCounter' 计 1。地面层不进盘面，
-- 种类只是一条数据（没有类型、没有值）。内置有双层果冻（第 39 关）与魔法地格（新玩法 8，第 48 关）。
module Match3.Element.Builtin.Ground
  ( jelly
  , magicGround
  , magicGroundName
  , magicWiden
  ) where

import Data.List (nub, sort)
import Match3.Board.Grid (inBounds)
import Match3.Element.Kind
import Match3.Element.Types
import Match3.Types

-- | 双层果冻：上方格子每被消除（或被边缘收走）一次去一层；每去一层按 CountNamed "jelly" 计 1。
jelly :: GroundKind
jelly = (groundKind "jelly")
  { groundHit = \n -> if n > 1 then Just (n - 1) else Nothing
  , groundCounter = Just (CountNamed "jelly")
  , groundLabel = Just "果冻"
  }

-- | 魔法地格（新玩法 8，开心消消乐的魔法地格）：地面层 "magic"（关卡里层数恒为 1，只用于显示）。
-- 不会被消耗（'groundHit' 用缺省：上方消除不去层、不计数），不占格、不挡交换 / 匹配，棋子照常落在上面。
-- 唯一的能力是扩爆（'groundWiden'）：特效（直线 / 炸弹，以及任何带 blast 的本体）在这一格上引爆时，
-- 爆炸范围向外扩一圈（'magicWiden'：原范围每格的八邻格都并进来）——直线从一行变三行、炸弹从 3×3 变 5×5。
-- 只看引爆格：爆炸只是扫过魔法地格不扩；扩出来的格与原范围一样算直接命中（障碍削层、特效连锁）。
-- 彩虹取色与特效 × 特效组合由成对交换规则给出的清除种子不扩；种子里的特效照常逐个引爆，落在魔法地格上的那枚照样扩。
-- | 魔法地格的元素名 "magic"（backlog #9 的「地面层新种类 magic」）。
magicGroundName :: ElementName
magicGroundName = "magic"

magicGround :: GroundKind
magicGround = (groundKind magicGroundName) {groundWiden = Just magicWiden}

-- | 扩一圈：原范围按原顺序在前，新并进来的格（原范围各格的八邻格、在盘内、不在原范围里）按行优先接在后面。
magicWiden :: Board -> [Pos] -> [Pos]
magicWiden b area = area ++ sort (nub [q | (r, c) <- area, dr <- [-1, 0, 1], dc <- [-1, 0, 1], let q = (r + dr, c + dc), inBounds b q, q `notElem` area])
