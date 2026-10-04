{-# LANGUAGE OverloadedStrings #-}
-- | 特殊×特殊合成：Line×Bomb、Rainbow×Line、Bomb×Bomb、Line×Line。第 8 刀起组合效果是一张有序的组合表
-- （'builtinComboRules'，解释器见 Match3.Element.Special；注册表 World.comboRules 持有、并成一条成对交换规则），
-- isSpecialCombo / comboClearSeeds 是这张表的判定与清种子；另有各组合的种类谓词与爆炸几何（扩展组合用）。
-- 两端须 specialActivates（软锁不发火）。普通三消与彩虹单端交换见 Board / Rainbow。
module Match3.Combos
  ( builtinComboRules
  , isLineBombCombo
  , isRainbowLineCombo
  , isBombBombCombo
  , isLineLineCombo
  , isSpecialCombo
  , comboClearSeeds
    -- * 魔力鸟组合增强（新玩法 4，规则开关 rainbow_combos）
  , rainbowComboMorph
    -- * 爆炸几何
  , bigBomb
  , fullRowCol
  , lineBombCross
  ) where

import Data.List (nub)
import Match3.Element.Special (comboFires, comboSeedsFor)
import Match3.Element.Types (ComboRule(..))
import Match3.Rainbow (rainbowClearSeeds)
import Match3.Board.Grid (inBounds)
import Match3.Types

at :: Board -> Pos -> Cell
at = boardAt

isLine :: GemKind -> Bool
isLine LineH = True
isLine LineV = True
isLine _ = False

isBombK :: GemKind -> Bool
isBombK Bomb = True
isBombK _ = False

isRainbowK :: GemKind -> Bool
isRainbowK Rainbow = True
isRainbowK _ = False

kindOf :: Cell -> Maybe GemKind
kindOf (Gem _ k _ _) = Just k
kindOf _ = Nothing

isLineBombCombo :: Board -> Pos -> Pos -> Bool
isLineBombCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just k1, Just k2) ->
      (isLine k1 && isBombK k2) || (isBombK k1 && isLine k2)
    _ -> False

isRainbowLineCombo :: Board -> Pos -> Pos -> Bool
isRainbowLineCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just k1, Just k2) ->
      (isRainbowK k1 && isLine k2) || (isLine k1 && isRainbowK k2)
    _ -> False

isBombBombCombo :: Board -> Pos -> Pos -> Bool
isBombBombCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just Bomb, Just Bomb) -> True
    _ -> False

isLineLineCombo :: Board -> Pos -> Pos -> Bool
isLineLineCombo b p1 p2 =
  case (kindOf (at b p1), kindOf (at b p2)) of
    (Just k1, Just k2) -> isLine k1 && isLine k2
    _ -> False

-- | 内置特殊块组合表（第 8 刀，顺序 = 旧 comboClearSeeds 的判定顺序）：炸弹 × 炸弹（两个 5×5）、
-- 直线 × 直线（两端各清整行整列）、直线 × 炸弹（以炸弹端为中心的 3 行 + 3 列）、彩虹 × 直线（彩虹取色，
-- 实际总被先于组合的彩虹单端交换规则接走）。表里没有的组合（彩虹 × 炸弹 / 彩虹 × 彩虹 / 普通宝石）不成立。
builtinComboRules :: [ComboRule]
builtinComboRules =
  [ ComboRule "bomb×bomb" (kindIs isBombK) (kindIs isBombK) (\b p q -> nub (bigBomb b p ++ bigBomb b q))
  , ComboRule "line×line" (kindIs isLine) (kindIs isLine) (\b p q -> nub (fullRowCol b p ++ fullRowCol b q))
  , ComboRule "line×bomb" (kindIs isLine) (kindIs isBombK) (\b _ q -> lineBombCross b q)
  , ComboRule "rainbow×line" (kindIs isRainbowK) (kindIs isLine) rainbowClearSeeds
  ]
  where
    kindIs f cell = maybe False f (kindOf cell)

-- | 特殊 × 特殊组合是否成立（交换前盘面，两端须 specialActivates）= 内置组合表（'builtinComboRules'）。
isSpecialCombo :: Board -> Pos -> Pos -> Bool
isSpecialCombo = comboFires builtinComboRules

-- | 组合的清除种子（交换后盘面）= 内置组合表；表里没有的组合为空。
comboClearSeeds :: Board -> Pos -> Pos -> [Pos]
comboClearSeeds = comboSeedsFor builtinComboRules

-- | 魔力鸟组合增强（新玩法 4，开心消消乐的「魔力鸟 + 特效」）：只在打开规则开关 "rainbow_combos" 的关卡里用
-- （关卡级机制 RainbowCombos 的 morph）。b0 = 交换前盘面、swapped = 交换后；一端彩虹、另一端直线 / 炸弹，
-- 两端都能点火（软锁不算）时成立，给出 (元素名, 变身格, 起手种子)：
--
-- * 彩虹 × 直线（"rainbow_line"）：盘上与直线**同色的普通宝石**（无冰、无叠层）全部变成直线，
--   横竖按格子奇偶交替（行 + 列为偶数 = 横向，奇数 = 竖向；原作随机，这里取确定的棋盘格）；
-- * 彩虹 × 炸弹（"rainbow_bomb"）：同色的普通宝石全部变成炸弹。
--
-- 变身格的来源 = 交换后彩虹所在的格（前端从那里「长出」新特效）。起手种子与原有彩虹取色相同（'rainbowClearSeeds'：
-- 彩虹 + 该色全部格，含交换来的直线 / 炸弹、带冰 / 叠层的同色宝石、倒计时 / 双面块），种子里的特效在同一轮
-- 连锁里全部引爆（原作逐个引爆，这里简化为同一轮展开）。
rainbowComboMorph :: Board -> Board -> Pos -> Pos -> Maybe (ElementName, [(Pos, Pos, Cell)], [Pos])
rainbowComboMorph b0 swapped p1 p2 = case (at b0 p1, at b0 p2) of
  (c1@(Gem _ Rainbow _ _), c2@(Gem col k _ _)) | ok c1 c2 k -> morphFrom p2 col k
  (c1@(Gem col k _ _), c2@(Gem _ Rainbow _ _)) | ok c1 c2 k -> morphFrom p1 col k
  _ -> Nothing
  where
    ok a b k = (isLine k || isBombK k) && specialActivates a && specialActivates b
    morphFrom src col k =
      Just
        ( if isBombK k then "rainbow_bomb" else "rainbow_line"
        , [ (src, q, Gem col (kindFor k q) 0 Nothing)
          | r <- boardRowIndices swapped
          , c <- boardColIndices swapped
          , let q = (r, c)
          , Gem col' Normal 0 Nothing <- [at swapped q]
          , col' == col
          ]
        , rainbowClearSeeds swapped p1 p2
        )
    kindFor k (r, c)
      | isBombK k = Bomb
      | even (r + c) = LineH
      | otherwise = LineV

-- | 5×5 blast centered at pos.（炸弹 × 炸弹一端的范围）
bigBomb :: Board -> Pos -> [Pos]
bigBomb b (r, c) =
  [ (rr, cc)
  | rr <- [r - 2 .. r + 2]
  , cc <- [c - 2 .. c + 2]
  , inBounds b (rr, cc)
  ]

-- | 整行 + 整列（直线 × 直线一端的范围）。
fullRowCol :: Board -> Pos -> [Pos]
fullRowCol b (r, c) =
  [(r, cc) | cc <- boardColIndices b]
    ++ [(rr, c) | rr <- boardRowIndices b]

-- | 3 行 + 3 列（直线 × 炸弹，以炸弹端为中心）。
lineBombCross :: Board -> Pos -> [Pos]
lineBombCross b (r, c) =
  nub $
    [ (rr, cc)
    | rr <- [r - 1 .. r + 1]
    , rr >= 0 && rr < boardNRows b
    , cc <- boardColIndices b
    ]
      ++ [ (rr, cc)
         | cc <- [c - 1 .. c + 1]
         , cc >= 0 && cc < boardNCols b
         , rr <- boardRowIndices b
         ]
