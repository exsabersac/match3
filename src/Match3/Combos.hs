-- | 特殊×特殊合成：Line×Bomb、Rainbow×Line、Bomb×Bomb、Line×Line。第 8 刀起组合效果是一张有序的组合表
-- （'builtinComboRules'，解释器见 Match3.Element.Special；注册表 Registry.comboRules 持有、并成一条成对交换规则），
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
    -- * 爆炸几何
  , bigBomb
  , fullRowCol
  , lineBombCross
  ) where

import Data.List (nub)
import Match3.Element.Special (comboFires, comboSeedsFor)
import Match3.Element.Types (ComboRule(..))
import Match3.Rainbow (rainbowClearSeeds)
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

-- | 5×5 blast centered at pos.（炸弹 × 炸弹一端的范围）
bigBomb :: Board -> Pos -> [Pos]
bigBomb _ (r, c) =
  [ (rr, cc)
  | rr <- [r - 2 .. r + 2]
  , cc <- [c - 2 .. c + 2]
  , rr >= 0
  , rr < boardSize
  , cc >= 0
  , cc < boardSize
  ]

-- | 整行 + 整列（直线 × 直线一端的范围）。
fullRowCol :: Board -> Pos -> [Pos]
fullRowCol _ (r, c) =
  [(r, cc) | cc <- [0 .. boardSize - 1]]
    ++ [(rr, c) | rr <- [0 .. boardSize - 1]]

-- | 3 行 + 3 列（直线 × 炸弹，以炸弹端为中心）。
lineBombCross :: Board -> Pos -> [Pos]
lineBombCross _ (r, c) =
  nub $
    [ (rr, cc)
    | rr <- [r - 1 .. r + 1]
    , rr >= 0
    , rr < boardSize
    , cc <- [0 .. boardSize - 1]
    ]
      ++ [ (rr, cc)
         | cc <- [c - 1 .. c + 1]
         , cc >= 0
         , cc < boardSize
         , rr <- [0 .. boardSize - 1]
         ]
