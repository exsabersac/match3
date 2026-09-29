-- | 元素类原型（阶段 1）：宝石（全用默认方法）、彩蛋、冰层（修饰器）三种走新机制（Match3.Element.Class），
-- 经适配层（'bridgeBody' / 'bridgeModifier'）桥接成旧的 ElementDef 记录，与其余旧记录并存。
--
-- 依赖：Element.Class / Types、Obstacles（彩蛋开启）、Combos / Rainbow（成对交换规则）。
module Match3.Element.Prototype
  ( -- * 新机制的元素
    PlainGem(..)
  , SpecialGem(..)
  , SurpriseEgg(..)
  , Ice(..)
    -- * 解码（格子 → 元素值）
  , decodePlainGem
  , decodeSpecialGem
  , decodeSurprise
  , decodeIce
    -- * 适配层
  , bridgeBody
  , bridgeModifier
  , prototypeDefs
  , specialBlast
  ) where

import Data.Maybe (fromMaybe)
import Match3.Board.Grid (inBounds)
import Match3.Combos (comboClearSeeds, isSpecialCombo)
import Match3.Element.Class
import Match3.Element.Types
import Match3.Obstacles (openSurprises)
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import Match3.Types

--------------------------------------------------------------------------------
-- 元素

-- | 普通宝石：除名字和写回格子外全用默认方法（原型 Piece：可交换、能点火、命中即消、洗牌重排……；
-- 颜色缺省取自写回的格子）。
newtype PlainGem = PlainGem Color
  deriving (Eq, Show)

instance Element PlainGem where
  name _ = "gem"
  toCell (PlainGem c) = Gem c Normal 0 Nothing

-- | 特殊块（直线 / 炸弹 / 彩虹）：洗牌保留；直线与炸弹有爆炸范围；彩虹 / 直线各挂一条成对交换规则。
data SpecialGem = SpecialGem Color GemKind
  deriving (Eq, Show)

instance Element SpecialGem where
  name (SpecialGem _ k) = case k of
    LineH -> "line_h"
    LineV -> "line_v"
    Bomb -> "bomb"
    Rainbow -> "rainbow"
    Normal -> "gem"
  toCell (SpecialGem c k) = Gem c k 0 Nothing
  keepOnShuffle (SpecialGem _ k) = k /= Normal
  blast (SpecialGem _ k) = specialBlast k
  -- 彩虹不进普通匹配提示（只经成对交换规则给提示）
  hintable (SpecialGem _ k) = k /= Rainbow
  swap (SpecialGem _ k) = case k of
    -- 彩虹取色：由交换对象决定清哪种颜色；先于特殊合成判定
    Rainbow -> Just (SwapRule 10 isRainbowSwap rainbowClearSeeds)
    -- 特殊 × 特殊合成：挂在直线上，规则本身检查两端（直线 / 炸弹 / 彩虹的组合）
    LineH -> Just (SwapRule 20 isSpecialCombo comboClearSeeds)
    _ -> Nothing

-- | 按种类的爆炸范围。
specialBlast :: GemKind -> Maybe (Pos -> [Pos])
specialBlast k = case k of
  LineH -> Just (\(r, _) -> [(r, c) | c <- [0 .. boardSize - 1]])
  LineV -> Just (\(_, c) -> [(r, c) | r <- [0 .. boardSize - 1]])
  Bomb -> Just (\(r, c) -> [(rr, cc) | rr <- [r - 1 .. r + 1], cc <- [c - 1 .. c + 1], inBounds (rr, cc)])
  _ -> Nothing

-- | 彩蛋：占格障碍；命中即破；开启规则 = 邻格真消除 / 直接命中时开出直线 / 炸弹（本轮坐住）或 3×3 爆炸。
-- 现行规则里彩蛋开一次就开出，没有要跨轮保存的状态（GameState 里也没有彩蛋专用字段），所以值是无字段的。
data SurpriseEgg = SurpriseEgg
  deriving (Eq, Show)

instance Element SurpriseEgg where
  name _ = "surprise"
  toCell _ = Surprise
  archetype _ = Blocker
  onHit _ = Destroy
  open _ = Just (OpenRule openSurprises)

-- | 冰层（修饰器）：不挡匹配 / 交换；多层冰只削一层（不点火），末层冰随宝石一起碎（并点火）。
newtype Ice = Ice Int
  deriving (Eq, Show)

instance Modifier Ice where
  modName _ = "ice"
  modApply (Ice n) cell = case cell of
    Gem c k _ ov -> Gem c k n ov
    _ -> cell
  modActivates (Ice n) = Just (n <= 1)
  modOnHit (Ice n)
    | n > 1 = Keep (Ice (n - 1))
    | otherwise = Shatter

--------------------------------------------------------------------------------
-- 解码

decodePlainGem :: Cell -> Maybe PlainGem
decodePlainGem cell = case cell of
  Gem c Normal _ _ -> Just (PlainGem c)
  _ -> Nothing

decodeSpecialGem :: Cell -> Maybe SpecialGem
decodeSpecialGem cell = case cell of
  Gem c k _ _ | k /= Normal -> Just (SpecialGem c k)
  _ -> Nothing

decodeSurprise :: Cell -> Maybe SurpriseEgg
decodeSurprise cell = case cell of
  Surprise -> Just SurpriseEgg
  _ -> Nothing

-- | 冰层：宝石格的冰层数（与旧定义一样对任意宝石格作答；注册表只在冰层 > 0 时问它）。
decodeIce :: Cell -> Maybe Ice
decodeIce cell = case cell of
  Gem _ _ n _ -> Just (Ice n)
  _ -> Nothing

--------------------------------------------------------------------------------
-- 适配层：新机制的元素 → 旧 ElementDef 记录

-- | 本体元素的桥：按原型值取类型级字段，按格子解码出的元素值取逐格字段；解码不出时按原型值作答。
bridgeBody :: Element e => Slot -> e -> (Cell -> Maybe e) -> ([Arg] -> Cell -> Maybe Cell) -> ElementDef
bridgeBody sl proto decode place =
  (baseDef (name proto))
    { edSlot = sl
    , edColor = \cell -> decode cell >>= color
    , edBlocksSwap = blocksSwap proto
    , edActivates = activates . dec
    , edFalls = falls proto
    , edPortal = portal proto
    , edDrains = drains proto
    , edOnHit = \cell -> case onHit (dec cell) of
        Absorb e' -> HitAbsorb (toCell e')
        Destroy -> HitDestroy
        Immune -> HitImmune
    , edAdjacent = adjacent proto
    , edCounter = counter proto
    , edDiffCounter = diffCounter proto
    , edBonusMoves = bonusMoves proto
    , edVacatesCarpet = vacatesCarpet proto
    , edKeepOnShuffle = keepOnShuffle . dec
    , edBlast = blast proto
    , edEnd = end proto
    , edPlace = place
    , edGround = ground proto
    , edSwap = swap proto
    , edOpen = open proto
    , edRecolorable = recolorable proto
    , edPushable = pushable proto
    }
  where
    dec = fromMaybe proto . decode

-- | 修饰器的桥：解码不出（不是这一层）时穿透。strip = 揭掉本层后的格子（修饰器返回 Remove 时用）。
bridgeModifier :: Modifier m => Slot -> m -> (Cell -> Maybe m) -> (Cell -> Cell) -> ([Arg] -> Cell -> Maybe Cell) -> ElementDef
bridgeModifier sl proto decode strip place =
  (baseDef (modName proto))
    { edSlot = sl
    , edBlocksMatch = modBlocksMatch proto
    , edBlocksSwap = modBlocksSwap proto
    , edActivates = \cell -> decode cell >>= modActivates
    , edOnHit = \cell -> case modOnHit <$> decode cell of
        Nothing -> HitPierce
        Just Pierce -> HitPierce
        Just (Keep m') -> HitAbsorb (modApply m' cell)
        Just Remove -> HitAbsorb (strip cell)
        Just Shatter -> HitDestroy
    , edAdjacent = modAdjacent proto
    , edStripOnClear = modStripOnClear proto
    , edEnd = modEnd proto
    , edPlace = place
    }

-- | 走新机制的内置定义：5 种宝石、冰层、彩蛋（名字 / 槽位与旧定义相同，注册表按名字替换旧记录）。
prototypeDefs :: [ElementDef]
prototypeDefs =
  [ bridgeBody (SlotCell (kindSlot Normal)) (PlainGem C1) decodePlainGem noPlace
  , special LineH
  , special LineV
  , special Bomb
  , special Rainbow
  , bridgeModifier SlotIce (Ice 1) decodeIce id $ \args cell -> case (args, cell) of
      ([AInt n], Gem col kind _ ov) -> Just (Gem col kind n ov)
      _ -> Nothing
  , bridgeBody (SlotCell 16) SurpriseEgg decodeSurprise (\_ _ -> Just Surprise)
  ]
  where
    noPlace _ _ = Nothing
    special k = bridgeBody (SlotCell (kindSlot k)) (SpecialGem C1 k) decodeSpecialGem noPlace
