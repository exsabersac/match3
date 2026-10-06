{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 打破型障碍：占格本体，被直接命中或邻格真消除时削层 / 打碎 / 变成别的元素。每种元素是一个原型值
-- （Match3.ECS.Archetype）：存储列 + 由状态派生的纯数据组件 + 自带的 system。
--
-- 共同特征：缺省原型（挡交换、不点火、会下落、洗牌保留），状态是层数或颜色；
-- 石头 / 宝箱 / 蜂蜜 / 蛋糕命中与邻消各削一层、末层消除（通用 chipHit / chipNear）；气球命中即破、邻格同色真消除打破；
-- 保险箱末层开成饼干；双面块命中翻成背面颜色的普通宝石；彩蛋命中即破，开启 system 开出直线 / 炸弹。
-- 魔法石（新玩法 2，Custom "magic_stone"）是固定格：打不动，邻格真消除充能，满 3 格后在步末发射清整行整列。
-- 雪怪 Boss（新玩法 5，Custom "snow_boss"）是占 2×2 的固定格：邻格真消除 / 直接命中扣血，血量归零整只消除；
-- 每 3 次交换在身边召唤一块雪块（1 层石头）。
-- 邻格 system 次序：石头 10 → 宝箱 20 → 蜂蜜 30 → 蛋糕 40 → 气球 50 → 保险箱 110 → 魔法石 180 → 雪怪 200。
-- 步末 system：魔法石（PhaseTick 20，倒计时之后）、雪怪（PhaseMove 30，毛球之后）。
module Match3.Element.Builtin.Obstacle
  ( stoneArch
  , chestArch
  , honeyArch
  , cakeArch
  , balloonArch
  , balloonPop
  , balloonPopLegacy
  , safeArch
  , flipArch
  , surpriseArch
  , magicStoneArch
  , magicStoneCharge
  , magicStoneFull
  , magicStoneFiring
  , magicStoneSeeds
  , snowBossArch
  , SnowBoss(..)
  , snowBossName
  , snowBossEvery
  , snowBossCells
  , snowBossColumn
  , snowBossEntity
  , snowBosses
  , snowBossHp
  , snowBossSpawn
  , decodeBoss
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Bits (xor)
import Data.List.NonEmpty (NonEmpty (..))

import Engine.Optics (Prism')
import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Element.Event (EndEffect(..), EndItem(..), EventKind(..))

import Match3.Element.Builtin.Common (boardSeed, colorField, colorPlace, nField, pickBy, plainGem, posSeed)
import Match3.ECS.Archetype
import Match3.ECS.Component
import Match3.Element.Near
import Match3.ECS.Stage
import Match3.ECS.System (System(..))
import Match3.Element.Types
import Match3.Element.Rules (chipHit, chipNear, entityDamage, nearBy)
import Match3.Obstacles
  ( balloonsAdjacentSameColor
  , openSurprises
  , orthoNeighbors
  )
import Match3.Types
import Match3.Types.Optics (_Cake, _Chest, _Honey, _Safe, _Stone)

-- | 多层障碍（状态 = 层数）：直接命中削一层、末层消除；邻消同样削层（邻格 system 'chipNear'，跳过直接命中格）。
durable :: ElementName -> Prism' Cell Int -> (Int -> Cell) -> CounterKey -> Int -> Archetype Int
durable n p con key order = (archetype n col)
  { aSpawn = layersPlace con
  , aHit = chipHit col
  , aTally = const emptyTally {tCounter = Just key}
  , aFace = \k -> baseFace (unElementName n) [nField k]
  , aSystems = [SysNear order (chipNear col)]
  }
  where
    col = prismColumn p

-- | 石头 / 宝箱 / 蜂蜜罐 / 蛋糕（层数 = 蛋糕层数）。
stoneArch, chestArch, honeyArch, cakeArch :: Archetype Int
stoneArch = durable "stone" _Stone Stone CountStones 10
chestArch = durable "chest" _Chest Chest CountChests 20
honeyArch = durable "honey" _Honey Honey CountHoney 30
cakeArch = durable "cake" _Cake Cake CountCakes 40

-- | 气球的存储列（状态 = 颜色）。
balloonColumn :: Column Color
balloonColumn = Column (\cell -> case cell of Balloon c -> Just c; _ -> Nothing) Balloon

-- | 气球：命中即破；邻格同色真消除打破（邻格 system 50）。
balloonArch :: Archetype Color
balloonArch = (archetype "balloon" balloonColumn)
  { aSpawn = colorPlace Balloon
  , aHit = const breakHit
  , aTally = const emptyTally {tCounter = Just CountBalloons}
  , aFace = \c -> baseFace "balloon" [colorField c]
  , aSystems = [SysNear 50 balloonPop]
  }

-- | 气球的邻格 system：触发格里有同色的就打破（打碎格后插，对齐旧列表序）。
balloonPop :: System NearWorld
balloonPop = nearBy balloonColumn SkipDirect DieAppend $ \ctx c ->
  if any (\(_, mc) -> mc == Just c) (ncTriggers ctx) then NearNudge Dies else NearIdle

-- | 旧气球列表序写法（对照 'balloonPop' / 性质测试）。
balloonPopLegacy :: System NearWorld
balloonPopLegacy = System $ \w -> w {nwDead = [p | p <- balloonsAdjacentSameColor (nwBoard w) (nwTrue w), p `notElem` nwDirect w], nwSit = []}

-- | 彩蛋的开启 system：前沿里的彩蛋开出直线 / 炸弹（本轮坐住）或 3×3 爆炸。
surpriseOpen :: System OpenWorld
surpriseOpen = System $ \w -> let (b, e, s) = openSurprises (owBoard w) (owFront w) in w {owBoard = b, owSeeds = e, owSits = s}

-- | 保险箱：直接命中削一层，末层开成饼干；邻消削层（邻格 system 110）；按个数差计「开启」；离格也算覆盖地毯。
safeArch :: Archetype Int
safeArch = (archetype "safe" col)
  { aSpawn = layersPlace Safe
  , aHit = absorbHit . open
  , aTally = const emptyTally {tVacatesCarpet = True}
  , aDiff = Just (DiffCount CountSafes 0)
  , aFace = \k -> baseFace "safe" [nField k]
  , aSystems = [SysNear 110 (nearBy col SkipDirect DiePrepend (\_ n -> NearNudge (Becomes (open n))))]
  }
  where
    col = prismColumn _Safe
    open n = if n <= 1 then Cookie else Safe (n - 1)

-- | 双面块（状态 = (正面, 背面)）：按正面颜色匹配、可交换 / 改色 / 推动 / 过传送门；命中翻成背面颜色的普通宝石。
flipArch :: Archetype (Color, Color)
flipArch = (archetype "flip" (Column get (uncurry Flip)))
  { aSpawn = \args _ -> exactArgs (Flip <$> argColor <*> argColor) args
  , aMatch = \(f, _) -> gemMatch (Just f)
  , aHit = \(_, b) -> absorbHit (Gem b Normal 0 Nothing)
  , aPhysics = const gemPhysics {pKeepShuffle = True}
  , aFace = \(f, b) -> baseFace "flip" [colorField f, ("b", FieldInt (fromEnum b + 1))]
  }
  where
    get cell = case cell of
      Flip f b -> Just (f, b)
      _ -> Nothing

-- | 彩蛋：占格障碍；命中即破；开启 system = 邻格真消除 / 直接命中时开出直线 / 炸弹（本轮坐住）或 3×3 爆炸。
-- 现行规则里彩蛋开一次就开出，没有要跨轮保存的状态（无状态列）。
surpriseArch :: Archetype ()
surpriseArch = (archetype "surprise" (unitColumn (== Surprise) Surprise))
  { aSpawn = \_ _ -> Just Surprise
  , aHit = const breakHit
  , aSystems = [SysOpen surpriseOpen]
  }

-- | 魔法石（新玩法 2，开心消消乐的魔法石）：占格本体 Custom "magic_stone" k，固定格（不下落、挡交换、洗牌保留、无色）。
-- 状态 k = 充能格数 0–3；4 = 发射中（只在步末那一轮存在）。
--
-- * 邻格（正交）有真消除的每一轮充能 1 格，满 3 格为止（邻格 system 'magicStoneCharge' 180；本轮被直接命中的不充能）；
-- * 玩家交换的步末（PhaseTick 20，倒计时之后）：满 3 格的魔法石转为发射中（记一条 EvTick 步末效果），
--   以它所在的整行 + 整列为种子引爆（和倒计时爆炸同一轮，种子里的特殊块照常点火、障碍照常受击）；
-- * 发射中的魔法石被自己的种子命中后归零（Absorb → 0 格），平时打不动（Immune）。
-- 道具（锤子 / 自由交换 / 十字）没有 PhaseTick 步末，充满的魔法石等到下一次交换的步末再发射。
magicStoneArch :: Archetype Int
magicStoneArch = (archetype "magic_stone" magicStoneColumn)
  { aSpawn = \args _ -> Just (colPut magicStoneColumn (maybe 0 (max 0 . min magicStoneFull) (prefixArgs argInt args)))
  , aHit = \k -> if k >= magicStoneFiring then absorbHit (colPut magicStoneColumn 0) else immuneHit
  , aPhysics = const fixedPhysics
  , aHud = noHud {hudLabel = Just "魔法石"}
  , aSystems = [SysNear 180 magicStoneCharge, SysEnd (tickSys 20 (effectSystem (magicStoneArm . ewBoard)) magicStoneSeeds)]
  }

magicStoneColumn :: Column Int
magicStoneColumn = customColumn "magic_stone"

-- | 魔法石的邻格 system：未满的充能 1 格。
magicStoneCharge :: System NearWorld
magicStoneCharge = nearBy magicStoneColumn SkipDirect DiePrepend $ \_ k ->
  NearNudge (if k < magicStoneFull then Becomes (colPut magicStoneColumn (k + 1)) else Untouched)

-- | 满格（可发射）的充能数。
magicStoneFull :: Int
magicStoneFull = 3

-- | 发射中的状态值。
magicStoneFiring :: Int
magicStoneFiring = 4

-- | 盘上魔法石的位置与状态（行优先）。
magicStones :: Board -> [(Pos, Int)]
magicStones = ifoldMap (\p cell -> [(p, k) | Custom "magic_stone" (CustomState k) <- [cell]])

-- | 步末（PhaseTick）：满格的魔法石转为发射中；记一条 EvTick "magic_stone" 效果（逐块）。
magicStoneArm :: Board -> (Maybe EndEffect, Board)
magicStoneArm b =
  let armed = [p | (p, k) <- magicStones b, k >= magicStoneFull, k < magicStoneFiring]
      cell = Custom "magic_stone" (CustomState magicStoneFiring)
      b' = foldl (\bd p -> boardSet bd p cell) b armed
  in (if null armed then Nothing else Just (EndEffect EvTick "magic_stone" [EndItem p p cell Nothing | p <- armed]), b')

-- | 发射种子：每块发射中的魔法石所在的整行 + 整列（去重；含它自己，命中后归零）。
magicStoneSeeds :: Board -> [Pos]
magicStoneSeeds b =
  let firing = [p | (p, k) <- magicStones b, k >= magicStoneFiring]
      cross (r, c) = [(r, x) | x <- boardColIndices b] ++ [(y, c) | y <- boardRowIndices b, y /= r]
  in foldr (\x acc -> if x `elem` acc then acc else x : acc) [] (concatMap cross firing)

-- | 雪怪 Boss（新玩法 5，开心消消乐的 Boss 关）：一只 Boss 占 2×2 的四格，每格本体都是 Custom "snow_boss" v，
-- v = ((满血 × 256 + 血量) × 4 + 召唤计数) × 4 + 象限（0 左上 / 1 右上 / 2 左下 / 3 右下；血量 ≤ 255）；
-- 四格的血量、满血与计数始终相同（满血只给前端画「受伤」表情，不参与规则）。
-- 固定格原型：挡交换、不下落、洗牌原地保留、无色、不进提示。
--
-- * 扣血（邻格规则 200，每轮一次）：伤害 = 本轮真消除格里与 Boss 正交相邻的格数（Boss 身外一圈 8 格）
--   + 本轮被直接命中（特效 / 道具 / 魔法石发射）的 Boss 格数；四格一起改写新血量；
-- * 直接命中时本格原样吃掉（Absorb 自身），所以锤子 / 十字可以打它，伤害在邻格规则里统一结算；
-- * 血量归零：四格一起并入本轮清除格（整只消失，上方的宝石照常落下）；
-- * 计数 CountNamed "snow_boss" 按前后盘面差计（左上格权重 = 血量、其余 0，见 'weighs'），= 本步扣掉的血；
--   关卡目标 goalCount (CountNamed "snow_boss") 满血值 =「击败 Boss」；
-- * 召唤（步末 PhaseMove 30，只在交换的步末）：计数 +1，满 'snowBossEvery' 次归零，并把身外一圈里的一颗普通宝石
--   变成雪块（1 层石头）；选哪一格由这一步步末开始时的盘面散列决定（与毛球同法，不消耗 gsGen）；
--   记一条 EvTick "snow_boss" 步末效果（四格计数变化 + 雪块格）。
data SnowBoss = SnowBoss
  { sbHp   :: Int
  , sbMax  :: Int
  , sbTurn :: Int
  , sbQuad :: Int
  }
  deriving (Eq, Show)

-- | 雪怪的存储列（四格各自打包 部件 / 血量 / 召唤计数）。
snowBossColumn :: Column SnowBoss
snowBossColumn = Column get bossToCell
  where
    get cell = case cell of
      Custom n s | n == snowBossName -> Just (decodeBoss s)
      _ -> Nothing

snowBossArch :: Archetype SnowBoss
snowBossArch = (archetype snowBossName snowBossColumn)
  { aSpawn = \args _ -> do
      (hp, q) <- exactArgs ((,) <$> argInt <*> argInt) args
      guard (hp > 0 && hp <= 255 && q >= 0 && q < 4)
      Just (bossToCell (SnowBoss hp hp 0 q))
  , aMatch = const (Match Nothing True True False)
  , aHit = absorbHit . bossToCell
  , aPhysics = const fixedPhysics
  , aTally = \b -> emptyTally {tDiffWeight = if sbQuad b == 0 then sbHp b else 0}
  , aDiff = Just (DiffCount (CountNamed snowBossName) 0)
  , aHud = noHud
      { hudLabel = Just "雪怪"
      , hudLoseHint = Just (LoseHint "用身边的消除和特效打雪怪，目标 " " 点血")
      , hudShowsHp = True
      }
  , aFace = \b -> noFace
      { fExtras =
          [ ("q", FaceInt (sbQuad b))
          , ("hurt", FaceBool (sbHp b * 2 <= sbMax b))
          , ("turn", FaceInt (sbTurn b))
          , ("every", FaceInt snowBossEvery)
          ]
      }
    -- 2×2 多格实体：扣血 system（次序 200），之后是步末召唤。
  , aSystems = [SysNear 200 (entityDamage snowBossColumn snowBossEntity), SysEnd (moveSys 30 (effectSystem snowBossRun))]
  }

-- | 2×2 多格实体：锚点 = 0 号部件，footprint = 'snowBossCells'；由 'aSystems' 里的 'entityDamage' 消费。
snowBossEntity :: Entity SnowBoss
snowBossEntity = Entity
  { footprint = snowBossCells
  , partNo = sbQuad
  , hitPoints = sbHp
  , withHp = \hp b -> b {sbHp = hp}
  }

snowBossName :: ElementName
snowBossName = "snow_boss"

-- | 写回格子（'decodeBoss' 的逆；各字段截到编码范围内）。
bossToCell :: SnowBoss -> Cell
bossToCell (SnowBoss hp mx t q) = Custom snowBossName (CustomState (((clamp mx * 256 + clamp hp) * 4 + t `mod` 4) * 4 + q `mod` 4))
  where
    clamp = max 0 . min 255

-- | 每隔几次交换召唤一块雪块。
snowBossEvery :: Int
snowBossEvery = 3

-- | 由格子状态解码（'bossToCell' 的逆）。
decodeBoss :: CustomState -> SnowBoss
decodeBoss (CustomState v) = SnowBoss ((v `div` 16) `mod` 256) (v `div` 4096) ((v `div` 4) `mod` 4) (v `mod` 4)

bossAt :: Board -> Pos -> Maybe SnowBoss
bossAt b p = case getCell b p of
  Custom n s | n == snowBossName -> Just (decodeBoss s)
  _ -> Nothing

-- | 一只 Boss 的四格（左上角 → 左上、右上、左下、右下）。
snowBossCells :: Pos -> [Pos]
snowBossCells (r, c) = [(r, c), (r, c + 1), (r + 1, c), (r + 1, c + 1)]

-- | 盘上的 Boss：(左上角, 左上格的状态)（行优先；左上格 = 象限 0 的格）。
snowBosses :: Board -> [(Pos, SnowBoss)]
snowBosses b = [(p, s) | p <- boardPositions b, Just s <- [bossAt b p], sbQuad s == 0]

-- | 盘上全部 Boss 的剩余血量之和（HUD 血条）。
snowBossHp :: Board -> Int
snowBossHp = sum . map (sbHp . snd) . snowBosses

-- | 一只 Boss 在盘上真实存在的格（象限对得上的）。
bossParts :: Board -> Pos -> [(Pos, SnowBoss)]
bossParts b anchor = [(p, s) | (q, p) <- zip [0 ..] (snowBossCells anchor), inBounds b p, Just s <- [bossAt b p], sbQuad s == q]

-- | Boss 身外一圈：与四格正交相邻、本身不是这四格的格（行优先去重）。
bossRing :: Board -> Pos -> [Pos]
bossRing b anchor =
  let body = snowBossCells anchor
  in foldr (\q acc -> if q `elem` acc then acc else q : acc) [] [q | x <- body, q <- orthoNeighbors x, inBounds b q, q `notElem` body]

-- | 召唤选格（纯函数，测试直接调用）：避让格 / 墙之外、身外一圈里的普通宝石（无冰无叠层）按盘面散列选一格
-- （'boardSeed' 依赖 @show board@，见 Element.Builtin.Common）。
snowBossSpawn :: [Pos] -> [Pos] -> Board -> Pos -> Maybe Pos
snowBossSpawn avoid walls b anchor =
  case [q | q <- bossRing b anchor, q `notElem` avoid, q `notElem` walls, plainGem (getCell b q)] of
    [] -> Nothing
    c : cs -> Just (pickBy (boardSeed b `xor` posSeed anchor) (c :| cs))

-- | 步末：每只 Boss 召唤计数 +1，满了归零并召唤雪块；记一条 EvTick（四格 + 雪块格）。
snowBossRun :: EndWorld -> (Maybe EndEffect, Board)
snowBossRun ctx =
  let b0 = ewBoard ctx in
  let (items, b') = foldl one ([], b0) (snowBosses b0)
  in (if null items then Nothing else Just (EndEffect EvTick snowBossName items), b')
  where
    one (acc, b) (anchor, s) =
      let t' = sbTurn s + 1
          full = t' >= snowBossEvery
          parts = bossParts b anchor
          bossItems = [(p, bossToCell x {sbTurn = if full then 0 else t'}) | (p, x) <- parts]
          spawn = if full then snowBossSpawn (ewAvoid ctx) (ewWalls ctx) b anchor else Nothing
          snow = [(q, Stone 1) | Just q <- [spawn]]
          changes = [(p, cell) | (p, cell) <- bossItems ++ snow, getCell b p /= cell]
          b1 = foldl (\bd (p, cell) -> setCell bd p cell) b changes
      in (acc ++ [EndItem p p cell Nothing | (p, cell) <- changes], b1)

-- | 放置：层数（缺省 1，至少 1）。精确匹配：只接受 @[]@ 或 @[AInt n]@。
layersPlace :: (Int -> Cell) -> Placer
layersPlace con args _ = con <$> exactArgs (max 1 <$> argInt <|> pure 1) args

--------------------------------------------------------------------------------
-- 条目（注册项的分派编号由 Registry 的解码探针推导）
