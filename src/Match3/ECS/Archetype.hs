{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- | ECS 的原型（archetype）与存储列：一种本体元素 = 一个 'Archetype' 值（ecs-3 起取代 Phase 类 + Codec）。
--
-- 存储：实体 = 盘面上的格（Pos），'Cell' 是一行**打包的组件**（存储编码不变：boardSeed 与快照依赖它的派生 Show）。
-- 原型只做两件事：
--
-- 1. 存储列 'Column'：这一行是不是我的（'colGet'）、我的类型化状态 s 怎么写回行（'colPut'）；
-- 2. 由状态派生纯数据组件（Match3.ECS.Component）：'aMatch' / 'aTally' / 'aFace'（配方 'aFaceKind'）；'aPhysics' 是纯数据（与状态无关）；
--    命中是配方 'aHitKind'（'HitKind' GADT，system / 'resolveHit' 解释成 'OnHit'）。
--
-- 原型不含行为：邻格反应、步末行动、成对交换、开启等元素特有逻辑是它带来的 system（'aSystems'，Match3.ECS.Stage），
-- 与其他 system 一样按阶段、按次序注册进调度表。
--
-- 仍含函数的例外（ecs-12）：'Column' / 'Placer'（存储编解码与关卡放置，本质是仿射映射）；
-- 'Entity'（多格实体的足迹 / 部件号 / 血量读写）；'MatchDerive' / 'TallyDerive' / 'FaceDerive'
-- （测试与少数不规则显示的逃生口）。生产路径的命中 / 匹配 / 计数 / 显示配方已纯数据化。
--
-- 查询按组件类型（'Component' 类做索引）：解码出的一行 'Row' 上 @rowGet \@Match row@、@rowGet \@Physics row@ …
module Match3.ECS.Archetype
  ( -- * 存储列
    Column(..)
  , prismColumn
  , customColumn
  , intColumn
  , unitColumn
  , query
    -- * 原型
  , HitKind(..)
  , ChipEnd(..)
  , resolveHit
  , aHit
  , MatchKind(..)
  , resolveMatch
  , aMatch
  , TallyKind(..)
  , resolveTally
  , aTally
  , FaceKind(..)
  , resolveFace
  , aFace
  , Archetype(..)
  , archetype
  , SomeArchetype(..)
  , archName
  , inertArch
    -- * 解码出的一行与按组件类型查询
  , Row(..)
  , rowName
  , rowCell
  , Component(..)
  , rowGet
  , rowProbe
    -- * 多格实体
  , Entity(..)
  ) where

import Data.Coerce (Coercible, coerce)
import Engine.Optics (Prism', preview, review)
import Match3.ECS.Component
import Match3.ECS.Stage (SysDef)
import Match3.Element.Types (CellField(..), FaceValue(..), Placer)
import Match3.Types (Board, Cell, CellContents(..), Color(..), CustomState(..), ElementName(..), GemKind(..), Pos, colorAt, gridFromRows, ifoldMap)

-- | 存储列：一行（'Cell'）↔ 类型化状态 s 的仿射编解码（并非每行都有这一列）。
data Column s = Column
  { colGet :: Cell -> Maybe s  -- ^ 这一行属于本原型时读出状态
  , colPut :: s -> Cell        -- ^ 状态写回行
  }

-- | 由棱镜得到的列（@prismColumn _Stone@：状态 = 层数）。
prismColumn :: Prism' Cell s -> Column s
prismColumn p = Column (preview p) (review p)

-- | 自定义本体的列：@Custom 名字 状态值@（状态 = 整数）。
customColumn :: ElementName -> Column Int
customColumn n = Column get (Custom n . CustomState)
  where
    get cell = case cell of
      Custom n' (CustomState k) | n' == n -> Just k
      _ -> Nothing

-- | Int newtype 状态的自定义本体列（'Coercible'：状态表示不是 Int 的类型用它就是类型错误）。
intColumn :: Coercible s Int => ElementName -> Column s
intColumn n = Column (fmap coerce . colGet (customColumn n)) (colPut (customColumn n) . coerce)

-- | 无状态的列：只认一种格子（饼干、魔法帽、彩蛋…）。
unitColumn :: (Cell -> Bool) -> Cell -> Column ()
unitColumn isMine cell = Column (\c -> if isMine c then Just () else Nothing) (const cell)

-- | 按列查询整盘：本原型的全部实体与状态（行优先）。
query :: Column s -> Board -> [(Pos, s)]
query col = ifoldMap (\p cell -> maybe [] (\s -> [(p, s)]) (colGet col cell))


-- | 多格实体（雪怪 Boss）：各部件仍是独立的格实体，记录告诉扣血 system 锚点怎么认（'partNo' == 0）、部件在哪
-- （'footprint'，第 i 项 = 第 i 号部件）、血量怎么读写。扣血由 Match3.Element.Rules.entityDamage 统一算。
-- 字段仍是函数（ecs-12：足迹形状与血量投影随状态类型而变，抽成纯数据表会绑死布局）；见模块头例外。
data Entity s = Entity
  { footprint :: Pos -> [Pos]
  , partNo :: s -> Int
  , hitPoints :: s -> Int
  , withHp :: Int -> s -> s
  }

-- | 命中配方（纯数据；ecs-9 / ecs-12）：原型挂配方，'resolveHit' / 查询时解释成运行时 'OnHit'（可带具体 Absorb 格）。
-- 全部构造子都不含函数（ecs-12 删掉未使用的 'HitDerive'）。
data ChipEnd
  = ChipVanish       -- ^ 末层 'Destroy'
  | ChipBecome Cell  -- ^ 末层 'Absorb' 成给定格（保险箱 → 饼干）
  deriving (Eq, Show)

data HitKind s where
  HitBreak :: Bool -> Maybe Blast -> HitKind s
    -- ^ 命中即消；参数 = 能否点火、爆炸范围
  HitImmune :: HitKind s
  HitChip :: ChipEnd -> HitKind Int
    -- ^ 多层障碍：层数 > 1 削一层，末层按 'ChipEnd'
  HitHold :: HitKind s
    -- ^ 吃掉命中、本格写成当前状态（雪怪各部件被打后写回自己）
  HitAtLeast :: Int -> HitKind Int -> HitKind Int -> HitKind Int
    -- ^ 状态 ≥ n 用前者，否则用后者（魔法石发射中归零 / 平时打不动）
  HitSet :: Int -> HitKind Int
    -- ^ 吃掉命中、写成给定整数状态（魔法石发射后归零）
  HitFlip :: HitKind (Color, Color)
    -- ^ 翻成背面颜色的普通宝石

-- | 把命中配方解释成运行时组件（需要存储列：'HitChip' / 'HitHold' / 'HitAtLeast' / 'HitSet' 要写回格子）。
resolveHit :: Column s -> HitKind s -> s -> OnHit
resolveHit col kind s = case kind of
  HitBreak fires blast -> OnHit Destroy fires blast
  HitImmune -> immuneHit
  HitChip end -> case end of
    ChipVanish | s <= 1 -> breakHit
               | otherwise -> absorbHit (colPut col (s - 1))
    ChipBecome cell | s <= 1 -> absorbHit cell
                    | otherwise -> absorbHit (colPut col (s - 1))
  HitHold -> absorbHit (colPut col s)
  HitAtLeast n a b -> resolveHit col (if s >= n then a else b) s
  HitSet n -> absorbHit (colPut col n)
  HitFlip -> let (_, back) = s in absorbHit (Gem back Normal 0 Nothing)


-- | 匹配配方（ecs-9 / ecs-12）：多数固定或「状态即颜色」；变色龙用 'MatchColorAt'；'MatchDerive' 仅逃生口。
data MatchKind s where
  MatchFixed :: Match -> MatchKind s
  MatchColor :: Bool -> MatchKind Color
    -- ^ 状态 = 颜色；参数 = 是否进提示（彩虹 = False）
  MatchColorAt :: Bool -> MatchKind Int
    -- ^ 状态 = 颜色下标（'colorAt'）；变色龙
  MatchFront :: MatchKind (Color, a)
    -- ^ 双面块：按正面颜色匹配
  MatchHintable :: MatchKind Bool
    -- ^ 占格障碍匹配，'mHintable' = 状态（测试同名替换用）
  MatchDerive :: (s -> Match) -> MatchKind s
    -- ^ 逃生口（测试里状态 = 原格取色等）

resolveMatch :: MatchKind s -> s -> Match
resolveMatch kind s = case kind of
  MatchFixed m -> m
  MatchColor hint -> (gemMatch (Just s)) {mHintable = hint}
  MatchColorAt hint -> (gemMatch (Just (colorAt s))) {mHintable = hint}
  MatchFront -> let (f, _) = s in gemMatch (Just f)
  MatchHintable -> obstacleMatch {mHintable = s}
  MatchDerive f -> f s

-- | 计数配方：多数固定；多格实体锚点加权用 'TallyEntityAnchor'；'TallyDerive' 仅逃生口。
data TallyKind s where
  TallyFixed :: Tally -> TallyKind s
  TallyEntityAnchor :: Entity s -> TallyKind s
    -- ^ 锚点（'partNo' == 0）权重 = 'hitPoints'，其余 0（雪怪）
  TallyDerive :: (s -> Tally) -> TallyKind s
    -- ^ 逃生口

resolveTally :: TallyKind s -> s -> Tally
resolveTally (TallyFixed t) _ = t
resolveTally (TallyEntityAnchor ent) s =
  emptyTally {tDiffWeight = if partNo ent s == 0 then hitPoints ent s else 0}
resolveTally (TallyDerive f) s = f s

-- | 显示配方（ecs-12）：多数是固定标签 / 层数 / 颜色字段；雪怪与扩展灯笼等用 'FaceDerive'。
data FaceKind s where
  FaceFixed :: Face -> FaceKind s
  FaceN :: String -> FaceKind Int
    -- ^ 标签 + 层数字段 n（石头 / 保险箱…）
  FaceTaggedColor :: String -> FaceKind Color
    -- ^ 标签 + 颜色字段 c（气球 / 染色瓶）
  FaceColorN :: String -> FaceKind (Color, Int)
    -- ^ 标签 + c + n（果汁机 / 倒计时）
  FaceDirs :: String -> FaceKind (Int, Int)
    -- ^ 标签 + dr / dc（蜗牛）
  FaceFlipShow :: FaceKind (Color, Color)
    -- ^ 双面块：正面色 + 背面编号
  FaceChameleon :: FaceKind Int
    -- ^ 附加字段 c = 'colorAt' 状态
  FaceDerive :: (s -> Face) -> FaceKind s
    -- ^ 逃生口（雪怪 hurt 表情、扩展灯笼 lit…）

resolveFace :: FaceKind s -> s -> Face
resolveFace kind s = case kind of
  FaceFixed f -> f
  FaceN tag -> baseFace tag [("n", FieldInt s)]
  FaceTaggedColor tag -> baseFace tag [("c", FieldInt (fromEnum s + 1))]
  FaceColorN tag -> let (c, k) = s in baseFace tag [("c", FieldInt (fromEnum c + 1)), ("n", FieldInt k)]
  FaceDirs tag -> let (dr, dc) = s in baseFace tag [("dr", FieldInt dr), ("dc", FieldInt dc)]
  FaceFlipShow -> let (f, b) = s in baseFace "flip" [("c", FieldInt (fromEnum f + 1)), ("b", FieldInt (fromEnum b + 1))]
  FaceChameleon -> noFace {fExtras = [("c", FaceColor (colorAt s))]}
  FaceDerive f -> f s

-- | 兼容读法：原型在状态下的命中组件（= 'resolveHit' 列 配方 状态）。
aHit :: Archetype s -> s -> OnHit
aHit a = resolveHit (aColumn a) (aHitKind a)

aMatch :: Archetype s -> s -> Match
aMatch a = resolveMatch (aMatchKind a)

aTally :: Archetype s -> s -> Tally
aTally a = resolveTally (aTallyKind a)

aFace :: Archetype s -> s -> Face
aFace a = resolveFace (aFaceKind a)

-- | 一种本体元素的原型（组件派生 + 自带 system；见模块头）。
data Archetype s = Archetype
  { aName :: ElementName       -- ^ 元素名（注册表的键）
  , aColumn :: Column s        -- ^ 存储列
  , aSpawn :: Placer           -- ^ 关卡放置（参数解析，属于存储层）
  , aMatchKind :: MatchKind s        -- ^ 匹配配方（见 'MatchKind'）
  , aHitKind :: HitKind s           -- ^ 命中配方（纯数据；见 'HitKind'）
  , aPhysics :: Physics              -- ^ 物理组件（与状态无关；ecs-9 起是纯数据）
  , aTallyKind :: TallyKind s        -- ^ 计数配方（见 'TallyKind'）
  , aFaceKind :: FaceKind s          -- ^ 显示配方（见 'FaceKind'）
  , aDiff :: Maybe DiffCount   -- ^ 原型级：按个数差计数
  , aHud :: Hud                -- ^ 原型级 HUD 数据
  , aSystems :: [SysDef]       -- ^ 本元素带来的 system（邻格 / 步末 / 交换 / 开启）
  }

-- | 缺省原型（惰性占格：挡匹配 / 交换、无色、打不动、会下落、洗牌保留、不计数、无显示、无 system、不能放置）。
-- 元素用记录更新改掉自己不同的组件。
archetype :: ElementName -> Column s -> Archetype s
archetype n col = Archetype
  { aName = n
  , aColumn = col
  , aSpawn = \_ _ -> Nothing
  , aMatchKind = MatchFixed obstacleMatch
  , aHitKind = HitImmune
  , aPhysics = obstaclePhysics
  , aTallyKind = TallyFixed emptyTally
  , aFaceKind = FaceFixed noFace
  , aDiff = Nothing
  , aHud = noHud
  , aSystems = []
  }

-- | 装箱的原型（注册表里的一项）。
data SomeArchetype = forall s. SomeArchetype (Archetype s)

archName :: SomeArchetype -> ElementName
archName (SomeArchetype a) = aName a

-- | 惰性占格：未注册的 Custom 名字、没有原型认领的格子的兜底（状态 = 原格，写回原格）。
inertArch :: ElementName -> Archetype Cell
inertArch n = archetype n (Column (const Nothing) id)

-- | 解码出的一行：认领它的原型 + 类型化状态。
data Row = forall s. Row (Archetype s) s

rowName :: Row -> ElementName
rowName (Row a _) = aName a

-- | 写回的格子（= 原型的 'colPut'）。
rowCell :: Row -> Cell
rowCell (Row a s) = colPut (aColumn a) s

-- | 可按类型查询的组件（存储索引）：原型怎样由状态派生它。
class Component c where
  component :: Archetype s -> s -> c

instance Component Match where
  component a s = resolveMatch (aMatchKind a) s

instance Component OnHit where
  component a s = resolveHit (aColumn a) (aHitKind a) s

instance Component Physics where
  component a _ = aPhysics a

instance Component Tally where
  component a s = resolveTally (aTallyKind a) s

instance Component Face where
  component a s = resolveFace (aFaceKind a) s

-- | 一行上的某个组件：@rowGet \@Physics row@。
rowGet :: Component c => Row -> c
rowGet (Row a s) = component a s
{-# INLINE rowGet #-}

-- | 一行全部组件的读数（透明性测试 / 调试用；爆炸范围在 8×8 宝石盘的 (3,3) 上解释）。
rowProbe :: Row -> [(String, String)]
rowProbe row =
  [ ("name", show (rowName row))
  , ("toCell", show (rowCell row))
  , ("mColor", show (mColor m))
  , ("mBlockMatch", show (mBlockMatch m))
  , ("mBlockSwap", show (mBlockSwap m))
  , ("mHintable", show (mHintable m))
  , ("hStrike", show (hStrike h))
  , ("hFires", show (hFires h))
  , ("hBlast", show (fmap (\bl -> blastArea bl probeBoard (3, 3)) (hBlast h)))
  , ("physics", show (rowGet row :: Physics))
  , ("tally", show (rowGet row :: Tally))
  , ("fBase", show (fBase f))
  , ("fExtras", show (fExtras f))
  ]
  where
    m = rowGet row :: Match
    h = rowGet row :: OnHit
    f = rowGet row :: Face
    probeBoard = gridFromRows (replicate 8 (replicate 8 (Gem C1 Normal 0 Nothing)))

