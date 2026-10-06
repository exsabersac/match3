{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- | ECS 的原型（archetype）与存储列：一种本体元素 = 一个 'Archetype' 值（ecs-3 起取代 Phase 类 + Codec）。
--
-- 存储：实体 = 盘面上的格（Pos），'Cell' 是一行**打包的组件**（存储编码不变：boardSeed 与快照依赖它的派生 Show）。
-- 原型只做两件事：
--
-- 1. 存储列 'Column'：这一行是不是我的（'colGet'）、我的类型化状态 s 怎么写回行（'colPut'）；
-- 2. 由状态派生纯数据组件（Match3.ECS.Component）：'aMatch' / 'aHit' / 'aPhysics' / 'aTally' / 'aFace'。
--
-- 原型不含行为：邻格反应、步末行动、成对交换、开启等元素特有逻辑是它带来的 system（'aSystems'，Match3.ECS.Stage），
-- 与其他 system 一样按阶段、按次序注册进调度表。
--
-- 查询按组件类型（'Component' 类做索引）：解码出的一行 'Row' 上 @rowGet \@Match row@、@rowGet \@Physics row@ …
module Match3.ECS.Archetype
  ( -- * 存储列
    Column(..)
  , prismColumn
  , customColumn
  , unitColumn
  , query
    -- * 原型
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
    -- * 多格实体
  , Entity(..)
  ) where

import Engine.Optics (Prism', preview, review)
import Match3.ECS.Component
import Match3.ECS.Stage (SysDef)
import Match3.Element.Types (Placer)
import Match3.Types (Board, Cell, CellContents(..), CustomState(..), ElementName(..), Pos, ifoldMap)

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

-- | 无状态的列：只认一种格子（饼干、魔法帽、彩蛋…）。
unitColumn :: (Cell -> Bool) -> Cell -> Column ()
unitColumn isMine cell = Column (\c -> if isMine c then Just () else Nothing) (const cell)

-- | 按列查询整盘：本原型的全部实体与状态（行优先）。
query :: Column s -> Board -> [(Pos, s)]
query col = ifoldMap (\p cell -> maybe [] (\s -> [(p, s)]) (colGet col cell))

-- | 一种本体元素的原型（组件派生 + 自带 system；见模块头）。
data Archetype s = Archetype
  { aName :: ElementName       -- ^ 元素名（注册表的键）
  , aColumn :: Column s        -- ^ 存储列
  , aSpawn :: Placer           -- ^ 关卡放置（参数解析，属于存储层）
  , aMatch :: s -> Match
  , aHit :: s -> OnHit
  , aPhysics :: s -> Physics
  , aTally :: s -> Tally
  , aFace :: s -> Face
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
  , aMatch = const obstacleMatch
  , aHit = const immuneHit
  , aPhysics = const obstaclePhysics
  , aTally = const emptyTally
  , aFace = const noFace
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
  component = aMatch

instance Component OnHit where
  component = aHit

instance Component Physics where
  component = aPhysics

instance Component Tally where
  component = aTally

instance Component Face where
  component = aFace

-- | 一行上的某个组件：@rowGet \@Physics row@。
rowGet :: Component c => Row -> c
rowGet (Row a s) = component a s
{-# INLINE rowGet #-}

-- | 多格实体（雪怪 Boss）：各部件仍是独立的格实体，记录告诉扣血 system 锚点怎么认（'partNo' == 0）、部件在哪
-- （'footprint'，第 i 项 = 第 i 号部件）、血量怎么读写。扣血由 Match3.Element.Rules.entityDamage 统一算。
data Entity s = Entity
  { footprint :: Pos -> [Pos]
  , partNo :: s -> Int
  , hitPoints :: s -> Int
  , withHp :: Int -> s -> s
  }
