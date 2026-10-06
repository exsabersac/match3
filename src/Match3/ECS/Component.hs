-- | ECS 的组件：格子实体上挂的**纯数据**（ecs-3 起取代 Phase 类的 onMatch / onHit / physics / liveMeta / view 方法）。
--
-- 每种组件只是一条记录，不含行为；行为全在 system 里（Match3.ECS.Stage 的各阶段 system，以及注册表的查询）。
-- 原型（Match3.ECS.Archetype）负责把一行存储（'Cell'）解码成类型化状态，再由状态派生这些组件。
--
-- * 'Match'   —— 颜色、挡匹配、挡交换、进提示（原 MatchRule / onMatch）；
-- * 'Physics' —— 固定、下落、过门、可改色、可推、洗牌保留、收集边（原 physics）；
-- * 'OnHit'   —— 直接命中的反应 'Strike'、能否点火、爆炸范围 'Blast'（爆炸范围从函数改成数据）；
-- * 'Tally'   —— 计数键、差计数权重、离格算地毯（原 Meta / liveMeta）；原型级的差计数是 'DiffCount'；
-- * 'Face'    —— 前端标签 + 字段 + 附加字段（原 view）；
-- * 'Hud'     —— 原型级的 HUD 数据：中文名、失败提示 'LoseHint'（原来是函数，现为数据）、目标图标、是否显示血条。
module Match3.ECS.Component
  ( -- * 匹配
    Match(..)
  , gemMatch
  , obstacleMatch
    -- * 物理
  , Physics(..)
  , fixedPhysics
  , gemPhysics
  , obstaclePhysics
    -- * 命中
  , Strike(..)
  , Blast(..)
  , blastArea
  , OnHit(..)
  , gemHit
  , immuneHit
  , breakHit
  , absorbHit
    -- * 计数
  , Tally(..)
  , emptyTally
  , DiffCount(..)
    -- * 显示
  , Face(..)
  , noFace
  , baseFace
  , Hud(..)
  , noHud
  , LoseHint(..)
  , renderLoseHint
  ) where

import Match3.Board.Grid (inBounds)
import Match3.Element.Types (CellField, CounterKey, Edge, FaceValue)
import Match3.Types (Board, Cell, Color, Pos, boardColIndices, boardRowIndices)

--------------------------------------------------------------------------------
-- 匹配

-- | 匹配组件：参与匹配的颜色（Nothing = 无色）、挡匹配、挡交换、普通匹配提示是否试这一格。
data Match = Match
  { mColor :: Maybe Color
  , mBlockMatch :: Bool
  , mBlockSwap :: Bool
  , mHintable :: Bool
  }
  deriving (Eq, Show)

-- | 普通棋子：按颜色匹配、可交换、进提示。
gemMatch :: Maybe Color -> Match
gemMatch mc = Match mc False False True

-- | 占格障碍：无色、挡匹配、挡交换（仍会被提示扫描试到，但无色不成消）。
obstacleMatch :: Match
obstacleMatch = Match Nothing True True True

--------------------------------------------------------------------------------
-- 物理

-- | 物理组件。
data Physics = Physics
  { pFixed :: Bool        -- ^ 固定格（不随重力移动）
  , pFalls :: Bool        -- ^ 随重力下落
  , pPortal :: Bool       -- ^ 能穿过传送门
  , pRecolor :: Bool      -- ^ 可被魔法帽 / 染色瓶改色
  , pPush :: Bool         -- ^ 可被蜗牛推动
  , pKeepShuffle :: Bool  -- ^ 洗牌时原样保留
  , pDrains :: [Edge]     -- ^ 在哪些边被收走
  }
  deriving (Eq, Show)

fixedPhysics :: Physics
fixedPhysics = Physics True False False False False True []

gemPhysics :: Physics
gemPhysics = Physics False True True True True False []

-- | 占格障碍：下落、洗牌保留、不过门、不改色 / 不推动。
obstaclePhysics :: Physics
obstaclePhysics = Physics False True False False False True []

--------------------------------------------------------------------------------
-- 命中

-- | 直接命中的反应：吃掉命中（本格写成新内容）/ 本格被消除 / 打不动。
data Strike
  = Absorb Cell
  | Destroy
  | Immune
  deriving (Eq, Show)

-- | 爆炸范围（数据）：整行 / 整列 / 3×3（裁到盘内）。由 'blastArea' 解释。
data Blast = BlastRow | BlastCol | BlastSquare
  deriving (Eq, Show)

-- | 爆炸范围的解释器（引爆格 p，盘面决定行列数）。
blastArea :: Blast -> Board -> Pos -> [Pos]
blastArea bl b (r, c) = case bl of
  BlastRow -> [(r, x) | x <- boardColIndices b]
  BlastCol -> [(y, c) | y <- boardRowIndices b]
  BlastSquare -> [(rr, cc) | rr <- [r - 1 .. r + 1], cc <- [c - 1 .. c + 1], inBounds b (rr, cc)]

-- | 命中组件：反应、能否点火、爆炸范围（Nothing = 不爆炸）。
data OnHit = OnHit
  { hStrike :: Strike
  , hFires :: Bool
  , hBlast :: Maybe Blast
  }
  deriving (Eq, Show)

-- | 普通宝石：命中即消、能点火、不爆炸。
gemHit :: OnHit
gemHit = OnHit Destroy True Nothing

-- | 打不动。
immuneHit :: OnHit
immuneHit = OnHit Immune False Nothing

-- | 命中即破（不点火）。
breakHit :: OnHit
breakHit = OnHit Destroy False Nothing

-- | 吃掉命中，本格写成给定内容（不点火）。
absorbHit :: Cell -> OnHit
absorbHit cell = OnHit (Absorb cell) False Nothing

--------------------------------------------------------------------------------
-- 计数

-- | 计数组件（按格）。
data Tally = Tally
  { tCounter :: Maybe CounterKey      -- ^ 进入清除格时的计数键
  , tDiffWeight :: Int                -- ^ 按前后个数差计数时这一格的权重（缺省 1；雪怪左上格 = 血量）
  , tVacatesCarpet :: Bool            -- ^ 离开格子（不进清除格）也算覆盖地毯
  }
  deriving (Eq, Show)

emptyTally :: Tally
emptyTally = Tally Nothing 1 False

-- | 原型级：按步前 / 步后盘面上的个数差（按 'tDiffWeight' 加权）计数（保险箱开启、时间精灵、雪怪血量）。
data DiffCount = DiffCount
  { dcKey :: CounterKey  -- ^ 计数键
  , dcBonus :: Int       -- ^ 每少一个奖励的步数
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- 显示

-- | 显示组件：前端格子的类型标签与基本字段（Nothing = View 的缺省：Custom 格 ("custom", name / v)，其余 = 元素名）
-- 与附加字段（网页格子 JSON 追加在基本字段之后）。只影响显示，不参与规则。
data Face = Face
  { fBase :: Maybe (String, [(String, CellField)])
  , fExtras :: [(String, FaceValue)]
  }
  deriving (Eq, Show)

-- | 没有自己的标签、没有附加字段。
noFace :: Face
noFace = Face Nothing []

-- | 只有基本字段的显示。
baseFace :: String -> [(String, CellField)] -> Face
baseFace t fs = Face (Just (t, fs)) []

-- | 失败提示（数据）：@前缀 ++ show 目标值 ++ 后缀@。
data LoseHint = LoseHint String String
  deriving (Eq, Show)

renderLoseHint :: LoseHint -> Int -> String
renderLoseHint (LoseHint pre post) n = pre ++ show n ++ post

-- | 原型级 HUD 数据（全是数据；缺省 'noHud' = 都不提供）。
data Hud = Hud
  { hudLabel :: Maybe String          -- ^ 按元素名计数的目标（CountNamed）的中文名（HUD / 网页 / 失败提示）
  , hudLoseHint :: Maybe LoseHint     -- ^ 该目标的失败提示；缺省「消除<中文名>，目标 n 个」
  , hudGoalIcon :: Maybe String       -- ^ 目标图标贴图名（覆盖默认的元素名）
  , hudShowsHp :: Bool                -- ^ HUD 显示血条：盘上剩余 HP = 本元素各格 'tDiffWeight' 之和
  }
  deriving (Eq, Show)

noHud :: Hud
noHud = Hud Nothing Nothing Nothing False
