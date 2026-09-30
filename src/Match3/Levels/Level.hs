-- | 关卡记录（第 6 刀：每关的全部静态数据聚在一条记录里）：名字、步数、目标，
-- 以及第 6 刀前分散在 Game.Level / Match3.Carpet 各自按下标 case 的并行表——装饰放置表、皮带、传送门、飞碟、地毯、地面层。
--
-- 依赖：Match3.Types、Match3.Element.Types（放置表）、Match3.Conveyor（Belt）、Match3.Ufo。
-- 关卡表本身在 Match3.Levels.Campaign；开局如何用这些字段见 Match3.Game.Level.newGameAtLevel。
module Match3.Levels.Level
  ( Level(..)
  , DropSpec(..)
  , level
  , levelConfig
  , placeEach
  , layersAt
  ) where

import Match3.Conveyor (Belt)
import Match3.Element.Types (Arg(..), Placement(..))
import Match3.Types
import Match3.Ufo (Ufo)

data Level = Level
  { lvlIndex      :: Int
  , lvlName       :: String
  , lvlMoves      :: MovesLeft
  , lvlGoal       :: LevelGoal
  , lvlPlacements :: [Placement]   -- ^ 装饰放置表（按元素名；应用顺序 = 列表顺序；第 6 刀前的 levelPlacements）
  , lvlBelts      :: [Belt]        -- ^ 传送带路径（第 6 刀前的 levelBelts）
  , lvlPortals    :: [(Pos, Pos)]  -- ^ 双向传送门对（第 6 刀前的 levelPortals）
  , lvlUfos       :: [Ufo]         -- ^ 飞碟初始位置（第 6 刀前的 levelUfos）
  , lvlCarpets    :: [Pos]         -- ^ 未铺的地毯格（第 6 刀前的 Match3.Carpet.levelCarpets）
  , lvlGround     :: Ground        -- ^ 地面层（第 6 刀前的 levelGround）
  , lvlRules      :: [ElementName] -- ^ 本关打开的规则开关（按名字，关卡级元素在 levelStart 里读；如 "bomb_shapes" = L / T 形生成炸弹）
  , lvlDrops      :: [DropSpec]    -- ^ 掉落口（新玩法 6，关卡级元素 CookieDrop 在 levelStart 里读）；空 = 没有掉落口
  } deriving (Eq, Show)

-- | 掉落口（新玩法 6）：补子时 'dropCells' 里的空洞若盘上的 'dropCell' 少于 'dropKeep' 个，就补 'dropCell'
-- （如饼干 'Cookie'）而不是宝石。规则完全由盘面决定，不额外消耗随机数（见 Element.Builtin.Level.dropRefill）。
data DropSpec = DropSpec
  { dropCells :: [Pos]   -- ^ 掉落口格（一般是顶行）
  , dropCell  :: Cell    -- ^ 掉下来的格子
  , dropKeep  :: Int     -- ^ 盘上少于这么多个 dropCell 时才掉
  } deriving (Eq, Show)

-- | 只有名字 / 步数 / 目标、没有任何装饰与关卡级元素的关（关卡表用记录更新补字段；每日挑战直接用）。
level :: Int -> String -> MovesLeft -> LevelGoal -> Level
level i name moves goal =
  Level
    { lvlIndex = i
    , lvlName = name
    , lvlMoves = moves
    , lvlGoal = goal
    , lvlPlacements = []
    , lvlBelts = []
    , lvlPortals = []
    , lvlUfos = []
    , lvlCarpets = []
    , lvlGround = []
    , lvlRules = []
    , lvlDrops = []
    }

levelConfig :: Level -> GameConfig
levelConfig l = GameConfig { cfgMoves = lvlMoves l, cfgGoal = lvlGoal l }

-- | 逐格参数不同的放置（层数 / 颜色）：按列表顺序展开成单格放置。
placeEach :: ElementName -> (a -> [Arg]) -> [(Pos, a)] -> [Placement]
placeEach name f xs = [Place name (f x) [p] | (p, x) <- xs]

-- | 按层数放置（每格层数不同）。
layersAt :: ElementName -> [(Pos, Int)] -> [Placement]
layersAt name = placeEach name (\n -> [AInt n])
