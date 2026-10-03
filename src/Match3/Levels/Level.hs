-- | 关卡记录（第 6 刀：每关的全部静态数据聚在一条记录里）：名字、步数、目标，
-- 以及第 6 刀前分散在 Game.Level / Match3.Carpet 各自按下标 case 的并行表——装饰放置表、皮带、传送门、飞碟、地毯、地面层。
--
-- 依赖：Match3.Types、Match3.Element.Types（放置表）、Match3.Conveyor（Belt）、Match3.Ufo。
-- 关卡表本身在 Match3.Levels.Campaign；开局如何用这些字段见 Match3.Game.Level.newGameAtLevel。
--
-- Haskell 特性第 8 项（数据边界，见 docs/haskell-features/08-数据边界.md）：关卡数据的校验是 Applicative 的
-- 'Validation'——各项检查互相独立，'<*>' 把所有失败的 'LevelIssue' 拼在一起，一次报全（'Either' 遇到第一处就停）。
-- 只检查现有全部关卡与每日关都已满足的不变量（测试 level_validation_all_levels_valid 锁定）。
module Match3.Levels.Level
  ( Level(..)
  , DropSpec(..)
  , level
  , levelConfig
  , placeEach
  , layersAt
  , checkLevelDims
  , assertLevelDims
    -- * 校验（第 8 项）
  , Validation(..)
  , failure
  , validationToEither
  , LevelIssue(..)
  , renderIssue
  , validateLevel
  , checkLevel
  , assertLevel
  ) where

import Data.List (intercalate, nub)
import Match3.Conveyor (Belt)
import Match3.Element.Types (Arg(..), Placement(..))
import Match3.Types
import Match3.Ufo (Ufo(..))

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
  , lvlRows       :: Int           -- ^ 盘面行数（缺省 8；允许 minBoardDim..maxBoardDim）
  , lvlCols       :: Int           -- ^ 盘面列数（缺省 8；允许 minBoardDim..maxBoardDim；可与行数不同）
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
    , lvlRows = boardSize
    , lvlCols = boardSize
    }

-- | 校验关卡行列是否在允许范围内；越界返回 Left（加载时拒绝，不静默夹取）。
-- 第 8 项起由 'validateDims' 与 'renderIssue' 给出，文字与之前逐字节相同。
checkLevelDims :: Level -> Either String Level
checkLevelDims l = either (Left . renderAll l) Right (validationToEither (validateDims l *> pure l))

-- | 加载边界：尺寸合法则原样返回，否则 error（不夹取）。
assertLevelDims :: Level -> Level
assertLevelDims l = either error id (checkLevelDims l)

--------------------------------------------------------------------------------
-- 校验（第 8 项）

-- | 校验结果：成功带值，失败带累积的错误。只有 Functor / Applicative，**没有 Monad**：
-- 'Failure e1 <*> Failure e2 = Failure (e1 <> e2)'（两边都跑、错误拼起来），而 Monad 的 '>>=' 在左边失败时拿不到值、
-- 无法再跑右边，按 Monad 定律 @(<*>) = ap@ 只能得到 'Failure e1'——两者矛盾，所以不给 Monad 实例
-- （后一项检查依赖前一项结果时，在 'Either' 里写，见 'validationToEither'）。
data Validation e a = Failure e | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> Success _ = Failure e1
  Success _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success (f a)

-- | 单个错误的失败。
failure :: e -> Validation [e] a
failure e = Failure [e]

-- | 转成 Either（要按顺序依赖结果时用）。
validationToEither :: Validation e a -> Either e a
validationToEither v = case v of
  Failure e -> Left e
  Success a -> Right a

-- | 关卡数据的一处问题。编号（皮带 / 传送门 / 飞碟 / 掉落口 / 放置项）从 0 起，按关卡记录里的列表顺序。
data LevelIssue
  = BadDims Int Int                          -- ^ 行列越出 minBoardDim..maxBoardDim（行, 列）
  | BeltTooShort Int Int                     -- ^ 第 i 条皮带少于 2 格（编号, 格数）
  | BeltOutOfBounds Int Pos                  -- ^ 皮带格越界
  | BeltDuplicate Int Pos                    -- ^ 同一条皮带里重复的格
  | PortalOutOfBounds Int Pos                -- ^ 传送门端点越界
  | PortalSameCell Int Pos                   -- ^ 传送门两端是同一格
  | UfoOutOfBounds Int Pos                   -- ^ 飞碟初始格越界
  | CarpetOutOfBounds Pos                    -- ^ 地毯格越界
  | CarpetDuplicate Pos                      -- ^ 地毯格重复
  | GroundOutOfBounds Pos                    -- ^ 地面层格越界
  | GroundDuplicate Pos                      -- ^ 地面层格重复
  | GroundNoLayers Pos Int                   -- ^ 地面层层数 < 1
  | DropNoCells Int                          -- ^ 掉落口没有格
  | DropOutOfBounds Int Pos                  -- ^ 掉落口格越界
  | DropKeepNonPositive Int Int              -- ^ 掉落口的保持数 < 1
  | PlacementOutOfBounds Int ElementName Pos -- ^ 放置表第 i 项的目标格越界
  deriving (Eq, Show)

-- | 问题的中文描述。'BadDims' 与第 8 项前 checkLevelDims 的文字逐字节相同。
renderIssue :: Level -> LevelIssue -> String
renderIssue l issue = case issue of
  BadDims r c ->
    "关卡「" ++ lvlName l ++ "」尺寸 " ++ show r ++ "×" ++ show c ++ " 超出允许范围 " ++ show minBoardDim ++ "–" ++ show maxBoardDim
  BeltTooShort i n -> named ("第 " ++ show i ++ " 条皮带只有 " ++ show n ++ " 格（至少 2 格）")
  BeltOutOfBounds i p -> named ("第 " ++ show i ++ " 条皮带的格 " ++ show p ++ " 越界")
  BeltDuplicate i p -> named ("第 " ++ show i ++ " 条皮带重复经过 " ++ show p)
  PortalOutOfBounds i p -> named ("第 " ++ show i ++ " 对传送门的端点 " ++ show p ++ " 越界")
  PortalSameCell i p -> named ("第 " ++ show i ++ " 对传送门两端都是 " ++ show p)
  UfoOutOfBounds i p -> named ("第 " ++ show i ++ " 个飞碟的初始格 " ++ show p ++ " 越界")
  CarpetOutOfBounds p -> named ("地毯格 " ++ show p ++ " 越界")
  CarpetDuplicate p -> named ("地毯格 " ++ show p ++ " 重复")
  GroundOutOfBounds p -> named ("地面层格 " ++ show p ++ " 越界")
  GroundDuplicate p -> named ("地面层格 " ++ show p ++ " 重复")
  GroundNoLayers p n -> named ("地面层格 " ++ show p ++ " 的层数 " ++ show n ++ " 小于 1")
  DropNoCells i -> named ("第 " ++ show i ++ " 个掉落口没有格")
  DropOutOfBounds i p -> named ("第 " ++ show i ++ " 个掉落口的格 " ++ show p ++ " 越界")
  DropKeepNonPositive i n -> named ("第 " ++ show i ++ " 个掉落口的保持数 " ++ show n ++ " 小于 1")
  PlacementOutOfBounds i n p -> named ("放置表第 " ++ show i ++ " 项（" ++ unElementName n ++ "）的格 " ++ show p ++ " 越界")
  where
    named msg = "关卡「" ++ lvlName l ++ "」" ++ msg

-- | 多处问题拼成一段（用「；」分隔）；只有一处时就是这一处的文字。
renderAll :: Level -> [LevelIssue] -> String
renderAll l = intercalate "；" . map (renderIssue l)

-- | 只查行列（checkLevelDims 用）。
validateDims :: Level -> Validation [LevelIssue] ()
validateDims l = check (validBoardDim (lvlRows l) && validBoardDim (lvlCols l)) (BadDims (lvlRows l) (lvlCols l))

-- | 全部检查，问题按下面的顺序一次列全（行列、皮带、传送门、飞碟、地毯、地面层、掉落口、放置表）。
-- 越界一律按关卡的行列（lvlRows × lvlCols）判断；行列本身越界时其余检查照常进行。
--
-- 只收「现有全部关卡都满足」的不变量：皮带至少 2 格、格在界内、同一条皮带不重复——**不要求首尾相邻或连成环**
-- （第 10 关「传送」的皮带是一条直线，首尾不相邻，移位按「末格回到首格」照常工作）。
validateLevel :: Level -> Validation [LevelIssue] Level
validateLevel l =
  l
    <$ validateDims l
    <* each (zip [0 ..] (lvlBelts l)) belt
    <* each (zip [0 ..] (lvlPortals l)) portal
    <* each (zip [0 ..] (lvlUfos l)) (\(i, u) -> inside (UfoOutOfBounds i) (ufoCell u))
    <* each (lvlCarpets l) (inside CarpetOutOfBounds)
    <* dups CarpetDuplicate (lvlCarpets l)
    <* each (lvlGround l) ground
    <* dups GroundDuplicate (map fst (lvlGround l))
    <* each (zip [0 ..] (lvlDrops l)) drop1
    <* each (zip [0 ..] (lvlPlacements l)) place1
  where
    inBoard (r, c) = r >= 0 && c >= 0 && r < lvlRows l && c < lvlCols l
    inside mk p = check (inBoard p) (mk p)
    belt (i, cells) =
      check (length cells >= 2) (BeltTooShort i (length cells))
        <* each cells (inside (BeltOutOfBounds i))
        <* dups (BeltDuplicate i) cells
    portal (i, (a, b)) = inside (PortalOutOfBounds i) a <* inside (PortalOutOfBounds i) b <* check (a /= b) (PortalSameCell i a)
    ground (p, (_, n)) = inside GroundOutOfBounds p <* check (n >= 1) (GroundNoLayers p n)
    drop1 (i, d) =
      check (not (null (dropCells d))) (DropNoCells i)
        <* each (dropCells d) (inside (DropOutOfBounds i))
        <* check (dropKeep d >= 1) (DropKeepNonPositive i (dropKeep d))
    place1 (i, Place n _ ps) = each ps (inside (PlacementOutOfBounds i n))

-- | 条件不成立时报一处问题。
check :: Bool -> LevelIssue -> Validation [LevelIssue] ()
check ok issue = if ok then pure () else failure issue

-- | 对每一项都检查（全部跑完，问题按列表顺序拼接）。
each :: [a] -> (a -> Validation [LevelIssue] ()) -> Validation [LevelIssue] ()
each xs f = foldr (\x acc -> f x <* acc) (pure ()) xs

-- | 列表里第二次及以后出现的值各报一次（每个重复值报一次）。
dups :: Eq a => (a -> LevelIssue) -> [a] -> Validation [LevelIssue] ()
dups mk xs = each (nub [x | (i, x) <- zip [0 :: Int ..] xs, x `elem` take i xs]) (failure . mk)

-- | 全部检查，失败时给出拼好的文字（只有行列一处问题时与 'checkLevelDims' 的文字相同）。
checkLevel :: Level -> Either String Level
checkLevel l = either (Left . renderAll l) Right (validationToEither (validateLevel l))

-- | 加载边界（第 8 项起 lookupLevel 用它）：全部检查通过则原样返回，否则 error，消息列出全部问题。
assertLevel :: Level -> Level
assertLevel l = either error id (checkLevel l)

levelConfig :: Level -> GameConfig
levelConfig l = GameConfig { cfgMoves = lvlMoves l, cfgGoal = lvlGoal l }

-- | 逐格参数不同的放置（层数 / 颜色）：按列表顺序展开成单格放置。
placeEach :: ElementName -> (a -> [Arg]) -> [(Pos, a)] -> [Placement]
placeEach name f xs = [Place name (f x) [p] | (p, x) <- xs]

-- | 按层数放置（每格层数不同）。
layersAt :: ElementName -> [(Pos, Int)] -> [Placement]
layersAt name = placeEach name (\n -> [AInt n])
