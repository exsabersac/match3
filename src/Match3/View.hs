-- | 视图模型：从 GameState / 回放状态算出前端要画的东西——整局 HUD 视图（关卡、分数、步数、
-- 道具、连击、结局）、目标视图、棋盘视图（关卡级元素与提示）、关卡进度点、右下角分数徽章、关卡列表，
-- 以及单格的结构化描述。前端（网页是唯一前端；原 SDL 桌面版已移除）
-- 经 web/hs/Match3Web/Api.hs 的 JSON 读这里，不各自从 GameState 现算。
--
-- 全部是纯函数、只读。
-- 依赖：Match3.Types、Match3.Game.*、Match3.Levels.*（直接 import 子模块，不经前端门面 Match3.Core）、
-- Match3.Engine（连击数经通用接口 gameStatus 取）。
module Match3.View
  ( -- * 整局视图
    GameView (..)
  , gameView
  , Boosters (..)
  , PlayStatus (..)
  , titleLine
    -- * Boss 血条
  , BossView (..)
  , bossView
  , bossViewWith
    -- * 规则开关角标
  , RuleBadge (..)
  , ruleBadgeTable
  , ruleBadge
  , ruleBadges
    -- * 目标视图
  , GoalInfo (..)
  , goalInfo
  , goalLine
  , goalLabel
  , countLabel
  , colorLabel
  , namedGoalLabelTable
  , namedGoalIcon
    -- * 棋盘视图
  , BoardView (..)
  , boardView
    -- * 关卡进度点
  , LevelDot (..)
  , levelDots
    -- * 回放 / 分数徽章
  , ReplayView (..)
  , ScoreBadge (..)
  , scoreBadge
    -- * 关卡列表
  , LevelView (..)
  , levelViews
    -- * 单格描述
  , CellField (..)
  , cellFace
  , cellFaceWith
  , FaceValue (..)
  , cellExtras
  , cellExtrasWith
  , overlayName
  , overlayLayers
  , colorNum
  , kindCode
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Engine.Game (Game (..))
import Match3.Board.Default (findHint)
import Match3.Counts (CounterKey(..))
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Phase (Face(..), Phase(..), SomePhase(..), phaseName)
import Match3.ECS.Registry (Registry, boardBossHpWith, bodyOf, faceFieldsWith)
import Match3.Element.Types (CellField (..), FaceValue (..))
import Match3.Engine (match3Game)
import Match3.Game.Outcome (loseHint)
import Match3.Game.State (GameState(..), gsBelts, gsCarpetOpen, gsGround, gsPortals, gsProgress, gsUfos)
import Match3.GoalLabel (colorLabel, countLabel, goalViewLabel, namedGoalIcon, namedGoalLabelTable)
import Match3.Element.Level (levelDrops)
import Match3.Levels.Campaign (allLevels, levelCarpets, levelCount, lookupLevel)
import Match3.Levels.Level (Level(..))
import Match3.Types
  ( Board
  , Cell
  , CellContents(..)
  , CellOverlay(..)
  , Color(..)
  , CustomState(..)
  , ElementName(..)
  , GemKind(..)
  , GoalView(..)
  , LevelGoal(..)
  , Meter(..)
  , Terminal(..)
  , Pos
  , Quota(..)
  , goalTarget
  , goalView
  )
import Match3.Ufo (Ufo(..))

--------------------------------------------------------------------------------
-- 整局视图

-- | 道具剩余次数。
data Boosters = Boosters
  { bHammers :: Int
  , bFreeSwaps :: Int
  , bCrossClears :: Int
  }
  deriving (Eq, Show)

-- | 结局 / 局面状态（标题 'titleLine' 的结局段）。Just 其余结果（MoveApplied 等）按进行中处理。
data PlayStatus
  = PlayWon Int
  | PlayCleared Int Int  -- ^ 分数、下一关下标
  | PlayLost Int
  | PlayShuffled         -- ^ 进行中、本步自动洗过牌
  | PlayOn
  deriving (Eq, Show)

-- | 一帧 HUD / 标题 / 网页状态要用的全部读数。
data GameView = GameView
  { gvLevel :: Int         -- ^ gsLevel 原值（标题 "L<n>"、网页 level）
  , gvLevelIndex :: Int    -- ^ 夹到 [0, levelCount-1] 的关卡下标（关名图 name_<i>、进度点）
  , gvLevelName :: String  -- ^ 夹紧下标的关名（标题）
  , gvRawName :: String    -- ^ 原下标的关名，无此关为 "?"（网页 name）
  , gvDaily :: Bool
  , gvRules :: [String]    -- ^ 本关打开的规则开关（Level.lvlRules，如 "bomb_shapes"）；每日挑战为空（HUD 角标）
  , gvScore :: Int
  , gvMoves :: Int
  , gvBoosters :: Boosters
  , gvCombo :: Int         -- ^ 经通用接口 gameStatus 取的连击数（= gsCombo）
  , gvShuffled :: Bool
  , gvOver :: Maybe Terminal
  , gvStatus :: PlayStatus
  , gvGoal :: GoalInfo
  , gvBoss :: Maybe BossView  -- ^ Boss 血条：目标配额对应的元素提供 boardBossHp 时才有
  , gvBoard :: BoardView
  }

gameView :: GameState -> GameView
gameView gs =
  GameView
    { gvLevel = lvl
    , gvLevelIndex = li
    , gvLevelName = maybe "?" lvlName mlvl
    , gvRawName = maybe "?" lvlName (lookupLevel lvl)
    , gvDaily = gsDaily gs
    , gvRules = if gsDaily gs then [] else maybe [] (map unElementName . lvlRules) (lookupLevel lvl)
    , gvScore = gsScore gs
    , gvMoves = mv
    , gvBoosters = Boosters (gsHammers gs) (gsFreeSwaps gs) (gsCrossClears gs)
    , gvCombo = fromMaybe 0 (lookup "combo" (gameStatus match3Game gs))
    , gvShuffled = gsShuffled gs
    , gvOver = gsOver gs
    , gvStatus = case gsOver gs of
        Just (TWon s) -> PlayWon s
        Just (TLevelClear s n) -> PlayCleared s n
        Just (TLost s) -> PlayLost s
        Nothing -> if gsShuffled gs then PlayShuffled else PlayOn
    , gvGoal = goalInfo (gsGoal gs) (gsProgress gs)
    , gvBoss = bossView gs
    , gvBoard = boardView gs
    }
  where
    lvl = gsLevel gs
    li = min lvl (levelCount - 1)
    mlvl = lookupLevel li
    mv = gsMoves gs

-- | HUD 血条（新玩法 5 雪怪 Boss）：剩余血量（盘上全部 Boss 的血量之和，击败后为 0）与满血值（= 目标值）。
data BossView = BossView
  { bvHp :: Int
  , bvMax :: Int
  }
  deriving (Eq, Show)

-- | 目标配额对应的元素提供 'boardBossHp' 时给出血条；满血值 = 该配额的目标值（不点名具体元素）。
bossView :: GameState -> Maybe BossView
bossView = bossViewWith defaultRegistry

-- | 'bossView'，用给定的元素世界（扩展 Boss 元素时用）。
bossViewWith :: Registry -> GameState -> Maybe BossView
bossViewWith world gs =
  case
    [ BossView (max 0 (min t hp)) t
    | Quota (MeterCount (CountNamed n)) t <- goalQuotas (gsGoal gs)
    , Just hp <- [boardBossHpWith world (gsBoard gs) n]
    ] of
    (b : _) -> Just b
    [] -> Nothing

-- | 窗口标题（不含末尾的 "  |  " 与提示消息；网页写进 document.title）。
titleLine :: GameView -> String
titleLine gv =
  "L"
    ++ show (gvLevel gv + 1)
    ++ " "
    ++ gvLevelName gv
    ++ "  "
    ++ goalLine (gvGoal gv)
    ++ "  moves="
    ++ show (gvMoves gv)
    ++ comboBits
    ++ "  Hm="
    ++ show (bHammers b)
    ++ " Sw="
    ++ show (bFreeSwaps b)
    ++ " Cr="
    ++ show (bCrossClears b)
    ++ status
  where
    b = gvBoosters gv
    comboBits = if gvCombo gv > 1 then "  combo x" ++ show (gvCombo gv) else ""
    status = case gvStatus gv of
      PlayWon s -> " CLEAR! score=" <> show s
      PlayCleared s n -> " LEVEL UP ->" <> show (n + 1) <> " score=" <> show s
      PlayLost s -> " LOSE score=" <> show s
      _ -> ""

--------------------------------------------------------------------------------
-- 规则开关角标

-- | HUD 上的一枚规则开关角标：本关打开的每个规则开关（'gvRules' 的每一项）一枚，画在关名右侧。
-- 网页（web/hs/Match3Web/Api.hs 的
-- state.rules → www/hud.js）画同样的图标，文字用画布字体渲染 'rbText'（网页图集不含文字贴图）。
-- 新玩法加规则开关时在 'ruleBadgeTable' 登记一行，网页自动显示，不用改 HUD 代码。
data RuleBadge = RuleBadge
  { rbRule :: String        -- ^ 规则开关名（Level.lvlRules 里的名字，如 "bomb_shapes"）
  , rbText :: String        -- ^ 角标文字（网页用画布字体画）
  , rbIcons :: [String]     -- ^ 图标贴图名，从下往上叠画（网页图集里的贴图）；可为空
  }
  deriving (Eq, Show)

-- | 已登记的规则开关角标（顺序无关，按 gvRules 的顺序画）。
ruleBadgeTable :: [RuleBadge]
ruleBadgeTable =
  [ RuleBadge "bomb_shapes" "L/T 形出炸弹" ["bomb_glow", "bomb_mark"]   -- 新玩法 1：L / T 形出炸弹（第 41 关）
  , RuleBadge "rainbow_combos" "彩虹组合变身" ["rainbow"]                 -- 新玩法 4：魔力鸟组合增强（第 44 关）
  ]

-- | 查一个规则开关的角标。没登记的规则也显示（不静默丢掉）：文字 = 规则名、无图标。
ruleBadge :: String -> RuleBadge
ruleBadge r = fromMaybe (RuleBadge r r []) (find ((== r) . rbRule) ruleBadgeTable)

-- | 本关要画的全部角标（按 gvRules 的顺序；每日挑战为空）。
ruleBadges :: GameView -> [RuleBadge]
ruleBadges = map ruleBadge . gvRules

--------------------------------------------------------------------------------
-- 目标视图

data GoalInfo = GoalInfo
  { giGoal :: LevelGoal
  , giView :: GoalView
  , giProgress :: Int      -- ^ 核心 gsProgress（标题与网页同一个数）
  , giTarget :: Int        -- ^ goalTarget
  , giKind :: String       -- ^ show 的首词（网页 goal.kind）
  , giText :: String       -- ^ show 全文（网页 goal.text）
  , giName :: Maybe ElementName  -- ^ 按元素名计数的目标（网页 goal.name）
  , giLoseHint :: String
  }

-- | 目标 + 当前进度（关卡列表里没有局面，进度传 0）。
goalInfo :: LevelGoal -> Int -> GoalInfo
goalInfo g prog =
  GoalInfo
    { giGoal = g
    , giView = v
    , giProgress = prog
    , giTarget = goalTarget g
    , giKind = takeWhile (/= ' ') (show g)
    , giText = show g
    , giName = case v of
        ViewCount (CountNamed n) _ -> Just n
        _ -> Nothing
    , giLoseHint = loseHint g
    }
  where
    v = goalView g

-- | 标题里的目标段（中文标签 'goalLabel'：分数=12/100、收集红色宝石=3/20、碎石=3/8、变色龙=6/30 …；
-- 合 main 9f5504e 前是英文标签 score / collect RED / stone / 元素内部名）。
goalLine :: GoalInfo -> String
goalLine gi = goalLabel gi ++ "=" ++ show (giProgress gi) ++ "/" ++ show target
  where
    target = case giView gi of
      ViewScore t -> t
      ViewCollect _ n -> n
      ViewCount _ n -> n
      _ -> giTarget gi

-- | 目标的中文显示名（HUD「目标 …」标签；网页 state.goal.label / m3Levels 的 goal.label、窗口标题的目标段都取这里）。
-- 表本身（'countLabel' / 'colorLabel' / 'namedGoalLabelTable'）合 main 9f5504e 后下移到 Match3.GoalLabel
-- （失败提示 Match3.Game.Outcome.loseHint 也用它），这里重新导出，对外 API 不变。
goalLabel :: GoalInfo -> String
goalLabel = goalViewLabel . giView

--------------------------------------------------------------------------------
-- 棋盘视图

-- | 棋盘与关卡级元素（字段惰性：只读用到的部分）。
data BoardView = BoardView
  { bvBoard :: Board
  , bvFoundHint :: Maybe (Pos, Pos)  -- ^ 当前盘面上找到的一步（findHint；网页 hint）
  , bvLastCleared :: [Pos]
  , bvGround :: [(Pos, (ElementName, Int))]
  , bvBelts :: [[Pos]]
  , bvPortals :: [(Pos, Pos)]
  , bvUfos :: [Ufo]
  , bvCarpets :: [Pos]               -- ^ 本关地毯格（levelCarpets gsLevel）
  , bvCarpetOpen :: [Pos]
  , bvDrops :: [Pos]                 -- ^ 掉落口格（新玩法 6，Element.Level.levelDrops）；没有掉落口为空
  }

boardView :: GameState -> BoardView
boardView gs =
  BoardView
    { bvBoard = gsBoard gs
    , bvFoundHint = findHint (gsBoard gs)
    , bvLastCleared = gsLastCleared gs
    , bvGround = gsGround gs
    , bvBelts = gsBelts gs
    , bvPortals = gsPortals gs
    , bvUfos = gsUfos gs
    , bvCarpets = levelCarpets (gsLevel gs)
    , bvCarpetOpen = gsCarpetOpen gs
    , bvDrops = levelDrops (gsLevelElems gs)
    }

--------------------------------------------------------------------------------
-- 关卡进度点

data LevelDot = DotCurrent | DotDone | DotUnlocked | DotLocked
  deriving (Eq, Show)

-- | 各关一个点（共 levelCount 个，下标 = 关卡下标）：当前关、已过、已解锁未过、未解锁。
-- 网页 m3Progress 传夹紧的下标与最高解锁关。
levelDots :: Int -> Int -> [LevelDot]
levelDots cur maxReached = map dot [0 .. levelCount - 1]
  where
    dot i
      | i == cur = DotCurrent
      | i < cur = DotDone
      | i <= maxReached = DotUnlocked
      | otherwise = DotLocked

--------------------------------------------------------------------------------
-- 回放 / 分数徽章

-- | 正在回放的一步（当前轮连击数、滚动中的显示分数）。
data ReplayView = ReplayView
  { rvCombo :: Int
  , rvShownScore :: Int
  }
  deriving (Eq, Show)

-- | HUD 右下角：回放中连击 ≥2 显示「连击 xN」，否则滚动分数；播完后短暂显示本步最高连击总结；
-- 其余显示得分（shuffled 为真时标签是「已洗牌」）。
data ScoreBadge
  = BadgeCombo Int
  | BadgeRolling Int
  | BadgeSummary Int
  | BadgeScore Bool Int
  deriving (Eq, Show)

-- | 回放视图、总结剩余帧数、本步最高连击、整局视图 → 徽章。
scoreBadge :: Maybe ReplayView -> Int -> Int -> GameView -> ScoreBadge
scoreBadge replay summaryLeft best gv = case replay of
  Just rv
    | rvCombo rv >= 2 -> BadgeCombo (rvCombo rv)
    | otherwise -> BadgeRolling (rvShownScore rv)
  Nothing
    | summaryLeft > 0 && best >= 2 -> BadgeSummary best
    | otherwise -> BadgeScore (gvShuffled gv) (gvScore gv)

--------------------------------------------------------------------------------
-- 关卡列表

data LevelView = LevelView
  { lvIndex :: Int
  , lvName :: String
  , lvMoves :: Int
  , lvGoal :: GoalInfo
  }

levelViews :: [LevelView]
levelViews = [LevelView (lvlIndex l) (lvlName l) (lvlMoves l) (goalInfo (lvlGoal l) 0) | l <- allLevels]

--------------------------------------------------------------------------------
-- 单格描述

-- | 结构化描述里的一个字段值。
-- | 单格的结构化描述：类型标签 + 按固定顺序的字段（网页 JSON 的 t / c / k / i / o / n …；
-- 渲染层按 t 查表，www/cells.js）。
cellFace :: Cell -> (String, [(String, CellField)])
cellFace = cellFaceWith defaultRegistry

-- | 'cellFace'，用给定的世界解码（扩展元素）。宝石格（冰层 / 叠层都在宝石格的字段里）按存储编码给出；
-- 其余格问本体 'view' 的 fBase（Match3.Element.Phase），没给时：Custom 格 = ("custom", name / v)，
-- 其余 = (元素名, 无字段)。元素类重构第 6 刀前这里是按 Cell 构造器写死的 case，网页 JSON 逐字节不变。
cellFaceWith :: Registry -> Cell -> (String, [(String, CellField)])
cellFaceWith w cell = case cell of
  Gem c k ice ov ->
    ( "G"
    , [ col c, ("k", FieldText (kindCode k)), ("i", FieldInt ice)
      , ("o", maybe FieldNull (FieldText . overlayName) ov), ("n", FieldInt (maybe 0 overlayLayers ov)) ] )
  _ -> case bodyOf w cell of
    body@(SomePhase e) -> case fBase (view e) of
      Just f -> f
      Nothing -> case cell of
        Custom name v -> ("custom", [("name", FieldText (unElementName name)), ("v", FieldInt (unCustomState v))])
        _ -> (unElementName (phaseName body), [])
  where
    col c = ("c", FieldInt (colorNum c))

-- | 单格的显示附加字段：元素自己提供（Phase 'view' 的 fExtras，Match3.Element.Phase），这里不按元素名特判。
-- 内置：雪怪 Boss 的 q（象限 0–3）/ hurt（血量是否过半）/ turn（召唤计数）/ every（召唤周期），变色龙的 c（当前颜色）。
-- 网页 JSON 把它们按顺序追加在 cellFace 字段之后；app/pure/UI/CellFace.hs 按名字读。
cellExtras :: Cell -> [(String, FaceValue)]
cellExtras = cellExtrasWith defaultRegistry

-- | 'cellExtras'，用给定的元素世界解码（扩展元素）。
cellExtrasWith :: Registry -> Cell -> [(String, FaceValue)]
cellExtrasWith = faceFieldsWith

overlayName :: CellOverlay -> String
overlayName ov = case ov of
  Grass -> "grass"
  Vine -> "vine"
  Choco -> "choco"
  Fog _ -> "fog"
  Chain _ -> "chain"
  Freeze _ -> "freeze"
  Curtain _ -> "curtain"
  Steam -> "steam"

overlayLayers :: CellOverlay -> Int
overlayLayers ov = case ov of
  Fog k -> k
  Chain k -> k
  Freeze k -> k
  Curtain k -> k
  _ -> 0

-- | 颜色编号 1..5。
colorNum :: Color -> Int
colorNum c = fromEnum c + 1

kindCode :: GemKind -> String
kindCode k = case k of
  Normal -> "N"
  LineH -> "H"
  LineV -> "V"
  Bomb -> "B"
  Rainbow -> "R"
