-- | 视图模型（第 11 刀）：从 GameState / 回放状态算出前端要画的东西——整局 HUD 视图（关卡、分数、步数、
-- 道具、连击、结局）、目标视图、棋盘视图（逐格底层标记）、关卡进度点、右下角分数徽章、关卡列表，
-- 以及单格的结构化描述。桌面版（UI.HudArt / UI.HudBlocks / UI.Actions 标题 / UI.Board* 底层）与
-- 网页版（web/hs/Match3Web/Api.hs 的 JSON）都读这里，不再各自从 GameState 现算。
--
-- 全部是纯函数、只读；字段逐个对应第 11 刀前各前端的现算式（逐字搬迁，JSON / 标题 / 画面不变）。
-- 依赖：Match3.Core、Match3.Engine（连击数经通用接口 gameStatus 取）。
module Match3.View
  ( -- * 整局视图
    GameView (..)
  , gameView
  , Boosters (..)
  , PlayStatus (..)
  , titleLine
    -- * 目标视图
  , GoalInfo (..)
  , goalInfo
  , goalLine
  , goalBracket
  , countTag
  , colorTag
    -- * 棋盘视图
  , BoardView (..)
  , boardView
  , CarpetMark (..)
  , carpetAt
  , groundAtView
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
  , overlayName
  , overlayLayers
  , colorNum
  , kindCode
  ) where

import Data.Maybe (fromMaybe)
import Engine.Game (Game (..))
import Match3.Core
import Match3.Engine (match3Game)

--------------------------------------------------------------------------------
-- 整局视图

-- | 道具剩余次数。
data Boosters = Boosters
  { bHammers :: Int
  , bFreeSwaps :: Int
  , bCrossClears :: Int
  }
  deriving (Eq, Show)

-- | 结局 / 局面状态（HUD 色条与标题共用）。Just 其余结果（MoveApplied 等）按进行中处理。
data PlayStatus
  = PlayWon Int
  | PlayCleared Int Int  -- ^ 分数、下一关下标
  | PlayLost Int
  | PlayShuffled         -- ^ 进行中、本步自动洗过牌
  | PlayOn
  deriving (Eq, Show)

-- | 一帧 HUD / 标题 / 网页状态要用的全部读数。
data GameView = GameView
  { gvLevel :: Int         -- ^ gsLevel 原值（标题 "L<n>"、网页 level、几何版进度点）
  , gvLevelIndex :: Int    -- ^ 夹到 [0, levelCount-1] 的关卡下标（贴图版徽章 / 名字图、步数上限）
  , gvLevelName :: String  -- ^ 夹紧下标的关名（标题）
  , gvRawName :: String    -- ^ 原下标的关名，无此关为 "?"（网页 name）
  , gvDaily :: Bool
  , gvScore :: Int
  , gvMoves :: Int
  , gvMoveCap :: Int       -- ^ 步数条满格值：max 当前步数 该关印制步数
  , gvBoosters :: Boosters
  , gvCombo :: Int         -- ^ 经通用接口 gameStatus 取的连击数（= gsCombo）
  , gvShuffled :: Bool
  , gvOver :: Maybe Outcome
  , gvStatus :: PlayStatus
  , gvGoal :: GoalInfo
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
    , gvScore = gsScore gs
    , gvMoves = mv
    , gvMoveCap = max mv (maybe mv lvlMoves mlvl)
    , gvBoosters = Boosters (gsHammers gs) (gsFreeSwaps gs) (gsCrossClears gs)
    , gvCombo = fromMaybe 0 (lookup "combo" (gameStatus match3Game gs))
    , gvShuffled = gsShuffled gs
    , gvOver = gsOver gs
    , gvStatus = case gsOver gs of
        Just (Won s) -> PlayWon s
        Just (LevelClear s n) -> PlayCleared s n
        Just (Lost s) -> PlayLost s
        _ -> if gsShuffled gs then PlayShuffled else PlayOn
    , gvGoal = goalInfo (gsGoal gs) (gsProgress gs)
    , gvBoard = boardView gs
    }
  where
    lvl = gsLevel gs
    li = min lvl (levelCount - 1)
    mlvl = lookupLevel li
    mv = gsMoves gs

-- | 桌面版窗口标题（不含末尾的 "  |  " 与提示消息）。
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
-- 目标视图

data GoalInfo = GoalInfo
  { giGoal :: LevelGoal
  , giView :: GoalView
  , giProgress :: Int      -- ^ 核心 gsProgress（桌面 HUD / 标题 / 网页同一个数）
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

-- | 标题里的目标段（score=12/100、collect RED=3/20、stones=3/8 …）。
goalLine :: GoalInfo -> String
goalLine gi = case giView gi of
  ViewScore t -> "score=" ++ show prog ++ "/" ++ show t
  ViewCollect col n -> "collect " ++ colorTag col ++ "=" ++ show prog ++ "/" ++ show n
  ViewCollectMulti _ -> "multi " ++ show prog ++ "/" ++ show (giTarget gi)
  ViewCount k n -> countTag k ++ "=" ++ show prog ++ "/" ++ show n
  ViewOther _ -> "goal=" ++ show prog ++ "/" ++ show (giTarget gi)
  where
    prog = giProgress gi

-- | 提示消息里的收集进度后缀（[RED 3/20]、[chest 1/4]；分数等目标为空串）。
goalBracket :: GoalInfo -> String
goalBracket gi = case giView gi of
  ViewCollect col n -> bracket (colorTag col) n
  ViewCollectMulti _ -> bracket "multi" (giTarget gi)
  ViewCount k n -> bracket (countTag k) n
  _ -> ""
  where
    bracket tag n = " [" ++ tag ++ " " ++ show (giProgress gi) ++ "/" ++ show n ++ "]"

-- | 计数键在窗口标题 / 状态栏里的标签（stones=3/8、[chest 1/4] …；名字目标用元素名）。
countTag :: CounterKey -> String
countTag k = case k of
  CountStones -> "stones"
  CountChests -> "chest"
  CountHoney -> "honey"
  CountBalloons -> "balloon"
  CountCookies -> "cookie"
  CountCakes -> "cake"
  CountSafes -> "safe"
  CountUfo -> "ufo"
  CountCarpets -> "carpet"
  CountSpirits -> "spirit"
  CountColor c -> colorTag c
  CountNamed name -> unElementName name

-- | 颜色的三字母标签。
colorTag :: Color -> String
colorTag C1 = "RED"
colorTag C2 = "GRN"
colorTag C3 = "BLU"
colorTag C4 = "YEL"
colorTag C5 = "PRP"

--------------------------------------------------------------------------------
-- 棋盘视图

-- | 地毯标记：不是地毯格 / 未铺 / 已铺。
data CarpetMark = CarpetNone | CarpetCovered | CarpetOpen
  deriving (Eq, Show)

-- | 棋盘与关卡级元素（字段惰性：只读用到的部分）。
data BoardView = BoardView
  { bvBoard :: Board
  , bvHint :: Maybe (Pos, Pos)       -- ^ 玩家按 H 要到的提示（gsHint；桌面提示光）
  , bvFoundHint :: Maybe (Pos, Pos)  -- ^ 当前盘面上找到的一步（findHint；网页 hint）
  , bvLastCleared :: [Pos]
  , bvGround :: [(Pos, (ElementName, Int))]
  , bvBelts :: [[Pos]]
  , bvPortals :: [(Pos, Pos)]
  , bvUfos :: [Ufo]
  , bvCarpets :: [Pos]               -- ^ 本关地毯格（levelCarpets gsLevel）
  , bvCarpetOpen :: [Pos]
  }

boardView :: GameState -> BoardView
boardView gs =
  BoardView
    { bvBoard = gsBoard gs
    , bvHint = gsHint gs
    , bvFoundHint = findHint (gsBoard gs)
    , bvLastCleared = gsLastCleared gs
    , bvGround = gsGround gs
    , bvBelts = gsBelts gs
    , bvPortals = gsPortals gs
    , bvUfos = gsUfos gs
    , bvCarpets = levelCarpets (gsLevel gs)
    , bvCarpetOpen = gsCarpetOpen gs
    }

-- | 一格的地毯标记：已铺优先；未铺 = 本关地毯格且未铺。
carpetAt :: BoardView -> Pos -> CarpetMark
carpetAt bv pos
  | pos `elem` bvCarpetOpen bv = CarpetOpen
  | pos `elem` bvCarpets bv = CarpetCovered
  | otherwise = CarpetNone

-- | 一格的地面层（元素名, 层数）。
groundAtView :: BoardView -> Pos -> Maybe (ElementName, Int)
groundAtView bv pos = lookup pos (bvGround bv)

--------------------------------------------------------------------------------
-- 关卡进度点

data LevelDot = DotCurrent | DotDone | DotUnlocked | DotLocked
  deriving (Eq, Show)

-- | 各关一个点（共 levelCount 个，下标 = 关卡下标）：当前关、已过、已解锁未过、未解锁。
-- 贴图版传夹紧的下标与 appMaxReached；几何版传 gsLevel 原值、只分当前 / 已过 / 其余。
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
data CellField = FieldInt Int | FieldText String | FieldNull
  deriving (Eq, Show)

-- | 单格的结构化描述：类型标签 + 按固定顺序的字段（网页 JSON 的 t / c / k / i / o / n …；
-- 桌面版按 UI.CellTable 画，不读这里）。
cellFace :: Cell -> (String, [(String, CellField)])
cellFace cell = case cell of
  Gem c k ice ov ->
    ( "G"
    , [ col c, ("k", FieldText (kindCode k)), ("i", FieldInt ice)
      , ("o", maybe FieldNull (FieldText . overlayName) ov), n (maybe 0 overlayLayers ov) ] )
  Stone k -> ("stone", [n k])
  Chest k -> ("chest", [n k])
  Honey k -> ("honey", [n k])
  Balloon c -> ("balloon", [col c])
  Cookie -> ("cookie", [])
  Cake k -> ("cake", [n k])
  MagicHat -> ("hat", [])
  Maker c k -> ("maker", [col c, n k])
  Snail dr dc -> ("snail", [("dr", FieldInt dr), ("dc", FieldInt dc)])
  Safe k -> ("safe", [n k])
  Flip f b -> ("flip", [col f, ("b", FieldInt (colorNum b))])
  Surprise -> ("surprise", [])
  Bottle c -> ("bottle", [col c])
  TimeSpirit -> ("spirit", [])
  Countdown c k -> ("countdown", [col c, n k])
  Custom name v -> ("custom", [("name", FieldText (unElementName name)), ("v", FieldInt (unCustomState v))])
  where
    n k = ("n", FieldInt k)
    col c = ("c", FieldInt (colorNum c))

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
