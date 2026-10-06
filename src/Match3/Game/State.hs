{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | 对局状态：GameState 及其（部分字段）相等语义、撤销快照、本步特效 MoveFx 的边沿触发、提示与撤销。
--
-- 依赖：Match3.Types、Match3.Board.*（findHint）、Element.Level（关卡级元素的读写）、Ufo / Conveyor（读数类型）。
-- 不变量：gsCombo / gsLastCleared 只描述最近一次**真正结算**的一步；任何被拒操作经
-- clearMoveFx / rejectMove 清零，moveFx 对 NoMatch / InvalidSwap / 已终局一律返回空，
-- 前端因此不会重播上一步的连击（护栏 failed_swap_resets_combo_feedback 等）。
module Match3.Game.State
  ( GameState(..)
    -- * 关卡级元素（内置五种的派生读数 + 写入函数）
  , gsBelts
  , gsPortals
  , gsUfos
  , gsCarpetOpen
  , gsGround
  , setLevelElem
  , setUfos
  , setBelts
  , setPortals
  , setCarpetOpen
  , setGround
    -- * 透镜（Haskell 特性第 6 项：字段与上面五个派生读数 / 写入对的光学视图）
  , gsBoardL
  , gsMovesL
  , gsHammersL
  , gsFreeSwapsL
  , gsCrossClearsL
  , gsUfosL
  , gsBeltsL
  , gsPortalsL
  , gsCarpetOpenL
  , gsGroundL
  , gsCount
  , gsProgress
  , gsGoalMet
  , gsCollected
  , gsColorBag
  , clearMoveFx
  , rejectMove
  , MoveFx(..)
  , moveFx
  , applyHint
  , applyHintWith
  ) where

import Data.Maybe (isJust)
import Engine.Optics (Lens', lens)
import Match3.Counts (CounterKey(..), Counts, colorBag, countOf, namedCounts)
import Match3.Board.Match (findHintWith)
import Match3.Element.Builtin (BeltLevel(..), BombShapes(..), CarpetLevel(..), CookieDrop(..), RainbowCombos(..), GroundLayer(..), PortalLevel(..), UfoLevel(..), defaultRegistry)
import Match3.Element.Mechanic (Mechanic, SomeMechanic, fromMechanic)
import Match3.Element.Level (levelBelts, levelCarpetOpen, levelGround, levelPortals, levelUfos, putLevel)
import Match3.ECS.Registry (Registry)
import Match3.Ufo (Ufo(..))
import Match3.Conveyor (Belt)
import Match3.Types
import System.Random (StdGen)

-- | 一局的全部规则状态。前端只读；所有修改都经 trySwap / use* / shuffleGame 等纯函数返回新值；撤销历史不在这里（由 Engine.History 持有）。
data GameState = GameState
  { gsBoard         :: Board
  , gsScore         :: Score
  , gsMoves         :: MovesLeft
  , gsGoal          :: LevelGoal
  , gsCounts        :: Counts       -- 按计数键累计（各色清除 CountColor / 石块 / 宝箱 / 蜂蜜罐 / 气球 / 饼干 / 蛋糕 /
                                    -- 保险箱 / 时间精灵 / 飞碟吸收 CountUfo / 地毯覆盖 CountCarpets / 扩展元素 CountNamed 名字），
                                    -- 读法见 gsCount；颜色袋也在这里（gsColorBag / gsCollected 是派生读数）
  , gsGen           :: StdGen
  , gsOver          :: Maybe Terminal  -- 终局（Nothing = 还能走）；只由结算按 terminalOf 写入
  , gsLevel         :: Int
  , gsHint          :: Maybe (Pos, Pos)
  , gsCombo         :: Int   -- last move max cascade wave (0 if none)
  , gsShuffled      :: Bool  -- True if last ensurePlayable reshuffled
  , gsHammers       :: Int    -- hammer booster charges
  , gsFreeSwaps     :: Int    -- free-swap booster charges (any two cells)
  , gsCrossClears   :: Int    -- cross-clear booster charges
  , gsLastCleared   :: [Pos]  -- cells cleared last move (UI particles; not belt/snail noise)
  , gsDaily         :: Bool   -- True for date-seeded daily challenge (通关≠战役推进)
  , gsLevelElems    :: [SomeMechanic]
    -- ^ 一局的全部关卡级元素（状态在元素值里；内置 = 飞碟 / 皮带 / 传送门 / 地毯 / 地面层，
    -- 读数 gsUfos / gsBelts / gsPortals / gsCarpetOpen / gsGround 由它派生）。
    -- 开局见 Match3.Element.Level.startLevelsWith，节拍与写回见同模块。
  }

-- | 传送带路径（派生读数）。
gsBelts :: GameState -> [Belt]
gsBelts = levelBelts . gsLevelElems

-- | 双向传送门对（派生读数）。
gsPortals :: GameState -> [(Pos, Pos)]
gsPortals = levelPortals . gsLevelElems

-- | 飞碟（派生读数）。
gsUfos :: GameState -> [Ufo]
gsUfos = levelUfos . gsLevelElems

-- | 未覆盖的地毯格（派生读数）。
gsCarpetOpen :: GameState -> [Pos]
gsCarpetOpen = levelCarpetOpen . gsLevelElems

-- | 地面层（派生读数；元素名 + 层数）。
gsGround :: GameState -> Ground
gsGround = levelGround . gsLevelElems

-- | 写入一个关卡级元素的状态（同名替换，没有则追加）。
setLevelElem :: Mechanic l => l -> GameState -> GameState
setLevelElem l gs = gs {gsLevelElems = putLevel l (gsLevelElems gs)}

-- | 写入飞碟（替换 'UfoLevel'）。
setUfos :: [Ufo] -> GameState -> GameState
setUfos = setLevelElem . UfoLevel

-- | 写入传送带（替换 'BeltLevel'）。
setBelts :: [Belt] -> GameState -> GameState
setBelts = setLevelElem . BeltLevel

-- | 写入传送门（替换 'PortalLevel'）。
setPortals :: [(Pos, Pos)] -> GameState -> GameState
setPortals = setLevelElem . PortalLevel

-- | 写入未覆盖的地毯格（替换 'CarpetLevel'）。
setCarpetOpen :: [Pos] -> GameState -> GameState
setCarpetOpen = setLevelElem . CarpetLevel

-- | 写入地面层（替换 'GroundLayer'）。
setGround :: Ground -> GameState -> GameState
setGround = setLevelElem . GroundLayer

-- 透镜（Haskell 特性第 6 项，docs/haskell-features/06-测试与光学.md）。
--
-- 字段透镜就是「读字段 + 记录更新」；五个派生读数（gsUfos …）与上面的写入函数（setUfos …）本来就是一对
-- get / set，包成透镜后可以和盘面、单元格的光学复合（如 gsBoardL . cellAt p . overlay . _Fog）。
-- 透镜定律（get-put / put-get / put-put）在 test/Spec/Optics.hs 里检查；派生读数的 get-put 只在
-- 「这种关卡级元素在 gsLevelElems 里恰好一份」时成立（开局总是如此，见 startLevelsWith），测试里同时演示了反例。

gsBoardL :: Lens' GameState Board
gsBoardL = lens gsBoard (\gs v -> gs {gsBoard = v})

gsMovesL :: Lens' GameState MovesLeft
gsMovesL = lens gsMoves (\gs v -> gs {gsMoves = v})

gsHammersL :: Lens' GameState Int
gsHammersL = lens gsHammers (\gs v -> gs {gsHammers = v})

gsFreeSwapsL :: Lens' GameState Int
gsFreeSwapsL = lens gsFreeSwaps (\gs v -> gs {gsFreeSwaps = v})

gsCrossClearsL :: Lens' GameState Int
gsCrossClearsL = lens gsCrossClears (\gs v -> gs {gsCrossClears = v})

gsUfosL :: Lens' GameState [Ufo]
gsUfosL = lens gsUfos (flip setUfos)

gsBeltsL :: Lens' GameState [Belt]
gsBeltsL = lens gsBelts (flip setBelts)

gsPortalsL :: Lens' GameState [(Pos, Pos)]
gsPortalsL = lens gsPortals (flip setPortals)

gsCarpetOpenL :: Lens' GameState [Pos]
gsCarpetOpenL = lens gsCarpetOpen (flip setCarpetOpen)

gsGroundL :: Lens' GameState Ground
gsGroundL = lens gsGround (flip setGround)

-- | 内置关卡级元素之一（Show 按固定字段名打印它们的状态，不进 extras）。
builtinLevel :: SomeMechanic -> Bool
builtinLevel e =
  isJust (fromMechanic e :: Maybe UfoLevel)
    || isJust (fromMechanic e :: Maybe BeltLevel)
    || isJust (fromMechanic e :: Maybe PortalLevel)
    || isJust (fromMechanic e :: Maybe CarpetLevel)
    || isJust (fromMechanic e :: Maybe GroundLayer)
    || isJust (fromMechanic e :: Maybe BombShapes)
    || isJust (fromMechanic e :: Maybe RainbowCombos)
    || isJust (fromMechanic e :: Maybe CookieDrop)

-- | 文本固定的 Show（元素查询快照与测试指纹对 show 取散列，格式不能变）：按派生 Show 的记录格式打印，
-- 皮带 / 传送门 / 飞碟 / 地毯 / 地面层从 gsLevelElems 投影、各计数按固定字段名与位置打印，gsOver 经 'fromTerminal' 按 Outcome 打印。
-- gsElementCounts 打印 namedCounts（按名字升序；快照里每局至多一个名字，顺序无歧义）。
-- 没有固定字段名的键（CountSpirits、扩展元素借用的其余内置键）不打印，相等判断仍比较全部计数。
instance Show GameState where
  showsPrec d gs =
    showParen (d >= 11) $
      showString "GameState {"
        . field "gsBoard" (gsBoard gs) . sep
        . field "gsScore" (gsScore gs) . sep
        . field "gsMoves" (gsMoves gs) . sep
        . field "gsGoal" (gsGoal gs) . sep
        . field "gsCollected" (gsCollected gs) . sep
        . field "gsColorBag" (gsColorBag gs) . sep
        . field "gsStonesCleared" (cnt CountStones) . sep
        . field "gsChestsCleared" (cnt CountChests) . sep
        . field "gsHoneyCleared" (cnt CountHoney) . sep
        . field "gsBalloonsPopped" (cnt CountBalloons) . sep
        . field "gsCookiesCollected" (cnt CountCookies) . sep
        . field "gsCakesCleared" (cnt CountCakes) . sep
        . field "gsSafesOpened" (cnt CountSafes) . sep
        . field "gsGen" (gsGen gs) . sep
        . field "gsOver" (fromTerminal <$> gsOver gs) . sep
        . field "gsLevel" (gsLevel gs) . sep
        . field "gsHint" (gsHint gs) . sep
        . field "gsCombo" (gsCombo gs) . sep
        . field "gsShuffled" (gsShuffled gs) . sep
        . field "gsBelts" (gsBelts gs) . sep
        . field "gsPortals" (gsPortals gs) . sep
        . field "gsHammers" (gsHammers gs) . sep
        . field "gsFreeSwaps" (gsFreeSwaps gs) . sep
        . field "gsCrossClears" (gsCrossClears gs) . sep
        . field "gsUfos" (gsUfos gs) . sep
        . field "gsUfoCollected" (cnt CountUfo) . sep
        . field "gsCarpetOpen" (gsCarpetOpen gs) . sep
        . field "gsCarpetsCovered" (cnt CountCarpets) . sep
        . field "gsLastCleared" (gsLastCleared gs) . sep
        . field "gsDaily" (gsDaily gs) . sep
        . field "gsElementCounts" (namedCounts (gsCounts gs)) . sep
        . field "gsGround" (gsGround gs)
        . extras
        . showChar '}'
    where
      -- 内置之外的关卡级元素（扩展）：有才打印，内置对局不多出文本
      extras = case [e | e <- gsLevelElems gs, not (builtinLevel e)] of
        [] -> id
        es -> sep . field "gsLevelExtra" es
      cnt k = gsCount k gs
      sep = showString ", "
      field :: Show a => String -> a -> ShowS
      field name v = showString name . showString " = " . showsPrec 0 v

instance Eq GameState where
  a == b =
    gsBoard a == gsBoard b
      && gsScore a == gsScore b
      && gsMoves a == gsMoves b
      && gsGoal a == gsGoal b
      && gsCounts a == gsCounts b
      && gsOver a == gsOver b
      && gsLevel a == gsLevel b
      && gsDaily a == gsDaily b
      && gsHammers a == gsHammers b
      && gsFreeSwaps a == gsFreeSwaps b
      && gsCrossClears a == gsCrossClears b
      && gsLevelElems a == gsLevelElems b

-- | 某计数键的累计个数（缺省 0；如石块清除数 = gsCount CountStones）。
gsCount :: CounterKey -> GameState -> Int
gsCount k = countOf k . gsCounts

-- | 目标进度（HUD / 标题 / 网页的主进度，Match3.Goal.goalProgress）。
gsProgress :: GameState -> Int
gsProgress gs = goalProgress (gsGoal gs) (gsScore gs) (gsCounts gs)

-- | 当前计数是否满足关卡目标。
gsGoalMet :: GameState -> Bool
gsGoalMet gs = goalMet (gsGoal gs) (gsScore gs) (gsCounts gs)

-- | 不计分数的目标进度（派生读数）：分数目标恒 0，其余等于 gsProgress（金标准 col= 锁定）。
gsCollected :: GameState -> Int
gsCollected gs = goalProgress (gsGoal gs) 0 (gsCounts gs)

-- | 各色累计清除数（派生读数），按 allColors 顺序、含 0。
gsColorBag :: GameState -> [(Color, Int)]
gsColorBag = colorBag . gsCounts

-- | 清空「上一步」的 UI 反馈字段（连击波数 / 本步清除格）。
-- 这两个字段只描述最近一次**真正结算**的一步；任何没有结算的操作（无匹配回滚、
-- 挡交换、非相邻、道具无效、洗牌、撤销）都必须把它们归零，否则前端会把旧值
-- 当成新一步的结果，再播一遍爆击（连击）特效。规则判定不读这两个字段。
clearMoveFx :: GameState -> GameState
clearMoveFx gs = gs { gsCombo = 0, gsLastCleared = [] }

-- | 交换 / 道具被拒（NoMatch）时的回滚状态：盘面不变，清提示与本步反馈。
rejectMove :: GameState -> GameState
rejectMove gs = (clearMoveFx gs) { gsHint = Nothing, gsShuffled = False }

-- | 一次操作之后，前端该播的特效（边沿触发，只看这一次调用的结果）。
data MoveFx = MoveFx
  { fxCombo   :: Int    -- ^ 本步连击波数；> 1 才播连击 / 爆击特效
  , fxCleared :: [Pos]  -- ^ 本步清除格（闪光 + 粒子）
  } deriving (Eq, Show)

-- | 由「操作前状态、操作后状态、结果」决定要不要播特效。
-- 只有这次调用真正结算了一步（MoveApplied / 本次才产生的 Won / Lost / LevelClear）
-- 才返回 after 的连击与清除格；NoMatch / InvalidSwap / 操作前就已结束（trySwap 把已有
-- 终局换回 Outcome 原样返回）一律返回空，不会重播上一步的爆击特效。
moveFx :: GameState -> GameState -> Outcome -> MoveFx
moveFx before after out
  | isJust (gsOver before) = noFx
  | otherwise = case out of
      NoMatch -> noFx
      InvalidSwap -> noFx
      _ -> MoveFx (gsCombo after) (gsLastCleared after)
  where
    noFx = MoveFx 0 []

-- | 计算一手可走的交换并记在 gsHint（不改盘面）。
applyHint :: GameState -> (GameState, Maybe (Pos, Pos))
applyHint = applyHintWith defaultRegistry

-- | applyHint（指定元素世界）：可走判定用这张表里的挡交换 / 匹配色定义。
applyHintWith :: Registry -> GameState -> (GameState, Maybe (Pos, Pos))
applyHintWith world gs =
  let h = findHintWith world (gsBoard gs)
  in (gs { gsHint = h }, h)
