{-# LANGUAGE OverloadedStrings #-}
-- | 关卡级元素：不在格子里的机制。第 7 刀（7a）起状态在元素值里（一局的全部关卡级元素 = GameState.gsLevelElems），
-- 取代第 7 刀前 GameState 的专用字段 gsUfos / gsBelts / gsPortals / gsCarpetOpen / gsGround。
--
-- 共同特征：都是 LevelElement，主流程在固定的流水线节拍上发消息（Match3.Element.Message），各自只回复
-- 自己关心的那几条，回复时交回推进后的自身：飞碟 = 补子之后（Refilled）、皮带 = 步末倒计时之后（EndTicked）
-- 与会走元素的避让格（AvoidCells）、传送门 = 沉降时（Settling）与会走元素的墙（WallCells）、
-- 地毯 = 步末结算（Covering）、地面层 = 每轮之后（GroundHit）。开局状态由关卡记录给出（levelStart）。
-- 规则开关 BombShapes = 每步结算开始时（Shaping）改本关的形状表。
-- 掉落口 CookieDrop（新玩法 6）= 补子策略查询（Refilling）时包一层「掉落口格补收集物」。
-- 前四种与规则开关去掉（removeLevel）即不生效；地面层是核心元素（levelCore），其中每层的行为由注册表的地面层条目决定。
module Match3.Element.Builtin.Level
  ( UfoLevel(..)
  , BeltLevel(..)
  , PortalLevel(..)
  , CarpetLevel(..)
  , GroundLayer(..)
  , BombShapes(..)
  , RainbowCombos(..)
  , CookieDrop(..)
  , dropRefill
  , portalTeleport
  ) where

import Data.Foldable (toList)
import Data.List (nub)
import Match3.Board.Grid (MBoard, atM, setM)
import Match3.Board.Refill (RefillCtx(..), RefillPolicy(..))
import Match3.Carpet (coverCarpets)
import Match3.Counts (CounterKey(..))
import Match3.Conveyor (Belt, beltMoves)
import Match3.Combos (rainbowComboMorph)
import Match3.Element.Builtin.Gem (withBombShapes)
import Match3.Element.Class
import Match3.Levels.Level (DropSpec(..), Level(..))
import Match3.Element.Message
import Match3.Types
import Match3.Ufo (Ufo, mkUfo, stepUfos)

-- | 飞碟：每轮补子之后（Refilled）整轮吸收并移动。开局 = 关卡记录的飞碟；没有放置而目标是飞碟吸收时放一个 (1,3) C1。
newtype UfoLevel = UfoLevel [Ufo]
  deriving (Eq, Show)

instance LevelElement UfoLevel where
  levelName _ = "ufo"
  levelReply (UfoLevel us) msg = case fromMessage msg of
    Just (Refilled b acc) -> let (ps, us') = stepUfos b us in Just (SomeMessage (Refilled b (acc ++ ps)), UfoLevel us')
    Nothing -> Nothing
  levelStart lvl _
    | null (lvlUfos lvl) = UfoLevel (case goalView (lvlGoal lvl) of
        ViewCount CountUfo _ -> [mkUfo (1, 3) C1]
        _ -> [])
    | otherwise = UfoLevel (lvlUfos lvl)

-- | 皮带：玩家交换的步末、倒计时之后（EndTicked）给出移位；会走的元素跳过皮带格（AvoidCells）。
-- 没有皮带时什么都不回复（= 没有皮带后的再连锁，与第 7 刀前「皮带为空」相同）。
newtype BeltLevel = BeltLevel [Belt]
  deriving (Eq, Show)

instance LevelElement BeltLevel where
  levelName _ = "belt"
  levelReply (BeltLevel belts) msg
    | null belts = Nothing
    | Just (EndTicked acc) <- fromMessage msg = Just (SomeMessage (EndTicked (acc ++ beltMoves belts)), BeltLevel belts)
    | Just (AvoidCells acc) <- fromMessage msg = Just (SomeMessage (AvoidCells (acc ++ concat belts)), BeltLevel belts)
    | otherwise = Nothing
  levelStart lvl _ = BeltLevel (lvlBelts lvl)

-- | 传送门：沉降时（Settling）传送可穿门的本体；端点是会走元素的墙（WallCells）。
newtype PortalLevel = PortalLevel [(Pos, Pos)]
  deriving (Eq, Show)

instance LevelElement PortalLevel where
  levelName _ = "portal"
  levelReply (PortalLevel ps) msg
    | Just (Settling canPass mb) <- fromMessage msg = Just (SomeMessage (Settling canPass (portalTeleport canPass ps mb)), PortalLevel ps)
    | Just (WallCells acc) <- fromMessage msg = Just (SomeMessage (WallCells (acc ++ concatMap (\(a, b) -> [a, b]) ps)), PortalLevel ps)
    | otherwise = Nothing
  levelStart lvl _ = PortalLevel (lvlPortals lvl)

-- | 地毯：步末结算时（Covering）覆盖目标格。开局 = 关卡记录的地毯；没有放置而目标是地毯时按固定表铺 max n 1 格。
newtype CarpetLevel = CarpetLevel [Pos]
  deriving (Eq, Show)

instance LevelElement CarpetLevel where
  levelName _ = "carpet"
  levelReply (CarpetLevel open0) msg = case fromMessage msg of
    Just (Covering hit n) -> let (open', k) = coverCarpets open0 hit in Just (SomeMessage (Covering hit (n + k)), CarpetLevel open')
    Nothing -> Nothing
  levelStart lvl _
    | null (lvlCarpets lvl) = CarpetLevel (case goalView (lvlGoal lvl) of
        ViewCount CountCarpets n ->
          take (max n 1)
            [ (3, 2), (3, 3), (3, 4), (3, 5)
            , (4, 2), (4, 3), (4, 4), (4, 5)
            , (2, 2), (2, 5), (5, 2), (5, 5)
            ]
        _ -> [])
    | otherwise = CarpetLevel (lvlCarpets lvl)

-- | 地面层（段 2c 的扩展槽，内置有双层果冻）：格子下面的层（元素名 + 层数）。每轮之后（GroundHit）按注册表的
-- 地面层规则被命中；核心元素（不经 removeLevel 开关）。开局 = 关卡记录的地面层。
newtype GroundLayer = GroundLayer Ground
  deriving (Eq, Show)

instance LevelElement GroundLayer where
  levelName _ = "ground"
  levelReply (GroundLayer g) msg = case fromMessage msg of
    Just (GroundHit hitG hits acc) -> let (g', cs) = hitG hits g in Just (SomeMessage (GroundHit hitG hits (acc ++ cs)), GroundLayer g')
    Nothing -> Nothing
  levelStart lvl _ = GroundLayer (lvlGround lvl)
  levelCore _ = True

-- | 规则开关「L / T 形生成炸弹」（新玩法，开心消消乐的爆炸特效）：本关的 lvlRules 含 "bomb_shapes" 时开，
-- 每步结算开始时回复形状表查询（Shaping），把 L / T 规则插进本关的形状表；关着时什么都不回复（= 内置表，
-- 原有关卡与每日挑战不受影响）。状态不变，不参与撤销差异；GameState 的 Show 不打印它（内置元素）。
newtype BombShapes = BombShapes Bool
  deriving (Eq, Show)

instance LevelElement BombShapes where
  levelName _ = "bomb_shapes"
  levelReply (BombShapes on) msg
    | on, Just (Shaping rules) <- fromMessage msg = Just (SomeMessage (Shaping (withBombShapes rules)), BombShapes on)
    | otherwise = Nothing
  levelStart lvl _ = BombShapes ("bomb_shapes" `elem` lvlRules lvl)

-- | 规则开关「魔力鸟组合增强」（新玩法 4）：本关的 lvlRules 含 "rainbow_combos" 时开，玩家交换成立前回复
-- 变身查询（Morphing）：彩虹 × 直线 → 同色普通宝石全部变直线再引爆，彩虹 × 炸弹 → 全部变炸弹再引爆
-- （规则见 Match3.Combos.rainbowComboMorph）。关着时什么都不回复（= 原有彩虹取色，原有关卡与每日挑战不受影响）。
-- 状态不变；GameState 的 Show 不打印它（内置元素）。
newtype RainbowCombos = RainbowCombos Bool
  deriving (Eq, Show)

instance LevelElement RainbowCombos where
  levelName _ = "rainbow_combos"
  levelReply (RainbowCombos on) msg
    | on
    , Just (Morphing b0 swapped p1 p2 Nothing) <- fromMessage msg
    , Just (n, cells, seeds) <- rainbowComboMorph b0 swapped p1 p2 =
        Just (SomeMessage (Morphing b0 swapped p1 p2 (Just (Morph n cells seeds))), RainbowCombos on)
    | otherwise = Nothing
  levelStart lvl _ = RainbowCombos ("rainbow_combos" `elem` lvlRules lvl)

-- | 掉落口（新玩法 6，开心消消乐的金豆荚掉落口）：本关的 lvlDrops 非空时，回复补子策略查询（Refilling），
-- 把收到的策略包一层 'dropRefill'；没有掉落口时什么都不回复（= 原策略，原有关卡与每日挑战不受影响）。
-- 状态不变；GameState 的 Show 不打印它（内置元素）。
newtype CookieDrop = CookieDrop [DropSpec]
  deriving (Eq, Show)

instance LevelElement CookieDrop where
  levelName _ = "cookie_drop"
  levelReply (CookieDrop ds) msg
    | not (null ds), Just (Refilling p) <- fromMessage msg = Just (SomeMessage (Refilling (dropRefill ds p)), CookieDrop ds)
    | otherwise = Nothing
  levelStart lvl _ = CookieDrop (lvlDrops lvl)

-- | 掉落口补子：每个空洞先照原策略补（随机数照常消耗，所以生成器的推进与没有掉落口时相同），
-- 若空洞是某个掉落口格、且此刻盘上（已补的格子算在内）与 dropCell 同种的格少于 dropKeep 个，就换成 dropCell。
-- 「同种」= 同一个 Custom 名字（不看状态值：新玩法 7 的变色龙掉下来是 C1，换过色的仍算同种），内置格按相等
-- （饼干 = Cookie，与第 46 关原先的逐格相等相同）。多个掉落口规格取第一个满足的；同一次补子里先补的掉落口先占名额（行优先）。
dropRefill :: [DropSpec] -> RefillPolicy -> RefillPolicy
dropRefill ds base = RefillPolicy (refillName base ++ "+drop") pick
  where
    pick ctx g =
      let (c, g') = refillCell base ctx g
          onBoard d = length (filter (maybe False (sameKind (dropCell d))) (toList (rcBoard ctx)))
      in case [d | d <- ds, rcPos ctx `elem` dropCells d, onBoard d < dropKeep d] of
           d : _ -> (dropCell d, g')
           [] -> (c, g')
    sameKind x y = case (x, y) of
      (Custom n _, Custom m _) -> n == m
      _ -> x == y

-- | 传送门的实现（PortalLevel 回复 Settling 时调用；第 7 刀前在 Board.Gravity）：可穿门谓词由注册表给出。
portalTeleport :: (Cell -> Bool) -> [(Pos, Pos)] -> MBoard -> MBoard
portalTeleport canPort portals mb =
  -- Each pair teleports at most one way per settle (A→B else B→A) to avoid bounce-back.
  foldl tryPair mb (nub portals)
  where
    transferable (Just cell) = canPort cell
    transferable Nothing = False
    tryPair m (a, b) =
      case (atM m a, atM m b) of
        (ca, Nothing)
          | transferable ca -> setM (setM m a Nothing) b ca
        (Nothing, cb)
          | transferable cb -> setM (setM m b Nothing) a cb
        _ -> m
