{-# LANGUAGE OverloadedStrings #-}
-- | 关卡级元素：不在格子里的机制。第 7 刀（7a）起状态在元素值里（一局的全部关卡级元素 = GameState.gsLevelElems），
-- 取代第 7 刀前 GameState 的专用字段 gsUfos / gsBelts / gsPortals / gsCarpetOpen / gsGround。
--
-- ecs-5 起每种机制是一条原型记录（Match3.Element.Mechanic 的 'Mechanic'：名字 + 读数组件 'LevelView' +
-- 按节拍的 system 'MechSys'），状态类型仍是各自的 newtype（GameState 的 Show 按类型认内置机制）。
-- 各自另给一个装箱函数（@ufoLevel us@ = 原型 + 状态）。去掉（removeMechanic）即不生效。
-- 地面层 GroundLayer 是核心机制，定义在 Match3.Element.Mechanic（这里再导出）。
module Match3.Element.Builtin.Level
  ( -- * 状态类型
    UfoLevel(..)
  , BeltLevel(..)
  , PortalLevel(..)
  , CarpetLevel(..)
  , GroundLayer(..)
  , BombShapes(..)
  , RainbowCombos(..)
  , CookieDrop(..)
    -- * 原型
  , ufoMech
  , beltMech
  , portalMech
  , carpetMech
  , groundLayerMech
  , bombShapesMech
  , rainbowCombosMech
  , cookieDropMech
    -- * 装箱
  , ufoLevel
  , beltLevel
  , portalLevel
  , carpetLevel
  , groundLayer
  , bombShapes
  , rainbowCombos
  , cookieDrop
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
import Match3.Element.Mechanic
import Match3.Levels.Level (DropSpec(..), Level(..))
import Match3.Types
import Match3.Ufo (Ufo, mkUfo, stepUfos)
import System.Random (RandomGen)

-- | 飞碟：每轮补子之后（OnRefilled）整轮吸收并移动。开局 = 关卡记录的飞碟；没有放置而目标是飞碟吸收时放一个 (1,3) C1。
newtype UfoLevel = UfoLevel [Ufo]
  deriving (Eq, Show)

ufoMech :: Mechanic UfoLevel
ufoMech = (mechanic "ufo")
  { mechView = \(UfoLevel us) -> noView {lvUfos = Just us}
  , mechSystems =
      [ OnStart $ \lvl _ ->
          if null (lvlUfos lvl)
            then UfoLevel (case goalView (lvlGoal lvl) of
              ViewCount CountUfo _ -> [mkUfo (1, 3) C1]
              _ -> [])
            else UfoLevel (lvlUfos lvl)
      , OnRefilled $ \b acc (UfoLevel us) -> let (ps, us') = stepUfos b us in Just (acc ++ ps, UfoLevel us')
      ]
  }

ufoLevel :: [Ufo] -> SomeMechanic
ufoLevel = SomeMechanic ufoMech . UfoLevel

-- | 皮带：玩家交换的步末、倒计时之后（OnEndTick）给出移位；会走的元素跳过皮带格（AnswerAvoid）。
newtype BeltLevel = BeltLevel [Belt]
  deriving (Eq, Show)

beltMech :: Mechanic BeltLevel
beltMech = (mechanic "belt")
  { mechView = \(BeltLevel bs) -> noView {lvBelts = Just bs}
  , mechSystems =
      [ OnStart (\lvl _ -> BeltLevel (lvlBelts lvl))
      , OnEndTick $ \acc (BeltLevel bs) -> if null bs then Nothing else Just (acc ++ beltMoves bs, BeltLevel bs)
      , AnswerAvoid $ \acc (BeltLevel bs) -> if null bs then Nothing else Just (acc ++ concat bs)
      ]
  }

beltLevel :: [Belt] -> SomeMechanic
beltLevel = SomeMechanic beltMech . BeltLevel

-- | 传送门：沉降时（OnSettling）传送可穿门的本体；端点是会走元素的墙（AnswerWall）。
newtype PortalLevel = PortalLevel [(Pos, Pos)]
  deriving (Eq, Show)

portalMech :: Mechanic PortalLevel
portalMech = (mechanic "portal")
  { mechView = \(PortalLevel ps) -> noView {lvPortals = Just ps}
  , mechSystems =
      [ OnStart (\lvl _ -> PortalLevel (lvlPortals lvl))
      , OnSettling $ \canPass mb (PortalLevel ps) -> Just (portalTeleport canPass ps mb, PortalLevel ps)
      , AnswerWall $ \acc (PortalLevel ps) -> Just (acc ++ concatMap (\(a, b) -> [a, b]) ps)
      ]
  }

portalLevel :: [(Pos, Pos)] -> SomeMechanic
portalLevel = SomeMechanic portalMech . PortalLevel

-- | 地毯：步末结算时（OnCovering）覆盖目标格。
newtype CarpetLevel = CarpetLevel [Pos]
  deriving (Eq, Show)

carpetMech :: Mechanic CarpetLevel
carpetMech = (mechanic "carpet")
  { mechView = \(CarpetLevel ps) -> noView {lvCarpetOpen = Just ps}
  , mechSystems =
      [ OnStart $ \lvl _ ->
          if null (lvlCarpets lvl)
            then CarpetLevel (case goalView (lvlGoal lvl) of
              ViewCount CountCarpets n ->
                take (max n 1)
                  [ (3, 2), (3, 3), (3, 4), (3, 5)
                  , (4, 2), (4, 3), (4, 4), (4, 5)
                  , (2, 2), (2, 5), (5, 2), (5, 5)
                  ]
              _ -> [])
            else CarpetLevel (lvlCarpets lvl)
      , OnCovering $ \hit n (CarpetLevel open0) -> let (open', k) = coverCarpets open0 hit in Just (n + k, CarpetLevel open')
      ]
  }

carpetLevel :: [Pos] -> SomeMechanic
carpetLevel = SomeMechanic carpetMech . CarpetLevel

-- | 规则开关「L / T 形生成炸弹」。
newtype BombShapes = BombShapes Bool
  deriving (Eq, Show)

bombShapesMech :: Mechanic BombShapes
bombShapesMech = (mechanic "bomb_shapes")
  { mechSystems =
      [ OnStart (\lvl _ -> BombShapes ("bomb_shapes" `elem` lvlRules lvl))
      , AnswerShapes $ \rules (BombShapes on) -> if on then Just (withBombShapes rules) else Nothing
      ]
  }

bombShapes :: Bool -> SomeMechanic
bombShapes = SomeMechanic bombShapesMech . BombShapes

-- | 规则开关「魔力鸟组合增强」。
newtype RainbowCombos = RainbowCombos Bool
  deriving (Eq, Show)

rainbowCombosMech :: Mechanic RainbowCombos
rainbowCombosMech = (mechanic "rainbow_combos")
  { mechSystems =
      [ OnStart (\lvl _ -> RainbowCombos ("rainbow_combos" `elem` lvlRules lvl))
      , AnswerMorph $ \b0 swapped p1 p2 (RainbowCombos on) ->
          if on then (\(n, cells, seeds) -> Morph n cells seeds) <$> rainbowComboMorph b0 swapped p1 p2 else Nothing
      ]
  }

rainbowCombos :: Bool -> SomeMechanic
rainbowCombos = SomeMechanic rainbowCombosMech . RainbowCombos

-- | 掉落口（新玩法 6）。
newtype CookieDrop = CookieDrop [DropSpec]
  deriving (Eq, Show)

cookieDropMech :: Mechanic CookieDrop
cookieDropMech = (mechanic "cookie_drop")
  { mechView = \(CookieDrop ds) -> noView {lvDrops = Just (concatMap dropCells ds)}
  , mechSystems =
      [ OnStart (\lvl _ -> CookieDrop (lvlDrops lvl))
      , AnswerRefill $ \p (CookieDrop ds) -> if null ds then Nothing else Just (dropRefill ds p)
      ]
  }

cookieDrop :: [DropSpec] -> SomeMechanic
cookieDrop = SomeMechanic cookieDropMech . CookieDrop

-- | 掉落口补子：每个空洞先照原策略补（随机数照常消耗，所以生成器的推进与没有掉落口时相同），
-- 若空洞是某个掉落口格、且此刻盘上（已补的格子算在内）与 dropCell 同种的格少于 dropKeep 个，就换成 dropCell。
-- 「同种」= 同一个 Custom 名字（不看状态值：新玩法 7 的变色龙掉下来是 C1，换过色的仍算同种），内置格按相等
-- （饼干 = Cookie，与第 46 关原先的逐格相等相同）。多个掉落口规格取第一个满足的；同一次补子里先补的掉落口先占名额（行优先）。
dropRefill :: [DropSpec] -> RefillPolicy -> RefillPolicy
dropRefill ds base = RefillPolicy (refillName base ++ "+drop") pick
  where
    pick :: RandomGen g => RefillCtx -> g -> (Cell, g)
    pick ctx g =
      let (c, g') = refillCell base ctx g
          onBoard d = length (filter (maybe False (sameKind (dropCell d))) (toList (rcBoard ctx)))
      in case [d | d <- ds, rcPos ctx `elem` dropCells d, onBoard d < dropKeep d] of
           d : _ -> (dropCell d, g')
           [] -> (c, g')
    sameKind x y = case (x, y) of
      (Custom n _, Custom m _) -> n == m
      _ -> x == y

-- | 传送门的实现（传送门的 OnSettling system 调用；第 7 刀前在 Board.Gravity）：可穿门谓词由元素世界给出。
portalTeleport :: (Cell -> Bool) -> [(Pos, Pos)] -> MBoard -> MBoard
portalTeleport canPort pairs mb =
  -- Each pair teleports at most one way per settle (A→B else B→A) to avoid bounce-back.
  foldl tryPair mb (nub pairs)
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
