{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素的世界（相当于 xmonad 的 layoutHook）：主流程查询元素行为的**唯一入口**（各 *With 函数）。
--
-- 注册的只是一张**有序的列表**（本体 = 原型值 'Archetype'、叠层 'Layer'、地面层 = 'GroundKind' 记录），引擎由它解码格子：
-- 叠层由外向内 'peel'（冰层在外、叠层在内，这是 Cell 存储编码决定的），剩下的格子交给本体原型的存储列；
-- 都不认识时得到惰性占格原型 'inertArch'。各查询就是在解码出的一行（'Row'）上按类型取组件（Match3.ECS.Component），
-- 带叠层的格由 'wholeMatch' / 'wholeHit' / 'wholePhysics' 自上而下合成（合成规则只在这里写一次）。
--
-- 分派缓存（按 cellSlot / 叠层编号 / Custom 名字建的候选表）是引擎内部的事，不是元素作者写的东西：
-- 某个编号的候选 = 存储列 / 'peel' 接受该编号代表格的种类（注册倒序：同名 / 同格以后注册的为准）；
-- 候选都不认领时再按注册倒序试全部本体种类，最后才是惰性占格。各元素带来的 system（原型的 'aSystems'、
-- 叠层的 layerRules）也在建世界时收集一次、按阶段与次序排好（调度表）。
--
-- 另持三张规则表——特殊块形状规则、特殊块组合规则、补子策略（'shapeRules' / 'comboRules' / 'refillPolicyWith'）、
-- 关卡级元素（'SomeMechanic'）的种类表，以及只在一步结算期间有意义的本步上下文（'StepCtx'：魔法地格的扩爆格）。
-- 元素类重构第 4 刀起，旧的注册表模块并进这里（Registry 取代旧注册表类型、Def 取代旧注册项）。
module Match3.ECS.Registry
  ( -- * 注册项
    Def(..)
  , kindDef
  , layerDef
  , groundDef
  , inertDef
  , defName
  , defPlace
  , Placer
    -- * 世界
  , Registry
  , mkRegistry
  , mkRegistryChecked
  , RegistryError(..)
  , register
  , registryDefs
  , registryKinds
  , registryLayers
  , registryGrounds
  , lookupDef
  , lookupGround
    -- * 本步上下文
  , StepCtx(..)
  , noStep
    -- * 解码
  , decodeBody
  , decodeLayers
  , bodyOf
  , recodeWith
  , body
  , upperOf
  , wholeMatch
  , wholeHit
  , wholePhysics
  , elementName
  , topLayerName
    -- * 显示（ViewCaps）
  , faceFieldsWith
  , displayLabelWith
  , loseHintWith
  , displayLabels
  , boardBossHpWith
  , goalIconWith
    -- * 主流程的查询（钩子）
  , matchColorWith
  , colorOfWith
  , blocksSwapWith
  , upperBlocksSwapWith
  , swapBlockedWith
  , activatesWith
  , fallsWith
  , portalWith
  , drainsWith
  , drainEdgesWith
  , directHitWith
  , Strike(..)
  , chipOnHitWith
  , hitImmuneWith
  , stripOnClearWith
  , nearSystems
  , runAdjacentWith
  , counterWith
  , diffCountersWith
  , countElementWith
  , weighElementWith
  , vacatesCarpetWith
  , keepOnShuffleWith
  , blastWith
  , widenAtWith
  , groundWideningWith
  , setWidening
  , widenedCells
  , hintableWith
  , endSystems
  , PlaceError(..)
  , placeWith
  , hitGroundWith
  , placeAllWith
    -- * 成对交换、开启、改色 / 推动谓词
  , swapSystems
  , elementSwapSystems
  , swapOpeningWith
  , swapFiresWith
  , openWith
  , recolorableWith
  , pushableWith
    -- * 规则表（第 8 刀）：特殊块形状、特殊块组合、补子策略
  , shapeRules
  , setShapeRules
  , comboRules
  , setComboRules
  , refillPolicyWith
  , setRefillPolicy
    -- * 关卡级机制（Match3.Element.Mechanic）
  , registerMechanic
  , removeMechanic
  , mechanicDefs
  ) where

import Control.Monad (foldM)
import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.List (nub, sortOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (Sum(..))
import Match3.Board.Grid (getCell, setCell)
import Match3.Board.Refill (RefillPolicy, defaultRefill)
import Match3.Element.Mechanic (SomeMechanic, mechNameOf)
import Data.Maybe (fromMaybe)
import Match3.ECS.Archetype
import Match3.ECS.Component
import Match3.Element.Kind

import Match3.Element.Layer
import Match3.Element.Rules (layerRules)
import Match3.ECS.Stage
import Match3.ECS.System (Scheduled(..), System, at, schedule)
import Match3.Element.Special (comboSwapSystem)
import Match3.Element.Types
import Match3.Types

-- | 世界里的一项（注册顺序有意义：名字表、显示名表、规则同优先级时的先后）。
data Def
  = KindDef SomeArchetype
  | LayerDef SomeLayer
  | GroundDef GroundKind
  | InertDef ElementName  -- ^ 只登记名字的惰性占格（Custom 名字 状态值；放置 = 'customPlace'）

-- | @kindDef stoneArch@、@layerDef \@Ice@、@groundDef magicGround@。
kindDef :: Archetype s -> Def
kindDef = KindDef . SomeArchetype

layerDef :: forall l. Layer l => Def
layerDef = LayerDef (someLayer @l)

groundDef :: GroundKind -> Def
groundDef = GroundDef

-- | 只登记名字的惰性占格：挡交换、无色、会下落、打不动、洗牌保留（测试 / 扩展用）。
inertDef :: ElementName -> Def
inertDef = InertDef

defName :: Def -> ElementName
defName d = case d of
  KindDef a -> archName a
  LayerDef (SomeLayer p) -> layerName p
  GroundDef g -> groundName g
  InertDef n -> n

-- | 注册项的关卡放置（地面层不经放置表）。
defPlace :: Def -> Placer
defPlace d = case d of
  KindDef (SomeArchetype a) -> aSpawn a
  LayerDef (SomeLayer p) -> layerPlace p
  GroundDef _ -> \_ _ -> Nothing
  InertDef n -> customPlace n

-- | 世界：注册项 + 解码缓存 + 规则缓存（各类型的规则收集一次、按次序排好）+ 规则表 + 本步上下文。
-- 用 'mkRegistry' / 'register' 构造；字段不导出。
data Registry = Registry
  { wDefs     :: [Def]
  , wSlots    :: Array Int [SomeArchetype]   -- 内置本体编号（cellSlot）→ 候选原型（注册倒序）
  , wAll      :: [SomeArchetype]             -- 全部本体原型（注册倒序；候选都不认领时兜底）
  , wCustom   :: [(ElementName, [SomeArchetype])]
  , wIce      :: [SomeLayer]            -- 冰层位置的候选
  , wOverlays :: Array Int [SomeLayer]  -- 叠层编号（overlaySlot）→ 候选
  , wNear     :: [Scheduled NearWorld]            -- 邻格 system，按次序排好（稳定）
  , wEnd      :: [EndSys]                         -- 步末 system，按 (阶段, 次序) 排好（稳定）
  , wDiff     :: [(ElementName, CounterKey, Int)] -- 按个数差计数的元素：(名字, 计数键, 每个的奖励步数)
  , wSwap     :: [SwapSys]                        -- 成对交换，按次序排好（稳定）
  , wOpen     :: [System OpenWorld]               -- 开启 system（注册顺序）
  , wLevel    :: [SomeMechanic]                   -- 关卡级机制（注册顺序；同名以后注册的为准）
  , wShapes   :: [ShapeRule]                      -- 特殊块形状规则表（有序）
  , wCombos   :: [ComboRule]                      -- 特殊块组合表（有序）
  , wRefill   :: RefillPolicy                     -- 补子策略（关卡级机制可经 refillPolicy 换掉）
  , wStep     :: StepCtx                          -- 本步上下文（缺省 'noStep'）
  }

-- | 本步上下文：只在一步结算期间有意义的东西，由 Element.Level.levelRegistryIn 每步按关卡状态填，注册时为空。
newtype StepCtx = StepCtx
  { stepWiden :: [(Pos, Board -> [Pos] -> [Pos])]  -- ^ 本步的扩爆格（新玩法 8）：格 → 爆炸范围改写（魔法地格）
  }

-- | 空的本步上下文（没有扩爆格）。
noStep :: StepCtx
noStep = StepCtx []

-- | 由注册项建世界（总函数，不报错；检查见 'mkRegistryChecked'）。同名多项以后出现的为准，位置取第一次出现处。
-- 规则表（形状 / 组合）为空、补子策略为 defaultRefill、没有关卡级元素。
mkRegistry :: [Def] -> Registry
mkRegistry defs0 =
  Registry
    { wDefs = defs
    , wNear = schedule [at o f | SysNear o f <- passes]
    , wEnd = sortOn (\r -> (esPhase r, esOrder r)) [r | SysEnd r <- passes]
    , wDiff = [(aName a, dcKey d, dcBonus d) | KindDef (SomeArchetype a) <- defs, Just d <- [aDiff a]]
    , wSwap = sortOn swOrder [r | SysSwap r <- passes]
    , wOpen = [r | SysOpen r <- passes]
    , wLevel = []
    , wShapes = []
    , wCombos = []
    , wRefill = defaultRefill
    , wStep = noStep
    , wSlots = listArray (0, 19) [[k | k <- kinds, any (accepts k) (slotProbes i)] | i <- [0 .. 19]]
    , wAll = kinds
    , wCustom = [(n, ks) | n <- nub (map archName kinds), let ks = [k | k <- kinds, archName k == n, any (accepts k) [customCell n s | s <- [0, 1, 5]]], not (null ks)]
    , wIce = [l | l <- layers, any (peels l) iceProbes]
    , wOverlays = listArray (0, 7) [[l | l <- layers, any (peels l) (overlayProbes i)] | i <- [0 .. 7]]
    }
  where
    defs = [d | n <- nub (map defName defs0), Just d <- [listToMaybe (reverse [d' | d' <- defs0, defName d' == n])]]
    kinds = reverse [k | KindDef k <- defs]
    layers = reverse [l | LayerDef l <- defs]
    accepts (SomeArchetype a) cell = isJust (colGet (aColumn a) cell)
    peels (SomeLayer p) cell = isJust (peelAs p cell)
    passes = concatMap defPasses defs
    defPasses d = case d of
      KindDef (SomeArchetype a) -> aSystems a
      LayerDef (SomeLayer p) -> layerRules p
      _ -> []

-- | 往世界里加（或按名字替换）一个注册项。测试专用元素就这样接进来，主流程不用改。
-- 关卡级元素、规则表（形状 / 组合 / 补子策略）与本步上下文原样保留。
register :: Def -> Registry -> Registry
register d w =
  (mkRegistry (wDefs w ++ [d]))
    { wLevel = wLevel w
    , wShapes = wShapes w
    , wCombos = wCombos w
    , wRefill = wRefill w
    , wStep = wStep w
    }

-- | 建世界时发现的注册错误（'mkRegistryChecked'）。
data RegistryError
  = DuplicateName ElementName        -- ^ 同名注册项出现多次
  | SharedCell String [ElementName]  -- ^ 同一种格子（本体编号 / 冰层 / 叠层编号）被多个种类认领（注册顺序）
  | Unclaimed ElementName            -- ^ 种类不认领任何格子（解码永远轮不到它）
  deriving (Eq, Show)

-- | 由注册项建世界并检查：名字互不相同、每种格子至多一个种类认领、每个本体 / 叠层种类至少认领一种格子。
-- 有错时返回全部错误；没错时与 'mkRegistry' 建出同一个世界。
mkRegistryChecked :: [Def] -> Either [RegistryError] Registry
mkRegistryChecked defs0 = case dups ++ shared ++ unclaimed of
  [] -> Right w
  errs -> Left errs
  where
    w = mkRegistry defs0
    names = map defName defs0
    dups = [DuplicateName n | n <- nub names, length (filter (== n) names) > 1]
    lname (SomeLayer p) = layerName p
    cells = [("cell " ++ show i, map archName (reverse (wSlots w ! i))) | i <- [0 .. 19]]
      ++ [("ice", map lname (reverse (wIce w)))]
      ++ [("overlay " ++ show i, map lname (reverse (wOverlays w ! i))) | i <- [0 .. 7]]
    shared = [SharedCell c ns | (c, ns) <- cells, length ns > 1]
    claimed = concatMap snd cells ++ map fst (wCustom w)
    unclaimed = [Unclaimed n | d <- wDefs w, isTyped d, let n = defName d, n `notElem` claimed]
    isTyped d = case d of
      KindDef _ -> True
      LayerDef _ -> True
      _ -> False

-- | 各内置本体编号的代表格（已拆掉叠层）。
slotProbes :: Int -> [Cell]
slotProbes i = case i of
  _ | i <= 4 -> [Gem c ([Normal, LineH, LineV, Bomb, Rainbow] !! i) 0 Nothing | c <- [C1, C4]]
  5 -> [Stone 1, Stone 2]
  6 -> [Chest 1, Chest 2]
  7 -> [Honey 1, Honey 2]
  8 -> [Balloon C1, Balloon C4]
  9 -> [Cookie]
  10 -> [Cake 1, Cake 2]
  11 -> [MagicHat]
  12 -> [Maker C1 3, Maker C4 1]
  13 -> [Snail 0 1, Snail 1 0]
  14 -> [Safe 1, Safe 2]
  15 -> [Flip C1 C2]
  16 -> [Surprise]
  17 -> [Bottle C1]
  18 -> [TimeSpirit]
  _ -> [Countdown C1 1, Countdown C4 3]

iceProbes :: [Cell]
iceProbes = [Gem C1 Normal 1 Nothing, Gem C4 Bomb 2 Nothing]

overlayProbes :: Int -> [Cell]
overlayProbes i = [Gem C1 Normal 0 (Just o)]
  where
    o = [Grass, Vine, Choco, Fog 1, Chain 1, Freeze 1, Curtain 1, Steam] !! i

-- | 全部注册项（注册顺序）。
registryDefs :: Registry -> [Def]
registryDefs = wDefs

registryKinds :: Registry -> [SomeArchetype]
registryKinds w = [k | KindDef k <- wDefs w]

registryLayers :: Registry -> [SomeLayer]
registryLayers w = [l | LayerDef l <- wDefs w]

registryGrounds :: Registry -> [GroundKind]
registryGrounds w = [g | GroundDef g <- wDefs w]

-- | 按名字找注册项。
lookupDef :: Registry -> ElementName -> Maybe Def
lookupDef w n = listToMaybe [d | d <- wDefs w, defName d == n]

-- | 按名字找地面层种类。
lookupGround :: Registry -> ElementName -> Maybe GroundKind
lookupGround w n = listToMaybe [g | GroundDef g <- wDefs w, groundName g == n]

-- 候选表上第一个认领该格的原型（显式递归：解码在匹配 / 提示 / 计数的热路径上，不建中间列表）。
firstDecode :: [SomeArchetype] -> Cell -> Maybe Row
firstDecode ks0 cell = go ks0
  where
    go [] = Nothing
    go (SomeArchetype a : ks) = case colGet (aColumn a) cell of
      Just st -> Just (Row a st)
      Nothing -> go ks

-- | 本体层的一行（参数是拆掉叠层之后的格子，或原格：本体不看叠层）。
decodeBody :: Registry -> Cell -> Row
decodeBody w cell = case cell of
  Custom n _ -> fromMaybe (Row (inertArch n) cell) (lookup n (wCustom w) >>= (`firstDecode` cell))
  _ ->
    let i = cellSlot cell
        cands = if inRange (bounds (wSlots w)) i then wSlots w ! i else []
    in case firstDecode cands cell of
         Just r -> r
         Nothing -> fromMaybe (Row (inertArch (ElementName "?")) cell) (firstDecode (wAll w) cell)

-- | 本体之上的各层（自外向内：冰层 → 叠层）与拆完之后的格子。
decodeLayers :: Registry -> Cell -> ([SomeLayerValue], Cell)
decodeLayers w cell
  | hasLayers cell = case cell of
      Gem _ _ ice ov ->
        let !(ls1, c1) = if ice > 0 then tryLayers (wIce w) cell else ([], cell)
            !(ls2, c2) = case ov of
              Just o -> tryLayers (wOverlays w ! overlaySlot o) c1
              Nothing -> ([], c1)
        in (ls1 ++ ls2, c2)
      _ -> ([], cell)
  | otherwise = ([], cell)
  where
    tryLayers [] c = ([], c)
    tryLayers (SomeLayer p : ls) c = case peelAs p c of
      Just (l, inner) -> ([SomeLayerValue l], inner)
      Nothing -> tryLayers ls c

-- | 格子是否可能带冰层 / 叠层（只有宝石格带层；不带层时解码跳过拆层，热路径不分配）。
hasLayers :: Cell -> Bool
hasLayers cell = case cell of
  Gem _ _ ice ov -> ice > 0 || isJust ov
  _ -> False
{-# INLINE hasLayers #-}

-- | 解码再写回：本体行的格子外面按原次序盖回各层（往返测试用；对任何格子都应是恒等）。
recodeWith :: Registry -> Cell -> Cell
recodeWith w cell =
  let (ls, inner) = decodeLayers w cell
   in foldr (\(SomeLayerValue l) c -> putOn l c) (rowCell (decodeBody w inner)) ls

-- | 本体层的一行（拆掉冰层 / 叠层之后）。
bodyOf :: Registry -> Cell -> Row
bodyOf w cell
  | hasLayers cell = decodeBody w (snd (decodeLayers w cell))
  | otherwise = decodeBody w cell

-- | 本体的某个组件（按类型查询）：@body \@Physics world cell@。
body :: Component c => Registry -> Cell -> c
body w = rowGet . bodyOf w
{-# INLINE body #-}

-- | 本体之上的各层（自上而下）：冰层（ice > 0）、叠层。
upperOf :: Registry -> Cell -> [SomeLayerValue]
upperOf w cell
  | hasLayers cell = fst (decodeLayers w cell)
  | otherwise = []

-- | 叠层与本体：(本体一行, [(某层, 该层下面的格子)])（层自外向内；层下面的格子由里层用 'putOn' 重建）。
covered :: Registry -> Cell -> (Row, [(SomeLayerValue, Cell)])
covered w cell =
  let (ls, inner) = decodeLayers w cell
      row = decodeBody w inner
      unders = drop 1 (scanr (\(SomeLayerValue l) c -> putOn l c) (rowCell row) ls)
  in (row, zip ls unders)

-- | 整格的匹配组件：挡匹配 / 挡交换 = 任一层 OR 本体；颜色 / 提示取本体。
wholeMatch :: Registry -> Cell -> Match
wholeMatch w cell
  | hasLayers cell =
      let (row, ls) = covered w cell
          m = rowGet row
      in m { mBlockMatch = any (\(SomeLayerValue l, _) -> layerBlocksMatch l) ls || mBlockMatch m
           , mBlockSwap = any (\(SomeLayerValue l, _) -> layerBlocksSwap l) ls || mBlockSwap m
           }
  | otherwise = body w cell

-- | 整格的命中组件：自上而下，本层有意见（'layerFires' = Just）就听本层点火；命中由本层先回答——
-- 穿透（'Pierce'）时问里层、里层吃掉命中就把本层盖回去；'Keep' 换成新层值；'Peel' 揭掉本层；'Shatter' 本格消除。
-- 爆炸范围取本体。
wholeHit :: Registry -> Cell -> OnHit
wholeHit w cell
  | hasLayers cell = let (row, ls) = covered w cell in foldr cover (rowGet row) ls
  | otherwise = body w cell
  where
    cover (SomeLayerValue l, under) (OnHit st fi bl) =
      let fi' = fromMaybe fi (layerFires l)
       in case layerHit l of
            Pierce -> OnHit (case st of Absorb inner -> Absorb (putOn l inner); r -> r) fi' bl
            Keep l' -> OnHit (Absorb (putOn l' under)) fi' bl
            Peel -> OnHit (Absorb under) fi' bl
            Shatter -> OnHit Destroy fi' bl

-- | 整格的物理组件：有任何一层就洗牌保留，其余取本体。
wholePhysics :: Registry -> Cell -> Physics
wholePhysics w cell
  | hasLayers cell && not (null (upperOf w cell)) = (body w cell) {pKeepShuffle = True}
  | otherwise = body w cell

-- | 本体的元素名。
elementName :: Registry -> Cell -> ElementName
elementName w = rowName . bodyOf w

-- | 最上面一层的元素名（事件里「波及了什么」用）。
topLayerName :: Registry -> Cell -> ElementName
topLayerName w cell = case upperOf w cell of
  (lv : _) -> layerValueName lv
  [] -> elementName w cell

--------------------------------------------------------------------------------
-- 显示（只给前端 / 文案用，不参与规则）

-- | 本体格子的显示附加字段（'Face' 组件的 fExtras；未注册的 Custom 名字 = 无）。
faceFieldsWith :: Registry -> Cell -> [(String, FaceValue)]
faceFieldsWith world = fExtras . body world

-- | 按元素名计数的目标的中文名（本体 'label' / 地面层 'groundLabel'；没登记 = Nothing）。
displayLabelWith :: Registry -> ElementName -> Maybe String
displayLabelWith world n = lookupDef world n >>= defLabel

defLabel :: Def -> Maybe String
defLabel d = case d of
  KindDef (SomeArchetype a) -> hudLabel (aHud a)
  GroundDef g -> groundLabel g
  _ -> Nothing

-- | 按元素名计数的目标的失败提示。
loseHintWith :: Registry -> ElementName -> Maybe (Int -> String)
loseHintWith world n = lookupDef world n >>= \d -> case d of
  KindDef (SomeArchetype a) -> renderLoseHint <$> hudLoseHint (aHud a)
  GroundDef g -> groundLoseHint g
  _ -> Nothing

-- | 全部登记了中文名的元素：[(元素名, 中文名)]（注册顺序）。
displayLabels :: Registry -> [(ElementName, String)]
displayLabels world = [(defName d, l) | d <- registryDefs world, Just l <- [defLabel d]]

-- | 目标名对应的 HUD 血条剩余 HP（原型 'hudShowsHp' 时 = 盘上本元素各格的差计数权重之和；未登记 / 不提供 = Nothing）。
boardBossHpWith :: Registry -> Board -> ElementName -> Maybe Int
boardBossHpWith world board n = case lookupDef world n of
  Just (KindDef (SomeArchetype a)) | hudShowsHp (aHud a) -> Just (weighElementWith world n board)
  _ -> Nothing

-- | 目标图标贴图名（元素 'goalIconName'；未覆盖 = Nothing，调用方用元素名本身）。
goalIconWith :: Registry -> ElementName -> Maybe String
goalIconWith world n = case lookupDef world n of
  Just (KindDef (SomeArchetype a)) -> hudGoalIcon (aHud a)
  _ -> Nothing

--------------------------------------------------------------------------------
-- 钩子

-- | 参与匹配的颜色：任一层挡匹配则 Nothing，否则本体颜色。（匹配、提示的热路径。）
matchColorWith :: Registry -> Cell -> Maybe Color
matchColorWith world cell =
  let m = wholeMatch world cell
   in if mBlockMatch m then Nothing else mColor m

-- | 本体颜色（颜色袋计数用，不看叠层）。
colorOfWith :: Registry -> Cell -> Maybe Color
colorOfWith world = mColor . body world

-- | 本格不能被交换（任一层挡）。
blocksSwapWith :: Registry -> Cell -> Bool
blocksSwapWith world = mBlockSwap . wholeMatch world

-- | 只看上层（冰 / 叠层）是否挡交换（彩虹 / 特殊合成提示用，本体由它们自己判定）。
upperBlocksSwapWith :: Registry -> Cell -> Bool
upperBlocksSwapWith world = any (\(SomeLayerValue l) -> layerBlocksSwap l) . upperOf world

-- | 交换两格是否被挡（任一端挡即挡）。
swapBlockedWith :: Registry -> Board -> Pos -> Pos -> Bool
swapBlockedWith world b p1 p2 = blocksSwapWith world (getCell b p1) || blocksSwapWith world (getCell b p2)

-- | 特殊块能否点火：自上而下第一个有意见的层决定，都没有意见则问本体。
activatesWith :: Registry -> Cell -> Bool
activatesWith world = hFires . wholeHit world

-- | 本体随重力下落。
fallsWith :: Registry -> Cell -> Bool
fallsWith world = pFalls . body world

-- | 本体能穿过传送门。
portalWith :: Registry -> Cell -> Bool
portalWith world = pPortal . body world

-- | 本体会被边缘收走。
drainsWith :: Registry -> Cell -> Bool
drainsWith world = not . null . drainEdgesWith world

-- | 本体会在哪些边被收走（段 2c：边缘收集方向可配）。
drainEdgesWith :: Registry -> Cell -> [Edge]
drainEdgesWith world = pDrains . body world

-- | 直接命中：叠层自上而下先回答（穿透的问里面），吃掉命中时格子写成新的内容。
directHitWith :: Registry -> Cell -> Strike
directHitWith world = hStrike . wholeHit world

-- | 对一组种子逐格结算直接命中（去重后按顺序）：返回 (新盘面, 被消除的格)。
-- 被消除的格按「后命中的在前」排列（与旧 chipIceOnClear 相同，下游只当集合用）。
chipOnHitWith :: Registry -> Board -> [Pos] -> (Board, [Pos])
chipOnHitWith world b seeds = foldl step (b, []) (nub seeds)
  where
    step (board, clearable) p = case directHitWith world (getCell board p) of
      Absorb cell' -> (setCell board p cell', clearable)
      Destroy -> (board, p : clearable)
      Immune -> (board, clearable)

-- | 直接命中打不动（锤子对它拒绝且不扣次数）。
hitImmuneWith :: Registry -> Cell -> Bool
hitImmuneWith world cell = directHitWith world cell == Immune

-- | 真消除格上随格清掉的叠层（草 / 藤 / 巧）：去掉这些层，其余各层原样盖回。
stripOnClearWith :: Registry -> Board -> [Pos] -> Board
stripOnClearWith world b ps = foldl strip b (nub ps)
  where
    strip board p =
      let (ls, inner) = decodeLayers (world) (getCell board p)
          kept = [lv | lv@(SomeLayerValue l) <- ls, not (layerStripsOnClear l)]
      in if length kept == length ls
           then board
           else setCell board p (foldr (\(SomeLayerValue l) c -> putOn l c) inner kept)

-- | 邻格 system（已按次序排好）。
nearSystems :: Registry -> [Scheduled NearWorld]
nearSystems = wNear
-- | 按顺序跑完一轮的全部邻格波及：返回 (盘面, 打碎的格（按规则顺序拼接）, 新生成需坐住的格)。
-- 每条规则的 acProtect = 起始保护格 ++ 之前各规则的 aoSit。
runAdjacentWith :: Registry -> [Pos] -> [Pos] -> [Pos] -> Board -> (Board, [Pos], [Pos])
runAdjacentWith world trueClears direct protect0 b0 =
  runNearStage (wNear world) (nearWorld (recolorableWith world) trueClears direct protect0 b0)

-- | 本体进入清除格时的计数键。
counterWith :: Registry -> Cell -> Maybe CounterKey
counterWith world = tCounter . body world

-- | 按前后个数差计数的元素：(名字, 计数键, 每个的奖励步数)（保险箱、时间精灵、自定义）。
diffCountersWith :: Registry -> [(ElementName, CounterKey, Int)]
diffCountersWith = wDiff

-- | 盘上本体为该元素的格数（盘面是 Foldable：按格 foldMap 到 'Sum'）。
countElementWith :: Registry -> ElementName -> Board -> Int
countElementWith world n = getSum . foldMap (\cell -> if elementName world cell == n then Sum 1 else mempty)

-- | 盘上本体为该元素的格按 'diffWeight' 加权的总数（按差计数用）。权重缺省 1，这时与 'countElementWith' 相同；
-- 新玩法 5 雪怪 Boss 的左上格权重 = 血量，其余格 0。
weighElementWith :: Registry -> ElementName -> Board -> Int
weighElementWith world n = getSum . foldMap (\cell -> case bodyOf world cell of
  row | rowName row == n -> Sum (tDiffWeight (rowGet row))
  _ -> mempty)

-- | 本体离开格子（不进清除格）也算覆盖地毯。
vacatesCarpetWith :: Registry -> Cell -> Bool
vacatesCarpetWith world = tVacatesCarpet . body world

-- | 洗牌时原样放回：有冰 / 叠层，或本体要求保留。
keepOnShuffleWith :: Registry -> Cell -> Bool
keepOnShuffleWith world = pKeepShuffle . wholePhysics world

-- | 本体被消除且能点火时的爆炸范围（不能点火 / 没有爆炸 → []）。新玩法 8：引爆格是本步的扩爆格
-- （'setWidening'，魔法地格）时再按它的改写函数扩大；没有扩爆格（缺省）时就是本体的 blast。
blastWith :: Registry -> Board -> Cell -> Pos -> [Pos]
blastWith world b cell p = case hBlast (body world cell) of
  Just bl | activatesWith world cell -> widenAtWith world b p (blastArea bl b p)
  _ -> []

-- | 按本步的扩爆格改写一个爆炸范围（p = 引爆格；p 不是扩爆格时原样返回）。
widenAtWith :: Registry -> Board -> Pos -> [Pos] -> [Pos]
widenAtWith world b p area = foldl (\a f -> f b a) area [f | (q, f) <- stepWiden (wStep world), q == p]

-- | 地面层里带扩爆规则（'widenRule'）的格（新玩法 8：魔法地格）与各自的改写函数；地面层按格序。
groundWideningWith :: Registry -> Ground -> [(Pos, Board -> [Pos] -> [Pos])]
groundWideningWith world g =
  [(p, w) | (p, (n, _)) <- g, Just gk <- [lookupGround (world) n], Just w <- [groundWiden gk]]

-- | 设定本步的扩爆格（新玩法 8；每步结算开始时由 Element.Level.levelRegistryIn 调用）。
setWidening :: [(Pos, Board -> [Pos] -> [Pos])] -> Registry -> Registry
setWidening ws world = world {wStep = (wStep world) {stepWiden = ws}}

-- | 本步的扩爆格（测试 / 文档用）。
widenedCells :: Registry -> [Pos]
widenedCells = map fst . stepWiden . wStep

-- | 普通匹配提示是否试这个格（彩虹 = False：它只经成对交换规则给提示）。
hintableWith :: Registry -> Cell -> Bool
hintableWith world = mHintable . body world

-- | 某步末阶段的 system（按次序）。
endSystems :: Registry -> EndPhase -> [EndSys]
endSystems world ph = [r | r <- wEnd world, esPhase r == ph]

-- | 放置失败的原因（第 6 刀：placeWith 不再直接 error）。
data PlaceError
  = UnknownElement ElementName          -- ^ 元素世界里没有这个名字
  | PlaceOutOfBounds ElementName Pos    -- ^ 放置格不在盘面内
  deriving (Eq, Show)

-- | 按名字放置一个元素到若干格（按列表顺序逐格；元素的放置函数对某格返回 Nothing 时该格不变）。
-- 未注册的名字 / 越界格返回 Left（静态关卡数据由 Game.Level.placeStatic 统一转成带关卡名的 error）。
placeWith :: Registry -> ElementName -> [Arg] -> Board -> [Pos] -> Either PlaceError Board
placeWith world n args b0 ps = case lookupDef world n of
  Nothing -> Left (UnknownElement n)
  Just d -> foldM (one d) b0 ps
  where
    one d b p
      | not (inRange (bounds (boardArray b)) p) = Left (PlaceOutOfBounds n p)
      | otherwise = Right (maybe b (setCell b p) (defPlace d args (getCell b p)))

-- | 按顺序应用一张放置表（遇到第一处失败即返回 Left）。
placeAllWith :: Registry -> Board -> [Placement] -> Either PlaceError Board
placeAllWith world = foldM (\b (Place n args ps) -> placeWith world n args b ps)

-- | 地面层被上方消除命中一次（段 2c）：hits = 本轮的消除格（去重），每格至多命中一次。
-- 返回（新地面层，按计数名的去层数）。只有注册为地面层的名字会反应（'groundHit'）；
-- 计数键取 'groundCounter'，只有 CountNamed 返回（结算时并入 gsCounts；其余键忽略）。
hitGroundWith :: Registry -> [Pos] -> Ground -> (Ground, [(ElementName, Int)])
hitGroundWith world hits = foldr one ([], [])
  where
    one (p, (n, layers)) (acc, counts)
      | p `elem` hits
      , Just g <- lookupGround (world) n =
          let after = groundHit g layers
              removed = layers - maybe 0 id after
              counts' = case groundCounter g of
                Just (CountNamed k) | removed > 0 -> (k, removed) : counts
                _ -> counts
          in (maybe acc (\l -> (p, (n, l)) : acc) after, counts')
      | otherwise = ((p, (n, layers)) : acc, counts)

--------------------------------------------------------------------------------
-- 成对交换、开启、改色 / 推动

-- | 成对交换规则（已按 swOrder 排好）：元素声明的（elementSwapSystems）+ 组合表并成的一条（第 8 刀，次序 20）。
swapSystems :: Registry -> [SwapSys]
swapSystems world = case wCombos world of
  [] -> wSwap world
  combos -> sortOn swOrder (wSwap world ++ [comboSwapSystem combos])

-- | 只是元素自己声明的成对交换规则（不含组合表；按 swOrder 排好）。
elementSwapSystems :: Registry -> [SwapSys]
elementSwapSystems = wSwap

-- | 交换起手：交换前盘面 b0 上第一条成立的成对规则，在交换后盘面 swapped 上给出的种子；都不成立时 Nothing。
swapOpeningWith :: Registry -> Board -> Board -> Pos -> Pos -> Maybe [Pos]
swapOpeningWith world b0 swapped p1 p2 =
  listToMaybe [swSeeds r swapped p1 p2 | r <- swapSystems world, swFires r b0 p1 p2]

-- | 是否有成对规则成立（交换前盘面）。
swapFiresWith :: Registry -> Board -> Pos -> Pos -> Bool
swapFiresWith world b p1 p2 = any (\r -> swFires r b p1 p2) (swapSystems world)

-- | 一批前沿上的开启（彩蛋类）：依次跑各开启规则，返回 (盘面, 爆炸种子, 本轮坐住的格)。
-- 只有一条规则时结果就是它自己的输出（内置只有彩蛋）。
openWith :: Registry -> Board -> [Pos] -> (Board, [Pos], [Pos])
openWith world = runOpenStage (wOpen world)

-- | 本体可被魔法帽 / 染色瓶改色。
recolorableWith :: Registry -> Cell -> Bool
recolorableWith world = pRecolor . body world

-- | 本体可被蜗牛推动。
pushableWith :: Registry -> Cell -> Bool
pushableWith world = pPush . body world

--------------------------------------------------------------------------------
-- 规则表（第 8 刀）

-- | 特殊块形状规则表（有序；Board.Clear.spawnSpecialsWith 用）。mkRegistry 建出的表为空（不生成特殊块），
-- 内置元素世界是 Element.Builtin.Gem.builtinShapeRules。
shapeRules :: Registry -> [ShapeRule]
shapeRules = wShapes

-- | 换掉形状规则表（扩展一条形状规则 = 把它插到表里合适的位置）。
setShapeRules :: [ShapeRule] -> Registry -> Registry
setShapeRules rs world = world {wShapes = rs}

-- | 特殊块组合表（有序）。非空时整张表并成一条次序 comboOrder（20）的成对交换规则（见 'swapSystems'）。
-- mkRegistry 建出的表为空，内置元素世界是 Match3.Combos.builtinComboRules。
comboRules :: Registry -> [ComboRule]
comboRules = wCombos

-- | 换掉组合表。
setComboRules :: [ComboRule] -> Registry -> Registry
setComboRules rs world = world {wCombos = rs}

-- | 元素世界的补子策略（缺省 Board.Refill.defaultRefill）；关卡级机制可以经 refillPolicy 换掉（见 Gravity.activeRefill）。
refillPolicyWith :: Registry -> RefillPolicy
refillPolicyWith = wRefill

-- | 换掉元素世界的补子策略。
setRefillPolicy :: RefillPolicy -> Registry -> Registry
setRefillPolicy p world = world {wRefill = p}

--------------------------------------------------------------------------------
-- 关卡级元素

-- | 注册（或按名字替换）一个关卡级元素。
registerMechanic :: SomeMechanic -> Registry -> Registry
registerMechanic d world = world {wLevel = [x | x <- wLevel world, mechNameOf x /= mechNameOf d] ++ [d]}

-- | 去掉一个关卡级元素（测试用：去掉后该机制不生效）。
removeMechanic :: ElementName -> Registry -> Registry
removeMechanic n world = world {wLevel = [x | x <- wLevel world, mechNameOf x /= n]}

-- | 全部关卡级元素（注册顺序）。
mechanicDefs :: Registry -> [SomeMechanic]
mechanicDefs = wLevel
