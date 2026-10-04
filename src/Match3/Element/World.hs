{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素的世界（相当于 xmonad 的 layoutHook）：主流程查询元素行为的**唯一入口**（各 *With 函数）。
--
-- 注册的只是一张**有序的类型列表**（本体种类 'Kind'、叠层 'Layer'、地面层 'GroundKind'），引擎由它解码格子：
-- 叠层由外向内 'peel'（冰层在外、叠层在内，这是 Cell 存储编码决定的），剩下的格子交给本体的 'fromCell'；
-- 都不认识时得到惰性占格 'Inert'。各查询就是在解码出的元素值上调能力类的方法（Match3.Element.Ability）。
--
-- 分派缓存（按 cellSlot / 叠层编号 / Custom 名字建的候选表）是引擎内部的事，不是元素作者写的东西：
-- 某个编号的候选 = 'fromCell' / 'peel' 接受该编号代表格的种类（注册倒序：同名 / 同格以后注册的为准）；
-- 候选都不认领时再按注册倒序试全部本体种类，最后才是惰性占格。规则（Match3.Element.Rules 的 kindRules /
-- layerRules：方法给出的邻格 / 蔓延规则 + 逃生口 boardPasses / layerPasses）也在建世界时收集一次、按次序排好。
--
-- 另持三张规则表——特殊块形状规则、特殊块组合规则、补子策略（'shapeRules' / 'comboRules' / 'refillPolicyWith'）、
-- 关卡级元素（'SomeLevelElement'）的种类表，以及只在一步结算期间有意义的本步上下文（'StepCtx'：魔法地格的扩爆格）。
-- 元素类重构第 4 刀起，旧的注册表 Match3.Element.World（World / Def）并进这里。
module Match3.Element.World
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
  , World
  , mkWorld
  , mkWorldChecked
  , WorldError(..)
  , register
  , worldDefs
  , worldKinds
  , worldLayers
  , worldGrounds
  , lookupDef
  , lookupGround
    -- * 本步上下文
  , StepCtx(..)
  , noStep
    -- * 解码
  , decodeBody
  , decodeLayers
  , decode
  , bodyOf
  , upperOf
  , elementOf
  , elementName
  , topLayerName
    -- * 显示（ViewCaps）
  , faceFieldsWith
  , displayLabelWith
  , loseHintWith
  , displayLabels
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
  , adjacentRules
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
  , endRules
  , PlaceError(..)
  , placeWith
  , hitGroundWith
  , placeAllWith
    -- * 成对交换、开启、改色 / 推动谓词
  , swapRules
  , elementSwapRules
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
    -- * 关卡级元素（消息）
  , registerLevel
  , removeLevel
  , levelDefs
  , askLevels
  ) where

import Control.Monad (foldM)
import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.List (mapAccumL, nub, sortOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (Sum(..))
import Match3.Board.Grid (getCell, setCell)
import Match3.Board.Refill (RefillPolicy, defaultRefill)
import Match3.Element.Ability
import Match3.Element.Class (Message, SomeLevelElement(..), SomeMessage(..), fromMessage, levelNameOf, levelReply)
import Match3.Element.Kind
import Match3.Element.Layer
import Match3.Element.Rules (kindRules, layerRules)
import Match3.Element.Special (comboSwapRule)
import Match3.Element.Types
import Match3.Types

-- | 世界里的一项（注册顺序有意义：名字表、显示名表、规则同优先级时的先后）。
data Def
  = KindDef SomeKind
  | LayerDef SomeLayer
  | GroundDef SomeGround
  | InertDef ElementName  -- ^ 只登记名字的惰性占格（Custom 名字 状态值；放置 = 'customPlace'）

-- | @kindDef \@StoneE@、@layerDef \@Ice@、@groundDef \@Jelly@。
kindDef :: forall e. Kind e => Def
kindDef = KindDef (someKind @e)

layerDef :: forall l. Layer l => Def
layerDef = LayerDef (someLayer @l)

groundDef :: forall g. GroundKind g => Def
groundDef = GroundDef (someGround @g)

-- | 只登记名字的惰性占格：挡交换、无色、会下落、打不动、洗牌保留（测试 / 扩展用）。
inertDef :: ElementName -> Def
inertDef = InertDef

defName :: Def -> ElementName
defName d = case d of
  KindDef (SomeKind p) -> kindName p
  LayerDef (SomeLayer p) -> layerName p
  GroundDef (SomeGround p) -> groundName p
  InertDef n -> n

-- | 注册项的关卡放置（地面层不经放置表）。
defPlace :: Def -> Placer
defPlace d = case d of
  KindDef (SomeKind p) -> place p
  LayerDef (SomeLayer p) -> layerPlace p
  GroundDef _ -> \_ _ -> Nothing
  InertDef n -> customPlace n

-- | 世界：注册项 + 解码缓存 + 规则缓存（各类型的规则收集一次、按次序排好）+ 规则表 + 本步上下文。
-- 用 'mkWorld' / 'register' 构造；字段不导出。
data World = World
  { wDefs     :: [Def]
  , wSlots    :: Array Int [SomeKind]   -- 内置本体编号（cellSlot）→ 候选（注册倒序）
  , wAll      :: [SomeKind]             -- 全部本体（注册倒序；候选都不认领时兜底）
  , wCustom   :: [(ElementName, [SomeKind])]
  , wIce      :: [SomeLayer]            -- 冰层位置的候选
  , wOverlays :: Array Int [SomeLayer]  -- 叠层编号（overlaySlot）→ 候选
  , wAdjacent :: [AdjacentRule]                   -- 按 arOrder 排好（稳定）
  , wEnd      :: [EndRule]                        -- 按 (阶段, erOrder) 排好（稳定）
  , wDiff     :: [(ElementName, CounterKey, Int)] -- 按个数差计数的元素：(名字, 计数键, 每个的奖励步数)
  , wSwap     :: [SwapRule]                       -- 成对交换规则，按 srOrder 排好（稳定）
  , wOpen     :: [OpenRule]                       -- 开启规则（注册顺序）
  , wLevel    :: [SomeLevelElement]               -- 关卡级元素（注册顺序；同名以后注册的为准）
  , wShapes   :: [ShapeRule]                      -- 特殊块形状规则表（有序）
  , wCombos   :: [ComboRule]                      -- 特殊块组合表（有序）
  , wRefill   :: RefillPolicy                     -- 补子策略（关卡级元素可经 Refilling 换掉）
  , wStep     :: StepCtx                          -- 本步上下文（缺省 'noStep'）
  }

-- | 本步上下文：只在一步结算期间有意义的东西，由 Element.Level.levelWorldIn 每步按关卡状态填，注册时为空。
newtype StepCtx = StepCtx
  { stepWiden :: [(Pos, Board -> [Pos] -> [Pos])]  -- ^ 本步的扩爆格（新玩法 8）：格 → 爆炸范围改写（魔法地格）
  }

-- | 空的本步上下文（没有扩爆格）。
noStep :: StepCtx
noStep = StepCtx []

-- | 由注册项建世界（总函数，不报错；检查见 'mkWorldChecked'）。同名多项以后出现的为准，位置取第一次出现处。
-- 规则表（形状 / 组合）为空、补子策略为 defaultRefill、没有关卡级元素。
mkWorld :: [Def] -> World
mkWorld defs0 =
  World
    { wDefs = defs
    , wAdjacent = sortOn arOrder [AdjacentRule o f | AdjacentPass o f <- passes]
    , wEnd = sortOn (\r -> (erPhase r, erOrder r)) [r | EndPass r <- passes]
    , wDiff = [(kindName p, k, bonusMoves p) | KindDef (SomeKind p) <- defs, Just k <- [diffCounter p]]
    , wSwap = sortOn srOrder [r | SwapPass r <- passes]
    , wOpen = [r | OpenPass r <- passes]
    , wLevel = []
    , wShapes = []
    , wCombos = []
    , wRefill = defaultRefill
    , wStep = noStep
    , wSlots = listArray (0, 19) [[k | k <- kinds, any (accepts k) (slotProbes i)] | i <- [0 .. 19]]
    , wAll = kinds
    , wCustom = [(n, ks) | n <- nub [kindName p | SomeKind p <- kinds], let ks = [k | k@(SomeKind p) <- kinds, kindName p == n, any (accepts k) [customCell n s | s <- [0, 1, 5]]], not (null ks)]
    , wIce = [l | l <- layers, any (peels l) iceProbes]
    , wOverlays = listArray (0, 7) [[l | l <- layers, any (peels l) (overlayProbes i)] | i <- [0 .. 7]]
    }
  where
    defs = [d | n <- nub (map defName defs0), Just d <- [listToMaybe (reverse [d' | d' <- defs0, defName d' == n])]]
    kinds = reverse [k | KindDef k <- defs]
    layers = reverse [l | LayerDef l <- defs]
    accepts (SomeKind p) cell = isJust (fromCellAs p cell)
    peels (SomeLayer p) cell = isJust (peelAs p cell)
    passes = concatMap defPasses defs
    defPasses d = case d of
      KindDef (SomeKind p) -> kindRules p
      LayerDef (SomeLayer p) -> layerRules p
      _ -> []

-- | 往世界里加（或按名字替换）一个注册项。测试专用元素就这样接进来，主流程不用改。
-- 关卡级元素、规则表（形状 / 组合 / 补子策略）与本步上下文原样保留。
register :: Def -> World -> World
register d w =
  (mkWorld (wDefs w ++ [d]))
    { wLevel = wLevel w
    , wShapes = wShapes w
    , wCombos = wCombos w
    , wRefill = wRefill w
    , wStep = wStep w
    }

-- | 建世界时发现的注册错误（'mkWorldChecked'）。
data WorldError
  = DuplicateName ElementName        -- ^ 同名注册项出现多次
  | SharedCell String [ElementName]  -- ^ 同一种格子（本体编号 / 冰层 / 叠层编号）被多个种类认领（注册顺序）
  | Unclaimed ElementName            -- ^ 种类不认领任何格子（解码永远轮不到它）
  deriving (Eq, Show)

-- | 由注册项建世界并检查：名字互不相同、每种格子至多一个种类认领、每个本体 / 叠层种类至少认领一种格子。
-- 有错时返回全部错误；没错时与 'mkWorld' 建出同一个世界。
mkWorldChecked :: [Def] -> Either [WorldError] World
mkWorldChecked defs0 = case dups ++ shared ++ unclaimed of
  [] -> Right w
  errs -> Left errs
  where
    w = mkWorld defs0
    names = map defName defs0
    dups = [DuplicateName n | n <- nub names, length (filter (== n) names) > 1]
    kname (SomeKind p) = kindName p
    lname (SomeLayer p) = layerName p
    cells = [("cell " ++ show i, map kname (reverse (wSlots w ! i))) | i <- [0 .. 19]]
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
worldDefs :: World -> [Def]
worldDefs = wDefs

worldKinds :: World -> [SomeKind]
worldKinds w = [k | KindDef k <- wDefs w]

worldLayers :: World -> [SomeLayer]
worldLayers w = [l | LayerDef l <- wDefs w]

worldGrounds :: World -> [SomeGround]
worldGrounds w = [g | GroundDef g <- wDefs w]

-- | 按名字找注册项。
lookupDef :: World -> ElementName -> Maybe Def
lookupDef w n = listToMaybe [d | d <- wDefs w, defName d == n]

-- | 按名字找地面层种类。
lookupGround :: World -> ElementName -> Maybe SomeGround
lookupGround w n = listToMaybe [g | GroundDef g@(SomeGround p) <- wDefs w, groundName p == n]

firstDecode :: [SomeKind] -> Cell -> Maybe SomeElement
firstDecode ks cell = listToMaybe [SomeElement e | SomeKind p <- ks, Just e <- [fromCellAs p cell]]

-- | 本体层的元素值（参数是拆掉叠层之后的格子，或原格：本体不看叠层）。
decodeBody :: World -> Cell -> SomeElement
decodeBody w cell = case cell of
  Custom n _ -> maybe (SomeElement (Inert n cell)) id (lookup n (wCustom w) >>= (`firstDecode` cell))  -- 含 'InertDef'
  _ ->
    let i = cellSlot cell
        cands = if inRange (bounds (wSlots w)) i then wSlots w ! i else []
    in case firstDecode cands cell of
         Just e -> e
         Nothing -> maybe (SomeElement (Inert (ElementName "?") cell)) id (firstDecode (wAll w) cell)

-- | 本体之上的各层（自外向内：冰层 → 叠层）与拆完之后的格子。
decodeLayers :: World -> Cell -> ([SomeLayerValue], Cell)
decodeLayers w cell = case cell of
  Gem _ _ ice ov ->
    let (ls1, c1) = if ice > 0 then tryLayers (wIce w) cell else ([], cell)
        (ls2, c2) = case ov of
          Just o -> tryLayers (wOverlays w ! overlaySlot o) c1
          Nothing -> ([], c1)
    in (ls1 ++ ls2, c2)
  _ -> ([], cell)
  where
    tryLayers ls c = case [(SomeLayerValue l, inner) | SomeLayer p <- ls, Just (l, inner) <- [peelAs p c]] of
      ((lv, inner) : _) -> ([lv], inner)
      [] -> ([], c)

-- | 整个格子的元素值：叠层（自外向内）包着本体。
decode :: World -> Cell -> SomeElement
decode w cell =
  let (ls, inner) = decodeLayers w cell
  in foldr (\(SomeLayerValue l) e -> SomeElement (Layered l e)) (decodeBody w inner) ls

-- | 本体层的元素值（拆掉冰层 / 叠层之后）。
bodyOf :: World -> Cell -> SomeElement
bodyOf w cell = decodeBody w (snd (decodeLayers w cell))

-- | 本体之上的各层（自上而下）：冰层（ice > 0）、叠层。
upperOf :: World -> Cell -> [SomeLayerValue]
upperOf w = fst . decodeLayers w

-- | 整个格子的元素值：叠层（冰 → 叠层，自外向内）包着本体。
elementOf :: World -> Cell -> SomeElement
elementOf = decode

-- | 本体的元素名。
elementName :: World -> Cell -> ElementName
elementName w = nameOf . bodyOf w

-- | 最上面一层的元素名（事件里「波及了什么」用）。
topLayerName :: World -> Cell -> ElementName
topLayerName w cell = case upperOf w cell of
  (lv : _) -> layerValueName lv
  [] -> elementName w cell

--------------------------------------------------------------------------------
-- 显示（只给前端 / 文案用，不参与规则）

-- | 本体格子的显示附加字段（元素的 'face'；未注册的 Custom 名字 = 无）。
faceFieldsWith :: World -> Cell -> [(String, FaceValue)]
faceFieldsWith reg = face . bodyOf reg

-- | 按元素名计数的目标的中文名（本体 'label' / 地面层 'groundLabel'；没登记 = Nothing）。
displayLabelWith :: World -> ElementName -> Maybe String
displayLabelWith reg n = lookupDef reg n >>= defLabel

defLabel :: Def -> Maybe String
defLabel d = case d of
  KindDef (SomeKind p) -> label p
  GroundDef (SomeGround p) -> groundLabel p
  _ -> Nothing

-- | 按元素名计数的目标的失败提示。
loseHintWith :: World -> ElementName -> Maybe (Int -> String)
loseHintWith reg n = lookupDef reg n >>= \d -> case d of
  KindDef (SomeKind p) -> loseHint p
  GroundDef (SomeGround p) -> groundLoseHint p
  _ -> Nothing

-- | 全部登记了中文名的元素：[(元素名, 中文名)]（注册顺序）。
displayLabels :: World -> [(ElementName, String)]
displayLabels reg = [(defName d, l) | d <- worldDefs reg, Just l <- [defLabel d]]

--------------------------------------------------------------------------------
-- 钩子

-- | 参与匹配的颜色：任一层挡匹配则 Nothing，否则本体颜色。（匹配、提示的热路径。）
matchColorWith :: World -> Cell -> Maybe Color
matchColorWith reg = matchColor . elementOf reg

-- | 本体颜色（颜色袋计数用，不看叠层）。
colorOfWith :: World -> Cell -> Maybe Color
colorOfWith reg = color . bodyOf reg

-- | 本格不能被交换（任一层挡）。
blocksSwapWith :: World -> Cell -> Bool
blocksSwapWith reg = blocksSwap . elementOf reg

-- | 只看上层（冰 / 叠层）是否挡交换（彩虹 / 特殊合成提示用，本体由它们自己判定）。
upperBlocksSwapWith :: World -> Cell -> Bool
upperBlocksSwapWith reg = any (\(SomeLayerValue l) -> layerBlocksSwap l) . upperOf reg

-- | 交换两格是否被挡（任一端挡即挡）。
swapBlockedWith :: World -> Board -> Pos -> Pos -> Bool
swapBlockedWith reg b p1 p2 = blocksSwapWith reg (getCell b p1) || blocksSwapWith reg (getCell b p2)

-- | 特殊块能否点火：自上而下第一个有意见的层决定，都没有意见则问本体。
activatesWith :: World -> Cell -> Bool
activatesWith reg = fires . elementOf reg

-- | 本体随重力下落。
fallsWith :: World -> Cell -> Bool
fallsWith reg = falls . bodyOf reg

-- | 本体能穿过传送门。
portalWith :: World -> Cell -> Bool
portalWith reg = portal . bodyOf reg

-- | 本体会被边缘收走。
drainsWith :: World -> Cell -> Bool
drainsWith reg = not . null . drains . bodyOf reg

-- | 本体会在哪些边被收走（段 2c：边缘收集方向可配）。
drainEdgesWith :: World -> Cell -> [Edge]
drainEdgesWith reg = drains . bodyOf reg

-- | 直接命中：叠层自上而下先回答（穿透的问里面），吃掉命中时格子写成新的内容。
directHitWith :: World -> Cell -> Strike
directHitWith reg = struck . elementOf reg

-- | 对一组种子逐格结算直接命中（去重后按顺序）：返回 (新盘面, 被消除的格)。
-- 被消除的格按「后命中的在前」排列（与旧 chipIceOnClear 相同，下游只当集合用）。
chipOnHitWith :: World -> Board -> [Pos] -> (Board, [Pos])
chipOnHitWith reg b seeds = foldl step (b, []) (nub seeds)
  where
    step (board, clearable) p = case directHitWith reg (getCell board p) of
      Absorb cell' -> (setCell board p cell', clearable)
      Destroy -> (board, p : clearable)
      Immune -> (board, clearable)

-- | 直接命中打不动（锤子对它拒绝且不扣次数）。
hitImmuneWith :: World -> Cell -> Bool
hitImmuneWith reg cell = directHitWith reg cell == Immune

-- | 真消除格上随格清掉的叠层（草 / 藤 / 巧）：去掉这些层，其余各层原样盖回。
stripOnClearWith :: World -> Board -> [Pos] -> Board
stripOnClearWith reg b ps = foldl strip b (nub ps)
  where
    strip board p =
      let (ls, inner) = decodeLayers (reg) (getCell board p)
          kept = [lv | lv@(SomeLayerValue l) <- ls, not (layerStripsOnClear l)]
      in if length kept == length ls
           then board
           else setCell board p (foldr (\(SomeLayerValue l) c -> putOn l c) inner kept)

-- | 邻格波及规则（已按 arOrder 排好）。
adjacentRules :: World -> [AdjacentRule]
adjacentRules = wAdjacent
-- | 按顺序跑完一轮的全部邻格波及：返回 (盘面, 打碎的格（按规则顺序拼接）, 新生成需坐住的格)。
-- 每条规则的 acProtect = 起始保护格 ++ 之前各规则的 aoSit。
runAdjacentWith :: World -> [Pos] -> [Pos] -> [Pos] -> Board -> (Board, [Pos], [Pos])
runAdjacentWith reg trueClears direct protect0 b0 =
  let ((b', _), outs) = mapAccumL one (b0, nub protect0) (wAdjacent reg)
  in (b', concatMap fst outs, concatMap snd outs)
  where
    -- 第 9 项：mapAccumL 把「穿过各规则的状态」（盘面, 保护格）和「每条规则的输出」（打碎格, 坐住格）分开；
    -- 第 9 项前是四元组 foldl + 两个前插列表 + reverse（按规则顺序拼接，结果相同）。
    -- 保护格 = nub (起始保护格 ++ 之前的坐住格)，增量维护（nub (xs ++ ys) = nub xs ++ [y | y <- nub ys, y `notElem` xs]）
    one (board, protect) rule =
      let out = arRun rule (AdjCtx trueClears direct protect (recolorableWith reg)) board
          new = aoSit out
      in ((aoBoard out, protect ++ [p | p <- nub new, p `notElem` protect]), (aoDead out, new))

-- | 本体进入清除格时的计数键。
counterWith :: World -> Cell -> Maybe CounterKey
counterWith reg = counter . bodyOf reg

-- | 按前后个数差计数的元素：(名字, 计数键, 每个的奖励步数)（保险箱、时间精灵、自定义）。
diffCountersWith :: World -> [(ElementName, CounterKey, Int)]
diffCountersWith = wDiff

-- | 盘上本体为该元素的格数（盘面是 Foldable：按格 foldMap 到 'Sum'）。
countElementWith :: World -> ElementName -> Board -> Int
countElementWith reg n = getSum . foldMap (\cell -> if elementName reg cell == n then Sum 1 else mempty)

-- | 盘上本体为该元素的格按 'diffWeight' 加权的总数（按差计数用）。权重缺省 1，这时与 'countElementWith' 相同；
-- 新玩法 5 雪怪 Boss 的左上格权重 = 血量，其余格 0。
weighElementWith :: World -> ElementName -> Board -> Int
weighElementWith reg n = getSum . foldMap (\cell -> let e = bodyOf reg cell in if nameOf e == n then Sum (diffWeight e) else mempty)

-- | 本体离开格子（不进清除格）也算覆盖地毯。
vacatesCarpetWith :: World -> Cell -> Bool
vacatesCarpetWith reg = vacatesCarpet . bodyOf reg

-- | 洗牌时原样放回：有冰 / 叠层，或本体要求保留。
keepOnShuffleWith :: World -> Cell -> Bool
keepOnShuffleWith reg = keepOnShuffle . elementOf reg

-- | 本体被消除且能点火时的爆炸范围（不能点火 / 没有爆炸 → []）。新玩法 8：引爆格是本步的扩爆格
-- （'setWidening'，魔法地格）时再按它的改写函数扩大；没有扩爆格（缺省）时就是本体的 blast。
blastWith :: World -> Board -> Cell -> Pos -> [Pos]
blastWith reg b cell p = case blast (bodyOf reg cell) of
  Just f | activatesWith reg cell -> widenAtWith reg b p (f b p)
  _ -> []

-- | 按本步的扩爆格改写一个爆炸范围（p = 引爆格；p 不是扩爆格时原样返回）。
widenAtWith :: World -> Board -> Pos -> [Pos] -> [Pos]
widenAtWith reg b p area = foldl (\a f -> f b a) area [f | (q, f) <- stepWiden (wStep reg), q == p]

-- | 地面层里带扩爆规则（'widenRule'）的格（新玩法 8：魔法地格）与各自的改写函数；地面层按格序。
groundWideningWith :: World -> Ground -> [(Pos, Board -> [Pos] -> [Pos])]
groundWideningWith reg g =
  [(p, w) | (p, (n, _)) <- g, Just (SomeGround gp) <- [lookupGround (reg) n], Just w <- [groundWiden gp]]

-- | 设定本步的扩爆格（新玩法 8；每步结算开始时由 Element.Level.levelWorldIn 调用）。
setWidening :: [(Pos, Board -> [Pos] -> [Pos])] -> World -> World
setWidening ws reg = reg {wStep = (wStep reg) {stepWiden = ws}}

-- | 本步的扩爆格（测试 / 文档用）。
widenedCells :: World -> [Pos]
widenedCells = map fst . stepWiden . wStep

-- | 普通匹配提示是否试这个格（彩虹 = False：它只经成对交换规则给提示）。
hintableWith :: World -> Cell -> Bool
hintableWith reg = hintable . bodyOf reg

-- | 某步末阶段的规则（按 erOrder）。
endRules :: World -> EndPhase -> [EndRule]
endRules reg ph = [r | r <- wEnd reg, erPhase r == ph]

-- | 放置失败的原因（第 6 刀：placeWith 不再直接 error）。
data PlaceError
  = UnknownElement ElementName          -- ^ 注册表里没有这个名字
  | PlaceOutOfBounds ElementName Pos    -- ^ 放置格不在盘面内
  deriving (Eq, Show)

-- | 按名字放置一个元素到若干格（按列表顺序逐格；元素的放置函数对某格返回 Nothing 时该格不变）。
-- 未注册的名字 / 越界格返回 Left（静态关卡数据由 Game.Level.placeStatic 统一转成带关卡名的 error）。
placeWith :: World -> ElementName -> [Arg] -> Board -> [Pos] -> Either PlaceError Board
placeWith reg n args b0 ps = case lookupDef reg n of
  Nothing -> Left (UnknownElement n)
  Just d -> foldM (one d) b0 ps
  where
    one d b p
      | not (inRange (bounds (boardArray b)) p) = Left (PlaceOutOfBounds n p)
      | otherwise = Right (maybe b (setCell b p) (defPlace d args (getCell b p)))

-- | 按顺序应用一张放置表（遇到第一处失败即返回 Left）。
placeAllWith :: World -> Board -> [Placement] -> Either PlaceError Board
placeAllWith reg = foldM (\b (Place n args ps) -> placeWith reg n args b ps)

-- | 地面层被上方消除命中一次（段 2c）：hits = 本轮的消除格（去重），每格至多命中一次。
-- 返回（新地面层，按计数名的去层数）。只有注册为地面层的名字会反应（'groundHit'）；
-- 计数键取 'groundCounter'，只有 CountNamed 返回（结算时并入 gsCounts；其余键忽略）。
hitGroundWith :: World -> [Pos] -> Ground -> (Ground, [(ElementName, Int)])
hitGroundWith reg hits = foldr one ([], [])
  where
    one (p, (n, layers)) (acc, counts)
      | p `elem` hits
      , Just (SomeGround g) <- lookupGround (reg) n =
          let after = groundHit g layers
              removed = layers - maybe 0 id after
              counts' = case groundCounter g of
                Just (CountNamed k) | removed > 0 -> (k, removed) : counts
                _ -> counts
          in (maybe acc (\l -> (p, (n, l)) : acc) after, counts')
      | otherwise = ((p, (n, layers)) : acc, counts)

--------------------------------------------------------------------------------
-- 成对交换、开启、改色 / 推动

-- | 成对交换规则（已按 srOrder 排好）：元素声明的（elementSwapRules）+ 组合表并成的一条（第 8 刀，次序 20）。
swapRules :: World -> [SwapRule]
swapRules reg = case wCombos reg of
  [] -> wSwap reg
  combos -> sortOn srOrder (wSwap reg ++ [comboSwapRule combos])

-- | 只是元素自己声明的成对交换规则（不含组合表；按 srOrder 排好）。
elementSwapRules :: World -> [SwapRule]
elementSwapRules = wSwap

-- | 交换起手：交换前盘面 b0 上第一条成立的成对规则，在交换后盘面 swapped 上给出的种子；都不成立时 Nothing。
swapOpeningWith :: World -> Board -> Board -> Pos -> Pos -> Maybe [Pos]
swapOpeningWith reg b0 swapped p1 p2 =
  listToMaybe [srSeeds r swapped p1 p2 | r <- swapRules reg, srFires r b0 p1 p2]

-- | 是否有成对规则成立（交换前盘面）。
swapFiresWith :: World -> Board -> Pos -> Pos -> Bool
swapFiresWith reg b p1 p2 = any (\r -> srFires r b p1 p2) (swapRules reg)

-- | 一批前沿上的开启（彩蛋类）：依次跑各开启规则，返回 (盘面, 爆炸种子, 本轮坐住的格)。
-- 只有一条规则时结果就是它自己的输出（内置只有彩蛋）。
openWith :: World -> Board -> [Pos] -> (Board, [Pos], [Pos])
openWith reg b front = case wOpen reg of
  [] -> (b, [], [])
  (r : rs) -> foldl step (orOpen r b front) rs
  where
    step (b1, e1, s1) r' =
      let (b2, e2, s2) = orOpen r' b1 front
      in (b2, nub (e1 ++ e2), nub (s1 ++ s2))

-- | 本体可被魔法帽 / 染色瓶改色。
recolorableWith :: World -> Cell -> Bool
recolorableWith reg = recolorable . bodyOf reg

-- | 本体可被蜗牛推动。
pushableWith :: World -> Cell -> Bool
pushableWith reg = pushable . bodyOf reg

--------------------------------------------------------------------------------
-- 规则表（第 8 刀）

-- | 特殊块形状规则表（有序；Board.Clear.spawnSpecialsWith 用）。mkWorld 建出的表为空（不生成特殊块），
-- 内置注册表是 Element.Builtin.Gem.builtinShapeRules。
shapeRules :: World -> [ShapeRule]
shapeRules = wShapes

-- | 换掉形状规则表（扩展一条形状规则 = 把它插到表里合适的位置）。
setShapeRules :: [ShapeRule] -> World -> World
setShapeRules rs reg = reg {wShapes = rs}

-- | 特殊块组合表（有序）。非空时整张表并成一条次序 comboOrder（20）的成对交换规则（见 'swapRules'）。
-- mkWorld 建出的表为空，内置注册表是 Match3.Combos.builtinComboRules。
comboRules :: World -> [ComboRule]
comboRules = wCombos

-- | 换掉组合表。
setComboRules :: [ComboRule] -> World -> World
setComboRules rs reg = reg {wCombos = rs}

-- | 注册表的补子策略（缺省 Board.Refill.defaultRefill）；关卡级元素可以经 Refilling 消息换掉（见 Gravity.activeRefill）。
refillPolicyWith :: World -> RefillPolicy
refillPolicyWith = wRefill

-- | 换掉注册表的补子策略。
setRefillPolicy :: RefillPolicy -> World -> World
setRefillPolicy p reg = reg {wRefill = p}

--------------------------------------------------------------------------------
-- 关卡级元素

-- | 注册（或按名字替换）一个关卡级元素。
registerLevel :: SomeLevelElement -> World -> World
registerLevel d reg = reg {wLevel = [x | x <- wLevel reg, levelNameOf x /= levelNameOf d] ++ [d]}

-- | 去掉一个关卡级元素（测试用：去掉后该机制不生效）。
removeLevel :: ElementName -> World -> World
removeLevel n reg = reg {wLevel = [x | x <- wLevel reg, levelNameOf x /= n]}

-- | 全部关卡级元素（注册顺序）。
levelDefs :: World -> [SomeLevelElement]
levelDefs = wLevel

-- | 问注册的关卡级元素（原型值，不带一局的状态；一局里的节拍见 Match3.Element.Level.askLevelsIn）：
-- 问题与回复同类型（累积器），按注册顺序**折叠所有回复者**（前一个的回复是后一个的问题）；没人回复时 Nothing。
askLevels :: Message q => World -> q -> Maybe q
askLevels reg q0 = foldl one Nothing (wLevel reg)
  where
    one acc (SomeLevelElement l) = case levelReply l (SomeMessage (maybe q0 id acc)) of
      Just (reply, _) | Just q' <- fromMessage reply -> Just q'
      _ -> acc
