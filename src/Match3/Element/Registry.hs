-- | 元素注册表：元素名 → 构造器（'Entry'），以及主流程查询元素行为的**唯一入口**（各 *With 函数）。
--
-- 构造器负责两件事：从格子解码出元素值（盘面以 'Cell' 存储）、按关卡放置参数写格子（关卡解析用）。
-- 分派：内置本体按 cellSlot 编号、叠层按 overlaySlot 编号用数组 O(1) 取解码器；冰层单独一个；
-- Custom 按名字查（未注册的名字退回 'Inert'：挡交换、打不动、会下落的惰性占格）。
-- 一个格子解码成「修饰器（冰 → 叠层）包着本体」的元素值（'elementOf'），各查询就是在它上面取能力（Element.Class 的查询函数）。
-- 关卡级元素（'SomeLevelElement'）的种类表（注册顺序；一局的状态与节拍见 Match3.Element.Level）。
--
-- 第 8 刀：另持三张规则表——特殊块形状规则、特殊块组合规则、补子策略（'shapeRules' / 'comboRules' / 'refillPolicyWith'）。
--
-- 依赖：Element.Class / Message / Types / Special、Match3.Types、Board.Grid、Board.Refill。不含任何具体元素（内置见 Element.Builtin）。
module Match3.Element.Registry
  ( Registry
  , Entry
  , entryName
  , entrySlot
  , Placer
    -- * 构造器
  , bodyEntry
  , customEntry
  , customEntryWith
  , modifierEntry
  , groundEntry
  , inertEntry
  , mkRegistry
  , mkRegistryChecked
  , RegistryError(..)
  , register
  , registryDefs
  , lookupElement
    -- * 解码
  , bodyOf
  , upperOf
  , elementOf
  , elementName
  , topLayerName
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
import Data.Array (Array, accumArray, bounds, inRange, (!))
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Match3.Board.Grid (getCell, setCell)
import Match3.Board.Refill (RefillPolicy, defaultRefill)
import Match3.Element.Class
import Match3.Element.Special (comboSwapRule)
import Match3.Element.Types
import Match3.Types

-- | 关卡放置：给出参数与原格，返回新格（Nothing = 不放）。
type Placer = [Arg] -> Cell -> Maybe Cell

-- | 构造器的原型值与解码器。
data Proto
  = PBody SomeElement (Cell -> Maybe SomeElement)   -- 内置本体槽位
  | PCustom SomeElement (CustomState -> SomeElement)  -- Custom 名字 状态值
  | PMod SomeModifier (Cell -> Maybe SomeModifier)  -- 冰层 / 叠层
  | PGround SomeElement                             -- 地面层（gsGround 里按名字）

-- | 注册表条目 = 名字 → 构造器（原型值 + 解码器 + 放置）。
data Entry = Entry
  { entryName  :: ElementName
  , entrySlot  :: Slot
  , entryProto :: Proto
  , entryPlace :: Placer
  }

-- | 内置本体槽位的构造器：原型值、解码器、放置。槽号由原型写回的格子推导（'cellSlot'），
-- 原型写回 Custom 时推不出内置槽位（'SlotNone'，mkRegistryChecked 报错）。
bodyEntry :: Element e => e -> (Cell -> Maybe e) -> Placer -> Entry
bodyEntry proto dec = Entry (name proto) (bodySlot (toCell proto)) (PBody (SomeElement proto) (fmap SomeElement . dec))

-- | 本体格子的内置槽位。
bodySlot :: Cell -> Slot
bodySlot cell = case cell of
  Custom _ _ -> SlotNone
  _ -> SlotCell (cellSlot cell)

-- | 自定义本体（格子 = Custom 名字 状态值）：原型值、状态值 → 元素值；放置 = Custom 名字 参数（缺省 1）。
-- 第 6b 刀：状态值是 CustomState（newtype），解码函数收 CustomState。
customEntry :: Element e => e -> (CustomState -> e) -> Entry
customEntry proto mk = customEntryWith proto mk (\args _ -> Just (Custom n (CustomState (case args of (AInt k : _) -> k; _ -> 1))))
  where
    n = name proto

-- | 自定义本体，放置自定。
customEntryWith :: Element e => e -> (CustomState -> e) -> Placer -> Entry
customEntryWith proto mk = Entry (name proto) SlotCustom (PCustom (SomeElement proto) (SomeElement . mk))

-- | 修饰器（冰层 / 叠层）的构造器：原型值、解码器、放置。槽位由原型写到一颗裸宝石上的结果推导：
-- 盖了叠层 = SlotOverlay ('overlaySlot')，否则有冰层 = SlotIce，都没有 = 'SlotNone'。
modifierEntry :: Modifier m => m -> (Cell -> Maybe m) -> Placer -> Entry
modifierEntry proto dec = Entry (modName proto) (modifierSlot proto) (PMod (SomeModifier proto) (fmap SomeModifier . dec))

modifierSlot :: Modifier m => m -> Slot
modifierSlot m = case modApply m (Gem C1 Normal 0 Nothing) of
  Gem _ _ _ (Just o) -> SlotOverlay (overlaySlot o)
  Gem _ _ ice Nothing | ice > 0 -> SlotIce
  _ -> SlotNone

-- | 地面层元素（不占格；层数在 gsGround 里）：规则取原型值的 'ground' 与 'counter'；不经放置表放。
groundEntry :: Element e => e -> Entry
groundEntry proto = Entry (name proto) SlotGround (PGround (SomeElement proto)) (\_ _ -> Nothing)

-- | 惰性占格（旧 baseDef 的等价物）：挡交换、无色、会下落、打不动、洗牌保留。
inertEntry :: ElementName -> Entry
inertEntry n = customEntry (Inert n (Custom n (CustomState 1))) (Inert n . Custom n)

-- | 注册表。用 mkRegistry / register 构造；字段不导出（分派数组由条目列表派生）。
data Registry = Registry
  { regDefs     :: [Entry]                          -- 注册顺序
  , regCells    :: Array Int (Cell -> SomeElement)  -- 内置本体的解码器（按 cellSlot；边界由条目算出）
  , regOverlays :: Array Int (Cell -> Maybe SomeModifier)  -- 叠层（按 overlaySlot；边界由条目算出）
  , regIce      :: Cell -> Maybe SomeModifier
  , regCustom   :: [(ElementName, CustomState -> SomeElement)]
  , regGround   :: [(ElementName, SomeElement)]
  , regAdjacent :: [AdjacentRule]                   -- 按 arOrder 排好（稳定）
  , regEnd      :: [EndRule]                        -- 按 (阶段, erOrder) 排好（稳定）
  , regDiff     :: [(ElementName, CounterKey, Int)]    -- 按个数差计数的元素：(名字, 计数键, 每个的奖励步数)
  , regSwap     :: [SwapRule]                       -- 成对交换规则，按 srOrder 排好（稳定）
  , regOpen     :: [OpenRule]                       -- 开启规则（注册顺序）
  , regLevel    :: [SomeLevelElement]                      -- 关卡级元素（注册顺序；同名以后注册的为准）
  , regShapes   :: [ShapeRule]                      -- 特殊块形状规则表（有序；第 8 刀）
  , regCombos   :: [ComboRule]                      -- 特殊块组合表（有序；第 8 刀）
  , regRefill   :: RefillPolicy                     -- 补子策略（第 8 刀；关卡级元素可经 Refilling 换掉）
  , regWiden    :: [(Pos, Board -> [Pos] -> [Pos])]  -- 本步的扩爆格（新玩法 8）：格 → 爆炸范围改写；缺省空，
                                                    -- 每步由 Element.Level.levelRegistryIn 按地面层的 widenRule 填
  }

-- | 建表时发现的条目错误（'mkRegistryChecked'）。
data RegistryError
  = DuplicateName ElementName        -- ^ 同名条目出现多次
  | DuplicateSlot Slot [ElementName] -- ^ 同一内置槽位（本体 / 叠层 / 冰层）被多个条目占用
  | NoSlot ElementName               -- ^ 原型推不出内置槽位（'SlotNone'）
  deriving (Eq, Show)

-- | 由条目列表建表并检查：名字互不相同、内置槽位互不相同、每个内置构造器都推得出槽位。
-- 有错时返回全部错误（按条目顺序）；没错时与 'mkRegistry' 建出同一张表。
mkRegistryChecked :: [Entry] -> Either [RegistryError] Registry
mkRegistryChecked defs =
  case dupNames ++ dupSlots ++ noSlots of
    [] -> Right (mkRegistry defs)
    errs -> Left errs
  where
    names = map entryName defs
    dupNames = [DuplicateName n | n <- nub names, length (filter (== n) names) > 1]
    builtinSlot sl = case sl of
      SlotCell _ -> True
      SlotOverlay _ -> True
      SlotIce -> True
      _ -> False
    slots = nub [entrySlot d | d <- defs, builtinSlot (entrySlot d)]
    dupSlots =
      [ DuplicateSlot sl ns
      | sl <- slots
      , let ns = [entryName d | d <- defs, entrySlot d == sl]
      , length ns > 1
      ]
    noSlots = [NoSlot (entryName d) | d <- defs, entrySlot d == SlotNone]

-- | 由条目列表建表（总函数，不报错）。同一槽位 / 同名的多个条目以后出现的为准；推不出槽位的条目不参与分派。
-- 分派数组的边界由条目的槽号算出，查不到的槽号退回缺省（本体 = 惰性占格，叠层 = 无）。
mkRegistry :: [Entry] -> Registry
mkRegistry defs0 =
  let defs = dedupe defs0
      opaque cell = SomeElement (Inert (ElementName "?") cell)
      cellDecs = [(i, \cell -> fromMaybe (opaque cell) (dec cell)) | Entry {entrySlot = SlotCell i, entryProto = PBody _ dec} <- defs]
      ovDecs = [(i, dec) | Entry {entrySlot = SlotOverlay i, entryProto = PMod _ dec} <- defs]
      cells = accumArray (\_ d -> d) opaque (slotBounds (map fst cellDecs)) cellDecs
      ovs = accumArray (\_ d -> d) (const Nothing) (slotBounds (map fst ovDecs)) ovDecs
      ice = fromMaybe (const Nothing) (listToMaybe (reverse [dec | Entry {entrySlot = SlotIce, entryProto = PMod _ dec} <- defs]))
      bodies = concat [bodyProto d | d <- defs]
  in Registry
       { regDefs = defs
       , regCells = cells
       , regOverlays = ovs
       , regIce = ice
       , regCustom = [(entryName d, mk) | d@Entry {entryProto = PCustom _ mk} <- defs]
       , regGround = [(entryName d, g) | d@Entry {entryProto = PGround g} <- defs]
       , regAdjacent = sortOn arOrder (concatMap ruleAdj defs)
       , regEnd = sortOn (\r -> (erPhase r, erOrder r)) (concatMap ruleEnd defs)
       , regDiff = [(name e, k, bonusMoves e) | e <- bodies, Just k <- [diffCounter e]]
       , regSwap = sortOn srOrder (mapMaybe swapRule bodies)
       , regOpen = mapMaybe openRule bodies
       , regLevel = []
       , regShapes = []
       , regCombos = []
       , regRefill = defaultRefill
       , regWiden = []
       }
  where
    -- 同名只留最后一个，位置取第一次出现处（注册顺序稳定）
    dedupe ds =
      [ d
      | n <- nub (map entryName ds)
      , Just d <- [listToMaybe (reverse [d' | d' <- ds, entryName d' == n])]
      ]
    -- 槽号 0..最大者；没有条目时为空区间
    slotBounds is = (0, maximum (-1 : is))
    bodyProto d = case entryProto d of
      PBody e _ -> [e]
      PCustom e _ -> [e]
      PGround e -> [e]
      PMod _ _ -> []
    ruleAdj d = case entryProto d of
      PMod (SomeModifier m) _ -> maybe [] pure (modAdjacent m)
      _ -> concatMap (maybe [] pure . adjacentRule) (bodyProto d)
    ruleEnd d = case entryProto d of
      PMod (SomeModifier m) _ -> maybe [] pure (modEnd m)
      _ -> concatMap (maybe [] pure . endRule) (bodyProto d)

-- | 往注册表里加（或按名字替换）一个条目。测试专用元素就这样接进来，主流程不用改。
-- 关卡级元素与规则表（形状 / 组合 / 补子策略）原样保留。
register :: Entry -> Registry -> Registry
register d reg =
  (mkRegistry (regDefs reg ++ [d]))
    { regLevel = regLevel reg
    , regShapes = regShapes reg
    , regCombos = regCombos reg
    , regRefill = regRefill reg
    , regWiden = regWiden reg
    }

-- | 全部条目（注册顺序）。
registryDefs :: Registry -> [Entry]
registryDefs = regDefs

-- | 按名字找条目（关卡放置、计数、文档用）。
lookupElement :: Registry -> ElementName -> Maybe Entry
lookupElement reg n = listToMaybe [d | d <- regDefs reg, entryName d == n]

--------------------------------------------------------------------------------
-- 解码

-- | 本体层的元素值。
bodyOf :: Registry -> Cell -> SomeElement
bodyOf reg cell = case cell of
  Custom n k -> maybe (SomeElement (Inert n cell)) ($ k) (lookup n (regCustom reg))
  _ -> slotAt (regCells reg) (cellSlot cell) (\c -> SomeElement (Inert (ElementName "?") c)) cell

-- | 按槽号取分派数组里的解码器；越界（注册表里没有该槽位的条目）取缺省。
slotAt :: Array Int a -> Int -> a -> a
slotAt arr i dflt
  | inRange (bounds arr) i = arr ! i
  | otherwise = dflt

-- | 本体之上的各层（自上而下）：冰层（ice > 0）、叠层。
upperOf :: Registry -> Cell -> [SomeModifier]
upperOf reg cell = case cell of
  Gem _ _ ice ov ->
    [m | ice > 0, Just m <- [regIce reg cell]]
      ++ [m | Just o <- [ov], Just m <- [slotAt (regOverlays reg) (overlaySlot o) (const Nothing) cell]]
  _ -> []

-- | 整个格子的元素值：修饰器（冰 → 叠层，自上而下）包着本体。
elementOf :: Registry -> Cell -> SomeElement
elementOf reg cell = foldr (\(SomeModifier m) e -> modify m e) (bodyOf reg cell) (upperOf reg cell)

-- | 本体的元素名。
elementName :: Registry -> Cell -> ElementName
elementName reg = name . bodyOf reg

-- | 最上面一层的元素名（事件里「波及了什么」用）。
topLayerName :: Registry -> Cell -> ElementName
topLayerName reg cell = case upperOf reg cell of
  (SomeModifier m : _) -> modName m
  [] -> elementName reg cell

--------------------------------------------------------------------------------
-- 钩子

-- | 参与匹配的颜色：任一上层挡匹配则 Nothing，否则本体颜色。（匹配、提示的热路径。）
matchColorWith :: Registry -> Cell -> Maybe Color
matchColorWith reg cell
  | any (\(SomeModifier m) -> modBlocksMatch m) (upperOf reg cell) = Nothing
  | otherwise = matchColor (bodyOf reg cell)

-- | 本体颜色（颜色袋计数用，不看叠层）。
colorOfWith :: Registry -> Cell -> Maybe Color
colorOfWith reg = color . bodyOf reg

-- | 本格不能被交换（任一层挡）。
blocksSwapWith :: Registry -> Cell -> Bool
blocksSwapWith reg = blocksSwap . elementOf reg

-- | 只看上层（冰 / 叠层）是否挡交换（彩虹 / 特殊合成提示用，本体由它们自己判定）。
upperBlocksSwapWith :: Registry -> Cell -> Bool
upperBlocksSwapWith reg = any (\(SomeModifier m) -> modBlocksSwap m) . upperOf reg

-- | 交换两格是否被挡（任一端挡即挡）。
swapBlockedWith :: Registry -> Board -> Pos -> Pos -> Bool
swapBlockedWith reg b p1 p2 = blocksSwapWith reg (getCell b p1) || blocksSwapWith reg (getCell b p2)

-- | 特殊块能否点火：自上而下第一个有意见的层决定；都没有意见则 False。
activatesWith :: Registry -> Cell -> Bool
activatesWith reg = fromMaybe False . activates . elementOf reg

-- | 本体随重力下落。
fallsWith :: Registry -> Cell -> Bool
fallsWith reg = falls . bodyOf reg

-- | 本体能穿过传送门。
portalWith :: Registry -> Cell -> Bool
portalWith reg = portal . bodyOf reg

-- | 本体会被边缘收走。
drainsWith :: Registry -> Cell -> Bool
drainsWith reg = not . null . drains . bodyOf reg

-- | 本体会在哪些边被收走（段 2c：边缘收集方向可配）。
drainEdgesWith :: Registry -> Cell -> [Edge]
drainEdgesWith reg = drains . bodyOf reg

-- | 直接命中：修饰器自上而下先说（穿透的问里面），吃掉命中时格子写成新的元素值。
directHitWith :: Registry -> Cell -> HitResult
directHitWith reg cell = case onHit (elementOf reg cell) of
  Absorb e -> HitAbsorb (toCell e)
  Destroy -> HitDestroy
  Immune -> HitImmune

-- | 对一组种子逐格结算直接命中（去重后按顺序）：返回 (新盘面, 被消除的格)。
-- 被消除的格按「后命中的在前」排列（与旧 chipIceOnClear 相同，下游只当集合用）。
chipOnHitWith :: Registry -> Board -> [Pos] -> (Board, [Pos])
chipOnHitWith reg b seeds = foldl step (b, []) (nub seeds)
  where
    step (board, clearable) p = case directHitWith reg (getCell board p) of
      HitAbsorb cell' -> (setCell board p cell', clearable)
      HitDestroy -> (board, p : clearable)
      HitImmune -> (board, clearable)
      HitPierce -> (board, clearable)

-- | 直接命中打不动（锤子对它拒绝且不扣次数）。
hitImmuneWith :: Registry -> Cell -> Bool
hitImmuneWith reg cell = directHitWith reg cell == HitImmune

-- | 真消除格上随格清掉的叠层（草 / 藤 / 巧）。
stripOnClearWith :: Registry -> Board -> [Pos] -> Board
stripOnClearWith reg b ps = foldl strip b (nub ps)
  where
    strip board p = case getCell board p of
      cell@(Gem col kind ice (Just o))
        | Just (SomeModifier m) <- slotAt (regOverlays reg) (overlaySlot o) (const Nothing) cell
        , modStripOnClear m ->
            setCell board p (Gem col kind ice Nothing)
      _ -> board

-- | 邻格波及规则（已按 arOrder 排好）。
adjacentRules :: Registry -> [AdjacentRule]
adjacentRules = regAdjacent

-- | 按顺序跑完一轮的全部邻格波及：返回 (盘面, 打碎的格（按规则顺序拼接）, 新生成需坐住的格)。
-- 每条规则的 acProtect = 起始保护格 ++ 之前各规则的 aoSit。
runAdjacentWith :: Registry -> [Pos] -> [Pos] -> [Pos] -> Board -> (Board, [Pos], [Pos])
runAdjacentWith reg trueClears direct protect0 b0 =
  let (b', deadRev, sitsRev, _) = foldl one (b0, [], [], nub protect0) (regAdjacent reg)
  in (b', concat (reverse deadRev), concat (reverse sitsRev))
  where
    -- 打碎格 / 坐住格按规则反向累积，收尾再反转拼接；保护格 = nub (起始保护格 ++ 之前的坐住格)，
    -- 增量维护（nub (xs ++ ys) = nub xs ++ [y | y <- nub ys, y `notElem` xs]）
    one (board, deadRev, sitsRev, protect) rule =
      let out = arRun rule (AdjCtx trueClears direct protect (recolorableWith reg)) board
          new = aoSit out
      in (aoBoard out, aoDead out : deadRev, new : sitsRev, protect ++ [p | p <- nub new, p `notElem` protect])

-- | 本体进入清除格时的计数键。
counterWith :: Registry -> Cell -> Maybe CounterKey
counterWith reg = counter . bodyOf reg

-- | 按前后个数差计数的元素：(名字, 计数键, 每个的奖励步数)（保险箱、时间精灵、自定义）。
diffCountersWith :: Registry -> [(ElementName, CounterKey, Int)]
diffCountersWith = regDiff

-- | 盘上本体为该元素的格数。
countElementWith :: Registry -> ElementName -> Board -> Int
countElementWith reg n b = length [() | cell <- boardCells b, elementName reg cell == n]

-- | 盘上本体为该元素的格按 'diffWeight' 加权的总数（按差计数用）。权重缺省 1，这时与 'countElementWith' 相同；
-- 新玩法 5 雪怪 Boss 的左上格权重 = 血量，其余格 0。
weighElementWith :: Registry -> ElementName -> Board -> Int
weighElementWith reg n b = sum [diffWeight (bodyOf reg cell) | cell <- boardCells b, elementName reg cell == n]

-- | 本体离开格子（不进清除格）也算覆盖地毯。
vacatesCarpetWith :: Registry -> Cell -> Bool
vacatesCarpetWith reg = vacatesCarpet . bodyOf reg

-- | 洗牌时原样放回：有冰 / 叠层，或本体要求保留。
keepOnShuffleWith :: Registry -> Cell -> Bool
keepOnShuffleWith reg = keepOnShuffle . elementOf reg

-- | 本体被消除且能点火时的爆炸范围（不能点火 / 没有爆炸 → []）。新玩法 8：引爆格是本步的扩爆格
-- （'setWidening'，魔法地格）时再按它的改写函数扩大；没有扩爆格（缺省）时就是本体的 blast。
blastWith :: Registry -> Board -> Cell -> Pos -> [Pos]
blastWith reg b cell p = case blast (bodyOf reg cell) of
  Just f | activatesWith reg cell -> widenAtWith reg b p (f b p)
  _ -> []

-- | 按本步的扩爆格改写一个爆炸范围（p = 引爆格；p 不是扩爆格时原样返回）。
widenAtWith :: Registry -> Board -> Pos -> [Pos] -> [Pos]
widenAtWith reg b p area = foldl (\a w -> w b a) area [w | (q, w) <- regWiden reg, q == p]

-- | 地面层里带扩爆规则（'widenRule'）的格（新玩法 8：魔法地格）与各自的改写函数；地面层按格序。
groundWideningWith :: Registry -> Ground -> [(Pos, Board -> [Pos] -> [Pos])]
groundWideningWith reg g =
  [(p, w) | (p, (n, _)) <- g, Just e <- [lookup n (regGround reg)], Just w <- [widenRule e]]

-- | 设定本步的扩爆格（新玩法 8；每步结算开始时由 Element.Level.levelRegistryIn 调用）。
setWidening :: [(Pos, Board -> [Pos] -> [Pos])] -> Registry -> Registry
setWidening ws reg = reg {regWiden = ws}

-- | 本步的扩爆格（测试 / 文档用）。
widenedCells :: Registry -> [Pos]
widenedCells = map fst . regWiden

-- | 普通匹配提示是否试这个格（彩虹 = False：它只经成对交换规则给提示）。
hintableWith :: Registry -> Cell -> Bool
hintableWith reg = hintable . bodyOf reg

-- | 某步末阶段的规则（按 erOrder）。
endRules :: Registry -> EndPhase -> [EndRule]
endRules reg ph = [r | r <- regEnd reg, erPhase r == ph]

-- | 放置失败的原因（第 6 刀：placeWith 不再直接 error）。
data PlaceError
  = UnknownElement ElementName          -- ^ 注册表里没有这个名字
  | PlaceOutOfBounds ElementName Pos    -- ^ 放置格不在盘面内
  deriving (Eq, Show)

-- | 按名字放置一个元素到若干格（按列表顺序逐格；元素的放置函数对某格返回 Nothing 时该格不变）。
-- 未注册的名字 / 越界格返回 Left（静态关卡数据由 Game.Level.placeStatic 统一转成带关卡名的 error）。
placeWith :: Registry -> ElementName -> [Arg] -> Board -> [Pos] -> Either PlaceError Board
placeWith reg n args b0 ps = case lookupElement reg n of
  Nothing -> Left (UnknownElement n)
  Just d -> foldM (one d) b0 ps
  where
    one d b p
      | not (inRange (bounds (boardArray b)) p) = Left (PlaceOutOfBounds n p)
      | otherwise = Right (maybe b (setCell b p) (entryPlace d args (getCell b p)))

-- | 按顺序应用一张放置表（遇到第一处失败即返回 Left）。
placeAllWith :: Registry -> Board -> [Placement] -> Either PlaceError Board
placeAllWith reg = foldM (\b (Place n args ps) -> placeWith reg n args b ps)

-- | 地面层被上方消除命中一次（段 2c）：hits = 本轮的消除格（去重），每格至多命中一次。
-- 返回（新地面层，按计数名的去层数）。只有注册为地面层、且原型值有 'ground' 的名字会反应；
-- 计数键取原型值的 'counter'，只有 CountNamed 返回（结算时并入 gsCounts；其余键忽略）。
hitGroundWith :: Registry -> [Pos] -> Ground -> (Ground, [(ElementName, Int)])
hitGroundWith reg hits = foldr one ([], [])
  where
    one (p, (n, layers)) (acc, counts)
      | p `elem` hits
      , Just g <- lookup n (regGround reg)
      , Just react <- groundRule g =
          let after = react layers
              removed = layers - maybe 0 id after
              counts' = case counter g of
                Just (CountNamed k) | removed > 0 -> (k, removed) : counts
                _ -> counts
          in (maybe acc (\l -> (p, (n, l)) : acc) after, counts')
      | otherwise = ((p, (n, layers)) : acc, counts)

--------------------------------------------------------------------------------
-- 成对交换、开启、改色 / 推动

-- | 成对交换规则（已按 srOrder 排好）：元素声明的（elementSwapRules）+ 组合表并成的一条（第 8 刀，次序 20）。
swapRules :: Registry -> [SwapRule]
swapRules reg = case regCombos reg of
  [] -> regSwap reg
  combos -> sortOn srOrder (regSwap reg ++ [comboSwapRule combos])

-- | 只是元素自己声明的成对交换规则（不含组合表；按 srOrder 排好）。
elementSwapRules :: Registry -> [SwapRule]
elementSwapRules = regSwap

-- | 交换起手：交换前盘面 b0 上第一条成立的成对规则，在交换后盘面 swapped 上给出的种子；都不成立时 Nothing。
swapOpeningWith :: Registry -> Board -> Board -> Pos -> Pos -> Maybe [Pos]
swapOpeningWith reg b0 swapped p1 p2 =
  listToMaybe [srSeeds r swapped p1 p2 | r <- swapRules reg, srFires r b0 p1 p2]

-- | 是否有成对规则成立（交换前盘面）。
swapFiresWith :: Registry -> Board -> Pos -> Pos -> Bool
swapFiresWith reg b p1 p2 = any (\r -> srFires r b p1 p2) (swapRules reg)

-- | 一批前沿上的开启（彩蛋类）：依次跑各开启规则，返回 (盘面, 爆炸种子, 本轮坐住的格)。
-- 只有一条规则时结果就是它自己的输出（内置只有彩蛋）。
openWith :: Registry -> Board -> [Pos] -> (Board, [Pos], [Pos])
openWith reg b front = case regOpen reg of
  [] -> (b, [], [])
  (r : rs) -> foldl step (orOpen r b front) rs
  where
    step (b1, e1, s1) r' =
      let (b2, e2, s2) = orOpen r' b1 front
      in (b2, nub (e1 ++ e2), nub (s1 ++ s2))

-- | 本体可被魔法帽 / 染色瓶改色。
recolorableWith :: Registry -> Cell -> Bool
recolorableWith reg = recolorable . bodyOf reg

-- | 本体可被蜗牛推动。
pushableWith :: Registry -> Cell -> Bool
pushableWith reg = pushable . bodyOf reg

--------------------------------------------------------------------------------
-- 规则表（第 8 刀）

-- | 特殊块形状规则表（有序；Board.Clear.spawnSpecialsWith 用）。mkRegistry 建出的表为空（不生成特殊块），
-- 内置注册表是 Element.Builtin.Gem.builtinShapeRules。
shapeRules :: Registry -> [ShapeRule]
shapeRules = regShapes

-- | 换掉形状规则表（扩展一条形状规则 = 把它插到表里合适的位置）。
setShapeRules :: [ShapeRule] -> Registry -> Registry
setShapeRules rs reg = reg {regShapes = rs}

-- | 特殊块组合表（有序）。非空时整张表并成一条次序 comboOrder（20）的成对交换规则（见 'swapRules'）。
-- mkRegistry 建出的表为空，内置注册表是 Match3.Combos.builtinComboRules。
comboRules :: Registry -> [ComboRule]
comboRules = regCombos

-- | 换掉组合表。
setComboRules :: [ComboRule] -> Registry -> Registry
setComboRules rs reg = reg {regCombos = rs}

-- | 注册表的补子策略（缺省 Board.Refill.defaultRefill）；关卡级元素可以经 Refilling 消息换掉（见 Gravity.activeRefill）。
refillPolicyWith :: Registry -> RefillPolicy
refillPolicyWith = regRefill

-- | 换掉注册表的补子策略。
setRefillPolicy :: RefillPolicy -> Registry -> Registry
setRefillPolicy p reg = reg {regRefill = p}

--------------------------------------------------------------------------------
-- 关卡级元素

-- | 注册（或按名字替换）一个关卡级元素。
registerLevel :: SomeLevelElement -> Registry -> Registry
registerLevel d reg = reg {regLevel = [x | x <- regLevel reg, levelNameOf x /= levelNameOf d] ++ [d]}

-- | 去掉一个关卡级元素（测试用：去掉后该机制不生效）。
removeLevel :: ElementName -> Registry -> Registry
removeLevel n reg = reg {regLevel = [x | x <- regLevel reg, levelNameOf x /= n]}

-- | 全部关卡级元素（注册顺序）。
levelDefs :: Registry -> [SomeLevelElement]
levelDefs = regLevel

-- | 问注册的关卡级元素（原型值，不带一局的状态；一局里的节拍见 Match3.Element.Level.askLevelsIn）：
-- 问题与回复同类型（累积器），按注册顺序**折叠所有回复者**（前一个的回复是后一个的问题）；没人回复时 Nothing。
askLevels :: Message q => Registry -> q -> Maybe q
askLevels reg q0 = foldl one Nothing (regLevel reg)
  where
    one acc (SomeLevelElement l) = case levelReply l (SomeMessage (maybe q0 id acc)) of
      Just (reply, _) | Just q' <- fromMessage reply -> Just q'
      _ -> acc
