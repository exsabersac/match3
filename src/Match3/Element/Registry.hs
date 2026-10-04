-- | 元素注册表：主流程查询元素行为的**唯一入口**（各 *With 函数）。
--
-- 注册的元素只是一张有序的类型列表（Match3.Element.World 的 'Def'：本体 'Kind' / 叠层 'Layer' / 地面层 'GroundKind'），
-- 解码由 World 做（叠层由外向内 peel，剩下的交给本体 fromCell）；各查询就是在解码出的元素值上调能力类的方法
-- （Match3.Element.Ability）。规则（邻格 / 步末 / 成对交换 / 开启）由各类型的 boardPasses / layerPasses 收集一次、
-- 按次序排好缓存在这里。关卡级元素（'SomeLevelElement'）的种类表（注册顺序；一局的状态与节拍见 Match3.Element.Level）。
--
-- 另持三张规则表——特殊块形状规则、特殊块组合规则、补子策略（'shapeRules' / 'comboRules' / 'refillPolicyWith'）。
module Match3.Element.Registry
  ( Registry
  , Entry
  , entryName
  , Placer
    -- * 注册项
  , kindDef
  , layerDef
  , groundDef
  , inertDef
  , mkRegistry
  , mkRegistryChecked
  , RegistryError
  , WorldError(..)
  , register
  , registryDefs
  , registryWorld
  , lookupElement
    -- * 解码
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
import Data.Array (bounds, inRange)
import Data.List (mapAccumL, nub, sortOn)
import Data.Maybe (listToMaybe)
import Data.Monoid (Sum(..))
import Match3.Board.Grid (getCell, setCell)
import Match3.Board.Refill (RefillPolicy, defaultRefill)
import Match3.Element.Ability
import Match3.Element.Class (Message, SomeLevelElement(..), SomeMessage(..), fromMessage, levelNameOf, levelReply)
import Match3.Element.Kind
import Match3.Element.Layer
import Match3.Element.Special (comboSwapRule)
import Match3.Element.Types
import Match3.Element.World
import Match3.Types

-- | 注册项（= World 的 'Def'）。
type Entry = Def

entryName :: Entry -> ElementName
entryName = defName

-- | 建表时发现的注册错误（= World 的 'WorldError'）。
type RegistryError = WorldError

-- | 注册表。用 mkRegistry / register 构造；字段不导出（解码缓存与规则表由注册项派生）。
data Registry = Registry
  { regWorld    :: World
  , regAdjacent :: [AdjacentRule]                   -- 按 arOrder 排好（稳定）
  , regEnd      :: [EndRule]                        -- 按 (阶段, erOrder) 排好（稳定）
  , regDiff     :: [(ElementName, CounterKey, Int)] -- 按个数差计数的元素：(名字, 计数键, 每个的奖励步数)
  , regSwap     :: [SwapRule]                       -- 成对交换规则，按 srOrder 排好（稳定）
  , regOpen     :: [OpenRule]                       -- 开启规则（注册顺序）
  , regLevel    :: [SomeLevelElement]               -- 关卡级元素（注册顺序；同名以后注册的为准）
  , regShapes   :: [ShapeRule]                      -- 特殊块形状规则表（有序）
  , regCombos   :: [ComboRule]                      -- 特殊块组合表（有序）
  , regRefill   :: RefillPolicy                     -- 补子策略（关卡级元素可经 Refilling 换掉）
  , regWiden    :: [(Pos, Board -> [Pos] -> [Pos])] -- 本步的扩爆格（新玩法 8）：格 → 爆炸范围改写；缺省空，
                                                    -- 每步由 Element.Level.levelRegistryIn 按地面层的 groundWiden 填
  }

-- | 由注册项建表并检查（见 'mkWorldChecked'）。没错时与 'mkRegistry' 建出同一张表。
mkRegistryChecked :: [Entry] -> Either [RegistryError] Registry
mkRegistryChecked defs = fmap (const (mkRegistry defs)) (mkWorldChecked defs)

-- | 由注册项建表（总函数，不报错）。同名多项以后出现的为准，位置取第一次出现处。
mkRegistry :: [Entry] -> Registry
mkRegistry defs0 =
  let w = mkWorld defs0
      passes = concatMap defPasses (worldDefs w)
  in Registry
       { regWorld = w
       , regAdjacent = sortOn arOrder [AdjacentRule o f | AdjacentPass o f <- passes]
       , regEnd = sortOn (\r -> (erPhase r, erOrder r)) [r | EndPass r <- passes]
       , regDiff = [(kindName p, k, bonusMoves p) | SomeKind p <- worldKinds w, Just k <- [diffCounter p]]
       , regSwap = sortOn srOrder [r | SwapPass r <- passes]
       , regOpen = [r | OpenPass r <- passes]
       , regLevel = []
       , regShapes = []
       , regCombos = []
       , regRefill = defaultRefill
       , regWiden = []
       }
  where
    defPasses d = case d of
      KindDef (SomeKind p) -> boardPasses p
      LayerDef (SomeLayer p) -> layerPasses p
      _ -> []

-- | 往注册表里加（或按名字替换）一个注册项。测试专用元素就这样接进来，主流程不用改。
-- 关卡级元素与规则表（形状 / 组合 / 补子策略）原样保留。
register :: Entry -> Registry -> Registry
register d reg =
  (mkRegistry (worldDefs (regWorld reg) ++ [d]))
    { regLevel = regLevel reg
    , regShapes = regShapes reg
    , regCombos = regCombos reg
    , regRefill = regRefill reg
    , regWiden = regWiden reg
    }

-- | 全部注册项（注册顺序）。
registryDefs :: Registry -> [Entry]
registryDefs = worldDefs . regWorld

-- | 注册表的元素世界（解码用）。
registryWorld :: Registry -> World
registryWorld = regWorld

-- | 按名字找注册项（关卡放置、计数、文档用）。
lookupElement :: Registry -> ElementName -> Maybe Entry
lookupElement reg = lookupDef (regWorld reg)

--------------------------------------------------------------------------------
-- 显示（只给前端 / 文案用，不参与规则）

-- | 本体格子的显示附加字段（元素的 'face'；未注册的 Custom 名字 = 无）。
faceFieldsWith :: Registry -> Cell -> [(String, FaceValue)]
faceFieldsWith reg = face . bodyOf reg

-- | 按元素名计数的目标的中文名（本体 'label' / 地面层 'groundLabel'；没登记 = Nothing）。
displayLabelWith :: Registry -> ElementName -> Maybe String
displayLabelWith reg n = lookupElement reg n >>= defLabel

defLabel :: Entry -> Maybe String
defLabel d = case d of
  KindDef (SomeKind p) -> label p
  GroundDef (SomeGround p) -> groundLabel p
  _ -> Nothing

-- | 按元素名计数的目标的失败提示。
loseHintWith :: Registry -> ElementName -> Maybe (Int -> String)
loseHintWith reg n = lookupElement reg n >>= \d -> case d of
  KindDef (SomeKind p) -> loseHint p
  GroundDef (SomeGround p) -> groundLoseHint p
  _ -> Nothing

-- | 全部登记了中文名的元素：[(元素名, 中文名)]（注册顺序）。
displayLabels :: Registry -> [(ElementName, String)]
displayLabels reg = [(entryName d, l) | d <- registryDefs reg, Just l <- [defLabel d]]

--------------------------------------------------------------------------------
-- 解码

-- | 本体层的元素值（拆掉冰层 / 叠层之后）。
bodyOf :: Registry -> Cell -> SomeElement
bodyOf reg cell = decodeBody (regWorld reg) (snd (decodeLayers (regWorld reg) cell))

-- | 本体之上的各层（自上而下）：冰层（ice > 0）、叠层。
upperOf :: Registry -> Cell -> [SomeLayerValue]
upperOf reg = fst . decodeLayers (regWorld reg)

-- | 整个格子的元素值：叠层（冰 → 叠层，自外向内）包着本体。
elementOf :: Registry -> Cell -> SomeElement
elementOf reg = decode (regWorld reg)

-- | 本体的元素名。
elementName :: Registry -> Cell -> ElementName
elementName reg = nameOf . bodyOf reg

-- | 最上面一层的元素名（事件里「波及了什么」用）。
topLayerName :: Registry -> Cell -> ElementName
topLayerName reg cell = case upperOf reg cell of
  (lv : _) -> layerValueName lv
  [] -> elementName reg cell

--------------------------------------------------------------------------------
-- 钩子

-- | 参与匹配的颜色：任一层挡匹配则 Nothing，否则本体颜色。（匹配、提示的热路径。）
matchColorWith :: Registry -> Cell -> Maybe Color
matchColorWith reg = matchColor . elementOf reg

-- | 本体颜色（颜色袋计数用，不看叠层）。
colorOfWith :: Registry -> Cell -> Maybe Color
colorOfWith reg = color . bodyOf reg

-- | 本格不能被交换（任一层挡）。
blocksSwapWith :: Registry -> Cell -> Bool
blocksSwapWith reg = blocksSwap . elementOf reg

-- | 只看上层（冰 / 叠层）是否挡交换（彩虹 / 特殊合成提示用，本体由它们自己判定）。
upperBlocksSwapWith :: Registry -> Cell -> Bool
upperBlocksSwapWith reg = any (\(SomeLayerValue l) -> layerBlocksSwap l) . upperOf reg

-- | 交换两格是否被挡（任一端挡即挡）。
swapBlockedWith :: Registry -> Board -> Pos -> Pos -> Bool
swapBlockedWith reg b p1 p2 = blocksSwapWith reg (getCell b p1) || blocksSwapWith reg (getCell b p2)

-- | 特殊块能否点火：自上而下第一个有意见的层决定，都没有意见则问本体。
activatesWith :: Registry -> Cell -> Bool
activatesWith reg = fires . elementOf reg

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

-- | 直接命中：叠层自上而下先回答（穿透的问里面），吃掉命中时格子写成新的内容。
directHitWith :: Registry -> Cell -> HitResult
directHitWith reg cell = case struck (elementOf reg cell) of
  Absorb cell' -> HitAbsorb cell'
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

-- | 真消除格上随格清掉的叠层（草 / 藤 / 巧）：去掉这些层，其余各层原样盖回。
stripOnClearWith :: Registry -> Board -> [Pos] -> Board
stripOnClearWith reg b ps = foldl strip b (nub ps)
  where
    strip board p =
      let (ls, inner) = decodeLayers (regWorld reg) (getCell board p)
          kept = [lv | lv@(SomeLayerValue l) <- ls, not (layerStripsOnClear l)]
      in if length kept == length ls
           then board
           else setCell board p (foldr (\(SomeLayerValue l) c -> putOn l c) inner kept)

-- | 邻格波及规则（已按 arOrder 排好）。
adjacentRules :: Registry -> [AdjacentRule]
adjacentRules = regAdjacent
-- | 按顺序跑完一轮的全部邻格波及：返回 (盘面, 打碎的格（按规则顺序拼接）, 新生成需坐住的格)。
-- 每条规则的 acProtect = 起始保护格 ++ 之前各规则的 aoSit。
runAdjacentWith :: Registry -> [Pos] -> [Pos] -> [Pos] -> Board -> (Board, [Pos], [Pos])
runAdjacentWith reg trueClears direct protect0 b0 =
  let ((b', _), outs) = mapAccumL one (b0, nub protect0) (regAdjacent reg)
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
counterWith :: Registry -> Cell -> Maybe CounterKey
counterWith reg = counter . bodyOf reg

-- | 按前后个数差计数的元素：(名字, 计数键, 每个的奖励步数)（保险箱、时间精灵、自定义）。
diffCountersWith :: Registry -> [(ElementName, CounterKey, Int)]
diffCountersWith = regDiff

-- | 盘上本体为该元素的格数（盘面是 Foldable：按格 foldMap 到 'Sum'）。
countElementWith :: Registry -> ElementName -> Board -> Int
countElementWith reg n = getSum . foldMap (\cell -> if elementName reg cell == n then Sum 1 else mempty)

-- | 盘上本体为该元素的格按 'diffWeight' 加权的总数（按差计数用）。权重缺省 1，这时与 'countElementWith' 相同；
-- 新玩法 5 雪怪 Boss 的左上格权重 = 血量，其余格 0。
weighElementWith :: Registry -> ElementName -> Board -> Int
weighElementWith reg n = getSum . foldMap (\cell -> let e = bodyOf reg cell in if nameOf e == n then Sum (diffWeight e) else mempty)

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
  [(p, w) | (p, (n, _)) <- g, Just (SomeGround gp) <- [lookupGround (regWorld reg) n], Just w <- [groundWiden gp]]

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
      | otherwise = Right (maybe b (setCell b p) (defPlace d args (getCell b p)))

-- | 按顺序应用一张放置表（遇到第一处失败即返回 Left）。
placeAllWith :: Registry -> Board -> [Placement] -> Either PlaceError Board
placeAllWith reg = foldM (\b (Place n args ps) -> placeWith reg n args b ps)

-- | 地面层被上方消除命中一次（段 2c）：hits = 本轮的消除格（去重），每格至多命中一次。
-- 返回（新地面层，按计数名的去层数）。只有注册为地面层的名字会反应（'groundHit'）；
-- 计数键取 'groundCounter'，只有 CountNamed 返回（结算时并入 gsCounts；其余键忽略）。
hitGroundWith :: Registry -> [Pos] -> Ground -> (Ground, [(ElementName, Int)])
hitGroundWith reg hits = foldr one ([], [])
  where
    one (p, (n, layers)) (acc, counts)
      | p `elem` hits
      , Just (SomeGround g) <- lookupGround (regWorld reg) n =
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
