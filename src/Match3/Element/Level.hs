-- | 一局的关卡级元素：GameState.gsLevelElems :: ['SomeLevelElement'] 的开局、读写、按节拍发消息，
-- 以及给 Board 层的钩子记录（'levelHooksWith'）。
--
-- 注册表与状态的分工（同格子元素：格子存数据、注册表给行为开关）：
--
-- * 开局（'startLevelsWith'）：注册表里的每种关卡级元素（注册顺序）+ 核心元素（地面层），各自由关卡记录给出初始状态。
-- * 每个节拍参与的元素（内部的 @active@）：按注册顺序取 gsLevelElems 里同名的状态，gsLevelElems 里没有的用注册的原型值
--   （所以只 registerLevel、不改开局也能接入无状态的元素）；gsLevelElems 里没注册的名字不参与
--   （removeLevel 后即不生效），核心元素（'levelCore'）总参与。
-- * 回复者推进后的状态写回 gsLevelElems（同名替换；原来没有的只在状态真的变了时追加）。
--
-- 依赖：Element.Class / Message / World、Builtin.Level（内置元素的状态读数）、Board.Hooks、Levels.Level。
module Match3.Element.Level
  ( -- * 一局的关卡级元素
    startLevelsWith
  , coreLevels
  , askLevelsIn
  , levelState
  , putLevel
    -- * 内置元素的状态读数
  , levelUfos
  , levelBelts
  , levelPortals
  , levelCarpetOpen
  , levelGround
  , levelDrops
    -- * 节拍
  , levelHooksWith
  , levelWorldIn
  , morphIn
  , beltShiftIn
  , avoidCellsIn
  , wallCellsIn
  , coverIn
  , hitGroundIn
  , judgeIn
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Conveyor (Belt)
import Match3.Element.Builtin.Level (BeltLevel(..), CarpetLevel(..), CookieDrop(..), GroundLayer(..), PortalLevel(..), UfoLevel(..))
import Match3.Element.Class
import Match3.Element.Message
import Match3.Element.World (World, groundWideningWith, hitGroundWith, levelDefs, portalWith, refillPolicyWith, setShapeRules, setWidening, shapeRules)
import Match3.Levels.Level (DropSpec(..), Level)
import Match3.Types
import Match3.Ufo (Ufo)

-- | 核心关卡级元素的原型（不经注册表开关）：地面层。
coreLevels :: [SomeLevelElement]
coreLevels = [SomeLevelElement (GroundLayer [])]

-- | 开局的关卡级元素：注册表里的各种（注册顺序）+ 核心元素（与注册的同名时以注册的为准），
-- 各自按关卡记录给出初始状态（'levelStart'）。内置 = [飞碟, 皮带, 传送门, 地毯, 地面层]。
startLevelsWith :: World -> Level -> [SomeLevelElement]
startLevelsWith reg lvl = map start (kinds ++ [c | c <- coreLevels, levelNameOf c `notElem` map levelNameOf kinds])
  where
    kinds = levelDefs reg
    start (SomeLevelElement l) = SomeLevelElement (levelStart lvl l)

-- | 本节拍参与的元素，带「状态是否已在 gsLevelElems 里」。
active :: World -> [SomeLevelElement] -> [(SomeLevelElement, Bool)]
active reg elems =
  [ maybe (k, False) (\e -> (e, True)) (named (levelNameOf k))
  | k <- kinds
  ]
    ++ [ (e, True)
       | e@(SomeLevelElement l) <- elems
       , levelCore l
       , levelNameOf e `notElem` map levelNameOf kinds
       ]
  where
    kinds = levelDefs reg
    named n = listToMaybe [e | e <- elems, levelNameOf e == n]

-- | 在一个节拍上问一局的关卡级元素：问题与回复同类型（累积器），按参与顺序（注册顺序的各种 + 核心元素）**折叠所有回复者**
-- （前一个的回复是后一个的问题），各回复者推进后的状态依次写回；返回 (最终回复, 写回后的关卡级元素)。
-- 没人回复时 Nothing（内置元素每种消息只有一个回复者）。
askLevelsIn :: Message q => World -> [SomeLevelElement] -> q -> Maybe (q, [SomeLevelElement])
askLevelsIn reg elems0 q0 = foldl one Nothing (active reg elems0)
  where
    one acc (e@(SomeLevelElement l), stored) =
      let (q, es) = maybe (q0, elems0) id acc
      in case levelReply l (SomeMessage q) of
           Just (reply, l') | Just q' <- fromMessage reply -> Just (q', writeBack es stored e (SomeLevelElement l'))
           _ -> acc
    writeBack es stored e e'
      | stored = replaceNamed e' es
      | e' == e = es
      | otherwise = es ++ [e']

-- | 同名替换（没有则追加）。
replaceNamed :: SomeLevelElement -> [SomeLevelElement] -> [SomeLevelElement]
replaceNamed e es
  | any same es = map (\x -> if same x then e else x) es
  | otherwise = es ++ [e]
  where
    same x = levelNameOf x == levelNameOf e

-- | 某类型的关卡级元素的状态（第一个类型对得上的）。
levelState :: LevelElement l => [SomeLevelElement] -> Maybe l
levelState = listToMaybe . mapMaybe fromLevelElement

-- | 写入一个关卡级元素的状态（同名替换，没有则追加）。
putLevel :: LevelElement l => l -> [SomeLevelElement] -> [SomeLevelElement]
putLevel = replaceNamed . SomeLevelElement

-- | 飞碟。
levelUfos :: [SomeLevelElement] -> [Ufo]
levelUfos = maybe [] (\(UfoLevel us) -> us) . levelState

-- | 传送带路径。
levelBelts :: [SomeLevelElement] -> [Belt]
levelBelts = maybe [] (\(BeltLevel bs) -> bs) . levelState

-- | 传送门对。
levelPortals :: [SomeLevelElement] -> [(Pos, Pos)]
levelPortals = maybe [] (\(PortalLevel ps) -> ps) . levelState

-- | 未覆盖的地毯格。
levelCarpetOpen :: [SomeLevelElement] -> [Pos]
levelCarpetOpen = maybe [] (\(CarpetLevel ps) -> ps) . levelState

-- | 地面层。
levelGround :: [SomeLevelElement] -> Ground
levelGround = maybe [] (\(GroundLayer g) -> g) . levelState

-- | 掉落口格（新玩法 6；没有掉落口的关卡为空）：前端画掉落口标记用。
levelDrops :: [SomeLevelElement] -> [Pos]
levelDrops = maybe [] (\(CookieDrop ds) -> concatMap dropCells ds) . levelState

-- | Board 层的钩子：沉降节拍发 'Settling'（可穿门谓词 = 注册表的本体定义），补子之后发 'Refilled'，
-- 补子策略问 'Refilling'（初值 = 注册表的策略）。
-- 没人回复时不传送 / 不吸收 / 用注册表的补子策略。
levelHooksWith :: World -> [SomeLevelElement] -> LevelHooks
levelHooksWith reg elems = hooks
  where
    hooks =
      LevelHooks
        { onSettle = \mb -> maybe mb (\(Settling _ mb', _) -> mb') (askLevelsIn reg elems (Settling (portalWith reg) mb))
        , onAbsorb = \b -> case askLevelsIn reg elems (Refilled b []) of
            Just (Refilled _ ps, elems') -> (ps, levelHooksWith reg elems')
            Nothing -> ([], hooks)
        , hookRefill = (\(Refilling p, _) -> p) <$> askLevelsIn reg elems (Refilling (refillPolicyWith reg))
        , hookLevel = elems
        }

-- | 本步的世界：问一次形状表（'Shaping'，初值 = 世界的表）；有元素回复就换上回复的表，否则原样。
-- 每步结算开始时调用（Game.Resolve.resolveMoveWith）；内置关卡里只有规则开关 BombShapes 打开时回复。
-- 新玩法 8：地面层里有带扩爆规则的格（魔法地格）时，再把它们写进本步上下文（'StepCtx'，'setWidening'）；
-- 没有这种格时世界原样（其余关卡与每日挑战不受影响）。
levelWorldIn :: World -> [SomeLevelElement] -> World
levelWorldIn reg elems = widened (maybe reg (\(Shaping rs, _) -> setShapeRules rs reg) (askLevelsIn reg elems (Shaping (shapeRules reg))))
  where
    widened r = case groundWideningWith r (levelGround elems) of
      [] -> r
      ws -> setWidening ws r

-- | 交换变身节拍（'Morphing'，新玩法 4）：玩家交换成立前问一次；Just = 本步先变身再按种子起手。
-- 内置关卡里只有规则开关 RainbowCombos（"rainbow_combos"）打开时回复。
morphIn :: World -> [SomeLevelElement] -> Board -> Board -> Pos -> Pos -> Maybe Morph
morphIn reg elems b0 swapped p1 p2 = askLevelsIn reg elems (Morphing b0 swapped p1 p2 Nothing) >>= \(Morphing _ _ _ _ m, _) -> m

-- | 皮带节拍（'EndTicked'）：Just (移位, 推进后的元素)；没人回复时 Nothing（没有皮带，也没有皮带后的再连锁）。
beltShiftIn :: World -> [SomeLevelElement] -> Maybe ([(Pos, Pos)], [SomeLevelElement])
beltShiftIn reg elems = (\(EndTicked mv, es) -> (mv, es)) <$> askLevelsIn reg elems (EndTicked [])

-- | 会走的元素要跳过的格（'AvoidCells'，内置 = 皮带格）。
avoidCellsIn :: World -> [SomeLevelElement] -> [Pos]
avoidCellsIn reg elems = maybe [] (\(AvoidCells ps, _) -> ps) (askLevelsIn reg elems (AvoidCells []))

-- | 会走的元素当墙的格（'WallCells'，内置 = 传送门端点）。
wallCellsIn :: World -> [SomeLevelElement] -> [Pos]
wallCellsIn reg elems = maybe [] (\(WallCells ps, _) -> ps) (askLevelsIn reg elems (WallCells []))

-- | 地毯节拍（'Covering'）：(新覆盖数, 推进后的元素)；没人回复时不覆盖。
coverIn :: World -> [Pos] -> [SomeLevelElement] -> (Int, [SomeLevelElement])
coverIn reg hit elems = maybe (0, elems) (\(Covering _ n, es) -> (n, es)) (askLevelsIn reg elems (Covering hit 0))

-- | 地面层节拍（'GroundHit'，规则 = 注册表的 hitGroundWith）：(按名字的去层数, 推进后的元素)。
hitGroundIn :: World -> [Pos] -> [SomeLevelElement] -> ([(ElementName, Int)], [SomeLevelElement])
hitGroundIn reg hits elems =
  maybe ([], elems) (\(GroundHit _ _ cs, es) -> (cs, es)) (askLevelsIn reg elems (GroundHit (hitGroundWith reg) hits []))

-- | 胜负节拍（'Judging'）：内置规则判出的结局交给关卡级元素复核，有回复就用回复里的结局。
-- 内置关卡级元素都不回复，所以内置关卡与每日挑战的结局与原来逐字相同（judge_default_no_replier）。
judgeIn :: World -> [SomeLevelElement] -> Board -> Score -> MovesLeft -> Outcome -> Outcome
judgeIn reg elems b score moves out = maybe out (\(Judging _ _ _ o, _) -> o) (askLevelsIn reg elems (Judging b score moves out))
