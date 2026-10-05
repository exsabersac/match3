{-# LANGUAGE RankNTypes #-}
-- | 一局的关卡级机制：GameState.gsLevelElems :: ['SomeMechanic'] 的开局、读写、按节拍折叠，
-- 以及给 Board 层的钩子记录（'levelHooksWith'）。
--
-- 世界与状态的分工（同格子元素：格子存数据、世界给行为开关）：
--
-- * 开局（'startLevelsWith'）：世界里的每种机制（注册顺序）+ 核心机制（地面层），各自由关卡记录给出初始状态。
-- * 每个节拍参与的机制（内部的 @active@）：按注册顺序取 gsLevelElems 里同名的状态，gsLevelElems 里没有的用注册的原型值
--   （所以只 registerMechanic、不改开局也能接入无状态的机制）；gsLevelElems 里没注册的名字不参与
--   （removeMechanic 后即不生效），核心机制（'mechCore'）总参与。
-- * 节拍是 'Mechanic' 的有类型方法（第 5 刀起取代开放消息）：'beatIn' 按参与顺序折叠所有回复者（前一个的回复是
--   后一个的输入），回复者推进后的状态写回 gsLevelElems（同名替换；原来没有的只在状态真的变了时追加）；
--   'queryIn' 是状态不变的节拍。
--
-- 依赖：Element.Mechanic / World、Board.Hooks、Levels.Level（不依赖任何内置机制：读数是 Mechanic 的方法）。
module Match3.Element.Level
  ( -- * 一局的关卡级机制
    startLevelsWith
  , coreLevels
  , beatIn
  , queryIn
  , levelState
  , putLevel
    -- * 读数
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
import Match3.Element.Mechanic
import Match3.Element.World (World, groundWideningWith, hitGroundWith, mechanicDefs, portalWith, refillPolicyWith, setShapeRules, setWidening, shapeRules)
import Match3.Levels.Level (Level)
import Match3.Types
import Match3.Ufo (Ufo)

-- | 核心关卡级机制的原型（不经注册开关）：地面层。
coreLevels :: [SomeMechanic]
coreLevels = [SomeMechanic (GroundLayer [])]

-- | 开局的关卡级机制：世界里的各种（注册顺序）+ 核心机制（与注册的同名时以注册的为准），
-- 各自按关卡记录给出初始状态（'mechStart'）。内置 = [飞碟, 皮带, 传送门, 地毯, 规则开关 …, 地面层]。
startLevelsWith :: World -> Level -> [SomeMechanic]
startLevelsWith world lvl = map start (kinds ++ [c | c <- coreLevels, mechNameOf c `notElem` map mechNameOf kinds])
  where
    kinds = mechanicDefs world
    start (SomeMechanic m) = SomeMechanic (mechStart lvl m)

-- | 本节拍参与的机制，带「状态是否已在 gsLevelElems 里」。
active :: World -> [SomeMechanic] -> [(SomeMechanic, Bool)]
active world elems =
  [ maybe (k, False) (\e -> (e, True)) (named (mechNameOf k))
  | k <- kinds
  ]
    ++ [ (e, True)
       | e@(SomeMechanic m) <- elems
       , mechCore m
       , mechNameOf e `notElem` map mechNameOf kinds
       ]
  where
    kinds = mechanicDefs world
    named n = listToMaybe [e | e <- elems, mechNameOf e == n]

-- | 在一个节拍上问一局的关卡级机制：@step m q@ = 机制 m 对输入 q 的回复（Nothing = 不回复）与推进后的自身。
-- 按参与顺序（注册顺序的各种 + 核心机制）**折叠所有回复者**（前一个的回复是后一个的输入），各回复者推进后的状态
-- 依次写回；返回 (最终回复, 写回后的关卡级机制)。没人回复时 Nothing。
beatIn :: World -> [SomeMechanic] -> q -> (forall m. Mechanic m => m -> q -> Maybe (q, m)) -> Maybe (q, [SomeMechanic])
beatIn world elems0 q0 step = foldl one Nothing (active world elems0)
  where
    one acc (e@(SomeMechanic m), stored) =
      let (q, es) = maybe (q0, elems0) id acc
      in case step m q of
           Just (q', m') -> Just (q', writeBack es stored e (SomeMechanic m'))
           Nothing -> acc
    writeBack es stored e e'
      | stored = replaceNamed e' es
      | e' == e = es
      | otherwise = es ++ [e']

-- | 状态不变的节拍（查询）：同 'beatIn' 的折叠，只要最终回复。
queryIn :: World -> [SomeMechanic] -> q -> (forall m. Mechanic m => m -> q -> Maybe q) -> Maybe q
queryIn world elems q0 ask = fst <$> beatIn world elems q0 (\m q -> (\q' -> (q', m)) <$> ask m q)

-- | 同名替换（没有则追加）。
replaceNamed :: SomeMechanic -> [SomeMechanic] -> [SomeMechanic]
replaceNamed e es
  | any same es = map (\x -> if same x then e else x) es
  | otherwise = es ++ [e]
  where
    same x = mechNameOf x == mechNameOf e

-- | 某类型的关卡级机制的状态（第一个类型对得上的）。
levelState :: Mechanic m => [SomeMechanic] -> Maybe m
levelState = listToMaybe . mapMaybe fromMechanic

-- | 写入一个关卡级机制的状态（同名替换，没有则追加）。
putLevel :: Mechanic m => m -> [SomeMechanic] -> [SomeMechanic]
putLevel = replaceNamed . SomeMechanic

-- | 读数：第一个提供这种读数的机制给出的值；没有提供者时为空。
reading :: (forall m. Mechanic m => m -> Maybe [a]) -> [SomeMechanic] -> [a]
reading f elems = maybe [] id (listToMaybe [xs | SomeMechanic m <- elems, Just xs <- [f m]])

-- | 飞碟。
levelUfos :: [SomeMechanic] -> [Ufo]
levelUfos = reading ufos

-- | 传送带路径。
levelBelts :: [SomeMechanic] -> [Belt]
levelBelts = reading belts

-- | 传送门对。
levelPortals :: [SomeMechanic] -> [(Pos, Pos)]
levelPortals = reading portals

-- | 未覆盖的地毯格。
levelCarpetOpen :: [SomeMechanic] -> [Pos]
levelCarpetOpen = reading carpetOpen

-- | 地面层。
levelGround :: [SomeMechanic] -> Ground
levelGround = reading ground

-- | 掉落口格（新玩法 6；没有掉落口的关卡为空）：前端画掉落口标记用。
levelDrops :: [SomeMechanic] -> [Pos]
levelDrops = reading drops

-- | Board 层的钩子：沉降节拍 'onSettling'（可穿门谓词 = 世界的本体定义），补子之后 'onRefilled'，
-- 补子策略问 'refillPolicy'（初值 = 世界的策略）。没人回复时不传送 / 不吸收 / 用世界的补子策略。
levelHooksWith :: World -> [SomeMechanic] -> LevelHooks
levelHooksWith world elems = hooks
  where
    canPass = portalWith world
    hooks =
      LevelHooks
        { onSettle = \mb -> maybe mb fst (beatIn world elems mb (\m q -> onSettling m canPass q))
        , onAbsorb = \b -> case beatIn world elems [] (\m acc -> onRefilled m b acc) of
            Just (ps, elems') -> (ps, levelHooksWith world elems')
            Nothing -> ([], hooks)
        , hookRefill = queryIn world elems (refillPolicyWith world) refillPolicy
        , hookLevel = elems
        }

-- | 本步的世界：问一次形状表（'shapes'，初值 = 世界的表）；有机制回复就换上回复的表，否则原样。
-- 每步结算开始时调用（Game.Resolve.resolveMoveWith）；内置关卡里只有规则开关 BombShapes 打开时回复。
-- 新玩法 8：地面层里有带扩爆规则的格（魔法地格）时，再把它们写进本步上下文（'StepCtx'，'setWidening'）；
-- 没有这种格时世界原样（其余关卡与每日挑战不受影响）。
levelWorldIn :: World -> [SomeMechanic] -> World
levelWorldIn world elems = widened (maybe world (\rs -> setShapeRules rs world) (queryIn world elems (shapeRules world) shapes))
  where
    widened r = case groundWideningWith r (levelGround elems) of
      [] -> r
      ws -> setWidening ws r

-- | 交换变身节拍（'morph'，新玩法 4）：玩家交换成立前问一次；Just = 本步先变身再按种子起手。第一个回复者为准。
-- 内置关卡里只有规则开关 RainbowCombos（"rainbow_combos"）打开时回复。
morphIn :: World -> [SomeMechanic] -> Board -> Board -> Pos -> Pos -> Maybe Morph
morphIn world elems b0 swapped p1 p2 =
  queryIn world elems Nothing (\m acc -> maybe (Just <$> morph m b0 swapped p1 p2) (const Nothing) acc) >>= id

-- | 皮带节拍（'onEndTick'）：Just (移位, 推进后的机制)；没人回复时 Nothing（没有皮带，也没有皮带后的再连锁）。
beltShiftIn :: World -> [SomeMechanic] -> Maybe ([(Pos, Pos)], [SomeMechanic])
beltShiftIn world elems = beatIn world elems [] onEndTick

-- | 会走的元素要跳过的格（'avoidCells'，内置 = 皮带格）。
avoidCellsIn :: World -> [SomeMechanic] -> [Pos]
avoidCellsIn world elems = maybe [] id (queryIn world elems [] avoidCells)

-- | 会走的元素当墙的格（'wallCells'，内置 = 传送门端点）。
wallCellsIn :: World -> [SomeMechanic] -> [Pos]
wallCellsIn world elems = maybe [] id (queryIn world elems [] wallCells)

-- | 地毯节拍（'onCover'）：(新覆盖数, 推进后的机制)；没人回复时不覆盖。
coverIn :: World -> [Pos] -> [SomeMechanic] -> (Int, [SomeMechanic])
coverIn world hit elems = maybe (0, elems) id (beatIn world elems 0 (\m n -> onCover m hit n))

-- | 地面层节拍（'onGroundHit'，规则 = 世界的 hitGroundWith）：(按名字的去层数, 推进后的机制)。
hitGroundIn :: World -> [Pos] -> [SomeMechanic] -> ([(ElementName, Int)], [SomeMechanic])
hitGroundIn world hits elems = maybe ([], elems) id (beatIn world elems [] (\m acc -> onGroundHit m (hitGroundWith world) hits acc))

-- | 胜负节拍（'judge'）：内置规则判出的结局交给关卡级机制复核，有回复就用回复里的结局。
-- 内置关卡级机制都不回复，所以内置关卡与每日挑战的结局与原来逐字相同（judge_default_no_replier）。
judgeIn :: World -> [SomeMechanic] -> Board -> Score -> MovesLeft -> Outcome -> Outcome
judgeIn world elems b score moves out = maybe out id (queryIn world elems out (\m o -> judge m b score moves o))
