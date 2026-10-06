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
-- * 节拍是机制原型里的有类型 system（'MechSys'，ecs-5 起取代 Mechanic 类与 GADT Beat）：'beatIn' 按参与顺序折叠
--   所有回复者（前一个的回复是后一个的输入），回复者推进后的状态写回 gsLevelElems（同名替换；原来没有的只在状态真的变了时追加）；
--   'queryIn' 是状态不变的节拍。
--
-- 依赖：Element.Mechanic / Registry、Board.Hooks、Levels.Level（不依赖任何内置机制：读数是机制的 'LevelView' 组件）。
module Match3.Element.Level
  ( -- * 一局的关卡级机制
    startLevelsWith
  , coreLevels
  , beatIn
  , queryIn
  , BeatPick
  , BeatAsk
    -- * 各节拍的挑选器
  , onRefilled
  , onEndTick
  , onSettling
  , onCovering
  , onGroundHit
  , askRefill
  , askShapes
  , askAvoid
  , askWall
  , askJudge
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
  , levelRegistryIn
  , morphIn
  , beltShiftIn
  , avoidCellsIn
  , wallCellsIn
  , coverIn
  , hitGroundIn
  , judgeIn
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Typeable (Typeable)
import Match3.Board.Grid (MBoard)
import Match3.Board.Refill (RefillPolicy)
import Match3.Element.Types (ShapeRule)
import Match3.Board.Hooks (LevelHooks(..))
import Match3.Conveyor (Belt)
import Match3.Element.Mechanic
import Match3.ECS.Registry (Registry, groundWideningWith, hitGroundWith, mechanicDefs, portalWith, refillPolicyWith, setShapeRules, setWidening, shapeRules)
import Match3.Levels.Level (Level)
import Match3.Types
import Match3.Ufo (Ufo)

-- | 核心关卡级机制的原型（不经注册开关）：地面层。
coreLevels :: [SomeMechanic]
coreLevels = [groundLayer []]

-- | 开局的关卡级机制：世界里的各种（注册顺序）+ 核心机制（与注册的同名时以注册的为准），
-- 各自按关卡记录给出初始状态（'mechStartOf'：跑 'OnStart' system）。内置 = [飞碟, 皮带, 传送门, 地毯, 规则开关 …, 地面层]。
startLevelsWith :: Registry -> Level -> [SomeMechanic]
startLevelsWith world lvl = map start (kinds ++ [c | c <- coreLevels, mechNameOf c `notElem` map mechNameOf kinds])
  where
    kinds = mechanicDefs world
    start = mechStartOf lvl

-- | 本节拍参与的机制，带「状态是否已在 gsLevelElems 里」。
active :: Registry -> [SomeMechanic] -> [(SomeMechanic, Bool)]
active world elems =
  [ maybe (k, False) (\e -> (e, True)) (named (mechNameOf k))
  | k <- kinds
  ]
    ++ [ (e, True)
       | e <- elems
       , mechCoreOf e
       , mechNameOf e `notElem` map mechNameOf kinds
       ]
  where
    kinds = mechanicDefs world
    named n = listToMaybe [e | e <- elems, mechNameOf e == n]

-- | 挑选器：从一种机制的 system 里挑出某个推进型节拍的那一个（输入累积 → 回复与新状态）。
type BeatPick q = forall m. MechSys m -> Maybe (q -> m -> Maybe (q, m))

-- | 问答型节拍的挑选器（只回复、不改状态）。
type BeatAsk q = forall m. MechSys m -> Maybe (q -> m -> Maybe q)

-- | 在一个节拍上问一局的关卡级机制：按参与顺序（注册顺序的各种 + 核心机制）**折叠所有回复者**（前一个的回复是
-- 后一个的输入），各回复者推进后的状态依次写回；返回 (最终回复, 写回后的关卡级机制)。没人回复时 Nothing。
beatIn :: Registry -> [SomeMechanic] -> q -> BeatPick q -> Maybe (q, [SomeMechanic])
beatIn world elems0 q0 pick = foldl one Nothing (active world elems0)
  where
    one acc (e, stored) =
      let (q, es) = maybe (q0, elems0) id acc
      in case replyOf pick q e of
           Just (q', e') -> Just (q', writeBack es stored e e')
           Nothing -> acc
    writeBack es stored e e'
      | stored = replaceNamed e' es
      | e' == e = es
      | otherwise = es ++ [e']

-- | 状态不变的节拍（问答）：同 'beatIn' 的折叠，只要最终回复。
queryIn :: Registry -> [SomeMechanic] -> q -> BeatAsk q -> Maybe q
queryIn world elems q0 ask = fst <$> beatIn world elems q0 (\sys -> (\f q m -> (\q' -> (q', m)) <$> f q m) <$> ask sys)

onRefilled :: Board -> BeatPick [Pos]
onRefilled b sys = case sys of
  OnRefilled f -> Just (f b)
  _ -> Nothing

onEndTick :: BeatPick [(Pos, Pos)]
onEndTick sys = case sys of
  OnEndTick f -> Just f
  _ -> Nothing

onSettling :: (Cell -> Bool) -> BeatPick MBoard
onSettling canPass sys = case sys of
  OnSettling f -> Just (\mb m -> f canPass mb m)
  _ -> Nothing

onCovering :: [Pos] -> BeatPick Int
onCovering hit sys = case sys of
  OnCovering f -> Just (f hit)
  _ -> Nothing

onGroundHit :: GroundRule -> [Pos] -> BeatPick [(ElementName, Int)]
onGroundHit rule hits sys = case sys of
  OnGroundHit f -> Just (f rule hits)
  _ -> Nothing

askRefill :: BeatAsk RefillPolicy
askRefill sys = case sys of
  AnswerRefill f -> Just f
  _ -> Nothing

askShapes :: BeatAsk [ShapeRule]
askShapes sys = case sys of
  AnswerShapes f -> Just f
  _ -> Nothing

askAvoid :: BeatAsk [Pos]
askAvoid sys = case sys of
  AnswerAvoid f -> Just f
  _ -> Nothing

askWall :: BeatAsk [Pos]
askWall sys = case sys of
  AnswerWall f -> Just f
  _ -> Nothing

askJudge :: Board -> Score -> MovesLeft -> BeatAsk Outcome
askJudge b score moves sys = case sys of
  AnswerJudge f -> Just (\o m -> f b score moves o m)
  _ -> Nothing

-- | 同名替换（没有则追加）。
replaceNamed :: SomeMechanic -> [SomeMechanic] -> [SomeMechanic]
replaceNamed e es
  | any same es = map (\x -> if same x then e else x) es
  | otherwise = es ++ [e]
  where
    n = mechNameOf e
    same x = mechNameOf x == n

-- | 某类型的关卡级机制的状态（第一个类型对得上的）。
levelState :: Typeable m => [SomeMechanic] -> Maybe m
levelState = listToMaybe . mapMaybe fromMechanic

-- | 写入一个关卡级机制（同名替换，没有则追加）。
putLevel :: SomeMechanic -> [SomeMechanic] -> [SomeMechanic]
putLevel = replaceNamed

-- | 读数：第一个提供这种读数的机制（读数组件 'LevelView' 的这一项不是 Nothing）给出的值；没有提供者时为空。
reading :: (LevelView -> Maybe [a]) -> [SomeMechanic] -> [a]
reading f elems = maybe [] id (listToMaybe [xs | e <- elems, Just xs <- [f (viewOf e)]])

-- | 飞碟。
levelUfos :: [SomeMechanic] -> [Ufo]
levelUfos = reading lvUfos

-- | 传送带路径。
levelBelts :: [SomeMechanic] -> [Belt]
levelBelts = reading lvBelts

-- | 传送门对。
levelPortals :: [SomeMechanic] -> [(Pos, Pos)]
levelPortals = reading lvPortals

-- | 未覆盖的地毯格。
levelCarpetOpen :: [SomeMechanic] -> [Pos]
levelCarpetOpen = reading lvCarpetOpen

-- | 地面层。
levelGround :: [SomeMechanic] -> Ground
levelGround = reading lvGround

-- | 掉落口格（新玩法 6；没有掉落口的关卡为空）：前端画掉落口标记用。
levelDrops :: [SomeMechanic] -> [Pos]
levelDrops = reading lvDrops

-- | Board 层的钩子：沉降节拍 'onSettling'（可穿门谓词 = 世界的本体定义），补子之后 'onRefilled'，
-- 补子策略问 'refillPolicy'（初值 = 世界的策略）。没人回复时不传送 / 不吸收 / 用世界的补子策略。
levelHooksWith :: Registry -> [SomeMechanic] -> LevelHooks
levelHooksWith world elems = hooks
  where
    canPass = portalWith world
    hooks =
      LevelHooks
        { onSettle = \mb -> maybe mb fst (beatIn world elems mb (onSettling canPass))
        , onAbsorb = \b -> case beatIn world elems [] (onRefilled b) of
            Just (ps, elems') -> (ps, levelHooksWith world elems')
            Nothing -> ([], hooks)
        , hookRefill = queryIn world elems (refillPolicyWith world) askRefill
        , hookLevel = elems
        }

-- | 本步的世界：问一次形状表（'shapes'，初值 = 世界的表）；有机制回复就换上回复的表，否则原样。
-- 每步结算开始时调用（Game.Resolve.resolveMoveWith）；内置关卡里只有规则开关 BombShapes 打开时回复。
-- 新玩法 8：地面层里有带扩爆规则的格（魔法地格）时，再把它们写进本步上下文（'StepCtx'，'setWidening'）；
-- 没有这种格时世界原样（其余关卡与每日挑战不受影响）。
levelRegistryIn :: Registry -> [SomeMechanic] -> Registry
levelRegistryIn world elems = widened (maybe world (\rs -> setShapeRules rs world) (queryIn world elems (shapeRules world) askShapes))
  where
    widened r = case groundWideningWith r (levelGround elems) of
      [] -> r
      ws -> setWidening ws r

-- | 交换变身节拍（'morph'，新玩法 4）：玩家交换成立前问一次；Just = 本步先变身再按种子起手。第一个回复者为准。
-- 内置关卡里只有规则开关 RainbowCombos（"rainbow_combos"）打开时回复。
morphIn :: Registry -> [SomeMechanic] -> Board -> Board -> Pos -> Pos -> Maybe Morph
morphIn world elems b0 swapped p1 p2 =
  queryIn world elems Nothing ask >>= id
  where
    -- 第一个回复者为准：累积已有回复时后面的机制不再回复
    ask :: BeatAsk (Maybe Morph)
    ask sys = case sys of
      AnswerMorph f -> Just (\acc m -> maybe (Just <$> f b0 swapped p1 p2 m) (const Nothing) acc)
      _ -> Nothing

-- | 皮带节拍（'onEndTick'）：Just (移位, 推进后的机制)；没人回复时 Nothing（没有皮带，也没有皮带后的再连锁）。
beltShiftIn :: Registry -> [SomeMechanic] -> Maybe ([(Pos, Pos)], [SomeMechanic])
beltShiftIn world elems = beatIn world elems [] onEndTick

-- | 会走的元素要跳过的格（'avoidCells'，内置 = 皮带格）。
avoidCellsIn :: Registry -> [SomeMechanic] -> [Pos]
avoidCellsIn world elems = maybe [] id (queryIn world elems [] askAvoid)

-- | 会走的元素当墙的格（'wallCells'，内置 = 传送门端点）。
wallCellsIn :: Registry -> [SomeMechanic] -> [Pos]
wallCellsIn world elems = maybe [] id (queryIn world elems [] askWall)

-- | 地毯节拍（'onCover'）：(新覆盖数, 推进后的机制)；没人回复时不覆盖。
coverIn :: Registry -> [Pos] -> [SomeMechanic] -> (Int, [SomeMechanic])
coverIn world hit elems = maybe (0, elems) id (beatIn world elems 0 (onCovering hit))

-- | 地面层节拍（'onGroundHit'，规则 = 世界的 hitGroundWith）：(按名字的去层数, 推进后的机制)。
hitGroundIn :: Registry -> [Pos] -> [SomeMechanic] -> ([(ElementName, Int)], [SomeMechanic])
hitGroundIn world hits elems = maybe ([], elems) id (beatIn world elems [] (onGroundHit (hitGroundWith world) hits))

-- | 胜负节拍（'judge'）：内置规则判出的结局交给关卡级机制复核，有回复就用回复里的结局。
-- 内置关卡级机制都不回复，所以内置关卡与每日挑战的结局与原来逐字相同（judge_default_no_replier）。
judgeIn :: Registry -> [SomeMechanic] -> Board -> Score -> MovesLeft -> Outcome -> Outcome
judgeIn world elems b score moves out = maybe out id (queryIn world elems out (askJudge b score moves))
