{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 元素的世界（相当于 xmonad 的 layoutHook）：注册的只是一张**有序的类型列表**（本体种类 'Kind'、
-- 叠层 'Layer'、地面层 'GroundKind'），引擎由它解码格子：叠层由外向内 'peel'（冰层在外、叠层在内，
-- 这是 Cell 存储编码决定的），剩下的格子交给本体的 'fromCell'；都不认识时得到惰性占格 'Inert'。
--
-- 分派缓存（按 cellSlot / 叠层编号 / Custom 名字建的候选表）是引擎内部的事，不是元素作者写的东西：
-- 某个编号的候选 = 'fromCell' / 'peel' 接受该编号代表格的种类（注册倒序：同名 / 同格以后注册的为准）；
-- 候选都不认领时再按注册倒序试全部本体种类，最后才是惰性占格。
module Match3.Element.World
  ( -- * 注册项
    Def(..)
  , kindDef
  , layerDef
  , groundDef
  , inertDef
  , defName
  , defPlace
    -- * 世界
  , World
  , mkWorld
  , mkWorldChecked
  , WorldError(..)
  , worldDefs
  , worldKinds
  , worldLayers
  , worldGrounds
  , lookupDef
  , lookupGround
    -- * 解码
  , decodeBody
  , decodeLayers
  , decode
  ) where

import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe)
import Match3.Element.Ability
import Match3.Element.Kind
import Match3.Element.Layer
import Match3.Element.Types (Placer, cellSlot, overlaySlot)
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

-- | 世界：注册项 + 解码缓存。用 'mkWorld' 构造。
data World = World
  { wDefs     :: [Def]
  , wSlots    :: Array Int [SomeKind]   -- 内置本体编号（cellSlot）→ 候选（注册倒序）
  , wAll      :: [SomeKind]             -- 全部本体（注册倒序；候选都不认领时兜底）
  , wCustom   :: [(ElementName, [SomeKind])]
  , wIce      :: [SomeLayer]            -- 冰层位置的候选
  , wOverlays :: Array Int [SomeLayer]  -- 叠层编号（overlaySlot）→ 候选
  }

-- | 由注册项建世界。同名多项以后出现的为准，位置取第一次出现处。
mkWorld :: [Def] -> World
mkWorld defs0 =
  World
    { wDefs = defs
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
