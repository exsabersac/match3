-- | 内置元素：每种元素一个类型 + 一个 Element / Modifier / LevelElement instance（xmonad LayoutClass 风格，
-- 见 Match3.Element.Class），只覆盖自己用到的方法；注册表条目（名字 → 构造器：原型值、解码、放置）
-- 在 'builtinDefs'。具体效果仍复用原机制模块（Obstacles / Grass / Snail / Countdown / Rainbow / Combos /
-- Ufo / Conveyor / Carpet / Gravity）的函数，行为逐字不变（金标准与元素查询快照锁定）。
--
-- 邻格波及的顺序（arOrder）：石头 10 → 宝箱 20 → 蜂蜜 30 → 蛋糕 40 → 气球 50 → 魔法帽 60 → 迷雾 70 →
-- 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 保险箱 110 → 时间精灵 120 → 果汁机 130 → 染色瓶 140 → 巧克力 150 →
-- 蒸汽 160 → 气泡 170。步末：倒计时（PhaseTick）；藤 10 → 巧 20 → 蒸汽 30（PhaseSpread）；蜗牛（PhaseMove）。
-- 成对交换：彩虹取色 10 → 特殊合成 20。关卡级元素（飞碟 / 皮带 / 传送门 / 地毯）按消息回复流水线节拍。
module Match3.Element.Builtin
  ( defaultRegistry
  , builtinDefs
  , builtinLevelDefs
  , traceSnails
    -- * 元素类型（测试 / 扩展用）
  , PlainGem(..)
  , SpecialGem(..)
  , SurpriseEgg(..)
  , Ice(..)
  , Jelly(..)
  , Bubble(..)
  , UfoLevel(..)
  , BeltLevel(..)
  , PortalLevel(..)
  , CarpetLevel(..)
  , specialBlast
  , putOverlay
  ) where

import Match3.Board.Grid (getCell, inBounds)
import Match3.Board.Gravity (portalTeleport)
import Match3.Carpet (coverCarpets)
import Match3.Combos (comboClearSeeds, isSpecialCombo)
import Match3.Conveyor (beltMoves)
import Match3.Countdown (explodeSeedsFor, tickCountdowns)
import Match3.Element.Class
import Match3.Element.Event
import Match3.Element.Message
import Match3.Element.Registry
import Match3.Element.Types
import Match3.Grass
  ( chipAdjacentChainExcept
  , chipAdjacentCurtainExcept
  , chipAdjacentFogExcept
  , chipAdjacentFreezeExcept
  , clearChocoAdjacent
  , clearSteamAdjacent
  , spreadChoco
  , spreadSteam
  , spreadVines
  )
import Match3.Obstacles
  ( chargeAdjacentMakersSit
  , chipAdjacentBalloonsExcept
  , chipAdjacentCakesExcept
  , chipAdjacentChestsExcept
  , chipAdjacentHoneyExcept
  , chipAdjacentSafesExcept
  , chipAdjacentStonesExcept
  , chipAdjacentTimeSpiritsExcept
  , openSurprises
  , orthoNeighbors
  , triggerAdjacentBottlesBy
  , triggerAdjacentHatsBy
  )
import Match3.Rainbow (isRainbowSwap, rainbowClearSeeds)
import qualified Match3.Snail as Snail
import Match3.Snail (snailPositions, stepSnailAtBy)
import Match3.Types
import Match3.Ufo (stepUfos)

-- | 内置注册表：全部内置元素。主流程的旧函数名（不带 With）都用它。
defaultRegistry :: Registry
defaultRegistry = foldl (flip registerLevel) (mkRegistry builtinDefs) builtinLevelDefs

-- | 全部内置条目（注册顺序 = 文档里的清单顺序）：名字 → 构造器（原型值、从格子解码、关卡放置）。
builtinDefs :: [Entry]
builtinDefs =
  [ bodyEntry 0 (PlainGem C1) (\cell -> case cell of Gem c _ _ _ -> Just (PlainGem c); _ -> Nothing) noPlace
  , special LineH
  , special LineV
  , special Bomb
  , special Rainbow
  , modifierEntry SlotIce (Ice 1) (\cell -> case cell of Gem _ _ n _ -> Just (Ice n); _ -> Nothing) $ \args cell -> case (args, cell) of
      ([AInt n], Gem col kind _ ov) -> Just (Gem col kind n ov)
      _ -> Nothing
  , overlay 0 Grass (\o -> case o of Grass -> Just GrassL; _ -> Nothing)
  , overlay 1 Vine (\o -> case o of Vine -> Just VineL; _ -> Nothing)
  , overlay 2 Choco (\o -> case o of Choco -> Just ChocoL; _ -> Nothing)
  , layeredOverlay 3 (FogL 1) (\o -> case o of Fog n -> Just (FogL n); _ -> Nothing) Fog
  , layeredOverlay 4 (ChainL 1) (\o -> case o of Chain n -> Just (ChainL n); _ -> Nothing) Chain
  , layeredOverlay 5 (FreezeL 1) (\o -> case o of Freeze n -> Just (FreezeL n); _ -> Nothing) Freeze
  , layeredOverlay 6 (CurtainL 1) (\o -> case o of Curtain n -> Just (CurtainL n); _ -> Nothing) Curtain
  , overlay 7 Steam (\o -> case o of Steam -> Just SteamL; _ -> Nothing)
  , bodyEntry 5 (StoneE 1) (\cell -> case cell of Stone n -> Just (StoneE n); _ -> Nothing) (layersPlace Stone)
  , bodyEntry 6 (ChestE 1) (\cell -> case cell of Chest n -> Just (ChestE n); _ -> Nothing) (layersPlace Chest)
  , bodyEntry 7 (HoneyE 1) (\cell -> case cell of Honey n -> Just (HoneyE n); _ -> Nothing) (layersPlace Honey)
  , bodyEntry 8 (BalloonE C1) (\cell -> case cell of Balloon c -> Just (BalloonE c); _ -> Nothing) (colorPlace Balloon)
  , bodyEntry 9 CookieE (\cell -> case cell of Cookie -> Just CookieE; _ -> Nothing) (\_ _ -> Just Cookie)
  , bodyEntry 10 (CakeE 1) (\cell -> case cell of Cake n -> Just (CakeE n); _ -> Nothing) (layersPlace Cake)
  , bodyEntry 11 MagicHatE (\cell -> case cell of MagicHat -> Just MagicHatE; _ -> Nothing) (\_ _ -> Just MagicHat)
  , bodyEntry 12 (MakerE C1 3) (\cell -> case cell of Maker c n -> Just (MakerE c n); _ -> Nothing) $ \args _ -> case args of
      [AColor c, AInt n] -> Just (Maker c (max 1 n))
      [AColor c] -> Just (Maker c 3)
      _ -> Nothing
  , bodyEntry 13 (SnailE 0 1) (\cell -> case cell of Snail dr dc -> Just (SnailE dr dc); _ -> Nothing) $ \args _ -> case args of
      [AInt dr, AInt dc] -> Just (mkSnail dr dc)
      _ -> Nothing
  , bodyEntry 14 (SafeE 1) (\cell -> case cell of Safe n -> Just (SafeE n); _ -> Nothing) (layersPlace Safe)
  , bodyEntry 15 (FlipE C1 C2) (\cell -> case cell of Flip f b -> Just (FlipE f b); _ -> Nothing) $ \args _ -> case args of
      [AColor f, AColor b] -> Just (Flip f b)
      _ -> Nothing
  , bodyEntry 16 SurpriseEgg (\cell -> case cell of Surprise -> Just SurpriseEgg; _ -> Nothing) (\_ _ -> Just Surprise)
  , bodyEntry 17 (BottleE C1) (\cell -> case cell of Bottle c -> Just (BottleE c); _ -> Nothing) (colorPlace Bottle)
  , bodyEntry 18 TimeSpiritE (\cell -> case cell of TimeSpirit -> Just TimeSpiritE; _ -> Nothing) (\_ _ -> Just TimeSpirit)
  , bodyEntry 19 (CountdownE C1 1) (\cell -> case cell of Countdown c n -> Just (CountdownE c n); _ -> Nothing) $ \args cell -> case (args, cell) of
      ([AInt n], Gem col _ _ _) -> Just (mkCountdown col n)
      ([AInt n], Countdown col _) -> Just (mkCountdown col n)
      _ -> Nothing
  , groundEntry (Jelly 2)
  , customEntry (Bubble 1) Bubble
  ]
  where
    noPlace _ _ = Nothing
    special k = bodyEntry (kindSlot k) (SpecialGem C1 k) (\cell -> case cell of Gem c _ _ _ -> Just (SpecialGem c k); _ -> Nothing) noPlace
    ovDecode f cell = case cell of
      Gem _ _ _ (Just o) -> f o
      _ -> Nothing
    -- 叠层放置 = 盖在宝石上（替换原叠层）
    overlay i ov f = modifierEntry (SlotOverlay i) (fromJustProto (f ov)) (ovDecode f) $ \_ cell -> case cell of
      Gem col kind ice _ -> Just (Gem col kind ice (Just ov))
      _ -> Nothing
    -- 带层数的叠层：放置参数 = 层数（原样）
    layeredOverlay i proto f con = modifierEntry (SlotOverlay i) proto (ovDecode f) $ \args cell -> case (args, cell) of
      ([AInt n], Gem col kind ice _) -> Just (Gem col kind ice (Just (con n)))
      _ -> Nothing
    fromJustProto = maybe (error "builtinDefs: overlay prototype") id

-- | 内置关卡级元素：按消息回复流水线节拍；去掉某项（removeLevel）即该机制不生效。
builtinLevelDefs :: [SomeLevel]
builtinLevelDefs = [SomeLevel UfoLevel, SomeLevel BeltLevel, SomeLevel PortalLevel, SomeLevel CarpetLevel]

--------------------------------------------------------------------------------
-- 宝石

-- | 普通宝石：除名字和写回格子外全用默认方法（原型 Piece：可交换、能点火、命中即消、洗牌重排……；
-- 颜色缺省取自写回的格子）。
newtype PlainGem = PlainGem Color
  deriving (Eq, Show)

instance Element PlainGem where
  name _ = "gem"
  toCell (PlainGem c) = Gem c Normal 0 Nothing

-- | 特殊块（直线 / 炸弹 / 彩虹）：洗牌保留；直线与炸弹有爆炸范围；彩虹 / 直线各挂一条成对交换规则。
data SpecialGem = SpecialGem Color GemKind
  deriving (Eq, Show)

instance Element SpecialGem where
  name (SpecialGem _ k) = case k of
    LineH -> "line_h"
    LineV -> "line_v"
    Bomb -> "bomb"
    Rainbow -> "rainbow"
    Normal -> "gem"
  toCell (SpecialGem c k) = Gem c k 0 Nothing
  keepOnShuffle (SpecialGem _ k) = k /= Normal
  blast (SpecialGem _ k) = specialBlast k
  -- 彩虹不进普通匹配提示（只经成对交换规则给提示）
  hintable (SpecialGem _ k) = k /= Rainbow
  swapRule (SpecialGem _ k) = case k of
    -- 彩虹取色：由交换对象决定清哪种颜色；先于特殊合成判定
    Rainbow -> Just (SwapRule 10 isRainbowSwap rainbowClearSeeds)
    -- 特殊 × 特殊合成：挂在直线上，规则本身检查两端（直线 / 炸弹 / 彩虹的组合）
    LineH -> Just (SwapRule 20 isSpecialCombo comboClearSeeds)
    _ -> Nothing

-- | 按种类的爆炸范围。
specialBlast :: GemKind -> Maybe (Pos -> [Pos])
specialBlast k = case k of
  LineH -> Just (\(r, _) -> [(r, c) | c <- [0 .. boardSize - 1]])
  LineV -> Just (\(_, c) -> [(r, c) | r <- [0 .. boardSize - 1]])
  Bomb -> Just (\(r, c) -> [(rr, cc) | rr <- [r - 1 .. r + 1], cc <- [c - 1 .. c + 1], inBounds (rr, cc)])
  _ -> Nothing

--------------------------------------------------------------------------------
-- 冰层与叠层（修饰器）

-- | 冰层：不挡匹配 / 交换；多层冰只削一层（不点火），末层冰随宝石一起碎（并点火）。
newtype Ice = Ice Int
  deriving (Eq, Show)

instance Modifier Ice where
  modName _ = "ice"
  modApply (Ice n) cell = case cell of
    Gem c k _ ov -> Gem c k n ov
    _ -> cell
  modActivates (Ice n) = Just (n <= 1)
  modOnHit (Ice n)
    | n > 1 = Keep (Ice (n - 1))
    | otherwise = Shatter

-- | 把叠层写回宝石格（替换原叠层）。
putOverlay :: CellOverlay -> Cell -> Cell
putOverlay ov cell = case cell of
  Gem c k i _ -> Gem c k i (Just ov)
  _ -> cell

-- | 草：真消除时随格清掉。
data GrassL = GrassL
  deriving (Eq, Show)

instance Modifier GrassL where
  modName _ = "grass"
  modApply _ = putOverlay Grass
  modStripOnClear _ = True

-- | 藤：真消除时随格清掉；步末向相邻裸宝石蔓延。
data VineL = VineL
  deriving (Eq, Show)

instance Modifier VineL where
  modName _ = "vine"
  modApply _ = putOverlay Vine
  modStripOnClear _ = True
  modEnd _ = Just (spreadRule 10 SpreadVine spreadVines)

-- | 巧克力：真消除时随格清掉；邻格真消除清掉它；步末蔓延。
data ChocoL = ChocoL
  deriving (Eq, Show)

instance Modifier ChocoL where
  modName _ = "choco"
  modApply _ = putOverlay Choco
  modStripOnClear _ = True
  modAdjacent _ = Just (AdjacentRule 150 (\ctx b -> AdjOut (clearChocoAdjacent b (acTrue ctx)) [] []))
  modEnd _ = Just (spreadRule 20 SpreadChoco spreadChoco)

-- | 迷雾：挡匹配，邻消揭一层。
newtype FogL = FogL Int
  deriving (Eq, Show)

instance Modifier FogL where
  modName _ = "fog"
  modApply (FogL n) = putOverlay (Fog n)
  modBlocksMatch _ = True
  modAdjacent _ = Just (layerChip 70 chipAdjacentFogExcept)

-- | 锁链：挡匹配 / 交换、不点火；直接命中与邻消各揭一层。
newtype ChainL = ChainL Int
  deriving (Eq, Show)

instance Modifier ChainL where
  modName _ = "chain"
  modApply (ChainL n) = putOverlay (Chain n)
  modBlocksMatch _ = True
  modBlocksSwap _ = True
  modActivates _ = Just False
  modOnHit (ChainL n) = peel n ChainL
  modAdjacent _ = Just (layerChip 80 chipAdjacentChainExcept)

-- | 火箭冰冻：不挡匹配、挡交换；邻消揭一层。
newtype FreezeL = FreezeL Int
  deriving (Eq, Show)

instance Modifier FreezeL where
  modName _ = "freeze"
  modApply (FreezeL n) = putOverlay (Freeze n)
  modBlocksSwap _ = True
  modAdjacent _ = Just (layerChip 90 chipAdjacentFreezeExcept)

-- | 窗帘：挡匹配、不点火；直接命中与邻消各揭一层。
newtype CurtainL = CurtainL Int
  deriving (Eq, Show)

instance Modifier CurtainL where
  modName _ = "curtain"
  modApply (CurtainL n) = putOverlay (Curtain n)
  modBlocksMatch _ = True
  modActivates _ = Just False
  modOnHit (CurtainL n) = peel n CurtainL
  modAdjacent _ = Just (layerChip 100 chipAdjacentCurtainExcept)

-- | 蒸汽：挡匹配；邻格真消除清掉它；步末蔓延。
data SteamL = SteamL
  deriving (Eq, Show)

instance Modifier SteamL where
  modName _ = "steam"
  modApply _ = putOverlay Steam
  modBlocksMatch _ = True
  modAdjacent _ = Just (AdjacentRule 160 (\ctx b -> AdjOut (clearSteamAdjacent b (acTrue ctx)) [] []))
  modEnd _ = Just (spreadRule 30 SpreadSteam spreadSteam)

-- | 直接命中揭一层（锁链 / 窗帘）：宝石留下，不消除。
peel :: Int -> (Int -> m) -> ModHit m
peel n con
  | n <= 1 = Remove
  | otherwise = Keep (con (n - 1))

-- | 叠层的邻消规则：揭一层，不打碎格子。
layerChip :: Int -> (Board -> [Pos] -> [Pos] -> (Board, Int)) -> AdjacentRule
layerChip order f = AdjacentRule order (\ctx b -> AdjOut (fst (f b (acTrue ctx) (acDirect ctx))) [] [])

--------------------------------------------------------------------------------
-- 占格障碍

-- | 石头：直接命中削一层、末层消除；邻消同样削层。
newtype StoneE = StoneE Int
  deriving (Eq, Show)

instance Element StoneE where
  name _ = "stone"
  toCell (StoneE n) = Stone n
  archetype _ = Blocker
  onHit (StoneE n) = chip n StoneE
  adjacentRule _ = Just (AdjacentRule 10 (deadRule chipAdjacentStonesExcept))
  counter _ = Just CountStones

-- | 宝箱：同石头。
newtype ChestE = ChestE Int
  deriving (Eq, Show)

instance Element ChestE where
  name _ = "chest"
  toCell (ChestE n) = Chest n
  archetype _ = Blocker
  onHit (ChestE n) = chip n ChestE
  adjacentRule _ = Just (AdjacentRule 20 (deadRule chipAdjacentChestsExcept))
  counter _ = Just CountChests

-- | 蜂蜜罐：同石头。
newtype HoneyE = HoneyE Int
  deriving (Eq, Show)

instance Element HoneyE where
  name _ = "honey"
  toCell (HoneyE n) = Honey n
  archetype _ = Blocker
  onHit (HoneyE n) = chip n HoneyE
  adjacentRule _ = Just (AdjacentRule 30 (deadRule chipAdjacentHoneyExcept))
  counter _ = Just CountHoney

-- | 蛋糕：同石头（层数 = 蛋糕层数）。
newtype CakeE = CakeE Int
  deriving (Eq, Show)

instance Element CakeE where
  name _ = "cake"
  toCell (CakeE n) = Cake n
  archetype _ = Blocker
  onHit (CakeE n) = chip n CakeE
  adjacentRule _ = Just (AdjacentRule 40 (deadRule chipAdjacentCakesExcept))
  counter _ = Just CountCakes

-- | 气球：命中即破；邻格同色真消除打破。
newtype BalloonE = BalloonE Color
  deriving (Eq, Show)

instance Element BalloonE where
  name _ = "balloon"
  toCell (BalloonE c) = Balloon c
  archetype _ = Blocker
  onHit _ = Destroy
  adjacentRule _ = Just (AdjacentRule 50 (deadRule chipAdjacentBalloonsExcept))
  counter _ = Just CountBalloons

-- | 饼干：打不动；随重力下落、可过传送门，落到底边被收走；离格也算覆盖地毯。
data CookieE = CookieE
  deriving (Eq, Show)

instance Element CookieE where
  name _ = "cookie"
  toCell _ = Cookie
  archetype _ = Blocker
  portal _ = True
  drains _ = [EdgeBottom]
  counter _ = Just CountCookies
  vacatesCarpet _ = True

-- | 魔法帽（固定格）：邻格真消除时给相邻宝石换色。
data MagicHatE = MagicHatE
  deriving (Eq, Show)

instance Element MagicHatE where
  name _ = "magic_hat"
  toCell _ = MagicHat
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 60 (\ctx b -> AdjOut (triggerAdjacentHatsBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] []))

-- | 果汁机（固定格）：邻格同色真消除充能，满了产出炸弹（本轮坐住）。
data MakerE = MakerE Color Int
  deriving (Eq, Show)

instance Element MakerE where
  name _ = "maker"
  toCell (MakerE c n) = Maker c n
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 130 (\ctx b -> let (b', sit) = chargeAdjacentMakersSit b (acTrue ctx) in AdjOut b' [] sit))

-- | 蜗牛（固定格）：步末爬行 / 推动。
data SnailE = SnailE Int Int
  deriving (Eq, Show)

instance Element SnailE where
  name _ = "snail"
  toCell (SnailE dr dc) = Snail dr dc
  archetype _ = Fixed
  endRule _ = Just (EndRule PhaseMove 10 snailRun (const []) (const []))

-- | 保险箱：直接命中削一层，末层开成饼干；邻消削层；按个数差计「开启」；离格也算覆盖地毯。
newtype SafeE = SafeE Int
  deriving (Eq, Show)

instance Element SafeE where
  name _ = "safe"
  toCell (SafeE n) = Safe n
  archetype _ = Blocker
  onHit (SafeE n)
    | n <= 1 = Absorb (SomeElement CookieE)
    | otherwise = Absorb (SomeElement (SafeE (n - 1)))
  adjacentRule _ = Just (AdjacentRule 110 (\ctx b -> AdjOut (fst (chipAdjacentSafesExcept b (acTrue ctx) (acDirect ctx))) [] []))
  diffCounter _ = Just CountSafes
  vacatesCarpet _ = True

-- | 双面块：按正面颜色匹配、可交换 / 改色 / 推动 / 过传送门；命中翻成背面颜色的普通宝石。
data FlipE = FlipE Color Color
  deriving (Eq, Show)

instance Element FlipE where
  name _ = "flip"
  toCell (FlipE f b) = Flip f b
  archetype _ = Blocker
  color (FlipE f _) = Just f
  blocksSwap _ = False
  portal _ = True
  pushable _ = True
  recolorable _ = True
  onHit (FlipE _ back) = Absorb (SomeElement (PlainGem back))

-- | 彩蛋：占格障碍；命中即破；开启规则 = 邻格真消除 / 直接命中时开出直线 / 炸弹（本轮坐住）或 3×3 爆炸。
-- 现行规则里彩蛋开一次就开出，没有要跨轮保存的状态，所以值是无字段的。
data SurpriseEgg = SurpriseEgg
  deriving (Eq, Show)

instance Element SurpriseEgg where
  name _ = "surprise"
  toCell _ = Surprise
  archetype _ = Blocker
  onHit _ = Destroy
  openRule _ = Just (OpenRule openSurprises)

-- | 染色瓶（固定格）：邻格真消除时把正交相邻的宝石染成瓶子颜色。
newtype BottleE = BottleE Color
  deriving (Eq, Show)

instance Element BottleE where
  name _ = "bottle"
  toCell (BottleE c) = Bottle c
  archetype _ = Fixed
  adjacentRule _ = Just (AdjacentRule 140 (\ctx b -> AdjOut (triggerAdjacentBottlesBy (acRecolor ctx) b (acTrue ctx) (acProtect ctx)) [] []))

-- | 时间精灵：命中 / 邻消即破，按个数差每个奖励 2 步。
data TimeSpiritE = TimeSpiritE
  deriving (Eq, Show)

instance Element TimeSpiritE where
  name _ = "time_spirit"
  toCell _ = TimeSpirit
  archetype _ = Blocker
  onHit _ = Destroy
  adjacentRule _ = Just (AdjacentRule 120 (deadRule chipAdjacentTimeSpiritsExcept))
  diffCounter _ = Just CountSpirits
  bonusMoves _ = 2

-- | 倒计时炸弹：按颜色匹配、可交换 / 改色 / 推动 / 过传送门，不点火；步末减一，归零 3×3 爆炸。
data CountdownE = CountdownE Color Int
  deriving (Eq, Show)

instance Element CountdownE where
  name _ = "countdown"
  toCell (CountdownE c n) = Countdown c n
  archetype _ = Blocker
  color (CountdownE c _) = Just c
  blocksSwap _ = False
  portal _ = True
  pushable _ = True
  recolorable _ = True
  onHit _ = Destroy
  endRule _ = Just (EndRule PhaseTick 10 tickRun explodeSeedsFor (const []))

--------------------------------------------------------------------------------
-- 段 5：果冻（地面层）与气泡（Custom）

-- | 双层果冻：地面层（在 gsGround 里，不占格、不挡交换 / 匹配、不随重力 / 洗牌移动）。
-- 上方格子每被消除（或被边缘收走）一次去一层；每去一层按 CountNamed "jelly" 计 1。
-- 值 = 层数（写回格子只用于显示，地面层不进盘面）。
newtype Jelly = Jelly Int
  deriving (Eq, Show)

instance Element Jelly where
  name _ = "jelly"
  toCell (Jelly n) = Custom "jelly" n
  groundRule _ = Just (\n -> if n > 1 then Just (n - 1) else Nothing)
  counter _ = Just (CountNamed "jelly")

-- | 气泡：占格本体 Custom "bubble" k。无色、挡交换、随重力下落、不穿传送门、洗牌保留；
-- 邻格有真消除（任意颜色）即破，直接命中也破；破掉计 CountNamed "bubble"。
newtype Bubble = Bubble Int
  deriving (Eq, Show)

instance Element Bubble where
  name _ = "bubble"
  toCell (Bubble k) = Custom "bubble" k
  archetype _ = Blocker
  onHit _ = Destroy
  adjacentRule _ = Just (AdjacentRule 170 bubbleAdjacent)
  counter _ = Just (CountNamed "bubble")

bubbleAdjacent :: AdjCtx -> Board -> AdjOut
bubbleAdjacent ctx b =
  let popped =
        [ q
        | q <- nubOrd [q' | p <- acTrue ctx, q' <- orthoNeighbors p, inBounds q']
        , q `notElem` acDirect ctx
        , q `notElem` acTrue ctx
        , isBubble (getCell b q)
        ]
  in AdjOut b popped []
  where
    isBubble cell = case cell of
      Custom "bubble" _ -> True
      _ -> False
    nubOrd = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

--------------------------------------------------------------------------------
-- 关卡级元素（按消息回复流水线节拍）

-- | 飞碟：每轮补子之后（Refilled）整轮吸收。
data UfoLevel = UfoLevel

instance LevelElement UfoLevel where
  levelName _ = "ufo"
  levelReply _ msg = case fromMessage msg of
    Just (Refilled us b) -> Just (SomeMessage (uncurry Absorbed (stepUfos b us)))
    Nothing -> Nothing

-- | 皮带：玩家交换的步末、倒计时之后（EndTicked）给出移位。
data BeltLevel = BeltLevel

instance LevelElement BeltLevel where
  levelName _ = "belt"
  levelReply _ msg = case fromMessage msg of
    Just (EndTicked belts) -> Just (SomeMessage (Shifted (beltMoves belts)))
    Nothing -> Nothing

-- | 传送门：沉降时（Settling）传送可穿门的本体。
data PortalLevel = PortalLevel

instance LevelElement PortalLevel where
  levelName _ = "portal"
  levelReply _ msg = case fromMessage msg of
    Just (Settling canPass ps mb) -> Just (SomeMessage (Settled (portalTeleport canPass ps mb)))
    Nothing -> Nothing

-- | 地毯：步末结算时（Covering）覆盖目标格。
data CarpetLevel = CarpetLevel

instance LevelElement CarpetLevel where
  levelName _ = "carpet"
  levelReply _ msg = case fromMessage msg of
    Just (Covering open0 hit) -> Just (SomeMessage (uncurry Covered (coverCarpets open0 hit)))
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- 共用小件

-- | 多层障碍受直接命中：削一层，末层消除。
chip :: Element e => Int -> (Int -> e) -> Hit
chip n con
  | n <= 1 = Destroy
  | otherwise = Absorb (SomeElement (con (n - 1)))

-- | 邻消规则：只削层 / 打碎，打碎的格并入清除格。
deadRule :: (Board -> [Pos] -> [Pos] -> (Board, [Pos])) -> AdjCtx -> Board -> AdjOut
deadRule f ctx b = let (b', dead) = f b (acTrue ctx) (acDirect ctx) in AdjOut b' dead []

-- | 放置：层数（缺省 1，至少 1）。
layersPlace :: (Int -> Cell) -> Placer
layersPlace con args _ = case args of
  [AInt n] -> Just (con (max 1 n))
  [] -> Just (con 1)
  _ -> Nothing

-- | 放置：一个颜色参数。
colorPlace :: (Color -> Cell) -> Placer
colorPlace con args _ = case args of
  [AColor c] -> Just (con c)
  _ -> Nothing

-- | 蔓延：每只幸存的叠层向正交相邻的裸宝石长一格；记录 (来源, 新格)。
spreadRule :: Int -> SpreadKind -> (Board -> Board) -> EndRule
spreadRule order kind spread = EndRule PhaseSpread order run (const []) (const [])
  where
    run _ b =
      let b' = spread b
          ps = spreadPairs kind b b'
      in (if null ps then Nothing else Just (EndSpread kind ps), b')

-- | 倒计时减一；列出数值真的变了的格。
tickRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
tickRun _ b =
  let b' = tickCountdowns b
      ticked = [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell b p /= getCell b' p]
  in (if null ticked then Nothing else Just (EndCountdownTick ticked), b')

-- | 蜗牛爬行（跳过本步被皮带移过的格，传送门端点当墙）。
snailRun :: EndCtx -> Board -> (Maybe EndEffect, Board)
snailRun ctx b =
  let (ms, b') = traceSnailsBy (ecPushable ctx) (ecAvoid ctx) (ecWalls ctx) b
  in (if null ms then Nothing else Just (EndSnail ms), b')

-- | stepSnailsAvoidingBlocked 的逐只记录版：对同一快照顺序逐只调用 stepSnailAtBlocked，
-- 结果盘面与原函数完全一致（测试锁定）。
traceSnails :: [Pos] -> [Pos] -> Board -> ([SnailMove], Board)
traceSnails = traceSnailsBy Snail.pushable

-- | traceSnails，可推动谓词由调用方给出（步末上下文 ecPushable = 注册表的 pushable）。
traceSnailsBy :: (Cell -> Bool) -> [Pos] -> [Pos] -> Board -> ([SnailMove], Board)
traceSnailsBy canPush avoid walls b0 = foldl one ([], b0) [p | p <- snailPositions b0, p `notElem` avoid]
  where
    one (acc, board) pos = case getCell board pos of
      Snail dr dc ->
        let board' = stepSnailAtBy canPush walls board pos
            next = (fst pos + dr, snd pos + dc)
            mv = case getCell board' pos of
              Snail dr' dc' -> SnailMove pos pos (dr', dc') Nothing
              pushed -> SnailMove pos next (dr, dc) (Just pushed)
        in (acc ++ [mv], board')
      _ -> (acc, board)
