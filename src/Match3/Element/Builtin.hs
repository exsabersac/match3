-- | 内置元素的定义：把原来散在 Match / Clear / Ice / Gravity / Cascade / Tally / Shuffle / Trace / Level
-- 里按构造器写死的分支，收拢成每种元素一个 ElementDef。具体效果仍复用原机制模块
-- （Obstacles / Grass / Snail / Countdown）的函数，行为逐字不变（金标准锁定）。
--
-- 依赖：Element.Types / Registry / Event、Board.Grid、Obstacles、Grass、Snail、Countdown。
-- 邻格波及的顺序（arOrder）：石头 10 → 宝箱 20 → 蜂蜜 30 → 蛋糕 40 → 气球 50 → 魔法帽 60 → 迷雾 70 →
-- 锁链 80 → 火箭冰冻 90 → 窗帘 100 → 保险箱 110 → 时间精灵 120 → 果汁机 130 → 染色瓶 140 → 巧克力 150 → 蒸汽 160。
-- 步末：倒计时（PhaseTick）；藤 10 → 巧 20 → 蒸汽 30（PhaseSpread）；蜗牛（PhaseMove）。
-- 仍走专门分支的（不在这里定义行为）：彩蛋开启、彩虹、特殊合成、飞碟、皮带 / 传送门 / 地毯（关卡特性）。
module Match3.Element.Builtin
  ( defaultRegistry
  , builtinDefs
  , traceSnails
  ) where

import Match3.Board.Grid (getCell, inBounds)
import Match3.Countdown (explodeSeedsFor, tickCountdowns)
import Match3.Element.Event
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
  , triggerAdjacentBottlesExcept
  , triggerAdjacentHatsExcept
  )
import Match3.Snail (snailPositions, stepSnailAtBlocked)
import Match3.Types

-- | 内置注册表：全部内置元素。主流程的旧函数名（不带 With）都用它。
defaultRegistry :: Registry
defaultRegistry = mkRegistry builtinDefs

-- | 全部内置定义（注册顺序 = 文档里的清单顺序）。
builtinDefs :: [ElementDef]
builtinDefs =
  [ gemDef "gem" Normal Nothing
  , gemDef "line_h" LineH (Just (\(r, _) -> [(r, c) | c <- [0 .. boardSize - 1]]))
  , gemDef "line_v" LineV (Just (\(_, c) -> [(r, c) | r <- [0 .. boardSize - 1]]))
  , gemDef "bomb" Bomb (Just (\(r, c) -> [(rr, cc) | rr <- [r - 1 .. r + 1], cc <- [c - 1 .. c + 1], inBounds (rr, cc)]))
  , gemDef "rainbow" Rainbow Nothing
  , iceElem
  , overlayElem "grass" 0 Grass
  , (overlayElem "vine" 1 Vine) {edEnd = Just (spreadRule 10 SpreadVine spreadVines)}
  , (overlayElem "choco" 2 Choco)
      { edAdjacent = Just (AdjacentRule 150 (\ctx b -> AdjOut (clearChocoAdjacent b (acTrue ctx)) [] []))
      , edEnd = Just (spreadRule 20 SpreadChoco spreadChoco)
      }
  , layeredOverlay "fog" 3 Fog 70 chipAdjacentFogExcept
  , (layeredOverlay "chain" 4 Chain 80 chipAdjacentChainExcept) {edBlocksSwap = True, edActivates = const (Just False), edOnHit = peel Chain}
  , (layeredOverlay "freeze" 5 Freeze 90 chipAdjacentFreezeExcept) {edBlocksMatch = False, edBlocksSwap = True}
  , (layeredOverlay "curtain" 6 Curtain 100 chipAdjacentCurtainExcept) {edActivates = const (Just False), edOnHit = peel Curtain}
  , (overlayElem "steam" 7 Steam)
      { edBlocksMatch = True
      , edAdjacent = Just (AdjacentRule 160 (\ctx b -> AdjOut (clearSteamAdjacent b (acTrue ctx)) [] []))
      , edEnd = Just (spreadRule 30 SpreadSteam spreadSteam)
      }
  , (layered "stone" 5 Stone 10 chipAdjacentStonesExcept) {edCounter = Just CountStones}
  , (layered "chest" 6 Chest 20 chipAdjacentChestsExcept) {edCounter = Just CountChests}
  , (layered "honey" 7 Honey 30 chipAdjacentHoneyExcept) {edCounter = Just CountHoney}
  , (blocker "balloon" 8)
      { edOnHit = const HitDestroy
      , edAdjacent = Just (AdjacentRule 50 (deadRule chipAdjacentBalloonsExcept))
      , edCounter = Just CountBalloons
      , edPlace = colorPlace Balloon
      }
  , (blocker "cookie" 9)
      { edPortal = True
      , edDrains = True
      , edCounter = Just CountCookies
      , edVacatesCarpet = True
      , edPlace = \_ _ -> Just Cookie
      }
  , (layered "cake" 10 Cake 40 chipAdjacentCakesExcept) {edCounter = Just CountCakes}
  , (fixed "magic_hat" 11)
      { edAdjacent = Just (AdjacentRule 60 (\ctx b -> AdjOut (triggerAdjacentHatsExcept b (acTrue ctx) (acProtect ctx)) [] []))
      , edPlace = \_ _ -> Just MagicHat
      }
  , (fixed "maker" 12)
      { edAdjacent = Just (AdjacentRule 130 (\ctx b -> let (b', sit) = chargeAdjacentMakersSit b (acTrue ctx) in AdjOut b' [] sit))
      , edPlace = \args _ -> case args of
          [AColor c, AInt n] -> Just (Maker c (max 1 n))
          [AColor c] -> Just (Maker c 3)
          _ -> Nothing
      }
  , (fixed "snail" 13)
      { edEnd = Just (EndRule PhaseMove 10 snailRun (const []))
      , edPlace = \args _ -> case args of
          [AInt dr, AInt dc] -> Just (mkSnail dr dc)
          _ -> Nothing
      }
  , (blocker "safe" 14)
      { edOnHit = \cell -> case cell of
          Safe n | n <= 1 -> HitAbsorb Cookie
                 | otherwise -> HitAbsorb (Safe (n - 1))
          _ -> HitImmune
      , edAdjacent = Just (AdjacentRule 110 (\ctx b -> AdjOut (fst (chipAdjacentSafesExcept b (acTrue ctx) (acDirect ctx))) [] []))
      , edDiffCounter = Just CountSafes
      , edVacatesCarpet = True
      , edPlace = layersPlace Safe
      }
  , (blocker "flip" 15)
      { edColor = \cell -> case cell of
          Flip f _ -> Just f
          _ -> Nothing
      , edBlocksSwap = False
      , edPortal = True
      , edOnHit = \cell -> case cell of
          Flip _ back -> HitAbsorb (mkGem back)
          _ -> HitImmune
      , edPlace = \args _ -> case args of
          [AColor f, AColor b] -> Just (Flip f b)
          _ -> Nothing
      }
  , (blocker "surprise" 16) {edOnHit = const HitDestroy, edPlace = \_ _ -> Just Surprise}
  , (fixed "bottle" 17)
      { edAdjacent = Just (AdjacentRule 140 (\ctx b -> AdjOut (triggerAdjacentBottlesExcept b (acTrue ctx) (acProtect ctx)) [] []))
      , edPlace = colorPlace Bottle
      }
  , (blocker "time_spirit" 18)
      { edOnHit = const HitDestroy
      , edAdjacent = Just (AdjacentRule 120 (deadRule chipAdjacentTimeSpiritsExcept))
      , edDiffCounter = Just CountSpirits
      , edBonusMoves = 2
      , edPlace = \_ _ -> Just TimeSpirit
      }
  , (blocker "countdown" 19)
      { edColor = \cell -> case cell of
          Countdown c _ -> Just c
          _ -> Nothing
      , edBlocksSwap = False
      , edPortal = True
      , edOnHit = const HitDestroy
      , edEnd = Just (EndRule PhaseTick 10 tickRun explodeSeedsFor)
      , edPlace = \args cell -> case (args, cell) of
          ([AInt n], Gem col _ _ _) -> Just (mkCountdown col n)
          ([AInt n], Countdown col _) -> Just (mkCountdown col n)
          _ -> Nothing
      }
  ]

--------------------------------------------------------------------------------
-- 模板

-- | 宝石本体（按种类）：有色、可交换、能点火、会下落、可过传送门、命中即消；特殊块洗牌保留。
gemDef :: ElementName -> GemKind -> Maybe (Pos -> [Pos]) -> ElementDef
gemDef name k blast =
  (baseDef name)
    { edSlot = SlotCell (kindSlot k)
    , edColor = \cell -> case cell of
        Gem c _ _ _ -> Just c
        _ -> Nothing
    , edBlocksSwap = False
    , edActivates = const (Just True)
    , edPortal = True
    , edOnHit = const HitDestroy
    , edKeepOnShuffle = const (k /= Normal)
    , edBlast = blast
    , edPlace = \_ _ -> Nothing
    }

-- | 占格障碍的起点：挡交换、无色、会下落、打不动、洗牌保留。
blocker :: ElementName -> Int -> ElementDef
blocker name i = (baseDef name) {edSlot = SlotCell i}

-- | 固定格（不随重力下落，把列分段）：魔法帽 / 果汁机 / 蜗牛 / 染色瓶。
fixed :: ElementName -> Int -> ElementDef
fixed name i = (blocker name i) {edFalls = False}

-- | 多层障碍（石头 / 宝箱 / 蜂蜜 / 蛋糕）：直接命中削一层、末层消除；邻消同样削层。
layered :: ElementName -> Int -> (Int -> Cell) -> Int -> (Board -> [Pos] -> [Pos] -> (Board, [Pos])) -> ElementDef
layered name i con order chip =
  (blocker name i)
    { edOnHit = \cell -> case layersOf cell of
        Just n | n <= 1 -> HitDestroy
               | otherwise -> HitAbsorb (con (n - 1))
        Nothing -> HitImmune
    , edAdjacent = Just (AdjacentRule order (deadRule chip))
    , edPlace = layersPlace con
    }
  where
    layersOf cell = case cell of
      Stone n -> Just n
      Chest n -> Just n
      Honey n -> Just n
      Cake n -> Just n
      _ -> Nothing

-- | 邻消规则：只削层 / 打碎，打碎的格并入清除格。
deadRule :: (Board -> [Pos] -> [Pos] -> (Board, [Pos])) -> AdjCtx -> Board -> AdjOut
deadRule chip ctx b = let (b', dead) = chip b (acTrue ctx) (acDirect ctx) in AdjOut b' dead []

-- | 放置：层数（缺省 1，至少 1）。
layersPlace :: (Int -> Cell) -> [Arg] -> Cell -> Maybe Cell
layersPlace con args _ = case args of
  [AInt n] -> Just (con (max 1 n))
  [] -> Just (con 1)
  _ -> Nothing

-- | 放置：一个颜色参数。
colorPlace :: (Color -> Cell) -> [Arg] -> Cell -> Maybe Cell
colorPlace con args _ = case args of
  [AColor c] -> Just (con c)
  _ -> Nothing

-- | 冰层：不挡匹配 / 交换；多冰只削冰（不点火），末层冰随宝石消除（并点火）。
iceElem :: ElementDef
iceElem =
  (baseDef "ice")
    { edSlot = SlotIce
    , edBlocksSwap = False
    , edActivates = \cell -> case cell of
        Gem _ _ n _ -> Just (n <= 1)
        _ -> Nothing
    , edOnHit = \cell -> case cell of
        Gem col kind n o
          | n > 1 -> HitAbsorb (Gem col kind (n - 1) o)
          | otherwise -> HitDestroy
        _ -> HitPierce
    , edPlace = \args cell -> case (args, cell) of
        ([AInt n], Gem col kind _ ov) -> Just (Gem col kind n ov)
        _ -> Nothing
    }

-- | 叠层的起点：不挡匹配 / 交换、不影响点火、直接命中穿透；放置 = 盖在宝石上（替换原叠层）。
overlayElem :: ElementName -> Int -> CellOverlay -> ElementDef
overlayElem name i ov =
  (baseDef name)
    { edSlot = SlotOverlay i
    , edBlocksSwap = False
    , edActivates = const Nothing
    , edOnHit = const HitPierce
    , edStripOnClear = ov `elem` [Grass, Vine, Choco]
    , edPlace = \_ cell -> case cell of
        Gem col kind ice _ -> Just (Gem col kind ice (Just ov))
        _ -> Nothing
    }

-- | 带层数的叠层（迷雾 / 锁链 / 火箭冰冻 / 窗帘）：挡匹配，邻消揭一层；放置参数 = 层数（原样）。
layeredOverlay :: ElementName -> Int -> (Int -> CellOverlay) -> Int -> (Board -> [Pos] -> [Pos] -> (Board, Int)) -> ElementDef
layeredOverlay name i con order chip =
  (overlayElem name i (con 1))
    { edBlocksMatch = True
    , edAdjacent = Just (AdjacentRule order (\ctx b -> AdjOut (fst (chip b (acTrue ctx) (acDirect ctx))) [] []))
    , edPlace = \args cell -> case (args, cell) of
        ([AInt n], Gem col kind ice _) -> Just (Gem col kind ice (Just (con n)))
        _ -> Nothing
    }

-- | 直接命中揭一层（锁链 / 窗帘）：宝石留下，不消除。
peel :: (Int -> CellOverlay) -> Cell -> HitResult
peel con cell = case cell of
  Gem col kind _ (Just ov) ->
    let n = case ov of
          Chain k -> k
          Curtain k -> k
          _ -> 1
    in if n <= 1
         then HitAbsorb (Gem col kind 0 Nothing)
         else HitAbsorb (Gem col kind 0 (Just (con (n - 1))))
  _ -> HitPierce

--------------------------------------------------------------------------------
-- 步末规则

-- | 蔓延：每只幸存的叠层向正交相邻的裸宝石长一格；记录 (来源, 新格)。
spreadRule :: Int -> SpreadKind -> (Board -> Board) -> EndRule
spreadRule order kind spread = EndRule PhaseSpread order run (const [])
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
  let (ms, b') = traceSnails (ecAvoid ctx) (ecWalls ctx) b
  in (if null ms then Nothing else Just (EndSnail ms), b')

-- | stepSnailsAvoidingBlocked 的逐只记录版：对同一快照顺序逐只调用 stepSnailAtBlocked，
-- 结果盘面与原函数完全一致（测试锁定）。
traceSnails :: [Pos] -> [Pos] -> Board -> ([SnailMove], Board)
traceSnails avoid walls b0 = foldl one ([], b0) [p | p <- snailPositions b0, p `notElem` avoid]
  where
    one (acc, board) pos = case getCell board pos of
      Snail dr dc ->
        let board' = stepSnailAtBlocked walls board pos
            next = (fst pos + dr, snd pos + dc)
            mv = case getCell board' pos of
              Snail dr' dc' -> SnailMove pos pos (dr', dc') Nothing
              pushed -> SnailMove pos next (dr, dc) (Just pushed)
        in (acc ++ [mv], board')
      _ -> (acc, board)
