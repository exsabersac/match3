-- | 一次操作（玩家交换 / 锤子 / 自由交换 / 十字清除）的**公共结算**：主连锁 → 步末效果 →
-- 计数与目标 → 结局判定 → 自动洗牌，并由同一次计算产出回放脚本 MoveTrace。
--
-- 第二刀之前，trySwap 与三种道具各有一份几乎相同的结算代码（计数、目标、结局、洗牌四处重复），
-- 回放 trace* 又各自重算一遍；现在四处入口只负责「校验 + 选择起手方式」，其余全部在这里。
--
-- 依赖：Match3.Board（记录版连锁 CascadeRun）、State、Tally、Outcome、Shuffle、Trace、元素注册表
-- （步末阶段 PhaseTick / PhaseSpread / PhaseMove 的规则、按差计数、地毯腾空都查注册表；皮带是关卡特性）。
-- 不变量（逐字保持旧行为，金标准锁定）：
--   * 玩家交换的步末顺序：倒计时 tick / 爆炸 → 皮带移位 + 皮带后连锁 → 藤 / 巧 / 蒸汽蔓延 → 蜗牛 →
--     （蜗牛推出匹配）再连锁一次；道具只有蔓延，没有倒计时 / 皮带 / 蜗牛；
--   * 连击数：第一段的最大波次，之后每段有清除时叠加该段的最大波次；
--   * 交换耗 1 步，道具不耗步但扣对应次数；时间精灵每只 +2 步；
--   * 只有 MoveApplied（未终局）才调用 ensurePlayable；洗牌前的盘面 / 生成器记在 mtFinal / mtGen。
module Match3.Game.Resolve
  ( MoveKind(..)
  , Opening(..)
  , resolveMove
  , resolveMoveWith
  , combineCombo
  ) where

import Data.List (nub)
import Match3.Board
  ( CascadeRun(..)
  , CascadeTally(..)
  , cascadeAfterBeltWith
  , cascadeCountdownsWith
  , cascadeMatchesWith
  , cascadeSeedsWith
  , hasAnyMatchWith
  , stillRun
  )
import Match3.Carpet (coverCarpets)
import Match3.Conveyor (shiftBelts)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry, endRules)
import Match3.Element.Types (Counter(..), EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import Match3.Game.Outcome
import Match3.Game.Shuffle
import Match3.Game.State
import Match3.Game.Tally
import Match3.Game.Trace
import System.Random (StdGen)

-- | 操作种类：决定步末效果、步数 / 道具次数的扣法。
data MoveKind = KindSwap | KindHammer | KindFreeSwap | KindCross
  deriving (Eq, Show)

-- | 主连锁的起手方式。
data Opening
  = OpenMatch (Maybe Pos)        -- ^ 普通匹配连锁（prefer = 新特殊块的优先生成位）
  | OpenSeeds (Maybe Pos) [Pos]  -- ^ 种子起手（彩虹 / 特殊合成 / 锤子 / 十字）
  deriving (Eq, Show)

-- | 多段连锁的连击数：第一段的最大波次，之后每段有清除时把该段的最大波次叠加上去。
combineCombo :: [CascadeTally] -> Int
combineCombo [] = 0
combineCombo (t0 : ts) = foldl step (ctMaxWave t0) ts
  where
    step c t = max c (if ctCells t > 0 then c + ctMaxWave t else c)

-- | 公共结算。start = 第一轮之前的盘面（交换后 / 道具原盘）；调用方已完成全部校验
-- （越界、无次数、挡交换、无匹配等拒绝路径不进这里）。返回 (新状态, 结局, 回放脚本)。
resolveMove :: MoveKind -> Board -> Opening -> GameState -> (GameState, Outcome, MoveTrace)
resolveMove = resolveMoveWith defaultRegistry

-- | 公共结算（指定注册表）：主连锁、步末规则、计数、洗牌都用这张表里的元素定义。
resolveMoveWith :: Registry -> MoveKind -> Board -> Opening -> GameState -> (GameState, Outcome, MoveTrace)
resolveMoveWith reg kind start opening gs =
  let portals = gsPortals gs
      seg0 = case opening of
        OpenMatch prefer -> cascadeMatchesWith reg prefer (gsUfos gs) portals (gsGen gs) start
        OpenSeeds prefer seeds -> cascadeSeedsWith reg prefer seeds (gsUfos gs) portals (gsGen gs) start
      (segs, ends, board1, vacateAfter) =
        if kind == KindSwap then swapEnd reg gs seg0 else boosterEnd reg seg0
      finalSeg = last segs
      tallies = map crTally segs
      ufosF = crUfos finalSeg
      gFinal = crGen finalSeg
      -- 计数
      colors = foldl1 mergeTallies (map ctColors tallies)
      total f = sum (map f tallies)
      stonesHit = total ctStones
      chestsHit = total ctChests
      honeyHit = total ctHoney
      balloonHit = total ctBalloons
      cookieHit = total ctCookies
      cakeHit = total ctCakes
      uAbs = total ctUfoAbsorbed
      gained = total ctScore
      clearedAll = concatMap ctCleared tallies
      combo = combineCombo tallies
      -- 按前后盘面差计数（保险箱开启、时间精灵 +2 步、自定义）
      diffs = diffCountsWith reg (gsBoard gs) board1
      safesHit = sum [dcCount d | d <- diffs, dcCounter d == CountSafes]
      bonusMoves = sum (map dcBonus diffs)
      namedCounts =
        foldl addNamed (gsElementCounts gs)
          (concatMap ctNamed tallies ++ [(n, dcCount d) | d <- diffs, CountNamed n <- [dcCounter d], dcCount d > 0])
      (carpetOpen', carpetHit) =
        coverCarpets (gsCarpetOpen gs) (clearedAll ++ carpetVacateSeedsWith reg (gsBoard gs) vacateAfter)
      ufoCollected' = gsUfoCollected gs + uAbs
      cookies' = gsCookiesCollected gs + cookieHit
      cakes' = gsCakesCleared gs + cakeHit
      safes' = gsSafesOpened gs + safesHit
      carpets' = gsCarpetsCovered gs + carpetHit
      collected' = case gsGoal gs of
        GoalCollect col _ -> gsCollected gs + lookupColor colors col
        GoalCollectMulti reqs ->
          let bag' = mergeTallies (gsColorBag gs) colors
          in sum [min n (lookupColor bag' c) | (c, n) <- reqs]
        GoalClearStone _ -> gsStonesCleared gs + stonesHit
        GoalChest _ -> gsChestsCleared gs + chestsHit
        GoalHoney _ -> gsHoneyCleared gs + honeyHit
        GoalBalloon _ -> gsBalloonsPopped gs + balloonHit
        GoalCookie _ -> cookies'
        GoalCake _ -> cakes'
        GoalSafe _ -> safes'
        GoalCarpet _ -> carpets'
        GoalScore _ -> gsCollected gs
        GoalUfo _ -> ufoCollected'
      -- 步数与道具次数
      spend g = case kind of
        KindSwap -> g {gsMoves = gsMoves gs - 1 + bonusMoves}
        KindHammer -> g {gsMoves = gsMoves gs + bonusMoves, gsHammers = gsHammers gs - 1}
        KindFreeSwap -> g {gsMoves = gsMoves gs + bonusMoves, gsFreeSwaps = gsFreeSwaps gs - 1}
        KindCross -> g {gsMoves = gsMoves gs + bonusMoves, gsCrossClears = gsCrossClears gs - 1}
      gs' =
        spend
          gs
            { gsBoard = board1
            , gsScore = gsScore gs + gained
            , gsCollected = collected'
            , gsColorBag = mergeTallies (gsColorBag gs) colors
            , gsStonesCleared = gsStonesCleared gs + stonesHit
            , gsChestsCleared = gsChestsCleared gs + chestsHit
            , gsHoneyCleared = gsHoneyCleared gs + honeyHit
            , gsBalloonsPopped = gsBalloonsPopped gs + balloonHit
            , gsCookiesCollected = cookies'
            , gsCakesCleared = cakes'
            , gsSafesOpened = safes'
            , gsGen = gFinal
            , gsHistory = take 20 (snapshot gs : gsHistory gs)
            , gsHint = Nothing
            , gsCombo = combo
            , gsShuffled = False
            , gsUfos = ufosF
            , gsUfoCollected = ufoCollected'
            , gsCarpetOpen = carpetOpen'
            , gsCarpetsCovered = carpets'
            , gsLastCleared = nub clearedAll
            , gsElementCounts = namedCounts
            }
      outcome = decideOutcome gs' gained
      gs'' = case outcome of
        Won s -> gs' {gsOver = Just (Won s)}
        Lost s -> gs' {gsOver = Just (Lost s)}
        LevelClear s n -> gs' {gsOver = Just (LevelClear s n)}
        _ -> gs'
      -- 未终局时，没有可走步则自动洗牌
      gs''' = case outcome of
        MoveApplied _ -> ensurePlayableWith reg gs''
        _ -> gs''
      trace =
        MoveTrace
          { mtStart = start
          , mtWaves = concatMap crWaves segs
          , mtFinal = board1
          , mtEnd = ends
          , mtGen = gFinal
          , mtShuffle = if gsShuffled gs''' then Just (gsBoard gs''') else Nothing
          }
  in (gs''', outcome, trace)

-- | 自定义计数累加（按名字，保持首次出现顺序）。
addNamed :: [(String, Int)] -> (String, Int) -> [(String, Int)]
addNamed [] kv = [kv]
addNamed ((k', v') : rest) (k, v)
  | k == k' = (k', v' + v) : rest
  | otherwise = (k', v') : addNamed rest (k, v)

-- | 依次跑某阶段的步末规则：返回 (步末记录, 终盘)。空效果不记录。
runPhase :: Registry -> EndPhase -> EndCtx -> Int -> Board -> ([EndStep], Board)
runPhase reg ph ctx k b0 = foldl one ([], b0) (endRules reg ph)
  where
    one (acc, before) rule =
      let (eff, after) = erRun rule ctx before
      in (acc ++ [EndStep k before after e | Just e <- [eff]], after)

-- | 玩家交换的步末：倒计时（PhaseTick）→ 皮带 → 蔓延（PhaseSpread）→ 会走的元素（PhaseMove）→（成消）再连锁。
-- 返回 (四段连锁, 步末记录, 终盘, 地毯腾空比较用的盘面)。
swapEnd :: Registry -> GameState -> CascadeRun StdGen -> ([CascadeRun StdGen], [EndStep], Board, Board)
swapEnd reg gs seg0 =
  let portals = gsPortals gs
      belts = gsBelts gs
      ws0 = crWaves seg0
      board0' = crBoard seg0
      -- 倒计时 tick / 归零爆炸（带飞碟与传送门）
      seg1 = cascadeCountdownsWith reg (crUfos seg0) portals (crGen seg0) board0'
      (endTick, _) = runPhase reg PhaseTick (EndCtx [] []) (length ws0) board0'
      -- 皮带：移位后连锁 / 沉降（收皮带送到底行的饼干）
      boardCd = crBoard seg1
      boardBelt = shiftBelts boardCd belts
      nBelt = length ws0 + length (crWaves seg1)
      endBelt =
        [ EndStep nBelt boardCd boardBelt (EndBeltShift mv)
        | not (null belts)
        , let mv = beltMoves belts
        , not (null mv)
        ]
      seg2 =
        if null belts
          then stillRun boardCd (crUfos seg1) (crGen seg1)
          else cascadeAfterBeltWith reg (crUfos seg1) portals (crGen seg1) boardBelt
      -- 蔓延，然后会走的元素（跳过皮带格；传送门端点当墙）
      boardBeltCas = crBoard seg2
      nEnd = nBelt + length (crWaves seg2)
      beltCells = nub (concat belts)
      portalEnds = nub (concatMap (\(a, b) -> [a, b]) portals)
      (endSpread, boardSpread) = traceSpreadsWith reg nEnd boardBeltCas
      (endMove, boardSnail) = runPhase reg PhaseMove (EndCtx beltCells portalEnds) nEnd boardSpread
      -- 蜗牛推出的匹配再连锁一次（不再重复步末效果）
      seg3 =
        if hasAnyMatchWith reg boardSnail
          then cascadeMatchesWith reg Nothing (crUfos seg2) portals (crGen seg2) boardSnail
          else stillRun boardSnail (crUfos seg2) (crGen seg2)
      board1 = crBoard seg3
  in ([seg0, seg1, seg2, seg3], endTick ++ endBelt ++ endSpread ++ endMove, board1, board1)

-- | 道具的步末：只有蔓延。地毯腾空比较用蔓延前的盘面（与旧实现一致）。
boosterEnd :: Registry -> CascadeRun StdGen -> ([CascadeRun StdGen], [EndStep], Board, Board)
boosterEnd reg seg0 =
  let boardH = crBoard seg0
      (ends, board1) = traceSpreadsWith reg (length (crWaves seg0)) boardH
  in ([seg0], ends, board1, boardH)
