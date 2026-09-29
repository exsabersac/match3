-- | 一次操作（玩家交换 / 锤子 / 自由交换 / 十字清除）的**公共结算**：主连锁 → 步末效果 →
-- 计数与目标 → 结局判定 → 自动洗牌，并由同一次计算产出回放脚本 MoveTrace。
--
-- 第二刀之前，trySwap 与三种道具各有一份几乎相同的结算代码（计数、目标、结局、洗牌四处重复），
-- 回放 trace* 又各自重算一遍；现在四处入口只负责「校验 + 选择起手方式」，其余全部在这里。
--
-- 依赖：Match3.Board（记录版连锁 CascadeRun）、State、Tally、Outcome、Shuffle、Trace、各步末机制模块。
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
  , combineCombo
  ) where

import Data.List (nub)
import Match3.Board
  ( CascadeRun(..)
  , CascadeTally(..)
  , cascadeAfterBelt
  , cascadeCountdowns
  , cascadeMatches
  , cascadeSeeds
  , getCell
  , hasAnyMatch
  , stillRun
  )
import Match3.Carpet (coverCarpets)
import Match3.Conveyor (shiftBelts)
import Match3.Countdown (tickCountdowns)
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
resolveMove kind start opening gs =
  let portals = gsPortals gs
      seg0 = case opening of
        OpenMatch prefer -> cascadeMatches prefer (gsUfos gs) portals (gsGen gs) start
        OpenSeeds prefer seeds -> cascadeSeeds prefer seeds (gsUfos gs) portals (gsGen gs) start
      (segs, ends, board1, vacateAfter) =
        if kind == KindSwap then swapEnd gs seg0 else boosterEnd seg0
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
      safesHit = max 0 (countSafes (gsBoard gs) - countSafes board1)
      spiritHit = max 0 (countTimeSpirits (gsBoard gs) - countTimeSpirits board1)
      (carpetOpen', carpetHit) =
        coverCarpets (gsCarpetOpen gs) (clearedAll ++ carpetVacateSeeds (gsBoard gs) vacateAfter)
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
        KindSwap -> g {gsMoves = gsMoves gs - 1 + 2 * spiritHit}
        KindHammer -> g {gsMoves = gsMoves gs + 2 * spiritHit, gsHammers = gsHammers gs - 1}
        KindFreeSwap -> g {gsMoves = gsMoves gs + 2 * spiritHit, gsFreeSwaps = gsFreeSwaps gs - 1}
        KindCross -> g {gsMoves = gsMoves gs + 2 * spiritHit, gsCrossClears = gsCrossClears gs - 1}
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
            }
      outcome = decideOutcome gs' gained
      gs'' = case outcome of
        Won s -> gs' {gsOver = Just (Won s)}
        Lost s -> gs' {gsOver = Just (Lost s)}
        LevelClear s n -> gs' {gsOver = Just (LevelClear s n)}
        _ -> gs'
      -- 未终局时，没有可走步则自动洗牌
      gs''' = case outcome of
        MoveApplied _ -> ensurePlayable gs''
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

-- | 玩家交换的步末：倒计时 → 皮带 → 蔓延 → 蜗牛 →（成消）再连锁。
-- 返回 (四段连锁, 步末记录, 终盘, 地毯腾空比较用的盘面)。
swapEnd :: GameState -> CascadeRun StdGen -> ([CascadeRun StdGen], [EndStep], Board, Board)
swapEnd gs seg0 =
  let portals = gsPortals gs
      belts = gsBelts gs
      ws0 = crWaves seg0
      board0' = crBoard seg0
      -- 倒计时 tick / 归零爆炸（带飞碟与传送门）
      seg1 = cascadeCountdowns (crUfos seg0) portals (crGen seg0) board0'
      bTick = tickCountdowns board0'
      ticked = [p | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let p = (r, c), getCell board0' p /= getCell bTick p]
      endTick = [EndStep (length ws0) board0' bTick (EndCountdownTick ticked) | not (null ticked)]
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
          else cascadeAfterBelt (crUfos seg1) portals (crGen seg1) boardBelt
      -- 藤 → 巧 → 蒸汽蔓延，然后蜗牛爬（跳过皮带格；传送门端点当墙）
      boardBeltCas = crBoard seg2
      nEnd = nBelt + length (crWaves seg2)
      beltCells = nub (concat belts)
      portalEnds = nub (concatMap (\(a, b) -> [a, b]) portals)
      (endSpread, boardSpread) = traceSpreads nEnd boardBeltCas
      (snails, boardSnail) = traceSnails beltCells portalEnds boardSpread
      endSnail = [EndStep nEnd boardSpread boardSnail (EndSnail snails) | not (null snails)]
      -- 蜗牛推出的匹配再连锁一次（不再重复步末效果）
      seg3 =
        if hasAnyMatch boardSnail
          then cascadeMatches Nothing (crUfos seg2) portals (crGen seg2) boardSnail
          else stillRun boardSnail (crUfos seg2) (crGen seg2)
      board1 = crBoard seg3
  in ([seg0, seg1, seg2, seg3], endTick ++ endBelt ++ endSpread ++ endSnail, board1, board1)

-- | 道具的步末：只有藤 / 巧 / 蒸汽蔓延。地毯腾空比较用蔓延前的盘面（与旧实现一致）。
boosterEnd :: CascadeRun StdGen -> ([CascadeRun StdGen], [EndStep], Board, Board)
boosterEnd seg0 =
  let boardH = crBoard seg0
      (ends, board1) = traceSpreads (length (crWaves seg0)) boardH
  in ([seg0], ends, board1, boardH)
