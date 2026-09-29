{-# LANGUAGE ScopedTypeVariables #-}

-- | 连锁：**单一实现**。每一种连锁起手（普通匹配 / 种子 / 皮带后 / 倒计时）只有一个核心函数，
-- 同时产出「结算计数」（CascadeTally）和「逐轮回放」（[CascadeWave]），两者来自同一次计算，
-- 结算与回放因此天然一致（第二刀之前是 runCascade* 与 traceCascade* 两份平行实现）。
--
-- 核心：cascadeMatchesFrom / cascadeSeeds / cascadeAfterBelt / cascadeCountdowns，返回 CascadeRun。
-- 兼容层：runCascade* / resolveCountdowns / runPostBeltCascade 仍返回旧的元组，traceCascade* 仍返回
-- (轮次, 终盘, 飞碟, 生成器)，全部由核心结果投影而来（测试与旧调用方不必改）；新代码请用记录版。
--
-- 依赖：Grid、Match、Clear、Gravity、Ufo、元素注册表（计数键 edCounter、倒计时 = PhaseTick 步末规则）。
-- 不变量：每轮 = clear → settleBoardPortals → refill → stepUfos（→ 飞碟吸收单独一轮），随机数按此顺序消耗；
-- 计数口径（逐字保持旧实现，由金标准锁定）：
--   * 匹配轮的颜色袋按「清除格 ∪ 本轮底行收饼干位」在消除前盘面上计色；种子轮 / 飞碟轮只按清除格计色；
--   * 障碍计数按清除格在消除前盘面上的格子种类计；饼干 = 被清除的饼干 + 沉降时底行收走的饼干；
--   * 种子起手的最大波次：续连锁有清除时取续连锁的最大波次，否则取「已完成的起手轮数」。
module Match3.Board.Cascade
  ( -- * 记录版（单一实现）
    CascadeTally(..)
  , zeroTally
  , CascadeRun(..)
  , cascadeMatches
  , cascadeMatchesFrom
  , cascadeSeeds
  , cascadeAfterBelt
  , cascadeCountdowns
  , stillRun
    -- * 指定注册表（元素框架；不带 With 的 = 内置注册表）
  , cascadeMatchesWith
  , cascadeMatchesFromWith
  , cascadeSeedsWith
  , cascadeAfterBeltWith
  , cascadeCountdownsWith
    -- * 回放数据
  , CascadeWave(..)
    -- * 兼容层（旧元组 API，由记录版投影）
  , stepCascade
  , stepCascadeAt
  , stepCascadeDetailed
  , runCascade
  , runCascadeAt
  , runCascadeScored
  , runCascadeScoredWithUfos
  , runCascadeScoredWithUfosFromWave
  , runCascadeScoredFromSeeds
  , runCascadeScoredFromSeedsWithUfos
  , runPostBeltCascade
  , resolveCountdowns
  , traceCascade
  , traceCascadeFromWave
  , traceCascadeFromSeeds
  , tracePostBeltCascade
  , traceCountdowns
  ) where

import Data.List (nub)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (Registry, counterWith, endRules)
import Match3.Element.Types (Counter(..), EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import Match3.Ufo (Ufo, stepUfos)
import System.Random (RandomGen)
import Match3.Board.Clear
import Match3.Board.Gravity
import Match3.Board.Grid
import Match3.Board.Match

--------------------------------------------------------------------------------
-- 数据

-- | 连锁中的一轮（一次「消除 → 下落 → 补子」）。
data CascadeWave = CascadeWave
  { cwBefore  :: Board           -- ^ 本轮消除前的盘面
  , cwCleared :: [Pos]           -- ^ 本轮被消掉的格（真消除 + 被打碎的障碍）；可能为空（仅沉降）
  , cwDrained :: [Pos]           -- ^ 沉降途中底行被收走的饼干位
  , cwHoles   :: [[Maybe Cell]]  -- ^ 消除并放下新特殊块之后、下落之前（Nothing = 空洞）
  , cwAfter   :: Board           -- ^ 重力 / 传送门 / 补子之后
  , cwScore   :: Score           -- ^ 本轮得分（与结算的波次计分相同）
  } deriving (Eq, Show)

-- | 一段连锁的累计计数（替代旧的 15 元元组）。
data CascadeTally = CascadeTally
  { ctCells       :: Int            -- ^ 清除格数（含打碎的障碍）
  , ctScore       :: Score          -- ^ 波次计分之和
  , ctMaxWave     :: Int            -- ^ 最大波次（连击数）
  , ctColors      :: [(Color, Int)] -- ^ 颜色袋（按 allColors 顺序）
  , ctStones      :: Int
  , ctChests      :: Int
  , ctHoney       :: Int
  , ctBalloons    :: Int
  , ctCookies     :: Int            -- ^ 被清除的饼干 + 底行收走的饼干
  , ctCakes       :: Int
  , ctUfoAbsorbed :: Int            -- ^ 飞碟吸走的格数（GoalUfo）
  , ctCleared     :: [Pos]          -- ^ 清除格 + 收饼干位（GoalCarpet / 前端粒子）
  , ctNamed       :: [(String, Int)] -- ^ 自定义计数（元素定义的 edCounter = CountNamed 名字），按首次出现排序
  } deriving (Eq, Show)

-- | 什么都没发生的计数（颜色袋按 allColors 全 0）。
zeroTally :: CascadeTally
zeroTally = CascadeTally 0 0 0 (zip allColors (repeat 0)) 0 0 0 0 0 0 0 [] []

-- | 一段连锁的完整结果：终盘、计数、飞碟、逐轮回放、生成器。
data CascadeRun g = CascadeRun
  { crBoard :: Board
  , crTally :: CascadeTally
  , crUfos  :: [Ufo]
  , crWaves :: [CascadeWave]
  , crGen   :: g
  }

-- | 没有任何清除 / 沉降的一段（盘面、飞碟、生成器原样）。
stillRun :: Board -> [Ufo] -> g -> CascadeRun g
stillRun b ufos g = CascadeRun b zeroTally ufos [] g

-- | 清除格在消除前盘面上的计数（按本体定义的 edCounter）。
data Hits = Hits
  { hStones, hChests, hHoney, hBalloons, hCookies, hCakes :: !Int
  , hNamed :: [(String, Int)]
  }

noHits :: Hits
noHits = Hits 0 0 0 0 0 0 []

plusHits :: Hits -> Hits -> Hits
plusHits a b =
  Hits
    (hStones a + hStones b) (hChests a + hChests b) (hHoney a + hHoney b)
    (hBalloons a + hBalloons b) (hCookies a + hCookies b) (hCakes a + hCakes b)
    (addNamed (hNamed a) (hNamed b))

-- | 沉降时底行收走的饼干也计入饼干数。
withCookies :: Int -> Hits -> Hits
withCookies n h = h {hCookies = hCookies h + n}

bumpNamed :: String -> Int -> [(String, Int)] -> [(String, Int)]
bumpNamed k v [] = [(k, v)]
bumpNamed k v ((k', v') : rest)
  | k == k' = (k', v' + v) : rest
  | otherwise = (k', v') : bumpNamed k v rest

addNamed :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
addNamed = foldl (\acc (k, v) -> bumpNamed k v acc)

hitsOn :: Registry -> Board -> [Pos] -> Hits
hitsOn reg b = foldl (\h p -> bump (counterWith reg (getCell b p)) h) noHits
  where
    bump Nothing h = h
    bump (Just k) h = case k of
      CountStones -> h {hStones = hStones h + 1}
      CountChests -> h {hChests = hChests h + 1}
      CountHoney -> h {hHoney = hHoney h + 1}
      CountBalloons -> h {hBalloons = hBalloons h + 1}
      CountCookies -> h {hCookies = hCookies h + 1}
      CountCakes -> h {hCakes = hCakes h + 1}
      CountSafes -> h   -- 保险箱 / 时间精灵按前后盘面差计（Game.Tally），不在清除格里计
      CountSpirits -> h
      CountNamed n -> h {hNamed = bumpNamed n 1 (hNamed h)}

tallyHits :: CascadeTally -> Hits
tallyHits t = Hits (ctStones t) (ctChests t) (ctHoney t) (ctBalloons t) (ctCookies t) (ctCakes t) (ctNamed t)

mkTally :: Int -> Score -> Int -> [(Color, Int)] -> Hits -> Int -> [Pos] -> CascadeTally
mkTally cells score maxW colors h uAbs cleared =
  CascadeTally cells score maxW colors (hStones h) (hChests h) (hHoney h) (hBalloons h) (hCookies h) (hCakes h) uAbs cleared (hNamed h)

addColors :: Registry -> [(Color, Int)] -> Board -> [Pos] -> [(Color, Int)]
addColors reg tallies b pos = [(col, cnt + countColorWith reg b pos col) | (col, cnt) <- tallies]

mergeColors :: [(Color, Int)] -> [(Color, Int)] -> [(Color, Int)]
mergeColors a b = [(col, lc a col + lc b col) | col <- allColors]
  where
    lc xs col = maybe 0 id (lookup col xs)

--------------------------------------------------------------------------------
-- 核心：普通匹配连锁

-- | 普通匹配连锁到稳定（波次从 1 开始）。prefer 只作用于第一轮的特殊块生成位。
cascadeMatches :: RandomGen g => Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeMatches = cascadeMatchesWith defaultRegistry

-- | cascadeMatches（指定注册表）。
cascadeMatchesWith :: RandomGen g => Registry -> Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeMatchesWith reg = cascadeMatchesFromWith reg 0

-- | 普通匹配连锁，波次编号从 startW 之后继续（种子起手 / 飞碟吸收已占用的轮数）。
cascadeMatchesFrom :: RandomGen g => Int -> Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeMatchesFrom = cascadeMatchesFromWith defaultRegistry

-- | cascadeMatchesFrom（指定注册表）。
-- 每轮：clearMatchesDetailed → settleBoardPortals → refill → stepUfos；若飞碟吸到格子，
-- 吸收单独算下一轮（clearUfoAbsorbed → settle → refill）。没有匹配时最大波次 = startW。
cascadeMatchesFromWith :: RandomGen g => Registry -> Int -> Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeMatchesFromWith reg startW prefer0 ufos0 portals g0 b0 =
  go prefer0 g0 b0 0 0 startW (zip allColors (repeat 0)) noHits 0 ufos0 [] []
  where
    go pref g b cells score maxW tallies hits uAbs ufos clearedAcc wavesAcc
      | not (hasAnyMatchWith reg b) =
          CascadeRun b (mkTally cells score maxW tallies hits uAbs (nub clearedAcc)) ufos (reverse wavesAcc) g
      | otherwise =
          let (mb, n, pos) = clearMatchesDetailedWith reg pref b
              h1 = hitsOn reg b pos
              (settled, cookiesFallen, cookSites) = settleBoardPortalsWith reg portals mb
              (b', g') = refill g settled
              posD = nub (pos ++ cookSites)
              wave = maxW + 1
              w1 = CascadeWave b pos cookSites mb b' (scoreForWave wave n)
              score' = score + scoreForWave wave n
              tallies' = addColors reg tallies b posD
              hits1 = hits `plusHits` withCookies cookiesFallen h1
              (absorbed, ufos') = stepUfos b' ufos
          in if null absorbed
               then
                 go Nothing g' b' (cells + n) score' wave tallies' hits1
                   uAbs ufos' (clearedAcc ++ posD) (w1 : wavesAcc)
               else
                 -- 飞碟吸收单独算一轮（波次 wave + 1）
                 let (mb2, n2, pos2) = clearUfoAbsorbedWith reg b' absorbed
                     h2 = hitsOn reg b' pos2
                     (settled2, cokFall, cookSites2) = settleBoardPortalsWith reg portals mb2
                     (b3, g3) = refill g' settled2
                     w2 = CascadeWave b' pos2 cookSites2 mb2 b3 (scoreForWave (wave + 1) n2)
                     score2 = score' + scoreForWave (wave + 1) n2
                     tallies2 = addColors reg tallies' b' pos2
                     uAbs' = uAbs + length [p | p <- absorbed, p `elem` pos2]
                 in go Nothing g3 b3 (cells + n + n2) score2 (wave + 1) tallies2
                      (hits1 `plusHits` withCookies cokFall h2)
                      uAbs' ufos' (clearedAcc ++ posD ++ pos2 ++ cookSites2) (w2 : w1 : wavesAcc)

--------------------------------------------------------------------------------
-- 核心：种子起手

-- | 种子起手的连锁（彩虹 / 特殊合成 / 道具 / 倒计时爆炸）：第一轮清种子（波次 1）并沉降补子，
-- 跑一次飞碟（吸到则单独一轮，波次 2），再接普通匹配连锁（波次编号衔接「已完成的起手轮数」）。
-- 种子为空时等同 cascadeMatches。
cascadeSeeds :: RandomGen g => Maybe Pos -> [Pos] -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeSeeds = cascadeSeedsWith defaultRegistry

-- | cascadeSeeds（指定注册表）。
cascadeSeedsWith :: RandomGen g => Registry -> Maybe Pos -> [Pos] -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeSeedsWith reg prefer seeds ufos0 portals g b
  | null seeds = cascadeMatchesWith reg prefer ufos0 portals g b
  | otherwise =
      let (mb, n, pos) = clearFromSeedsDetailedWith reg prefer b seeds
          h0 = hitsOn reg b pos
          (settled0, cookiesFall0, cookSites0) = settleBoardPortalsWith reg portals mb
          (b1, g1) = refill g settled0
          w0 = CascadeWave b pos cookSites0 mb b1 (scoreForWave 1 n)
          score0 = scoreForWave 1 n
          tallies0 = [(col, countColorWith reg b pos col) | col <- allColors]
          (absorbed, ufos1) = stepUfos b1 ufos0
          (wU, b1', g1', nU, hitsU, talliesU, uAbs0, posU) =
            if null absorbed
              then ([], b1, g1, 0, noHits, zip allColors (repeat 0), 0, [])
              else
                let (mb2, n2, pos2) = clearUfoAbsorbedWith reg b1 absorbed
                    h2 = hitsOn reg b1 pos2
                    (settled2, cokFall2, cookSitesU) = settleBoardPortalsWith reg portals mb2
                    (b2u, g2u) = refill g1 settled2
                    t2 = [(col, countColorWith reg b1 pos2 col) | col <- allColors]
                in ( [CascadeWave b1 pos2 cookSitesU mb2 b2u (if n2 > 0 then scoreForWave 2 n2 else 0)]
                   , b2u, g2u, n2, withCookies cokFall2 h2, t2
                   , length [p | p <- absorbed, p `elem` pos2], nub (pos2 ++ cookSitesU) )
          wavesDone = (if n > 0 then 1 else 0) + (if nU > 0 then 1 else 0)
          -- 续连锁：波次倍数接在起手轮之后
          rest = cascadeMatchesFromWith reg wavesDone Nothing ufos1 portals g1' b1'
          t2r = crTally rest
          maxW = if ctCells t2r > 0 then ctMaxWave t2r else wavesDone
          tally =
            mkTally
              (n + nU + ctCells t2r)
              (score0 + (if nU > 0 then scoreForWave 2 nU else 0) + ctScore t2r)
              maxW
              (mergeColors (mergeColors tallies0 talliesU) (ctColors t2r))
              (withCookies cookiesFall0 h0 `plusHits` hitsU `plusHits` tallyHits t2r)
              (uAbs0 + ctUfoAbsorbed t2r)
              (nub (pos ++ cookSites0 ++ posU ++ ctCleared t2r))
      in CascadeRun (crBoard rest) tally (crUfos rest) (w0 : wU ++ crWaves rest) (crGen rest)

--------------------------------------------------------------------------------
-- 核心：皮带移位之后

-- | 皮带移位之后：成消则整段连锁；否则仍沉降一次（收皮带送到底行的饼干，回放记为一个
-- 只有沉降的轮次，盘面没变且没收饼干时不记），沉降后成消再接连锁并补上饼干数与收饼干位。
cascadeAfterBelt :: RandomGen g => [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeAfterBelt = cascadeAfterBeltWith defaultRegistry

-- | cascadeAfterBelt（指定注册表）。
cascadeAfterBeltWith :: RandomGen g => Registry -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeAfterBeltWith reg ufos portals g boardBelt
  | hasAnyMatchWith reg boardBelt = cascadeMatchesWith reg Nothing ufos portals g boardBelt
  | otherwise =
      let mb = toM boardBelt
          (settled, nCook, cookSites) = settleBoardPortalsWith reg portals mb
          (b1, g1) = refill g settled
          settleWave =
            [ CascadeWave boardBelt [] cookSites mb b1 0
            | b1 /= boardBelt || not (null cookSites)
            ]
      in if hasAnyMatchWith reg b1
           then
             let r = cascadeMatchesWith reg Nothing ufos portals g1 b1
                 t = crTally r
             in r { crTally = t {ctCookies = ctCookies t + nCook, ctCleared = nub (cookSites ++ ctCleared t)}
                  , crWaves = settleWave ++ crWaves r }
           else
             CascadeRun b1 zeroTally {ctCookies = nCook, ctCleared = cookSites} ufos settleWave g1

--------------------------------------------------------------------------------
-- 核心：倒计时

-- | 一步之后倒计时 -1；归零的 3×3 爆炸走种子连锁（带飞碟与传送门）。
-- 没有归零时终盘就是 tick 之后的盘面（数字减一），不产生回放轮次。
cascadeCountdowns :: RandomGen g => [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeCountdowns = cascadeCountdownsWith defaultRegistry

-- | cascadeCountdowns（指定注册表）：依次跑 PhaseTick 阶段的步末规则，再合并各规则的引爆种子。
cascadeCountdownsWith :: RandomGen g => Registry -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeCountdownsWith reg ufos0 portals g b =
  let rules = endRules reg PhaseTick
      bTick = foldl (\bd r -> snd (erRun r (EndCtx [] []) bd)) b rules
      seeds = nub (concatMap (\r -> erSeeds r bTick) rules)
  in if null seeds
       then stillRun bTick ufos0 g
       else cascadeSeedsWith reg Nothing seeds ufos0 portals g bTick

--------------------------------------------------------------------------------
-- 兼容层：旧元组 API

type Tuple15 g = (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)

toTuple15 :: CascadeRun g -> Tuple15 g
toTuple15 r =
  let t = crTally r
  in ( crBoard r, ctCells t, ctScore t, ctMaxWave t, ctColors t, ctStones t, ctChests t, ctHoney t
     , ctBalloons t, ctCookies t, ctCakes t, ctUfoAbsorbed t, crUfos r, ctCleared t, crGen r )

toTuple12 :: CascadeRun g -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, g)
toTuple12 r =
  let t = crTally r
  in ( crBoard r, ctCells t, ctScore t, ctMaxWave t, ctColors t, ctStones t, ctChests t, ctHoney t
     , ctBalloons t, ctCookies t, ctCakes t, crGen r )

toTrace :: CascadeRun g -> ([CascadeWave], Board, [Ufo], g)
toTrace r = (crWaves r, crBoard r, crUfos r, crGen r)

-- | 一步连锁（无优先生成位）；无匹配时返回 Nothing。
stepCascade :: RandomGen g => g -> Board -> Maybe (Board, Int, g)
stepCascade = stepCascadeAt Nothing

-- | stepCascadeDetailed 的简化版：只返回新盘面、清除数和生成器。
stepCascadeAt :: RandomGen g => Maybe Pos -> g -> Board -> Maybe (Board, Int, g)
stepCascadeAt prefer g b =
  case stepCascadeDetailed prefer [] g b of
    Nothing -> Nothing
    Just (b', n, _, _, _, _, _, _, _, g') -> Just (b', n, g')

-- | 恰好一轮匹配消除 + 沉降补子（不跑飞碟）：(新盘面, 清除数, 清除格 ∪ 收饼干位, 石头, 宝箱,
-- 蜂蜜, 气球, 饼干, 蛋糕, 生成器)。与 cascadeMatchesFrom 的单轮完全相同（同一组调用）。
stepCascadeDetailed
  :: RandomGen g
  => Maybe Pos
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> Maybe (Board, Int, [Pos], Int, Int, Int, Int, Int, Int, g)
stepCascadeDetailed prefer portals g b
  | not (hasAnyMatch b) = Nothing
  | otherwise =
      let (mb, n, pos) = clearMatchesDetailed prefer b
          h = hitsOn defaultRegistry b pos
          (settled, cookiesFallen, cookSites) = settleBoardPortals portals mb
          (b', g') = refill g settled
      in Just (b', n, nub (pos ++ cookSites), hStones h, hChests h, hHoney h, hBalloons h, hCookies h + cookiesFallen, hCakes h, g')

-- | 连锁到稳定（无优先生成位），返回最终盘面、累计清除数、生成器。
runCascade :: RandomGen g => g -> Board -> (Board, Int, g)
runCascade = runCascadeAt Nothing

-- | 第一轮用 prefer 生成特殊块；返回 (终盘, 清除数, 生成器)。
runCascadeAt :: RandomGen g => Maybe Pos -> g -> Board -> (Board, Int, g)
runCascadeAt prefer g b =
  let r = cascadeMatches prefer [] [] g b
  in (crBoard r, ctCells (crTally r), crGen r)

-- | 旧 12 元组：(盘面, 清除数, 得分, 最大波次, 颜色袋, 石头, 宝箱, 蜂蜜, 气球, 饼干, 蛋糕, 生成器)。
runCascadeScored
  :: RandomGen g
  => Maybe Pos
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, g)
runCascadeScored prefer g b = toTuple12 (cascadeMatches prefer [] [] g b)

-- | 旧 15 元组版 cascadeMatches：(… 蛋糕, 飞碟吸收数, 飞碟, 清除格, 生成器)。
runCascadeScoredWithUfos
  :: RandomGen g
  => Maybe Pos
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredWithUfos prefer ufos0 portals g b = toTuple15 (cascadeMatches prefer ufos0 portals g b)

-- | 旧 15 元组版 cascadeMatchesFrom。
runCascadeScoredWithUfosFromWave
  :: RandomGen g
  => Int
  -> Maybe Pos
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredWithUfosFromWave startW prefer ufos0 portals g b =
  toTuple15 (cascadeMatchesFrom startW prefer ufos0 portals g b)

-- | 旧 12 元组版 cascadeSeeds（无飞碟 / 传送门）。
runCascadeScoredFromSeeds
  :: RandomGen g
  => Maybe Pos
  -> [Pos]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, g)
runCascadeScoredFromSeeds prefer seeds g b = toTuple12 (cascadeSeeds prefer seeds [] [] g b)

-- | 旧 15 元组版 cascadeSeeds。
runCascadeScoredFromSeedsWithUfos
  :: RandomGen g
  => Maybe Pos
  -> [Pos]
  -> [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runCascadeScoredFromSeedsWithUfos prefer seeds ufos0 portals g b =
  toTuple15 (cascadeSeeds prefer seeds ufos0 portals g b)

-- | 旧 15 元组版 cascadeAfterBelt。
runPostBeltCascade
  :: RandomGen g
  => [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
runPostBeltCascade ufos portals g b = toTuple15 (cascadeAfterBelt ufos portals g b)

-- | 旧 15 元组版 cascadeCountdowns。
resolveCountdowns
  :: RandomGen g
  => [Ufo]
  -> [(Pos, Pos)]
  -> g
  -> Board
  -> (Board, Int, Score, Int, [(Color, Int)], Int, Int, Int, Int, Int, Int, Int, [Ufo], [Pos], g)
resolveCountdowns ufos0 portals g b = toTuple15 (cascadeCountdowns ufos0 portals g b)

-- | cascadeMatches 的回放投影：(每一轮, 终盘, 飞碟, 生成器)。
traceCascade
  :: RandomGen g
  => Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCascade prefer ufos portals g b = toTrace (cascadeMatches prefer ufos portals g b)

-- | cascadeMatchesFrom 的回放投影。
traceCascadeFromWave
  :: RandomGen g
  => Int -> Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCascadeFromWave startW prefer ufos portals g b = toTrace (cascadeMatchesFrom startW prefer ufos portals g b)

-- | cascadeSeeds 的回放投影。
traceCascadeFromSeeds
  :: RandomGen g
  => Maybe Pos -> [Pos] -> [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCascadeFromSeeds prefer seeds ufos portals g b = toTrace (cascadeSeeds prefer seeds ufos portals g b)

-- | cascadeAfterBelt 的回放投影。
tracePostBeltCascade
  :: RandomGen g
  => [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
tracePostBeltCascade ufos portals g b = toTrace (cascadeAfterBelt ufos portals g b)

-- | cascadeCountdowns 的回放投影。
traceCountdowns
  :: RandomGen g
  => [Ufo] -> [(Pos, Pos)] -> g -> Board
  -> ([CascadeWave], Board, [Ufo], g)
traceCountdowns ufos portals g b = toTrace (cascadeCountdowns ufos portals g b)
