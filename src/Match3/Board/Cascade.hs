{-# LANGUAGE ScopedTypeVariables #-}

-- | 连锁：**单一实现**。每一种连锁起手（普通匹配 / 种子 / 皮带后 / 倒计时）只有一个核心函数，
-- 同时产出「结算计数」（CascadeTally）和「逐轮回放」（[CascadeWave]），两者来自同一次计算，
-- 结算与回放因此天然一致（第二刀之前是 runCascade* 与 traceCascade* 两份平行实现）。
--
-- 核心：cascadeMatchesFromWith / cascadeSeedsWith / cascadeAfterBeltWith / cascadeCountdownsWith（全部收 Registry），返回 CascadeRun。
-- 第三刀删除了旧元组兼容层（runCascade* / resolveCountdowns / runPostBeltCascade / traceCascade*），
-- 调用方直接读 CascadeRun / CascadeTally 的字段；stepCascadeAtWith 保留为「恰好一轮」的小工具。段 2c 起本模块不依赖内置注册表（便捷旧名在 Match3.Board.Default）。
--
-- 依赖：Grid、Match、Clear、Gravity、Ufo、元素注册表（计数键 counter、倒计时 = PhaseTick 步末规则）。
-- 不变量：每轮 = clear → settleBoardPortals → refill → 整轮吸收（飞碟，段 4 起经注册表的关卡级元素 absorbWith；→ 吸收单独一轮），随机数按此顺序消耗；
-- 计数口径（逐字保持旧实现，由金标准锁定）：
--   * 匹配轮的颜色袋按「清除格 ∪ 本轮底行收饼干位」在消除前盘面上计色；种子轮 / 飞碟轮只按清除格计色；
--   * 障碍计数按清除格在消除前盘面上的格子种类计；饼干 = 被清除的饼干 + 沉降时底行收走的饼干；
--   * 种子起手的最大波次：续连锁有清除时取续连锁的最大波次，否则取「已完成的起手轮数」。
module Match3.Board.Cascade
  ( -- * 记录版（单一实现）
    CascadeTally(..)
  , zeroTally
  , CascadeRun(..)
  , stillRun
    -- * 指定注册表（元素框架；内置注册表的便捷入口见 Match3.Board.Default）
  , cascadeMatchesWith
  , cascadeMatchesFromWith
  , cascadeSeedsWith
  , cascadeAfterBeltWith
  , cascadeCountdownsWith
  , cascadeAfterEndWith
  , endHolesWith
    -- * 回放数据
  , CascadeWave(..)
    -- * 单轮
  , stepCascadeAtWith
  ) where

import Data.List (nub)
import Match3.Element.Registry (Registry, absorbWith, counterWith, endRules, pushableWith)
import Match3.Element.Types (Counter(..), EndCtx(..), EndPhase(..), EndRule(..))
import Match3.Types
import Match3.Ufo (Ufo)
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
  , ctNamed       :: [(String, Int)] -- ^ 自定义计数（元素定义的 counter = CountNamed 名字），按首次出现排序
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

-- | 清除格在消除前盘面上的计数（按本体定义的 counter）。
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

-- | 沉降时被边缘收走的格按各自的 counter 计数（内置只有饼干 → CountCookies，与旧「底行收饼干计入饼干数」相同）。
withDrained :: Registry -> [(Pos, Cell)] -> Hits -> Hits
withDrained reg drained h = foldl (\hh (_, cell) -> bumpHit (counterWith reg cell) hh) h drained

-- | 把一组命中加进已有计数（皮带后沉降收走的格）。
addHits :: Hits -> CascadeTally -> CascadeTally
addHits h t =
  t { ctStones = ctStones t + hStones h, ctChests = ctChests t + hChests h, ctHoney = ctHoney t + hHoney h
    , ctBalloons = ctBalloons t + hBalloons h, ctCookies = ctCookies t + hCookies h, ctCakes = ctCakes t + hCakes h
    , ctNamed = addNamed (ctNamed t) (hNamed h) }

bumpNamed :: String -> Int -> [(String, Int)] -> [(String, Int)]
bumpNamed k v [] = [(k, v)]
bumpNamed k v ((k', v') : rest)
  | k == k' = (k', v' + v) : rest
  | otherwise = (k', v') : bumpNamed k v rest

addNamed :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
addNamed = foldl (\acc (k, v) -> bumpNamed k v acc)

hitsOn :: Registry -> Board -> [Pos] -> Hits
hitsOn reg b = foldl (\h p -> bumpHit (counterWith reg (getCell b p)) h) noHits

-- | 一个格子的计数键加进命中。
bumpHit :: Maybe Counter -> Hits -> Hits
bumpHit Nothing h = h
bumpHit (Just k) h = case k of
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

-- | cascadeMatches（指定注册表）。
cascadeMatchesWith :: RandomGen g => Registry -> Maybe Pos -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeMatchesWith reg = cascadeMatchesFromWith reg 0

-- | cascadeMatchesFrom（指定注册表）。
-- 每轮：clearMatchesDetailed → settleBoardPortals → refill → 飞碟吸收（注册表的关卡级元素回复 Refilled 消息，内置 = stepUfos）；若飞碟吸到格子，
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
              (settled, cookiesFallen) = settleDrainWith reg portals mb
              cookSites = map fst cookiesFallen
              (b', g') = refill g settled
              posD = nub (pos ++ cookSites)
              wave = maxW + 1
              w1 = CascadeWave b pos cookSites mb b' (scoreForWave wave n)
              score' = score + scoreForWave wave n
              tallies' = addColors reg tallies b posD
              hits1 = hits `plusHits` withDrained reg cookiesFallen h1
              (absorbed, ufos') = absorbWith reg ufos b'
          in if null absorbed
               then
                 go Nothing g' b' (cells + n) score' wave tallies' hits1
                   uAbs ufos' (clearedAcc ++ posD) (w1 : wavesAcc)
               else
                 -- 飞碟吸收单独算一轮（波次 wave + 1）
                 let (mb2, n2, pos2) = clearUfoAbsorbedWith reg b' absorbed
                     h2 = hitsOn reg b' pos2
                     (settled2, cokFall) = settleDrainWith reg portals mb2
                     cookSites2 = map fst cokFall
                     (b3, g3) = refill g' settled2
                     w2 = CascadeWave b' pos2 cookSites2 mb2 b3 (scoreForWave (wave + 1) n2)
                     score2 = score' + scoreForWave (wave + 1) n2
                     tallies2 = addColors reg tallies' b' pos2
                     uAbs' = uAbs + length [p | p <- absorbed, p `elem` pos2]
                 in go Nothing g3 b3 (cells + n + n2) score2 (wave + 1) tallies2
                      (hits1 `plusHits` withDrained reg cokFall h2)
                      uAbs' ufos' (clearedAcc ++ posD ++ pos2 ++ cookSites2) (w2 : w1 : wavesAcc)

--------------------------------------------------------------------------------
-- 核心：种子起手

-- | cascadeSeeds（指定注册表）。
cascadeSeedsWith :: RandomGen g => Registry -> Maybe Pos -> [Pos] -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeSeedsWith reg prefer seeds ufos0 portals g b
  | null seeds = cascadeMatchesWith reg prefer ufos0 portals g b
  | otherwise =
      let (mb, n, pos) = clearFromSeedsDetailedWith reg prefer b seeds
          h0 = hitsOn reg b pos
          (settled0, cookiesFall0) = settleDrainWith reg portals mb
          cookSites0 = map fst cookiesFall0
          (b1, g1) = refill g settled0
          w0 = CascadeWave b pos cookSites0 mb b1 (scoreForWave 1 n)
          score0 = scoreForWave 1 n
          tallies0 = [(col, countColorWith reg b pos col) | col <- allColors]
          (absorbed, ufos1) = absorbWith reg ufos0 b1
          (wU, b1', g1', nU, hitsU, talliesU, uAbs0, posU) =
            if null absorbed
              then ([], b1, g1, 0, noHits, zip allColors (repeat 0), 0, [])
              else
                let (mb2, n2, pos2) = clearUfoAbsorbedWith reg b1 absorbed
                    h2 = hitsOn reg b1 pos2
                    (settled2, cokFall2) = settleDrainWith reg portals mb2
                    cookSitesU = map fst cokFall2
                    (b2u, g2u) = refill g1 settled2
                    t2 = [(col, countColorWith reg b1 pos2 col) | col <- allColors]
                in ( [CascadeWave b1 pos2 cookSitesU mb2 b2u (if n2 > 0 then scoreForWave 2 n2 else 0)]
                   , b2u, g2u, n2, withDrained reg cokFall2 h2, t2
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
              (withDrained reg cookiesFall0 h0 `plusHits` hitsU `plusHits` tallyHits t2r)
              (uAbs0 + ctUfoAbsorbed t2r)
              (nub (pos ++ cookSites0 ++ posU ++ ctCleared t2r))
      in CascadeRun (crBoard rest) tally (crUfos rest) (w0 : wU ++ crWaves rest) (crGen rest)

--------------------------------------------------------------------------------
-- 核心：皮带移位之后

-- | cascadeAfterBelt（指定注册表）。
cascadeAfterBeltWith :: RandomGen g => Registry -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeAfterBeltWith reg ufos portals g boardBelt
  | hasAnyMatchWith reg boardBelt = cascadeMatchesWith reg Nothing ufos portals g boardBelt
  | otherwise =
      let mb = toM boardBelt
          (settled, nCook) = settleDrainWith reg portals mb
          cookSites = map fst nCook
          (b1, g1) = refill g settled
          settleWave =
            [ CascadeWave boardBelt [] cookSites mb b1 0
            | b1 /= boardBelt || not (null cookSites)
            ]
      in if hasAnyMatchWith reg b1
           then
             let r = cascadeMatchesWith reg Nothing ufos portals g1 b1
                 t = crTally r
             in r { crTally = (addHits (withDrained reg nCook noHits) t) {ctCleared = nub (cookSites ++ ctCleared t)}
                  , crWaves = settleWave ++ crWaves r }
           else
             CascadeRun b1 (addHits (withDrained reg nCook noHits) zeroTally) {ctCleared = cookSites} ufos settleWave g1

--------------------------------------------------------------------------------
-- 核心：步末之后的补结算（段 2c）

-- | 全部步末规则在终盘上声明的空洞（erHoles，去重，按规则顺序）。内置规则恒为 []。
endHolesWith :: Registry -> Board -> [Pos]
endHolesWith reg b = nub (concat [erHoles r b | ph <- [PhaseTick, PhaseSpread, PhaseMove], r <- endRules reg ph])

-- | 步末补结算（统一路径）：把 holes 挖空，沉降（重力 / 边缘收集 / 传送门）+ 补子；盘面有变化或
-- 收走了格时记一个只有沉降的轮次；之后成消则接普通连锁（波次从 1 起）。
-- 没有空洞、边上也没有待收格时沉降是恒等、refill 不消耗随机数，结果等于旧的「成消才连锁」：
-- 内置元素的步末从不留下空洞（38 关 × 多种子扫描确认，见 docs/testing.md），金标准因此不变。
cascadeAfterEndWith :: RandomGen g => Registry -> [Pos] -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeAfterEndWith reg holes ufos portals g b =
  let mb = foldl (\m p -> setM m p Nothing) (toM b) holes
      (settled, drained) = settleDrainWith reg portals mb
      sites = map fst drained
      (b1, g1) = refill g settled
      settleWave = [CascadeWave b [] sites mb b1 0 | b1 /= b || not (null sites)]
      settleHits = withDrained reg drained noHits
  in if hasAnyMatchWith reg b1
       then
         let r = cascadeMatchesWith reg Nothing ufos portals g1 b1
             t = crTally r
         in r { crTally = (addHits settleHits t) {ctCleared = nub (sites ++ ctCleared t)}
              , crWaves = settleWave ++ crWaves r }
       else CascadeRun b1 (addHits settleHits zeroTally) {ctCleared = sites} ufos settleWave g1

--------------------------------------------------------------------------------
-- 核心：倒计时

-- | cascadeCountdowns（指定注册表）：依次跑 PhaseTick 阶段的步末规则，再合并各规则的引爆种子。
cascadeCountdownsWith :: RandomGen g => Registry -> [Ufo] -> [(Pos, Pos)] -> g -> Board -> CascadeRun g
cascadeCountdownsWith reg ufos0 portals g b =
  let rules = endRules reg PhaseTick
      bTick = foldl (\bd r -> snd (erRun r (EndCtx [] [] (pushableWith reg)) bd)) b rules
      seeds = nub (concatMap (\r -> erSeeds r bTick) rules)
  in if null seeds
       then stillRun bTick ufos0 g
       else cascadeSeedsWith reg Nothing seeds ufos0 portals g bTick

--------------------------------------------------------------------------------
-- 单轮

-- | 恰好一轮匹配消除 + 沉降补子（指定注册表；不跑飞碟、无传送门）；无匹配时返回 Nothing。
-- 与 cascadeMatchesFromWith 的单轮是同一组调用：clear → settleBoardPortals → refill；
-- prefer 为第一轮新特殊块的优先生成位。
stepCascadeAtWith :: RandomGen g => Registry -> Maybe Pos -> g -> Board -> Maybe (Board, Int, g)
stepCascadeAtWith reg prefer g b
  | not (hasAnyMatchWith reg b) = Nothing
  | otherwise =
      let (mb, n, _) = clearMatchesDetailedWith reg prefer b
          (settled, _, _) = settleBoardPortalsWith reg [] mb
          (b', g') = refill g settled
      in Just (b', n, g')
