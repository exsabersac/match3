{-# LANGUAGE DeriveFunctor #-}

-- | 惰性与递归模式（Haskell 特性第 4 项，docs/haskell-features/04-惰性与递归模式.md）：
--
-- * 拒绝采样改成「无穷抽样流上取第一个合格的」（Engine.Stream）之后，盘面与推进后的生成器
--   与第 4 项前的手写尾递归（Spec.Support.LegacyLazy，逐字副本）逐种子相同；
-- * 自动洗牌的有限重试（splitAtS 24 + find + 兜底第 25 次）与旧的计数循环逐项相同（整个 GameState，含生成器），
--   含「24 次都不合格、用第 25 次」的兜底分支；
-- * 流是真的惰性：被拒绝之后的元素、合格之后的元素都不会被求值（用 error 占位证明）；
-- * runPlayer 加了 bang pattern、不再压空事件表之后，帧数 / 事件 / 终态与旧写法相同；
--   再手写 Fix / cata / ana / hylo，证明「展开成帧、折叠出结果」的 hylo 也得到同一结果，并且能惰性地流出无穷回放的事件；
-- * countsFromList 由 foldl 改 foldl' 结果相同；runActions 本来就是余递归的，能吃无穷动作表。
module Spec.Lazy
  ( tests
  ) where

import ComboFx (Cascade(..), CascadeEvent(..), EndStage(..), WaveView, cascadeStages, fastStep, newCascade, wvScore)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Engine.Game (Game(..), runActions)
import Engine.Playback (Cue(..), Player, Stages(..), Tick(..), acceleratePlayer, cueStages, newPlayer, runPlayer, stepPlayer)
import Engine.Stream
import Match3.Board.Random (randomBoardSized, randomPlayableBoardSized, randomStableBoardSized)
import Match3.Core
import Match3.Element (defaultRegistry)
import Match3.Element.Level (levelRegistryIn)
import Match3.Board.Match (hasValidMoveWith)
import Match3.Game.Move (resolveSwap)
import Match3.Game.Shuffle (ensurePlayableWith)
import Match3.Game.Trace (traceEvents)
import qualified Spec.Support.LegacyLazy as Old
import Spec.Support (findMatchPair)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Toy

tests :: [TestTree]
tests =
  [ testCase "lazy_samplers_same_as_legacy" lazy_samplers_same_as_legacy
  , testCase "lazy_ensure_playable_same_as_legacy" lazy_ensure_playable_same_as_legacy
  , testCase "lazy_stream_is_lazy" lazy_stream_is_lazy
  , testCase "lazy_run_player_same_as_legacy_and_hylo" lazy_run_player_same_as_legacy_and_hylo
  , testCase "lazy_counts_and_run_actions" lazy_counts_and_run_actions
  ]

--------------------------------------------------------------------------------
-- 拒绝采样

-- | 关卡用到的几种行列，外加两个很小的盘：5 色的大盘「没三连却也没可走步」几乎不会出现，
-- 小盘上外层拒绝（稳定但不可玩，重抽）才走得到。
sizes :: [(Int, Int)]
sizes = [(8, 8), (9, 9), (7, 7), (6, 8), (9, 7), (3, 4), (4, 3)]

lazy_samplers_same_as_legacy :: Assertion
lazy_samplers_same_as_legacy = do
  let cases = [(rc, seed) | rc <- sizes, seed <- [0 .. 199 :: Int]]
  mapM_
    ( \((r, c), seed) -> do
        let g = mkStdGen seed
            lbl = show (r, c) ++ " seed " ++ show seed
            (bS, gS) = randomStableBoardSized r c g
            (bS', gS') = Old.oldRandomStableBoardSized r c g
            (bP, gP) = randomPlayableBoardSized r c g
            (bP', gP') = Old.oldRandomPlayableBoardSized r c g
        assertEqual ("stable board " ++ lbl) bS' bS
        assertEqual ("stable gen " ++ lbl) (show gS') (show gS)
        assertEqual ("playable board " ++ lbl) bP' bP
        assertEqual ("playable gen " ++ lbl) (show gP') (show gP)
    )
    cases
  -- 用例确实走到了「拒绝后重抽」：多数种子第一抽就带三连
  let rejected = [() | ((r, c), seed) <- cases, hasAnyMatch (fst (randomBoardSized r c (mkStdGen seed)))]
  assertBool ("stable sampler rejected at least once in most cases: " ++ show (length rejected)) (2 * length rejected > length cases)
  let outerRejected =
        [ () | ((r, c), seed) <- cases
             , not (hasValidMove (fst (randomStableBoardSized r c (mkStdGen seed)))) ]
  assertBool "playable sampler's outer loop is exercised" (not (null outerRejected))

--------------------------------------------------------------------------------
-- 自动洗牌

-- | 指定行列、没有可走步也没有三连的 5 色斜纹盘（与 Spec.Support.stuckNoMoveBoard 同一图案）。
stuckSized :: Int -> Int -> Board
stuckSized rows cols = boardFromRows [[mkGem (toEnum ((r + c) `mod` 5)) | c <- [0 .. cols - 1]] | r <- [0 .. rows - 1]]

-- | 石头铺满、只在 (偶, 偶) 留普通宝石：宝石互不相邻，怎么洗都没有可走步 → 24 次都不合格，用第 25 次。
hopeless :: Int -> Int -> Board
hopeless rows cols =
  boardFromRows [[if even r && even c then mkGem (toEnum ((r + c) `mod` 5)) else mkStone | c <- [0 .. cols - 1]] | r <- [0 .. rows - 1]]

lazy_ensure_playable_same_as_legacy :: Assertion
lazy_ensure_playable_same_as_legacy = do
  let levelCases =
        [ (concat ["L", show li, " seed ", show seed, " ", tag], reg, gs)
        | li <- [0 .. levelCount - 1]
        , seed <- [1, 2 :: Int]
        , Just gs0 <- [campaignGame li seed]
        , let reg = levelRegistryIn defaultRegistry (gsLevelElems gs0)
              (rows, cols) = boardDims (gsBoard gs0)
        , (tag, b) <- [("start", gsBoard gs0), ("stuck", stuckSized rows cols), ("hopeless", hopeless rows cols)]
        , let gs = gs0 {gsBoard = b, gsGen = mkStdGen (97 * li + seed), gsShuffled = False}
        ]
      overCase = case campaignGame 0 1 of
        Just gs0 -> [("over", defaultRegistry, gs0 {gsBoard = stuckSized 8 8, gsOver = Just (Won 0)})]
        Nothing -> []
      allCases = levelCases ++ overCase
  mapM_
    (\(lbl, reg, gs) -> assertEqual lbl (Old.oldEnsurePlayableWith reg gs) (ensurePlayableWith reg gs))
    allCases
  -- 三条路都走到：原盘可走（不洗）、重洗后找到可走盘、24 次都不合格用第 25 次
  let outcomes = [(gsShuffled r, hasValidMoveWith reg (gsBoard r)) | (_, reg, gs) <- levelCases, let r = ensurePlayableWith reg gs]
  assertBool "some boards kept" ((False, True) `elem` outcomes)
  assertBool "some boards reshuffled into a playable one" ((True, True) `elem` outcomes)
  assertBool "some boards fell back to the 25th shuffle" ((True, False) `elem` outcomes)

--------------------------------------------------------------------------------
-- 流的惰性

lazy_stream_is_lazy :: Assertion
lazy_stream_is_lazy = do
  let boom = error "惰性流：这一项不该被求值"
  -- findS 只看到第一个合格的为止
  findS even (1 :> 3 :> 4 :> boom) @?= (4 :: Int)
  -- splitAtS 的前缀与剩余流都不碰后面的元素
  fst (splitAtS 3 (1 :> 2 :> 3 :> boom)) @?= [1, 2, 3 :: Int]
  headS (snd (splitAtS 2 (1 :> 2 :> 3 :> boom))) @?= (3 :: Int)
  -- 自动洗牌的形状：第 2 次就合格时，第 3 次以后（含兜底的第 25 次）都不求值
  let (checked, rest) = splitAtS 24 (1 :> 2 :> boom) :: ([Int], Stream Int)
  fromMaybe (headS rest) (find even checked) @?= 2
  -- 与列表版本一致
  takeS 10 (iterateS (* 3) 1) @?= take 10 (iterate (* 3) (1 :: Integer))
  let lcg s = (s `mod` 7, (s * 1103515245 + 12345) `mod` 2147483648) :: (Int, Int)
      manual s n = if n <= (0 :: Int) then [] else let x@(_, s') = lcg s in x : manual s' (n - 1)
  takeS 20 (draws lcg 42) @?= manual 42 20
  takeS 20 (unfoldS lcg 42) @?= map fst (manual 42 20)

--------------------------------------------------------------------------------
-- runPlayer：严格性改写 + 手写 hylo

newtype Fix f = Fix (f (Fix f))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fix x) = alg (fmap (cata alg) x)

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- | hylo = cata alg . ana coalg，但不建中间的 Fix：展开一层、立刻折叠一层。
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg

-- | 播放的基函子：播完（终态），或者再播一帧（这一帧触发的事件 + 剩下的回放）。
data TickF st ev r = DoneF st | PlayingF [ev] r
  deriving (Functor)

tickCoalg :: Int -> Stages st ev -> Player st -> TickF st ev (Player st)
tickCoalg fs sm p = case stepPlayer fs sm p of
  Done final -> DoneF final
  Playing p' evs -> PlayingF evs p'

-- | 惰性的代数（~ 模式：不等后面的帧播完就先交出这一帧的事件）。
tickAlg :: TickF st ev (Int, [ev], st) -> (Int, [ev], st)
tickAlg (DoneF st) = (1, [], st)
tickAlg (PlayingF evs ~(n, rest, st)) = (n + 1, evs ++ rest, st)

hyloPlayer :: Int -> Stages st ev -> Player st -> (Int, [ev], st)
hyloPlayer fs sm = hylo tickAlg (tickCoalg fs sm)

cataAnaPlayer :: Int -> Stages st ev -> Player st -> (Int, [ev], st)
cataAnaPlayer fs sm = cata tickAlg . ana (tickCoalg fs sm)

-- | 四种跑法（现在的 runPlayer、旧写法、hylo、cata . ana）逐项相同：帧数、事件序列、终态（事件与终态经投影比较）。
samePlays :: (Eq v, Show v, Eq s, Show s) => String -> (st -> s) -> (ev -> v) -> Int -> Stages st ev -> Player st -> Assertion
samePlays lbl fin ev fs sm p = do
  let view (n, evs, st) = (n, map ev evs, fin st)
      new = view (runPlayer fs sm p)
  assertEqual (lbl ++ ": legacy") (view (Old.oldRunPlayer fs sm p)) new
  assertEqual (lbl ++ ": hylo") (view (hyloPlayer fs sm p)) new
  assertEqual (lbl ++ ": cata . ana") (view (cataAnaPlayer fs sm p)) new

lazy_run_player_same_as_legacy_and_hylo :: Assertion
lazy_run_player_same_as_legacy_and_hylo = do
  -- 自定义阶段机：倒数 n → 0，每段 len 帧，进入时报出段号
  let countdown len = Stages (const len) (\n -> if n <= 0 then Left n else Right (n - 1, [n - 1]))
  mapM_
    ( \(len, n, fs) -> do
        samePlays ("countdown " ++ show (len, n, fs)) id id fs (countdown len) (newPlayer (n :: Int))
    )
    [(len, n, fs) | len <- [0, 1, 2, 5], n <- [0, 1, 3, 10], fs <- [1, 3]]
  -- 固定队列：各种帧数（含 0 帧的提示），正常速度与加速
  let cueLists = [[Cue f (i :: Int) | (i, f) <- zip [0 ..] fsList] | fsList <- [[], [3], [0, 0], [2, 0, 5, 1], [6, 20], [1, 1, 1, 1, 1, 1]]]
  mapM_
    ( \cs -> do
        samePlays ("cues " ++ show (map cueFrames cs)) length id 3 cueStages (newPlayer cs)
        samePlays ("cues fast " ++ show (map cueFrames cs)) length id 3 cueStages (acceleratePlayer (newPlayer cs))
    )
    cueLists
  -- 真实连锁回放：每关开局走一手能消的，正常速度与加速
  let real =
        [ (li, c)
        | li <- [0 .. levelCount - 1]
        , Just gs0 <- [campaignGame li 1]
        , Just (a, b) <- [findMatchPair (gsBoard gs0)]
        , let (gs, _, mt) = resolveSwap a b gs0
              c = newCascade mt (traceEvents mt) (gsBoard gs) (gsScore gs0)
        ]
  assertBool "real cascades found" (length real > 20)
  mapM_
    ( \(li, c) -> do
        samePlays ("level " ++ show li) cascadeView eventView fastStep cascadeStages (newPlayer c)
        samePlays ("level fast " ++ show li) cascadeView eventView fastStep cascadeStages (acceleratePlayer (newPlayer c))
    )
    real
  -- 惰性：永不结束的阶段机（每帧一段、进入时报段号）。runPlayer / 旧写法都要播完才返回，
  -- hylo 加惰性代数则能边播边交出事件：取前 5 个事件只展开 5 帧。
  let forever = Stages (const 1) (\n -> Right (n + 1, [n + 1 :: Int]))
      (_, evsForever, _) = hyloPlayer 1 forever (newPlayer (0 :: Int))
  take 5 evsForever @?= [1 .. 5]
  where
    cascadeView c = (cDone c, cCombo c, cBest c, cGain c)
    eventView e = case e of
      EvHighlight k v -> (0 :: Int, k, waveKey v)
      EvVanish k v -> (1, k, waveKey v)
      EvEndStage st -> (2, stFrames st, 0)
    waveKey :: WaveView -> Int
    waveKey = wvScore

--------------------------------------------------------------------------------
-- countsFromList / runActions

lazy_counts_and_run_actions :: Assertion
lazy_counts_and_run_actions = do
  let keys = [CountColor c | c <- allColors] ++ [CountUfo, CountCookies, CountStones]
      lists =
        [ [(keys !! (i `mod` length keys), (i * 7 + seed) `mod` 5 - 2) | i <- [seed .. seed + len]]
        | seed <- [0 .. 40], len <- [0, 1, 5, 30, 200]
        ]
  mapM_ (\xs -> assertEqual (show (take 3 xs)) (Old.oldCountsFromList xs) (countsFromList xs)) lists
  -- runActions 是余递归的（每一步先交出 Step，再递归）：无穷动作表也只跑到结局为止
  let s0 = gameNew toyGame 4 7
  length (runActions toyGame s0 (repeat Reset)) @?= 5
  length (take 3 (runActions toyGame s0 (cycle [Inc 1, Reset]))) @?= 3
