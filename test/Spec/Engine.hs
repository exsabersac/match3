{-# LANGUAGE ScopedTypeVariables #-}

-- | 多游戏通用接口：玩具实现跑通 step / 结局 / 播放层，通用层源码不依赖三消，三消实例与直接调用一致。
-- （由 test/Spec.hs 按功能拆出；测试名与断言逐字不变，入口 test/Spec.hs 按原名汇总。）
module Spec.Engine
  ( tests
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Engine.History (History(..), Undoable(..), historyDepth, startHistory)
import Numeric (showHex)
import Spec.Support.Source (importsOf, mentionsIdent, sourcesUnder, sourcesUnderAll)
import Match3.Core
import Match3.Game.Trace (traceEvents)
import Engine.Effect (Effect(..))
import Engine.Game (Game(..), Step(..), finalState, runActions, stepEffects)
import Engine.Playback (Cue(..), Stages(..), Tick(..), acceleratePlayer, cueStages, effectCues, newPlayer, playerProgress, runPlayer, stepPlayer)
import qualified Match3.Engine as M3E
import Toy
import Test.Tasty
import Test.Tasty.HUnit

-- | 本模块的测试（原名，平铺进顶层 "match3" 组，--list-tests 路径与拆分前相同）。
tests :: [TestTree]
tests =
  [ testCase "engine_toy_counter_game" engine_toy_counter_game
  , testCase "engine_layer_is_game_agnostic" engine_layer_is_game_agnostic
  , testCase "engine_match3_instance_matches_direct_api" engine_match3_instance_matches_direct_api
  , testCase "engine_undo_after_terminal_matches_legacy_play" engine_undo_after_terminal_matches_legacy_play
  , testCase "engine_frontend_steps_only_via_gameStep" engine_frontend_steps_only_via_gameStep
  ]

--------------------------------------------------------------------------------
-- 第三刀：多游戏通用接口

-- | 玩具实现（test/Toy.hs，只 import Engine.*）：通用接口 + 通用纯播放层不依赖三消就能跑通
-- 开局、step、被拒动作、结局判定（胜 / 超出判负 / 步数用完判负）、效果映射与按节拍播放（含加速）。
engine_toy_counter_game :: Assertion
engine_toy_counter_game = do
  let s0 = gameNew toyGame 4 7
  assertEqual "seeded start" (ToyState 0 5 5) s0
  assertEqual "not over at start" Nothing (gameOutcome toyGame s0)
  assertEqual "4 candidate actions" 4 (length (gameActions toyGame s0))
  -- 胜：被拒的 Inc 9 不改状态、不耗步；到达目标即停，后面的动作不再执行
  let steps = runActions toyGame s0 [Inc 2, Inc 9, Inc 2, Reset, Inc 3, Inc 2, Inc 1]
  assertEqual "stops at terminal" 6 (length steps)
  assertEqual "accepted flags" [True, False, True, True, True, True] (map stepAccepted steps)
  assertEqual "rejected keeps state" (stepState (steps !! 0)) (stepState (steps !! 1))
  assertEqual "rejected has no events" [] (stepEvents (steps !! 1))
  assertEqual "won" (Just ToyWon) (stepOutcome (last steps))
  assertEqual "final" (ToyState 5 5 0) (finalState toyGame s0 [Inc 2, Inc 9, Inc 2, Reset, Inc 3, Inc 2, Inc 1])
  assertEqual "events" [Bumped 2, Bumped 2, Zeroed, Bumped 3, Bumped 2, Finished ToyWon] (concatMap stepEvents steps)
  assertEqual "terminal: no actions" [] (gameActions toyGame (stepState (last steps)))
  assertEqual "terminal: step rejected" False (stepAccepted (gameStep toyGame (stepState (last steps)) (Inc 1)))
  -- 负：超出目标 / 步数用完
  assertEqual "overshoot lost" [Nothing, Just ToyLost] (map stepOutcome (runActions toyGame s0 [Inc 3, Inc 3, Inc 1]))
  assertEqual "turns out lost" (Just ToyLost) (stepOutcome (last (runActions toyGame s0 (replicate 9 Reset))))
  assertEqual "turns out after 5" 5 (length (runActions toyGame s0 (replicate 9 Reset)))
  assertEqual "status" [("count", 0), ("target", 5), ("turns", 5)] (gameStatus toyGame s0)
  -- 通用效果：按节拍分组排队，通用播放器逐帧播放
  let effs = concatMap (stepEffects toyGame) steps
      cues = effectCues toyFrames effs
  assertEqual "effects mapped" 6 (length effs)
  assertEqual "two beats" [6, 20] (map cueFrames cues)
  let (frames, fired, _) = runPlayer 3 cueStages (newPlayer cues)
  assertEqual "normal speed frames" 26 frames
  assertEqual "entering 2nd cue fires its payload" [map efKind (last effs : [])] (map (map efKind) fired)
  let (framesFast, _, _) = runPlayer 3 cueStages (acceleratePlayer (newPlayer cues))
  assertEqual "accelerated frames" 9 framesFast
  let p3 = iterate (\p -> case stepPlayer 3 cueStages p of Playing p' _ -> p'; Done _ -> p) (newPlayer cues) !! 3
  assertEqual "progress" 0.5 (playerProgress cueStages p3)
  -- 自定义阶段机：倒数 3 → 0，每段 2 帧，进入时报出段号
  let countdown = Stages (const 2) (\n -> if n <= 0 then Left n else Right (n - 1, [n - 1]))
      (fr, evs, final) = runPlayer 3 countdown (newPlayer (3 :: Int))
  assertEqual "custom stages events" [2, 1, 0 :: Int] evs
  assertEqual "custom stages frames" 8 fr
  assertEqual "custom stages final" 0 final

-- | 通用层（src/Engine/*、app/Shell/*）与玩具实现不 import 任何 Match3 模块（依赖方向单向）。
engine_layer_is_game_agnostic :: Assertion
engine_layer_is_game_agnostic = do
  files <- (++ ["test/Toy.hs"]) <$> sourcesUnderAll ["src/Engine", "app/Shell"]
  assertBool "scanned the generic layer" (all (`elem` files) ["src/Engine/Game.hs", "src/Engine/Playback.hs", "app/Shell/Loop.hs"])
  srcs <- mapM readFile files
  let bad =
        [ f ++ ": import " ++ m
        | (f, src) <- zip files srcs
        , m <- importsOf src
        , m == "Match3" || "Match3." `isPrefixOf` m
        ]
  assertEqual "no Match3 imports in the generic layer" [] bad

-- | 三消实例（Match3.Engine）：经通用接口 step 的结果与直接调用旧入口（trySwap / use* / trace* /
-- shuffleGame / applyHint）逐位相同；撤销（段 3 起在 Engine.History，经 match3Shell）回到走步前的快照；runActions 在结局处停下；候选动作都会被接受；效果映射不丢事件。
engine_match3_instance_matches_direct_api :: Assertion
engine_match3_instance_matches_direct_api = do
  let g = M3E.match3Game
      sameGs a b = show a == show b
  forM_' [(li, seed) | li <- [0, 6, 12, 27], seed <- [1, 2]] $ \(li, seed) -> do
    let s0 = gameNew g (M3E.Campaign li) seed
        tag = "L" ++ show li ++ " s" ++ show seed
    assertBool (tag ++ " new") (sameGs s0 (newGameAtLevel li (levelConfig (allLevels !! li)) seed))
    let acts = take 3 (gameActions g s0)
    assertBool (tag ++ " has actions") (not (null acts))
    forM_' acts $ \a -> case a of
      M3E.Swap p q -> do
        let st = gameStep g s0 a
            (gs', out) = trySwap p q s0
        assertBool (tag ++ " swap state") (sameGs (stepState st) gs')
        assertBool (tag ++ " accepted") (stepAccepted st)
        assertEqual (tag ++ " outcome") (gsOver gs') (stepOutcome st)
        assertEqual (tag ++ " events") (traceEvents (traceSwap p q s0)) (stepEvents st)
        assertEqual (tag ++ " effects") (length (stepEvents st)) (length (stepEffects g st))
        assertEqual (tag ++ " score effects")
          (gsScore gs' - gsScore s0)
          (sum [efAmount e | e <- stepEffects g st, efKind e == "score"])
        let pd = M3E.play a s0
        assertEqual (tag ++ " fx") (moveFx s0 gs' out) (M3E.pdFx pd)
        assertEqual (tag ++ " outcome value") (Just out) (M3E.pdOutcome pd)
        -- 撤销（通用历史层）：回到走步前的快照，清掉本步特效字段 / 提示 / 洗牌标记
        let sh = M3E.match3Shell
            h1 = stepState (gameStep sh (startHistory s0) (Act a))
            su = gameStep sh h1 Undo
        assertBool (tag ++ " swap via shell") (sameGs (histNow h1) gs')
        assertEqual (tag ++ " history depth") 1 (historyDepth h1)
        assertBool (tag ++ " undo") (stepAccepted su && sameGs (histNow (stepState su)) ((clearMoveFx s0) {gsHint = Nothing, gsShuffled = False}))
      _ -> assertFailure "gameActions should only list swaps"
    -- 道具与被拒的动作
    let hm = gameStep g s0 (M3E.Hammer (4, 4))
    assertBool (tag ++ " hammer") (sameGs (stepState hm) (fst (useHammer (4, 4) s0)))
    assertEqual (tag ++ " hammer events") (traceEvents (traceHammer (4, 4) s0)) (stepEvents hm)
    let cr = gameStep g s0 (M3E.CrossClear (3, 3))
    assertBool (tag ++ " cross") (sameGs (stepState cr) (fst (useCrossClear (3, 3) s0)))
    let bad = gameStep g s0 (M3E.Swap (0, 0) (5, 5))
    assertEqual (tag ++ " rejected") (False, []) (stepAccepted bad, stepEvents bad)
    assertEqual (tag ++ " rejected outcome") (Just InvalidSwap) (M3E.pdOutcome (M3E.play (M3E.Swap (0, 0) (5, 5)) s0))
    assertEqual (tag ++ " undo at start rejected") False (stepAccepted (gameStep M3E.match3Shell (startHistory s0) Undo))
    let sh = gameStep g s0 M3E.Shuffle
    assertBool (tag ++ " shuffle") (sameGs (stepState sh) (shuffleGame s0))
    assertEqual (tag ++ " shuffle event") ["shuffle"] (map efKind (stepEffects g sh))
    assertEqual (tag ++ " hint") (snd (applyHint s0)) (M3E.pdHint (M3E.play M3E.Hint s0))
    assertEqual (tag ++ " status combo") (Just (gsCombo s0)) (lookup "combo" (gameStatus g s0))
  -- 结局：只剩 1 步时第一步之后即停
  let s1 = (gameNew g (M3E.Campaign 0) 3) {gsMoves = 1}
      acts1 = take 2 (gameActions g s1)
      run1 = runActions g s1 (acts1 ++ acts1)
  assertEqual "stops after terminal" 1 (length run1)
  assertBool "terminal outcome" (isJust (stepOutcome (last run1)))
  assertEqual "terminal: no actions" 0 (length (gameActions g (stepState (last run1))))
  assertEqual "terminal: step rejected" [False] [stepAccepted (gameStep g (stepState (last run1)) a) | a <- take 1 acts1]
  where
    forM_' xs f = mapM_ f xs

--------------------------------------------------------------------------------
-- 段 3：撤销走通用接口（历史在 Engine.History）

-- | 终局后撤销：经 match3Shell 的 gameStep 走到终局再连撤三次，每一步的状态投影与 13094d1 上直接调
-- Match3.Engine.play（当时的 Undo 动作 + GameState.gsHistory）逐位相同。期望值由 13094d1 上的同一段投影生成
-- （生成程序与本测试的 legacyProj 逐字相同，只把历史深度换成 length gsHistory）。
-- 场景：(关卡下标, 种子, 步数, 是否把目标改成 1 分)——前四个判负、后两个过关；走法取 findHint。
engine_undo_after_terminal_matches_legacy_play :: Assertion
engine_undo_after_terminal_matches_legacy_play =
  mapM_ one expected
  where
    g = M3E.match3Shell
    one (sc@(li, seed, mv, easy), over, rows) = do
      let base = (newGameAtLevel li (levelConfig (allLevels !! li)) seed) {gsMoves = mv}
          s0 = startHistory (if easy then base {gsGoal = GoalScore 1} else base)
          go h
            | isJust (gsOver (histNow h)) = h
            | otherwise = case findHint (gsBoard (histNow h)) of
                Just (p, q) ->
                  let st = gameStep g h (Act (M3E.Swap p q))
                  in if stepAccepted st then go (stepState st) else h
                Nothing -> h
          hT = go s0
          u1 = gameStep g hT Undo
          u2 = gameStep g (stepState u1) Undo
          u3 = gameStep g (stepState u2) Undo
          got = (True, legacyProj hT) : [(stepAccepted u, legacyProj (stepState u)) | u <- [u1, u2, u3]]
          tag = show sc
      assertEqual (tag ++ " terminal outcome") over (gsOver (histNow hT))
      assertEqual (tag ++ " undo after terminal accepted") True (stepAccepted u1)
      assertEqual (tag ++ " undo clears outcome") Nothing (gsOver (histNow (stepState u1)))
      assertEqual (tag ++ " matches 13094d1 play") rows got
    expected :: [((Int, Int, Int, Bool), Maybe Outcome, [(Bool, String)])]
    expected =
      [ ((0,3,2,False), Just (Lost 60), [(True,"40356bf74a743c8a"),(True,"d268eecfca70bd1d"),(True,"5ab25fef8fe39d36"),(False,"5ab25fef8fe39d36")])
      , ((6,1,3,False), Just (Lost 150), [(True,"58c5c9b29d6dc90e"),(True,"f4821e13f8413159"),(True,"8fd1ffda3978ffd"),(True,"163a90e589b8877a")])
      , ((12,2,2,False), Just (Lost 100), [(True,"444f5e63d84b1509"),(True,"509df4883823f965"),(True,"d4df5580278e281"),(False,"d4df5580278e281")])
      , ((27,1,2,False), Just (Lost 190), [(True,"ef3584929c694a4c"),(True,"d7ecea137581da25"),(True,"7a2c7232ad3e8763"),(False,"7a2c7232ad3e8763")])
      , ((4,1,1,True), Just (LevelClear 30 5), [(True,"6c3fab5bd2d46e0c"),(True,"5acbcdbf9831cb2"),(False,"5acbcdbf9831cb2"),(False,"5acbcdbf9831cb2")])
      , ((20,2,3,True), Just (LevelClear 30 21), [(True,"b9020797ba2d56c2"),(True,"3af9cee4e045dd8d"),(False,"3af9cee4e045dd8d"),(False,"3af9cee4e045dd8d")])
      ]

-- | 与 13094d1 共有字段的状态投影（FNV-1a 64）；最后一项是历史深度（旧：length gsHistory，新：historyDepth）。
legacyProj :: History GameState -> String
legacyProj h =
  let gs = histNow h
  in fnv (unlines
       [ show (gsBoard gs), show (gsScore gs), show (gsMoves gs), show (gsGoal gs), show (gsCollected gs), show (gsColorBag gs)
       , show (gsCount CountStones gs, gsCount CountChests gs, gsCount CountHoney gs, gsCount CountBalloons gs, gsCount CountCookies gs, gsCount CountCakes gs, gsCount CountSafes gs)
       , show (gsGen gs), show (gsOver gs), show (gsLevel gs), show (gsHint gs), show (gsCombo gs), show (gsShuffled gs)
       , show (gsBelts gs), show (gsPortals gs), show (gsHammers gs, gsFreeSwaps gs, gsCrossClears gs), show (gsUfos gs), show (gsCount CountUfo gs)
       , show (gsCarpetOpen gs), show (gsCount CountCarpets gs), show (gsLastCleared gs), show (gsDaily gs), show (namedCounts (gsCounts gs)), show (historyDepth h) ])
  where
    fnv s = showHex (foldl' (\acc c -> (acc `xor` fromIntegral (ord c)) * 1099511628211) (14695981039346656037 :: Word64) s) ""

-- | 前端（app/ 下全部 .hs）不再直接调用三消的 play / playWith，也没有 undoMove：动作一律经通用接口
-- gameStep（UI.Actions.stepShell → Match3.Engine.match3Shell），撤销由 Engine.History 处理。
-- 扫描去掉注释与字符串后的标识符（含限定名 M3E.play）；同时确认前端确实经 match3Shell 的 gameStep。
engine_frontend_steps_only_via_gameStep :: Assertion
engine_frontend_steps_only_via_gameStep = do
  files <- sourcesUnder "app"
  assertBool "scanned the whole front end" (length files >= 20)
  srcs <- mapM readFile files
  let banned = ["play", "playWith", "undoMove"]
      bad =
        [ f ++ ": " ++ w
        | (f, src) <- zip files srcs
        , w <- banned
        , mentionsIdent w src
        ]
      uses = [f | (f, src) <- zip files srcs, any ("gameStep M3E.match3Shell" `isPrefixOf`) (tailsS src)]
  assertEqual "no direct play / playWith / undoMove in app/" [] bad
  assertBool "front end steps through match3Shell's gameStep" (not (null uses))
  where
    tailsS [] = [[]]
    tailsS xs@(_ : r) = xs : tailsS r
