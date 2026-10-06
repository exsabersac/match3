{-# LANGUAGE OverloadedStrings #-}
-- | 行为金标准（golden）：固定种子下「关卡 × 种子 × 逐步推进」以及道具、撤销、洗牌和几个手工局面的
-- 规则结果，投影成稳定的文本，一个用例一行，行首是「关卡 / 种子 / 第几步」。
--
-- 用途：重构（第二刀起）前后逐字比对，证明玩家可见的规则行为没有变化。
-- 约束（重要）：
--   * 入库时（4fbcefc）取数只经过门面 Match3.Board / Match3.Game，那一版代码在 3bd26d8 与 5eef3e3 上
--     都能原样编译、两边输出全等。第三刀删除了这两个门面，本文件改为直接 import Board.* / Game.*
--     子模块与记录版连锁 CascadeRun，因此**今后不能再原样在 51b1cfa 及更早的提交上编译**；
--     要和旧提交比对，取 4fbcefc 版的 Golden.hs 到旧提交上跑（输出与 golden.txt 逐字相同）；
--   * 不对内部类型直接调用 show，全部用下面手写的投影函数（格子短码、具名计数）；
--     唯一例外是 StdGen 的 show（随机数状态）；
--   * 回放脚本 mtWaves / mtEnd 投影成文本后用手写 FNV-1a 64 压缩，不引入新依赖；
--   * 以后内部表示变了，只改这里的投影函数，golden.txt 一个字都不动。
--
-- 重新生成（只在确认行为「应该」变化时，机制刀停期间不应发生）：见 test/golden/regen.sh。
module Golden
  ( goldenLines
  , goldenSections
  , main
  ) where

import Match3.Board.Default (cascadeMatches, cascadeSeeds, findHint, builtinHooks, hookLevel)
import Match3.Element.Level (levelUfos)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Match3.Board.Cascade (CascadeRun(..), CascadeTally(..), CascadeWave(..))
import Match3.Counts (CounterKey(..), colorBag, countOf, namedCounts)
import Match3.Board.Grid (getCell, setCell, MBoard, mboardRows)
import Match3.Board.Random (randomBoard, randomPlayableBoard)
import Engine.Game (Game(..), Step(..))
import Engine.History (History(..), Undoable(..), historyDepth, pushHistory, replaceNow, startHistory, undoHistory)
import Match3.Element.Builtin (defaultRegistry)
import Match3.ECS.Registry (Registry, inertDef, register)
import qualified Match3.Engine as M3E
import Match3.Game.Boosters
import Match3.Game.Level
import Match3.Game.Move
import Match3.Game.Outcome
import Match3.Game.Shuffle
import Match3.Game.State
import Match3.Game.Trace
import Match3.Levels.Campaign (allLevels, lookupLevel)
import Match3.Levels.Level (Level, levelConfig)
import Match3.Types
import Match3.Ufo (Ufo(..), mkUfo)
import Numeric (showHex)
import System.Random (StdGen, mkStdGen)

-- | 生成器入口：ghc -main-is Golden（见 regen.sh）。
-- | 战役第 1 关（关卡表为空时直接报错）。
firstLevel :: Level
firstLevel = fromMaybe (error "firstLevel: allLevels is empty") (lookupLevel 0)

-- | 第 li 关（0 基）按该关步数与目标、给定种子开局；没有这一关直接报错。
levelGame :: Int -> Int -> GameState
levelGame li seed = fromMaybe (error ("levelGame: no level " ++ show li)) (campaignGame li seed)

main :: IO ()
main = mapM_ putStrLn goldenLines

--------------------------------------------------------------------------------
-- 压缩：FNV-1a 64（按 Char 的码点逐个异或后乘素数）

fnv1a :: String -> String
fnv1a s =
  let h = foldl' (\acc ch -> (acc `xor` fromIntegral (ord ch)) * 1099511628211) (14695981039346656037 :: Word64) s
      hx = showHex h ""
  in replicate (16 - length hx) '0' ++ hx

--------------------------------------------------------------------------------
-- 投影：格子短码 / 盘面 / 状态 / 回放

pColor :: Color -> String
pColor c = show (fromEnum c + 1)

pKind :: GemKind -> String
pKind k = case k of
  Normal -> "n"
  LineH -> "h"
  LineV -> "v"
  Bomb -> "b"
  Rainbow -> "r"

pOverlay :: Maybe CellOverlay -> String
pOverlay o = case o of
  Nothing -> ""
  Just Grass -> "+G"
  Just Vine -> "+V"
  Just Choco -> "+C"
  Just (Fog n) -> "+F" ++ show n
  Just (Chain n) -> "+K" ++ show n
  Just (Freeze n) -> "+Z" ++ show n
  Just (Curtain n) -> "+U" ++ show n
  Just Steam -> "+S"

-- | 一格的固定短码：占格种类 + 颜色 / 层数 / 冰层 / 叠层。
pCell :: Cell -> String
pCell cell = case cell of
  Gem c k ice ov -> pColor c ++ pKind k ++ (if ice > 0 then "i" ++ show ice else "") ++ pOverlay ov
  Stone n -> "S" ++ show n
  Chest n -> "X" ++ show n
  Honey n -> "Y" ++ show n
  Balloon c -> "B" ++ pColor c
  Cookie -> "k"
  Cake n -> "c" ++ show n
  MagicHat -> "M"
  Maker c n -> "J" ++ pColor c ++ ":" ++ show n
  Snail dr dc -> "N" ++ show dr ++ ":" ++ show dc
  Safe n -> "V" ++ show n
  Flip f b -> "F" ++ pColor f ++ pColor b
  Surprise -> "?"
  Bottle c -> "D" ++ pColor c
  TimeSpirit -> "T"
  Countdown c n -> "@" ++ pColor c ++ ":" ++ show n
  Custom n v -> "E" ++ unElementName n ++ ":" ++ show v

pBoard :: Board -> String
pBoard = intercalate "/" . map (intercalate "," . map pCell) . boardRows

pHoles :: MBoard -> String
pHoles = intercalate "/" . map (intercalate "," . map (maybe "_" pCell)) . mboardRows

pPos :: Pos -> String
pPos (r, c) = show r ++ show c

pPosList :: [Pos] -> String
pPosList ps = "[" ++ unwords (map pPos ps) ++ "]"

pOutcome :: Outcome -> String
pOutcome o = case o of
  InvalidSwap -> "I"
  NoMatch -> "N"
  MoveApplied s -> "M" ++ show s
  Won s -> "W" ++ show s
  Lost s -> "L" ++ show s
  LevelClear s n -> "C" ++ show s ++ ">" ++ show n

pGoal :: LevelGoal -> String
pGoal g = case goalView g of
  ViewScore t -> "score" ++ show t
  ViewCollect c n -> "collect" ++ pColor c ++ ":" ++ show n
  ViewCollectMulti rs -> "multi" ++ concat [pColor c ++ ":" ++ show n ++ ";" | (c, n) <- rs]
  ViewCount k n -> case k of
    CountStones -> "stone" ++ show n
    CountChests -> "chest" ++ show n
    CountHoney -> "honey" ++ show n
    CountBalloons -> "balloon" ++ show n
    CountCookies -> "cookie" ++ show n
    CountCakes -> "cake" ++ show n
    CountSafes -> "safe" ++ show n
    CountUfo -> "ufo" ++ show n
    CountCarpets -> "carpet" ++ show n
    CountNamed name -> "named" ++ unElementName name ++ ":" ++ show n
    _ -> show g
  ViewOther _ -> show g

pBag :: [(Color, Int)] -> String
pBag xs = concat [pColor c ++ ":" ++ show n ++ ";" | (c, n) <- xs]

pUfos :: [Ufo] -> String
pUfos us = "[" ++ unwords [pPos (ufoCell u) ++ "c" ++ pColor (ufoColor u) | u <- us] ++ "]"

pMaybePair :: Maybe (Pos, Pos) -> String
pMaybePair Nothing = "-"
pMaybePair (Just (a, b)) = pPos a ++ "-" ++ pPos b

-- | 状态里除盘面外的全部规则字段（具名）。段 3 起撤销历史不在 GameState 里，
-- hist= 取自同一局面在 Engine.History 里的历史深度（数值与原 length gsHistory 相同）。
pCounters :: History GameState -> String
pCounters h =
  let gs = histNow h
  in unwords
    [ "sc=" ++ show (gsScore gs)
    , "mv=" ++ show (gsMoves gs)
    , "goal=" ++ pGoal (gsGoal gs)
    , "col=" ++ show (gsCollected gs)
    , "bag=" ++ pBag (gsColorBag gs)
    , "stone=" ++ show (gsCount CountStones gs)
    , "chest=" ++ show (gsCount CountChests gs)
    , "honey=" ++ show (gsCount CountHoney gs)
    , "balloon=" ++ show (gsCount CountBalloons gs)
    , "cookie=" ++ show (gsCount CountCookies gs)
    , "cake=" ++ show (gsCount CountCakes gs)
    , "safe=" ++ show (gsCount CountSafes gs)
    , "ufoc=" ++ show (gsCount CountUfo gs)
    , "carpet=" ++ show (gsCount CountCarpets gs)
    , "copen=" ++ pPosList (gsCarpetOpen gs)
    , "over=" ++ maybe "-" (pOutcome . fromTerminal) (gsOver gs)
    , "lv=" ++ show (gsLevel gs)
    , "daily=" ++ show (gsDaily gs)
    , "hint=" ++ pMaybePair (gsHint gs)
    , "combo=" ++ show (gsCombo gs)
    , "shuf=" ++ show (gsShuffled gs)
    , "last=" ++ pPosList (gsLastCleared gs)
    , "boost=" ++ show (gsHammers gs) ++ ":" ++ show (gsFreeSwaps gs) ++ ":" ++ show (gsCrossClears gs)
    , "ufos=" ++ pUfos (gsUfos gs)
    , "belts=" ++ concatMap pPosList (gsBelts gs)
    , "portals=" ++ concat [pPos a ++ "~" ++ pPos b ++ ";" | (a, b) <- gsPortals gs]
    , "hist=" ++ show (historyDepth h)
    , "gen=" ++ show (gsGen gs)
    ]

-- | 完整状态（盘面压缩成哈希，计数明文）。
pState :: History GameState -> String
pState h = "{b#" ++ fnv1a (pBoard (gsBoard (histNow h))) ++ " " ++ pCounters h ++ "}"

-- | 状态整体压缩成一个哈希（辅助列用）。
hState :: History GameState -> String
hState h = fnv1a (pBoard (gsBoard (histNow h)) ++ "|" ++ pCounters h)

-- | 规则层一次走步的结果写进历史（段 3）：与 Engine.History.withHistory 同一规则——未终局时被接受的
-- 走步（交换 / 道具，resolveMove 真正结算）记快照，其余（被拒 / 已结束）只换当前状态。
recH :: History GameState -> (GameState, Outcome) -> History GameState
recH h (g, o)
  | applied o && gsOver (histNow h) == Nothing = pushHistory M3E.match3History h g
  | otherwise = replaceNow h g

-- | 同一历史换一个当前状态（提示 / 洗牌 / 自动洗牌 / 结局判定这类不记快照的操作）。
nowH :: History GameState -> GameState -> History GameState
nowH = replaceNow

pWave :: CascadeWave -> String
pWave w =
  intercalate "|"
    [ pBoard (cwBefore w)
    , pPosList (cwCleared w)
    , pPosList (cwDrained w)
    , pHoles (cwHoles w)
    , pBoard (cwAfter w)
    , show (cwScore w)
    ]

-- 第 7 刀 7b：EndEffect 改为通用形状（事件类型 + 元素名 + 逐项 EndItem），这里按事件类型手写成与之前逐字相同的文本。
pEffect :: EndEffect -> String
pEffect e = case endEffectKind e of
  EvTick -> "tick" ++ pPosList (map eiTo items)
  EvBelt -> "belt" ++ pairsText
  EvSpread -> "spread" ++ pSpread (endEffectElement e) ++ pairsText
  EvMove -> "snail" ++ concat [pPos (eiFrom m) ++ ">" ++ pPos (eiTo m) ++ pDir (endItemDir m) ++ "p" ++ maybe "-" pCell (eiBack m) ++ ";" | m <- items]
  k -> show k ++ pairsText
  where
    items = endEffectItems e
    pairsText = concat [pPos a ++ ">" ++ pPos b ++ ";" | (a, b) <- endEffectPairs e]
    pDir d = case d of
      Just (dr, dc) -> "d" ++ show dr ++ ":" ++ show dc
      Nothing -> "d?"
    pSpread n = case n of
      "vine" -> "V"
      "choco" -> "C"
      "steam" -> "S"
      _ -> unElementName n

pEnd :: EndStep -> String
pEnd s = intercalate "|" [show (esAfterWaves s), pBoard (esBefore s), pBoard (esAfter s), pEffect (esEffect s)]

-- | 回放脚本：轮数 / 步末数明文，全文压缩成哈希。
pTrace :: MoveTrace -> String
pTrace mt =
  "w" ++ show (length (mtWaves mt)) ++ "e" ++ show (length (mtEnd mt)) ++ "#"
    ++ fnv1a (intercalate "\n" ([pBoard (mtStart mt)] ++ map pWave (mtWaves mt) ++ ["--"] ++ map pEnd (mtEnd mt) ++ [pBoard (mtFinal mt)]))

pFx :: MoveFx -> String
pFx fx = show (fxCombo fx) ++ pPosList (fxCleared fx)

--------------------------------------------------------------------------------
-- 用例

allPairs :: [(Pos, Pos)]
allPairs =
  [ ((r, c), p2)
  | r <- [0 .. boardSize - 1]
  , c <- [0 .. boardSize - 1]
  , p2 <- [(r, c + 1), (r + 1, c)]
  , fst p2 >= 0 && fst p2 < boardSize && snd p2 >= 0 && snd p2 < boardSize
  ]

applied :: Outcome -> Bool
applied o = o /= NoMatch && o /= InvalidSwap

-- | 一步：从全部可成交的相邻交换里按固定公式挑一手；没有可走步时强制洗牌。
-- 每 3 步额外记三种道具（有次数 / 无次数），每步都记撤销 / 提示 / 洗牌 / 自动洗牌 / 被拒交换。
stepLines :: String -> Int -> History GameState -> ([String], Maybe (History GameState))
stepLines tag i h0 =
  let gs0 = histNow h0
      valid = [ (p1, p2, r) | (p1, p2) <- allPairs, let r@(_, o) = trySwap p1 p2 gs0, applied o ]
      pre = tag ++ " #" ++ pad2 i
  in case valid of
       [] ->
         let h1 = nowH h0 (shuffleGame gs0)
         in ([pre ++ " shuffle " ++ pState h1], if gsOver (histNow h1) == Nothing then Just h1 else Nothing)
       _ ->
         let (p1, p2, (gs1, o)) = valid !! ((i * 7 + 3) `mod` length valid)
             h1 = recH h0 (gs1, o)
             mainL =
               unwords
                 [ pre, "swap", pPos p1 ++ "-" ++ pPos p2, "out=" ++ pOutcome o
                 , "st=" ++ pState h1
                 , "fx=" ++ pFx (moveFx gs0 gs1 o)
                 , "tr=" ++ pTrace (traceSwap p1 p2 gs0)
                 , "nvalid=" ++ show (length valid)
                 ]
             auxL =
               unwords
                 [ pre, "aux"
                 , "undo=" ++ maybe "-" hState (undoHistory M3E.match3History h1)
                 , "hint=" ++ (let (g, h) = applyHint gs1 in hState (nowH h1 g) ++ ":" ++ pMaybePair h)
                 , "shuf=" ++ hState (nowH h1 (shuffleGame gs1))
                 , "ens=" ++ hState (nowH h1 (ensurePlayable gs1))
                 , "chk=" ++ pOutcome (checkOutcome gs1)
                 , "rej=" ++ (let r@(_, o') = trySwap (0, 0) (5, 5) gs1 in hState (recH h1 r) ++ pOutcome o')
                 , "trej=" ++ pTrace (traceSwap (0, 0) (0, 0) gs1)
                 ]
             boostL
               | i `mod` 3 /= 0 = []
               | otherwise =
                   let hp = (i `mod` boardSize, (i * 3) `mod` boardSize)
                       fp1 = (i `mod` boardSize, 0)
                       fp2 = ((i + 3) `mod` boardSize, 5)
                       gsB = gs0 {gsHammers = 3, gsCrossClears = 3, gsFreeSwaps = 3}
                       hB = nowH h0 gsB
                       one name r@(g, oo) mt = name ++ "=" ++ pOutcome oo ++ ":" ++ hState (recH hB r) ++ ":" ++ pFx (moveFx gsB g oo) ++ ":" ++ pTrace mt
                       none name r@(_, oo) = name ++ "0=" ++ pOutcome oo ++ ":" ++ hState (recH h0 r)
                   in [ unwords
                          [ pre, "boost"
                          , none "ham" (useHammer hp gs0), none "crs" (useCrossClear hp gs0), none "fsw" (useFreeSwap fp1 fp2 gs0)
                          , one "ham" (useHammer hp gsB) (traceHammer hp gsB)
                          , one "crs" (useCrossClear hp gsB) (traceCrossClear hp gsB)
                          , one "fsw" (useFreeSwap fp1 fp2 gsB) (traceFreeSwap fp1 fp2 gsB)
                          ]
                      ]
         in (mainL : auxL : boostL, if gsOver gs1 == Nothing then Just h1 else Nothing)

pad2 :: Int -> String
pad2 n = if n < 10 then '0' : show n else show n

runGame :: String -> GameState -> Int -> [String]
runGame tag gs0 n = (tag ++ " start " ++ pState (startHistory gs0) ++ " board=" ++ pBoard (gsBoard gs0)) : go 0 (startHistory gs0)
  where
    go i gs
      | i >= n = []
      | otherwise =
          let (ls, next) = stepLines tag i gs
          in ls ++ maybe [tag ++ " #" ++ pad2 i ++ " end"] (go (i + 1)) next

-- | 前 2344 行覆盖的关卡数：段 5 起关卡表在其后追加（第 39 / 40 关），原有各段只跑前 38 关，
-- 新关卡的行统一追加在文件末尾（seg5Lines / seg6Blocks），原有行逐字不变。
campaign38 :: Int
campaign38 = 38

-- | 全部行：38 关 × 种子 {1,2} × 15 步、2 个每日式开局、手工局面、连锁 API、开局，最后是 H4–H6（段 1 追加）。
goldenLines :: [String]
goldenLines = concat goldenSections

-- | 金标准按「互不依赖的一段」切开（Haskell 特性第 5 项）：concat 起来就是 goldenLines，逐行顺序不变。
-- 每段只依赖固定的关卡 / 种子，彼此不共享状态，所以可以按任意顺序、在任意线程上求值
-- （Spec.Golden 用 Spec.Support.Parallel 并行求值后按原顺序拼回）。第 5 项前 goldenLines 直接是下面各段的 (++)。
goldenSections :: [[String]]
goldenSections =
  [ runGame ("L" ++ pad2 (li + 1) ++ " s" ++ show seed) (levelGame li seed) 15
  | li <- [0 .. campaign38 - 1]
  , seed <- [1, 2 :: Int]
  ]
    ++ [ runGame ("D" ++ show seed) (newDailyGame (GameConfig 20 (goalScore 900)) seed) 10
       | seed <- [20260929, 20260101 :: Int]
       ]
    ++ [handmade, cascadeLines, levelLines, handmade2, seg5Lines]
    ++ seg6Blocks

-- | 手工局面（护栏测试用到的几个）：蜗牛撞墙 / 推格、巧克力关无匹配交换、首个 3 连锁。
handmade :: [String]
handmade =
  let base = newGame defaultConfig 7
      snailGs = base {gsBoard = setCell (setCell (gsBoard base) (0, 0) (Snail 0 (-1))) (3, 3) (Snail 0 1)}
      chocoGs = levelGame 4 1
      comboGs =
        case [ gs0 | seed <- [1 .. 400 :: Int], let gs0 = newGameAtLevel 0 (levelConfig firstLevel) seed
             , Just (p1, p2) <- [findHint (gsBoard gs0)], let (gs1, out) = trySwap p1 p2 gs0, out /= NoMatch, gsCombo gs1 >= 3 ] of
          (g : _) -> g
          [] -> base
      allSwaps = allSwapsOf
  in runGame "H1-snail" snailGs 8 ++ allSwaps "H1-snail" snailGs
       ++ runGame "H2-choco" chocoGs 8 ++ allSwaps "H2-choco" chocoGs
       ++ runGame "H3-combo" comboGs 8 ++ allSwaps "H3-combo" comboGs

-- | 某局面的全部可成交交换（每个一行：结局、状态、回放）。
allSwapsOf :: String -> GameState -> [String]
allSwapsOf tag gs =
  [ unwords [tag, "pair", pPos p1 ++ "-" ++ pPos p2, "out=" ++ pOutcome o, "st=" ++ pState (recH (startHistory gs) r), "tr=" ++ pTrace (traceSwap p1 p2 gs)]
  | (p1, p2) <- allPairs
  , let r@(_, o) = trySwap p1 p2 gs
  , applied o
  ]

-- | 连锁 API（Match3.Board.Cascade 记录版）：随机盘 × 飞碟 / 传送门的普通连锁与种子连锁，结算与回放各一份。
cascadeLines :: [String]
cascadeLines =
  [ unwords
      [ "C" ++ pad2 seed ++ "/" ++ show k, "run=" ++ runP, "trace=" ++ trP ]
  | seed <- [1 .. 30 :: Int]
  , (k, (ufos, portals)) <- zip [0 :: Int ..] [([], []), ([mkUfo (2, 3) C1], []), ([], [((0, 1), (7, 6)), ((0, 6), (7, 1))])]
  , let (b0, g1) = randomBoard (mkStdGen seed)
        run = cascadeMatches Nothing (builtinHooks ufos portals) g1 b0
        runP = pRun run
        trP = pTr run
  ]
    ++ [ unwords [ "S" ++ pad2 seed ++ "/" ++ show k, "run=" ++ pRun run, "trace=" ++ pTr run ]
       | seed <- [1 .. 20 :: Int]
       , (k, (seeds, ufos)) <- zip [0 :: Int ..] [([(3, 3)], []), ([(r, 4) | r <- [0 .. 7]], [mkUfo (1, 1) C2]), ([(2, c) | c <- [0 .. 7]] ++ [(r, 2) | r <- [0 .. 7]], [])]
       , let (b0, g1) = randomPlayableBoard (mkStdGen seed)
             run = cascadeSeeds Nothing seeds (builtinHooks ufos []) g1 b0
       ]
  where
    -- 结算投影（原 15 元组的字段顺序）与回放投影（原 (轮次, 终盘, 飞碟, 生成器)）都从同一个 CascadeRun 取。
    pRun r =
      let CascadeTally {ctCells = cells, ctScore = score, ctMaxWave = maxW, ctCounts = cnts, ctCleared = cleared} = crTally r
          c k = countOf k cnts
          (stones, chests, honey, balloons) = (c CountStones, c CountChests, c CountHoney, c CountBalloons)
          (cookies, cakes, uAbs) = (c CountCookies, c CountCakes, c CountUfo)
          (b, ufos', g) = (crBoard r, levelUfos (hookLevel (crHooks r)), crGen r)
      in unwords
        [ "b#" ++ fnv1a (pBoard b), "cells=" ++ show cells, "score=" ++ show score, "maxw=" ++ show maxW, "bag=" ++ pBag (colorBag cnts)
        , "stone=" ++ show stones, "chest=" ++ show chests, "honey=" ++ show honey, "balloon=" ++ show balloons
        , "cookie=" ++ show cookies, "cake=" ++ show cakes, "uabs=" ++ show uAbs, "ufos=" ++ pUfos ufos'
        , "cleared=" ++ pPosList cleared, "gen=" ++ show (g :: StdGen) ]
    pTr r =
      let (ws, b, ufos', g) = (crWaves r, crBoard r, levelUfos (hookLevel (crHooks r)), crGen r)
      in unwords
        [ "w" ++ show (length ws) ++ "#" ++ fnv1a (intercalate "\n" (map pWave ws)), "b#" ++ fnv1a (pBoard b), "ufos=" ++ pUfos ufos', "gen=" ++ show (g :: StdGen) ]

-- | 开局 / 重开 / 下一关（关卡装饰与随机数消费顺序）。
levelLines :: [String]
levelLines =
  [ "G" ++ pad2 (li + 1) ++ " s" ++ show s ++ " new " ++ pState (startHistory gs) ++ " board=" ++ pBoard (gsBoard gs)
  | li <- [0 .. campaign38 - 1]
  , s <- [0, 5, 99 :: Int]
  , let gs = levelGame li s
  ]
    ++ [ "R" ++ pad2 (li + 1) ++ " s" ++ show s ++ " restart " ++ pState (startHistory (restartLevel (levelGame li 1) s))
       | li <- [0, 10, 27, 35], s <- [4, 8 :: Int] ]
    ++ [ "X" ++ pad2 (li + 1) ++ " next " ++ pState (startHistory (nextLevel (levelGame li 1) 4))
       | li <- [0, 10, 27] ]

--------------------------------------------------------------------------------
-- 手工局面第二批 H4–H6（段 1 追加在 golden.txt 末尾，前 2186 行不动）。
-- 生成方式：本段代码与 4fbcefc 版 Golden.hs 上的同一段（只换取数入口）分别在 13094d1 与 31275da 上运行，
-- 两边逐行全等后入库。三个局面的选型理由见 docs/testing.md「行为金标准」。

-- | 把若干格写进盘面。
setMany :: Board -> [(Pos, Cell)] -> Board
setMany = foldl (\b (p, cell) -> setCell b p cell)

-- | 给某格的宝石换上叠层 / 冰层（保留原颜色与种类；非宝石格换成该色普通宝石）。
layered :: Board -> Pos -> Color -> Int -> Maybe CellOverlay -> (Pos, Cell)
layered b p fallback ice ov = case getCell b p of
  Gem c k _ _ -> (p, Gem c k ice ov)
  _ -> (p, Gem fallback Normal ice ov)

-- | 长局：不因分数 / 步数提前结束。
longRun :: GameState -> GameState
longRun gs = gs {gsGoal = goalScore 100000, gsMoves = 30}

-- | H4：步末全阶段 + 蔓延紧贴本步消除留下的空洞（第 28 关：皮带、传送门、地毯；再放藤 / 巧 / 蒸汽、
-- 蜗牛、将归零的倒计时和饼干）。锁住「步末之后是否还要补结算」这一处的现有行为。
h4Gs :: GameState
h4Gs =
  let base = longRun (levelGame 27 3)
      b0 = gsBoard base
      cells =
        [ layered b0 (5, 1) C1 0 (Just Vine), layered b0 (5, 2) C2 0 (Just Vine)
        , layered b0 (6, 5) C3 0 (Just Choco), layered b0 (6, 6) C4 0 (Just Choco)
        , layered b0 (4, 6) C5 0 (Just Steam), layered b0 (2, 1) C1 0 (Just Steam)
        , ((3, 0), Snail 0 1), ((5, 7), Snail (-1) 0)
        , ((2, 6), Countdown C3 2), ((6, 2), Countdown C2 1)
        , ((5, 4), Cookie), ((2, 3), Cookie)
        ]
  in base {gsBoard = setMany b0 cells}

-- | H5：多种叠层同格（冰 × 锁链 / 草 / 迷雾 / 冰冻 / 窗帘 / 藤 / 巧 / 蒸汽，含特殊块），压在第 37 关的地毯上。
h5Gs :: GameState
h5Gs =
  let base = longRun (levelGame 36 3)
      b0 = gsBoard base
      cells =
        [ layered b0 (3, 2) C1 2 (Just (Chain 1)), layered b0 (3, 3) C2 1 (Just Grass)
        , layered b0 (3, 4) C3 1 (Just (Fog 2)), layered b0 (3, 5) C4 2 (Just (Freeze 1))
        , layered b0 (4, 2) C5 1 (Just (Curtain 2)), layered b0 (4, 3) C1 1 (Just Vine)
        , layered b0 (4, 4) C2 2 (Just Choco), layered b0 (4, 5) C3 1 (Just Steam)
        , layered b0 (2, 3) C4 3 (Just (Chain 2)), layered b0 (5, 3) C5 1 (Just Grass)
        , ((2, 4), Gem C1 LineH 1 (Just (Fog 1))), ((5, 4), Gem C2 Bomb 2 (Just (Chain 1)))
        , ((2, 2), Gem C3 LineV 1 (Just Grass)), ((5, 5), Gem C4 Normal 1 (Just (Curtain 1)))
        ]
  in base {gsBoard = setMany b0 cells}

-- | H6 的开局：第 28 关再放直线 / 炸弹 / 冰宝石 / 石头（洗牌必须原样保留的格）。
h6Gs :: GameState
h6Gs =
  let base = longRun (levelGame 27 5)
      b0 = gsBoard base
      cells =
        [ ((2, 2), Gem C1 LineH 0 Nothing), ((5, 5), Gem C2 Bomb 0 Nothing)
        , ((6, 1), Gem C3 Normal 1 Nothing), ((3, 6), Gem C4 LineV 0 (Just Grass)), ((0, 0), Stone 2)
        ]
  in base {gsBoard = setMany b0 cells}

-- | H6 的死局：斜纹 5 色 (r + c) mod 5（无可走步、无现成匹配）加一块石头，自动洗牌必须触发。
h6Dead :: GameState
h6Dead =
  let base = newGame defaultConfig 11
      stripes = boardFromRows [[mkGem (toEnum ((r + c) `mod` 5)) | c <- [0 .. boardSize - 1]] | r <- [0 .. boardSize - 1]]
  in base {gsBoard = setCell stripes (3, 3) (Stone 2)}

-- | H6 用的自定义元素世界：内置表 + 一个测试专用的惰性元素（不在盘面上）。
-- 经 match3GameWith 的洗牌 / 交换动作走元素世界路径，结果必须与内置表逐字相同。
h6Reg :: Registry
h6Reg = register (inertDef "golden_probe") defaultRegistry

-- | H6：洗牌 + 自定义元素世界。每步先手动洗牌（Shuffle 动作），再按固定公式挑一手成交的交换；
-- 最后记死局的自动洗牌与手动洗牌。
h6Lines :: [String]
h6Lines =
  let g = M3E.match3ShellWith h6Reg
      shuf s = stepState (gameStep g s (Act M3E.Shuffle))
      swapTo s p1 p2 =
        let st = gameStep g s (Act (M3E.Swap p1 p2))
        in if stepAccepted st then Just (stepState st) else Nothing
  in h6Run shuf swapTo (\h -> nowH h (ensurePlayableWith h6Reg (histNow h)))

-- | H6 的推进与投影（两边共用的部分；只有三种操作的取数入口不同）。
-- 段 3 起状态是 History GameState（经 match3ShellWith 的通用历史层；洗牌不记快照、交换记快照）。
h6Run :: (History GameState -> History GameState) -> (History GameState -> Pos -> Pos -> Maybe (History GameState)) -> (History GameState -> History GameState) -> [String]
h6Run shuf swapTo ens =
  ("H6-shuffle start " ++ pState (startHistory h6Gs) ++ " board=" ++ pBoard (gsBoard h6Gs)) : go 0 (startHistory h6Gs) ++ deadLines
  where
    tag = "H6-shuffle"
    dead = startHistory h6Dead
    go i gs
      | i >= (8 :: Int) = []
      | otherwise =
          let pre = tag ++ " #" ++ pad2 i
              s1 = shuf gs
              l1 = pre ++ " shuf st=" ++ pState s1 ++ " board=" ++ pBoard (gsBoard (histNow s1))
              valid = [(p1, p2, g) | (p1, p2) <- allPairs, Just g <- [swapTo s1 p1 p2]]
          in case valid of
               [] -> [l1, pre ++ " stuck"]
               _ ->
                 let (p1, p2, g) = valid !! ((i * 7 + 3) `mod` length valid)
                     l2 = unwords [pre, "swap", pPos p1 ++ "-" ++ pPos p2, "st=" ++ pState g, "nvalid=" ++ show (length valid)]
                 in l1 : l2 : (if gsOver (histNow g) == Nothing then go (i + 1) g else [pre ++ " end"])
    deadLines =
      [ tag ++ " dead " ++ pState dead ++ " board=" ++ pBoard (gsBoard h6Dead)
      , tag ++ " ens " ++ pState (ens dead) ++ " board=" ++ pBoard (gsBoard (histNow (ens dead)))
      , tag ++ " dshuf " ++ pState (shuf dead) ++ " board=" ++ pBoard (gsBoard (histNow (shuf dead)))
      ]

-- | 第二批全部行。
handmade2 :: [String]
handmade2 =
  runGame "H4-spread" h4Gs 10 ++ allSwapsOf "H4-spread" h4Gs
    ++ runGame "H5-layers" h5Gs 10 ++ allSwapsOf "H5-layers" h5Gs
    ++ h6Lines

--------------------------------------------------------------------------------
-- 段 5 追加（golden.txt 末尾，前 2344 行不动）：第 39 关（双层果冻）/ 第 40 关（气泡）。
-- 与 L01–L38 同一走法（stepLines）× 种子 {1,2} × 15 步；每步后多一行 ext：地面层与具名计数
-- （pState 不含这两个字段，为了不改旧行，只在新行里补）。再加开局行（同 levelLines 的 G 行格式）。

pExt :: History GameState -> String
pExt h = "gnd=" ++ show (gsGround (histNow h)) ++ " named=" ++ show (namedCounts (gsCounts (histNow h)))

runGame5 :: String -> GameState -> Int -> [String]
runGame5 tag gs0 n =
  (tag ++ " start " ++ pState h0 ++ " " ++ pExt h0 ++ " board=" ++ pBoard (gsBoard gs0)) : go 0 h0
  where
    h0 = startHistory gs0
    go i h
      | i >= n = []
      | otherwise =
          let (ls, next) = stepLines tag i h
          in ls ++ maybe [tag ++ " #" ++ pad2 i ++ " end"] (\h1 -> (tag ++ " #" ++ pad2 i ++ " ext " ++ pExt h1) : go (i + 1) h1) next

seg5Lines :: [String]
seg5Lines = levelBlock [campaign38 .. campaign40 - 1]

-- | 段 5 之后的 40 关原有关卡数（第 1–40 关）。
campaign40 :: Int
campaign40 = 40

-- | 新玩法关卡（第 41 关起，2026-09-30 解冻后追加）：同段 5 的投影，**每关一整块**依次追加在文件末尾
-- （先一关的逐步投影再它的开局），再加新关卡时前面的行不动。
-- 第 5 项起按关分块（goldenSections 里每关一段；第 5 项前是 seg6Lines = concatMap (levelBlock . pure) …）。
seg6Blocks :: [[String]]
seg6Blocks = map (levelBlock . pure) [campaign40 .. length allLevels - 1]

-- | 一批关卡的逐步投影（种子 1–2 × 15 步）与开局（种子 0 / 5 / 99）。
levelBlock :: [Int] -> [String]
levelBlock lis =
  concat
    [ runGame5 ("L" ++ pad2 (li + 1) ++ " s" ++ show seed) (levelGame li seed) 15
    | li <- lis
    , seed <- [1, 2 :: Int]
    ]
    ++ [ "G" ++ pad2 (li + 1) ++ " s" ++ show s ++ " new " ++ pState (startHistory gs) ++ " " ++ pExt (startHistory gs) ++ " board=" ++ pBoard (gsBoard gs)
       | li <- lis
       , s <- [0, 5, 99 :: Int]
       , let gs = levelGame li s
       ]
