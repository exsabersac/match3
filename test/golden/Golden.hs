-- | 行为金标准（golden）：固定种子下「关卡 × 种子 × 逐步推进」以及道具、撤销、洗牌和几个手工局面的
-- 规则结果，投影成稳定的文本，一个用例一行，行首是「关卡 / 种子 / 第几步」。
--
-- 用途：重构（第二刀起）前后逐字比对，证明玩家可见的规则行为没有变化。
-- 约束（重要）：
--   * 取数只经过门面 Match3.Board / Match3.Game（以及数据类型 Match3.Types / Match3.Ufo），
--     这份代码在 3bd26d8 与 5eef3e3 上都能原样编译，两边输出全等；
--   * 不对内部类型直接调用 show，全部用下面手写的投影函数（格子短码、具名计数）；
--     唯一例外是 StdGen 的 show（随机数状态）；
--   * 回放脚本 mtWaves / mtEnd 投影成文本后用手写 FNV-1a 64 压缩，不引入新依赖；
--   * 以后内部表示变了，只改这里的投影函数，golden.txt 一个字都不动。
--
-- 重新生成（只在确认行为「应该」变化时，机制刀停期间不应发生）：见 test/golden/regen.sh。
module Golden
  ( goldenLines
  , main
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl', intercalate)
import Data.Word (Word64)
import Match3.Board
  ( CascadeWave(..)
  , findHint
  , inBounds
  , randomBoard
  , randomPlayableBoard
  , runCascadeScoredFromSeedsWithUfos
  , runCascadeScoredWithUfos
  , setCell
  , traceCascade
  , traceCascadeFromSeeds
  )
import Match3.Game
import Match3.Types
import Match3.Ufo (Ufo(..), mkUfo)
import Numeric (showHex)
import System.Random (StdGen, mkStdGen)

-- | 生成器入口：ghc -main-is Golden（见 regen.sh）。
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
  Custom n v -> "E" ++ n ++ ":" ++ show v

pBoard :: Board -> String
pBoard = intercalate "/" . map (intercalate "," . map pCell)

pHoles :: [[Maybe Cell]] -> String
pHoles = intercalate "/" . map (intercalate "," . map (maybe "_" pCell))

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
pGoal g = case g of
  GoalScore t -> "score" ++ show t
  GoalCollect c n -> "collect" ++ pColor c ++ ":" ++ show n
  GoalCollectMulti rs -> "multi" ++ concat [pColor c ++ ":" ++ show n ++ ";" | (c, n) <- rs]
  GoalClearStone n -> "stone" ++ show n
  GoalChest n -> "chest" ++ show n
  GoalHoney n -> "honey" ++ show n
  GoalBalloon n -> "balloon" ++ show n
  GoalCookie n -> "cookie" ++ show n
  GoalCake n -> "cake" ++ show n
  GoalSafe n -> "safe" ++ show n
  GoalUfo n -> "ufo" ++ show n
  GoalCarpet n -> "carpet" ++ show n

pBag :: [(Color, Int)] -> String
pBag xs = concat [pColor c ++ ":" ++ show n ++ ";" | (c, n) <- xs]

pUfos :: [Ufo] -> String
pUfos us = "[" ++ unwords [pPos (ufoCell u) ++ "c" ++ pColor (ufoColor u) | u <- us] ++ "]"

pMaybePair :: Maybe (Pos, Pos) -> String
pMaybePair Nothing = "-"
pMaybePair (Just (a, b)) = pPos a ++ "-" ++ pPos b

-- | 状态里除盘面外的全部规则字段（具名）。
pCounters :: GameState -> String
pCounters gs =
  unwords
    [ "sc=" ++ show (gsScore gs)
    , "mv=" ++ show (gsMoves gs)
    , "goal=" ++ pGoal (gsGoal gs)
    , "col=" ++ show (gsCollected gs)
    , "bag=" ++ pBag (gsColorBag gs)
    , "stone=" ++ show (gsStonesCleared gs)
    , "chest=" ++ show (gsChestsCleared gs)
    , "honey=" ++ show (gsHoneyCleared gs)
    , "balloon=" ++ show (gsBalloonsPopped gs)
    , "cookie=" ++ show (gsCookiesCollected gs)
    , "cake=" ++ show (gsCakesCleared gs)
    , "safe=" ++ show (gsSafesOpened gs)
    , "ufoc=" ++ show (gsUfoCollected gs)
    , "carpet=" ++ show (gsCarpetsCovered gs)
    , "copen=" ++ pPosList (gsCarpetOpen gs)
    , "over=" ++ maybe "-" pOutcome (gsOver gs)
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
    , "hist=" ++ show (length (gsHistory gs))
    , "gen=" ++ show (gsGen gs)
    ]

-- | 完整状态（盘面压缩成哈希，计数明文）。
pState :: GameState -> String
pState gs = "{b#" ++ fnv1a (pBoard (gsBoard gs)) ++ " " ++ pCounters gs ++ "}"

-- | 状态整体压缩成一个哈希（辅助列用）。
hState :: GameState -> String
hState gs = fnv1a (pBoard (gsBoard gs) ++ "|" ++ pCounters gs)

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

pEffect :: EndEffect -> String
pEffect e = case e of
  EndCountdownTick ps -> "tick" ++ pPosList ps
  EndBeltShift mv -> "belt" ++ concat [pPos a ++ ">" ++ pPos b ++ ";" | (a, b) <- mv]
  EndSpread k ps -> "spread" ++ pSpread k ++ concat [pPos a ++ ">" ++ pPos b ++ ";" | (a, b) <- ps]
  EndSnail ms -> "snail" ++ concat [pPos (smFrom m) ++ ">" ++ pPos (smTo m) ++ "d" ++ show (fst (smDir m)) ++ ":" ++ show (snd (smDir m)) ++ "p" ++ maybe "-" pCell (smPushed m) ++ ";" | m <- ms]
  where
    pSpread k = case k of
      SpreadVine -> "V"
      SpreadChoco -> "C"
      SpreadSteam -> "S"

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
  , inBounds p2
  ]

applied :: Outcome -> Bool
applied o = o /= NoMatch && o /= InvalidSwap

-- | 一步：从全部可成交的相邻交换里按固定公式挑一手；没有可走步时强制洗牌。
-- 每 3 步额外记三种道具（有次数 / 无次数），每步都记撤销 / 提示 / 洗牌 / 自动洗牌 / 被拒交换。
stepLines :: String -> Int -> GameState -> ([String], Maybe GameState)
stepLines tag i gs0 =
  let valid = [ (p1, p2, r) | (p1, p2) <- allPairs, let r@(_, o) = trySwap p1 p2 gs0, applied o ]
      pre = tag ++ " #" ++ pad2 i
  in case valid of
       [] ->
         let gs1 = shuffleGame gs0
         in ([pre ++ " shuffle " ++ pState gs1], if gsOver gs1 == Nothing then Just gs1 else Nothing)
       _ ->
         let (p1, p2, (gs1, o)) = valid !! ((i * 7 + 3) `mod` length valid)
             mainL =
               unwords
                 [ pre, "swap", pPos p1 ++ "-" ++ pPos p2, "out=" ++ pOutcome o
                 , "st=" ++ pState gs1
                 , "fx=" ++ pFx (moveFx gs0 gs1 o)
                 , "tr=" ++ pTrace (traceSwap p1 p2 gs0)
                 , "nvalid=" ++ show (length valid)
                 ]
             auxL =
               unwords
                 [ pre, "aux"
                 , "undo=" ++ maybe "-" hState (undoMove gs1)
                 , "hint=" ++ (let (g, h) = applyHint gs1 in hState g ++ ":" ++ pMaybePair h)
                 , "shuf=" ++ hState (shuffleGame gs1)
                 , "ens=" ++ hState (ensurePlayable gs1)
                 , "chk=" ++ pOutcome (checkOutcome gs1)
                 , "rej=" ++ (let (g, o') = trySwap (0, 0) (5, 5) gs1 in hState g ++ pOutcome o')
                 , "trej=" ++ pTrace (traceSwap (0, 0) (0, 0) gs1)
                 ]
             boostL
               | i `mod` 3 /= 0 = []
               | otherwise =
                   let hp = (i `mod` boardSize, (i * 3) `mod` boardSize)
                       fp1 = (i `mod` boardSize, 0)
                       fp2 = ((i + 3) `mod` boardSize, 5)
                       gsB = gs0 {gsHammers = 3, gsCrossClears = 3, gsFreeSwaps = 3}
                       one name (g, oo) mt = name ++ "=" ++ pOutcome oo ++ ":" ++ hState g ++ ":" ++ pFx (moveFx gsB g oo) ++ ":" ++ pTrace mt
                       none name (g, oo) = name ++ "0=" ++ pOutcome oo ++ ":" ++ hState g
                   in [ unwords
                          [ pre, "boost"
                          , none "ham" (useHammer hp gs0), none "crs" (useCrossClear hp gs0), none "fsw" (useFreeSwap fp1 fp2 gs0)
                          , one "ham" (useHammer hp gsB) (traceHammer hp gsB)
                          , one "crs" (useCrossClear hp gsB) (traceCrossClear hp gsB)
                          , one "fsw" (useFreeSwap fp1 fp2 gsB) (traceFreeSwap fp1 fp2 gsB)
                          ]
                      ]
         in (mainL : auxL : boostL, if gsOver gs1 == Nothing then Just gs1 else Nothing)

pad2 :: Int -> String
pad2 n = if n < 10 then '0' : show n else show n

runGame :: String -> GameState -> Int -> [String]
runGame tag gs0 n = (tag ++ " start " ++ pState gs0 ++ " board=" ++ pBoard (gsBoard gs0)) : go 0 gs0
  where
    go i gs
      | i >= n = []
      | otherwise =
          let (ls, next) = stepLines tag i gs
          in ls ++ maybe [tag ++ " #" ++ pad2 i ++ " end"] (go (i + 1)) next

-- | 全部行：38 关 × 种子 {1,2} × 15 步、2 个每日式开局、手工局面、连锁 API。
goldenLines :: [String]
goldenLines =
  concat
    [ runGame ("L" ++ pad2 (li + 1) ++ " s" ++ show seed) (newGameAtLevel li (levelConfig (allLevels !! li)) seed) 15
    | li <- [0 .. length allLevels - 1]
    , seed <- [1, 2 :: Int]
    ]
    ++ concat
      [ runGame ("D" ++ show seed) (newDailyGame (GameConfig 20 (GoalScore 900)) seed) 10
      | seed <- [20260929, 20260101 :: Int]
      ]
    ++ handmade
    ++ cascadeLines
    ++ levelLines

-- | 手工局面（护栏测试用到的几个）：蜗牛撞墙 / 推格、巧克力关无匹配交换、首个 3 连锁。
handmade :: [String]
handmade =
  let base = newGame defaultConfig 7
      snailGs = base {gsBoard = setCell (setCell (gsBoard base) (0, 0) (Snail 0 (-1))) (3, 3) (Snail 0 1)}
      chocoGs = newGameAtLevel 4 (levelConfig (allLevels !! 4)) 1
      comboGs =
        case [ gs0 | seed <- [1 .. 400 :: Int], let gs0 = newGameAtLevel 0 (levelConfig (head allLevels)) seed
             , Just (p1, p2) <- [findHint (gsBoard gs0)], let (gs1, out) = trySwap p1 p2 gs0, out /= NoMatch, gsCombo gs1 >= 3 ] of
          (g : _) -> g
          [] -> base
      allSwaps tag gs =
        [ unwords [tag, "pair", pPos p1 ++ "-" ++ pPos p2, "out=" ++ pOutcome o, "st=" ++ pState g, "tr=" ++ pTrace (traceSwap p1 p2 gs)]
        | (p1, p2) <- allPairs
        , let (g, o) = trySwap p1 p2 gs
        , applied o
        ]
  in runGame "H1-snail" snailGs 8 ++ allSwaps "H1-snail" snailGs
       ++ runGame "H2-choco" chocoGs 8 ++ allSwaps "H2-choco" chocoGs
       ++ runGame "H3-combo" comboGs 8 ++ allSwaps "H3-combo" comboGs

-- | 连锁 API（门面 Match3.Board）：随机盘 × 飞碟 / 传送门的普通连锁与种子连锁，结算与回放各一份。
cascadeLines :: [String]
cascadeLines =
  [ unwords
      [ "C" ++ pad2 seed ++ "/" ++ show k, "run=" ++ runP, "trace=" ++ trP ]
  | seed <- [1 .. 30 :: Int]
  , (k, (ufos, portals)) <- zip [0 :: Int ..] [([], []), ([mkUfo (2, 3) C1], []), ([], [((0, 1), (7, 6)), ((0, 6), (7, 1))])]
  , let (b0, g1) = randomBoard (mkStdGen seed)
        runP = pRun (runCascadeScoredWithUfos Nothing ufos portals g1 b0)
        trP = pTr (traceCascade Nothing ufos portals g1 b0)
  ]
    ++ [ unwords [ "S" ++ pad2 seed ++ "/" ++ show k, "run=" ++ pRun (runCascadeScoredFromSeedsWithUfos Nothing seeds ufos [] g1 b0), "trace=" ++ pTr (traceCascadeFromSeeds Nothing seeds ufos [] g1 b0) ]
       | seed <- [1 .. 20 :: Int]
       , (k, (seeds, ufos)) <- zip [0 :: Int ..] [([(3, 3)], []), ([(r, 4) | r <- [0 .. 7]], [mkUfo (1, 1) C2]), ([(2, c) | c <- [0 .. 7]] ++ [(r, 2) | r <- [0 .. 7]], [])]
       , let (b0, g1) = randomPlayableBoard (mkStdGen seed)
       ]
  where
    pRun (b, cells, score, maxW, tallies, stones, chests, honey, balloons, cookies, cakes, uAbs, ufos', cleared, g) =
      unwords
        [ "b#" ++ fnv1a (pBoard b), "cells=" ++ show cells, "score=" ++ show score, "maxw=" ++ show maxW, "bag=" ++ pBag tallies
        , "stone=" ++ show stones, "chest=" ++ show chests, "honey=" ++ show honey, "balloon=" ++ show balloons
        , "cookie=" ++ show cookies, "cake=" ++ show cakes, "uabs=" ++ show uAbs, "ufos=" ++ pUfos ufos'
        , "cleared=" ++ pPosList cleared, "gen=" ++ show (g :: StdGen) ]
    pTr (ws, b, ufos', g) =
      unwords
        [ "w" ++ show (length ws) ++ "#" ++ fnv1a (intercalate "\n" (map pWave ws)), "b#" ++ fnv1a (pBoard b), "ufos=" ++ pUfos ufos', "gen=" ++ show (g :: StdGen) ]

-- | 开局 / 重开 / 下一关（关卡装饰与随机数消费顺序）。
levelLines :: [String]
levelLines =
  [ "G" ++ pad2 (li + 1) ++ " s" ++ show s ++ " new " ++ pState gs ++ " board=" ++ pBoard (gsBoard gs)
  | li <- [0 .. length allLevels - 1]
  , s <- [0, 5, 99 :: Int]
  , let gs = newGameAtLevel li (levelConfig (allLevels !! li)) s
  ]
    ++ [ "R" ++ pad2 (li + 1) ++ " s" ++ show s ++ " restart " ++ pState (restartLevel (newGameAtLevel li (levelConfig (allLevels !! li)) 1) s)
       | li <- [0, 10, 27, 35], s <- [4, 8 :: Int] ]
    ++ [ "X" ++ pad2 (li + 1) ++ " next " ++ pState (nextLevel (newGameAtLevel li (levelConfig (allLevels !! li)) 1) 4)
       | li <- [0, 10, 27] ]
