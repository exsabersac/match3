{-# LANGUAGE OverloadedStrings #-}
-- | 元素对照快照（元素类重构第 0 刀）：与实现无关的「元素怎么反应」读数，入库为 test/golden/element-oracle.txt。
--
-- 和 element-queries.txt 互补：那份锁住样本格上的查询、规则整体在 5 张样例盘上的输出与 40 关逐手对局；
-- 这份把覆盖面铺开——
--
-- * V：更多样本格（冰 0–3 × 全部叠层 × 五种宝石、全部自定义元素的各状态、雪怪各部件）× 全部值查询
--   （含 hintable / 显示附加字段 / 按差计数权重）；
-- * N：每个注册名的显示名、失败提示、按差计数；P：每个注册名 × 放置参数 × 原格；
-- * AR / ER / SR：**逐条**邻格 / 步末 / 成对交换规则在 400 张随机盘（混入全部元素、约 1/3 带雪怪）上的输出散列，
--   以及该规则真正改动了盘面 / 产出了结果的盘数（证明随机盘能触发它）；
-- * RA / OP / CH / ST / BW / HG / CT：整轮邻格、开启、直接命中、随格清除、扩爆、地面层、计数在随机盘上的散列。
--
-- 重构期间内部 API 会变：只改投影（本文件），快照一个字都不动。生成：stack exec -- ghc … -main-is ElementOracle。
module ElementOracle
  ( oracleLines
  , oracleLinesWith
  , main
  ) where

import Data.Bits (shiftR, xor)
import Data.Char (ord)
import Data.List (nub)
import Data.Word (Word64)
import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Element
import Match3.Game.Level (newGameAtLevel)
import Match3.Game.State (gsBoard)
import Match3.Levels.Campaign (lookupLevel)
import Match3.Levels.Level (levelConfig)
import Match3.Types
import Numeric (showHex)

main :: IO ()
main = mapM_ putStrLn oracleLines


hash :: String -> String
hash s =
  let h = foldl' (\acc ch -> (acc `xor` fromIntegral (ord ch)) * 1099511628211) (14695981039346656037 :: Word64) s
  in showHex h ""

-- 确定性的伪随机数（不依赖 random 包的版本）：splitmix64。
newtype Rng = Rng Word64

next :: Rng -> (Word64, Rng)
next (Rng s) =
  let s' = s + 0x9e3779b97f4a7c15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in (z2 `xor` (z2 `shiftR` 31), Rng s')

-- | [0, n) 里的一个数。
roll :: Int -> Rng -> (Int, Rng)
roll n g = let (w, g') = next g in (fromIntegral (w `mod` fromIntegral n), g')

pick :: [a] -> Rng -> (a, Rng)
pick xs g = let (i, g') = roll (length xs) g in (xs !! i, g')

colors :: [Color]
colors = [minBound .. maxBound]

overlays :: [CellOverlay]
overlays = [Grass, Vine, Choco, Fog 1, Fog 2, Chain 1, Chain 2, Freeze 1, Freeze 2, Curtain 1, Curtain 3, Steam]

custom :: ElementName -> Int -> Cell
custom n k = Custom n (CustomState k)

-- | 雪怪部件格（血量、上限、回合、部件号）。
bossCell :: Int -> Int -> Int -> Int -> Cell
bossCell hp mx t q = custom "snow_boss" (((mx * 256 + hp) * 4 + t) * 4 + q)

-- | 非宝石的样本格（每种本体的各种状态）。
otherCells :: [Cell]
otherCells =
  concat
    [ [Stone n | n <- [1 .. 4]]
    , [Chest n | n <- [1 .. 3]]
    , [Honey n | n <- [1 .. 3]]
    , [Balloon c | c <- colors]
    , [Cookie]
    , [Cake n | n <- [1 .. 4]]
    , [MagicHat]
    , [Maker c n | c <- colors, n <- [1, 2, 3]]
    , [Snail 0 1, Snail 1 0, Snail 0 (-1), Snail (-1) 0]
    , [Safe n | n <- [1 .. 4]]
    , [Flip f b | f <- colors, b <- [C2, C5]]
    , [Surprise]
    , [Bottle c | c <- colors]
    , [TimeSpirit]
    , [Countdown c n | c <- colors, n <- [0, 1, 2, 5]]
    , [custom "bubble" k | k <- [0 .. 3]]
    , [custom "fuzzball" k | k <- [0 .. 2]]
    , [custom "chameleon" k | k <- [0 .. 6]]
    , [custom "magic_stone" k | k <- [0 .. 5]]
    , [bossCell hp mx t q | (hp, mx) <- [(1, 1), (3, 6), (6, 6), (0, 4)], t <- [0, 2], q <- [0 .. 3]]
    , [custom "jelly" 1, custom "magic" 1, custom "unregistered" 3, custom "ice" 2]
    ]

-- | 全部样本格：宝石（五种 × 冰 0–3 × 无 / 全部叠层，两种颜色）+ 其余本体。
valueCells :: [Cell]
valueCells =
  [Gem c k i ov | c <- [C2, C5], k <- [Normal, LineH, LineV, Bomb, Rainbow], i <- [0 .. 3], ov <- Nothing : map Just overlays]
    ++ otherCells

-- | 底盘：无匹配的固定花色盘（同 element-queries）。
base :: Board
base = foldl (\b (p, col) -> setCell b p (mkGem col)) start [((r, c), toEnum ((r * 3 + c * 2 + (r `div` 2)) `mod` 5)) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]
  where
    start = gsBoard (newGameAtLevel 0 (levelConfig lvl) 1)
    lvl = maybe (error "no level 0") id (lookupLevel 0)

allPos :: [Pos]
allPos = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

boardText :: Board -> String
boardText b = show (map (getCell b) allPos)

valueLine :: Registry -> Cell -> String
valueLine world cell =
  "V " ++ show cell ++ " | "
    ++ unwords
      [ show (matchColorWith world cell)
      , show (colorOfWith world cell)
      , show (blocksSwapWith world cell)
      , show (upperBlocksSwapWith world cell)
      , show (activatesWith world cell)
      , show (fallsWith world cell)
      , show (portalWith world cell)
      , show (drainEdgesWith world cell)
      , strikeColumn (directHitWith world cell)
      , show (hitImmuneWith world cell)
      , show (counterWith world cell)
      , show (vacatesCarpetWith world cell)
      , show (keepOnShuffleWith world cell)
      , show (hintableWith world cell)
      , show (recolorableWith world cell)
      , show (pushableWith world cell)
      , show (elementName world cell)
      , show (topLayerName world cell)
      , show (faceFieldsWith world cell)
      , show (weighElementWith world (elementName world cell) (setCell base (2, 2) cell))
      , show (countElementWith world (elementName world cell) (setCell base (2, 2) cell))
      , show (blastWith world base cell (3, 3))
      , show (blastWith world base cell (0, 7))
      , show (getCell (stripOnClearWith world (setCell base (2, 2) cell) [(2, 2)]) (2, 2))
      , show (swapBlockedWith world (setCell base (2, 2) cell) (2, 2) (2, 3))
      ]

names :: Registry -> [ElementName]
names world = map defName (registryDefs world)

nameLines :: Registry -> [String]
nameLines world =
  [ "N " ++ show n ++ " " ++ show (displayLabelWith world n) ++ " " ++ show (fmap ($ 7) (loseHintWith world n))
  | n <- names world ++ ["unregistered", "ufo"]
  ]
    ++ ["N labels " ++ show (displayLabels world), "N diff " ++ show (diffCountersWith world)]

placeLines :: Registry -> [String]
placeLines world =
  [ "P " ++ unElementName n ++ " " ++ show args ++ " " ++ show cell ++ " -> " ++ either show (show . (`getCell` (4, 4))) (placeWith world n args (setCell base (4, 4) cell) [(4, 4)])
  | n <- names world ++ ["unregistered"]
  , args <- [[], [AInt 0], [AInt 1], [AInt 2], [AInt 4], [AInt 300], [AColor C2], [AColor C1, AColor C4], [AColor C3, AInt 5], [AInt 1, AInt 0], [AInt 5, AInt 2], [AInt 0, AInt (-1)], [AInt 2, AColor C1]]
  , cell <- [mkGem C2, Gem C3 LineH 1 (Just Grass), Gem C4 Normal 0 (Just (Chain 2)), Stone 1, Countdown C4 2, custom "bubble" 1]
  ]

--------------------------------------------------------------------------------
-- 随机盘

-- | 一个随机格：多数是宝石（带冰 / 叠层），其余从全部本体里取。
randomCell :: Rng -> (Cell, Rng)
randomCell g0 =
  let (kind, g1) = roll 100 g0
  in if kind < 62
       then
         let (c, g2) = pick colors g1
             (k, g3) = pick (replicate 12 Normal ++ [LineH, LineV, Bomb, Rainbow]) g2
             (i, g4) = pick [0, 0, 0, 0, 0, 1, 2, 3] g3
             (ov, g5) = pick (replicate 14 Nothing ++ map Just overlays) g4
         in (Gem c k i ov, g5)
       else pick [x | x <- otherCells, not (isBoss x)] g1
  where
    isBoss cell = case cell of
      Custom "snow_boss" _ -> True
      _ -> False

randomBoard :: Rng -> (Board, Rng)
randomBoard g0 =
  let (b1, g1) = foldl (\(b, g) p -> let (cell, g') = randomCell g in (setCell b p cell, g')) (base, g0) allPos
      (hasBoss, g2) = roll 3 g1
      (r, g3) = roll (boardSize - 1) g2
      (c, g4) = roll (boardSize - 1) g3
      (mx, g5) = roll 8 g4
      (hp0, g6) = roll (mx + 1) g5
      (t, g7) = roll 3 g6
      hp = max 1 hp0
      boss = [((r + dr, c + dc), bossCell hp (max hp mx) t q) | (q, (dr, dc)) <- zip [0 ..] [(0, 0), (0, 1), (1, 0), (1, 1)]]
  in (if hasBoss == 0 then foldl (\b (p, cell) -> setCell b p cell) b1 boss else b1, g7)

-- | 一个随机位置子集（每格以 1/k 的概率入选）。
randomPositions :: Int -> Rng -> ([Pos], Rng)
randomPositions k g0 = foldl (\(ps, g) p -> let (x, g') = roll k g in (if x == 0 then ps ++ [p] else ps, g')) ([], g0) allPos

data Sample = Sample
  { smBoard   :: Board
  , smTrue    :: [Pos]
  , smDirect  :: [Pos]
  , smProtect :: [Pos]
  , smPairs   :: [(Pos, Pos)]
  , smGround  :: Ground
  }

samples :: [Sample]
samples = take 400 (go (Rng 20261004))
  where
    go g0 =
      let (b, g1) = randomBoard g0
          (tc, g2) = randomPositions 4 g1
          (dc, g3) = randomPositions 6 g2
          (pc, g4) = randomPositions 12 g3
          (prs, g5) = randomPairs 12 g4
          (gr, g6) = randomGround g5
      in Sample b tc dc pc prs gr : go g6
    randomPairs n g0 = foldl (\(acc, g) _ -> let (p, g') = pick allPos g; (d, g'') = pick [(0, 1), (1, 0)] g'; q = (fst p + fst d, snd p + snd d) in (if inBounds base q then acc ++ [(p, q)] else acc, g'')) ([], g0) [1 .. n :: Int]
    randomGround g0 =
      let (ps, g1) = randomPositions 5 g0
      in foldl (\(acc, g) p -> let (n, g') = pick ["jelly", "jelly", "magic", "moss"] g; (l, g'') = pick [1, 2, 3] g' in (acc ++ [(p, (n, l))], g'')) ([], g1) ps

-- | 一条规则在全部随机盘上的输出散列 + 起作用的盘数。
ruleSummary :: (Sample -> (Bool, String)) -> String
ruleSummary f =
  let outs = map f samples
  in hash (concatMap snd outs) ++ " active " ++ show (length (filter fst outs))

adjOutText :: Board -> AdjOut -> (Bool, String)
adjOutText b0 out = (aoBoard out /= b0 || not (null (aoDead out)) || not (null (aoSit out)), show (boardText (aoBoard out), aoDead out, aoSit out))

ruleLines :: Registry -> [String]
ruleLines world =
  [ "AR " ++ show i ++ " " ++ show (arOrder r) ++ " " ++ ruleSummary (\s -> adjOutText (smBoard s) (arRun r (AdjCtx (smTrue s) (smDirect s) (smProtect s) (recolorableWith world)) (smBoard s)))
  | (i, r) <- zip [0 :: Int ..] (adjacentRules world)
  ]
    ++ [ "ER " ++ show ph ++ " " ++ show (erOrder r) ++ " " ++ ruleSummary (endOut r)
       | ph <- [PhaseTick, PhaseSpread, PhaseMove]
       , r <- endRules world ph
       ]
    ++ [ "SR " ++ show (srOrder r) ++ " " ++ ruleSummary (\s -> let xs = [(srFires r (smBoard s) p q, srSeeds r (smBoard s) p q) | (p, q) <- smPairs s ++ rainbowPairs (smBoard s)] in (any fst xs, show xs))
       | r <- swapRules world
       ]
    ++ [ "SO " ++ ruleSummary (\s -> let xs = [swapOpeningWith world (smBoard s) (swapped (smBoard s) p q) p q | (p, q) <- smPairs s ++ rainbowPairs (smBoard s)] in (any (/= Nothing) xs, show xs)) ]
    ++ [ "RA " ++ ruleSummary (\s -> let (b, d, st) = runAdjacentWith world (smTrue s) (smDirect s) (smProtect s) (smBoard s) in (b /= smBoard s || not (null d) || not (null st), show (boardText b, d, st))) ]
    ++ [ "OP " ++ ruleSummary (\s -> let (b, e, st) = openWith world (smBoard s) (smTrue s) in (b /= smBoard s, show (boardText b, e, st))) ]
    ++ [ "CH " ++ ruleSummary (\s -> let (b, d) = chipOnHitWith world (smBoard s) (smTrue s ++ smDirect s) in (b /= smBoard s || not (null d), show (boardText b, d))) ]
    ++ [ "ST " ++ ruleSummary (\s -> let b = stripOnClearWith world (smBoard s) (smTrue s) in (b /= smBoard s, boardText b)) ]
    ++ [ "BW " ++ ruleSummary (\s -> let ws = groundWideningWith world (smGround s); r' = setWidening ws world; xs = [blastWith r' (smBoard s) (getCell (smBoard s) p) p | p <- smTrue s] in (any (not . null) xs && not (null ws), show (map fst ws, xs))) ]
    ++ [ "HG " ++ ruleSummary (\s -> let out = hitGroundWith world (smTrue s) (smGround s) in (fst out /= smGround s, show out)) ]
    ++ [ "CT " ++ ruleSummary (\s -> let xs = [(countElementWith world n (smBoard s), weighElementWith world n (smBoard s)) | n <- names world] in (True, show xs)) ]
  where
    endOut r s =
      let ctxs = [EndCtx [] [] (pushableWith world), EndCtx (smProtect s) (take 4 (smDirect s)) (pushableWith world)]
          outs = [(eff, boardText b', erSeeds r b', erHoles r b') | ctx <- ctxs, let (eff, b') = erRun r ctx (smBoard s)]
      in (any (\(eff, _, sd, _) -> eff /= Nothing || not (null sd)) outs, show outs)
    swapped b p q = setCell (setCell b p (getCell b q)) q (getCell b p)
    -- 让成对交换规则有机会成立：每个彩虹 / 变色龙格和它的右、下邻格
    rainbowPairs b = nub [(p, q) | p <- allPos, special (getCell b p), q <- [(fst p, snd p + 1), (fst p + 1, snd p)], inBounds b q]
    special cell = case cell of
      Gem _ k _ _ -> k /= Normal
      Custom "chameleon" _ -> True
      _ -> False

oracleLines :: [String]
oracleLines = oracleLinesWith defaultRegistry

-- | 同一套投影，换一张元素世界（第 1 刀：把内置条目换成新类经适配器注册的版本，快照应逐行不变）。
oracleLinesWith :: Registry -> [String]
oracleLinesWith world = map (valueLine world) valueCells ++ nameLines world ++ placeLines world ++ ruleLines world

-- | 直接命中一列的文本。对照文件的格式固定为第 4 刀前的构造子名（HitAbsorb / HitDestroy / HitImmune），
-- 即 "Hit" ++ show Strike；旧名只收在这一处，改对照文件的输出格式要另报。
strikeColumn :: Strike -> String
strikeColumn s = "Hit" ++ show s
