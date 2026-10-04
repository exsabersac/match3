{-# LANGUAGE OverloadedStrings #-}
-- | 元素查询快照：主流程经注册表问「这个格子怎么反应」的全部入口（*With 查询、规则表、规则在样例盘上的输出、
-- 放置、逐手对局），在一大批样本格 / 盘面上投影成稳定文本，入库为 test/golden/element-queries.txt。
--
-- 用途：元素类迁移（阶段 2）删掉扁平 ElementDef 记录之后，新旧两条路径已无法在同一进程里并排比对；
-- 快照在阶段 1（9ae6a7b，新旧记录并存、金标准全等）上生成，阶段 2 逐行比对。
-- 维护纪律同 Golden：内部 API 变了只改投影，快照一个字都不动。
module ElementQueries
  ( queryLines
  , main
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Match3.Board.Grid (getCell, inBounds, setCell)
import Match3.Board.Match (findHintWith)
import Match3.Element
import Match3.Element.Class (levelNameOf)
import Match3.Game.Boosters (resolveCrossClearWith, resolveHammerWith)
import Match3.Game.Level (campaignGame, newGameAtLevel)
import Match3.Game.Move (resolveSwapWith)
import Match3.Game.State
import Match3.Levels.Campaign (allLevels, lookupLevel)
import Match3.Levels.Level (Level, levelConfig)
import Match3.Types
import Numeric (showHex)

main :: IO ()
main = mapM_ putStrLn queryLines

-- | 战役第 1 关（关卡表为空时直接报错）。
firstLevel :: Level
firstLevel = fromMaybe (error "firstLevel: allLevels is empty") (lookupLevel 0)

-- | 第 li 关（0 基）按该关步数与目标、给定种子开局；没有这一关直接报错（第 6 刀：取代 allLevels !! li）。
levelGame :: Int -> Int -> GameState
levelGame li seed = fromMaybe (error ("levelGame: no level " ++ show li)) (campaignGame li seed)

reg :: World
reg = defaultWorld

hash :: String -> String
hash s =
  let h = foldl' (\acc ch -> (acc `xor` fromIntegral (ord ch)) * 1099511628211) (14695981039346656037 :: Word64) s
  in showHex h ""

colors :: [Color]
colors = [minBound .. maxBound]

-- | 样本格：每种构造器的各种状态，宝石 × 冰 × 全部叠层。
sampleCells :: [Cell]
sampleCells =
  [ Gem c k i ov
  | c <- [C1, C4]
  , k <- [Normal, LineH, LineV, Bomb, Rainbow]
  , i <- [0 .. 2]
  , ov <- Nothing : map Just [Grass, Vine, Choco, Fog 1, Fog 2, Chain 1, Chain 2, Freeze 1, Freeze 2, Curtain 1, Curtain 2, Steam]
  ]
    ++ concat
      [ [Stone n | n <- [1 .. 3]]
      , [Chest n | n <- [1, 2]]
      , [Honey n | n <- [1, 2]]
      , [Balloon c | c <- colors]
      , [Cookie]
      , [Cake n | n <- [1 .. 3]]
      , [MagicHat]
      , [Maker c n | c <- [C2, C5], n <- [1, 3]]
      , [Snail 0 1, Snail 1 0, Snail 0 (-1), Snail (-1) 0]
      , [Safe n | n <- [1 .. 3]]
      , [Flip f b | f <- [C1, C3], b <- [C2, C5]]
      , [Surprise]
      , [Bottle c | c <- colors]
      , [TimeSpirit]
      , [Countdown c n | c <- [C1, C5], n <- [1, 5]]
      , [Custom "bubble" (CustomState 1), Custom "bubble" (CustomState 2), Custom "unregistered" (CustomState 3)]
      ]

cellLine :: Cell -> String
cellLine cell =
  "Q " ++ show cell ++ " | "
    ++ unwords
      [ show (matchColorWith reg cell)
      , show (colorOfWith reg cell)
      , show (blocksSwapWith reg cell)
      , show (upperBlocksSwapWith reg cell)
      , show (activatesWith reg cell)
      , show (fallsWith reg cell)
      , show (portalWith reg cell)
      , show (drainsWith reg cell)
      , show (drainEdgesWith reg cell)
      , "Hit" ++ show (directHitWith reg cell)  -- 第 4 刀起是 Strike；行里仍写旧名 HitAbsorb / HitDestroy / HitImmune
      , show (hitImmuneWith reg cell)
      , show (counterWith reg cell)
      , show (vacatesCarpetWith reg cell)
      , show (keepOnShuffleWith reg cell)
      , show (blastWith reg base cell (3, 3))
      , show (blastWith reg base cell (0, 7))
      , show (recolorableWith reg cell)
      , show (pushableWith reg cell)
      , show (elementName reg cell)
      , show (topLayerName reg cell)
      , show (getCell (stripOnClearWith reg (setCell base (2, 2) cell) [(2, 2)]) (2, 2))
      ]

-- | 底盘：无匹配的固定花色盘。
base :: Board
base = mkBoard [[toEnum ((r * 3 + c * 2 + (r `div` 2)) `mod` 5) | c <- [0 .. boardSize - 1]] | r <- [0 .. boardSize - 1]]
  where
    mkBoard rows = foldl (\b (p, col) -> setCell b p (mkGem col)) (fst (emptyish rows)) [((r, c), col) | (r, row) <- zip [0 ..] rows, (c, col) <- zip [0 ..] row]
    emptyish _ = (initialBoard, ())

initialBoard :: Board
initialBoard = gsBoard (newGameAtLevel 0 (levelConfig firstLevel) 1)

-- | 全元素样例盘：每个样本格放一格（按行优先填满，多出来的丢掉），再补一条 C1 三连做真消除源。
zoo :: Int -> Board
zoo off = foldl (\b (p, cell) -> setCell b p cell) base (zip ps (drop off sampleCells))
  where
    ps = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

placeNames :: [ElementName]
placeNames =
  [ "ice", "grass", "vine", "choco", "fog", "chain", "freeze", "curtain", "steam", "stone", "chest", "honey", "balloon", "cookie"
  , "cake", "magic_hat", "maker", "snail", "safe", "flip", "surprise", "bottle", "time_spirit", "countdown", "bubble", "jelly", "gem" ]

placeLines :: [String]
placeLines =
  [ "P " ++ unElementName n ++ " " ++ show args ++ " " ++ show cell ++ " -> " ++ show (getCell (either (error . show) id (placeWith reg n args (setCell base (4, 4) cell) [(4, 4)])) (4, 4))
  | n <- placeNames
  , args <- [[], [AInt 1], [AInt 3], [AColor C2], [AColor C1, AColor C4], [AColor C3, AInt 5], [AInt 1, AInt 0], [AInt 0, AInt (-1)]]
  , cell <- [mkGem C2, Gem C3 LineH 1 (Just Grass), Stone 1, Countdown C4 2, Custom "bubble" (CustomState 1)]
  ]

ruleLines :: [String]
ruleLines =
  [ "R adjacent " ++ show (map arOrder (adjacentRules reg))
  , "R end " ++ show [(ph, map erOrder (endRules reg ph)) | ph <- [PhaseTick, PhaseSpread, PhaseMove]]
  , "R swap " ++ show (map srOrder (swapRules reg))
  , "R names " ++ show (map defName (worldDefs reg))
  , "R diff " ++ show [(n, Just k, bonus) | (n, k, bonus) <- diffCountersWith reg]
  , "R level " ++ show (map levelNameOf (levelDefs reg))
  ]
    ++ [ "A " ++ show off ++ " " ++ show tc ++ " " ++ hash (show (runAdjacentWith reg tc [(0, 0)] [(7, 7)] (zoo off)))
       | off <- [0, 60, 120, 180, 240]
       , tc <- [[(3, 3)], [(r, c) | r <- [2 .. 5], c <- [2 .. 5]], [(r, c) | r <- [0 .. 7], c <- [0 .. 7], even (r + c)]]
       ]
    ++ [ "E " ++ show off ++ " " ++ show ph ++ " " ++ hash (show [(fst (erRun r (EndCtx [(1, 1)] [(6, 6)] (pushableWith reg)) (zoo off)), erSeeds r (zoo off), erHoles r (zoo off), boardText (snd (erRun r (EndCtx [] [] (pushableWith reg)) (zoo off)))) | r <- endRules reg ph])
       | off <- [0, 60, 120, 180, 240]
       , ph <- [PhaseTick, PhaseSpread, PhaseMove]
       ]
    ++ [ "S " ++ show off ++ " " ++ hash (show [(p1, p2, map (\r -> (srFires r (zoo off) p1 p2, srSeeds r (zoo off) p1 p2)) (swapRules reg), swapOpeningWith reg (zoo off) (zoo off) p1 p2) | (p1, p2) <- pairs])
       | off <- [0, 60, 120, 180, 240]
       ]
    ++ [ "O " ++ show off ++ " " ++ hash (show (let (b, e, s) = openWith reg (zoo off) front in (boardText b, e, s)))
       | off <- [0, 60, 120, 180, 240]
       , front <- [[(r, c) | r <- [0 .. 7], c <- [0 .. 7]]]
       ]
    ++ [ "C " ++ show off ++ " " ++ hash (show (chipOnHitWith reg (zoo off) [(r, c) | r <- [0 .. 7], c <- [0 .. 7]]))
       | off <- [0, 60, 120, 180, 240]
       ]
    ++ [ "G " ++ show (hitGroundWith reg [(1, 1), (2, 2)] [((1, 1), ("jelly", 2)), ((2, 2), ("jelly", 1)), ((3, 3), ("jelly", 2)), ((1, 1), ("moss", 1))]) ]
  where
    pairs = [((r, c), q) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], q <- [(r, c + 1), (r + 1, c)], inBounds base q]

boardText :: Board -> String
boardText b = show [getCell b (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

-- | 逐手对局：全部关卡 × 种子 1–2 × 12 手（走提示），每手前试锤子与十字，按关卡 / 种子给一行散列。
playLines :: [String]
playLines =
  [ "M " ++ show (li + 1) ++ " " ++ show seed ++ " " ++ hash (concat (go 12 (levelGame li seed)))
  | li <- [0 .. length allLevels - 1]
  , seed <- [1, 2]
  ]
  where
    go :: Int -> GameState -> [String]
    go 0 _ = []
    go k gs
      | gsOver gs /= Nothing = []
      | otherwise =
          let hint = findHintWith reg (gsBoard gs)
              hp = (k `mod` boardSize, (k * 3) `mod` boardSize)
              probes = [show hint, show (resolveHammerWith reg hp gs), show (resolveCrossClearWith reg hp gs)]
          in case hint of
               Nothing -> probes
               Just (a, b) ->
                 let r@(gs', _, _) = resolveSwapWith reg a b gs
                 in probes ++ [show r] ++ go (k - 1) gs'

queryLines :: [String]
queryLines = map cellLine sampleCells ++ placeLines ++ ruleLines ++ playLines
