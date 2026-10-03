{-# LANGUAGE OverloadedStrings #-}

-- | 第 7 项（网格几何）之前的代码逐字副本，只给 Spec.GridGeometry 做新旧对照。
--
-- 函数体照抄 49d57ad（feat/hs-rules-dedup）的写法：手写的邻格列表（上下左右 / 上左右下 / 右下左上 / 右下）、
-- @p <- boardPositions b@ 再回盘读格的列表推导、收裸 Int 的 Engine.GridUI 像素换算与 Match3.Daily。
-- 只改了名字冲突（模块内本地的 ortho 分别叫 grassOrtho / ufoOrtho）、本地别名 at 换成同义的 getCell，与注释。
module Spec.Support.LegacyGeometry
  ( -- * 邻格顺序
    orthoNeighbors
  , grassOrtho
  , ufoOrtho
  , moveUfo
  , spreadPairs
  , adjacent
  , orthoAdjacent
  , swapCandidates
    -- * 带坐标的遍历
  , positionsWith
  , snailPositions
  , countdownsAtZero
  , extractDecorWith
  , rainbowClearSeeds
  , isFuzzball
  , fuzzballPositions
  , chameleonPositions
  , magicStones
    -- * 像素与日期
  , gridCellAt
  , gridCellOrigin
  , dailySeed
  , dailyConfig
  ) where

import Data.List (nub, sort)
import Engine.GridUI (GridGeom (..), gridHeight, gridInside, gridWidth)
import Engine.Optics (Getting, has)
import Data.Monoid (Any)
import Match3.Board.Grid (getCell, inBounds)
import Match3.Counts (CounterKey (..))
import Match3.Element.Builtin.Collectible (chameleonColor)
import Match3.Element.Registry (Registry, keepOnShuffleWith)
import Match3.Game.Shuffle (CellDecor (..))
import Match3.Types
import Match3.Rainbow (isRainbow)
import Match3.Ufo (Ufo (..))

--------------------------------------------------------------------------------
-- 邻格顺序

-- | Match3.Obstacles：上 / 下 / 左 / 右。
orthoNeighbors :: Pos -> [Pos]
orthoNeighbors (r, c) =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | Match3.Grass 的本地 ortho：上 / 下 / 左 / 右，界内。
grassOrtho :: Board -> Pos -> [Pos]
grassOrtho b (r, c) =
  filter (inBounds b) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | Match3.Ufo 的本地 ortho：上 / 下 / 左 / 右，界内。
ufoOrtho :: Board -> Pos -> [Pos]
ufoOrtho b (r, c) =
  filter (inBounds b) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | Match3.Ufo.moveUfo：右 → 下 → 左 → 上。
moveUfo :: Board -> [Pos] -> Ufo -> Ufo
moveUfo b absorbed (Ufo pos col) =
  case sort absorbed of
    (p : _) -> Ufo p col
    [] ->
      let (r, c) = pos
          cands =
            filter (inBounds b) [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
      in case cands of
           (p : _) -> Ufo p col
           [] -> Ufo pos col

-- | Match3.Element.Event.spreadPairs：来源取上 / 左 / 右 / 下第一个。
spreadPairs :: CellOverlay -> Board -> Board -> [(Pos, Pos)]
spreadPairs ov before after =
  [ (src, q)
  | r <- boardRowIndices before
  , c <- boardColIndices before
  , let q = (r, c)
  , cellOverlay (getCell before q) == Nothing
  , cellOverlay (getCell after q) == Just ov
  , let srcs = [n | n <- [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)], inBounds before n, cellOverlay (getCell before n) == Just ov]
  , let src = case srcs of
          (n : _) -> n
          [] -> q
  ]

-- | Match3.Board.Grid.adjacent。
adjacent :: Pos -> Pos -> Bool
adjacent (r1, c1) (r2, c2) =
  (abs (r1 - r2) == 1 && c1 == c2) || (abs (c1 - c2) == 1 && r1 == r2)

-- | Engine.GridUI.orthoAdjacent。
orthoAdjacent :: (Int, Int) -> (Int, Int) -> Bool
orthoAdjacent (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) == 1

-- | Match3.Engine 的合法动作 / Match3.Board.Match 的提示搜索里枚举的格对：每格配右、下两格（界内）。
-- （Engine 另有一个恒真的 @adjacent p q@ 过滤，照抄。）
swapCandidates :: Board -> [(Pos, Pos)]
swapCandidates b =
  [ (p, q)
  | (r, c) <- boardPositions b
  , let p = (r, c)
  , q <- [(r, c + 1), (r + 1, c)]
  , inBounds b q
  , adjacent p q
  ]

--------------------------------------------------------------------------------
-- 带坐标的遍历

-- | Match3.Grass.positionsWith（第 7 项前）。
positionsWith :: Getting Any Cell a -> Board -> [Pos]
positionsWith o b = [p | p <- boardPositions b, has o (getCell b p)]

-- | Match3.Snail.snailPositions。
snailPositions :: Board -> [Pos]
snailPositions b =
  sort
    [ p
    | p <- boardPositions b
    , isSnail (getCell b p)
    ]

-- | Match3.Countdown.countdownsAtZero。
countdownsAtZero :: Board -> [Pos]
countdownsAtZero b =
  [ p
  | p <- boardPositions b
  , case boardAt b p of
      Countdown _ 0 -> True
      _ -> False
  ]

-- | Match3.Game.Shuffle.extractDecorWith。
extractDecorWith :: Registry -> Board -> [CellDecor]
extractDecorWith reg b =
  [ CellDecor p cell
  | p <- boardPositions b
  , let cell = getCell b p
  , keepOnShuffleWith reg cell
  ]

-- | Match3.Rainbow.rainbowClearSeeds（含本地 colorPositions）。
rainbowClearSeeds :: Board -> Pos -> Pos -> [Pos]
rainbowClearSeeds b p1 p2 =
  nub (rainbows ++ targets)
  where
    at = boardAt
    c1 = at b p1
    c2 = at b p2
    rainbows =
      [ p
      | p <- [p1, p2]
      , isRainbow (at b p)
      ]
    targets = case (c1, c2) of
      (Gem _ Rainbow _ _, Gem _ Rainbow _ _) ->
        [ p
        | p <- boardPositions b
        , isGem (at b p)
        ]
      (Gem _ Rainbow _ _, Gem col _ _ _) -> colorPositions b col
      (Gem col _ _ _, Gem _ Rainbow _ _) -> colorPositions b col
      (Gem _ Rainbow _ _, Countdown col _) -> colorPositions b col
      (Countdown col _, Gem _ Rainbow _ _) -> colorPositions b col
      (Gem _ Rainbow _ _, Flip col _) -> colorPositions b col
      (Flip col _, Gem _ Rainbow _ _) -> colorPositions b col
      _ -> []

colorPositions :: Board -> Color -> [Pos]
colorPositions b col =
  [ p
  | p <- boardPositions b
  , case boardAt b p of
      Gem col' _ _ _ -> col' == col
      Countdown col' _ -> col' == col
      Stone _ -> False
      Chest _ -> False
      Honey _ -> False
      Balloon _ -> False
      Cookie -> False
      Cake _ -> False
      MagicHat -> False
      Maker _ _ -> False
      Snail _ _ -> False
      Safe _ -> False
      Surprise -> False
      Bottle _ -> False
      TimeSpirit -> False
      Flip col' _ -> col' == col
      Custom _ _ -> False
  ]

-- | Match3.Element.Builtin.Actor 的 isFuzzball（模块内部函数，照抄）。
isFuzzball :: Cell -> Bool
isFuzzball cell = case cell of
  Custom "fuzzball" _ -> True
  _ -> False

-- | Match3.Element.Builtin.Actor.fuzzballJumps 里的 balls。
fuzzballPositions :: Board -> [Pos]
fuzzballPositions b0 = [p | p <- boardPositions b0, isFuzzball (getCell b0 p)]

-- | Match3.Element.Builtin.Collectible.chameleonRainbowSeeds 里的同色变色龙。
chameleonPositions :: Color -> Board -> [Pos]
chameleonPositions col b = [p | p <- boardPositions b, chameleonColor (getCell b p) == Just col]

-- | Match3.Element.Builtin.Obstacle.magicStones（模块内部函数，不导出，这里连同新写法一起抄）。
magicStones :: Board -> [(Pos, Int)]
magicStones b = [(p, k) | p <- boardPositions b, Custom "magic_stone" (CustomState k) <- [getCell b p]]

--------------------------------------------------------------------------------
-- 像素与日期

-- | Engine.GridUI.gridCellAt（收两个裸的像素分量）。
gridCellAt :: Integral a => GridGeom a -> a -> a -> Maybe (Int, Int)
gridCellAt g px py =
  let x = px - ggLeft g
      y = py - ggTop g
  in if x < 0 || y < 0 || x >= gridWidth g || y >= gridHeight g
       then Nothing
       else
         let c = fromIntegral (x `div` ggCell g)
             r = fromIntegral (y `div` ggCell g)
         in if gridInside g (r, c) then Just (r, c) else Nothing

-- | Engine.GridUI.gridCellOrigin（回裸的 (x, y)）。
gridCellOrigin :: Num a => GridGeom a -> (Int, Int) -> (a, a)
gridCellOrigin g (r, c) = (ggLeft g + fromIntegral c * ggCell g, ggTop g + fromIntegral r * ggCell g)

-- | Match3.Daily.dailySeed（三个 Int）。
dailySeed :: Int -> Int -> Int -> Int
dailySeed year month day =
  year * 10000 + month * 100 + day

-- | Match3.Daily.dailyConfig（三个 Int）。
dailyConfig :: Int -> Int -> Int -> GameConfig
dailyConfig year month day =
  let s = dailySeed year month day
      flavor = s `mod` 10
  in case flavor of
       0 -> GameConfig 28 (goalScore 600)
       1 -> GameConfig 28 (goalCollect C1 18)
       2 -> GameConfig 28 (goalColors [(C2, 10), (C4, 10)])
       3 -> GameConfig 26 (goalCount CountStones 6)
       4 -> GameConfig 26 (goalCount CountHoney 6)
       5 -> GameConfig 26 (goalCount CountUfo 8)
       6 -> GameConfig 26 (goalCount CountChests 5)
       7 -> GameConfig 26 (goalCount CountCakes 5)
       8 -> GameConfig 26 (goalCount CountSafes 4)
       _ -> GameConfig 26 (goalCount CountBalloons 6)
