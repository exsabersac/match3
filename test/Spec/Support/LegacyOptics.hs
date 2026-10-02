-- | Haskell 特性第 6 项前的叠层代码逐字副本（Match3.Grass 的全部函数、Match3.Types.Overlay 的读数与写入），
-- 只给 Spec.Optics 对照用：新写法（光学，四种揭层 / 三种蔓延各写一次）与这里逐项相同。
module Spec.Support.LegacyOptics
  ( clearOverlaysOn
  , clearChocoAdjacent
  , clearSteamAdjacent
  , chipAdjacentFog
  , chipAdjacentFogExcept
  , chipAdjacentChain
  , chipAdjacentChainExcept
  , chipAdjacentFreeze
  , chipAdjacentFreezeExcept
  , chipAdjacentCurtain
  , chipAdjacentCurtainExcept
  , vinePositions
  , chocoPositions
  , fogPositions
  , chainPositions
  , freezePositions
  , curtainPositions
  , steamPositions
  , spreadVines
  , spreadChoco
  , spreadSteam
  , hasGrass
  , hasVine
  , hasChoco
  , hasFog
  , fogLayers
  , hasChain
  , chainLayers
  , hasFreeze
  , freezeLayers
  , hasCurtain
  , curtainLayers
  , hasSteam
  , clearOverlay
  , setOverlay
  ) where

import Data.List (nub)
import Match3.Board.Grid (inBounds)
import Match3.Types hiding (hasGrass, hasVine, hasChoco, hasFog, fogLayers, hasChain, chainLayers, hasFreeze, freezeLayers, hasCurtain, curtainLayers, hasSteam, clearOverlay, setOverlay)

-- Match3.Grass（第 6 项前）

at :: Board -> Pos -> Cell
at = boardAt

setAt :: Board -> Pos -> Cell -> Board
setAt = boardSet

ortho :: Board -> Pos -> [Pos]
ortho b (r, c) =
  filter (inBounds b) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | Strip Grass/Vine/Choco on *true clear* cells (so they will not spread).
-- Callers must pass iceFree/trueClears — not raw expand seeds — so soft hits
-- (ice>1 chip / Flip / Chain·Curtain peel) keep on-cell overlays.
-- Does NOT strip peel-locks (Chain/Curtain/Fog/Freeze/Steam) — those peel via
-- chipAdjacent* or direct-hit peel in chipIceOnClear (hammer/cross/line).
clearOverlaysOn :: Board -> [Pos] -> Board
clearOverlaysOn b seeds = foldl strip b (nub seeds)
  where
    strip board p =
      case at board p of
        Gem col kind ice (Just Grass) ->
          setAt board p (Gem col kind ice Nothing)
        Gem col kind ice (Just Vine) ->
          setAt board p (Gem col kind ice Nothing)
        Gem col kind ice (Just Choco) ->
          setAt board p (Gem col kind ice Nothing)
        _ -> board

-- | Chocolate (巧克力): cleared when orthogonally adjacent to a *true* clear hole.
-- Callers must pass iceFree/surpFree (not raw expand seeds): ice-chip / Flip soft
-- hits must not extinguish chocolate. Gem under chocolate stays.
clearChocoAdjacent :: Board -> [Pos] -> Board
clearChocoAdjacent b seeds =
  foldl strip b targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , hasChoco (at b q)
        ]
    strip board p =
      case at board p of
        Gem col kind ice (Just Choco) ->
          setAt board p (Gem col kind ice Nothing)
        _ -> board

-- | Fog / cloud (迷雾): peel one layer on cells orthogonally adjacent to clears.
-- Fog 1 -> strip; Fog n>1 -> Fog (n-1). Gem stays. Returns fully cleared fog count.
chipAdjacentFog :: Board -> [Pos] -> (Board, Int)
chipAdjacentFog b seeds = chipAdjacentFogExcept b seeds []

-- | Like chipAdjacentFog but skips cells in 'except' (already direct-hit this wave).
chipAdjacentFogExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentFogExcept b seeds except =
  foldl hit (b, 0) targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , q `notElem` except
        , hasFog (at b q)
        ]
    hit (board, n) p =
      case at board p of
        Gem col kind ice (Just (Fog layers))
          | layers <= 1 ->
              (setAt board p (Gem col kind ice Nothing), n + 1)
          | otherwise ->
              (setAt board p (Gem col kind ice (Just (Fog (layers - 1)))), n)
        _ -> (board, n)

vinePositions :: Board -> [Pos]
vinePositions b =
  [ (r, c)
  | (r, c) <- boardPositions b
  , hasVine (at b (r, c))
  ]

chocoPositions :: Board -> [Pos]
chocoPositions b =
  [ (r, c)
  | (r, c) <- boardPositions b
  , hasChoco (at b (r, c))
  ]

fogPositions :: Board -> [Pos]
fogPositions b =
  [ (r, c)
  | (r, c) <- boardPositions b
  , hasFog (at b (r, c))
  ]

-- | Chain / iron lock (锁链): peel one layer on cells orthogonally adjacent to clears.
-- Chain 1 -> strip; Chain n>1 -> Chain (n-1). Gem stays. Returns fully unlocked count.
chipAdjacentChain :: Board -> [Pos] -> (Board, Int)
chipAdjacentChain b seeds = chipAdjacentChainExcept b seeds []

-- | Like chipAdjacentChain but skips cells in 'except' (already direct-hit this wave).
chipAdjacentChainExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentChainExcept b seeds except =
  foldl hit (b, 0) targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , q `notElem` except
        , hasChain (at b q)
        ]
    hit (board, n) p =
      case at board p of
        Gem col kind ice (Just (Chain layers))
          | layers <= 1 ->
              (setAt board p (Gem col kind ice Nothing), n + 1)
          | otherwise ->
              (setAt board p (Gem col kind ice (Just (Chain (layers - 1)))), n)
        _ -> (board, n)

chainPositions :: Board -> [Pos]
chainPositions b =
  [ (r, c)
  | (r, c) <- boardPositions b
  , hasChain (at b (r, c))
  ]

-- | Rocket freeze (火箭冰冻): peel one layer on cells orthogonally adjacent to clears.
-- Freeze 1 -> strip; Freeze n>1 -> Freeze (n-1). Gem stays and can still match.
-- Returns fully thawed count.
chipAdjacentFreeze :: Board -> [Pos] -> (Board, Int)
chipAdjacentFreeze b seeds = chipAdjacentFreezeExcept b seeds []

-- | Like chipAdjacentFreeze but skips cells in 'except' (already direct-hit this wave).
chipAdjacentFreezeExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentFreezeExcept b seeds except =
  foldl hit (b, 0) targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , q `notElem` except
        , hasFreeze (at b q)
        ]
    hit (board, n) p =
      case at board p of
        Gem col kind ice (Just (Freeze layers))
          | layers <= 1 ->
              (setAt board p (Gem col kind ice Nothing), n + 1)
          | otherwise ->
              (setAt board p (Gem col kind ice (Just (Freeze (layers - 1)))), n)
        _ -> (board, n)

freezePositions :: Board -> [Pos]
freezePositions b =
  [ (r, c)
  | (r, c) <- boardPositions b
  , hasFreeze (at b (r, c))
  ]

-- | Curtain / roller shade (窗帘): peel one layer on cells orthogonally adjacent to clears.
-- Curtain 1 -> strip; Curtain n>1 -> Curtain (n-1). Gem stays. Returns fully cleared count.
chipAdjacentCurtain :: Board -> [Pos] -> (Board, Int)
chipAdjacentCurtain b seeds = chipAdjacentCurtainExcept b seeds []

-- | Like chipAdjacentCurtain but skips cells in 'except' (already direct-hit this wave).
chipAdjacentCurtainExcept :: Board -> [Pos] -> [Pos] -> (Board, Int)
chipAdjacentCurtainExcept b seeds except =
  foldl hit (b, 0) targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , q `notElem` except
        , hasCurtain (at b q)
        ]
    hit (board, n) p =
      case at board p of
        Gem col kind ice (Just (Curtain layers))
          | layers <= 1 ->
              (setAt board p (Gem col kind ice Nothing), n + 1)
          | otherwise ->
              (setAt board p (Gem col kind ice (Just (Curtain (layers - 1)))), n)
        _ -> (board, n)

curtainPositions :: Board -> [Pos]
curtainPositions b =
  [ (r, c)
  | (r, c) <- boardPositions b
  , hasCurtain (at b (r, c))
  ]

-- | Each remaining vine spreads onto every orthogonally adjacent bare gem
-- (no overlay, not stone). Newly placed vines do not chain-spread this turn.
spreadVines :: Board -> Board
spreadVines b =
  let sources = vinePositions b
      targets =
        nub
          [ q
          | p <- sources
          , q <- ortho b p
          , case at b q of
              Gem _ _ _ Nothing -> True
              _ -> False
          ]
  in foldl plant b targets
  where
    plant board p =
      case at board p of
        Gem col kind ice Nothing ->
          setAt board p (Gem col kind ice (Just Vine))
        _ -> board

-- | Surviving chocolate spreads onto adjacent bare gems (same rules as vine).
-- Cleared chocolate is already stripped, so it cannot seed new spreads.
spreadChoco :: Board -> Board
spreadChoco b =
  let sources = chocoPositions b
      targets =
        nub
          [ q
          | p <- sources
          , q <- ortho b p
          , case at b q of
              Gem _ _ _ Nothing -> True
              _ -> False
          ]
  in foldl plant b targets
  where
    plant board p =
      case at board p of
        Gem col kind ice Nothing ->
          setAt board p (Gem col kind ice (Just Choco))
        _ -> board

steamPositions :: Board -> [Pos]
steamPositions b =
  [ (r, c)
  | (r, c) <- boardPositions b
  , hasSteam (at b (r, c))
  ]

-- | Steam (蒸汽): extinguished when orthogonally adjacent to a *true* clear hole.
-- Same seed discipline as clearChocoAdjacent (no soft-hit extinguish).
clearSteamAdjacent :: Board -> [Pos] -> Board
clearSteamAdjacent b seeds =
  foldl strip b targets
  where
    targets =
      nub
        [ q
        | p <- nub seeds
        , q <- ortho b p
        , hasSteam (at b q)
        ]
    strip board p =
      case at board p of
        Gem col kind ice (Just Steam) ->
          setAt board p (Gem col kind ice Nothing)
        _ -> board

-- | Surviving steam spreads onto adjacent bare gems (same rules as vine/choco).
spreadSteam :: Board -> Board
spreadSteam b =
  let sources = steamPositions b
      targets =
        nub
          [ q
          | p <- sources
          , q <- ortho b p
          , case at b q of
              Gem _ _ _ Nothing -> True
              _ -> False
          ]
  in foldl plant b targets
  where
    plant board p =
      case at board p of
        Gem col kind ice Nothing ->
          setAt board p (Gem col kind ice (Just Steam))
        _ -> board


-- Match3.Types.Overlay（第 6 项前）

hasGrass :: Cell -> Bool
hasGrass c = cellOverlay c == Just Grass

hasVine :: Cell -> Bool
hasVine c = cellOverlay c == Just Vine

hasChoco :: Cell -> Bool
hasChoco c = cellOverlay c == Just Choco

hasFog :: Cell -> Bool
hasFog c = case cellOverlay c of
  Just (Fog _) -> True
  _ -> False

fogLayers :: Cell -> Int
fogLayers c = case cellOverlay c of
  Just (Fog n) -> n
  _ -> 0

hasChain :: Cell -> Bool
hasChain c = case cellOverlay c of
  Just (Chain _) -> True
  _ -> False

chainLayers :: Cell -> Int
chainLayers c = case cellOverlay c of
  Just (Chain n) -> n
  _ -> 0

hasFreeze :: Cell -> Bool
hasFreeze c = case cellOverlay c of
  Just (Freeze _) -> True
  _ -> False

freezeLayers :: Cell -> Int
freezeLayers c = case cellOverlay c of
  Just (Freeze n) -> n
  _ -> 0

hasCurtain :: Cell -> Bool
hasCurtain c = case cellOverlay c of
  Just (Curtain _) -> True
  _ -> False

curtainLayers :: Cell -> Int
curtainLayers c = case cellOverlay c of
  Just (Curtain n) -> n
  _ -> 0

-- | Strip overlay, keep gem/ice.
clearOverlay :: Cell -> Cell
clearOverlay (Gem col kind ice _) = Gem col kind ice Nothing
clearOverlay x = x

setOverlay :: Maybe CellOverlay -> Cell -> Cell
setOverlay o (Gem col kind ice _) = Gem col kind ice o
setOverlay _ x = x


hasSteam :: Cell -> Bool
hasSteam c = cellOverlay c == Just Steam

