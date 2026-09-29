{-# LANGUAGE ScopedTypeVariables #-}

-- | 测试辅助：多个测试模块共用的局面构造、查找与断言助手（原 test/Spec.hs 的非测试顶层定义，逐字搬运）。
module Spec.Support
  ( findNoMatchPair
  , findMatchPair
  , stuckNoMoveBoard
  , stableBoard
  , comboMoveState
  , noMatchSwap
  , noFx
  , withComboState
  , checkWaveChain
  , nonEmptyWaves
  , replayTimeline
  , effectName
  , checkEffectDetail
  , crateDef
  , crateBoard
  , moveApplied
  , cratesOn
  ) where

import Control.Monad (foldM)
import Data.List (nub)
import Match3.Core
import Match3.Element (ElementDef(edCounter, edFalls, edOnHit, edAdjacent), Counter(CountNamed), AdjacentRule(AdjacentRule), AdjCtx(acDirect, acTrue), AdjOut(AdjOut), HitResult(HitImmune, HitDestroy, HitAbsorb), baseDef)
import Match3.Types (isCustom)
import Test.Tasty.HUnit


findNoMatchPair :: Board -> Maybe (Pos, Pos)
findNoMatchPair b =
  case
    [ ((r, c), (r, c + 1))
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 2]
    , not (hasAnyMatch (swapCells b (r, c) (r, c + 1)))
    ] of
    (p : _) -> Just p
    [] -> Nothing

findMatchPair :: Board -> Maybe (Pos, Pos)
findMatchPair b =
  case
    [ ((r, c), (r, c + 1))
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 2]
    , hasAnyMatch (swapCells b (r, c) (r, c + 1))
    ]
      ++ [ ((r, c), (r + 1, c))
         | r <- [0 .. boardSize - 2]
         , c <- [0 .. boardSize - 1]
         , hasAnyMatch (swapCells b (r, c) (r + 1, c))
         ] of
    (p : _) -> Just p
    [] -> Nothing

-- | Cyclic (r+c) mod 5 board: stable and no valid adjacent swap.
stuckNoMoveBoard :: Board
stuckNoMoveBoard = boardFromRows $
  [ [ mkGem (toEnum ((r + c) `mod` 5))
    | c <- [0 .. boardSize - 1]
    ]
  | r <- [0 .. boardSize - 1]
  ]

--------------------------------------------------------------------------------
-- Stone blockers
--------------------------------------------------------------------------------

-- | Stable board with no accidental 3-runs.
stableBoard :: Board
stableBoard = boardFromRows $
  [ map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
  , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
  , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
  , map mkGem [C4, C5, C1, C2, C3, C4, C5, C1]
  , map mkGem [C5, C1, C2, C3, C4, C5, C1, C2]
  , map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
  , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
  , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
  ]

--------------------------------------------------------------------------------
-- 爆击（连击）特效不重播：失败操作必须清掉上一步的 UI 反馈
--------------------------------------------------------------------------------

-- | 第 1 关若干种子里找一步「连击 >= 2」的交换，返回交换后的状态（固定、可复现）。
comboMoveState :: Maybe (GameState, GameState)
comboMoveState =
  case
    [ (gs0, gs1)
    | seed <- [1 .. 400 :: Int]
    , let gs0 = newGameAtLevel 0 (levelConfig (head allLevels)) seed
    , r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , p2 <- [(r, c + 1), (r + 1, c)]
    , inBounds p2
    , let (gs1, out) = trySwap (r, c) p2 gs0
    , isApplied out
    , gsCombo gs1 >= 2
    ] of
    (x : _) -> Just x
    [] -> Nothing
  where
    isApplied (MoveApplied _) = True
    isApplied _ = False

-- | 当前盘面上一对「交换后 NoMatch」的相邻格。
noMatchSwap :: GameState -> Maybe (Pos, Pos)
noMatchSwap gs =
  case
    [ (p1, p2)
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 2]
    , let p1 = (r, c)
          p2 = (r, c + 1)
    , snd (trySwap p1 p2 gs) == NoMatch
    ] of
    (x : _) -> Just x
    [] -> Nothing

noFx :: MoveFx
noFx = MoveFx 0 []

withComboState :: (GameState -> GameState -> Assertion) -> Assertion
withComboState k = case comboMoveState of
  Nothing -> assertFailure "need a cascading (combo >= 2) move"
  Just (gs0, gs1) -> do
    let fx1 = moveFx gs0 gs1 (MoveApplied 0)
    assertBool "combo move fires combo fx" (fxCombo fx1 >= 2)
    assertBool "combo move has clear sites" (not (null (fxCleared fx1)))
    k gs0 gs1

--------------------------------------------------------------------------------
-- 逐轮回放（trace*）：只记录快照，结果必须与结算函数完全一致
--------------------------------------------------------------------------------

-- | 逐轮回放脚本的通用一致性检查：轮与轮首尾相接、得分之和、清除格并集。
checkWaveChain :: String -> Board -> [CascadeWave] -> Board -> Assertion
checkWaveChain tag start ws final = do
  case ws of
    [] -> start @?= final
    (w : _) -> assertEqual (tag ++ ": first wave starts from start board") start (cwBefore w)
  sequence_
    [ assertEqual (tag ++ ": wave " ++ show i ++ " holes -> after keeps shape") boardSize (length (cwHoles a))
    | (i, a) <- zip [1 :: Int ..] ws
    ]
  case reverse ws of
    [] -> pure ()
    (lastW : _) -> assertEqual (tag ++ ": last wave ends at final board") final (cwAfter lastW)
  sequence_
    [ assertEqual (tag ++ ": wave " ++ show i ++ " continues from previous") (cwAfter a) (cwBefore b)
    | (i, (a, b)) <- zip [2 :: Int ..] (zip ws (drop 1 ws))
    ]

nonEmptyWaves :: [CascadeWave] -> Int
nonEmptyWaves = length . filter (not . null . cwCleared)

-- | 按时间线重放一步：轮 0..k-1 → esAfterWaves == k 的步末效果 → 轮 k …，逐段首尾相接，
-- 每个步末效果用 applyEndEffect 重放得到 esAfter，最后到达 mtFinal。返回各类步末效果的名字。
replayTimeline :: String -> MoveTrace -> IO [String]
replayTimeline tag mt = go 0 (mtStart mt) (mtWaves mt) (mtEnd mt) []
  where
    go i cur ws ends acc = do
      let (now, later) = span ((== i) . esAfterWaves) ends
      cur' <-
        foldM
          ( \b e -> do
              assertEqual (tag ++ ": end step after wave " ++ show i ++ " starts from current board") b (esBefore e)
              assertEqual (tag ++ ": applyEndEffect reproduces esAfter") (esAfter e) (applyEndEffect (esEffect e) (esBefore e))
              assertBool (tag ++ ": end step changes the board") (esBefore e /= esAfter e)
              checkEffectDetail tag e
              pure (esAfter e)
          )
          cur
          now
      let acc' = acc ++ map (effectName . esEffect) now
      case ws of
        [] -> do
          assertBool (tag ++ ": no end step left after last wave") (null later)
          assertEqual (tag ++ ": timeline reaches mtFinal") (mtFinal mt) cur'
          pure acc'
        (w : rest) -> do
          assertEqual (tag ++ ": wave " ++ show i ++ " starts from current board") cur' (cwBefore w)
          go (i + 1) (cwAfter w) rest later acc'

effectName :: EndEffect -> String
effectName e = case e of
  EndCountdownTick _ -> "tick"
  EndBeltShift _ -> "belt"
  EndSpread k _ -> show k
  EndSnail _ -> "snail"

-- | 细节自洽：蔓延来源正交相邻且之前就带该覆盖层；蜗牛只走一格或原地掉头；皮带 / 倒计时格真的变了。
checkEffectDetail :: String -> EndStep -> Assertion
checkEffectDetail tag e = case esEffect e of
  EndSpread k pairs -> do
    let ov = case k of
          SpreadVine -> Vine
          SpreadChoco -> Choco
          SpreadSteam -> Steam
    sequence_
      [ do
          assertBool (tag ++ ": spread source adjacent " ++ show (src, q)) (adjacent src q)
          assertEqual (tag ++ ": spread source had overlay") (Just ov) (cellOverlay (getCell (esBefore e) src))
          assertEqual (tag ++ ": spread target was bare") Nothing (cellOverlay (getCell (esBefore e) q))
      | (src, q) <- pairs
      ]
  EndSnail ms ->
    sequence_
      [ assertBool (tag ++ ": snail moves at most one cell " ++ show m) (smFrom m == smTo m || adjacent (smFrom m) (smTo m))
      | m <- ms
      ]
  EndBeltShift mv -> assertBool (tag ++ ": belt moves listed") (not (null mv))
  EndCountdownTick ps ->
    sequence_
      [ assertBool (tag ++ ": countdown ticked at " ++ show p) (getCell (esBefore e) p /= getCell (esAfter e) p) | p <- ps ]

--------------------------------------------------------------------------------
-- 第二刀 2b：元素框架

-- | 测试专用元素「木箱」（只在测试里定义，主流程源码里没有它）：Custom "crate" n，n = 剩余耐久。
-- 定义：挡交换（baseDef 缺省）、不随重力下落、被邻格真消除波及一次耐久 -1、耐久 1 时再被波及就碎
-- （并入清除格，计数 CountNamed "crate"）；直接命中（锤子 / 爆炸）同样 -1 / 碎。
crateDef :: ElementDef
crateDef =
  (baseDef "crate")
    { edFalls = False
    , edOnHit = \cell -> case cell of
        Custom _ n | n <= 1 -> HitDestroy
                   | otherwise -> HitAbsorb (Custom "crate" (n - 1))
        _ -> HitImmune
    , edAdjacent = Just (AdjacentRule 200 crateAdjacent)
    , edCounter = Just (CountNamed "crate")
    }
  where
    isCrate c = case c of
      Custom "crate" _ -> True
      _ -> False
    crateAdjacent ctx b =
      let targets =
            nub [q | p <- acTrue ctx, q <- orthoNeighbors p, inBounds q, q `notElem` acDirect ctx, isCrate (getCell b q)]
          hit (bd, dead) q = case getCell bd q of
            Custom _ n | n <= 1 -> (bd, dead ++ [q])
                       | otherwise -> (setCell bd q (Custom "crate" (n - 1)), dead)
            _ -> (bd, dead)
          (b', dead') = foldl hit (b, []) targets
      in AdjOut b' dead' []

-- | 木箱局面：(0,1) 放木箱；交换 (1,2)↔(2,2) 在第 1 行凑出 C5 连消，(1,1) 与木箱正交相邻。
crateBoard :: Int -> Board
crateBoard durability =
  foldl (\b (p, c) -> setCell b p c) stableBoard
    [((1, 0), mkGem C5), ((1, 1), mkGem C5), ((0, 1), Custom "crate" durability)]

-- | 这一手是否真正结算（不是 NoMatch / InvalidSwap）。
moveApplied :: Outcome -> Bool
moveApplied o = o /= NoMatch && o /= InvalidSwap

cratesOn :: Board -> [(Pos, Cell)]
cratesOn b = [((r, c), cell) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1], let cell = getCell b (r, c), isCustom cell]
