{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | 测试辅助：多个测试模块共用的局面构造、查找与断言助手（tripleBoard / tripleMove / allPos / isWin / firstWave / firstLevel …），
-- 以及固定例子测试用的指纹 'digest'。
-- 源码扫描工具在 "Spec.Support.Source"，这里一并导出。
module Spec.Support
  ( -- * 通用局面与查询
    allPos
  , setCells
  , customsOn
  , isCustomNamed
  , tripleBoard
  , tripleMove
  , isWin
  , firstLevel
  , levelAt
  , levelGame
  , firstWave
    -- * 原有助手
  , findNoMatchPair
  , findMatchPair
  , findMatchPairNaive
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
  , stepThenUndo
    -- * 固定例子的指纹
  , digest
    -- * 内置内容的数量清单
  , module Spec.Support.Inventory
    -- * 源码扫描
  , module Spec.Support.Source
  ) where
import Data.Proxy (Proxy(..))

import Control.Monad (foldM)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Word (Word64)
import Match3.Board.Default (hasAnyMatch)
import Match3.Game.Move (trySwap)
import Match3.Game.State (moveFx)
import Match3.Game.Trace (applyEndEffect)
import Match3.Obstacles (orthoNeighbors)
import Numeric (showHex)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Match3.Core
import Match3.Element.Phase
import Match3.Board.Grid (adjacent)
import Match3.Element.Builtin (defaultWorld)
import Match3.Game.Level (newGameAtLevel)
import Match3.Levels.Campaign (lookupLevel)
import Match3.Levels.Level (levelConfig)
import Match3.Types (boardSize)
import Match3.Board.Grid (inBounds, setCell, swapCells)
import Match3.Element (Def, AdjCtx(acDirect, acTrue), AdjOut(AdjOut), kindDef)
import Match3.Element.Kind (BoardPass(..), Kind(..), customPlace, fromCustom)
import Match3.Types (cellKind, cellOverlay, isCustom)
import Match3.Element.Event (EventKind(..))
import Engine.Game (Game(..), Step(..))
import Engine.History (History(..), Undoable(..), startHistory)
import Match3.Element.World (World, swapBlockedWith)
import qualified Match3.Engine as M3E
import Test.Tasty.HUnit
import Spec.Support.Inventory
import Spec.Support.Source

-- | 棋盘全部格子（行优先）。
allPos :: [Pos]
allPos = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

-- | 依次写入若干格。
setCells :: Board -> [(Pos, Cell)] -> Board
setCells = foldl (\b (p, c) -> setCell b p c)

-- | 盘上名字为 n 的自定义格位置（行优先）。
customsOn :: ElementName -> Board -> [Pos]
customsOn n b = [p | p <- allPos, isCustomNamed n (getCell b p)]

-- | 是否是名字为 n 的自定义格。
isCustomNamed :: ElementName -> Cell -> Bool
isCustomNamed n cell = case cell of
  Custom m _ -> m == n
  _ -> False

-- | 第 1 行 (1,0)(1,1) 为 C5 的稳定盘：交换 (1,2)↔(2,2) 后第 1 行 (1,0)–(1,3) 是 C5 四连（(1,3) 本来就是 C5）。
tripleBoard :: Board
tripleBoard = setCells stableBoard [((1, 0), mkGem C5), ((1, 1), mkGem C5)]

tripleMove :: (Pos, Pos)
tripleMove = ((1, 2), (2, 2))

-- | 结局是否是过关（Won / LevelClear）。
isWin :: Maybe Terminal -> Bool
isWin o = case o of
  Just (TWon _) -> True
  Just (TLevelClear _ _) -> True
  _ -> False

-- | 战役第 1 关（allLevels 的第一项；关卡表为空时直接报错）。
firstLevel :: HasCallStack => Level
firstLevel = levelAt 0

-- | 第 li 关（0 基）的关卡记录；没有这一关直接报错。
levelAt :: HasCallStack => Int -> Level
levelAt li = fromMaybe (error ("levelAt: no level " ++ show li)) (lookupLevel li)

-- | 第 li 关按该关步数与目标、给定种子开局（= newGameAtLevel li (levelConfig (levelAt li)) seed）。
levelGame :: HasCallStack => Int -> Int -> GameState
levelGame li seed = fromMaybe (error ("levelGame: no level " ++ show li)) (campaignGame li seed)

-- | 走步报告的第一轮连锁；没有任何一轮时断言失败。
firstWave :: HasCallStack => MoveTrace -> IO CascadeWave
firstWave mt = case mtWaves mt of
  (w : _) -> pure w
  [] -> assertFailure "expected at least one cascade wave"

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

-- | 第一对「引擎会接受」的相邻交换（先横后竖、行优先）：两格都不挡交换（'swapBlockedWith'，同 'resolveSwap'
-- 的拒绝条件），且交换后有三连。旧版只看交换后是否成消，在第 43 关会选中「宝石 × 毛球」这种被引擎以 NoMatch
-- 拒绝的对（毛球是 Blocker）；回归见 GoalsLevels.find_match_pair_engine_accepts。
findMatchPair :: Board -> Maybe (Pos, Pos)
findMatchPair b =
  case filter accepted (horizontalPairs ++ verticalPairs) of
    (p : _) -> Just p
    [] -> Nothing
  where
    accepted (p1, p2) = not (swapBlockedWith defaultWorld b p1 p2) && hasAnyMatch (swapCells b p1 p2)

-- | 旧版 'findMatchPair'（不查能否交换），只给回归测试对照用。
findMatchPairNaive :: Board -> Maybe (Pos, Pos)
findMatchPairNaive b =
  case [pq | pq@(p1, p2) <- horizontalPairs ++ verticalPairs, hasAnyMatch (swapCells b p1 p2)] of
    (p : _) -> Just p
    [] -> Nothing

-- | 全部相邻格对：先横向（行优先），再纵向。
horizontalPairs, verticalPairs :: [(Pos, Pos)]
horizontalPairs = [((r, c), (r, c + 1)) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 2]]
verticalPairs = [((r, c), (r + 1, c)) | r <- [0 .. boardSize - 2], c <- [0 .. boardSize - 1]]

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
    , let gs0 = newGameAtLevel 0 (levelConfig firstLevel) seed
    , r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 1]
    , p2 <- [(r, c + 1), (r + 1, c)]
    , fst p2 >= 0 && fst p2 < boardSize && snd p2 >= 0 && snd p2 < boardSize
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
    [ assertEqual (tag ++ ": wave " ++ show i ++ " holes -> after keeps shape") boardSize (length (mboardRows (cwHoles a)))
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

-- | 步末效果的名字 = 元素名（countdown / belt / vine / choco / steam / snail）。
effectName :: EndEffect -> String
effectName = unElementName . endEffectElement

-- | 细节自洽：蔓延来源正交相邻且之前就带该覆盖层（魔力鸟组合的变身：来源是彩虹、目标由同色普通宝石变成特效）；蜗牛只走一格或原地掉头；皮带 / 倒计时格真的变了。
checkEffectDetail :: String -> EndStep -> Assertion
checkEffectDetail tag e = case endEffectKind eff of
  EvSpread
    | endEffectElement eff `elem` ["rainbow_line", "rainbow_bomb"] ->
        -- 新玩法 4：魔力鸟组合的变身（第 0 轮之前）：来源是彩虹，目标原是同色普通宝石、变成直线 / 炸弹
        sequence_
          [ do
              assertBool (tag ++ ": morph source is the rainbow " ++ show src) (cellKind (getCell (esBefore e) src) == Just Rainbow)
              assertEqual (tag ++ ": morph target was a plain gem " ++ show q) (Just Normal) (cellKind (getCell (esBefore e) q))
              assertEqual (tag ++ ": morph keeps the colour " ++ show q) (gemColorOf (getCell (esBefore e) q)) (gemColorOf (getCell (esAfter e) q))
              assertBool (tag ++ ": morph target became a line / bomb " ++ show q) (cellKind (getCell (esAfter e) q) `elem` map Just [LineH, LineV, Bomb])
          | (src, q) <- endEffectPairs eff
          ]
  EvSpread -> do
    let ov = case endEffectElement eff of
          "vine" -> Vine
          "choco" -> Choco
          _ -> Steam
    sequence_
      [ do
          assertBool (tag ++ ": spread source adjacent " ++ show (src, q)) (adjacent src q)
          assertEqual (tag ++ ": spread source had overlay") (Just ov) (cellOverlay (getCell (esBefore e) src))
          assertEqual (tag ++ ": spread target was bare") Nothing (cellOverlay (getCell (esBefore e) q))
      | (src, q) <- endEffectPairs eff
      ]
  EvMove ->
    sequence_
      [ assertBool (tag ++ ": snail moves at most one cell " ++ show m) (eiFrom m == eiTo m || adjacent (eiFrom m) (eiTo m))
      | m <- endEffectItems eff
      ]
  EvBelt -> assertBool (tag ++ ": belt moves listed") (not (null (endEffectItems eff)))
  EvTick ->
    sequence_
      [ assertBool (tag ++ ": countdown ticked at " ++ show p) (getCell (esBefore e) p /= getCell (esAfter e) p) | p <- map eiTo (endEffectItems eff) ]
  k -> assertFailure (tag ++ ": unexpected end effect kind " ++ show k)
  where
    eff = esEffect e
    gemColorOf cell = case cell of
      Gem c _ _ _ -> Just c
      _ -> Nothing

--------------------------------------------------------------------------------
-- 元素框架

-- | 测试专用元素「木箱」（只在测试里定义，主流程源码里没有它）：Custom "crate" n，n = 剩余耐久。
-- 定义：固定格（挡交换、不随重力下落）、被邻格真消除波及一次耐久 -1、耐久 1 时再被波及就碎
-- （并入清除格，计数 CountNamed "crate"）；直接命中（锤子 / 爆炸）同样 -1 / 碎。状态（耐久）在元素值里。
newtype Crate = Crate Int
  deriving (Eq, Show)

instance Phase Crate where
  codec = Codec
    { cName = "crate"
    , cToCell = intCell "crate"
    , cFromCell = const Nothing
    , cPlace = \_ _ -> Nothing
    , cMeta = emptyMeta { metaCounter = Just (CountNamed "crate") }
    , cNear = Nothing
    }
  onMatch _ = obstacleMatch
  onHit _ (Crate n) = HitOut (if n <= 1 then Destroy else Absorb (toCell (Crate (n - 1)))) False Nothing Nothing
  physics _ = fixedPhysics
  view _ = noFace

instance Kind Crate where
  kindName _ = "crate"
  fromCell = fromCustom "crate" Crate
  place _ = customPlace "crate"
  boardPasses _ = [AdjacentPass 200 crateAdjacent]

-- | 木箱的邻格规则：真消除格的正交邻格里的木箱（直接命中格除外）耐久 -1，耐久 1 的碎掉。
crateAdjacent :: AdjCtx -> Board -> AdjOut
crateAdjacent ctx b =
  let isCrate c = case c of
        Custom "crate" _ -> True
        _ -> False
      targets =
        nub [q | p <- acTrue ctx, q <- orthoNeighbors p, inBounds b q, q `notElem` acDirect ctx, isCrate (getCell b q)]
      bump (bd, dead) q = case getCell bd q of
        Custom _ (CustomState n) | n <= 1 -> (bd, dead ++ [q])
                   | otherwise -> (setCell bd q (Custom "crate" (CustomState (n - 1))), dead)
        _ -> (bd, dead)
      (b', dead') = foldl bump (b, []) targets
  in AdjOut b' dead' []

crateDef :: Def
crateDef = kindDef @Crate

-- | 木箱局面：(0,1) 放木箱；交换 (1,2)↔(2,2) 在第 1 行凑出 C5 连消，(1,1) 与木箱正交相邻。
crateBoard :: Int -> Board
crateBoard durability =
  setCells stableBoard [((1, 0), mkGem C5), ((1, 1), mkGem C5), ((0, 1), Custom "crate" (CustomState durability))]

-- | 这一手是否真正结算（不是 NoMatch / InvalidSwap）。
moveApplied :: Outcome -> Bool
moveApplied o = o /= NoMatch && o /= InvalidSwap

cratesOn :: Board -> [(Pos, Cell)]
cratesOn b = [(p, cell) | p <- allPos, let cell = getCell b p, isCustom cell]

-- | 撤销只在通用历史层（Engine.History）。从 gs 经带历史的通用接口 match3ShellWith world 执行一个动作，
-- 再执行 Undo，返回撤销后的状态；走步或撤销被拒时 Nothing。
stepThenUndo :: World -> GameState -> M3E.Action -> Maybe GameState
stepThenUndo world gs act =
  let g = M3E.match3ShellWith world
      s1 = gameStep g (startHistory gs) (Act act)
      s2 = gameStep g (stepState s1) Undo
  in if stepAccepted s1 && stepAccepted s2 then Just (histNow (stepState s2)) else Nothing

--------------------------------------------------------------------------------
-- 固定例子的指纹

-- | 长输出的指纹（FNV-1a 64 位，按 Char 码点逐个折叠，16 位十六进制）。固定例子测试用它把成百上千个结果
-- （盘面、回放、生成器的 show）写成一个字面量：结果有任何一个字符不同，指纹就不同。
digest :: String -> String
digest s = let h = foldl step 14695981039346656037 s in replicate (16 - length (showHex h "")) '0' ++ showHex h ""
  where
    step :: Word64 -> Char -> Word64
    step acc ch = (acc `xor` fromIntegral (ord ch)) * 1099511628211
