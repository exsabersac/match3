{-# LANGUAGE NamedFieldPuns #-}

-- | 开局与关卡装饰：新局 / 指定关 / 每日 / 重开 / 下一关，按关卡记录铺装饰（decorateLevel）、
-- 目标所需装饰补齐、皮带 / 传送门 / 飞碟 / 地毯 / 地面层取自关卡记录、步数携带。
-- 第 6 刀：各关的这些数据都在 Match3.Levels.Campaign 的关卡记录里（之前是本模块里按下标 case 的并行表）；
-- 放置表经 placeAllWith 返回 Either，静态数据在 placeStatic 这一处边界上转成带关卡名的 error。
--
-- 依赖：State、Shuffle（开局 ensurePlayable）、Match3.Board.Random、Match3.Levels.*、元素注册表（装饰按元素名放置）。
-- 不变量：同一 (关卡, 种子) 总得到同一开局（随机数只来自 mkStdGen seed）。
module Match3.Game.Level
  ( newGame
  , placeStatic
  , decorateLevel
  , decorateLevelWith
  , ensureGoalDecor
  , goalDecorWith
  , campaignGame
  , newGameAtLevel
  , newDailyGame
  , restart
  , restartLevel
  , carryMovesBonus
  , nextLevel
  ) where

import Data.Maybe (fromMaybe)
import Match3.Board.Random (randomPlayableBoard)
import Match3.Ufo (mkUfo)
import Match3.Counts (CounterKey(..), noCounts)
import Match3.Element.Builtin (defaultRegistry)
import Match3.Element.Registry (PlaceError, Registry, countElementWith, placeAllWith)
import Match3.Element.Types (Arg(..), Placement(..))
import Match3.Levels.Campaign (clampLevelIndex, lookupLevel)
import Match3.Levels.Level
import Match3.Types
import System.Random (mkStdGen)
import Match3.Game.Shuffle
import Match3.Game.State

-- | 按给定配置与种子开局，使用第 1 关（下标 0）的装饰。
newGame :: GameConfig -> Int -> GameState
newGame = newGameAtLevel 0

-- | 静态关卡数据的边界（第 6 刀）：放置表写错（未注册的元素名 / 越界格）说明关卡数据有误，这里带上是哪份数据报错。
-- 内置关卡与每日挑战都放置成功由测试锁定（level_placements_all_right）。
placeStatic :: String -> Either PlaceError Board -> Board
placeStatic what = either (\e -> error ("关卡数据有误（" ++ what ++ "）：" ++ show e)) id

-- | 某关的装饰（按放置表；第二刀 2b 起按元素名引用，由元素条目的放置函数解释；第 6 刀起放置表在关卡记录里）。
decorateLevel :: Level -> Board -> Board
decorateLevel l =
  placeStatic ("第 " ++ show (lvlIndex l + 1) ++ " 关「" ++ lvlName l ++ "」的装饰") . decorateLevelWith defaultRegistry l

-- | decorateLevel（指定注册表），失败时返回 Left。
decorateLevelWith :: Registry -> Level -> Board -> Either PlaceError Board
decorateLevelWith reg l b = placeAllWith reg b (lvlPlacements l)

-- | Daily (and any bare board) must still be completable: if the goal needs
-- board entities but level décor did not place enough, seed a minimal set.
-- 第二刀 2b：目标 → (元素名, 放置表)；计数按元素名（countElementWith）。
ensureGoalDecor :: LevelGoal -> Board -> Board
ensureGoalDecor goal = placeStatic ("目标 " ++ show goal ++ " 的补齐装饰") . goalDecorWith defaultRegistry goal

-- | ensureGoalDecor（指定注册表），失败时返回 Left。
goalDecorWith :: Registry -> LevelGoal -> Board -> Either PlaceError Board
goalDecorWith reg goal b =
  case goalDecor goal of
    Just (name, n, placements)
      | countElementWith reg name b < n -> placeAllWith reg b placements
    _ -> Right b
  where
    slots7 = [(2, 2), (2, 5), (4, 1), (4, 3), (4, 5), (6, 2), (6, 5)]
    layered name n = Just (name, n, [Place name [AInt 1] (take (max n 6) slots7)])
    goalDecor g = case goalView g of
      ViewCount CountStones n -> Just ("stone", n, [Place "stone" [] (take (max n 6) [(4, 1), (4, 3), (4, 5), (5, 2), (5, 4), (6, 1), (6, 3), (6, 5)])])
      ViewCount CountHoney n -> layered "honey" n
      ViewCount CountChests n -> layered "chest" n
      ViewCount CountCakes n -> layered "cake" n
      ViewCount CountSafes n -> layered "safe" n
      ViewCount CountBalloons n ->
        Just ("balloon", n, placeEach "balloon" (\c -> [AColor c]) (take (max n 6) (zip slots7 [C1, C3, C2, C1, C4, C3, C5])))
      -- Cookies high on board so clearing below can drop them to the exit row
      ViewCount CountCookies n -> Just ("cookie", n, [Place "cookie" [] (take (max n 6) [(0, 1), (0, 3), (0, 5), (1, 2), (1, 4), (1, 6), (2, 1), (2, 3), (2, 5)])])
      _ -> Nothing

-- | 指定关卡（0 基下标）+ 配置 + 种子开局：可玩随机盘 → 关卡装饰 → 目标所需装饰，
-- 填好皮带 / 传送门 / 飞碟 / 地毯 / 地面层字段 → ensurePlayable。同一 (关卡, 种子) 总是同一开局。
-- 装饰与关卡级元素取自 lookupLevel li 的记录（下标越界 = 没有装饰，只按目标补齐）；步数与目标取自 cfg。
newGameAtLevel :: Int -> GameConfig -> Int -> GameState
newGameAtLevel li cfg seed =
  let g0 = mkStdGen seed
      (board0, g1) = randomPlayableBoard g0
      lvl = lookupLevel li
      field f = maybe [] f lvl
      board = ensureGoalDecor (cfgGoal cfg) (maybe id decorateLevel lvl board0)
      gs0 =
        GameState
          { gsBoard = board
          , gsScore = 0
          , gsMoves = cfgMoves cfg
          , gsGoal = cfgGoal cfg
          , gsCounts = noCounts
          , gsGen = g1
          , gsOver = Nothing
          , gsLevel = li
          , gsHint = Nothing
          , gsCombo = 0
          , gsShuffled = False
          , gsBelts = field lvlBelts
          , gsPortals = field lvlPortals
          , gsHammers = 2
          , gsFreeSwaps = 1
          , gsCrossClears = 1
          , gsUfos =
              let placed = field lvlUfos
              in if null placed
                   then case goalView (cfgGoal cfg) of
                          ViewCount CountUfo _ -> [mkUfo (1, 3) C1]
                          _ -> []
                   else placed
          , gsCarpetOpen =
              let placed = field lvlCarpets
              in if null placed
                   then case goalView (cfgGoal cfg) of
                          ViewCount CountCarpets n ->
                            take (max n 1)
                              [ (3, 2), (3, 3), (3, 4), (3, 5)
                              , (4, 2), (4, 3), (4, 4), (4, 5)
                              , (2, 2), (2, 5), (5, 2), (5, 5)
                              ]
                          _ -> []
                   else placed
          , gsLastCleared = []
          , gsDaily = False
          , gsGround = field lvlGround
          }
  -- Décor can remove the only legal swap (e.g. dense 终章); auto-reshuffle gems.
  in ensurePlayable gs0

-- | Date-seeded daily challenge: same bare board as L0 décor, but clears as Won
-- (not LevelClear into campaign) and never bumps map unlock.
newDailyGame :: GameConfig -> Int -> GameState
newDailyGame cfg seed =
  (newGameAtLevel 0 cfg seed) { gsDaily = True }

-- | newGame 的别名（冻结 API）。
restart :: GameConfig -> Int -> GameState
restart = newGame

-- | 按关卡下标开战役局（该关的步数与目标）；没有这一关为 Nothing（第 6 刀：取代各处的 allLevels !! i）。
campaignGame :: Int -> Int -> Maybe GameState
campaignGame li seed = (\l -> newGameAtLevel (lvlIndex l) (levelConfig l) seed) <$> lookupLevel li

-- | 同一关换种子重开（关卡下标先夹到关卡表范围内；关卡表为空时按默认配置开局）。
restartLevel :: GameState -> Int -> GameState
restartLevel gs seed =
  fromMaybe (newGameAtLevel (gsLevel gs) defaultConfig seed) (campaignGame (clampLevelIndex (gsLevel gs)) seed)

-- | 战役过关剩余步携带入下一关，上限 3；与三星分母（印制步数）无关。
-- | Leftover moves carried into the next campaign level (cap 3).
carryMovesBonus :: MovesLeft -> MovesLeft
carryMovesBonus left = min 3 (max 0 left)


-- | 进入下一关：LevelClear 时取它给出的下一关并携带剩余步数（carryMovesBonus，最多 3 步）；
-- 其它情况进入当前关的下一关（不超过最后一关），不携带步数。
nextLevel :: GameState -> Int -> GameState
nextLevel gs seed =
  let idx = case gsOver gs of
        Just (LevelClear _ n) -> n
        _ -> clampLevelIndex (gsLevel gs + 1)
      bonus = case gsOver gs of
        Just (LevelClear _ _) -> carryMovesBonus (gsMoves gs)
        _ -> 0
      gs' = fromMaybe (newGameAtLevel idx defaultConfig seed) (campaignGame idx seed)
  in gs' { gsMoves = gsMoves gs' + bonus }
