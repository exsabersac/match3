{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | 开局与关卡装饰：新局 / 指定关 / 每日 / 重开 / 下一关，按关卡记录铺装饰（decorateLevel）、
-- 目标所需装饰补齐、关卡级元素（皮带 / 传送门 / 飞碟 / 地毯 / 地面层，在 gsLevelElems）由关卡记录开出、步数携带。
-- 各关的这些数据都在 Match3.Levels.Campaign 的关卡记录里；
-- 放置表经 placeAllWith 返回 Either，静态数据在 placeStatic 这一处边界上转成带关卡名的 error。
--
-- 依赖：State、Shuffle（开局 ensurePlayable）、Match3.Board.Random、Match3.Levels.*、元素元素世界（装饰按元素名放置）。
-- 不变量：同一 (关卡, 种子) 总得到同一开局（随机数只来自 mkStdGen seed）。
module Match3.Game.Level
  ( newGame
  , placeStatic
  , decorateLevel
  , decorateLevelWith
  , goalDecorWith
  , campaignGame
  , newGameAtLevel
  , newGameAtLevelWith
  , newGameForLevelWith
  , newDailyGame
  , restart
  , restartLevel
  , carryMovesBonus
  , nextLevel
  ) where

import Data.Maybe (fromMaybe)
import Engine.Optics (over)
import Match3.Board.Random (randomPlayableBoardSized)
import Match3.Counts (CounterKey(..), noCounts)
import Match3.Element.Level (startLevelsWith)
import Match3.Element.Builtin (defaultWorld)
import Match3.Element.World (PlaceError, World, countElementWith, placeAllWith)
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

-- | 静态关卡数据的边界：放置表写错（未注册的元素名 / 越界格）说明关卡数据有误，这里带上是哪份数据报错。
-- 内置关卡与每日挑战都放置成功由测试锁定（level_placements_all_right）。
placeStatic :: String -> Either PlaceError Board -> Board
placeStatic what = either (\e -> error ("关卡数据有误（" ++ what ++ "）：" ++ show e)) id

-- | 某关的装饰（关卡记录里的放置表；按元素名引用，由元素条目的放置函数解释）。
decorateLevel :: Level -> Board -> Board
decorateLevel l =
  placeStatic ("第 " ++ show (lvlIndex l + 1) ++ " 关「" ++ lvlName l ++ "」的装饰") . decorateLevelWith defaultWorld l

-- | decorateLevel（指定元素世界），失败时返回 Left。
decorateLevelWith :: World -> Level -> Board -> Either PlaceError Board
decorateLevelWith world l b = placeAllWith world b (lvlPlacements l)

-- | 目标所需的补齐装饰：每日挑战（以及任何裸盘面）也必须能完成——目标需要盘上实体、而关卡装饰放得不够时，
-- 按目标的放置表补一组最小的（目标 → (元素名, 个数, 放置表)；计数按元素名 countElementWith）。失败时返回 Left。
goalDecorWith :: World -> LevelGoal -> Board -> Either PlaceError Board
goalDecorWith world goal b =
  case goalDecor goal of
    Just (name, n, placements)
      | countElementWith world name b < n -> placeAllWith world b placements
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
-- 按关卡记录开出关卡级元素（gsLevelElems）→ ensurePlayable。同一 (关卡, 种子) 总是同一开局。
-- 装饰与关卡级元素取自 lookupLevel li 的记录（下标越界 = 没有装饰与关卡级元素，只按目标补齐）；步数与目标取自 cfg。
newGameAtLevel :: Int -> GameConfig -> Int -> GameState
newGameAtLevel = newGameAtLevelWith defaultWorld

-- | newGameAtLevel（指定元素世界）：装饰、目标补齐、可玩判定用这张表；关卡级元素 = 这张表里注册的各种 + 核心元素，
-- 各自由关卡记录（lvlGoal 换成本局目标）给出开局状态（飞碟 / 地毯的目标补齐在各自的 mechStart 里）。
newGameAtLevelWith :: World -> Int -> GameConfig -> Int -> GameState
newGameAtLevelWith world li cfg seed = newGameFromWith world li (lookupLevel li) cfg seed

-- | 按一份完整的关卡记录开局（不必在战役表里，例如关卡包 / 编辑器 / 测试）：步数与目标取自记录，
-- 装饰、关卡级元素、行列都按记录；gsLevel = lvlIndex（结局与下一关仍按战役下标判定）。
-- 开局前按 'assertLevel' 校验记录（同 lookupLevel）。对战役里的关，与 'campaignGame' 逐字相同（engine_level_setup_matches_campaign）。
newGameForLevelWith :: World -> Level -> Int -> GameState
newGameForLevelWith world l seed = newGameFromWith world (lvlIndex l) (Just (assertLevel l)) (levelConfig l) seed

-- | 开局的公共部分：关卡下标、关卡记录（Nothing = 没有装饰与关卡级元素，只按目标补齐）、配置、种子。
newGameFromWith :: World -> Int -> Maybe Level -> GameConfig -> Int -> GameState
newGameFromWith world li lvl cfg seed =
  let g0 = mkStdGen seed
      rows = maybe boardSize lvlRows lvl
      cols = maybe boardSize lvlCols lvl
      (board0, g1) = randomPlayableBoardSized rows cols g0
      decorate l = placeStatic ("第 " ++ show (lvlIndex l + 1) ++ " 关「" ++ lvlName l ++ "」的装饰") . decorateLevelWith world l
      -- 新玩法 6：有掉落口的关卡不做目标补齐（收集物由掉落口陆续补进场）；没有掉落口（lvlDrops = []）时照常补齐
      goalDecor
        | maybe False (not . null . lvlDrops) lvl = id
        | otherwise = placeStatic ("目标 " ++ show (cfgGoal cfg) ++ " 的补齐装饰") . goalDecorWith world (cfgGoal cfg)
      board = goalDecor (maybe id decorate lvl board0)
      start = (fromMaybe (level li "" (cfgMoves cfg) (cfgGoal cfg)) lvl) {lvlGoal = cfgGoal cfg}
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
          , gsHammers = 2
          , gsFreeSwaps = 1
          , gsCrossClears = 1
          , gsLastCleared = []
          , gsDaily = False
          , gsLevelElems = startLevelsWith world start
          }
  -- Décor can remove the only legal swap (e.g. dense 终章); auto-reshuffle gems.
  in ensurePlayableWith world gs0

-- | Date-seeded daily challenge: same bare board as L0 décor, but clears as Won
-- (not LevelClear into campaign) and never bumps map unlock.
newDailyGame :: GameConfig -> Int -> GameState
newDailyGame cfg seed =
  (newGameAtLevel 0 cfg seed) { gsDaily = True }

-- | newGame 的别名（冻结 API）。
restart :: GameConfig -> Int -> GameState
restart = newGame

-- | 按关卡下标开战役局（该关的步数与目标）；没有这一关为 Nothing。
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
        Just (TLevelClear _ n) -> n
        _ -> clampLevelIndex (gsLevel gs + 1)
      bonus = case gsOver gs of
        Just (TLevelClear _ _) -> carryMovesBonus (gsMoves gs)
        _ -> 0
      gs' = fromMaybe (newGameAtLevel idx defaultConfig seed) (campaignGame idx seed)
  in over gsMovesL (+ bonus) gs'
