-- | 第 11 刀：视图模型 Match3.View 与通用网格组件 Engine.GridUI。
--
-- 对照的旧实现是本文件里的字面副本：第 11 刀前 UI.Actions.updateTitle（标题）、UI.Input.collectMsg、
-- UI.HudArt / UI.HudBlocks（进度点、步数上限、道具、分数徽章、结局色条分支）、UI.BoardPrim / UI.BoardArt
-- （地毯标记）、web/hs/Match3Web/Api.hs（encodeState / encodeGoal / encodeCell / apiLevels 的现算式）、
-- UI.Layout.pixelToCell / cellOrigin / allCells、UI.Input 的点选 / 拖动判定。
-- 另有源码扫描：这些前端不再从 GameState 现算（读视图模型 / 通用网格组件）。
module Spec.View
  ( tests
  ) where

import Data.List (isInfixOf, nub)
import Engine.GridUI
import Match3.Core
import Match3.View
import Spec.Support (levelGame)
import Spec.Support.Source (mentionsIdent, readCode)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "view_fields_match_legacy_reads" view_fields_match_legacy_reads
  , testCase "view_title_and_bracket_match_legacy" view_title_and_bracket_match_legacy
  , testCase "view_carpet_marks_match_legacy" view_carpet_marks_match_legacy
  , testCase "view_level_dots_match_legacy" view_level_dots_match_legacy
  , testCase "view_score_badge_matches_legacy" view_score_badge_matches_legacy
  , testCase "view_cell_face_and_level_list_match_legacy" view_cell_face_and_level_list_match_legacy
  , testCase "grid_ui_geometry_matches_legacy_layout" grid_ui_geometry_matches_legacy_layout
  , testCase "grid_ui_click_drag_highlight" grid_ui_click_drag_highlight
  , testCase "frontends_read_view_model" frontends_read_view_model
  ]

--------------------------------------------------------------------------------
-- 样本局面：每关两个种子，按找到的提示连走几步；外加每日挑战、越界关卡号、改过道具 / 结局的局面。

playOn :: Int -> GameState -> [GameState]
playOn 0 gs = [gs]
playOn k gs = gs : case (gsOver gs, findHint (gsBoard gs)) of
  (Nothing, Just (a, b)) -> let (gs', _) = trySwap a b gs in playOn (k - 1) gs'
  _ -> []

samples :: [GameState]
samples =
  concat [playOn 4 (levelGame li seed) | li <- [0 .. levelCount - 1], seed <- [3, 11]]
    ++ playOn 3 (newDailyGame (dailyConfig 2026 9 30) (dailySeed 2026 9 30))
    ++ [ g0 {gsLevel = levelCount + 2}
       , g0 {gsHammers = 0, gsFreeSwaps = 5, gsCrossClears = 2, gsShuffled = True}
       , g0 {gsOver = Just (Won 1234)}
       , g0 {gsOver = Just (LevelClear 99 4)}
       , g0 {gsOver = Just (Lost 7), gsShuffled = True}
       , g0 {gsOver = Just (MoveApplied 5), gsShuffled = True}
       , g0 {gsMoves = 99}
       ]
  where
    g0 = levelGame 0 5

--------------------------------------------------------------------------------
-- 旧实现副本

legacyTitle :: GameState -> String
legacyTitle gs =
  let levelName = maybe "?" lvlName (lookupLevel (min (gsLevel gs) (levelCount - 1)))
      status = case gsOver gs of
        Just (Won s) -> " CLEAR! score=" <> show s
        Just (LevelClear s n) -> " LEVEL UP ->" <> show (n + 1) <> " score=" <> show s
        Just (Lost s) -> " LOSE score=" <> show s
        _ -> ""
      combo = gsCombo gs
      comboBits = if combo > 1 then "  combo x" ++ show combo else ""
      prog = gsProgress gs
      goalBits = case goalView (gsGoal gs) of
        ViewScore t -> "score=" ++ show prog ++ "/" ++ show t
        ViewCollect col n -> "collect " ++ colorTag col ++ "=" ++ show prog ++ "/" ++ show n
        ViewCollectMulti _ -> "multi " ++ show prog ++ "/" ++ show (goalTarget (gsGoal gs))
        ViewCount k n -> countTag k ++ "=" ++ show prog ++ "/" ++ show n
        ViewOther _ -> "goal=" ++ show prog ++ "/" ++ show (goalTarget (gsGoal gs))
  in "L" ++ show (gsLevel gs + 1) ++ " " ++ levelName ++ "  " ++ goalBits ++ "  moves=" ++ show (gsMoves gs)
       ++ comboBits ++ "  Hm=" ++ show (gsHammers gs) ++ " Sw=" ++ show (gsFreeSwaps gs)
       ++ " Cr=" ++ show (gsCrossClears gs) ++ status

legacyCollect :: GameState -> String
legacyCollect gs' = case goalView (gsGoal gs') of
  ViewCollect col n -> bracket (colorTag col) n
  ViewCollectMulti _ -> bracket "multi" (goalTarget (gsGoal gs'))
  ViewCount k n -> bracket (countTag k) n
  _ -> ""
  where
    bracket tag n = " [" ++ tag ++ " " ++ show (gsProgress gs') ++ "/" ++ show n ++ "]"

-- 几何版结局色条的分支（颜色换成标签）。
legacyStatusStrip :: GameState -> String
legacyStatusStrip gs = case gsOver gs of
  Just (Won _) -> "won"
  Just (LevelClear _ _) -> "clear"
  Just (Lost _) -> "lost"
  _ -> if gsShuffled gs then "shuffled" else "on"

stripOf :: PlayStatus -> String
stripOf st = case st of
  PlayWon _ -> "won"
  PlayCleared _ _ -> "clear"
  PlayLost _ -> "lost"
  PlayShuffled -> "shuffled"
  PlayOn -> "on"

legacyCellFields :: Cell -> [(String, CellField)]
legacyCellFields cell =
  let t x = ("t", FieldText x)
      n k = ("n", FieldInt k)
      col c = ("c", FieldInt (fromEnum c + 1))
      ovName ov = case ov of
        Grass -> "grass"; Vine -> "vine"; Choco -> "choco"; Fog _ -> "fog"; Chain _ -> "chain"
        Freeze _ -> "freeze"; Curtain _ -> "curtain"; Steam -> "steam"
      ovLayers ov = case ov of Fog k -> k; Chain k -> k; Freeze k -> k; Curtain k -> k; _ -> 0
      kc k = case k of Normal -> "N"; LineH -> "H"; LineV -> "V"; Bomb -> "B"; Rainbow -> "R"
  in case cell of
       Gem c k ice ov ->
         [ t "G", col c, ("k", FieldText (kc k)), ("i", FieldInt ice)
         , ("o", maybe FieldNull (FieldText . ovName) ov), ("n", FieldInt (maybe 0 ovLayers ov)) ]
       Stone k -> [t "stone", n k]
       Chest k -> [t "chest", n k]
       Honey k -> [t "honey", n k]
       Balloon c -> [t "balloon", col c]
       Cookie -> [t "cookie"]
       Cake k -> [t "cake", n k]
       MagicHat -> [t "hat"]
       Maker c k -> [t "maker", col c, n k]
       Snail dr dc -> [t "snail", ("dr", FieldInt dr), ("dc", FieldInt dc)]
       Safe k -> [t "safe", n k]
       Flip f b -> [t "flip", col f, ("b", FieldInt (fromEnum b + 1))]
       Surprise -> [t "surprise"]
       Bottle c -> [t "bottle", col c]
       TimeSpirit -> [t "spirit"]
       Countdown c k -> [t "countdown", col c, n k]
       Custom name v -> [t "custom", ("name", FieldText (unElementName name)), ("v", FieldInt (unCustomState v))]

--------------------------------------------------------------------------------

view_fields_match_legacy_reads :: Assertion
view_fields_match_legacy_reads = do
  assertBool "enough samples" (length samples > 100)
  mapM_ check (zip [0 :: Int ..] samples)
  where
    check (i, gs) = do
      let gv = gameView gs
          tag = "sample " ++ show i ++ " L" ++ show (gsLevel gs)
          li = min (gsLevel gs) (levelCount - 1)
          mv = gsMoves gs
          gi = gvGoal gv
          bv = gvBoard gv
      gvLevel gv @?= gsLevel gs
      assertEqual (tag ++ " index") li (gvLevelIndex gv)
      assertEqual (tag ++ " raw name") (maybe "?" lvlName (lookupLevel (gsLevel gs))) (gvRawName gv)
      assertEqual (tag ++ " move cap") (max mv (maybe mv lvlMoves (lookupLevel li))) (gvMoveCap gv)
      assertEqual (tag ++ " score/moves/daily") (gsScore gs, mv, gsDaily gs) (gvScore gv, gvMoves gv, gvDaily gv)
      assertEqual (tag ++ " boosters") (gsHammers gs, gsFreeSwaps gs, gsCrossClears gs)
        (bHammers (gvBoosters gv), bFreeSwaps (gvBoosters gv), bCrossClears (gvBoosters gv))
      assertEqual (tag ++ " combo") (gsCombo gs) (gvCombo gv)
      assertEqual (tag ++ " shuffled/over") (gsShuffled gs, gsOver gs) (gvShuffled gv, gvOver gv)
      assertEqual (tag ++ " status strip") (legacyStatusStrip gs) (stripOf (gvStatus gv))
      assertEqual (tag ++ " progress/target") (gsProgress gs, goalTarget (gsGoal gs)) (giProgress gi, giTarget gi)
      assertEqual (tag ++ " goal kind/text") (takeWhile (/= ' ') (show (gsGoal gs)), show (gsGoal gs)) (giKind gi, giText gi)
      assertEqual (tag ++ " goal name") [n | ViewCount (CountNamed n) _ <- [goalView (gsGoal gs)]] (maybe [] pure (giName gi))
      assertEqual (tag ++ " lose hint") (loseHint (gsGoal gs)) (giLoseHint gi)
      assertEqual (tag ++ " hints") (gsHint gs, findHint (gsBoard gs)) (bvHint bv, bvFoundHint bv)
      assertEqual (tag ++ " board") (boardRows (gsBoard gs)) (boardRows (bvBoard bv))
      assertEqual (tag ++ " level layer")
        (gsLastCleared gs, gsGround gs, gsBelts gs, gsPortals gs, levelCarpets (gsLevel gs), gsCarpetOpen gs)
        (bvLastCleared bv, bvGround bv, bvBelts bv, bvPortals bv, bvCarpets bv, bvCarpetOpen bv)
      assertEqual (tag ++ " ufos") (map (\u -> (ufoCell u, ufoColor u)) (gsUfos gs)) (map (\u -> (ufoCell u, ufoColor u)) (bvUfos bv))
      assertEqual (tag ++ " ground cells") [lookup p (gsGround gs) | p <- allPos'] [groundAtView bv p | p <- allPos']

allPos' :: [Pos]
allPos' = [(r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1]]

view_title_and_bracket_match_legacy :: Assertion
view_title_and_bracket_match_legacy =
  mapM_
    ( \gs -> do
        assertEqual "title" (legacyTitle gs) (titleLine (gameView gs))
        assertEqual "collect bracket" (legacyCollect gs) (goalBracket (gvGoal (gameView gs)))
    )
    samples

view_carpet_marks_match_legacy :: Assertion
view_carpet_marks_match_legacy = do
  let carpetSamples = [gs | gs <- samples, not (null (levelCarpets (gsLevel gs)))]
  assertBool "has carpet levels" (not (null carpetSamples))
  assertBool "some carpet opened" (any (not . null . gsCarpetOpen) carpetSamples)
  mapM_
    ( \gs -> do
        let bv = boardView gs
        mapM_
          ( \pos -> do
              -- 几何版（BoardPrim）的旧式子
              let openP = pos `elem` gsCarpetOpen gs
                  coveredP = pos `elem` levelCarpets (gsLevel gs) && not openP && not (null (levelCarpets (gsLevel gs)))
                  -- 贴图版（BoardArt）的旧式子
                  coveredA = pos `elem` levelCarpets (gsLevel gs) && not openP
                  m = carpetAt bv pos
              assertEqual "open" openP (m == CarpetOpen)
              assertEqual "covered (prim)" coveredP (m == CarpetCovered)
              assertEqual "covered (art)" coveredA (m == CarpetCovered)
          )
          allPos'
    )
    samples

view_level_dots_match_legacy :: Assertion
view_level_dots_match_legacy =
  sequence_
    [ do
        let ds = levelDots cur maxR
            art i
              | i == cur = "cur"
              | i < cur = "done"
              | i <= maxR = "unlocked"
              | otherwise = "locked"
            artOf d = case d of DotCurrent -> "cur"; DotDone -> "done"; DotUnlocked -> "unlocked"; DotLocked -> "locked"
            prim i = if i == cur then "cur" else if i < cur then "done" else "other"
            primOf d = case d of DotCurrent -> "cur"; DotDone -> "done"; _ -> "other"
        length ds @?= levelCount
        assertEqual "art dots" (map art [0 .. levelCount - 1]) (map artOf ds)
        assertEqual "prim dots" (map prim [0 .. levelCount - 1]) (map primOf (levelDots cur (-1)))
    | cur <- [-1 .. levelCount + 1]
    , maxR <- [-1, 0, cur, cur + 3, levelCount - 1, levelCount + 5]
    ]

view_score_badge_matches_legacy :: Assertion
view_score_badge_matches_legacy =
  sequence_
    [ do
        let gs = g0 {gsShuffled = sh, gsScore = 321}
            gv = gameView gs
            badge = badgeOf replay showLeft best gv
            -- 贴图版 HudArt 右下角的旧分支
            art = case replay of
              Just (combo, shown)
                | combo >= 2 -> "combo " ++ show combo
                | otherwise -> "rolling " ++ show shown
              Nothing
                | showLeft > 0 && best >= 2 -> "summary " ++ show best
                | otherwise -> (if gsShuffled gs then "shuffle " else "score ") ++ show (gsScore gs)
            artOf b = case b of
              BadgeCombo n -> "combo " ++ show n
              BadgeRolling n -> "rolling " ++ show n
              BadgeSummary n -> "summary " ++ show n
              BadgeScore s n -> (if s then "shuffle " else "score ") ++ show n
            -- 几何版 hudComboBadge 的旧分支
            prim = case replay of
              Just (combo, _) | combo >= 2 -> Just (combo, False)
              Just _ -> Nothing
              Nothing
                | showLeft > 0 && best >= 2 -> Just (best, True)
                | otherwise -> Nothing
            primOf b = case b of
              BadgeCombo n -> Just (n, False)
              BadgeSummary n -> Just (n, True)
              _ -> Nothing
        assertEqual "art badge" art (artOf badge)
        assertEqual "prim badge" prim (primOf badge)
    | replay0 <- Nothing : [Just (c, s) | c <- [0, 1, 2, 5], s <- [0, 40]]
    , let replay = replay0
    , showLeft <- [0, 1, 30]
    , best <- [0, 1, 2, 7]
    , sh <- [False, True]
    ]
  where
    g0 = levelGame 0 5
    badgeOf r = scoreBadge (fmap (uncurry ReplayView) r)

view_cell_face_and_level_list_match_legacy :: Assertion
view_cell_face_and_level_list_match_legacy = do
  let cells =
        concatMap (concat . boardRows . gsBoard) samples
          ++ [ Gem c k ice ov | c <- allColors, k <- [Normal, LineH, LineV, Bomb, Rainbow], ice <- [0, 2]
             , ov <- Nothing : map Just [Grass, Vine, Choco, Fog 2, Chain 1, Freeze 3, Curtain 1, Steam] ]
          ++ [ Stone 2, Chest 1, Honey 3, Balloon C2, Cookie, Cake 4, MagicHat, Maker C3 2, Snail 0 1, Safe 2
             , Flip C1 C4, Surprise, Bottle C5, TimeSpirit, Countdown C2 3 ]
  mapM_ (\cell -> let (t, fs) = cellFace cell in assertEqual (show cell) (legacyCellFields cell) (("t", FieldText t) : fs)) cells
  length levelViews @?= length allLevels
  sequence_
    [ assertEqual "level list" (lvlIndex l, lvlName l, lvlMoves l, show (lvlGoal l), goalTarget (lvlGoal l))
        (lvIndex v, lvName v, lvMoves v, giText (lvGoal v), giTarget (lvGoal v))
    | (l, v) <- zip allLevels levelViews
    ]

--------------------------------------------------------------------------------
-- 通用网格组件

-- 第 11 刀前 UI.Layout 的式子（padPx 16、hudH 108、cellPx 56、8×8）。
legacyPixelToCell :: Int -> Int -> Maybe Pos
legacyPixelToCell mx my =
  let x = mx - 16
      y = my - 16 - 108
      boardPx = 56 * boardSize
  in if x < 0 || y < 0 || x >= boardPx || y >= boardPx
       then Nothing
       else
         let c = x `div` 56
             r = y `div` 56
         in if inBounds (r, c) then Just (r, c) else Nothing

boardGeom :: GridGeom Int
boardGeom = GridGeom {ggLeft = 16, ggTop = 16 + 108, ggCell = 56, ggRows = boardSize, ggCols = boardSize}

grid_ui_geometry_matches_legacy_layout :: Assertion
grid_ui_geometry_matches_legacy_layout = do
  sequence_
    [ assertEqual (show (x, y)) (legacyPixelToCell x y) (gridCellAt boardGeom x y)
    | x <- [-20 .. 500], y <- [-20, -1, 0, 100, 123, 124, 125, 179, 180, 181, 400, 571, 572, 579, 580, 581, 700]
    ]
  sequence_
    [ assertEqual (show (x, y)) (legacyPixelToCell x y) (gridCellAt boardGeom x y)
    | x <- [0, 15, 16, 17, 71, 72, 463, 464, 479, 480, 481], y <- [-20 .. 700]
    ]
  sequence_
    [ do
        gridCellOrigin boardGeom (r, c) @?= (16 + c * 56, 16 + 108 + r * 56)
        let (x, y) = gridCellOrigin boardGeom (r, c)
        gridCellAt boardGeom x y @?= Just (r, c)
        gridCellAt boardGeom (x + 55) (y + 55) @?= Just (r, c)
    | (r, c) <- allPos'
    ]
  gridCells boardGeom @?= allPos'
  (gridWidth boardGeom, gridHeight boardGeom) @?= (448, 448)
  -- 非正方网格：行列不混
  let g = GridGeom {ggLeft = 0, ggTop = 0, ggCell = 10, ggRows = 2, ggCols = 3} :: GridGeom Int
  gridCells g @?= [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]
  gridCellAt g 25 15 @?= Just (1, 2)
  gridCellAt g 15 25 @?= Nothing
  -- 4 邻接与三消的 adjacent 一致
  sequence_ [assertEqual (show (a, b)) (adjacent a b) (orthoAdjacent a b) | a <- allPos', b <- allPos']

grid_ui_click_drag_highlight :: Assertion
grid_ui_click_drag_highlight = do
  -- 点选：与第 11 刀前 UI.Input.cellClick 的 ToolNone / ToolFreeSwap 分支相同
  gridClick Nothing (2, 3) @?= ClickSelect ((2, 3) :: Pos)
  gridClick (Just (2, 3)) (2, 3) @?= (ClickDeselect :: Click Pos)
  gridClick (Just (2, 3)) (5, 5) @?= ClickPair (2, 3) ((5, 5) :: Pos)
  -- 拖动：落在另一格且相邻才成对（旧式子 Just p2 | p1 /= p2 && adjacent p1 p2）
  sequence_
    [ assertEqual (show (p1, m)) (legacy p1 m) (fmap snd (gridDragRelease adjacent p1 m))
    | p1 <- [(0, 0), (3, 4), (7, 7)], m <- Nothing : map Just allPos'
    ]
  gridDragRelease adjacent (3, 4) (Just (3, 5)) @?= Just ((3, 4), (3, 5))
  -- 高亮
  let hl :: Highlight Pos
      hl = Highlight {hlSelected = Just (1, 1), hlHint = [(2, 2), (2, 3)], hlFlash = [(4, 4)], hlPinned = Nothing}
  (isSelected hl (1, 1), isSelected hl (2, 2)) @?= (True, False)
  (isHinted hl (2, 3), isHinted hl (1, 1)) @?= (True, False)
  (isFlashing hl (4, 4), isFlashing hl (2, 2)) @?= (True, False)
  [p | p <- allPos', isSelected noHighlight p || isHinted noHighlight p || isFlashing noHighlight p] @?= ([] :: [Pos])
  where
    legacy p1 m = case m of
      Just p2 | p1 /= p2 && adjacent p1 p2 -> Just p2
      _ -> Nothing

--------------------------------------------------------------------------------
-- 源码扫描

frontends_read_view_model :: Assertion
frontends_read_view_model = do
  api <- readCode "web/hs/Match3Web/Api.hs"
  let stateReads =
        [ "gsLevel", "gsScore", "gsMoves", "gsGoal", "gsProgress", "goalTarget", "gsOver", "loseHint", "gsCombo"
        , "gsShuffled", "gsBoard", "findHint", "gsLastCleared", "gsGround", "gsBelts", "gsPortals", "gsUfos"
        , "levelCarpets", "gsCarpetOpen", "lookupLevel", "allLevels", "goalView" ]
  assertEqual "web Api reads no GameState fields" [] (filter (`mentionsIdent` api) stateReads)
  let hudFiles = ["app/UI/HudArt.hs", "app/UI/HudBlocks.hs", "app/UI/HudPrim.hs"]
      hudBanned = ["gsHammers", "gsFreeSwaps", "gsCrossClears", "gsProgress", "goalTarget", "lookupLevel", "levelCount", "allLevels", "gsShuffled", "gsDaily"]
  bad <- concat <$> mapM (\f -> (\src -> [(f, i) | i <- hudBanned, i `mentionsIdent` src]) <$> readCode f) hudFiles
  assertEqual "HUD reads view model" [] bad
  let boardFiles = ["app/UI/BoardPrim.hs", "app/UI/BoardArt.hs"]
      boardBanned = ["levelCarpets", "gsCarpetOpen", "gsGround", "groundAt", "gsHint", "gsBelts", "gsPortals"]
  badB <- concat <$> mapM (\f -> (\src -> [(f, i) | i <- boardBanned, i `mentionsIdent` src]) <$> readCode f) boardFiles
  assertEqual "board underlay reads view model" [] badB
  actions <- readCode "app/UI/Actions.hs"
  assertBool "title from titleLine" ("titleLine" `mentionsIdent` actions && not ("goalView" `mentionsIdent` actions))
  layout <- readCode "app/UI/Layout.hs"
  assertBool "pixelToCell via GridUI" ("gridCellAt boardGrid" `isInfixOf` layout && "gridCellOrigin boardGrid" `isInfixOf` layout)
  input <- readCode "app/UI/Input.hs"
  assertBool "click/drag via GridUI" (all (`mentionsIdent` input) ["gridClick", "gridDragRelease"])
  assertBool "collect bracket via view" (not ("goalView" `mentionsIdent` input))
  -- 标签表只有一份（在 Match3.View）
  goalStyle <- readCode "app/UI/GoalStyle.hs"
  assertBool "no tag tables in GoalStyle" (not (any (`mentionsIdent` goalStyle) ["countTag", "colorTag"]))
  -- 规则开关角标：两个前端都按 ruleBadges 通用地画（HUD 不点名具体规则）；关卡用到的每个规则开关都登记了角标；
  -- 角标文字与 tools/gen_assets.py 里桌面文字贴图（ZH 表）的字面相同，图标是 gen_assets.py 生成的贴图
  hudArt <- readCode "app/UI/HudArt.hs"
  assertBool "HudArt draws ruleBadges generically" ("ruleBadges" `mentionsIdent` hudArt && not (any (`isInfixOf` hudArt) ["\"bomb_shapes\"", "\"zh_rule_bomb\""]))
  assertBool "web Api encodes ruleBadges" ("ruleBadges" `mentionsIdent` api)
  let levelRules = nub [unElementName r | l <- allLevels, r <- lvlRules l]
  assertEqual "every level rule has a registered badge" [] [r | r <- levelRules, r `notElem` map rbRule ruleBadgeTable]
  assertEqual "level 41 badges" [("bomb_shapes", "L/T 形出炸弹")] [(rbRule b, rbText b) | b <- ruleBadges (gameView (levelGame 40 1))]
  assertEqual "level 1 has no badge" [] (ruleBadges (gameView (levelGame 0 1)))
  assertEqual "unregistered rule falls back to its name" (RuleBadge "x_rule" "x_rule" [] "zh_rule_x_rule") (ruleBadge "x_rule")
  gen <- readFile "tools/gen_assets.py"
  assertEqual "badge text = gen_assets.py ZH text" []
    [rbRule b | b <- ruleBadgeTable, not (("\"" ++ drop 3 (rbTextSprite b) ++ "\": \"" ++ rbText b ++ "\"") `isInfixOf` gen)]
  assertEqual "badge icons are generated sprites" []
    [ic | b <- ruleBadgeTable, ic <- rbIcons b, not (("sp[\"" ++ ic ++ "\"]") `isInfixOf` gen)]
  -- 目标中文显示名（goalLabel，网页 HUD「目标 …」的唯一来源）：网页接口输出它；全部关卡与每日挑战的目标都有中文名
  -- （不含英文标识符，即元素名目标都在 namedGoalLabelTable 登记了）；没登记的名字退回元素名本身
  assertBool "web Api encodes goalLabel" ("goalLabel" `mentionsIdent` api)
  let rawIdent = any (\ch -> (ch >= 'a' && ch <= 'z') || ch == '_')
      dailyGoals = [goalInfo (cfgGoal (dailyConfig 2026 m d)) 0 | m <- [1 .. 12], d <- [1 .. 28]]
  assertEqual "every level goal has a Chinese label" [] [(lvIndex l, goalLabel (lvGoal l)) | l <- levelViews, rawIdent (goalLabel (lvGoal l))]
  assertEqual "every daily goal has a Chinese label" [] [goalLabel g | g <- dailyGoals, rawIdent (goalLabel g)]
  assertEqual "level 43 goal label" "毛球" (goalLabel (gvGoal (gameView (levelGame 42 1))))
  assertEqual "unregistered named goal falls back to its name" "x_elem" (goalLabel (goalInfo (goalCount (CountNamed (ElementName "x_elem")) 3) 0))
