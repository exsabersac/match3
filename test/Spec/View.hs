-- | 视图模型 Match3.View。
--
-- 对照的旧实现是本文件里的字面副本：视图模型之前的原桌面版 UI.Actions.updateTitle（标题）、
-- UI.HudArt / UI.HudBlocks（进度点、道具、分数徽章、结局色条分支）、web/hs/Match3Web/Api.hs（encodeState /
-- encodeGoal / encodeCell / apiLevels 的现算式；结局分支读 fromTerminal <$> gsOver，即当时的 Outcome）。
-- 另有源码扫描：前端不从 GameState 现算（读视图模型）。
module Spec.View
  ( tests
  ) where

import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Match3.Board.Default (findHint)
import Match3.Core
import Match3.Daily (dailySeed)
import Match3.Game.Outcome (loseHint)
import Match3.Game.State (gsUfos)
import Match3.Levels.Campaign (allLevels, levelCount, lookupLevel)
import Match3.Daily (dailyConfig)
import Match3.Element.Builtin (SnowBoss(..), chameleonCell)
import Match3.Element.Ability (toCell)
import Match3.Game.Move (trySwap)
import Match3.Game.State (gsBelts, gsCarpetOpen, gsGround, gsPortals, gsProgress)
import Match3.Levels.Campaign (levelCarpets)
import Match3.Types (goalCount, goalTarget)
import Match3.View
import Spec.Support (levelGame)
import Spec.Support.Source (importsOf, mentionsIdent, readCode, sourcesUnderAll)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "view_fields_match_legacy_reads" view_fields_match_legacy_reads
  , testCase "view_title_matches_legacy" view_title_matches_legacy
  , testCase "view_level_dots_match_legacy" view_level_dots_match_legacy
  , testCase "view_score_badge_matches_legacy" view_score_badge_matches_legacy
  , testCase "view_cell_face_and_level_list_match_legacy" view_cell_face_and_level_list_match_legacy
  , testCase "frontends_read_view_model" frontends_read_view_model
  , testCase "frontends_import_core_api" frontends_import_core_api
  , testCase "outcome_lose_hint_no_internal_names" outcome_lose_hint_no_internal_names
  , testCase "view_cell_extras_from_elements" view_cell_extras_from_elements
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
    ++ playOn 3 (newDailyGame (dailyConfig (Year 2026) (Month 9) (Day 30)) (dailySeed (Year 2026) (Month 9) (Day 30)))
    ++ [ g0 {gsLevel = levelCount + 2}
       , g0 {gsHammers = 0, gsFreeSwaps = 5, gsCrossClears = 2, gsShuffled = True}
       , g0 {gsOver = Just (TWon 1234)}
       , g0 {gsOver = Just (TLevelClear 99 4)}
       , g0 {gsOver = Just (TLost 7), gsShuffled = True}
       , g0 {gsMoves = 99}
       ]
  where
    g0 = levelGame 0 5

--------------------------------------------------------------------------------
-- 旧实现副本

legacyTitle :: GameState -> String
legacyTitle gs =
  let levelName = maybe "?" lvlName (lookupLevel (min (gsLevel gs) (levelCount - 1)))
      status = case fromTerminal <$> gsOver gs of
        Just (Won s) -> " CLEAR! score=" <> show s
        Just (LevelClear s n) -> " LEVEL UP ->" <> show (n + 1) <> " score=" <> show s
        Just (Lost s) -> " LOSE score=" <> show s
        _ -> ""
      combo = gsCombo gs
      comboBits = if combo > 1 then "  combo x" ++ show combo else ""
      prog = gsProgress gs
      -- 目标段：合 main 9f5504e 后换成中文标签（原为 score= / collect RED= / multi / countTag= / goal=，数字不变）
      lbl = goalLabel (goalInfo (gsGoal gs) prog)
      goalBits = case goalView (gsGoal gs) of
        ViewScore t -> lbl ++ "=" ++ show prog ++ "/" ++ show t
        ViewCollect _ n -> lbl ++ "=" ++ show prog ++ "/" ++ show n
        ViewCollectMulti _ -> lbl ++ "=" ++ show prog ++ "/" ++ show (goalTarget (gsGoal gs))
        ViewCount _ n -> lbl ++ "=" ++ show prog ++ "/" ++ show n
        ViewOther _ -> lbl ++ "=" ++ show prog ++ "/" ++ show (goalTarget (gsGoal gs))
  in "L" ++ show (gsLevel gs + 1) ++ " " ++ levelName ++ "  " ++ goalBits ++ "  moves=" ++ show (gsMoves gs)
       ++ comboBits ++ "  Hm=" ++ show (gsHammers gs) ++ " Sw=" ++ show (gsFreeSwaps gs)
       ++ " Cr=" ++ show (gsCrossClears gs) ++ status

-- 几何版结局色条的分支（颜色换成标签）。
legacyStatusStrip :: GameState -> String
legacyStatusStrip gs = case fromTerminal <$> gsOver gs of
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
      assertEqual (tag ++ " hint") (findHint (gsBoard gs)) (bvFoundHint bv)
      assertEqual (tag ++ " board") (boardRows (gsBoard gs)) (boardRows (bvBoard bv))
      assertEqual (tag ++ " level layer")
        (gsLastCleared gs, gsGround gs, gsBelts gs, gsPortals gs, levelCarpets (gsLevel gs), gsCarpetOpen gs)
        (bvLastCleared bv, bvGround bv, bvBelts bv, bvPortals bv, bvCarpets bv, bvCarpetOpen bv)
      assertEqual (tag ++ " ufos") (map (\u -> (ufoCell u, ufoColor u)) (gsUfos gs)) (map (\u -> (ufoCell u, ufoColor u)) (bvUfos bv))

view_title_matches_legacy :: Assertion
view_title_matches_legacy =
  mapM_ (\gs -> assertEqual "title" (legacyTitle gs) (titleLine (gameView gs))) samples

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
-- 源码扫描

-- | 前端（app/pure 与 web/hs）从库里只 import 前端 API：Match3.Core、视图模型 Match3.View、对局外壳 Match3.Engine、
-- 效果事件 Match3.Element.Event 与通用层 Engine.*；Match3.Core 导出的函数与不带构造的类型都有前端在用
-- （带 (..) 的类型前端可能只用构造或字段，不查）。
frontends_import_core_api :: Assertion
frontends_import_core_api = do
  files <- sourcesUnderAll ["app", "web/hs"]
  assertBool "scan covers app/pure and web/hs" (all (`elem` files) ["app/pure/ComboFx.hs", "web/hs/Match3Web/Api.hs"])
  srcs <- mapM (\f -> (,) f <$> readCode f) files
  let frontendApi m = m `elem` ["Match3.Core", "Match3.View", "Match3.Engine", "Match3.Element.Event"] || "Engine." `isPrefixOf` m
      fromLibrary m = "Match3." `isPrefixOf` m || "Engine." `isPrefixOf` m
  assertEqual "front ends import only the front-end API" []
    [(f, m) | (f, s) <- srcs, m <- importsOf s, fromLibrary m, not (frontendApi m)]
  core <- readCode "src/Match3/Core.hs"
  let exportLines = takeWhile (not . (") where" `isInfixOf`)) (drop 1 (dropWhile (not . ("module Match3.Core" `isPrefixOf`)) (lines core)))
      exportItems = [w | l <- exportLines, w : _ <- [words (dropWhile (`elem` " (,") l)]]
      plainExports = [w | w <- exportItems, '(' `notElem` w]
  assertBool "Match3.Core export list parsed" (length plainExports > 50)
  assertEqual "every Match3.Core function / plain type is used by a front end" []
    [n | n <- plainExports, not (any (mentionsIdent n . snd) srcs)]

frontends_read_view_model :: Assertion
frontends_read_view_model = do
  api <- readCode "web/hs/Match3Web/Api.hs"
  let stateReads =
        [ "gsLevel", "gsScore", "gsMoves", "gsGoal", "gsProgress", "goalTarget", "gsOver", "loseHint", "gsCombo"
        , "gsShuffled", "gsBoard", "findHint", "gsLastCleared", "gsGround", "gsBelts", "gsPortals", "gsUfos"
        , "levelCarpets", "gsCarpetOpen", "lookupLevel", "allLevels", "goalView" ]
  assertEqual "web Api reads no GameState fields" [] (filter (`mentionsIdent` api) stateReads)
  -- 标题行读视图模型（网页 state.title = titleLine）
  assertBool "web title from titleLine" ("titleLine" `mentionsIdent` api)
  -- 规则开关角标：网页按 state.rules（ruleBadges）通用地画（hud.js 不点名具体规则）；关卡用到的每个规则开关都登记了角标；
  -- 图标是 gen_assets.py 生成的贴图
  hudJs <- readFile "web/www/hud.js"
  assertBool "web hud.js draws rule badges generically" (not (any (`isInfixOf` hudJs) ["\"bomb_shapes\"", "\"rainbow_combos\""]))
  assertBool "web Api encodes ruleBadges" ("ruleBadges" `mentionsIdent` api)
  let levelRules = nub [unElementName r | l <- allLevels, r <- lvlRules l]
  assertEqual "every level rule has a registered badge" [] [r | r <- levelRules, r `notElem` map rbRule ruleBadgeTable]
  assertEqual "level 41 badges" [("bomb_shapes", "L/T 形出炸弹")] [(rbRule b, rbText b) | b <- ruleBadges (gameView (levelGame 40 1))]
  assertEqual "level 1 has no badge" [] (ruleBadges (gameView (levelGame 0 1)))
  assertEqual "unregistered rule falls back to its name" (RuleBadge "x_rule" "x_rule" []) (ruleBadge "x_rule")
  gen <- readFile "tools/gen_assets.py"
  assertEqual "badge icons are generated sprites" []
    [ic | b <- ruleBadgeTable, ic <- rbIcons b, not (("sp[\"" ++ ic ++ "\"]") `isInfixOf` gen)]
  -- 目标中文显示名（goalLabel，网页 HUD「目标 …」的唯一来源）：网页接口输出它；全部关卡与每日挑战的目标都有中文名
  -- （不含英文标识符，即元素名目标都在 namedGoalLabelTable 登记了）；没登记的名字退回元素名本身
  assertBool "web Api encodes goalLabel" ("goalLabel" `mentionsIdent` api)
  let rawIdent = any (\ch -> (ch >= 'a' && ch <= 'z') || ch == '_')
      dailyGoals = [goalInfo (cfgGoal (dailyConfig (Year 2026) (Month m) (Day d))) 0 | m <- [1 .. 12], d <- [1 .. 28]]
  assertEqual "every level goal has a Chinese label" [] [(lvIndex l, goalLabel (lvGoal l)) | l <- levelViews, rawIdent (goalLabel (lvGoal l))]
  assertEqual "every daily goal has a Chinese label" [] [goalLabel g | g <- dailyGoals, rawIdent (goalLabel g)]
  assertEqual "level 43 goal label" "毛球" (goalLabel (gvGoal (gameView (levelGame 42 1))))
  assertEqual "level 45 goal label" "雪怪" (goalLabel (gvGoal (gameView (levelGame 44 1))))
  assertEqual "level 46 goal label" "饼干" (goalLabel (gvGoal (gameView (levelGame 45 1))))
  assertEqual "level 47 goal label" "变色龙" (goalLabel (gvGoal (gameView (levelGame 46 1))))
  assertEqual "unregistered named goal falls back to its name" "x_elem" (goalLabel (goalInfo (goalCount (CountNamed (ElementName "x_elem")) 3) 0))

-- | 失败提示（Match3.Game.Outcome.loseHint，视图字段 giLoseHint）与标题的目标段（goalLine）
-- 只用中文标签：全部关卡与每日挑战都不含 [a-z_]（不露出 fuzzball / chameleon 这类元素内部名，也不再有 score / stone 等英文标签）；
-- 标签与 goalLabel 同源（Match3.GoalLabel）。
outcome_lose_hint_no_internal_names :: Assertion
outcome_lose_hint_no_internal_names = do
  let rawIdent = any (\ch -> (ch >= 'a' && ch <= 'z') || ch == '_')
      levelGoals = [(li + 1, lvlGoal l) | (li, l) <- zip [0 :: Int ..] allLevels]
      dailyGoals = [(m * 100 + d, cfgGoal (dailyConfig (Year 2026) (Month m) (Day d))) | m <- [1 .. 12], d <- [1 .. 28]]
      goals = levelGoals ++ dailyGoals
  assertEqual "lose hints" [] [(i, loseHint g) | (i, g) <- goals, rawIdent (loseHint g)]
  assertEqual "view lose hints" [] [(i, giLoseHint (goalInfo g 0)) | (i, g) <- goals, rawIdent (giLoseHint (goalInfo g 0))]
  assertEqual "title goal segments" [] [(i, goalLine (goalInfo g 0)) | (i, g) <- goals, rawIdent (goalLine (goalInfo g 0))]
  -- 测试跑手报告过漏出内部名的四关（jelly / bubble / fuzzball / snow_boss，含 Boss 目标）逐字核对
  assertEqual "level 39 lose hint" "消除果冻，目标 32 个" (loseHint (gsGoal (levelGame 38 1)))
  assertEqual "level 40 lose hint" "消除气泡，目标 12 个" (loseHint (gsGoal (levelGame 39 1)))
  assertEqual "level 47 lose hint" "消除变色龙，目标 30 个" (loseHint (gsGoal (levelGame 46 1)))
  assertEqual "level 43 lose hint" "消除毛球，目标 14 个" (loseHint (gsGoal (levelGame 42 1)))
  assertEqual "level 45 lose hint" "用身边的消除和特效打雪怪，目标 40 点血" (loseHint (gsGoal (levelGame 44 1)))
  -- 碎石目标（测试跑手报告第 8 / 41 / 42 / 44 关写成「砸箱子」）：与 goalLabel 的「碎石」一致
  assertEqual "level 8 lose hint" "用邻消或特效砸开碎石，目标 8 个" (loseHint (gsGoal (levelGame 7 1)))
  assertEqual "level 48 lose hint" "用邻消或特效砸开碎石，目标 8 个" (loseHint (gsGoal (levelGame 47 1)))
  assertEqual "stone lose hints use the stone label" [] [i | (i, g) <- goals, ViewCount CountStones _ <- [goalView g], not (goalLabel (goalInfo g 0) `isInfixOf` loseHint g) || "箱子" `isInfixOf` loseHint g]
  assertEqual "level 47 title segment" "变色龙=6/30" (goalLine (goalInfo (gsGoal (levelGame 46 1)) 6))
  outcome <- readCode "src/Match3/Game/Outcome.hs"
  assertBool "loseHint reads the shared label table" ("countLabel" `mentionsIdent` outcome)

-- | 显示附加字段与目标中文名由元素条目提供（能力记录的显示组）：雪怪 Boss 的 q / hurt / turn / every、变色龙的 c
-- （网页 JSON 的顺序就是这里的顺序），其余内置格子没有；中文名表 = 六个登记了 labelled 的元素；
-- View / GoalLabel / 网页 Api / 调色板不再点名这些元素。
view_cell_extras_from_elements :: Assertion
view_cell_extras_from_elements = do
  assertEqual "snow boss (hurt)" [("q", FaceInt 3), ("hurt", FaceBool True), ("turn", FaceInt 2), ("every", FaceInt 3)] (cellExtras (toCell (SnowBoss 20 40 2 3)))
  assertEqual "snow boss (not hurt)" [("q", FaceInt 0), ("hurt", FaceBool False), ("turn", FaceInt 0), ("every", FaceInt 3)] (cellExtras (toCell (SnowBoss 21 40 0 0)))
  assertEqual "chameleon" [[("c", FaceColor c)] | c <- allColors] (map (cellExtras . chameleonCell) allColors)
  assertEqual "other cells have none" [] (concatMap cellExtras [mkGem C1, Stone 2, Countdown C2 3, Custom (ElementName "fuzzball") (CustomState 1), Custom (ElementName "no_such") (CustomState 0)])
  assertEqual "named goal labels"
    (sort [("jelly", "果冻"), ("bubble", "气泡"), ("magic_stone", "魔法石"), ("fuzzball", "毛球"), ("snow_boss", "雪怪"), ("chameleon", "变色龙")])
    (sort namedGoalLabelTable)
  let files = ["src/Match3/View.hs", "src/Match3/GoalLabel.hs", "src/Match3/Game/Outcome.hs", "web/hs/Match3Web/Api.hs", "app/pure/UI/Palette.hs", "app/pure/UI/CellFace.hs"]
      names = ["chameleonName", "chameleonColor", "decodeBoss", "snowBossEvery"]
  srcs <- mapM readCode files
  assertEqual "no element-specific display code" [] [(f, n) | (f, s) <- zip files srcs, n <- names, mentionsIdent n s]

