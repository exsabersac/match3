-- | 桌面走步提示文案（app/pure/UI/MoveText.hs）：拖拽 / 点击交换与锤子 / 十字 / 自由交换的
-- 「结算结果 → 一句话」逐字钉住（窗口标题「  |  」之后与 HUD 提示行显示的就是它），
-- 以及道具结果之后的点选模式、文案表只在 app/pure 里（源码扫描）。
module Spec.MoveText
  ( tests
  ) where

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Match3.Core
import Spec.Support.Source (readCode, sourcesUnder)
import Test.Tasty
import Test.Tasty.HUnit
import UI.MoveText

tests :: [TestTree]
tests =
  [ testCase "move_text_boosters_pinned" move_text_boosters_pinned
  , testCase "move_text_drag_pinned" move_text_drag_pinned
  , testCase "move_text_click_pinned" move_text_click_pinned
  , testCase "move_text_lost_reads_goal_of" move_text_lost_reads_goal_of
  , testCase "move_text_keeps_tool_only_free_swap_no_match" move_text_keeps_tool_only_free_swap_no_match
  , testCase "move_text_tables_only_in_app_pure" move_text_tables_only_in_app_pure
  ]

-- | 第 i 关（0 基）种子 1 开局。
levelAt :: Int -> GameState
levelAt i = fromMaybe (error ("levelAt " ++ show i)) (campaignGame i 1)

-- | 第 1 关：分数目标 300（loseHint = 「再冲冲分数吧，目标 300」，goalBracket 为空）。
scoreGs :: GameState
scoreGs = levelAt 0

noFx :: MoveFx
noFx = MoveFx 0 []

-- | 每种结果各一个代表值（Lost 的文案读目标，分数只出现在交换的文案里）。
outcomes :: [Outcome]
outcomes = [InvalidSwap, NoMatch, MoveApplied 120, LevelClear 450 3, Won 9000, Lost 210]

pinned :: MoveUi -> GameState -> GameState -> MoveFx -> [String] -> Assertion
pinned ui gsBefore gsAfter fx expected =
  map (moveMsg ui gsBefore gsAfter fx) outcomes @?= expected

move_text_boosters_pinned :: Assertion
move_text_boosters_pinned = do
  pinned UiHammer scoreGs scoreGs noFx
    [ "No hammers left", "Hammer failed", "Hammer +120", "Hammer cleared level!", "Hammer won!", "再冲冲分数吧，目标 300" ]
  pinned UiCross scoreGs scoreGs noFx
    [ "No cross-clears left", "Cross failed", "Cross +120", "Cross cleared level!", "Cross won!", "再冲冲分数吧，目标 300" ]
  pinned UiFreeSwap scoreGs scoreGs noFx
    [ "Free-swap invalid / empty", "Free-swap: no match; not spent", "Free-swap +120", "Free-swap cleared level!"
    , "Free-swap won!", "再冲冲分数吧，目标 300" ]
  -- 道具文案不看连击与收集进度
  moveMsg UiHammer scoreGs (levelAt 1) (MoveFx 4 []) (MoveApplied 7) @?= "Hammer +7"

move_text_drag_pinned :: Assertion
move_text_drag_pinned = do
  pinned UiDrag scoreGs scoreGs noFx
    [ "Need adjacent", "No match; rolled back", "Drag +120", "Level clear -> L4", "YOU WIN score=9000"
    , "Out of moves score=210 — 再冲冲分数吧，目标 300" ]
  -- 拖拽文案不看连击、收集进度与自动洗牌
  moveMsg UiDrag scoreGs ((levelAt 1) {gsShuffled = True}) (MoveFx 3 []) (MoveApplied 5) @?= "Drag +5"

move_text_click_pinned :: Assertion
move_text_click_pinned = do
  pinned UiClick scoreGs scoreGs noFx
    [ "Need 4-neighbor adjacent", "No match; rolled back", "Cleared +120", "Level clear +450 -> L4 (N/Space/click)"
    , "YOU WIN score=9000 — N/click", "Out of moves score=210 — 再冲冲分数吧，目标 300 — R/click" ]
  -- 连击 > 1 才写，1 不写
  moveMsg UiClick scoreGs scoreGs (MoveFx 1 []) (MoveApplied 60) @?= "Cleared +60"
  moveMsg UiClick scoreGs scoreGs (MoveFx 3 []) (MoveApplied 60) @?= "Cleared +60 combo x3"
  moveMsg UiClick scoreGs scoreGs (MoveFx 3 []) (LevelClear 60 0) @?= "Level clear +60 combo x3 -> L1 (N/Space/click)"
  -- 收集进度读走步后的状态（第 2 关：收集红色 20 个），自动洗牌读走步后的 gsShuffled；顺序：连击、进度、洗牌
  let collectGs = levelAt 1
  moveMsg UiClick scoreGs collectGs noFx (MoveApplied 60) @?= "Cleared +60 [收集红色宝石 0/20]"
  moveMsg UiClick scoreGs collectGs {gsShuffled = True} (MoveFx 2 []) (MoveApplied 60)
    @?= "Cleared +60 combo x2 [收集红色宝石 0/20] (auto-shuffled)"
  moveMsg UiClick collectGs scoreGs {gsShuffled = True} noFx (MoveApplied 60) @?= "Cleared +60 (auto-shuffled)"
  -- 进度后缀只跟在 MoveApplied 后面
  moveMsg UiClick scoreGs collectGs (MoveFx 2 []) (LevelClear 60 1) @?= "Level clear +60 combo x2 -> L2 (N/Space/click)"

-- | Lost 的目标提示：交换读走步前的状态，道具读走步后的状态（同一局里目标不变，两者相同；这里故意给不同的目标钉住读哪个）。
move_text_lost_reads_goal_of :: Assertion
move_text_lost_reads_goal_of = do
  let gsBefore = scoreGs -- 目标 300
      gsAfter = levelAt 2 -- 目标 500
      lost ui = moveMsg ui gsBefore gsAfter noFx (Lost 1)
  lost UiDrag @?= "Out of moves score=1 — 再冲冲分数吧，目标 300"
  lost UiClick @?= "Out of moves score=1 — 再冲冲分数吧，目标 300 — R/click"
  map lost [UiHammer, UiCross, UiFreeSwap] @?= replicate 3 "再冲冲分数吧，目标 500"

move_text_keeps_tool_only_free_swap_no_match :: Assertion
move_text_keeps_tool_only_free_swap_no_match =
  [(ui, o) | ui <- [minBound .. maxBound], o <- outcomes, keepsTool ui o] @?= [(UiFreeSwap, NoMatch)]

-- | 走步结果的文案只写在 UI.MoveText：app/ 其余模块（去掉注释）里没有这些字面量。
move_text_tables_only_in_app_pure :: Assertion
move_text_tables_only_in_app_pure = do
  files <- filter (/= "app/pure/UI/MoveText.hs") <$> sourcesUnder "app"
  let needles =
        [ "Hammer failed", "Cross failed", "Free-swap invalid", "Free-swap: no match", "cleared level!", " won!"
        , "Need adjacent", "Need 4-neighbor", "Drag +", "Cleared +", "YOU WIN", "Out of moves", "Level clear" ]
  hits <- concat <$> mapM (\f -> (\src -> [(f, n) | n <- needles, n `isInfixOf` src]) <$> readCode f) files
  hits @?= []
