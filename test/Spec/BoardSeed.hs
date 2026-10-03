{-# LANGUAGE OverloadedStrings #-}

-- | 按盘面散列选格（Match3.Element.Builtin.Common 的 boardSeed / posSeed / pickBy）。
--
-- 毛球跳格（第 43 关）与雪怪召唤（第 45 关）的选格散列的是 @show board@：派生 'Show' 的输出就是规则的一部分。
-- 这里把几张固定盘面的散列值、Show 字符串、第 43 / 45 关开局的实际选格都写死：
-- 改了 Cell / Grid 的 Show（加构造器、调字段、换字段类型）或改了散列，这组测试会先于金标准失败，并说明原因。
module Spec.BoardSeed
  ( tests
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Match3.Core
import Match3.Element (fuzzballJumps, snowBossSpawn, snowBosses)
import Match3.Element.Builtin.Common (boardSeed, pickBy, posSeed)
import Spec.Support.Arbitrary (genAnyBoard)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testCase "board_seed_pins_show_dependency" board_seed_pins_show_dependency
  , testCase "board_seed_pins_fuzzball_and_boss_choices" board_seed_pins_fuzzball_and_boss_choices
  , testProperty "qc_board_seed_is_fnv1a64_of_show" qc_board_seed_is_fnv1a64_of_show
  ]

-- | 失败时的说明。
why :: String
why =
  "boardSeed = FNV-1a(show board)：Show Cell / Show Grid 改了（或散列改了）会改变第 43 关毛球跳格与第 45 关雪怪召唤的选格；"
    ++ "要么保住旧的 Show 输出，要么确认是有意的玩法变化并重录金标准，再更新这里的数值。"

small :: Board
small = boardFromRows [[mkGem C1, mkGem C2], [Stone 2, Custom "fuzzball" (CustomState 1)]]

-- | 带特效、冰、叠层、蜗牛、倒计时、果汁机、Custom 的盘面（Show 里出现的字段种类尽量多）。
mixed :: Board
mixed =
  boardFromRows
    [ [Gem C3 LineH 1 (Just (Fog 2)), Snail 0 1, Countdown C4 3]
    , [Maker C5 2, Gem C2 Normal 0 (Just Grass), Custom "snow_boss" (CustomState 4660)]
    ]

opening :: Int -> Board
opening li = gsBoard (fromMaybe (error ("no level " ++ show li)) (campaignGame li 1))

-- | 写死的 Show 字符串与散列值。
board_seed_pins_show_dependency :: Assertion
board_seed_pins_show_dependency = do
  assertEqual ("show small（" ++ why ++ "）")
    "[[Gem C1 Normal 0 Nothing,Gem C2 Normal 0 Nothing],[Stone 2,Custom \"fuzzball\" 1]]"
    (show small)
  assertEqual ("boardSeed small（" ++ why ++ "）") 9964182599890457731 (boardSeed small)
  assertEqual ("boardSeed mixed（" ++ why ++ "）") 3638797480788856494 (boardSeed mixed)
  assertEqual ("boardSeed 第 43 关种子 1 开局（" ++ why ++ "）") 11591321152028132765 (boardSeed (opening 42))
  assertEqual ("boardSeed 第 45 关种子 1 开局（" ++ why ++ "）") 16479831807265665447 (boardSeed (opening 44))
  assertEqual ("posSeed（" ++ why ++ "）")
    [16978737048683228184, 6314448312567794895, 12011035506999344416]
    (map posSeed [(0, 0), (3, 4), (7, 7)])

-- | 第 43 关种子 1 开局的毛球跳格、第 45 关种子 1 开局的雪怪召唤格。
board_seed_pins_fuzzball_and_boss_choices :: Assertion
board_seed_pins_fuzzball_and_boss_choices = do
  assertEqual ("第 43 关种子 1 开局的毛球跳格（" ++ why ++ "）")
    [ ((0, 1), (0, 2)), ((0, 6), (1, 6)), ((1, 3), (1, 4)), ((2, 0), (3, 0)), ((2, 5), (2, 4)), ((3, 2), (2, 2)), ((3, 7), (4, 7))
    , ((4, 4), (5, 4)), ((5, 1), (6, 1)), ((5, 6), (5, 5)), ((6, 3), (5, 3)), ((6, 7), (6, 6)), ((7, 0), (6, 0)), ((7, 5), (7, 4))
    ]
    (fst (fuzzballJumps [] [] (opening 42)))
  let b45 = opening 44
  assertEqual ("第 45 关种子 1 开局的雪怪召唤格（" ++ why ++ "）")
    [((2, 3), Just (1, 4))]
    [(a, snowBossSpawn [] [] b45 a) | (a, _) <- snowBosses b45]

-- | 散列就是标准的 64 位 FNV-1a（与 Word64 写法的参考实现逐值相同），输入是 show；pickBy = 下标 (散列 mod 候选数)。
qc_board_seed_is_fnv1a64_of_show :: Property
qc_board_seed_is_fnv1a64_of_show =
  forAll genAnyBoard $ \b -> forAll arbitrary $ \(r, c) -> forAll arbitrary $ \(h, x, xs) ->
    let ys = (x :: Int) : xs
    in conjoin
         [ counterexample "boardSeed" (boardSeed b === toInteger (fnv64 (show b)))
         , counterexample "posSeed" (posSeed (r, c) === toInteger (fnv64 (show ((r, c) :: Pos))))
         , counterexample "pickBy" (pickBy (abs h) (x :| xs) === ys !! fromInteger (abs h `mod` toInteger (length ys)))
         ]
  where
    fnv64 :: String -> Word64
    fnv64 = foldl' (\acc ch -> (acc `xor` fromIntegral (ord ch)) * 1099511628211) 14695981039346656037
