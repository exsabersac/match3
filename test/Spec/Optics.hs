{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Haskell 特性第 6 项：光学（docs/haskell-features/06-测试与光学.md）。
--
-- * 定律：透镜 get-put / put-get / put-put（盘面一格、GameState 的字段与派生读数）、遍历的恒等与合成律、
--   棱镜的两条往返律；派生读数的 get-put 只在「这种关卡级元素恰好一份」时成立，反例用 expectFailure 固定下来；
-- * Match3.Grass / Match3.Types.Overlay 的行为由 9 个叠层单元测试、元素查询快照与金标准锁定。
module Spec.Optics
  ( tests
  ) where

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (isJust)
import Engine.Optics
import Match3.Core
import Match3.Types (boardPositions)
import Match3.Element.Mechanic (mechNameOf)
import Match3.Game.State (gsBeltsL, gsBoardL, gsCarpetOpenL, gsCrossClearsL, gsFreeSwapsL, gsGroundL, gsHammersL, gsMovesL, gsPortalsL, gsUfosL)
import Match3.Types (boardCells, cellOverlay, isGem)
import Match3.Types.Optics
import Match3.Ufo (mkUfo)
import Spec.Properties (genColor, genGem, genOverlay, genCell, genPick, genPos, genStart, playPicks, startState)
import Spec.Support.Arbitrary (AnyBoard(..), genAnyBoard)
import Engine.Game (Step(..))
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testProperty "qc_optics_cell_at_lens_laws" qc_optics_cell_at_lens_laws
  , testProperty "qc_optics_state_lens_laws" qc_optics_state_lens_laws
  , testProperty "qc_optics_level_field_get_put_needs_invariant" qc_optics_level_field_get_put_needs_invariant
  , testProperty "qc_optics_traversal_laws" qc_optics_traversal_laws
  , testProperty "qc_optics_prism_laws" qc_optics_prism_laws
  ]

--------------------------------------------------------------------------------
-- 定律

-- | 透镜三定律（相等用给定的 eq；GameState 的 Eq 不比生成器等字段，所以另比 show）。
lensLaws :: (Show a, Eq a) => (s -> s -> Bool) -> String -> Lens' s a -> s -> a -> a -> Property
lensLaws eq name l s a b =
  counterexample name $
    conjoin
      [ counterexample "put-get: view l (set l a s) == a" (view l (set l a s) === a)
      , counterexample "get-put: set l (view l s) s == s" (set l (view l s) s `eq` s)
      , counterexample "put-put: set l b (set l a s) == set l b s" (set l b (set l a s) `eq` set l b s)
      ]

sameState :: GameState -> GameState -> Bool
sameState x y = x == y && show x == show y

-- | 盘面一格：任意行列、任意界内坐标、任意格。
qc_optics_cell_at_lens_laws :: Property
qc_optics_cell_at_lens_laws =
  property $ \(AnyBoard b) (NonNegative i) ->
    let ps = boardPositions b
        p = ps !! (i `mod` length ps)
    in forAll genCell $ \x -> forAll genCell $ \y ->
         lensLaws (==) ("cellAt " ++ show p) (cellAt p) b x y

-- | 局中状态（开局后随机走 0–3 步）。
genState :: Gen GameState
genState = do
  start <- genStart
  k <- choose (0, 3)
  picks <- vectorOf k genPick
  let s0 = startState start
      (_, steps) = playPicks s0 picks
  pure (last (s0 : map stepState steps))

-- | GameState 的字段透镜与五个派生读数的透镜：局中状态上三条定律都成立。
qc_optics_state_lens_laws :: Property
qc_optics_state_lens_laws =
  forAll genState $ \gs ->
    conjoin
      [ forAll2 genAnyBoard $ lensLaws sameState "gsBoardL" gsBoardL gs
      , forAll2 (choose (-3, 40)) $ lensLaws sameState "gsMovesL" gsMovesL gs
      , forAll2 (choose (-1, 5)) $ lensLaws sameState "gsHammersL" gsHammersL gs
      , forAll2 (choose (-1, 5)) $ lensLaws sameState "gsFreeSwapsL" gsFreeSwapsL gs
      , forAll2 (choose (-1, 5)) $ lensLaws sameState "gsCrossClearsL" gsCrossClearsL gs
      , forAll2 (small (mkUfo <$> genPos <*> genColor)) $ lensLaws sameState "gsUfosL" gsUfosL gs
      , forAll2 (small (small genPos)) $ lensLaws sameState "gsBeltsL" gsBeltsL gs
      , forAll2 (small ((,) <$> genPos <*> genPos)) $ lensLaws sameState "gsPortalsL" gsPortalsL gs
      , forAll2 (small genPos) $ lensLaws sameState "gsCarpetOpenL" gsCarpetOpenL gs
      , forAll2 (small ((,) <$> genPos <*> ((,) <$> elements ["moss", "qc_layer"] <*> choose (1, 3)))) $ lensLaws sameState "gsGroundL" gsGroundL gs
      ]
  where
    small g = choose (0, 3) >>= \k -> vectorOf k g
    forAll2 :: Show a => Gen a -> (a -> a -> Property) -> Property
    forAll2 g f = forAll g $ \a -> forAll g $ \b -> f a b

-- | 派生读数是「按名字找关卡级元素，没有就当空表」：元素不在 gsLevelElems 里时，写回读到的空表会**追加**一个元素，
-- get-put 不成立（put-get / put-put 仍成立）。开局总是带全部内置元素（startLevelsWith），所以游戏里碰不到；
-- 这里把反例固定下来——QuickCheck 必须找到反例，这条性质才算通过。
qc_optics_level_field_get_put_needs_invariant :: Property
qc_optics_level_field_get_put_needs_invariant =
  expectFailure $
    forAllBlind genState $ \gs ->
      let s = gs {gsLevelElems = filter ((/= "ufo") . mechNameOf) (gsLevelElems gs)}
          names = map mechNameOf . gsLevelElems
      in -- Show 只按旧字段名打印读数，看不出差别；Eq 比较 gsLevelElems，能看出多了一个空的 ufo 元素
         counterexample (show (names s) ++ " -> " ++ show (names (set gsUfosL (view gsUfosL s) s))) $
           set gsUfosL (view gsUfosL s) s == s

-- | 遍历定律：恒等律 t Identity = Identity；合成律 t (Compose . fmap g . f) = Compose . fmap (t g) . t f
-- （f 用 Writer 记下访问顺序，g 用 Maybe 可能失败）；另有 over 的合成与焦点个数。
traversalLaws :: (Eq s, Show s, Eq a, Show a) => String -> Traversal' s a -> (a -> a) -> (a -> a) -> (a -> Bool) -> s -> Property
traversalLaws name t f g ok s =
  counterexample name $
    conjoin
      [ counterexample "identity" (runIdentity (t Identity s) === s)
      , counterexample "composition" (getCompose (t (Compose . fmap g' . f') s) === fmap (t g') (t f' s))
      , counterexample "over fuses" (over t f (over t g s) === over t (f . g) s)
      , counterexample "toListOf order = Writer log" (fst (t f' s) === toListOf t s)
      ]
  where
    f' a = ([a], f a)
    g' a = if ok a then Just (g a) else Nothing

qc_optics_traversal_laws :: Property
qc_optics_traversal_laws =
  property $ \(AnyBoard b) ->
    let simplify = set gemOverlay Nothing
        recolor cell = case cell ^? _Gem of
          Just (_, k, i, o) -> review _Gem (C2, k, i, o)
          Nothing -> cell
        overlaid = length [() | cell <- boardCells b, isJust (cellOverlay cell)]
    in classify (overlaid > 0) "has overlays" $
         conjoin
           [ traversalLaws "cells" cells simplify recolor isGem b
           , traversalLaws "cells . gemOverlay" (cells . gemOverlay) (const Nothing) (fmap bump) isJust b
           , traversalLaws "cells . overlay . _Fog" (cells . overlay . _Fog) (+ 1) (* 2) even b
           , toListOf cells b === boardCells b
           , length (toListOf (cells . overlay) b) === overlaid
           , counterexample "fog +1 by hand" $
               over (cells . overlay . _Fog) (+ 1) b
                 === fmap (\cell -> case cell of Gem c k i (Just (Fog n)) -> Gem c k i (Just (Fog (n + 1))); _ -> cell) b
           ]
  where
    bump o = case o of
      Fog n -> Fog (n + 1)
      other -> other

-- | 棱镜：review 后 preview 拿回原值；preview 成功时 review 回去等于原值。
prismLaws :: (Eq s, Show s, Eq a, Show a) => String -> Prism' s a -> a -> s -> Property
prismLaws name p a s =
  counterexample name $
    conjoin
      [ counterexample "preview . review = Just" (preview p (review p a) === Just a)
      , counterexample "preview s = Just x => review x = s" (maybe (property True) (\x -> review p x === s) (preview p s))
      ]

qc_optics_prism_laws :: Property
qc_optics_prism_laws =
  forAll genOverlay $ \o ->
    forAll genCell $ \cell ->
      forAll genGem $ \gem ->
        forAll (choose (1, 4)) $ \n ->
          classify (isJust (preview overlay cell)) "cell has overlay" $
            conjoin
              [ prismLaws "_Fog" _Fog n o
              , prismLaws "_Chain" _Chain n o
              , prismLaws "_Freeze" _Freeze n o
              , prismLaws "_Curtain" _Curtain n o
              , prismLaws "only Grass" (only Grass) () o
              , prismLaws "only Steam" (only Steam) () o
              , prismLaws "_Just" _Just o (cellOverlay cell)
              , maybe (property False) (\g -> prismLaws "_Gem" _Gem g cell) (preview _Gem gem)
              ]
