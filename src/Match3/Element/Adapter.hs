{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | 过渡期的适配器（元素类重构第 1 刀，第 2 刀删除）：把新类（Match3.Element.Ability / Kind / Layer）的
-- instance 变成旧注册表的条目（'Entry'），新旧两种写法可以在同一张注册表里并存、逐项对照。
module Match3.Element.Adapter
  ( kindEntry
  , layerEntry
  , groundKindEntry
  ) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import qualified Match3.Element.Ability as A
import qualified Match3.Element.Class as C
import Match3.Element.Kind
import Match3.Element.Layer
import Match3.Element.Types (AdjacentRule(..))
import Match3.Element.Registry (Entry, bodyEntry, customEntryWith, groundEntry, modifierEntry)
import Match3.Types

-- | 新写法的本体种类 → 旧条目（原型值只用来推旧注册表的槽位）。
kindEntry :: forall e. Kind e => e -> Entry
kindEntry proto = case A.toCell proto of
  Custom n _ -> customEntryWith (Old proto) (\s -> Old (fromMaybe (error ("kindEntry: cannot decode " ++ show n)) (fromCell (Custom n s)))) (place p)
  _ -> bodyEntry (Old proto) (fmap Old . fromCell) (place p)
  where
    p = Proxy :: Proxy e

newtype Old e = Old e
  deriving (Eq, Show)

instance Kind e => C.Element (Old e) where
  name (Old e) = A.nameOf e
  toCell (Old e) = A.toCell e
  caps (Old e) =
    C.Caps
      { C.capArchetype = C.Piece
      , C.capMatch = C.MatchCaps (const (A.color e)) (A.blocksMatch e) (A.blocksSwap e) (A.hintable e) (listToMaybe [r | SwapPass r <- passes])
      , C.capHit = C.HitCaps (Just (A.fires e)) (hit (A.struck e)) (A.blast e) False (listToMaybe [AdjacentRule o f | AdjacentPass o f <- passes]) (listToMaybe [r | OpenPass r <- passes])
      , C.capMove = C.MoveCaps (A.falls e) (A.portal e) (A.drains e) (A.keepOnShuffle e) (A.recolorable e) (A.pushable e)
      , C.capCount = C.CountCaps (A.counter e) (diffCounter p) (A.diffWeight e) (bonusMoves p) (A.vacatesCarpet e)
      , C.capStep = C.StepCaps (listToMaybe [r | EndPass r <- passes]) Nothing Nothing (const Nothing)
      , C.capView = C.ViewCaps (A.face e) (label p) (loseHint p)
      }
    where
      p = Proxy :: Proxy e
      passes = boardPasses p
      hit s = case s of
        A.Absorb cell -> C.Absorb (C.SomeElement (C.Inert (A.nameOf e) cell))
        A.Destroy -> C.Destroy
        A.Immune -> C.Immune

-- | 新写法的叠层 → 旧条目。
layerEntry :: forall l. Layer l => l -> Entry
layerEntry proto = modifierEntry (OldL proto) (fmap (OldL . fst) . peel) (layerPlace (Proxy :: Proxy l))

newtype OldL l = OldL l
  deriving (Eq, Show)

instance Layer l => C.Modifier (OldL l) where
  modName (OldL _) = layerName (Proxy :: Proxy l)
  modApply (OldL l) = putOn l
  modBlocksMatch (OldL l) = layerBlocksMatch l
  modBlocksSwap (OldL l) = layerBlocksSwap l
  modActivates (OldL l) = layerFires l
  modOnHit (OldL l) = case layerHit l of
    Pierce -> C.Pierce
    Keep l' -> C.Keep (OldL l')
    Peel -> C.Remove
    Shatter -> C.Shatter
  modStripOnClear (OldL l) = layerStripsOnClear l
  modAdjacent _ = listToMaybe [AdjacentRule o f | AdjacentPass o f <- layerPasses (Proxy :: Proxy l)]
  modEnd _ = listToMaybe [r | EndPass r <- layerPasses (Proxy :: Proxy l)]

-- | 新写法的地面层 → 旧条目。
groundKindEntry :: forall g. (GroundKind g, Typeable g) => Entry
groundKindEntry = groundEntry (OldG :: OldG g)

data OldG g = OldG
  deriving (Eq, Show)

instance (GroundKind g, Typeable g) => C.Element (OldG g) where
  name _ = groundName (Proxy :: Proxy g)
  caps _ =
    let c = C.capsOf C.Piece
        p = Proxy :: Proxy g
    in c
         { C.capCount = (C.capCount c) {C.ccCounter = groundCounter p}
         , C.capStep = (C.capStep c) {C.stGround = Just (groundHit p), C.stWiden = groundWiden p}
         , C.capView = (C.capView c) {C.vwLabel = groundLabel p, C.vwLoseHint = groundLoseHint p}
         }
  toCell _ = Custom (groundName (Proxy :: Proxy g)) (CustomState 1)
