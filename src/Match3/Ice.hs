-- | Ice layers on gems (开心消消乐冰层): a match chips one layer;
-- when the last layer breaks the gem clears in the same wave.
module Match3.Ice
  ( chipIceOnClear
  , iceLayers
  , mkIceGem
  ) where

import Data.List (nub)
import Match3.Types

-- | Chip one ice layer on each seed / handle direct-hit peel locks.
-- ice>1: decrement, keep gem; ice==1: last layer + gem clear.
-- ice==0 + Chain/Curtain: peel one lock layer (gem stays) — hammer/cross/line.
-- ice==0 bare/Freeze: gem clears. Stone: chip one layer (like Safe).
chipIceOnClear :: Board -> [Pos] -> (Board, [Pos])
chipIceOnClear b seeds = foldl step (b, []) (nub seeds)
  where
    step (board, clearable) p =
      case get board p of
        Gem col kind n o
          | n > 1 ->
              (set board p (Gem col kind (n - 1) o), clearable)
          | n == 1 ->
              -- last ice: gem clears (overlays go with the cell)
              (board, p : clearable)
          | Just (Chain layers) <- o ->
              if layers <= 1
                then (set board p (Gem col kind 0 Nothing), clearable)
                else (set board p (Gem col kind 0 (Just (Chain (layers - 1)))), clearable)
          | Just (Curtain layers) <- o ->
              if layers <= 1
                then (set board p (Gem col kind 0 Nothing), clearable)
                else (set board p (Gem col kind 0 (Just (Curtain (layers - 1)))), clearable)
          | otherwise ->
              -- bare gem / Freeze / Fog / Steam / Grass…: clear
              (board, p : clearable)
        Stone n ->
          -- Direct hit chips one stone layer (hammer/cross); last layer removes
          if n <= 1
            then (board, p : clearable)
            else (set board p (mkStoneLayers (n - 1)), clearable)
        Chest _ ->
          (board, p : clearable)
        Honey _ ->
          (board, p : clearable)
        Balloon _ ->
          (board, p : clearable)
        Cookie ->
          (board, p : clearable)
        Cake _ ->
          (board, p : clearable)
        MagicHat ->
          (board, p : clearable)
        Maker _ _ ->
          -- Makers only charge via adjacent same-color; immune to direct clear seeds
          (board, clearable)
        Snail _ _ ->
          -- Snails crawl; immune to direct clear seeds (persist as mobile blockers)
          (board, clearable)
        Safe n ->
          -- Direct hit chips safe; last layer opens into Cookie (stays on board)
          if n <= 1
            then (set board p mkCookie, clearable)
            else (set board p (mkSafeLayers (n - 1)), clearable)
        Flip _ back ->
          -- Dual-face: first hit flips to Normal gem of back color (does not clear)
          (set board p (mkGem back), clearable)
        Surprise ->
          -- Hammer / direct seed: treat as opened explosion center (clears)
          (board, p : clearable)
        Bottle _ ->
          -- Dye bottle immune to direct clear (like Maker); stays
          (board, clearable)
        TimeSpirit ->
          -- Time spirit clears on direct hit (hammer / blast)
          (board, p : clearable)
        Countdown _ _ ->
          (board, p : clearable)
    get board (r, c) = (board !! r) !! c
    set board (r, c) v =
      take r board
        ++ [take c row ++ [v] ++ drop (c + 1) row]
        ++ drop (r + 1) board
      where
        row = board !! r
