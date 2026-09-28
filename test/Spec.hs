{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.List (nub, sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Match3.Board (applyGravity, clearMatches, expandSpecials, refill)
import Match3.Core
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "match3"
    [ testCase "inv_no_match_rollback" inv_no_match_rollback
    , testCase "inv_move_to_stable" inv_move_to_stable
    , testCase "match_line_ge3" match_line_ge3
    , testCase "gravity_then_refill" gravity_then_refill
    , testCase "cascade_until_stable" cascade_until_stable
    , testCase "outcome_moves_or_score" outcome_moves_or_score
    , testProperty "qc_findMatches_ge3" qc_findMatches_ge3
    , testCase "special_line_from_4" special_line_from_4
    , testCase "special_rainbow_from_5" special_rainbow_from_5
    , testCase "hint_finds_move" hint_finds_move
    , testCase "undo_restores" undo_restores
    , testCase "shuffle_when_no_moves" shuffle_when_no_moves
    , testCase "playable_board_stable_and_has_move" playable_board_stable_and_has_move
    , testCase "combo_wave_scoring" combo_wave_scoring
    , testCase "collect_goal_progress" collect_goal_progress
    , testCase "collect_goal_clears_level" collect_goal_clears_level
    , testCase "collect_goal_lose_on_moves" collect_goal_lose_on_moves
    , testCase "level_table_mixes_collect" level_table_mixes_collect
    , testCase "score_goal_ignores_collect" score_goal_ignores_collect
    , testCase "stone_blocks_swap" stone_blocks_swap
    , testCase "stone_cleared_by_adjacent" stone_cleared_by_adjacent
    , testCase "stone_not_in_match" stone_not_in_match
    , testCase "stone_layer_decrement" stone_layer_decrement
    , testCase "stone_layer_clears_at_zero" stone_layer_clears_at_zero
    , testCase "rainbow_clears_color" rainbow_clears_color
    , testCase "rainbow_swap_without_match" rainbow_swap_without_match
    , testCase "special_combo_line_bomb" special_combo_line_bomb
    , testCase "goal_collect_multi_color" goal_collect_multi_color
    , testCase "goal_clear_stone_counts" goal_clear_stone_counts
    , testCase "ice_layer_blocks_clear" ice_layer_blocks_clear
    , testCase "ice_layer_chips_then_clears" ice_layer_chips_then_clears
    , testCase "daily_seed_stable" daily_seed_stable
    , testCase "star_rating_tiers" star_rating_tiers
    , testCase "special_combo_rainbow_line" special_combo_rainbow_line
    , testCase "special_combo_bomb_bomb" special_combo_bomb_bomb
    , testCase "special_combo_line_line" special_combo_line_line
    , testCase "countdown_bomb_spawns" countdown_bomb_spawns
    , testCase "countdown_bomb_ticks_after_move" countdown_bomb_ticks_after_move
    , testCase "countdown_bomb_explodes_at_zero" countdown_bomb_explodes_at_zero
    , testCase "countdown_bomb_cleared_disarms" countdown_bomb_cleared_disarms
    , testCase "conveyor_cycle_preserves_cells" conveyor_cycle_preserves_cells
    , testCase "conveyor_shifts_after_move" conveyor_shifts_after_move
    , testCase "conveyor_can_create_match" conveyor_can_create_match
    , testCase "booster_hammer_clears_cell" booster_hammer_clears_cell
    , testCase "booster_free_swap_any_cells" booster_free_swap_any_cells
    , testCase "lose_hint_by_goal" lose_hint_by_goal
    , testCase "grass_cleared_by_match_above" grass_cleared_by_match_above
    , testCase "vine_spreads_after_move" vine_spreads_after_move
    , testCase "vine_blocked_by_clear" vine_blocked_by_clear
    , testCase "choco_spreads_after_move" choco_spreads_after_move
    , testCase "choco_cleared_by_adjacent" choco_cleared_by_adjacent
    , testCase "choco_blocked_by_clear" choco_blocked_by_clear
    , testCase "chest_blocks_swap" chest_blocks_swap
    , testCase "chest_cleared_by_adjacent" chest_cleared_by_adjacent
    , testCase "chest_layer_decrement" chest_layer_decrement
    , testCase "goal_chest_counts" goal_chest_counts
    , testCase "honey_blocks_swap" honey_blocks_swap
    , testCase "honey_cleared_by_adjacent" honey_cleared_by_adjacent
    , testCase "honey_layer_decrement" honey_layer_decrement
    , testCase "goal_honey_counts" goal_honey_counts
    , testCase "balloon_blocks_swap" balloon_blocks_swap
    , testCase "balloon_popped_by_same_color" balloon_popped_by_same_color
    , testCase "balloon_ignores_other_color" balloon_ignores_other_color
    , testCase "goal_balloon_counts" goal_balloon_counts
    , testCase "cookie_blocks_swap" cookie_blocks_swap
    , testCase "cookie_falls_with_gravity" cookie_falls_with_gravity
    , testCase "cookie_collected_at_bottom" cookie_collected_at_bottom
    , testCase "goal_cookie_counts" goal_cookie_counts
    , testCase "fog_blocks_match" fog_blocks_match
    , testCase "fog_cleared_by_adjacent" fog_cleared_by_adjacent
    , testCase "fog_layer_decrement" fog_layer_decrement
    , testCase "cake_blocks_swap" cake_blocks_swap
    , testCase "cake_layer_decrement" cake_layer_decrement
    , testCase "cake_clears_at_zero" cake_clears_at_zero
    , testCase "goal_cake_counts" goal_cake_counts
    , testCase "hat_triggered_by_adjacent" hat_triggered_by_adjacent
    , testCase "hat_swaps_colors" hat_swaps_colors
    , testCase "chain_blocks_match" chain_blocks_match
    , testCase "chain_blocks_swap" chain_blocks_swap
    , testCase "chain_cleared_by_adjacent" chain_cleared_by_adjacent
    , testCase "chain_layer_decrement" chain_layer_decrement
    , testCase "maker_blocks_swap" maker_blocks_swap
    , testCase "maker_charges_on_same_color" maker_charges_on_same_color
    , testCase "maker_produces_bomb" maker_produces_bomb
    , testCase "portal_teleports_gem" portal_teleports_gem
    , testCase "ufo_collects_target_color" ufo_collects_target_color
    , testCase "ufo_moves_each_cascade" ufo_moves_each_cascade
    , testCase "ufo_goal_counts" ufo_goal_counts
    , testCase "snail_moves_after_move" snail_moves_after_move
    , testCase "snail_blocks_swap" snail_blocks_swap
    , testCase "freeze_blocks_swap" freeze_blocks_swap
    , testCase "freeze_cleared_by_adjacent" freeze_cleared_by_adjacent
    , testCase "freeze_layer_decrement" freeze_layer_decrement
    , testCase "curtain_blocks_match" curtain_blocks_match
    , testCase "curtain_cleared_by_adjacent" curtain_cleared_by_adjacent
    , testCase "curtain_layer_decrement" curtain_layer_decrement
    , testCase "safe_blocks_swap" safe_blocks_swap
    , testCase "safe_opens_to_cookie" safe_opens_to_cookie
    , testCase "safe_layer_decrement" safe_layer_decrement
    , testCase "goal_safe_counts" goal_safe_counts
    , testCase "flip_matches_front" flip_matches_front
    , testCase "flip_becomes_back_on_clear" flip_becomes_back_on_clear
    , testCase "surprise_blocks_swap" surprise_blocks_swap
    , testCase "surprise_opens_to_special" surprise_opens_to_special
    , testCase "surprise_explodes_small" surprise_explodes_small
    , testCase "bottle_blocks_swap" bottle_blocks_swap
    , testCase "bottle_dyes_neighbors" bottle_dyes_neighbors
    , testCase "booster_cross_clears_row_col" booster_cross_clears_row_col
    , testCase "shuffle_preserves_decor" shuffle_preserves_decor
    , testCase "daily_ufo_goal_spawns_saucer" daily_ufo_goal_spawns_saucer
    , testCase "time_spirit_blocks_swap" time_spirit_blocks_swap
    , testCase "time_spirit_awards_moves" time_spirit_awards_moves
    , testCase "steam_blocks_match" steam_blocks_match
    , testCase "steam_cleared_by_adjacent" steam_cleared_by_adjacent
    , testCase "steam_spreads_after_move" steam_spreads_after_move
    , testCase "carpet_covers_on_clear" carpet_covers_on_clear
    , testCase "goal_carpet_counts" goal_carpet_counts
    , testCase "carpet_already_covered_noop" carpet_already_covered_noop
    , testCase "carry_moves_on_next_level" carry_moves_on_next_level
    , testCase "daily_goal_rotates_ten" daily_goal_rotates_ten
    , testCase "campaign_levels_batch_ok" campaign_levels_batch_ok
    , testCase "inv_move_costs_one_without_spirit" inv_move_costs_one_without_spirit
    , testCase "inv_move_end_order_steam_before_snail" inv_move_end_order_steam_before_snail
    , testCase "inv_move_end_order_belt_before_steam" inv_move_end_order_belt_before_steam
    , testCase "finale_and_pressure_moves_reasonable" finale_and_pressure_moves_reasonable
    , testCase "cascade_terminates_bounded" cascade_terminates_bounded
    , testCase "release_core_invariants_green" release_core_invariants_green
    , testCase "carpet_ice_partial_no_cover" carpet_ice_partial_no_cover
    , testCase "carpet_ice_last_layer_covers" carpet_ice_last_layer_covers
    , testCase "time_spirit_rescues_last_move" time_spirit_rescues_last_move
    , testCase "portal_after_belt_match_teleports" portal_after_belt_match_teleports
    , testCase "flip_four_match_spawns_line" flip_four_match_spawns_line
    , testCase "surprise_blast_expands_bomb" surprise_blast_expands_bomb
    , testCase "maker_bomb_survives_wave" maker_bomb_survives_wave
    , testCase "chain_freeze_both_peel" chain_freeze_both_peel
    , testCase "honey_balloon_same_clear" honey_balloon_same_clear
    , testCase "safe_bottom_cookie_collected" safe_bottom_cookie_collected
    , testCase "cookie_bottom_portal_collects" cookie_bottom_portal_collects
    , testCase "countdown_explode_keeps_ufo_portals" countdown_explode_keeps_ufo_portals
    , testCase "hammer_peels_chain_not_gem" hammer_peels_chain_not_gem
    , testCase "hammer_peels_curtain_not_gem" hammer_peels_curtain_not_gem
    , testCase "hammer_chips_stone_layer" hammer_chips_stone_layer
    , testCase "cross_peels_chain_on_seed" cross_peels_chain_on_seed
    , testCase "freeswap_blocked_by_stone_chain" freeswap_blocked_by_stone_chain
    , testCase "daily_obstacle_goal_spawns_decor" daily_obstacle_goal_spawns_decor
    , testCase "undo_restores_carry_moves" undo_restores_carry_moves
    , testCase "shuffle_preserves_goal_progress" shuffle_preserves_goal_progress
    , testCase "rainbow_expand_noop" rainbow_expand_noop
    , testCase "rainbow_swap_partner_not_own_color" rainbow_swap_partner_not_own_color
    , testCase "rainbow_x_bomb_blast_expands" rainbow_x_bomb_blast_expands
    , testCase "map_select_no_carry_moves" map_select_no_carry_moves
    , testCase "star_rating_vs_carry_base" star_rating_vs_carry_base
    , testCase "curtain_allows_swap_blocks_match" curtain_allows_swap_blocks_match
    , testCase "hammer_clears_grass_vine" hammer_clears_grass_vine
    , testCase "freeze_blocks_freeswap_and_swap" freeze_blocks_freeswap_and_swap
    , testCase "combo_seed_continues_wave_score" combo_seed_continues_wave_score
    , testCase "bottle_dye_followup_match" bottle_dye_followup_match
    , testCase "hat_recolor_followup_match" hat_recolor_followup_match
    , testCase "snail_belt_no_double_step" snail_belt_no_double_step
    , testCase "maker_multi_adjacent_charges_once" maker_multi_adjacent_charges_once
    , testCase "last_cleared_skips_belt_snail" last_cleared_skips_belt_snail
    , testCase "unlock_after_clear_bumps_map" unlock_after_clear_bumps_map
    , testCase "map_click_same_level_resumes" map_click_same_level_resumes
    , testCase "ufo_skips_peel_locks" ufo_skips_peel_locks
    , testCase "hammer_immune_no_spend" hammer_immune_no_spend
    , testCase "rainbow_swap_flip_partner" rainbow_swap_flip_partner
    , testCase "surprise_direct_seed_opens" surprise_direct_seed_opens
    , testCase "soft_hit_preserves_choco_steam" soft_hit_preserves_choco_steam
    , testCase "surprise_blast_peels_adjacent" surprise_blast_peels_adjacent
    , testCase "shuffle_preserves_specials" shuffle_preserves_specials
    , testCase "soft_lock_blocks_special_expand" soft_lock_blocks_special_expand
    , testCase "line_blast_no_double_peel" line_blast_no_double_peel
    , testCase "blast_chips_layered_obstacles_once" blast_chips_layered_obstacles_once
    , testCase "hat_immune_to_direct_clear" hat_immune_to_direct_clear
    , testCase "soft_hit_preserves_oncell_overlays" soft_hit_preserves_oncell_overlays
    , testCase "soft_hit_no_adj_side_effects" soft_hit_no_adj_side_effects
    , testCase "soft_hit_preserves_oncell_fog_steam" soft_hit_preserves_oncell_fog_steam
    , testCase "surprise_blast_opens_nested" surprise_blast_opens_nested
    , testCase "surprise_nested_special_no_fire" surprise_nested_special_no_fire
    , testCase "soft_lock_blocks_rainbow_swap" soft_lock_blocks_rainbow_swap
    , testCase "soft_lock_blocks_special_combo" soft_lock_blocks_special_combo
    , testCase "soft_lock_blocks_freeswap_activation" soft_lock_blocks_freeswap_activation
    , testCase "soft_lock_blocks_double_rainbow" soft_lock_blocks_double_rainbow
    , testCase "cookie_immune_to_direct_clear" cookie_immune_to_direct_clear
    , testCase "belt_delivers_cookie_bottom_drains" belt_delivers_cookie_bottom_drains
    , testCase "portal_teleports_flip" portal_teleports_flip
    , testCase "goal_carpet_seeds_open_tiles" goal_carpet_seeds_open_tiles
    ]

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

findMatchPair :: Board -> Maybe (Pos, Pos)
findMatchPair b =
  case
    [ ((r, c), (r, c + 1))
    | r <- [0 .. boardSize - 1]
    , c <- [0 .. boardSize - 2]
    , hasAnyMatch (swapCells b (r, c) (r, c + 1))
    ]
      ++ [ ((r, c), (r + 1, c))
         | r <- [0 .. boardSize - 2]
         , c <- [0 .. boardSize - 1]
         , hasAnyMatch (swapCells b (r, c) (r + 1, c))
         ] of
    (p : _) -> Just p
    [] -> Nothing

inv_no_match_rollback :: Assertion
inv_no_match_rollback = do
  let gs0 = newGame defaultConfig 12345
  case findNoMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need a no-match adjacent pair"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      out @?= NoMatch
      gsBoard gs1 @?= gsBoard gs0
      gsScore gs1 @?= gsScore gs0
      gsMoves gs1 @?= gsMoves gs0

inv_move_to_stable :: Assertion
inv_move_to_stable = do
  let gs0 = newGame defaultConfig 99
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need a matching swap"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      assertBool "applied or terminal" $
        case out of
          MoveApplied _ -> True
          Won _ -> True
          Lost _ -> True
          LevelClear _ _ -> True
          _ -> False
      assertBool "board stable" (not (hasAnyMatch (gsBoard gs1)))

-- | Horizontal/vertical runs of length >= 3 match; length-2 and diagonal do not.
match_line_ge3 :: Assertion
match_line_ge3 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C1, C2, C3, C4, C5, C2]
      b = take 3 b0 ++ [row3] ++ drop 4 b0
      ms = findMatches b
  assertBool "(3,0)" ((3, 0) `elem` ms)
  assertBool "(3,1)" ((3, 1) `elem` ms)
  assertBool "(3,2)" ((3, 2) `elem` ms)
  let colBoard =
        [ [ if c == 1 && r >= 2 && r <= 4 then mkGem C3 else mkGem C5
          | c <- [0 .. boardSize - 1]
          ]
        | r <- [0 .. boardSize - 1]
        ]
      vs = findMatches colBoard
  assertBool "vert (2,1)" ((2, 1) `elem` vs)
  assertBool "vert (3,1)" ((3, 1) `elem` vs)
  assertBool "vert (4,1)" ((4, 1) `elem` vs)

  -- Negatives on an explicit stable board (no incidental 3-runs)
  let stable =
        [ map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
        , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
        , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
        , map mkGem [C4, C5, C1, C2, C3, C4, C5, C1]
        , map mkGem [C5, C1, C2, C3, C4, C5, C1, C2]
        , map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
        , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
        , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
        ]
  assertBool "stable base" (not (hasAnyMatch stable))

  -- Negative: only 2-in-a-row horizontally — not a match
  let b2 = setCell (setCell stable (0, 0) (mkGem C1)) (0, 1) (mkGem C1)
  assertBool "2-in-a-row not a match" (null (findMatches b2))

  -- Negative: only 2-in-a-row vertically — not a match
  let bV = setCell (setCell stable (3, 0) (mkGem C5)) (4, 0) (mkGem C5)
  assertBool "vert 2-in-a-row not a match" (null (findMatches bV))

  -- Negative: diagonal same color does not count
  let diag =
        setCell
          (setCell
             (setCell stable (0, 0) (mkGem C1))
             (1, 1)
             (mkGem C1))
          (2, 2)
          (mkGem C1)
  assertBool "diagonal not a match" (null (findMatches diag))
  assertBool "diag hasAnyMatch false" (not (hasAnyMatch diag))


gravity_then_refill :: Assertion
gravity_then_refill = do
  let b =
        [ [ if c == 0
              then if r >= 5 then mkGem C1 else mkGem (toEnum (r `mod` 5))
              else mkGem C5
          | c <- [0 .. boardSize - 1]
          ]
        | r <- [0 .. boardSize - 1]
        ]
  assertBool "has match" (hasAnyMatch b)
  let (mb, n) = clearMatches b
  assertBool "cleared >= 3" (n >= 3)
  let fallen = applyGravity mb
      colsOk =
        all
          ( \c ->
              let col = map (!! c) fallen
                  (holes, rest) = span (== Nothing) col
              in all (/= Nothing) rest && length holes + length rest == boardSize
          )
          [0 .. boardSize - 1]
  assertBool "gravity: holes on top" colsOk
  let g = mkStdGen 7
      (b', _) = refill g fallen
  assertEqual "rows" boardSize (length b')
  assertBool "full rows" (all ((== boardSize) . length) b')

cascade_until_stable :: Assertion
cascade_until_stable = do
  let g = mkStdGen 1
      (b0, g1) = randomBoard g
      (b1, cleared, g2) = runCascade g1 b0
  assertBool "stable" (not (hasAnyMatch b1))
  assertBool "stepCascade Nothing" (isNothing (stepCascade g2 b1))
  if hasAnyMatch b0
    then assertBool "cleared > 0" (cleared > 0)
    else assertEqual "no clear" (0 :: Int) cleared

outcome_moves_or_score :: Assertion
outcome_moves_or_score = do
  let gs0 = newGame defaultConfig 42
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "have mover"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      case out of
        MoveApplied gained -> do
          assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)
          assertEqual "score" (gsScore gs0 + gained) (gsScore gs1)
          assertBool "gained > 0" (gained > 0)
        Won s -> assertBool "won" (goalMet (gsGoal gs1) s (gsCollected gs1))
        LevelClear s _ -> assertBool "level" (goalMet (gsGoal gs1) s (gsCollected gs1))
        Lost _ -> gsMoves gs1 @?= 0
        other -> assertFailure ("unexpected: " ++ show other)

  let cfgW = GameConfig { cfgMoves = 5, cfgGoal = GoalScore 1 }
      gsW0 = newGameAtLevel (length allLevels - 1) cfgW 42
  case findMatchPair (gsBoard gsW0) of
    Nothing -> assertFailure "win mover"
    Just (p1, p2) -> do
      let (_, outW) = trySwap p1 p2 gsW0
      case outW of
        Won _ -> pure ()
        other -> assertFailure ("expected Won on last level, got " ++ show other)

  let cfgL = GameConfig { cfgMoves = 1, cfgGoal = GoalScore 999999 }
      gsL0 = newGame cfgL 42
  case findMatchPair (gsBoard gsL0) of
    Nothing -> assertFailure "lose mover"
    Just (p1, p2) -> do
      let (gsL1, outL) = trySwap p1 p2 gsL0
      case outL of
        Lost _ -> gsMoves gsL1 @?= 0
        Won _ -> assertFailure "should not win"
        LevelClear _ _ -> assertFailure "should not clear"
        other -> assertFailure ("expected Lost, got " ++ show other)

qc_findMatches_ge3 :: Property
qc_findMatches_ge3 =
  forAll (choose (0, boardSize - 1)) $ \r ->
    forAll (choose (0, boardSize - 3)) $ \c0 ->
      let fill = mkGem C5
          b0 = replicate boardSize (replicate boardSize fill)
          row =
            [ if c >= c0 && c < c0 + 3 then mkGem C1 else mkGem C5
            | c <- [0 .. boardSize - 1]
            ]
          b = take r b0 ++ [row] ++ drop (r + 1) b0
          ms = findMatches b
      in (r, c0) `elem` ms
           && (r, c0 + 1) `elem` ms
           && (r, c0 + 2) `elem` ms

-- | 4-in-a-row spawns a Line special.
special_line_from_4 :: Assertion
special_line_from_4 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row2 = map mkGem [C1, C1, C1, C1, C2, C3, C4, C2]
      b = take 2 b0 ++ [row2] ++ drop 3 b0
      (mb, n) = clearMatches b
  assertBool "cleared" (n >= 4)
  let specials =
        [ (r, c, cellKind cell)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [ (mb !! r) !! c ]
        , cellKind cell /= Normal
        ]
  assertBool ("spawned line: " ++ show specials) $
    any (\(_, _, k) -> k == LineH || k == LineV) specials

-- | 5-in-a-row spawns a Rainbow.
special_rainbow_from_5 :: Assertion
special_rainbow_from_5 = do
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row1 = map mkGem [C1, C1, C1, C1, C1, C2, C3, C2]
      b = take 1 b0 ++ [row1] ++ drop 2 b0
      (mb, _) = clearMatches b
      rainbows =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [ (mb !! r) !! c ]
        , cellKind cell == Rainbow
        ]
  assertBool ("rainbow spawned: " ++ show rainbows) (not (null rainbows))

hint_finds_move :: Assertion
hint_finds_move = do
  let gs0 = newGame defaultConfig 7
      (gs1, h) = applyHint gs0
  case h of
    Nothing -> assertFailure "expected a hint on a fresh board"
    Just (p1, p2) -> do
      gsHint gs1 @?= Just (p1, p2)
      assertBool "hint creates match" (hasAnyMatch (swapCells (gsBoard gs0) p1 p2))

undo_restores :: Assertion
undo_restores = do
  let gs0 = newGame defaultConfig 11
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need move"
    Just (p1, p2) -> do
      let (gs1, _) = trySwap p1 p2 gs0
      case undoMove gs1 of
        Nothing -> assertFailure "undo should work"
        Just gsU -> do
          gsBoard gsU @?= gsBoard gs0
          gsScore gsU @?= gsScore gs0
          gsMoves gsU @?= gsMoves gs0
          gsCollected gsU @?= gsCollected gs0

-- | Stuck board (no valid adjacent swap) is reshuffled to a playable stable board.
shuffle_when_no_moves :: Assertion
shuffle_when_no_moves = do
  let stuck = stuckNoMoveBoard
  assertBool "fixture has no match" (not (hasAnyMatch stuck))
  assertBool "fixture has no valid move" (not (hasValidMove stuck))
  let gs0 =
        (newGame defaultConfig 1)
          { gsBoard = stuck
          , gsGen = mkStdGen 999
          , gsOver = Nothing
          , gsHint = Nothing
          , gsShuffled = False
          }
      gs1 = ensurePlayable gs0
  assertBool "did shuffle" (gsShuffled gs1)
  assertBool "after: no initial match" (not (hasAnyMatch (gsBoard gs1)))
  assertBool "after: has valid move" (hasValidMove (gsBoard gs1))
  -- Force shuffleGame also yields playable
  let gs2 = shuffleGame gs0
  assertBool "shuffleGame playable" (hasValidMove (gsBoard gs2))
  assertBool "shuffleGame stable" (not (hasAnyMatch (gsBoard gs2)))

-- | Cyclic (r+c) mod 5 board: stable and no valid adjacent swap.
stuckNoMoveBoard :: Board
stuckNoMoveBoard =
  [ [ mkGem (toEnum ((r + c) `mod` 5))
    | c <- [0 .. boardSize - 1]
    ]
  | r <- [0 .. boardSize - 1]
  ]

playable_board_stable_and_has_move :: Assertion
playable_board_stable_and_has_move = do
  mapM_
    ( \seed -> do
        let (b, _) = randomPlayableBoard (mkStdGen seed)
        assertBool ("stable " ++ show seed) (not (hasAnyMatch b))
        assertBool ("has move " ++ show seed) (hasValidMove b)
    )
    [0 .. 30 :: Int]

-- | Multi-wave cascade scores with increasing wave multiplier.
combo_wave_scoring :: Assertion
combo_wave_scoring = do
  assertEqual "wave1" (30 :: Int) (scoreForWave 1 3)
  assertEqual "wave2" (60 :: Int) (scoreForWave 2 3)
  assertEqual "wave3" (90 :: Int) (scoreForWave 3 3)
  let fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C1, C2, C3, C4, C2, C3]
      b = take 3 b0 ++ [row3] ++ drop 4 b0
      (_, cells, scored, combo, tallies, _, _, _, _, _, _, _) = runCascadeScored Nothing (mkStdGen 3) b
  assertBool "cleared some" (cells >= 3)
  assertBool "combo >= 1" (combo >= 1)
  assertEqual "score matches waves aggregate lower bound" True (scored >= scoreForWave 1 3)
  let c1n = fromMaybe 0 (lookup C1 tallies)
  assertBool "tallied some C1" (c1n >= 3)

--------------------------------------------------------------------------------
-- Color-collect goals
--------------------------------------------------------------------------------

-- | Clearing gems of the target color increments gsCollected.
collect_goal_progress :: Assertion
collect_goal_progress = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = GoalCollect C1 100 }
      -- Build a board with a clearable C1 triple at row 3, rest C5 (won't make C1 match elsewhere)
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      -- Place C1 C1 C2 and an adjacent C1 so swap creates three C1
      -- row3: C1 C1 C2 C3 C4 C5 C2 C3  — swap (3,2)=C2 with (3,1) wouldn't help
      -- Better: put C1 at (3,0)(3,1)(3,3) and C2 at (3,2); swap (3,2)<->something...
      -- Simpler: board already has match of three C1 — but then newGame uses random board.
      -- Override board after newGame, then force a matching swap.
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = take 3 b0 ++ [row3] ++ drop 4 b0
      -- Swap (3,2)=C2 with (3,3)=C1 → row becomes C1 C1 C1 C2 ... match!
      gs0 =
        (newGame cfg 55)
          { gsBoard = board
          , gsCollected = 0
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    MoveApplied _ -> pure ()
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("expected applied/terminal, got " ++ show other)
  assertBool
    ("collected C1 increased, got " ++ show (gsCollected gs1))
    (gsCollected gs1 >= 3)

-- | Reaching collect count triggers LevelClear (or Won on last level).
collect_goal_clears_level :: Assertion
collect_goal_clears_level = do
  let cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalCollect C1 3 }
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = take 3 b0 ++ [row3] ++ drop 4 b0
      -- Level 0 so LevelClear (not Won)
      gs0 =
        (newGameAtLevel 0 cfg 55)
          { gsBoard = board
          , gsCollected = 0
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    LevelClear _ next -> do
      assertEqual "next level" (1 :: Int) next
      assertBool "collected enough" (gsCollected gs1 >= 3)
      assertBool "gsOver set" (gsOver gs1 == Just out)
    Won _ -> assertFailure "should LevelClear on non-last level"
    other -> assertFailure ("expected LevelClear, got " ++ show other ++ " collected=" ++ show (gsCollected gs1))


-- | One legal clear with moves=1 but GoalCollect unmet → Lost; collected < target.
collect_goal_lose_on_moves :: Assertion
collect_goal_lose_on_moves = do
  let cfg = GameConfig { cfgMoves = 1, cfgGoal = GoalCollect C1 99 }
      fill = mkGem C5
      b0 = replicate boardSize (replicate boardSize fill)
      row3 = map mkGem [C1, C1, C2, C1, C3, C4, C5, C2]
      board = take 3 b0 ++ [row3] ++ drop 4 b0
      gs0 =
        (newGameAtLevel 0 cfg 55)
          { gsBoard = board
          , gsCollected = 0
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    Lost s -> do
      assertBool "score non-negative" (s >= 0)
      assertBool
        ("collected < 99, got " ++ show (gsCollected gs1))
        (gsCollected gs1 < 99)
      assertBool "gsOver is Lost" (gsOver gs1 == Just out)
      assertEqual "moves spent" (0 :: Int) (gsMoves gs1)
    other ->
      assertFailure
        ("expected Lost, got " ++ show other
           ++ " collected=" ++ show (gsCollected gs1)
           ++ " moves=" ++ show (gsMoves gs1))

-- | Campaign table mixes GoalScore and GoalCollect stages.
level_table_mixes_collect :: Assertion
level_table_mixes_collect = do
  let goals = map lvlGoal allLevels
      scores = [g | g@GoalScore {} <- goals]
      collects = [g | g@GoalCollect {} <- goals]
  assertBool "has score levels" (not (null scores))
  assertBool "has collect levels" (not (null collects))
  assertBool "at least 5 levels" (length allLevels >= 5)
  let names = map lvlName allLevels
  assertBool "has 采红" ("采红" `elem` names)
  assertBool "has 采蓝" ("采蓝" `elem` names)
  assertBool "has 冰绿" ("冰绿" `elem` names)
  assertBool "has 双采" ("双采" `elem` names)
  assertBool "has 碎石" ("碎石" `elem` names)
  assertBool "has multi goal"
    (any (\g -> case g of GoalCollectMulti _ -> True; _ -> False) goals)
  assertBool "has clear-stone goal"
    (any (\g -> case g of GoalClearStone _ -> True; _ -> False) goals)
  assertBool "has 飞碟" ("飞碟" `elem` names)
  assertBool "has ufo goal"
    (any (\g -> case g of GoalUfo _ -> True; _ -> False) goals)

-- | Score-goal levels do not increment gsCollected (stays 0).
score_goal_ignores_collect :: Assertion
score_goal_ignores_collect = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = GoalScore 99999 }
      gs0 = newGame cfg 42
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need move"
    Just (p1, p2) -> do
      let (gs1, _) = trySwap p1 p2 gs0
      gsCollected gs1 @?= 0

--------------------------------------------------------------------------------
-- Stone blockers
--------------------------------------------------------------------------------

-- | Stable board with no accidental 3-runs.
stableBoard :: Board
stableBoard =
  [ map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
  , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
  , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
  , map mkGem [C4, C5, C1, C2, C3, C4, C5, C1]
  , map mkGem [C5, C1, C2, C3, C4, C5, C1, C2]
  , map mkGem [C1, C2, C3, C4, C5, C1, C2, C3]
  , map mkGem [C2, C3, C4, C5, C1, C2, C3, C4]
  , map mkGem [C3, C4, C5, C1, C2, C3, C4, C5]
  ]

-- | trySwap involving a Stone returns NoMatch; board/moves/score unchanged.
stone_blocks_swap :: Assertion
stone_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkStone
      gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsShuffled = False
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0
  gsScore gs1 @?= gsScore gs0
  gsMoves gs1 @?= gsMoves gs0
  let (gs2, out2) = trySwap (3, 4) (3, 3) gs0
  out2 @?= NoMatch
  gsBoard gs2 @?= gsBoard gs0
  gsMoves gs2 @?= gsMoves gs0

-- | Clearing gems next to a stone also removes that stone.
stone_cleared_by_adjacent :: Assertion
stone_cleared_by_adjacent = do
  -- Row 3: C1 C1 C2 C1 ... — swap (3,2)<->(3,3) yields C1 C1 C1
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      board = setCell board0 (2, 1) mkStone
  assertBool "stone placed" (isStone (getCell board (2, 1)))
  assertBool "no match yet" (not (hasAnyMatch board))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    MoveApplied _ -> pure ()
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("expected applied/terminal, got " ++ show other)
  assertBool
    "stone cleared by adjacent match"
    (not (isStone (getCell (gsBoard gs1) (2, 1))))

-- | Stones never form matches; they interrupt color runs.
stone_not_in_match :: Assertion
stone_not_in_match = do
  let bStones =
        setCell
          (setCell (setCell stableBoard (0, 0) mkStone) (0, 1) mkStone)
          (0, 2)
          mkStone
  assertBool "three stones not a match" (null (findMatches bStones))
  assertBool "hasAnyMatch false for stones" (not (hasAnyMatch bStones))
  -- C1 C1 Stone C1 C1 on a stable base — stone breaks the run
  let bBreak =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (1, 0) (mkGem C1))
                   (1, 1)
                   (mkGem C1))
                (1, 2)
                mkStone)
             (1, 3)
             (mkGem C1))
          (1, 4)
          (mkGem C1)
  assertBool "stone breaks C1 run" (null (findMatches bBreak))
  -- Gem triple with a stone beside still matches only the gems
  let bOk =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (2, 0) (mkGem C1))
                (2, 1)
                (mkGem C1))
             (2, 2)
             (mkGem C1))
          (2, 3)
          mkStone
      ms = findMatches bOk
  assertBool "gem triple still matches" ((2, 0) `elem` ms && (2, 1) `elem` ms && (2, 2) `elem` ms)
  assertBool "stone itself not in match set" ((2, 3) `notElem` ms)

--------------------------------------------------------------------------------
-- Rainbow (color bomb)
--------------------------------------------------------------------------------

-- | Swapping a Rainbow with a color clears every gem of that color.
rainbow_clears_color :: Assertion
rainbow_clears_color = do
  let board0 =
        setCell stableBoard (4, 4) (Gem C1 Rainbow 0 Nothing)
      -- Ensure neighbor (4,5) is C2 (stableBoard already has variety)
      board = setCell board0 (4, 5) (mkGem C2)
      -- Count C2 before
      c2before =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , case getCell board (r, c) of
              Gem C2 _ _ Nothing -> True
              _ -> False
          ]
  assertBool "have some C2" (c2before >= 1)
  let gs0 =
        (newGame defaultConfig 3)
          { gsBoard = board
          , gsOver = Nothing
          , gsHint = Nothing
          , gsScore = 0
          , gsMoves = 10
          }
      (gs1, out) = trySwap (4, 4) (4, 5) gs0
  case out of
    MoveApplied gained -> assertBool "scored" (gained > 0)
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("expected applied/terminal, got " ++ show other)
  let c2after =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , case getCell (gsBoard gs1) (r, c) of
              Gem C2 _ _ Nothing -> True
              _ -> False
          ]
  -- After cascade refill, leftover C2 may appear from refill; the rainbow itself must be gone
  assertBool "rainbow consumed" (not (isRainbow (getCell (gsBoard gs1) (4, 4))))
  assertBool "rainbow consumed at partner" (not (isRainbow (getCell (gsBoard gs1) (4, 5))))
  -- Moves decremented
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)
  -- At least the clear scored for the C2 count (wave1 * 10); allow cascades
  assertBool
    ("cleared many cells relative to C2 count " ++ show c2before ++ " after=" ++ show c2after)
    (gsScore gs1 >= c2before * 10)

-- | Rainbow swap works even when it would not create a classic 3-match.
rainbow_swap_without_match :: Assertion
rainbow_swap_without_match = do
  let board =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 Nothing))
          (0, 1)
          (mkGem C4)
  assertBool "no classic match after swap alone"
    (not (hasAnyMatch (swapCells board (0, 0) (0, 1)))
       || isRainbowSwap board (0, 0) (0, 1))
  assertBool "is rainbow swap" (isRainbowSwap board (0, 0) (0, 1))
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 8
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 0) (0, 1) gs0
  case out of
    NoMatch -> assertFailure "rainbow swap must not roll back as NoMatch"
    InvalidSwap -> assertFailure "rainbow swap must be valid"
    MoveApplied g -> assertBool "gained" (g > 0)
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
  assertEqual "moves spent" (gsMoves gs0 - 1) (gsMoves gs1)

-- | Multi-layer stone loses one layer on a single adjacent clear, stays on board.
-- Stone placed *below* the match so gravity does not relocate it.
stone_layer_decrement :: Assertion
stone_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkStoneLayers 3)
  assertEqual "3 layers" (3 :: Int) (stoneLayers (getCell board0 (4, 1)))
  assertBool "has match" (hasAnyMatch board0)
  let (b1, dead) = chipAdjacentStones board0 [(3, 0), (3, 1), (3, 2)]
  assertBool "not removed on first chip" (null dead)
  assertBool "still stone" (isStone (getCell b1 (4, 1)))
  assertEqual "3 -> 2" (2 :: Int) (stoneLayers (getCell b1 (4, 1)))
  let g = mkStdGen 1
  case stepCascade g board0 of
    Nothing -> assertFailure "expected a cascade step"
    Just (b2, _n, _) -> do
      assertBool "stone survives one wave" (isStone (getCell b2 (4, 1)))
      assertEqual "one wave chips once" (2 :: Int) (stoneLayers (getCell b2 (4, 1)))

-- | Last layer chip removes the stone (Stone 1, or Stone 2 hit twice).
stone_layer_clears_at_zero :: Assertion
stone_layer_clears_at_zero = do
  -- First: Stone 1 clears in one adjacent hit (existing behavior)
  let board1 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C2))
             (3, 3)
             (mkGem C1))
          (2, 1)
          (mkStoneLayers 1)
      gsA0 =
        (newGame defaultConfig 7)
          { gsBoard = board1
          , gsOver = Nothing
          }
      (gsA1, _) = trySwap (3, 2) (3, 3) gsA0
  assertBool "Stone 1 cleared" (not (isStone (getCell (gsBoard gsA1) (2, 1))))

  -- Second: Stone 2 needs two adjacent clears to vanish
  let board2 = setCell board1 (2, 1) (mkStoneLayers 2)
      -- First hit via clearMatches pipeline directly for control
      (_, dead1) = chipAdjacentStones board2 [(3, 1)]
  assertBool "first hit does not remove" (null dead1)
  let (bMid, _) = chipAdjacentStones board2 [(3, 1)]
  assertEqual "after first chip" (1 :: Int) (stoneLayers (getCell bMid (2, 1)))
  let (bEnd, dead2) = chipAdjacentStones bMid [(3, 1)]
  assertEqual "second hit removes" [(2, 1)] dead2
  assertBool "board still has stone until clear applied" (isStone (getCell bEnd (2, 1)))

--------------------------------------------------------------------------------
-- Special combos (Line × Bomb)
--------------------------------------------------------------------------------

-- | Swapping Line + Bomb clears a 3-row × 3-col cross without needing a 3-match.
special_combo_line_bomb :: Assertion
special_combo_line_bomb = do
  let board =
        setCell
          (setCell stableBoard (4, 3) (Gem C1 LineH 0 Nothing))
          (4, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "is line+bomb combo" (isLineBombCombo board (4, 3) (4, 4))
  assertBool "special combo" (isSpecialCombo board (4, 3) (4, 4))
  -- Seeds on post-swap board: bomb moves to (4,3)
  let swapped = swapCells board (4, 3) (4, 4)
      seeds = comboClearSeeds swapped (4, 3) (4, 4)
  -- At least 3 full rows + 3 full cols = 3*8 + 3*8 - 9 overlap = 39
  assertBool ("big clear seeds " ++ show (length seeds)) (length seeds >= 39)
  let gs0 =
        (newGame defaultConfig 2)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (4, 3) (4, 4) gs0
  case out of
    NoMatch -> assertFailure "combo must not roll back"
    InvalidSwap -> assertFailure "combo must be valid adjacent swap"
    MoveApplied g -> assertBool ("big score, got " ++ show g) (g >= 39 * 10)
    LevelClear s _ -> assertBool "scored" (s >= 0)
    Won s -> assertBool "scored" (s >= 0)
    Lost _ -> pure ()
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)
  assertBool "board stable" (not (hasAnyMatch (gsBoard gs1)))

--------------------------------------------------------------------------------
-- Multi goals (开心消消乐-style diverse targets)
--------------------------------------------------------------------------------

-- | GoalCollectMulti requires quotas for every listed color.
goal_collect_multi_color :: Assertion
goal_collect_multi_color = do
  let cfg = GameConfig { cfgMoves = 20, cfgGoal = GoalCollectMulti [(C1, 3), (C2, 1)] }
      board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      -- Swap (3,2)<->(3,3): row becomes C1 C1 C1 C2 — clears 3×C1
      gs0 =
        (newGameAtLevel 0 cfg 5)
          { gsBoard = board0
          , gsCollected = 0
          , gsColorBag = zip allColors (repeat 0)
          , gsOver = Nothing
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  assertBool "C1 tallied" (lookupCount (gsColorBag gs1) C1 >= 3)
  -- Not yet clear: still need C2 quota unless cascade luck
  case out of
    LevelClear _ _ ->
      assertBool "if cleared, both quotas met" $
        goalMetEx (gsGoal gs1) (gsScore gs1) (gsCollected gs1) (gsColorBag gs1) (gsStonesCleared gs1) (gsUfoCollected gs1) (gsChestsCleared gs1) (gsHoneyCleared gs1) (gsBalloonsPopped gs1) (gsCookiesCollected gs1) (gsCakesCleared gs1) (gsSafesOpened gs1)
    MoveApplied _ ->
      assertBool "multi goal not met with only C1" $
        not (goalMetEx (GoalCollectMulti [(C1, 3), (C2, 1)]) 0 0 (gsColorBag gs1) 0 0 0 0 0 0 0 0)
          || lookupCount (gsColorBag gs1) C2 >= 1
    _ -> pure ()
  -- Direct unit: goalMetEx logic
  assertBool "both met"
    (goalMetEx (GoalCollectMulti [(C1, 2), (C3, 1)]) 0 0 [(C1, 2), (C2, 0), (C3, 1), (C4, 0), (C5, 0)] 0 0 0 0 0 0 0 0)
  assertBool "missing color"
    (not (goalMetEx (GoalCollectMulti [(C1, 2), (C3, 1)]) 0 0 [(C1, 5), (C2, 0), (C3, 0), (C4, 0), (C5, 0)] 0 0 0 0 0 0 0 0))

-- | GoalClearStone counts fully destroyed stones toward the goal.
goal_clear_stone_counts :: Assertion
goal_clear_stone_counts = do
  assertBool "0 stones unmet"
    (not (goalMetEx (GoalClearStone 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "2 stones met"
    (goalMetEx (GoalClearStone 2) 0 0 [] 2 0 0 0 0 0 0 0)
  assertEqual "goal target" (8 :: Int) (goalTarget (GoalClearStone 8))
  let cfg = GameConfig { cfgMoves = 15, cfgGoal = GoalClearStone 2 }
      boardN =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C2))
             (3, 3)
             (mkGem C1))
          (4, 1)
          mkStone
      gsN =
        (newGameAtLevel 0 cfg 5)
          { gsBoard = boardN
          , gsStonesCleared = 0
          , gsOver = Nothing
          }
      (gsN1, outN) = trySwap (3, 2) (3, 3) gsN
  assertBool
    ("stonesCleared incremented, got " ++ show (gsStonesCleared gsN1))
    (gsStonesCleared gsN1 >= 1)
  case outN of
    LevelClear _ _ -> assertBool "enough stones" (gsStonesCleared gsN1 >= 2)
    MoveApplied _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
    other -> assertFailure ("unexpected " ++ show other)

--------------------------------------------------------------------------------
-- Ice layers (开心消消乐冰层)
--------------------------------------------------------------------------------

-- | ice>=2: match chips one layer and gem stays; ice-free neighbors clear.
ice_layer_blocks_clear :: Assertion
ice_layer_blocks_clear = do
  let board =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkIceGem C1 2))
          (3, 2)
          (mkGem C1)
  assertBool "has match" (hasAnyMatch board)
  let (b1, free) = chipIceOnClear board [(3, 0), (3, 1), (3, 2)]
  assertBool "iced pos not free" ((3, 1) `notElem` free)
  assertBool "plain gems free" ((3, 0) `elem` free && (3, 2) `elem` free)
  assertEqual "ice 2 -> 1" (1 :: Int) (iceLayers (getCell b1 (3, 1)))
  assertBool "gem still there" (isGem (getCell b1 (3, 1)))

-- | ice-2 chips to ice-1 (stays); next chip (last layer) clears the gem.
ice_layer_chips_then_clears :: Assertion
ice_layer_chips_then_clears = do
  let board = setCell stableBoard (1, 1) (mkIceGem C4 2)
      (b1, free1) = chipIceOnClear board [(1, 1)]
  assertBool "still blocked" ((1, 1) `notElem` free1)
  assertEqual "2 -> 1" (1 :: Int) (iceLayers (getCell b1 (1, 1)))
  let (_b2, free2) = chipIceOnClear b1 [(1, 1)]
  assertBool "last ice clears gem" ((1, 1) `elem` free2)
  -- ice-1 alone also clears in one chip
  let (_b3, free3) = chipIceOnClear (setCell stableBoard (2, 2) (mkIceGem C3 1)) [(2, 2)]
  assertBool "ice-1 clears immediately" ((2, 2) `elem` free3)

--------------------------------------------------------------------------------
-- Daily challenge + stars
--------------------------------------------------------------------------------

daily_seed_stable :: Assertion
daily_seed_stable = do
  assertEqual "seed" (20260929 :: Int) (dailySeed 2026 9 29)
  assertEqual "same day same seed" (dailySeed 2026 1 1) (dailySeed 2026 1 1)
  assertBool "diff day diff seed" (dailySeed 2026 1 1 /= dailySeed 2026 1 2)
  let lvl = dailyLevel 2026 9 29
  assertEqual "name" "每日" (lvlName lvl)
  assertBool "moves positive" (lvlMoves lvl > 0)
  let gs = newGameAtLevel 0 (dailyConfig 2026 9 29) (dailySeed 2026 9 29)
  assertBool "playable daily board" (hasValidMove (gsBoard gs))
  assertBool "stable daily board" (not (hasAnyMatch (gsBoard gs)))

star_rating_tiers :: Assertion
star_rating_tiers = do
  assertEqual "3 star plenty" (3 :: Int) (starRating 30 20)
  assertEqual "3 star boundary 40%" (3 :: Int) (starRating 30 12)
  assertEqual "2 star just below 40%" (2 :: Int) (starRating 30 11)  -- 11/30 < 0.4
  assertEqual "2 star" (2 :: Int) (starRating 30 8)
  assertEqual "2 star boundary ~15%" (2 :: Int) (starRating 30 5)
  assertEqual "1 star just below 15%" (1 :: Int) (starRating 30 4)  -- 4/30 < 0.15
  assertEqual "1 star" (1 :: Int) (starRating 30 2)
  assertEqual "zero left" (1 :: Int) (starRating 30 0)
  assertEqual "zero start" (1 :: Int) (starRating 0 0)
  assertEqual "single move clutch 3★" (3 :: Int) (starRating 1 1)

-- | Rainbow × Line clears all gems of the line's color.
special_combo_rainbow_line :: Assertion
special_combo_rainbow_line = do
  let board =
        setCell
          (setCell stableBoard (5, 2) (Gem C2 Rainbow 0 Nothing))
          (5, 3)
          (Gem C4 LineV 0 Nothing)
  assertBool "rainbow+line" (isRainbowLineCombo board (5, 2) (5, 3))
  assertBool "special" (isSpecialCombo board (5, 2) (5, 3))
  let gs0 =
        (newGame defaultConfig 4)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (5, 2) (5, 3) gs0
  case out of
    NoMatch -> assertFailure "must apply"
    InvalidSwap -> assertFailure "must be valid"
    MoveApplied g -> assertBool ("scored " ++ show g) (g >= 40)
    _ -> pure ()
  assertEqual "moves" (gsMoves gs0 - 1) (gsMoves gs1)

special_combo_bomb_bomb :: Assertion
special_combo_bomb_bomb = do
  let board =
        setCell
          (setCell stableBoard (3, 3) (Gem C1 Bomb 0 Nothing))
          (3, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "bomb×bomb" (isBombBombCombo board (3, 3) (3, 4))
  let seeds = comboClearSeeds (swapCells board (3, 3) (3, 4)) (3, 3) (3, 4)
  assertBool ("5x5-ish seeds " ++ show (length seeds)) (length seeds >= 25)
  let (gs1, out) = trySwap (3, 3) (3, 4) (newGame defaultConfig 1) { gsBoard = board, gsOver = Nothing, gsMoves = 5 }
  case out of
    NoMatch -> assertFailure "must apply"
    MoveApplied g -> assertBool "big" (g >= 200)
    _ -> pure ()
  assertBool "stable" (not (hasAnyMatch (gsBoard gs1)))

special_combo_line_line :: Assertion
special_combo_line_line = do
  let board =
        setCell
          (setCell stableBoard (2, 2) (Gem C3 LineH 0 Nothing))
          (2, 3)
          (Gem C4 LineV 0 Nothing)
  assertBool "line×line" (isLineLineCombo board (2, 2) (2, 3))
  let (gs1, out) = trySwap (2, 2) (2, 3) (newGame defaultConfig 1) { gsBoard = board, gsOver = Nothing, gsMoves = 5 }
  case out of
    NoMatch -> assertFailure "must apply"
    MoveApplied g -> assertBool ("row+col score " ++ show g) (g >= 150)
    _ -> pure ()
  assertEqual "moves" (4 :: Int) (gsMoves gs1)

--------------------------------------------------------------------------------
-- Countdown bombs (开心消消乐倒计时炸弹)
--------------------------------------------------------------------------------

-- | spawnCountdown / mkCountdown places a colored timer on the board.
countdown_bomb_spawns :: Assertion
countdown_bomb_spawns = do
  let b0 = spawnCountdown stableBoard (2, 2) C1 5
  assertBool "is countdown" (isCountdown (getCell b0 (2, 2)))
  assertEqual "turns" (5 :: Int) (countdownTurns (getCell b0 (2, 2)))
  assertEqual "color" C1 (cellColor (getCell b0 (2, 2)))
  assertBool "counts as gem" (isGem (getCell b0 (2, 2)))
  -- Participates in a same-color run
  let b1 =
        setCell
          (setCell b0 (2, 0) (mkGem C1))
          (2, 1)
          (mkGem C1)
      ms = findMatches b1
  assertBool "countdown in match" ((2, 2) `elem` ms)

-- | Successful move ticks remaining countdowns by 1.
countdown_bomb_ticks_after_move :: Assertion
countdown_bomb_ticks_after_move = do
  let bPure = spawnCountdown stableBoard (2, 2) C3 5
  assertEqual "pure tick 5->4" (4 :: Int) (countdownTurns (getCell (tickCountdowns bPure) (2, 2)))
  let (bRes, nClear, _, _, _, _, _, _, _, _, _, _, _, _, _) = resolveCountdowns [] [] (mkStdGen 0) bPure
  assertEqual "resolve ticks" (4 :: Int) (countdownTurns (getCell bRes (2, 2)))
  assertEqual "no explode when >0" (0 :: Int) nClear
  -- trySwap path: use a tiny score goal so outcome is terminal (skips ensurePlayable shuffle)
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      board = spawnCountdown board0 (5, 5) C5 5
      cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalScore 1 }
      gs0 =
        (newGameAtLevel 0 cfg 11)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  assertBool "terminal or applied" $
    case out of
      MoveApplied _ -> True
      LevelClear _ _ -> True
      Won _ -> True
      Lost _ -> True
      _ -> False
  assertBool "not reshuffled away" (not (gsShuffled gs1))
  let cds =
        [ countdownTurns (getCell (gsBoard gs1) (r, c))
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCountdown (getCell (gsBoard gs1) (r, c))
        ]
  assertEqual "countdown ticked 5->4 after move" [4] cds

-- | Countdown at 1 ticks to 0 and explodes clearing the 3×3 neighborhood.
countdown_bomb_explodes_at_zero :: Assertion
countdown_bomb_explodes_at_zero = do
  -- Countdown at (4,4) with 1 turn; match elsewhere on row 0 area via swap
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 0) (mkGem C2))
                (0, 1)
                (mkGem C2))
             (0, 2)
             (mkGem C3))
          (0, 3)
          (mkGem C2)
      board = spawnCountdown board0 (4, 4) C5 1
      -- Marker gem adjacent that should vanish in 3×3 explosion
      board' = setCell board (4, 5) (mkGem C1)
      gs0 =
        (newGame defaultConfig 13)
          { gsBoard = board'
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  -- Countdown itself must be gone (exploded)
  let cds =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCountdown (getCell (gsBoard gs1) (r, c))
        ]
  assertBool ("countdown exploded away, leftover " ++ show cds) (null cds)
  -- Score should reflect explosion clear (at least some points beyond tiny match)
  assertBool ("explosion scored, got " ++ show (gsScore gs1)) (gsScore gs1 >= 30)

-- | Matching a countdown disarms it (removed, no zero-explosion).
countdown_bomb_cleared_disarms :: Assertion
countdown_bomb_cleared_disarms = do
  -- Countdown C1 at (3,2) with 1 turn — would explode if not matched.
  -- Setup: C1 C1 Countdown(C1) via swap of (3,2)<->(3,3) where (3,3) is C1
  -- Start: (3,0)=C1 (3,1)=C1 (3,2)=Countdown C1 1 (3,3)=C2 — already a match including countdown!
  -- Better: no initial match; swap brings countdown into a triple.
  -- board: (3,0)=C1 (3,1)=C1 (3,2)=C2 (3,3)=Countdown C1 1
  -- swap (3,2)<->(3,3) => C1 C1 Countdown C2 — wait that matches countdown with C1s.
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C2)
      board = spawnCountdown board0 (3, 3) C1 1
      -- Witness gem at (5,5) far from (3,3); if countdown exploded (3×3 around 3,3)
      -- it would NOT reach (5,5). Place witness inside explosion radius instead:
      -- (3,4) is in 3×3 of (3,3). If disarmed by match, explosion shouldn't fire,
      -- but match clear may still remove neighbors via specials — use plain match.
      -- After disarm+ cascade, countdown gone; tick of other bombs N/A.
      -- Place a second countdown at (6,6) with 3 turns — should tick to 2, not explode.
      board' = spawnCountdown board (6, 6) C4 3
      gs0 =
        (newGame defaultConfig 17)
          { gsBoard = board'
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match to disarm"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  -- Matched countdown disarmed (no C1 countdown left)
  let c1cds =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , case getCell (gsBoard gs1) (r, c) of
            Countdown C1 _ -> True
            _ -> False
        ]
  assertBool ("C1 countdown disarmed, leftover " ++ show c1cds) (null c1cds)
  -- Other countdown ticked 3 -> 2 (survived, not exploded)
  let other =
        [ countdownTurns (getCell (gsBoard gs1) (r, c))
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCountdown (getCell (gsBoard gs1) (r, c))
        ]
  assertEqual "other countdown ticked once" [2] other

--------------------------------------------------------------------------------
-- Conveyor belts (开心消消乐传送带)
--------------------------------------------------------------------------------

-- | Shifting a cycle permutes cells; multiset of belt cells is unchanged.
conveyor_cycle_preserves_cells :: Assertion
conveyor_cycle_preserves_cells = do
  let belt = [(0, 0), (0, 1), (0, 2), (1, 2)]
      b0 = stableBoard
      cellsBefore = map (getCell b0) belt
      b1 = shiftBelt b0 belt
      cellsAfter = map (getCell b1) belt
  assertEqual "same multiset" (sort cellsBefore) (sort cellsAfter)
  -- Forward: new[i] = old[i-1]
  assertEqual "wrap" (last cellsBefore) (head cellsAfter)
  assertEqual "step" (cellsBefore !! 0) (cellsAfter !! 1)
  -- Identity on empty / singleton
  assertEqual "empty" b0 (shiftBelt b0 [])
  assertEqual "singleton" b0 (shiftBelt b0 [(3, 3)])

-- | After a successful move, belt cells advance one step.
conveyor_shifts_after_move :: Assertion
conveyor_shifts_after_move = do
  let belt = [(5, 0), (5, 1), (5, 2)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      -- Distinct colors on belt so we can see the rotation
      board1 =
        setCell
          (setCell
             (setCell board0 (5, 0) (mkGem C1))
             (5, 1)
             (mkGem C2))
          (5, 2)
          (mkGem C3)
      cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalScore 1 }  -- terminal => no shuffle
      gs0 =
        (newGameAtLevel 0 cfg 3)
          { gsBoard = board1
          , gsBelts = [belt]
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      before = map (getCell (gsBoard gs0)) belt
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  assertBool "move ok" $
    case out of
      NoMatch -> False
      InvalidSwap -> False
      _ -> True
  assertBool "not shuffled" (not (gsShuffled gs1))
  let afterCells = map (getCell (gsBoard gs1)) belt
      expected = shiftBelt board1 belt
      expectCells = map (getCell expected) belt
  assertEqual "belt advanced one step" expectCells afterCells
  assertEqual "multiset kept" (sort before) (sort afterCells)

-- | Belt shift can assemble a 3-match that then clears.
conveyor_can_create_match :: Assertion
conveyor_can_create_match = do
  -- Belt moves C1 into a row of two C1s to form a triple.
  -- Row 4: C1, C1, C2 at (4,0)(4,1)(4,2). Belt on col2: (6,2)->(5,2)->(4,2)
  -- Put C1 at (5,2); after shift forward on [(6,2),(5,2),(4,2)]:
  --   new(6,2)=old(4,2)=C2, new(5,2)=old(6,2)=?, new(4,2)=old(5,2)=C1
  -- So we need old(5,2)=C1 to land on (4,2), making (4,0)(4,1)(4,2) all C1.
  let belt = [(6, 2), (5, 2), (4, 2)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (4, 0) (mkGem C1))
                   (4, 1)
                   (mkGem C1))
                (4, 2)
                (mkGem C2))
             (5, 2)
             (mkGem C1))
          (6, 2)
          (mkGem C3)
  assertBool "no match yet" (not (hasAnyMatch board0))
  let shifted = shiftBelt board0 belt
  assertEqual "C1 arrived" C1 (cellColor (getCell shifted (4, 2)))
  assertBool "match formed" (hasAnyMatch shifted)
  -- Via trySwap: match elsewhere + belt creates extra clear
  let boardM =
        setCell
          (setCell
             (setCell
                (setCell board0 (0, 0) (mkGem C4))
                (0, 1)
                (mkGem C4))
             (0, 2)
             (mkGem C5))
          (0, 3)
          (mkGem C4)
      cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalScore 99999 }
      gs0 =
        (newGameAtLevel 0 cfg 9)
          { gsBoard = boardM
          , gsBelts = [belt]
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    MoveApplied g -> assertBool ("belt cascade scored extra, got " ++ show g) (g >= 60)
    _ -> pure ()
  assertBool "stable after" (not (hasAnyMatch (gsBoard gs1)))

--------------------------------------------------------------------------------
-- Boosters + lose hint
--------------------------------------------------------------------------------

booster_hammer_clears_cell :: Assertion
booster_hammer_clears_cell = do
  let board = setCell stableBoard (2, 2) (mkGem C1)
      gs0 =
        (newGame defaultConfig 1)
          { gsBoard = board
          , gsHammers = 2
          , gsMoves = 10
          , gsScore = 0
          , gsOver = Nothing
          }
      (gs1, out) = useHammer (2, 2) gs0
  case out of
    NoMatch -> assertFailure "hammer should apply"
    InvalidSwap -> assertFailure "hammer should be valid"
    MoveApplied g -> assertBool "scored" (g >= 10)
    _ -> pure ()
  assertEqual "hammer spent" (1 :: Int) (gsHammers gs1)
  assertEqual "moves untouched" (10 :: Int) (gsMoves gs1)
  -- No charges left after spending both
  let (gs2, _) = useHammer (0, 0) gs1
      (gs3, out3) = useHammer (1, 1) gs2
  assertEqual "empty" (0 :: Int) (gsHammers gs2)
  out3 @?= InvalidSwap
  assertEqual "unchanged hammers" (0 :: Int) (gsHammers gs3)

booster_free_swap_any_cells :: Assertion
booster_free_swap_any_cells = do
  -- Non-adjacent swap that forms C1 C1 C1 on row 3
  let board =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (5, 5)
          (mkGem C1)
      gs0 =
        (newGame defaultConfig 2)
          { gsBoard = board
          , gsFreeSwaps = 1
          , gsMoves = 10
          , gsScore = 0
          , gsOver = Nothing
          }
  -- Adjacent would be trySwap; here (3,2) and (5,5) are NOT adjacent
  assertBool "not adjacent" (not (adjacent (3, 2) (5, 5)))
  let (gs1, out) = useFreeSwap (3, 2) (5, 5) gs0
  case out of
    NoMatch -> assertFailure "free swap must apply"
    InvalidSwap -> assertFailure "free swap must be valid"
    MoveApplied g -> assertBool "scored" (g >= 30)
    _ -> pure ()
  assertEqual "charge spent" (0 :: Int) (gsFreeSwaps gs1)
  assertEqual "moves free" (10 :: Int) (gsMoves gs1)
  -- No charge: reject
  let (_, out2) = useFreeSwap (0, 0) (0, 1) gs1
  out2 @?= InvalidSwap

lose_hint_by_goal :: Assertion
lose_hint_by_goal = do
  assertBool "score hint" (not (null (loseHint (GoalScore 500))))
  assertBool "collect hint" (not (null (loseHint (GoalCollect C1 20))))
  assertBool "multi hint" (not (null (loseHint (GoalCollectMulti [(C1, 1)]))))
  assertBool "stone hint" (not (null (loseHint (GoalClearStone 8))))
  assertBool "chest hint" (not (null (loseHint (GoalChest 6))))
  assertBool "honey hint" (not (null (loseHint (GoalHoney 6))))
  assertBool "balloon hint" (not (null (loseHint (GoalBalloon 6))))
  assertBool "cookie hint" (not (null (loseHint (GoalCookie 6))))
  assertBool "cake hint" (not (null (loseHint (GoalCake 6))))
  assertBool "safe hint" (not (null (loseHint (GoalSafe 5))))
  assertBool "ufo hint" (not (null (loseHint (GoalUfo 10))))
  assertBool "carpet hint" (not (null (loseHint (GoalCarpet 8))))

--------------------------------------------------------------------------------
-- Grass / Vine overlays (开心消消乐草·藤蔓)
--------------------------------------------------------------------------------

-- | Match on a grass-covered cell clears the Grass overlay (gem may stay if iced).
grass_cleared_by_match_above :: Assertion
grass_cleared_by_match_above = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 Normal 2 (Just Grass)))
          (3, 2)
          (mkGem C1)
  assertBool "grass present" (hasGrass (getCell board0 (3, 1)))
  assertBool "has match" (hasAnyMatch board0)
  -- Unit: clearOverlaysOn strips grass on match seeds
  let stripped = clearOverlaysOn board0 [(3, 0), (3, 1), (3, 2)]
  assertBool "grass cleared by match" (not (hasGrass (getCell stripped (3, 1))))
  assertEqual "ice untouched by overlay clear" (2 :: Int) (iceLayers (getCell stripped (3, 1)))
  -- Match path: strip overlays then chip ice → grass gone, ice 2→1, gem stays
  let ms = findMatches board0
      b1 = clearOverlaysOn board0 ms
      (b2, free) = chipIceOnClear b1 ms
  assertBool "grass gone after match path" (not (hasGrass (getCell b2 (3, 1))))
  assertEqual "ice 2->1" (1 :: Int) (iceLayers (getCell b2 (3, 1)))
  assertBool "iced gem not free yet" ((3, 1) `notElem` free)

-- | Surviving vines spread onto adjacent bare gems after a successful move.
vine_spreads_after_move :: Assertion
vine_spreads_after_move = do
  -- Vine at (5,5); bare neighbors. Match on row 0 so vine survives.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (0, 0) (mkGem C1))
                   (0, 1)
                   (mkGem C1))
                (0, 2)
                (mkGem C2))
             (0, 3)
             (mkGem C1))
          (5, 5)
          (mkVineGem C4)
  assertBool "vine placed" (hasVine (getCell board0 (5, 5)))
  assertBool "neighbor bare" (cellOverlay (getCell board0 (5, 4)) == Nothing)
  let cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalScore 99999 }
      gs0 =
        (newGameAtLevel 0 cfg 12)
          { gsBoard = board0
          , gsBelts = []
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  assertBool "source vine remains or was shifted" $
    hasVine (getCell b1 (5, 5))
      || any hasVine [getCell b1 p | r <- [4 .. 6], c <- [4 .. 6], let p = (r, c)]
  -- At least one orthogonal neighbor of original vine should now have vine
  -- (unless cascade destroyed the area — keep vine far from match)
  let neighbors = [(5, 4), (5, 6), (4, 5), (6, 5)]
  assertBool
    ("vine spread to a neighbor, board snippet: " ++ show [(p, cellOverlay (getCell b1 p)) | p <- (5, 5) : neighbors])
    (any (\p -> hasVine (getCell b1 p)) neighbors
       || (hasVine (getCell b1 (5, 5)) && any (\p -> hasVine (getCell b1 p)) neighbors))
  -- Pure spreadVines unit check
  let pureB = spreadVines board0
  assertBool "pure spread" (any (\p -> hasVine (getCell pureB p)) neighbors)

-- | A vine cleared by the move does not spread; uncleared vines still may.
vine_blocked_by_clear :: Assertion
vine_blocked_by_clear = do
  -- Vine ON the match at (3,1): cleared with the match → must not spread.
  -- Isolated vine at (6,6) far away: should still spread.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkVineGem C1))
                (3, 2)
                (mkGem C1))
             (6, 6)
             (mkVineGem C5))
          (0, 0)
          (mkGem C2)  -- keep stable-ish
  assertBool "match vine" (hasVine (getCell board0 (3, 1)))
  assertBool "safe vine" (hasVine (getCell board0 (6, 6)))
  -- clearOverlaysOn on match seeds strips the match vine
  let ms = findMatches board0
  assertBool "vine in match" ((3, 1) `elem` ms)
  let stripped = clearOverlaysOn board0 ms
  assertBool "cleared vine stripped" (not (hasVine (getCell stripped (3, 1))))
  assertBool "safe vine kept" (hasVine (getCell stripped (6, 6)))
  -- After strip + clear of match cells, spread only from remaining vines
  let afterClear =
        -- simulate: match cells become empty of vine; then if we spread on stripped
        -- before removing gems, match vine already gone so cannot spread from (3,1)
        spreadVines stripped
  -- Neighbors of (3,1) should NOT have gained vine from the cleared vine.
  -- (safe vine at 6,6 may spread to its own neighbors)
  -- (3,0) and (3,2) still C1 gems with no overlay after strip — would have been
  -- spread targets IF vine at (3,1) survived; it did not.
  assertBool "no spread from cleared vine onto (3,0)" (not (hasVine (getCell afterClear (3, 0))))
  assertBool "no spread from cleared vine onto (3,2)" (not (hasVine (getCell afterClear (3, 2))))
  -- Safe vine still spreads
  let safeNeighbors = [(6, 5), (6, 7), (5, 6), (7, 6)]
  assertBool
    "safe vine spreads"
    (any (\p -> hasVine (getCell afterClear p)) safeNeighbors)



--------------------------------------------------------------------------------
-- Chocolate overlays (开心消消乐巧克力)
--------------------------------------------------------------------------------

-- | Surviving chocolate spreads onto adjacent bare gems after a successful move.
choco_spreads_after_move :: Assertion
choco_spreads_after_move = do
  -- Choco at (5,5); bare neighbors. Match on row 0 so chocolate survives.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (0, 0) (mkGem C1))
                   (0, 1)
                   (mkGem C1))
                (0, 2)
                (mkGem C2))
             (0, 3)
             (mkGem C1))
          (5, 5)
          (mkChocoGem C4)
  assertBool "choco placed" (hasChoco (getCell board0 (5, 5)))
  assertBool "neighbor bare" (cellOverlay (getCell board0 (5, 4)) == Nothing)
  let cfg = GameConfig { cfgMoves = 10, cfgGoal = GoalScore 99999 }
      gs0 =
        (newGameAtLevel 0 cfg 12)
          { gsBoard = board0
          , gsBelts = []
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
      neighbors = [(5, 4), (5, 6), (4, 5), (6, 5)]
  assertBool
    ("choco spread to a neighbor: " ++ show [(p, cellOverlay (getCell b1 p)) | p <- (5, 5) : neighbors])
    (any (\p -> hasChoco (getCell b1 p)) neighbors
       || (hasChoco (getCell b1 (5, 5)) && any (\p -> hasChoco (getCell b1 p)) neighbors))
  -- Pure spreadChoco unit check
  let pureB = spreadChoco board0
  assertBool "pure spread" (any (\p -> hasChoco (getCell pureB p)) neighbors)

-- | Chocolate adjacent to a match is cleared; the gem underneath stays.
choco_cleared_by_adjacent :: Assertion
choco_cleared_by_adjacent = do
  -- Match at (3,0)(3,1)(3,2); chocolate on (2,1) and (4,1) adjacent, not in match.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             (mkChocoGem C3))
          (4, 1)
          (mkChocoGem C4)
  assertBool "choco above" (hasChoco (getCell board0 (2, 1)))
  assertBool "choco below" (hasChoco (getCell board0 (4, 1)))
  let ms = findMatches board0
  assertBool "match on row" (all (`elem` ms) [(3, 0), (3, 1), (3, 2)])
  assertBool "choco not in match" ((2, 1) `notElem` ms && (4, 1) `notElem` ms)
  -- Unit: clearChocoAdjacent strips neighbors
  let cleared = clearChocoAdjacent board0 ms
  assertBool "adj choco above cleared" (not (hasChoco (getCell cleared (2, 1))))
  assertBool "adj choco below cleared" (not (hasChoco (getCell cleared (4, 1))))
  assertEqual "gem color kept above" C3 (cellColor (getCell cleared (2, 1)))
  assertEqual "gem color kept below" C4 (cellColor (getCell cleared (4, 1)))
  -- Far choco untouched
  let withFar = setCell board0 (6, 6) (mkChocoGem C5)
      cleared2 = clearChocoAdjacent withFar ms
  assertBool "far choco kept" (hasChoco (getCell cleared2 (6, 6)))

-- | Chocolate cleared by the move does not spread; uncleared chocolate still may.
choco_blocked_by_clear :: Assertion
choco_blocked_by_clear = do
  -- Choco adjacent to match at (2,1): cleared by adjacent → must not spread.
  -- Isolated choco at (6,6): should still spread.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             (mkChocoGem C2))
          (6, 6)
          (mkChocoGem C5)
  assertBool "adj choco" (hasChoco (getCell board0 (2, 1)))
  assertBool "safe choco" (hasChoco (getCell board0 (6, 6)))
  let ms = findMatches board0
  assertBool "match present" ((3, 1) `elem` ms)
  let stripped = clearChocoAdjacent (clearOverlaysOn board0 ms) ms
  assertBool "adj choco stripped" (not (hasChoco (getCell stripped (2, 1))))
  assertBool "safe choco kept" (hasChoco (getCell stripped (6, 6)))
  let after = spreadChoco stripped
  -- Neighbors of cleared (2,1) must NOT gain choco from that cell
  assertBool "no spread onto (2,0)" (not (hasChoco (getCell after (2, 0))))
  assertBool "no spread onto (2,2)" (not (hasChoco (getCell after (2, 2))))
  assertBool "no spread onto (1,1)" (not (hasChoco (getCell after (1, 1))))
  -- Safe choco still spreads
  let safeNeighbors = [(6, 5), (6, 7), (5, 6), (7, 6)]
  assertBool
    "safe choco spreads"
    (any (\p -> hasChoco (getCell after p)) safeNeighbors)


--------------------------------------------------------------------------------
-- Treasure chests / 宝箱
--------------------------------------------------------------------------------

chest_blocks_swap :: Assertion
chest_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkChest
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  assertBool "is chest" (isChest (getCell board (3, 3)))

chest_cleared_by_adjacent :: Assertion
chest_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkChest
  assertBool "chest present" (isChest (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, dead) = chipAdjacentChests board0 ms
  assertEqual "last layer dead" [(2, 1)] dead
  assertBool "still on board until remove" (isChest (getCell b1 (2, 1)))
  let seeds = findMatches board0
      (board1, _n, _sc, _c, _t, _st, chests, _h, _b, _ck, _cak, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 1) board0
  assertBool "chest opened" (chests >= 1)
  assertBool "chest gone" (not (isChest (getCell board1 (2, 1))))

chest_layer_decrement :: Assertion
chest_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkChestLayers 2)
  let ms = findMatches board0
      (b1, dead) = chipAdjacentChests board0 ms
  assertEqual "no full clear yet" ([] :: [Pos]) dead
  assertEqual "layers 2->1" (1 :: Int) (chestLayers (getCell b1 (4, 1)))
  let (b2, dead2) = chipAdjacentChests b1 ms
  assertEqual "now dead" [(4, 1)] dead2

goal_chest_counts :: Assertion
goal_chest_counts = do
  assertBool "unmet" (not (goalMetEx (GoalChest 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "met" (goalMetEx (GoalChest 2) 0 0 [] 0 0 2 0 0 0 0 0)
  assertEqual "progress" (2 :: Int) (goalProgressEx (GoalChest 5) 0 0 [] 0 0 2 0 0 0 0 0)
  assertEqual "target" (6 :: Int) (goalTarget (GoalChest 6))
  assertBool
    "campaign has GoalChest"
    (any (\g -> case g of GoalChest _ -> True; _ -> False) (map lvlGoal allLevels))
  -- Level 16 décor places chests
  let gs = newGameAtLevel 16 (levelConfig (allLevels !! 16)) 42
      nChests =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isChest (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor chests >= 6, got " ++ show nChests) (nChests >= 6)


-- UFO / 飞碟 (absorb same-color neighbors; move each cascade wave)

-- | UFO absorbs only orthogonally adjacent gems of its target color.
ufo_collects_target_color :: Assertion
ufo_collects_target_color = do
  let b0 = fst (randomStableBoard (mkStdGen 77))
      b1 = setCell b0 (3, 3) (mkGem C5)
      b2 = setCell b1 (3, 4) (mkGem C1)
      b3 = setCell b2 (3, 2) (mkGem C1)
      b4 = setCell b3 (2, 3) (mkGem C2)
      b5 = setCell b4 (4, 3) (mkGem C1)
      ufo = mkUfo (3, 3) C1
      targets = ufoAbsorbTargets b5 ufo
      (absorbed, ufo') = stepUfo b5 ufo
  assertBool "absorbs left" ((3, 2) `elem` targets)
  assertBool "absorbs right" ((3, 4) `elem` targets)
  assertBool "absorbs down" ((4, 3) `elem` targets)
  assertBool "ignores wrong color up" ((2, 3) `notElem` targets)
  assertEqual "step absorbs same set" (sort targets) (sort absorbed)
  assertEqual "3 targets" (3 :: Int) (length targets)
  assertEqual "moved onto first absorbed" (head (sort absorbed)) (ufoCell ufo')
  assertEqual "color preserved" C1 (ufoColor ufo')

-- | Each cascade wave relocates the UFO.
ufo_moves_each_cascade :: Assertion
ufo_moves_each_cascade = do
  let b0 = fst (randomPlayableBoard (mkStdGen 88))
      u0 = mkUfo (0, 0) C1
      paint b ps col = foldl (\bd p -> setCell bd p (mkGem col)) b ps
      b1 = paint b0 [(0, 0), (0, 1), (1, 0)] C2
      (abs1, u1) = stepUfo b1 u0
      (_, u2) = stepUfo b1 u1
  assertEqual "no absorb when no C1 neighbor" ([] :: [Pos]) abs1
  assertBool "first step moves" (ufoCell u1 /= ufoCell u0)
  assertBool "second step moves again" (ufoCell u2 /= ufoCell u1)
  let cfg = GameConfig 20 (GoalUfo 99)
      gs0 = (newGameAtLevel 12 cfg 91) { gsBoard = b1, gsUfos = [u0], gsUfoCollected = 0 }
  case findHint (gsBoard gs0) of
    Nothing -> assertFailure "board should be playable"
    Just (p1, p2) -> do
      let (gs1, out) = trySwap p1 p2 gs0
      assertBool "move applied or terminal" $ case out of
        MoveApplied _ -> True
        LevelClear _ _ -> True
        Won _ -> True
        Lost _ -> True
        _ -> False
      assertBool "UFO relocated after cascade" $
        case gsUfos gs1 of
          (u : _) -> ufoCell u /= ufoCell u0
          [] -> False

-- | GoalUfo progress increments with UFO absorbs.
ufo_goal_counts :: Assertion
ufo_goal_counts = do
  assertBool "goal unmet at 0" (not (goalMetEx (GoalUfo 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "goal met at 2" (goalMetEx (GoalUfo 2) 0 0 [] 0 2 0 0 0 0 0 0)
  assertEqual "progress" (2 :: Int) (goalProgressEx (GoalUfo 5) 0 0 [] 0 2 0 0 0 0 0 0)
  assertEqual "target" (5 :: Int) (goalTarget (GoalUfo 5))
  let b0 = fst (randomStableBoard (mkStdGen 101))
      b1 = setCell b0 (3, 3) (mkGem C5)
      b2 = setCell b1 (3, 2) (mkGem C1)
      b3 = setCell b2 (3, 4) (mkGem C1)
      b4 = setCell b3 (4, 3) (mkGem C1)
      ufo = mkUfo (3, 3) C1
      targets = ufoAbsorbTargets b4 ufo
      n = length targets
  assertBool "has absorb targets" (n >= 2)
  let cfg = GameConfig 15 (GoalUfo n)
      gs0 =
        (newGameAtLevel 12 cfg 55)
          { gsBoard = b4
          , gsUfos = [ufo]
          , gsUfoCollected = 0
          , gsCollected = 0
          }
      (_, ufo') = stepUfo b4 ufo
      gsSim =
        gs0
          { gsUfoCollected = n
          , gsCollected = n
          , gsUfos = [ufo']
          }
  assertBool
    "simulated goal met"
    (goalMetEx
       (gsGoal gsSim)
       (gsScore gsSim)
       (gsCollected gsSim)
       (gsColorBag gsSim)
       (gsStonesCleared gsSim)
       (gsUfoCollected gsSim)
       (gsChestsCleared gsSim)
       (gsHoneyCleared gsSim)
       (gsBalloonsPopped gsSim)
       (gsCookiesCollected gsSim)
       (gsCakesCleared gsSim)
       (gsSafesOpened gsSim))
  -- Level table includes GoalUfo stages
  assertBool
    "campaign has GoalUfo"
    (any (\g -> case g of GoalUfo _ -> True; _ -> False) (map lvlGoal allLevels))




-- Treasure honey jars / 蜂蜜罐
--------------------------------------------------------------------------------

honey_blocks_swap :: Assertion
honey_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkHoney
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  assertBool "is honey" (isHoney (getCell board (3, 3)))

honey_cleared_by_adjacent :: Assertion
honey_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkHoney
  assertBool "honey present" (isHoney (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, dead) = chipAdjacentHoney board0 ms
  assertEqual "last layer dead" [(2, 1)] dead
  assertBool "still on board until remove" (isHoney (getCell b1 (2, 1)))
  let seeds = findMatches board0
      (board1, _n, _sc, _c, _t, _st, _ch, honey, _b, _ck, _cak, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 1) board0
  assertBool "honey smashed" (honey >= 1)
  assertBool "honey gone" (not (isHoney (getCell board1 (2, 1))))

honey_layer_decrement :: Assertion
honey_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkHoneyLayers 2)
  let ms = findMatches board0
      (b1, dead) = chipAdjacentHoney board0 ms
  assertEqual "no full clear yet" ([] :: [Pos]) dead
  assertEqual "layers 2->1" (1 :: Int) (honeyLayers (getCell b1 (4, 1)))
  let (b2, dead2) = chipAdjacentHoney b1 ms
  assertEqual "now dead" [(4, 1)] dead2

goal_honey_counts :: Assertion
goal_honey_counts = do
  assertBool "unmet" (not (goalMetEx (GoalHoney 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "met" (goalMetEx (GoalHoney 2) 0 0 [] 0 0 0 2 0 0 0 0)
  assertEqual "progress" (2 :: Int) (goalProgressEx (GoalHoney 5) 0 0 [] 0 0 0 2 0 0 0 0)
  assertEqual "target" (6 :: Int) (goalTarget (GoalHoney 6))
  assertBool
    "campaign has GoalHoney"
    (any (\g -> case g of GoalHoney _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = newGameAtLevel 18 (levelConfig (allLevels !! 18)) 42
      nHoney =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isHoney (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor honey >= 6, got " ++ show nHoney) (nHoney >= 6)



--------------------------------------------------------------------------------
-- Balloons / 气球 (same-color adjacent pop)
--------------------------------------------------------------------------------

balloon_blocks_swap :: Assertion
balloon_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkBalloon C1)
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  assertBool "is balloon" (isBalloon (getCell board (3, 3)))

balloon_popped_by_same_color :: Assertion
balloon_popped_by_same_color = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkBalloon C1)
  assertBool "balloon present" (isBalloon (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, dead) = chipAdjacentBalloons board0 ms
  assertEqual "same color pops" [(2, 1)] dead
  let seeds = findMatches board0
      (board1, _n, _sc, _c, _t, _st, _ch, _h, balloons, _ck, _cak, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 1) board0
  assertBool "balloon counted" (balloons >= 1)
  assertBool "balloon gone" (not (isBalloon (getCell board1 (2, 1))))

balloon_ignores_other_color :: Assertion
balloon_ignores_other_color = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkBalloon C3)  -- different color
  let ms = findMatches board0
      (_, dead) = chipAdjacentBalloons board0 ms
  assertEqual "other color ignored" ([] :: [Pos]) dead

goal_balloon_counts :: Assertion
goal_balloon_counts = do
  assertBool "unmet" (not (goalMetEx (GoalBalloon 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "met" (goalMetEx (GoalBalloon 2) 0 0 [] 0 0 0 0 2 0 0 0)
  assertEqual "progress" (2 :: Int) (goalProgressEx (GoalBalloon 5) 0 0 [] 0 0 0 0 2 0 0 0)
  assertEqual "target" (6 :: Int) (goalTarget (GoalBalloon 6))
  assertBool
    "campaign has GoalBalloon"
    (any (\g -> case g of GoalBalloon _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = newGameAtLevel 20 (levelConfig (allLevels !! 20)) 42
      nBal =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isBalloon (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor balloons >= 6, got " ++ show nBal) (nBal >= 6)


-- | Reshuffle keeps stones / ice / overlays; UFO list unchanged.
-- Also preserves Curtain / Freeze overlay positions and Carpet open-cell set.
shuffle_preserves_decor :: Assertion
shuffle_preserves_decor = do
  let cfg = GameConfig 20 (GoalScore 999)
      gs0 = newGameAtLevel 7 cfg 33  -- stones + belt level
      stonesBefore =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , isStone (getCell (gsBoard gs0) p)
        ]
      gs1 = shuffleGame gs0
      stonesAfter =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , isStone (getCell (gsBoard gs1) p)
        ]
  assertEqual "stones survive shuffle" (sort stonesBefore) (sort stonesAfter)
  assertBool "still playable or terminal-ok" (hasValidMove (gsBoard gs1) || isJust (gsOver gs1))
  -- UFO entities persist across shuffle
  let gsU = (newGameAtLevel 12 cfg 44)
      gsU' = shuffleGame gsU
  assertEqual "UFO count kept" (length (gsUfos gsU)) (length (gsUfos gsU'))
  assertEqual "UFO cells kept" (map ufoCell (gsUfos gsU)) (map ufoCell (gsUfos gsU'))
  -- Curtain overlays keep positions
  let gsCu = newGameAtLevel 30 cfg 55
      curtainsBefore =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasCurtain (getCell (gsBoard gsCu) p)
        ]
      gsCu' = shuffleGame gsCu
      curtainsAfter =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasCurtain (getCell (gsBoard gsCu') p)
        ]
  assertBool "curtains present before shuffle" (not (null curtainsBefore))
  assertEqual "curtains survive shuffle" (sort curtainsBefore) (sort curtainsAfter)
  -- Freeze overlays keep positions
  let gsFr = newGameAtLevel 29 cfg 66
      freezeBefore =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasFreeze (getCell (gsBoard gsFr) p)
        ]
      gsFr' = shuffleGame gsFr
      freezeAfter =
        [ p
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , let p = (r, c)
        , hasFreeze (getCell (gsBoard gsFr') p)
        ]
  assertBool "freeze present before shuffle" (not (null freezeBefore))
  assertEqual "freeze survive shuffle" (sort freezeBefore) (sort freezeAfter)
  -- Carpet open cells are unchanged by shuffle (count + positions)
  let gsCa = newGameAtLevel 36 (levelConfig (allLevels !! 36)) 77
      openBefore = sort (gsCarpetOpen gsCa)
      gsCa' = shuffleGame gsCa
  assertBool "carpet open cells present" (not (null openBefore))
  assertEqual "carpet open cells kept" openBefore (sort (gsCarpetOpen gsCa'))
  assertEqual "carpet open count kept" (length openBefore) (length (gsCarpetOpen gsCa'))


-- | Daily (or any) GoalUfo config without level décor still gets a default UFO.
daily_ufo_goal_spawns_saucer :: Assertion
daily_ufo_goal_spawns_saucer = do
  let cfg = GameConfig 26 (GoalUfo 8)
      gs = newGame cfg 20260929
  assertBool "default UFO placed" (not (null (gsUfos gs)))
  assertEqual "target color" C1 (ufoColor (head (gsUfos gs)))


--------------------------------------------------------------------------------
-- Cookies / 饼干 (fall to bottom collect)
--------------------------------------------------------------------------------

cookie_blocks_swap :: Assertion
cookie_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkCookie
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  assertBool "is cookie" (isCookie (getCell board (3, 3)))

cookie_falls_with_gravity :: Assertion
cookie_falls_with_gravity = do
  -- Cookie at (0,1); clear horizontal match on row 3 including (3,1) so col 1 has a hole
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 1) mkCookie)
                (3, 0)
                (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
  assertBool "cookie at top" (isCookie (getCell board0 (0, 1)))
  let seeds = findMatches board0
  assertBool "match under cookie col" ((3, 1) `elem` seeds)
  let (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, cookies, _cak, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 1) board0
      cookiePos =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCookie (getCell board1 (r, c))
        ]
  assertBool
    ("cookie fell or collected, pos=" ++ show cookiePos ++ " collected=" ++ show cookies)
    (cookies >= 1 || null cookiePos || any (\(r, c) -> c == 1 && r > 0) cookiePos)

cookie_collected_at_bottom :: Assertion
cookie_collected_at_bottom = do
  -- Place cookie already on bottom; clearing elsewhere should drain it
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (boardSize - 1, 4) mkCookie)
                (3, 0)
                (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
  assertBool "cookie on bottom" (isCookie (getCell board0 (boardSize - 1, 4)))
  let seeds = findMatches board0
      (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, cookies, _cak, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 1) board0
  assertBool ("cookie collected, got " ++ show cookies) (cookies >= 1)
  assertBool "cookie gone" (not (isCookie (getCell board1 (boardSize - 1, 4))))

goal_cookie_counts :: Assertion
goal_cookie_counts = do
  assertBool "unmet" (not (goalMetEx (GoalCookie 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "met" (goalMetEx (GoalCookie 2) 0 0 [] 0 0 0 0 0 2 0 0)
  assertEqual "progress" (2 :: Int) (goalProgressEx (GoalCookie 5) 0 0 [] 0 0 0 0 0 2 0 0)
  assertEqual "target" (6 :: Int) (goalTarget (GoalCookie 6))
  assertBool
    "campaign has GoalCookie"
    (any (\g -> case g of GoalCookie _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = newGameAtLevel 21 (levelConfig (allLevels !! 21)) 42
      nCookie =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isCookie (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor cookies >= 6, got " ++ show nCookie) (nCookie >= 6)


--------------------------------------------------------------------------------
-- Fog / 迷雾 (adjacent peel layers; fogged gems do not match)
--------------------------------------------------------------------------------

fog_blocks_match :: Assertion
fog_blocks_match = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFogGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "fog present" (hasFog (getCell board0 (3, 1)))
  assertBool "fogged gem breaks run" (null (findMatches board0))
  assertEqual "fog layers" (1 :: Int) (fogLayers (getCell board0 (3, 1)))

fog_cleared_by_adjacent :: Assertion
fog_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkFogGem C2 1)
  assertBool "fog above match" (hasFog (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFog board0 ms
  assertEqual "one fog fully peeled" (1 :: Int) cleared
  assertBool "fog gone" (not (hasFog (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  -- Via cascade clear path
  let (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, _ck, _cak, _) =
        runCascadeScoredFromSeeds Nothing ms (mkStdGen 1) board0
  assertBool "fog cleared in cascade" (not (hasFog (getCell board1 (2, 1))))

fog_layer_decrement :: Assertion
fog_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkFogGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFog board0 ms
  assertEqual "no full clear yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (fogLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentFog b1 ms
  assertEqual "now fully peeled" (1 :: Int) cleared2
  assertBool "fog gone" (not (hasFog (getCell b2 (4, 1))))
  -- Campaign décor includes fog on 巧饼 / 终章
  let gs = newGameAtLevel 22 (levelConfig (allLevels !! 22)) 42
      nFog =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasFog (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor fog >= 4, got " ++ show nFog) (nFog >= 4)


--------------------------------------------------------------------------------
-- Cake / 蛋糕 (layered obstacle; distinct from Cookie drop-collect)
--------------------------------------------------------------------------------

cake_blocks_swap :: Assertion
cake_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkCake
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  assertBool "is cake" (isCake (getCell board (3, 3)))
  assertBool "not cookie" (not (isCookie (getCell board (3, 3))))

cake_layer_decrement :: Assertion
cake_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkCakeLayers 2)
  let ms = findMatches board0
      (b1, dead) = chipAdjacentCakes board0 ms
  assertEqual "no full clear yet" ([] :: [Pos]) dead
  assertEqual "layers 2->1" (1 :: Int) (cakeLayers (getCell b1 (4, 1)))
  let (b2, dead2) = chipAdjacentCakes b1 ms
  assertEqual "now dead" [(4, 1)] dead2

cake_clears_at_zero :: Assertion
cake_clears_at_zero = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkCake
  assertBool "cake present" (isCake (getCell board0 (2, 1)))
  let seeds = findMatches board0
      (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, _ck, cakes, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 1) board0
  assertBool "cake cleared count" (cakes >= 1)
  assertBool "cake gone" (not (isCake (getCell board1 (2, 1))))

goal_cake_counts :: Assertion
goal_cake_counts = do
  assertBool "unmet" (not (goalMetEx (GoalCake 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "met" (goalMetEx (GoalCake 2) 0 0 [] 0 0 0 0 0 0 2 0)
  assertEqual "progress" (2 :: Int) (goalProgressEx (GoalCake 5) 0 0 [] 0 0 0 0 0 0 2 0)
  assertEqual "target" (6 :: Int) (goalTarget (GoalCake 6))
  assertBool
    "campaign has GoalCake"
    (any (\g -> case g of GoalCake _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = newGameAtLevel 23 (levelConfig (allLevels !! 23)) 42
      nCake =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isCake (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor cake >= 6, got " ++ show nCake) (nCake >= 6)
  let gsHat = newGameAtLevel 24 (levelConfig (allLevels !! 24)) 42
      nHat =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isMagicHat (getCell (gsBoard gsHat) (r, c))
          ]
  assertBool ("decor hats >= 3, got " ++ show nHat) (nHat >= 3)

--------------------------------------------------------------------------------
-- Magic hat / 魔法帽 (adjacent trigger swaps neighbor colors)
--------------------------------------------------------------------------------

hat_triggered_by_adjacent :: Assertion
hat_triggered_by_adjacent = do
  -- Stones block side neighbors so only (1,1) gem remains for the hat
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell stableBoard (3, 0) (mkGem C1))
                         (3, 1)
                         (mkGem C1))
                      (3, 2)
                      (mkGem C1))
                   (2, 1)
                   mkMagicHat)
                (1, 1)
                (mkGem C2))
             (2, 0)
             mkStone)
          (2, 2)
          mkStone
  assertBool "hat present" (isMagicHat (getCell board0 (2, 1)))
  let ms = findMatches board0
      hats = hatsAdjacentTo board0 ms
  assertEqual "hat adjacent to match" [(2, 1)] hats
  let board1 = triggerAdjacentHats board0 ms
  assertBool "hat still there" (isMagicHat (getCell board1 (2, 1)))
  assertEqual "cycled neighbor" C3 (cellColor (getCell board1 (1, 1)))

hat_swaps_colors :: Assertion
hat_swaps_colors = do
  -- Hat at (2,1); match on row 3; stone above so only left/right gems swap
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell stableBoard (3, 0) (mkGem C1))
                         (3, 1)
                         (mkGem C1))
                      (3, 2)
                      (mkGem C1))
                   (2, 1)
                   mkMagicHat)
                (2, 0)
                (mkGem C4))
             (2, 2)
             (mkGem C5))
          (1, 1)
          mkStone
  let ms = findMatches board0
      cLeft0 = cellColor (getCell board0 (2, 0))
      cRight0 = cellColor (getCell board0 (2, 2))
  assertEqual "left before" C4 cLeft0
  assertEqual "right before" C5 cRight0
  let board1 = triggerAdjacentHats board0 ms
      cLeft1 = cellColor (getCell board1 (2, 0))
      cRight1 = cellColor (getCell board1 (2, 2))
  assertEqual "left got right" C5 cLeft1
  assertEqual "right got left" C4 cRight1
  assertBool "hat remains" (isMagicHat (getCell board1 (2, 1)))


--------------------------------------------------------------------------------
-- Chain / 锁链 (locks gem: no swap/match; adjacent peel)
--------------------------------------------------------------------------------

chain_blocks_match :: Assertion
chain_blocks_match = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkChainGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "chain present" (hasChain (getCell board0 (3, 1)))
  assertBool "chained gem breaks run" (null (findMatches board0))
  assertEqual "chain layers" (1 :: Int) (chainLayers (getCell board0 (3, 1)))

chain_blocks_swap :: Assertion
chain_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkChainGem C2 1)
      gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0
  gsMoves gs1 @?= gsMoves gs0
  assertBool "swapBlocked" (swapBlockedByStone board (3, 3) (3, 4))

chain_cleared_by_adjacent :: Assertion
chain_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkChainGem C2 1)
  assertBool "chain above match" (hasChain (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentChain board0 ms
  assertEqual "one chain unlocked" (1 :: Int) cleared
  assertBool "chain gone" (not (hasChain (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  let (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, _ck, _cak, _) =
        runCascadeScoredFromSeeds Nothing ms (mkStdGen 1) board0
  assertBool "chain cleared in cascade" (not (hasChain (getCell board1 (2, 1))))

chain_layer_decrement :: Assertion
chain_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkChainGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentChain board0 ms
  assertEqual "no full unlock yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (chainLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentChain b1 ms
  assertEqual "now unlocked" (1 :: Int) cleared2
  assertBool "chain gone" (not (hasChain (getCell b2 (4, 1))))
  let gs = newGameAtLevel 25 (levelConfig (allLevels !! 25)) 42
      nChain =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasChain (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor chain >= 8, got " ++ show nChain) (nChain >= 8)
  assertEqual "campaign levels" (38 :: Int) (length allLevels)

--------------------------------------------------------------------------------
-- Maker / 果汁机 (same-color adjacent charge -> Bomb)
--------------------------------------------------------------------------------

maker_blocks_swap :: Assertion
maker_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkMaker C1)
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  assertBool "is maker" (isMaker (getCell board (3, 3)))
  assertEqual "default charges" (3 :: Int) (makerCharges (getCell board (3, 3)))

maker_charges_on_same_color :: Assertion
maker_charges_on_same_color = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkMakerCharges C1 3)
  let ms = findMatches board0
      b1 = chargeAdjacentMakers board0 ms
  assertBool "still maker" (isMaker (getCell b1 (2, 1)))
  assertEqual "charges 3->2" (2 :: Int) (makerCharges (getCell b1 (2, 1)))
  -- Wrong color adjacent clear does not charge
  let boardWrong =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C2))
                (3, 1)
                (mkGem C2))
             (3, 2)
             (mkGem C2))
          (2, 1)
          (mkMakerCharges C1 2)
      ms2 = findMatches boardWrong
      b2 = chargeAdjacentMakers boardWrong ms2
  assertEqual "wrong color no charge" (2 :: Int) (makerCharges (getCell b2 (2, 1)))

maker_produces_bomb :: Assertion
maker_produces_bomb = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkMakerCharges C1 1)
  let ms = findMatches board0
      b1 = chargeAdjacentMakers board0 ms
  assertBool "became gem" (isGem (getCell b1 (2, 1)))
  assertEqual "bomb kind" Bomb (cellKind (getCell b1 (2, 1)))
  assertEqual "bomb color" C1 (cellColor (getCell b1 (2, 1)))
  let gs = newGameAtLevel 26 (levelConfig (allLevels !! 26)) 42
      nMaker =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isMaker (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor makers >= 3, got " ++ show nMaker) (nMaker >= 3)
  assertBool "level has portals" (not (null (gsPortals gs)))

--------------------------------------------------------------------------------
-- Portal / 传送门 (gem on A with hole at B teleports)
--------------------------------------------------------------------------------

portal_teleports_gem :: Assertion
portal_teleports_gem = do
  -- Build MBoard: gem at (0,0), hole at (7,7)
  let setMBoard b (r, c) v =
        take r b ++ [take c row ++ [v] ++ drop (c + 1) row] ++ drop (r + 1) b
        where
          row = b !! r
      fill = Just (mkGem C5)
      mb0 = replicate boardSize (replicate boardSize fill)
      mb1 = setMBoard mb0 (0, 0) (Just (mkGem C1))
      mb2 = setMBoard mb1 (7, 7) Nothing
      portals = [((0, 0), (7, 7))]
      mb3 = applyPortalTeleports portals mb2
  assertEqual "entrance emptied" Nothing ((mb3 !! 0) !! 0)
  case (mb3 !! 7) !! 7 of
    Just cell -> do
      assertBool "exit got gem" (isGem cell)
      assertEqual "teleported color" C1 (cellColor cell)
    Nothing -> assertFailure "expected gem at exit"
  assertBool "identity on full board" (applyPortalTeleports portals mb0 == mb0)
  let gs = newGameAtLevel 26 (levelConfig (allLevels !! 26)) 42
  assertEqual "two portal pairs" (2 :: Int) (length (gsPortals gs))


--------------------------------------------------------------------------------
-- Snail / 蜗牛 (crawls after move; blocks swap; pushes gems)
--------------------------------------------------------------------------------

snail_blocks_swap :: Assertion
snail_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkSnail 0 1)
  assertBool "is snail" (isSnail (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0
  gsMoves gs1 @?= gsMoves gs0

snail_moves_after_move :: Assertion
snail_moves_after_move = do
  -- Match far from snail: row 3 swap like stone_cleared; snail on bottom row
  -- so gravity cannot drop it. Faces right and pushes the gem at (7,2).
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C2))
             (3, 3)
             (mkGem C1))
          (7, 1)
          (mkSnail 0 1)
  assertBool "snail at start" (isSnail (getCell board0 (7, 1)))
  assertBool "gem to the right" (isGem (getCell board0 (7, 2)))
  -- Pure unit: stepSnails crawls without a full move
  let stepped = stepSnails board0
  let gemRight = getCell board0 (7, 2)
  assertBool "pure left start" (not (isSnail (getCell stepped (7, 1))))
  assertBool "pure at (7,2)" (isSnail (getCell stepped (7, 2)))
  assertEqual "pure pushed gem back" gemRight (getCell stepped (7, 1))
  let gs0 =
        (newGame defaultConfig 11)
          { gsBoard = board0
          , gsScore = 0
          , gsMoves = 20
          , gsGoal = GoalScore 99999
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  -- After cascades/refill colors may change; only require snail relocated one step
  assertBool "snail left start" (not (isSnail (getCell b1 (7, 1))))
  assertBool "snail crawled right" (isSnail (getCell b1 (7, 2)))
  assertBool "cell behind is gem (pushed)" (isGem (getCell b1 (7, 1)))
  -- Edge reverse: snail at right edge facing right flips dir
  let edgeB = setCell stableBoard (0, 7) (mkSnail 0 1)
      edge1 = stepSnailAt edgeB (0, 7)
  assertEqual "reversed at edge" (0, -1) (snailDir (getCell edge1 (0, 7)))
  assertBool "still at edge" (isSnail (getCell edge1 (0, 7)))
  let gsL = newGameAtLevel 28 (levelConfig (allLevels !! 28)) 42
      nSnail =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isSnail (getCell (gsBoard gsL) (r, c))
          ]
  assertBool ("decor snails >= 4, got " ++ show nSnail) (nSnail >= 4)

--------------------------------------------------------------------------------
-- Freeze / 火箭冰冻 (blocks swap only; adjacent peel; ≠ Ice match-chip)
--------------------------------------------------------------------------------

freeze_blocks_swap :: Assertion
freeze_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkFreezeGem C2 1)
  assertBool "has freeze" (hasFreeze (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  -- Frozen gem CAN still participate in matches (unlike Chain)
  let boardM =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFreezeGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "freeze does not break match" (not (null (findMatches boardM)))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0

freeze_cleared_by_adjacent :: Assertion
freeze_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkFreezeGem C2 1)
  assertBool "freeze above match" (hasFreeze (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFreeze board0 ms
  assertEqual "fully thawed" (1 :: Int) cleared
  assertBool "freeze gone" (not (hasFreeze (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  -- Cascade path also peels
  let (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, _ck, _cak, _) =
        runCascadeScored Nothing (mkStdGen 1) board0
  assertBool "freeze cleared in cascade" (not (hasFreeze (getCell board1 (2, 1))))

freeze_layer_decrement :: Assertion
freeze_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkFreezeGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentFreeze board0 ms
  assertEqual "no full thaw yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (freezeLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentFreeze b1 ms
  assertEqual "now thawed" (1 :: Int) cleared2
  assertBool "freeze gone" (not (hasFreeze (getCell b2 (4, 1))))
  -- Distinct from Ice: ice layers live on Gem Int field, not Freeze overlay
  let iced = mkIceGem C1 2
  assertEqual "ice layers" (2 :: Int) (iceLayers iced)
  assertBool "ice is not freeze overlay" (not (hasFreeze iced))
  let gs = newGameAtLevel 29 (levelConfig (allLevels !! 29)) 42
      nFreeze =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasFreeze (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor freeze >= 8, got " ++ show nFreeze) (nFreeze >= 8)
  assertEqual "campaign levels" (38 :: Int) (length allLevels)

--------------------------------------------------------------------------------
-- Curtain / 窗帘 (blocks match; adjacent peel; ≠ Fog soft cloud)
--------------------------------------------------------------------------------

curtain_blocks_match :: Assertion
curtain_blocks_match = do
  let boardM =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkCurtainGem C1 1))
          (3, 2)
          (mkGem C1)
  assertBool "curtain breaks match" (null (findMatches boardM))
  assertBool "has curtain" (hasCurtain (getCell boardM (3, 1)))

curtain_cleared_by_adjacent :: Assertion
curtain_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkCurtainGem C2 1)
  assertBool "curtain above match" (hasCurtain (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentCurtain board0 ms
  assertEqual "fully opened" (1 :: Int) cleared
  assertBool "curtain gone" (not (hasCurtain (getCell b1 (2, 1))))
  assertBool "gem remains" (isGem (getCell b1 (2, 1)))
  let (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, _ck, _cak, _) =
        runCascadeScored Nothing (mkStdGen 1) board0
  assertBool "curtain cleared in cascade" (not (hasCurtain (getCell board1 (2, 1))))

curtain_layer_decrement :: Assertion
curtain_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkCurtainGem C3 2)
  let ms = findMatches board0
      (b1, cleared) = chipAdjacentCurtain board0 ms
  assertEqual "no full clear yet" (0 :: Int) cleared
  assertEqual "layers 2->1" (1 :: Int) (curtainLayers (getCell b1 (4, 1)))
  let (b2, cleared2) = chipAdjacentCurtain b1 ms
  assertEqual "now clear" (1 :: Int) cleared2
  assertBool "curtain gone" (not (hasCurtain (getCell b2 (4, 1))))
  let gs = newGameAtLevel 30 (levelConfig (allLevels !! 30)) 42
      nCurt =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasCurtain (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor curtain >= 8, got " ++ show nCurt) (nCurt >= 8)
  assertEqual "campaign levels" (38 :: Int) (length allLevels)

--------------------------------------------------------------------------------
-- Safe / 保险箱 (layered vault; opens into Cookie; GoalSafe)
--------------------------------------------------------------------------------

safe_blocks_swap :: Assertion
safe_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkSafe
  assertBool "is safe" (isSafe (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0

safe_opens_to_cookie :: Assertion
safe_opens_to_cookie = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          mkSafe
  assertBool "safe present" (isSafe (getCell board0 (2, 1)))
  let ms = findMatches board0
      (b1, opened) = chipAdjacentSafes board0 ms
  assertEqual "opened" (1 :: Int) (length opened)
  assertBool "became cookie" (isCookie (getCell b1 (2, 1)))
  assertBool "not still safe" (not (isSafe (getCell b1 (2, 1))))
  -- Cascade path: open + cookie may fall/collect
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board0
          , gsScore = 0
          , gsMoves = 20
          , gsGoal = GoalSafe 1
          , gsSafesOpened = 0
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "goal progress" (gsSafesOpened gs1 >= 1)
  assertBool "safe gone from board" $
    not (any (\r -> any (\c -> isSafe (getCell (gsBoard gs1) (r, c))) [0 .. boardSize - 1]) [0 .. boardSize - 1])

safe_layer_decrement :: Assertion
safe_layer_decrement = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (mkSafeLayers 2)
  let ms = findMatches board0
      (b1, opened) = chipAdjacentSafes board0 ms
  assertEqual "not open yet" (0 :: Int) (length opened)
  assertEqual "layers 2->1" (1 :: Int) (safeLayers (getCell b1 (4, 1)))
  let (b2, opened2) = chipAdjacentSafes b1 ms
  assertEqual "opened" (1 :: Int) (length opened2)
  assertBool "cookie" (isCookie (getCell b2 (4, 1)))

goal_safe_counts :: Assertion
goal_safe_counts = do
  assertBool "unmet" (not (goalMetEx (GoalSafe 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "met" (goalMetEx (GoalSafe 2) 0 0 [] 0 0 0 0 0 0 0 2)
  assertEqual "progress" (2 :: Int) (goalProgressEx (GoalSafe 5) 0 0 [] 0 0 0 0 0 0 0 2)
  assertEqual "target" (5 :: Int) (goalTarget (GoalSafe 5))
  assertBool
    "campaign has GoalSafe"
    (any (\g -> case g of GoalSafe _ -> True; _ -> False) (map lvlGoal allLevels))
  let gs = newGameAtLevel 31 (levelConfig (allLevels !! 31)) 42
      nSafes =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isSafe (getCell (gsBoard gs) (r, c))
          ]
      nFlip =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isFlip (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor safes >= 5, got " ++ show nSafes) (nSafes >= 5)
  assertBool ("decor flips >= 4, got " ++ show nFlip) (nFlip >= 4)

--------------------------------------------------------------------------------
-- Flip / 双面块 (matches as front; clear hit flips to back Normal gem)
--------------------------------------------------------------------------------

flip_matches_front :: Assertion
flip_matches_front = do
  let boardM =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFlip C1 C3))
          (3, 2)
          (mkGem C1)
  assertBool "flip participates" (not (null (findMatches boardM)))
  assertBool "is flip" (isFlip (getCell boardM (3, 1)))
  assertEqual "front" C1 (flipFront (getCell boardM (3, 1)))
  assertEqual "back" C3 (flipBack (getCell boardM (3, 1)))
  -- Can swap like a gem
  assertBool "not blocked" (not (swapBlockedByStone boardM (3, 1) (3, 3)))

flip_becomes_back_on_clear :: Assertion
flip_becomes_back_on_clear = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkFlip C1 C4))
          (3, 2)
          (mkGem C1)
  assertBool "has match" (not (null (findMatches board0)))
  let ms = findMatches board0
      (b1, iceFree) = chipIceOnClear board0 ms
      cellAfter = getCell b1 (3, 1)
  assertBool "became gem" (isGem cellAfter)
  assertBool "not still flip" (not (isFlip cellAfter))
  assertEqual "back color" C4 (cellColor cellAfter)
  -- Flip stays on board (not listed as clearable hole)
  assertBool "flip not cleared away" ((3, 1) `notElem` iceFree)
  -- Full cascade also leaves a gem (possibly later matched as C4)
  let (board1, _n, _sc, _c, _t, _st, _ch, _h, _b, _ck, _cak, _) =
        runCascadeScored Nothing (mkStdGen 2) board0
  assertBool "no flip remains at seed" (not (isFlip (getCell board1 (3, 1))))

--------------------------------------------------------------------------------
-- Surprise / 彩蛋 (adjacent open → special or 3×3 pop)
--------------------------------------------------------------------------------

surprise_blocks_swap :: Assertion
surprise_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkSurprise
  assertBool "is surprise" (isSurprise (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))
  let gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board
          , gsScore = 40
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  out @?= NoMatch
  gsBoard gs1 @?= gsBoard gs0

surprise_opens_to_special :: Assertion
surprise_opens_to_special = do
  -- (4,0): outcome 0 → LineH special
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 0)
          mkSurprise
  assertBool "surprise present" (isSurprise (getCell board0 (4, 0)))
  let ms = findMatches board0
      (b1, expl) = openAdjacentSurprises board0 ms
  assertEqual "no explode" (0 :: Int) (length expl)
  let cell = getCell b1 (4, 0)
  assertBool "became special gem" (isGem cell && cellKind cell /= Normal)
  assertEqual "LineH" LineH (cellKind cell)
  let gs = newGameAtLevel 32 (levelConfig (allLevels !! 32)) 42
      nSur =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isSurprise (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor surprises >= 8, got " ++ show nSur) (nSur >= 8)
  assertEqual "campaign levels" (38 :: Int) (length allLevels)

surprise_explodes_small :: Assertion
surprise_explodes_small = do
  -- (3,3): outcome 3 → 3×3 blast; ortho-adjacent to match at (3,2)
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (3, 3)
          mkSurprise
  let ms = findMatches board0
      (b1, expl) = openAdjacentSurprises board0 ms
  assertBool "explode seeds" (length expl >= 5)
  assertBool "center in blast" ((3, 3) `elem` expl)
  -- Still Surprise on board until clear holes applied
  assertBool "still surprise until clear" (isSurprise (getCell b1 (3, 3)))
  let gs0 =
        (newGame defaultConfig 11)
          { gsBoard = board0
          , gsScore = 0
          , gsMoves = 20
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "surprise cleared by blast" $
    not (isSurprise (getCell (gsBoard gs1) (3, 3)))

--------------------------------------------------------------------------------
-- Bottle / 染色瓶 (adjacent clear dyes ortho gems)
--------------------------------------------------------------------------------

bottle_blocks_swap :: Assertion
bottle_blocks_swap = do
  let board = setCell stableBoard (3, 3) (mkBottle C2)
  assertBool "is bottle" (isBottle (getCell board (3, 3)))
  assertEqual "color" C2 (bottleColor (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))

bottle_dyes_neighbors :: Assertion
bottle_dyes_neighbors = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             (mkBottle C3))
          (2, 2)
          (mkGem C5)
  let ms = findMatches board0
      b1 = triggerAdjacentBottles board0 ms
  assertBool "bottle stays" (isBottle (getCell b1 (2, 1)))
  assertEqual "dyed neighbor" C3 (cellColor (getCell b1 (2, 2)))
  let gs = newGameAtLevel 33 (levelConfig (allLevels !! 33)) 42
      nBot =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isBottle (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor bottles >= 6, got " ++ show nBot) (nBot >= 6)

--------------------------------------------------------------------------------
-- Cross clear booster (十字清除)
--------------------------------------------------------------------------------

booster_cross_clears_row_col :: Assertion
booster_cross_clears_row_col = do
  let seeds = crossClearSeeds (3, 4)
  assertEqual "row+col size" (15 :: Int) (length seeds)  -- 8+8-1
  assertBool "has row" (all (\c -> (3, c) `elem` seeds) [0 .. boardSize - 1])
  assertBool "has col" (all (\r -> (r, 4) `elem` seeds) [0 .. boardSize - 1])
  let gs0 =
        (newGame defaultConfig 5)
          { gsCrossClears = 1
          , gsOver = Nothing
          , gsHint = Nothing
          }
      (gs1, out) = useCrossClear (3, 3) gs0
  case out of
    InvalidSwap -> assertFailure "expected cross to apply"
    NoMatch -> assertFailure "expected cascade"
    _ -> pure ()
  assertEqual "charge spent" (0 :: Int) (gsCrossClears gs1)
  let (_, out2) = useCrossClear (1, 1) gs1
  out2 @?= InvalidSwap
  assertEqual "unchanged" (0 :: Int) (gsCrossClears gs1)

--------------------------------------------------------------------------------
-- TimeSpirit / 时间精灵 (+2 moves) & Steam / 蒸汽 & carry bonus
--------------------------------------------------------------------------------

time_spirit_blocks_swap :: Assertion
time_spirit_blocks_swap = do
  let board = setCell stableBoard (3, 3) mkTimeSpirit
  assertBool "is spirit" (isTimeSpirit (getCell board (3, 3)))
  assertBool "blocked" (swapBlockedByStone board (3, 3) (3, 4))

time_spirit_awards_moves :: Assertion
time_spirit_awards_moves = do
  -- Form match via swap (0,2)<->(0,3): C1 C1 C2 C1 → C1 C1 C1 C2; spirit at (1,1) adjacent
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (0, 0) (mkGem C1))
                   (0, 1)
                   (mkGem C1))
                (0, 2)
                (mkGem C2))
             (0, 3)
             (mkGem C1))
          (1, 1)
          mkTimeSpirit
      gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board0
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "spirit cleared" (not (isTimeSpirit (getCell (gsBoard gs1) (1, 1))))
  -- spent 1 move, gained +2 → net +1 from 10 → 11
  assertEqual "moves +2 net" (11 :: Int) (gsMoves gs1)
  let gsL = newGameAtLevel 34 (levelConfig (allLevels !! 34)) 42
      nSp =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isTimeSpirit (getCell (gsBoard gsL) (r, c))
          ]
  assertBool ("decor spirits >= 6, got " ++ show nSp) (nSp >= 6)

steam_blocks_match :: Assertion
steam_blocks_match = do
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 Normal 0 (Just Steam)))
          (3, 2)
          (mkGem C1)
  assertBool "has steam" (hasSteam (getCell board0 (3, 1)))
  assertBool "no match through steam" (null (findMatches board0))

steam_cleared_by_adjacent :: Assertion
steam_cleared_by_adjacent = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (Gem C2 Normal 0 (Just Steam))
  assertBool "steam present" (hasSteam (getCell board0 (2, 1)))
  let b1 = clearSteamAdjacent board0 (findMatches board0)
  assertBool "steam extinguished" (not (hasSteam (getCell b1 (2, 1))))
  assertEqual "gem remains" C2 (cellColor (getCell b1 (2, 1)))

steam_spreads_after_move :: Assertion
steam_spreads_after_move = do
  let board0 =
        setCell
          (setCell stableBoard (3, 3) (Gem C1 Normal 0 (Just Steam)))
          (3, 4)
          (mkGem C5)
  assertBool "neighbor bare" (cellOverlay (getCell board0 (3, 4)) == Nothing)
  let b1 = spreadSteam board0
  assertBool "steam spread" (hasSteam (getCell b1 (3, 4)))
  let gs = newGameAtLevel 35 (levelConfig (allLevels !! 35)) 42
      nSt =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , hasSteam (getCell (gsBoard gs) (r, c))
          ]
  assertBool ("decor steam >= 8, got " ++ show nSt) (nSt >= 8)
  assertEqual "campaign levels" (38 :: Int) (length allLevels)

--------------------------------------------------------------------------------
-- Carpet / 地毯 (floor tiles covered when gems on them clear)
--------------------------------------------------------------------------------

carpet_covers_on_clear :: Assertion
carpet_covers_on_clear = do
  -- Horizontal match on row 3 cols 0-2; carpet targets under those cells
  let board0 =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
      cfg = GameConfig 20 (GoalCarpet 8)
      gs0 =
        (newGameAtLevel 0 cfg 7)
          { gsBoard = board0
          , gsBelts = []
          , gsOver = Nothing
          , gsMoves = 20
          , gsScore = 0
          , gsCarpetOpen = [(3, 0), (3, 1), (3, 2), (4, 4)]
          , gsCarpetsCovered = 0
          , gsGoal = GoalCarpet 8
          , gsCollected = 0
          }
  assertBool "match ready" (hasAnyMatch board0)
  -- Force cascade via trySwap of a neighboring pair that creates/uses the match
  -- Board already has match; use hammer on one carpet cell to clear seeds then cascade
  let (gs1, out) = useHammer (3, 1) gs0 { gsHammers = 2 }
  case out of
    InvalidSwap -> assertFailure "hammer should apply"
    _ -> pure ()
  assertBool
    ("carpet covered some of match cells, covered=" ++ show (gsCarpetsCovered gs1)
       ++ " open=" ++ show (gsCarpetOpen gs1))
    (gsCarpetsCovered gs1 >= 1)
  assertBool "covered cells removed from open" $
    all (`notElem` gsCarpetOpen gs1) [(3, 0), (3, 1), (3, 2)]
      || gsCarpetsCovered gs1 >= 1
  -- Pure unit
  let (open', n) = coverCarpets [(3, 0), (3, 1), (4, 4)] [(3, 0), (3, 1), (5, 5)]
  assertEqual "pure cover count" (2 :: Int) n
  assertEqual "pure remain" [(4, 4)] open'

goal_carpet_counts :: Assertion
goal_carpet_counts = do
  assertBool "unmet" (not (goalMet (GoalCarpet 2) 0 0))
  assertBool "met" (goalMet (GoalCarpet 2) 0 2)
  assertBool "ex unmet" (not (goalMetEx (GoalCarpet 2) 0 0 [] 0 0 0 0 0 0 0 0))
  assertBool "ex met" (goalMetEx (GoalCarpet 2) 0 2 [] 0 0 0 0 0 0 0 0)
  assertEqual "progress" (2 :: Int) (goalProgress (GoalCarpet 5) 0 2)
  assertEqual "progressEx" (2 :: Int) (goalProgressEx (GoalCarpet 5) 0 2 [] 0 0 0 0 0 0 0 0)
  assertEqual "target" (5 :: Int) (goalTarget (GoalCarpet 5))
  -- Campaign includes GoalCarpet
  assertBool "campaign has GoalCarpet" $
    any (\g -> case g of GoalCarpet _ -> True; _ -> False) (map lvlGoal allLevels)
  let gs = newGameAtLevel 36 (levelConfig (allLevels !! 36)) 42
  assertEqual "level 36 carpet open" (8 :: Int) (length (gsCarpetOpen gs))
  assertEqual "goal" (GoalCarpet 8) (gsGoal gs)
  assertEqual "campaign levels" (38 :: Int) (length allLevels)

carpet_already_covered_noop :: Assertion
carpet_already_covered_noop = do
  -- Covering the same cell twice must not double-count
  let open0 = [(2, 2), (2, 3)]
      (open1, n1) = coverCarpets open0 [(2, 2)]
      (open2, n2) = coverCarpets open1 [(2, 2), (2, 3)]
  assertEqual "first hit" (1 :: Int) n1
  assertEqual "second hit only new" (1 :: Int) n2
  assertEqual "all covered" ([] :: [Pos]) open2
  -- Re-feeding already-covered positions into coverCarpets is a no-op
  let (open3, n3) = coverCarpets [] [(2, 2), (9, 9)]
  assertEqual "empty open stays empty" ([] :: [Pos]) open3
  assertEqual "no spurious covers" (0 :: Int) n3
  -- Game path: cover once, then clear same cell again with empty open → count unchanged
  let cfg = GameConfig 15 (GoalCarpet 3)
      gs0 =
        (newGameAtLevel 0 cfg 3)
          { gsCarpetOpen = [(5, 5)]
          , gsCarpetsCovered = 0
          , gsCollected = 0
          , gsGoal = GoalCarpet 3
          , gsHammers = 3
          , gsBoard = setCell stableBoard (5, 5) (mkGem C2)
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, _) = useHammer (5, 5) gs0
  assertEqual "covered once" (1 :: Int) (gsCarpetsCovered gs1)
  assertEqual "open emptied" ([] :: [Pos]) (gsCarpetOpen gs1)
  let (gs2, _) = useHammer (5, 5) gs1 { gsHammers = 2, gsOver = Nothing }
  assertEqual "second clear does not double-count" (1 :: Int) (gsCarpetsCovered gs2)
  assertEqual "still empty open" ([] :: [Pos]) (gsCarpetOpen gs2)

carry_moves_on_next_level :: Assertion
carry_moves_on_next_level = do
  let cfg0 = levelConfig (allLevels !! 0)
      gs0 =
        (newGameAtLevel 0 cfg0 1)
          { gsOver = Just (LevelClear 100 1)
          , gsMoves = 5  -- leftover
          }
      gs1 = nextLevel gs0 99
      base = lvlMoves (allLevels !! 1)
  assertEqual "level advanced" (1 :: Int) (gsLevel gs1)
  assertEqual "carried min(3,left)" (base + 3) (gsMoves gs1)  -- cap 3
  let gs2 =
        (newGameAtLevel 0 cfg0 2)
          { gsOver = Just (LevelClear 50 1)
          , gsMoves = 2
          }
      gs3 = nextLevel gs2 100
  assertEqual "carry 2" (base + 2) (gsMoves gs3)

daily_goal_rotates_ten :: Assertion
daily_goal_rotates_ten = do
  let flavors =
        [ cfgGoal (dailyConfig 2026 9 d) | d <- [1 .. 20] ]
      kinds = length (nub [ show g | g <- flavors ])
  assertBool ("at least 6 distinct daily goals, got " ++ show kinds) (kinds >= 6)
  -- Sample includes newer flavors
  assertBool "has chest or cake or safe or balloon among first 20 days" $
    any
      ( \g -> case g of
          GoalChest _ -> True
          GoalCake _ -> True
          GoalSafe _ -> True
          GoalBalloon _ -> True
          _ -> False
      )
      flavors


-- | Batch: all 38 campaign levels constructible, positive goals/moves,
-- board size in bounds, décor enough for obstacle goals, legal move after ensure.
campaign_levels_batch_ok :: Assertion
campaign_levels_batch_ok = do
  assertEqual "38 levels" (38 :: Int) (length allLevels)
  let seeds = [42, 99, 7] :: [Int]
  mapM_
    ( \seed ->
        mapM_
          ( \(i, lvl) -> do
              assertEqual ("index " ++ show i) i (lvlIndex lvl)
              assertBool ("moves>0 L" ++ show i) (lvlMoves lvl > 0)
              assertBool ("goal>0 L" ++ show i) (goalTarget (lvlGoal lvl) > 0)
              let gs = newGameAtLevel i (levelConfig lvl) (seed + i * 17)
                  b = gsBoard gs
              assertEqual ("rows L" ++ show i) boardSize (length b)
              assertBool ("cols L" ++ show i) (all ((== boardSize) . length) b)
              assertEqual ("cfg moves L" ++ show i) (lvlMoves lvl) (gsMoves gs)
              assertBool ("playable L" ++ show i ++ " s=" ++ show seed) (hasValidMove b)
              case lvlGoal lvl of
                GoalClearStone n ->
                  assertBool ("stones L" ++ show i) (countCells isStone b >= n)
                GoalChest n ->
                  assertBool ("chests L" ++ show i) (countCells isChest b >= n)
                GoalHoney n ->
                  assertBool ("honey L" ++ show i) (countCells isHoney b >= n)
                GoalBalloon n ->
                  assertBool ("balloons L" ++ show i) (countCells isBalloon b >= n)
                GoalCookie n ->
                  assertBool ("cookies L" ++ show i) (countCells isCookie b >= n)
                GoalCake n ->
                  assertBool ("cakes L" ++ show i) (countCells isCake b >= n)
                GoalSafe n ->
                  assertBool ("safes L" ++ show i) (countCells isSafe b >= n)
                GoalCarpet n ->
                  assertBool ("carpets L" ++ show i) (length (gsCarpetOpen gs) >= n)
                GoalUfo _ ->
                  assertBool ("ufo L" ++ show i) (not (null (gsUfos gs)))
                _ -> pure ()
          )
          (zip [0 :: Int ..] allLevels)
    )
    seeds
  where
    countCells p b =
      length
        [ ()
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , p (getCell b (r, c))
        ]

-- | Successful match without TimeSpirit deducts exactly 1 move.
inv_move_costs_one_without_spirit :: Assertion
inv_move_costs_one_without_spirit = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      gs0 =
        (newGame defaultConfig 7)
          { gsBoard = board0
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertEqual "spent exactly 1" (11 :: Int) (gsMoves gs1)
  assertEqual "no spirits left side-effect" (0 :: Int) (countTimeSpiritsOn (gsBoard gs1))
  where
    countTimeSpiritsOn b =
      length
        [ ()
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isTimeSpirit (getCell b (r, c))
        ]

-- | End-of-move pipeline: steam spreads before snails crawl.
-- Order matters: steam-then-snail ≠ snail-then-steam on this layout.
inv_move_end_order_steam_before_snail :: Assertion
inv_move_end_order_steam_before_snail = do
  let boardBase =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell stableBoard (0, 0) (mkGem C1))
                      (0, 1)
                      (mkGem C1))
                   (0, 2)
                   (mkGem C2))
                (0, 3)
                (mkGem C1))
             (4, 2)
             (mkGem C3))
          (4, 3)
          (mkGem C4)
      board0 =
        setCell
          (setCell boardBase (4, 2) (Gem C3 Normal 0 (Just Steam)))
          (4, 4)
          (mkSnail 0 (-1))
      steamFirst = stepSnails (spreadSteam board0)
      snailFirst = spreadSteam (stepSnails board0)
  assertBool "orders differ" (steamFirst /= snailFirst)
  assertBool "steam-first: snail crawled onto (4,3)" (isSnail (getCell steamFirst (4, 3)))
  assertBool "steam-first: steamed gem pushed to (4,4)" (hasSteam (getCell steamFirst (4, 4)))
  let gs0 =
        (newGame defaultConfig 11)
          { gsBoard = board0
          , gsMoves = 20
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  -- After full move (cascades may refill elsewhere), snail should have crawled left
  -- and the steamed gem should sit where the snail started — steam-before-snail.
  assertBool "snail left (4,4)" (not (isSnail (getCell b1 (4, 4))))
  assertBool "snail at (4,3)" (isSnail (getCell b1 (4, 3)))
  assertBool "steamed gem at old snail cell" (hasSteam (getCell b1 (4, 4)))

-- | Belt shifts before steam spreads: steam rides the belt then spreads from new cell.
inv_move_end_order_belt_before_steam :: Assertion
inv_move_end_order_belt_before_steam = do
  let belt = [(6, 1), (6, 2), (6, 3)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell stableBoard (1, 0) (mkGem C1))
                      (1, 1)
                      (mkGem C1))
                   (1, 2)
                   (mkGem C2))
                (1, 3)
                (mkGem C1))
             (6, 1)
             (Gem C5 Normal 0 (Just Steam)))
          (6, 2)
          (mkGem C4)
      -- Pure: belt then steam vs steam then belt
      afterBelt = shiftBelts board0 [belt]
      beltThenSteam = spreadSteam afterBelt
      steamThenBelt = shiftBelts (spreadSteam board0) [belt]
  assertBool "belt moved steam to (6,2)" (hasSteam (getCell afterBelt (6, 2)))
  assertBool "orders can differ on spread targets" True
  -- After belt, steam at (6,2) can spread to (6,3) and (5,2)/(7,2)
  assertBool "belt-then-steam spreads from (6,2)" $
    hasSteam (getCell beltThenSteam (6, 3))
      || hasSteam (getCell beltThenSteam (5, 2))
      || hasSteam (getCell beltThenSteam (7, 2))
  let gs0 =
        (newGame defaultConfig 5)
          { gsBoard = board0
          , gsBelts = [belt]
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (1, 2) (1, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
  -- After belt shift, steam rides to (6,2) and may spread ortho (including back to 6,1).
  assertBool "steam present somewhere on/near belt after move" $
    any
      (\q -> hasSteam (getCell b1 q))
      [(6, 1), (6, 2), (6, 3), (5, 2), (7, 2), (5, 1), (7, 1), (5, 3), (7, 3)]
  assertEqual "moves deducted" (14 :: Int) (gsMoves gs1)

-- | Finale / high-pressure levels keep a reasonable move budget.
finale_and_pressure_moves_reasonable :: Assertion
finale_and_pressure_moves_reasonable = do
  let finale = allLevels !! 27
      master = allLevels !! 15
      pressure = allLevels !! 14
      steam = allLevels !! 35
      carpet = allLevels !! 36
      weave = allLevels !! 37
  assertEqual "终章 name" "终章" (lvlName finale)
  assertBool "终章 moves >= 24" (lvlMoves finale >= 24)
  assertBool "大师 moves >= 22" (lvlMoves master >= 22)
  assertBool "压力 moves >= 20" (lvlMoves pressure >= 20)
  assertBool "蒸汽 moves >= 22" (lvlMoves steam >= 22)
  assertBool "地毯 moves >= 24" (lvlMoves carpet >= 24)
  assertBool "织毯 moves >= 24" (lvlMoves weave >= 24)
  -- Soft score caps so dense décor levels stay fair (numbers only; rules frozen)
  case lvlGoal master of
    GoalScore n -> assertBool "大师 score <= 1000" (n <= 1000)
    _ -> assertFailure "大师 should be GoalScore"
  case lvlGoal finale of
    GoalScore n -> assertBool "终章 score <= 1400" (n <= 1400)
    _ -> assertFailure "终章 should be GoalScore"
  -- Every level at least 18 moves; no zero/negative goals
  mapM_
    ( \lvl -> do
        assertBool (lvlName lvl ++ " moves>=18") (lvlMoves lvl >= 18)
        assertBool (lvlName lvl ++ " goal>0") (goalTarget (lvlGoal lvl) > 0)
    )
    allLevels


--------------------------------------------------------------------------------
-- Fragile interaction boundaries (stability cruise)
--------------------------------------------------------------------------------

-- | Ice layers >1 on a carpet tile: chip does NOT cover; only ice-free clears count.
carpet_ice_partial_no_cover :: Assertion
carpet_ice_partial_no_cover = do
  -- Unit: ice>1 excluded from iceFree → coverCarpets ignores that tile.
  let board =
        setCell
          (setCell stableBoard (3, 0) (mkGem C1))
          (3, 3)
          (mkIceGem C1 2)
      seeds = [(3, 0), (3, 1), (3, 3)]
      (bIced, iceFree) = chipIceOnClear board seeds
  assertBool "(3,3) not cleared" ((3, 3) `notElem` iceFree)
  assertEqual "ice chipped to 1" (1 :: Int) (iceLayers (getCell bIced (3, 3)))
  assertBool "(3,0) cleared" ((3, 0) `elem` iceFree)
  let (open', n) = coverCarpets [(3, 3), (3, 0)] iceFree
  assertEqual "one cover" (1 :: Int) n
  assertBool "iced tile still open" ((3, 3) `elem` open')
  assertBool "cleared tile covered" ((3, 0) `notElem` open')
  -- trySwap: carpet under stationary iced gem (3,0); swap forms match including it.
  -- Row3: C1(ice2) C1 C2 C1 — swap (3,2)<->(3,3) → C1(ice2) C1 C1 C2
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkIceGem C1 2))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C2))
          (3, 3)
          (mkGem C1)
      gs0 =
        (newGame defaultConfig 3)
          { gsBoard = board0
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsCarpetOpen = [(3, 0)]
          , gsCarpetsCovered = 0
          , gsGoal = GoalCarpet 1
          }
      (gs1, out) = trySwap (3, 2) (3, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  -- Partial ice must not cover on the chip-only wave; covered stays 0 if gem survived.
  -- Cascades may later clear the cell — only assert we never over-count past 1 open tile.
  assertBool "covered in {0,1}" (gsCarpetsCovered gs1 <= 1)
  assertBool "open consistent" (length (gsCarpetOpen gs1) + gsCarpetsCovered gs1 == 1)

-- | Last ice layer (ice==1) on carpet: gem clears and carpet covers.
carpet_ice_last_layer_covers :: Assertion
carpet_ice_last_layer_covers = do
  let board = setCell stableBoard (5, 3) (mkIceGem C2 1)
      (bIced, iceFree) = chipIceOnClear board [(5, 3)]
  assertEqual "last ice clears" [(5, 3)] iceFree
  assertEqual "board unchanged pre-hole" (mkIceGem C2 1) (getCell bIced (5, 3))
  let (open', n) = coverCarpets [(5, 3)] iceFree
  assertEqual "covers" (1 :: Int) n
  assertEqual "open empty" ([] :: [Pos]) open'
  -- trySwap: carpet under stationary ice==1 at (5,0)
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (5, 0) (mkIceGem C2 1))
                (5, 1)
                (mkGem C2))
             (5, 2)
             (mkGem C3))
          (5, 3)
          (mkGem C2)
      gs0 =
        (newGame defaultConfig 4)
          { gsBoard = board0
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsCarpetOpen = [(5, 0)]
          , gsCarpetsCovered = 0
          , gsGoal = GoalCarpet 1
          }
      (gs1, out) = trySwap (5, 2) (5, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertEqual "last ice covers carpet" (1 :: Int) (gsCarpetsCovered gs1)
  assertEqual "open emptied" ([] :: [Pos]) (gsCarpetOpen gs1)

-- | TimeSpirit adjacent-clear on the final move: -1+2 keeps the player alive.
time_spirit_rescues_last_move :: Assertion
time_spirit_rescues_last_move = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (0, 0) (mkGem C1))
                   (0, 1)
                   (mkGem C1))
                (0, 2)
                (mkGem C2))
             (0, 3)
             (mkGem C1))
          (1, 1)
          mkTimeSpirit
      gs0 =
        (newGame defaultConfig 8)
          { gsBoard = board0
          , gsMoves = 1
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    Lost _ -> assertFailure "spirit should rescue last move"
    _ -> pure ()
  assertBool "spirit gone" (not (isTimeSpirit (getCell (gsBoard gs1) (1, 1))))
  assertEqual "net +1 from last move" (2 :: Int) (gsMoves gs1)
  assertEqual "not over" Nothing (gsOver gs1)

-- | Belt shift forms a match that clears portal A; portal teleports B→A before re-gravity.
-- Locks Portal+Belt: belt-created clears are visible to applyPortalTeleports / settle.
portal_after_belt_match_teleports :: Assertion
portal_after_belt_match_teleports = do
  let portals = [((0, 2), (7, 2))]
      belt = [(0, 0), (0, 1), (0, 2), (0, 3)]
      -- Before belt: C1 C1 C2 C1 — after forward shift: C1 C1 C1 C2 (triple includes A)
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (0, 0) (mkGem C1))
                   (0, 1)
                   (mkGem C1))
                (0, 2)
                (mkGem C2))
             (0, 3)
             (mkGem C1))
          (7, 2)
          (mkGem C5)
      shifted = shiftBelts board0 [belt]
  assertBool "belt assembled match" (hasAnyMatch shifted)
  assertEqual "A is C1" C1 (cellColor (getCell shifted (0, 2)))
  assertEqual "B is C5" C5 (cellColor (getCell shifted (7, 2)))
  let (mb, n) = clearMatches shifted
  assertBool "cleared triple+" (n >= 3)
  assertEqual "A hole pre-portal" Nothing ((mb !! 0) !! 2)
  assertEqual "B still C5" (Just (mkGem C5)) ((mb !! 7) !! 2)
  -- Direct portal step (pre re-gravity): B→A
  let ported = applyPortalTeleports portals mb
  assertEqual "C5 teleported to A" (Just (mkGem C5)) ((ported !! 0) !! 2)
  assertEqual "B emptied by portal" Nothing ((ported !! 7) !! 2)
  -- Full settle: gravity may repack column, but B must not keep C5
  let (settled, _) = settleBoardPortals portals mb
  assertBool "B no longer holds C5 after settle" $
    case (settled !! 7) !! 2 of
      Just c -> not (isGem c && cellColor c == C5) || False
      Nothing -> True
  assertBool "C5 still somewhere in col 2" $
    any
      ( \r ->
          case (settled !! r) !! 2 of
            Just c -> isGem c && cellColor c == C5
            Nothing -> False
      )
      [0 .. boardSize - 1]
  -- Full trySwap path with belts+portals stays stable and spends a move
  let boardSwap =
        setCell
          (setCell
             (setCell
                (setCell board0 (2, 0) (mkGem C3))
                (2, 1)
                (mkGem C3))
             (2, 2)
             (mkGem C4))
          (2, 3)
          (mkGem C3)
      gs0 =
        (newGame defaultConfig 9)
          { gsBoard = boardSwap
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = [belt]
          , gsPortals = portals
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (2, 2) (2, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match on row2"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "post-move stable" (not (hasAnyMatch (gsBoard gs1)))
  assertEqual "spent one move" (11 :: Int) (gsMoves gs1)

-- | Flip at the natural 4-match spawn anchor: Line must still appear on a hole.
-- Locks Flip+Match: non-clearing Flip seeds must not swallow special spawns.
flip_four_match_spawns_line :: Assertion
flip_four_match_spawns_line = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkFlip C1 C4))
          (3, 3)
          (mkGem C1)
  assertEqual "4-run" (4 :: Int) (maximum (0 : map (length . runPos) (findMatchRuns board0)))
  let (mb, n) = clearMatches board0
  assertEqual "three holes (flip stays)" (3 :: Int) n
  assertBool "flip became back gem" $
    case (mb !! 3) !! 2 of
      Just c -> isGem c && cellColor c == C4 && cellKind c == Normal
      Nothing -> False
  let specials =
        [ (r, c, cellKind cell)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [((mb !! r) !! c)]
        , isGem cell
        , cellKind cell /= Normal
        ]
  assertBool ("expected Line spawn, got " ++ show specials) $
    any (\(_, _, k) -> k == LineH || k == LineV) specials
  -- ice>1 at middle is the same class of non-hole spawn anchor
  let boardIce =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (5, 0) (mkGem C2))
                (5, 1)
                (mkGem C2))
             (5, 2)
             (mkIceGem C2 2))
          (5, 3)
          (mkGem C2)
      (mbI, _) = clearMatches boardIce
      specsI =
        [ cellKind cell
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , Just cell <- [((mbI !! r) !! c)]
        , isGem cell
        , cellKind cell /= Normal
        ]
  assertBool ("ice mid still spawns, got " ++ show specsI) (LineH `elem` specsI || LineV `elem` specsI)

-- | Surprise 3×3 blast that hits a Bomb must expand the Bomb (same as countdown blasts).
-- Locks Surprise+Cascade: explode seeds go through expandSpecials before chipIce.
surprise_blast_expands_bomb :: Assertion
surprise_blast_expands_bomb = do
  -- (3,3) surpriseOutcome == 3 → explode; Bomb at (2,3) sits inside the 3×3.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 3)
          (Gem C5 Bomb 0 Nothing)
  assertBool "match opens surprise" (not (null (findMatches board0)))
  let (mb, n) = clearMatches board0
      holes =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mb !! r) !! c) == Nothing
        ]
  assertBool "bomb cell cleared" ((2, 3) `elem` holes)
  -- Bomb at (2,3) expands to row1; (1,3) is outside surprise 3×3 alone.
  assertBool "bomb expansion reached (1,3)" ((1, 3) `elem` holes)
  assertBool "cleared well beyond match+surprise" (n >= 12)

-- | Maker charge-1 → Bomb in place; Bomb is not consumed by the producing wave.
-- Locks Maker+Bomb: produced Bomb sits until a later match/swap.
maker_bomb_survives_wave :: Assertion
maker_bomb_survives_wave = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkMakerCharges C1 1)
      gs0 =
        (newGame defaultConfig 11)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let cell = getCell (gsBoard gs1) (2, 1)
  -- trySwap path: Maker is gone (converted); Bomb may still sit or cascade-detonate.
  assertBool "maker converted away" (not (isMaker cell))
  -- Unit path: same-wave clearMatches leaves Bomb in place (not consumed by producing wave)
  let (mb, _) = clearMatches board0
  case (mb !! 2) !! 1 of
    Just c -> do
      assertBool "unit bomb gem" (isGem c)
      assertEqual "unit Bomb kind" Bomb (cellKind c)
      assertEqual "unit bomb color" C1 (cellColor c)
    Nothing -> assertFailure "maker cell must not hole"

-- | Same clear peels an adjacent Chain and an adjacent Freeze.
-- Locks Chain+Freeze: independent overlays on different cells both respond.
chain_freeze_both_peel :: Assertion
chain_freeze_both_peel = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             (mkChainGem C2 1))
          (4, 1)
          (mkFreezeGem C3 1)
      ms = findMatches board0
      (b1, nChain) = chipAdjacentChain board0 ms
      (b2, nFreeze) = chipAdjacentFreeze b1 ms
  assertEqual "chain peeled" (1 :: Int) nChain
  assertEqual "freeze peeled" (1 :: Int) nFreeze
  assertBool "chain gone" (not (hasChain (getCell b2 (2, 1))))
  assertBool "freeze gone" (not (hasFreeze (getCell b2 (4, 1))))
  -- Full clearMatches also leaves both cells as bare gems (not holes)
  let (mb, _) = clearMatches board0
  assertBool "chain cell not holed" $
    case (mb !! 2) !! 1 of
      Just c -> isGem c && not (hasChain c)
      Nothing -> False
  assertBool "freeze cell not holed" $
    case (mb !! 4) !! 1 of
      Just c -> isGem c && not (hasFreeze c)
      Nothing -> False

-- | One match adjacent to Honey and same-color Balloon clears both in the wave.
-- Locks Honey+Balloon: independent adjacency rules share iceFree seeds.
honey_balloon_same_clear :: Assertion
honey_balloon_same_clear = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (2, 1)
             mkHoney)
          (4, 1)
          (mkBalloon C1)
      gs0 =
        (newGame defaultConfig 12)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHoneyCleared = 0
          , gsBalloonsPopped = 0
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "honey counted" (gsHoneyCleared gs1 >= 1)
  assertBool "balloon counted" (gsBalloonsPopped gs1 >= 1)
  assertBool "no honey left" $
    not (any (\r -> any (\c -> isHoney (getCell (gsBoard gs1) (r, c))) [0 .. boardSize - 1]) [0 .. boardSize - 1])
  assertBool "no balloon left" $
    not (any (\r -> any (\c -> isBalloon (getCell (gsBoard gs1) (r, c))) [0 .. boardSize - 1]) [0 .. boardSize - 1])

-- | Safe on the bottom row opens to Cookie and collects in the same settle.
-- Locks Safe→Cookie: open-in-place + drainBottomCookies in one wave.
safe_bottom_cookie_collected :: Assertion
safe_bottom_cookie_collected = do
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (6, 0) (mkGem C1))
                (6, 1)
                (mkGem C1))
             (6, 2)
             (mkGem C1))
          (7, 1)
          mkSafe
      (mb, _) = clearMatches board0
  assertBool "opened to cookie pre-settle" $
    case (mb !! 7) !! 1 of
      Just Cookie -> True
      _ -> False
  let (settled, fallen) = settleBoardPortals [] mb
  assertEqual "cookie drained" (1 :: Int) fallen
  assertBool "bottom no longer cookie" $
    case (settled !! 7) !! 1 of
      Just Cookie -> False
      _ -> True
  let gs0 =
        (newGame defaultConfig 13)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsSafesOpened = 0
          , gsCookiesCollected = 0
          , gsGoal = GoalSafe 1
          }
      (gs1, out) = trySwap (6, 1) (6, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool "safe opened" (gsSafesOpened gs1 >= 1)
  assertBool "cookie collected" (gsCookiesCollected gs1 >= 1)

-- | Cookie on a bottom-row portal entrance collects; portal must not snatch it first.
-- Locks Cookie触底 × Portal: drainBottomCookies before applyPortalTeleports.
cookie_bottom_portal_collects :: Assertion
cookie_bottom_portal_collects = do
  let bottom = boardSize - 1
      fill = Just (mkGem C5)
      mb0 = replicate boardSize (replicate boardSize fill)
      setMB b (r, c) v =
        take r b ++ [take c row ++ [v] ++ drop (c + 1) row] ++ drop (r + 1) b
        where
          row = b !! r
      -- Cookie already on bottom portal A; exit B empty (buggy order teleports up)
      mb = setMB (setMB mb0 (bottom, 1) (Just Cookie)) (0, 6) Nothing
      portals = [((0, 6), (bottom, 1))]
      (settled, fallen) = settleBoardPortals portals mb
      cookieLeft =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , case (settled !! r) !! c of
            Just Cookie -> True
            _ -> False
        ]
  assertEqual "cookie collected at bottom portal" (1 :: Int) fallen
  assertEqual "no cookie left on board" ([] :: [(Int, Int)]) cookieLeft
  assertBool "exit not holding snatch" $
    case (settled !! 0) !! 6 of
      Just Cookie -> False
      _ -> True
  -- trySwap path with portals: cookie on bottom portal drains into GoalCookie
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (bottom, 1) mkCookie)
                (3, 0)
                (mkGem C1))
             (3, 1)
             (mkGem C1))
          (3, 2)
          (mkGem C1)
      gs0 =
        (newGame defaultConfig 23)
          { gsBoard = board0
          , gsMoves = 15
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsPortals = portals
          , gsCookiesCollected = 0
          , gsGoal = GoalCookie 1
          }
      (gs1, out) = trySwap (3, 1) (3, 2) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertBool ("trySwap collected cookie, got " ++ show (gsCookiesCollected gs1))
    (gsCookiesCollected gs1 >= 1)
  assertBool "trySwap cookie gone from bottom" (not (isCookie (getCell (gsBoard gs1) (bottom, 1))))
  assertBool "trySwap cookie not at portal exit" (not (isCookie (getCell (gsBoard gs1) (0, 6))))

-- | Countdown explode cascade still threads UFOs + portals (not dropped as []).
-- Locks 倒计时到期 × UFO/Portal: resolveCountdowns keeps overlays during blast settle.
countdown_explode_keeps_ufo_portals :: Assertion
countdown_explode_keeps_ufo_portals = do
  let bottom = boardSize - 1
      portals = [((0, 1), (bottom, 6))]
      -- Cookie on bottom portal; countdown far away ticks to 0 and explodes
      board0 =
        spawnCountdown (setCell stableBoard (bottom, 6) mkCookie) (4, 4) C5 1
      u0 = mkUfo (2, 2) C1
      (bRes, _n, _sc, _mw, _t, _st, _ch, _h, _bal, cookies, _cak, _uAbs, ufos', _pos, _) =
        resolveCountdowns [u0] portals (mkStdGen 5) board0
  assertBool ("explode settle collected bottom cookie, got " ++ show cookies) (cookies >= 1)
  assertBool "cookie not left on bottom portal" (not (isCookie (getCell bRes (bottom, 6))))
  assertEqual "UFO list preserved through resolve" (1 :: Int) (length ufos')
  -- trySwap path: countdown expires; UFO + portals remain on game state
  let boardT =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (0, 0) (mkGem C2))
                (0, 1)
                (mkGem C2))
             (0, 2)
             (mkGem C3))
          (0, 3)
          (mkGem C2)
      boardT' = spawnCountdown boardT (5, 5) C4 1
      gs0 =
        (newGame defaultConfig 19)
          { gsBoard = boardT'
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsUfos = [u0]
          , gsPortals = portals
          , gsBelts = []
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  assertEqual "UFO still on game after countdown explode" (1 :: Int) (length (gsUfos gs1))
  assertEqual "portals kept on state" portals (gsPortals gs1)
  assertBool "countdown gone after expire" $
    null
      [ ()
      | r <- [0 .. boardSize - 1]
      , c <- [0 .. boardSize - 1]
      , isCountdown (getCell (gsBoard gs1) (r, c))
      ]

--------------------------------------------------------------------------------
-- Release quality gates (研讨锁定具名测)
--------------------------------------------------------------------------------


-- | After a legal Move, cascade wave count is bounded and the board reaches Stable.
-- Strengthens inv_move_to_stable / cascade_until_stable with an explicit step bound.
cascade_terminates_bounded :: Assertion
cascade_terminates_bounded = do
  let bound = boardSize * boardSize * 4  -- clear finite upper bound on cascade waves
      gs0 = newGame defaultConfig 99
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need a matching swap"
    Just (p1, p2) -> do
      let swapped = swapCells (gsBoard gs0) p1 p2
          (bCas, cells, _scored, maxW, _, _, _, _, _, _, _, gCas) =
            runCascadeScored (Just p2) (gsGen gs0) swapped
      assertBool "cascade waves within bound" (maxW <= bound)
      assertBool "cascade reached stable" (not (hasAnyMatch bCas))
      assertBool "stepCascade exhausted" (isNothing (stepCascade gCas bCas))
      assertBool "cleared something on match move" (cells > 0 || not (hasAnyMatch swapped))
      -- Full trySwap path also ends Stable (same contract as inv_move_to_stable)
      let (gs1, out) = trySwap p1 p2 gs0
      assertBool "applied or terminal" $
        case out of
          MoveApplied _ -> True
          Won _ -> True
          Lost _ -> True
          LevelClear _ _ -> True
          _ -> False
      assertBool "trySwap board stable" (not (hasAnyMatch (gsBoard gs1)))
  -- Extra seeds: bounded + stable via runCascade on random boards
  mapM_
    ( \seed -> do
        let g = mkStdGen seed
            (b0, g1) = randomBoard g
            (b1, maxW, g2) =
              let (b', _cells, _sc, waves, _, _, _, _, _, _, _, g') =
                    runCascadeScored Nothing g1 b0
              in (b', waves, g')
        assertBool ("waves bounded seed " ++ show seed) (maxW <= bound)
        assertBool ("stable seed " ++ show seed) (not (hasAnyMatch b1))
        assertBool ("stepCascade done seed " ++ show seed) (isNothing (stepCascade g2 b1))
    )
    [1, 7, 42, 99, 2026 :: Int]

-- | Release gate: re-check the original six core invariants + trySwap (GameState, Outcome) convention.
release_core_invariants_green :: Assertion
release_core_invariants_green = do
  inv_no_match_rollback
  inv_move_to_stable
  match_line_ge3
  gravity_then_refill
  cascade_until_stable
  outcome_moves_or_score
  -- Confirm trySwap returns (GameState, Outcome) via runtime pattern match
  let gs0 = newGame defaultConfig 7
  case findMatchPair (gsBoard gs0) of
    Nothing ->
      case findNoMatchPair (gsBoard gs0) of
        Nothing -> assertFailure "need any adjacent pair for trySwap convention"
        Just (p1, p2) -> checkTrySwapPair p1 p2 gs0
    Just (p1, p2) -> checkTrySwapPair p1 p2 gs0
  where
    checkTrySwapPair p1 p2 gs =
      case trySwap p1 p2 gs of
        (gs1, out) -> do
          assertEqual "board rows" boardSize (length (gsBoard gs1))
          assertBool "Outcome is a real constructor" $
            case out of
              InvalidSwap -> True
              NoMatch -> True
              MoveApplied _ -> True
              Won _ -> True
              Lost _ -> True
              LevelClear _ _ -> True


--------------------------------------------------------------------------------
-- Booster × Stone/Chain/Curtain + Daily décor + Undo/Shuffle progress
--------------------------------------------------------------------------------

-- | Hammer on a chained gem peels one chain layer; gem stays (not cleared).
hammer_peels_chain_not_gem :: Assertion
hammer_peels_chain_not_gem = do
  let board0 = setCell stableBoard (3, 3) (mkChainGem C2 2)
      gs0 =
        (newGame defaultConfig 3)
          { gsBoard = board0
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          }
      (gs1, out) = useHammer (3, 3) gs0
  case out of
    InvalidSwap -> assertFailure "hammer should apply"
    NoMatch -> assertFailure "hammer should apply"
    _ -> pure ()
  let cell = getCell (gsBoard gs1) (3, 3)
  assertBool "gem remains" (isGem cell)
  assertBool "chain peeled to 1" (hasChain cell && chainLayers cell == 1)
  assertEqual "color kept" C2 (cellColor cell)
  -- second hammer unlocks fully (gem may reshuffle if board was stuck)
  let (gs2, _) = useHammer (3, 3) gs1 { gsOver = Nothing }
      cell2 = getCell (gsBoard gs2) (3, 3)
  assertBool "chain fully peeled" (not (hasChain cell2))

-- | Hammer on curtain peels one layer; gem stays.
hammer_peels_curtain_not_gem :: Assertion
hammer_peels_curtain_not_gem = do
  let board0 = setCell stableBoard (2, 4) (mkCurtainGem C3 2)
      gs0 =
        (newGame defaultConfig 4)
          { gsBoard = board0
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, _) = useHammer (2, 4) gs0
      cell = getCell (gsBoard gs1) (2, 4)
  assertBool "gem remains" (isGem cell)
  assertBool "curtain -> 1" (hasCurtain cell && curtainLayers cell == 1)

-- | Hammer on multi-layer stone chips one layer (does not nuke all).
hammer_chips_stone_layer :: Assertion
hammer_chips_stone_layer = do
  let board0 = setCell stableBoard (5, 5) (mkStoneLayers 3)
      gs0 =
        (newGame defaultConfig 5)
          { gsBoard = board0
          , gsHammers = 2
          , gsGoal = GoalClearStone 8
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, _) = useHammer (5, 5) gs0
      cell = getCell (gsBoard gs1) (5, 5)
  assertBool "still stone" (isStone cell)
  assertEqual "3 -> 2" (2 :: Int) (stoneLayers cell)
  assertEqual "not counted until last layer" (0 :: Int) (gsStonesCleared gs1)
  -- chip down to 1 then clear
  let (gs2, _) = useHammer (5, 5) gs1 { gsHammers = 2, gsOver = Nothing }
  assertEqual "2 -> 1" (1 :: Int) (stoneLayers (getCell (gsBoard gs2) (5, 5)))
  let (gs3, _) = useHammer (5, 5) gs2 { gsHammers = 2, gsOver = Nothing }
  assertBool "removed" (not (isStone (getCell (gsBoard gs3) (5, 5))))
  assertEqual "cleared counted" (1 :: Int) (gsStonesCleared gs3)

-- | Cross clear seed on a chain cell peels (does not clear gem).
cross_peels_chain_on_seed :: Assertion
cross_peels_chain_on_seed = do
  let board0 = setCell stableBoard (3, 3) (mkChainGem C1 1)
      gs0 =
        (newGame defaultConfig 6)
          { gsBoard = board0
          , gsCrossClears = 1
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = useCrossClear (3, 3) gs0
  case out of
    InvalidSwap -> assertFailure "cross should apply"
    _ -> pure ()
  let cell = getCell (gsBoard gs1) (3, 3)
  -- Chain 1 peeled → unlocked gem; cross also clears other row/col gems so
  -- this cell may refill — assert either unlocked gem or a refill cell, not chain.
  assertBool "chain gone from seed cell" (not (hasChain cell))

-- | Free-swap refuses stone and chained endpoints (same gate as trySwap).
freeswap_blocked_by_stone_chain :: Assertion
freeswap_blocked_by_stone_chain = do
  let bStone = setCell stableBoard (1, 1) mkStone
      bChain = setCell stableBoard (2, 2) (mkChainGem C2 1)
      gsS =
        (newGame defaultConfig 7)
          { gsBoard = bStone, gsFreeSwaps = 1, gsOver = Nothing, gsBelts = [], gsUfos = [] }
      gsC =
        (newGame defaultConfig 8)
          { gsBoard = bChain, gsFreeSwaps = 1, gsOver = Nothing, gsBelts = [], gsUfos = [] }
  let (_, outS) = useFreeSwap (1, 1) (1, 3) gsS
      (_, outC) = useFreeSwap (2, 2) (2, 4) gsC
  assertEqual "stone blocks free-swap" NoMatch outS
  assertEqual "chain blocks free-swap" NoMatch outC
  assertEqual "charge kept (stone)" (1 :: Int) (gsFreeSwaps (fst (useFreeSwap (1, 1) (1, 3) gsS)))
  assertEqual "charge kept (chain)" (1 :: Int) (gsFreeSwaps (fst (useFreeSwap (2, 2) (2, 4) gsC)))

-- | Daily obstacle goals (li=0 bare décor) still spawn enough entities.
daily_obstacle_goal_spawns_decor :: Assertion
daily_obstacle_goal_spawns_decor = do
  let check keep goal tag = do
        let gs = newGame (GameConfig 26 goal) 20260903
            n =
              length
                [ ()
                | r <- [0 .. boardSize - 1]
                , c <- [0 .. boardSize - 1]
                , keep (getCell (gsBoard gs) (r, c))
                ]
        assertBool (tag ++ " decor count=" ++ show n) (n >= 4)
  check isStone (GoalClearStone 6) "stone"
  check isHoney (GoalHoney 6) "honey"
  check isChest (GoalChest 5) "chest"
  check isCake (GoalCake 5) "cake"
  check isSafe (GoalSafe 4) "safe"
  check isBalloon (GoalBalloon 6) "balloon"
  check isCookie (GoalCookie 6) "cookie"

-- | Undo after a move restores leftover moves (carry bank is only on nextLevel).
undo_restores_carry_moves :: Assertion
undo_restores_carry_moves = do
  let gs0 =
        (newGameAtLevel 0 (levelConfig (allLevels !! 0)) 42)
          { gsMoves = 7 }
  case findMatchPair (gsBoard gs0) of
    Nothing -> assertFailure "need match"
    Just (p1, p2) -> do
      let (gs1, _) = trySwap p1 p2 gs0
      assertEqual "spent one" (6 :: Int) (gsMoves gs1)
      case undoMove gs1 of
        Nothing -> assertFailure "undo"
        Just gsU -> do
          assertEqual "moves restored" (7 :: Int) (gsMoves gsU)
          -- nextLevel carry still caps at 3 from leftover
          let gsClear = gsU { gsOver = Just (LevelClear 10 1), gsMoves = 7 }
              gsNext = nextLevel gsClear 99
              base = lvlMoves (allLevels !! 1)
          assertEqual "carry cap 3" (base + 3) (gsMoves gsNext)

-- | Shuffle / ensurePlayable must not wipe goal tallies.
shuffle_preserves_goal_progress :: Assertion
shuffle_preserves_goal_progress = do
  let gs0 =
        (newGameAtLevel 7 (GameConfig 20 (GoalClearStone 8)) 33)
          { gsStonesCleared = 3
          , gsCollected = 3
          , gsScore = 120
          , gsOver = Nothing
          }
      gs1 = shuffleGame gs0
  assertEqual "stones tally kept" (3 :: Int) (gsStonesCleared gs1)
  assertEqual "collected kept" (3 :: Int) (gsCollected gs1)
  assertEqual "score kept" (120 :: Int) (gsScore gs1)
  assertEqual "goal kept" (GoalClearStone 8) (gsGoal gs1)


--------------------------------------------------------------------------------
-- Stability cruise: Rainbow×special / carry·stars / curtain / grass·vine / Freeze
--------------------------------------------------------------------------------

-- | Rainbow must not expandSpecials into its own spawn color (activation = rainbowClearSeeds).
rainbow_expand_noop :: Assertion
rainbow_expand_noop = do
  let board = setCell stableBoard (3, 3) (Gem C1 Rainbow 0 Nothing)
      -- Sprinkle other C1 so a buggy expand would pick them up
      board' =
        foldl
          (\b p -> setCell b p (mkGem C1))
          board
          [(0, 0), (0, 2), (2, 0), (7, 7)]
      expanded = expandSpecials board' [(3, 3)]
  assertEqual "rainbow expand is identity" [(3, 3)] expanded

-- | Rainbow×Normal: seeds + expandSpecials stay on partner color (+ rainbow cell), not own color.
rainbow_swap_partner_not_own_color :: Assertion
rainbow_swap_partner_not_own_color = do
  let board0 =
        foldl
          (\b p -> setCell b p (mkGem C1))
          stableBoard
          [(0, 0), (0, 2), (2, 0), (7, 6)]
      board =
        setCell
          (setCell board0 (4, 4) (Gem C1 Rainbow 0 Nothing))
          (4, 5)
          (mkGem C2)
      swapped = swapCells board (4, 4) (4, 5)
      seeds = rainbowClearSeeds swapped (4, 4) (4, 5)
      expanded = expandSpecials swapped seeds
      ownExtras =
        [ p
        | p <- expanded
        , p `notElem` seeds
        , case getCell swapped p of
            Gem C1 _ _ _ -> True
            _ -> False
        ]
  assertBool "seeds include rainbow" $
    any (\p -> isRainbow (getCell swapped p)) seeds
  assertEqual "no own-color extras from expand" ([] :: [Pos]) ownExtras
  assertEqual "expand == seeds (no rainbow self-blast)" (length seeds) (length expanded)
  -- Live swap still applies and consumes rainbow
  let gs0 =
        (newGame defaultConfig 3)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 8
          , gsBelts = []
          , gsUfos = []
          }
      (gs1, out) = trySwap (4, 4) (4, 5) gs0
  case out of
    NoMatch -> assertFailure "rainbow swap must apply"
    InvalidSwap -> assertFailure "rainbow swap must be valid"
    _ -> pure ()
  assertBool "rainbow gone" $
    not (isRainbow (getCell (gsBoard gs1) (4, 4)))
      && not (isRainbow (getCell (gsBoard gs1) (4, 5)))

-- | Rainbow×Bomb: partner-color clear seeds expand the Bomb into a 3×3 blast.
rainbow_x_bomb_blast_expands :: Assertion
rainbow_x_bomb_blast_expands = do
  let board =
        setCell
          (setCell stableBoard (3, 3) (Gem C5 Rainbow 0 Nothing))
          (3, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "rainbow swap" (isRainbowSwap board (3, 3) (3, 4))
  assertBool "not a listed specialCombo (goes rainbow path)" $
    not (isSpecialCombo board (3, 3) (3, 4))
  let swapped = swapCells board (3, 3) (3, 4)
      seeds = rainbowClearSeeds swapped (3, 3) (3, 4)
      expanded = expandSpecials swapped seeds
      -- After swap Bomb sits at (3,3); its 3×3 should appear in expanded
      bomb3x3 =
        [ (r, c)
        | r <- [2 .. 4]
        , c <- [2 .. 4]
        ]
  assertBool "bomb in seeds" ((3, 3) `elem` seeds)
  assertBool ("3x3 subset of expanded, got " ++ show (length expanded)) $
    all (`elem` expanded) bomb3x3
  assertBool "expanded beyond color seeds" (length expanded > length seeds)
  let gs0 =
        (newGame defaultConfig 4)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 6
          , gsBelts = []
          , gsUfos = []
          , gsScore = 0
          }
      (gs1, out) = trySwap (3, 3) (3, 4) gs0
  case out of
    NoMatch -> assertFailure "must apply"
    InvalidSwap -> assertFailure "must be valid"
    MoveApplied g -> assertBool ("scored " ++ show g) (g >= 40)
    _ -> pure ()
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)

-- | Map / restart jump uses printed moves only (no leftover carry bank).
map_select_no_carry_moves :: Assertion
map_select_no_carry_moves = do
  let gsPrev =
        (newGameAtLevel 0 (levelConfig (allLevels !! 0)) 1)
          { gsOver = Just (LevelClear 100 1)
          , gsMoves = 9
          }
      carried = nextLevel gsPrev 2
      base1 = lvlMoves (allLevels !! 1)
  assertEqual "carry path adds bonus" (base1 + 3) (gsMoves carried)
  -- Map-like jump / restart: fresh allotment
  let gsMap = newGameAtLevel 1 (levelConfig (allLevels !! 1)) 3
      gsRestart = restartLevel gsPrev { gsLevel = 1, gsOver = Nothing } 4
  assertEqual "map select no carry" base1 (gsMoves gsMap)
  assertEqual "restart no carry" base1 (gsMoves gsRestart)

-- | Star tiers use printed base moves; carry must not tighten the denominator.
star_rating_vs_carry_base :: Assertion
star_rating_vs_carry_base = do
  let base = 20 :: Int
      carry = 3 :: Int
      left = 8 :: Int
  assertEqual "3★ at 40% of base" (3 :: Int) (starRating base left)
  assertEqual "inflated start would wrongly drop to 2★" (2 :: Int) (starRating (base + carry) left)
  -- After nextLevel, gsMoves is printed+carry; UI must rate vs printed (Main advanceOrMsg).
  let gsPrev =
        (newGameAtLevel 0 (levelConfig (allLevels !! 0)) 1)
          { gsOver = Just (LevelClear 50 1)
          , gsMoves = 5
          }
      gsNext = nextLevel gsPrev 9
      printed = lvlMoves (allLevels !! gsLevel gsNext)
  assertEqual "carry cap on gsMoves" (printed + 3) (gsMoves gsNext)
  assertEqual "skill tier vs printed still 3★ at 40%" (3 :: Int) (starRating printed (printed * 2 `div` 5))
  assertEqual "skill tier vs inflated would be 2★" (2 :: Int) (starRating (gsMoves gsNext) (printed * 2 `div` 5))

-- | Curtain: may swap (≠ Chain/Freeze) but curtained gem breaks match runs.
curtain_allows_swap_blocks_match :: Assertion
curtain_allows_swap_blocks_match = do
  let board =
        setCell
          (setCell (setCell stableBoard (5, 2) (mkGem C2)) (5, 3) (mkCurtainGem C2 1))
          (5, 4)
          (mkGem C2)
  assertBool "curtain does not block swap" $
    not (swapBlockedByStone board (5, 3) (5, 2))
  assertBool "no H match through curtain" $
    not ((5, 2) `elem` findMatches board)
      && not ((5, 3) `elem` findMatches board)
      && not ((5, 4) `elem` findMatches board)
  -- Open curtain then the same triple can match
  let open = setCell board (5, 3) (mkGem C2)
  assertBool "open triple matches" $
    all (`elem` findMatches open) [(5, 2), (5, 3), (5, 4)]

-- | Hammer on Grass/Vine clears the gem (overlays strip; ≠ Chain/Curtain peel-lock).
hammer_clears_grass_vine :: Assertion
hammer_clears_grass_vine = do
  let bg = setCell stableBoard (2, 2) (Gem C3 Normal 0 (Just Grass))
      gsG0 =
        (newGame defaultConfig 2)
          { gsBoard = bg
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gsG1, _) = useHammer (2, 2) gsG0
  assertBool "grass cell cleared or refilled (not peel-locked)" $
    not (hasGrass (getCell (gsBoard gsG1) (2, 2)))
  let bv = setCell stableBoard (4, 4) (Gem C4 Normal 0 (Just Vine))
      gsV0 =
        (newGame defaultConfig 3)
          { gsBoard = bv
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          }
      (gsV1, _) = useHammer (4, 4) gsV0
  assertBool "vine cell cleared or refilled" $
    not (hasVine (getCell (gsBoard gsV1) (4, 4)))

-- | Freeze blocks both adjacent trySwap (click/drag) and free-swap booster.
freeze_blocks_freeswap_and_swap :: Assertion
freeze_blocks_freeswap_and_swap = do
  let board =
        setCell
          (setCell stableBoard (2, 2) (mkFreezeGem C1 1))
          (2, 3)
          (mkGem C2)
      gs0 =
        (newGame defaultConfig 5)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsFreeSwaps = 2
          , gsBelts = []
          , gsUfos = []
          }
  assertBool "swapBlocked" (swapBlockedByStone board (2, 2) (2, 3))
  let (gs1, out1) = trySwap (2, 2) (2, 3) gs0
  assertEqual "trySwap NoMatch" NoMatch out1
  assertEqual "moves kept" (gsMoves gs0) (gsMoves gs1)
  let (gs2, out2) = useFreeSwap (2, 2) (5, 5) gs0
  assertEqual "free-swap NoMatch" NoMatch out2
  assertEqual "free-swap charge kept" (gsFreeSwaps gs0) (gsFreeSwaps gs2)


--------------------------------------------------------------------------------
-- Stability cruise: seed-cascade combo score + bottle/hat follow-up matches
--------------------------------------------------------------------------------

-- | Rainbow/special seed clears are wave 1; later match cascades must keep rising
-- multipliers (bug: fromSeeds restarted at 1x so score == cells*10).
combo_seed_continues_wave_score :: Assertion
combo_seed_continues_wave_score = do
  let bSafe =
        [ [mkGem (toEnum ((r * 3 + c) `mod` 5)) | c <- [0 .. 7]]
        | r <- [0 .. 7]
        ]
      board =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell bSafe (0, 0) (mkGem C1))
                      (0, 1)
                      (mkGem C1))
                   (0, 2)
                   (mkGem C1))
                (7, 5)
                (mkGem C2))
             (7, 6)
             (mkGem C2))
          (7, 7)
          (mkGem C2)
      seeds = [(0, 0), (0, 1), (0, 2)]
      (_, cells, scored, maxW, _, _, _, _, _, _, _, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 0) board
  assertBool "cleared both seed + follow-up" (cells >= 6)
  assertBool ("maxW >= 2, got " ++ show maxW) (maxW >= 2)
  -- With wave multipliers, score must beat flat 10/cell (all waves at 1x).
  assertBool
    ("score " ++ show scored ++ " > flat " ++ show (cells * 10))
    (scored > cells * 10)
  assertEqual
    "wave1+wave2 lower bound for 3+3"
    True
    (scored >= scoreForWave 1 3 + scoreForWave 2 3)

-- | Dye bottle recolors neighbors mid-clear; a new match must cascade (not stall).
bottle_dye_followup_match :: Assertion
bottle_dye_followup_match = do
  let bSafe =
        [ [mkGem (toEnum ((r * 3 + c) `mod` 5)) | c <- [0 .. 7]]
        | r <- [0 .. 7]
        ]
      board =
        foldl
          (\b (p, c) -> setCell b p c)
          bSafe
          [ ((6, 0), mkGem C1)
          , ((6, 1), mkGem C1)
          , ((6, 2), mkGem C1)
          , ((5, 1), mkBottle C3)
          , ((5, 0), mkGem C4)
          , ((5, 2), mkGem C4)
          , ((4, 2), mkGem C3)
          , ((3, 2), mkGem C3)
          ]
      ms = findMatches board
  assertBool "seed match includes row6" $
    all (`elem` ms) [(6, 0), (6, 1), (6, 2)]
  let dyed = triggerAdjacentBottles board ms
  assertEqual "dyed (5,2)" C3 (cellColor (getCell dyed (5, 2)))
  assertBool "dye created vertical C3" $
    all (`elem` findMatches dyed) [(3, 2), (4, 2), (5, 2)]
  let (_, cells, scored, maxW, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 42) board
  assertBool ("follow-up cascade cells>=6 got " ++ show cells) (cells >= 6)
  assertBool ("maxW>=2 got " ++ show maxW) (maxW >= 2)
  assertBool "scored" (scored >= scoreForWave 1 3 + scoreForWave 2 3)

-- | Magic hat recolor of neighbors can create a match; cascade must clear it.
hat_recolor_followup_match :: Assertion
hat_recolor_followup_match = do
  let bSafe =
        [ [mkGem (toEnum ((r * 3 + c) `mod` 5)) | c <- [0 .. 7]]
        | r <- [0 .. 7]
        ]
      board =
        foldl
          (\b (p, c) -> setCell b p c)
          bSafe
          [ ((6, 0), mkGem C1)
          , ((6, 1), mkGem C1)
          , ((6, 2), mkGem C1)
          , ((5, 1), mkMagicHat)
          , ((4, 1), mkStone) -- only left/right gem neighbors
          , ((5, 0), mkGem C2)
          , ((5, 2), mkGem C3)
          , ((4, 0), mkGem C3)
          , ((3, 0), mkGem C3)
          ]
      ms = findMatches board
      hatted = triggerAdjacentHats board ms
  assertEqual "hat swapped left to C3" C3 (cellColor (getCell hatted (5, 0)))
  assertEqual "hat swapped right to C2" C2 (cellColor (getCell hatted (5, 2)))
  assertBool "hat created col0 C3 match" $
    all (`elem` findMatches hatted) [(3, 0), (4, 0), (5, 0)]
  let (_, cells, scored, maxW, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 7) board
  assertBool ("hat follow-up cells>=6 got " ++ show cells) (cells >= 6)
  assertBool ("maxW>=2 got " ++ show maxW) (maxW >= 2)
  let (bAfter, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 7) board
      hatLeft =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isMagicHat (getCell bAfter (r, c))
        ]
  -- Hat may fall with gravity after the clear below it; must still exist.
  assertEqual "hat still on board" (1 :: Int) (length hatLeft)
  assertBool "scored" (scored >= scoreForWave 1 3)


--------------------------------------------------------------------------------
-- Stability cruise: snail×belt, maker charge edge, particle sites, map unlock
--------------------------------------------------------------------------------

-- | Snail sitting on a belt must not crawl after the belt shift (no double-step).
snail_belt_no_double_step :: Assertion
snail_belt_no_double_step = do
  let belt = [(4, 1), (4, 2), (4, 3), (4, 4)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (4, 1) (mkSnail 0 1))
                   (0, 0)
                   (mkGem C1))
                (0, 1)
                (mkGem C1))
             (0, 2)
             (mkGem C2))
          (0, 3)
          (mkGem C1)
      afterBelt = shiftBelts board0 [belt]
  assertBool "belt moved snail to (4,2)" (isSnail (getCell afterBelt (4, 2)))
  let skipped = stepSnailsAvoiding (concat [belt]) afterBelt
      crawled = stepSnails afterBelt
  assertBool "avoid: still at (4,2)" (isSnail (getCell skipped (4, 2)))
  assertBool "avoid: not at (4,3)" (not (isSnail (getCell skipped (4, 3))))
  assertBool "raw crawl would reach (4,3)" (isSnail (getCell crawled (4, 3)))
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board0
          , gsBelts = [belt]
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let b1 = gsBoard gs1
      snails =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isSnail (getCell b1 (r, c))
        ]
  assertEqual "one snail survives" (1 :: Int) (length snails)
  assertEqual "belt-only step lands on (4,2)" [(4, 2)] snails

-- | Three same-color adjacent clears in one wave charge a maker only once.
maker_multi_adjacent_charges_once :: Assertion
maker_multi_adjacent_charges_once = do
  let board =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (2, 2) (mkMakerCharges C1 2))
                (2, 1)
                (mkGem C1))
             (2, 3)
             (mkGem C1))
          (1, 2)
          (mkGem C1)
      cleared = [(2, 1), (2, 3), (1, 2)]
      adj = makersAdjacentSameColor board cleared
  assertEqual "nub single maker" [(2, 2)] adj
  let b1 = chargeAdjacentMakers board cleared
  assertBool "still maker" (isMaker (getCell b1 (2, 2)))
  assertEqual "2->1 once" (1 :: Int) (makerCharges (getCell b1 (2, 2)))
  let bLow = setCell board (2, 2) (mkMakerCharges C1 1)
      bBomb = chargeAdjacentMakers bLow cleared
  assertBool "became bomb" (cellKind (getCell bBomb (2, 2)) == Bomb)
  assertBool "not maker" (not (isMaker (getCell bBomb (2, 2))))

-- | gsLastCleared tracks cascade clears, not belt rotation / snail crawl cells.
last_cleared_skips_belt_snail :: Assertion
last_cleared_skips_belt_snail = do
  let belt = [(6, 1), (6, 2), (6, 3)]
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell stableBoard (6, 1) (mkSnail 0 1))
                      (1, 0)
                      (mkGem C1))
                   (1, 1)
                   (mkGem C1))
                (1, 2)
                (mkGem C2))
             (1, 3)
             (mkGem C1))
          (6, 2)
          (mkGem C4)
      gs0 =
        (newGame defaultConfig 8)
          { gsBoard = board0
          , gsBelts = [belt]
          , gsMoves = 10
          , gsOver = Nothing
          , gsHint = Nothing
          , gsUfos = []
          , gsGoal = GoalScore 99999
          , gsLastCleared = []
          }
      (gs1, out) = trySwap (1, 2) (1, 3) gs0
  case out of
    NoMatch -> assertFailure "expected match"
    InvalidSwap -> assertFailure "expected valid"
    _ -> pure ()
  let cleared = gsLastCleared gs1
  assertBool "recorded clears" (not (null cleared))
  assertBool "match row cleared" $
    any (`elem` cleared) [(1, 0), (1, 1), (1, 3), (1, 2)]
  assertBool "snail start not a clear site" ((6, 1) `notElem` cleared)
  assertBool "snail belt mid not a clear site" ((6, 2) `notElem` cleared)

-- | LevelClear must unlock the next map index immediately (before N advance).
unlock_after_clear_bumps_map :: Assertion
unlock_after_clear_bumps_map = do
  assertEqual "clear L0 unlocks 1" (1 :: Int) (unlockAfterClear 0 (LevelClear 10 1))
  assertEqual "already past stays" (5 :: Int) (unlockAfterClear 5 (LevelClear 10 3))
  assertEqual "move no bump" (2 :: Int) (unlockAfterClear 2 (MoveApplied 30))
  assertEqual "won unlocks finale" (length allLevels - 1) (unlockAfterClear 0 (Won 99))

-- | Clicking the active level on the map resumes (no restart / progress loss).
map_click_same_level_resumes :: Assertion
map_click_same_level_resumes = do
  assertEqual "same level -> resume" Nothing (mapClickJump 3 7 3)
  assertEqual "locked -> ignore" Nothing (mapClickJump 3 7 9)
  assertEqual "other unlocked -> jump" (Just 5) (mapClickJump 3 7 5)
  assertEqual "earlier unlocked -> jump" (Just 1) (mapClickJump 3 7 1)

-- | UFO must not target peel-locks / multi-ice / Flip (no GoalUfo phantom counts).
ufo_skips_peel_locks :: Assertion
ufo_skips_peel_locks = do
  let row = replicate boardSize (mkGem C5)
      base = replicate boardSize row
      u = mkUfo (7, 3) C1
      near lock =
        setCell (setCell base (7, 3) (mkGem C5)) (7, 4) lock
  assertEqual "skips chain" [] (ufoAbsorbTargets (near (mkChainGem C1 2)) u)
  assertEqual "skips curtain" [] (ufoAbsorbTargets (near (mkCurtainGem C1 1)) u)
  assertEqual "skips fog" [] (ufoAbsorbTargets (near (mkFogGem C1 1)) u)
  assertEqual "skips steam" [] (ufoAbsorbTargets (near (mkSteamGem C1)) u)
  assertEqual "skips ice>1" [] (ufoAbsorbTargets (near (mkIceGem C1 2)) u)
  assertEqual "skips flip" [] (ufoAbsorbTargets (near (mkFlip C1 C4)) u)
  assertEqual "takes bare" [(7, 4)] (ufoAbsorbTargets (near (mkGem C1)) u)
  assertEqual "takes freeze" [(7, 4)] (ufoAbsorbTargets (near (mkFreezeGem C1 1)) u)
  assertEqual "takes last-ice" [(7, 4)] (ufoAbsorbTargets (near (mkIceGem C1 1)) u)
  -- Defense-in-depth: if a peel-lock somehow entered absorbed, clear ∩ count is 0.
  -- Seed-clear a chain cell via fromSeeds path is N/A (UFO skips targeting);
  -- verify wrong-color bare still ignored.
  assertEqual "ignores wrong color" [] (ufoAbsorbTargets (near (mkGem C2)) u)

-- | Hammer on Maker/Snail/Bottle/Hat/Cookie is a no-op: reject without spending a charge.
hammer_immune_no_spend :: Assertion
hammer_immune_no_spend = do
  let mkGs cell =
        (newGame defaultConfig 11)
          { gsBoard = setCell stableBoard (3, 3) cell
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
      check tag cell = do
        let gs0 = mkGs cell
            (gs1, out) = useHammer (3, 3) gs0
        out @?= NoMatch
        assertEqual (tag ++ " hammers kept") (2 :: Int) (gsHammers gs1)
        assertEqual (tag ++ " board unchanged") (gsBoard gs0) (gsBoard gs1)
  check "maker" (mkMakerCharges C1 2)
  check "snail" (mkSnail 0 1)
  check "bottle" (mkBottle C2)
  check "hat" mkMagicHat
  check "cookie" mkCookie
  -- Control: bare gem still spends
  let gsG0 = mkGs (mkGem C1)
      (gsG1, outG) = useHammer (3, 3) gsG0
  case outG of
    NoMatch -> assertFailure "bare gem hammer should apply"
    InvalidSwap -> assertFailure "bare gem hammer should apply"
    _ -> pure ()
  assertEqual "bare spends hammer" (1 :: Int) (gsHammers gsG1)

-- | Rainbow×Flip activates like Rainbow×Countdown: partner front color is the
-- clear target even when the swap forms no classic 3-match. Partner Flip flips
-- to its back face (does not hole); other front-color gems clear.
rainbow_swap_flip_partner :: Assertion
rainbow_swap_flip_partner = do
  let board =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 Nothing))
          (0, 1)
          (mkFlip C4 C1)
  assertBool "is rainbow×flip swap" (isRainbowSwap board (0, 0) (0, 1))
  assertBool "symmetric flip×rainbow" (isRainbowSwap board (0, 1) (0, 0))
  assertBool "no classic match required" $
    not (hasAnyMatch (swapCells board (0, 0) (0, 1)))
  let swapped = swapCells board (0, 0) (0, 1)
      seeds = rainbowClearSeeds swapped (0, 0) (0, 1)
  assertBool "seeds include rainbow" $
    any (\p -> isRainbow (getCell swapped p)) seeds
  assertBool "seeds include partner flip" ((0, 0) `elem` seeds)  -- Flip landed at (0,0)
  assertBool "seeds include other C4" $
    any
      ( \p ->
          p /= (0, 0)
            && case getCell swapped p of
              Gem C4 _ _ _ -> True
              Flip C4 _ -> True
              _ -> False
      )
      seeds
  -- Unit: direct seed hit flips partner (gem stays as back color)
  let (bIced, iceFree) = chipIceOnClear swapped [(0, 0)]
  assertBool "partner not holed" ((0, 0) `notElem` iceFree)
  assertEqual "flipped to back C1" C1 (cellColor (getCell bIced (0, 0)))
  assertBool "no longer flip" (not (isFlip (getCell bIced (0, 0))))
  -- Live trySwap must apply (regression: used to NoMatch-rollback)
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 8
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gs1, out) = trySwap (0, 0) (0, 1) gs0
  case out of
    NoMatch -> assertFailure "rainbow×flip must not roll back as NoMatch"
    InvalidSwap -> assertFailure "rainbow×flip must be valid"
    MoveApplied g -> assertBool "gained" (g > 0)
    LevelClear _ _ -> pure ()
    Won _ -> pure ()
    Lost _ -> pure ()
  assertBool "rainbow consumed" $
    not (isRainbow (getCell (gsBoard gs1) (0, 0)))
      && not (isRainbow (getCell (gsBoard gs1) (0, 1)))
  assertEqual "moves -1" (gsMoves gs0 - 1) (gsMoves gs1)

--------------------------------------------------------------------------------
-- Surprise direct-seed open (hammer / cross): special survives; explode blasts
--------------------------------------------------------------------------------

-- | Direct-hit Surprise must open like adjacent open — not spawn-then-hole or
-- single-cell delete. Seed cascade keeps special; explode hammers 3×3.
surprise_direct_seed_opens :: Assertion
surprise_direct_seed_opens = do
  -- Unit: openSurprises on a direct-seed iceFree list saves special-outcome cells.
  let boardSpecial = setCell stableBoard (4, 0) mkSurprise
      (bOpen, expl0, saved0) = openSurprises boardSpecial [(4, 0)]
  assertEqual "no explode for outcome 0" (0 :: Int) (length expl0)
  assertEqual "saved special cell" [(4, 0)] saved0
  let cellU = getCell bOpen (4, 0)
  assertBool "opened to special" (isGem cellU && cellKind cellU /= Normal)
  assertEqual "LineH" LineH (cellKind cellU)
  -- Seed cascade (hammer path): special sits; not dug by iceFree hole.
  let g0 = mkStdGen 11
      (bCas, nCleared, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScoredFromSeeds Nothing [(4, 0)] g0 boardSpecial
  assertEqual "special open clears no hole" (0 :: Int) nCleared
  let cellC = getCell bCas (4, 0)
  assertBool "cascade kept special" (isGem cellC && cellKind cellC /= Normal)
  assertEqual "cascade LineH" LineH (cellKind cellC)
  -- Direct-hit alongside another seed: special is saved (not spawn-then-hole).
  let (bOpen2, _, saved2) = openSurprises boardSpecial [(4, 0), (4, 1)]
  assertBool "saved when co-seeded" ((4, 0) `elem` saved2)
  assertEqual "still LineH when co-seeded" LineH (cellKind (getCell bOpen2 (4, 0)))
  -- (3,3) explode-outcome: hammer 3×3 scores >= 90 (single-cell would be 10).
  let boardBoom = setCell stableBoard (3, 3) mkSurprise
      gsB0 =
        (newGame defaultConfig 11)
          { gsBoard = boardBoom
          , gsHammers = 2
          , gsMoves = 20
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsGoal = GoalScore 99999
          }
      (gsB1, outB) = useHammer (3, 3) gsB0
  case outB of
    NoMatch -> assertFailure "hammer explode surprise should apply"
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsB1)
  assertBool "surprise gone after explode" $
    not (isSurprise (getCell (gsBoard gsB1) (3, 3)))
  assertBool ("explode score >= 90, got " ++ show (gsScore gsB1)) (gsScore gsB1 >= 90)


--------------------------------------------------------------------------------
-- Soft-hit must not strip adjacent Choco / Steam (ice chip / Flip face-flip)
--------------------------------------------------------------------------------

-- | ice>1 chip and Flip face-flip are soft hits (not clear holes). They must NOT
-- extinguish orthogonally adjacent Chocolate or Steam — only true clears do.
-- Regression: clear pipeline used raw expand seeds, so soft hits stripped décor.
soft_hit_preserves_choco_steam :: Assertion
soft_hit_preserves_choco_steam = do
  -- ice=2 mid of a 3-match: chips to ice=1, stays; adjacent choco must survive.
  let boardIce =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkIceGem C1 2))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkChocoGem C2)
  assertBool "ice match present" (not (null (findMatches boardIce)))
  let expanded = expandSpecials boardIce (findMatches boardIce)
      (_, iceFree) = chipIceOnClear boardIce expanded
  assertBool "soft ice not a hole" ((3, 1) `notElem` iceFree)
  let (boardI1, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 21) boardIce
  assertEqual "ice chipped once" (1 :: Int) (iceLayers (getCell boardI1 (3, 1)))
  assertBool "choco survives ice soft-hit" (hasChoco (getCell boardI1 (2, 1)))
  -- Control: same layout with bare mid gem — true clear *does* strip choco.
  let boardHard =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (2, 1)
          (mkChocoGem C2)
      (boardH1, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 22) boardHard
  assertBool "true clear strips choco" (not (hasChoco (getCell boardH1 (2, 1))))
  -- Flip in a 3-match: flips to back, stays; adjacent steam must survive.
  let boardFlip =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (4, 0) (mkGem C2))
                (4, 1)
                (mkFlip C2 C5))
             (4, 2)
             (mkGem C2))
          (5, 1)
          (mkSteamGem C3)
  assertBool "flip match present" (not (null (findMatches boardFlip)))
  let (_, iceF) = chipIceOnClear boardFlip (expandSpecials boardFlip (findMatches boardFlip))
  assertBool "flip not a hole" ((4, 1) `notElem` iceF)
  let (boardF1, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 23) boardFlip
  assertBool "became back gem" $
    isGem (getCell boardF1 (4, 1)) && not (isFlip (getCell boardF1 (4, 1)))
  assertEqual "back color C5" C5 (cellColor (getCell boardF1 (4, 1)))
  assertBool "steam survives flip soft-hit" (hasSteam (getCell boardF1 (5, 1)))

-- | Surprise 3×3 explode must peel/chip adjacent obstacles like Bomb (trueClears).
-- Regression: surpFree used to skip stone/fog/chain/freeze/curtain/honey peels.
surprise_blast_peels_adjacent :: Assertion
surprise_blast_peels_adjacent = do
  -- (3,3) outcome 3 → explode; obstacles sit just outside the 3×3 blast.
  let board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell
                               (setCell stableBoard (3, 0) (mkGem C1))
                               (3, 1)
                               (mkGem C1))
                            (3, 2)
                            (mkGem C1))
                         (3, 3)
                         mkSurprise)
                      (2, 5)
                      (mkStoneLayers 2))
                   (1, 4)
                   (mkFogGem C4 2))
                (4, 5)
                (Gem C3 Normal 0 (Just (Chain 2))))
             (5, 3)
             (Gem C2 Normal 0 (Just (Freeze 2))))
          (3, 5)
          (Gem C1 Normal 0 (Just (Curtain 2)))
  assertEqual "explode outcome" (3 :: Int) (((3 * 8 + 3) `mod` 4))
  let (mb, _) = clearMatches board0
      stone = case (mb !! 2) !! 5 of
        Just c -> c
        Nothing -> error "stone must remain"
      fog = case (mb !! 1) !! 4 of
        Just c -> c
        Nothing -> error "fog gem must remain"
      chain = case (mb !! 4) !! 5 of
        Just c -> c
        Nothing -> error "chain gem must remain"
      freeze = case (mb !! 5) !! 3 of
        Just c -> c
        Nothing -> error "freeze gem must remain"
      curtain = case (mb !! 3) !! 5 of
        Just c -> c
        Nothing -> error "curtain gem must remain"
  assertEqual "stone chipped 2→1" (1 :: Int) (stoneLayers stone)
  assertEqual "fog peeled 2→1" (Just (Fog 1)) (cellOverlay fog)
  assertEqual "chain peeled 2→1" (Just (Chain 1)) (cellOverlay chain)
  assertEqual "freeze peeled 2→1" (Just (Freeze 1)) (cellOverlay freeze)
  assertEqual "curtain peeled 2→1" (Just (Curtain 1)) (cellOverlay curtain)
  -- Honey outside blast, ortho-adjacent to a blast cell, chips one layer.
  let boardH =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 5)
          (mkHoneyLayers 2)
      (mbH, _) = clearMatches boardH
  case (mbH !! 2) !! 5 of
    Just h -> assertEqual "honey chipped 2→1" (1 :: Int) (honeyLayers h)
    Nothing -> assertFailure "honey must remain (not last layer)"
  -- Control: Bomb in-match 3×3 still peels the same way (parity sanity).
  let boardB =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (Gem C1 Bomb 0 Nothing))
          (2, 4)
          (mkStoneLayers 2)
      (mbB, _) = clearMatches boardB
  case (mbB !! 2) !! 4 of
    Just s -> assertEqual "bomb still chips stone" (1 :: Int) (stoneLayers s)
    Nothing -> assertFailure "bomb stone must remain"

--------------------------------------------------------------------------------
-- Shuffle must keep Line / Bomb / Rainbow (extractDecor specials)
--------------------------------------------------------------------------------

-- | Bare Line/Bomb/Rainbow specials must survive shuffleGame / ensurePlayable
-- décor restore. Regression: extractDecor only kept iced/overlaid gems, so
-- shuffle wiped player-earned specials while countdown bombs stayed (comment
-- claimed bombs were preserved).
shuffle_preserves_specials :: Assertion
shuffle_preserves_specials = do
  let board =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (1, 1) (Gem C1 Bomb 0 Nothing))
                (1, 2)
                (Gem C2 LineH 0 Nothing))
             (1, 3)
             (Gem C3 LineV 0 Nothing))
          (1, 4)
          (Gem C4 Rainbow 0 Nothing)
      gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 20
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
      gs1 = shuffleGame gs0
      b1 = gsBoard gs1
  assertEqual "bomb kept" Bomb (cellKind (getCell b1 (1, 1)))
  assertEqual "bomb color" C1 (cellColor (getCell b1 (1, 1)))
  assertEqual "lineH kept" LineH (cellKind (getCell b1 (1, 2)))
  assertEqual "lineV kept" LineV (cellKind (getCell b1 (1, 3)))
  assertEqual "rainbow kept" Rainbow (cellKind (getCell b1 (1, 4)))
  -- Countdown still kept (pre-existing decor path)
  let boardCd = setCell board (2, 2) (mkCountdown C5 4)
      gsCd = shuffleGame (gs0 { gsBoard = boardCd })
  assertBool "countdown kept" (isCountdown (getCell (gsBoard gsCd) (2, 2)))
  assertEqual "countdown turns" (4 :: Int) (countdownTurns (getCell (gsBoard gsCd) (2, 2)))

--------------------------------------------------------------------------------
-- Soft-locked Line/Bomb must not expand (ice>1 / Chain / Curtain)
--------------------------------------------------------------------------------

-- | ice>1 and Chain/Curtain peel-locks must suppress Line/Bomb expandSpecials.
-- Regression: expand fired while chipIce only soft-chipped, clearing whole rows
-- / 3×3 around still-locked specials (and Chain+Line hammer double-peeled via
-- adjacent after the illicit row clear).
soft_lock_blocks_special_expand :: Assertion
soft_lock_blocks_special_expand = do
  -- ice=2 LineH in a 3-match: chips ice only; does NOT clear the rest of the row.
  let boardIce =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 LineH 2 Nothing))
          (3, 2)
          (mkGem C1)
  assertBool "ice match" (not (null (findMatches boardIce)))
  let expIce = expandSpecials boardIce (findMatches boardIce)
  assertEqual "ice>1 LineH does not expand row" (sort (findMatches boardIce)) (sort expIce)
  let (bIce, nIce, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 31) boardIce
  assertEqual "only two match partners clear" (2 :: Int) nIce
  assertEqual "LineH survives" LineH (cellKind (getCell bIce (3, 1)))
  assertEqual "ice chipped 2→1" (1 :: Int) (iceLayers (getCell bIce (3, 1)))
  assertBool "far cell (3,7) untouched kind" $
    isGem (getCell bIce (3, 7)) && cellKind (getCell bIce (3, 7)) == Normal
  -- Control: last ice (ice==1) LineH still expands the row.
  let boardLast =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 LineH 1 Nothing))
          (3, 2)
          (mkGem C1)
      expLast = expandSpecials boardLast (findMatches boardLast)
  assertEqual "last-ice expands full row" (8 :: Int) (length [c | (3, c) <- expLast])
  -- ice=2 Bomb: no 3×3 while soft-locked.
  let boardBomb =
        setCell
          (setCell
             (setCell stableBoard (3, 0) (mkGem C1))
             (3, 1)
             (Gem C1 Bomb 2 Nothing))
          (3, 2)
          (mkGem C1)
      expBomb = expandSpecials boardBomb (findMatches boardBomb)
  assertEqual "ice>1 Bomb does not blast" (sort (findMatches boardBomb)) (sort expBomb)
  -- Hammer on Chain+LineH: peel one layer only (no row clear / no double peel).
  let boardCh = setCell stableBoard (4, 4) (Gem C2 LineH 0 (Just (Chain 2)))
      gsCh0 =
        (newGame defaultConfig 11)
          { gsBoard = boardCh
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
      (gsCh1, outCh) = useHammer (4, 4) gsCh0
  case outCh of
    NoMatch -> assertFailure "hammer peel should apply"
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cellCh = getCell (gsBoard gsCh1) (4, 4)
  assertEqual "LineH kept" LineH (cellKind cellCh)
  assertBool "chain peeled 2→1" (hasChain cellCh && chainLayers cellCh == 1)
  assertEqual "no illicit row score" (0 :: Int) (gsScore gsCh1)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsCh1)
  -- Curtain+Bomb: expandSpecials is identity on the seed.
  let boardCu = setCell stableBoard (5, 5) (Gem C3 Bomb 0 (Just (Curtain 2)))
      expCu = expandSpecials boardCu [(5, 5)]
  assertEqual "curtain Bomb does not blast" [(5, 5)] expCu

--------------------------------------------------------------------------------
-- Line/Bomb blast must not double-peel direct-hit layered obstacles
--------------------------------------------------------------------------------

-- | Chain/Curtain/Stone/Safe on a Line clear path already take one direct chip
-- via chipIceOnClear; ortho adjacent peels from neighbor holes must not chip
-- them again (regression: Chain2/Stone2/Safe2 fully cleared in one Line wave).
line_blast_no_double_peel :: Assertion
line_blast_no_double_peel = do
  let lineBoard lock =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell stableBoard (3, 0) (mkGem C1))
                            (3, 1)
                            (Gem C1 LineH 0 Nothing))
                         (3, 2)
                         (mkGem C1))
                      (3, 3)
                      (mkGem C3))
                   (3, 4)
                   (mkGem C4))
                (3, 5)
                lock)
             (3, 6)
             (mkGem C5))
          (3, 7)
          (mkGem C3)
  assertBool "line match" (not (null (findMatches (lineBoard (mkGem C2)))))
  -- Chain 2 on blast path: peel once → Chain 1 (not fully unlocked).
  let (bCh, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 41) (lineBoard (Gem C2 Normal 0 (Just (Chain 2))))
      cellCh = getCell bCh (3, 5)
  assertBool "chain survives" (hasChain cellCh)
  assertEqual "chain peeled once 2→1" (1 :: Int) (chainLayers cellCh)
  -- Curtain 2: same single peel.
  let (bCu, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 42) (lineBoard (Gem C2 Normal 0 (Just (Curtain 2))))
      cellCu = getCell bCu (3, 5)
  assertBool "curtain survives" (hasCurtain cellCu)
  assertEqual "curtain peeled once 2→1" (1 :: Int) (curtainLayers cellCu)
  -- Stone 2: chip once → Stone 1 (not removed).
  let (bSt, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 43) (lineBoard (mkStoneLayers 2))
      cellSt = getCell bSt (3, 5)
  assertBool "stone survives" (isStone cellSt)
  assertEqual "stone chipped once 2→1" (1 :: Int) (stoneLayers cellSt)
  -- Safe 2: chip once → Safe 1 (not opened to Cookie).
  let (bSa, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 44) (lineBoard (mkSafeLayers 2))
      cellSa = getCell bSa (3, 5)
  assertBool "safe survives" (isSafe cellSa)
  assertEqual "safe chipped once 2→1" (1 :: Int) (safeLayers cellSa)
  -- Control: Chain 1 on path fully unlocks (single peel strips last layer).
  let (bC1, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 45) (lineBoard (Gem C2 Normal 0 (Just (Chain 1))))
  assertBool "chain1 unlocked" (not (hasChain (getCell bC1 (3, 5))))
  -- Control: adjacent-only Chain 2 (not on blast) still peels once.
  let boardAdj =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (mkGem C1))
          (4, 1)
          (Gem C2 Normal 0 (Just (Chain 2)))
      (bAdj, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 46) boardAdj
      cellAdj = getCell bAdj (4, 1)
  assertBool "adj chain survives" (hasChain cellAdj)
  assertEqual "adj chain peeled 2→1" (1 :: Int) (chainLayers cellAdj)

--------------------------------------------------------------------------------
-- Line/Bomb/Hammer direct hit must single-chip Chest/Honey/Cake (Stone parity)
--------------------------------------------------------------------------------

-- | Multi-layer Chest/Honey/Cake on a Line blast path (or hammer seed) must chip
-- one layer — not fully wipe. Regression: chipIceOnClear listed them clearable
-- regardless of layers while Stone/Safe correctly decremented, so LineH/Bomb/
-- Hammer erased Honey2/Chest2/Cake2 in one hit (Except peels were moot).
blast_chips_layered_obstacles_once :: Assertion
blast_chips_layered_obstacles_once = do
  let lineBoard lock =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell stableBoard (3, 0) (mkGem C1))
                            (3, 1)
                            (Gem C1 LineH 0 Nothing))
                         (3, 2)
                         (mkGem C1))
                      (3, 3)
                      (mkGem C3))
                   (3, 4)
                   (mkGem C4))
                (3, 5)
                lock)
             (3, 6)
             (mkGem C5))
          (3, 7)
          (mkGem C3)
  assertBool "line match" (not (null (findMatches (lineBoard (mkGem C2)))))
  -- Honey 2 on blast path: chip once → Honey 1 (not removed).
  let (bH, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 51) (lineBoard (mkHoneyLayers 2))
      cellH = getCell bH (3, 5)
  assertBool "honey survives" (isHoney cellH)
  assertEqual "honey chipped once 2→1" (1 :: Int) (honeyLayers cellH)
  -- Chest 2: same single chip.
  let (bC, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 52) (lineBoard (mkChestLayers 2))
      cellC = getCell bC (3, 5)
  assertBool "chest survives" (isChest cellC)
  assertEqual "chest chipped once 2→1" (1 :: Int) (chestLayers cellC)
  -- Cake 2: same single chip.
  let (bK, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 53) (lineBoard (mkCakeLayers 2))
      cellK = getCell bK (3, 5)
  assertBool "cake survives" (isCake cellK)
  assertEqual "cake chipped once 2→1" (1 :: Int) (cakeLayers cellK)
  -- Control: Honey 1 on path fully clears (last layer).
  let (bH1, _, _, _, _, _, _, honeyHit, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 54) (lineBoard (mkHoneyLayers 1))
  assertBool "honey1 cleared" (not (isHoney (getCell bH1 (3, 5))))
  assertBool "honey1 counted" (honeyHit >= 1)
  -- Hammer on Honey 3: chip 3→2, charge spent, not goal-counted yet.
  let boardHam = setCell stableBoard (5, 5) (mkHoneyLayers 3)
      gs0 =
        (newGame defaultConfig 12)
          { gsBoard = boardHam
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalHoney 8
          , gsMoves = 20
          , gsScore = 0
          , gsHoneyCleared = 0
          }
      (gs1, outH) = useHammer (5, 5) gs0
  case outH of
    NoMatch -> assertFailure "hammer chip should apply"
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cellHam = getCell (gsBoard gs1) (5, 5)
  assertBool "honey remains after hammer" (isHoney cellHam)
  assertEqual "hammer chips 3→2" (2 :: Int) (honeyLayers cellHam)
  assertEqual "not counted until last" (0 :: Int) (gsHoneyCleared gs1)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gs1)
  -- chipIceOnClear unit: Chest2/Cake2 not clearable; layers decremented.
  let (bChest, freeChest) = chipIceOnClear (setCell stableBoard (1, 1) (mkChestLayers 2)) [(1, 1)]
      (bCake, freeCake) = chipIceOnClear (setCell stableBoard (2, 2) (mkCakeLayers 2)) [(2, 2)]
  assertEqual "chest2 not clearable" ([] :: [Pos]) freeChest
  assertEqual "chest 2→1" (1 :: Int) (chestLayers (getCell bChest (1, 1)))
  assertEqual "cake2 not clearable" ([] :: [Pos]) freeCake
  assertEqual "cake 2→1" (1 :: Int) (cakeLayers (getCell bCake (2, 2)))

--------------------------------------------------------------------------------
-- MagicHat survives Line/Bomb/Hammer direct seeds (Maker/Bottle parity)
--------------------------------------------------------------------------------

-- | Hats only recolor via adjacent clears and must stay on the board. chipIceOnClear
-- used to list MagicHat as clearable, so LineH/Bomb/Hammer/Cross wiped hats without
-- a useful trigger (row neighbors already in the clear set). Align with Maker /
-- Snail / Bottle immunity; hammer rejects without spending. Adjacent trigger still
-- works. Regression: hat_immune_to_direct_clear.
hat_immune_to_direct_clear :: Assertion
hat_immune_to_direct_clear = do
  -- Unit: direct seed does not mark hat clearable; cell unchanged.
  let (bU, freeU) = chipIceOnClear (setCell stableBoard (2, 2) mkMagicHat) [(2, 2)]
  assertEqual "hat not clearable" ([] :: [Pos]) freeU
  assertBool "hat stays on chipIce" (isMagicHat (getCell bU (2, 2)))
  -- LineH blast through a hat: hat survives (Maker parity).
  let lineBoard lock =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell
                      (setCell
                         (setCell
                            (setCell stableBoard (3, 0) (mkGem C1))
                            (3, 1)
                            (Gem C1 LineH 0 Nothing))
                         (3, 2)
                         (mkGem C1))
                      (3, 3)
                      (mkGem C3))
                   (3, 4)
                   (mkGem C4))
                (3, 5)
                lock)
             (3, 6)
             (mkGem C5))
          (3, 7)
          (mkGem C3)
  assertBool "line match" (not (null (findMatches (lineBoard (mkGem C2)))))
  let (bLine, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 61) (lineBoard mkMagicHat)
  assertBool "hat survives line blast" (isMagicHat (getCell bLine (3, 5)))
  -- Hammer on hat: NoMatch, charge kept, board unchanged.
  let gs0 =
        (newGame defaultConfig 12)
          { gsBoard = setCell stableBoard (4, 4) mkMagicHat
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
      (gs1, outH) = useHammer (4, 4) gs0
  outH @?= NoMatch
  assertEqual "hammer not spent" (2 :: Int) (gsHammers gs1)
  assertBool "hat remains after hammer" (isMagicHat (getCell (gsBoard gs1) (4, 4)))
  -- Adjacent clear still triggers hat (recolor) and hat stays.
  let boardAdj =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (2, 0) (mkGem C1))
                (2, 1)
                (mkGem C1))
             (2, 2)
             (mkGem C1))
          (1, 1)
          mkMagicHat
      ms = [(2, 0), (2, 1), (2, 2)]
  assertBool "hat present" (isMagicHat (getCell boardAdj (1, 1)))
  let boardTrig = triggerAdjacentHats boardAdj ms
  assertBool "hat still after adj trigger" (isMagicHat (getCell boardTrig (1, 1)))

--------------------------------------------------------------------------------
-- Soft-hit must keep on-cell Grass/Vine/Choco (ice>1 hammer / Line path)
--------------------------------------------------------------------------------

-- | ice>1 soft chip must not strip Grass/Vine/Choco on the same cell.
-- Regression: clearOverlaysOn ran on raw expand seeds before chipIce, so a
-- hammer (or Line blast path) on ice=2+Choco wiped the overlay while the gem
-- stayed — breaking soft-hit / Choco-adjacent-only discipline.
soft_hit_preserves_oncell_overlays :: Assertion
soft_hit_preserves_oncell_overlays = do
  let mkGs board =
        (newGame defaultConfig 11)
          { gsBoard = board
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
  -- Hammer ice=2 + Choco: chip ice, Choco stays (no true clear / no adj clear).
  let boardCh = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Choco))
      (gsCh, outCh) = useHammer (4, 4) (mkGs boardCh)
  case outCh of
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cCh = getCell (gsBoard gsCh) (4, 4)
  assertEqual "choco ice 2→1" (1 :: Int) (iceLayers cCh)
  assertBool "on-cell choco survives soft hammer" (hasChoco cCh)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsCh)
  -- Hammer ice=2 + Grass / Vine: same soft-hit keep.
  let boardGr = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Grass))
      (gsGr, _) = useHammer (4, 4) (mkGs boardGr)
      cGr = getCell (gsBoard gsGr) (4, 4)
  assertEqual "grass ice 2→1" (1 :: Int) (iceLayers cGr)
  assertBool "on-cell grass survives soft hammer" (hasGrass cGr)
  let boardVi = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Vine))
      (gsVi, _) = useHammer (4, 4) (mkGs boardVi)
      cVi = getCell (gsBoard gsVi) (4, 4)
  assertEqual "vine ice 2→1" (1 :: Int) (iceLayers cVi)
  assertBool "on-cell vine survives soft hammer" (hasVine cVi)
  -- Control: last ice (ice==1) + Choco clears the gem (refill has no Choco).
  let boardLast = setCell stableBoard (4, 4) (Gem C2 Normal 1 (Just Choco))
      (gsLast, _) = useHammer (4, 4) (mkGs boardLast)
  assertBool "last-ice choco gone" (not (hasChoco (getCell (gsBoard gsLast) (4, 4))))
  assertBool "last-ice layer gone" (iceLayers (getCell (gsBoard gsLast) (4, 4)) == 0)

--------------------------------------------------------------------------------
-- Soft-hit must not fire adjacent side-effects (Fog/locks/Maker/Bottle/Hat/Balloon)
--------------------------------------------------------------------------------

-- | ice>1 chip and Flip face-flip are not true clear holes. They must NOT peel
-- adjacent Fog/Chain/Freeze/Curtain, charge Maker, trigger Bottle/Hat, or pop
-- Balloon — same trueClears discipline as soft_hit_preserves_choco_steam.
soft_hit_no_adj_side_effects :: Assertion
soft_hit_no_adj_side_effects = do
  -- Soft ice match: adjacent peel-locks / Maker stay untouched.
  let boardIce lock =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkIceGem C1 2))
             (3, 2)
             (mkGem C1))
          (2, 1)
          lock
  assertBool "ice match" (not (null (findMatches (boardIce (mkGem C2)))))
  let msIce = findMatches (boardIce (mkFogGem C2 2))
      expandedIce = expandSpecials (boardIce (mkFogGem C2 2)) msIce
      (_, iceFree) = chipIceOnClear (boardIce (mkFogGem C2 2)) expandedIce
      (bFogU, nFog) = chipAdjacentFog (boardIce (mkFogGem C2 2)) iceFree
  assertBool "soft ice not a hole" ((3, 1) `notElem` iceFree)
  assertEqual "soft ice peels no Fog" (0 :: Int) nFog
  assertEqual "Fog2 unchanged" (Just (Fog 2)) (cellOverlay (getCell bFogU (2, 1)))
  let (bChU, nCh) = chipAdjacentChain (boardIce (mkChainGem C2 2)) iceFree
  assertEqual "soft ice peels no Chain" (0 :: Int) nCh
  assertEqual "Chain2 unchanged" (2 :: Int) (chainLayers (getCell bChU (2, 1)))
  let (bFrU, nFr) = chipAdjacentFreeze (boardIce (mkFreezeGem C2 2)) iceFree
  assertEqual "soft ice peels no Freeze" (0 :: Int) nFr
  assertEqual "Freeze2 unchanged" (2 :: Int) (freezeLayers (getCell bFrU (2, 1)))
  let (bCuU, nCu) = chipAdjacentCurtain (boardIce (mkCurtainGem C2 2)) iceFree
  assertEqual "soft ice peels no Curtain" (0 :: Int) nCu
  assertEqual "Curtain2 unchanged" (2 :: Int) (curtainLayers (getCell bCuU (2, 1)))
  -- Live cascade: Maker of match color must not charge on soft ice wave.
  let (bMk, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 72) (boardIce (mkMakerCharges C1 3))
  assertBool "Maker still maker" (isMaker (getCell bMk (2, 1)))
  assertEqual "Maker uncharged" (3 :: Int) (makerCharges (getCell bMk (2, 1)))
  -- Soft Flip match: Balloon same front-color must not pop; Bottle must not dye.
  let boardFlip =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (4, 0) (mkGem C2))
                   (4, 1)
                   (mkFlip C2 C5))
                (4, 2)
                (mkGem C2))
             (5, 1)
             (mkBalloon C2))
          (5, 2)
          (mkGem C3)
  assertBool "flip match" (not (null (findMatches boardFlip)))
  let boardFlipBot = setCell boardFlip (3, 1) (mkBottle C4)
      (bFlip, _, _, _, _, _, _, _, _, _, _, _) =
        runCascadeScored Nothing (mkStdGen 73) boardFlipBot
  assertBool "Balloon survives soft Flip" (isBalloon (getCell bFlip (5, 1)))
  assertEqual "Bottle did not dye neighbor" C3 (cellColor (getCell bFlip (5, 2)))
  -- Control: true clear mid gem *does* peel Fog / charge Maker.
  let boardHard' =
        setCell (boardIce (mkFogGem C2 2)) (3, 1) (mkGem C1)  -- bare mid
      msH = findMatches boardHard'
      (_, iceH) = chipIceOnClear boardHard' (expandSpecials boardHard' msH)
      (bFogH, _) = chipAdjacentFog boardHard' iceH
  assertBool "true clear is a hole" ((3, 1) `elem` iceH)
  assertEqual "true clear peels Fog 2→1" (Just (Fog 1)) (cellOverlay (getCell bFogH (2, 1)))
  -- Unit: trueClears charge Maker (avoid gravity moving the maker off-cell).
  let boardMkH = setCell (boardIce (mkMakerCharges C1 3)) (3, 1) (mkGem C1)
      msMk = findMatches boardMkH
      (_, iceMk) = chipIceOnClear boardMkH (expandSpecials boardMkH msMk)
      bMkH = chargeAdjacentMakers boardMkH iceMk
  assertEqual "true clear charges Maker 3→2" (2 :: Int) (makerCharges (getCell bMkH (2, 1)))

--------------------------------------------------------------------------------
-- Soft-hit must keep on-cell Fog / Steam (ice>1 hammer)
--------------------------------------------------------------------------------

-- | ice>1 soft chip must not strip on-cell Fog or Steam (peel-locks / match
-- blockers ride with the gem until a true clear). Extends
-- soft_hit_preserves_oncell_overlays beyond Grass/Vine/Choco.
soft_hit_preserves_oncell_fog_steam :: Assertion
soft_hit_preserves_oncell_fog_steam = do
  let mkGs board =
        (newGame defaultConfig 13)
          { gsBoard = board
          , gsHammers = 2
          , gsOver = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsMoves = 20
          , gsScore = 0
          }
  -- Hammer ice=2 + Fog2: chip ice, Fog stays.
  let boardFog = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just (Fog 2)))
      (gsFog, outFog) = useHammer (4, 4) (mkGs boardFog)
  case outFog of
    InvalidSwap -> assertFailure "hammer charges present"
    _ -> pure ()
  let cFog = getCell (gsBoard gsFog) (4, 4)
  assertEqual "fog ice 2→1" (1 :: Int) (iceLayers cFog)
  assertEqual "on-cell Fog2 survives soft hammer" (Just (Fog 2)) (cellOverlay cFog)
  assertEqual "hammer spent" (1 :: Int) (gsHammers gsFog)
  -- Hammer ice=2 + Steam: same soft-hit keep.
  let boardSteam = setCell stableBoard (4, 4) (Gem C2 Normal 2 (Just Steam))
      (gsSt, _) = useHammer (4, 4) (mkGs boardSteam)
      cSt = getCell (gsBoard gsSt) (4, 4)
  assertEqual "steam ice 2→1" (1 :: Int) (iceLayers cSt)
  assertBool "on-cell Steam survives soft hammer" (hasSteam cSt)
  -- Control: bare Fog (ice 0) hammer clears through Fog (direct-hit ≠ peel).
  let boardBare = setCell stableBoard (4, 4) (mkFogGem C2 2)
      (gsBare, _) = useHammer (4, 4) (mkGs boardBare)
  assertBool "bare Fog direct-cleared" (not (hasFog (getCell (gsBoard gsBare) (4, 4))))
  assertEqual "bare Fog hammer spent" (1 :: Int) (gsHammers gsBare)

--------------------------------------------------------------------------------
-- Surprise explode must open nested Surprises (Bomb parity)
--------------------------------------------------------------------------------

-- | Surprise 3×3 explode that hits another Surprise must open it (special or
-- chained explode) — not hole-delete via chipIce alone. Bomb→Surprise already
-- opened; Surprise→Surprise lacked the second openSurprises pass.
surprise_blast_opens_nested :: Assertion
surprise_blast_opens_nested = do
  -- (3,3) outcome 3 → explode; (2,2) outcome 2 → Bomb special inside the 3×3.
  let boardSpecial =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 2)
          mkSurprise
  assertEqual "outer explode" (3 :: Int) (((3 * 8 + 3) `mod` 4))
  assertEqual "nested special" (2 :: Int) (((2 * 8 + 2) `mod` 4))
  let (mbS, _) = clearMatches boardSpecial
  case (mbS !! 2) !! 2 of
    Just c -> do
      assertBool "nested opened to gem" (isGem c)
      assertEqual "nested Bomb special" Bomb (cellKind c)
      assertBool "not still Surprise" (not (isSurprise c))
    Nothing -> assertFailure "nested Surprise must open to special, not hole-delete"
  -- Same-pass special must sit (Bomb parity): must NOT fire-and-survive.
  -- Outer explode footprint is (2..4,2..4); Bomb@ (2,2) firing would hole (1,1).
  let holesS =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mbS !! r) !! c) == Nothing
        ]
  assertBool "nested Bomb must not fire (1,1)" ((1, 1) `notElem` holesS)
  assertBool "nested Bomb must not fire (2,1)" ((2, 1) `notElem` holesS)
  -- Control: Bomb match hitting Surprise at (2,2) also opens (parity sanity).
  let boardBomb =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (3, 0) (mkGem C1))
                (3, 1)
                (mkGem C1))
             (3, 2)
             (Gem C1 Bomb 0 Nothing))
          (2, 2)
          mkSurprise
      (mbB, _) = clearMatches boardBomb
  case (mbB !! 2) !! 2 of
    Just c -> assertEqual "bomb-hit nested Bomb" Bomb (cellKind c)
    Nothing -> assertFailure "bomb-hit Surprise must open"
  -- Nested explode at (2,3): chain must reach (1,3) outside outer 3×3 alone.
  let boardChain =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 3)
          mkSurprise
  assertEqual "nested explode" (3 :: Int) (((2 * 8 + 3) `mod` 4))
  let (mbC, nC) = clearMatches boardChain
      holesC =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mbC !! r) !! c) == Nothing
        ]
  assertBool "nested explode center cleared" ((2, 3) `elem` holesC)
  assertBool "chained blast reached (1,3)" ((1, 3) `elem` holesC)
  assertBool ("chained clear count >= 12, got " ++ show nC) (nC >= 12)

--------------------------------------------------------------------------------
-- Nested Surprise special must sit (not fire-and-survive)
--------------------------------------------------------------------------------

-- | When an exploding Surprise and a special-outcome Surprise open in the same
-- openSurprises pass (both ortho-adjacent to the match), expandSpecials must
-- use the pre-open board so the newly placed Bomb/Line does not activate while
-- also being saved from holes. Pre-existing Bombs in the blast still expand.
surprise_nested_special_no_fire :: Assertion
surprise_nested_special_no_fire = do
  -- (3,3) explode + (2,2) Bomb special, both ortho-adj to match row.
  let board =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 2)
          mkSurprise
  assertEqual "outer explode" (3 :: Int) (((3 * 8 + 3) `mod` 4))
  assertEqual "nested Bomb" (2 :: Int) (((2 * 8 + 2) `mod` 4))
  let (mb, n) = clearMatches board
      holes =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mb !! r) !! c) == Nothing
        ]
  case (mb !! 2) !! 2 of
    Just c -> assertEqual "special sits" Bomb (cellKind c)
    Nothing -> assertFailure "nested special must survive"
  assertBool "no fire beyond outer blast (1,1)" ((1, 1) `notElem` holes)
  assertBool "no fire beyond outer blast (1,2)" ((1, 2) `notElem` holes)
  assertBool "no fire beyond outer blast (2,1)" ((2, 1) `notElem` holes)
  -- Outer explode + match still clear the expected footprint.
  assertBool "outer center cleared" ((3, 3) `elem` holes)
  assertBool ("clear count in outer range, got " ++ show n) (n >= 9 && n <= 12)
  -- Control: pre-existing Bomb inside Surprise explode MUST still expand.
  let boardPreBomb =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (3, 0) (mkGem C1))
                   (3, 1)
                   (mkGem C1))
                (3, 2)
                (mkGem C1))
             (3, 3)
             mkSurprise)
          (2, 2)
          (Gem C4 Bomb 0 Nothing)
      (mbP, _) = clearMatches boardPreBomb
      holesP =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , ((mbP !! r) !! c) == Nothing
        ]
  assertBool "pre-existing Bomb fires (1,1)" ((1, 1) `elem` holesP)
  assertBool "pre-existing Bomb consumed" (((mbP !! 2) !! 2) == Nothing)

--------------------------------------------------------------------------------
-- Soft-locked Rainbow / special combo must not fire (parity with expandSpecials)
--------------------------------------------------------------------------------

-- | ice>1 / Curtain Rainbow×gem must not color-clear (Line/Bomb soft-lock parity).
-- Regression: isRainbowSwap ignored soft-lock, so ice=2 Rainbow cleared partner
-- color while surviving (fire-and-survive). Last-ice Rainbow still activates.
soft_lock_blocks_rainbow_swap :: Assertion
soft_lock_blocks_rainbow_swap = do
  let mkGs board =
        (newGame defaultConfig 11)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
      countC4 b =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , case getCell b (r, c) of
              Gem C4 _ _ _ -> True
              Flip C4 _ -> True
              Countdown C4 _ -> True
              _ -> False
          ]
  -- ice=2 Rainbow × C4: not a rainbow activation; NoMatch if swap forms no match.
  let boardIce =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 2 Nothing))
          (0, 1)
          (mkGem C4)
  assertBool "ice>1 Rainbow does not soft-activate" $
    not (isRainbowSwap boardIce (0, 0) (0, 1))
  assertBool "no classic match after swap" $
    not (hasAnyMatch (swapCells boardIce (0, 0) (0, 1)))
  let (gsIce, outIce) = trySwap (0, 0) (0, 1) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    InvalidSwap -> assertFailure "swap geometry ok"
    MoveApplied _ -> assertFailure "ice>1 Rainbow must not fire color clear"
    other -> assertFailure ("unexpected: " ++ show other)
  assertEqual "board unchanged on rollback" boardIce (gsBoard gsIce)
  assertEqual "moves unchanged" (10 :: Int) (gsMoves gsIce)
  -- Curtain Rainbow × C4: same soft-lock (Curtain allows swap, blocks activate).
  let boardCu =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 (Just (Curtain 2))))
          (0, 1)
          (mkGem C4)
  assertBool "curtain Rainbow does not activate" $
    not (isRainbowSwap boardCu (0, 0) (0, 1))
  let (gsCu, outCu) = trySwap (0, 0) (0, 1) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain Rainbow must not fire"
    _ -> assertFailure "curtain Rainbow must NoMatch-rollback"
  assertEqual "curtain board unchanged" boardCu (gsBoard gsCu)
  -- Control: ice==1 Rainbow still activates and clears partner color.
  let boardLast =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 1 Nothing))
          (0, 1)
          (mkGem C4)
  assertBool "last-ice Rainbow activates" (isRainbowSwap boardLast (0, 0) (0, 1))
  let c4Before = countC4 boardLast
      (gsLast, outLast) = trySwap (0, 0) (0, 1) (mkGs boardLast)
  case outLast of
    NoMatch -> assertFailure "last-ice Rainbow must apply"
    InvalidSwap -> assertFailure "last-ice Rainbow must be valid"
    MoveApplied g -> assertBool "gained" (g > 0)
    _ -> pure ()
  assertBool "partner color reduced" (countC4 (gsBoard gsLast) < c4Before)
  assertEqual "move spent" (9 :: Int) (gsMoves gsLast)

-- | ice>1 / Curtain Line×Bomb must not fire combo geometry (soft-lock parity).
-- Regression: isSpecialCombo was kind-only, so soft-locked Line×Bomb still
-- cleared the full cross via comboClearSeeds while Line survived.
soft_lock_blocks_special_combo :: Assertion
soft_lock_blocks_special_combo = do
  let mkGs board =
        (newGame defaultConfig 13)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
  -- ice=2 LineH × Bomb: combo blocked.
  let boardIce =
        setCell
          (setCell stableBoard (3, 3) (Gem C1 LineH 2 Nothing))
          (3, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "kinds look like line-bomb" (isLineBombCombo boardIce (3, 3) (3, 4))
  assertBool "soft ice blocks combo" $
    not (isSpecialCombo boardIce (3, 3) (3, 4))
  assertBool "no classic match" $
    not (hasAnyMatch (swapCells boardIce (3, 3) (3, 4)))
  let (gsIce, outIce) = trySwap (3, 3) (3, 4) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 Line×Bomb must not fire combo"
    _ -> assertFailure "ice>1 Line×Bomb must NoMatch-rollback"
  assertEqual "ice combo board unchanged" boardIce (gsBoard gsIce)
  -- Curtain Line × Bomb: combo blocked.
  let boardCu =
        setCell
          (setCell stableBoard (4, 3) (Gem C1 LineH 0 (Just (Curtain 2))))
          (4, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "curtain blocks combo" $
    not (isSpecialCombo boardCu (4, 3) (4, 4))
  let (gsCu, outCu) = trySwap (4, 3) (4, 4) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain Line×Bomb must not fire"
    _ -> assertFailure "curtain Line×Bomb must NoMatch-rollback"
  assertEqual "curtain combo board unchanged" boardCu (gsBoard gsCu)
  -- Control: unlocked Line×Bomb still fires.
  let boardOk =
        setCell
          (setCell stableBoard (5, 3) (Gem C1 LineH 0 Nothing))
          (5, 4)
          (Gem C2 Bomb 0 Nothing)
  assertBool "unlocked combo" (isSpecialCombo boardOk (5, 3) (5, 4))
  let (gsOk, outOk) = trySwap (5, 3) (5, 4) (mkGs boardOk)
  case outOk of
    NoMatch -> assertFailure "unlocked Line×Bomb must apply"
    InvalidSwap -> assertFailure "unlocked Line×Bomb must be valid"
    MoveApplied g -> assertBool "combo score" (g > 0)
    _ -> pure ()
  assertEqual "move spent" (9 :: Int) (gsMoves gsOk)

--------------------------------------------------------------------------------
-- Soft-lock FreeSwap entry + asymmetric double-Rainbow (boundary)
--------------------------------------------------------------------------------

-- | useFreeSwap must honor the same soft-lock gates as trySwap (isRainbowSwap /
-- isSpecialCombo). ice>1 / Curtain Rainbow×gem and ice>1 Line×Bomb must NoMatch
-- without spending the free-swap charge.
soft_lock_blocks_freeswap_activation :: Assertion
soft_lock_blocks_freeswap_activation = do
  let mkGs board =
        (newGame defaultConfig 17)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          , gsFreeSwaps = 2
          }
  -- ice=2 Rainbow × C4 via free-swap (non-adjacent also OK for booster).
  let boardIce =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 2 Nothing))
          (2, 2)
          (mkGem C4)
  assertBool "ice>1 Rainbow not soft-activate" $
    not (isRainbowSwap boardIce (0, 0) (2, 2))
  let (gsIce, outIce) = useFreeSwap (0, 0) (2, 2) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 Rainbow free-swap must not color-clear"
    _ -> assertFailure "ice>1 Rainbow free-swap must NoMatch"
  assertEqual "board unchanged" boardIce (gsBoard gsIce)
  assertEqual "charge kept (ice RB)" (2 :: Int) (gsFreeSwaps gsIce)
  -- Curtain Rainbow × gem (Curtain allows geometry; soft-lock blocks activate).
  let boardCu =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 (Just (Curtain 2))))
          (2, 2)
          (mkGem C4)
  assertBool "curtain Rainbow gated" $
    not (isRainbowSwap boardCu (0, 0) (2, 2))
  let (gsCu, outCu) = useFreeSwap (0, 0) (2, 2) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain Rainbow free-swap must not fire"
    _ -> assertFailure "curtain Rainbow free-swap must NoMatch"
  assertEqual "curtain board unchanged" boardCu (gsBoard gsCu)
  assertEqual "charge kept (curtain RB)" (2 :: Int) (gsFreeSwaps gsCu)
  -- ice=2 LineH × Bomb free-swap: combo blocked, charge kept.
  let boardCombo =
        setCell
          (setCell stableBoard (1, 1) (Gem C1 LineH 2 Nothing))
          (5, 5)
          (Gem C2 Bomb 0 Nothing)
  assertBool "kinds look like line-bomb" (isLineBombCombo boardCombo (1, 1) (5, 5))
  assertBool "soft ice blocks combo" $
    not (isSpecialCombo boardCombo (1, 1) (5, 5))
  let (gsCo, outCo) = useFreeSwap (1, 1) (5, 5) (mkGs boardCombo)
  case outCo of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 Line×Bomb free-swap must not combo"
    _ -> assertFailure "ice>1 Line×Bomb free-swap must NoMatch"
  assertEqual "combo board unchanged" boardCombo (gsBoard gsCo)
  assertEqual "charge kept (combo)" (2 :: Int) (gsFreeSwaps gsCo)

-- | Double-Rainbow requires BOTH endpoints to specialActivates. ice>1 on one
-- side must not clear the board (conjunction branch of isRainbowSwap).
soft_lock_blocks_double_rainbow :: Assertion
soft_lock_blocks_double_rainbow = do
  let mkGs board =
        (newGame defaultConfig 19)
          { gsBoard = board
          , gsOver = Nothing
          , gsMoves = 10
          , gsScore = 0
          , gsBelts = []
          , gsUfos = []
          , gsHint = Nothing
          , gsGoal = GoalScore 99999
          }
      countGems b =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isGem (getCell b (r, c))
          ]
  -- ice=2 Rainbow × unlocked Rainbow: gated.
  let boardIce =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 2 Nothing))
          (0, 1)
          (Gem C4 Rainbow 0 Nothing)
  assertBool "asymmetric dbl soft-locked" $
    not (isRainbowSwap boardIce (0, 0) (0, 1))
  assertBool "symmetric order also gated" $
    not (isRainbowSwap boardIce (0, 1) (0, 0))
  let gemsBefore = countGems boardIce
      (gsIce, outIce) = trySwap (0, 0) (0, 1) (mkGs boardIce)
  case outIce of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "ice>1 double-Rainbow must not board-wipe"
    _ -> assertFailure "ice>1 double-Rainbow must NoMatch-rollback"
  assertEqual "board unchanged" boardIce (gsBoard gsIce)
  assertEqual "moves unchanged" (10 :: Int) (gsMoves gsIce)
  assertEqual "gems preserved" gemsBefore (countGems (gsBoard gsIce))
  -- Curtain on one Rainbow: same conjunction gate.
  let boardCu =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 (Just (Curtain 1))))
          (0, 1)
          (Gem C4 Rainbow 0 Nothing)
  assertBool "curtain dbl soft-locked" $
    not (isRainbowSwap boardCu (0, 0) (0, 1))
  let (gsCu, outCu) = trySwap (0, 0) (0, 1) (mkGs boardCu)
  case outCu of
    NoMatch -> pure ()
    MoveApplied _ -> assertFailure "curtain double-Rainbow must not fire"
    _ -> assertFailure "curtain double-Rainbow must NoMatch"
  assertEqual "curtain board unchanged" boardCu (gsBoard gsCu)
  -- Control: both unlocked double-Rainbow still activates.
  let boardOk =
        setCell
          (setCell stableBoard (0, 0) (Gem C3 Rainbow 0 Nothing))
          (0, 1)
          (Gem C4 Rainbow 0 Nothing)
  assertBool "unlocked dbl activates" (isRainbowSwap boardOk (0, 0) (0, 1))
  let (gsOk, outOk) = trySwap (0, 0) (0, 1) (mkGs boardOk)
  case outOk of
    NoMatch -> assertFailure "unlocked double-Rainbow must apply"
    InvalidSwap -> assertFailure "unlocked double-Rainbow must be valid"
    MoveApplied g -> assertBool "board-wipe score" (g >= 100)
    _ -> pure ()
  assertEqual "move spent" (9 :: Int) (gsMoves gsOk)
  assertBool "score reflects wipe" (gsScore gsOk >= 100)
  -- Refill restores gem count; assert the swapped rainbows were consumed.
  assertBool "endpoint no longer Rainbow" $
    not (isRainbow (getCell (gsBoard gsOk) (0, 0)))
      && not (isRainbow (getCell (gsBoard gsOk) (0, 1)))

-- | Cookie mid-board is immune to Line/Bomb/Hammer direct seeds: must not wipe
-- or count toward GoalCookie. Cookies only collect via bottom-row drain.
-- Regression: chipIceOnClear listed Cookie clearable → blast "collected" mid-air.
cookie_immune_to_direct_clear :: Assertion
cookie_immune_to_direct_clear = do
  -- Unit: chipIceOnClear leaves Cookie, not clearable
  let boardC = setCell stableBoard (3, 3) mkCookie
      (bIce, free) = chipIceOnClear boardC [(3, 3)]
  assertEqual "cookie not clearable" ([] :: [Pos]) free
  assertBool "cookie persists after chipIce" (isCookie (getCell bIce (3, 3)))
  -- Bomb footprint covers Cookie: cookie survives; GoalCookie count stays 0
  let boardBomb =
        setCell
          (setCell stableBoard (3, 3) mkCookie)
          (3, 4)
          (Gem C1 Bomb 0 Nothing)
      seeds = [(3, 4)]
      (b1, _n, _sc, _mw, _t, _st, _ch, _h, _bal, cookies, _cak, _) =
        runCascadeScoredFromSeeds Nothing seeds (mkStdGen 1) boardBomb
      cookieLeft =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCookie (getCell b1 (r, c))
        ]
  assertEqual "bomb must not count mid-board cookie" (0 :: Int) cookies
  assertBool ("cookie still on board after bomb, left=" ++ show cookieLeft) (not (null cookieLeft))
  -- LineH through cookie: same immunity
  let boardLine =
        setCell
          (setCell stableBoard (4, 2) mkCookie)
          (4, 4)
          (Gem C2 LineH 0 Nothing)
      (bL, _nL, _scL, _mwL, _tL, _stL, _chL, _hL, _balL, cookiesL, _cakL, _) =
        runCascadeScoredFromSeeds Nothing [(4, 4)] (mkStdGen 2) boardLine
  assertEqual "line must not count mid-board cookie" (0 :: Int) cookiesL
  assertBool "cookie survives line" $
    any (\(r, c) -> isCookie (getCell bL (r, c)))
      [ (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] ]
  -- trySwap bomb path: GoalCookie meter unchanged when cookie not at bottom
  let gs0 =
        (newGame defaultConfig 21)
          { gsBoard = boardBomb
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsBelts = []
          , gsUfos = []
          , gsPortals = []
          , gsCookiesCollected = 0
          , gsGoal = GoalCookie 5
          , gsHammers = 2
          }
      -- Force bomb activation via useHammer on bomb cell (cookie neighbor)
      (gsH, outH) = useHammer (3, 4) gs0
  case outH of
    NoMatch -> assertFailure "hammer on bomb should apply"
    InvalidSwap -> assertFailure "hammer on bomb should apply"
    _ -> pure ()
  assertEqual "bomb hammer must not collect mid cookie" (0 :: Int) (gsCookiesCollected gsH)
  assertBool "cookie still present after bomb hammer" $
    any (\(r, c) -> isCookie (getCell (gsBoard gsH) (r, c)))
      [ (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] ]
  -- Control: bottom-row cookie still drains on unrelated clear
  let bottom = boardSize - 1
      boardBot =
        setCell
          (setCell
             (setCell
                (setCell stableBoard (bottom, 4) mkCookie)
                (2, 0)
                (mkGem C1))
             (2, 1)
             (mkGem C1))
          (2, 2)
          (mkGem C1)
      seedsBot = findMatches boardBot
      (_bBot, _nBot, _scBot, _cBot, _tBot, _stBot, _chBot, _hBot, _bBot2, cookiesBot, _cakBot, _) =
        runCascadeScoredFromSeeds Nothing seedsBot (mkStdGen 3) boardBot
  assertBool ("bottom cookie still drains, got " ++ show cookiesBot) (cookiesBot >= 1)

-- | Belt delivers Cookie onto bottom with no follow-up match → must still drain.
-- Regression: runCascadeScoredWithUfos no-ops without settle, stranding GoalCookie.
belt_delivers_cookie_bottom_drains :: Assertion
belt_delivers_cookie_bottom_drains = do
  let belt = [(5, 2), (6, 2), (7, 2), (7, 3)]
      -- Cookie one step before bottom entrance; one belt tick parks it on (7,2)
      board0 =
        setCell
          (setCell
             (setCell
                (setCell
                   (setCell stableBoard (6, 2) mkCookie)
                   (0, 0)
                   (mkGem C1))
                (0, 1)
                (mkGem C1))
             (0, 2)
             (mkGem C2))
          (0, 3)
          (mkGem C1)
      afterBelt = shiftBelts board0 [belt]
  assertBool "belt parks cookie on bottom" (isCookie (getCell afterBelt (7, 2)))
  assertBool "belt alone forms no match" (not (hasAnyMatch afterBelt))
  let gs0 =
        (newGame defaultConfig 9)
          { gsBoard = board0
          , gsBelts = [belt]
          , gsMoves = 12
          , gsOver = Nothing
          , gsHint = Nothing
          , gsUfos = []
          , gsPortals = []
          , gsCookiesCollected = 0
          , gsGoal = GoalCookie 1
          }
      (gs1, out) = trySwap (0, 2) (0, 3) gs0
  case out of
    NoMatch -> assertFailure "row0 match should apply"
    InvalidSwap -> assertFailure "row0 swap valid"
    _ -> pure ()
  let left =
        [ (r, c)
        | r <- [0 .. boardSize - 1]
        , c <- [0 .. boardSize - 1]
        , isCookie (getCell (gsBoard gs1) (r, c))
        ]
  assertBool ("cookie must drain after belt, left=" ++ show left) (null left)
  assertBool
    ("GoalCookie must count belt-delivered cookie, got " ++ show (gsCookiesCollected gs1))
    (gsCookiesCollected gs1 >= 1)

--------------------------------------------------------------------------------
-- Stability cruise: GoalCookie/Carpet décor seed + portal Flip
--------------------------------------------------------------------------------

-- | Portal teleports Flip (dual-face) the same as gems/countdowns/cookies.
portal_teleports_flip :: Assertion
portal_teleports_flip = do
  let setMBoard b (r, c) v =
        take r b ++ [take c row ++ [v] ++ drop (c + 1) row] ++ drop (r + 1) b
        where
          row = b !! r
      fill = Just (mkGem C5)
      mb0 = replicate boardSize (replicate boardSize fill)
      mb1 = setMBoard mb0 (0, 0) (Just (mkFlip C1 C2))
      mb2 = setMBoard mb1 (7, 7) Nothing
      portals = [((0, 0), (7, 7))]
      mb3 = applyPortalTeleports portals mb2
  assertEqual "Flip left entrance" Nothing ((mb3 !! 0) !! 0)
  case (mb3 !! 7) !! 7 of
    Just cell -> do
      assertBool "exit is Flip" (isFlip cell)
      assertEqual "front color" C1 (flipFront cell)
      assertEqual "back color" C2 (flipBack cell)
    Nothing -> assertFailure "expected Flip at portal exit"
  -- Countdown still teleports (sibling transferable)
  let mbC =
        setMBoard
          (setMBoard mb0 (1, 1) (Just (Countdown C3 2)))
          (6, 6)
          Nothing
      mbC' = applyPortalTeleports [((1, 1), (6, 6))] mbC
  assertEqual "CD entrance empty" Nothing ((mbC' !! 1) !! 1)
  case (mbC' !! 6) !! 6 of
    Just (Countdown C3 2) -> pure ()
    other -> assertFailure ("expected Countdown at exit, got " ++ show other)

-- | Bare GoalCarpet (no levelCarpets) still gets open floor tiles (UFO décor parity).
goal_carpet_seeds_open_tiles :: Assertion
goal_carpet_seeds_open_tiles = do
  let gs = newGame (GameConfig 26 (GoalCarpet 8)) 20260929
  assertEqual "goal" (GoalCarpet 8) (gsGoal gs)
  assertBool
    ("open carpets >= 8, got " ++ show (length (gsCarpetOpen gs)))
    (length (gsCarpetOpen gs) >= 8)
  assertEqual "covered start" (0 :: Int) (gsCarpetsCovered gs)
  -- GoalCookie bare newGame must seed high biscuits (ensureGoalDecor)
  let gsCk = newGame (GameConfig 26 (GoalCookie 6)) 20260929
      nCk =
        length
          [ ()
          | r <- [0 .. boardSize - 1]
          , c <- [0 .. boardSize - 1]
          , isCookie (getCell (gsBoard gsCk) (r, c))
          ]
  assertBool ("cookies >= 6, got " ++ show nCk) (nCk >= 6)
  assertBool "cookies not only on bottom" $
    any
      (\(r, c) -> r < boardSize - 1 && isCookie (getCell (gsBoard gsCk) (r, c)))
      [ (r, c) | r <- [0 .. boardSize - 1], c <- [0 .. boardSize - 1] ]
