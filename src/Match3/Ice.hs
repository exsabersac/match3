-- | 宝石冰层（Int）：匹配/清除种子时削一层；末层同波清除宝石。
-- 与 overlay 火箭冰冻 Freeze（只挡交换）不同。
--
-- 第二刀 2b：直接命中改由元素框架按层结算（冰层 → 叠层 → 本体，见 Match3.Element.World.directHitWith，
-- 各层反应在 Match3.Element.Builtin）；chipIceOnClear 保留为内置元素世界上的同名入口。
module Match3.Ice
  ( chipIceOnClear
  , iceLayers
  , mkIceGem
  ) where

import Match3.Element.Builtin (defaultWorld)
import Match3.Element.World (chipOnHitWith)
import Match3.Types

-- | Chip one ice layer on each seed / handle direct-hit peel locks.
-- ice>1: decrement, keep gem; ice==1: last layer + gem clear.
-- ice==0 + Chain/Curtain: peel one lock layer (gem stays) — hammer/cross/line.
-- ice==0 bare/Freeze/Fog: gem clears. Layered blockers (Stone/Chest/Honey/Cake/Safe)
-- chip one layer per direct hit (hammer/cross/line/bomb); last layer clears
-- (Safe opens to Cookie). MagicHat/Maker/Snail/Bottle/Cookie are immune (persist);
-- cookies only collect via bottom-row drain, never mid-board blast wipe.
chipIceOnClear :: Board -> [Pos] -> (Board, [Pos])
chipIceOnClear = chipOnHitWith defaultWorld
