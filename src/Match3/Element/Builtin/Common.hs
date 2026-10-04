-- | 内置元素的共用小件：被两个以上分组文件用到的规则 / 放置辅助函数。
-- 只在一个分组里用到的辅助函数留在该分组文件里（如 Obstacle 的 chip / layersPlace、Layer 的 peel / layerChip）。
module Match3.Element.Builtin.Common
  ( deadRule
  , colorPlace
    -- * 按盘面散列选格（毛球跳格 / 雪怪召唤）
  , boardSeed
  , posSeed
  , pickBy
  , plainGem
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty (..))
import Match3.Element.Types
import Match3.Types

-- | 邻消规则：只削层 / 打碎，打碎的格并入清除格（石头 / 宝箱 / 蜂蜜 / 蛋糕 / 气球 / 时间精灵）。
deadRule :: (Board -> [Pos] -> [Pos] -> (Board, [Pos])) -> AdjCtx -> Board -> AdjOut
deadRule f ctx b = let (b', dead) = f b (acTrue ctx) (acDirect ctx) in AdjOut b' dead []

-- | 放置：一个颜色参数（气球 / 染色瓶；精确匹配）。
colorPlace :: (Color -> Cell) -> Placer
colorPlace con args _ = con <$> exactArgs argColor args

--------------------------------------------------------------------------------
-- 按盘面散列选格
--
-- 毛球跳格（Actor.fuzzballJumps，第 43 关）与雪怪召唤（Obstacle.snowBossSpawn，第 45 关）不用随机数生成器，
-- 而是用「盘面 + 坐标」的散列在候选格里挑一格：同一盘面总挑同一格，回放、撤销、原生与 wasm 都一致。
--
-- **隐藏依赖：散列的输入是 'show' 的字符串。** 'boardSeed' 散列的是 @show board@，也就是
-- 'Board'（Grid）与 'CellContents' / 'Color' / 'GemKind' / 'CellOverlay' 等派生 'Show' 实例的输出；
-- 'posSeed' 散列的是 @show (r, c)@。所以给 'CellContents' 加 / 改构造器、调字段顺序、把某个字段换成别的类型
-- （例如 @Snail Int Int@ 换成方向），或改 'Grid' 的 'Show'，都会**改变第 43 / 45 关的对局**。
-- 测试 Spec.BoardSeed 钉住了几张固定盘面的散列值，Show 一变就会失败；那时要么保住旧的 Show 输出，
-- 要么确认这是有意的玩法变化并重录金标准。散列算法（64 位 FNV-1a）也不能换。

-- | 64 位 FNV-1a（偏移基 14695981039346656037、素数 1099511628211、模 2^64），逐字符取 'ord'。
fnv :: String -> Integer
fnv = foldl' (\h ch -> ((h `xor` fromIntegral (ord ch)) * 1099511628211) `mod` 18446744073709551616) 14695981039346656037

-- | 盘面的种子 = @fnv (show board)@（依赖派生 Show，见上）。
boardSeed :: Board -> Integer
boardSeed = fnv . show

-- | 坐标的种子 = @fnv (show (r, c))@。
posSeed :: Pos -> Integer
posSeed = fnv . show

-- | 按散列值在非空候选里挑一个：下标 = 散列 mod 候选数（候选按调用方给的顺序）。
pickBy :: Integer -> NonEmpty a -> a
pickBy h (x :| xs) =
  case drop (fromIntegral (h `mod` fromIntegral (length xs + 1))) (x : xs) of
    y : _ -> y
    [] -> x -- 不会发生：下标 < 候选数

-- | 普通宝石（无特效、无冰、无叠层）：毛球能跳去、雪块能召唤到的格。
plainGem :: Cell -> Bool
plainGem cell = case cell of
  Gem _ Normal 0 Nothing -> True
  _ -> False
