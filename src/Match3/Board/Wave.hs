-- | 连锁的回放数据：一轮（「消除 → 下落 → 补子」）的记录 'CascadeWave'（第 3 项从 Match3.Board.Cascade 拆出，
-- Cascade 原样再导出，对外不变）。拆出来是因为效果层（Match3.Board.Effect）的「发出一轮」能力要用到这个类型，
-- 而 Cascade 又依赖效果层。
--
-- 依赖：Board.Grid（MBoard 的行列表视图）、Types。不变量：Show 与派生的 Show 逐字相同，只是 cwHoles 按行列表打印。
module Match3.Board.Wave
  ( CascadeWave(..)
  ) where

import Match3.Board.Grid (MBoard, mboardRows)
import Match3.Types

-- | 连锁中的一轮（一次「消除 → 下落 → 补子」）。
data CascadeWave = CascadeWave
  { cwBefore  :: Board           -- ^ 本轮消除前的盘面
  , cwCleared :: [Pos]           -- ^ 本轮被消掉的格（真消除 + 被打碎的障碍）；可能为空（仅沉降）
  , cwDrained :: [Pos]           -- ^ 沉降途中底行被收走的饼干位
  , cwHoles   :: MBoard          -- ^ 消除并放下新特殊块之后、下落之前（Nothing = 空洞）
  , cwAfter   :: Board           -- ^ 重力 / 传送门 / 补子之后
  , cwScore   :: Score           -- ^ 本轮得分（与结算的波次计分相同）
  } deriving (Eq)

-- | 与派生的 Show 逐字相同，只是 cwHoles 仍按行列表打印（第 3 刀前 MBoard 是 [[Maybe Cell]]；
-- 元素查询快照对 show 的结果取散列，打印形式因此保持不变）。
instance Show CascadeWave where
  showsPrec d w =
    showParen (d >= 11) $
      showString "CascadeWave {cwBefore = " . showsPrec 0 (cwBefore w)
        . showString ", cwCleared = " . showsPrec 0 (cwCleared w)
        . showString ", cwDrained = " . showsPrec 0 (cwDrained w)
        . showString ", cwHoles = " . showsPrec 0 (mboardRows (cwHoles w))
        . showString ", cwAfter = " . showsPrec 0 (cwAfter w)
        . showString ", cwScore = " . showsPrec 0 (cwScore w)
        . showChar '}'
