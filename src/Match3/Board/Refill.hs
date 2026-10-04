{-# LANGUAGE RankNTypes #-}
-- | 顶部补子策略（第 8 刀）：沉降之后的空洞由谁、按什么规则补上。
--
-- 旧实现（Gravity.refill）把「逐个空洞随机选一色、补一颗普通宝石」写死在主流程里；现在补子的格子来自一个
-- 'RefillPolicy' 记录，主流程（Gravity.settleRefillWith / Cascade 的每轮沉降）只调 'refillWith'。
-- 策略的来源（见 Gravity.activeRefill）：关卡级元素对 'Match3.Element.Message.Refilling' 的回复
-- （LevelHooks.hookRefill）优先，否则注册表的策略（World.refillPolicyWith，缺省 'defaultRefill'）。
--
-- 不变量：'refillWith' 按行优先顺序逐个空洞问策略，每个空洞恰好调一次 'refillCell'；
-- 'defaultRefill' 每个空洞恰好消耗一次 randomR (0, numColors - 1)，与旧 refill 逐字相同（盘面与生成器状态都相同）。
--
-- 依赖：Board.Grid（MBoard、randomColor）、Match3.Types。不依赖注册表。
module Match3.Board.Refill
  ( RefillCtx(..)
  , RefillPolicy(..)
  , defaultRefill
  , colorsRefill
  , refillWith
  ) where

import Data.Array (assocs, (//))
import Match3.Board.Grid (MBoard, randomColor)
import Match3.Types
import System.Random (RandomGen, randomR)

-- | 补一个空洞时策略看到的上下文：空洞位置、当前盘面（行优先在它之前的空洞已经补上）。
data RefillCtx = RefillCtx
  { rcPos   :: Pos
  , rcBoard :: MBoard
  }

-- | 补子策略：名字（文档 / 测试用）与「补这个空洞的格子」。随机数只能经给出的生成器消耗。
data RefillPolicy = RefillPolicy
  { refillName :: String
  , refillCell :: forall g. RandomGen g => RefillCtx -> g -> (Cell, g)
  }

-- | 缺省策略（与第 8 刀前逐字相同）：每个空洞随机选一色（numColors 色），补一颗普通宝石。
defaultRefill :: RefillPolicy
defaultRefill = RefillPolicy "random-gem" (\_ g -> let (c, g') = randomColor g in (mkGem c, g'))

-- | 只用前 n 色（关卡颜色数；n 截到 1..numColors）的随机普通宝石。colorsRefill numColors 与 'defaultRefill' 逐字相同。
colorsRefill :: Int -> RefillPolicy
colorsRefill n = RefillPolicy ("random-gem-" ++ show k) (\_ g -> let (i, g') = randomR (0, k - 1) g in (mkGem (colorAt i), g'))
  where
    k = max 1 (min numColors n)

-- | 按策略补满空洞：行优先逐个空洞问策略，返回补满的盘面与推进后的生成器。
refillWith :: RandomGen g => RefillPolicy -> g -> MBoard -> (Board, g)
refillWith pol g0 mb0 =
  let (mb, g') = foldl' step (mb0, g0) [p | (p, Nothing) <- assocs mb0]
  in (boardFromArray (fmap (maybe (mkGem C1) id) mb), g')
  where
    step (mb, g) p =
      let (c, g1) = refillCell pol (RefillCtx p mb) g
      in (mb // [(p, Just c)], g1)
