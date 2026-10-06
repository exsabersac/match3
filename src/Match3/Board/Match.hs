{-# LANGUAGE ScopedTypeVariables #-}

-- | 匹配检测与提示：找 ≥3 连（MatchRun；第 8 刀起定义在 Element.Types，这里再导出）、是否存在匹配、是否存在可走的一手（findHint）。
--
-- 依赖：Grid、元素元素世界（哪些格参与匹配 / 能交换 / 进普通匹配提示；成对交换规则 = 彩虹与特殊合成也算可走）。只读盘面。
-- 第二刀 2b：「哪些格打断连线」不再按构造器列举，改为查元素世界 matchColorWith
-- （本体颜色 + 挡匹配的叠层）；新增元素只需在它的 instance 里声明。元素类迁移起普通匹配提示排除彩虹本体
-- 改查元素的 hintable（原先直接调 Rainbow.isRainbow）。段 2c 起本模块不依赖内置元素世界，全部函数收 Registry；不带 With 的旧名在 Match3.Board.Default。
--
-- 性能（Haskell 特性第 5 项，见 docs/haskell-features/05-性能与并发.md）：匹配扫描是整个规则里最热的路径
-- （金标准生成的剖析里 findMatchRunsWith / hasAnyMatchWith 合计约四成时间，其中 matchColorWith 被调用四千多万次，
-- 每格每次扫描要问两遍元素世界：行一遍、列一遍）。现在每次扫描先把整盘的「匹配码」算进一张 unboxed 数组
-- （'matchCodesWith'：每格只问一次元素世界），行 / 列扫描都在这张 Int 数组上做；提示搜索对每个候选交换
-- 不再复制整盘，只在读码时把两个下标对调。结果与第 5 项前逐项相同（Spec.Perf 对照）。
-- 也试过把提示搜索写成 STUArray 上「就地换过去、查、换回来」：与这里的纯函数写法一样快（文档 §4），所以没用 ST。
module Match3.Board.Match
  ( MatchRun(..)
  , matchCodesWith
  , findMatchRunsWith
  , groupGemRunsWith
  , findMatchesWith
  , hasAnyMatchWith
  , hasValidMoveWith
  , findHintWith
  ) where

import Data.Array (Array, elems)
import qualified Data.Array as A
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.List (nub)
import Data.Maybe (isJust)
import Match3.ECS.Registry (Registry, blocksSwapWith, colorOfWith, hintableWith, matchColorWith, swapSystems, upperBlocksSwapWith)
import Match3.ECS.Stage (SwapSys(..))
import Match3.Element.Types (MatchRun(..))
import Match3.Types
import Match3.Board.Grid

-- | 整盘的匹配码：matchColorWith 为 Just c 的格是 fromEnum c，为 Nothing（障碍、被叠层挡住的宝石……）的格是 -1。
-- 匹配只看各格自己的 matchColorWith（与位置、邻格无关），所以「换两格」= 「换两格的码」。
-- UArray 的元素是未装箱的 Int，整张数组是一块连续内存，读一格不用解引用 thunk / 构造器。
matchCodesWith :: Registry -> Board -> UArray Pos Int
matchCodesWith world b =
  let arr = boardArray b
  in listArray (A.bounds arr) [maybe noCode fromEnum (matchColorWith world cell) | cell <- elems arr]

noCode :: Int
noCode = -1

-- | 一条线（按给定顺序的格）上长度 ≥ 3 的同码连续段（码 < 0 的格打断连线），与 groupGemRunsWith 后取 ≥ 3 的段相同。
codeRuns :: (Pos -> Int) -> [Pos] -> [(Int, [Pos])]
codeRuns code = go
  where
    go [] = []
    go (p : ps)
      | k < 0 = go ps
      | otherwise =
          let (same, rest) = span ((== k) . code) ps
              more = go rest
          in case same of
               (_ : _ : _) -> (k, p : same) : more
               _ -> more
      where
        k = code p

-- | 一列码里是否有长度 ≥ 3 的同码连续段（码 < 0 打断）。
codesHaveRun :: [Int] -> Bool
codesHaveRun = go noCode (0 :: Int)
  where
    go _ _ [] = False
    go prev n (k : ks)
      | k < 0 = go noCode 0 ks
      | k == prev = n + 1 >= 3 || go prev (n + 1) ks
      | otherwise = go k 1 ks

-- | findMatchRuns（指定元素世界）：先行后列，各自按下标升序；段内位置按线的方向升序（与第 5 项前相同）。
findMatchRunsWith :: Registry -> Board -> [MatchRun]
findMatchRunsWith world b =
  [MatchRun (toEnum k) ps True | r <- rows, (k, ps) <- codeRuns code [(r, c) | c <- cols]]
    ++ [MatchRun (toEnum k) ps False | c <- cols, (k, ps) <- codeRuns code [(r, c) | r <- rows]]
  where
    codes = matchCodesWith world b
    code = (codes !)
    rows = boardRowIndices b
    cols = boardColIndices b

-- | 连续同色段：matchColorWith 为 Nothing 的格（障碍、被迷雾 / 锁链 / 窗帘 / 蒸汽盖住的宝石等）打断连线。
-- 火箭冰冻不挡匹配（冻住的宝石照样成消）。
groupGemRunsWith :: Registry -> Board -> [Pos] -> [(Color, [Pos])]
groupGemRunsWith _ _ [] = []
groupGemRunsWith world b (p : ps) = case matchColorWith world (getCell b p) of
  Nothing -> groupGemRunsWith world b ps
  Just col -> go [p] col ps
  where
    go run col [] = [(col, reverse run)]
    go run col (q : qs) = case matchColorWith world (getCell b q) of
      Just col' | col' == col -> go (q : run) col qs
      _ -> (col, reverse run) : groupGemRunsWith world b (q : qs)

-- | hasAnyMatch（指定元素世界）：= not (null (findMatchRunsWith world b))，但不建连线列表，找到第一条就停。
hasAnyMatchWith :: Registry -> Board -> Bool
hasAnyMatchWith world b =
  any (\r -> codesHaveRun [codes ! (r, c) | c <- cols]) rows
    || any (\c -> codesHaveRun [codes ! (r, c) | r <- rows]) cols
  where
    codes = matchCodesWith world b
    rows = boardRowIndices b
    cols = boardColIndices b

-- | hasValidMove（指定元素世界）。
hasValidMoveWith :: Registry -> Board -> Bool
hasValidMoveWith world = maybe False (const True) . findHintWith world

-- | findHint（指定元素世界）。普通匹配提示只试「有色且能交换」的格；成对交换规则（段 4：元素世界的成对交换规则，
-- 内置 = 彩虹、特殊合成，按 swOrder 逐条）的提示只排除上层（锁链 / 火箭冰冻）挡交换的格，本体由规则自己判定。
findHintWith :: Registry -> Board -> Maybe (Pos, Pos)
findHintWith world b =
  case matchHints ++ concatMap ruleHints (swapSystems world) of
    (x : _) -> Just x
    [] -> Nothing
  where
    rows = boardRowIndices b
    cols = boardColIndices b
    matchHints =
      [ (p1, p2)
      | r <- rows
      , c <- cols
      , let p1 = (r, c)
      , hintable (getCell b p1)
      , p2 <- neighborsInBounds rightAndDown b p1
      , hintable (getCell b p2)
      , hintableWith world (getCell b p1) && hintableWith world (getCell b p2)
      , swapMakesMatch p1 p2
      ]
    ruleHints rule =
      [ (p1, p2)
      | r <- rows
      , c <- cols
      , let p1 = (r, c)
      , p2 <- neighborsInBounds rightAndDown b p1
      , not (upperLocked (getCell b p1) || upperLocked (getCell b p2))
      , swFires rule b p1 p2
      ]
    hintable cell = isJust (colorOfWith world cell) && not (blocksSwapWith world cell)
    upperLocked = upperBlocksSwapWith world
    -- 局部检查（第 3 刀）：交换后的盘面有匹配 ⇔ 没被交换触及的行 / 列在原盘上已有 ≥3 连，
    -- 或交换两格所在的行 / 列在交换后有 ≥3 连。与整盘 hasAnyMatchWith (swapCells b p1 p2) 逐格等价
    -- （匹配只看各格自己的 matchColorWith，连线按行 / 列独立），遍历顺序与返回结果不变。
    -- 第 5 项：原盘的行 / 列结论从匹配码算（仍是惰性数组，用到才算、最多算一次）；被触及的行 / 列读「换过之后」的码：
    -- at q 在 q 是交换的两格之一时读另一格的码。第 5 项前每个候选要 swapCells（两次整盘复制）再逐格问元素世界。
    codes = matchCodesWith world b
    rowPs r = [(r, c) | c <- cols]
    colPs c = [(r, c) | r <- rows]
    rowHas = A.listArray (0, boardNRows b - 1) [codesHaveRun (map (codes !) (rowPs r)) | r <- rows] :: Array Int Bool
    colHas = A.listArray (0, boardNCols b - 1) [codesHaveRun (map (codes !) (colPs c)) | c <- cols] :: Array Int Bool
    swapMakesMatch p1@(r1, c1) p2@(r2, c2) =
      let rs = nub [r1, r2]
          cs = nub [c1, c2]
          at q
            | q == p1 = codes ! p2
            | q == p2 = codes ! p1
            | otherwise = codes ! q
      in any (codesHaveRun . map at . rowPs) rs
           || any (codesHaveRun . map at . colPs) cs
           || or [rowHas A.! r | r <- rows, r `notElem` rs]
           || or [colHas A.! c | c <- cols, c `notElem` cs]

-- | 所有匹配格（指定元素世界）：各连线位置的去重并集。
findMatchesWith :: Registry -> Board -> [Pos]
findMatchesWith world b = nub (concatMap runPos (findMatchRunsWith world b))
