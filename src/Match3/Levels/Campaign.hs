{-# LANGUAGE OverloadedStrings #-}
-- | 战役关卡表（第 6 刀从 Match3.Types 移来，并把各关的装饰 / 皮带 / 传送门 / 飞碟 / 地毯 / 地面层并进每关的记录）。
-- 按下标取关一律经 lookupLevel（返回 Maybe；第 6 刀前各处写 allLevels !! i）。
--
-- 依赖：Match3.Levels.Level、Match3.Types、Match3.Element.Types（放置表）、Match3.Ufo（mkUfo）。
-- 不变量：关卡内容与第 6 刀前逐项相同（金标准 / 元素查询快照 / 新旧整局对照锁定）；lvlIndex = 在表中的下标。
module Match3.Levels.Campaign
  ( allLevels
  , lookupLevel
  , levelCount
  , clampLevelIndex
  , levelCarpets
  ) where

import Match3.Counts (CounterKey(..))
import Match3.Element.Types (Arg(..), Placement(..))
import Match3.Levels.Level
import Match3.Types
import Match3.Ufo (mkUfo)

-- | 第 i 关（0 基下标）；越界为 Nothing。取关时校验行列（越界尺寸 error，不夹取）。
lookupLevel :: Int -> Maybe Level
lookupLevel i
  | i < 0 = Nothing
  | otherwise = case drop i allLevels of
      l : _ -> Just (assertLevelDims l)
      [] -> Nothing

-- | 关卡数（= length allLevels）。
levelCount :: Int
levelCount = length allLevels

-- | 把下标夹到关卡表范围 [0, levelCount - 1]（前端 / 开局按「当前关，越界取最近的一关」读关卡时用）。
clampLevelIndex :: Int -> Int
clampLevelIndex i = max 0 (min (levelCount - 1) i)

-- | 第 i 关的未铺地毯格（lookupLevel 的简写；没有这关 / 没有地毯 = []）。前端画地毯底、网页版编码用。
levelCarpets :: Int -> [Pos]
levelCarpets = maybe [] lvlCarpets . lookupLevel

-- | Mixed campaign: score / collect / stone / chest / honey / balloon / cookie / cake / hat / chain / maker / portal / UFO / snail / freeze / curtain / safe / flip / surprise / bottle / time-spirit / steam / carpet / hazards / jelly / bubble (段 5); difficulty ramps.
allLevels :: [Level]
allLevels =
  [ level 0 "入门" 30 (goalScore 300)
  , level 1 "采红" 30 (goalCollect C1 20)
  , level 2 "热身" 26 (goalScore 500)
  , level 3 "采蓝" 26 (goalCollect C3 22)
  , (level 4 "进阶" 24 (goalScore 700))
      { lvlPlacements = [Place "choco" [] [(2, 3), (3, 2), (3, 5), (5, 4)]]
      }
    -- Ice seals on a few gems (开心消消乐冰层入门)
  , (level 5 "冰绿" 24 (goalCollect C2 26))
      { lvlPlacements = [Place "ice" [AInt 1] [(2, 2), (2, 5), (5, 3), (6, 6)]]
      }
  , level 6 "双采" 28 (goalColors [(C1, 12), (C3, 12)])
  , (level 7 "碎石" 26 (goalCount CountStones 8))
      { lvlPlacements = [Place "stone" [] [(4, 1), (4, 3), (4, 5), (5, 2), (5, 4), (6, 1), (6, 3), (6, 5)]]
      , lvlBelts = [[(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, 5), (2, 4), (2, 3), (2, 2), (2, 1)]]
      }
  , (level 8 "草场" 24 (goalScore 600))
      { lvlPlacements = [Place "grass" [] [(2, 2), (2, 5), (3, 3), (3, 6), (5, 1), (5, 4), (6, 3), (6, 6)]]
      }
  , (level 9 "藤袭" 22 (goalCollect C1 18))
      { lvlPlacements = [Place "vine" [] [(2, 2), (2, 4), (4, 3), (5, 5)]]
      }
  , (level 10 "传送" 22 (goalScore 800))
      { lvlPlacements = [Place "grass" [] [(5, 1), (5, 3), (5, 5), (6, 2), (6, 4)]]
      , lvlBelts = [[(3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7)]]
      }
  , (level 11 "轰炸" 20 (goalScore 750))
      { lvlPlacements = [Place "countdown" [AInt 4] [(2, 2), (2, 5), (5, 3), (6, 6)]]
      }
  , (level 12 "飞碟" 24 (goalCount CountUfo 10))
      { lvlUfos = [mkUfo (2, 3) C1]
      }
  , (level 13 "碟猎" 20 (goalCount CountUfo 14))
      { lvlBelts = [[(4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7)]]
      , lvlUfos = [mkUfo (1, 2) C1, mkUfo (1, 5) C3]
      }
  , (level 14 "压力" 20 (goalColors [(C1, 10), (C2, 10), (C3, 8)]))
      { lvlPlacements =
          [ Place "grass" [] [(2, 2), (3, 5), (5, 3)]
          , Place "choco" [] [(1, 1), (1, 6), (6, 2)]
          ]
      }
  , (level 15 "大师" 22 (goalScore 1000))
      { lvlPlacements =
          [ Place "stone" [] [(4, 0), (4, 7), (5, 1), (5, 6)]
          , Place "grass" [] [(2, 1), (2, 6)]
          , Place "vine" [] [(6, 3)]
          , Place "choco" [] [(1, 3), (7, 4)]
          , Place "chest" [AInt 1] [(7, 1)]
          , Place "chest" [AInt 2] [(7, 6)]
          , Place "countdown" [AInt 4] [(3, 3)]
          ]
      , lvlBelts =
          [ [(0, 2), (0, 3), (0, 4), (0, 5), (1, 5), (1, 4), (1, 3), (1, 2)]
          , [(6, 1), (6, 2), (6, 3), (6, 4), (6, 5)]
          ]
      , lvlUfos = [mkUfo (0, 4) C2]
      }
  , (level 16 "宝箱" 24 (goalCount CountChests 6))
      { lvlPlacements = layersAt "chest" [((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1)]
      }
  , (level 17 "巧箱" 22 (goalCount CountChests 5))
      { lvlPlacements =
          layersAt "chest" [((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1)]
            ++ [Place "choco" [] [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]]
      }
    -- 蜂蜜: honey jars to smash
  , (level 18 "蜂蜜" 24 (goalCount CountHoney 6))
      { lvlPlacements = layersAt "honey" [((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1)]
      }
  , (level 19 "蜜压" 22 (goalCount CountHoney 5))
      { lvlPlacements =
          layersAt "honey" [((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1)]
            ++ [Place "choco" [] [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]]
      }
    -- 气球: colored balloons popped by same-color adjacent clears
  , (level 20 "气球" 24 (goalCount CountBalloons 6))
      { lvlPlacements = placeEach "balloon" (\c -> [AColor c]) [((2, 2), C1), ((2, 5), C3), ((4, 1), C2), ((4, 3), C1), ((4, 5), C4), ((6, 2), C3), ((6, 5), C5)]
      }
    -- 饼干: cookies high on board; clear below so they fall to bottom
  , (level 21 "饼干" 24 (goalCount CountCookies 6))
      { lvlPlacements = [Place "cookie" [] [(0, 1), (0, 3), (0, 5), (1, 2), (1, 4), (1, 6), (2, 3)]]
      }
  , (level 22 "巧饼" 22 (goalCount CountCookies 5))
      { lvlPlacements =
          [ Place "cookie" [] [(0, 2), (0, 5), (1, 1), (1, 4), (1, 6), (2, 3)]
          , Place "choco" [] [(3, 1), (3, 6)]
          , Place "fog" [AInt 1] [(4, 2), (4, 3), (4, 4), (5, 3), (6, 2), (6, 5)]
          ]
      }
    -- 蛋糕: layered cakes to smash (distinct from Cookie)
  , (level 23 "蛋糕" 24 (goalCount CountCakes 6))
      { lvlPlacements = layersAt "cake" [((2, 2), 1), ((2, 5), 1), ((4, 1), 2), ((4, 3), 1), ((4, 5), 2), ((6, 2), 1), ((6, 5), 1)]
      }
    -- 帽宴: cakes + magic hats mixed
  , (level 24 "帽宴" 22 (goalCount CountCakes 5))
      { lvlPlacements =
          layersAt "cake" [((3, 2), 1), ((3, 5), 2), ((5, 3), 1), ((5, 4), 1), ((6, 6), 1)]
            ++ [ Place "magic_hat" [] [(1, 1), (1, 6), (2, 3), (4, 0), (4, 7)]
               , Place "choco" [] [(6, 1), (6, 5)]
               ]
      }
    -- 锁链: iron chains lock gems; peel by adjacent clear
  , (level 25 "锁链" 22 (goalScore 900))
      { lvlPlacements =
          [ Place "chain" [AInt 1] [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
          , Place "chain" [AInt 2] [(4, 3), (4, 4)]
          , Place "stone" [] [(1, 1), (1, 6)]
          , Place "choco" [] [(5, 1), (5, 6)]
          ]
      }
    -- 果汁: juice makers + fog; portals in lvlPortals
  , (level 26 "果汁" 24 (goalCollect C1 18))
      { lvlPlacements =
          placeEach "maker" (\(c, n) -> [AColor c, AInt n]) [((2, 2), (C1, 2)), ((2, 5), (C1, 3)), ((5, 3), (C1, 2)), ((5, 4), (C3, 2))]
            ++ [ Place "fog" [AInt 1] [(3, 1), (3, 6), (6, 2), (6, 5)]
               , Place "choco" [] [(1, 3), (1, 4)]
               ]
      , lvlPortals = [((0, 1), (7, 6)), ((0, 6), (7, 1))]
      }
  , (level 27 "终章" 24 (goalScore 1400))
      { lvlPlacements =
          [ Place "stone" [] [(0, 0), (0, 7), (7, 0), (7, 7)]
          , Place "chest" [] [(3, 1), (3, 6)]
          , Place "honey" [] [(2, 3), (2, 4)]
          ]
            ++ placeEach "balloon" (\c -> [AColor c]) [((5, 1), C1), ((5, 6), C3)]
            ++ [ Place "cookie" [] [(0, 2), (0, 5)]
               , Place "cake" [AInt 1] [(1, 3), (1, 4)]
               , Place "magic_hat" [] [(7, 2), (7, 5)]
               , Place "maker" [AColor C2, AInt 2] [(4, 0), (4, 7)]
               , Place "choco" [] [(2, 2), (2, 5)]
               , Place "fog" [AInt 2] [(5, 2), (5, 5)]
               , Place "chain" [AInt 1] [(6, 1), (6, 6)]
               , Place "freeze" [AInt 1] [(3, 3), (3, 4)]
               , Place "curtain" [AInt 1] [(5, 3), (5, 4)]
               , Place "safe" [AInt 1] [(6, 4)]
               , Place "flip" [AColor C1, AColor C3] [(7, 3)]
               , Place "surprise" [] [(5, 0)]
                 -- Bottle off portal entrance (0,3) *and* off row-1 belt: Bottle never clears
                 -- and is not portal-transferable; on a belt cell it permanently occupies one
                 -- slot of the cycle (same immortal-blocker class as portal endpoints).
               , Place "bottle" [AColor C1] [(2, 7)]
               , Place "vine" [] [(6, 3)]
                 -- Snail off portal row 0: Cookie at (0,5) would reverse it onto portal A (0,3).
                 -- Engine also walls portal endpoints; décor keeps crawl path clear of the pair.
               , Place "snail" [AInt 0, AInt 1] [(2, 0)]
               , Place "countdown" [AInt 5] [(4, 4)]
               ]
      , lvlBelts = [[(1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7)]]
      , lvlPortals = [((0, 3), (7, 4))]
      , lvlUfos = [mkUfo (2, 4) C1]
      , lvlCarpets = [(4, 2), (4, 5), (5, 3), (5, 4)]  -- 终章: a few carpet tiles mixed into the finale
      }
    -- 蜗牛: crawling snails that push gems after each move
  , (level 28 "蜗牛" 20 (goalScore 850))
      { lvlPlacements =
          placeEach "snail" (\(dr, dc) -> [AInt dr, AInt dc])
            [((1, 1), (0, 1)), ((1, 6), (0, -1)), ((4, 2), (1, 0)), ((4, 5), (1, 0)), ((6, 3), (0, 1)), ((2, 4), (0, -1))]
      }
    -- 冰冻: rocket freeze overlays (block swap, peel by adjacent; gems still match)
  , (level 29 "冰冻" 20 (goalCollect C2 16))
      { lvlPlacements =
          [ Place "freeze" [AInt 1] [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
          , Place "freeze" [AInt 2] [(4, 3), (4, 4)]
          , Place "choco" [] [(1, 1), (1, 6)]
          ]
      }
    -- 窗帘: curtain columns (遮挡整列/区域，邻消揭开)
  , (level 30 "窗帘" 22 (goalCollect C1 16))
      { lvlPlacements =
          [ Place "curtain" [AInt 1] [(r, c) | r <- [1, 2, 3, 4, 5, 6], c <- [1, 6]]
          , Place "curtain" [AInt 2] [(2, 3), (2, 4), (5, 3), (5, 4)]
          , Place "choco" [] [(0, 2), (0, 5)]
          ]
      }
    -- 金库: safes open into cookies; dual-face flips mixed in
  , (level 31 "金库" 22 (goalCount CountSafes 5))
      { lvlPlacements =
          layersAt "safe" [((2, 2), 1), ((2, 5), 2), ((4, 1), 1), ((4, 3), 2), ((4, 5), 1), ((6, 2), 1), ((6, 5), 2)]
            ++ placeEach "flip" (\(f, bk) -> [AColor f, AColor bk])
                 [ ((1, 1), (C1, C3)), ((1, 6), (C2, C4)), ((3, 0), (C3, C1))
                 , ((3, 7), (C4, C2)), ((5, 3), (C1, C2)), ((5, 4), (C2, C1))
                 ]
            ++ [Place "choco" [] [(7, 1), (7, 6)]]
      }
    -- surprise boxes: open to special or 3x3 pop
  , (level 32 "惊喜" 22 (goalScore 900))
      { lvlPlacements =
          [ Place "surprise" [] [(1, 1), (1, 6), (2, 3), (3, 1), (3, 6), (4, 0), (4, 4), (5, 2), (5, 5), (6, 3)]
          , Place "choco" [] [(7, 2), (7, 5)]
          ]
      }
    -- dye bottles paint neighbors on adjacent clear
  , (level 33 "染色" 22 (goalCollect C3 16))
      { lvlPlacements =
          placeEach "bottle" (\c -> [AColor c])
            [ ((2, 2), C3), ((2, 5), C3), ((4, 1), C1), ((4, 6), C3)
            , ((5, 3), C3), ((5, 4), C2), ((6, 2), C3), ((6, 5), C3)
            ]
            ++ [Place "fog" [AInt 1] [(1, 3), (1, 4)]]
      }
    -- 时灵: time spirits award +2 moves when adjacent-cleared
  , (level 34 "时灵" 22 (goalScore 850))
      { lvlPlacements = [Place "time_spirit" [] [(1, 1), (1, 6), (2, 3), (3, 2), (3, 5), (4, 4), (5, 1), (5, 6), (6, 3)]]
      }
    -- 蒸汽: steam overlays block match, adjacent extinguish, then spread
  , (level 35 "蒸汽" 22 (goalCollect C2 16))
      { lvlPlacements =
          [ Place "steam" [] [(2, 2), (2, 5), (3, 3), (3, 4), (4, 1), (4, 6), (5, 3), (6, 2), (6, 5)]
          , Place "steam" [] [(1, 3), (1, 4), (4, 3), (4, 4)]
          , Place "choco" [] [(7, 2), (7, 5)]
          ]
      }
    -- 地毯: open floor tiles in lvlCarpets; light choco garnish
  , (level 36 "地毯" 24 (goalCount CountCarpets 8))
      { lvlPlacements = [Place "choco" [] [(1, 1), (1, 6), (6, 1), (6, 6)]]
      , lvlCarpets = [(3, 2), (3, 3), (3, 4), (3, 5), (4, 2), (4, 3), (4, 4), (4, 5)]  -- 地毯: 2×4 center patch
      }
    -- 织毯: carpet + choco + fog mix
  , (level 37 "织毯" 24 (goalCount CountCarpets 12))
      { lvlPlacements =
          [ Place "choco" [] [(1, 2), (1, 5), (6, 2), (6, 5)]
          , Place "fog" [AInt 1] [(0, 3), (0, 4), (7, 3), (7, 4)]
          ]
      , lvlCarpets =
          -- 织毯: larger patch + corners
          [ (2, 2), (2, 3), (2, 4), (2, 5)
          , (3, 2), (3, 5), (4, 2), (4, 5)
          , (5, 2), (5, 3), (5, 4), (5, 5)
          ]
      }
    -- 段 5：追加在 38 关之后（前 38 关不变）；目标按元素名计数（CountNamed）
  , (level 38 "果冻" 24 (goalCount (CountNamed "jelly") 32))
      { lvlGround =
          -- 16 格双层果冻（共 32 层）：中间两行各 6 格 + 四个内角
          [(p, ("jelly", 2)) | p <- [(r, c) | r <- [3, 4], c <- [1 .. 6]] ++ [(1, 1), (1, 6), (6, 1), (6, 6)]]
      }
    -- 气泡（段 5）：12 个，邻消即破、随重力下落
  , (level 39 "气泡" 22 (goalCount (CountNamed "bubble") 12))
      { lvlPlacements = [Place "bubble" [] [(1, 1), (1, 6), (2, 3), (2, 4), (3, 0), (3, 7), (4, 2), (4, 5), (5, 1), (5, 6), (6, 3), (6, 4)]]
      }
    -- 爆破（新玩法：规则开关 bomb_shapes）：本关 L / T 形消除生成炸弹；10 块石头分两堆，炸弹 3×3 一次能砸好几块
  , (level 40 "爆破" 24 (goalCount CountStones 10))
      { lvlPlacements = [Place "stone" [] [(5, 1), (5, 2), (6, 1), (6, 2), (7, 1), (5, 5), (5, 6), (6, 5), (6, 6), (7, 6)]]
      , lvlRules = ["bomb_shapes"]
      }
    -- 魔石（新玩法 2）：四块魔法石（固定、打不动），邻格消除充能 3 格后步末发射清整行整列；
    -- 8 块双层石头都在魔法石的行 / 列尽头，靠发射或旁边消除砸开
  , (level 41 "魔石" 24 (goalCount CountStones 8))
      { lvlPlacements =
          [ Place "magic_stone" [] [(2, 2), (2, 5), (5, 2), (5, 5)]
          , Place "stone" [AInt 2] [(0, 2), (0, 5), (7, 2), (7, 5), (2, 0), (2, 7), (5, 0), (5, 7)]
          ]
      }
    -- 毛球（新玩法 3）：14 个毛球分散在全盘（隔行错开），挡交换、会下落，旁边一消就灭；每步末跳到相邻的普通宝石格
  , (level 42 "毛球" 22 (goalCount (CountNamed "fuzzball") 14))
      { lvlPlacements = [Place "fuzzball" [] [(0, 1), (0, 6), (1, 3), (2, 0), (2, 5), (3, 2), (3, 7), (4, 4), (5, 1), (5, 6), (6, 3), (7, 0), (7, 5), (6, 7)]]
      }
    -- 魔力鸟（新玩法 4：规则开关 rainbow_combos）：彩虹 × 直线 → 同色全部变直线再引爆，彩虹 × 炸弹 → 全部变炸弹再引爆；
    -- 开局中间放好两组「彩虹 + 直线 / 炸弹」，12 块双层石头在四边，一次变身爆炸只削一层
  , (level 43 "魔力鸟" 24 (goalCount CountStones 12))
      { lvlPlacements =
          [ Place "rainbow" [] [(3, 2), (4, 5)]
          , Place "line_h" [] [(3, 3)]
          , Place "bomb" [] [(4, 4)]
          , Place "stone" [AInt 2] [(0, 0), (0, 3), (0, 4), (0, 7), (7, 0), (7, 3), (7, 4), (7, 7), (2, 0), (5, 0), (2, 7), (5, 7)]
          ]
      , lvlRules = ["rainbow_combos"]
      }
    -- 雪怪（新玩法 5）：Boss 占 (2,3)–(3,4) 的 2×2，挡交换、不下落；身边的消除与打到它的特效扣血，
    -- 血量归零即过关（目标 = 击败 Boss）；每 3 次交换在身边召唤一块雪块（1 层石头）
  , (level 44 "雪怪" 24 (goalCount (CountNamed "snow_boss") snowBossLevelHp))
      { lvlPlacements = bossAt (2, 3) snowBossLevelHp
      }
    -- 饼干掉落口（新玩法 6）：顶行 (0,1) / (0,3) / (0,4) / (0,6) 是掉落口，开局各有一块饼干；盘上饼干少于 4 块时
    -- 补子在掉落口补饼干（不额外消耗随机数）；饼干落到底行被收走（原有的底边收集），目标收 8 块
  , (level 45 "掉落口" 26 (goalCount CountCookies 8))
      { lvlPlacements = [Place "cookie" [] cookieDropCells]
      , lvlDrops = [DropSpec cookieDropCells Cookie 4]
      }
    -- 变色龙（新玩法 7）：变色龙可交换、按当前颜色匹配，每次交换后按 C1 → C2 → … → C5 → C1 换一种颜色
    -- （会立刻连成三消的颜色顺延跳过）；开局 2 只、颜色取原格宝石；顶行 (0,3) 是掉落口（复用新玩法 6），盘上少于
    -- 2 只时补进一只 C1 变色龙；变色龙被消除计数，18 步消掉 30 只
  , (level 46 "变色龙" chameleonMoves (goalCount (CountNamed "chameleon") chameleonGoal))
      { lvlPlacements = [Place "chameleon" [] chameleonCells]
      , lvlDrops = [DropSpec chameleonDropCells (mkCustom "chameleon" 0) chameleonKeep]
      }
    -- 魔法地格（新玩法 8）：地面层 "magic"（不会被消耗），特效在魔法地格上引爆时范围向外扩一圈
    -- （直线一行 → 三行、炸弹 3×3 → 5×5）；本关打开 L / T 形出炸弹（规则开关 bomb_shapes，新玩法 1），特效多一些。
    -- 四块魔法地格在 (6,2) / (6,5) / (5,3) / (5,4)，底行是 8 块三层石头：在第 6 行魔法地格上引爆的横消扩成三行、
    -- 正好扫到底行一整排；18 步打碎 8 块
  , (level 47 "魔法格" magicGroundMoves (goalCount CountStones magicGroundGoal))
      { lvlGround = [(p, ("magic", 1)) | p <- magicGroundCells]
      , lvlPlacements = [Place "stone" [AInt magicGroundStoneLayers] magicGroundStones]
      , lvlRules = ["bomb_shapes"]
      }
    -- 可变矩形盘面验收：6×9（行×列），无新元素；仅得分目标，装饰为空，证明非 8×8 可玩
  , (level 48 "宽域" wideBoardMoves (goalScore wideBoardGoal))
      { lvlRows = 6
      , lvlCols = 9
      }
  ]

-- | 第 48 关的魔法地格。
magicGroundCells :: [Pos]
magicGroundCells = [(6, 2), (6, 5), (5, 3), (5, 4)]

-- | 第 48 关开局的石头（三层，见 magicGroundStoneLayers）。
magicGroundStones :: [Pos]
magicGroundStones = [(7, c) | c <- [0 .. 7]]

-- | 第 48 关石头的层数。
magicGroundStoneLayers :: Int
magicGroundStoneLayers = 3

-- | 第 48 关的目标块数（打碎的石头）。
magicGroundGoal :: Int
magicGroundGoal = 8

-- | 第 48 关的步数。
magicGroundMoves :: MovesLeft
magicGroundMoves = 18

-- | 第 49 关（宽域）步数。
wideBoardMoves :: MovesLeft
wideBoardMoves = 24

-- | 第 49 关得分目标。
wideBoardGoal :: Int
wideBoardGoal = 500

-- | 第 47 关开局的变色龙格。
chameleonCells :: [Pos]
chameleonCells = [(3, 1), (5, 6)]

-- | 第 47 关的掉落口格。
chameleonDropCells :: [Pos]
chameleonDropCells = [(0, 3)]

-- | 第 47 关盘上保持的变色龙只数。
chameleonKeep :: Int
chameleonKeep = 2

-- | 第 47 关的目标只数。
chameleonGoal :: Int
chameleonGoal = 30

-- | 第 47 关的步数。
chameleonMoves :: MovesLeft
chameleonMoves = 18

-- | 第 46 关的掉落口格。
cookieDropCells :: [Pos]
cookieDropCells = [(0, 1), (0, 3), (0, 4), (0, 6)]

-- | 第 45 关雪怪的满血值（= 关卡目标值）。
snowBossLevelHp :: Int
snowBossLevelHp = 40

-- | 一只雪怪 Boss 的放置表：左上角 + 血量 → 四格（放置参数 [血量, 象限]，象限 0 左上 / 1 右上 / 2 左下 / 3 右下）。
bossAt :: Pos -> Int -> [Placement]
bossAt (r, c) hp = [Place "snow_boss" [AInt hp, AInt q] [p] | (q, p) <- zip [0 ..] [(r, c), (r, c + 1), (r + 1, c), (r + 1, c + 1)]]
