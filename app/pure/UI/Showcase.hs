-- | 元素展示盘（桌面 MATCH3_SHOWCASE=1 / 网页 ?showcase=1）：把棋盘换成「全部棋子一览」，用于检查贴图（仅展示，不影响规则模块）。
-- 桌面 UI.Env（重新导出）与网页 Match3Web.Api 共用；原在 UI.Env，web-sdl-parity 时逐字移进 app/pure。
--
-- 依赖：Match3.Core。纯函数。
module UI.Showcase
  ( showcaseState
  , showcaseBoard
  ) where

import Match3.Core

-- | 把局面的棋盘换成展示盘，并摆上飞碟 / 传送门 / 传送带 / 打开的地毯各一例；清掉提示。
showcaseState :: GameState -> GameState
showcaseState gs =
  setUfos [Ufo (7, 7) C3]
    . setPortals [((7, 5), (7, 6))]
    . setBelts [[(7, 0), (7, 1), (7, 2), (7, 3)]]
    . setCarpetOpen [(7, 4)]
    $ gs
        { gsBoard = showcaseBoard
        , gsHint = Nothing
        }

-- | 8×8 展示盘：五色宝石、特殊块与各种元素 / 覆盖物各一格。
showcaseBoard :: Board
showcaseBoard = boardFromRows
  [ [mkGem C1, mkGem C2, mkGem C3, mkGem C4, mkGem C5, Gem C1 LineH 0 Nothing, Gem C2 LineV 0 Nothing, Gem C3 Bomb 0 Nothing]
  , [Gem C4 Rainbow 0 Nothing, mkFlip C1 C3, mkCountdown C2 3, mkIceGem C5 1, mkIceGem C1 2, mkIceGem C2 3, mkGrassGem C3, mkVineGem C4]
  , [mkChocoGem C5, mkFogGem C1 1, mkFogGem C2 3, mkChainGem C3 1, mkChainGem C4 2, mkFreezeGem C5 1, mkFreezeGem C1 2, mkCurtainGem C2 1]
  , [mkCurtainGem C3 2, mkSteamGem C4, mkStoneLayers 1, mkStoneLayers 2, mkStoneLayers 3, mkChestLayers 1, mkChestLayers 2, mkHoneyLayers 2]
  , [mkBalloon C1, mkBalloon C2, mkBalloon C3, mkBalloon C4, mkBalloon C5, mkCookie, mkCakeLayers 1, mkCakeLayers 3]
  , [mkMagicHat, mkMakerCharges C1 3, mkMakerCharges C3 2, mkSnail 0 1, mkSnail 1 0, mkSafeLayers 2, mkSurprise, mkTimeSpirit]
  , [mkBottle C1, mkBottle C2, mkBottle C3, mkBottle C4, mkBottle C5, mkSnail 0 (-1), mkSnail (-1) 0, mkGem C1]
  , [mkGem C2, mkGem C3, mkGem C4, mkGem C5, mkGem C1, mkGem C2, mkGem C3, mkGem C4]
  ]
