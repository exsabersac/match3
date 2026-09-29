{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 前端状态类型：App（整个界面状态）、Anim（交换 / 下落 / 逐轮回放）、粒子、道具点选模式，
-- 以及动画帧数常量、是否在播放（animBusy）等只读查询。
--
-- 依赖：Match3.Core、ComboFx（Cascade / TextPop）、Art。不含 IO。
-- 不变量：appAnim ≠ AnimNone 期间视为「播放中」，输入层据此锁定交换 / 道具 / 撤销 / 洗牌。
module UI.Types
  ( swapFrames
  , fallFrames
  , Anim(..)
  , Particle(..)
  , ToolMode(..)
  , App(..)
  , appGame
  , helpKeysMsg
  , animBusy
  , playingCascade
  , playingPlayer
  ) where

import Art
import ComboFx
import Data.Text (Text)
import Data.Word (Word8)
import Engine.History (History (..))
import Engine.Playback (Player (..))
import Foreign.C.Types (CInt)
import Match3.Core

-- | 交换补间与轻落各自的帧数（60 fps）。
swapFrames, fallFrames :: Int
swapFrames = 10
fallFrames = 12

-- | Visual-only animation; rules already applied.
data Anim
  = AnimNone
  | AnimSwap
      { asP1 :: Pos
      , asP2 :: Pos
      , asBefore :: Board
      , asFrame :: Int
      , asNext :: Anim   -- ^ 交换播完之后接着播什么（逐轮连锁 / 轻落）
      }
  | AnimFall
      { afBoard :: Board
      , afFrame :: Int
      }
  | AnimCascade CascadePlayer  -- ^ 逐轮回放连锁（通用播放器 + ComboFx 阶段机）

-- | Simple rectangle particle (no textures).
data Particle = Particle
  { pX     :: Float
  , pY     :: Float
  , pVX    :: Float
  , pVY    :: Float
  , pLife  :: Int
  , pMax   :: Int
  , pR     :: Word8
  , pG     :: Word8
  , pB     :: Word8
  , pSize  :: CInt
  }

-- | Booster click-flow modes (开心消消乐道具点选).
data ToolMode
  = ToolNone
  | ToolHammer          -- next cell click hammers
  | ToolFreeSwap (Maybe Pos)  -- first click stores, second free-swaps
  | ToolCross           -- next cell click cross-clears row+col
  deriving (Eq, Show)

-- | 整个前端的可变状态（放在 IORef 里）；规则状态只在 appHist（当前局面 + 撤销历史，
-- 由通用层 Engine.History 维护，段 3），其余字段都是表现。
data App = App
  { appHist      :: History GameState
  , appSel       :: Maybe Pos
  , appMsg       :: Text
  , appFlash     :: [(Pos, Int)]
  , appPulse     :: Int
  , appAnim      :: Anim
  , appComboShow :: Int  -- ^ 连锁结束后 HUD「N 连击！」总结剩余帧数
  , appComboBest :: Int  -- ^ 总结里显示的本步最高连击
  , appPops      :: [TextPop]  -- ^ 「连击 xN」弹字 + 本轮得分浮字
  , appShake     :: Int  -- ^ 震屏剩余帧数
  , appShakeAmp  :: Int  -- ^ 震屏振幅（逻辑像素）
  , appParticles :: [Particle]
  , appTipFrames :: Int  -- first-level tip/highlight countdown
  , appHelpFrames :: Int -- brief help strip after start / unpause
  , appPaused    :: Bool -- pause + full key help overlay
  , appStartMoves :: MovesLeft
  , appDragFrom  :: Maybe Pos
  , appTool      :: ToolMode
  , appMapOpen   :: Bool  -- level map overlay (选关)
  , appMaxReached :: Int  -- highest unlocked campaign index
  , appArt       :: Maybe Art  -- 贴图集；Nothing 时回退到矩形绘制
  , appScale     :: Float  -- 渲染倍率：物理像素 / 逻辑像素（Retina = 2）；0 表示尚未同步
  , appMouseScale :: Float -- 鼠标倍率：窗口坐标 / 逻辑像素（macOS Retina = 1；MATCH3_SCALE=N 时 = N）
  }

-- | 标题栏 / 键位条的默认提示。
helpKeysMsg :: Text
helpKeysMsg = "H hint | 1 hammer | 2 free-swap | 3 cross | U undo | S shuffle | D daily | M map | R restart | N next | P pause | Esc"

-- | 当前局面（appHist 的当前状态）。
appGame :: App -> GameState
appGame = histNow . appHist

-- | Ignore input while a tween is playing (rules already committed).
animBusy :: App -> Bool
animBusy app = case appAnim app of
  AnimNone -> False
  _ -> True

-- | 当前（或交换之后）的逐轮回放器。
playingPlayer :: App -> Maybe CascadePlayer
playingPlayer app = case appAnim app of
  AnimCascade p -> Just p
  AnimSwap {asNext = AnimCascade p} -> Just p
  _ -> Nothing

-- | 当前（或交换之后）的逐轮回放进度（阶段状态）。
playingCascade :: App -> Maybe Cascade
playingCascade = fmap plStage . playingPlayer
