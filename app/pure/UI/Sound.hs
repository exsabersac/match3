-- | 音效钩子（第 10 刀预留接口，不依赖任何音频库、不实际发声）。
--
-- 数据流：回放阶段机（ComboFx）每切换一个阶段吐出一个 'CascadeEvent'，
-- 'cascadeEventKinds' 把它还原成规则层的效果事件种类，再逐个查表现表的音效钩子
-- （'UI.Presentation.effectSound'，即表项的 prSound 字段）。查到的音效名由 UI.Playback
-- 追加进 App 的 appSounds 队列，UI.Plugin 每帧推进后调用 'playSounds' 并清空队列。
--
-- 内置表里所有 prSound 都是 Nothing，所以队列永远为空、'playSounds' 是空操作：
-- 画面、帧序和规则结果与没有这个钩子时完全相同。接入真实音频时只需
--   1. 在 UI.Presentation.presentationTable 里给想出声的事件填上 @prSound = Just "名字"@；
--   2. 把 'playSounds' 换成真正的播放实现（例如按名字查已加载的音效再播放）。
-- 依赖：ComboFx、UI.Presentation、Match3.Element.Event。纯模块（'playSounds' 之外），网页可共用。
module UI.Sound
  ( cascadeEventKinds
  , cascadeSounds
  , playSounds
  ) where

import ComboFx (CascadeEvent (..), EndStage (..), WaveView (..))
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Match3.Core (EndStep (..))
import Match3.Element.Event (EndEffect (..), Event (..), EventKind (..))
import UI.Presentation (SoundName, effectSound)

-- | 回放阶段事件对应的效果事件种类：进入高亮 = 连击（仅连击轮，与「连击 xN」弹字同时）；
-- 进入消失 = 本轮的轮内效果种类（爆炸 / 消除 / 波及 / 底收 / 得分，去重、按首次出现的顺序）；
-- 步末段 = 该段每一步的效果种类。
cascadeEventKinds :: CascadeEvent -> [EventKind]
cascadeEventKinds ev = case ev of
  EvHighlight k _ -> [EvCombo | k >= 2]
  EvVanish _ v -> nub [evKind e | e <- wvEvents v, evKind e /= EvCombo]
  EvEndStage st -> map (endEffectKind . esEffect) (stSteps st)

-- | 回放阶段事件要触发的音效名（按表现表的音效钩子查；内置全部为空）。
cascadeSounds :: CascadeEvent -> [SoundName]
cascadeSounds = mapMaybe effectSound . cascadeEventKinds

-- | 播放一批音效：预留接口，当前为空操作（不引入音频依赖）。
playSounds :: [SoundName] -> IO ()
playSounds _ = pure ()
