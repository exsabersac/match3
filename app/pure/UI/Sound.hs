-- | 回放阶段 → 音效名（纯部分，不依赖任何音频库、自己不发声）。
--
-- 数据流：回放阶段机（ComboFx）每切换一个阶段吐出一个 'CascadeEvent'，
-- 'cascadeEventKinds' 把它还原成规则层的效果事件种类，再逐个查表现表的音效名
-- （'UI.Presentation.effectSound'，即表项的 prSound 字段；内置表：消除 = "clear"、爆炸 = "special"，其余无声）。
-- 查到的音效名由 UI.Playback 追加进 App 的 appSounds 队列（交换结果的 swap / illegal / win / lose 由 UI.Input 追加），
-- UI.Plugin 每帧推进后把队列交给桌面的 UI.Audio.cue 播放并清空。
--
-- 'playSounds' 是空操作，桌面版不用它（真正播放在 UI.Audio；网页在 JS 端用 Web Audio）。
-- 新事件要出声：在 UI.Presentation.presentationTable 给它填 @prSound = Just "名字"@，并在 UI.Audio 加载同名素材。
-- 依赖：ComboFx、UI.Presentation、Match3.Element.Event。纯模块。
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

-- | 回放阶段事件要触发的音效名（按表现表的 prSound 查）。
cascadeSounds :: CascadeEvent -> [SoundName]
cascadeSounds = mapMaybe effectSound . cascadeEventKinds

-- | 空操作（不引入音频依赖）；桌面播放在 UI.Audio.cue。
playSounds :: [SoundName] -> IO ()
playSounds _ = pure ()
