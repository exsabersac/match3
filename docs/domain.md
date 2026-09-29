# 领域词汇（中英对照）

与 `src/Match3/Types.hs`、`GameState` 及机制模块对齐。标识符保持英文；阅读文档时可用下表对照。

## 棋盘与基本单位

| 中文 | 英文 / 类型 | 说明 |
|------|-------------|------|
| 棋盘 | `Board` = `[[Cell]]` | 固定 `boardSize = 8`（8×8） |
| 格 / 坐标 | `Pos` = `(Int, Int)` | 行、列，左上为原点 |
| 单元格内容 | `Cell` / `CellContents` | 宝石、障碍、装饰等之和类型 |
| 颜色 | `Color` = `C1`…`C5` | 五色；UI 常映射为红/绿/蓝等 |
| 宝石种类 | `GemKind` | `Normal` / `LineH` / `LineV` / `Bomb` / `Rainbow` |
| 叠层 | `CellOverlay` | 草/藤/巧/雾/锁链/火箭冰冻/窗帘/蒸汽 |
| 冰层 | gem 上 `Int` ice | 匹配削层；末层同波清除；≠ `Freeze` |

## 宝石与特殊块

| 中文 | 构造 / 谓词 | 要点 |
|------|-------------|------|
| 普通宝石 | `mkGem` / `Gem c Normal …` | 三消基本单位 |
| 直线 | `LineH` / `LineV` | 四消生成；激活清整行或整列 |
| 炸弹 | `Bomb` | 合成用；激活清 3×3 一类范围（见 Combos/扩展） |
| 彩虹 | `Rainbow` / `isRainbow` | 五消生成；与搭档交换清搭档色；同色扩展为空操作 |
| 倒计时炸弹 | `Countdown c n` | 可按色匹配；步末 −1；归零 3×3 |
| 双面块 | `Flip front back` | 正面色参与匹配；命中翻成背面 Normal |

## 障碍与收集物（占格，一般不可匹配）

| 中文 | 类型 | 行为摘要 |
|------|------|----------|
| 石头箱 | `Stone n` | 挡交换；邻消削层；`GoalClearStone` |
| 宝箱 | `Chest n` | 邻消/直线·炸弹·锤削层；`GoalChest` |
| 蜂蜜罐 | `Honey n` | 同上；`GoalHoney` |
| 蛋糕 | `Cake n` | 分层障碍（≠饼干）；`GoalCake` |
| 保险箱 | `Safe n` | 削层后开出 `Cookie`；`GoalSafe` |
| 气球 | `Balloon c` | 同色邻消才爆；`GoalBalloon` |
| 饼干 | `Cookie` | 重力下落；仅底行收集；中盘特殊/锤无效；`GoalCookie` |
| 魔法帽 | `MagicHat` | 邻消触发换色；对直线/炸弹/锤直接清免疫 |
| 果汁机 | `Maker c n` | 同色邻消充能；归零变 Bomb |
| 彩蛋 | `Surprise` | 打开 → 特殊或 3×3 |
| 染色瓶 | `Bottle c` | 邻消染正交邻格 |
| 时间精灵 | `TimeSpirit` | 邻消 +2 步 |
| 蜗牛 | `Snail dr dc` | 挡交换；步末爬一格推宝石 |

## 叠层（overlay，挂在宝石上）

| 中文 | 构造 | 交换 | 匹配 | 清除方式 |
|------|------|------|------|----------|
| 草 | `Grass` | 可 | 可 | 本格进入真清除 |
| 藤蔓 | `Vine` | 可 | 可 | 清除；否则步末蔓延 |
| 巧克力 | `Choco` | 可 | 可 | 邻消清除；否则步末蔓延 |
| 迷雾 | `Fog n` | — | 雾下不可匹配 | 邻消揭层 |
| 锁链 | `Chain n` | 不可 | 不可 | 邻消揭层；软锁特殊不激活 |
| 火箭冰冻 | `Freeze n` | 不可 | **可** | 邻消揭层 |
| 窗帘 | `Curtain n` | 可（实现允许换） | 不可 | 邻消揭层；软锁特殊不激活 |
| 蒸汽 | `Steam` | — | 挡匹配 | 邻消扑灭；否则步末蔓延 |

**软锁（soft-lock）**：`specialActivates` —— `ice>1` 或 `Chain` / `Curtain` 时直线/炸弹/彩虹不点火（削层/揭层但不引爆）。`Fog` / `Steam` / `Freeze` 不构成此类软锁。

## 地图机制（非占格或外置状态）

| 中文 | 字段 / 类型 | 说明 |
|------|-------------|------|
| 传送带 | `Belt` = `[Pos]`，`gsBelts` | 步末沿环移位；可再连锁 |
| 传送门 | `gsPortals :: [(Pos,Pos)]` | 双向；沉降时 A 有子 B 空则传送 |
| 飞碟 | `Ufo{ufoCell,ufoColor}`，`gsUfos` | 波末吸正交同色再移格；吸走≠引爆 |
| 地毯 | `gsCarpetOpen` / `gsCarpetsCovered` | 未铺目标格；清除/饼干腾空/保险箱开启可覆盖 |

## 目标与结局

| 中文 | 类型 | 说明 |
|------|------|------|
| 分数目标 | `GoalScore` | `gsScore` |
| 单色收集 | `GoalCollect` | `gsCollected` + 颜色袋 |
| 多色收集 | `GoalCollectMulti` | `gsColorBag` |
| 碎石/宝箱/蜂蜜/气球/饼干/蛋糕/保险箱/飞碟/地毯 | 对应 `Goal*` | 各 `gs*Cleared` / `gsUfoCollected` / `gsCarpetsCovered` 等 |
| 步数 | `gsMoves` / `MovesLeft` | 成功步 −1；时间精灵可 +2 |
| 步数银行 | `carryMovesBonus` | 战役过关最多带 3 步 |
| 交换无效 | `InvalidSwap` | 越界/非邻/无次数等 |
| 无匹配回滚 | `NoMatch` | 盘面不变（清提示） |
| 步已应用 | `MoveApplied Score` | 本步得分增量 |
| 战役过关 | `LevelClear Score Int` | 下一关 0-based 下标 |
| 通关 / 失败 | `Won` / `Lost` | 每日通关为 `Won`（不推进战役解锁） |

## 道具

| 中文 | 状态 / API | 说明 |
|------|------------|------|
| 锤子 | `gsHammers` / `useHammer` | 不耗步；对 Maker/Snail/Bottle/Hat/Cookie 免疫且不扣次 |
| 任意交换 | `gsFreeSwaps` / `useFreeSwap` | 任意两格；不耗步；不 tick 倒计时 |
| 十字清除 | `gsCrossClears` / `useCrossClear` | 整行+整列种子 |

## 战役与每日

| 中文 | API | 说明 |
|------|-----|------|
| 关卡表 | `allLevels`（38） | 名称中文；见 README 表 |
| 选关解锁 | `unlockAfterOutcome` | 每日 `Won` **不**抬地图进度 |
| 每日 | `newDailyGame` / `dailySeed` | 日期种子；10 种目标轮换 |
| 三星 | `starRating start left` | ≥40% 印制步剩余 → 3★；≥15% → 2★；否则 1★ |

## 回放与表现

规则结果不变；以下词条描述一步结算后「怎么播」。详见 [`rules-pipeline.md` §8](rules-pipeline.md#8-与前端的边界) 与 [`ui-art.md` 连击表现](ui-art.md#连击表现逐轮回放)。

| 中文 | 类型 / API | 说明 |
|------|------------|------|
| 轮 / 波（一次消除 → 下落 → 补子） | `CascadeWave`（`cwBefore` / `cwCleared` / `cwDrained` / `cwHoles` / `cwAfter` / `cwScore`） | 一步里的第 k 轮；UFO 吸收单独算一轮 |
| 连击 xN | `gsCombo`、`MoveFx.fxCombo` | 本步有清除的轮数；第 2 轮起弹「连击 xN」，x2 / x3 / x4 / x5+ 分级样式与震屏 |
| 连击总结 | HUD「N 连击！」 | 全部播完后最高连击 ≥ 2 才显示，约 1.6 s |
| 得分浮字 | 「+N」 | 每轮 `cwScore`，从本轮消除格中心飘起 |
| 回放脚本 | `MoveTrace { mtStart, mtWaves, mtFinal, mtEnd, mtGen, mtShuffle }` | `traceSwap` / `traceFreeSwap` / `traceHammer` / `traceCrossClear` 生成；`mtFinal` / `mtGen` 为洗牌前的稳定盘与生成器，`mtShuffle` 为洗牌后盘面 |
| 步末效果 | `EndStep { esAfterWaves, esBefore, esAfter, esEffect }` | 插在第 `esAfterWaves` 轮之后的非消除变化 |
| 步末效果种类 | `EndEffect` = `EndCountdownTick` / `EndBeltShift` / `EndSpread SpreadKind` / `EndSnail [SnailMove]` | 倒计时减一 / 皮带移位 / 藤·巧·蒸汽蔓延 / 蜗牛爬行；`applyEndEffect` 可重放回盘面 |
| 蜗牛一步 | `SnailMove { smFrom, smTo, smDir, smPushed }` | `smFrom == smTo` 表示碰壁掉头 |
| 效果事件 | `Event { evKind, evWave, evElement, evCells, evAmount }`、`traceEvents` | 回放脚本按时间线展开：`EvBlast` / `EvClear` / `EvHit` / `EvDrain` / `EvScore` / `EvCombo` / 步末 `EvTick` / `EvBelt` / `EvSpread` / `EvMove` / `EvShuffle` |
| 元素（定义 / 注册表） | `ElementDef`、`Registry`、`defaultRegistry` | 一种格子内容在各时机的反应（见 [architecture.md](architecture.md#元素框架与事件)）；`gsElementCounts` 记注册表元素的具名计数 |
| 自动洗牌（表现段） | 前端 `StShuffle` | **不是** `EndEffect`：`mtFinal` ≠ 结算后 `gsBoard` 时前端追加，22 帧 |
| 本步特效 | `MoveFx { fxCombo, fxCleared }` / `moveFx` | 边沿触发；`NoMatch` / `InvalidSwap` / 已终局为空 → 不重播上一步 |
| 回放加速 | 点击 / 空格 / 回车 / `N` | 每帧推进 3 帧（`fastStep`）；播放期间锁定交换、道具、撤销、洗牌 |
