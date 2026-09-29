# 领域词汇（中英对照）

与 `src/Match3/Types.hs`、`GameState` 及机制模块对齐。标识符保持英文；阅读文档时可用下表对照。

## 棋盘与基本单位

| 中文 | 英文 / 类型 | 说明 |
|------|-------------|------|
| 棋盘 | `Board`（`Array (Int,Int) Cell`，`boardFromRows` / `boardRows` 与行列表互转） | 固定 `boardSize = 8`（8×8）；读格 O(1) |
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
| 气泡 | `Custom "bubble" 1`（段 5） | 无色；挡交换；随重力下落；邻格有真消除（任意颜色）或被直接命中即破，一次就破；`GoalNamed "bubble" N` |

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
| 地毯 | `gsCarpetOpen` / `gsCount CountCarpets` | 未铺目标格；清除/饼干腾空/保险箱开启可覆盖 |
| 地面层（扩展槽） | `gsGround :: Ground`（`[(Pos,(名字, 层数))]`），`SlotGround` / `groundRule` | 段 2c：格子下面的层，不占格、不随重力 / 洗牌移动；上方格子每被消除 / 收走一次削一层并按元素计数。内置关卡恒为空，供扩展元素（如果冻）使用 |
| 边缘收集 | `drains :: [Edge]`（`EdgeBottom` / `EdgeLeft` / `EdgeRight` / `EdgeTop`） | 段 2c：收集物到达声明的边即被收走；内置只有饼干（底边） |
| 步末补结算 | `EndRule.erHoles`、`cascadeAfterWith (AfterEnd …)` | 段 2c：步末阶段之后挖掉的格按常规沉降 / 补子 / 连锁；内置元素不触发 |
| 关卡级元素 | `LevelElement` / `SomeLevel`（`UfoLevel` / `BeltLevel` / `PortalLevel` / `CarpetLevel`），节拍消息 `Refilled` / `EndTicked` / `Settling` / `Covering` | 段 4 起飞碟 / 皮带 / 传送门 / 地毯的实现经注册表取（`defaultRegistry` 里注册为 ufo / belt / portal / carpet）；元素类迁移后改为回复流水线节拍消息；状态仍在上面各自的 `GameState` 字段；去掉即不生效 |

## 双层果冻与气泡（段 5）

两种新元素都只经注册表（`Element.Builtin.Ground` 的 `Jelly` 与 `Element.Builtin.Collectible` 的 `Bubble`）与段 2c 的白名单钩子接入，主流程（`Board.*` / `Game.*` / `Engine.*`）没有改动（源码扫描 `jb_main_flow_untouched_scan`）。

| | 双层果冻 `jelly` | 气泡 `bubble` |
|---|---|---|
| 所在层 | 地面层（`SlotGround`，`gsGround` 里 `(格, ("jelly", 层数))`），**不占格** | 本体（`Custom "bubble" 1`），**占格** |
| 层数 / 耐久 | 2 层（关卡里铺满层；也可单层） | 1（一次就破） |
| 怎么去掉 | 上方格子每被**消除**（匹配、特殊块、道具、连锁都算）或被边缘收走一次，去一层（`groundRule`：2 → 1 → 清掉） | 正交邻格有**真消除**（任意颜色，`adjacentRule` 顺序 170）即破；被**直接命中**（锤子 / 十字 / 直线 / 炸弹的爆炸范围）也破（`onHit = Destroy`） |
| 计数 / 目标 | 每去一层计 1（`CountNamed "jelly"`）；`GoalNamed "jelly" N` 按**层**数 | 每破一个计 1（`CountNamed "bubble"`）；`GoalNamed "bubble" N` |
| 交换 / 匹配 | 不影响（上面的宝石照常交换、匹配） | 挡交换；无色，不参与匹配（三个气泡连一排也不消） |
| 重力 / 洗牌 | 不随重力、不随洗牌移动（地面层固定在格上） | 随重力下落（下方格被清空就往下落）；不穿传送门；洗牌时原位保留 |
| 撤销 | 层数随快照恢复 | 随盘面恢复 |
| 关卡 | 第 39 关「果冻」：16 格双层（中间两行 × 6 + 四个内角），24 步，`GoalNamed "jelly" 32` | 第 40 关「气泡」：12 个气泡散布在第 1–6 行，22 步，`GoalNamed "bubble" 12` |

取舍说明：
- **果冻按层计目标**：目标数字 = 总层数，HUD 进度每去一层 +1，玩家能看到「消一次、薄一层」；不另设「整格清完才算」的计数。
- **气泡一次就破、不分颜色**：和已有的气球（同色邻消才爆）、蜂蜜 / 宝箱（多层）区分开，是「最软」的障碍；难点来自它**会下落、挡交换**——落到底部或角落后只能靠旁边成消或道具。
- **气泡不做「上浮」**：真正的气泡往上飘需要反向重力，是主流程改动；这里保持普通重力，只经注册表字段接入。
- **气泡不可交换**：可交换的话需要给无色格定义「交换后是否成立」的新规则，超出白名单；挡交换沿用原型 `Blocker` 的默认方法。
- 两关追加在第 38 关之后，前 38 关不改；因此第 38 关「织毯」不再是终章（过关由 `Won` 变为 `LevelClear` 进入第 39 关），终章变成第 40 关。

## 目标与结局

| 中文 | 类型 | 说明 |
|------|------|------|
| 分数目标 | `goalScore t`（`Show`：`GoalScore t`） | `gsScore` |
| 单色收集 | `goalCollect 色 n`（`GoalCollect`） | `gsCount (CountColor 色)` |
| 多色收集 | `goalColors [(色, n)]`（`GoalCollectMulti`） | 每色 `gsCount (CountColor 色)` ≥ 配额；进度 = Σ min(配额, 该色数) |
| 碎石/宝箱/蜂蜜/气球/饼干/蛋糕/保险箱/飞碟/地毯 | `goalCount 键 n`（`GoalClearStone` … `GoalCarpet`） | `gsCounts` 里对应的键（第 4 刀前是 10 个专用字段）：`gsCount CountStones` / `CountChests` / `CountHoney` / `CountBalloons` / `CountCookies` / `CountCakes` / `CountSafes` / `CountUfo` / `CountCarpets` |
| 按名字计数 | `goalCount (CountNamed 名字) N`（`GoalNamed 名字 N`） | 段 2c：`gsCount (CountNamed 名字)` 累计 ≥ N（扩展元素经 `counter` / `diffCounter = CountNamed 名字` 计数） |

目标是数据（第 5 刀，`Match3.Goal`）：`LevelGoal` = 一组配额，每项 = 度量（分数或一个计数键）+ 目标值。达成 ⟺ 每项度量 ≥ 目标值；进度（HUD / 标题 / 网页）单项 = 度量本身（不截断），多项 = Σ min(目标值, 度量)；目标值 = 各项之和。结算、HUD、标题栏、网页版都调同一组函数（`goalMet` / `goalProgress` / `goalTarget`，状态上的简写 `gsGoalMet` / `gsProgress`）。括号里是 `Show` 的写法：与第 5 刀前的构造器写法逐字相同（元素查询快照对 `show` 取散列、网页版按首词取目标种类）。`gsCollected` / `gsColorBag` 第 5 刀起不是字段，是由 `gsCounts` 派生的读数（旧字段的值，`Show` 仍在原位置打印）。
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
| 关卡表 | `allLevels`（40：前 38 关 + 段 5 追加的第 39 关「果冻」、第 40 关「气泡」） | 名称中文；见 README 表；第 40 关是终章（过关为 `Won`） |
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
| 元素（类 / 注册表） | `Element`（类型类，一种元素 = 一个类型 + 一个 instance）、`SomeElement`、修饰器 `Modifier`、`Registry`（名字 → 构造器 `Entry`）、`defaultRegistry` | 一种格子内容在各时机的反应，状态在元素值里（见 [architecture.md](architecture.md#元素框架与事件)）；`gsCounts` 的 `CountNamed 名字` 记注册表元素的具名计数（`namedCounts` 列出） |
| 成对交换规则 / 开启规则 | `SwapRule`（`swapRule`）/ `OpenRule`（`openRule`） | 段 4：彩虹取色、特殊 × 特殊合成是成对交换规则（交换两端的组合直接给起手种子）；彩蛋是开启规则（开出的格本轮坐住） |
| 可改色 / 可推动 | `recolorable` / `pushable` | 段 4：魔法帽 / 染色瓶改色、蜗牛推动的对象由注册表判定；内置 = 宝石各种类、倒计时、双面块 |
| 自动洗牌（表现段） | 前端 `StShuffle` | **不是** `EndEffect`：`mtFinal` ≠ 结算后 `gsBoard` 时前端追加，22 帧 |
| 本步特效 | `MoveFx { fxCombo, fxCleared }` / `moveFx` | 边沿触发；`NoMatch` / `InvalidSwap` / 已终局为空 → 不重播上一步 |
| 回放加速 | 点击 / 空格 / 回车 / `N` | 每帧推进 3 帧（`fastStep`）；播放期间锁定交换、道具、撤销、洗牌 |
| 波次视图 | `ComboFx.WaveView { wvWave, wvEvents }`、`wvCleared` / `wvScore` | 一轮的底图快照 + 本轮效果事件；高亮 / 消失 / 粒子 / 得分浮字读事件 |

## 多游戏接口

与三消无关的通用词条（见 [architecture.md](architecture.md#多游戏接口)）。

| 中文 | 类型 / API | 说明 |
|------|------------|------|
| 游戏 | `Engine.Game.Game cfg s a e o r` | 一条函数记录：开局 / 推进一步 / 结局判定 / 候选动作 / 状态摘要 / 效果映射；`r` 是整步报告（`stepReport`） |
| 撤销历史 | `Engine.History.History s` / `Undoable a`（`Act a` / `Undo`）/ `withHistory` | 段 3：历史只存在这里（`GameState` 不再带历史）；终局后仍可撤销 |
| 一步结果 | `Step { stepState, stepEvents, stepOutcome, stepAccepted }` | 被拒时状态不变、没有事件 |
| 种子 | `Seed = Int` | 只在开局 `gameNew` 用；之后随机数只来自状态 |
| 通用效果 | `Engine.Effect.Effect { efBeat, efKind, efSubject, efSpots, efAmount }` | 播放层只认它；同一节拍的效果同时播 |
| 阶段机 / 播放器 | `Engine.Playback.Stages`、`Player { plStage, plFrame, plFast }` | 游戏给出阶段长度与后继，播放器管帧号与加速 |
| 外壳 / 插件 | `Shell.Loop.runShell`、`Plugin` | SDL 窗口与固定步长主循环；具体游戏的输入映射与绘制作为插件接入 |
| 三消动作 | `Match3.Engine.Action` = `Swap` / `Hammer` / `FreeSwap` / `CrossClear` / `Hint` / `Shuffle`（撤销是通用层的 `Undo`） | 一次结算得到 `Played`（状态、`Outcome`、回放脚本、`MoveFx`、事件），经 `gameStep` 的 `stepReport` 带回；外壳用 `match3Shell` |
