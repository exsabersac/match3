# 领域词汇（中英对照）

与 `src/Match3/Types.hs`（第 6 刀起是 `Types/*.hs` 的门面）、`GameState`、关卡记录（`src/Match3/Levels/`）及机制模块对齐。标识符保持英文；阅读文档时可用下表对照。

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
| 传送带 | `Belt` = `[Pos]`，`BeltLevel` 的状态（读数 `gsBelts`） | 步末沿环移位；可再连锁 |
| 传送门 | `PortalLevel [(Pos,Pos)]`（读数 `gsPortals`） | 双向；沉降时 A 有子 B 空则传送 |
| 飞碟 | `Ufo{ufoCell,ufoColor}`，`UfoLevel` 的状态（读数 `gsUfos`） | 波末吸正交同色再移格；吸走≠引爆 |
| 地毯 | `CarpetLevel` 的状态（读数 `gsCarpetOpen`）/ `gsCount CountCarpets` | 未铺目标格；清除/饼干腾空/保险箱开启可覆盖 |
| 地面层（扩展槽） | `GroundLayer Ground`（读数 `gsGround`，`[(Pos,(ElementName, 层数))]`），`SlotGround` / `groundRule` | 段 2c：格子下面的层，不占格、不随重力 / 洗牌移动；上方格子每被消除 / 收走一次削一层并按元素计数。内置关卡恒为空，供扩展元素（如果冻）使用 |
| 边缘收集 | `drains :: [Edge]`（`EdgeBottom` / `EdgeLeft` / `EdgeRight` / `EdgeTop`） | 段 2c：收集物到达声明的边即被收走；内置只有饼干（底边） |
| 步末补结算 | `EndRule.erHoles`、`cascadeAfterWith (AfterEnd …)` | 段 2c：步末阶段之后挖掉的格按常规沉降 / 补子 / 连锁；内置元素不触发 |
| 关卡级元素 | `LevelElement` / `SomeLevelElement`（`UfoLevel` / `BeltLevel` / `PortalLevel` / `CarpetLevel` / 核心元素 `GroundLayer`），一局的全部在 `gsLevelElems`；节拍消息 `Refilled` / `EndTicked` / `Settling` / `Covering` / `GroundHit`；Board 层的钩子 `LevelHooks` | 段 4 起飞碟 / 皮带 / 传送门 / 地毯的实现经注册表取（`defaultRegistry` 里注册为 ufo / belt / portal / carpet）；元素类迁移后改为回复流水线节拍消息；第 7 刀起状态在元素值里（取代五个 `GameState` 专用字段，旧名为派生读数），开局状态由 `levelStart` 从关卡记录取；去掉即不生效（地面层除外） |

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

## L / T 形出炸弹（新玩法 1，规则开关 `bomb_shapes`）

对照开心消消乐：同色 5 颗排成 L 形或 T 形时生成「爆炸特效」。本项目的炸弹原来只能靠关卡放置、彩蛋、果汁机得到，形状表里没有 L / T 规则。

| 项 | 规则 |
|---|---|
| 判定 | 同一轮里同色的一条横线和一条竖线（各 ≥3）有公共格 = L / T 形（十字也算）；交点就是公共格 |
| 生成 | 横线在交点放一颗该色炸弹（交点这一轮真被挖空时）；竖线认领但不生成（不会再出竖向直线） |
| 优先级 | 形状表里排在「五连 → 彩虹」之后、「四连 → 直线」之前：带五连的 T 仍出彩虹；带四连的 L / T 出炸弹、不出直线；不交叉的连线照旧 |
| 开关 | 关卡记录 `lvlRules` 含 `"bomb_shapes"` 时打开（关卡级元素 `BombShapes`，每步结算开始时回复 `Shaping`，把 `ltBombRule` 插进本关的形状表）；原有 40 关与每日挑战都关着，行为不变 |
| 关卡 | 第 41 关「爆破」：24 步，碎 10 块石头（左右两堆，炸弹 3×3 一次能砸好几块），`goalCount CountStones 10` |
| 前端 | 炸弹沿用原有贴图与爆炸表现；开关打开的关卡 HUD 关名右侧画炸弹角标 +「L/T 形出炸弹」（视图模型 `gvRules` → `Match3.View.ruleBadge`；网页版经 `state.rules` 在关卡面板里画同样的角标） |

取舍说明：
- **按关打开，不全局打开**：全局打开会改变原有 40 关的对局（金标准会大面积变化），需要另行批准；规则开关让新旧关卡并存。
- **交点即落点**：不按交换落点放炸弹（交换落点可能不在交点上），这样 L / T 的炸弹位置固定可预期。

## 魔法石（新玩法 2，`Custom "magic_stone"`）

对照开心消消乐的魔法石：固定在格子上的石头，旁边消除几次就充满，充满后放出横竖两道光清掉整行整列。

| 项 | 规则 |
|---|---|
| 本体 | `Custom "magic_stone" k`（`Element.Builtin.Obstacle` 的 `MagicStone`），固定格：不下落、挡交换、无色（不参与匹配）、洗牌原地保留；平时打不动（锤子 / 十字 / 特效都不影响它） |
| 状态 | `k` = 充能格数 0–3；4 = 发射中（只在步末那一轮出现） |
| 充能 | 正交邻格有**真消除**的每一轮充 1 格（同一轮多个邻格也只 1 格），满 3 为止；本轮被直接命中的魔法石不充能（邻格规则 180） |
| 发射 | 玩家交换的步末（`PhaseTick` 20，倒计时之后）：满 3 格的魔法石转为发射中（记一条 `EvTick "magic_stone"` 步末效果），以所在**整行 + 整列**为种子引爆，和倒计时爆炸同一段连锁；种子里的特殊块照常点火、障碍照常受击（石头削一层等） |
| 归零 | 发射中的魔法石被自己的种子命中后归零（`Absorb` → 0 格）；发射清掉的邻格不会给它再充能（本轮被直接命中） |
| 道具 | 锤子 / 自由交换 / 十字没有 `PhaseTick` 步末：充满的魔法石等到下一次交换的步末才发射 |
| 放置 | 放置表 `Place "magic_stone" [AInt k] 格`，参数 = 初始充能（缺省 0，夹到 0–3） |
| 关卡 | 第 42 关「魔石」：24 步，四块魔法石在 (2,2)(2,5)(5,2)(5,5)，8 块双层石头都在它们的行 / 列尽头，`goalCount CountStones 8` |
| 前端 | 贴图 `magic_stone_0..3`：紫色八角石板 + 十字符文，底部 3 个充能槽点亮 k 个；满 3 格外发光、符文变金并轻微浮动（提示本步末会发射）；几何版为紫方块 + 3 个金色小格 |

取舍说明：
- **只在步末发射**：和原作一样不在连锁中途打断，发射的整行整列走现成的种子引爆（同倒计时），不改流水线。
- **永久存在**：原作的魔法石可以反复充能发射，这里同样发射后归零、继续充能；它不计入任何目标，关卡目标交给它打到的东西（第 42 关是石头）。
- **道具不触发**：道具结算只有蔓延步末（`PhaseSpread`），保持原有步末表不变。

## 目标与结局

| 中文 | 类型 | 说明 |
|------|------|------|
| 分数目标 | `goalScore t`（`Show`：`GoalScore t`） | `gsScore` |
| 单色收集 | `goalCollect 色 n`（`GoalCollect`） | `gsCount (CountColor 色)` |
| 多色收集 | `goalColors [(色, n)]`（`GoalCollectMulti`） | 每色 `gsCount (CountColor 色)` ≥ 配额；进度 = Σ min(配额, 该色数) |
| 碎石/宝箱/蜂蜜/气球/饼干/蛋糕/保险箱/飞碟/地毯 | `goalCount 键 n`（`GoalClearStone` … `GoalCarpet`） | `gsCounts` 里对应的键（第 4 刀前是 10 个专用字段）：`gsCount CountStones` / `CountChests` / `CountHoney` / `CountBalloons` / `CountCookies` / `CountCakes` / `CountSafes` / `CountUfo` / `CountCarpets` |
| 按名字计数 | `goalCount (CountNamed 名字) N`（`GoalNamed 名字 N`） | 段 2c：`gsCount (CountNamed 名字)` 累计 ≥ N（扩展元素经能力 `counts` / `countsDiff (CountNamed 名字)`，即查询 `counter` / `diffCounter`，计数） |

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
| 关卡表 | `allLevels`（42：前 38 关 + 段 5 追加的第 39 关「果冻」、第 40 关「气泡」+ 新玩法 1 的第 41 关「爆破」+ 新玩法 2 的第 42 关「魔石」；`Match3.Levels.Campaign`） | 名称中文；见 README 表；第 42 关是终章（过关为 `Won`；新关追加后，原终章过关由 `Won` 变为 `LevelClear` 进入下一关）；按下标取关用 `lookupLevel`（`Maybe`） |
| 关卡记录 | `Level`（`lvlIndex` / `lvlName` / `lvlMoves` / `lvlGoal` / `lvlPlacements` / `lvlBelts` / `lvlPortals` / `lvlUfos` / `lvlCarpets` / `lvlGround` / `lvlRules`） | 第 6 刀：一关的全部数据（步数、目标、装饰放置表、皮带、传送门、飞碟、地毯、地面层）在同一条记录里；`lvlRules`（新玩法 1）= 本关打开的规则开关名，缺省空 |
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
| 步末效果 | `EndEffect { endEffectKind, endEffectElement, endEffectItems }`（第 7 刀 7b 起的通用形状） | 事件类型 `EvTick` / `EvBelt` / `EvSpread` / `EvMove` + 元素名 countdown / belt / vine·choco·steam / snail；`applyEndEffect` 逐项重放回盘面 |
| 步末一项 | `EndItem { eiFrom, eiTo, eiCell, eiBack }` | 目标格写成 `eiCell`，`eiBack` 为 `Just` 时来源格写成它；蜗牛 `eiFrom == eiTo` 表示碰壁掉头，新朝向 = `endItemDir` |
| 效果事件 | `Event { evKind, evWave, evElement, evCells, evAmount }`、`traceEvents` | 回放脚本按时间线展开：`EvBlast` / `EvClear` / `EvHit` / `EvDrain` / `EvScore` / `EvCombo` / 步末 `EvTick` / `EvBelt` / `EvSpread` / `EvMove` / `EvShuffle` |
| 元素（类 / 注册表） | `Element`（类型类，一种元素 = 一个类型 + 一个 instance；第 9 刀起类只有 `name` / `toCell` / `caps`）、能力记录 `Caps`（五组：匹配与交换 `MatchCaps` / 消除与受击 `HitCaps` / 重力与移动 `MoveCaps` / 计数与目标 `CountCaps` / 步末与变化 `StepCaps`，带按原型的缺省值，元素用 `Match3.Element.Caps` 的简写只声明用到的几项）、`SomeElement`、修饰器 `Modifier`、`Registry`（名字 → 构造器 `Entry`）、`defaultRegistry` | 一种格子内容在各时机的反应，状态在元素值里（见 [architecture.md](architecture.md#元素框架与事件)）；`gsCounts` 的 `CountNamed 名字` 记注册表元素的具名计数（`namedCounts` 列出） |
| 元素名 / 自定义状态 | `ElementName`、`CustomState`（`Match3.Types.Name`，第 6b 刀起为 newtype） | 元素名是注册表 / 放置表 / 地面层 / `Custom` 格 / 效果事件 / `CountNamed` 的键；`Custom 名字 状态` 的状态值包在 `CustomState` 里。两者打印与底层字符串 / 整数相同（`Custom "bubble" 1`） |
| 成对交换规则 / 开启规则 | `SwapRule`（`swapRule`）/ `OpenRule`（`openRule`） | 段 4：彩虹取色、特殊 × 特殊合成是成对交换规则（交换两端的组合直接给起手种子）；彩蛋是开启规则（开出的格本轮坐住） |
| 特殊块形状规则 | `ShapeRule { shapeName, shapeSpawn }`、`ShapeCtx`（第 8 刀） | 匹配形状 → 生成哪种特殊块；有序表（注册表 `shapeRules`），每条连线取第一条认领它的规则。内置 `builtinShapeRules`：5 连彩虹、横 4 横消、竖 4 竖消；L / T 形 → 炸弹（`ltBombRule`）只在规则开关 `bomb_shapes` 打开的关卡插入（见下文「L / T 形出炸弹」） |
| 特殊块组合规则 | `ComboRule { comboName, comboFirst, comboSecond, comboSeeds }`（第 8 刀） | 两个特殊块交换时的组合效果；有序表（注册表 `comboRules`），两个方向都试（对称），整张表并成成对交换规则 20。内置 `builtinComboRules`：炸弹 × 炸弹、直线 × 直线、直线 × 炸弹、彩虹 × 直线 |
| 补子策略 | `RefillPolicy { refillName, refillCell }`、`RefillCtx`（第 8 刀） | 沉降后空洞补什么：注册表的策略（`refillPolicyWith`，缺省 `defaultRefill` = 随机五色普通宝石），关卡级元素可回复 `Refilling` 换掉；`colorsRefill n` = 只用前 n 色 |
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
