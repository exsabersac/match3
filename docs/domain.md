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
| 变色龙 | `Custom "chameleon" k`（`chameleonCell` / `chameleonColor`） | 新玩法 7：k = 当前颜色下标；按当前颜色匹配；交换的步末按固定顺序换色（见下文） |

## 障碍与收集物（占格，一般不可匹配）

| 中文 | 类型 | 行为摘要 |
|------|------|----------|
| 石头箱 | `Stone n` | 挡交换；邻消削层；`GoalClearStone` |
| 宝箱 | `Chest n` | 邻消/直线·炸弹·锤削层；`GoalChest` |
| 蜂蜜罐 | `Honey n` | 同上；`GoalHoney` |
| 蛋糕 | `Cake n` | 分层障碍（≠饼干）；`GoalCake` |
| 保险箱 | `Safe n` | 削层后开出 `Cookie`；`GoalSafe` |
| 气球 | `Balloon c` | 同色邻消才爆；`GoalBalloon` |
| 饼干 | `Cookie` | 重力下落；仅底行收集；中盘特殊/锤无效；`GoalCookie`；新玩法 6 起可由掉落口（`lvlDrops`）在补子时陆续补进场 |
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
| 地面层（扩展槽） | `GroundLayer Ground`（读数 `gsGround`，`[(Pos,(ElementName, 层数))]`），`SlotGround` / `groundRule` | 段 2c：格子下面的层，不占格、不随重力 / 洗牌移动；上方格子每被消除 / 收走一次削一层并按元素计数。内置关卡恒为空，供扩展元素（如果冻）使用；新玩法 8 的魔法地格（`"magic"`）也在这一层，但没有 `groundRule`（不被消耗、不计数），只带扩爆规则 `widenRule` |
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

## 毛球（新玩法 3，`Custom "fuzzball"`）

对照开心消消乐的毛球：占住格子的小毛团，挡着交换、跟着宝石往下掉，旁边一消就被消灭；每走一步它会自己跳到旁边的格子上。

| 项 | 规则 |
|---|---|
| 本体 | `Custom "fuzzball" 1`（`Element.Builtin.Actor` 的 `Fuzzball`，状态值不用），原型 `blocker`：挡交换、无色（不参与匹配）、随重力下落、洗牌原地保留 |
| 消灭 | 正交邻格有**真消除**的那一轮被消灭（邻格规则 190，并入清除格）；被特效 / 锤子 / 十字直接命中也消灭（`breaks`）；每消灭一个计 `CountNamed "fuzzball"` |
| 跳格 | 玩家交换的步末（`PhaseMove` 20，蜗牛之后）：每个毛球（行优先）跳到一个正交相邻的**普通宝石**格（无冰、无叠层、非特殊块），与那颗宝石换位；记一条 `EvBelt "fuzzball"` 步末效果，每跳一次两项（毛球 原格 → 新格、宝石 新格 → 原格） |
| 不跳 | 四周没有普通宝石（障碍 / 特殊块 / 另一个毛球 / 带冰或叠层的格）、皮带本步移过的格、传送门端点（当墙）、同一步已被别的毛球跳过的格都不选；没有候选就原地不动 |
| 随机 | 候选格按「本步步末开始时的盘面散列 xor 毛球位置散列」取模选出——同一盘面结果确定，**不消耗 `gsGen`**：没有毛球的关卡随机序列、盘面与快照都不变（`fz_other_levels_unchanged`） |
| 道具 | 锤子 / 自由交换 / 十字没有 `PhaseMove` 步末：道具那一步毛球不跳 |
| 跳后成消 | 跳格换过来的宝石若凑成三连，由步末补结算（`settle`）照常消除，毛球旁边的消除同样会消灭它 |
| 放置 | 放置表 `Place "fuzzball" [] 格` |
| 关卡 | 第 43 关「毛球」：22 步，14 个毛球分散在全盘（隔行错开：(0,1) (0,6) (1,3) (2,0) (2,5) (3,2) (3,7) (4,4) (5,1) (5,6) (6,3) (6,7) (7,0) (7,5)），`goalCount (CountNamed "fuzzball") 14`。2026-09-30 加难：原布局 10 个挤在上三行，按提示走种子 1 三步就过（30 个种子里 26 局过关），现为 19/30、种子 1 要 14 步 |
| 前端 | 贴图 `fuzzball`：灰粉色毛团（一圈绒球轮廓 + 呆毛）+ 两只大眼睛，轻微浮动；几何版为粉灰方块 + 四角绒毛 + 两只眼睛；跳格借用皮带的平移动画（两格互相滑过去） |

取舍说明：
- **伪随机不用 `gsGen`**：原作的毛球随机跳；若从 `gsGen` 取数，所有关卡的补子序列都会移位（金标准大面积变化）。按盘面散列取舍既有「看起来随机」的效果，又保证只有毛球关卡的行为变化。
- **借用皮带动画**：步末效果记成 `EvBelt`（元素名 `fuzzball`），前端的皮带播放按「原格 → 新格」平滑滑动，正好是两格互换；`EvMove` 会被画成蜗牛，所以不用它。前端纯模块（`app/pure`）一行未改。
- **只跳到普通宝石**：不和特殊块、障碍、带冰 / 叠层的格换位，避免把关卡目标（冰、叠层）或特殊块挪走。

## 魔力鸟组合增强（新玩法 4，规则开关 `rainbow_combos`）

对照开心消消乐的「魔力鸟 + 特效」：魔力鸟（本项目叫彩虹）和直线交换，棋盘上所有同色宝石都变成直线再引爆；和炸弹交换，同色宝石全变成炸弹再引爆。

| 项 | 规则 |
|---|---|
| 成立 | 玩家交换的一端是彩虹、另一端是直线（横 / 竖）或炸弹，两端都能点火（软锁的不算，同原有彩虹规则）；彩虹 × 普通宝石、彩虹 × 彩虹、直线 × 炸弹等照旧 |
| 彩虹 × 直线 | 盘上与直线**同色的普通宝石**（无冰、无叠层、非特效）全部变成直线：行 + 列为偶数的格变横向、奇数的变竖向（原作方向随机，这里取确定的棋盘格交替，不耗随机数） |
| 彩虹 × 炸弹 | 同色的普通宝石全部变成炸弹 |
| 引爆 | 变身之后按原有彩虹取色的种子起手（彩虹 + 该色全部格，含交换来的直线 / 炸弹、带冰 / 叠层的同色宝石、倒计时 / 双面块），种子里的特效**在同一轮里一起引爆**（原作逐个依次引爆，这里简化为同一轮连锁展开；之后的下落 / 连锁照常） |
| 不变身的同色格 | 带冰 / 叠层的同色宝石、已有的同色特效、倒计时 / 双面块只作为种子被命中（削冰、清叠层、特效照常点火），不改变种类 |
| 开关 | 关卡记录 `lvlRules` 含 `"rainbow_combos"` 时打开（关卡级元素 `RainbowCombos`，玩家交换成立前回复新消息 `Morphing`）；原有 43 关、每日挑战都关着，彩虹 × 直线 / 炸弹仍是「只清同色」 |
| 回放 | 变身记成第 0 轮之前的一条步末效果（`EvSpread`，元素名 `rainbow_line` / `rainbow_bomb`，每项 = 彩虹所在格 → 目标格、变身后的格）；`mtStart` 仍是交换后的盘面，第一轮从变身后的盘面开始 |
| 放置 | 新玩法 4 起直线 / 炸弹 / 彩虹可以写进放置表：`Place "rainbow" [] 格`（颜色取原格的宝石） |
| 关卡 | 第 44 关「魔力鸟」：24 步，开局放好两组组合（彩虹 (3,2) + 横向直线 (3,3)、炸弹 (4,4) + 彩虹 (4,5)），12 块双层石头在四边，`goalCount CountStones 12` |
| 前端 | 变身段按「长出新格」播放：同色宝石从中心长成直线 / 炸弹（白光前沿），然后第一轮高亮、爆炸；HUD 关名右侧画彩虹角标 +「彩虹组合变身」 |

取舍说明：
- **按关打开**：全局打开会改变原有关卡里出现过的彩虹 × 特效结果（金标准 / 元素查询快照都会变），所以和 `bomb_shapes` 一样做成规则开关，只在第 44 关打开；原有 43 关的快照逐字不变（`rc_old_levels_unchanged` 逐关比对同一局面）。
- **同一轮引爆**：逐个依次引爆需要把一轮拆成多轮，会动连锁流水线；同一轮展开的清除范围相同（直线 / 炸弹的爆炸互相覆盖），只少了「一颗接一颗」的节奏。
- **方向用棋盘格**：随机方向需要消耗 `gsGen`，会让之后的补子序列跟着变；棋盘格交替保证横竖各半、结果确定。
- **组合表不动**：变身需要改盘面，组合表（`ComboRule`）只给种子；因此新增一条「交换变身」节拍，由关卡级元素回复，主流程只多问一次、没人回复时走原有起手。

## 雪怪 Boss（新玩法 5，`Custom "snow_boss"`）

对照开心消消乐的 Boss 关：一只占 2×2 格的雪怪，在它身边消除、或用特效 / 道具打它就掉血，血量归零即过关；它每隔几步在身边召唤雪块。

| 项 | 规则 |
|---|---|
| 形态 | 一只 Boss = 四个 `Custom "snow_boss" v` 格（象限 0 左上 / 1 右上 / 2 左下 / 3 右下），`v = ((满血 × 256 + 血量) × 4 + 召唤计数) × 4 + 象限`，四格的血量 / 满血 / 计数始终相同；固定格原型：挡交换、不下落、洗牌原地保留、无色、不进提示 |
| 掉血 | 邻格规则 200（在毛球 190 之后，每轮一次）：伤害 = 本轮**真消除格**里落在 Boss 身外一圈（与四格正交相邻的 8 格）的格数 + 本轮被**直接命中**（特效爆炸 / 锤子 / 十字 / 魔法石发射）的 Boss 格数；四格一起改写成新血量。斜角格不算 |
| 受击 | 直接命中 = `Absorb` 自身（原样吃掉、不碎），伤害统一在邻格规则里结算，所以同一格一轮只算一次 |
| 击败 | 血量归零时四格一起并入本轮清除格，整只消失，上方的宝石照常落下补满 |
| 目标 | 「击败 Boss」= `goalCount (CountNamed "snow_boss") 满血`：计数按步前 / 步后盘面差计（`countsDiff`），新的通用钩子 `weighs`（`ccDiffWeight`）让左上格按剩余血量计权、其余三格权重 0，于是差值 = 本步扣掉的血 |
| 召唤 | 步末规则 `PhaseMove` 30（在蜗牛 10、毛球 20 之后，只在交换的步末；道具不推进）：召唤计数 +1，满 3 次归零，并把身外一圈里的一颗**普通宝石**（无冰无叠层、不在避让格 / 墙上）变成雪块 = 1 层石头；选格按这一步步末开始时的盘面散列（与毛球同法，不耗 `gsGen`）；一圈里没有普通宝石时本次不召唤 |
| 回放 | 每次交换的步末记一条 `EvTick "snow_boss"`：四格的新计数（召唤时还有雪块格），`applyEndEffect` 可重放 |
| 放置 | `Place "snow_boss" [AInt 血量 (1–255), AInt 象限] 格`；关卡里用 `bossAt (行, 列) 血量` 一次铺四格 |
| 关卡 | 第 45 关「雪怪」：24 步，Boss 左上角在 (2,3)（占 (2,3)–(3,4)），40 血，目标击败 Boss |
| 视图 | `Match3.View.gvBoss :: Maybe BossView`（目标里有 `CountNamed "snow_boss"` 时为 `Just`，`bvHp` = 满血 − 已扣、`bvMax` = 目标数）；`bossPart :: Cell -> Maybe BossPart`（象限、是否过半受伤、召唤计数 / 周期） |
| 前端 | 贴图 `snow_boss_0..3`（整只冰蓝雪怪切成四块）/ `snow_boss_hurt_0..3`（血量 ≤ 一半时的受伤表情），右下块上三个召唤进度点；HUD 目标条换成红色血条 + `snow_boss` 头像 +「HP 剩余/满血」，过半后深红闪烁；几何版为冰蓝 2×2 方块 + 眼睛 + 同样的血条 |

取舍说明：
- **四个 Custom 格而不是关卡级元素**：backlog 原写「关卡级元素记位置和血量」；这里按「占 2×2 格、挡交换不下落」的要求，直接用四个固定格表达——挡交换 / 不下落 / 洗牌保留 / 前端画格都走现成的固定格路径，状态随盘面走（撤销 / 回放 / 快照天然正确），`gsLevelElems` 不变。代价是血量在四格里各存一份，规则里总是四格一起改写。
- **最小通用钩子**：只加了一个计数能力 `weighs n`（`CountCaps.ccDiffWeight`，缺省 1；`Registry.weighElementWith`、`Game.Tally.diffCountsWith` 按权重求和）。缺省权重 1 时与原来的「个数差」逐字相同，所以原有元素（双面块等 `countsDiff`）不受影响。掉血、击败、召唤都用已有的邻格规则 / 直接命中 / 步末规则表达，主流程没有点名雪怪。
- **确定性召唤**：召唤选格若从 `gsGen` 取随机数，会让补子序列移位、所有后续局面变化；按盘面散列保证只有第 45 关行为变化（`sb_other_levels_unchanged` 去掉雪怪条目逐关比对）。
- **雪块 = 1 层石头**：不引入新障碍类型，前端与计数都现成；打碎雪块照常计石头。
- **扣血只看真消除 + 直接命中**：与魔法石 / 毛球一致，被波及的叠层 / 冰层（没有真消除）不算，避免一次特效在一格上重复扣血。

## 饼干掉落口（新玩法 6，关卡级元素 `CookieDrop`）

对照开心消消乐的金豆荚掉落口：指定列的顶端是掉落口，收集物从那里陆续掉进场，落到底部被收走。收集与出口复用已有的饼干（`Cookie`：挡交换、随重力下落、`drainsAt [EdgeBottom]` 底边收走、计 `CountCookies`），新增的只有「掉落口」。

| 项 | 规则 |
|---|---|
| 关卡数据 | 关卡记录新字段 `lvlDrops :: [DropSpec]`（缺省 `[]`）；`DropSpec { dropCells :: [Pos], dropCell :: Cell, dropKeep :: Int }` = 掉落口格、掉下来的格子、盘上少于多少块时才掉 |
| 掉落 | 补子时（每轮沉降之后、以及步末补结算），掉落口格上的空洞若此刻盘上（本次已补的格子算在内）的 `dropCell` 少于 `dropKeep` 块，就补 `dropCell` 而不是宝石；多个空洞按行优先先补的先占名额；掉落口格没有空洞（那一列没有消除）时不掉 |
| 确定性 | 每个空洞仍照原补子策略调一次（随机数照常消耗），掉落口只替换结果，所以 `gsGen` 的推进与没有掉落口时完全相同；掉不掉只由盘面决定 |
| 接入 | 关卡级元素 `CookieDrop [DropSpec]`（`levelStart` 读 `lvlDrops`）回复已有的补子策略查询 `Refilling`，把收到的策略包一层 `dropRefill`；`lvlDrops` 为空时不回复（= 原策略）。去掉注册（`removeLevel "cookie_drop"`）即不掉 |
| 开局 | 有掉落口的关卡不做目标补齐（`ensureGoalDecor` 原本会按目标数一次铺满饼干），开局只有关卡放置的几块，其余由掉落口补 |
| 收集 | 原有规则不变：饼干到达底行即被收走、计 `CountCookies`；目标 `goalCount CountCookies n` |
| 关卡 | 第 46 关「掉落口」：26 步，掉落口在顶行 (0,1) / (0,3) / (0,4) / (0,6)，开局四个掉落口上各一块饼干，盘上少于 4 块时掉，目标收 8 块 |
| 视图 / 前端 | `Match3.View.BoardView.bvDrops`（掉落口格，读 `Element.Level.levelDrops`）；桌面版在掉落口格上沿画金色漏斗 `cookie_drop`（画在棋子之上，几何版 `drawDropMark` 是金色台阶 + 白色箭头） |

取舍说明：
- **按「盘上少于 N 块」而不是「每隔 N 步」**：补子策略只看得到盘面（`RefillCtx` = 空洞位置 + 当前盘面），按盘面数量决定既不用给 `GameState` 加步数计数，也天然限制了场上饼干的数量（不会越积越多堵死棋盘）；原作的节奏感由「收走一块才掉一块」体现。
- **随机数照常消耗**：若掉落口格跳过随机取色，补子序列会从第一次掉落起移位；照常消耗再替换，保证同一局面去掉掉落口后后续补子逐位相同，只有掉落口格本身不同（`cd_refill_same_rng_as_base`）。
- **复用而非新增收集物**：饼干已经有下落、底边收集、计数、目标、贴图与网页端画法；掉落口只是一种补子策略，主流程的补子节拍（第 8 刀的 `Refilling`）原样可用。唯一的主流程改动是开局跳过目标补齐（只在 `lvlDrops` 非空时）。
- **同种按名字数**（新玩法 7 起）：`dropKeep` 数的是「与 `dropCell` 同种」的格——`Custom` 按名字（不看状态值），内置格按相等（饼干 = `Cookie`，与原先逐格相等完全相同）。这样第 47 关掉下来的红色变色龙换色后仍算在名额里。

## 变色龙（新玩法 7，`Custom "chameleon"`）

对照开心消消乐的变色糖：它是一颗会换色的宝石，玩家要抓住它当前的颜色凑三消。工程上是一个 `Custom` 本体元素（`Element.Builtin.Collectible.Chameleon`），全部行为经已有的能力声明接入，主流程不改。

| 项 | 规则 |
|---|---|
| 编码 | `Custom "chameleon" (CustomState k)`，k = 当前颜色下标（`colorAt k`，0..4 = C1 红 / C2 绿 / C3 蓝 / C4 黄 / C5 紫） |
| 原型 | 普通棋子（`piece`）：可交换、按当前颜色参与匹配与提示（`colorIs (colorAt k)`）、命中即消、随重力下落、过传送门、可被蜗牛推动；另外洗牌原样保留（洗牌只重排普通宝石）、不可被魔法帽 / 染色瓶改色 |
| 换色 | 玩家交换的步末（`PhaseMove` 40，蜗牛 10 / 毛球 20 / 雪怪 30 之后）：每只（行优先，在逐只换过的盘面上）按 C1 → C2 → C3 → C4 → C5 → C1 从下一种颜色试起，取第一种不会让它立刻连成三消的颜色（五种里最后一种是原色）；纯按盘面，不消耗 `gsGen`。道具（锤子 / 任意交换 / 十字）的步末表没有 move 阶段，不换色 |
| 记录 | 步末效果 `EvTick "chameleon"`，每项 `(p, p, 换色后的格)`；前端按倒计时的 tick 段播放（前半旧色、后半新色 + 光晕） |
| 计数 | 被消除（匹配 / 特效 / 道具）计 `CountNamed "chameleon"`；目标 `goalCount (CountNamed "chameleon") n`；同时照常计入颜色袋 |
| 彩虹 | 成对交换规则 15（在彩虹取色 10 之后、特殊合成 20 之前，挂在变色龙上）：彩虹 × 变色龙 = 把变色龙那端当同色普通宝石问内置彩虹取色，再加上同色的全部变色龙 |
| 放置 | `Place "chameleon" [] 格`：颜色取原格宝石（开局不会凭空连成三消）；`[AColor c]` 指定颜色；原格不是宝石时不放 |
| 关卡 | 第 47 关「变色龙」：18 步，目标消 30 只；开局 2 只在 (3,1) / (5,6)；复用新玩法 6 的掉落口：顶行 (0,3)，盘上少于 2 只时补一只 C1（红）变色龙 |
| 视图 / 前端 | 格子即 `Custom "chameleon" k`（网页 Api 的 `cellFace` = `{t:"custom", name:"chameleon", v:k}`）；桌面版先画 `gem_<颜色>` 再画缓慢旋转的五色环 `chameleon`（几何版 `primChameleon`：普通宝石 + 五色描边）；HUD 目标图标 `chameleon_icon`（黄宝石 + 五色环） |

取舍说明：
- **换色跳过会立刻成消的颜色**：若换色后直接连成三消由步末补结算自动消掉，变色龙会「自己把自己消掉」（试验：10 只、不跳过时一步贪心 2–6 步就清完），玩法就不需要「抓时机」了；跳过这种颜色后，只有玩家的交换能消它。判断只看不带叠层的宝石与变色龙（其余格打断连线），五种都会连成（只在换色前就有现成三消时）才取下一种、交给补结算。
- **所有变色龙同一节奏**：原作每颗各自换色；这里全盘同一步末、同一顺序，玩家可以预判「下一步它会变成什么」（除非被跳过）。
- **目标 30 只配掉落口**：只放固定数量时变色龙密度高、连锁顺带就能消掉大半（一步贪心 10 只 / 20 步 30 局全过）；改成「场上最多 2 只、消一只补一只」后要一只只瞄准（见 testing.md 的模拟胜率）。

## 魔法地格（新玩法 8，地面层 `"magic"`）

对照开心消消乐的魔法地格：一种铺在格子下面、永远不会消失的地面，特效在它上面引爆时爆炸范围变大。工程上是一个地面层元素（`Element.Builtin.Ground.MagicGround`）加一个缺省什么都不做的通用钩子「扩爆」。

| 项 | 规则 |
|---|---|
| 编码 | 地面层 `(格, ("magic", 1))`（关卡记录 `lvlGround`，开局进 `GroundLayer`，读数 `gsGround`）；值恒为 1，只用于显示（`toCell` = `Custom "magic" 1`，不落到盘面上） |
| 存在 | 永久：没有 `groundRule`，上方格子被消除 / 收走都不去层、不计数；不占格、不挡交换 / 匹配，棋子照常落在上面；不随重力 / 洗牌 / 皮带移动 |
| 扩爆 | 直线 / 炸弹（任何带 `blast` 的本体，内置只有这两种）的**引爆格**是魔法地格时，爆炸范围向外扩一圈：原范围每格的八邻格（盘内）并进来（`magicWiden`）——横直线一行 → 三行、竖直线一列 → 三列、炸弹 3×3 → 5×5（贴边截断） |
| 命中 | 扩出来的格与原范围一样算直接命中：障碍削层、其中的特效照常连锁；同一轮里一格只受一次（碎石被直接命中又挨着消除，也只削 1 层，同原规则） |
| 只看引爆格 | 爆炸只是扫过魔法地格不扩；彩虹取色、特效 × 特效组合（组合表）、十字道具、魔法石发射、倒计时 3×3、彩蛋开出的爆炸这些**种子**不经 `blast`，不扩；种子里的直线 / 炸弹照常逐个引爆，落在魔法地格上的那枚照样扩（例：直线 × 直线交换到魔法地格的那一端是竖直线，它扩成三列） |
| 道具 | 锤子 / 任意交换 / 十字触发的特效同样按引爆格扩（与交换走同一条结算路径） |
| 接入 | 能力 `widens 改写`（`StepCaps.stWiden`，缺省 `Nothing`）；每步结算开始时 `Element.Level.levelRegistryIn` 把地面层里带扩爆规则的格写进注册表的本步扩爆格 `regWiden`（缺省 `[]`；没有这种格时注册表原样返回），`Registry.blastWith` 在引爆格是扩爆格时改写范围；`Engine.playWith` 展开事件也用这张本步注册表，`EvBlast` 的覆盖格含扩出来的一圈 |
| 随机 | 不消耗 `gsGen`；地面层在每步开始时取一次（魔法地格不变，与逐轮取相同） |
| 关卡 | 第 48 关「魔法格」：18 步，目标碎石 8 块；底行 8 块三层碎石（`Place "stone" [AInt 3]`）；魔法地格 4 格 (6,2) / (6,5) / (5,3) / (5,4)；打开 `bomb_shapes`（L / T 形出炸弹） |
| 视图 / 前端 | 视图模型照旧（`bvGround` / `groundAtView` 给出 `("magic", 1)`；网页 Api `state.ground` 里 `{p, name:"magic", layers:1}`）；桌面贴图 `magic`（紫色符文地砖，画在棋子下面），几何版 `primMagic`（紫色双线框 + 四角小方点，画在棋子上面）；扩大的爆炸没有新动画，按 `EvBlast` / 清除格原样高亮；HUD 目标仍是碎石 |

取舍说明（backlog 只写了「特效在魔法地格上引爆时范围扩一圈」，下面几条是补的细节）：
- **永久、不计数**：原作的魔法地格不会被消耗；这里没有地面反应规则，所以不需要给它计数或目标，关卡目标交给它帮忙打到的东西（第 48 关是碎石）。
- **「扩一圈」= 按八邻格膨胀**：对任意形状的范围都有定义（直线、炸弹、将来的新特效），直线变三行、炸弹变 5×5，与原作「横竖各多一格」的观感一致；新格按行优先接在原范围后面，事件里的顺序确定。
- **只看引爆格**：「扫过就扩」会让一次爆炸在多个魔法地格上连续放大、范围难以预判；只看引爆格时玩家知道「把特效做在紫格上」。
- **种子不扩，种子里的特效扩**：组合表 / 彩虹 / 十字给出的是一整片种子而不是某个特效的 `blast`，再扩会改变这些已有规则的几何；但种子里的直线 / 炸弹本来就逐个经 `blast` 引爆，保持「只看引爆格」的一致性，不给它们开例外。
- **每步取一次**：扩爆格在每步开始时写进本步注册表（和 `bomb_shapes` 的形状表同一处），而不是逐轮查地面层；魔法地格不会变化，所以结果相同，改动也只在 `levelRegistryIn` 一处。
- **底行碎石配 (5,3) / (5,4)**：第 5 行的直线在魔法地格上扩到 4–6 行，第 6 行被消掉时邻消削到底行，整排碎石一起掉一层；第 6 行的两格让竖直线 / 炸弹扩到三列 / 5×5 直接打到底行。魔法地格放在第 4 行时扩出的一圈到不了底行（贪心 10/30，与不放魔法地格的 9/30 相当），所以放在第 5 / 6 行。

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
| 关卡表 | `allLevels`（49：前 38 关 + 段 5 追加的第 39 关「果冻」、第 40 关「气泡」+ 新玩法 1 的第 41 关「爆破」+ 新玩法 2 的第 42 关「魔石」+ 新玩法 3 的第 43 关「毛球」+ 新玩法 4 的第 44 关「魔力鸟」+ 新玩法 5 的第 45 关「雪怪」+ 新玩法 6 的第 46 关「掉落口」+ 新玩法 7 的第 47 关「变色龙」+ 新玩法 8 的第 48 关「魔法格」+ 矩形盘面的第 49 关「宽域」（6×9）；`Match3.Levels.Campaign`） | 名称中文；见 README 表；第 49 关是终章（过关为 `Won`；新关追加后，原终章过关由 `Won` 变为 `LevelClear` 进入下一关）；按下标取关用 `lookupLevel`（`Maybe`） |
| 关卡记录 | `Level`（`lvlIndex` / `lvlName` / `lvlMoves` / `lvlGoal` / `lvlPlacements` / `lvlBelts` / `lvlPortals` / `lvlUfos` / `lvlCarpets` / `lvlGround` / `lvlRules`） | 第 6 刀：一关的全部数据（步数、目标、装饰放置表、皮带、传送门、飞碟、地毯、地面层）在同一条记录里；`lvlRules`（新玩法 1）= 本关打开的规则开关名（`bomb_shapes` / `rainbow_combos`），缺省空 |
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
