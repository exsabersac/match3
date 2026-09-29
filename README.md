# Match-3 消消乐（Haskell + SDL2）

8×8、五色可玩三消，对标开心消消乐常见机制：特殊块、多层障碍、草/藤蔓/巧克力/迷雾/锁链/火箭冰冻/窗帘、蒸汽、蜗牛、宝箱、保险箱、蜂蜜罐、蛋糕、魔法帽、果汁机、气球、饼干掉落收集、双面块、彩蛋惊喜盒、染色瓶、时间精灵、地毯、传送带、双向传送门、倒计时炸弹、飞碟、道具点选、多样目标、每日挑战、步数携带。纯规则在 library（`Match3.Core`），SDL 前端为 `match3-sdl`。

![关卡 16「大师」截图](docs/images/screenshot-l16.png)

**贴图**：宝石用「颜色 + 形状」双编码（红圆 / 绿方 / 蓝菱 / 黄星 / 紫三角），每种障碍都有独立图标，多层障碍会显示层数角标。美术说明、完整图例和重新生成方法见 [`docs/ui-art.md`](docs/ui-art.md)。贴图是 SDL2 核心可以直接读取的 32 位 BMP，macOS **不用装新的 brew 包**（不需要 sdl2_image）；缺少 `assets/` 时会自动退回几何图形渲染。

## 30 秒上手

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
# Linux 一次安装 SDL2 头文件；运行期依赖 libsdl2-2.0-0
sudo apt-get install -y libsdl2-dev

# macOS Apple Silicon（Homebrew SDL2）额外需要：
# export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig

stack test                            # 库测，无需显示器；期望 228 通过
stack build && stack exec match3-sdl
```

1. **左键**点两格相邻交换，或**拖拽**到相邻格；无三连会回滚
2. 开局底部有键位条；**P** 暂停看完整键位（H 提示 / **1** 锤子 / **2** 任意交换 / **3** 十字清除 / **M** 选关地图 / U 撤销 / S 洗牌 / D 每日 / R 重开 / N 过关）
3. 第一关会短暂黄框提示可消一手；达目标后按 **N** / 空格 / 点击继续

无显示器冒烟：`xvfb-run -a stack exec match3-sdl`。`stack test` 不需要显示。

更细的设计说明见 [`docs/`](docs/README.md)；键位表见 [`docs/ui-controls.md`](docs/ui-controls.md)。itch.io 文案见 [`ITCH.md`](ITCH.md)。

## 功能一览

- 相邻交换（点击或拖拽）；横/竖 ≥3 消除
- **特殊块**：四消 → 直线（Line）；五消 → 彩虹（Rainbow，与搭档交换只清搭档色；同色扩展为空操作）；炸弹（Bomb）供合成
- **特殊合成**：Line×Bomb（3×3 十字）、Rainbow×Line / Rainbow×Bomb（搭档色 + 直线/炸弹扩展）、Bomb×Bomb（5×5）、Line×Line（整行+整列）
- **石头箱**（`Stone n`）：多层障碍；邻消削层；末层移除
- **宝箱**（`Chest n`）：邻消 / 直线·炸弹·锤子削一层；目标 `GoalChest`
- **蜂蜜罐**（`Honey n`）：同上削层；目标 `GoalHoney`
- **气球**（`Balloon c`）：邻格**同色**消除才爆；目标 `GoalBalloon`
- **饼干**（`Cookie`）：随重力下落，仅**底行**收集（中盘直线/炸弹/锤子无效）；目标 `GoalCookie`
- **蛋糕**（`Cake n`）：分层障碍（≠ Cookie）；邻消/直线·炸弹·锤子削层；目标 `GoalCake`
- **魔法帽**（`MagicHat`）：邻消触发，交换/重染邻格宝石色
- **锁链**（`Chain n`）：邻消揭层；锁住不可交换也不可匹配
- **火箭冰冻**（`Freeze n`）：只挡交换（仍可匹配）；邻消揭层；≠ 冰层 Ice（Ice 在宝石本身匹配时削层）
- **蜗牛**（`Snail dr dc`）：挡交换；成功步末爬一格（推宝石；碰边/障碍/传送门端点掉头）
- **窗帘**（`Curtain n`）：邻消揭层；帘下宝石不参与匹配
- **保险箱**（`Safe n`）：邻消削层；开出 Cookie；目标 `GoalSafe`
- **双面块**（`Flip front back`）：正面参与匹配；命中翻成背面 Normal 宝石
- **彩蛋**（`Surprise`）：邻消或直接种子（锤/十字/直线）打开 → 直线/炸弹或 3×3 小爆
- **染色瓶**（`Bottle c`）：邻消把正交邻格宝石染成瓶色
- **时间精灵**（`TimeSpirit`）：邻消清除，本关 **+2 步**
- **蒸汽**（`Steam`）：挡匹配；邻消扑灭；存活蒸汽步末蔓延
- **地毯**（地毯目标地砖）：该格宝石消除则铺地毯；目标 `GoalCarpet`
- **步数银行**：战役过关最多携带 **3** 步入下一关
- **果汁机**（`Maker c n`）：同色邻消充能；归零产出该色 Bomb
- **传送门**（成对 Portal）：重力后 A 有子且 B 为空则 A→B（双向）
- **冰层**：宝石上 ice；匹配削层；末层同波清除；UI 裂纹（≠ 火箭冰冻 overlay）
- **草 / 藤蔓 / 巧克力 / 迷雾**（`CellOverlay`）：草匹配清除；藤步末蔓延；巧克力邻消清除且存活蔓延；迷雾 `Fog n` 邻消揭层（雾下不匹配）
- **道具**：`1` 锤子；`2` 任意两格交换；`3` 十字清除（行+列）；有限次数；先选格再按 1/3 仍可用；锤/十字对锁链/窗帘揭一层、对石头削一层（≠ 一击清空）
- **传送带**（`Belt`）：循环移位；步末移位后可再触发连锁
- **倒计时炸弹**（`Countdown`）：步末 −1；归零 3×3；匹配/特殊可解除
- **目标**：分数 / 单色收集 / 多色收集 / 碎石 / 开宝箱 / 砸蜂蜜 / 爆气球 / 收饼干 / 清蛋糕 / 开保险箱 / 飞碟吸收 / 铺地毯
- **飞碟**（`Ufo`）：每波连锁末 `stepUfo` 吸正交同色再移格；目标 `GoalUfo`
- **每日挑战**（`D`）：日期种子盘面 + **10** 种轮换目标；障碍类目标会自动补装饰；通关为 **Won**（不进战役 `LevelClear`/解锁）；三星按**关卡印制步数**剩余比例（携带不抬高分母）
- 连击波次计分、提示、撤销、自动洗牌、**38** 关战役地图（CH1–CH7 章节分隔）
- HUD、道具次数、粒子、交换/下落补间、连锁逐轮回放与连击分级（见下节）、藤蔓蔓延提示、飞碟叠层、暂停帮助、过关/胜利/失败叠层

## 画面与反馈

![5 连锁逐轮回放：每一轮的高亮、消失、下落补子、落定](docs/images/combo-strip.png)

- **连锁一轮一轮地播**：每一轮先高亮要消的宝石（其余棋盘压暗），再让它们消失，上方宝石落下、新宝石从顶上补进来，落定后才进入下一轮，能看清连锁是怎么一环扣一环的。
- **连击分级**：从第 2 轮起弹出「连击 x2」「x3」「x4」「x5」……颜色依次是浅金、橙、红、紫（x5 及以上彩虹流转），字越来越大，棋盘的轻微震动也越来越强。
- **得分浮字**：每一轮都会从消除的位置飘出本轮得分「+N」。
- **连击总结**：连锁结束后，如果达到 2 连击以上，右下角面板显示「N 连击！」。
- **回合末变化也有动画**：倒计时炸弹数字减一、传送带滑动、藤蔓 / 巧克力 / 蒸汽从相邻格「长」过来、蜗牛爬一格并推开宝石，按实际发生的顺序依次播放，同一时刻的这些变化合计不超过约 0.6 秒；棋盘无路可走时自动洗牌（单独约 0.4 秒）。
- **想快点看完**：播放中点一下鼠标，或按空格 / 回车 / `N`，约 3 倍速播完，每一轮依然看得见。
- **播放期间锁定操作**：动画没播完时不能交换、用道具、撤销或洗牌；暂停、提示、选关地图、重开照常可用。过关 / 失败面板等动画播完才出现。
- **无效交换不重播**：换了凑不成三连的两格只会弹回原位，不会把上一次的连击特效再放一遍。

步末动画的逐段截图和时长见 [`docs/ui-art.md`](docs/ui-art.md#连击表现逐轮回放)。

## 关卡表

| # | 名称 | 步数 | 目标 | 装饰 |
|---|------|------|------|------|
| 1 | 入门 | 30 | 分数 300 | — |
| 2 | 采红 | 30 | 收集 20× 红 | — |
| 3 | 热身 | 26 | 分数 500 | — |
| 4 | 采蓝 | 26 | 收集 22× 蓝 | — |
| 5 | 进阶 | 24 | 分数 700 | 巧克力 |
| 6 | 冰绿 | 24 | 收集 26× 绿 | 冰层 |
| 7 | 双采 | 28 | 红 12 + 蓝 12 | — |
| 8 | 碎石 | 26 | 毁 8 石头 | 石头 + 传送带 |
| 9 | 草场 | 24 | 分数 600 | 草 |
| 10 | 藤袭 | 22 | 收集 18× 红 | 藤蔓 |
| 11 | 传送 | 22 | 分数 800 | 草 + 传送带 |
| 12 | 轰炸 | 20 | 分数 750 | 倒计时炸弹 |
| 13 | 飞碟 | 24 | 飞碟吸收 10 | UFO C1 |
| 14 | 碟猎 | 20 | 飞碟吸收 14 | 双 UFO + 传送带 |
| 15 | 压力 | 20 | 多色红/绿/蓝 | 草 + 巧克力 |
| 16 | 大师 | 22 | 分数 1000 | 石+草+藤+巧+炸+带+UFO |
| 17 | 宝箱 | 24 | 开 6 宝箱 | 宝箱 |
| 18 | 巧箱 | 22 | 开 5 宝箱 | 宝箱 + 巧克力 |
| 19 | 蜂蜜 | 24 | 砸 6 蜂蜜罐 | 蜂蜜罐 |
| 20 | 蜜压 | 22 | 砸 5 蜂蜜罐 | 蜂蜜 + 巧克力 |
| 21 | 气球 | 24 | 爆 6 气球 | 彩色气球 |
| 22 | 饼干 | 24 | 收 6 饼干 | 高位饼干 |
| 23 | 巧饼 | 22 | 收 5 饼干 | 饼干 + 巧克力 + 迷雾 |
| 24 | 蛋糕 | 24 | 清 6 蛋糕 | 分层蛋糕 |
| 25 | 帽宴 | 22 | 清 5 蛋糕 | 蛋糕 + 魔法帽 + 巧克力 |
| 26 | 锁链 | 22 | 分数 900 | 锁链 + 石头 + 巧克力 |
| 27 | 果汁 | 24 | 收集 18× 红 | 果汁机 + 迷雾 + 传送门 |
| 28 | 终章 | 24 | 分数 1400 | 终章混合装饰 |
| 29 | 蜗牛 | 20 | 分数 850 | 爬行蜗牛 |
| 30 | 冰冻 | 20 | 收集 16× 绿 | 火箭冰冻 + 巧克力 |
| 31 | 窗帘 | 22 | 收集 16× 红 | 窗帘列 + 巧克力 |
| 32 | 金库 | 22 | 开 5 保险箱 | 保险箱→饼干 + 双面 + 巧克力 |
| 33 | 惊喜 | 22 | 分数 900 | 彩蛋 + 巧克力 |
| 34 | 染色 | 22 | 收集 16× 蓝 | 染色瓶 + 迷雾 |
| 35 | 时灵 | 22 | 分数 850 | 时间精灵（+2 步） |
| 36 | 蒸汽 | 22 | 收集 16× 绿 | 蒸汽 + 巧克力 |
| 37 | 地毯 | 24 | 铺 8 地毯 | 地毯 + 巧克力 |
| 38 | 织毯 | 24 | 铺 12 地毯 | 地毯 + 巧克力 + 迷雾 |

按 `D` 进入**每日**挑战（日历日期作种子）。

## 操作

| 输入 | 作用 |
|------|------|
| 左键 / 拖拽 | 相邻交换（动画播放中点击＝加速） |
| `1` 再点格 | 锤子清一格；再按 `1` 取消 |
| `2` 再点两格 | 任意两格交换；再按 `2` 取消 |
| `3` 再点格 | 十字清除（行+列）；再按 `3` 取消 |
| 先选格再 `1`/`3` | 对该格锤/十字（旧快捷） |
| `H` | 提示 |
| `U` | 撤销 |
| `S` | 洗牌 |
| `D` | 每日挑战 |
| `M` | 选关地图；点已解锁节点 |
| `N` / 空格 / 回车 | 叠层后下一关 / 重试；动画播放中＝约 3 倍加速 |
| `R` | 重开关（暂停中也可用） |
| `P` | 暂停 + 键位帮助（冻结动画；清掉进行中的拖拽） |
| `Esc` / `Q` | 退出 |

外观速查：红圆 / 绿方 / 蓝菱 / 黄星 / 紫三角＝五色宝石；白金箭头光带＝直线（方向即消除方向）；橙色光晕 + 黑炸弹标记＝炸弹；七彩旋涡＝彩虹；右下角数字＝剩余层数（果汁机为剩余次数）。每种障碍的图标见 [`docs/images/legend.png`](docs/images/legend.png)。

## 对标开心消消乐（机制对照）

| 开心消消乐 | 本项目 |
|-----------|--------|
| 三消 / 四消横竖线 / 五消彩色精灵 | ✅ Normal / LineH·V / Rainbow |
| 炸弹与特殊合成 | ✅ Bomb；Line×Bomb / Rainbow×Line / Bomb×Bomb / Line×Line |
| 箱子 / 多层障碍 | ✅ `Stone n` |
| 宝箱 / 饼干 / 蛋糕 / 魔法帽 / 锁链 / 火箭冰冻 / 窗帘 / 保险箱 / 双面 / 彩蛋 / 染色瓶 / 时间精灵 / 蒸汽 / 地毯 / 蜗牛 / 果汁机 / 传送门 / 迷雾 / 冰层 / 草藤巧 / 倒计时 / 飞碟 / 每日三星 / 选关 / 传送带 / 道具 | ✅ 见上文与 [`docs/domain.md`](docs/domain.md) |

## 构建与测试

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
# macOS Apple Silicon：
# export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig

stack build && stack test && stack exec match3-sdl
```

Stackage：**lts-21.25** / GHC **9.4.8**（`stack.yaml` 已 `system-ghc: true`）。

## 目录结构

```
src/Engine/   Game Effect Playback（多游戏通用层：接口 / 通用效果 / 纯播放层；不依赖 Match3）
src/Match3/   Types Core Engine Obstacles Rainbow Combos Ice
              Daily Countdown Conveyor Boosters Grass Ufo Snail Carpet
              （Engine = 三消作为通用接口的第一个实现）
src/Match3/Board/  Grid Match Clear Gravity Cascade Random
src/Match3/Game/   State Tally Outcome Shuffle Level Trace Resolve Move Boosters
src/Match3/Element/ Types Registry Builtin Event（元素框架：定义 / 注册表 / 内置元素 / 效果事件；Element.hs 为再导出外观）
app/Main.hs   SDL2 前端入口（读环境变量 → runShell）
app/Shell/    Loop（通用 SDL 外壳：窗口 / 固定步长主循环 / 插件钩子；不依赖 Match3）
app/UI/       三消插件：Plugin Types Layout Env Input Actions Playback Draw Cascade EndStage
              BoardArt BoardPrim CellTable HudArt HudPrim TextArt Glyph LevelMap
app/UI/Cell/  Prim Art（每种元素一个几何 / 贴图渲染函数，经 CellTable 查表）
app/Art.hs    贴图图集加载 / 九宫格面板 / 降级
app/ComboFx.hs 连锁逐轮回放 / 步末动画的纯阶段机与时间线常量
assets/       生成的贴图（atlas.bmp / atlas1.bmp 图集分页 + atlas.txt + background.bmp；2x 高分屏规格）
tools/        gen_assets.py（Pillow 程序化生成贴图与图例）；golden/ 旧提交比对用的 Golden.hs 存档（不参与编译）
test/Spec.hs  测试入口（只汇总；228 命名用例）
test/Spec/    按功能拆分的测试模块（GridMatch / Gravity / Cascade / Specials / Obstacles.* / Boosters / GoalsLevels / Element / Engine / UIEvents / ReplayUndo / Golden / Properties）与共用辅助 Support
test/Toy.hs   通用接口的玩具实现（一维计数器，只 import Engine.*）
test/golden/ 行为金标准（Golden.hs 投影 + golden.txt）
docs/         中文设计文档（架构 / 领域 / 规则流水线 / 测试 / 键位 / 美术）
```

冻结规则 API 形态：`trySwap` / `runMove` / `ensurePlayable` / `shuffleGame` / `Outcome` / `GoalCollect`。

## 发布状态

- 战役：**38** 关（地图 CH1–CH7），批量可构造 / 可玩 / 装饰与目标对齐
- 测试：`stack test` **228**（Tasty + QuickCheck）
- 许可证：BSD-3-Clause（见 `LICENSE`，英文法律文本保持原文）
