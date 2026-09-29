-- | 网页端与纯核心之间的「薄接口层」（纯函数，不依赖 JSFFI，原生 GHC 也能编译测试）。
--
-- 设计原则：
--   * 一切规则判定（能否交换、消除、下落、补子、连锁、计分、胜负）都调用 Match3.Core，
--     这里只做「GameState / MoveTrace → JSON 文本」的序列化，不含任何规则。
--   * 输出是手写的最小 JSON（不引入 aeson，减小 wasm 体积与依赖面）。
--   * 同一步里先用 traceSwap 取逐轮快照（供前端播放动画），再用 trySwap 真正结算；
--     两者走相同的随机数与步骤（核心测试锁定），所以 trace 的 mtFinal 与结算后盘面一致。
module Match3Web.Api
  ( apiNew
  , apiSwap
  , apiState
  , apiLevels
  , encodeState
  , encodeOutcome
  , encodeTrace
  , jsonString
  ) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)

import Match3.Core

-- ---------------------------------------------------------------------------
-- 对外接口（被 WebMain 的 JSFFI 导出包装）

-- | 新开一局：关卡序号（0 起）+ 随机种子。越界关卡号夹到合法范围。
apiNew :: Int -> Int -> (GameState, String)
apiNew li seed =
  let i = max 0 (min (length allLevels - 1) li)
      lvl = allLevels !! i
      gs = newGameAtLevel i (levelConfig lvl) seed
  in (gs, obj [("ok", "true"), ("state", encodeState gs)])

-- | 交换 (r1,c1) 与 (r2,c2)。返回新状态和 JSON：
--   { ok, outcome, trace:{start,waves,end,final}, state }
-- 被拒（NoMatch / InvalidSwap / 已结束）时 trace.waves 为空，状态不变（NoMatch 也不扣步）。
apiSwap :: Pos -> Pos -> GameState -> (GameState, String)
apiSwap p1 p2 gs =
  let mt = traceSwap p1 p2 gs          -- 逐轮快照（纯数据，只用于播放）
      (gs', out) = trySwap p1 p2 gs    -- 真正结算（规则全部在核心里）
  in ( gs'
     , obj
         [ ("ok", "true")
         , ("outcome", encodeOutcome out)
         , ("trace", encodeTrace mt)
         , ("state", encodeState gs')
         ]
     )

-- | 仅序列化当前状态。
apiState :: GameState -> String
apiState gs = obj [("ok", "true"), ("state", encodeState gs)]

-- | 关卡列表（序号、中文名、步数、目标描述），给选关界面用。
apiLevels :: String
apiLevels =
  arr
    [ obj
        [ ("index", int (lvlIndex l))
        , ("name", str (lvlName l))
        , ("moves", int (lvlMoves l))
        , ("goal", encodeGoal (lvlGoal l))
        ]
    | l <- allLevels
    ]

-- ---------------------------------------------------------------------------
-- 状态 / 结果 / 回放脚本

encodeState :: GameState -> String
encodeState gs =
  obj
    [ ("level", int (gsLevel gs))
    , ("name", str (levelName (gsLevel gs)))
    , ("score", int (gsScore gs))
    , ("moves", int (gsMoves gs))
    , ("goal", encodeGoal (gsGoal gs))
    , ("progress", int (progress gs))
    , ("target", int (goalTarget (gsGoal gs)))
    , ("over", maybe "null" encodeOutcome (gsOver gs))
    , ("loseHint", str (loseHint (gsGoal gs)))
    , ("combo", int (gsCombo gs))
    , ("shuffled", bool (gsShuffled gs))
    , ("hint", maybe "null" encodePair (findHint (gsBoard gs)))
    , ("lastCleared", arr (map encodePos (gsLastCleared gs)))
    , ("board", encodeBoard (gsBoard gs))
    ]
  where
    levelName i = case [lvlName l | l <- allLevels, lvlIndex l == i] of
      (n : _) -> n
      [] -> "?"

-- | 目标进度：参数顺序与核心 goalSatisfied 调用 goalMetEx 的顺序一致。
progress :: GameState -> Int
progress gs =
  goalProgressEx
    (gsGoal gs) (gsScore gs) (gsCollected gs) (gsColorBag gs)
    (gsStonesCleared gs) (gsUfoCollected gs) (gsChestsCleared gs)
    (gsHoneyCleared gs) (gsBalloonsPopped gs) (gsCookiesCollected gs)
    (gsCakesCleared gs) (gsSafesOpened gs)

encodeOutcome :: Outcome -> String
encodeOutcome o = case o of
  InvalidSwap -> tag "InvalidSwap" []
  NoMatch -> tag "NoMatch" []
  MoveApplied s -> tag "MoveApplied" [("score", int s)]
  Won s -> tag "Won" [("score", int s)]
  Lost s -> tag "Lost" [("score", int s)]
  LevelClear s n -> tag "LevelClear" [("score", int s), ("next", int n)]
  where
    tag t kvs = obj (("tag", str t) : kvs)

encodeGoal :: LevelGoal -> String
encodeGoal g = obj [("kind", str (goalKind g)), ("text", str (show g)), ("target", int (goalTarget g))]
  where
    goalKind x = takeWhile (/= ' ') (show x)

-- | 一步的逐轮回放：start → waves[0..] → end（步末效果）→ final。
encodeTrace :: MoveTrace -> String
encodeTrace mt =
  obj
    [ ("start", encodeBoard (mtStart mt))
    , ("waves", arr (map encodeWave (mtWaves mt)))
    , ("end", arr (map encodeEnd (mtEnd mt)))
    , ("final", encodeBoard (mtFinal mt))
    ]

encodeWave :: CascadeWave -> String
encodeWave w =
  obj
    [ ("before", encodeBoard (cwBefore w))
    , ("cleared", arr (map encodePos (cwCleared w)))
    , ("holes", arr [arr (map (maybe "null" encodeCell) row) | row <- cwHoles w])
    , ("after", encodeBoard (cwAfter w))
    , ("score", int (cwScore w))
    ]

encodeEnd :: EndStep -> String
encodeEnd e =
  obj
    [ ("afterWaves", int (esAfterWaves e))
    , ("before", encodeBoard (esBefore e))
    , ("after", encodeBoard (esAfter e))
    , ("effect", str (show (esEffect e)))  -- 细节先用 show，完整版再结构化
    ]

-- ---------------------------------------------------------------------------
-- 盘面编码：Board = [[Cell]]（行优先，8×8）
--   宝石：{"t":"G","c":1..5,"k":"N|H|V|B|R","i":冰层,"o":覆盖物或 null}
--   其他格：{"t":"X","s":"<核心 show 文本>","c":颜色或 0}
-- 渲染层只需按 t/c/k 画色块；非宝石格先用文字占位，完整版再逐种出图。

encodeBoard :: Board -> String
encodeBoard b = arr [arr (map encodeCell row) | row <- b]

encodeCell :: Cell -> String
encodeCell cell = case cell of
  Gem c k ice ov ->
    obj
      [ ("t", str "G")
      , ("c", int (colorNum c))
      , ("k", str (kindCode k))
      , ("i", int ice)
      , ("o", maybe "null" (str . show) ov)
      ]
  Countdown c n -> other (colorNum c) ("Countdown " ++ show n)
  Flip f _ -> other (colorNum f) (show cell)
  Balloon c -> other (colorNum c) (show cell)
  Bottle c -> other (colorNum c) (show cell)
  Maker c _ -> other (colorNum c) (show cell)
  _ -> other 0 (show cell)
  where
    other c s = obj [("t", str "X"), ("c", int c), ("s", str s)]

colorNum :: Color -> Int
colorNum c = fromEnum c + 1

kindCode :: GemKind -> String
kindCode k = case k of
  Normal -> "N"
  LineH -> "H"
  LineV -> "V"
  Bomb -> "B"
  Rainbow -> "R"

encodePos :: Pos -> String
encodePos (r, c) = arr [int r, int c]

encodePair :: (Pos, Pos) -> String
encodePair (a, b) = arr [encodePos a, encodePos b]

-- ---------------------------------------------------------------------------
-- 极简 JSON 构造器

obj :: [(String, String)] -> String
obj kvs = "{" ++ intercalate "," [str k ++ ":" ++ v | (k, v) <- kvs] ++ "}"

arr :: [String] -> String
arr xs = "[" ++ intercalate "," xs ++ "]"

int :: Int -> String
int = show

bool :: Bool -> String
bool True = "true"
bool False = "false"

-- | 对外暴露的 JSON 字符串编码（WebMain 的错误信息用）。
jsonString :: String -> String
jsonString = str

-- | JSON 字符串转义；非 ASCII（中文关卡名等）原样输出，由 toJSString 负责 UTF-8。
str :: String -> String
str s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc ch
      | ord ch < 0x20 = "\\u" ++ pad4 (showHex (ord ch) "")
      | otherwise = [ch]
    pad4 h = replicate (4 - length h) '0' ++ h
