-- 此文件实现 王垠 博客中的 计算器解释器
-- http://www.yinwang.org/blog-cn/2012/08/01/interpreter

-- 三种数据类型 Double Bool String
-- 运算式 Op
-- 变量 Param
-- 函数 Lambda
data Exp =
  Value Double
  | Boolean Bool
  | Str String
  | Param String
  | Lambda Exp Exp
  | Op String Exp Exp
    deriving (Show)

-- 环境变量
type Env = [(String, Exp)]

-- 扩展已有 env
extEnv :: String -> Exp -> Env -> Env
-- 后插入的可以屏蔽先插入的，就近原则
extEnv s v env = (s, v) : env

calc :: String -> Exp -> Exp -> Exp

calc "+" (Value l) (Value r) = Value (l + r)
calc "-" (Value l) (Value r) = Value (l - r)
calc "*" (Value l) (Value r) = Value (l * r)
calc "/" (Value l) (Value r) = Value (l / r)

