-- 此文件实现 王垠 博客中的 计算器解释器
-- http://www.yinwang.org/blog-cn/2012/08/01/interpreter
module HkLang
(   Exp (..)
  , hk
  , runTests
) where

import Data.Maybe (fromMaybe)

data Exp =
  Value Float
  | Boolean Bool
  | Str String
  | Param String
  | Lambda Exp Exp
  | Op String Exp Exp
  | Closure Exp Env -- 闭包
  | If Exp Exp Exp -- If 逻辑判断
  | Let Exp Exp Exp
  | Call Exp Exp -- 函数调用
  | Error String
    deriving (Show)

-- 环境变量
type Env = [(String, Exp)]

-- 扩展已有 env
extEnv :: String -> Exp -> Env -> Env
-- 后插入的可以屏蔽先插入的，就近原则
extEnv s v env = (s, v) : env

calc :: String -> Exp -> Exp -> Exp
calc "+" (Value v1') (Value v2')      = Value (v1' + v2')
calc "-" (Value v1') (Value v2')      = Value (v1' - v2')
calc "*" (Value v1') (Value v2')      = Value (v1' * v2')
calc "/" (Value v1') (Value v2')      = Value (v1' / v2')
calc "<=" (Value v1') (Value v2')     = Boolean (v1' <= v2')
calc ">=" (Value v1') (Value v2')     = Boolean (v1' >= v2')
calc "<" (Value v1') (Value v2')      = Boolean (v1' < v2')
calc ">" (Value v1') (Value v2')      = Boolean (v1' > v2')
calc "==" (Value v1') (Value v2')     = Boolean (v1' == v2')
calc "/=" (Value v1') (Value v2')     = Boolean (v1' /= v2')
calc "&&" (Boolean v1') (Boolean v2') = Boolean (v1' && v2')
calc "||" (Boolean v1') (Boolean v2') = Boolean (v1' || v2')
calc "<" (Boolean v1') (Boolean v2')  = Boolean (v1' < v2')
calc ">" (Boolean v1') (Boolean v2')  = Boolean (v1' > v2')
calc "<=" (Boolean v1') (Boolean v2') = Boolean (v1' <= v2')
calc ">=" (Boolean v1') (Boolean v2') = Boolean (v1' >= v2')
calc "==" (Boolean v1') (Boolean v2') = Boolean (v1' == v2')
calc "/=" (Boolean v1') (Boolean v2') = Boolean (v1' == v2')
calc "==" (Str v1') (Str v2')         = Boolean (v1' == v2')
calc "/=" (Str v1') (Str v2')         = Boolean (v1' /= v2')
calc "++" (Str v1') (Str v2')         = Str (v1' ++ v2')
calc "<" (Str v1') (Str v2')          = Boolean (v1' < v2')
calc ">" (Str v1') (Str v2')          = Boolean (v1' > v2')
calc "<=" (Str v1') (Str v2')         = Boolean (v1' <= v2')
calc ">=" (Str v1') (Str v2')         = Boolean (v1' >= v2')
calc _ _ _                            = Error "syntax error"

interp :: Exp -> Env -> Exp

interp (Param x) env      = fromMaybe (Error ("Cannot find variable" ++ x)) (lookup x env)
interp (Str x) _          = Str x
interp (Value x) _        = Value x
interp (Boolean x) _      = Boolean x
interp s@(Lambda _ _) env = Closure s env
interp (Let (Param x) e1 e2) env = interp e2 (extEnv x (interp e1 env) env)
interp (Op op e1 e2) env  =
  let v1 = interp e1 env
      v2 = interp e2 env
  in calc op v1 v2

interp (If cond e1 e2) env =
  let c = interp cond env
  in case c of
    Error _       -> Error "syntax error"
    Boolean False -> interp e2 env
    _             -> interp e1 env

interp (Call e1 e2) env   =
  case v2 of
    Value _   -> callExp
    Boolean _ -> callExp
    Str _     -> callExp
    _         -> Error "syntax error"
  where
    v2      = interp e2 env
    col     = interp e1 env
    callExp = case col of
                -- e 是函数，x 是参数，将值 v2 与 参数 x 绑定，扩展到 env 中
                (Closure (Lambda (Param x) e) env') -> interp e (extEnv x v2 env')
                _                                   -> Error "syntax error"

hk :: Exp -> Exp
hk = flip interp []

tests :: [Exp]
tests = [hk (Let (Param "x") (Value 2)
              (Let (Param "f") (Lambda (Param "y") (Op "*" (Param "x") (Param "y")))
                (Let (Param "x") (Value 4)
                  (Call (Param "f") (Value 3))))),
         hk (Let (Param "x") (Value 3.9) (Op "/" (Param "x") (Value 4.32))),
         hk (Let (Param "x") (Value 8.75) (Op ">=" (Param "x") (Value 7))),
         hk (Op "==" (Boolean True) (Boolean False)),
         hk (Op "++" (Str "test") (Str " case")),
         hk (If (Op ">=" (Value 2.3) (Value (-2.754))) (Str "Yes") (Str "No"))
        ]

run :: [Exp] -> IO ()

run [] = return ()

run (x:xs) = do
  print x
  run xs

runTests :: IO ()
runTests = run tests
