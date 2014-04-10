module Lib.Evaluator where

import Lib.Tokens
import Lib.AST

eval :: AST -> Double
eval (Factor n)         = n
eval (Expr Plus  e0 e1) = eval e0 + eval e1
eval (Expr Minus e0 e1) = eval e0 - eval e1
eval (Term Div   e0 e1) = eval e0 / eval e1
eval (Term Times e0 e1) = eval e0 * eval e1
eval Empty              = error "eval: AST contained Empty; bug is parse?"

-- eof
