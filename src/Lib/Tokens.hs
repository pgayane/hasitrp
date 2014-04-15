module Lib.Tokens where

data Add = Plus  | Minus deriving (Show, Eq)
data Mul = Times | Div   deriving (Show, Eq)

data Token = TokAdd Add
           | TokMul Mul
           | TokNum Double
           | TokLB
           | TokRB
           deriving (Show, Eq)

-- eof
