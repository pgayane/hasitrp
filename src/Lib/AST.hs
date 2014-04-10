module Lib.AST where

import Lib.Tokens

-- Expr --> Expr {+,-} Term
--        | Term
--
-- Term --> Term {*,/} Factor
--        | Factor
--
-- Factor --> Number
--          | ( Expr )

data AST = Expr Add AST AST
         | Term Mul AST AST
         | Factor Double
         | Empty
         deriving (Show, Eq)

-- eof
