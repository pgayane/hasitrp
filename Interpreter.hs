--module Interpreter (Expr(..), interp) where
import Data.Char 

data Token = TokOp Operator | TokNum Int
    deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer ('+':e) = ((TokOp Plus): tokenizer(e))
tokenizer ('-':e) = ((TokOp Minus): tokenizer(e))
tokenizer ('*':e) = ((TokOp Times): tokenizer(e))
tokenizer ('/':e) = ((TokOp Div): tokenizer(e))
tokenizer (e0:e) | isSpace e0 = tokenizer e
                | isDigit e0 = (TokNum (digitToInt e0) : tokenizer(e))

main = print $ tokenizer " 1 + 4 / 3 "