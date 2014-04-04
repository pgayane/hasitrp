--module Interpreter (Expr(..), interp) where
import Data.Char 

data Token = TokOp Operator | TokNum Int | TokLB | TokRB
    deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer ('+':e) = ((TokOp Plus): tokenizer(e))
tokenizer ('-':e) = ((TokOp Minus): tokenizer(e))
tokenizer ('*':e) = ((TokOp Times): tokenizer(e))
tokenizer ('/':e) = ((TokOp Div): tokenizer(e))
tokenizer ('(':e) = (TokLB : tokenizer(e))
tokenizer (')':e) = (TokRB : tokenizer(e))
tokenizer (e0:e) | isSpace e0 = tokenizer e
                | isDigit e0 = tokenizeNum (e0:e)


getNum :: String -> (String, String)
getNum [] = ([],[])
getNum(e0:e) | isDigit e0 = let(num, rest) = getNum(e) in ((e0:num),rest)
             | otherwise = ([],(e0:e))

tokenizeNum :: String -> [Token]
tokenizeNum (e0:e) = let(num, rest) = getNum(e) in TokNum(read (e0:num)) : tokenizer(rest)


main = print $ tokenizer "(1-43) + 56 / 3 "