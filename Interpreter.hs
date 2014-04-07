--module Interpreter (Expr(..), interp) where
import Data.Char 

data Expr = SumExpr Operator Expr Expr |
            ProdExpr Operator Expr Expr |
            NumExpr Double
        deriving Show

data Token = TokOp Operator | TokNum Double | TokLB | TokRB | TokEnd
    deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('+':e) = ((TokOp Plus): tokenize(e))
tokenize ('-':e) = ((TokOp Minus): tokenize(e))
tokenize ('*':e) = ((TokOp Times): tokenize(e))
tokenize ('/':e) = ((TokOp Div): tokenize(e))
tokenize ('(':e) = (TokLB : tokenize(e))
tokenize (')':e) = (TokRB : tokenize(e))
tokenize (e0:e) | isSpace e0 = tokenize e
                | isDigit e0 = tokenizeNum (e0:e)


getNum :: String -> (String, String)
getNum [] = ([],[])
getNum(e0:e) | isDigit e0 = let(num, rest) = getNum(e) in ((e0:num),rest)
             | otherwise = ([],(e0:e))

tokenizeNum :: String -> [Token]
tokenizeNum (e0:e) = let(num, rest) = getNum(e) in TokNum(read (e0:num)) : tokenize(rest)


expression :: [Token] -> (Expr, [Token])
expression toks = 
   let (subexps, toks') = subexp toks
   in
      case lookAhead toks' of
         -- Term [+-] Expression
         (TokOp op) | elem op [Plus, Minus] -> 
            let (exTree, toks'') = expression (accept toks') 
            in (SumExpr op subexps exTree, toks'')
         -- Term
         _ -> (subexps, toks')

subexp :: [Token] -> (Expr, [Token])
subexp toks = 
   let (facTree, toks') = item toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = subexp (accept toks') 
            in (ProdExpr op facTree termTree, toks'')
         _ -> (facTree, toks')

item :: [Token] -> (Expr, [Token])
item (TokNum t: toks) = (NumExpr t, toks)
item (TokLB : toks) = let (exp, toks') = expression (toks)
                            in
                            if lookAhead toks' /= TokRB
                            then error "Missing right parenthesis"
                            else (exp, accept toks')
item (_:toks) = error $ "Parse error on token: " ++ show toks

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

parse :: [Token] -> Expr
parse toks = let (tree, toks') = expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

eval :: Expr -> Double
eval (NumExpr n) = n
eval (SumExpr Plus e0 e1) = (eval e0) + (eval e1)
eval (SumExpr Minus e0 e1) = (eval e0) - (eval e1)
eval (ProdExpr Div e0 e1) = (eval e0) / (eval e1)
eval (ProdExpr Times e0 e1) = (eval e0) * (eval e1)

main = (print . eval . parse . tokenize) "(2+3+4)*2 + (2*5 + 2)/6"