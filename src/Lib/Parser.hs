module Lib.Parser where
    
import Text.Printf (printf)
import Debug.Trace (trace)
import Control.Monad (forM)

import Lib.Tokens
import Lib.AST

type Parse a         = (a, [Token])
type ParseEither a   = Either String (Parse a)
type SimpleParser a  = [Token] -> ParseEither a
type TopDownParser a = Parse a -> ParseEither a

-- top level parsing function
parse :: SimpleParser AST
parse toks = parseExpr (Empty, toks)

-- AST level expr parser
parseExpr :: TopDownParser AST
parseExpr state@(_,    []) = return state
parseExpr state@(Empty, _) = either Left                 parseExpr $ parseTerm state
parseExpr state            = either (\_ -> return state) parseExpr $ parseExprOp state
-- helper for parseExpr to make proper binary operations which are exprs
parseExprOp :: TopDownParser AST
parseExprOp (left, toka) = do
    (op, tokb) <- parseAdd toka
    (right, tokc) <- parseTerm (Empty, tokb)
    return (Expr op left right, tokc)

-- AST level term parser
parseTerm :: TopDownParser AST
parseTerm state@(_,       []) = return state
parseTerm       (Empty, toks) = either Left                 parseTerm $ parseFactor toks
parseTerm state               = either (\_ -> return state) parseTerm $ parseTermOp state
-- helper for parseTerm to make proper binary operations which are terms                                                                 
parseTermOp :: TopDownParser AST
parseTermOp (left, toka) = do
    (op, tokb) <- parseMul toka
    (right, tokc) <- parseFactor tokb
    return (Term op left right, tokc)

-- AST level factor parser
parseFactor :: SimpleParser AST
parseFactor (TokNum n:toka) = return (Factor n, toka)
parseFactor (TokLB   :toka) = parseFactorParens toka
parseFactor               _ = Left "Expected a grammatical factor"
-- helper for parseFactor to make parenthesized exprs which are factors
parseFactorParens :: SimpleParser AST
parseFactorParens toka = do
    (expr, tokb) <- parseExpr (Empty, toka)
    case tokb of
        TokRB:tokc -> return (expr, tokc)
        _          -> Left "Expected a right paren"

-- consume an operator in {+,-} from state
parseAdd :: SimpleParser Add
parseAdd (TokAdd op:ta) = Right (op, ta)
parseAdd              _ = Left "Expected an additive operator"

-- consume an operator in {*,/} from state
parseMul :: SimpleParser Mul
parseMul (TokMul op:ta) = Right (op, ta)
parseMul              _ = Left "Expected a multiplicative operator"

lexed_ :: [[Token]]
lexed_ = [ [TokNum 20.0,TokAdd Minus,TokNum 6.0,TokAdd Plus,TokNum 4.0]
         , [TokNum 20.0,TokAdd Minus,TokLB,TokNum 6.0,TokAdd Plus,TokNum 4.0,TokRB]
         , [TokNum 20.0,TokAdd Minus,TokNum 2.0,TokMul Times,TokNum 3.0,TokAdd Plus,TokNum 44.0,TokMul Div,TokNum 11.0]
         , [TokLB,TokLB,TokNum 2.0,TokAdd Plus,TokNum 3.0,TokRB,TokMul Times,TokNum 4.0,TokAdd Plus,TokNum 6.0,TokAdd Plus,TokNum 2.0,TokMul Times,TokNum 5.0,TokRB,TokMul Div,TokNum 4.0]
         ]

parsed_ :: [Either String (AST, [Token])]
parsed_ = [ Right (Expr Plus (Expr Minus (Factor 20.0) (Factor 6.0)) (Factor 4.0),[])
          , Right (Expr Minus (Factor 20.0) (Expr Plus (Factor 6.0) (Factor 4.0)),[])
          , Right (Expr Plus (Expr Minus (Factor 20.0) (Term Times (Factor 2.0) (Factor 3.0))) (Term Div (Factor 44.0) (Factor 11.0)),[])
          , Right (Term Div (Expr Plus (Expr Plus (Term Times (Expr Plus (Factor 2.0) (Factor 3.0)) (Factor 4.0)) (Factor 6.0)) (Term Times (Factor 2.0) (Factor 5.0))) (Factor 4.0),[])
          ]

test :: IO ()
test = do
    results <- forM (zip lexed_ parsed_) $ \(toks, expected) -> do
        let ast = parse toks
        if ast == expected
        then putStr "."
        else printf "\nTest failed\nInput: %s\nOutput:   %s\nExpected: %s\n" (show toks) (show ast) (show expected)
    putStr"\n"
    return ()

-- eof
