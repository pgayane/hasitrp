import Text.Printf (printf)
import Debug.Trace (trace)
import Control.Monad (forM_)

-- shared
data Add = Plus | Minus
         deriving (Show, Eq)

data Mul = Times | Div
         deriving (Show, Eq)

-- tokens
data Token = TokAdd Add
           | TokMul Mul
           | TokNum Double
           | TokLB
           | TokRB
           deriving (Show, Eq)

-- ast
data Expr  = Ee Expr Add Term
           | Et Term
           | Eempty Int
           deriving(Show, Eq)

data Term  = Tt Term Mul Factor
           | Tf Factor
           | Tempty Int
           deriving(Show, Eq)

data Factor = Fe Expr
            | Fn Double
            deriving(Show, Eq)

-- parser helper: take the left argument when it is success otherwise it take the right argument
eor :: Either a b -> Either a b -> Either a b
eor x y = case x of Right _ -> x
                    Left  _ -> y

-- top level parsing function
parse :: [Token] -> Either String (Expr, [Token])
parse toks = parseExpr (Eempty undefined, toks)

parseExpr :: (Expr, [Token]) -> Either String (Expr, [Token])
parseExpr state@(_,       []) = return state
parseExpr state@(Eempty _, _) = either Left                 parseExpr $ parseExprEt state
parseExpr state               = either (\_ -> return state) parseExpr $ parseExprEe state

-- parseExpr helper to produce the Et constructor
parseExprEt :: (Expr, [Token]) -> Either String (Expr, [Token])
parseExprEt (Eempty _, toka) = do
    (term, tokb) <- parseTerm (Tempty undefined, toka)
    return (Et term, tokb)
-- parseExpr helper to produce the Ee constructor
parseExprEe :: (Expr, [Token]) -> Either String (Expr, [Token])
parseExprEe (left, toka) = do
    (op, tokb) <- parseAdd toka
    (right, tokc) <- parseTerm (Tempty undefined, tokb)
    return (Ee left op right, tokc)

parseTerm :: (Term, [Token]) -> Either String (Term, [Token])
parseTerm state@(_,       []) = return state
parseTerm state@(Tempty _, _) = either Left                 parseTerm $ parseTermTf state
parseTerm state               = either (\_ -> return state) parseTerm $ parseTermTt state
                                                                 
-- parseTerm helper to produce the Tt constructor
parseTermTt :: (Term, [Token]) -> Either String (Term, [Token])
parseTermTt (left, toka) = do
    (op, tokb) <- parseMul toka
    (right, tokc) <- parseFactor tokb
    return (Tt left op right, tokc)
-- parseTerm helper to produce the Tf constructor
parseTermTf :: (Term, [Token]) -> Either String (Term, [Token])
parseTermTf (Tempty _, toka) = do
    (factor, tokb) <- parseFactor toka
    return (Tf factor, tokb)

parseFactor :: [Token] -> Either String (Factor, [Token])
parseFactor (TokNum n:toka) = return (Fn n, toka)
parseFactor (TokLB   :toka) = parseFactorFe toka
parseFactor               _ = Left "Expected a grammatical factor"

-- parseFactor helper to produce the Fe constructor
parseFactorFe :: [Token] -> Either String (Factor, [Token])
parseFactorFe toka = do
    (expr, tokb) <- parseExpr (Eempty undefined, toka)
    case tokb of
        TokRB:tokc -> return (Fe expr, tokc)
        _          -> Left "Expected a right paren"

-- consume an operator in {+,-} from state
parseAdd :: [Token] -> Either String (Add, [Token])
parseAdd (TokAdd op:ta) = Right (op, ta)
parseAdd              _ = Left "Expected an additive operator"

-- consume an operator in {*,/} from state
parseMul :: [Token] -> Either String (Mul, [Token])
parseMul (TokMul op:ta) = Right (op, ta)
parseMul              _ = Left "Expected a multiplicative operator"

examples :: [[Token]]
examples = [ [ TokNum 20
             , TokAdd Minus
             , TokNum 6
             , TokAdd Plus
             , TokNum 4
             ]
           , [ TokNum 20
             , TokAdd Minus
             , TokLB
             , TokNum 6, TokAdd Plus, TokNum 4
             , TokRB
             ]
           , [ TokNum 20
             , TokAdd Minus
             , TokNum 2, TokMul Times, TokNum 3
             , TokAdd Plus
             , TokNum 44, TokMul Div, TokNum 11
             ]
           , [ TokLB
             , TokLB
             , TokNum 2.0, TokAdd Plus, TokNum 3.0
             , TokRB
             , TokMul Times, TokNum 4.0
             , TokAdd Plus
             , TokNum 6.0
             , TokAdd Plus
             , TokNum 2.0, TokMul Times, TokNum 5.0
             , TokRB
             , TokMul Div, TokNum 4.0
             ]
           ]

main = forM_ examples $ \toks -> do
    print toks
    print $ parse toks

-- eof
