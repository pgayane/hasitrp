import Text.Printf (printf)
import Debug.Trace (trace)

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

parse :: [Token] -> Either String (Expr, [Token])
parse toks = parseExpr (trace "parse expr" (error "Empty1"), toks)

parseAdd :: [Token] -> Either String (Add, [Token])
parseAdd (TokAdd op:ta) = Right (op, ta)
parseAdd              _ = Left "Expected an additive operator"

parseMul :: [Token] -> Either String (Mul, [Token])
parseMul (TokMul op:ta) = Right (op, ta)
parseMul              _ = Left "Expected a multiplicative operator"

parseExpr :: (Expr, [Token]) -> Either String (Expr, [Token])
parseExpr x@(tree,   []) = return x
--parseExpr x@(tree, TokRB:toka) = return x
parseExpr x@(left, toka) = either (\_ -> return x) parseExpr (ee `eor` et)
    where
        ee = do (op, tokb) <- parseAdd toka
                (right, tokc) <- parseTerm (trace "parseExpr 1 term" (Tempty $ error ("Empty2" ++ show x)), tokb)
                return (Ee left op right, tokc)
        et = do (term, tokb) <- parseTerm (trace "parseExpr 2 term" (Tempty $ error ("Empty3" ++ show x)), toka)
                return (Et term, tokb)

parseTerm :: (Term, [Token]) -> Either String (Term, [Token])
parseTerm x@(tree,       []) = return x
parseTerm x@(Tempty _, toka) = either Left parseTerm $ do (factor, tokb) <- parseFactor toka
                                                          return (Tf factor, tokb)
parseTerm x@(left,     toka) = either (\_ -> return x) parseTerm $ do (op, tokb) <- parseMul toka
                                                                      (right, tokc) <- parseFactor tokb
                                                                      return (Tt left op right, tokc)

-- either (\_ -> trace ("parseTerm 3 fail: " ++ show x) $ return x) parseTerm (tt `eor` tf)
--     where
--         tt = 
--              kb
--              
--         tf = 
             

parseFactor :: [Token] -> Either String (Factor, [Token])
parseFactor (TokNum n:toka) = return (Fn n, toka)
parseFactor (TokLB   :toka) = do (expr, tokb) <- parseExpr (trace "parseFactor ( expr" (error "Empty3"), toka)
                                 case tokb of TokRB:tokc -> return (Fe expr, tokc)
                                              _          -> Left "Expected a right paren"
parseFactor               _ = Left "Expected a grammatical factor"

main = do
--  let lexed = [ TokNum 20.0
--              , TokAdd Minus
--              , TokNum 2.0
--              , TokMul Times
--              , TokNum 3.0
--              , TokAdd Plus
--              , TokNum 44.0
--              , TokMul Div
--              , TokNum 11.0
--              ]
--  print lexed
--  print $ parse lexed
--  let lexed = [ TokNum 20
--              , TokAdd Minus
--              , TokNum 6
--              , TokAdd Plus
--              , TokNum 4
--              ]
--  print lexed
--  print $ parse lexed
    let lexed = [ TokNum 20
                , TokAdd Minus
                , TokLB
                , TokNum 6
                , TokAdd Plus
                , TokNum 4
                , TokRB
                ]
    print lexed
    print $ parse lexed

-- eof
