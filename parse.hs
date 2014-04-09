import Text.Printf (printf)
import Debug.Trace (trace)
import Control.Monad (forM)

-- shared
data Add = Plus  | Minus deriving (Show, Eq)
data Mul = Times | Div   deriving (Show, Eq)

-- tokens
data Token = TokAdd Add
           | TokMul Mul
           | TokNum Double
           | TokLB
           | TokRB
           deriving (Show, Eq)

-- ast
data AST = Bin2 Add AST AST
         | Bin1 Mul AST AST
         | Num Double
         | Empty
         deriving (Show, Eq)

-- parser helper: take the left argument when it is success otherwise it take the right argument
eor :: Either a b -> Either a b -> Either a b
eor x y = case x of Right _ -> x
                    Left  _ -> y

-- top level parsing function
parse :: [Token] -> Either String (AST, [Token])
parse toks = parseExpr (Empty, toks)

parseExpr :: (AST, [Token]) -> Either String (AST, [Token])
parseExpr state@(_,    []) = return state
parseExpr state@(Empty, _) = either Left                 parseExpr $ parseTerm state
parseExpr state            = either (\_ -> return state) parseExpr $ parseExprEe state

parseExprEe :: (AST, [Token]) -> Either String (AST, [Token])
parseExprEe (left, toka) = do
    (op, tokb) <- parseAdd toka
    (right, tokc) <- parseTerm (Empty, tokb)
    return (Bin2 op left right, tokc)

parseTerm :: (AST, [Token]) -> Either String (AST, [Token])
parseTerm state@(_,       []) = return state
parseTerm       (Empty, toks) = either Left                 parseTerm $ parseFactor toks
parseTerm state               = either (\_ -> return state) parseTerm $ parseTermTt state
                                                                 
parseTermTt :: (AST, [Token]) -> Either String (AST, [Token])
parseTermTt (left, toka) = do
    (op, tokb) <- parseMul toka
    (right, tokc) <- parseFactor tokb
    return (Bin1 op left right, tokc)

parseFactor :: [Token] -> Either String (AST, [Token])
parseFactor (TokNum n:toka) = return (Num n, toka)
parseFactor (TokLB   :toka) = parseFactorFe toka
parseFactor               _ = Left "Expected a grammatical factor"

-- parseFactor helper to produce the Fe constructor
parseFactorFe :: [Token] -> Either String (AST, [Token])
parseFactorFe toka = do
    (expr, tokb) <- parseExpr (Empty, toka)
    case tokb of
        TokRB:tokc -> return (expr, tokc)
        _          -> Left "Expected a right paren"

-- consume an operator in {+,-} from state
parseAdd :: [Token] -> Either String (Add, [Token])
parseAdd (TokAdd op:ta) = Right (op, ta)
parseAdd              _ = Left "Expected an additive operator"

-- consume an operator in {*,/} from state
parseMul :: [Token] -> Either String (Mul, [Token])
parseMul (TokMul op:ta) = Right (op, ta)
parseMul              _ = Left "Expected a multiplicative operator"

lexed_ :: [[Token]]
lexed_ = [ [TokNum 20.0,TokAdd Minus,TokNum 6.0,TokAdd Plus,TokNum 4.0]
         , [TokNum 20.0,TokAdd Minus,TokLB,TokNum 6.0,TokAdd Plus,TokNum 4.0,TokRB]
         , [TokNum 20.0,TokAdd Minus,TokNum 2.0,TokMul Times,TokNum 3.0,TokAdd Plus,TokNum 44.0,TokMul Div,TokNum 11.0]
         , [TokLB,TokLB,TokNum 2.0,TokAdd Plus,TokNum 3.0,TokRB,TokMul Times,TokNum 4.0,TokAdd Plus,TokNum 6.0,TokAdd Plus,TokNum 2.0,TokMul Times,TokNum 5.0,TokRB,TokMul Div,TokNum 4.0]
         ]

parsed_ :: [Either String (AST, [Token])]
parsed_ = [ Right (Bin2 Plus (Bin2 Minus (Num 20.0) (Num 6.0)) (Num 4.0),[])
          , Right (Bin2 Minus (Num 20.0) (Bin2 Plus (Num 6.0) (Num 4.0)),[])
          , Right (Bin2 Plus (Bin2 Minus (Num 20.0) (Bin1 Times (Num 2.0) (Num 3.0))) (Bin1 Div (Num 44.0) (Num 11.0)),[])
          , Right (Bin1 Div (Bin2 Plus (Bin2 Plus (Bin1 Times (Bin2 Plus (Num 2.0) (Num 3.0)) (Num 4.0)) (Num 6.0)) (Bin1 Times (Num 2.0) (Num 5.0))) (Num 4.0),[])
          ]

main :: IO ()
main = do
    results <- forM (zip lexed_ parsed_) $ \(toks, expected) -> do
        let ast = parse toks
        print $ ast == expected
    return ()

-- eof
