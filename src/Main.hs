import System.IO (stdout, hFlush)
import Control.Monad (forever, when, unless)
import Control.Exception (try, evaluate, ErrorCall)

import Lib.Lexer
import Lib.Parser
import Lib.Evaluator

interpret :: String -> Double
interpret s | null extra = eval ast
            | otherwise  = error $ "Unparsed tokens " ++ show extra
    where 
        (ast, extra) = either error id . parse . tokenize $ s

main :: IO ()
main = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    unless (null input) $ do
        let num = interpret input
        e <- try $ evaluate num :: IO (Either ErrorCall Double)
        either print print e

-- eof
