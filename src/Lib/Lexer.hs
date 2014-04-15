module Lib.Lexer where

import Data.Char 

import Lib.Tokens

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('+':e) = TokAdd Plus : tokenize e
tokenize ('-':e) = TokAdd Minus: tokenize e
tokenize ('*':e) = TokMul Times: tokenize e
tokenize ('/':e) = TokMul Div  : tokenize e
tokenize ('(':e) = TokLB       : tokenize e
tokenize (')':e) = TokRB       : tokenize e
tokenize (e0:e) | isSpace e0 = tokenize e
                | isDigit e0 = tokenizeNum (e0:e)
tokenize _ = error "Invalid token in string"

getNum :: String -> (String, String)
getNum [] = ([],[])
getNum (e0:e) | isDigit e0 = let (num, rest) = getNum e
                              in (e0:num, rest)
              | otherwise = ([], e0:e)

tokenizeNum :: String -> [Token]
tokenizeNum (e0:e) = let (num, rest) = getNum e
                      in TokNum(read (e0:num)) : tokenize rest

-- eof
