module Main where

import System.IO ( hFlush, stdout )
import Data.Char


data Token
    = ADD
    | SUB
    | MUL
    | DIV
    | LPAREN
    | RPAREN
    | NUMBER
    deriving (Show, Eq)

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isDigit c = NUMBER : tokenize (dropWhile isDigit cs)
    | c == '+'  = ADD : tokenize cs
    | c == '-'  = SUB : tokenize cs
    | c == '*'  = MUL : tokenize cs
    | c == '/'  = DIV : tokenize cs
    | c == '('  = LPAREN : tokenize cs
    | c == ')'  = RPAREN : tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]
    where
        (num, rest) = span isDigit (c:cs)

repl :: IO ()
repl = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  if input == ":q"
    then return ()
    else do
        let tokens = tokenize input
        putStrLn $ show tokens
        repl


main :: IO ()
main = repl
