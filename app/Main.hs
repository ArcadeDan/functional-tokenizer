module Main where

import System.IO ( hFlush, stdout )
import Data.Char ( isAlpha, isAlphaNum, isDigit, isSpace )


data Token
    = ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | EXP
    | LBRACKET
    | RBRACKET
    | LPAREN
    | RPAREN
    | NUMBER Integer
    | IDENTIFIER String
    | KEYWORD String
    deriving (Show, Eq)

keyowrds :: [String]
keyowrds = ["if", "then", "else", "let", "in", "true", "false"]


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isDigit c = NUMBER (read num) : tokenize rest
    | isAlpha c = let (str, rest) = span isAlphaNum (c:cs)
                  in if str `elem` keyowrds
                        then KEYWORD str : tokenize rest
                        else IDENTIFIER str : tokenize rest
                        
    | c == '+'  = ADD : tokenize cs
    | c == '-'  = SUB : tokenize cs
    | c == '*'  = MUL : tokenize cs
    | c == '/'  = DIV : tokenize cs
    | c == '%'  = MOD : tokenize cs
    | c == '^'  = EXP : tokenize cs
    | c == '['  = LBRACKET : tokenize cs
    | c == ']'  = RBRACKET : tokenize cs
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
