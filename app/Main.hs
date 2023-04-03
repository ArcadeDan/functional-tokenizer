module Main where

import System.IO ( hFlush, stdout )

repl :: IO b
repl = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  putStrLn input
  repl


main :: IO ()
main = repl
