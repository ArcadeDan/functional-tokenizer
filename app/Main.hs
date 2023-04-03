module Main where

import System.IO ( hFlush, stdout )


repl :: IO ()
repl = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  if input == ":q"
    then return ()
    else do
        putStrLn input
        repl


main :: IO ()
main = repl
