module Main where

import System.Environment (getArgs)
import Text.Parsec hiding (State(..))
import Turing

-- | Read a Turing Machine and tape from the command line and show the result. If no arguments are provided, the Machine and tape are read from the console.
main = do
  putStrLn ""
  args <- getArgs
  input <- if length args > 0
    then return $ unwords args
    else getLine
  case parse computation "" input of
    Right computation -> putStrLn $ uncurry showResults computation
    Left  error       -> print error
