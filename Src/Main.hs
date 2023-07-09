module Main where

import System.Environment

import Src.Syntax
import Src.Execution
import Src.Parsing

main :: IO ()
main = do
    args <- getArgs
    let programName = head args
    program <- readFile programName
    let s = show $ execProgram (parseProgram program) (State [("x", 1)])
    putStrLn s