module Lib
    ( someFunc
    ) where

import Lexer
import System.Environment
import Data.List
import System.IO

-- |Executes interactive mode if no argument is given
-- if a single argument is given, read file and execute
parseArgs :: [String] -> IO ()
parseArgs []        = interactive
parseArgs (arg:[])  = execute arg
parseArgs args      = putStrLn $ "Invalid Options: " ++ intercalate " " args

-- |Reads WAST user input and executes it
interactive :: IO ()
interactive = do 
    putStrLn "Character: "
    input <- getLine
    print $ tokenizeAll input

-- |Reads a file and executes 
execute :: String -> IO ()
execute filename = do
    contents <- readFile filename
    print $ filter (/= '\n') contents -- TODO: clean up input string

someFunc :: IO ()
someFunc = parseArgs =<< getArgs
