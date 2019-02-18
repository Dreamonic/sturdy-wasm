module Lib
    ( someFunc
    ) where

import              Lexer
import              System.Environment
import              Data.List
import qualified    Data.Text as T (strip, pack, unpack)
import              System.IO

-- |Cleans up WAST
cleanInput :: String -> String
cleanInput str = intercalate " " (filter (/="") (map (T.unpack . T.strip . T.pack) (lines str)))

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
    print $ tokenize input

-- |Reads a file and executes 
execute :: String -> IO ()
execute filename = do
    contents <- readFile filename
    print $ tokenize $ cleanInput contents -- TODO: clean up input string

someFunc :: IO ()
someFunc = parseArgs =<< getArgs
