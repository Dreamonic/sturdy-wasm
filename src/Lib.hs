module Lib
    ( someFunc
    ) where


import Lexer

someFunc :: IO ()
someFunc = do
    putStrLn "Character: "
    input <- getLine
    print (tokenizeString input)
