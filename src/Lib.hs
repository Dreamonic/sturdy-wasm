module Lib
    ( someFunc
    ) where


import Lexer
import Parser
import Executor

someFunc :: IO ()
someFunc = do
  print (show $ validParams wasmAdd [I32Val 4, I32Val 2])
--    putStrLn "Character: "
--    input <- getLine
--    print (tokenizeString input)
