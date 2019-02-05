module Lib
    ( someFunc
    ) where


import Lexer
import Executor

someFunc :: IO ()
someFunc = do
  executeFunc wasmAdd
--    putStrLn "Character: "
--    input <- getLine
--    print (tokenizeString input)
