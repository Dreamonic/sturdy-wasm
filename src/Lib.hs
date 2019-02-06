module Lib
    ( someFunc
    ) where

import Lexer
import Parser
import Executor

someFunc :: IO ()
someFunc = do
  print (show $ executeFunc wasmAdd [I32Val 8, I32Val 2])
  print (show $ executeFunc wasmAddThree [F32Val 2.2, F32Val 6.7, F32Val 13.21])
--    putStrLn "Character: "
--    input <- getLine
--    print (tokenizeString input)

wasmAdd = Func "add" [Param "a" I32, Param "b" I32] (Block [Result I32] [
  LocalGet "a",
  LocalGet "b",
  Numeric (Add I32)
  ])

wasmAddThree = Func "addThree" [Param "a" F32, Param "b" F32, Param "c" F32]
  (Block [Result F32] [
    LocalGet "a",
    LocalGet "b",
    Numeric (Add F32),
    LocalGet "c",
    Numeric (Add F32)
    ])
