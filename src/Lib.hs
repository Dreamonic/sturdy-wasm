module Lib
    ( someFunc
    ) where

import Lexer
import Parser
import Executor

someFunc :: IO ()
someFunc = do
  print (show $ execFunc foo [I32Val 8, I32Val 2, I32Val 5, I32Val 7])
  print (show $ execFunc addThree [F32Val 2.2, F32Val 6.7, F32Val 13.21])
--    putStrLn "Character: "
--    input <- getLine
--    print (tokenizeString input)

foo = Func "foo" [Param "a" I32, Param "b" I32, Param "c" I32,
  Param "d" I32] (Block [Result I32] [
    LocalGet "a",
    LocalGet "b",
    Numeric (Div I32 Signed),
    LocalGet "c",
    LocalGet "d",
    Numeric (Mul I32),
    Numeric (Sub I32)
    ])

addThree = Func "addThree" [Param "a" F32, Param "b" F32, Param "c" F32]
  (Block [Result F32] [
    LocalGet "a",
    LocalGet "b",
    Numeric (Add F32),
    LocalGet "c",
    Numeric (Add F32)
    ])
