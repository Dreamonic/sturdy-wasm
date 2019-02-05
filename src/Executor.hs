module Executor

where

import Parser

wasmAdd = Func "add" [Param "a" I32, Param "b" I32] [Result I32] (Block [
  LocalGet "a",
  LocalGet "b",
  Numeric (Add I32)
  ])

--executeFunc :: Func [WasmVal] -> Maybe [WasmVal]
--executeFunc (Func name params block) = putStrLn name
