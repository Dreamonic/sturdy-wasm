module Instruction.Util(
    WasmStack(..)
    , WasmHeap(..)
    , WasmMem(..)
    , wasmNot
    , boolToWasmVal
) where

import Parser
import qualified Data.Map as Map

type WasmStack = [WasmVal]
type WasmHeap = Map.Map String WasmVal
type WasmMem = (WasmStack, WasmHeap)

boolToWasmVal :: Bool -> WasmVal
boolToWasmVal b = if b then I32Val 1 else I32Val 0

wasmNot :: WasmVal -> WasmVal
wasmNot (I32Val x) = if x==0 then I32Val 1 else I32Val 0