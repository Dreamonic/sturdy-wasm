module Instruction.Relational(
    execRelational
) where

import Parser
import Instruction.Util
import Exception
import qualified Control.Exception as E

-- |    Execute a relational instruction.
--      
--      Supported Instructions are: 
--      i32: i32.eq, i32.ne, i32.lt_s, i32.le_s, i32.gt_s, i32.ge_s
execRelational :: Instr -> WasmStack -> WasmHeap -> WasmMem
execRelational instr (x:y:stack) locals = (execR instr x y:stack, locals)

-- |    Determines which instruction is called, and delegate it to the right method.
execR :: Instr -> WasmVal -> WasmVal -> WasmVal
execR (Numeric (Eql typ)) x y = wasmEquals x y
execR (Numeric (Ne typ)) x y = wasmNotEquals x y
execR (Numeric (Lt typ sign)) x y = wasmLessThan x y
execR (Numeric (Le typ sign)) x y = wasmLessOrEqual x y
execR (Numeric (Gt typ sign)) x y = wasmGreaterThan x y
execR (Numeric (Ge typ sign)) x y = wasmGreaterOrEqual x y
execR instr _ _ = E.throw $ NotImplemented instr


-- |    The .eq operator in WASM.
wasmEquals :: WasmVal -> WasmVal -> WasmVal
wasmEquals (I32Val x) (I32Val y) = boolToWasmVal (x == y)

-- |    The .lt_s operator in WASM.
wasmLessThan :: WasmVal -> WasmVal -> WasmVal
wasmLessThan (I32Val x) (I32Val y) = boolToWasmVal (x < y)

-- |    The .le_s operator in WASM.
wasmLessOrEqual :: WasmVal -> WasmVal -> WasmVal
wasmLessOrEqual (I32Val x) (I32Val y) = boolToWasmVal(x <= y)

-- |    The .ne operator in WASM.
wasmNotEquals :: WasmVal -> WasmVal -> WasmVal
wasmNotEquals x y = wasmNot $ wasmEquals x y

-- |    The .gt_s opeartor in WASM.
wasmGreaterThan :: WasmVal -> WasmVal -> WasmVal
wasmGreaterThan x y = wasmNot $ wasmLessOrEqual x y

-- |    The .gt_e operator in WASM.
wasmGreaterOrEqual :: WasmVal -> WasmVal -> WasmVal
wasmGreaterOrEqual x y = wasmNot $ wasmLessThan x y
