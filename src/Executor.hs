module Executor
  ( execFunc
  ) where

import Parser
import qualified Data.Map as Map

-- |Interprets and executes the given wasm function using the given list of
-- WasmVal parameters.
-- If the parameter list does not match the required parameters of the function
-- definition an error is thrown. Errors are also thrown if instructions in the
-- function are not supported or lead to run-time errors.
execFunc :: Func -> [WasmVal] -> [WasmVal]
execFunc (Func name params block) vals =
  if (validParams params vals) then
    let kvPairs = zipWith (\(Param name _) x -> (name, x)) params vals
    in fst (execBlock block [] (Map.fromList kvPairs))
    else error ("Type error: given stack " ++ (show vals) ++ " does not match "
      ++ "the parameter types " ++ (show params) ++ " of function " ++ name
      ++ ".")

-- |Returns a modification of the given WasmVal stack and execution
-- environment by executing the instructions in the given block.
-- An error might be thrown if instructions in the function are not supported or
-- lead to run-time errors.
execBlock :: Block -> [WasmVal] -> Map.Map String WasmVal ->
  ([WasmVal], Map.Map String WasmVal)
execBlock (Block res []) stack locals = if validResults res (reverse stack)
  then (reverse stack, locals) else
    error ("Type error: resulting stack " ++ (show stack) ++ " does not "
      ++ "match result types " ++ (show res) ++ " of the block.")
execBlock (Block res (i:is)) stack locals =
  let update = execInstr i stack locals
  in  execBlock (Block res (is)) (fst update) (snd update)

-- |Determines whether or not the given WasmVal stack matches a given parameter
-- definition. Match checking is done using the valsOfType function.
validParams :: [Param] -> [WasmVal] -> Bool
validParams params vals =
  let types = map (\(Param _ typ) -> typ) params
  in valsOfType vals types

-- |Determines whether or not the given WasmVal stack matches a given list of
-- return values. Match checking is done using the valsOfType function.
validResults :: [Result] -> [WasmVal] -> Bool
validResults results vals =
  let types = map (\(Result typ) -> typ) results
  in valsOfType vals types

-- |Checks if WasmVals in a list correspond with a list of WasmTypes. False is
-- returned if the two lists differ in size.
valsOfType :: [WasmVal] -> [WasmType] -> Bool
valsOfType vals types =
  let zipped = zipWith ofType vals types
  in (length types) == (length vals) && (foldl (&&) True zipped)

-- |Throws an error with the message prefixed with "TypeError: ".
typeError :: String -> a
typeError message = error ("TypeError: " ++ message)

-- |Returns a modification of the given WasmVal stack and execution environment
-- by executing the given Instr in the given block as if it were an actual
-- wasm instruction.
-- Throws an error if there is no implementation for the given Instr or if the
-- Instr cannot be executed on this given environment.
execInstr :: Instr -> [WasmVal] -> Map.Map String WasmVal ->
  ([WasmVal], Map.Map String WasmVal)
execInstr (EnterBlock block) stack locals = execBlock block stack locals
execInstr (If instr) (x:stack) locals = if valToBool x
  then execInstr instr stack locals else (stack, locals)
execInstr (LocalGet tag) stack locals = case Map.lookup tag locals of
  Just val -> (val:stack, locals)
  Nothing -> error ("No local variable in scope with tag \"" ++ tag ++ "\".")
execInstr (LocalSet tag) (x:stack) locals = (stack, Map.insert tag x locals)
execInstr (LocalTee tag) stack locals =
  let whenSet = execInstr (LocalSet tag) stack locals
  in  execInstr (LocalGet tag) (fst whenSet) (snd whenSet)
execInstr (Numeric (Const val)) stack locals = (val:stack, locals)
execInstr (Numeric (Add typ)) (x:y:stack) locals =
  ((binOp typ y x (+) (+)):stack, locals)
execInstr (Numeric (Sub typ)) (x:y:stack) locals =
  ((binOp typ y x (-) (-)):stack, locals)
execInstr (Numeric (Mul typ)) (x:y:stack) locals =
  ((binOp typ y x (*) (*)):stack, locals)
execInstr (Numeric (Div typ Signed)) (x:y:stack) locals =
  ((binOp typ y x div (/)):stack, locals)
execInstr Nop stack locals = (stack, locals)
execInstr instr _ _ = error ("Couldn't evaluate " ++ (show instr) ++ ".")

-- |Defines wasm I32 values as "falsy" if equal to 0 and "truthy" otherwise.
-- Throws an error if a value of any other type than I32 is given.
valToBool :: WasmVal -> Bool
valToBool val = case val of
  I32Val x -> x /= 0
  val -> typeError ((show val) ++ " cannot be evaluated as a boolean.")

-- |Evaluates an expression of two WasmVals and a numeric binary operator and
-- returns the resulting WasmVal.
-- Throws an error if the operation and of the WasmVals aren't all of the same
-- type.
binOp :: WasmType -> WasmVal -> WasmVal -> (Integer -> Integer -> Integer) ->
  (Double -> Double -> Double) -> WasmVal
binOp typ x y opI opF = if not (x `ofType` typ && y `ofType` typ)
  then typeError ("binary operation type does not match the types of its "
    ++ "arguments.")
  else case (x, y) of
    (I32Val a, I32Val b) -> I32Val (a `opI` b)
    (I64Val a, I64Val b) -> I64Val (a `opI` b)
    (F32Val a, F32Val b) -> F32Val (a `opF` b)
    (F64Val a, F64Val b) -> F64Val (a `opF` b)
    (_, _) -> typeError "binary operation on two different types."
